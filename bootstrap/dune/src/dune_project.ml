open! Stdune
open Import
open Dsexp.Of_sexp

module Kind = struct
  type t =
    | Dune
    | Jbuilder

  let to_sexp t =
    Sexp.Atom
      (match t with
       | Dune -> "dune"
       | Jbuilder -> "jbuilder")
end

module Name : sig
  type t = private
    | Named     of string
    | Anonymous of Path.t

  val compare : t -> t -> Ordering.t

  val to_string_hum : t -> string

  val dparse : t Dsexp.Of_sexp.t
  val to_sexp : t Sexp.To_sexp.t

  val encode : t -> string
  val decode : string -> t

  val anonymous : Path.t -> t option
  val named : string -> t option

  val anonymous_root : t

  module Infix : Comparable.OPS with type t = t
end = struct
  module T = struct
    type t =
      | Named     of string
      | Anonymous of Path.t

    let compare a b =
      match a, b with
      | Named     x, Named     y -> String.compare x y
      | Anonymous x, Anonymous y -> Path.compare   x y
      | Named     _, Anonymous _ -> Lt
      | Anonymous _, Named     _ -> Gt
  end

  include T

  module Infix = Comparable.Operators(T)

  let anonymous_root = Anonymous Path.root

  let to_string_hum = function
    | Named     s -> s
    | Anonymous p -> sprintf "<anonymous %s>" (Path.to_string_maybe_quoted p)

  let to_sexp = function
    | Named s -> Sexp.To_sexp.string s
    | Anonymous p ->
      List [ Atom "anonymous"
           ; Path.to_sexp p
           ]

  let validate name =
    let len = String.length name in
    len > 0 &&
    String.for_all name ~f:(function
      | '.' | '/' -> false
      | _         -> true)

  let named name =
    if validate name then
      Some (Named name)
    else
      None

  let anonymous path =
    if Path.is_managed path then
      Some (Anonymous path)
    else
      None

  let dparse =
    Dsexp.Of_sexp.plain_string (fun ~loc s ->
      if validate s then
        Named s
      else
        Dsexp.Of_sexp.of_sexp_errorf loc "invalid project name")

  let encode = function
    | Named     s -> s
    | Anonymous p ->
      if Path.is_root p then
        "."
      else
        "." ^ String.map (Path.to_string p)
                ~f:(function
                  | '/' -> '.'
                  | c   -> c)

  let decode =
    let invalid s =
      (* Users would see this error if they did "dune build
         _build/default/.ppx/..." *)
      die "Invalid encoded project name: %S" s
    in
    fun s ->
      match s with
      | "" -> invalid s
      | "." -> anonymous_root
      | _ when s.[0] = '.' ->
        let p =
          Path.of_string
            (String.split s ~on:'.'
             |> List.tl
             |> String.concat ~sep:"/")
        in
        if not (Path.is_managed p) then invalid s;
        Anonymous p
      | _ when validate s -> Named s
      | _ -> invalid s
end

module Project_file = struct
  type t =
    { file           : Path.t
    ; mutable exists : bool
    }

  let to_sexp { file; exists } =
    Sexp.To_sexp.(
      record
        [ "file", Path.to_sexp file
        ; "exists", bool exists
        ])
end

type t =
  { kind          : Kind.t
  ; name          : Name.t
  ; root          : Path.Local.t
  ; version       : string option
  ; packages      : Package.t Package.Name.Map.t
  ; stanza_parser : Stanza.t list Dsexp.Of_sexp.t
  ; project_file  : Project_file.t
  }

let packages t = t.packages
let version t = t.version
let name t = t.name
let root t = t.root
let stanza_parser t = t.stanza_parser

include Versioned_file.Make(struct
    type t = Stanza.Parser.t list
  end)

module Project_file_edit = struct
  open Project_file

  let notify_user s =
    kerrf ~f:print_to_console "@{<warning>Info@}: %s\n" s

  let ensure_exists t =
    if not t.exists then begin
      let ver = (Lang.get_exn "dune").version in
      let s = sprintf "(lang dune %s)" (Syntax.Version.to_string ver) in
      notify_user
        (sprintf "creating file %s with this contents: %s"
           (Path.to_string_maybe_quoted t.file) s);
      Io.write_file t.file (s ^ "\n") ~binary:false;
      t.exists <- true
    end

  let get t =
    ensure_exists t;
    t.file

  let append t str =
    let file = get t in
    let prev = Io.read_file file ~binary:false in
    notify_user
      (sprintf "appending this line to %s: %s"
         (Path.to_string_maybe_quoted file) str);
    Io.with_file_out file ~binary:false ~f:(fun oc ->
      List.iter [prev; str] ~f:(fun s ->
        output_string oc s;
        let len = String.length s in
        if len > 0 && s.[len - 1] <> '\n' then output_char oc '\n'))
end

let ensure_project_file_exists t =
  Project_file_edit.ensure_exists t.project_file

let append_to_project_file t str =
  Project_file_edit.append t.project_file str

module Extension = struct
  type t =
    { syntax  : Syntax.t
    ; stanzas : Stanza.Parser.t list Dsexp.Of_sexp.t
    }

  type instance =
    { extension  : t
    ; version    : Syntax.Version.t
    ; loc        : Loc.t
    ; parse_args : Stanza.Parser.t list Dsexp.Of_sexp.t -> Stanza.Parser.t list
    }

  let extensions = Hashtbl.create 32

  let register syntax stanzas =
    let name = Syntax.name syntax in
    if Hashtbl.mem extensions name then
      Exn.code_error "Dune_project.Extension.register: already registered"
        [ "name", Sexp.To_sexp.string name ];
    Hashtbl.add extensions name { syntax; stanzas }

  let instantiate ~loc ~parse_args (name_loc, name) (ver_loc, ver) =
    match Hashtbl.find extensions name with
    | None ->
      Errors.fail name_loc "Unknown extension %S.%s" name
        (hint name (Hashtbl.keys extensions))
    | Some t ->
      Syntax.check_supported t.syntax (ver_loc, ver);
      { extension = t
      ; version = ver
      ; loc
      ; parse_args
      }

  (* Extensions that are not selected in the dune-project file are
     automatically available at their latest version.  When used, dune
     will automatically edit the dune-project file. *)
  let automatic ~project_file ~f =
    Hashtbl.foldi extensions ~init:[] ~f:(fun name ext acc ->
      if f name then
        let version = Syntax.greatest_supported_version ext.syntax in
        let parse_args p =
          let open Dsexp.Of_sexp in
          let dune_project_edited = ref false in
          parse (enter p) Univ_map.empty (List (Loc.of_pos __POS__, []))
          |> List.map ~f:(fun (name, p) ->
            (name,
             return () >>= fun () ->
             if not !dune_project_edited then begin
               dune_project_edited := true;
               Project_file_edit.append project_file
                 (Dsexp.to_string ~syntax:Dune
                    (List [ Dsexp.atom "using"
                          ; Dsexp.atom name
                          ; Dsexp.atom (Syntax.Version.to_string version)
                          ]))
             end;
             p))
        in
        { extension = ext
        ; version
        ; loc = Loc.none
        ; parse_args
        } :: acc
      else
        acc)
end

let make_parsing_context ~(lang : Lang.Instance.t) ~extensions =
  let acc = Univ_map.singleton (Syntax.key lang.syntax) lang.version in
  List.fold_left extensions ~init:acc
    ~f:(fun acc (ext : Extension.instance) ->
      Univ_map.add acc (Syntax.key ext.extension.syntax) ext.version)

let key =
  Univ_map.Key.create ~name:"dune-project"
    (fun { name; root; version; project_file; kind
         ; stanza_parser = _; packages = _ } ->
      Sexp.To_sexp.record
        [ "name", Name.to_sexp name
        ; "root", Path.Local.to_sexp root
        ; "version", Sexp.To_sexp.(option string) version
        ; "project_file", Project_file.to_sexp project_file
        ; "kind", Kind.to_sexp kind
        ])

let set t = Dsexp.Of_sexp.set key t
let get_exn () =
  let open Dsexp.Of_sexp in
  get key >>| function
  | Some t -> t
  | None ->
    Exn.code_error "Current project is unset" []

let filename = "dune-project"

let get_local_path p =
  match Path.kind p with
  | External _ -> assert false
  | Local    p -> p

let anonymous = lazy (
  let lang = Lang.get_exn "dune" in
  let parsing_context = make_parsing_context ~lang ~extensions:[] in
  { kind          = Dune
  ; name          = Name.anonymous_root
  ; packages      = Package.Name.Map.empty
  ; root          = get_local_path Path.root
  ; version       = None
  ; stanza_parser =
      Dsexp.Of_sexp.(set_many parsing_context (sum lang.data))
  ; project_file  = { file = Path.relative Path.root filename; exists = false }
  })

let default_name ~dir ~packages =
  match Package.Name.Map.choose packages with
  | None -> Option.value_exn (Name.anonymous dir)
  | Some (_, pkg) ->
    let pkg =
      let open Package.Name.Infix in
      Package.Name.Map.fold packages ~init:pkg ~f:(fun pkg acc ->
        if acc.Package.name <= pkg.Package.name then
          acc
        else
          pkg)
    in
    let name = Package.Name.to_string pkg.name in
    match Name.named name with
    | Some x -> x
    | None ->
      Errors.fail (Loc.in_file (Path.to_string (Package.opam_file pkg)))
        "%S is not a valid opam package name."
        name

let name_field ~dir ~packages =
    let%map name = field_o "name" Name.dparse in
    match name with
    | Some x -> x
    | None   -> default_name ~dir ~packages

let parse ~dir ~lang ~packages ~file =
  fields
    (let%map name = name_field ~dir ~packages
     and version = field_o "version" string
     and extensions =
       multi_field "using"
         (let%map loc = loc
          and name = located string
          and ver = located Syntax.Version.dparse
          and parse_args = capture
          in
          (* We don't parse the arguments quite yet as we want to set
             the version of extensions before parsing them. *)
          Extension.instantiate ~loc ~parse_args name ver)
     in
     match
       String.Map.of_list
         (List.map extensions ~f:(fun (e : Extension.instance) ->
            (Syntax.name e.extension.syntax, e.loc)))
     with
     | Error (name, _, loc) ->
       Errors.fail loc "Extension %S specified for the second time." name
     | Ok map ->
       let project_file : Project_file.t = { file; exists = true } in
       let extensions =
         extensions @
         Extension.automatic ~project_file
           ~f:(fun name -> not (String.Map.mem map name))
       in
       let parsing_context = make_parsing_context ~lang ~extensions in
       let stanzas =
         List.concat
           (lang.data ::
            List.map extensions ~f:(fun (ext : Extension.instance) ->
              ext.parse_args
                (Dsexp.Of_sexp.set_many parsing_context ext.extension.stanzas)))
       in
       { kind = Dune
       ; name
       ; root = get_local_path dir
       ; version
       ; packages
       ; stanza_parser = Dsexp.Of_sexp.(set_many parsing_context (sum stanzas))
       ; project_file
       })

let load_dune_project ~dir packages =
  let file = Path.relative dir filename in
  load file ~f:(fun lang -> parse ~dir ~lang ~packages ~file)

let make_jbuilder_project ~dir packages =
  let lang = Lang.get_exn "dune" in
  let parsing_context = make_parsing_context ~lang ~extensions:[] in
  { kind = Jbuilder
  ; name = default_name ~dir ~packages
  ; root = get_local_path dir
  ; version = None
  ; packages
  ; stanza_parser =
      Dsexp.Of_sexp.(set_many parsing_context (sum lang.data))
  ; project_file = { file = Path.relative dir filename; exists = false }
  }

let read_name file =
  load file ~f:(fun _lang ->
    fields
      (let%map name = field_o "name" string
       and () = junk_everything
       in
       name))

let load ~dir ~files =
  let packages =
    String.Set.fold files ~init:[] ~f:(fun fn acc ->
      match Filename.split_extension fn with
      | (pkg, ".opam") when pkg <> "" ->
        let version_from_opam_file =
          let opam = Opam_file.load (Path.relative dir fn) in
          match Opam_file.get_field opam "version" with
          | Some (String (_, s)) -> Some s
          | _ -> None
        in
        let name = Package.Name.of_string pkg in
        (name,
         { Package.
           name
         ; path = dir
         ; version_from_opam_file
         }) :: acc
      | _ -> acc)
    |> Package.Name.Map.of_list_exn
  in
  if String.Set.mem files filename then
    Some (load_dune_project ~dir packages)
  else if not (Package.Name.Map.is_empty packages) then
    Some (make_jbuilder_project ~dir packages)
  else
    None
