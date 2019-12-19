open! Stdune

let opam_ext = ".opam"

let is_opam_file path = String.is_suffix (Path.to_string path) ~suffix:opam_ext

module Name = struct
  module T =
    Interned.Make
      (struct
        let initial_size = 16

        let resize_policy = Interned.Conservative

        let order = Interned.Natural
      end)
      ()

  include T

  let of_string_opt s =
    (* TODO verify no dots or spaces *)
    if s = "" then
      None
    else
      Some (make s)

  let of_string pkg =
    match of_string_opt pkg with
    | Some p -> p
    | None ->
      Code_error.raise "Invalid package name"
        [ ("pkg", Dyn.Encoder.string pkg) ]

  let invalid_package_name (loc, s) =
    User_error.make ~loc [ Pp.textf "%S is an invalid package name" s ]

  let of_string_user_error (loc, s) =
    match of_string_opt s with
    | Some s -> Ok s
    | None -> Error (invalid_package_name (loc, s))

  let parse_string_exn s =
    match of_string_user_error s with
    | Ok s -> s
    | Error err -> raise (User_error.E err)

  let of_basename basename =
    let open Option.O in
    let* name = String.drop_suffix basename ~suffix:opam_ext in
    of_string_opt name

  let opam_fn (t : t) = to_string t ^ opam_ext

  let meta_fn (t : t) = "META." ^ to_string t

  let version_fn (t : t) = to_string t ^ ".version"

  let pp fmt t = Format.pp_print_string fmt (to_string t)

  let decode =
    let open Dune_lang.Decoder in
    map_validate (located string) ~f:of_string_user_error

  let encode t = Dune_lang.Encoder.(string (to_string t))

  let to_dyn t = Dyn.Encoder.string (to_string t)

  module Infix = Comparator.Operators (T)
end

module Dependency = struct
  module Op = struct
    type t =
      | Eq
      | Gte
      | Lte
      | Gt
      | Lt
      | Neq

    let map =
      [ ("=", Eq); (">=", Gte); ("<=", Lte); (">", Gt); ("<", Lt); ("<>", Neq) ]

    let to_dyn =
      let open Dyn.Encoder in
      function
      | Eq -> string "Eq"
      | Gt -> string "Gt"
      | Gte -> string "Gte"
      | Lte -> string "Lte"
      | Lt -> string "Lt"
      | Neq -> string "Neq"

    let to_relop : t -> OpamParserTypes.relop = function
      | Eq -> `Eq
      | Gte -> `Geq
      | Lte -> `Leq
      | Gt -> `Gt
      | Lt -> `Lt
      | Neq -> `Neq
  end

  module Constraint = struct
    module Var = struct
      type t =
        | QVar of string
        | Var of string

      let decode =
        let open Dune_lang.Decoder in
        let+ s = string in
        if String.is_prefix s ~prefix:":" then
          Var (String.drop s 1)
        else
          QVar s

      let to_opam : t -> OpamParserTypes.value =
        let nopos = Opam_file.nopos in
        function
        | QVar x -> String (nopos, x)
        | Var x -> Ident (nopos, x)
    end

    type t =
      | Bvar of Var.t
      | Uop of Op.t * Var.t
      | And of t list
      | Or of t list

    let decode =
      let open Dune_lang.Decoder in
      let ops =
        List.map Op.map ~f:(fun (name, op) ->
            ( name
            , let+ x = Var.decode in
              Uop (op, x) ))
      in
      let ops =
        ( "!="
        , let+ loc = loc in
          User_error.raise ~loc [ Pp.text "Use <> instead of !=" ] )
        :: ops
      in
      fix (fun t ->
          let logops =
            [ ( "and"
              , let+ x = repeat t in
                And x )
            ; ( "or"
              , let+ x = repeat t in
                Or x )
            ]
          in
          peek_exn
          >>= function
          | Atom (_loc, A s) when String.is_prefix s ~prefix:":" ->
            let+ () = junk in
            Bvar (Var (String.drop s 1))
          | _ -> sum (ops @ logops))

    let rec to_dyn =
      let open Dyn.Encoder in
      function
      | Bvar (QVar v) -> constr "Bvar" [ Dyn.String v ]
      | Bvar (Var v) -> constr "Bvar" [ Dyn.String (":" ^ v) ]
      | Uop (b, QVar v) -> constr "Uop" [ Op.to_dyn b; Dyn.String v ]
      | Uop (b, Var v) -> constr "Uop" [ Op.to_dyn b; Dyn.String (":" ^ v) ]
      | And t -> constr "And" (List.map ~f:to_dyn t)
      | Or t -> constr "Or" (List.map ~f:to_dyn t)
  end

  type t =
    { name : Name.t
    ; constraint_ : Constraint.t option
    }

  let decode =
    let open Dune_lang.Decoder in
    let constrained =
      let+ name = Name.decode
      and+ expr = Constraint.decode in
      { name; constraint_ = Some expr }
    in
    if_list ~then_:(enter constrained)
      ~else_:
        (let+ name = Name.decode in
         { name; constraint_ = None })

  let rec opam_constraint : Constraint.t -> OpamParserTypes.value =
    let nopos = Opam_file.nopos in
    function
    | Bvar v -> Constraint.Var.to_opam v
    | Uop (op, v) ->
      Prefix_relop (nopos, Op.to_relop op, Constraint.Var.to_opam v)
    | And [ c ] -> opam_constraint c
    | And (c :: cs) ->
      Logop (nopos, `And, opam_constraint c, opam_constraint (And cs))
    | Or [ c ] -> opam_constraint c
    | Or (c :: cs) ->
      Logop (nopos, `Or, opam_constraint c, opam_constraint (And cs))
    | And []
    | Or [] ->
      Code_error.raise "opam_constraint" []

  let opam_depend : t -> OpamParserTypes.value =
    let nopos = Opam_file.nopos in
    fun { name; constraint_ } ->
      let constraint_ = Option.map ~f:opam_constraint constraint_ in
      let pkg : OpamParserTypes.value = String (nopos, Name.to_string name) in
      match constraint_ with
      | None -> pkg
      | Some c -> Option (nopos, pkg, [ c ])

  let to_dyn { name; constraint_ } =
    let open Dyn.Encoder in
    record
      [ ("name", Name.to_dyn name)
      ; ("constr", Dyn.Option (Option.map ~f:Constraint.to_dyn constraint_))
      ]
end

module Kind = struct
  type has_opam = bool

  type t =
    | Dune of has_opam
    | Opam

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Dune b -> constr "Dune" [ bool b ]
    | Opam -> constr "Opam" []
end

module Source_kind = struct
  type t =
    | Github of string * string
    | Url of string

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Github (user, repo) -> constr "Github" [ string user; string repo ]
    | Url url -> constr "Url" [ string url ]

  let pp fmt = function
    | Github (user, repo) ->
      Format.fprintf fmt "git+https://github.com/%s/%s.git" user repo
    | Url u -> Format.pp_print_string fmt u

  let decode =
    let open Dune_lang.Decoder in
    sum
      [ ( "github"
        , plain_string (fun ~loc s ->
              match String.split ~on:'/' s with
              | [ user; repo ] -> Github (user, repo)
              | _ ->
                User_error.raise ~loc
                  [ Pp.textf "GitHub repository must be of form user/repo" ])
        )
      ; ("uri", string >>| fun s -> Url s)
      ]
end

module Info = struct
  type t =
    { source : Source_kind.t option
    ; license : string option
    ; authors : string list option
    ; homepage : string option
    ; bug_reports : string option
    ; documentation : string option
    ; maintainers : string list option
    }

  let source t = t.source

  let license t = t.license

  let authors t = t.authors

  let homepage t =
    match (t.homepage, t.source) with
    | None, Some (Github (user, repo)) ->
      Some (sprintf "https://github.com/%s/%s" user repo)
    | s, _ -> s

  let bug_reports t =
    match (t.bug_reports, t.source) with
    | None, Some (Github (user, repo)) ->
      Some (sprintf "https://github.com/%s/%s/issues" user repo)
    | s, _ -> s

  let documentation t = t.documentation

  let maintainers t = t.maintainers

  let empty =
    { source = None
    ; license = None
    ; authors = None
    ; homepage = None
    ; bug_reports = None
    ; documentation = None
    ; maintainers = None
    }

  let to_dyn
      { source
      ; license
      ; authors
      ; homepage
      ; bug_reports
      ; documentation
      ; maintainers
      } =
    let open Dyn.Encoder in
    record
      [ ("source", (option Source_kind.to_dyn) source)
      ; ("license", (option string) license)
      ; ("homepage", (option string) homepage)
      ; ("documentation", (option string) documentation)
      ; ("bug_reports", (option string) bug_reports)
      ; ("maintainers", option (list string) maintainers)
      ; ("authors", option (list string) authors)
      ]

  let decode ?since () =
    let open Dune_lang.Decoder in
    let v default = Option.value since ~default in
    let+ source =
      field_o "source"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 7)) >>> Source_kind.decode)
    and+ authors =
      field_o "authors"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 9)) >>> repeat string)
    and+ license =
      field_o "license"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 9)) >>> string)
    and+ homepage =
      field_o "homepage"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> string)
    and+ documentation =
      field_o "documentation"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> string)
    and+ bug_reports =
      field_o "bug_reports"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> string)
    and+ maintainers =
      field_o "maintainers"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> repeat string)
    in
    { source
    ; authors
    ; license
    ; homepage
    ; documentation
    ; bug_reports
    ; maintainers
    }

  let superpose t1 t2 =
    let f o1 o2 =
      match o2 with
      | Some _ as x -> x
      | None -> o1
    in
    { source = f t1.source t2.source
    ; authors = f t1.authors t2.authors
    ; license = f t1.license t2.license
    ; homepage = f t1.homepage t2.homepage
    ; documentation = f t1.documentation t2.documentation
    ; bug_reports = f t1.bug_reports t2.bug_reports
    ; maintainers = f t1.maintainers t2.maintainers
    }
end

type t =
  { name : Name.t
  ; loc : Loc.t
  ; synopsis : string option
  ; description : string option
  ; depends : Dependency.t list
  ; conflicts : Dependency.t list
  ; depopts : Dependency.t list
  ; info : Info.t
  ; path : Path.Source.t
  ; version : string option
  ; kind : Kind.t
  ; tags : string list
  ; deprecated_package_names : Loc.t Name.Map.t
  }

(* Package name are globally unique, so we can reasonably expect that there
   will always be only a single value of type [t] with a given name in memory.
   That's why we only hash the name. *)
let hash t = Name.hash t.name

let decode ~dir =
  let open Dune_lang.Decoder in
  fields
  @@ let+ loc = loc
     and+ name = field "name" Name.decode
     and+ synopsis = field_o "synopsis" string
     and+ description = field_o "description" string
     and+ depends = field ~default:[] "depends" (repeat Dependency.decode)
     and+ conflicts = field ~default:[] "conflicts" (repeat Dependency.decode)
     and+ depopts = field ~default:[] "depopts" (repeat Dependency.decode)
     and+ info = Info.decode ~since:(2, 0) ()
     and+ tags = field "tags" (enter (repeat string)) ~default:[]
     and+ deprecated_package_names =
       field ~default:[] "deprecated_package_names"
         ( Dune_lang.Syntax.since Stanza.syntax (2, 0)
         >>> repeat (located Name.decode) )
     in
     let deprecated_package_names =
       match
         Name.Map.of_list_map deprecated_package_names ~f:(fun (loc, s) ->
             (s, loc))
       with
       | Ok x -> x
       | Error (name, (loc1, _), (loc2, _)) ->
         User_error.raise
           [ Pp.textf "Deprecated package name %s is declared twice:"
               (Name.to_string name)
           ; Pp.textf "- %s" (Loc.to_file_colon_line loc1)
           ; Pp.textf "- %s" (Loc.to_file_colon_line loc2)
           ]
     in
     { name
     ; loc
     ; synopsis
     ; description
     ; depends
     ; conflicts
     ; depopts
     ; info
     ; path = dir
     ; version = None
     ; kind = Dune false
     ; tags
     ; deprecated_package_names
     }

let to_dyn
    { name
    ; path
    ; version
    ; synopsis
    ; description
    ; depends
    ; conflicts
    ; depopts
    ; info
    ; kind
    ; tags
    ; loc = _
    ; deprecated_package_names
    } =
  let open Dyn.Encoder in
  record
    [ ("name", Name.to_dyn name)
    ; ("path", Path.Source.to_dyn path)
    ; ("synopsis", option string synopsis)
    ; ("description", option string description)
    ; ("depends", list Dependency.to_dyn depends)
    ; ("conflicts", list Dependency.to_dyn conflicts)
    ; ("depopts", list Dependency.to_dyn depopts)
    ; ("info", Info.to_dyn info)
    ; ("kind", Kind.to_dyn kind)
    ; ("tags", list string tags)
    ; ("version", option string version)
    ; ( "deprecated_package_names"
      , Name.Map.to_dyn Loc.to_dyn deprecated_package_names )
    ]

let opam_file t = Path.Source.relative t.path (Name.opam_fn t.name)

let meta_file t = Path.Source.relative t.path (Name.meta_fn t.name)

let file ~dir ~name = Path.relative dir (Name.to_string name ^ opam_ext)

let deprecated_meta_file t name =
  Path.Source.relative t.path (Name.meta_fn name)
