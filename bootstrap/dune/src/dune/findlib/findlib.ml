open! Stdune
open Import
module Opam_package = Package
module P = Variant
module Ps = Variant.Set

let meta_fn = "META"

(* An assignment or addition *)
module Rule = struct
  type t =
    { preds_required : Ps.t
    ; preds_forbidden : Ps.t
    ; value : string
    }

  let to_dyn { preds_required; preds_forbidden; value } =
    let open Dyn.Encoder in
    record
      [ ("preds_required", Ps.to_dyn preds_required)
      ; ("preds_forbidden", Ps.to_dyn preds_forbidden)
      ; ("value", string value)
      ]

  let formal_predicates_count t =
    Ps.cardinal t.preds_required + Ps.cardinal t.preds_forbidden

  let matches t ~preds =
    Ps.is_subset t.preds_required ~of_:preds
    && Ps.is_empty (Ps.inter preds t.preds_forbidden)

  let make (rule : Meta.rule) =
    let preds_required, preds_forbidden =
      List.partition_map rule.predicates ~f:(function
        | Pos x -> Left x
        | Neg x -> Right x)
    in
    { preds_required = Ps.make preds_required
    ; preds_forbidden = Ps.make preds_forbidden
    ; value = rule.value
    }
end

(* Set of rules for a given variable of a package. Implements the algorithm
   described here:

   http://projects.camlcity.org/projects/dl/findlib-1.6.3/doc/ref-html/r729.html *)
module Rules = struct
  (* To implement the algorithm, [set_rules] is sorted by decreasing number of
     formal predicates, then according to the order of the META file.
     [add_rules] are in the same order as in the META file. *)
  type t =
    { set_rules : Rule.t list
    ; add_rules : Rule.t list
    }

  let to_dyn { set_rules; add_rules } =
    let open Dyn.Encoder in
    record
      [ ("set_rules", list Rule.to_dyn set_rules)
      ; ("add_rules", list Rule.to_dyn add_rules)
      ]

  let interpret t ~preds =
    let rec find_set_rule = function
      | [] -> None
      | rule :: rules ->
        if Rule.matches rule ~preds then
          Some rule.value
        else
          find_set_rule rules
    in
    let v = find_set_rule t.set_rules in
    List.fold_left t.add_rules ~init:v ~f:(fun v rule ->
        if Rule.matches rule ~preds then
          Some (Option.value ~default:"" v ^ " " ^ rule.value)
        else
          v)

  let of_meta_rules (rules : Meta.Simplified.Rules.t) =
    let add_rules = List.map rules.add_rules ~f:Rule.make in
    let set_rules =
      List.map rules.set_rules ~f:Rule.make
      |> List.stable_sort ~compare:(fun a b ->
             compare
               (Rule.formal_predicates_count b)
               (Rule.formal_predicates_count a))
    in
    { add_rules; set_rules }
end

module Vars = struct
  type t = Rules.t String.Map.t

  let get (t : t) var preds =
    Option.map (String.Map.find t var) ~f:(fun r ->
        Option.value ~default:"" (Rules.interpret r ~preds))

  let get_words t var preds =
    match get t var preds with
    | None -> []
    | Some s -> String.extract_comma_space_separated_words s
end

module Config = struct
  type t =
    { vars : Vars.t
    ; preds : Ps.t
    }

  let to_dyn { vars; preds } =
    let open Dyn.Encoder in
    record
      [ ("vars", String.Map.to_dyn Rules.to_dyn vars)
      ; ("preds", Ps.to_dyn preds)
      ]

  let load path ~toolchain ~context =
    let path = Path.extend_basename path ~suffix:".d" in
    let conf_file = Path.relative path (toolchain ^ ".conf") in
    if not (Path.exists conf_file) then
      User_error.raise
        [ Pp.textf "ocamlfind toolchain %s isn't defined in %s (context: %s)"
            toolchain
            (Path.to_string_maybe_quoted path)
            context
        ];
    let vars = (Meta.load ~name:None conf_file).vars in
    { vars = String.Map.map vars ~f:Rules.of_meta_rules
    ; preds = Ps.make [ toolchain ]
    }

  let get { vars; preds } var = Vars.get vars var preds

  let env t =
    let preds = Ps.add t.preds (P.make "env") in
    String.Map.filter_map ~f:(Rules.interpret ~preds) t.vars
    |> Env.of_string_map
end

module Unavailable_reason = struct
  type t =
    | Not_found
    | Hidden of Dune_package.Lib.t

  let to_string = function
    | Not_found -> "not found"
    | Hidden pkg ->
      let info = Dune_package.Lib.info pkg in
      let obj_dir = Lib_info.obj_dir info in
      let dir = Obj_dir.dir obj_dir in
      sprintf "in %s is hidden (unsatisfied 'exist_if')"
        (Path.to_string_maybe_quoted dir)

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Not_found -> constr "Not_found" []
    | Hidden lib ->
      let info = Dune_package.Lib.info lib in
      let obj_dir = Lib_info.obj_dir info in
      let dir = Obj_dir.dir obj_dir in
      constr "Hidden" [ Path.to_dyn dir ]
end

type t =
  { stdlib_dir : Path.t
  ; paths : Path.t list
  ; builtins : Meta.Simplified.t Lib_name.Map.t
  ; packages :
      (Lib_name.t, (Dune_package.Entry.t, Unavailable_reason.t) result) Table.t
  }

module Package = struct
  type t =
    { meta_file : Path.t
    ; name : Lib_name.t
    ; dir : Path.t
    ; vars : Vars.t
    }

  let loc t = Loc.in_dir t.meta_file

  let name t = t.name

  let preds = Ps.of_list [ P.ppx_driver; P.mt; P.mt_posix ]

  let get_paths t var preds =
    List.map (Vars.get_words t.vars var preds) ~f:(Path.relative t.dir)

  let make_archives t var preds =
    Mode.Dict.of_func (fun ~mode ->
        get_paths t var (Ps.add preds (Mode.variant mode)))

  let version t = Vars.get t.vars "version" Ps.empty

  let description t = Vars.get t.vars "description" Ps.empty

  let jsoo_runtime t = get_paths t "jsoo_runtime" Ps.empty

  let requires t =
    Vars.get_words t.vars "requires" preds
    |> List.map ~f:(Lib_name.of_string_exn ~loc:None)

  let ppx_runtime_deps t =
    Vars.get_words t.vars "ppx_runtime_deps" preds
    |> List.map ~f:(Lib_name.of_string_exn ~loc:None)

  let archives t = make_archives t "archive" preds

  let plugins t =
    Mode.Dict.map2 ~f:( @ )
      (make_archives t "archive" (Ps.add preds Variant.plugin))
      (make_archives t "plugin" preds)

  let dune_file t =
    let fn =
      Path.relative t.dir (sprintf "%s.dune" (Lib_name.to_string t.name))
    in
    Option.some_if (Path.exists fn) fn

  let to_dune t =
    let loc = loc t in
    let add_loc x = (loc, x) in
    let () =
      dune_file t
      |> Option.iter ~f:(fun p ->
             User_warning.emit ~loc:(Loc.in_file p)
               [ Pp.text
                   ".dune files are ignored since 2.0. Reinstall the library \
                    with dune >= 2.0 to get rid of this warning and enable \
                    support for the subsystem this library provides."
               ])
    in
    let archives = archives t in
    let obj_dir = Obj_dir.make_external_no_private ~dir:t.dir in
    let modes : Mode.Dict.Set.t =
      (* libraries without archives are compatible with all modes. mainly a
         hack for compiler-libs which doesn't have any archives *)
      let discovered = Mode.Dict.map ~f:List.is_non_empty archives in
      if Mode.Dict.Set.is_empty discovered then
        Mode.Dict.Set.all
      else
        discovered
    in
    let info : Path.t Lib_info.t =
      let name = name t in
      let kind = Lib_kind.Normal in
      let sub_systems = Sub_system_name.Map.empty in
      let synopsis = description t in
      let status = Lib_info.Status.Installed in
      let src_dir = Obj_dir.dir obj_dir in
      let version = version t in
      let dune_version = None in
      let virtual_deps = [] in
      let implements = None in
      let orig_src_dir = None in
      let main_module_name : Lib_info.Main_module_name.t = This None in
      let enabled = Lib_info.Enabled_status.Normal in
      let requires =
        requires t |> List.map ~f:(fun name -> Lib_dep.direct (add_loc name))
      in
      let ppx_runtime_deps = List.map ~f:add_loc (ppx_runtime_deps t) in
      let special_builtin_support : Lib_info.Special_builtin_support.t option =
        (* findlib has been around for much longer than dune, so it is
           acceptable to have a special case in dune for findlib. *)
        match Lib_name.to_string t.name with
        | "findlib.dynload" -> Some Findlib_dynload
        | _ -> None
      in
      let foreign_objects = Lib_info.Source.External [] in
      let plugins = plugins t in
      let jsoo_runtime = jsoo_runtime t in
      let jsoo_archive = None in
      let pps = [] in
      let virtual_ = None in
      let variant = None in
      let known_implementations = P.Map.empty in
      let default_implementation = None in
      let wrapped = None in
      Lib_info.create ~loc ~name ~kind ~status ~src_dir ~orig_src_dir ~obj_dir
        ~version ~synopsis ~main_module_name ~sub_systems ~requires
        ~foreign_objects ~plugins ~archives ~ppx_runtime_deps
        ~foreign_archives:[] ~native_archives:[] ~foreign_dll_files:[]
        ~jsoo_runtime ~jsoo_archive ~pps ~enabled ~virtual_deps ~dune_version
        ~virtual_ ~implements ~variant ~known_implementations
        ~default_implementation ~modes ~wrapped ~special_builtin_support
        ~exit_module:None
    in
    Dune_package.Lib.make ~info ~modules:None ~main_module_name:None

  (* XXX remove *)

  let parse db ~meta_file ~name ~parent_dir ~vars =
    let pkg_dir = Vars.get vars "directory" Ps.empty in
    let dir =
      match pkg_dir with
      | None
      | Some "" ->
        parent_dir
      | Some pkg_dir ->
        if pkg_dir.[0] = '+' || pkg_dir.[0] = '^' then
          Path.relative db.stdlib_dir (String.drop pkg_dir 1)
        else if Filename.is_relative pkg_dir then
          Path.relative parent_dir pkg_dir
        else
          Path.of_filename_relative_to_initial_cwd pkg_dir
    in
    let pkg = { meta_file; name; dir; vars } in
    let exists_if = Vars.get_words vars "exists_if" Ps.empty in
    let exists =
      match exists_if with
      | _ :: _ ->
        List.for_all exists_if ~f:(fun fn ->
            Path.exists (Path.relative dir fn))
      | [] -> (
        if not (Lib_name.Map.mem db.builtins (Lib_name.root_lib name)) then
          true
        else
          (* The META files for installed packages are sometimes broken, i.e.
             META files for libraries that were not installed by the compiler
             are still present:

             https://github.com/ocaml/dune/issues/563

             To workaround this problem, for builtin packages we check that at
             least one of the archive is present. *)
          match archives pkg with
          | { byte = []; native = [] } -> true
          | { byte; native } -> List.exists (byte @ native) ~f:Path.exists )
    in
    if exists then
      Ok pkg
    else
      Error pkg
end

let paths t = t.paths

let dummy_package t ~name =
  let dir =
    match t.paths with
    | [] -> t.stdlib_dir
    | dir :: _ ->
      Lib_name.package_name name |> Opam_package.Name.to_string
      |> Path.relative dir
  in
  { Package.meta_file = Path.relative dir meta_fn
  ; name
  ; dir
  ; vars = String.Map.empty
  }
  |> Package.to_dune

type db = t

module Meta_source : sig
  type t = private
    { dir : Path.t
    ; meta_file : Path.t
    ; meta : Meta.Simplified.t
    }

  val internal : db -> meta:Meta.Simplified.t -> t

  val parse_and_acknowledge : t -> db -> unit

  val discover : dir:Path.t -> name:Lib_name.t -> t option
end = struct
  type t =
    { dir : Path.t
    ; meta_file : Path.t
    ; meta : Meta.Simplified.t
    }

  let create ~dir ~meta_file ~name =
    let meta = Meta.load meta_file ~name:(Some name) in
    { dir; meta; meta_file }

  let internal db ~meta =
    { dir = db.stdlib_dir; meta_file = Path.of_string "<internal>"; meta }

  let discover ~dir ~name =
    let meta_file = Path.relative dir meta_fn in
    if Path.exists meta_file then
      Some (create ~dir ~meta_file ~name)
    else
      (* Alternative layout *)
      let open Option.O in
      let* dir = Path.parent dir in
      let meta_file =
        Path.relative dir (meta_fn ^ "." ^ Lib_name.to_string name)
      in
      if Path.exists meta_file then
        Some (create ~dir ~meta_file ~name)
      else
        None

  (* Parse a single package from a META file *)
  let parse_package t ~meta_file ~name ~parent_dir ~vars =
    match Package.parse t ~meta_file ~name ~parent_dir ~vars with
    | Ok pkg -> (pkg.dir, Ok (Dune_package.Entry.Library (Package.to_dune pkg)))
    | Error pkg ->
      (pkg.dir, Error (Unavailable_reason.Hidden (Package.to_dune pkg)))

  (* Parse all the packages defined in a META file and add them to [t.packages] *)
  let parse_and_acknowledge { dir; meta_file; meta } db =
    let rec loop ~dir ~full_name (meta : Meta.Simplified.t) =
      let vars = String.Map.map meta.vars ~f:Rules.of_meta_rules in
      let dir, res =
        parse_package db ~meta_file ~name:full_name ~parent_dir:dir ~vars
      in
      Table.set db.packages full_name res;
      List.iter meta.subs ~f:(fun (meta : Meta.Simplified.t) ->
          let full_name =
            match meta.name with
            | None -> full_name
            | Some name -> Lib_name.nest full_name name
          in
          loop ~dir ~full_name meta)
    in
    loop ~dir ~full_name:(Option.value_exn meta.name) meta
end

module Discovered_package = struct
  type t =
    | Dune of Dune_package.t
    | Findlib of Meta_source.t

  let builtin_for_dune =
    Dune
      { name = Opam_package.Name.of_string "dune"
      ; entries =
          [ Deprecated_library_name
              { loc = Loc.of_pos __POS__
              ; old_public_name =
                  Lib_name.of_string_exn "dune.configurator" ~loc:None
              ; new_public_name =
                  Lib_name.of_string_exn "dune-configurator" ~loc:None
              }
          ]
      ; version = None
      ; dir = Path.root
      }
end

(* Search for a <package>/{META,dune-package} file in the findlib search path,
   parse it and add its contents to [t.packages] *)
let find_and_acknowledge_package t ~fq_name =
  let root_name = Lib_name.root_lib fq_name in
  let rec loop dirs : Discovered_package.t option =
    match dirs with
    | [] -> (
      match Lib_name.to_string root_name with
      | "dune" -> Some Discovered_package.builtin_for_dune
      | _ ->
        Lib_name.Map.find t.builtins root_name
        |> Option.map ~f:(fun meta ->
               Discovered_package.Findlib (Meta_source.internal t ~meta)) )
    | dir :: dirs -> (
      let dir = Path.relative dir (Lib_name.to_string root_name) in
      let dune = Path.relative dir Dune_package.fn in
      match
        if Path.exists dune then
          Dune_package.Or_meta.load dune
        else
          Dune_package.Or_meta.Use_meta
      with
      | Dune_package p -> Some (Dune p)
      | Use_meta -> (
        match Meta_source.discover ~dir ~name:root_name with
        | None -> loop dirs
        | Some meta_source -> Some (Findlib meta_source) ) )
  in
  match loop t.paths with
  | None -> Table.set t.packages root_name (Error Not_found)
  | Some (Findlib findlib_package) ->
    Meta_source.parse_and_acknowledge findlib_package t
  | Some (Dune pkg) ->
    List.iter pkg.entries ~f:(fun entry ->
        let name = Dune_package.Entry.name entry in
        Table.set t.packages name (Ok entry))

let find t name =
  match Table.find t.packages name with
  | Some x -> x
  | None -> (
    find_and_acknowledge_package t ~fq_name:name;
    match Table.find t.packages name with
    | Some x -> x
    | None ->
      let res = Error Unavailable_reason.Not_found in
      Table.set t.packages name res;
      res )

let available t name = Result.is_ok (find t name)

let root_packages t =
  let pkgs =
    List.concat_map t.paths ~f:(fun dir ->
        match Path.readdir_unsorted dir with
        | Error ENOENT -> []
        | Error unix_error ->
          User_error.raise
            [ Pp.textf "Unable to read directory %s for findlib package"
                (Path.to_string_maybe_quoted dir)
            ; Pp.textf "Reason: %s" (Unix.error_message unix_error)
            ]
        | Ok listing ->
          List.filter_map listing ~f:(fun name ->
              if Path.exists (Path.relative dir (name ^ "/" ^ meta_fn)) then
                Some (Lib_name.of_string_exn ~loc:None name)
              else
                None))
    |> Lib_name.Set.of_list
  in
  let builtins = Lib_name.Set.of_list (Lib_name.Map.keys t.builtins) in
  Lib_name.Set.union pkgs builtins

let load_all_packages t =
  Lib_name.Set.iter (root_packages t) ~f:(fun pkg ->
      find_and_acknowledge_package t ~fq_name:pkg)

let all_packages t =
  load_all_packages t;
  Table.fold t.packages ~init:[] ~f:(fun x acc ->
      match x with
      | Ok p -> p :: acc
      | Error _ -> acc)
  |> List.sort ~compare:(fun a b ->
         Lib_name.compare
           (Dune_package.Entry.name a)
           (Dune_package.Entry.name b))

let create ~stdlib_dir ~paths ~version =
  { stdlib_dir
  ; paths
  ; builtins = Meta.builtins ~stdlib_dir ~version
  ; packages = Table.create (module Lib_name) 1024
  }

let all_unavailable_packages t =
  load_all_packages t;
  Table.foldi t.packages ~init:[] ~f:(fun name x acc ->
      match x with
      | Ok _ -> acc
      | Error e -> (name, e) :: acc)
  |> List.sort ~compare:(fun (a, _) (b, _) -> Lib_name.compare a b)
