open! Stdune
open Import
module Menhir_rules = Menhir
open Dune_file
open! No_io

module Modules_field_evaluator : sig
  type t = private
    { all_modules : Module.Name_map.t
    ; virtual_modules : Module.Name_map.t
    }

  val eval
    :  modules:Module.Name_map.t
    -> buildable:Buildable.t
    -> virtual_modules:Ordered_set_lang.t option
    -> private_modules:Ordered_set_lang.t
    -> t
end = struct
  type t =
    { all_modules : Module.Name_map.t
    ; virtual_modules : Module.Name_map.t
    }

  let eval =
    let module Value = struct
      type t = (Module.t, Module.Name.t) result

      type key = Module.Name.t

      let key = function
        | Error s -> s
        | Ok m -> Module.name m
    end in
    let module Eval = Ordered_set_lang.Make_loc(Module.Name)(Value) in
    let parse ~all_modules ~fake_modules ~loc s =
      let name = Module.Name.of_string s in
      match Module.Name.Map.find all_modules name with
      | Some m -> Ok m
      | None ->
        fake_modules := Module.Name.Map.add !fake_modules name loc;
        Error name
    in
    fun ~all_modules ~standard osl ->
      let fake_modules = ref Module.Name.Map.empty in
      let parse = parse ~fake_modules ~all_modules in
      let standard = Module.Name.Map.map standard ~f:(fun m -> Ok m) in
      let modules = Eval.eval_unordered ~parse ~standard osl in
      ( !fake_modules
      , Module.Name.Map.filter_map modules ~f:(fun (loc, m) ->
          match m with
          | Ok m -> Some (loc, m)
          | Error s ->
            Errors.fail loc "Module %a doesn't exist." Module.Name.pp s)
      )

  module Module_errors = struct
    type t =
      { missing_modules      : (Loc.t * Module.t) list
      ; missing_intf_only    : (Loc.t * Module.t) list
      ; virt_intf_overlaps   : (Loc.t * Module.t) list
      ; private_virt_modules : (Loc.t * Module.t) list
      }

    let empty =
      { missing_modules      = []
      ; missing_intf_only    = []
      ; virt_intf_overlaps   = []
      ; private_virt_modules = []
      }

    let map { missing_modules ; missing_intf_only ; virt_intf_overlaps
            ; private_virt_modules } ~f =
      { missing_modules = f missing_modules
      ; missing_intf_only = f missing_intf_only
      ; virt_intf_overlaps = f virt_intf_overlaps
      ; private_virt_modules = f private_virt_modules
      }
  end

  let find_errors ~modules ~intf_only ~virtual_modules ~private_modules =
    let missing_modules =
      Module.Name.Map.fold intf_only ~init:[]
        ~f:(fun ((_, (module_ : Module.t)) as module_loc) acc ->
          if Option.is_none module_.impl then
            acc
          else
            module_loc :: acc)
    in
    let errors =
      Module.Name.Map.fold virtual_modules ~init:Module_errors.empty
        ~f:(fun (_, (module_ : Module.t) as module_loc) acc ->
          if Option.is_some module_.impl then
            { acc with missing_modules = module_loc :: acc.missing_modules }
          else if Module.Name.Map.mem intf_only (Module.name module_) then
            { acc with virt_intf_overlaps = module_loc :: acc.virt_intf_overlaps
            }
          else if Module.Name.Map.mem private_modules (Module.name module_) then
            { acc with private_virt_modules =
                         module_loc :: acc.private_virt_modules
            }
          else
            acc)
    in
    let missing_intf_only =
      Module.Name.Map.fold modules ~init:[]
        ~f:(fun (_, (module_ : Module.t) as module_loc) acc ->
          if Option.is_some module_.impl then
            acc
          else if not (Module.Name.Map.mem intf_only (Module.name module_))
                && not (Module.Name.Map.mem virtual_modules (Module.name module_))
          then
            module_loc :: acc
          else
            acc) in
    assert (List.is_empty errors.missing_intf_only);
    { errors with
      missing_modules = List.rev_append errors.missing_modules missing_modules
    ; missing_intf_only
    }
    |> Module_errors.map ~f:List.rev

  let check_invalid_module_listing ~(buildable : Buildable.t) ~intf_only
        ~modules ~virtual_modules ~private_modules =
    let { Module_errors.
          missing_modules
        ; missing_intf_only
        ; virt_intf_overlaps
        ; private_virt_modules
        } = find_errors ~modules ~intf_only ~virtual_modules ~private_modules
    in
    let uncapitalized =
      List.map ~f:(fun (_, m) -> Module.name m |> Module.Name.uncapitalize) in
    let line_list modules =
      List.map ~f:(fun (_, m) ->
        Module.name m |> Module.Name.to_string |> sprintf "- %s") modules
      |> String.concat ~sep:"\n"
    in
    begin match private_virt_modules with
    | [] -> ()
    | (loc, _) :: _ ->
      Errors.fail loc
        "The following modules are declared as virtual and private: \
        \n%s\nThis is not possible."
        (line_list private_virt_modules)
    end;
    begin match virt_intf_overlaps with
    | [] -> ()
    | (loc, _) :: _ ->
      Errors.fail loc
        "These modules appear in the virtual_libraries \
         and modules_without_implementation fields: \
         \n%s\nThis is not possible."
        (line_list virt_intf_overlaps)
    end;
    if missing_intf_only <> [] then begin
      match Ordered_set_lang.loc buildable.modules_without_implementation with
      | None ->
        Errors.warn buildable.loc
          "Some modules don't have an implementation.\
           \nYou need to add the following field to this stanza:\
           \n\
           \n  %s\
           \n\
           \nThis will become an error in the future."
          (let tag = Dsexp.unsafe_atom_of_string
                       "modules_without_implementation" in
           let modules =
             missing_intf_only
             |> uncapitalized
             |> List.map ~f:Dsexp.To_sexp.string
           in
           Dsexp.to_string ~syntax:Dune (List (tag :: modules)))
      | Some loc ->
        Errors.warn loc
          "The following modules must be listed here as they don't \
           have an implementation:\n\
           %s\n\
           This will become an error in the future."
          (line_list missing_intf_only)
    end;
    begin match missing_modules with
    | [] -> ()
    | (loc, module_) :: _ ->
      (* CR-soon jdimino for jdimino: report all errors *)
      Errors.fail loc
        "Module %a has an implementation, it cannot be listed here"
        Module.Name.pp (Module.name module_)
    end

  let eval ~modules:(all_modules : Module.Name_map.t)
        ~buildable:(conf : Buildable.t) ~virtual_modules
        ~private_modules =
    let (fake_modules, modules) =
      eval ~standard:all_modules ~all_modules conf.modules in
    let (fake_modules, intf_only) =
      let (fake_modules', intf_only) =
        eval ~standard:Module.Name.Map.empty ~all_modules
          conf.modules_without_implementation in
      ( Module.Name.Map.superpose fake_modules' fake_modules
      , intf_only
      )
    in
    let (fake_modules, virtual_modules) =
      match virtual_modules with
      | None -> (fake_modules, Module.Name.Map.empty)
      | Some virtual_modules ->
        let (fake_modules', virtual_modules) =
          eval ~standard:Module.Name.Map.empty ~all_modules
            virtual_modules in
        ( Module.Name.Map.superpose fake_modules' fake_modules
        , virtual_modules
        )
    in
    let (fake_modules, private_modules) =
      let (fake_modules', private_modules) =
        eval ~standard:Module.Name.Map.empty ~all_modules private_modules
      in
      ( Module.Name.Map.superpose fake_modules' fake_modules
      , private_modules
      )
    in
    Module.Name.Map.iteri fake_modules ~f:(fun m loc ->
      Errors.warn loc "Module %a is excluded but it doesn't exist."
        Module.Name.pp m
    );
    check_invalid_module_listing ~buildable:conf ~intf_only
      ~modules ~virtual_modules ~private_modules;
    let drop_locs = Module.Name.Map.map ~f:snd in
    { all_modules =
        Module.Name.Map.map modules ~f:(fun (_, m) ->
          if Module.Name.Map.mem private_modules (Module.name m) then
            Module.set_private m
          else
            m)
    ; virtual_modules = drop_locs virtual_modules
    }
end

module Executables_modules = struct
  type t = Module.Name_map.t
end

type modules =
  { libraries : Lib_modules.t Lib_name.Map.t
  ; executables : Executables_modules.t String.Map.t
  ; (* Map from modules to the buildable they are part of *)
    rev_map : Buildable.t Module.Name.Map.t
  }

let empty_modules =
  { libraries = Lib_name.Map.empty
  ; executables = String.Map.empty
  ; rev_map = Module.Name.Map.empty
  }
type t =
  { kind : kind
  ; dir : Path.t
  ; text_files : String.Set.t
  ; modules : modules Lazy.t
  ; mlds : (Dune_file.Documentation.t * Path.t list) list Lazy.t
  }

and kind =
  | Standalone
  | Group_root of t list Lazy.t
  | Group_part of t

let kind t = t.kind
let dir t = t.dir

let dirs t =
  match t.kind with
  | Standalone -> [t]
  | Group_root (lazy l)
  | Group_part { kind = Group_root (lazy l); _ } -> t :: l
  | Group_part { kind = _; _ } -> assert false

let text_files t = t.text_files

let modules_of_library t ~name =
  let map = (Lazy.force t.modules).libraries in
  match Lib_name.Map.find map name with
  | Some m -> m
  | None ->
    Exn.code_error "Dir_contents.modules_of_library"
      [ "name", Lib_name.to_sexp name
      ; "available", Sexp.To_sexp.(list Lib_name.to_sexp) (Lib_name.Map.keys map)
      ]

let modules_of_executables t ~first_exe =
  let map = (Lazy.force t.modules).executables in
  match String.Map.find map first_exe with
  | Some m -> m
  | None ->
    Exn.code_error "Dir_contents.modules_of_executables"
      [ "first_exe", Sexp.To_sexp.string first_exe
      ; "available", Sexp.To_sexp.(list string) (String.Map.keys map)
      ]

let lookup_module t name =
  Module.Name.Map.find (Lazy.force t.modules).rev_map name

let mlds t (doc : Documentation.t) =
  let map = Lazy.force t.mlds in
  match
    List.find_map map ~f:(fun (doc', x) ->
      Option.some_if (Loc.equal doc.loc doc'.loc) x)
  with
  | Some x -> x
  | None ->
    Exn.code_error "Dir_contents.mlds"
      [ "doc", Loc.to_sexp doc.loc
      ; "available", Sexp.To_sexp.(list Loc.to_sexp)
                       (List.map map ~f:(fun (d, _) -> d.Documentation.loc))
      ]

(* As a side-effect, setup user rules and copy_files rules. *)
let load_text_files sctx ft_dir d =
  let { Super_context.Dir_with_jbuild.
        ctx_dir = dir
      ; src_dir
      ; scope
      ; stanzas
      ; _
      } = d
  in
  (* Interpret a few stanzas in order to determine the list of
     files generated by the user. *)
  let generated_files =
    List.concat_map stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Menhir.T menhir ->
        Menhir_rules.targets menhir
      | Rule rule ->
        List.map (Simple_rules.user_rule sctx rule ~dir ~scope)
          ~f:Path.basename
      | Copy_files def ->
        List.map (Simple_rules.copy_files sctx def ~src_dir ~dir ~scope)
          ~f:Path.basename
      | Library { buildable; _ } | Executables { buildable; _ } ->
        (* Manually add files generated by the (select ...)
           dependencies *)
        List.filter_map buildable.libraries ~f:(fun dep ->
          match (dep : Dune_file.Lib_dep.t) with
          | Direct _ -> None
          | Select s -> Some s.result_fn)
      | _ -> [])
    |> String.Set.of_list
  in
  String.Set.union generated_files (File_tree.Dir.files ft_dir)

let modules_of_files ~dir ~files =
  let make_module syntax base fn =
    (Module.Name.of_string base,
     Module.File.make syntax (Path.relative dir fn))
  in
  let impl_files, intf_files =
    String.Set.to_list files
    |> List.filter_partition_map ~f:(fun fn ->
      (* we aren't using Filename.extension because we want to handle
         filenames such as foo.cppo.ml *)
      match String.lsplit2 fn ~on:'.' with
      | Some (s, "ml" ) -> Left  (make_module OCaml  s fn)
      | Some (s, "re" ) -> Left  (make_module Reason s fn)
      | Some (s, "mli") -> Right (make_module OCaml  s fn)
      | Some (s, "rei") -> Right (make_module Reason s fn)
      | _ -> Skip)
  in
  let parse_one_set (files : (Module.Name.t * Module.File.t) list)  =
    match Module.Name.Map.of_list files with
    | Ok x -> x
    | Error (name, f1, f2) ->
      let src_dir = Path.drop_build_context_exn dir in
      die "Too many files for module %a in %a:\
           \n- %a\
           \n- %a"
        Module.Name.pp name
        Path.pp src_dir
        Path.pp f1.path
        Path.pp f2.path
  in
  let impls = parse_one_set impl_files in
  let intfs = parse_one_set intf_files in
  Module.Name.Map.merge impls intfs ~f:(fun name impl intf ->
    Some (Module.make name ~visibility:Public ?impl ?intf))

let build_modules_map (d : Super_context.Dir_with_jbuild.t) ~scope ~modules =
  let libs, exes =
    List.filter_partition_map d.stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Library lib ->
        let { Modules_field_evaluator.
              all_modules = modules
            ; virtual_modules
            } =
          Modules_field_evaluator.eval ~modules
            ~buildable:lib.buildable
            ~virtual_modules:lib.virtual_modules
            ~private_modules:lib.private_modules
        in
        let main_module_name =
          match Library.main_module_name lib with
          | Some _ as mmn -> mmn
          | None ->
            let name = Library.best_name lib in
            let loc = fst lib.name in
            Lib.DB.resolve (Scope.libs scope) (loc, name)
            |> Result.bind ~f:Lib.main_module_name
            |> Result.ok_exn
        in
        Left ( lib
             , Lib_modules.make lib ~dir:d.ctx_dir modules ~virtual_modules
                 ~main_module_name
             )
      | Executables exes
      | Tests { exes; _} ->
        let { Modules_field_evaluator.
              all_modules = modules
            ; virtual_modules = _
            } =
          Modules_field_evaluator.eval ~modules
            ~buildable:exes.buildable
            ~virtual_modules:None
            ~private_modules:Ordered_set_lang.standard
        in
        Right (exes, modules)
      | _ -> Skip)
  in
  let libraries =
    match
      Lib_name.Map.of_list_map libs ~f:(fun (lib, m) -> Library.best_name lib, m)
    with
    | Ok x -> x
    | Error (name, _, (lib2, _)) ->
      Errors.fail lib2.buildable.loc
        "Library %a appears for the second time \
         in this directory"
        Lib_name.pp_quoted name
  in
  let executables =
    match
      String.Map.of_list_map exes
        ~f:(fun (exes, m) -> snd (List.hd exes.names), m)
    with
    | Ok x -> x
    | Error (name, _, (exes2, _)) ->
      Errors.fail exes2.buildable.loc
        "Executable %S appears for the second time \
         in this directory"
        name
  in
  let rev_map =
    let rev_modules =
      List.rev_append
        (List.concat_map libs ~f:(fun (l, m) ->
           let modules = Lib_modules.modules m in
           List.map (Module.Name.Map.values modules) ~f:(fun m ->
             (Module.name m, l.buildable))))
        (List.concat_map exes ~f:(fun (e, m) ->
           List.map (Module.Name.Map.values m) ~f:(fun m ->
             (Module.name m, e.buildable))))
    in
    match d.kind with
    | Dune -> begin
        match Module.Name.Map.of_list rev_modules with
        | Ok x -> x
        | Error (name, _, _) ->
          let open Module.Name.Infix in
          let locs =
            List.filter_map rev_modules ~f:(fun (n, b) ->
              Option.some_if (n = name) b.loc)
            |> List.sort ~compare
          in
          Errors.fail (Loc.in_file (List.hd locs).start.pos_fname)
            "Module %a is used in several stanzas:@\n\
             @[<v>%a@]@\n\
             @[%a@]"
            Module.Name.pp_quote name
            (Fmt.list (Fmt.prefix (Fmt.string "- ") Loc.pp_file_colon_line))
            locs
            Format.pp_print_text
            "To fix this error, you must specify an explicit \"modules\" \
             field in every library, executable, and executables stanzas in \
             this dune file. Note that each module cannot appear in more \
             than one \"modules\" field - it must belong to a single library \
             or executable."
      end
    | Jbuild ->
      Module.Name.Map.of_list_multi rev_modules
      |> Module.Name.Map.mapi ~f:(fun name buildables ->
        match buildables with
        | [] -> assert false
        | [b] -> b
        | b :: rest ->
          let locs =
            List.sort ~compare
              (b.Buildable.loc :: List.map rest ~f:(fun b -> b.Buildable.loc))
          in
          Errors.warn (Loc.in_file b.loc.start.pos_fname)
            "Module %a is used in several stanzas:@\n\
             @[<v>%a@]@\n\
             @[%a@]@\n\
             This warning will become an error in the future."
            Module.Name.pp_quote name
            (Fmt.list (Fmt.prefix (Fmt.string "- ") Loc.pp_file_colon_line))
            locs
            Format.pp_print_text
            "To remove this warning, you must specify an explicit \"modules\" \
             field in every library, executable, and executables stanzas in \
             this jbuild file. Note that each module cannot appear in more \
             than one \"modules\" field - it must belong to a single library \
             or executable.";
          b)
  in
  { libraries; executables; rev_map }

let build_mlds_map (d : Super_context.Dir_with_jbuild.t) ~files =
  let dir = d.ctx_dir in
  let mlds = lazy (
    String.Set.fold files ~init:String.Map.empty ~f:(fun fn acc ->
      match String.lsplit2 fn ~on:'.' with
      | Some (s, "mld") -> String.Map.add acc s fn
      | _ -> acc))
  in
  List.filter_map d.stanzas ~f:(function
    | Documentation doc ->
      let mlds =
        let mlds = Lazy.force mlds in
        Ordered_set_lang.String.eval_unordered doc.mld_files
          ~parse:(fun ~loc s ->
            match String.Map.find mlds s with
            | Some s ->
              s
            | None ->
              Errors.fail loc "%s.mld doesn't exist in %s" s
                (Path.to_string_maybe_quoted
                   (Path.drop_optional_build_context dir))
          )
          ~standard:mlds
      in
      Some (doc, List.map (String.Map.values mlds) ~f:(Path.relative dir))
    | _ -> None)

module Dir_status = struct
  type t =
    | Standalone of
        (File_tree.Dir.t * Super_context.Dir_with_jbuild.t option) option
    (* Directory not part of a multi-directory group. The argument is
       [None] for directory that are not from the source tree, such as
       generated ones. *)

    | Group_root of File_tree.Dir.t
                    * Super_context.Dir_with_jbuild.t
    (* Directory with [(include_subdirs x)] where [x] is not [no] *)

    | Is_component_of_a_group_but_not_the_root of
        Super_context.Dir_with_jbuild.t option
    (* Sub-directory of a [Group_root _] *)

  let is_standalone = function
    | Standalone _ -> true
    | _ -> false

  let cache = Hashtbl.create 32

  let get_include_subdirs stanzas =
    List.fold_left stanzas ~init:None ~f:(fun acc stanza ->
      match stanza with
      | Include_subdirs (loc, x) ->
        if Option.is_some acc then
          Errors.fail loc "The 'include_subdirs' stanza cannot appear \
                        more than once";
        Some x
      | _ -> acc)

  let check_no_module_consumer stanzas =
    List.iter stanzas ~f:(fun stanza ->
      match stanza with
      | Library { buildable; _} | Executables { buildable; _ }
      | Tests { exes = { buildable; _ }; _ } ->
        Errors.fail buildable.loc
          "This stanza is not allowed in a sub-directory of directory with \
           (include_subdirs unqualified).\n\
           Hint: add (include_subdirs no) to this file."
      | _ -> ())

  let rec get sctx ~dir =
    match Hashtbl.find cache dir with
    | Some t -> t
    | None ->
      let t =
        match
          Option.bind (Path.drop_build_context dir)
            ~f:(File_tree.find_dir (Super_context.file_tree sctx))
        with
        | None -> begin
            match Path.parent dir with
            | None -> Standalone None
            | Some dir ->
              if is_standalone (get sctx ~dir) then
                Standalone None
              else
                Is_component_of_a_group_but_not_the_root None
          end
        | Some ft_dir ->
          let project_root =
            File_tree.Dir.project ft_dir
            |> Dune_project.root
            |> Path.of_local in
          match Super_context.stanzas_in sctx ~dir with
          | None ->
            if Path.equal dir project_root ||
               is_standalone (get sctx ~dir:(Path.parent_exn dir)) then
              Standalone (Some (ft_dir, None))
            else
              Is_component_of_a_group_but_not_the_root None
          | Some d ->
            match get_include_subdirs d.stanzas with
            | Some Unqualified ->
              Group_root (ft_dir, d)
            | Some No ->
              Standalone (Some (ft_dir, Some d))
            | None ->
              if dir <> project_root &&
                 not (is_standalone (get sctx ~dir:(Path.parent_exn dir)))
              then begin
                check_no_module_consumer d.stanzas;
                Is_component_of_a_group_but_not_the_root (Some d)
              end else
                Standalone (Some (ft_dir, Some d))
      in
      Hashtbl.add cache dir t;
      t

  let get_assuming_parent_is_part_of_group sctx ~dir ft_dir =
    match Hashtbl.find cache (File_tree.Dir.path ft_dir) with
    | Some t -> t
    | None ->
      let t =
        match Super_context.stanzas_in sctx ~dir with
        | None -> Is_component_of_a_group_but_not_the_root None
        | Some d ->
          match get_include_subdirs d.stanzas with
          | Some Unqualified ->
            Group_root (ft_dir, d)
          | Some No ->
            Standalone (Some (ft_dir, Some d))
          | None ->
            check_no_module_consumer d.stanzas;
            Is_component_of_a_group_but_not_the_root (Some d)
      in
      Hashtbl.add cache dir t;
      t
end

let cache = Hashtbl.create 32

let clear_cache () =
  Hashtbl.reset cache;
  Hashtbl.reset Dir_status.cache

let () = Hooks.End_of_build.always clear_cache

let rec get sctx ~dir =
  match Hashtbl.find cache dir with
  | Some t -> t
  | None ->
    match Dir_status.get sctx ~dir with
    | Standalone x ->
      let t =
        match x with
        | Some (ft_dir, Some d) ->
          let files = load_text_files sctx ft_dir d in
          { kind = Standalone
          ; dir
          ; text_files = files
          ; modules = lazy (build_modules_map d ~scope:d.scope
                              ~modules:(modules_of_files ~dir:d.ctx_dir ~files))
          ; mlds = lazy (build_mlds_map d ~files)
          }
        | Some (_, None)
        | None ->
          { kind = Standalone
          ; dir
          ; text_files = String.Set.empty
          ; modules = lazy empty_modules
          ; mlds = lazy []
          }
      in
      Hashtbl.add cache dir t;
      t
    | Is_component_of_a_group_but_not_the_root _ -> begin
        match Hashtbl.find cache dir with
        | Some t -> t
        | None ->
          ignore (get sctx ~dir:(Path.parent_exn dir) : t);
          (* Filled while scanning the group root *)
          Option.value_exn (Hashtbl.find cache dir)
      end
    | Group_root (ft_dir, d) ->
      let rec walk ft_dir ~dir acc =
        match
          Dir_status.get_assuming_parent_is_part_of_group sctx ft_dir ~dir
        with
        | Is_component_of_a_group_but_not_the_root d ->
          let files =
            match d with
            | None -> File_tree.Dir.files ft_dir
            | Some d -> load_text_files sctx ft_dir d
          in
          walk_children ft_dir ~dir ((dir, files) :: acc)
        | _ -> acc
      and walk_children ft_dir ~dir acc =
        String.Map.foldi (File_tree.Dir.sub_dirs ft_dir) ~init:acc
          ~f:(fun name ft_dir acc ->
            let dir = Path.relative dir name in
            walk ft_dir ~dir acc)
      in
      let files = load_text_files sctx ft_dir d in
      let subdirs = walk_children ft_dir ~dir [] in
      let modules = lazy (
        let modules =
          List.fold_left ((dir, files) :: subdirs) ~init:Module.Name.Map.empty
            ~f:(fun acc (dir, files) ->
              let modules = modules_of_files ~dir ~files in
              Module.Name.Map.union acc modules ~f:(fun name x y ->
                Errors.fail (Loc.in_file
                            (Path.to_string
                               (match File_tree.Dir.dune_file ft_dir with
                                | None ->
                                  Path.relative (File_tree.Dir.path ft_dir)
                                    "_unknown_"
                                | Some d -> File_tree.Dune_file.path d)))
                  "Module %a appears in several directories:\
                   @\n- %a\
                   @\n- %a"
                  Module.Name.pp_quote name
                  Path.pp (Module.dir x)
                  Path.pp (Module.dir y)))
        in
        build_modules_map d ~scope:d.scope ~modules)
      in
      let t =
        { kind = Group_root
                   (lazy (List.map subdirs ~f:(fun (dir, _) -> get sctx ~dir)))
        ; dir
        ; text_files = files
        ; modules
        ; mlds = lazy (build_mlds_map d ~files)
        }
      in
      Hashtbl.add cache dir t;
      List.iter subdirs ~f:(fun (dir, files) ->
        Hashtbl.add cache dir
          { kind = Group_part t
          ; dir
          ; text_files = files
          ; modules
          ; mlds = lazy (build_mlds_map d ~files)
          });
      t
