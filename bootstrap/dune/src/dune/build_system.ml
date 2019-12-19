open! Stdune
open Import
open Fiber.O
module Pre_rule = Rule

let () = Hooks.End_of_build.always Memo.reset

module Fs : sig
  val mkdir_p : Path.Build.t -> unit

  (** Creates directory if inside build path, otherwise asserts that directory
      exists. *)
  val mkdir_p_or_check_exists : loc:Loc.t option -> Path.t -> unit

  val assert_exists : loc:Loc.t option -> Path.t -> unit
end = struct
  let mkdir_p_def =
    Memo.create "mkdir_p" ~doc:"mkdir_p"
      ~input:(module Path.Build)
      ~output:(Simple (module Unit))
      ~visibility:Hidden Sync
      (fun p -> Path.mkdir_p (Path.build p))

  let mkdir_p = Memo.exec mkdir_p_def

  let assert_exists_def =
    Memo.create "assert_path_exists" ~doc:"Path.exists"
      ~input:(module Path)
      ~output:(Simple (module Bool))
      ~visibility:Hidden Sync Path.exists

  let assert_exists ~loc path =
    if not (Memo.exec assert_exists_def path) then
      User_error.raise ?loc
        [ Pp.textf "%S does not exist" (Path.to_string_maybe_quoted path) ]

  let mkdir_p_or_check_exists ~loc path =
    match Path.as_in_build_dir path with
    | None -> assert_exists ~loc path
    | Some path -> mkdir_p path
end

module Promoted_to_delete : sig
  val add : Path.t -> unit

  val load : unit -> Path.Set.t
end = struct
  module P = Persistent.Make (struct
    type t = Path.Set.t

    let name = "PROMOTED-TO-DELETE"

    let version = 1
  end)

  let db = ref Path.Set.empty

  let fn = Path.relative Path.build_dir ".to-delete-in-source-tree"

  let needs_dumping = ref false

  let add p =
    if not (Path.Set.mem !db p) then (
      needs_dumping := true;
      db := Path.Set.add !db p
    )

  let load () = Option.value ~default:Path.Set.empty (P.load fn)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then (
      needs_dumping := false;
      load () |> Path.Set.union !db |> P.dump fn
    )

  let () = Hooks.End_of_build.always dump
end

let files_in_source_tree_to_delete () = Promoted_to_delete.load ()

let rule_loc ~info ~dir =
  match (info : Rule.Info.t) with
  | From_dune_file loc -> loc
  | Internal
  | Source_file_copy ->
    let dir = Path.drop_optional_build_context_src_exn (Path.build dir) in
    let file =
      match
        Option.bind (File_tree.find_dir dir) ~f:File_tree.Dir.dune_file
      with
      | Some file -> File_tree.Dune_file.path file
      | None -> Path.Source.relative dir "_unknown_"
    in
    Loc.in_file (Path.source file)

module Internal_rule = struct
  module Id = struct
    include Id.Make ()

    module Top_closure_f =
      Top_closure.Make
        (Set)
        (struct
          type 'a t = 'a Fiber.t

          let return = Fiber.return

          let ( >>= ) = Fiber.O.( >>= )
        end)

    module Top_closure = Top_closure.Make (Set) (Monad.Id)
  end

  module T = struct
    type t =
      { id : Id.t
      ; static_deps : Static_deps.t Fiber.Once.t
      ; targets : Path.Build.Set.t
      ; context : Context.t option
      ; build : Action.t Build.t
      ; mode : Rule.Mode.t
      ; info : Rule.Info.t
      ; dir : Path.Build.t
      ; env : Env.t option
      ; locks : Path.t list
      }

    let compare a b = Id.compare a.id b.id

    let to_dyn t : Dyn.t =
      Record
        [ ("id", Id.to_dyn t.id)
        ; ("loc", Dyn.Encoder.option Loc.to_dyn (Rule.Info.loc t.info))
        ]
  end

  include T
  module O = Comparable.Make (T)
  module Set = O.Set

  let equal a b = Id.equal a.id b.id

  let hash t = Id.hash t.id

  let lib_deps t =
    (* Forcing this lazy ensures that the various globs and [if_file_exists]
       are resolved inside the [Build.t] value. *)
    let+ _ = Fiber.Once.get t.static_deps in
    Build.lib_deps t.build

  (* Represent the build goal given by the user. This rule is never actually
     executed and is only used starting point of all dependency paths. *)
  let root =
    { id = Id.gen ()
    ; static_deps =
        Fiber.Once.create (fun () -> Fiber.return Static_deps.empty)
    ; targets = Path.Build.Set.empty
    ; context = None
    ; build = Build.return (Action.Progn [])
    ; mode = Standard
    ; info = Internal
    ; dir = Path.Build.root
    ; env = None
    ; locks = []
    }

  (* Create a shim for the main build goal *)
  let shim_of_build_goal ~build ~static_deps =
    { root with id = Id.gen (); static_deps; build }

  let effective_env rule =
    match (rule.env, rule.context) with
    | None, None -> Env.initial
    | Some e, _ -> e
    | None, Some c -> c.env
end

module Alias0 = struct
  include Alias

  let dep t = Build.path (Path.build (stamp_file t))

  let dep_multi_contexts ~dir ~name ~contexts =
    ignore (find_dir_specified_on_command_line ~dir);
    let context_to_stamp_file ctx =
      let ctx_dir = Context_name.build_dir ctx in
      let dir = Path.Build.(append_source ctx_dir dir) in
      Path.build (stamp_file (make ~dir name))
    in
    Build.paths (List.map contexts ~f:context_to_stamp_file)

  open Build.O

  let dep_rec_internal ~name ~dir ~ctx_dir =
    let f dir acc =
      let path = Path.Build.append_source ctx_dir (File_tree.Dir.path dir) in
      let fn = stamp_file (make ~dir:path name) in
      let fn = Path.build fn in
      Build.map2 ~f:( && ) acc
        (Build.if_file_exists fn
           ~then_:(Build.path fn >>> Build.return false)
           ~else_:(Build.return true))
    in
    Build.lazy_no_targets
      ( lazy
        (File_tree.Dir.fold dir ~traverse:Sub_dirs.Status.Set.normal_only
           ~init:(Build.return true) ~f) )

  let dep_rec t ~loc =
    let ctx_dir, src_dir =
      Path.Build.extract_build_context_dir_exn (Alias.dir t)
    in
    match File_tree.find_dir src_dir with
    | None ->
      Build.fail
        { fail =
            (fun () ->
              User_error.raise ~loc
                [ Pp.textf "Don't know about directory %s!"
                    (Path.Source.to_string_maybe_quoted src_dir)
                ])
        }
    | Some dir ->
      let name = Alias.name t in
      let+ is_empty = dep_rec_internal ~name ~dir ~ctx_dir in
      if is_empty && not (is_standard name) then
        User_error.raise ~loc
          [ Pp.text "This alias is empty."
          ; Pp.textf "Alias %S is not defined in %s or any of its descendants."
              (Alias.Name.to_string name)
              (Path.Source.to_string_maybe_quoted src_dir)
          ]

  let dep_rec_multi_contexts ~dir:src_dir ~name ~contexts =
    let open Build.O in
    let dir = find_dir_specified_on_command_line ~dir:src_dir in
    let+ is_empty_list =
      Build.all
        (List.map contexts ~f:(fun ctx ->
             let ctx_dir = Context_name.build_dir ctx in
             dep_rec_internal ~name ~dir ~ctx_dir))
    in
    let is_empty = List.for_all is_empty_list ~f:Fn.id in
    if is_empty && not (is_standard name) then
      User_error.raise
        [ Pp.textf "Alias %S specified on the command line is empty."
            (Alias.Name.to_string name)
        ; Pp.textf "It is not defined in %s or any of its descendants."
            (Path.Source.to_string_maybe_quoted src_dir)
        ]

  let package_install ~(context : Context.t) ~pkg =
    make
      (Alias.Name.of_string (sprintf ".%s-files" (Package.Name.to_string pkg)))
      ~dir:context.build_dir
end

module Loaded = struct
  type build =
    { allowed_subdirs : Path.Unspecified.w Dir_set.t
    ; rules_produced : Rules.t
    ; targets_here : Internal_rule.t Path.Build.Map.t
    ; targets_of_alias_dir : Internal_rule.t Path.Build.Map.t
    }

  type t =
    | Non_build of Path.Set.t
    | Build of build

  let no_rules ~allowed_subdirs =
    Build
      { allowed_subdirs
      ; rules_produced = Rules.empty
      ; targets_here = Path.Build.Map.empty
      ; targets_of_alias_dir = Path.Build.Map.empty
      }
end

module Dir_triage = struct
  type t =
    | Known of Loaded.t
    | Alias_dir_of of Path.Build.t
    | Need_step2
end

(* Stores information needed to determine if rule need to be reexecuted. *)
module Trace_db : sig
  module Entry : sig
    type t =
      { rule_digest : Digest.t
      ; dynamic_deps_stages : (Action_exec.Dynamic_dep.Set.t * Digest.t) list
      ; targets_digest : Digest.t
      }
  end

  val get : Path.t -> Entry.t option

  val set : Path.t -> Entry.t -> unit
end = struct
  module Entry = struct
    type t =
      { rule_digest : Digest.t
      ; dynamic_deps_stages : (Action_exec.Dynamic_dep.Set.t * Digest.t) list
      ; targets_digest : Digest.t
      }
  end

  (* Keyed by the first target *)
  type t = Entry.t Path.Table.t

  let file = Path.relative Path.build_dir ".db"

  module P = Persistent.Make (struct
    type nonrec t = t

    let name = "INCREMENTAL-DB"

    let version = 4
  end)

  let needs_dumping = ref false

  let t =
    lazy
      ( match P.load file with
      | Some t -> t
      | None -> Path.Table.create 1024 )

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then (
      needs_dumping := false;
      P.dump file (Lazy.force t)
    )

  let () = Hooks.End_of_build.always dump

  let get path =
    let t = Lazy.force t in
    Path.Table.find t path

  let set path e =
    let t = Lazy.force t in
    needs_dumping := true;
    Path.Table.replace t ~key:path ~data:e
end

module Subdir_set = struct
  type t =
    | All
    | These of String.Set.t

  let to_dir_set = function
    | All -> Dir_set.universal
    | These s ->
      String.Set.to_list s
      |> List.map ~f:Path.Local.of_string
      |> Dir_set.of_list

  let of_dir_set d =
    match Dir_set.toplevel_subdirs d with
    | Infinite -> All
    | Finite s -> These s

  let of_list l = These (String.Set.of_list l)

  let empty = These String.Set.empty

  let mem t dir =
    match t with
    | All -> true
    | These t -> String.Set.mem t dir

  let union a b =
    match (a, b) with
    | All, _
    | _, All ->
      All
    | These a, These b -> These (String.Set.union a b)

  let union_all = List.fold_left ~init:empty ~f:union
end

type extra_sub_directories_to_keep = Subdir_set.t

module Action_and_deps = struct
  type t = Action.t * Dep.Set.t

  let to_dyn (action, deps) =
    let open Dyn.Encoder in
    let action =
      Action.for_shell action |> Action.For_shell.encode |> Dune_lang.to_dyn
    in
    record [ ("action", action); ("deps", Dep.Set.to_dyn deps) ]
end

module Rule_fn = struct
  let loc_decl = Fdecl.create Dyn.Encoder.opaque

  let loc () = Fdecl.get loc_decl ()
end

module Context_or_install = struct
  type t =
    | Install of Context_name.t
    | Context of Context_name.t

  let to_dyn = function
    | Install ctx -> Dyn.List [ Dyn.String "install"; Context_name.to_dyn ctx ]
    | Context s -> Context_name.to_dyn s
end

type caching =
  { cache : (module Dune_cache.Caching)
  ; check_probability : float
  }

type t =
  { (* File specification by targets *)
    files : Internal_rule.t Path.Build.Table.t
  ; contexts : Context.t Context_name.Map.t
  ; init_rules : Rules.t Fdecl.t
  ; gen_rules :
      (   Context_or_install.t
       -> (dir:Path.Build.t -> string list -> extra_sub_directories_to_keep)
          option)
      Fdecl.t
  ; (* Package files are part of *)
    packages : (Path.Build.t -> Package.Name.Set.t) Fdecl.t
  ; caching : caching option
  ; sandboxing_preference : Sandbox_mode.t list
  ; mutable rule_done : int
  ; mutable rule_total : int
  }

let t = ref None

let set x =
  match !t with
  | None -> t := Some x
  | Some _ -> Code_error.raise "build system already initialized" []

let get_build_system () =
  match !t with
  | Some t -> t
  | None -> Code_error.raise "build system not yet initialized" []

let reset () = t := None

let t = get_build_system

let pp_paths set =
  Pp.enumerate (Path.Set.to_list set) ~f:(fun p ->
      Pp.verbatim
        (Path.to_string_maybe_quoted (Path.drop_optional_build_context p)))

let set_rule_generators ~init ~gen_rules =
  let t = t () in
  let (), init_rules = Rules.collect (fun () -> init ()) in
  Fdecl.set t.init_rules init_rules;
  Fdecl.set t.gen_rules gen_rules

let get_cache () =
  let t = t () in
  t.caching

let get_dir_triage t ~dir =
  match Dpath.analyse_dir dir with
  | Source dir ->
    Dir_triage.Known
      (Non_build (Path.set_of_source_paths (File_tree.files_of dir)))
  | External _ ->
    Dir_triage.Known
      (Non_build
         ( match Path.readdir_unsorted dir with
         | Error Unix.ENOENT -> Path.Set.empty
         | Error m ->
           User_warning.emit
             [ Pp.textf "Unable to read %s" (Path.to_string_maybe_quoted dir)
             ; Pp.textf "Reason: %s" (Unix.error_message m)
             ];
           Path.Set.empty
         | Ok filenames -> Path.Set.of_listing ~dir ~filenames ))
  | Build (Regular Root) ->
    let allowed_subdirs =
      Subdir_set.to_dir_set
        (Subdir_set.of_list
           ( ( [ Dpath.Build.alias_dir; Dpath.Build.install_dir ]
             |> List.map ~f:Path.Build.basename )
           @ ( Context_name.Map.keys t.contexts
             |> List.map ~f:Context_name.to_string ) ))
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Install Root) ->
    let allowed_subdirs =
      Subdir_set.to_dir_set
        (Subdir_set.of_list
           ( Context_name.Map.keys t.contexts
           |> List.map ~f:Context_name.to_string ))
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Alias p) ->
    let build_dir = Dpath.Target_dir.build_dir p in
    Alias_dir_of build_dir
  | Build (Invalid _) ->
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs:Dir_set.empty)
  | Build (Install (With_context _))
  | Build (Regular (With_context _)) ->
    Need_step2

let add_spec_exn t fn rule =
  match Path.Build.Table.find t.files fn with
  | None -> Path.Build.Table.set t.files fn rule
  | Some _ ->
    Code_error.raise
      "add_spec_exn called on the same file twice. This should be prevented \
       by the check in [compile_rules]"
      [ ("file", Path.Build.to_dyn fn) ]

let add_rules_exn t rules =
  Path.Build.Map.iteri rules ~f:(fun key data -> add_spec_exn t key data)

let report_rule_conflict fn (rule' : Internal_rule.t) (rule : Internal_rule.t)
    =
  let describe (rule : Internal_rule.t) =
    match rule.info with
    | From_dune_file { start; _ } ->
      start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
    | Internal -> "<internal location>"
    | Source_file_copy -> "file present in source tree"
  in
  let fn = Path.build fn in
  User_error.raise
    [ Pp.textf "Multiple rules generated for %s:"
        (Path.to_string_maybe_quoted fn)
    ; Pp.textf "- %s" (describe rule')
    ; Pp.textf "- %s" (describe rule)
    ]
    ~hints:
      ( match (rule.info, rule'.info) with
      | Source_file_copy, _
      | _, Source_file_copy ->
        [ Pp.textf "rm -f %s"
            (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
        ]
      | _ -> [] )

(* This contains the targets of the actions that are being executed. On exit,
   we need to delete them as they might contain garbage *)
let pending_targets = ref Path.Build.Set.empty

let () =
  Hooks.End_of_build.always (fun () ->
      let fns = !pending_targets in
      pending_targets := Path.Build.Set.empty;
      Path.Build.Set.iter fns ~f:(fun p -> Path.unlink_no_err (Path.build p)))

let compute_targets_digest targets =
  match
    List.map targets ~f:(fun target -> Cached_digest.file (Path.build target))
  with
  | l -> Some (Digest.generic l)
  | exception (Unix.Unix_error _ | Sys_error _) -> None

let compute_targets_digest_or_raise_error ~info targets =
  let good, bad =
    List.partition_map targets ~f:(fun target ->
        let fn = Path.build target in
        match Cached_digest.refresh fn with
        | digest -> Left (target, digest)
        | exception (Unix.Unix_error _ | Sys_error _) -> Right fn)
  in
  match bad with
  | [] -> (good, Digest.generic (List.map ~f:snd good))
  | missing ->
    User_error.raise ?loc:(Rule.Info.loc info)
      [ Pp.textf "Rule failed to generate the following targets:"
      ; pp_paths (Path.Set.of_list missing)
      ]

let sandbox_dir = Path.Build.relative Path.Build.root ".sandbox"

let locks : (Path.t, Fiber.Mutex.t) Table.t = Table.create (module Path) 32

let rec with_locks mutexes ~f =
  match mutexes with
  | [] -> f ()
  | m :: mutexes ->
    Fiber.Mutex.with_lock
      (Table.find_or_add locks m ~f:(fun _ -> Fiber.Mutex.create ()))
      (fun () -> with_locks mutexes ~f)

let remove_old_artifacts t ~dir ~(subdirs_to_keep : Subdir_set.t) =
  match Path.readdir_unsorted (Path.build dir) with
  | exception _ -> ()
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun fn ->
        let path = Path.Build.relative dir fn in
        let path_is_a_target = Path.Build.Table.mem t.files path in
        if path_is_a_target then
          ()
        else
          match Unix.lstat (Path.Build.to_string path) with
          | { st_kind = S_DIR; _ } -> (
            match subdirs_to_keep with
            | All -> ()
            | These set ->
              if String.Set.mem set fn then
                ()
              else
                Path.rm_rf (Path.build path) )
          | exception _ -> Path.unlink (Path.build path)
          | _ -> Path.unlink (Path.build path))

let no_rule_found t ~loc fn =
  let fail fn ~loc =
    User_error.raise ?loc
      [ Pp.textf "No rule found for %s" (Dpath.describe_target fn) ]
  in
  let hints ctx =
    let candidates =
      Context_name.Map.keys t.contexts |> List.map ~f:Context_name.to_string
    in
    User_message.did_you_mean (Context_name.to_string ctx) ~candidates
  in
  match Dpath.analyse_target fn with
  | Other _ -> fail fn ~loc
  | Regular (ctx, _) ->
    if Context_name.Map.mem t.contexts ctx then
      fail fn ~loc
    else
      User_error.raise
        [ Pp.textf "Trying to build %s but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Install (ctx, _) ->
    if Context_name.Map.mem t.contexts ctx then
      fail fn ~loc
    else
      User_error.raise
        [ Pp.textf
            "Trying to build %s for install but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Alias (ctx, fn') ->
    if Context_name.Map.mem t.contexts ctx then
      fail fn ~loc
    else
      let fn =
        Path.append_source (Path.build (Context_name.build_dir ctx)) fn'
      in
      User_error.raise
        [ Pp.textf
            "Trying to build alias %s but build context %s doesn't exist."
            (Path.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)

(* +-------------------- Adding rules to the system --------------------+ *)

module rec Load_rules : sig
  val load_dir : dir:Path.t -> Loaded.t

  val static_deps : Action.t Build.t -> Static_deps.t Fiber.Once.t

  val targets_of : dir:Path.t -> Path.Set.t
end = struct
  open Load_rules

  let compile_rule pre_rule =
    let { Pre_rule.context; env; build; targets; mode; locks; info; dir } =
      pre_rule
    in
    let static_deps = static_deps build in
    let rule =
      let id = Internal_rule.Id.gen () in
      { Internal_rule.id
      ; static_deps
      ; targets
      ; build
      ; context
      ; env
      ; locks
      ; mode
      ; info
      ; dir
      }
    in
    (targets, rule)

  let static_deps build =
    Fiber.Once.create (fun () ->
        Fiber.return (Build.static_deps build ~list_targets:targets_of))

  let create_copy_rules ~ctx_dir ~non_target_source_files =
    Path.Source.Set.to_list non_target_source_files
    |> List.map ~f:(fun path ->
           let ctx_path = Path.Build.append_source ctx_dir path in
           let build = Build.copy ~src:(Path.source path) ~dst:ctx_path in
           Pre_rule.make
           (* There's an [assert false] in [prepare_managed_paths] that blows
              up if we try to sandbox this. *)
             ~sandbox:Sandbox_config.no_sandboxing build ~context:None
             ~env:None ~info:Source_file_copy)

  let compile_rules ~dir rules =
    List.concat_map rules ~f:(fun rule ->
        let targets, rule = compile_rule rule in
        assert (Path.Build.( = ) dir rule.Internal_rule.dir);
        List.map (Path.Build.Set.to_list targets) ~f:(fun target ->
            (target, rule)))
    |> Path.Build.Map.of_list_reducei ~f:report_rule_conflict

  let targets_of ~dir =
    match load_dir ~dir with
    | Non_build targets -> targets
    | Build { targets_here; _ } ->
      Path.Build.Map.keys targets_here
      |> List.map ~f:Path.build |> Path.Set.of_list

  let compute_alias_rules t ~context_name ~(collected : Rules.Dir_rules.ready)
      ~dir ~sub_dir =
    let alias_dir =
      let context_name = Context_name.to_string context_name in
      Path.Build.append_source
        (Path.Build.relative Dpath.Build.alias_dir context_name)
        sub_dir
    in
    let alias_rules =
      let open Build.O in
      let aliases = collected.aliases in
      let aliases =
        if Alias.Name.Map.mem aliases Alias.Name.default then
          aliases
        else
          match Path.Build.extract_build_context_dir dir with
          | None -> aliases
          | Some (ctx_dir, src_dir) -> (
            match File_tree.find_dir src_dir with
            | None -> aliases
            | Some dir ->
              let default_alias =
                let dune_version =
                  File_tree.Dir.project dir |> Dune_project.dune_version
                in
                if dune_version >= (2, 0) then
                  Alias.Name.all
                else
                  Alias.Name.install
              in
              Alias.Name.Map.set aliases Alias.Name.default
                { deps = Path.Set.empty
                ; dyn_deps =
                    (let+ _ =
                       Alias0.dep_rec_internal ~name:default_alias ~dir
                         ~ctx_dir
                     in
                     Path.Set.empty)
                ; actions = Appendable_list.empty
                } )
      in
      Alias.Name.Map.foldi aliases ~init:[]
        ~f:(fun name
                { Rules.Dir_rules.Alias_spec.deps; dyn_deps; actions }
                rules
                ->
          let base_path =
            Path.Build.relative alias_dir (Alias.Name.to_string name)
          in
          let rules, action_stamp_files =
            List.fold_left (Appendable_list.to_list actions)
              ~init:(rules, Path.Set.empty)
              ~f:(fun (rules, action_stamp_files)
                      { Rules.Dir_rules.stamp
                      ; action
                      ; locks
                      ; context
                      ; loc
                      ; env
                      }
                      ->
                let path =
                  Path.Build.extend_basename base_path
                    ~suffix:("-" ^ Digest.to_string stamp)
                in
                let rule =
                  Pre_rule.make ~locks ~context:(Some context) ~env
                    ~info:(Rule.Info.of_loc_opt loc)
                    (Build.progn [ action; Build.create_file path ])
                in
                ( rule :: rules
                , Path.Set.add action_stamp_files (Path.build path) ))
          in
          let deps = Path.Set.union deps action_stamp_files in
          let path =
            Path.Build.extend_basename base_path ~suffix:Alias0.suffix
          in
          Pre_rule.make ~context:None ~env:None
            (let action =
               let+ () = Build.path_set deps
               and+ dyn_deps = Build.dyn_path_set_reuse dyn_deps in
               let deps = Path.Set.union deps dyn_deps in
               Action.with_stdout_to path
                 (Action.digest_files (Path.Set.to_list deps))
             in
             Build.action_dyn ~targets:[ path ] action)
          :: rules)
    in
    fun ~subdirs_to_keep ->
      let compiled = compile_rules ~dir:alias_dir alias_rules in
      add_rules_exn t compiled;
      remove_old_artifacts t ~dir:alias_dir ~subdirs_to_keep;
      compiled

  let filter_out_fallback_rules ~to_copy ~dir rules =
    List.filter rules ~f:(fun (rule : Pre_rule.t) ->
        match rule.mode with
        | Standard
        | Promote _
        | Ignore_source_files ->
          true
        | Fallback ->
          let source_files_for_targtes =
            (* All targets are in [dir] and we know it correspond to a
               directory of a build context since there are source files to
               copy, so this call can't fail. *)
            Path.Build.Set.to_list rule.targets
            |> List.map ~f:Path.Build.drop_build_context_exn
            |> Path.Source.Set.of_list
          in
          if Path.Source.Set.is_subset source_files_for_targtes ~of_:to_copy
          then
            (* All targets are present *)
            false
          else if
            Path.Source.Set.is_empty
              (Path.Source.Set.inter source_files_for_targtes to_copy)
          then
            (* No target is present *)
            true
          else
            let absent_targets =
              Path.Source.Set.diff source_files_for_targtes to_copy
            in
            let present_targets =
              Path.Source.Set.diff source_files_for_targtes absent_targets
            in
            User_error.raise
              ~loc:(rule_loc ~info:rule.info ~dir)
              [ Pp.text
                  "Some of the targets of this fallback rule are present in \
                   the source tree, and some are not. This is not allowed. \
                   Either none of the targets must be present in the source \
                   tree, either they must all be."
              ; Pp.nop
              ; Pp.text "The following targets are present:"
              ; pp_paths (Path.set_of_source_paths present_targets)
              ; Pp.nop
              ; Pp.text "The following targets are not:"
              ; pp_paths (Path.set_of_source_paths absent_targets)
              ])

  (** If both [a] and [a/b] are source directories, we don't allow the rules
      for [a] to define generated directories under [a/b] (e.g.
      [a/b/.generated-by-a]).

      The purpose is to avoid dependency cycles when computing the list of
      subdirectories of [b]: you'd need to load rules for [a], for which you
      often need to load rules for [a/b], at which point you need to do stale
      artifact deletion, so you need to have computed the set of children of
      [b] already.

      One reasonable alternative is to delay stale artifact deletion until it's
      actually necessary and I (aalekseyev) believe it to be a better approach,
      but for now this restriction gives us an easier and more incremental way
      forward.

      This is in addition to another more general restriction: a directory is
      only allowed to be generated if its parent knows about it.

      This module encodes those restrictions. *)
  module Generated_directory_restrictions : sig
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    (** Used by the child to ask about the restrictions placed by the parent. *)
    val allowed_by_parent : dir:Path.Build.t -> restriction

    (** Used by the parent to check what are the subdirs that it's allowed to
        generate rules in. *)
    val is_allowed_to_generate_rules_in :
      dir:Path.Build.t -> subdir:Path.Build.t -> bool
  end = struct
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    let corresponding_source_dir ~dir =
      match Dpath.analyse_target dir with
      | Install _
      | Alias _
      | Other _ ->
        None
      | Regular (_ctx, sub_dir) -> File_tree.find_dir sub_dir

    let source_subdirs_of_build_dir ~dir =
      match corresponding_source_dir ~dir with
      | None -> String.Set.empty
      | Some dir -> File_tree.Dir.sub_dir_names dir

    let is_allowed_to_generate_rules_in ~dir ~subdir =
      match Path.Local_gen.descendant ~of_:dir subdir with
      | None -> true
      | Some reach -> (
        match Path.Local_gen.split_first_component reach with
        | None ->
          (* allowed to generate rules inside itself *)
          true
        | Some (child, _) ->
          if
            Option.is_none
              (corresponding_source_dir
                 ~dir:(Path.Local_gen.relative dir child))
          then
            (* allowed to generate directories inside itself *)
            true
          else
            (* allowed to generate rules in child directories as long as the
               directory itself is not generated *)
            Option.is_some (corresponding_source_dir ~dir:subdir) )

    let allowed_dirs ~dir ~subdir : restriction =
      if String.Set.mem (source_subdirs_of_build_dir ~dir) subdir then
        Unrestricted
      else
        Restricted
          (Memo.Lazy.create (fun () ->
               match load_dir ~dir:(Path.build dir) with
               | Non_build _ -> Dir_set.just_the_root
               | Build { allowed_subdirs; _ } ->
                 Dir_set.descend allowed_subdirs subdir))

    let allowed_by_parent ~dir =
      allowed_dirs
        ~dir:(Path.Build.parent_exn dir)
        ~subdir:(Path.Build.basename dir)
  end

  let load_dir_step2_exn t ~dir =
    let context_name, sub_dir =
      match Dpath.analyse_path dir with
      | Build (Install (ctx, path)) -> (Context_or_install.Install ctx, path)
      | Build (Regular (ctx, path)) -> (Context_or_install.Context ctx, path)
      | Build (Alias _)
      | Build (Other _)
      | Source _
      | External _ ->
        Code_error.raise "[load_dir_step2_exn] was called on a strange path"
          [ ("path", Path.to_dyn dir) ]
    in
    (* the above check makes this safe *)
    let dir = Path.as_in_build_dir_exn dir in
    (* Load all the rules *)
    let extra_subdirs_to_keep, rules_produced =
      let gen_rules =
        match (Fdecl.get t.gen_rules) context_name with
        | None ->
          Code_error.raise "[gen_rules] did not specify rules for the context"
            [ ("context_name", Context_or_install.to_dyn context_name) ]
        | Some rules -> rules
      in
      Rules.collect (fun () -> gen_rules ~dir (Path.Source.explode sub_dir))
    in
    let rules =
      let dir = Path.build dir in
      Rules.Dir_rules.union
        (Rules.find rules_produced dir)
        (Rules.find (Fdecl.get t.init_rules) dir)
    in
    let collected = Rules.Dir_rules.consume rules in
    let rules = collected.rules in
    let alias_rules =
      match context_name with
      | Context context_name ->
        Some (compute_alias_rules t ~context_name ~collected ~dir ~sub_dir)
      | Install _ -> None
    in
    let file_tree_dir =
      match context_name with
      | Install _ -> None
      | Context _ -> File_tree.find_dir sub_dir
    in
    (* Compute the set of targets and the set of source files that must not be
       copied *)
    let source_files_to_ignore =
      List.fold_left rules ~init:Path.Build.Set.empty
        ~f:(fun acc_ignored { Pre_rule.targets; mode; _ } ->
          match mode with
          | Promote { only = None; _ }
          | Ignore_source_files ->
            Path.Build.Set.union targets acc_ignored
          | Promote { only = Some pred; _ } ->
            let to_ignore =
              Path.Build.Set.filter targets ~f:(fun target ->
                  Predicate_lang.Glob.exec pred
                    (Path.reach (Path.build target) ~from:(Path.build dir))
                    ~standard:Predicate_lang.any)
            in
            Path.Build.Set.union to_ignore acc_ignored
          | _ -> acc_ignored)
    in
    let source_files_to_ignore =
      Path.Build.Set.to_list source_files_to_ignore
      |> List.map ~f:Path.Build.drop_build_context_exn
      |> Path.Source.Set.of_list
    in
    (* Take into account the source files *)
    let to_copy, subdirs_to_keep =
      match context_name with
      | Install _ -> (None, String.Set.empty)
      | Context context_name ->
        (* This condition is [true] because of [get_dir_status] *)
        assert (Context_name.Map.mem t.contexts context_name);
        let files, subdirs =
          match file_tree_dir with
          | None -> (Path.Source.Set.empty, String.Set.empty)
          | Some dir ->
            (File_tree.Dir.file_paths dir, File_tree.Dir.sub_dir_names dir)
        in
        let files = Path.Source.Set.diff files source_files_to_ignore in
        if Path.Source.Set.is_empty files then
          (None, subdirs)
        else
          let ctx_path = Context_name.build_dir context_name in
          (Some (ctx_path, files), subdirs)
    in
    let subdirs_to_keep =
      match extra_subdirs_to_keep with
      | All -> Subdir_set.All
      | These set -> These (String.Set.union subdirs_to_keep set)
    in
    (* Filter out fallback rules *)
    let rules =
      match to_copy with
      | None ->
        (* If there are no source files to copy, fallback rules are
           automatically kept *)
        rules
      | Some (_, to_copy) -> filter_out_fallback_rules ~to_copy ~dir rules
    in
    (* Compile the rules and cleanup stale artifacts *)
    let rules =
      ( match to_copy with
      | None -> []
      | Some (ctx_dir, source_files) ->
        create_copy_rules ~ctx_dir ~non_target_source_files:source_files )
      @ rules
    in
    let targets_here = compile_rules ~dir rules in
    add_rules_exn t targets_here;
    let allowed_by_parent =
      Generated_directory_restrictions.allowed_by_parent ~dir
    in
    ( match allowed_by_parent with
    | Unrestricted -> ()
    | Restricted restriction -> (
      match Path.Build.Map.find (Rules.to_map rules_produced) dir with
      | None -> ()
      | Some rules ->
        if Dir_set.here (Memo.Lazy.force restriction) then
          ()
        else
          Code_error.raise
            "Generated rules in a directory not allowed by the parent"
            [ ("dir", Path.Build.to_dyn dir)
            ; ("rules", Rules.Dir_rules.to_dyn rules)
            ] ) );
    let rules_generated_in =
      Dir_set.of_list
        ( Path.Build.Map.keys (Rules.to_map rules_produced)
        |> List.filter_map ~f:(fun subdir ->
               Path.Local_gen.descendant ~of_:dir subdir) )
    in
    let allowed_granddescendants_of_parent =
      match allowed_by_parent with
      | Unrestricted ->
        (* in this case the parent isn't allowed to create any generated
           granddescendant directories *)
        Dir_set.empty
      | Restricted restriction -> Memo.Lazy.force restriction
    in
    let descendants_to_keep =
      Dir_set.union_all
        [ rules_generated_in
        ; Subdir_set.to_dir_set subdirs_to_keep
        ; allowed_granddescendants_of_parent
        ]
    in
    let violations =
      Path.Build.Map.filter_mapi (Rules.to_map rules_produced)
        ~f:(fun key data ->
          let allowed =
            Generated_directory_restrictions.is_allowed_to_generate_rules_in
              ~dir ~subdir:key
          in
          Option.some_if (not allowed) data)
    in
    if not (Path.Build.Map.is_empty violations) then
      Code_error.raise
        "Directory creates generated directories inside its descendant source \
         directories. This is not allowed."
        [ ("dir", Path.Build.to_dyn dir)
        ; ( "creates-rules-in"
          , Dyn.Encoder.(list (pair Path.Build.to_dyn Rules.Dir_rules.to_dyn))
              (Path.Build.Map.to_list violations) )
        ];
    let subdirs_to_keep = Subdir_set.of_dir_set descendants_to_keep in
    remove_old_artifacts t ~dir ~subdirs_to_keep;
    let alias_targets =
      Option.map ~f:(fun f -> f ~subdirs_to_keep) alias_rules
    in
    let targets_of_alias_dir =
      Option.value ~default:Path.Build.Map.empty alias_targets
    in
    Loaded.Build
      { allowed_subdirs = descendants_to_keep
      ; rules_produced
      ; targets_here
      ; targets_of_alias_dir
      }

  let load_dir_impl t ~dir : Loaded.t =
    match get_dir_triage t ~dir with
    | Known l -> l
    | Alias_dir_of dir' -> (
      match load_dir ~dir:(Path.build dir') with
      | Non_build _ -> Code_error.raise "Can only forward to a build dir" []
      | Build
          { targets_here = _
          ; targets_of_alias_dir
          ; rules_produced
          ; allowed_subdirs
          } ->
        Loaded.Build
          { targets_here = targets_of_alias_dir
          ; targets_of_alias_dir = Path.Build.Map.empty
          ; rules_produced
          ; allowed_subdirs
          } )
    | Need_step2 -> load_dir_step2_exn t ~dir

  let load_dir =
    let load_dir_impl dir = load_dir_impl (t ()) ~dir in
    let memo =
      Memo.create_hidden "load-dir" ~doc:"load dir"
        ~input:(module Path)
        Sync load_dir_impl
    in
    fun ~dir -> Memo.exec memo dir
end

open Load_rules

let load_dir_and_get_buildable_targets ~dir =
  let loaded = load_dir ~dir in
  match loaded with
  | Non_build _ -> Path.Build.Map.empty
  | Build { targets_here; _ } -> targets_here

let get_rule_other fn =
  Option.bind (Path.as_in_build_dir fn) ~f:(fun fn ->
      let dir = Path.Build.parent_exn fn in
      match load_dir ~dir:(Path.build dir) with
      | Non_build _ -> assert false
      | Build { targets_here; _ } -> Path.Build.Map.find targets_here fn)

and get_rule t path =
  let dir = Path.parent_exn path in
  if Path.is_strict_descendant_of_build_dir dir then
    let rules = load_dir_and_get_buildable_targets ~dir in
    let path = Path.as_in_build_dir_exn path in
    match Path.Build.Map.find rules path with
    | Some _ as some -> Fiber.return some
    | None ->
      let loc = Rule_fn.loc () in
      no_rule_found t ~loc path
  else if Path.exists path then
    Fiber.return None
  else
    let loc = Rule_fn.loc () in
    User_error.raise ?loc
      [ Pp.textf "File unavailable: %s" (Path.to_string_maybe_quoted path) ]

let all_targets t =
  let root = File_tree.root () in
  Context_name.Map.to_list t.contexts
  |> List.fold_left ~init:Path.Build.Set.empty ~f:(fun acc (_, ctx) ->
         File_tree.Dir.fold root ~traverse:Sub_dirs.Status.Set.all ~init:acc
           ~f:(fun dir acc ->
             match
               load_dir
                 ~dir:
                   (Path.build
                      (Path.Build.append_source ctx.Context.build_dir
                         (File_tree.Dir.path dir)))
             with
             | Non_build _ -> acc
             | Build { targets_here; targets_of_alias_dir; _ } ->
               List.fold_left ~init:acc ~f:Path.Build.Set.add
                 ( Path.Build.Map.keys targets_of_alias_dir
                 @ Path.Build.Map.keys targets_here )))

module type Rec = sig
  val build_file : Path.t -> unit Fiber.t

  val execute_rule : Internal_rule.t -> unit Fiber.t

  module Pred : sig
    val eval : File_selector.t -> Path.Set.t

    val build : File_selector.t -> unit Fiber.t
  end

  val evaluate_rule : Internal_rule.t -> (Action.t * Dep.Set.t) Fiber.t

  (* other stuff: *)
  val evaluate_rule_and_wait_for_dependencies :
    Internal_rule.t -> (Action.t * Dep.Set.t) Fiber.t
end

(* Separation between [Used_recursively] and [Exported] is necessary because at
   least one module in the recursive module group must be pure (only expose
   functions) *)
module rec Used_recursively : Rec = Exported

and Exported : sig
  include Rec

  (* exported to inspect cache cycles *)

  val evaluate_action_and_dynamic_deps_memo :
    ( Internal_rule.t
    , Action_and_deps.t
    , Internal_rule.t -> Action_and_deps.t Fiber.t )
    Memo.t

  val build_file_memo : (Path.t, unit, Path.t -> unit Fiber.t) Memo.t
end = struct
  open Used_recursively

  let build_deps =
    Dep.Set.parallel_iter ~f:(function
      | Alias a -> build_file (Path.build (Alias.stamp_file a))
      | File f -> build_file f
      | File_selector g -> Pred.build g
      | Universe
      | Env _
      | Sandbox_config _ ->
        Fiber.return ())

  let eval_pred = Pred.eval

  (* Evaluate a rule and return the action and set of dynamic dependencies *)
  let evaluate_action_and_dynamic_deps_memo =
    let f (rule : Internal_rule.t) =
      let* static_deps = Fiber.Once.get rule.static_deps in
      let rule_deps = Static_deps.rule_deps static_deps in
      let+ () = build_deps rule_deps in
      Build.exec ~eval_pred rule.build
    in
    Memo.create "evaluate-action-and-dynamic-deps"
      ~output:(Simple (module Action_and_deps))
      ~doc:
        "Evaluate the build description of a rule and return the action and \
         dynamic dependency of the rule."
      ~input:(module Internal_rule)
      ~visibility:Hidden Async f

  let evaluate_action_and_dynamic_deps =
    Memo.exec evaluate_action_and_dynamic_deps_memo

  let select_sandbox_mode (config : Sandbox_config.t) ~loc
      ~sandboxing_preference =
    let evaluate_sandboxing_preference preference =
      match Sandbox_mode.Set.mem config preference with
      | false -> None
      | true -> (
        match preference with
        | Some Symlink ->
          if Sandbox_mode.Set.mem config Sandbox_mode.copy then
            Some
              ( if Sys.win32 then
                Sandbox_mode.copy
              else
                Sandbox_mode.symlink )
          else
            User_error.raise ~loc
              [ Pp.text
                  "This rule requires sandboxing with symlinks, but that \
                   won't work on Windows."
              ]
        | _ -> Some preference )
    in
    match
      List.find_map sandboxing_preference ~f:evaluate_sandboxing_preference
    with
    | Some choice -> choice
    | None ->
      (* This is not trivial to reach because the user rules are checked at
         parse time and [sandboxing_preference] always includes all possible
         modes. However, it can still be reached if multiple sandbox config
         specs are combined into an unsatisfiable one. *)
      User_error.raise ~loc
        [ Pp.text
            "This rule forbids all sandboxing modes (but it also requires \
             sandboxing)"
        ]

  let evaluate_rule (rule : Internal_rule.t) =
    let* static_deps = Fiber.Once.get rule.static_deps in
    let+ action, dynamic_action_deps = evaluate_action_and_dynamic_deps rule in
    let static_action_deps = Static_deps.action_deps static_deps in
    let action_deps = Dep.Set.union static_action_deps dynamic_action_deps in
    (action, action_deps)

  (* Same as the function just bellow, but with less opportunity for
     parallelism. We keep this dead code here for documentation purposes as it
     is easier to read the one bellow. The reader only has to check that both
     function do the same thing. *)
  let _evaluate_rule_and_wait_for_dependencies rule =
    let* action, action_deps = evaluate_rule rule in
    let+ () = build_deps action_deps in
    (action, action_deps)

  (* The following function does exactly the same as the function above with
     the difference that it starts the build of static dependencies before we
     know the final action and set of dynamic dependencies. We do this to
     increase opportunities for parallelism. *)
  let evaluate_rule_and_wait_for_dependencies (rule : Internal_rule.t) =
    let* static_deps = Fiber.Once.get rule.static_deps in
    let static_action_deps = Static_deps.action_deps static_deps in
    (* Build the static dependencies in parallel with evaluation the action and
       dynamic dependencies *)
    let* action, dynamic_action_deps =
      Fiber.fork_and_join_unit
        (fun () -> build_deps static_action_deps)
        (fun () -> evaluate_action_and_dynamic_deps rule)
    in
    build_deps dynamic_action_deps
    >>>
    let action_deps = Dep.Set.union static_action_deps dynamic_action_deps in
    Fiber.return (action, action_deps)

  let start_rule t _rule = t.rule_total <- t.rule_total + 1

  (* Same as [rename] except that if the source doesn't exist we delete the
     destination *)
  let rename_optional_file ~src ~dst =
    let src = Path.Build.to_string src in
    let dst = Path.Build.to_string dst in
    match Unix.rename src dst with
    | () -> ()
    | exception Unix.Unix_error ((ENOENT | ENOTDIR), _, _) -> (
      match Unix.unlink dst with
      | exception Unix.Unix_error (ENOENT, _, _) -> ()
      | () -> () )

  let compute_rule_digest (rule : Internal_rule.t) ~deps ~action ~sandbox_mode
      =
    let targets_as_list = Path.Build.Set.to_list rule.targets in
    let env = Internal_rule.effective_env rule in
    let trace =
      ( Dep.Set.trace deps ~sandbox_mode ~env ~eval_pred
      , List.map targets_as_list ~f:(fun p -> Path.to_string (Path.build p))
      , Option.map rule.context ~f:(fun c -> c.name)
      , Action.for_shell action )
    in
    Digest.generic trace

  let compute_dependencies_digest deps ~sandbox_mode ~env ~eval_pred =
    Dep.Set.trace deps ~sandbox_mode ~env ~eval_pred
    |> (Digest.generic : Dep.Trace.t -> _)

  let execute_rule_impl rule =
    let t = t () in
    let { Internal_rule.dir
        ; targets
        ; env = _
        ; context
        ; mode
        ; locks
        ; id = _
        ; static_deps = _
        ; build = _
        ; info
        } =
      rule
    in
    start_rule t rule;
    let* action, deps = evaluate_rule_and_wait_for_dependencies rule in
    Stats.new_evaluated_rule ();
    Fs.mkdir_p dir;
    let targets_as_list = Path.Build.Set.to_list targets in
    let head_target = List.hd targets_as_list in
    let env = Internal_rule.effective_env rule in
    let rule_loc = rule_loc ~info ~dir in
    let is_action_dynamic = Action.is_dynamic action in
    let sandbox_mode =
      match Action.is_useful_to_sandbox action with
      | Clearly_not ->
        let config = Dep.Set.sandbox_config deps in
        if Sandbox_config.mem config Sandbox_mode.none then
          Sandbox_mode.none
        else
          User_error.raise ~loc:rule_loc
            [ Pp.text
                "Rule dependencies are configured to require sandboxing, but \
                 the rule has no actions that could potentially require \
                 sandboxing."
            ]
      | Maybe ->
        select_sandbox_mode ~loc:rule_loc
          (Dep.Set.sandbox_config deps)
          ~sandboxing_preference:t.sandboxing_preference
    in
    let always_rerun =
      let force_rerun =
        !Clflags.force
        && List.exists targets_as_list ~f:Dpath.Build.is_alias_stamp_file
      and depends_on_universe = Dep.Set.has_universe deps in
      force_rerun || depends_on_universe
    in
    let rule_digest = compute_rule_digest rule ~deps ~action ~sandbox_mode in
    let do_not_memoize = always_rerun || is_action_dynamic in
    (* Here we determine if we need to rerun the action based on information
       stored in Trace_db. *)
    let* rule_need_rerun =
      if always_rerun then
        Fiber.return true
      else
        (* [prev_trace] will be [None] if rule is run for the first time. *)
        let prev_trace = Trace_db.get (Path.build head_target) in
        (* [targets_digest] will be [None] if not all targets were build. *)
        let targets_digest = compute_targets_digest targets_as_list in
        let rule_or_targets_changed =
          match (prev_trace, targets_digest) with
          | Some prev_trace, Some targets_digest ->
            prev_trace.rule_digest <> rule_digest
            || prev_trace.targets_digest <> targets_digest
          | _ -> true
        in
        if rule_or_targets_changed then
          Fiber.return true
        else
          let prev_trace = Option.value_exn prev_trace in
          (* CR-someday aalekseyev: If there's a change at one of the last
             stages, we still re-run all the previous stages, which is a bit of
             a waste. We could remember what stage needs re-running and only
             re-run that (and later stages). *)
          let rec loop stages =
            match stages with
            | [] -> Fiber.return false
            | (deps, old_digest) :: rest ->
              let deps = Action_exec.Dynamic_dep.Set.to_dep_set deps in
              let* () = build_deps deps in
              let new_digest =
                compute_dependencies_digest deps ~sandbox_mode ~env ~eval_pred
              in
              if old_digest <> new_digest then
                Fiber.return true
              else
                loop rest
          in
          loop prev_trace.dynamic_deps_stages
    in
    let sandbox =
      Option.map sandbox_mode ~f:(fun mode ->
          let sandbox_suffix = rule_digest |> Digest.to_string in
          (Path.Build.relative sandbox_dir sandbox_suffix, mode))
    in
    let* () =
      if rule_need_rerun then (
        List.iter targets_as_list ~f:(fun target ->
            Cached_digest.remove (Path.build target);
            Path.unlink_no_err (Path.build target));
        let from_Dune_cache =
          match (do_not_memoize, t.caching) with
          | true, _
          | _, None ->
            None
          | false, Some { cache = (module Caching); _ } -> (
            match Caching.Cache.search Caching.cache rule_digest with
            | Ok (_, files) -> Some files
            | Error msg ->
              Log.infof "cache miss: %s" msg;
              None )
        and cache_checking =
          match t.caching with
          | Some { check_probability; _ } ->
            Random.float 1. < check_probability
          | _ -> false
        in
        let pulled_from_cache =
          match from_Dune_cache with
          | Some files when not cache_checking -> (
            let retrieve (file : Dune_cache.File.t) =
              let path = Path.build file.in_the_build_directory in
              Log.infof "retrieve %s from cache"
                (Path.to_string_maybe_quoted path);
              Path.link file.in_the_cache path;
              Cached_digest.set path file.digest;
              file.digest
            in
            match
              let digests = List.map files ~f:retrieve in
              Trace_db.set (Path.build head_target)
                (* We do not cache dynamic actions so [dynamic_deps_stages] is
                   alwa ys an empty list here. *)
                { rule_digest
                ; targets_digest = Digest.generic digests
                ; dynamic_deps_stages = []
                }
            with
            | exception Unix.(Unix_error (ENOENT, _, _)) -> false
            | () -> true )
          | _ -> false
        in
        if pulled_from_cache then
          Fiber.return ()
        else (
          pending_targets := Path.Build.Set.union targets !pending_targets;
          let loc = Rule.Info.loc info in
          let sandboxed, action =
            match sandbox with
            | None -> (None, action)
            | Some (sandbox_dir, sandbox_mode) ->
              Path.rm_rf (Path.build sandbox_dir);
              let sandboxed path : Path.Build.t =
                Path.Build.append_local sandbox_dir (Path.Build.local path)
              in
              Dep.Set.dirs deps
              |> Path.Set.iter ~f:(fun path ->
                     match Path.as_in_build_dir path with
                     | None -> Fs.assert_exists ~loc path
                     | Some path -> Fs.mkdir_p (sandboxed path));
              Fs.mkdir_p (sandboxed dir);
              ( Some sandboxed
              , Action.sandbox action ~sandboxed ~mode:sandbox_mode ~deps
                  ~eval_pred )
          in
          let chdirs = Action.chdirs action in
          Path.Set.iter chdirs ~f:Fs.(mkdir_p_or_check_exists ~loc);
          let+ exec_result =
            with_locks locks ~f:(fun () ->
                let copy_files_from_sandbox sandboxed =
                  List.iter targets_as_list ~f:(fun target ->
                      rename_optional_file ~src:(sandboxed target) ~dst:target)
                in
                let+ exec_result =
                  Action_exec.exec ~context ~env ~targets ~rule_loc ~build_deps
                    action
                in
                Option.iter sandboxed ~f:copy_files_from_sandbox;
                exec_result)
          in
          Option.iter sandbox ~f:(fun (p, _mode) -> Path.rm_rf (Path.build p));
          (* All went well, these targets are no longer pending *)
          pending_targets := Path.Build.Set.diff !pending_targets targets;
          let targets, targets_digest =
            compute_targets_digest_or_raise_error ~info targets_as_list
          in
          let () =
            (* Check cache. We don't check for missing file in the cache, since
               the file list is part of the rule hash this really never should
               happen. *)
            match from_Dune_cache with
            | Some cached when cache_checking ->
              (* This being [false] is unexpected and means we have a hash
                 collision *)
              let data_are_ok =
                match
                  List.for_all2 targets cached
                    ~f:(fun (target, _) (c : Dune_cache.File.t) ->
                      Path.Build.equal target c.in_the_build_directory)
                with
                | Ok b -> b
                | Error `Length_mismatch -> false
              in
              if not data_are_ok then
                let open Pp.O in
                let pp x l ~f =
                  Pp.box ~indent:2
                    ( Pp.verbatim x
                    ++ Dyn.pp
                         (Dyn.Encoder.list Path.Build.to_dyn (List.map l ~f))
                    )
                in
                User_warning.emit
                  [ Pp.text "unexpected list of targets in the cache"
                  ; pp "expected: " targets ~f:fst
                  ; pp "got:      " cached ~f:(fun (c : Dune_cache.File.t) ->
                        c.in_the_build_directory)
                  ]
              else
                List.iter2 targets cached
                  ~f:(fun (_, digest) (c : Dune_cache.File.t) ->
                    if not (Digest.equal digest c.digest) then
                      User_warning.emit
                        [ Pp.textf "cache mismatch on %s: hash differ with %s"
                            (Path.Build.to_string_maybe_quoted
                               c.in_the_build_directory)
                            (Path.Build.to_string_maybe_quoted
                               c.in_the_build_directory)
                        ])
            | _ -> ()
          in
          let f { cache = (module Caching : Dune_cache.Caching); _ } =
            ignore
              (Caching.Cache.promote Caching.cache targets rule_digest []
                 ~repository:None)
          in
          Option.iter ~f t.caching;
          let dynamic_deps_stages =
            List.map exec_result.dynamic_deps_stages ~f:(fun deps ->
                ( deps
                , Action_exec.Dynamic_dep.Set.to_dep_set deps
                  |> compute_dependencies_digest ~sandbox_mode ~env ~eval_pred
                ))
          in
          Trace_db.set (Path.build head_target)
            { rule_digest; dynamic_deps_stages; targets_digest }
        )
      ) else
        Fiber.return ()
    in
    let+ () =
      match (mode, !Clflags.promote) with
      | (Standard | Fallback | Ignore_source_files), _
      | Promote _, Some Never ->
        Fiber.return ()
      | Promote { lifetime; into; only }, (Some Automatically | None) ->
        Fiber.sequential_iter targets_as_list ~f:(fun path ->
            let consider_for_promotion =
              match only with
              | None -> true
              | Some pred ->
                Predicate_lang.Glob.exec pred
                  (Path.reach (Path.build path) ~from:(Path.build dir))
                  ~standard:Predicate_lang.any
            in
            match consider_for_promotion with
            | false -> Fiber.return ()
            | true -> (
              let in_source_tree = Path.Build.drop_build_context_exn path in
              let in_source_tree =
                match into with
                | None -> in_source_tree
                | Some { loc; dir } ->
                  Path.Source.relative
                    (Path.Source.relative
                       (Path.Source.parent_exn in_source_tree)
                       dir ~error_loc:loc)
                    (Path.Source.basename in_source_tree)
              in
              let path = Path.build path in
              let in_source_tree = Path.source in_source_tree in
              match
                Path.exists in_source_tree
                && Cached_digest.file path = Cached_digest.file in_source_tree
              with
              | true -> Fiber.return ()
              | false ->
                if lifetime = Until_clean then
                  Promoted_to_delete.add in_source_tree;
                Scheduler.ignore_for_watch in_source_tree;
                Artifact_substitution.copy_file () ~src:path
                  ~dst:in_source_tree ~get_vcs:File_tree.nearest_vcs ))
    in
    t.rule_done <- t.rule_done + 1

  (* a rule can have multiple files, but rule.run_rule may only be called once *)
  let build_file_impl path =
    let t = t () in
    let on_error exn = Dep_path.reraise exn (Path path) in
    Fiber.with_error_handler ~on_error (fun () ->
        get_rule t path
        >>= function
        | None ->
          (* file already exists *)
          Fiber.return ()
        | Some rule -> execute_rule rule)

  module Pred = struct
    let build_impl g =
      Pred.eval g |> Path.Set.to_list |> Fiber.parallel_iter ~f:build_file

    let eval_impl g =
      let dir = File_selector.dir g in
      Path.Set.filter (targets_of ~dir) ~f:(File_selector.test g)

    let eval =
      Memo.exec
        (Memo.create "eval-pred" ~doc:"Evaluate a predicate in a directory"
           ~input:(module File_selector)
           ~output:(Allow_cutoff (module Path.Set))
           ~visibility:Hidden Sync eval_impl)

    let build =
      Memo.exec
        (Memo.create "build-pred" ~doc:"build a predicate"
           ~input:(module File_selector)
           ~output:(Allow_cutoff (module Unit))
           ~visibility:Hidden Async build_impl)
  end

  let build_file_memo =
    Memo.create "build-file"
      ~output:(Allow_cutoff (module Unit))
      ~doc:"Build a file."
      ~input:(module Path)
      ~visibility:(Public Dpath.decode) Async build_file_impl

  let build_file = Memo.exec build_file_memo

  let execute_rule_memo =
    Memo.create "execute-rule"
      ~output:(Allow_cutoff (module Unit))
      ~doc:"-"
      ~input:(module Internal_rule)
      ~visibility:Hidden Async execute_rule_impl

  let execute_rule = Memo.exec execute_rule_memo

  let () =
    Fdecl.set Rule_fn.loc_decl (fun () ->
        let stack = Memo.get_call_stack () in
        List.find_map stack ~f:(fun frame ->
            match
              Memo.Stack_frame.as_instance_of frame ~of_:execute_rule_memo
            with
            | Some input -> Some input
            | None ->
              Memo.Stack_frame.as_instance_of frame
                ~of_:evaluate_action_and_dynamic_deps_memo)
        |> Option.bind ~f:(fun (rule : Internal_rule.t) ->
               Rule.Info.loc rule.info))
end

open Exported

let eval_pred = Pred.eval

let shim_of_build_goal request =
  let request =
    let open Build.O in
    let+ () = request in
    Action.Progn []
  in
  Internal_rule.shim_of_build_goal ~build:request
    ~static_deps:(static_deps request)

let build_request ~request =
  let result = Fdecl.create Dyn.Encoder.opaque in
  let request =
    let open Build.O in
    let+ res = request in
    Fdecl.set result res
  in
  let rule = shim_of_build_goal request in
  let+ _act, _deps = evaluate_rule_and_wait_for_dependencies rule in
  Fdecl.get result

let process_memcycle exn =
  let cycle =
    Memo.Cycle_error.get exn
    |> List.filter_map
         ~f:(Memo.Stack_frame.as_instance_of ~of_:build_file_memo)
  in
  match List.last cycle with
  | None ->
    let frames = Memo.Cycle_error.get exn in
    Code_error.raise "dependency cycle that does not involve any files"
      [ ("frames", Dyn.Encoder.(list Memo.Stack_frame.to_dyn) frames) ]
  | Some last ->
    let first = List.hd cycle in
    let cycle =
      if last = first then
        cycle
      else
        last :: cycle
    in
    User_error.raise
      [ Pp.text "Dependency cycle between the following files:"
      ; Pp.chain cycle ~f:(fun p ->
            Pp.verbatim (Path.to_string_maybe_quoted p))
      ]

module Rule = struct
  module Id = Internal_rule.Id

  module T = struct
    type t =
      { id : Id.t
      ; dir : Path.Build.t
      ; deps : Dep.Set.t
      ; targets : Path.Build.Set.t
      ; context : Context.t option
      ; action : Action.t
      }

    let compare a b = Id.compare a.id b.id

    let to_dyn _ = Dyn.opaque
  end

  include T
  module O = Comparable.Make (T)
  module Set = O.Set
end

let set_packages f =
  let t = t () in
  Fdecl.set t.packages f

let package_deps pkg files =
  let t = t () in
  let rules_seen = ref Internal_rule.Set.empty in
  let rec loop fn acc =
    match Path.as_in_build_dir fn with
    | None ->
      (* if this file isn't in the build dir, it doesnt belong to any packages
         and it doesn't have dependencies that do *)
      acc
    | Some fn ->
      let pkgs = Fdecl.get t.packages fn in
      if Package.Name.Set.is_empty pkgs || Package.Name.Set.mem pkgs pkg then
        loop_deps fn acc
      else
        Package.Name.Set.union acc pkgs
  and loop_deps fn acc =
    match Path.Build.Table.find t.files fn with
    | None -> acc
    | Some ir ->
      if Internal_rule.Set.mem !rules_seen ir then
        acc
      else (
        rules_seen := Internal_rule.Set.add !rules_seen ir;
        (* We know that at this point of execution, all the relevant ivars have
           been filled so the following calls to [X.peek_exn] cannot raise. *)
        let static_deps = Fiber.Once.peek_exn ir.static_deps in
        let static_action_deps = Static_deps.action_deps static_deps in
        let _act, dynamic_action_deps =
          Memo.peek_exn evaluate_action_and_dynamic_deps_memo ir
        in
        let action_deps =
          Path.Set.union
            (Dep.Set.paths static_action_deps ~eval_pred)
            (Dep.Set.paths dynamic_action_deps ~eval_pred)
        in
        Path.Set.fold action_deps ~init:acc ~f:loop
      )
  in
  let open Build.O in
  let+ () = Build.paths_for_rule files in
  (* We know that at this point of execution, all the relevant ivars have been
     filled *)
  Path.Set.fold files ~init:Package.Name.Set.empty ~f:(fun fn acc ->
      match Path.as_in_build_dir fn with
      | None -> acc
      | Some fn -> loop_deps fn acc)

let prefix_rules prefix ~f =
  let targets = Build.targets prefix in
  if not (Path.Build.Set.is_empty targets) then
    Code_error.raise "Build_system.prefix_rules' prefix contains targets"
      [ ("targets", Path.Build.Set.to_dyn targets) ];
  let res, rules = Rules.collect f in
  let open Build.O in
  Rules.produce
    (Rules.map_rules rules ~f:(fun rule ->
         { rule with build = prefix >>> rule.build }));
  res

module Alias = Alias0

let assert_not_in_memoized_function () =
  match Memo.get_call_stack () with
  | [] -> ()
  | stack ->
    Code_error.raise
      "Build_system.entry_point: called inside a memoized function"
      [ ("stack", Dyn.Encoder.list Memo.Stack_frame.to_dyn stack) ]

let process_exn_and_reraise =
  Exn_with_backtrace.map_and_reraise
    ~f:
      (Dep_path.map ~f:(function
        | Memo.Cycle_error.E exn -> process_memcycle exn
        | _ as exn -> exn))

let entry_point_async ~f =
  assert_not_in_memoized_function ();
  Fiber.with_error_handler f ~on_error:process_exn_and_reraise

let entry_point_sync ~f =
  assert_not_in_memoized_function ();
  match Exn_with_backtrace.try_with f with
  | Ok x -> x
  | Error exn -> process_exn_and_reraise exn

let do_build ~request =
  Hooks.End_of_build.once Promotion.finalize;
  entry_point_async ~f:(fun () -> build_request ~request)

let all_targets () =
  let t = t () in
  entry_point_sync ~f:(fun () -> all_targets t)

let targets_of ~dir = entry_point_sync ~f:(fun () -> targets_of ~dir)

let is_target file = Path.Set.mem (targets_of ~dir:(Path.parent_exn file)) file

module Print_rules : sig
  val evaluate_rules :
    recursive:bool -> request:unit Build.t -> Rule.t list Fiber.t
end = struct
  let rules_for_files rules deps =
    Dep.Set.paths deps ~eval_pred
    |> Path.Set.fold ~init:Rule.Set.empty ~f:(fun path acc ->
           match
             Path.as_in_build_dir path
             |> Option.bind ~f:(Path.Build.Map.find rules)
           with
           | None -> acc
           | Some rule -> Rule.Set.add acc rule)
    |> Rule.Set.to_list

  let evaluate_rules ~recursive ~request =
    entry_point_sync ~f:(fun () ->
        let rules = ref Internal_rule.Id.Map.empty in
        let rec run_rule (rule : Internal_rule.t) =
          if Internal_rule.Id.Map.mem !rules rule.id then
            Fiber.return ()
          else
            let* action, deps = evaluate_rule rule in
            let rule =
              { Rule.id = rule.id
              ; dir = rule.dir
              ; deps
              ; targets = rule.targets
              ; context = rule.context
              ; action
              }
            in
            rules := Internal_rule.Id.Map.set !rules rule.id rule;
            if recursive then
              Dep.Set.parallel_iter_files deps ~f:proc_rule ~eval_pred
            else
              Fiber.return ()
        and proc_rule dep =
          match get_rule_other dep with
          | None -> Fiber.return () (* external files *)
          | Some rule -> run_rule rule
        in
        let rule_shim = shim_of_build_goal request in
        let* _act, goal = evaluate_rule rule_shim in
        let+ () = Dep.Set.parallel_iter_files goal ~f:proc_rule ~eval_pred in
        let rules =
          Internal_rule.Id.Map.fold !rules ~init:Path.Build.Map.empty
            ~f:(fun (r : Rule.t) acc ->
              Path.Build.Set.fold r.targets ~init:acc ~f:(fun fn acc ->
                  Path.Build.Map.set acc fn r))
        in
        match
          Rule.Id.Top_closure.top_closure
            (rules_for_files rules goal)
            ~key:(fun (r : Rule.t) -> r.id)
            ~deps:(fun (r : Rule.t) -> rules_for_files rules r.deps)
        with
        | Ok l -> l
        | Error cycle ->
          User_error.raise
            [ Pp.text "Dependency cycle detected:"
            ; Pp.chain cycle ~f:(fun rule ->
                  Pp.verbatim
                    (Path.to_string_maybe_quoted
                       (Path.build
                          (Path.Build.Set.choose_exn rule.Rule.targets))))
            ])
end

include Print_rules

module All_lib_deps : sig
  val all_lib_deps :
       request:unit Build.t
    -> Lib_deps_info.t Path.Source.Map.t Context_name.Map.t Fiber.t
end = struct
  let static_deps_of_request request =
    Static_deps.paths @@ Build.static_deps request ~list_targets:targets_of

  let rules_for_files paths =
    Path.Set.fold paths ~init:[] ~f:(fun path acc ->
        match get_rule_other path with
        | None -> acc
        | Some rule -> rule :: acc)
    |> Internal_rule.Set.of_list |> Internal_rule.Set.to_list

  let rules_for_targets targets =
    Internal_rule.Id.Top_closure_f.top_closure (rules_for_files targets)
      ~key:(fun (r : Internal_rule.t) -> r.id)
      ~deps:(fun (r : Internal_rule.t) ->
        Fiber.Once.get r.static_deps
        >>| Static_deps.paths ~eval_pred
        >>| rules_for_files)
    >>| function
    | Ok l -> l
    | Error cycle ->
      User_error.raise
        [ Pp.text "Dependency cycle detected:"
        ; Pp.chain cycle ~f:(fun rule ->
              Pp.verbatim
                (Path.to_string_maybe_quoted
                   (Path.build
                      (Path.Build.Set.choose_exn rule.Internal_rule.targets))))
        ]

  let all_lib_deps ~request =
    let t = t () in
    let targets = static_deps_of_request request ~eval_pred in
    let* rules = rules_for_targets targets in
    let+ lib_deps =
      Fiber.parallel_map rules ~f:(fun rule ->
          let+ deps = Internal_rule.lib_deps rule in
          (rule, deps))
    in
    List.fold_left lib_deps ~init:[] ~f:(fun acc (rule, deps) ->
        if Lib_name.Map.is_empty deps then
          acc
        else
          match Path.Build.extract_build_context rule.Internal_rule.dir with
          | None -> acc
          | Some (context, p) ->
            let context = Context_name.of_string context in
            (context, (p, deps)) :: acc)
    |> Context_name.Map.of_list_multi
    |> Context_name.Map.filteri ~f:(fun ctx _ ->
           Context_name.Map.mem t.contexts ctx)
    |> Context_name.Map.map
         ~f:(Path.Source.Map.of_list_reduce ~f:Lib_deps_info.merge)
end

include All_lib_deps

let load_dir_and_produce_its_rules ~dir =
  let loaded = load_dir ~dir in
  match loaded with
  | Non_build _ -> ()
  | Build loaded -> Rules.produce loaded.rules_produced

let load_dir ~dir = load_dir_and_produce_its_rules ~dir

let init ~contexts ?caching ~sandboxing_preference =
  let contexts =
    List.map contexts ~f:(fun c -> (c.Context.name, c))
    |> Context_name.Map.of_list_exn
  in
  let caching =
    let f ({ cache = (module Caching : Dune_cache.Caching); _ } as v) =
      let cache =
        ( module struct
          module Cache = Caching.Cache

          let cache = Caching.Cache.set_build_dir Caching.cache Path.build_dir
        end : Dune_cache.Caching )
      in
      { v with cache }
    in
    Option.map ~f caching
  in
  let t =
    { contexts
    ; files = Path.Build.Table.create 1024
    ; packages = Fdecl.create Dyn.Encoder.opaque
    ; gen_rules = Fdecl.create Dyn.Encoder.opaque
    ; init_rules = Fdecl.create Dyn.Encoder.opaque
    ; caching
    ; sandboxing_preference = sandboxing_preference @ Sandbox_mode.all
    ; rule_done = 0
    ; rule_total = 0
    }
  in
  Console.Status_line.set (fun () ->
      Some
        (Pp.verbatim
           (sprintf "Done: %u/%u (jobs: %u)" t.rule_done t.rule_total
              (Scheduler.running_jobs_count ()))));
  set t
