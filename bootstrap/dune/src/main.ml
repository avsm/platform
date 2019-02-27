open! Stdune
open Import
open Fiber.O

let () = Inline_tests.linkme

type workspace =
  { contexts : Context.t list
  ; conf     : Dune_load.conf
  ; env      : Env.t
  }

type build_system =
  { workspace    : workspace
  ; scontexts    : Super_context.t String.Map.t
  }

let package_install_file w pkg =
  match Package.Name.Map.find w.conf.packages pkg with
  | None -> Error ()
  | Some p ->
    Ok (Path.relative p.path
          (Utils.install_file ~package:p.name ~findlib_toolchain:None))

let setup_env ~capture_outputs =
  let env =
    if capture_outputs || not (Lazy.force Colors.stderr_supports_colors) then
      Env.initial
    else
      Colors.setup_env_for_colors Env.initial
  in
  Env.add env ~var:"INSIDE_DUNE" ~value:"1"

let scan_workspace ?(log=Log.no_log)
      ?workspace ?workspace_file
      ?x
      ?ignore_promoted_rules
      ?(capture_outputs=true)
      ?profile
      () =
  let env = setup_env ~capture_outputs in
  let conf =
    Dune_load.load ?ignore_promoted_rules ()
  in
  let workspace =
    match workspace with
    | Some w -> w
    | None ->
      match workspace_file with
      | Some p ->
        if not (Path.exists p) then
          die "@{<error>Error@}: workspace file %s does not exist"
            (Path.to_string_maybe_quoted p);
        Workspace.load ?x ?profile p
      | None ->
        match
          let p = Path.of_string Workspace.filename in
          Option.some_if (Path.exists p) p
        with
        | Some p -> Workspace.load ?x ?profile p
        | None -> Workspace.default ?x ?profile ()
  in

  Context.create ~env workspace
  >>= fun contexts ->
  List.iter contexts ~f:(fun (ctx : Context.t) ->
    Log.infof log "@[<1>Dune context:@,%a@]@." Sexp.pp
      (Context.to_sexp ctx));
  Fiber.return
    { contexts
    ; conf
    ; env
    }

let init_build_system ?only_packages ?external_lib_deps_mode w =
  Option.iter only_packages ~f:(fun set ->
    Package.Name.Set.iter set ~f:(fun pkg ->
      if not (Package.Name.Map.mem w.conf.packages pkg) then
        let pkg_name = Package.Name.to_string pkg in
        die "@{<error>Error@}: I don't know about package %s \
             (passed through --only-packages/--release)%s"
          pkg_name
          (hint pkg_name
             (Package.Name.Map.keys w.conf.packages
              |> List.map ~f:Package.Name.to_string))));
  let rule_done  = ref 0 in
  let rule_total = ref 0 in
  let gen_status_line () =
    { Scheduler.
      message = Some (sprintf "Done: %u/%u" !rule_done !rule_total)
    ; show_jobs = true
    }
  in
  let hook (hook : Build_system.hook) =
    match hook with
    | Rule_started   -> incr rule_total
    | Rule_completed -> incr rule_done
  in
  Build_system.reset ();
  Build_system.init ~contexts:w.contexts ~file_tree:w.conf.file_tree ~hook;
  Scheduler.set_status_line_generator gen_status_line;
  Gen_rules.gen w.conf
    ~contexts:w.contexts
    ?only_packages
    ?external_lib_deps_mode
  >>| fun scontexts ->
  { workspace = w; scontexts }

let auto_concurrency =
  let v = ref None in
  fun ?(log=Log.no_log) () ->
    match !v with
    | Some n -> Fiber.return n
    | None ->
      (if Sys.win32 then
         match Env.get Env.initial "NUMBER_OF_PROCESSORS" with
         | None -> Fiber.return 1
         | Some s ->
           match int_of_string s with
           | exception _ -> Fiber.return 1
           | n -> Fiber.return n
       else
         let commands =
           [ "nproc", []
           ; "getconf", ["_NPROCESSORS_ONLN"]
           ; "getconf", ["NPROCESSORS_ONLN"]
           ]
         in
         let rec loop = function
           | [] -> Fiber.return 1
           | (prog, args) :: rest ->
             match Bin.which ~path:(Env.path Env.initial) prog with
             | None -> loop rest
             | Some prog ->
               Process.run_capture (Accept All) prog args ~env:Env.initial
                 ~stderr_to:(Process.Output.file Config.dev_null)
               >>= function
               | Error _ -> loop rest
               | Ok s ->
                 match int_of_string (String.trim s) with
                 | n -> Fiber.return n
                 | exception _ -> loop rest
         in
         loop commands)
      >>| fun n ->
      Log.infof log "Auto-detected concurrency: %d" n;
      v := Some n;
      n

let set_concurrency ?log (config : Config.t) =
  (match config.concurrency with
   | Fixed n -> Fiber.return n
   | Auto    -> auto_concurrency ?log ())
  >>| fun n ->
  if n >= 1 then Scheduler.set_concurrency n

(* Called by the script generated by ../build.ml *)
let bootstrap () =
  Colors.setup_err_formatter_colors ();
  Path.set_root Path.External.initial_cwd;
  Path.set_build_dir (Path.Kind.of_string "_boot");
  let main () =
    let anon s = raise (Arg.Bad (Printf.sprintf "don't know what to do with %s\n" s)) in
    let subst () =
      let config : Config.t =
        { display     = Quiet
        ; concurrency = Fixed 1
        }
      in
      Scheduler.go ~config Watermarks.subst;
      exit 0
    in
    let display = ref None in
    let display_mode =
      Arg.Symbol
        (List.map Config.Display.all ~f:fst,
         fun s ->
           display := List.assoc Config.Display.all s)
    in
    let concurrency = ref None in
    let concurrency_arg x =
      match Config.Concurrency.of_string x with
      | Error msg -> raise (Arg.Bad msg)
      | Ok c -> concurrency := Some c
    in
    let profile = ref None in
    Arg.parse
      [ "-j"           , String concurrency_arg, "JOBS concurrency"
      ; "--release"        , Unit (fun () -> profile := Some "release"),
        " set release mode"
      ; "--display"    , display_mode          , " set the display mode"
      ; "--subst"      , Unit subst            ,
        " substitute watermarks in source files"
      ; "--debug-backtraces",
        Set Clflags.debug_backtraces,
        " always print exception backtraces"
      ]
      anon "Usage: boot.exe [-j JOBS] [--dev]\nOptions are:";
    Clflags.debug_dep_path := true;
    let config =
      (* Only load the configuration with --dev *)
      if !profile <> Some "release" then
        Config.load_user_config_file ()
      else
        Config.default
    in
    let config =
      Config.merge config
        { display     = !display
        ; concurrency = !concurrency
        }
    in
    let config =
      Config.adapt_display config
        ~output_is_a_tty:(Lazy.force Colors.stderr_supports_colors)
    in
    let log = Log.create ~display:config.display () in
    Scheduler.go ~log ~config
      (fun () ->
         set_concurrency config
         >>= fun () ->
         scan_workspace ~log ~workspace:(Workspace.default ?profile:!profile ())
           ?profile:!profile
           ()
         >>= init_build_system
         >>= fun _ ->
         Build_system.do_build
           ~request:(Build.path (
             Path.relative Path.build_dir "default/dune.install")))
  in
  try
    main ()
  with
  | Fiber.Never -> exit 1
  | exn ->
    Report_error.report exn;
    exit 1

let find_context_exn t ~name =
  match List.find t.contexts ~f:(fun c -> c.name = name) with
  | Some ctx -> ctx
  | None ->
    die "@{<Error>Error@}: Context %S not found!@." name
