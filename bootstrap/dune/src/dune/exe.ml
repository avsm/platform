open! Stdune
open Import
open Build.O
module CC = Compilation_context
module SC = Super_context

module Program = struct
  type t =
    { name : string
    ; main_module_name : Module_name.t
    ; loc : Loc.t
    }
end

module Linkage = struct
  type t =
    { mode : Link_mode.t
    ; ext : string
    ; flags : string list
    }

  let byte = { mode = Byte; ext = ".bc"; flags = [] }

  let native = { mode = Native; ext = ".exe"; flags = [] }

  let custom context =
    { mode = Byte_with_stubs_statically_linked_in
    ; ext = ".exe"
    ; flags =
        [ Ocaml_version.custom_or_output_complete_exe context.Context.version ]
    }

  let native_or_custom (context : Context.t) =
    match context.ocamlopt with
    | None -> custom context
    | Some _ -> native

  let js = { mode = Byte; ext = ".bc.js"; flags = [] }

  let c_flags = [ "-output-obj" ]

  let o_flags = [ "-output-complete-obj" ]

  let so_flags_windows = o_flags

  let so_flags_unix = [ "-output-complete-obj"; "-runtime-variant"; "_pic" ]

  let of_user_config (ctx : Context.t) (m : Dune_file.Executables.Link_mode.t)
      =
    let link_mode : Link_mode.t =
      match m.mode with
      | Byte ->
        if ctx.disable_dynamically_linked_foreign_archives then
          (* When [disable_dynamically_linked_foreign_archives] is set to
             [true] in the workspace, we link in all stub archives statically
             into the runtime system. *)
          Byte_with_stubs_statically_linked_in
        else
          Byte
      | Native -> Native
      | Best ->
        if Option.is_some ctx.ocamlopt then
          Native
        else
          Byte_with_stubs_statically_linked_in
    in
    let ext =
      let same_as_mode : Mode.t =
        match m.mode with
        | Byte -> Byte
        | Native
        | Best ->
          (* From the point of view of the extension, [native] and [best] are
             the same *)
          Native
      in
      match (same_as_mode, m.kind) with
      | Byte, C -> ".bc.c"
      | Native, C ->
        User_error.raise ~loc:m.loc
          [ Pp.text "C file generation only supports bytecode!" ]
      | Byte, Exe -> ".bc"
      | Native, Exe -> ".exe"
      | Byte, Object -> ".bc" ^ ctx.lib_config.ext_obj
      | Native, Object -> ".exe" ^ ctx.lib_config.ext_obj
      | Byte, Shared_object -> ".bc" ^ ctx.lib_config.ext_dll
      | Native, Shared_object -> ctx.lib_config.ext_dll
      | Byte, Js -> ".bc.js"
      | Native, Js ->
        User_error.raise ~loc:m.loc
          [ Pp.text "Javascript generation only supports bytecode!" ]
    in
    let flags =
      match m.kind with
      | C -> c_flags
      | Js -> []
      | Exe -> (
        match link_mode with
        | Byte_with_stubs_statically_linked_in ->
          [ Ocaml_version.custom_or_output_complete_exe ctx.version ]
        | _ -> [] )
      | Object -> o_flags
      | Shared_object -> (
        let so_flags =
          let os_type = Ocaml_config.os_type ctx.ocaml_config in
          if String.equal os_type "Win32" then
            so_flags_windows
          else
            so_flags_unix
        in
        match link_mode with
        | Native ->
          (* The compiler doesn't pass these flags in native mode. This looks
             like a bug in the compiler. *)
          let native_c_libraries =
            Ocaml_config.native_c_libraries ctx.ocaml_config
          in
          List.concat_map native_c_libraries ~f:(fun flag ->
              [ "-cclib"; flag ])
          @ so_flags
        | Byte
        | Byte_with_stubs_statically_linked_in ->
          so_flags )
    in
    { ext; mode = link_mode; flags }
end

let exe_path_from_name cctx ~name ~(linkage : Linkage.t) =
  Path.Build.relative (CC.dir cctx) (name ^ linkage.ext)

let link_exe ~loc ~name ~(linkage : Linkage.t) ~cm_files ~link_time_code_gen
    ~promote ?(link_args = Build.return Command.Args.empty) ?(o_files = [])
    cctx =
  let sctx = CC.super_context cctx in
  let ctx = SC.context sctx in
  let dir = CC.dir cctx in
  let mode = Link_mode.mode linkage.mode in
  let exe = exe_path_from_name cctx ~name ~linkage in
  let compiler = Option.value_exn (Context.compiler ctx mode) in
  let top_sorted_cms = Cm_files.top_sorted_cms cm_files ~mode in
  let fdo_linker_script = Fdo.Linker_script.create cctx (Path.build exe) in
  SC.add_rule sctx ~loc ~dir
    ~mode:
      ( match promote with
      | None -> Standard
      | Some p -> Promote p )
    (let ocaml_flags = Ocaml_flags.get (CC.flags cctx) mode in
     let prefix =
       let dune_version =
         let scope = CC.scope cctx in
         let project = Scope.project scope in
         Dune_project.dune_version project
       in
       if dune_version >= (2, 0) then
         Cm_files.unsorted_objects_and_cms cm_files ~mode |> Build.paths
       else
         Cm_files.top_sorted_objects_and_cms cm_files ~mode
         |> Build.dyn_paths_unit
     in
     prefix
     >>> Command.run ~dir:(Path.build ctx.build_dir) (Ok compiler)
           [ Command.Args.dyn ocaml_flags
           ; A "-o"
           ; Target exe
           ; As linkage.flags
           ; Command.Args.Dyn link_args
           ; Command.of_result_map link_time_code_gen
               ~f:(fun { Link_time_code_gen.to_link; force_linkall } ->
                 S
                   [ As
                       ( if force_linkall then
                         [ "-linkall" ]
                       else
                         [] )
                   ; Lib.Lib_and_module.L.link_flags to_link
                       ~lib_config:ctx.lib_config ~mode:linkage.mode
                   ])
           ; Deps o_files
           ; Dyn (Build.map top_sorted_cms ~f:(fun x -> Command.Args.Deps x))
           ; Fdo.Linker_script.flags fdo_linker_script
           ])

let link_js ~name ~cm_files ~promote cctx =
  let sctx = CC.super_context cctx in
  let expander = CC.expander cctx in
  let js_of_ocaml =
    CC.js_of_ocaml cctx |> Option.value ~default:Dune_file.Js_of_ocaml.default
  in
  let src = exe_path_from_name cctx ~name ~linkage:Linkage.byte in
  let flags =
    Expander.expand_and_eval_set expander js_of_ocaml.flags
      ~standard:(Build.return (Js_of_ocaml_rules.standard sctx))
  in
  let top_sorted_cms = Cm_files.top_sorted_cms cm_files ~mode:Mode.Byte in
  Js_of_ocaml_rules.build_exe cctx ~js_of_ocaml ~src ~cm:top_sorted_cms
    ~flags:(Command.Args.dyn flags) ~promote

let build_and_link_many ~programs ~linkages ~promote ?link_args ?o_files cctx =
  let modules = Compilation_context.modules cctx in
  let dep_graphs = Dep_rules.rules cctx ~modules in
  Module_compilation.build_all cctx ~dep_graphs;
  let link_time_code_gen = Link_time_code_gen.handle_special_libs cctx in
  List.iter programs ~f:(fun { Program.name; main_module_name; loc } ->
      let cm_files =
        let sctx = CC.super_context cctx in
        let ctx = SC.context sctx in
        let obj_dir = CC.obj_dir cctx in
        let top_sorted_modules =
          let main =
            Option.value_exn (Modules.find modules main_module_name)
          in
          Dep_graph.top_closed_implementations dep_graphs.impl [ main ]
        in
        Cm_files.make ~obj_dir ~modules ~top_sorted_modules
          ~ext_obj:ctx.lib_config.ext_obj
      in
      List.iter linkages ~f:(fun linkage ->
          if linkage = Linkage.js then
            link_js ~name ~cm_files ~promote cctx
          else
            link_exe cctx ~loc ~name ~linkage ~cm_files ~link_time_code_gen
              ~promote ?link_args ?o_files))

let build_and_link ~program = build_and_link_many ~programs:[ program ]

let exe_path cctx ~(program : Program.t) ~linkage =
  exe_path_from_name cctx ~name:program.name ~linkage
