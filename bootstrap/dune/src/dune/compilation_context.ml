open! Stdune
open Import
module SC = Super_context

module Includes = struct
  type t = Command.Args.dynamic Command.Args.t Cm_kind.Dict.t

  let make ~opaque ~requires : _ Cm_kind.Dict.t =
    match requires with
    | Error exn ->
      Cm_kind.Dict.make_all
        (Command.Args.Fail { fail = (fun () -> raise exn) })
    | Ok libs ->
      let iflags = Lib.L.include_flags libs in
      let cmi_includes =
        Command.Args.memo
          (Command.Args.S
             [ iflags; Hidden_deps (Lib_file_deps.deps libs ~groups:[ Cmi ]) ])
      in
      let cmx_includes =
        Command.Args.memo
          (Command.Args.S
             [ iflags
             ; Hidden_deps
                 ( if opaque then
                   List.map libs ~f:(fun lib ->
                       ( lib
                       , if Lib.is_local lib then
                           [ Lib_file_deps.Group.Cmi ]
                         else
                           [ Cmi; Cmx ] ))
                   |> Lib_file_deps.deps_with_exts
                 else
                   Lib_file_deps.deps libs
                     ~groups:[ Lib_file_deps.Group.Cmi; Cmx ] )
             ])
      in
      { cmi = cmi_includes; cmo = cmi_includes; cmx = cmx_includes }

  let empty = Cm_kind.Dict.make_all Command.Args.empty
end

type t =
  { super_context : Super_context.t
  ; scope : Scope.t
  ; expander : Expander.t
  ; obj_dir : Path.Build.t Obj_dir.t
  ; modules : Modules.t
  ; flags : Ocaml_flags.t
  ; requires_compile : Lib.t list Or_exn.t
  ; requires_link : Lib.t list Or_exn.t Lazy.t
  ; includes : Includes.t
  ; preprocessing : Preprocessing.t
  ; opaque : bool
  ; stdlib : Ocaml_stdlib.t option
  ; js_of_ocaml : Dune_file.Js_of_ocaml.t option
  ; dynlink : bool
  ; sandbox : Sandbox_config.t
  ; package : Package.t option
  ; vimpl : Vimpl.t option
  ; modes : Mode.Dict.Set.t
  }

let super_context t = t.super_context

let scope t = t.scope

let expander t = t.expander

let dir t = Obj_dir.dir t.obj_dir

let obj_dir t = t.obj_dir

let modules t = t.modules

let flags t = t.flags

let requires_compile t = t.requires_compile

let requires_link t = Lazy.force t.requires_link

let includes t = t.includes

let preprocessing t = t.preprocessing

let opaque t = t.opaque

let stdlib t = t.stdlib

let js_of_ocaml t = t.js_of_ocaml

let dynlink t = t.dynlink

let sandbox t = t.sandbox

let package t = t.package

let vimpl t = t.vimpl

let modes t = t.modes

let context t = Super_context.context t.super_context

let create ~super_context ~scope ~expander ~obj_dir ~modules ~flags
    ~requires_compile ~requires_link ?(preprocessing = Preprocessing.dummy)
    ~opaque ?stdlib ~js_of_ocaml ~dynlink ~package ?vimpl ?modes () =
  let requires_compile =
    if Dune_project.implicit_transitive_deps (Scope.project scope) then
      Lazy.force requires_link
    else
      requires_compile
  in
  let sandbox =
    (* With sandboxing, there are a few build errors in ocaml platform
       1162238ae like: File "ocaml_modules/ocamlgraph/src/pack.ml", line 1:
       Error: The implementation ocaml_modules/ocamlgraph/src/pack.ml does not
       match the interface
       ocaml_modules/ocamlgraph/src/.graph.objs/byte/graph__Pack.cmi: *)
    Sandbox_config.no_sandboxing
  in
  let modes =
    let default =
      Mode.Dict.make_both (Some Dune_file.Mode_conf.Kind.Inherited)
    in
    Option.value ~default modes |> Mode.Dict.map ~f:Option.is_some
  in
  { super_context
  ; scope
  ; expander
  ; obj_dir
  ; modules
  ; flags
  ; requires_compile
  ; requires_link
  ; includes = Includes.make ~opaque ~requires:requires_compile
  ; preprocessing
  ; opaque
  ; stdlib
  ; js_of_ocaml
  ; dynlink
  ; sandbox
  ; package
  ; vimpl
  ; modes
  }

let for_alias_module t =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    Ocaml_flags.default ~profile:(SC.profile t.super_context) ~dune_version
  in
  let sandbox =
    let ctx = Super_context.context t.super_context in
    (* If the compiler reads the cmi for module alias even with [-w -49
       -no-alias-deps], we must sandbox the build of the alias module since the
       modules it references are built after. *)
    if Ocaml_version.always_reads_alias_cmi ctx.version then
      Sandbox_config.needs_sandboxing
    else
      Sandbox_config.no_special_requirements
  in
  { t with
    flags =
      Ocaml_flags.append_common flags
        [ "-w"; "-49"; "-nopervasives"; "-nostdlib" ]
  ; includes = Includes.empty
  ; stdlib = None
  ; sandbox
  }

let for_wrapped_compat t = { t with includes = Includes.empty; stdlib = None }
