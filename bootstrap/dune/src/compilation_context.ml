open! Stdune
open Import

module SC = Super_context

module Includes = struct
  type t = string list Arg_spec.t Cm_kind.Dict.t

  let make sctx ~opaque ~requires : _ Cm_kind.Dict.t =
    match requires with
    | Error exn -> Cm_kind.Dict.make_all (Arg_spec.Dyn (fun _ -> raise exn))
    | Ok libs ->
      let iflags =
        Lib.L.include_flags libs ~stdlib_dir:(SC.context sctx).stdlib_dir
      in
      let cmi_includes =
        Arg_spec.S [ iflags
                   ; Hidden_deps
                       (SC.Libs.file_deps sctx libs ~ext:".cmi")
                   ]
      in
      let cmx_includes =
        Arg_spec.S
          [ iflags
          ; Hidden_deps
              ( if opaque then
                  List.map libs ~f:(fun lib ->
                    (lib, if Lib.is_local lib then
                       ".cmi"
                     else
                       ".cmi-and-.cmx"))
                  |> SC.Libs.file_deps_with_exts sctx
                else
                  SC.Libs.file_deps sctx libs ~ext:".cmi-and-.cmx"
              )
          ]
      in
      { cmi = cmi_includes
      ; cmo = cmi_includes
      ; cmx = cmx_includes
      }

  let empty =
    Cm_kind.Dict.make_all (Arg_spec.As [])
end

type t =
  { super_context        : Super_context.t
  ; scope                : Scope.t
  ; dir                  : Path.t
  ; dir_kind             : File_tree.Dune_file.Kind.t
  ; obj_dir              : Path.t
  ; modules              : Module.t Module.Name.Map.t
  ; alias_module         : Module.t option
  ; lib_interface_module : Module.t option
  ; flags                : Ocaml_flags.t
  ; requires             : Lib.t list Or_exn.t
  ; includes             : Includes.t
  ; preprocessing        : Preprocessing.t
  ; no_keep_locs         : bool
  ; opaque               : bool
  }

let super_context        t = t.super_context
let scope                t = t.scope
let dir                  t = t.dir
let dir_kind             t = t.dir_kind
let obj_dir              t = t.obj_dir
let modules              t = t.modules
let alias_module         t = t.alias_module
let lib_interface_module t = t.lib_interface_module
let flags                t = t.flags
let requires             t = t.requires
let includes             t = t.includes
let preprocessing        t = t.preprocessing
let no_keep_locs         t = t.no_keep_locs
let opaque               t = t.opaque

let context              t = Super_context.context t.super_context

let create ~super_context ~scope ~dir ?(dir_kind=File_tree.Dune_file.Kind.Dune)
      ?(obj_dir=dir) ~modules ?alias_module ?lib_interface_module ~flags
      ~requires ?(preprocessing=Preprocessing.dummy) ?(no_keep_locs=false)
      ~opaque () =
  { super_context
  ; scope
  ; dir
  ; dir_kind
  ; obj_dir
  ; modules
  ; alias_module
  ; lib_interface_module
  ; flags
  ; requires
  ; includes = Includes.make super_context ~opaque ~requires
  ; preprocessing
  ; no_keep_locs
  ; opaque
  }

let for_alias_module t =
  let flags = Ocaml_flags.default ~profile:(SC.profile t.super_context) in
  { t with
    flags        = Ocaml_flags.append_common flags ["-w"; "-49"]
  ; includes     = Includes.empty
  ; alias_module = None
  }
