(** Representation and parsing of Dune files *)

open! Stdune
open Import

module Preprocess : sig
  module Pps : sig
    type t =
      { loc : Loc.t
      ; pps : (Loc.t * Lib_name.t) list
      ; flags : String_with_vars.t list
      ; staged : bool
      }

    val compare_no_locs : t -> t -> Ordering.t
  end

  type t =
    | No_preprocessing
    | Action of Loc.t * Action_dune_lang.t
    | Pps of Pps.t
    | Future_syntax of Loc.t

  module Without_future_syntax : sig
    type t =
      | No_preprocessing
      | Action of Loc.t * Action_dune_lang.t
      | Pps of Pps.t
  end

  val loc : t -> Loc.t option

  module Pp_flag_consumer : sig
    type t =
      | Compiler
      | Merlin
  end

  val remove_future_syntax :
    t -> for_:Pp_flag_consumer.t -> Ocaml_version.t -> Without_future_syntax.t
end

module Preprocess_map : sig
  type t = Preprocess.t Module_name.Per_item.t

  val decode : t Dune_lang.Decoder.t

  val no_preprocessing : t

  val default : t

  (** [find module_name] find the preprocessing specification for a given
      module *)
  val find : Module_name.t -> t -> Preprocess.t

  val pps : t -> (Loc.t * Lib_name.t) list
end

module Lint : sig
  type t = Preprocess_map.t

  val no_lint : t
end

module Js_of_ocaml : sig
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; javascript_files : string list
    }

  val default : t
end

module Lib_deps : sig
  type nonrec t = Lib_dep.t list

  val of_pps : Lib_name.t list -> t

  val info : t -> kind:Lib_deps_info.Kind.t -> Lib_deps_info.t

  val decode : allow_re_export:bool -> t Dune_lang.Decoder.t
end

(** [preprocess] and [preprocessor_deps] fields *)
val preprocess_fields :
  (Preprocess_map.t * Dep_conf.t list) Dune_lang.Decoder.fields_parser

module Buildable : sig
  type t =
    { loc : Loc.t
    ; modules : Ordered_set_lang.t
    ; modules_without_implementation : Ordered_set_lang.t
    ; libraries : Lib_dep.t list
    ; foreign_archives : (Loc.t * string) list
    ; foreign_stubs : Foreign.Stubs.t list
    ; preprocess : Preprocess_map.t
    ; preprocessor_deps : Dep_conf.t list
    ; lint : Lint.t
    ; flags : Ocaml_flags.Spec.t
    ; js_of_ocaml : Js_of_ocaml.t
    ; allow_overlapping_dependencies : bool
    }

  (** Check if the buildable has any foreign stubs or archives. *)
  val has_foreign : t -> bool

  (** Preprocessing specification used by all modules or [No_preprocessing] *)
  val single_preprocess : t -> Preprocess.t
end

module Public_lib : sig
  type t =
    { name : Loc.t * Lib_name.t  (** Full public name *)
    ; package : Package.t  (** Package it is part of *)
    ; sub_dir : string option
          (** Subdirectory inside the installation directory *)
    }

  val name : t -> Lib_name.t

  val package : t -> Package.t
end

module Mode_conf : sig
  type t =
    | Byte
    | Native
    | Best  (** [Native] if available and [Byte] if not *)

  val decode : t Dune_lang.Decoder.t

  val compare : t -> t -> Ordering.t

  val to_dyn : t -> Dyn.t

  module Kind : sig
    type t =
      | Inherited
      | Requested of Loc.t
  end

  module Map : sig
    type nonrec 'a t =
      { byte : 'a
      ; native : 'a
      ; best : 'a
      }
  end

  module Set : sig
    type nonrec t = Kind.t option Map.t

    val decode : t Dune_lang.Decoder.t

    module Details : sig
      type t = Kind.t option
    end

    val eval_detailed : t -> has_native:bool -> Details.t Mode.Dict.t

    val eval : t -> has_native:bool -> Mode.Dict.Set.t
  end
end

module External_variant : sig
  type t =
    { implementation : Loc.t * Lib_name.t
    ; virtual_lib : Loc.t * Lib_name.t
    ; variant : Variant.t
    ; project : Dune_project.t
    ; loc : Loc.t
    }
end

module Library : sig
  type t =
    { name : Loc.t * Lib_name.Local.t
    ; public : Public_lib.t option
    ; synopsis : string option
    ; install_c_headers : string list
    ; ppx_runtime_libraries : (Loc.t * Lib_name.t) list
    ; modes : Mode_conf.Set.t
    ; kind : Lib_kind.t
          (* TODO: It may be worth remaming [c_library_flags] to
             [link_time_flags_for_c_compiler] and [library_flags] to
             [link_time_flags_for_ocaml_compiler], both here and in the Dune
             language, to make it easier to understand the purpose of various
             flags. Also we could add [c_library_flags] to [Foreign.Stubs.t]. *)
    ; library_flags : Ordered_set_lang.Unexpanded.t
    ; c_library_flags : Ordered_set_lang.Unexpanded.t
    ; virtual_deps : (Loc.t * Lib_name.t) list
    ; wrapped : Wrapped.t Lib_info.Inherited.t
    ; optional : bool
    ; buildable : Buildable.t
    ; dynlink : Dynlink_supported.t
    ; project : Dune_project.t
    ; sub_systems : Sub_system_info.t Sub_system_name.Map.t
    ; dune_version : Dune_lang.Syntax.Version.t
    ; virtual_modules : Ordered_set_lang.t option
    ; implements : (Loc.t * Lib_name.t) option
    ; variant : Variant.t option
    ; default_implementation : (Loc.t * Lib_name.t) option
    ; private_modules : Ordered_set_lang.t option
    ; stdlib : Ocaml_stdlib.t option
    ; special_builtin_support : Lib_info.Special_builtin_support.t option
    ; enabled_if : Blang.t
    }

  (** Check if the library has any foreign stubs or archives. *)
  val has_foreign : t -> bool

  (** The name of the automatically built foreign stubs archive. *)
  val stubs_archive_name : t -> string

  (** The names of all foreign archives, including the foreign stubs archive. *)
  val foreign_archive_names : t -> string list

  (** The [lib*.a] files of all foreign archives, including foreign stubs.
      [dir] is the directory the library is declared in. *)
  val foreign_archives :
    t -> dir:Path.Build.t -> ext_lib:string -> Path.Build.t list

  (** The [dll*.so] files of all foreign archives, including foreign stubs.
      [dir] is the directory the library is declared in. *)
  val foreign_dll_files :
    t -> dir:Path.Build.t -> ext_dll:string -> Path.Build.t list

  (** The path to a library archive. [dir] is the directory the library is
      declared in. *)
  val archive : t -> dir:Path.Build.t -> ext:string -> Path.Build.t

  val best_name : t -> Lib_name.t

  val is_virtual : t -> bool

  val is_impl : t -> bool

  val obj_dir : dir:Path.Build.t -> t -> Path.Build.t Obj_dir.t

  val main_module_name : t -> Lib_info.Main_module_name.t

  val to_lib_info :
       t
    -> dir:Path.Build.t
    -> lib_config:Lib_config.t
    -> known_implementations:(Loc.t * Lib_name.t) Variant.Map.t
    -> Lib_info.local
end

module Install_conf : sig
  type 'file t =
    { section : Install.Section.t
    ; files : 'file list
    ; package : Package.t
    }
end

module Executables : sig
  module Link_mode : sig
    type t =
      { mode : Mode_conf.t
      ; kind : Binary_kind.t
      ; loc : Loc.t
      }

    include Dune_lang.Conv.S with type t := t

    val exe : t

    val object_ : t

    val shared_object : t

    val byte : t

    val native : t

    val byte_exe : t

    val js : t

    val compare : t -> t -> Ordering.t

    val to_dyn : t -> Dyn.t

    module Set : Set.S with type elt = t
  end

  type t =
    { names : (Loc.t * string) list
    ; link_flags : Ordered_set_lang.Unexpanded.t
    ; link_deps : Dep_conf.t list
    ; modes : Link_mode.Set.t
    ; optional : bool
    ; buildable : Buildable.t
    ; variants : (Loc.t * Variant.Set.t) option
    ; package : Package.t option
    ; promote : Rule.Promote.t option
    ; install_conf : File_binding.Unexpanded.t Install_conf.t option
    ; forbidden_libraries : (Loc.t * Lib_name.t) list
    ; bootstrap_info : string option
    }

  (** Check if the executables have any foreign stubs or archives. *)
  val has_foreign : t -> bool

  val obj_dir : t -> dir:Path.Build.t -> Path.Build.t Obj_dir.t
end

module Menhir : sig
  type t =
    { merge_into : string option
    ; flags : Ordered_set_lang.Unexpanded.t
    ; modules : string list
    ; mode : Rule.Mode.t
    ; loc : Loc.t
    ; infer : bool
    ; enabled_if : Blang.t
    }

  type Stanza.t += T of t
end

module Rule : sig
  module Targets : sig
    module Multiplicity : sig
      type t =
        | One
        | Multiple
    end

    type static =
      { targets : String_with_vars.t list
      ; multiplicity : Multiplicity.t
      }

    type t =
      | Static of static
      | Infer
  end

  type t =
    { targets : Targets.t
    ; deps : Dep_conf.t Bindings.t
    ; action : Loc.t * Action_dune_lang.t
    ; mode : Rule.Mode.t
    ; locks : String_with_vars.t list
    ; loc : Loc.t
    ; enabled_if : Blang.t
    ; alias : Alias.Name.t option
    ; package : Package.t option
    }
end

module Coq : sig
  type t =
    { name : Loc.t * Lib_name.Local.t
    ; public : Public_lib.t option
    ; synopsis : string option
    ; modules : Ordered_set_lang.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; libraries : (Loc.t * Lib_name.t) list  (** ocaml libraries *)
    ; loc : Loc.t
    ; enabled_if : Blang.t
    }

  val best_name : t -> Lib_name.t

  type Stanza.t += T of t
end

module Coqpp : sig
  type t =
    { modules : string list
    ; loc : Loc.t
    }

  type Stanza.t += T of t
end

module Alias_conf : sig
  type t =
    { name : Alias.Name.t
    ; deps : Dep_conf.t Bindings.t
    ; action : (Loc.t * Action_dune_lang.t) option
    ; locks : String_with_vars.t list
    ; package : Package.t option
    ; enabled_if : Blang.t
    ; loc : Loc.t
    }
end

module Copy_files : sig
  type t =
    { add_line_directive : bool
    ; glob : String_with_vars.t
    ; syntax_version : Dune_lang.Syntax.Version.t
    }
end

module Documentation : sig
  type t =
    { loc : Loc.t
    ; package : Package.t
    ; mld_files : Ordered_set_lang.t
    }
end

module Tests : sig
  type t =
    { exes : Executables.t
    ; locks : String_with_vars.t list
    ; package : Package.t option
    ; deps : Dep_conf.t Bindings.t
    ; enabled_if : Blang.t
    ; action : Action_dune_lang.t option
    }
end

module Toplevel : sig
  type t =
    { name : string
    ; libraries : (Loc.t * Lib_name.t) list
    ; variants : (Loc.t * Variant.Set.t) option
    ; loc : Loc.t
    }
end

module Include_subdirs : sig
  type qualification =
    | Unqualified
    | Qualified

  type t =
    | No
    | Include of qualification
end

module Deprecated_library_name : sig
  module Old_public_name : sig
    type t =
      { deprecated : bool
      ; public : Public_lib.t
      }
  end

  type t =
    { loc : Loc.t
    ; project : Dune_project.t
    ; old_public_name : Old_public_name.t
    ; new_public_name : Loc.t * Lib_name.t
    }
end

type Stanza.t +=
  | Library of Library.t
  | Foreign_library of Foreign.Library.t
  | Executables of Executables.t
  | Rule of Rule.t
  | Install of File_binding.Unexpanded.t Install_conf.t
  | Alias of Alias_conf.t
  | Copy_files of Copy_files.t
  | Documentation of Documentation.t
  | Tests of Tests.t
  | Include_subdirs of Loc.t * Include_subdirs.t
  | Toplevel of Toplevel.t
  | External_variant of External_variant.t
  | Deprecated_library_name of Deprecated_library_name.t

val stanza_package : Stanza.t -> Package.t option

module Stanzas : sig
  type t = Stanza.t list

  type syntax =
    | OCaml
    | Plain

  (** [of_ast project ast] is the list of [Stanza.t]s derived from decoding the
      [ast] according to the syntax given by [kind] in the context of the
      [project] *)
  val of_ast : Dune_project.t -> Dune_lang.Ast.t -> Stanza.t list

  (** [parse ~file ~kind project stanza_exprs] is a list of [Stanza.t]s derived
      from decoding the [stanza_exprs] from [Dune_lang.Ast.t]s to [Stanza.t]s.

      [file] is used to check for illegal recursive file inclusions and to
      anchor file includes given as relative paths.

      The stanzas are parsed in the context of the dune [project].

      The syntax [kind] determines whether the expected syntax is the
      depreciated jbuilder syntax or the version of Dune syntax specified by
      the current [project]. *)
  val parse : file:Path.Source.t -> Dune_project.t -> Dune_lang.Ast.t list -> t
end
