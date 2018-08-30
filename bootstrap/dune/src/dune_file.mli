(** Representation and parsing of jbuild files *)

open! Stdune
open Import

(** Ppx preprocessors  *)
module Pp : sig
  type t = private Lib_name.t
  val of_string : loc:Loc.t option -> string -> t
  val to_string : t -> string

  val to_lib_name : t -> Lib_name.t
  val compare : t -> t -> Ordering.t
end

module Preprocess : sig
  type pps =
    { loc   : Loc.t
    ; pps   : (Loc.t * Pp.t) list
    ; flags : string list
    ; staged : bool
    }

  type t =
    | No_preprocessing
    | Action of Loc.t * Action.Unexpanded.t
    | Pps    of pps
end

module Per_module : Per_item.S with type key = Module.Name.t

module Preprocess_map : sig
  type t = Preprocess.t Per_module.t

  val no_preprocessing : t
  val default : t

  (** [find module_name] find the preprocessing specification for a
      given module *)
  val find : Module.Name.t -> t -> Preprocess.t

  val pps : t -> (Loc.t * Pp.t) list
end

module Lint : sig
  type t = Preprocess_map.t

  val no_lint : t
end


module Js_of_ocaml : sig
  type t =
    { flags            : Ordered_set_lang.Unexpanded.t
    ; javascript_files : string list
    }

  val default : t
end

module Lib_dep : sig
  type choice =
    { required  : Lib_name.Set.t
    ; forbidden : Lib_name.Set.t
    ; file      : string
    }

  type select =
    { result_fn : string
    ; choices   : choice list
    ; loc       : Loc.t
    }

  type t =
    | Direct of (Loc.t * Lib_name.t)
    | Select of select

  val to_lib_names : t -> Lib_name.t list
  val direct : Loc.t * Lib_name.t -> t
  val of_pp : Loc.t * Pp.t -> t
end

module Lib_deps : sig
  type t = Lib_dep.t list
  val of_pps : Pp.t list -> t
  val info : t -> kind:Lib_deps_info.Kind.t -> Lib_deps_info.t
end

module Bindings : sig
  type 'a one =
    | Unnamed of 'a
    | Named of string * 'a list

  type 'a t = 'a one list

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val find : 'a t -> string -> 'a list option

  val fold : 'a t -> f:('a one -> 'acc -> 'acc) -> init:'acc -> 'acc

  val empty : 'a t

  val to_list : 'a t -> 'a list

  val singleton : 'a -> 'a t

  val dgen : 'a Dsexp.To_sexp.t -> 'a t Dsexp.To_sexp.t

  val to_sexp : 'a Sexp.To_sexp.t -> 'a t Sexp.To_sexp.t
end

module Dep_conf : sig
  type t =
    | File of String_with_vars.t
    | Alias of String_with_vars.t
    | Alias_rec of String_with_vars.t
    | Glob_files of String_with_vars.t
    | Source_tree of String_with_vars.t
    | Package of String_with_vars.t
    | Universe

  val remove_locs : t -> t

  include Dsexp.Sexpable with type t := t
  val to_sexp : t Sexp.To_sexp.t
end

module Buildable : sig
  type t =
    { loc                      : Loc.t
    ; modules                  : Ordered_set_lang.t
    ; modules_without_implementation : Ordered_set_lang.t
    ; libraries                : Lib_dep.t list
    ; preprocess               : Preprocess_map.t
    ; preprocessor_deps        : Dep_conf.t list
    ; lint                     : Lint.t
    ; flags                    : Ordered_set_lang.Unexpanded.t
    ; ocamlc_flags             : Ordered_set_lang.Unexpanded.t
    ; ocamlopt_flags           : Ordered_set_lang.Unexpanded.t
    ; js_of_ocaml              : Js_of_ocaml.t
    ; allow_overlapping_dependencies : bool
    }

  (** Preprocessing specification used by all modules or [No_preprocessing] *)
  val single_preprocess : t -> Preprocess.t
end

module Public_lib : sig
  type t =
    { name    : Loc.t * Lib_name.t (** Full public name *)
    ; package : Package.t      (** Package it is part of *)
    ; sub_dir : string option  (** Subdirectory inside the installation
                                   directory *)
    }

  val name : t -> Lib_name.t
end

module Sub_system_info : sig
  (** The type of all kind of sub-system information.
      This type is what we get just after parsing a [jbuild] file. *)
  type t = ..
  type sub_system = t = ..

  (** What the user must provide in order to define the parsing part
      of a sub-system. *)
  module type S = sig
    type t
    type sub_system += T of t

    (** Name of the sub-system *)
    val name : Sub_system_name.t

    (** Location of the parameters in the jbuild/dune file. *)
    val loc : t -> Loc.t

    (** Syntax for jbuild/dune files *)
    val syntax : Syntax.t

    (** Parse parameters written by the user in jbuid/dune files *)
    val parse : t Dsexp.Of_sexp.t
  end

  module Register(M : S) : sig end

  val get : Sub_system_name.t -> (module S)
end

module Mode_conf : sig
  type t =
    | Byte
    | Native
    | Best (** [Native] if available and [Byte] if not *)

  val dparse : t Dsexp.Of_sexp.t
  val compare : t -> t -> Ordering.t
  val pp : Format.formatter -> t -> unit

  module Set : sig
    include Set.S with type elt = t
    val dparse : t Dsexp.Of_sexp.t

    (** Both Byte and Native *)
    val default : t

    val eval : t -> has_native:bool -> Mode.Dict.Set.t
  end
end

module Library : sig
  module Kind : sig
    type t =
      | Normal
      | Ppx_deriver
      | Ppx_rewriter
  end

  type t =
    { name                     : Lib_name.Local.t
    ; public                   : Public_lib.t option
    ; synopsis                 : string option
    ; install_c_headers        : string list
    ; ppx_runtime_libraries    : (Loc.t * Lib_name.t) list
    ; modes                    : Mode_conf.Set.t
    ; kind                     : Kind.t
    ; c_flags                  : Ordered_set_lang.Unexpanded.t
    ; c_names                  : (Loc.t * string) list
    ; cxx_flags                : Ordered_set_lang.Unexpanded.t
    ; cxx_names                : (Loc.t * string) list
    ; library_flags            : Ordered_set_lang.Unexpanded.t
    ; c_library_flags          : Ordered_set_lang.Unexpanded.t
    ; self_build_stubs_archive : string option
    ; virtual_deps             : (Loc.t * Lib_name.t) list
    ; wrapped                  : bool
    ; optional                 : bool
    ; buildable                : Buildable.t
    ; dynlink                  : Dynlink_supported.t
    ; project                  : Dune_project.t
    ; sub_systems              : Sub_system_info.t Sub_system_name.Map.t
    ; no_keep_locs             : bool
    ; dune_version             : Syntax.Version.t
    ; virtual_modules          : Ordered_set_lang.t option
    ; implements               : (Loc.t * string) option
    }

  val has_stubs : t -> bool
  val stubs_archive : t -> dir:Path.t -> ext_lib:string -> Path.t
  val dll : t -> dir:Path.t -> ext_dll:string -> Path.t
  val archive : t -> dir:Path.t -> ext:string -> Path.t
  val best_name : t -> Lib_name.t
end

module Install_conf : sig
  type file =
    { src : string
    ; dst : string option
    }

  type t =
    { section : Install.Section.t
    ; files   : file list
    ; package : Package.t
    }
end

module Executables : sig
  module Link_mode : sig
    type t =
      { mode : Mode_conf.t
      ; kind : Binary_kind.t
      }

    include Dsexp.Sexpable with type t := t

    val exe           : t
    val object_       : t
    val shared_object : t
    val byte          : t
    val native        : t

    val compare : t -> t -> Ordering.t

    module Set : Set.S with type elt = t
  end

  type t =
    { names      : (Loc.t * string) list
    ; link_flags : Ordered_set_lang.Unexpanded.t
    ; link_deps  : Dep_conf.t list
    ; modes      : Link_mode.Set.t
    ; buildable  : Buildable.t
    }
end

module Rule : sig
  module Targets : sig
    type t =
      | Static of string list
      | Infer
  end

  module Mode : sig
    type t =
      | Standard
      (** Only use this rule if  the source files don't exist. *)
      | Fallback
      (** Silently promote the targets to the source tree. *)
      | Promote
      (** Same as [Promote] but [jbuilder clean] must delete the file *)
      | Promote_but_delete_on_clean
      (** Same as [Standard] however this is not a rule stanza, so it
          is not possible to add a [(fallback)] field to the rule. *)
      | Not_a_rule_stanza
      (** Just ignore the source files entirely. This is for cases
          where the targets are promoted only in a specific context,
          such as for .install files. *)
      | Ignore_source_files
  end

  type t =
    { targets  : Targets.t
    ; deps     : Dep_conf.t Bindings.t
    ; action   : Loc.t * Action.Unexpanded.t
    ; mode     : Mode.t
    ; locks    : String_with_vars.t list
    ; loc      : Loc.t
    }
end

module Menhir : sig
  type t =
    { merge_into : string option
    ; flags      : Ordered_set_lang.Unexpanded.t
    ; modules    : string list
    ; mode       : Rule.Mode.t
    ; loc        : Loc.t
    }

  type Stanza.t += T of t
end

module Alias_conf : sig
  type t =
    { name    : string
    ; deps    : Dep_conf.t Bindings.t
    ; action  : (Loc.t * Action.Unexpanded.t) option
    ; locks   : String_with_vars.t list
    ; package : Package.t option
    ; enabled_if : String_with_vars.t Blang.t option
    ; loc : Loc.t
    }
end

module Copy_files : sig
  type t =
    { add_line_directive : bool
    ; glob : String_with_vars.t
    }
end

module Documentation : sig
  type t =
    { loc         : Loc.t
    ; package     : Package.t
    ; mld_files   : Ordered_set_lang.t
    }
end

module Tests : sig
  type t =
    { exes       : Executables.t
    ; locks      : String_with_vars.t list
    ; package    : Package.t option
    ; deps       : Dep_conf.t Bindings.t
    ; enabled_if : String_with_vars.t Blang.t option
    }
end

module Include_subdirs : sig
  type t = No | Unqualified
end

type Stanza.t +=
  | Library         of Library.t
  | Executables     of Executables.t
  | Rule            of Rule.t
  | Install         of Install_conf.t
  | Alias           of Alias_conf.t
  | Copy_files      of Copy_files.t
  | Documentation   of Documentation.t
  | Tests           of Tests.t
  | Include_subdirs of Loc.t * Include_subdirs.t

module Stanzas : sig
  type t = Stanza.t list

  type syntax = OCaml | Plain

  val parse
    :  file:Path.t
    -> kind:File_tree.Dune_file.Kind.t
    -> Dune_project.t
    -> Dsexp.Ast.t list
    -> t
end
