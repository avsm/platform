(** Command line flags *)

(** Compilation flags for OCaml files *)
(*val ocaml_comp_flags : string list ref*)

(** [-g] *)
val g : bool ref

(** Print dependency path in case of error *)
val debug_dep_path : bool ref

(** Debug the findlib implementation *)
val debug_findlib : bool ref

(** Compiler warnings *)
val warnings : string ref

(** The command line for "Hint: try: jbuilder external-lib-deps ..." *)
val external_lib_deps_hint : string list ref

(** Capture the output of sub-commands *)
val capture_outputs : bool ref

(** Always print backtraces, to help debugging jbuilder itself *)
val debug_backtraces : bool ref

(** Command to use to diff things *)
val diff_command : string option ref

(** Automatically promote files *)
val auto_promote : bool ref

(** Force re-running actions associated to aliases *)
val force : bool ref
