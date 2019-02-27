type t =
  { debug_dep_path        : bool
  ; debug_findlib         : bool
  ; debug_backtraces      : bool
  ; profile               : string option
  ; workspace_file        : Arg.Path.t option
  ; root                  : string
  ; target_prefix         : string
  ; only_packages         : Dune.Package.Name.Set.t option
  ; capture_outputs       : bool
  ; x                     : string option
  ; diff_command          : string option
  ; auto_promote          : bool
  ; force                 : bool
  ; ignore_promoted_rules : bool
  ; build_dir             : string
  ; no_print_directory    : bool
  ; store_orig_src_dir    : bool
  ; (* Original arguments for the external-lib-deps hint *)
    orig_args             : string list
  ; config                : Dune.Config.t
  ; default_target        : string
  (* For build & runtest only *)
  ; watch : bool
  ; stats_trace_file : string option
  }

val prefix_target : t -> string -> string

val set_common : t -> targets:string list -> unit

val set_common_other : t -> targets:string list -> unit

val set_dirs : t -> unit

val help_secs
  : [> `Blocks of [> `P of string | `S of string ] list
    | `P of string
    | `S of string ] list

val footer : [> `Blocks of [> `P of string | `S of string ] list ]

val term : t Cmdliner.Term.t

val context_arg : doc:string -> string Cmdliner.Term.t

val default_build_dir : string
