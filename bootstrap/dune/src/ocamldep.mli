(** ocamldep management *)

module Dep_graph : sig
  type t

  val deps_of
    :  t
    -> Module.t
    -> (unit, Module.t list) Build.t

  val top_closed_implementations
    :  t
    -> Module.t list
    -> (unit, Module.t list) Build.t
end

module Dep_graphs : sig
  type t = Dep_graph.t Ml_kind.Dict.t

  val dummy : Module.t -> t
end

(** Generate ocamldep rules for all the modules in the context. *)
val rules
  :  Compilation_context.t
  -> Dep_graphs.t

(** Compute the dependencies of an auxiliary module. *)
val rules_for_auxiliary_module
  :  Compilation_context.t
  -> Module.t
  -> Dep_graphs.t
