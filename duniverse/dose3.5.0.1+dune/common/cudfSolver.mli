exception Error of string
exception Unsat

(** [execsolver] execute an external cudf solver.
    exec_pat : execution string
    criteria : optimization criteria
    cudf : a cudf document (preamble, universe, request)

    raise UnSat or Error
*)
val execsolver :
  string -> 
    string ->
      Cudf.cudf -> Cudf.preamble option * Cudf.universe
