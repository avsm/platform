

(** Parse a criteria field *)
val parse_criteria : Format822.field -> Criteria_types.criteria

(** Return the string encoding the criteria. if the given solver does not
    respect the MISC2012 syntax, the function fails *)
val to_string : ?solver:string -> Criteria_types.criteria -> string

val iter :
  (string * string * string * Re.re option -> unit) ->
  Criteria_types.criteria -> unit

(** Return true is the solver respect the MISC2012 syntax *)
val is_misc2012 : string -> bool

(** An associative list containing the criteria associated to the 
    following shortcuts : 
		- upgrade
		- dist-upgrade
		- install
		- remove
		- paranoid
		- trendy
*)
val default_criteria : (string * Criteria_types.criteria) list
