
(** Compare two versions in the strings using a strict semantic versioning
 * parser*)
val compare : string -> string -> int

(** Equality between two versions parsing them using the strict definition of
 * the semantic versioning.
 * *)
val equal : string -> string -> bool
