type t =
  | Atom of string
  | List of t list

module To_sexp : sig
  type sexp
  include Sexp_intf.Combinators with type 'a t = 'a -> t

  val record : (string * sexp) list -> sexp

  val unknown : _ t
end with type sexp := t

val to_string : t -> string

val pp : Format.formatter -> t -> unit
