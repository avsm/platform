(** Information about a package defined in the workspace *)

open Stdune

module Name : sig
  type t

  val of_string : string -> t

  val opam_fn : t -> string

  val pp : Format.formatter -> t -> unit

  include Interned.S with type t := t

  val t : t Sexp.Of_sexp.t
end

type t =
  { name                   : Name.t
  ; path                   : Path.t
  ; version_from_opam_file : string option
  }

val opam_file : t -> Path.t
