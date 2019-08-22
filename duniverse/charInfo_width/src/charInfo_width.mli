open CamomileLibraryDefault.Camomile
open Result

module Cfg = Cfg

val width: ?cfg: Cfg.t option -> UChar.t -> int
(** [width c] returns the column width of [c] where [c] is of type [Camomile.UChar.t] and the value returned is of type [int].
    This module is implemented purely in OCaml and follows the prototype of POSIX's wcwidth. i.e. If [c] is a printable character, the value is at least 0. If [c] is null character (L'\0'), the value is 0. Otherwise, -1 is returned.
*)

val width_exn: ?cfg: Cfg.t option -> UChar.t -> int
(** when encounter an unprintable character, [width_exn c] raises [Failure "unprintable character"] instead of returning -1. *)

module type UnicodeString_mini = sig
  type t
  val get : t -> int -> UChar.t
  val length : t -> int
end
(** minimization of Camomile.UnicodeString.Type *)

module String : functor (US : UnicodeString_mini) ->
  sig
    val width : ?cfg:Cfg.widthTable option -> US.t -> (int, int) result
    (** [width str] returns the column width of [str] and the value returned is of type [(int, int) result]. When [Ok width] returnted, [width] is the width of [str]. When [Error pos] returned, [pos] is the offset of the left most unprintable character in [str]. *)
  end

