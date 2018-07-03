(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Explicit error handling *)

(** This module provides helpers for values of type [('a, 'b) result Lwt.t].
    The module is experimental and may change in the future. *)

type (+'a, +'b) t = ('a, 'b) Result.result Lwt.t

val return : 'a -> ('a, _) t

val fail : 'b -> (_, 'b) t

val lift : ('a, 'b) Result.result -> ('a, 'b) t

val ok : 'a Lwt.t -> ('a, _) t

val catch : 'a Lwt.t -> ('a, exn) t
(** [catch x] behaves like [return y] if [x] evaluates to [y],
    and like [fail e] if [x] raises [e] *)

val get_exn : ('a, exn) t -> 'a Lwt.t
(** [get_exn] is the opposite of {!catch}: it unwraps the result type,
    returning the value in case of success, calls {!Lwt.fail} in
    case of error. *)

val map : ('a -> 'b) -> ('a,'e) t -> ('b,'e) t

val map_err : ('e1 -> 'e2) -> ('a,'e1) t -> ('a,'e2) t

val bind : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t

val bind_lwt : ('a,'e) t -> ('a -> 'b Lwt.t) -> ('b,'e) t

val bind_lwt_err : ('a,'e1) t -> ('e1 -> 'e2 Lwt.t) -> ('a,'e2) t

val bind_result : ('a,'e) t -> ('a -> ('b,'e) Result.result) -> ('b,'e) t

module Infix : sig
  val (>|=) : ('a,'e) t -> ('a -> 'b) -> ('b,'e) t
  val (>>=) : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
end

include module type of Infix
