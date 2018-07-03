(*
 * zed_re.mli
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

(** Regular expressions on ropes *)

open CamomileLibrary

type t
  (** Type of compiled regular expressions. *)

type match_result = (Zed_rope.Zip.t * Zed_rope.Zip.t) option array option
    (** Type of a match result. If the match fail, [None] is
        returned. Otherwise an array of matched sub-strings is
        returned, the index [0] corresponding to the full match, and
        other indexes to matched groups. *)

val compile : URe.regexp -> t
  (** [compile regexp] compiles the given regular expression. *)

val regexp_match : ?sem : URe.match_semantics -> t -> Zed_rope.t -> int -> match_result
  (** [regexp_match ?sem regexp rope pos] tries to match [regexp] on
      given rope, starting at [pos]. *)

val search_forward : ?sem : URe.match_semantics -> t -> Zed_rope.t -> int -> match_result
  (** [search_forward ?sem regexp rope pos] searches the given regular
      expression in [rope] starting at [pos]. *)

val search_backward : ?sem : URe.match_semantics -> t -> Zed_rope.t -> int -> match_result
  (** [search_backward ?sem regexp rope pos] searches the given
      regular expression in [rope] starting at [pos], in reverse
      order. *)
