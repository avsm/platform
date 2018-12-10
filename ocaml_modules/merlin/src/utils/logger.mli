(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

(** Log module
  *
  * 1. Provide functions to log arbitrary messages, filtered according to a
  * section and a verbosity level.
  *
  * 2. Allow to setup a destination for these log messages.
  *
  **)

type title = string
type section = string

val log    : section -> title -> string -> unit
val logf   : section -> title -> ('b, unit, string, unit) format4 -> 'b
val logfmt : section -> title -> (Format.formatter -> unit) -> unit
val logj   : section -> title -> (unit -> Std.json) -> unit
val log_flush : unit -> unit

val notify : section -> ('b, unit, string, unit) format4 -> 'b
val with_notifications : (section * string) list ref -> (unit -> 'a) -> 'a
val with_log_file : string option -> (unit -> 'a) -> 'a
