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

open Std

(** Load and cache dot-merlin files **)

type config = {
  dot_merlins  : string list;
  build_path   : string list;
  source_path  : string list;
  cmi_path     : string list;
  cmt_path     : string list;
  packages     : string list;
  flags        : string list with_workdir list;
  extensions   : string list;
  suffixes     : (string * string) list;
  stdlib       : string option;
  findlib      : string option;
  reader       : string list;
  findlib_path : string list;
  findlib_toolchain : string option;
  exclude_query_dir : bool;
}

(** Load one or more .merlin file *)
val load : stdlib:string -> string list -> config

val standard_library : ?conf:string -> ?path:string list -> ?toolchain:string -> unit -> string

val path_of_packages : ?conf:string -> ?path:string list -> ?toolchain:string -> string list ->
  (string list (* packages *) *
   Ppxsetup.t *
   string list (* failures *))

val list_packages : ?conf:string -> ?path:string list -> ?toolchain:string -> unit -> string list
