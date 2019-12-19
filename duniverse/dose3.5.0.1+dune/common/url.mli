(****************************************************************************)
(*  Copyright (C) 2011, 2012, 2013 Ralf Treinen                             *)
(*                       <ralf.treinen@pps.univ-paris-diedrot.fr>           *)
(*                                                                          *)
(*  This library is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU Lesser General Public License as          *)
(*  published by the Free Software Foundation, either version 3 of the      *)
(*  License, or (at your option) any later version.  A special linking      *)
(*  exception to the GNU Lesser General Public License applies to this      *)
(*  library, see the COPYING file for more information.                     *)
(****************************************************************************)

type debtypes = [ `Edsp | `Deb | `DebSrc ]
type rpmtypes = [ `Synthesis | `Hdlist ]
type othertypes = [ `Pef | `Csw | `Opam | `Npm ]

type filetypes = [ `Cudf | debtypes | rpmtypes | othertypes ]

val supported_input_types : filetypes list

type url = {
  scheme : filetypes;
  path   : string; (** db name or filename *)
};;

(* parsing of a string as url. Raises Invalid_url in case of error *)
val of_string: string -> url
exception Invalid_url of string;;

(* printable representation of an url *)
val to_string: url -> string
val scheme_to_string: filetypes -> string
val scheme_of_string: string -> filetypes
