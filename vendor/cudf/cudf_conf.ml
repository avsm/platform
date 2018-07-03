(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009-2012  Stefano Zacchiroli <zack@upsilon.cc>            *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

open Cudf_types

type stanza_typedecl = (string * typedecl) list

let preamble_typedecl = [
  "preamble",		`String None ;
  "property",		`Typedecl (Some []) ;
  "univ-checksum",	`String (Some "") ;
  "status-checksum",	`String (Some "") ;
  "req-checksum",	`String (Some "") ;
]

let package_typedecl = [
  "package",		`Pkgname None ;
  "version",		`Posint None ;
  "depends",		`Vpkgformula (Some []) ;
  "conflicts",		`Vpkglist (Some []) ;
  "provides",		`Veqpkglist (Some []) ;
  "installed",		`Bool (Some false) ;
  "was-installed",	`Bool (Some false) ;
  "keep",		`Enum (keep_enums, Some "none") ;
]

let request_typedecl = [
  "request",		`String None ;
  "install",		`Vpkglist (Some []) ;
  "remove",		`Vpkglist (Some []) ;
  "upgrade",		`Vpkglist (Some []) ;
]


let stanza_typedecl = [
  "preamble",	preamble_typedecl ;
  "package",	package_typedecl ;
  "request",	request_typedecl ;
]
