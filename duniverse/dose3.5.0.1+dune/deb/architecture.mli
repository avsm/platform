(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2010,2011 Ralf Treinen <ralf.treinen@pps.jussieu.fr>        *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

(** Debian architecture strings, as described in Section 5.6.8 of the 
    Dbian policy. Compliant with Debian policy version 3.9.2 *)

(** check whether a source architecture string matches a host
    architecture. The source architecture string may contain
    architecture wildcards ("linux-any", "any-i386"), or be "any" or
    "all". The host architecure is one that may be obtained by
    running "dpkg-architecture -qDEB_HOST_ARCH").
*)
val src_matches_arch: string -> string -> bool

(** fill the lookup table mapping debian architectures to debian triplets
 *  this function is called by src_matches_arch without supplying anything
 *  for the optional ttfile and ctfile arguments. If they are not None,
 *  then they can point to a file like /usr/share/dpkg/triplettable or
 *  /usr/share/dpkg/cputable respectively.
 *)
val read_triplettable: ?ttfile:(string option) -> ?ctfile:(string option) -> unit -> unit
