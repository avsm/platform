(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2011 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
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

(** Debian release files. This file format, and its use in apt, is described
    on http://wiki.debian.org/SecureApt.
*)

(** The type for representing the contents of one release file *)
type release = {
  fname : string;
  origin : string;
  label : string;
  suite : string;
  version : string;
  codename : string;
  date : string;
  architecture : string;
  component : string;
  notauto : bool;
  autoup : bool;
  description : string;
  md5sums : (string * string * string) list;
  sha1 : (string * string * string) list;
  sha256 : (string * string * string) list;
}

(** [parse_release_in filename channel] parses the contents of a release
    file of name [filename] from the input channel [channel]. Returns
    [Some(c)] when [c] is the contents of the release file read from the
    stream, or [None] when the stream contains only whitespace.

    The release file may be signed, but in this case the validity of 
    the signature is not checked!!
*)
val parse_release_in : string -> IO.input -> release option 
