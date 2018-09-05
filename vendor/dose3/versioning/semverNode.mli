(**************************************************************************************)
(*  Copyright (C) 2009-2015 Pietro Abate <pietro.abate@pps.univ-paris-diderot.fr>     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)
 
(** this functions follow the semantic versioning specification http://semver.org/ *)

(** Raw version components. Raw versions are not strictly semantic versions, but
    can also contains characters as 'x' and 'X' . Raw versions must be converted
    to semantic versions. *)
type raw_version = (string * string * string * string list * string list)

type ident = S of string | N of int
type version = {
  major: int;
  minor: int;
  patch: int;
  pre: ident list;
  build: string list;
}

(** Parses a string into a version. 
    Fail if the version can not be parsed *)
val parse_raw_version : string -> raw_version

(** Parses a string into a version. 
    Fail if the version can not be parsed *)
val parse_version : string -> version

(** Raise Failure if the string cannot be converted *)
val convert : raw_version -> version

(** recompose a version string. For all v:
    [equal(v,compose(parse_version v)) = true].  There may, however, be
    small syntactic differences between [v] and [compose(parse_version v)] *)
val compose : version -> string

(** Compare two versions. Raw versions must be converted to be compared *)
val compare_version : version -> version -> int

(** Compare two versions. Fail if one of the versions cannot be 
    parsed or compared *)
val compare : string -> string -> int

(** Equality between two versions *)
val equal : string -> string -> bool
