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


(** Functions for manipulating and comparing Debian version strings.
    Compliant with Debian policy version 3.9.2. and Debian developers
    reference version 3.4.6 *)

(** {2 Comparing debian version strings} *)

(** The following functions compare any two strings, that is these
    functions do not check whether the arguments are really legal
    debian versions. If the arguments are debian version strings, then
    the result is as required by debian policy. Note that two strings
    may be equivalent, that is denote the same debian version, even
    when they differ in syntax, as for instance "0:1.2.00" and
    "1.02-0".
*)

(** @return [true] iff the two strings define the same version. Hence,
    the result may be true even when the two string differ
    syntactically. *)
val equal : string -> string -> bool

(** [compare x y] returns 0 if x is eqivalent to y, -1 if x is smaller
    than y, and 1 if x is greater than y. This is consistent with
    [Pervasives.compare]. *)
val compare : string -> string -> int

(** {2 Decomposing and recomposing debian version strings} *)

(** Version strings may be the decomposed into epoch, upstream, and
    revision as described in debian policy section 5.6.12. An epoch is
    present if the version string contains a colon ':', in this case
    the leftmost colon separates the epoch from the rest. A revision
    is present if the string contains a dash '-', in this case the
    rightmost dash separates the rest from the revision.

    A version string that contains no dash (hence, no revision) is
    called native. Otherwise it is called non-native, in this case the
    revision may be empty (example: "1.2-") or not.

    A suffix of the upstream part of a native version, or the suffix
    of the revision part of a non-native version, may contain a part
    indicating a binary NMU (Debian Developers Reference, section
    5.10.2.1). A binary NMU part is of the form "+bN" where N is an
    unsigned integer.
*)

(** result type of the analysis of a version string. The binNMU part,
    if present, has been removed from the upstream (if native version)
    or revision (if non-native vesion).
*)
type version_analysis =
  | Native of string*string*string            (** epoch,upstream,binnmu *)
  | NonNative of string*string*string*string  (** epoch,upstream,revision,binnmu *)

(** decompose a version string *)
val decompose: string -> version_analysis

(** recompose a decomposed version string. For all v:
    [equal(v,compose(decompose v)) = true].  There may, however, be
    small syntactic differences between [v] and [compose(decompose v)] *)
val compose: version_analysis -> string 

(** return a version without its epoch and without its binNMU part *)
val strip_epoch_binnmu: string -> version_analysis

(** return a version without its epoch part *)
val strip_epoch: string -> version_analysis

(** return the epoch component of the version. empty string if none *)
val extract_epoch: string -> string
