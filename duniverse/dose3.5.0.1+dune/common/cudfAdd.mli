(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *) 
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

(** Library of additional functions for the CUDF format. *)

(** {2 Basic comparison operations for packages} *)

(** Equality test: two CUDF packages are equal if their names and versions
    are equal. *)
val equal : Cudf.package -> Cudf.package -> bool

(** Compare function: compares two CUDF packages using standard CUDF 
    comparison operator (i.e. comparing by their name and version). *)
val compare : Cudf.package -> Cudf.package -> int

(** {2 Specialized data structures for CUDF packages} *)

(** A hash function for CUDF packages, using only their name and version. *)
val hash : Cudf.package -> int

(** Sort function: sorts a CUDF packages list using the 
    standard CUDF comparison operator in ascending order. *)
val sort : ?asc: bool -> Cudf.package list -> Cudf.package list

(** {2 Data structures} *)

(** Specialized hashtable for CUDF packages. *)
module Cudf_hashtbl : (Hashtbl.S with type key = Cudf.package)

(** Specialized set data structure for CUDF packages. *)
module Cudf_set : (Set.S with type elt = Cudf.package)

(** Convert a list of CUDF packages to a set of CUDF packages. *)
val to_set : Cudf_set.elt list -> Cudf_set.t

(** {2 Extended function on Cudf data types } *)

(** Return the list of packages that that respect the given constraint *)
val who_provides : Cudf.universe -> Cudf_types.vpkg -> Cudf.package list

(** Return the list of packages satisfying the vpkg list *)
val resolve_deps : Cudf.universe -> Cudf_types.vpkglist -> Cudf.package list

(** Returns the list of packages that are dependencies of the given package *)
val who_depends : Cudf.universe -> Cudf.package -> Cudf.package list list

(** A table to associate to each id the list of packages id that are in
    conflict with it. Reflexive conflicts are made explicit.
*)
type ctable = (int, int list ref) ExtLib.Hashtbl.t

(** Create a ctable from a package universe *)
val init_conflicts : Cudf.universe -> ctable

(** Return the list of packages in conflict with the given package *)
val who_conflicts : ctable -> Cudf.universe -> Cudf.package -> Cudf.package list

(** Like who_provides but returns a list of cudf ids *)
val resolve_vpkg_int : Cudf.universe -> Cudf_types.vpkg -> int list

(** Like resolve_deps but returns a list of cudf ids *)
val resolve_vpkgs_int : Cudf.universe -> Cudf_types.vpkglist -> int list

(** {2 Functions to encode and decode strings. } *)
(* TODO: What are these functions doing in this module? *)

(** Encode a string.

    Replaces all the "not allowed" characters
    with their ASCII code (in hexadecimal format),
    prefixed with a '%' sign.
    
    Only "allowed" characters are letters, numbers and these: [@/+().-],
    all the others are replaced.
    
    Examples:
    {ul
    {li [encode "ab"  = "ab"]}
    {li [encode "|"   = "%7c"]}
    {li [encode "a|b" = "a%7cb"]}
    }
*)
val encode : string -> string

(** Decode a string. Opposite of the [encode] function.

    Replaces all the encoded "not allowed" characters
    in the string by their original (i.e. not encoded) versions.

    Examples:
    {ul
    {li [decode "ab" = "ab"]}
    {li [decode "%7c" = "|"]}
    {li [decode "a%7cb" = "a|b"]}
    }
*)
val decode : string -> string

(** {2 Additional functions on the CUDF data types. } *)

(** Returns a list of packages containing for each package [n]
    most recent version (default the latest version) *)
val latest: ?n : int -> Cudf.package list -> Cudf.package list

(** Set of strings *)
module StringSet : (Set.S with type elt = ExtLib.String.t)

(** Returns the set of all names in the given universe *)
val pkgnames : Cudf.universe -> StringSet.t

(** Add a new property to the given cudf preamble *)
val add_properties : Cudf.preamble -> Cudf_types.typedecl -> Cudf.preamble

(** return the value of the requested property. 
 * emit a warning and raise Not_found if the property does not exists *)
val get_property : string -> Cudf.package -> string

(** Returns true if the package is essential, that is the cudf package has
    a extra property named "essential" and its value is "yes" *)
val is_essential : Cudf.package -> bool

(** build a hash table that associates (package name, String version) to CUDF packages *)
val realversionmap : Cudf.package list ->
  (Cudf_types.pkgname * string, Cudf.package) ExtLib.Hashtbl.t

(** Return the unique cudf id of a package in a universe *)
val pkgtoint : Cudf.universe -> Cudf.package -> int

(** Given a universe and a cudf id returns the corresponding package.
    Raise Not_found if the id does not correspond to a package.
*)
val inttopkg : Cudf.universe -> int -> Cudf.package

(*
(** convert pef operators to cudf *)
val cudf_op:
  string -> [> `Eq | `Neq | `Geq | `Gt | `Leq | `Lt ] 

(** convert pef constraints to cudf *)
val cudf_constr:
  (string * string) option ->
  ([> `Eq | `Neq | `Geq | `Gt | `Leq | `Lt ] * string) option
*)

val compute_pool : Cudf.universe -> int list list array * int list array

val add_to_package_list :
  ('a, 'b list ref) ExtLib.Hashtbl.t -> 'a -> 'b -> unit

val get_package_list : ('a, 'b list ref) ExtLib.Hashtbl.t -> 'a -> 'b list

(** normalize_set l returns the list l without any duplicate element. *)
val normalize_set : int list -> int list

(** {2 Formatting, printing, converting to string. } *)

val string_of : (Format.formatter -> 'a -> 'b) -> 'a -> string

val pp_version : Format.formatter -> Cudf.package -> unit
val pp_package : Format.formatter -> Cudf.package -> unit

(** return a string containg either the value of the optional field
    "number" or the cudf version *)
val string_of_version : Cudf.package -> string

(** return a string of the form "name ( = version)" *)
val string_of_package : Cudf.package -> string

(* Function signature for cudf package printer. The output represents
   a triple (name, version, (field name, value) list *)
type pp = Cudf.package -> string * string option * string * (string * (string * bool)) list

(** [pp ?decode from_cudf pkg] package pretty printer.
    [from_cudf] a function that gets a (name,cudfversion) pair and returns a (name,realversion).
    [?fields] additional fields to print.
    [?decode] a function that decode the package name and version.
    
    returns : a pair (name,versiom,property list)

    note that if the package has version less then 0, then the version is printed
    as "nan"
*)
val pp :
  (Cudf_types.pkgname * Cudf_types.version -> string * string option * string) ->
  ?fields: string list->
  ?decode: (Cudf_types.pkgname -> string) -> pp

(** [default_pp] default package printer. Extracts string values from a 
    cudf package : Name, Version, Fields. Where Fields is a list of 
    field name , value pairs . If the version of the package is
    a negative number, the version version if printed as "nan". *)
val default_pp : pp

(** cudf vpkglist printer. *)
val pp_vpkg : pp -> Format.formatter -> Cudf_types.vpkg -> unit

(** cudf vpkglist printer. *)
val pp_vpkglist : pp -> Format.formatter -> Cudf_types.vpkglist -> unit

(** Compute the depenency cone of a list of packages *)
val cone: Cudf.universe -> Cudf.package list -> Cudf.package list
