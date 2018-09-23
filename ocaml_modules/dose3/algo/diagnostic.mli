(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** {2 Un-installability reasons} *)

(** The request provided to the solver *)
type request = Cudf.package list

type reason =
  |Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list)
  (** Not strictly a un-installability, Dependency (a,vpkglist,pkglist) is used
      to recontruct the the dependency path from the root package to the
      offending un-installable package *)
  |Missing of (Cudf.package * Cudf_types.vpkg list)
  (** Missing (a,vpkglist) means that the dependency
      [vpkglist] of package [a] cannot be satisfied *)
  |Conflict of (Cudf.package * Cudf.package * Cudf_types.vpkg)
  (** Conflict (a,b,vpkg) means that the package [a] is in conflict
      with package [b] because of vpkg *)

(** The result of an installability query *)
type result =
  |Success of (?all:bool -> unit -> Cudf.package list)
  (** If successfull returns a function that will
      return the installation set for the given query. Since
      not all packages are tested for installability directly, the
      installation set might be empty. In this case, the solver can
      be called again to provide the real installation set 
      using the parameter [~all:true] *)
  |Failure of (unit -> reason list)
  (** If unsuccessful returns a function containing the list of reason *)

(** The aggregated result from the solver *)
type diagnosis = { result : result; request : request; }

(** {2 Low level Integer Un-installability reasons} *)
type reason_int =
  |DependencyInt of (int * Cudf_types.vpkg list * int list)
  |MissingInt of (int * Cudf_types.vpkg list)
  |ConflictInt of (int * int * Cudf_types.vpkg)

(** the low-level result. All integers are sat solver indexes and need to be
    converted using a projection map. Moreover the result also contains the
    global constraints index that must filtered out before returing the final
    result to the user *)
type result_int =
  |SuccessInt of (?all:bool -> unit -> int list)
  |FailureInt of (unit -> reason_int list)

type request_int = int list

(** {3 Helpers Functions } *)

(** Turn an integer result into a cudf result *)
val diagnosis : Common.Util.projection -> Cudf.universe ->
  result_int -> request_int -> diagnosis

(** Turn an integer result into a cudf result *)
val result : Common.Util.projection -> Cudf.universe -> result_int -> result

(** Turn an integer request into a cudf request *)
val request : Cudf.universe -> request_int -> Cudf.package list

(** {2 Pretty Priting Functions } *)

module ResultHash : Hashtbl.S with type key = reason

(** Collect aggregate information about not installable packages *)
type summary = {
  mutable missing : int;
  mutable conflict : int;
  mutable unique_missing : int;
  mutable unique_conflict : int;
  mutable unique_selfconflict : int;
  summary : Cudf.package list ref ResultHash.t;
  statistic : ((int * int), int ref) Hashtbl.t
}
val default_result : int -> summary

(** [collect summary result]. Callback function to collect result 
    information in the [summary] data structure. Can be used to build
    a custom callback function for [Depsolver.listcheck] or [Depsolver.univcheck] *)
val collect : summary -> diagnosis -> unit

(** print output version *)
val pp_out_version : Format.formatter -> unit

(** default package pretty printer. *)
val pp_package : ?source:bool -> ?fields:bool -> Common.CudfAdd.pp ->
  Format.formatter -> Cudf.package -> unit

val pp_list :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

val print_error : ?condense : bool -> ?minimal : bool -> Common.CudfAdd.pp ->
  Cudf.package -> Format.formatter -> reason list -> unit

(** If the installablity query is successfull, [get_installationset] return 
    the associated installation set . If minimal is true (false by default),
    the installation set is restricted to the dependency cone of the packages
    specified in the installablity query. @Raise [Not_found] if the result is
    a failure. *)
val get_installationset : ?minimal : bool -> diagnosis -> Cudf.package list

(** True is the result of an installablity query is successfull. False otherwise *)
val is_solution : diagnosis -> bool

(** print a aggregate information of not installable packages.
    @param pp cudf package printer
    @param explain if true, print the list of all affected packages associated to
                       and installation problem. *)
val pp_summary :
  ?pp : Common.CudfAdd.pp ->
  ?explain : bool -> unit -> Format.formatter -> summary -> unit

(** [printf fmt d] print the output of the solver in yaml format 
    to the formatter [fmt].
    @param pp cudf package printer
    @param failure print the list of not installable packages
    @param success print the list of installable packages
    @param explain for installable packages, print the associated installation set
                       for not installable packages, print the all dependencies chains *)
val fprintf :
  ?pp : Common.CudfAdd.pp ->
  ?failure : bool ->
  ?success : bool ->
  ?explain : bool -> 
  ?minimal : bool -> 
  ?condense : bool -> Format.formatter -> diagnosis -> unit

(** like [fprintf] but print using the standard formatter *)
val printf :
  ?pp : Common.CudfAdd.pp ->
  ?failure : bool -> ?success : bool -> ?explain : bool -> diagnosis -> unit

(** print the explanation graph in dot format to the standard formatter *)
val print_dot :
  ?pp : Common.CudfAdd.pp -> ?condense : bool ->
  ?addmissing : bool -> ?dir : string -> diagnosis -> unit
