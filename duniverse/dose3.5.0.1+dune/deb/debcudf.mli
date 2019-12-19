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

(** Debian Specific Cudf conversion routines *)

(** abstract data type holding the conversion tables for the debcudf translation. *)
type tables

type extramap = (string * (string * Cudf_types.typedecl1)) list
type options = {
  extras_opt : extramap ;
  native : string option; (** the native architecture *)
  foreign : string list ; (** list of foreign architectures *)
  host : string option;   (** the host architecture - cross compile *)
  ignore_essential : bool ;
  builds_from : bool ;    (** whether to add the builds-from relationship from binary to source packages *)
  drop_bd_indep : bool ;  (** whether or not to ignore the Build-Depends-Indep field *)
  profiles : string list ; (** list of active build profiles *)
}

val default_options : options

(** initialize the version conversion tables *)
val init_tables :
  ?options:options -> ?step:int -> ?versionlist:Pef.Packages_types.version list ->
    Packages.package list -> tables

val clear : tables -> unit
(** return the cudf version associated to a tuple (name,version). 
 * return Not_found if there is not package or cudf version associated
 * to the tuple (name,version) *)
val get_cudf_version :
  tables -> Pef.Packages_types.name * Pef.Packages_types.version -> int

(** Get the orgininal debian package name. Remove deb -> cudf conversion cruft *)
val get_real_name : Cudf_types.pkgname -> (string * string option)

(** return the real version associated to a Cudf package *)
val get_real_version : tables -> Cudf_types.pkgname * Cudf_types.version -> 
  (Pef.Packages_types.name *
   Pef.Packages_types.architecture option *
   Pef.Packages_types.version)

val get_essential :
  ?options:options -> tables -> (Cudf_types.vpkglist * Cudf.package list) list

(** [tocudf tbl p] 
    convert the a debian package representation to cudf.
   - Version and package name normalization.
   - Adding self conflicts.
   - Virtual package normalization.
   - Adding priority information if avaiblable.
   - Mapping APT request.
   @param inst : set the {i Installed} cudf field
*)
val tocudf : tables -> ?options:options -> ?inst:bool -> Packages.package -> Cudf.package

(** declare the Cudf preamble used by cudf. Namely, debcudf add :
    - a property named {b Number} of type string containing the original debian version
    - a property named {b Source} of type string
    - a property named {b Sourceversion} of type string
    *)
val preamble : Cudf.preamble

(** create a Cudf universe from a debian package representation list. *)
val load_universe : ?options:options -> Packages.package list -> Cudf.universe

(** create a Cudf package list from a debian package representation list. *)
val load_list : ?options:options -> Packages.package list -> Cudf.package list
