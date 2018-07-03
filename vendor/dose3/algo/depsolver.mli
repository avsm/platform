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

(** Dependency solver. Implementation of the Edos algorithms *)

(** the solver is an abstract data type associated to a universe *)
type solver

(** initialize the solver. *)
val load :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    Cudf.universe -> solver

(** check if the universe universe is consistent (all installed packages are coinstallable)
    This function is a wrapper of Cudf_checker.is_consistent. *)
val is_consistent : Cudf.universe -> Diagnostic.diagnosis

(** check if the given package can be installed in the universe 
 
    Packages marked as `Keep_package must be always installed.*)
val edos_install :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    Cudf.universe -> Cudf.package -> Diagnostic.diagnosis

(** check if the give package list can be installed in the universe  *)
val edos_coinstall :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    Cudf.universe -> Cudf.package list -> Diagnostic.diagnosis

(** accept a list of list of packages and return the coinstallability test of
    the cartesian product. *)
val edos_coinstall_prod :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    Cudf.universe -> Cudf.package list list -> Diagnostic.diagnosis list

(** remove uninstallable packages from the universe .  *)
val trim :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    Cudf.universe -> Cudf.universe

(** remove uninstallable packages from the pkglist.  *)
val trimlist :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    Cudf.universe -> Cudf.package list -> Cudf.package list

(** return the list of broken packages.*)
val find_broken : 
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    Cudf.universe -> Cudf.package list

(** return the list of installable packages. *)
val find_installable : 
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    Cudf.universe -> Cudf.package list

(** return the list of broken packages.*)
val find_listbroken : 
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    Cudf.universe -> Cudf.package list -> Cudf.package list

(** return the list of installable packages. *)
val find_listinstallable : 
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    Cudf.universe -> Cudf.package list -> Cudf.package list

(** [univcheck ] check if all packages in the universe can be installed.
    Since not all packages are directly tested for installation, if a packages
    is installable, the installation might be empty. To obtain an installation
    set for each installable packages, the correct procedure is to iter on the
    list of packages.
 
    @param callback : execute a function for each package. This function can
    have side effects and can be used to collect the installation set or the
    failure reason.

    @return the number of broken packages
 *)
val univcheck :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    ?callback:(Diagnostic.diagnosis -> unit) -> 
      ?explain:bool -> Cudf.universe -> int

val univcheck_lowmem :  
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    ?callback:(Diagnostic.diagnosis -> unit) -> ?explain:bool -> Cudf.universe -> int

(** [listcheck ~callback:c subuniverse l] check if all packages in [l] can be
   installed.
  
   Invariant : l is a subset of universe can be installed in the solver universe.

   It is responsability of the user to pass listcheck an appropriate subuniverse`

   @param callback : execute a function for each package.
   @return the number of broken packages
 *)
val listcheck :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    ?callback:(Diagnostic.diagnosis -> unit) ->
      ?explain:bool -> Cudf.universe -> Cudf.package list -> int 

(** [dependency_closure index l] return the union of the dependency closure of
    all packages in [l] .

    @param maxdepth the maximum cone depth (infinite by default)
    @param conjunctive consider only conjunctive dependencies (false by default)
    @param universe the package universe
    @param pkglist a subset of [universe]
*)
val dependency_closure : 
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    ?maxdepth:int -> ?conjunctive:bool ->
      Cudf.universe -> Cudf.package list -> Cudf.package list

(** [reverse_dependencies univ ] compute the reverse dependency list of all
    packages in the universe [univ] *)
val reverse_dependencies : 
  Cudf.universe -> (Cudf.package list) Common.CudfAdd.Cudf_hashtbl.t

(** [reverse_dependencies_closure univ ] compute the reverse dependency list of all
    packages in [l] in the universe [univ] *)
val reverse_dependency_closure : ?maxdepth:int ->
  Cudf.universe -> Cudf.package list -> Cudf.package list

type enc = Cnf | Dimacs

(** [output_clauses enc univ] return a string encoded accordingly to [enc]
    (default cnf). 
  *)
val output_clauses :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
    ?enc : enc -> Cudf.universe -> string

(** The result of the depclean function is a tuple containing a package, a list
    of dependencies that are redundant and a list of conflicts that are redundant *)
type depclean_result = 
  (Cudf.package * 
    (Cudf_types.vpkglist * Cudf_types.vpkg * Cudf.package list) list *
    (Cudf_types.vpkg * Cudf.package list) list 
  )

(** For each package [p] in [packagelist], [depclean univ packagelist] detect 
    redundundant dependencies that refer to packages that are either missing
    or not co-installable together with the root package [p] *)
val depclean :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
  ?callback : (depclean_result -> unit) -> 
  Cudf.universe ->
  Cudf.package list ->
    depclean_result list

type solver_result =
  |Sat of (Cudf.preamble option * Cudf.universe)
  |Unsat of Diagnostic.diagnosis option
  |Error of string

(** an empty package used to enforce global contraints on the request *)
val dummy_request : Cudf.package

(** [check_request] check if there exists a solution for the give cudf document 
    if ?cmd is specified, it will be used to call an external cudf solver to
    satisfy the request.
    if ?criteria is specified it will be used as optimization criteria. 
    if ?explain is specified and there is no solution for the give request, the
    result will contain the failure reason. *)
val check_request :
  ?cmd : string ->
  ?criteria: string ->
  ?dummy: Cudf.package ->
  ?explain : bool ->
     Cudf.cudf -> solver_result

(** Same as [check_request], but allows to specify any function to call the
    external solver. It should raise [Depsolver.Unsat] on failure. *)
val check_request_using:
  ?call_solver:(Cudf.cudf -> Cudf.preamble option * Cudf.universe) ->
  ?criteria: string ->
  ?dummy: Cudf.package ->
  ?explain : bool ->
    Cudf.cudf -> solver_result

(** Build the installation graph from a cudf solution universe and sets of packages to be 
    installed/removed (see CudfAdd.make_summary) *)
val installation_graph: solution:Cudf.universe -> 
  (Common.CudfAdd.Cudf_set.t * Common.CudfAdd.Cudf_set.t) -> 
    Defaultgraphs.ActionGraph.G.t
