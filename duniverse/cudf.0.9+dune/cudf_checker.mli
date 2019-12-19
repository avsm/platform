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

(** Checkers for CUDF documents

    Consistency and problem-solution matching. *)

open Cudf_types
open Cudf

type inconsistency_reason =
  [ `Unsat_dep of (pkgname * version) * vpkgformula	(** unsatisfied dep. *)
  | `Conflict of (pkgname * version) * vpkglist	(** unsolved conflict(s) *)
  ]

type bad_solution_reason =
  [ inconsistency_reason
  | `Missing_install of vpkglist   (** install pkgs missing *)
  | `Missing_upgrade of vpkglist   (** upgrade pkgs missing *)
  | `Unremoved of vpkglist	   (** remove pkgs still there *)
  | `Downgrade of vpkglist	   (** upgrade pkgs downgraded *)
  | `Multi_upgrade of pkgname list (** upgrade pkgs aren't singleton *)
  | `Not_kept of pkgname * version * enum_keep	(** unattended "Keep" *)
  ]

(** provide a string explaining a given reason, meant for error messages *)
val explain_reason : bad_solution_reason -> string

(** check whether a given package formula is satisfied by a given
    package status

    @return [true, []] if the formula is satisfied; [false, f]
    otherwise, where f is a sub-formula of the input denoting an
    unsatisfiable formula (ideally, a witness of the unsatisfiability
    of the input formula) *)
val satisfy_formula : universe -> vpkgformula -> bool * vpkgformula

(** check whether a package list is not satisfied by a given package
    status

    @return [true, []] if the list is disjoint; [false, l]
    otherwise, where l is a list of packages satisfied by the universe
    (ideally, the reason of the non-disjointness) *)
val disjoint :
  universe -> ?ignore:(package -> bool) -> vpkglist -> bool * vpkglist

(** @return [true, None] if the given installation is consistent,
    [false, Some r] otherwise, where r is the inconsistency reason *)
val is_consistent : universe -> bool * inconsistency_reason option

(** [is_solution (status, req) sol] checks whether [sol] fulfills the CUDF
    upgrade scenario described by [(status, req)]

    {b Note}: the [sol] package universe must contain all relevant package
    metadata (e.g. Depends, Conflicts, etc.), copied from [status], a compact
    universe only containing package names and versions won't be enough. To
    load compact universes see {!Cudf_parser.load_solution}.

    {b Note}: in accordance with CUDF semantics, for a solution to be valid,
    the solution shall correspond to a consistent universe. A solution that
    does satisfy user request, but at the same time proposes an inconsistent
    universe (as per {!Cudf_checker.is_consistent}) will be reported by
    [is_solution] as not being a valid solution.

    @return [true, []] if this is the case, [false, l]
    otherwise, where r explains why the solution is bad *)
val is_solution : (universe * request) -> universe -> bool * bad_solution_reason list
