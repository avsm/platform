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

open ExtLib
open Printf

open Cudf_types
open Cudf

module PP = Cudf_types_pp

let (!!) pred = fun x -> not (pred x)

type inconsistency_reason =
  [ `Unsat_dep of (pkgname * version) * vpkgformula
  | `Conflict of (pkgname * version) * vpkglist
  ]

type bad_solution_reason =
  [ inconsistency_reason
  | `Missing_install of vpkglist
  | `Missing_upgrade of vpkglist
  | `Unremoved of vpkglist
  | `Downgrade of vpkglist
  | `Multi_upgrade of pkgname list
  | `Not_kept of pkgname * version * enum_keep
  ]

let explain_reason = function
  | `Unsat_dep ((name, ver), fmla) ->
      sprintf "Cannot satisfy dependencies %s of package %s (version %d)"
	(PP.string_of_vpkgformula fmla) name ver
  | `Conflict ((name, ver), pkgs) ->
      sprintf "Unresolved conflicts %s of package %s (version %d)"
	(PP.string_of_vpkglist pkgs) name ver
  | `Missing_install vpkgs ->
      "Unmet installation request, missing packages: " ^
	PP.string_of_vpkglist vpkgs
  | `Missing_upgrade vpkgs ->
      "Unmet upgrade request, missing packages: " ^
	PP.string_of_vpkglist vpkgs
  | `Unremoved vpkgs ->
      "Unmet remove request, still present packages: " ^
	PP.string_of_vpkglist vpkgs
  | `Downgrade vpkgs ->
      "Unmet upgrade request, not-upgraded: " ^
	PP.string_of_vpkglist vpkgs
  | `Multi_upgrade pkgs ->
      "Unmet upgrade request, not-unique: " ^ String.concat ", " pkgs
  | `Not_kept (name, ver, keep) ->
      sprintf "Unmet \"Keep\" request %s of package %s (version %d)"
	(PP.string_of_keep keep) name ver

(* XXX not tail-recursive *)
let satisfy_formula univ fmla =
  let reason = ref [] in
  let sat_pkg = mem_installed ~include_features:true univ in
  let sat =
    match List.filter (!! (List.exists sat_pkg)) fmla with
	[] -> true
      | unsat -> reason := unsat ; false
  in
    sat, !reason

let disjoint univ ?ignore pkgs =
  match
    List.filter (mem_installed ?ignore ~include_features:true univ) pkgs
  with
    | [] -> true, []
    | pkgs -> false, pkgs

let is_consistent univ =
  let msg = ref None in
    try
      iter_packages
	(fun pkg ->
	   if pkg.installed then begin
	     (match satisfy_formula univ pkg.depends with
		  false, fmla ->
		    msg := Some (`Unsat_dep ((pkg.package, pkg.version), fmla));
		    raise Exit
		| _ -> ());
	     (match disjoint univ ~ignore:((=%) pkg) pkg.conflicts with
		| false, pkgs ->
		    msg := Some (`Conflict ((pkg.package, pkg.version), pkgs));
		    raise Exit
		| _ -> ());
	   end)
	univ;
      true, !msg
    with Exit -> false, !msg

(* for reference, see CUDF §2.3.4, "semantics of requests" *)
let is_solution (univ, req) sol =
  let _ =
    if universe_size sol <> installed_size sol then
      prerr_endline ("WARNING: solution contains not-installed packages,"
		     ^ " they have been ignored")
  in
  let sat vpkg = fst (satisfy_formula sol [[vpkg]]) in
  let and_formula = List.map (fun vpkg -> [(vpkg :> vpkg)]) in
  let is_succ () = (* XXX not implemented, as it will be pointless with a
		      diff-like encoding of solutions *)
    true, [] in
  let is_cons () =	(* check solution consistency (i.e., dep./conflicts) *)
    match is_consistent sol with
      | true, _ -> true, []
      | false, None -> assert false
      | false, Some reason -> false, [reason] in
  let install_ok () =	(* check "Install" property semantics *)
    match List.filter (!! sat) req.install with
      | [] -> true, []
      | l -> false, [`Missing_install l] in
  let remove_ok () =	(* check "Remove" property semantics *)
    match disjoint sol req.remove with
      | true, _ -> true, []
      | false, pkgs -> false, [`Unremoved pkgs] in
  let upgrade_ok () =	(* check "Upgrade" property semantics *)
    match List.filter (!! sat) req.upgrade with
      | (_ :: _) as l -> false, [`Missing_upgrade l]
      | [] ->
	  let versions_of univ name =
	    List.map	(* real packages *)
	      (fun pkg -> Some pkg.version)
	      (get_installed univ name)
	    @ List.map	(* virtual packages; "None" means "all versions" *)
	      (fun (_pkg, version) -> version)
	      (who_provides univ (name, None)) in
	  let res =
	    List.fold_left
	      (fun (ok, downgrades, multi) ((name, _constr) as vpkg) ->
		 match List.unique (versions_of sol name) with
		   | [Some v] ->
		       let old_installed = versions_of univ name in
			 if not (List.for_all
				   (function Some v' -> v' <= v | None -> false)
				   (* XXX: this None will report attempted
				      upgrade of unversioned virtual packages
				      as downgrades. Maybe right, maybe not *)
				   old_installed)
			 then
			   false, vpkg :: downgrades, multi
			 else
			   true && ok, downgrades, multi
		   | [] -> (* impossible: cause the formula is satisfied *)
		       assert false
		   | _ -> false, downgrades, name :: multi)
	      (true, [], [])
	      req.upgrade
	  in
	    (match res with
	       | true, _, _ -> true, []
	       | false, downgrades, multi ->
		   false,
		   (if downgrades <> [] then [`Downgrade downgrades] else [])
		   @ (if multi <> [] then [`Multi_upgrade multi] else []))
  in
  let keep_ok () =	(* check "Keep" property semantics *)
    let to_be_kept =
      get_packages
	~filter:(fun pkg -> pkg.installed && pkg.keep <> `Keep_none) univ in
    List.fold_left
      (fun (ok, reasons) pkg ->
	 let pkg_ok =
	   match pkg.keep with
	     | `Keep_version ->
		 (try
		    (lookup_package sol (pkg.package, pkg.version)).installed
		  with Not_found -> false)
	     | `Keep_package ->
		 mem_installed ~include_features:false sol (pkg.package, None)
	     | `Keep_feature ->
		 fst (satisfy_formula sol (and_formula pkg.provides))
	     | _ -> assert false	(* [get_packages ~filter] is broken *)
	 in
	 if pkg_ok then
	   ok, reasons
	 else
	   false,
	 (`Not_kept (pkg.package, pkg.version, pkg.keep)) :: reasons)
      (true, [])
      to_be_kept
  in
  List.fold_left
    (fun (is_sol, msgs) test ->
       let res, msg = test () in
       res && is_sol, msg @ msgs)
    (true, [])
    [is_succ; is_cons; install_ok; remove_ok; upgrade_ok; keep_ok]

