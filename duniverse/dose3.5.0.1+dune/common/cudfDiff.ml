(**************************************************************************************)
(*  Copyright (C) 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2010 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** This module compute the difference between a cudf universe and a cudf solution *)

open ExtLib
open CudfAdd

type changeset = (Cudf_set.t * Cudf_set.t)
type difference = (string,changeset) Hashtbl.t

(* the 'package' is always taken from the universe *)
let to_set univ l =
  List.fold_left (fun s p ->
    let q = Cudf.lookup_package univ (p.Cudf.package,p.Cudf.version) in
    Cudf_set.add q s
  ) Cudf_set.empty l
;;

(* [make_solution univ sol] return a tuple with the set of packages to install and the
   set of packages to remove *)
let make_solution ~universe ~solution =
  let is_installed p = p.Cudf.installed in
  let before = CudfAdd.to_set (Cudf.get_packages ~filter:is_installed universe) in
  let after = CudfAdd.to_set (Cudf.get_packages ~filter:is_installed solution) in
  let install = Cudf_set.diff after before in
  let remove = Cudf_set.diff before after in
  (install,remove)

(** [make_difference univ sol] for each pkgname returns the list of all 
    versions that were installed (in the solution) or removed (from the
    universe) *)
let make_difference ~universe ~solution =
  let pkgnames = CudfAdd.pkgnames universe in
  let h = Hashtbl.create (StringSet.cardinal pkgnames) in
  StringSet.iter (fun pkgname ->
    let were_installed = to_set universe (Cudf.get_installed universe pkgname) in
    let are_installed = to_set universe (Cudf.get_installed solution pkgname) in
    let remove = Cudf_set.diff were_installed are_installed in
    let install = Cudf_set.diff are_installed were_installed in
    Hashtbl.add h pkgname (install,remove)
  ) pkgnames ;
  h

(**
   [all] : all versions of a package in the universe . 
   [s] : the set of version for version of a package in a solution 
   returns a list that contains for each version its status : installed, 
   removed, upgraded, etc
*)
type tmp_summary = {
  mutable i : Cudf.package list; (* installed *)
  mutable r : Cudf.package list; (* removed *)
  mutable u : (Cudf.package * Cudf.package) option ; (* upgraded *)
  mutable d : (Cudf.package * Cudf.package) option ; (* downgraded *)
  mutable nc : Cudf.package list; (* not changed *)
}

let uniqueversion all (install,remove) =
  let l = { u = None; d = None ; i = [] ; r = [] ; nc = [] } in
  let i = Cudf_set.filter (fun pkg -> pkg.Cudf.installed) all in
  if (Cudf_set.cardinal i <= 1) && ((Cudf_set.cardinal install) <= 1) then
    begin
      if (Cudf_set.cardinal install) = 1 then begin
        if (Cudf_set.cardinal i) = 1 then begin
          let np = Cudf_set.choose i in
          let op = Cudf_set.choose install in
          if np.Cudf.version < op.Cudf.version
          then l.u <- Some(np,op)
          else l.d <- Some(op,np)
        end
        else
          l.i <- Cudf_set.elements install;
      end else
        if not (Cudf_set.is_empty remove) then
          l.r <- Cudf_set.elements remove;
      end
  else begin
    if not (Cudf_set.is_empty remove) then
      l.r <- Cudf_set.elements remove;
    if not (Cudf_set.is_empty install) then
      l.i <- Cudf_set.elements install;
  end;
  l.nc <- Cudf_set.elements (Cudf_set.diff i (Cudf_set.union install remove));
  l
;;

type summary = {
  install : Cudf.package list;
  remove : Cudf.package list;
  upgrade : (Cudf.package * Cudf.package) list ;
  downgrade : (Cudf.package * Cudf.package) list ;
  notchange : Cudf.package list;
}

(* [summary univ diff] return a tuple with 5 lists containing packages
 * installed, upgraded, downgraded, removed, not upgraded 
 * (all packages marked as installed but not in the categories above) *)
let make_summary univ diff =
  let i = ref [] 
  and u = ref [] 
  and d = ref []
  and r = ref []
  and nc = ref [] in
  let names = CudfAdd.pkgnames univ in
  StringSet.iter (fun pkgname ->
    let all = CudfAdd.to_set (Cudf.lookup_packages univ pkgname) in
    let s = Hashtbl.find diff pkgname in
    let l = uniqueversion all s in
    i := l.i @ !i ;
    r := l.r @ !r ;
    if not (Option.is_none l.u) then
      u := (Option.get l.u) :: !u;
    if not (Option.is_none l.d) then
      d := (Option.get l.d) :: !d;
    nc := l.nc @ !nc;
  ) names;
  {install = !i;
   remove = !r;
   upgrade = !u;
   downgrade = !d;
   notchange = !nc }
;;
