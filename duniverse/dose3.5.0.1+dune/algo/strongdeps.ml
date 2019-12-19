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

(** Strong Dependencies *)

open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let mainbar = Util.Progress.create "Strongdeps_int.main"
let conjbar = Util.Progress.create "Strongdeps_int.conj"
let strongtimer = Util.Timer.create "Strongdeps_int.strong"
let conjtimer = Util.Timer.create "Strongdeps_int.conjdep"

(** check if p strongly depends on q.
    We check if it is possible to install p without q.  *)
(* ATT: this function makes a copy of the solver to add a clause to it *)
let strong_depends solver p q =
  Depsolver_int.S.reset solver.Depsolver_int.constraints;
  let solver = Depsolver_int.copy_solver solver in
  let lit = Depsolver_int.S.lit_of_var (solver.Depsolver_int.map#vartoint q) false in
  Depsolver_int.S.add_rule solver.Depsolver_int.constraints [|lit|] [];
  match Depsolver_int.solve solver ~explain:false [p] with
  |Diagnostic.FailureInt _ -> true
  |Diagnostic.SuccessInt _ -> false

(** check if [p] strong depends on any packages in [l] *)
let check_strong univ transitive graph solver p l =
  let pkg_p = CudfAdd.inttopkg univ p in
  List.iter (fun q ->
    let q = solver.Depsolver_int.map#inttovar q in
    let gid = (snd solver.Depsolver_int.globalid) in
    if q <> gid then
      let pkg_q = CudfAdd.inttopkg univ q in
      if p <> q then
        if not(Defaultgraphs.PackageGraph.G.mem_edge graph pkg_p pkg_q) then
          if strong_depends solver p q then 
            Defaultgraphs.PackageGraph.add_edge ~transitive graph pkg_p pkg_q
  ) l

(* true if at least one dependency is disjunctive *)
let somedisj (`CudfPool (_,cudfpool)) id = 
  let (depends,_) = cudfpool.(id) in
  if List.length depends > 0 then
    try
      List.iter (function (_,[_]) -> () | _ -> raise Not_found) depends;
      false
    with Not_found -> true
  else false

(** [strongdeps l] build the strong dependency graph of l *)
(* each package has a node in the graph, even if it does not have  
   any strong dependencies. If pkglist_size <> universe_size then we have to
   check the strong dependencies in any cases, as a disjunctive dependency
   might be hidden in the closure.
*)
let strongdeps_int ?(transitive=true) graph univ pkglist =
  let global_constraints = [] in
  let cudfpool = Depsolver_int.init_pool_univ ~global_constraints univ in
  let pkglist_size = List.length pkglist in
  let universe_size = Cudf.universe_size univ in

  Util.Progress.set_total mainbar pkglist_size;
  Util.Timer.start strongtimer;
  List.iter (fun pkg ->
    Util.Progress.progress mainbar;
    Defaultgraphs.PackageGraph.G.add_vertex graph pkg;
    let id = CudfAdd.pkgtoint univ pkg in
    if (pkglist_size <> universe_size) || (somedisj cudfpool id) then begin 
      let closure = Depsolver_int.dependency_closure_cache cudfpool [id] in
      let solver = Depsolver_int.init_solver_closure ~global_constraints cudfpool closure in
      match Depsolver_int.solve solver ~explain:true [id] with
      |Diagnostic.FailureInt(_) -> ()
      |Diagnostic.SuccessInt(f_int) ->
          check_strong univ transitive graph solver id (f_int ())
    end
  ) pkglist ;
  Util.Progress.reset mainbar;
  debug "strong dep graph: %d nodes, %d edges"
    (Defaultgraphs.PackageGraph.G.nb_vertex graph)
    (Defaultgraphs.PackageGraph.G.nb_edges graph);
  Util.Timer.stop strongtimer graph
;;

let strongdeps ?(transitive=true) univ pkglist =
  let size = Cudf.universe_size univ in
  let graph = Defaultgraphs.PackageGraph.G.create ~size () in
  strongdeps_int ~transitive graph univ pkglist
;;

let strongdeps_univ ?(transitive=true) univ =
  let size = Cudf.universe_size univ in
  let graph = Defaultgraphs.PackageGraph.G.create ~size () in
  Util.Progress.set_total conjbar size;

  Util.Timer.start conjtimer;
  let l = 
    Cudf.fold_packages (fun acc pkg ->
      Util.Progress.progress conjbar;
      Defaultgraphs.PackageGraph.conjdepgraph_int ~transitive graph univ pkg;
      pkg :: acc
    ) [] univ
  in
  Util.Progress.reset conjbar;
  Util.Timer.stop conjtimer ();
  debug "conj dep graph: nodes %d , edges %d"
    (Defaultgraphs.PackageGraph.G.nb_vertex graph)
    (Defaultgraphs.PackageGraph.G.nb_edges graph);
  let g = strongdeps_int ~transitive graph univ l in
  (* because the graph might still be transitive *)
  (* if not transitive then O.transitive_reduction g; *)
  g
;;

(** return the impact set (list) of the node [q] in [graph] *)
(** invariant : we assume the graph is NOT detransitivitized *)
let impactlist = Defaultgraphs.PackageGraph.pred_list

(** return the list of strong dependencies of the node [q] in [graph] *)
(** invariant : we assume the graph is NOT detransitivitized *)
let stronglist = Defaultgraphs.PackageGraph.succ_list

let impactset = Defaultgraphs.PackageGraph.pred_set

let strongset = Defaultgraphs.PackageGraph.succ_set


(** [strongdeps u l] build the strong dependency graph of all packages in 
    [l] wrt the universe [u] *)
let strongdeps ?(transitive=true) universe pkglist =
  strongdeps ~transitive universe (Depsolver.trimlist universe pkglist)

(** [strongdeps_univ u] build the strong dependency graph of 
    all packages in the universe [u] *)
let strongdeps_univ ?(transitive=true) universe =
  strongdeps_univ ~transitive (Depsolver.trim universe)

(** compute the impact set of the node [q], that is the list of all 
    packages [p] that strong depends on [q] *)
let impactset = Defaultgraphs.PackageGraph.pred_list

(** compute the conjunctive dependency graph *)
let conjdeps_univ universe =
  let g = Defaultgraphs.PackageGraph.G.create () in
  Cudf.iter_packages (fun pkg ->
    Defaultgraphs.PackageGraph.conjdepgraph_int g universe pkg
  ) (Depsolver.trim universe);
  g

(** compute the conjunctive dependency graph considering only packages 
    in [pkglist] *)
let conjdeps universe pkglist =
  let g = Defaultgraphs.PackageGraph.G.create () in
  List.iter (fun pkg ->
    Defaultgraphs.PackageGraph.conjdepgraph_int g universe pkg
  ) (Depsolver.trimlist universe pkglist);
  g
