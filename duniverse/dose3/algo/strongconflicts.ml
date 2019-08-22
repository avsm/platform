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

open Common
open CudfAdd
open Defaultgraphs

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module ICG = Strongconflicts_int.CG

type cfl_type = Explicit | Conjunctive | Other of Diagnostic.reason list

module CflE = struct
  type t = Cudf.package * Cudf.package * cfl_type
  let compare = Pervasives.compare
  let default = (Cudf.default_package, Cudf.default_package, Other [])
end

module CG = Graph.Imperative.Graph.ConcreteLabeled(PackageGraph.PkgV)(CflE)

(* tempy. *)
let reason univ rl =
  let from_sat = CudfAdd.inttopkg univ in
  List.map (function
    |Diagnostic.DependencyInt(i,vl,il) ->
      Diagnostic.Dependency(from_sat i,vl,List.map from_sat il)
    |Diagnostic.MissingInt(i,vl) ->
      Diagnostic.Missing(from_sat i,vl)
    |Diagnostic.ConflictInt(i,j,vpkg) ->
      Diagnostic.Conflict(from_sat i,from_sat j,vpkg)
  ) rl;;

let cvt univ =
  function
  | Strongconflicts_int.Explicit -> Explicit
  | Strongconflicts_int.Conjunctive -> Conjunctive
  | Strongconflicts_int.Other l -> Other (reason univ l);;

(** strongconflicts return the list of all strong conflicts in universe.
    
    invariant: the universe must contain only edos-installable packages : see
    Depsolver.trim.
*)

let strongconflicts universe =
  let g = CG.create () in
  let universe  = Depsolver.trim universe in
  let ig = Strongconflicts_int.strongconflicts universe in
  let inttovar = CudfAdd.inttopkg universe in
  (* convert output graph *)
  ICG.iter_vertex (fun v -> CG.add_vertex g (inttovar v)) ig;
  ICG.iter_edges_e (fun (x, (x', y', l), y) ->
    CG.add_edge_e g (inttovar x,
      (inttovar x', inttovar y', cvt universe l),
      inttovar y)
  ) ig;
  g

