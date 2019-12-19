(**************************************************************************************)
(*  Copyright (C) 2009-2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                *)
(*                      and Jaap Boender <boender@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009-2010 Mancoosi Project                                          *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open Graph
open ExtLib
open Common

let dombar = Util.Progress.create "Algo.dominators"

let domtimer = Util.Timer.create "Algo.Dominators.dominators_direct"
let tjntimer = Util.Timer.create "Algo.Dominators.dominators_tarjan"
let crtimer = Util.Timer.create "Algo.Dominators.cycle_reduction"
let sdtrtimer = Util.Timer.create "Algo.Dominators.sd_transitive_reduction"
let domtrtimer = Util.Timer.create "Algo.Dominators.dom_transitive_reduction"

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module G = Defaultgraphs.PackageGraph.G
module O = Defaultgraphs.GraphOper(G)
module S = Defaultgraphs.PackageGraph.S

let impactset (graph,pkg) = G.fold_pred S.add graph pkg (S.singleton pkg)
let scons (graph,pkg) = G.fold_succ S.add graph pkg (S.singleton pkg)

(* the dominators are computed on the strong dependency graph
 * with transitive archs *)
let dominators_direct ?(relative=None) graph = 
  debug "input graph SD : vertex %d - edges %d" (G.nb_vertex graph) (G.nb_edges graph);

  Util.Progress.set_total dombar (G.nb_vertex graph);
  Util.Timer.start domtimer;
  let domgraph = G.create () in
  G.iter_vertex (fun p ->
    Util.Progress.progress dombar;
    let isp = impactset (graph,p) in
    let sconsp = scons (graph,p) in
    G.iter_succ (fun q ->
      if not(CudfAdd.equal p q) then begin
        let isq = impactset (graph,q) in
        let dfs = S.diff isq sconsp in
        match relative with
        |None -> if S.subset dfs isp then G.add_edge domgraph p q
        |Some threshold ->
          let t = ( float ( S.cardinal (S.diff dfs isp)) *. 100.) /. ( float (S.cardinal isp)) in
          if t <= threshold then G.add_edge domgraph p q
      end
    ) graph p
  ) graph;
  Util.Timer.stop domtimer ();
  debug "after dominators direct : vertex %d - edges %d" (G.nb_vertex domgraph) (G.nb_edges domgraph);

  Util.Timer.start crtimer;
  Defaultgraphs.PackageGraph.cycle_reduction domgraph;
  Util.Timer.stop crtimer ();
  debug "after cycle reduction dominators : vertex %d - edges %d" (G.nb_vertex domgraph) (G.nb_edges domgraph);

  Util.Timer.start domtrtimer;
  O.transitive_reduction domgraph;
  Util.Timer.stop domtrtimer ();
  debug "after transitive reduction dominators : vertex %d - edges %d"
  (G.nb_vertex domgraph) (G.nb_edges domgraph);

  domgraph
;;

(* This function expects a strong dependency graph that might or not contain
 * transitive edges *)
let dominators_tarjan graph =
  debug "input graph SD : vertex %d - edges %d" (G.nb_vertex graph) (G.nb_edges graph);
  let start_pkg = { Cudf.default_package with Cudf.package = "START" } in

  let graph = G.copy graph in

  (* all cycles are cliques in the strong dependency graph *)
  Util.Timer.start crtimer;
  Defaultgraphs.PackageGraph.cycle_reduction graph;
  Util.Timer.stop crtimer ();
  debug "after cycle reduction SD : vertex %d - edges %d" (G.nb_vertex graph) (G.nb_edges graph);

  Util.Timer.start sdtrtimer;
  O.transitive_reduction graph;
  Util.Timer.stop sdtrtimer ();
  debug "after transitive reduction SD : vertex %d - edges %d" (G.nb_vertex graph) (G.nb_edges graph);

  (* connect it to all packages without incoming edges to a start vertex *)
  G.iter_vertex (fun v ->
    if (G.in_degree graph v) = 0 then
      G.add_edge graph start_pkg v;
  ) graph;

  Util.Timer.start tjntimer;
#if OCAMLGRAPHVERSION >= 186
  let module Dom = Dominator.Make_graph(G) in
#else
  let module Dom = Dominator.Make(G) in
#endif
  let idom = Dom.compute_all graph start_pkg in
  let domgr = idom.Dom.dom_graph () in
  Util.Timer.stop tjntimer ();

  G.remove_vertex graph start_pkg;
  G.remove_vertex domgr start_pkg;
  
  Util.Timer.start domtrtimer;
  O.transitive_reduction domgr;
  Util.Timer.stop domtrtimer ();
  debug "after transitive reduction dominators : vertex %d - edges %d" (G.nb_vertex domgr) (G.nb_edges domgr);

  domgr
;;

