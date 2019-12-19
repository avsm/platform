(**************************************************************************************)
(* Copyright (C) 2008 Stefano Zacchiroli <zack@debian.org> and                        *)
(* Jaap Boender <boender@pps.jussieu.fr> and                                          *)
(* Pietro Abate <pietro.abate@pps.jussieu.fr>                                         *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(*
 * More info about small world networks
 * http://en.wikipedia.org/wiki/Small-world_network
 *)

(** Small World analisys *)

open Graph
open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module Make (G: Sig.I ) = struct
  module VS = Set.Make (G.V)

  module UndG = Imperative.Graph.Concrete(G.V) 

  let undirect g =
    let g2 = UndG.create () in
    G.iter_vertex (fun v ->
      UndG.add_vertex g2 v;
      G.iter_succ (fun v' ->
        UndG.add_edge g2 v v'
      ) g v;
      G.iter_pred (fun v' ->
        UndG.add_edge g2 v' v
      ) g v
    ) g;
    g2
  ;;

  (* www.cs.cornell.edu/home/kleinber/...book/networks-book-ch20.pdf *)
  let clustering_coefficient graph vertex =
    let neighbours = G.succ graph vertex in
    let n = List.length neighbours in
    if n = 0 then 0.0
    else if n = 1 then 1.0
    else
    let n_edges = List.fold_left (fun old_sum v ->
            old_sum + (List.fold_left (fun old_sum' v' ->
                    if G.mem_edge graph v v' && v <> v'
                    then old_sum' + 1
                    else old_sum' 	
            ) 0 neighbours)
    ) 0 neighbours 
    and max_edges = if G.is_directed then n * (n-1) else n * (n-1) / 2
  in 
  float_of_int n_edges /. float_of_int max_edges	

  let average_distance graph vertex = 
    let rec add_successors distance visited vertices =
      (* Add successors breadth-first, we want the shortest path we can find *)
      let succs = ref [] in 
      let (n, sum) =
        List.fold_left (fun (old_n, old_sum) v ->
          if not (VS.mem v !visited) then
            begin
              visited := VS.add v !visited;
              succs := (G.succ graph v)::!succs;
              (old_n + 1, old_sum + distance)
            end
          else
            (old_n, old_sum)
        ) (0, 0) vertices
      in
      let sf =  List.flatten !succs in
      if sf <> [] then
        let (n', sum') = add_successors (distance + 1) visited sf in
          (n + n', sum + sum')
      else
        (n, sum)
    in
    let visited = ref (VS.singleton vertex) in
    let (n, sum) = add_successors 1 visited (G.succ graph vertex)
  in
  if sum = 0 then 0.0 else float_of_int sum /. float_of_int n
  ;;

  module MSin = Map.Make (struct
    type t = (G.V.t * G.t ref)
    let compare (v1,g) (v2,g) = (G.in_degree !g v1) - (G.in_degree !g v2)
  end)

  module MSout = Map.Make (struct
    type t = (G.V.t * G.t ref)
    let compare (v1,g) (v2,g) = (G.out_degree !g v1) - (G.out_degree !g v2)
  end)

  let _avgdegree = ref None
  let _outdata = ref None
  let _indata = ref None
  let _outdatadegree = ref MSout.empty
  let _indatadegree = ref MSin.empty

  let degree graph =
    let nv = G.nb_vertex graph in
    let outmax = ref 0 in
    let inmax = ref 0 in
    let outh = Hashtbl.create 1031 in
    let inh = Hashtbl.create 1031 in
    (* how many vertices have degree X *)
    let add h d =
      if d = 0 then () else
        try Hashtbl.replace h d ((Hashtbl.find h d) + 1)
        with Not_found -> Hashtbl.add h d 1
    in
    let total = 
      G.fold_vertex (fun v sum ->
        let outdeg = G.out_degree graph v in
        let indeg = G.in_degree graph v in
        add outh outdeg;
        add inh indeg;
        _indatadegree := MSin.add (v,ref graph) indeg !_indatadegree;
        _outdatadegree := MSout.add (v,ref graph) outdeg !_outdatadegree;
        sum + indeg
      ) graph 0
    in
    ( (float_of_int total) /. (float_of_int nv) , !outmax, !inmax, outh, inh)
  ;;

  let computeDegree graph =
    if Option.is_some !_avgdegree then ()
    else begin
      let (avdeg, maxout, maxin, outdata, indata) = degree graph in
      _avgdegree := Some(avdeg);
      _outdata := Some(outdata);
      _indata := Some(indata);
    end
  ;;

  let maxOutDegree graph =
    computeDegree graph;
    snd(MSout.max_binding !_outdatadegree)
  ;;

  let maxInDegree graph =
    computeDegree graph;
    snd(MSin.max_binding !_indatadegree)
  ;;

  let averageDegree graph =
    computeDegree graph;
    Option.get !_avgdegree
  ;;

  let zdp graph =
    G.fold_vertex (fun v sum ->
      if G.in_degree graph v = 0 && G.out_degree graph v = 0 then
        sum + 1
      else
        sum
    ) graph 0
  ;;

  let scatteredPlotIn graph =
    computeDegree graph;
    Option.get !_indata
  ;;

  let scatteredPlotOut graph =
    computeDegree graph;
    Option.get !_outdata
  ;;

  let scatteredPlotBoth graph =
    let add h i o =
      try Hashtbl.replace h (i, o) ((Hashtbl.find h (i, o)) + 1)
      with Not_found -> Hashtbl.add h (i, o) 1 in
    let h = Hashtbl.create 1031 in
    G.iter_vertex (fun v ->
      add h (G.in_degree graph v) (G.out_degree graph v)
    ) graph; 
    h
  ;;

  (* http://en.wikipedia.org/wiki/Centrality *)
  let centralityDegree graph fd =
    let n = float_of_int (G.nb_vertex graph) in
    let cd v = (float_of_int v) /. (n -. 1.0) in
    let m =
       G.fold_vertex (fun v max ->
         let s = (List.length (fd graph v)) in
         let m = cd s in
         if m > max then m else max
       ) graph 0.0
    in
    let c = 
      G.fold_vertex (fun v sum ->
        let s = (List.length (fd graph v)) in
        (sum +. (m -. (cd s)))
      ) graph 0.0
    in c /. (n -. 2.0)
  ;;

  let centralityOutDegree graph = centralityDegree graph G.succ
  let centralityInDegree graph = centralityDegree graph G.pred

  let clustering graph = 
    let n = float_of_int (G.nb_vertex graph) in
    let c =
      G.fold_vertex (fun v acc ->
          acc +. clustering_coefficient graph v
      ) graph 0.0
    in c /. n

  let averageShortestPathLength graph =
    let n = float_of_int (G.nb_vertex graph) in
    let c = 
      G.fold_vertex (fun v acc ->
        acc +. (average_distance graph v)
      ) graph 0.0
    in c /. n

  (* strongly directed components *)
  (* weakly directed compoenents = strongly directed compoenents if the graph
   * is not direct !!! *)
  let components graph = 
    let module C = Components.Make(G) in
    C.scc_array graph

  let weaklycomponents graph = 
    let module C = Components.Make(UndG) in
    C.scc_array (undirect graph)

  let numberComponents c = Array.length c

  let averageComponents c =
    let sum =
      Array.fold_left (fun acc i ->
        acc + List.length i
      ) 0 c
    in
    (float_of_int sum /. float_of_int (Array.length c))

  let largestComponent c =
    Array.sort (fun x y -> compare (List.length x) (List.length y)) c;
    (List.length c.(Array.length c - 1))
  ;;

  let density graph =
    let n = float_of_int (G.nb_vertex graph) in
    let ps_edg = n *. (n -. 1.0) in
    float_of_int (G.nb_edges graph) /. ps_edg

  let averageTwoStepReach graph =
    let module S = Set.Make(struct type t = G.vertex let compare = compare end) in
    let n = float_of_int (G.nb_vertex graph) in
    let t = 
      G.fold_vertex (fun i0 total ->
        let s = 
          G.fold_succ (fun i1 set1 ->
            G.fold_succ (fun i2 set2 ->
              S.add i2 set2
            ) graph i1 set1
          ) graph i0 (S.empty)
        in total +. (float_of_int (S.cardinal s));
      ) graph 0.0
    in t /. n

  let removezdp graph =
    let g = G.copy graph in
    G.iter_vertex (fun v ->
      if G.in_degree graph v = 0 && G.out_degree graph v = 0 then
        G.remove_vertex g v;
    ) graph;
    g
  ;;

(*
  (* bullshit *)
  let brokerage graph =
    let n = float_of_int (G.nb_vertex graph) in
    let ps_edg = n *. (n -. 1.0) in
    let total = ref 0 in
    G.iter_vertex (fun i0 ->
      G.iter_vertex (fun i1 ->
        if not (G.mem_edge graph i0 i1) then
          incr total
      ) graph
    ) graph;
    ((float_of_int !total) /. ps_edg)

  let shorter_path_length gr v =
    let module Bfs = Graph.Traverse.Bfs(G) in
    let seen = Hashtbl.create 1031 in
    let level = ref 0 in
    Bfs.prefix_component (fun v ->
      incr level ;
      Hashtbl.add see v !level
    ) gr v
    seen
  ;;

  let eccentricity gr =
    let vv = Hashtbl.create 1031 in
    G.iter_vertex (fun v ->
      let h = shorted_path_length gr v in
      Hashtbl.add vv v (Hashtbl.fold (fun k v acc -> max v acc) h 0)

    ) gr
  *)    

end
