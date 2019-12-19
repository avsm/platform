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

open ExtLib

exception Done

type prob = { a : float ; b : float ; c : float }
module Options =
struct
  let debug = ref 0
  let dot = ref false
  let plain = ref false
  let cudf = ref false
  let vertex = ref 23000 
  let edges = ref 100000
  let seed = ref 10 
(* (probA,probB,probC,probD) = (0.45,0.15.0.15.0.25) *)
  let prob = ref {a = 0.0 ; b = 0.0 ; c = 0.0}
end

let usage = Printf.sprintf "usage: %s [-options] vertex edges" (Sys.argv.(0))
let options =
  [
   ("-d", Arg.Int (fun i -> Options.debug := i), "Turn on debugging info level");
   ("--vertex", Arg.Int (fun i -> Options.vertex := i), "default 20K");
   ("--edges", Arg.Int (fun i -> Options.edges := i), "default 100K");
   ("--seed", Arg.Int (fun i -> Options.seed := i), "");
   ("-d", Arg.Int (fun i -> Options.debug := i), "Turn on debugging info level");
   ("--dot", Arg.Set Options.dot, "Print the graph in dot format");
   ("--cudf", Arg.Set Options.cudf, "Print the graph in cudf format");
   ("--plain", Arg.Set Options.plain, "Generate and unstructered graph");
   ("-A", Arg.Float (fun i -> Options.prob := { !Options.prob with a = i} ),
   "default (A = 0.45, B = 0.15, C = 0.15, D = 0.25)");
   ("-B", Arg.Float (fun i -> Options.prob := { !Options.prob with b = i} ),
   "default (A = 0.45, B = 0.15, C = 0.15, D = 0.25)");
   ("-C", Arg.Float (fun i -> Options.prob := { !Options.prob with c = i} ), 
   "default (A = 0.45, B = 0.15, C = 0.15, D = 0.25)");
  ]

module V = struct
  type t = int
  let compare (l1:t) (l2:t) = Pervasives.compare l1 l2
  let hash (l:t) = Hashtbl.hash l
  let equal (l1:t) (l2:t) = (l1 = l2)
end

module G = Graph.Imperative.Digraph.ConcreteBidirectional(V)

module Display =
  struct
    include G
    let vertex_name v = string_of_int v

    let graph_attributes = fun _ -> []
    let get_subgraph = fun _ -> None

    let default_edge_attributes = fun _ -> []
    let default_vertex_attributes = fun _ -> []

    let vertex_attributes v = []
    let edge_attributes e = []
  end

module D = Graph.Graphviz.Dot(Display)

let rec selectVertex (sVertX,eVertX) (sVertY,eVertY) (commA, commB, commC) =
  if abs(sVertX - eVertX) <= 1 && abs(sVertY - eVertY) <= 1 then
    (sVertX, eVertX, sVertY, eVertY)
  else 
    let initialSeed = Random.float(1.0) in
    let midPointX = (eVertX-sVertX) / 2 in
    let midPointY = (eVertY-sVertY) / 2 in

    if initialSeed >= 0.0 && initialSeed < commA then
      let sVertX = sVertX in
      let eVertX = eVertX - midPointX in
      let sVertY = sVertY in
      let eVertY = eVertY - midPointY in
      selectVertex (sVertX, eVertX) (sVertY, eVertY) (commA, commB, commC)
    else if initialSeed >= commA && initialSeed < commB then
      let sVertX = sVertX + midPointX in
      let eVertX = eVertX in
      let sVertY = sVertY in
      let eVertY = eVertY - midPointY in
      selectVertex (sVertX, eVertX) (sVertY, eVertY) (commA, commB, commC)
    else if initialSeed >= commB && initialSeed < commC then
      let sVertX = sVertX in
      let eVertX = eVertX - midPointX in
      let sVertY = sVertY + midPointY in
      let eVertY = eVertY in
      selectVertex (sVertX, eVertX) (sVertY, eVertY) (commA, commB, commC)
    else if initialSeed >= commC && initialSeed <= 1.0 then
      let sVertX = sVertX + midPointX in
      let eVertX = eVertX in
      let sVertY = sVertY + midPointY in
      let eVertY = eVertY in
      selectVertex (sVertX, eVertX) (sVertY, eVertY) (commA, commB, commC)
    else
      assert false
;;

let chooseEdge noOfEdges (s,e) (pA,pB,pC,pD) = 
  let commA = pA in
  let commB = commA +. pB in
  let commC = commB +. pC in 
  let (sVertX,eVertX) = (s,e) in
  let (sVertY,eVertY) = (s,e) in
  let gr = G.create () in
  let add_edge_rmat gr =
    let (sX, eX, sY, eY) =
      selectVertex (sVertX,eVertX) (sVertY,eVertY) (commA, commB, commC) in
    let src = max sX eX in
    let dst = min sY eY in
    if src <> dst then (* no self loops *)
      G.add_edge gr src dst 
  in
  let rec loop f g = function
    |0 -> ()
    |i -> (f g; loop f g (i-1))
  in
(*
  let module C = Graph.Components.Make(G) in
  let module Ch = Graph.Oper.Choose(G) in

  let add_edge_rnd gr =
      let src = Ch.choose_vertex gr in
      let dst = Ch.choose_vertex gr in
      G.add_edge gr src dst;
  in
  let reduce g =
    let (n,f) = C.scc g in
    let i = ref 0 in
    G.iter_edges (fun src dst ->
      if (f src = f dst) && (Random.float(1.0)) < 0.4 then begin
          ( G.remove_edge g src dst ;
          incr i )
      end
    ) g
    ;
    (n,!i)
  in
  let pedg = abs(noOfEdges / 100 * 70) in
  loop add_edge_rmat gr pedg;
  loop add_edge_rnd gr (noOfEdges - pedg);
  let nc = ref 0 in
  (* the number of strongly connected compoenents are 80% of the total n. of vertex *)
  while !nc < abs(!Options.vertex / 100 * 80) do
    let (n,i) = reduce gr in
    if !Options.debug >= 1 then Printf.eprintf "scc %d / edges removed %d\n" n i ;
    flush_all ();
    if (Random.float(1.0)) < 0.6 then
      loop add_edge_rmat gr i
    else
      loop add_edge_rnd gr i
    ;
    nc := n 
  done
*)
loop add_edge_rmat gr noOfEdges
  ;
  gr
;;

(*
let distrib1 n = int_of_float (floor((float_of_int n /. 100.0) *. 80.0))
let distrib2 n nor = 2

let fresh n =
  let i = ref n in
  function () -> (incr i; !i)

let addNode gr fresh vertex edgeList =
  let n = List.length edgeList in
  let npkg = distrib1 n in
  let npkglist,norlist = List.split_nth npkg edgeList in
  List.iter (G.add_edge gr vertex) npkglist ;
  let l = ref norlist in
  let i = ref 0 in
  let nor = n - npkg in
  while !i < nor do
    let vor = fresh () in
    let nchild = 
      let p = distrib2 n nor in
      if npkg + !i + p > n then n - (npkg + !i) else p
    in
    G.add_edge gr vertex vor;
    List.iter (G.add_edge gr vor) (List.take nchild !l);
    l := List.drop nchild !l;
    i := !i + nchild
  done
  ;
  assert ( n = (npkg + !i) )
;;

*)

let rmatGenGraph (pA,pB,pC,pD) =
  let (s,e) = (0,!Options.vertex -1) in
  let gr = chooseEdge !Options.edges (s,e) (pA,pB,pC,pD) in
  gr
;;

let main () =
  let _ =
    try Arg.parse options (fun f -> ()) usage
    with Arg.Bad s -> failwith s
  in
  Random.init !Options.seed ;
  if (!Options.prob.a +. !Options.prob.b +. !Options.prob.c) > 1.0 then
    (Printf.eprintf "wrong probalilities" ; exit 1)
  else if (!Options.prob.a +. !Options.prob.b +. !Options.prob.c) = 0.0 then
    Options.prob := { a = 0.45 ; b = 0.15 ; c = 0.15 }
  ;
  (* let gr = rmatGenGraph (0.20,0.35,0.25,0.0) in *)
  let gr = rmatGenGraph (!Options.prob.a, !Options.prob.b, !Options.prob.c, 0.0) in
  if !Options.dot then D.output_graph stdout gr ;
  if !Options.cudf then () 
;;

main () ;;
