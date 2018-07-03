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

module Options =
struct
  let verbose = ref false
  let vertex = ref 0
  let edges = ref 0
end

let usage = Printf.sprintf "usage: %s [--options] vertex edges" Sys.argv.(0)

let options =
  [
   ("--verbose",  Arg.Set  Options.verbose,  "print propositional solver diagnostic");
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

let progress label =
  let columns = 75 in
  let full = "* 100.0%" in
  let rotate = "|/-\\" in
  let rotation = ref 0 in
  let b = Buffer.create columns in
  fun (perc,total) -> begin
    Buffer.clear b;
    Buffer.add_char b '\r';
    Buffer.add_string b label;
    let f = floor (1000.0 *. (float perc) /. (float total)) in
    let f = f /. 10.0 in
    if f = 100.0 then Buffer.add_string b full 
    else begin
      rotation := (1 + !rotation) land 3;
      Printf.bprintf b "%c %%%4.1f" rotate.[!rotation] f
    end ;
    Buffer.output_buffer stderr b ;
    flush stderr
  end
;;

let count = ref 1 ;;
let fresh = function () -> (incr count; !count) ;;
let dynarr = DynArray.create () ;;
let add_edge gr src dst =
  DynArray.add dynarr src ;
  DynArray.add dynarr dst ;
  G.add_edge gr src dst 
;;

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

let rec rmat (s,e) (pA,pB,pC,pD) =
  let commA = pA in
  let commB = commA +. pB in
  let commC = commB +. pC in
  let (sVertX,eVertX) = (s,e) in
  let (sVertY,eVertY) = (s,e) in
  let (sX, eX, sY, eY) =
    selectVertex (sVertX,eVertX) (sVertY,eVertY) (commA, commB, commC) in
  let src = max sX eX in
  let dst = min sY eY in
  (* here I can easily compute the euclidian distance *)
  if src <> dst then (* no self loops *) (src,dst)
  else rmat (s,e) (pA,pB,pC,pD)
;;

(* p = alpha * ( (degree(v) + 1) / |V| ) + (1 - alpha) ( 1 / |E|) *)
let rec selectEdges1 ~alpha gr ned nvt =
  let rec pref_attach gr ned nvt =

    let idx = Random.int(DynArray.length dynarr) in 
    let dst = DynArray.get dynarr idx in
    let prob = Random.float(1.0) in
    if prob >= 0.0 && prob <= 0.6 then (fresh (),dst)
    else let src = Random.int(!count) in (src,dst)
  in

  let rec internal gr ned nvt =
    let dst = Random.int(!count) in 
    let src = Random.int(!count) in
    (src,dst)
  in

  let prob = Random.float(1.0) in

  if prob >= 0.0 && prob <= alpha then (* preferential attachement *)
    pref_attach gr ned nvt
  else if prob >= alpha && prob <= 1.0 then (* internal edge *)
    internal gr ned nvt
  else assert false
;;


let random_graph f mv0 vn en =
  let gr = G.create () in
  let rec loop f = function
    |0 -> ()
    |i -> (f (); loop f (i-1))
  in
  (* add initial nodes - no edges *)
  loop (fun () -> G.add_vertex gr (fresh ())) mv0 ;
  (* at each step we add a new node and an edge to an existing node
   * with probablity proportional to its in degree *)
  let progbar = progress "vertex " in
  let trg = ref mv0 in
  let vn' = ((vn / 100) * 70) in
  while !trg < vn' do
    let (src,dst) = rmat (0,vn-1) (0.45,0.15,0.15,0.25) in
    add_edge gr src dst ;
    trg := G.nb_vertex gr ;
    progbar (!trg,vn')
  done;
  Printf.eprintf "\n";
  Printf.eprintf "vertex = %d edges = %d\n" (G.nb_vertex gr) (G.nb_edges gr);
  let progbar = progress "edges " in
  let trg = ref (G.nb_edges gr) in
  while !trg < en do
    let ned = G.nb_edges gr in
    let nvt = G.nb_vertex gr in
    let (src,dst) = f ~alpha:0.4 gr ned nvt in
    add_edge gr src dst ;
    trg := G.nb_edges gr ;
    progbar (!trg,en)
  done ;
  Printf.eprintf "\n";
  Printf.eprintf "vertex = %d edges = %d\n" (G.nb_vertex gr) (G.nb_edges gr);
  gr
;;


let main () =
  Random.init 10 ;
  let args = ref [] in

  let _ =
    try Arg.parse options (fun f -> args := f::!args) usage
    with Arg.Bad s -> failwith s
  in

  let _ = 
    match !args with
    |[e;v] -> begin
        Options.vertex := int_of_string v ;
        Options.edges := int_of_string e ;
    end
    |_ -> (Printf.eprintf "wrong number of parameters\n%s" usage ; exit 1)
  in
 
  let m0 = ((!Options.vertex / 100) * 20) in
  let gr = random_graph selectEdges1 1 !Options.vertex !Options.edges in
  D.output_graph stdout gr
;;


main () ;;
