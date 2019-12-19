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

(* let enable_debug () =
  (* enable the progress bar for strongdeps *)
  Common.Util.set_verbosity Common.Util.Summary
;; *)

module Options = struct
  let load = ref false
  let detrans = ref false
  let dot = ref false
  let text = ref false
  let dump = ref false
end

let usage = Printf.sprintf "usage: %s [--options] doc" (Sys.argv.(0))
let options =
  [
   ("--load", Arg.Set Options.load, "load the marshalled data structure and not the dot file");
   ("--detrans", Arg.Set Options.detrans, "Transitive reduction.");
   ("--text", Arg.Set Options.text, "Diff like output");
   ("--dot", Arg.Set Options.dot, "Dot output");
   (* ("--debug", Arg.Unit enable_debug, "Print debug information"); *)
  ]

(* ----------------------------------- *)

let newedges = Hashtbl.create 1023
let oldedges = Hashtbl.create 1023
let oldnodes = Hashtbl.create 1023
let newnodes = Hashtbl.create 1023

module SG = Defaultgraphs.StrongDepGraph.G
module SO = Defaultgraphs.GraphOper(SG)
module Display = struct
    include SG
    let vertex_name (n,v) = Printf.sprintf "\"(%s,%s)\"" n v

    let graph_attributes = fun _ -> []
    let get_subgraph = fun _ -> None

    let default_edge_attributes = fun _ -> []
    let default_vertex_attributes = fun _ -> []

    let vertex_attributes v =
      if Hashtbl.mem oldnodes v then [`Style `Dotted]
      else if Hashtbl.mem newnodes v then []
      else []

    let edge_attributes e =
      if Hashtbl.mem newedges e then [`Color 255]
      else if Hashtbl.mem oldedges e then [`Style `Dotted]
      else []
  end
module SD = Graph.Graphviz.Dot(Display)
module SDIn = Defaultgraphs.StrongDepGraph.DIn

let load_marshal f =
  let ic = open_in f in
  let graph = ((Marshal.from_channel ic) :> SG.t) in
  close_in ic ;
  graph
;;

let load_dot f = SDIn.parse f

let out ?(dot=false) ?(text=false) ?(dump=false) ?(detrans=false) g =
  if !Options.dump || dump then begin
    SO.transitive_reduction g;
    let oc = open_out "graph.marshal" in
    Marshal.to_channel oc (g :> SG.t) [];
    close_out oc
  end ;

  if !Options.dot || dot then begin
    if !Options.detrans || detrans then
      SO.transitive_reduction g;
    SD.output_graph stdout g;
    print_newline ();
  end ;

  if !Options.text || text then begin
    Printf.printf "Nodes\n" ;
    Hashtbl.iter (fun (n,v) _ ->
      Printf.printf "> (%s,%s)\n" n v
    ) newnodes ;
    Hashtbl.iter (fun (n,v) _ ->
      Printf.printf "< (%s,%s)\n" n v
    ) oldnodes ;

    Printf.printf "Edges\n" ;
    Hashtbl.iter (fun ((nx,vx),(ny,vy)) _ ->
      Printf.printf "> (%s,%s) -> (%s,%s)\n" nx vx ny vy
    ) newedges ;
    Hashtbl.iter (fun ((nx,vx),(ny,vy)) _ ->
      Printf.printf "< (%s,%s) -> (%s,%s)\n" nx vx ny vy
    ) oldedges ;
  end
;;

let diff new_graph old_graph =
  SG.iter_vertex (fun x ->
    if not(SG.mem_vertex old_graph x) then
      Hashtbl.add newnodes x ();
  ) new_graph ;

  SG.iter_vertex (fun x ->
    if not(SG.mem_vertex new_graph x) then
      Hashtbl.add oldnodes x ();
  ) old_graph ;

  SG.iter_edges (fun x y ->
    if not(SG.mem_edge old_graph x y) then
      Hashtbl.add newedges (x,y) ()
  ) new_graph ;

  SG.iter_edges (fun x y ->
    if not(SG.mem_edge new_graph x y) then begin
      Hashtbl.add oldedges (x,y) () ;
      SG.add_edge new_graph x y ;
    end
  ) old_graph ;

  out new_graph 
;;

let main () =
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let files = ref [] in
  let _ =
    try Arg.parse options (fun f -> files := f::!files ) usage
    with Arg.Bad s -> Arg.usage options usage
  in
  match !files with
  |[newf;oldf] when !Options.load = true ->
      diff (load_marshal newf) (load_marshal oldf)
  |[newf;oldf] ->
      diff (load_dot newf) (load_dot oldf)
  |_ -> Arg.usage options usage
;;

main ();;

