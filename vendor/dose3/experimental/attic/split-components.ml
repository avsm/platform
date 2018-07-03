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

open Cudf
open ExtLib
open Common

module PGraph = Defaultgraphs.PackageGraph

module Options =
struct
  open OptParse

  let debug = StdOpt.store_true ()
  let quite = StdOpt.store_false ()
  let outdir = StdOpt.str_option ()

  let description = "Create one dot file for connected component of the dependecy graph"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options                 ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'q' ~long_name:"quite" ~help:"Do not print additional info" quite;
  add options                 ~long_name:"outdir" ~help:"specify the output directory" outdir;
end;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  let (universe,from_cudf,to_cudf) = Boilerplate.load_universe posargs in
  let dg = (PGraph.dependency_graph universe) in

  let output_ch p =
    match OptParse.Opt.opt Options.outdir with
    |None -> open_out (Printf.sprintf "%s.dot" p)
    |Some s -> open_out (Printf.sprintf "%s/%s.dot" s p)
  in

  let l =
    let cmp g1 g2 = (PGraph.UG.nb_vertex g2) - (PGraph.UG.nb_vertex g1) in
    List.sort ~cmp:cmp (PGraph.connected_components (PGraph.undirect dg))
  in
  List.iter (fun cc ->
    let l = ref [] in
    PGraph.UG.iter_vertex (fun v -> l := v :: !l) cc;
    let u = Cudf.load_universe !l in
    let g = PGraph.dependency_graph u in
    let maxv = ref (0, ref (List.hd(!l))) in
    PGraph.G.iter_vertex (fun v ->
      let d = PGraph.G.in_degree g v in
      if d > fst(!maxv) then
        maxv := (d, ref v)
    ) g;
    if OptParse.Opt.get Options.quite then
      Printf.printf "package:%s\nnumber of nodes:%d\nmax inbound:%d\n\n" 
      (!(snd(!maxv))).package
      (PGraph.G.nb_vertex g)
      (fst(!maxv));
    let outch = output_ch (!(snd(!maxv))).package in
    PGraph.D.output_graph outch g;
    close_out outch
  ) l
;;

main ();;
