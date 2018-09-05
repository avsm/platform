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

(* More info about small world networks
  http://en.wikipedia.org/wiki/Small-world_network *)

open Common
open Algo
open Doseparse

module Options = struct
  open OptParse
  let description = "Compute Small World statistic"
  let options = OptParser.make ~description
  include StdOptions.MakeOptions(struct let options = options end)
    
  let generic = StdOpt.store_true ()
  let scatterplots = StdOpt.store_true ()
  let connectivity = StdOpt.store_true ()
  let components = StdOpt.store_true ()
  let smallworld = StdOpt.store_true ()
  let centrality = StdOpt.store_true ()
  let strong_deps = StdOpt.store_true ()
  let detrans = StdOpt.store_true ()
  let closure = StdOpt.store_true ()
  let trim = StdOpt.store_true ()
  let combine_scatter = StdOpt.store_true ()
  let prefix = StdOpt.str_option ~default:"" ()

  open OptParser ;;
  add options ~long_name:"prefix" ~help:"Prefix output fils with <prefix>" prefix;
  add options ~short_name:'g' ~long_name:"generic" ~help:"" generic;
  add options ~short_name:'p' ~long_name:"scatterplots" ~help:"" scatterplots;
  add options ~short_name:'c' ~long_name:"connectivity" ~help:"" connectivity;
  add options ~short_name:'m' ~long_name:"components" ~help:"" components;
  add options ~short_name:'s' ~long_name:"smallworld" ~help:"" smallworld;
  add options ~short_name:'e' ~long_name:"centrality" ~help:"" centrality;
  add options ~long_name:"strong-deps" ~help:"" strong_deps;
  add options ~long_name:"combine-scatter" ~help:"" combine_scatter;
  add options ~long_name:"detrans" ~help:"" detrans;
  add options ~long_name:"transitive-closure" ~help:"" closure;
  add options ~long_name:"trim" ~help:"Consider only installable packages" trim;
end

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

(**********************************)

(* ounit anybody :) *)
type test_fun = unit -> unit
type test =
  |TestCase of test_fun
  |TestList of test list
  |TestLabel of string * test

let (>:) s t = TestLabel(s,t)
let (>::) s f = TestLabel(s, TestCase(f))
let (>:::) s l = TestLabel(s, TestList(l))

let print_test s f = (fun _ -> Printf.printf s f ; ())

let rec run outch = function
  |TestCase f -> ( f () ; Printf.fprintf outch "\n")
  |TestList l -> ( Printf.fprintf outch "\n"; List.iter (run outch) l )
  |TestLabel (l,t) -> ( Printf.fprintf outch "%s : " l ; (run outch) t )
;;

(**********************************)

module G = Defaultgraphs.PackageGraph.G
module O = Defaultgraphs.GraphOper(G)
module S = Statistics.Make(G)

let saveplot h outfile =
  let out = open_out outfile in
  Printf.fprintf out "#count degree\n" ;
  Hashtbl.iter (fun n i -> Printf.fprintf out "%d %d\n" i n ) h;
  close_out out
;;

let saveplot2 h outfile =
  let out = open_out outfile in
  Printf.fprintf out "#count in_degree out_degree\n" ;
  Hashtbl.iter (fun (n1, n2) i -> Printf.fprintf out "%d %d %d\n" i n1 n2) h;
  close_out out
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose);
  let universe = 
    let (_,u,_,_,_,_) = StdLoaders.load_universe posargs in
    if OptParse.Opt.get Options.trim then
      Depsolver.trim ~global_constraints:false u
    else u
  in
  let gr = 
    let gr' = if OptParse.Opt.get Options.strong_deps then
      Strongdeps.strongdeps_univ universe
    else 
      Defaultgraphs.PackageGraph.dependency_graph universe 
    in
    if OptParse.Opt.get Options.detrans then O.transitive_reduction gr';
    if OptParse.Opt.get Options.closure then O.O.transitive_closure gr'
    else gr'
  in
  let zdpgr = S.removezdp gr in
  let prefix = OptParse.Opt.get Options.prefix in
  let outch = if prefix = "" then stdout else open_out ( prefix ^ "stats" ) in
  let generic = "Generic" >::: [
    "  Vertex" >:: (fun _ -> Printf.fprintf outch "%d" (G.nb_vertex gr));
    "  Edges" >:: (fun _ -> Printf.fprintf outch "%d" (G.nb_edges gr));
    ]
  in
  let connectivity = "Connectivity" >::: [
    "  Average Degree"  >:: (fun _ -> Printf.fprintf outch "%.02f" (S.averageDegree gr));
    "  Density" >:: (fun _ -> Printf.fprintf outch "%0.5f" (S.density gr));
    "  Zero-degree Packages" >:: (fun _ -> 
      let total = float_of_int (G.nb_vertex gr) in
      let zdp = float_of_int (S.zdp gr) in
      let percent = (zdp /. total) *. 100. in
      Printf.fprintf outch "%0.f (%0.2f%% of %0.f)" zdp percent total
    );
    ]
  in
  let scc = S.components gr in
  let scczdp = S.components zdpgr in
  let componentsSC = "Strongly Connected Components" >::: [
    "  Number of Components SC"  >:: (fun _ -> Printf.fprintf outch "%d" (S.numberComponents scc));
    "  Average Components SC" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageComponents scc));
    "  Larges Component SC" >:: (fun _ -> Printf.fprintf outch "%d" (S.largestComponent scc));
    "  Number of Components SC (zdp)"  >:: (fun _ -> Printf.fprintf outch "%d" (S.numberComponents scczdp));
    "  Average Components SC (zpd)" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageComponents scczdp));
    ]
  in
  let wcc = S.weaklycomponents gr in
  let wcczdp = S.weaklycomponents zdpgr in
  let componentsWC = "Weakly Connected Components" >::: [
    "  Number of Components WC"  >:: (fun _ -> Printf.fprintf outch "%d" (S.numberComponents wcc));
    "  Average Components WC" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageComponents wcc));
    "  Larges Component WC" >:: (fun _ -> Printf.fprintf outch "%d" (S.largestComponent wcc));
    "  Number of Components WC (zdp)"  >:: (fun _ -> Printf.fprintf outch "%d" (S.numberComponents wcczdp));
    "  Average Components WC (zdp)" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageComponents wcczdp));
    ]
  in
  let smallworld = "Small World" >::: [
    "  Clustering Coefficient" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.clustering gr));
    "  Average Shortest Path Length" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageShortestPathLength gr));
    "  Average two step reach" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageTwoStepReach gr));
    ]
  in
  let centrality = "Centrality" >::: [
    "  Centrality Out Degree" >:: (fun _ -> Printf.fprintf outch "%0.5f" (S.centralityOutDegree gr));
    "  Centrality In Degree" >:: (fun _ -> Printf.fprintf outch "%0.5f" (S.centralityInDegree gr));
    "  Centrality Out Degree (zdp)" >:: (fun _ -> Printf.fprintf outch "%0.5f" (S.centralityOutDegree zdpgr));
    "  Centrality In Degree (zdp)" >:: (fun _ -> Printf.fprintf outch "%0.5f" (S.centralityInDegree zdpgr));
    ]
  in
  let scatterplots = "Scattered Plots" >:::
    if OptParse.Opt.get Options.combine_scatter then
      [
        "  Combined" >:: (fun _ ->
          saveplot2 (S.scatteredPlotBoth gr) (prefix^"degree.data"); print_string "Done";)
      ]
    else
    [
      "  Scattered Plot In" >:: (fun _ ->
        saveplot (S.scatteredPlotIn gr) (prefix^"indegree.data") ; print_string "Done" );
      "  Scattered Plot Out" >:: (fun _ ->
        saveplot (S.scatteredPlotOut gr) (prefix^"outdegree.data"); print_string "Done" )
  (*    "Hops Plot" >:: (fun _ ->
        saveplot (S.hopsplot gr 30) "hopsplot.data"; print_string "Done" );
        *) 
      ]
  in
  let t = ref [] in
  if not (OptParse.Opt.get Options.scatterplots) then t := scatterplots :: !t ;
  if not (OptParse.Opt.get Options.centrality) then t := centrality :: !t ;
  if not (OptParse.Opt.get Options.smallworld) then t := smallworld :: !t ;
  if not (OptParse.Opt.get Options.components) then t := componentsWC :: componentsSC :: !t ;
  if not (OptParse.Opt.get Options.connectivity) then t := connectivity :: !t ;
  if not (OptParse.Opt.get Options.generic) then t := generic :: !t ;
  run outch ("Dependency Graph Statistical Analysis" >::: !t);
  close_out outch;
;;

main () ;;
