(**************************************************************************************)
(*  Copyright (C) 2011 Pietro Abate <abate@pps.jussieu.fr>                            *)
(*  Copyright (C) 2010 Jaap Boender <boender@pps.jussieu.fr>                          *)
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
open Common
open Algo
open Doseparse

module Options = struct
  open OptParse
  let description = "Compute the dominator graph"
  let options = OptParser.make ~description
  include StdOptions.MakeOptions(struct let options = options end)

  include StdOptions.DistribOptions ;;
  StdOptions.DistribOptions.add_debian_options options ;;
  StdOptions.DistribOptions.add_opam_options options ;;

  let naive = StdOpt.store_true ()
  let outfile = StdOpt.str_option ()
  let clean_threshold = StdOpt.int_option ()
  let approximate = StdOpt.float_option ()

  open OptParser ;;
  add options ~short_name:'n' ~long_name:"naive" ~help:"Use a slower algorithm" naive;
  add options ~short_name:'o' ~long_name:"output" ~help:"Send output to file" outfile;
  add options ~long_name:"clean" ~help:"Remove all clusters with less then #n nodes" clean_threshold;
  add options ~long_name:"approx" ~help:"Use approximate strong dominance (with percentage)" approximate;

end

(* ----------------------------------- *)

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module G = Defaultgraphs.PackageGraph.G
module O = Defaultgraphs.PackageGraph.O
module D = Defaultgraphs.PackageGraph.DotPrinter
module S = Defaultgraphs.PackageGraph.S
module Dom = Dominators

let is graph pkg = G.fold_pred (fun p s -> S.add p s) graph pkg (S.singleton pkg) ;;
let scons graph pkg = G.fold_succ (fun p s -> S.add p s) graph pkg (S.singleton pkg) ;;

let clean_graph n g = 
  let nvertex = n in
  let open Defaultgraphs.PackageGraph in
  List.iter (fun l ->
    if List.length l <= nvertex then
      List.iter (G.remove_vertex g) l
  ) (connected_components (undirect g))
;;


let default_options = function
  |`Deb -> Some (
    StdOptions.Deb {
      Debian.Debcudf.default_options with
      Debian.Debcudf.ignore_essential = true
    })
  |`Edsp -> Some (
    StdOptions.Edsp {
      Debian.Debcudf.default_options with
      Debian.Debcudf.ignore_essential = true
    })
  |`Pef -> Some (StdOptions.Pef Debian.Debcudf.default_options)
  |_ -> None
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let timers = [
    "Strongdeps_int.strong";"Strongdeps_int.conjdep";
    "Algo.Dominators.dom_transitive_reduction";
    "Algo.Dominators.sd_transitive_reduction";
    "Algo.Dominators.dominators_direct"; "Algo.Dominators.dominators_tarjan" ] 
  in
  let bars = [
    "Strongdeps_int.main"; "Strongdeps_int.conj";
    "Algo.dominators"; "Defaultgraph.GraphOper.transitive_reduction"
  ]
  in
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose); 
  Util.Debug.disable "Depsolver_int";
  Util.Debug.disable "Strongdeps_int";
  StdDebug.enable_timers (OptParse.Opt.get Options.timers) timers;
  StdDebug.enable_bars (OptParse.Opt.get Options.progress) bars; 
  StdDebug.all_quiet (OptParse.Opt.get Options.quiet);
  (* let options = default_options (Input.guess_format [posargs]) in *)
  let options = Options.set_options (Input.guess_format [posargs]) in
  let (_,universe,_,_,_,_) = StdLoaders.load_universe ~options posargs in

  let dom_graph =
    if OptParse.Opt.get Options.naive then
      let g = Strongdeps.strongdeps_univ ~transitive:true universe in
      let relative = OptParse.Opt.opt Options.approximate in
      Dom.dominators_direct ~relative g
    else
      let g = Strongdeps.strongdeps_univ ~transitive:true universe in
      Dom.dominators_tarjan g
  in
  if OptParse.Opt.is_set Options.clean_threshold then 
    clean_graph (OptParse.Opt.get Options.clean_threshold) dom_graph;
  let oc =
    if OptParse.Opt.is_set Options.outfile then
      open_out (OptParse.Opt.get Options.outfile)
    else
      stdout
  in
  D.output_graph oc dom_graph;
  if oc <> stdout then close_out oc
;;

main () ;;
