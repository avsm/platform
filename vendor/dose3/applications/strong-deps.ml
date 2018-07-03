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
open Common
open Algo
open Doseparse

module Options = struct
  open OptParse
  let description = "Compute the strong dependency graph"
  let options = OptParser.make ~description
  include StdOptions.MakeOptions(struct let options = options end)

  include StdOptions.DistribOptions;;
  StdOptions.DistribOptions.add_debian_options options ;;
  StdOptions.DistribOptions.add_opam_options options ;;

  let dot = StdOpt.store_true ()
  let detrans = StdOpt.store_true ()
  let checkonly = StdOptions.vpkglist_option ()
  let conj_only = StdOpt.store_true ()

  open OptParser ;;
  add options ~long_name:"dot" ~help:"Print the strong dependency graph in dot format" dot;
  add options ~long_name:"detrans" ~help:"Print the transitive reduction of the strong dependency graph" detrans;
  add options ~long_name:"checkonly" ~help:"Check only these packages" checkonly;
  add options ~long_name:"conj-only" ~help:"Use the conjunctive graph only" conj_only;

end

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module G = Defaultgraphs.PackageGraph.G
module O = Defaultgraphs.GraphOper(G)

(* ----------------------------------- *)

let impactlist = Defaultgraphs.PackageGraph.pred_list
let rev_impactlist = Defaultgraphs.PackageGraph.succ_list

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
  let bars = ["Strongdeps_int.main";"Strongdeps_int.conj"] in
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose);
  StdDebug.enable_bars (OptParse.Opt.get Options.progress) bars;
  let options = Options.set_options (Input.guess_format [posargs]) in
  (* let options = default_options (Input.guess_format [posargs]) in *)
  let (_,universe,_,_,to_cudf,_) = StdLoaders.load_universe ~options posargs in
  if OptParse.Opt.is_set Options.checkonly then begin
    let pkglistlist =
        List.map (fun ((n,a),c) ->
          let (name,filter) = Pef.Pefcudf.pefvpkg to_cudf ((n,a),c) in
          Cudf.lookup_packages ~filter universe name
        ) (OptParse.Opt.get Options.checkonly)
    in
    List.iter (fun pkglist ->
      (* if --checkonly we compute the strong dependencies of the pkglist and
       * the we print either the detrans graph or a simple yaml list of packages *)
      let sdgraph =
        if OptParse.Opt.get Options.conj_only then 
          Strongdeps.conjdeps universe pkglist
        else 
          Strongdeps.strongdeps universe pkglist
      in
      if OptParse.Opt.get Options.dot then begin
        if OptParse.Opt.get Options.detrans then
          O.transitive_reduction sdgraph;
        Defaultgraphs.PackageGraph.DotPrinter.output_graph stdout sdgraph;
      end else begin
        let pp_list = Diagnostic.pp_list CudfAdd.pp_package in
        List.iter (fun q -> 
          let l = rev_impactlist sdgraph q in
          Format.printf "@[<v 1>root: %s@," (CudfAdd.string_of_package q);
          if List.length l > 0 then
            Format.printf "@[<v 1>strongdeps:@,%a@]" pp_list l
          else
            Format.printf "@[<v 1>strongdeps: no direct strong dependencies@]";
          Format.printf "@]@."
        ) pkglist
      end
    ) pkglistlist
  end else begin
    let sdgraph = 
      if OptParse.Opt.get Options.conj_only then
        Strongdeps.conjdeps_univ universe
      else 
        Strongdeps.strongdeps_univ universe
    in

    if OptParse.Opt.get Options.dot then begin
      if OptParse.Opt.get Options.detrans then
        O.transitive_reduction sdgraph;
      Defaultgraphs.PackageGraph.DotPrinter.output_graph stdout sdgraph;
    end else begin
      let depgraph = Defaultgraphs.PackageGraph.dependency_graph universe in
      let l = 
        G.fold_vertex (fun p l ->
          let uid = p in
          let strongimpact = List.length (impactlist sdgraph uid) in 
          let rev_strongimpact = List.length (rev_impactlist sdgraph uid) in
          let directimpact = List.length (impactlist depgraph p) in
          let rev_directimpact = List.length (rev_impactlist depgraph p) in
          (p,strongimpact - directimpact, 
            rev_strongimpact, strongimpact, 
            rev_directimpact, directimpact) :: l
        ) depgraph []
      in
      Printf.fprintf stdout "name, #str-out, #str-in, #dir-out, #dir-in, diff\n";
      List.iter (fun (p,diff,rs,s,rd,d) ->
        let pkg = CudfAdd.string_of_package p in
        Printf.fprintf stdout "%s , %d, %d, %d, %d, %d\n" pkg rs s rd d diff
      ) (List.sort ~cmp:(fun (_,x,_,_,_,_) (_,y,_,_,_,_) -> y - x) l);
    end
  end
;;

main ();;
