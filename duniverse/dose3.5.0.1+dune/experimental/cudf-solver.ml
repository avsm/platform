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
open DoseparseNoRpm

module Options = struct
  open OptParse
  let description = "Check if there exists a (non optimal) solution for a cudf problem"
  let options = OptParser.make ~description
  include StdOptions.MakeOptions(struct let options = options end)
    
  let verbose = StdOpt.incr_option ()
  let cudf = StdOpt.store_true ()
  let summary = StdOpt.store_true ()
  let action = StdOpt.store_true ()
  let critrem = StdOpt.store_true ()
  let critnew = StdOpt.store_true ()
  let critparanoid = StdOpt.store_true ()
  let critchg = StdOpt.store_true ()

  open OptParser

  include StdOptions.OutputOptions ;;
  StdOptions.OutputOptions.add_options options ;;

  add options ~long_name:"cudf"  ~help:"print the cudf solution (if any)" cudf;
  add options ~long_name:"action"  ~help:"print the action graph (if any)" action;
  add options ~long_name:"summary"  ~help:"print the installation summary (if any)" summary;
  add options ~short_name:'n' ~long_name:"new"  ~help:"specify an optimization criteria" critnew;
  add options ~short_name:'p' ~long_name:"paranoid"  ~help:"specify an optimization criteria" critparanoid;
  add options ~short_name:'r' ~long_name:"rem"  ~help:"specify an optimization criteria" critrem;
  add options ~short_name:'c' ~long_name:"chg"  ~help:"specify an optimization criteria" critchg;

end

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let pp_solution oc = function
  |{Diagnostic.result = Diagnostic.Success (f)} ->
      let is = f ~all:true () in
      Cudf_printer.pp_packages oc is
  |_ -> assert false

let pp_solution_callback (criteria,res) =
  match res with
  |{Diagnostic.result = Diagnostic.Success (f)} ->
      let is = f ~all:true () in
      Printf.eprintf "-----------\n";
      Cudf_printer.pp_packages stderr is;
      Printf.eprintf "Criteria: %s\n" (String.concat "," (List.map string_of_int (Array.to_list criteria)));
      Printf.eprintf "-----------\n"
  |_ -> assert false

let pp_summary ?(verbose=false) fmt summary =
  let open CudfDiff in
  let pp_pkg_list fmt l = Format.fprintf fmt "%s" (String.concat "," (List.map CudfAdd.string_of_package l)) in
  let pp_pkg_list_tran fmt l = pp_pkg_list fmt (List.map snd l) in
  if summary.install <> [] then
    Format.fprintf fmt "%d to install " (List.length summary.install);
  if summary.remove <> [] then
    Format.fprintf fmt "%d to remove " (List.length summary.remove);
  if summary.upgrade <> [] then
    Format.fprintf fmt "%d to upgrade " (List.length summary.upgrade);
  if summary.downgrade <> [] then
    Format.fprintf fmt "%d to downgrade " (List.length summary.downgrade);
  if summary.notchange <> [] then
    Format.fprintf fmt "%d not changed " (List.length summary.notchange);

  Format.fprintf fmt " @.";

  if summary.install <> [] then
    Format.fprintf fmt "Installed: %a@." pp_pkg_list summary.install;
  if summary.remove <> [] then
    Format.fprintf fmt "Removed: %a@." pp_pkg_list summary.remove;
  if summary.upgrade <> [] then
    Format.fprintf fmt "Upgraded: %a@." pp_pkg_list_tran summary.upgrade;
  if summary.downgrade <> [] then
    Format.fprintf fmt "Downgraded: %a@." pp_pkg_list_tran summary.downgrade;
  if summary.notchange <> [] && verbose then
    Format.fprintf fmt "UnChanged: %a@." pp_pkg_list summary.notchange;
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  StdDebug.enable_debug(OptParse.Opt.get Options.verbose);
  StdDebug.all_quiet (OptParse.Opt.get Options.quiet);
  match posargs with
  |[f] ->
      let (p,l,r) = 
        match StdLoaders.load_cudf f with
        |(None,u,Some r) -> (Cudf.default_preamble,u,r)
        |(Some p,u,Some r) -> (p,u,r)
        |_ ->  fatal "Not a void cudf document (missing request)"
      in 
      let r = 
        if OptParse.Opt.get Options.critparanoid then 
          Depsolver.check_request ~callback:pp_solution_callback ~criteria:"-count(removed),-count(new)" (p,l,r) 
        else if OptParse.Opt.get Options.critrem then 
          Depsolver.check_request ~callback:pp_solution_callback ~criteria:"-count(removed)" (p,l,r) 
        else if OptParse.Opt.get Options.critnew then
          Depsolver.check_request ~callback:pp_solution_callback ~criteria:"-count(new)" (p,l,r) 
        else if OptParse.Opt.get Options.critchg then
          Depsolver.check_request ~callback:pp_solution_callback ~criteria:"-count(changed)" (p,l,r) 
        else
          Depsolver.check_request (p,l,r) 
      in
      begin match r with
      |Algo.Depsolver.Error s ->
          fatal "%s" s
      |Algo.Depsolver.Unsat _ ->
          fatal "(UNSAT) No Solutions according to the given preferences"
      |Algo.Depsolver.Sat (solpre,soluniv) ->
        if OptParse.Opt.get Options.cudf then
          begin
          if not(Option.is_none solpre) then 
            Cudf_printer.pp_preamble stdout (Option.get solpre);
          Cudf_printer.pp_universe stdout soluniv
          end else
            if OptParse.Opt.get Options.action then
	    let filter p = p.Cudf.package <> "dose-dummy-request" in
          let soluniv = Cudf.load_universe (Cudf.get_packages ~filter soluniv) in
          let diff = CudfDiff.make_difference ~universe:l ~solution:soluniv in
          let summary = CudfDiff.make_summary l diff in
          (* add package to recompile, upgrade and downgrade *)
          let install,remove =
            let module S = CudfAdd.Cudf_set in
            let (i,r) = (S.empty, S.empty) in
            let i = List.fold_left (fun s p -> S.add p s) i summary.CudfDiff.install in
            let r = List.fold_left (fun s p -> S.add p s) r summary.CudfDiff.remove in
            let (i,r) = List.fold_left (fun (i,r) (p1,p2) -> (S.add p2 i,S.add p1 r)) (i,r) summary.CudfDiff.upgrade in
            let (i,r) = List.fold_left (fun (i,r) (p1,p2) -> (S.add p1 i,S.add p2 r)) (i,r) summary.CudfDiff.downgrade in
            let (i,r) =
              List.fold_left (fun (i,r) p ->
                if p.Cudf.installed then (S.add p i,S.add p r) else (i,r)
              ) (i,r) summary.CudfDiff.notchange in
            i,r
          in
          let g = Depsolver.installation_graph ~solution:soluniv (install,remove) in
          let al = Defaultgraphs.ActionGraph.get_partial_order g in
          List.iter (fun l ->
            List.iter (function
              |Defaultgraphs.ActionGraph.PkgV.Install p -> Printf.printf "Install %s\n" (CudfAdd.string_of_package p)
              |Defaultgraphs.ActionGraph.PkgV.Remove p -> Printf.printf "Remove %s\n" (CudfAdd.string_of_package p)
            ) l;
            Printf.printf "\n"
          ) (List.rev al);
          Defaultgraphs.ActionGraph.DotPrinter.print Format.std_formatter g
        else if OptParse.Opt.get Options.summary then
          print_endline "";
          let diff = CudfDiff.make_difference ~universe:l ~solution:soluniv in
          let summary = CudfDiff.make_summary l diff in
          pp_summary Format.std_formatter summary
      end
  |_ -> (Printf.eprintf "Too many arguments\n" ; exit 1)
;;

main () ;;
