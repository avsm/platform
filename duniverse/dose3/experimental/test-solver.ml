(**************************************************************************************)
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
module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse

  let verbose = StdOpt.incr_option ()
  let cudf = StdOpt.store_true ()
  let criteria = StdOpt.str_option ()
  let solver = StdOpt.str_option ()
  let out = StdOpt.str_option ()

  let description = "Finda a solution for a CUDF problem, with given optimisation criteria"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'v' ~help:"Print information (can be repeated)" verbose;
  add options                 ~long_name:"out"   ~help:"Output file" out;
  add options ~short_name:'s' ~long_name:"sol"  ~help:"print the cudf solution (if any)" cudf;
  add options                 ~long_name:"solver"  ~help:"specify solver command" solver;
  add options ~short_name:'c' ~long_name:"criteria"  ~help:"specify optimization criteria" criteria;
end

include Util.Logging(struct let label = __FILE__ end) ;;

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

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  match posargs with
  |[f] -> begin
      let (p,l,r) = 
        match Boilerplate.load_cudf f with
        |(None,u,Some r) -> (Cudf.default_preamble,u,r)
        |(Some p,u,Some r) -> (p,u,r)
        |_ ->  fatal "Invalid cudf document: missing request"
      in 
      let criteria = try OptParse.Opt.get Options.criteria with _ -> "-changed" in
      let cmd = try (OptParse.Opt.get Options.solver) with _ -> "aspcud" in
      let r = Depsolver.check_request ~cmd ~callback:pp_solution_callback ~criteria ~explain:true (p,l,r) in
      begin match r with
      |Algo.Depsolver.Error s -> fatal "%s" s
      |Algo.Depsolver.Unsat _ -> fatal "(UNSAT) No Solutions according to the given preferences"
      |Algo.Depsolver.Sat (solpre,soluniv) ->
        if OptParse.Opt.get Options.cudf then
          if not(Option.is_none solpre) then 
            Cudf_printer.pp_preamble stdout (Option.get solpre);
          Cudf_printer.pp_universe stdout soluniv
        end
      end
  |_ -> (Printf.eprintf "Too many arguments\n" ; exit 1)
;;

main () ;;
