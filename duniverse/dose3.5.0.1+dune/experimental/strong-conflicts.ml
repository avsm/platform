(**************************************************************************************)
(*  Copyright (c) 2009-2010 Jaap Boender                                              *)
(*  Copyright (C) 2009-2010 Mancoosi Project                                          *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(* attempt at computation of strong conflicts with dose3 (TLFKAlibmancoosi) *)

open ExtLib
open Common
open Algo
open Doseparse

module Options = struct
  open OptParse
  let description = "Compute Strong Conflicts"
  let options = OptParser.make ~description
  include StdOptions.MakeOptions(struct let options = options end)

  let log_file = StdOpt.str_option ()
  let out_file = StdOpt.str_option ()

  open OptParser
  add options ~long_name:"log" ~help:"Use log file" log_file;
  add options ~long_name:"output" ~help:"Use output file" out_file;
end

include Util.Logging(struct let label = __FILE__ end) ;;

let lc = ref None;;
let oc = ref stdout;;

let log s = 
  match !lc with
  | None -> ()
  | Some l -> output_string l s
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = ["Strongconflicts_int.local"; "Strongconflicts_int.seeding"] in
  let timers = ["Strongconflicts_int.main"] in
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose);
  StdDebug.enable_bars (OptParse.Opt.get Options.progress) bars;
  StdDebug.enable_timers (OptParse.Opt.get Options.timers) timers;

  if OptParse.Opt.is_set Options.log_file then 
    lc := Some (open_out (OptParse.Opt.get Options.log_file));

  if OptParse.Opt.is_set Options.out_file then
    oc := open_out (OptParse.Opt.get Options.out_file);

  let (_,universe,from_cudf,to_cudf,_) = StdLoaders.load_universe posargs in
  let universe = Depsolver.trim universe in
  let sc = Strongconflicts.strongconflicts universe in

  Strongconflicts.CG.iter_vertex (fun c1 ->
    let nc = Strongconflicts.CG.out_degree sc c1 in 
    Printf.fprintf !oc "%d %s:\n" nc c1.Cudf.package;
    let cf_ht = Hashtbl.create nc in 
    Strongconflicts.CG.iter_succ_e (fun (_, (r1, r2, ct), c2) ->
      (* aggregate strong conflicts by root *)
      try
        let cl = Hashtbl.find cf_ht (r1, r2) in
        Hashtbl.replace cf_ht (r1, r2) ((c2, ct)::cl)
      with Not_found ->
        Hashtbl.add cf_ht (r1, r2) [c2, ct];
    ) sc c1;
    Hashtbl.iter (fun (r1, r2) cl ->
      Printf.fprintf !oc "  %d (%s <-> %s)\n" (List.length cl)
        r1.Cudf.package r2.Cudf.package;
      List.iter (fun (c2, ct) -> 
        Printf.fprintf !oc "    * %s" c2.Cudf.package;
        (match ct with
        |Strongconflicts.Explicit -> Printf.fprintf !oc " (explicit)\n"
        |Strongconflicts.Conjunctive -> Printf.fprintf !oc " (conjunctive)\n"
        |Strongconflicts.Other d ->
            begin
              Printf.fprintf !oc " (other)\n";
              List.iter (function
                |Diagnostic.Dependency (p1, l, pl) ->
                        Printf.fprintf !oc "      - dependency: %s / %s / %s\n"
                          p1.Cudf.package
                          (String.concat ", " (List.rev_map fst l))
                          (String.concat "," (List.rev_map (fun p2 -> p2.Cudf.package) pl));
                |Diagnostic.Missing _    -> Printf.fprintf !oc "      - mss\n";
                |Diagnostic.Conflict _   -> Printf.fprintf !oc "      - cfl\n";
              ) d;
            end)
      ) cl
    ) cf_ht
  ) sc

(*
    Strongconflicts.CG.iter_edges (fun x y ->
          let (x,y) = swap (x,y) in
    Printf.printf "%s <-> %s\n" (CudfAdd.string_of_package x) (CudfAdd.string_of_package y)
    ) g
    ;
    info "Total strong conflicts %d" (Strongconflicts.CG.nb_edges g)
*)
;;

main () ;;
