(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Jaap Boender                                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common
open Cudf
open Algo
module Boilerplate=BoilerplateNoRpm

module Options = struct
  open OptParse
  let description = "Do the Treinen Tests"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let out_file = StdOpt.str_option ()
  let do_sd = StdOpt.store_false ()
  let do_sc = StdOpt.store_false ()

  open OptParser
  add options ~long_name:"output" ~help:"Use output file" out_file;
  add options ~long_name:"no-strong-deps" ~help:"Do not do SD test" do_sd;
  add options ~long_name:"no-strong-conflicts" ~help:"Do not do SC test" do_sc;
end
module SD = Defaultgraphs.PackageGraph.G
module SO = Defaultgraphs.PackageGraph.O
module SC = Strongconflicts.CG

let debug fmt = Util.make_debug "StrongConflict" fmt
let info fmt = Util.make_info "StrongConflict" fmt
let warning fmt = Util.make_warning "StrongConflict" fmt

let oc = ref stdout;;

let prio_of x =
begin
  match x with
  | `String y -> y
  | _ -> failwith "Package with wrong-typed priority"
end

let prio_val x =
begin
  if x = "required" then 1
  else if x = "important" then 2
  else if x = "standard" then 3
  else if x = "optional" then 4
  else if x = "extra" then 5
  else failwith (Printf.sprintf "Package with unknown priority (%s)" x)
end

let _ =
begin
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = [
    "Strongdeps_int.main";"Strongdeps_int.conj";
    "StrongDepGraph.transform.edges";"StrongDepGraph.transform.vertex";
    "Strongconflicts_int.local"; "Strongconflicts_int.seeding"
    ]
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) bars;

  if OptParse.Opt.is_set Options.out_file then 
    oc := open_out (OptParse.Opt.get Options.out_file);

  let (universe,from_cudf,to_cudf) = Boilerplate.load_universe posargs in
  let universe = Depsolver.trim universe in
  if OptParse.Opt.get Options.do_sd then
  let sd = Strongdeps.strongdeps_univ universe in
    SO.transitive_reduction sd;
  let sdht = Hashtbl.create 2048 in
  begin
    info "Testing for priority-challenged dependencies...";
    SD.iter_edges (fun p q ->
      let p_prio = prio_val (Cudf.lookup_package_property p "priority")
      and q_prio = prio_val (Cudf.lookup_package_property q "priority") in
        if p_prio < q_prio then
          Hashtbl.add sdht p q
    ) sd;
    let pkgs = List.unique ~cmp:(=%) (List.sort ~cmp:(<%)
      (Hashtbl.fold (fun p q acc -> p::acc) sdht [])) in
      List.iter (fun p ->
        Printf.fprintf !oc "%s (= %s) (priority: %s) depends on:\n"
          p.package (Cudf.lookup_package_property p "number") (Cudf.lookup_package_property p "priority");
        List.iter (fun q ->
          Printf.fprintf !oc "  - %s (= %s) (priority: %s)\n" q.package
            (Cudf.lookup_package_property q "number") (Cudf.lookup_package_property q "priority")
        ) (Hashtbl.find_all sdht p)
      ) pkgs
  end;
  if OptParse.Opt.get Options.do_sc then
  let sc = Strongconflicts.strongconflicts universe in
  begin
    info "Testing for conflicts between optional or lower packages...";
    SC.iter_edges (fun p q ->
      let p_prio = prio_of (List.assoc ("priority") p.pkg_extra)
      and q_prio = prio_of (List.assoc ("priority") q.pkg_extra) in
        if (prio_val p_prio) <= 4 && (prio_val q_prio) <= 4 then
          Printf.fprintf !oc "%s (%s) <-> %s (%s)\n"
            p.package p_prio q.package q_prio
    ) sc
  end
end;;
