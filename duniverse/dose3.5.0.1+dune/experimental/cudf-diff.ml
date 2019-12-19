(**************************************************************************************)
(*  Copyright (C) 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2010 Mancoosi Project                                               *)
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

let debug fmt = Util.make_debug __FILE__ fmt
let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt
let fatal fmt = Util.make_fatal __FILE__ fmt

module Cudf_set = CudfAdd.Cudf_set
module StringSet = CudfAdd.StringSet

module Options = struct
  open OptParse
  let description = 
    "Compare two or more solutions.\n"^
    "cudf-diff problemfile solver1:solutionfile1 solver2:solutionfile2 ..."
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)
end

let pp_set ~inst fmt s =
  let install pkg = if pkg.Cudf.installed && inst then "*" else "" in
  let rec aux fmt s =
    if (Cudf_set.cardinal s) = 1 then
      let v = Cudf_set.min_elt s in
      Format.fprintf fmt "@,%d%s" 
      v.Cudf.version (install v)
    else begin
      let v = Cudf_set.min_elt s in
      Format.fprintf fmt "@,%d%s," v.Cudf.version (install v);
      aux fmt (Cudf_set.remove v s) 
    end
  in
  if Cudf_set.is_empty s then ()
  else Format.fprintf fmt "@[%a@]" aux s

let rec pp_list ?(sep="") pp_element fmt = function
  |[h] -> Format.fprintf fmt "%a" pp_element h
  |h::t ->
      Format.fprintf fmt "%a%s@,%a"
      pp_element h sep (pp_list ~sep pp_element) t
  |[] -> ()
;;

let pp_cell fmt cell = Format.fprintf fmt "%s" cell

let pp_header widths fmt header =
  let first_row = Array.map (fun x -> String.make (x + 1) ' ') widths in
  Array.iteri (fun j cell ->
    Format.pp_set_tab fmt ();
    for z=0 to (Buffer.length header.(j)) - 1 do cell.[z] <- Buffer.nth header.(j) z  done;
    Format.fprintf fmt "%s" cell
  ) first_row
;;

let pp_row pp_cell fmt row =
  Array.iteri (fun j cell ->
    Format.pp_print_tab fmt ();
    Format.fprintf fmt "%a" pp_cell cell
  ) row
;;

let pp_tables pp_row fmt (header,table) =
  (* we build with the largest length of each column of the 
   * table and header *)
  let widths = Array.create (Array.length table.(0)) 0 in
  Array.iter (fun row ->
    Array.iteri (fun j cell ->
      widths.(j) <- max (Buffer.length cell) widths.(j)
    ) row
  ) table;
  Array.iteri (fun j cell ->
    widths.(j) <- max (Buffer.length cell) widths.(j)
  ) header;

  (* open the table box *)
  Format.pp_open_tbox fmt ();

  (* print the header *)
  Format.fprintf fmt "%a" (pp_header widths) header;
  (* print the table *)
  Array.iter (pp_row fmt) table;

  (* close the box *)
  Format.pp_close_tbox fmt ();
;;

let unchanged sols all pkgname =
  Array.exists (fun (solname,(filename,h)) ->
    let s = Hashtbl.find h pkgname in
    not(Cudf_set.equal all s.CudfDiff.unchanged)
  ) sols

let allequal sols pkgname =
  let (solname,(filename,h)) = sols.(0) in
  let ss = Hashtbl.find h pkgname in
  Array.exists (fun (solname,(filename,h)) ->
    let s = Hashtbl.find h pkgname in
    not(
      (Cudf_set.equal s.CudfDiff.installed ss.CudfDiff.installed) &&
      (Cudf_set.equal s.CudfDiff.removed ss.CudfDiff.removed) &&
      (Cudf_set.equal s.CudfDiff.unchanged ss.CudfDiff.unchanged)
    )
  ) sols

let pp_diff fmt (univ,hl) =
  let header = Array.init ((List.length hl) + 1) (fun _ -> Buffer.create 50) in
  let a_hl = Array.of_list hl in
  Format.bprintf header.(0) "package names";
  Array.iteri (fun i (solname,_) -> Format.bprintf header.(i+1) "%s" solname) a_hl;
  let names = CudfAdd.pkgnames univ in
  let table = 
    Array.init (StringSet.cardinal names) (fun _ ->
      Array.init ((List.length hl) + 1) (fun _ -> Buffer.create 50)
    )
  in
  let i = ref 0 in
  StringSet.iter (fun pkgname ->
    let all = CudfAdd.to_set (Cudf.lookup_packages univ pkgname) in
    if allequal a_hl pkgname then begin
      Format.bprintf table.(!i).(0) "%s{%a}" pkgname (pp_set ~inst:true) all;
      for j = 0 to (List.length hl) - 1 do
        let (solname,(filename,h)) = a_hl.(j) in
        let s = Hashtbl.find h pkgname in
        let pp_elem fmt = function
          |CudfDiff.In s -> Format.fprintf fmt "In{%a}" (pp_set ~inst:true) s
          |CudfDiff.Un s -> Format.fprintf fmt "Un{%a}" (pp_set ~inst:true) s
          |CudfDiff.Rm s -> Format.fprintf fmt "Rm{%a}" (pp_set ~inst:false) s
          |CudfDiff.Dw s -> Format.fprintf fmt "Dw{%a}" (pp_set ~inst:true) s
          |CudfDiff.Up s -> Format.fprintf fmt "Up{%a}" (pp_set ~inst:true) s
        in
        let l = CudfDiff.uniqueversion all s in
        Format.bprintf (table.(!i).(j+1)) "@[<h>%a@]" (pp_list ~sep:"," pp_elem) l  
      done;
    end;
    incr i;
  ) names;
  let pp_buffer fmt t = Format.fprintf fmt "%s" (Buffer.contents t) in 
  Format.fprintf fmt "@[%a@]" (pp_tables (pp_row pp_buffer)) (header,table)
;;

let parse_univ f1 =
  match Boilerplate.load_cudf f1 with
  |_,_,None -> fatal "file %s is not a valid cudf document" f1
  |_,u,Some r -> u,r
;;

let check_sol u r s =
  match Cudf_checker.is_solution (u,r) s with
  |false,reasonlist ->
      (List.iter (fun r ->
        Printf.eprintf "%s\n" (Cudf_checker.explain_reason r)
      ) reasonlist;
      false)
  |true,_ -> true
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [];

  match posargs with
  |[] -> (Printf.eprintf "You must specify at least a universe and a solution\n" ; exit 1)
  |[u] -> (Printf.eprintf "You must specify at least a solution\n" ; exit 1)
  |u::l ->
      let (univ,req) = parse_univ u in
      let hl =
        List.filter_map (fun f ->
          let (h,f) =
            match Str.split (Str.regexp ":") f with
            |[f] -> (f,f)
            |[h;f] -> (h,f)
            |_ -> assert false
          in
          let (_,s,_) = Boilerplate.load_cudf f in
          if check_sol univ req s then Some (h,(f,s))
          else (Printf.eprintf "%s is not a valid solution. Discarded\n" f ; None)
        ) l
      in
      let sollist = List.map (fun (h,(f,s)) -> (h,(f,CudfDiff.diff univ s))) hl in
      let fmt = Format.std_formatter in
      Format.fprintf fmt "@[%a@]@." pp_diff (univ,sollist);
;;

main ();;
