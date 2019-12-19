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

open IprLib

open Nappe
open Cudf
open ExtLib
open Installer
open Graph

exception Done

module Options =
struct
  let confile = ref ""
  let debug = ref 0
  let dot = ref false
  let info = ref false
  let strong_pred = ref false
  let src = ref ""
  let dst = ref ""
  let cone = ref ""

end

let usage = Printf.sprintf "usage: %s [-options] [cudf doc]" (Sys.argv.(0))
let options =
  [
   ("--confile",  Arg.String (fun l -> Options.confile := l ), "Specify a configuration file" );
   ("-d", Arg.Int (fun i -> Options.debug := i), "Turn on debugging info level");
   ("--dot", Arg.Set Options.dot, "Print the graph in dot format");
   ("--src",  Arg.String (fun l -> Options.src := l ), "Specify a list of packages to analyze" );
   ("--dst",  Arg.String (fun l -> Options.dst := l ), "Specify a pivot package" );
   ("--cone",  Arg.String (fun l -> Options.dst := l ), "Compute the dependency closure" );
   ("--info", Arg.Set Options.info, "Print various aggregate information");
   ("--pred", Arg.Set Options.strong_pred, "Print strong predecessor (not direct)");
  ]

let input_file = ref ""
let file f =
  try
    match f with
    |s when Str.string_match (Str.regexp "^[\n\t ]*$") s 0 ->
        (print_endline usage ; exit 1)
    |_ -> input_file := f
  with _ -> (print_endline usage ; exit 1)

let and_sep_re = Str.regexp "\\s*;\\s*"
let pkg_re = Str.regexp "(\\([a-z][a-z0-9.+-]+\\)\\s*,\\s*\\([0-9][0-9]*\\))"
let parse_pkg s =
  let parse_aux str =
    if Str.string_match pkg_re str 0 then begin
      (Str.matched_group 1 str, int_of_string (Str.matched_group 2 str))
    end
    else
      (Printf.eprintf "Parse error %s\n" str ; exit 1)
  in List.map parse_aux (Str.split and_sep_re s)

(* -------------------------------- *)

let parse_cudf doc =
  try
    let p = Cudf_parser.from_in_channel (open_in doc) in
    (* Printf.eprintf "parsing CUDF ...\n%!"; *)
    Cudf_parser.load p
  with
    Cudf_parser.Parse_error _
    | Cudf.Constraint_violation _ as exn ->
      Printf.eprintf "Error while loading CUDF from %s: %s\n%!"
      doc (Printexc.to_string exn);
      exit 1

(* ----------------------------------- *)

module G = Imperative.Matrix.Digraph 


let dependency_graph_matrix available =
  let gr = G.make (List.length available) in
  let h = Hashtbl.create (List.length available) in
  List.iter (fun (pid,dl,cl) ->
    let vpid = G.V.create pid in
    Hashtbl.add h vpid pid ;
    G.add_vertex gr vpid ;
    List.iter (
      List.iter (fun p ->
        let vp = G.V.create p in
        G.add_vertex gr vp ;
        G.add_edge gr vpid vp
      )
    ) dl
  ) available
  ;
  (gr,h)
;;

let main () =
  let _ =
    try Arg.parse options file usage
    with Arg.Bad s -> failwith s
  in
  let universe = fst(parse_cudf !input_file) in
  let (get_pid,get_name,lookup,conflicts) = Installer.lookup universe in
  let exp = Installer.pkg_exp (lookup,conflicts,get_pid) in
  let availableHash = Hashtbl.create (2 * (Cudf.universe_size universe)) in
  let available =
    Cudf.fold_packages (fun l pkg ->
      let (id,dl,cl) = exp pkg in
      Hashtbl.add availableHash id (dl,cl) ;
      (id,dl,cl)::l
    ) [] universe
  in
  let print_package i =
    let (p,v) = get_name i in
    Printf.sprintf "(%s,%d)" p v
  in
  let (gr,h) = dependency_graph_matrix available in
  let len = List.length available in
  let a = Array.make len [] in
  G.iter_vertex (fun v -> a.(v) <- (G.succ gr v)) gr;
  print_endline "links = [" ;
  Array.iter (fun l ->
    Printf.printf "[%s],\n"
    (String.concat "," (List.map string_of_int l))
  ) a ;
  print_endline "]";
  print_newline ();
  print_endline "conv = {";
  Hashtbl.iter (fun k v ->
    Printf.printf "%d : \"%s\",\n" k (print_package v)
  ) h
  ;
  print_endline "}";

;;

main ();;
