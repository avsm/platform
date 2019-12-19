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
open Debian
open Common

module Deb = Debian.Packages
module Src = Debian.Sources

(* module Options = struct end *)

let usage = Printf.sprintf "usage: %s [--options] candidates testing unstable sources" Sys.argv.(0) ;;

let options = [
  ("--debug", Arg.Unit (fun () -> Util.set_verbosity Util.Summary), "Print debug information");
];;

(* add a package only if it does not exist or it is a more recent version *)
let debunion u s =
  let tbl = Hashtbl.create (List.length u) in
  List.iter (fun p -> Hashtbl.add tbl p.Deb.name p) u;
  List.iter (fun x ->
    try
      let y = Hashtbl.find tbl x.Deb.name in
      if (Debian.Version.compare y.Deb.version x.Deb.version) = -1 then
          Hashtbl.replace tbl x.Deb.name x
    with Not_found -> 
        Hashtbl.add tbl x.Deb.name x
  ) s ;
  Hashtbl.fold (fun k v acc -> v::acc) tbl []
;;  

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let files = ref [] in
  let _ =
    try Arg.parse options (fun f -> files := !files @ [f] ) usage
    with Arg.Bad s -> failwith s
  in
  if List.length !files < 4 then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  let (candidates_f, testing_f, unstable_f, source_f) =
    match !files with
    |[c;t;u;s] -> (c,t,u,s)
    |_ -> assert false
  in

  Printf.eprintf "Read packages from unstable %!" ;
  let unstable = 
    let l = Deb.input_raw [unstable_f] in
    let h = Hashtbl.create (List.length l) in
    List.iter (fun p -> Hashtbl.add h (p.Deb.name,Some p.Deb.version) p) l ;
    h
  in
  Printf.eprintf "(%d) done\n%!" (Hashtbl.length unstable);

  Printf.eprintf "Read packages from testing %!" ;
  let testing = Deb.input_raw [testing_f] in
  let testinghash = 
    let l = Deb.input_raw [testing_f] in
    let h = Hashtbl.create (List.length l) in
    List.iter (fun p -> Hashtbl.add h (p.Deb.name,Some p.Deb.version) p) l ;
    h
  in
  Printf.eprintf "(%d) done\n%!" (Hashtbl.length testinghash);

  Printf.eprintf "Read source packages %!" ;
  let source = 
    let l = Src.input_raw [source_f] in
    let h = Hashtbl.create (List.length l) in
    List.iter (fun p -> Hashtbl.add h (p.Src.name,p.Src.version) p) l ;
    h
  in
  Printf.eprintf "(%d) done\n%!" (Hashtbl.length source);

  Printf.eprintf "Read Migration Candidates %!" ;
  let candidates =
    let ch = open_in candidates_f in
    let l = ref [] in
    let rex = Str.regexp "[ \t]*:[ \t]*" in
    try while true do
      let line = input_line ch in
      if line = "" then ()
      else
        begin match Str.split rex line with
        |["Package";n] ->
            let line = input_line ch in
            if line = "" then ()
            else 
              begin match Str.split rex line with
              |["Version";v] -> l := (n,Some v)::!l
              |_ -> ()
            end
        |_ -> ()
      end
    done ; assert false 
    with End_of_file -> (close_in ch ; !l)
  in
  Printf.eprintf "(%d) done\n%!" (List.length candidates);

  Printf.eprintf "Cleaning up Migration Candidates\n%!" ;
  (* binary migration candidates. *)
  let mcbin = List.filter_map (fun (n,v) -> 
    try 
      if Hashtbl.mem testinghash (n,v) then begin
         (*
          Printf.printf "Migration candidate %s (= %s) already in testing.  Ignoring\n%!" n (Option.get v);
          *)
        None
      end
      else
        Some (Hashtbl.find unstable (n,v));
    with Not_found -> begin
      (*
      Printf.printf "Migration candidate %s (= %s) not in unstable.  Ignoring\n%!" n (Option.get v);
      *)
      None
    end
    ) candidates
  in

  Printf.eprintf "Computing Migration Set Binaries (Install Check) %!" ;
  (* MS/bin = { p | Inst (p, Testing + MC/bin) == Success } *)
  let unionl = debunion testing mcbin in
  let tables = Debian.Debcudf.init_tables unionl in
  let universe =
    let l = List.map (Debian.Debcudf.tocudf tables) unionl in
    Cudf.load_universe l
  in

  let msbin =
    let mcbincudf =
      List.fold_left (fun acc p ->
        match Cudf.lookup_packages universe p.Deb.name with
        |[pkg] -> pkg::acc
        |_ -> assert false
      ) [] mcbin
    in
    let solver = Depsolver.init universe in
    let l = ref [] in
    let callback = function
      |{Diagnostic.result = Diagnostic.Success fl ;
        request = Diagnostic.Sng p} -> l := p::!l
      |{Diagnostic.result = Diagnostic.Failure (_) ;
        request = Diagnostic.Sng p} as r -> begin
        Printf.printf "Install Dependency problem.\n%!" ;
        Printf.printf "Source package %s cannot migrate because :\n%!"
        (Cudf.lookup_package_property p "Source") ;
        Diagnostic.print ~explain:true stdout r ;
        print_newline ()
      end
      |_ -> assert false
    in
    ignore (Depsolver.listcheck ~callback:callback solver mcbincudf);
    !l
  in

  Printf.eprintf "(%d) done\n%!" (List.length msbin);

  Printf.eprintf "Compute Migration Set Sources (Build Check) %!" ;
  (* MS/source = { S | \forall p \in S, p \in MS/bin } *)
  let mssource = 
    let srchash = Hashtbl.create 200 in
    List.iter (fun pkg ->
      let binname = pkg.Cudf.package in
      let binver = Cudf.lookup_package_property pkg "number" in
      let srcname = Cudf.lookup_package_property pkg "source" in
      let srcver = Cudf.lookup_package_property pkg "sourceversion" in
      try
        let binlist = Hashtbl.find srchash (srcname,srcver) in
        Hashtbl.replace srchash (srcname,srcver) (List.remove binlist (binname,None))
      with Not_found ->
        begin try
          let src = Hashtbl.find source (srcname,srcver) in
          let l = List.remove src.Src.binary (binname,None) in
          Hashtbl.add srchash (srcname,srcver) l
        with Not_found -> (
          Printf.printf
          "Package %s (= %s) is a migration candidate but I cannot find the corresponding source package %s (= %s). Ignoring.\n"
          binname binver srcname srcver ;
          print_newline ();
          )
        end
    ) msbin ;
    let sl = 
      Hashtbl.fold (fun (n,v) l acc ->
        if l = [] then (* XXX or all binaries are already in testing *)
          let src = Hashtbl.find source (n,v) in src::acc
        else 
          begin
          Printf.printf
          "Source package %s (= %s) cannot migrate because the following packages are not migration candidates : %s \n" 
          n v (String.concat "," (List.map (fun (n,_) -> n) l)) ;
          print_newline ();
          acc
        end
      ) srchash []
    in
    let sl = (Debian.Sources.sources2packages "amd64" sl) in
    let u =  sl @ unionl in
    let tables = Debcudf.init_tables u in
    let sourcecudf = List.map (Debcudf.tocudf tables) sl in
    let universe = Cudf.load_universe (List.map (Debcudf.tocudf tables) u) in
    let solver = Depsolver.init universe in
    let l = ref [] in
    let callback = function
      |{Diagnostic.result = Diagnostic.Success fl ;
        request = Diagnostic.Sng p } -> l := p::!l
      |{Diagnostic.result = Diagnostic.Failure (_) ;
        request = Diagnostic.Sng p } as r -> begin
        Printf.printf "Build Dependency problem.\n%!" ;
        Printf.printf "Source package %s cannot migrate because :\n%!"
        (Cudf.lookup_package_property p "Source") ;
        Diagnostic.print ~explain:true stdout r ;
        print_newline ()
      end
      |_ -> assert false
    in
    ignore(Depsolver.listcheck ~callback:callback solver sourcecudf);
    !l
  in

  Printf.eprintf "(%d) done\n%!" (List.length mssource);

  List.iter (fun pkg ->
    let srcname = Cudf.lookup_package_property pkg "Source" in
    let srcver = Cudf.lookup_package_property pkg "Sourceversion" in
    Printf.printf "Source Package %s (= %s) ready to migrate\n%!" srcname srcver
  ) mssource ; 
;;

main ();;
