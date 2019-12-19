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

(* TODO :
  * write a real command parser
  * add transactions support
  * add support to check if a single package is installable in the universe
  * add support to check if a list of packages are installable together
  * add support to check build-ability
 *) 

open Debian
open Common
module Deb = Debian.Packages

module Options = struct
  let show_successes = ref true
  let show_failures = ref true
  let explain_results = ref false
  let output_xml= ref false
end

let usage = Printf.sprintf "usage: %s [-options] uri" Sys.argv.(0) ;;

let options = [
  ("--explain", Arg.Set Options.explain_results, "Explain the results");
  ("--failures", Arg.Clear Options.show_successes, "Only show failures");
  ("--successes", Arg.Clear Options.show_failures, "Only show successes");
  ("--xml", Arg.Set Options.output_xml, "Output results in XML format");
  ("--debug", Arg.Unit (fun () -> Util.set_verbosity Util.Summary), "Print debug information");
];;

let check universe =
  Printf.eprintf "check packages ...%!";
  let result_printer = function
    |{Diagnostic.result = Diagnostic.Failure (_) } when !Options.show_successes -> ()
    |{Diagnostic.result = Diagnostic.Failure (_) } as r ->
          Diagnostic.print ~explain:!Options.explain_results stdout r 
    |r when !Options.show_failures -> ()
    |r -> Diagnostic.print ~explain:!Options.explain_results stdout r
  in
  let i = Depsolver.univcheck ~callback:result_printer universe in
  Printf.eprintf "Broken Packages: %d\n%!" i;
  Printf.eprintf "done\n%!";
;;

(* add a package only if it does not exist or it is a more recent version *)
let debianadd tbl x =
  try 
    let y = Hashtbl.find tbl x.Deb.name in
    if (Debian.Version.compare y.Deb.version x.Deb.version) = -1 then begin
      Hashtbl.remove tbl y.Deb.name ;
      Hashtbl.add tbl x.Deb.name x
    end
  with Not_found -> Hashtbl.add tbl x.Deb.name x
;;

let init ps =
  Printf.eprintf "init cache (%d packages) ... %!" (Hashtbl.length ps);
  let ul = Hashtbl.fold (fun _ v acc -> v::acc) ps [] in
  let tables = Debian.Debcudf.init_tables ul in
  let cache = Cudf.load_universe (List.map (Debian.Debcudf.tocudf tables) ul) in
  Printf.eprintf "done\n%!";
  cache
;;

(* invariant : ps contains only one version of each package *)
let add ps ch =
  let ll = Debian.Packages.parse_packages_in (debianadd ps) ch in
  if List.length ll = 0 then 
    Printf.eprintf "Nothing to read\n"
  else
    Printf.eprintf "read %d packages\n%!" (List.length ll)
  ;
  ps
;;

let rm ps ch =
  let ll = Debian.Packages.parse_packages_in (fun x -> x) ch in
  if List.length ll = 0 then 
    Printf.eprintf "Nothing to remove\n"
  else (
    Printf.eprintf "remove %d packages\n%!" (List.length ll);
    List.iter (fun p -> Hashtbl.remove ps p.Deb.name) ll
  );
  ps
;;

let create file =
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let _ = try Unix.unlink file with Unix.Unix_error _ -> () in
  let _ = Unix.bind socket (Unix.ADDR_UNIX file) in
  socket
;;

let server ?(timeout=0.0) socket =
  Unix.listen socket 0;
  Unix.setsockopt_float socket Unix.SO_RCVTIMEO timeout;
  let (fd, caller) = Unix.accept socket in
  let inch = Unix.in_channel_of_descr fd in
  inch
;;

let main () =
  let uri = ref "" in
  let _ =
    try Arg.parse options (fun f -> uri := f ) usage
    with Arg.Bad s -> failwith s
  in
  if !uri == "" then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end ;

  let cmdfile = "/tmp/cmd.sock" in
  let inputfile = "/tmp/input.sock" in
  let cmdsock = create cmdfile in
  let inputsock = create inputfile in

  let empty () = Hashtbl.create 30000 in
  let ps = ref (add (empty ()) (Input.open_file !uri)) in
  let cache = ref (init (empty ())) in 

  (* potentially unlimited undos, capped to 10 *)
  let undo = ref [] in
  let push undo ps =
    let l =
      if List.length undo > 9 then begin
        List.rev (List.tl (List.rev undo))
      end
      else undo
    in
    (Hashtbl.copy ps)::l
  in

  let timeout = 60.0 in
  try
    while true do
      begin try 
        Printf.eprintf "> %!";
        let ch = server cmdsock in
        let s = input_line ch in
        Printf.eprintf "%s\n%!" s;
        match s with
        |"add" -> (
          (* try *)
            let inputch = IO.input_channel (server ~timeout:timeout inputsock) in
            undo := push !undo !ps;
            ps := add !ps inputch
          (* with _ -> Printf.eprintf "Timeout\n%!" *)
        )
        |"rm" -> (
          try
            let inputch = IO.input_channel (server ~timeout:timeout inputsock) in
            undo := push !undo !ps;
            ps := rm !ps inputch
          with _ -> Printf.eprintf "Timeout\n%!"
        )
        |"check" -> (cache := init !ps ; check !cache)
        |"undo" -> (
          try ps := List.hd !undo ; undo := List.tl !undo
          with Failure _ -> Printf.eprintf "Nothing to undo\n")
        |_ -> Printf.eprintf "Command Not recognized\n%!"
      with End_of_file -> (print_endline "ddd" ; ()) end
    done
  with
  |Unix.Unix_error (e,s1,s2) ->
      Printf.eprintf "%s %s: %s" (Unix.error_message e) s1 s2
  |e -> prerr_endline (Printexc.to_string e)

  ;

  Pervasives.at_exit (fun () -> 
    (try Unix.unlink cmdfile with Unix.Unix_error _ -> ());
    (try Unix.unlink inputfile with Unix.Unix_error _ -> ());
    Util.dump Format.err_formatter
  );

;;

main () ;;
