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
open Db
open Db.Idbr

module Options =
struct
  let plain = ref false
  let installed_file = ref ""
  let outdir = ref ""
end

let usage = Printf.sprintf "usage: %s [-options] query" (Sys.argv.(0))
let options =
  [
    ("--plain", Arg.Set Options.plain,
    "Do not preserve debian semantic.  Creates a (possibly) unconsistent cudf document.");
    ("--installed", Arg.String (fun l -> Options.installed_file := l),
    "Get the installed packages from a file");
    ("--outdir", Arg.String (fun l -> Options.outdir := l),
    "Specify the results directory");
  ]

let main () =

  let uri = ref "" in
  let _ =
    try Arg.parse options (fun f -> uri := f ) usage
    with Arg.Bad s -> failwith s
  in
  if !uri == "" then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  let l = 
    match Input.parse_uri !uri with
    |(("pgsql"|"sqlite") as dbtype,info, Some query) -> begin
      let db = Backend.open_database dbtype info in
      let db = Backend.init_database db (Idbr.parse_query query) in
      let l = Backend.load_selection db (`All) in
      let tables = Debian.Debcudf.init_tables l in
      List.map (Debian.Debcudf.tocudf tables) l
    end
    |(s,_,_) -> failwith ( s ^ " Not supported")
  in

  let oc =
    if !Options.outdir <> "" then begin
      let dirname = !Options.outdir in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
      open_out (Filename.concat dirname ("res.cudf"))
    end else stdout
  in
  Printf.fprintf oc "%s\n" (
    Cudf_printer.string_of_preamble Debian.Debcudf.preamble
  );
  List.iter (fun pkg ->
    Printf.fprintf oc "%s\n" (Cudf_printer.string_of_package pkg)
  ) l
;;

main ();;

