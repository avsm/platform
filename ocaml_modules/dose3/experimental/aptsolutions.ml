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

(* parse apt-get -s output and generate a cudf solution of the problem *)

open ExtLib
open ExtString
open Common
module Boilerplate = BoilerplateNoRpm

include Util.Logging(struct let label = __FILE__ end) ;;

module Options = struct
  open OptParse
  let description = "parse an apt-get solution and generate a cudf solution"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let source = StdOpt.store_true ()
  let outdir = StdOpt.str_option ()
  let native_arch = StdOpt.str_option ()

  open OptParser
  add options ~long_name:"outdir" ~help:"Send output to a file" outdir;
  add options ~long_name:"from-cudf" ~help:"do no consider \"number\" property" source;
  add options ~long_name:"native-arch" ~help:"native architecture" native_arch;

end

(* ========================================= *)

let main () =
  let (doc,apt) =
    match OptParse.OptParser.parse_argv Options.options with
    |[] -> fatal "No input file specified"
    |_::[] -> fatal "a  bit more input"
    |doc::apt::_ -> (doc,apt)
  in

  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);

  let (preamble,universe) =
    match Input.parse_uri doc with
    |(Url.Cudf,(_,_,_,_,file),_) -> begin
      let p, u, _ = Boilerplate.load_cudf file in (p,u)
    end
    |(_,_,_) -> fatal "Only Cudf is supported"
  in

  let (install,remove) =
    let ch = Input.open_file apt in
    let (install,remove) = (ref [] , ref []) in
    (* Remv [old version] *)
    let re_rem = Str.regexp "^Remv \\([^ ]*\\) \\[\\([^]]*\\)\\].*$" in
    (* Inst [old version] (new version).* *)
    let re_inst = Str.regexp "^Inst \\([^ ]*\\) (\\([^ ]*\\) .*).*$" in
    let re_up = Str.regexp "^Inst \\([^ ]*\\) \\[\\([^]]*\\)\\] (\\([^ ]*\\) .*).*$" in
    begin try while true do
      let line = IO.read_line ch in
      if Str.string_match re_rem line 0 then begin
          let n = Str.matched_group 1 line in
          let v = Str.matched_group 2 line in
          info "remove %s %s" n v;
          let n = CudfAdd.encode ("i386:"^n) in
          remove := (n,v) :: !remove
      end
      else if Str.string_match re_inst line 0 then begin
          let n = Str.matched_group 1 line in
          let v = Str.matched_group 2 line in
          info "install %s %s" n v;
          let n = CudfAdd.encode ("i386:"^n) in
          install := (n,v) :: !install
      end
      else if Str.string_match re_up line 0 then begin
          let n = Str.matched_group 1 line in
          let vold = Str.matched_group 2 line in
          let vnew = Str.matched_group 3 line in
          info "upgrade %s %s -> %s" n vold vnew;
          let n = CudfAdd.encode ("i386:"^n) in
          install := (n,vnew) :: !install ;
          remove := (n,vold) :: !remove
      end
    done with IO.No_more_input -> () end;
    (!install,!remove)
  in

  if (List.length install) = 0 && (List.length remove) = 0 then begin
    info "Empty solution or not a solution";
    exit 0
  end;

  let t = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.iter_packages (fun pkg ->
    let n = pkg.Cudf.package in
    let v = 
      if OptParse.Opt.get Options.source then
        string_of_int pkg.Cudf.version
      else
        Cudf.lookup_package_property pkg "number" 
    in
    Hashtbl.add t (n,v) pkg;
  ) universe;

  List.iter (fun (n,v) ->
    if v = "" then
      List.iter (fun pkg ->
        Printf.eprintf "Remove package %s with version %s\n" n
        (Cudf.lookup_package_property pkg "number");
        Hashtbl.replace t (n,Cudf.lookup_package_property pkg "number") {pkg with Cudf.installed = false }
      ) (Cudf.get_installed universe n)
    else
      try
        let pkg = Hashtbl.find t (n,v) in
        Hashtbl.replace t (n,v) {pkg with Cudf.installed = false }
      with Not_found ->
        fatal 
        "Something wrong in the remove request.
        Package in the solution is not present in the universe (%s,%s)" n v;
  ) remove ;

  List.iter (fun (n,v) ->
    try 
      let pkg = Hashtbl.find t (n,v) in
      Hashtbl.replace t (n,v) {pkg with Cudf.installed = true }
    with Not_found -> begin
      if String.starts_with n "dummy_" then ()
      else
        fatal
        "Something wrong in the install request.
        Package in the solution is not present in the universe (%s,%s)" n v;
    end
  ) install ;

  let l = Hashtbl.fold (fun (n,v) pkg acc -> if pkg.Cudf.installed && v <> "" then pkg::acc else acc) t [] in
  if not (Option.is_none preamble) then begin
      Cudf_printer.pp_preamble stdout (Option.get preamble);
      Printf.fprintf stdout "\n"
  end;
  Cudf_printer.pp_packages stdout l
;;
    
main ();;

