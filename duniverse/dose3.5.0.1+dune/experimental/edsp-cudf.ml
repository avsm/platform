(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
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
open Debian
module Boilerplate = BoilerplateNoRpm

let info fmt = Util.make_info "edsp-cudf" fmt
let warning fmt = Util.make_warning "edsp-cudf" fmt
let debug fmt = Util.make_debug "edsp-cudf" fmt
let fatal fmt = Util.make_fatal "edsp-cudf" fmt

module Options = struct
  open OptParse
  let description = "edsp-cudf (EDSP v. 0.4)"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  open OptParser

end

let make_request universe request = 
  let select_packages l = List.map (fun (n,_,c) -> (n,None)) l in
  if request.Edsp.upgrade || request.Edsp.distupgrade then
    (* XXX we must take into account mult-arch requests. ignore atm *)
    let to_upgrade = function
      |[] ->
        let filter pkg = pkg.Cudf.installed in
        let l = Cudf.get_packages ~filter universe in
        List.map (fun pkg -> (pkg.Cudf.package,None)) l
      |l -> List.map (fun (n,_,c) -> (n,None)) l
    in
    {Cudf.default_request with 
    Cudf.request_id = request.Edsp.request;
    Cudf.upgrade = to_upgrade request.Edsp.install;
    Cudf.remove = select_packages request.Edsp.remove;
    }
  else
    {Cudf.default_request with
    Cudf.request_id = request.Edsp.request;
    Cudf.install = select_packages request.Edsp.install;
    Cudf.remove = select_packages request.Edsp.remove;
    }
;;

let main () =
  let timer1 = Util.Timer.create "parsing" in
  let timer2 = Util.Timer.create "conversion" in
  let timer3 = Util.Timer.create "cudfio" in
  let args = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers)
  ["parsing";"cudfio";"conversion"];

  let ch = 
    match args with 
    |[] -> (IO.input_channel stdin)
    |file::_ -> Input.open_file file 
  in
  
  Util.Timer.start timer1;
  let (request,pkglist) = Edsp.input_raw_ch ch in
  Util.Timer.stop timer1 ();
  
  if args <> [] then Input.close_ch ch;

  Util.Timer.start timer2;
  let tables = Debcudf.init_tables pkglist in
  let default_preamble =
    let l = List.map snd Edsp.extras_tocudf in
    CudfAdd.add_properties Debcudf.preamble l
  in
  
  let univ = Hashtbl.create (2*(List.length pkglist)-1) in
  let cudfpkglist = 
    List.map (fun pkg ->
      let p = Edsp.tocudf tables pkg in
      Hashtbl.add univ (p.Cudf.package,p.Cudf.version) pkg;
      p
    ) pkglist 
  in
  let universe = 
    try Cudf.load_universe cudfpkglist
    with Cudf.Constraint_violation s ->
      fatal "(CUDF) Malformed universe %s" s;
  in
  let cudf_request = make_request universe request in
  let cudf = (default_preamble,universe,cudf_request) in
  Util.Timer.stop timer2 ();

  Util.Timer.start timer3;
  Cudf_printer.pp_cudf stdout cudf;
  Util.Timer.stop timer3 ()
;;

main ();;

