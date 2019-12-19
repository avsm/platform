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
open Algo
open DoseparseNoRpm

module Src = Debian.Sources
module Deb = Debian.Packages

module Options = struct
  open OptParse
  let description =
    "Report the broken packages in a debian source list. \
     You must provide a (list of) Debian Packages file(s) and \
     a Debian Sources file in this order"
  let options = OptParser.make ~description
  include StdOptions.MakeOptions(struct let options = options end)

  let dump = StdOpt.str_option ()
  let maforeign = StdOpt.store_true ()
  let includextra = StdOpt.store_true ()
  let triplettable = StdOpt.str_option ()
  let cputable = StdOpt.str_option ()
  let dropalternatives = StdOpt.store_true ()

  include StdOptions.DistcheckOptions ;;
  StdOptions.DistcheckOptions.add_options options ;;


  include StdOptions.InputOptions ;;
  let default = 
    List.fold_left (fun acc e ->
      List.remove acc e
    ) StdOptions.InputOptions.default_options ["inputtype";"fg";"bg";"compare"]
  in
  StdOptions.InputOptions.add_options ~default options ;

  include StdOptions.OutputOptions ;;
  StdOptions.OutputOptions.add_options options ;;
  StdOptions.OutputOptions.add_option options ~long_name:"dump" ~help:"dump the cudf file" dump;

  include StdOptions.DistribOptions ;;
  StdOptions.DistribOptions.add_debian_options options ;;
  let group = StdOptions.DistribOptions.deb_group options in
  StdOptions.DistribOptions.add_option options ~group ~long_name:"deb-triplettable"
    ~help:"Path to an architecture triplet table like /usr/share/dpkg/triplettable" triplettable;
  StdOptions.DistribOptions.add_option options ~group ~long_name:"deb-cputable"
    ~help:"Path to a cpu table like /usr/share/dpkg/cputable" cputable;
  StdOptions.DistribOptions.add_option options ~group ~long_name:"deb-defaulted-m-a-foreign"
    ~help:"Convert Arch:all packages to Multi-Arch: foreign" maforeign;
  StdOptions.DistribOptions.add_option options ~group ~long_name:"deb-include-extra-source"
    ~help:"Include packages with Extra-Source-Only:yes (dropped by default)" includextra;
  StdOptions.DistribOptions.add_option options ~group ~long_name:"deb-emulate-sbuild"
    ~help:"replicate sbuild behaviour to only keep the first alternative of build dependencies" dropalternatives;

end

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let timer = Util.Timer.create "Solver"

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose);
  StdDebug.enable_timers (OptParse.Opt.get Options.timers) ["Solver";"Load.DebianSource"];
  Util.Debug.disable "Depsolver_int";
  StdDebug.all_quiet (OptParse.Opt.get Options.quiet);

  if not(OptParse.Opt.is_set Options.deb_native_arch) then
    fatal "You must at least specify the native architecture";

  let fmt =
    if OptParse.Opt.is_set Options.outfile then
      let oc = open_out (OptParse.Opt.get Options.outfile) in
      Format.formatter_of_out_channel oc
    else
      Format.std_formatter
  in
  (* we set the Debian.Debcudf options wrt the user provided options *)
  let options = Options.set_deb_options () in
  (* buildarch and native arch must be set to some architecture at this point *)
  let buildarch = Option.get options.Debian.Debcudf.native in
  (* hostarch can be None *)
  let hostarch = match options.Debian.Debcudf.host with None -> "" | Some s -> s in
  let noindep = options.Debian.Debcudf.drop_bd_indep in
  let dropalternatives = OptParse.Opt.get Options.dropalternatives in
  let profiles = options.Debian.Debcudf.profiles in

  let filter_external_sources par =
    if (OptParse.Opt.get Options.includextra) then true
    else
      try not(Pef.Packages.parse_bool
        ("extra-source-only",(Pef.Packages.assoc "extra-source-only" par)))
      with Not_found -> true
  in

  if (OptParse.Opt.is_set Options.triplettable)
  || OptParse.Opt.is_set Options.cputable then begin
    let ttfile = if OptParse.Opt.is_set Options.triplettable then
        Some (OptParse.Opt.get Options.triplettable)
      else None in
    let ctfile = if OptParse.Opt.is_set Options.cputable then
        Some (OptParse.Opt.get Options.cputable)
      else None in
    Architecture.read_triplettable ~ttfile ~ctfile ()
  end;

  let pkglist, srclist =
    match posargs with
    |[] | [_] -> fatal 
      "You must provide a list of Debian Packages files and \
       a Debian Sources file"
    |l -> 
        begin match List.rev l with
        |h::t ->
          let srclist =
            StdLoaders.deb_load_source 
              ~dropalternatives 
              ~profiles 
              ~filter:filter_external_sources 
              ~noindep buildarch hostarch h 
          in
          let pkglist = Deb.input_raw t in
          (pkglist,srclist)
        |_ -> fatal "An impossible situation occurred ?!#"
        end
  in
  let tables = Debcudf.init_tables (srclist @ pkglist) in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let from_cudf (p,v) = Debian.Debcudf.get_real_version tables (p,v) in
  let pp = CudfAdd.pp from_cudf in 

  (* XXX here latest could be a bit faster if done at the same time of the cudf
     conversion *)
  let sl = 
    let l = List.map (fun pkg -> Debcudf.tocudf ~options tables pkg) srclist in
    if OptParse.Opt.is_set Options.latest then
      CudfAdd.latest ~n:(OptParse.Opt.get Options.latest) l
    else
      l
  in
  let bl = 
    List.fold_left (fun acc pkg ->
      let pkg = 
        if OptParse.Opt.get Options.maforeign && pkg#architecture = "all" then
          pkg#set_multiarch `Foreign
        else pkg
      in
      (Debcudf.tocudf ~options tables pkg)::acc
    ) sl pkglist 
  in

  let universe = Cudf.load_universe bl in
  let universe_size = Cudf.universe_size universe in

  let failure = OptParse.Opt.get Options.failure in
  let success = OptParse.Opt.get Options.success in
  let explain =
    if success || failure then
      OptParse.Opt.get Options.explain
    else false
  in
  let minimal = OptParse.Opt.get Options.minimal in
  let summary = OptParse.Opt.get Options.summary in

  let checklist =
    if OptParse.Opt.is_set Options.checkonly then begin
      List.flatten (
        List.map (fun ((n,a),c) ->
          let (name,filter) = Pef.Pefcudf.pefvpkg to_cudf (("src:"^n,a),c) in
          Cudf.lookup_packages ~filter universe name
        ) (OptParse.Opt.get Options.checkonly)
      )
    end else sl
  in

  Diagnostic.pp_out_version fmt;

  if OptParse.Opt.is_set Options.deb_native_arch then
    Format.fprintf fmt "native-architecture: %s@."
      (OptParse.Opt.get Options.deb_native_arch);

  if OptParse.Opt.is_set Options.deb_foreign_archs then
    Format.fprintf fmt "foreign-architecture: %s@."
      (String.concat "," (OptParse.Opt.get Options.deb_foreign_archs));

  if OptParse.Opt.is_set Options.deb_host_arch then
    Format.fprintf fmt "host-architecture: %s@."
      (OptParse.Opt.get Options.deb_host_arch);

  let results = Diagnostic.default_result universe_size in

  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback d = 
    if summary then Diagnostic.collect results d ;
    if failure || success then
      Diagnostic.fprintf ~pp ~failure ~success ~explain ~minimal fmt d
  in

  Util.Timer.start timer;
  let subuniverse =
    if checklist <> sl then
      let l =
        if not(OptParse.Opt.get Options.deb_ignore_essential) then
          Cudf.fold_packages (fun acc pkg ->
              match pkg.Cudf.keep with
              |`Keep_package |`Keep_version  -> pkg::acc
              |_ -> acc
          ) checklist universe
        else checklist
      in
      Cudf.load_universe (CudfAdd.cone universe l)
    else universe
  in
  let nbp = Depsolver.listcheck ~callback ~explain subuniverse checklist in
  ignore(Util.Timer.stop timer ());

  if failure || success then Format.fprintf fmt "@]@.";

  let nb = universe_size in
  let nf = List.length sl in
  Format.fprintf fmt "binary-packages: %d@." nb;
  Format.fprintf fmt "source-packages: %d@." (if nf = 0 then nb else nf);
  Format.fprintf fmt "broken-packages: %d@." nbp;

  if summary then
    Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results;

  if OptParse.Opt.is_set Options.dump then begin
    let oc = open_out (OptParse.Opt.get Options.dump) in
    info "Dumping Cudf file";
    
    Cudf_printer.pp_preamble oc Debcudf.preamble;
    Printf.fprintf oc "\n";
    Cudf_printer.pp_universe oc universe
  end;
  nbp
;;

StdUtils.if_application
  ~alternatives:[
    "deb-buildcheck"; "debbuildcheck";"dose-builddebcheck";
    "deb-crossbuildcheck";"debcrossbuildcheck";
    "dose-debcrossbuildcheck"]
  __label main
;;
