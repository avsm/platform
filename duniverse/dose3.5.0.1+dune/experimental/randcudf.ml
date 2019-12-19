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
open Algo
open Debian

module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse
  let description = "Generate random cudf instances from Debian Packages files and cudf files"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let documents = StdOpt.int_option ~default:1 ()
  let install = StdOpt.int_option ~default:0 ()
  let upgrade = StdOpt.int_option ~default:0 ()
  let remove = StdOpt.int_option ~default:0 ()
  let keep = StdOpt.int_option ~default:0 ()
  let rstatus = StdOpt.int_option ~default:0 ()
  let status = StdOpt.str_option ()
  let rem_relop = StdOpt.float_option ~default:0.1 ()
  let inst_relop = StdOpt.float_option ~default:0.2 ()
  let upgradeAll = StdOpt.store_true ()
  let clusterUpgrade = StdOpt.store_true ()

  let outdir = StdOpt.str_option ~default:"./" ()
  let seed = StdOpt.int_option ~default:0 ()

  open OptParser
  add options ~short_name:'n' ~help:"generate n different random documents" documents;
  add options ~short_name:'i' ~long_name:"install" ~help:"install n random packages" install;
  add options ~short_name:'r' ~long_name:"remove" ~help:"remove n random package" remove;
  add options ~short_name:'u' ~long_name:"upgrade" ~help:"upgrade n random package" upgrade;
  add options ~short_name:'k' ~long_name:"keep" ~help:"add keep version to n random packages" keep;
  add options                 ~long_name:"status" ~help:"package status (822 debian format)" status;
  add options                 ~long_name:"all-upgrade" ~help:"generate one upgrade all cudf document" upgradeAll;
  add options                 ~long_name:"cluster-upgrade" ~help:"choose upgrade candidates from large source clusters" clusterUpgrade;
  add options                 ~long_name:"rstatus" ~help:"add #n packages to the initial status" rstatus;
  add options                 ~long_name:"instrelop" ~help:"relop probability for install requests" inst_relop;
  add options                 ~long_name:"remrelop" ~help:"relop probability for remove requests" rem_relop;
  add options                 ~long_name:"outdir" ~help:"specify the output directory" outdir;
  add options                 ~long_name:"seed" ~help:"specify the random generator seed" seed;

  include Boilerplate.MakeDistribOptions(struct let options = options end);;

end

(* -------------------------------- *)

include Util.Logging(struct let label = __FILE__ end) ;;

let get_random ?(ver=0.0) pkglist n =
  let a = Array.of_list pkglist in
  let max = (Array.length a) in
  let l = ref [] in
  for i=0 to n - 1 do
    let j = Random.int (max - 1) in
    let pkg = a.(j) in
    if ver = 0.0 then
      l := (pkg.Cudf.package,None)::!l
    else if ver = 1.0 then
      l := (pkg.Cudf.package,Some(`Eq, pkg.Cudf.version))::!l
    else if Random.float(1.0) < ver then begin
      let relop = 
        (* we set 60% the probability for =, and 20% the probability for < > *) 
        let r = Random.float(1.0) in
        if r < 0.6 then `Eq else
        if r >= 0.6 && r < 0.8 then `Lt else
        if r >= 0.8 && r <= 1.0 then `Gt
        else assert false (* not reachable ? *)
      in
      l := (pkg.Cudf.package,Some(relop,pkg.Cudf.version))::!l
    end
    else
      l := (pkg.Cudf.package,None)::!l
  done;
  !l
;;

(* select a package if it is installed and belongs to a cluster
 * that is composed of n or more packages *)
let select_packages_cluster ?(n=4) to_cudf pkglist =
  let th = Debian.Debutil.cluster pkglist in
  List.filter_map (fun pkg ->
    let (name,version) = (pkg.Debian.Packages.name, pkg.Debian.Packages.version) in
    if Debian.Packages.is_installed pkg then
      let (source, sourceversion) = Debian.Debutil.get_source pkg in
      try
        let l = Hashtbl.find th (source, sourceversion) in
        if List.length l = 0 then raise Not_found
        else if List.length l = 1 then
          let (_,_,pl) = List.hd l in
          if List.length pl >= n then 
            Some (name,snd (to_cudf (name,version)))
          else None
        else Some (name,snd(to_cudf (name,version)))
      with Not_found -> begin
        warning "Package %s is not indexed in the source clusters table" pkg.Debian.Packages.name;
        None
      end
    else None
  ) pkglist

let create_pkglist pkglist =
  let (>>) f g = g f in
  let build_hash n =
    let l = get_random ~ver:1.0 pkglist n in
    let h = Hashtbl.create (2 * n) in
    List.iter (fun pkg -> Hashtbl.add h pkg ()) l;
    h
  in
  let keep_hash = build_hash (OptParse.Opt.get Options.keep) in
  let rstatus_hash = build_hash (OptParse.Opt.get Options.rstatus) in
  let app h f pkg =
    if Hashtbl.mem h (pkg.Cudf.package,Some(`Eq, pkg.Cudf.version)) then f pkg else pkg
  in
  let installed = ref [] in
  let l =
    List.map (fun pkg ->
      pkg >>
      app rstatus_hash (fun p -> { p with Cudf.installed = true }) >>
      app keep_hash (fun p -> {p with Cudf.keep = `Keep_version}) >>
      (fun pkg -> if pkg.Cudf.installed then installed := pkg::!installed ; pkg)
    ) pkglist 
  in
  if (List.length !installed) = 0 then
    warning "No installed packages in this universe";
  (!installed, l, Cudf.load_universe l)
;;

let to_install_random removed p l =
  let l' = 
    List.filter(fun p ->
      try ignore(List.find (fun (q,_) -> p.Cudf.package = q) removed) ; false
      with Not_found -> true
    ) l
  in
  get_random ~ver:p l' (OptParse.Opt.get Options.install)
;;

let to_upgrade_random removed l =
  let l' = 
    List.filter(fun p ->
      try ignore(List.find (fun (q,_) -> p.Cudf.package = q) removed) ; false
      with Not_found -> true
    ) l
  in
  get_random l' (OptParse.Opt.get Options.upgrade)
;;

let to_remove_random p l =
  get_random ~ver:p l (OptParse.Opt.get Options.remove)
;; 

let create_cudf (preamble,universe,request)  =
  let oc = 
    if (OptParse.Opt.get Options.documents) > 1 then begin
      let tmpfile = Printf.sprintf "rand%d.cudf" (Random.int 1000) in
      let dirname = OptParse.Opt.get Options.outdir in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
      let file = (Filename.concat dirname (Filename.basename tmpfile)) in
      Printf.printf "%s\n%!" file ;
      open_out file
    end else stdout
  in
  Cudf_printer.pp_cudf oc (preamble,universe,request);
  if oc <> stdout then close_out oc
;;

let deb_load_list options ?(status=[]) dll =
  let pkglist = List.flatten dll in
  let pkglist = if status = [] then pkglist else Debian.Packages.merge status pkglist in
  let tables = Debian.Debcudf.init_tables pkglist in
  let from_cudf (p,i) = (p,Debian.Debcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let clusterUpgrade = 
    if OptParse.Opt.get Options.clusterUpgrade then
      select_packages_cluster to_cudf pkglist 
    else 
      []
  in
  let cll =
    List.map (fun l ->
      (* XXX this is stupid and slow *)
      List.map (Debian.Debcudf.tocudf tables ~options) (Debian.Packages.merge status l)
    ) dll
  in
  let preamble =
    let l = List.map snd options.Debian.Debcudf.extras_opt in
    Common.CudfAdd.add_properties Debian.Debcudf.preamble l
  in
  (preamble,cll,clusterUpgrade,from_cudf,to_cudf)

let edsp_load_list options file =
  let archs =
    if options.Debian.Debcudf.native <> "" then
      options.Debian.Debcudf.native :: options.Debian.Debcudf.foreign
    else []
  in
  let (_,pkglist) = Debian.Edsp.input_raw ~archs file in
  let tables = Debian.Debcudf.init_tables pkglist in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let from_cudf (p,i) = (p, Debian.Debcudf.get_real_version tables (p,i)) in
  let clusterUpgrade = 
    if OptParse.Opt.get Options.clusterUpgrade then
      select_packages_cluster to_cudf pkglist 
    else 
      []
  in
  let preamble =
    let l = List.map snd (Debian.Edsp.extras_tocudf @ options.Debian.Debcudf.extras_opt) in
    Common.CudfAdd.add_properties Debian.Debcudf.preamble l
  in
  let univ = Hashtbl.create (2*(List.length pkglist)-1) in
  let cudfpkglist =
    List.filter_map (fun pkg ->
      let p = Debian.Edsp.tocudf tables ~options pkg in
      if not(Hashtbl.mem univ (p.Cudf.package,p.Cudf.version)) then begin
        Hashtbl.add univ (p.Cudf.package,p.Cudf.version) pkg;
        Some p
      end else begin
        warning "Duplicated package (same version, name and architecture) : (%s,%s,%s)"
          pkg.Debian.Packages.name pkg.Debian.Packages.version pkg.Debian.Packages.architecture;
        None
      end
    ) pkglist
  in
  (preamble,[cudfpkglist],clusterUpgrade,from_cudf,to_cudf)

let deb_parse_input options ?(status=[]) urilist =
  let archs =
    if options.Debian.Debcudf.native <> "" then
      options.Debian.Debcudf.native :: options.Debian.Debcudf.foreign
    else []
  in
  let dll =
    List.map (fun l ->
      let filelist = List.map Boilerplate.unpack l in
      Debian.Packages.input_raw ~archs filelist
    ) urilist
  in
  deb_load_list options ~status dll

let extras_properties = [
  ("Size", ("size", `Nat (Some 0)));
  ("Installed-Size", ("installedsize", `Nat (Some 0)))
];;

let main () =

  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Util.Warning.all_disabled ();
  Random.init (OptParse.Opt.get Options.seed);
  let options = { (Options.set_deb_options ()) with
        Debcudf.extras_opt = extras_properties
      } 
  in

  let (preamble,pkglist,clusterUpgrade) = 
    let (preamble,pkglist,clusterUpgrade,_,_) = 
      let filelist = List.map (List.map Input.parse_uri) [posargs] in
      match Input.guess_format [posargs] with
      |Url.Cudf -> 
          let (preamble,pkglist,t,f) = Boilerplate.parse_input [posargs] in
          (preamble,pkglist,[],t,f)
      |Url.Deb -> 
          (* we assume the status file is alwasy in dpkg format ! *)
          let status =
            if OptParse.Opt.is_set Options.status then 
              Boilerplate.read_deb ~filter:Debian.Packages.status_filter
              (OptParse.Opt.get Options.status)
            else []
          in
          deb_parse_input options ~status filelist
      |Url.Edsp -> 
          let filelist = List.map (List.map Boilerplate.unpack) filelist in
          edsp_load_list options (List.hd (List.hd filelist))
      |_ -> fatal "format not supported"
    in
      (preamble,List.flatten pkglist,clusterUpgrade)
  in

  info "Package %d" (List.length pkglist);
  info "Generating %d random documents with" (OptParse.Opt.get Options.documents);
  info "install : %d" (OptParse.Opt.get Options.install);
  info "remove : %d" (OptParse.Opt.get Options.remove);
  info "upgrade : %d" (OptParse.Opt.get Options.upgrade);
  info "and %d upgrade all document" (if (OptParse.Opt.get Options.upgradeAll) then 1 else 0);
  
  let rp = OptParse.Opt.get Options.rem_relop in
  let ip = OptParse.Opt.get Options.inst_relop in
  let (installed,pkglist,universe) = create_pkglist pkglist in
  let upgrade_candidates =
    if (List.length clusterUpgrade) = 0 && (OptParse.Opt.get Options.clusterUpgrade) && 
      (OptParse.Opt.get Options.upgrade) > 1 then begin
      warning "unable to indentify clusters of packages. Using installed packages instead";
      installed
    end else if (OptParse.Opt.get Options.clusterUpgrade) && 
      (OptParse.Opt.get Options.upgrade) > 1 && (List.length clusterUpgrade) > 0 then begin
      info "Selecting upgrade packages from large clusters";
      List.map (fun (n,v) -> Cudf.lookup_package universe (n,v)) clusterUpgrade
    end else
      installed
  in
  for j = 0 to (OptParse.Opt.get Options.documents) - 1 do
    info "install / remove / upgrade requests";
    let rec one () = 
      (* we install with 20% probability to add a relop to the request
       * and we remove with 10% probability to add a relop to the request *)
      let request =
        let r = to_remove_random rp installed in
        { Cudf.request_id = "RAND-CUDF-GENERATOR" ;
          install = to_install_random r ip pkglist ;
          upgrade = to_upgrade_random r upgrade_candidates ;
          remove =  r ;
          req_extra = [] ; }
      in
      if Diagnostic.is_solution (Depsolver.check_request (None,pkglist,request)) then begin 
        info "#%d (installed %d) %!" j (List.length installed);
        create_cudf (preamble,universe,request)
      end else (Printf.printf ".%!" ; one () )
    in one ()
  done
  ;
  if (OptParse.Opt.get Options.upgradeAll) then begin
    info "upgrade all request";
    let request =
      { Cudf.request_id = "RAND-CUDF-GENERATOR" ;
        install = [] ;
        upgrade = List.map (fun pkg -> (pkg.Cudf.package,None)) installed ;
        remove =  [] ;
        req_extra = [] ; }
    in
    create_cudf (preamble,universe,request)
  end
;;

main ();;
