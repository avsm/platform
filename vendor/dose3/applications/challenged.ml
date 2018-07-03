(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate, Roberto Di Cosmo                       *)
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
open Algo
open DoseparseNoRpm
module Version = Versioning.Debian

let predbar = Util.Progress.create "challenged" ;;

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module Options = struct
  open OptParse
  let options = OptParser.make ~description:"Compute Challenged packages."
  include StdOptions.MakeOptions(struct let options = options end)

  let checkonly = StdOptions.pkglist_option ()
  let brokenlist = StdOpt.store_true ()
  let downgrades = StdOpt.store_true ()
  let latest = StdOpt.store_true ()
  let cluster = StdOpt.store_true ()
#ifdef HASPARMAP 
  let ncores = StdOpt.int_option ~default:1 ()
  let chunksize = StdOpt.int_option ()
#endif
  open OptParser ;;

#ifdef HASPARMAP 
  add options ~long_name:"ncores"
    ~help:"Number of cores to use on a multicore" ncores;
  add options ~long_name:"chunksize"
    ~help:"Size of each task executed by the workers (controls granularity)" chunksize;
#endif

  add options ~long_name:"checkonly"
    ~help:"Check only these packages ex. sn1 (=sv1),sn2 (=sv2)" checkonly;
  add options ~long_name:"broken" ~short_name:'b' 
    ~help:"Print the list of broken packages" brokenlist;
  add options ~long_name:"downgrade" ~short_name:'d' 
    ~help:"Check package downgrades" downgrades;
  add options ~long_name:"latest" ~short_name:'l' 
    ~help:"Consider on the latest version of each package" latest;

  add options ~short_name:'c' 
    ~help:"Print the list of packages in a cluster" cluster;

end

let pkgset u = 
  Cudf.fold_packages (fun s p -> 
    CudfAdd.Cudf_set.add p s
  ) CudfAdd.Cudf_set.empty u

let exclude pkgset pl =
  let sl = CudfAdd.to_set pl in
  CudfAdd.Cudf_set.elements (CudfAdd.Cudf_set.diff pkgset sl)
;;

let dummy pkg number version =
  {Cudf.default_package with
   Cudf.package = pkg.Cudf.package;
   version = version;
   provides = pkg.Cudf.provides;
   pkg_extra = [("number",`String number)]
  }

let upgrade tables pkgset universe broken migrationlist =
  let getv v = Debian.Debcudf.get_cudf_version tables ("",v) in
  let to_add = 
    List.fold_left (fun l ((pkg,_),target) ->
      let name = CudfAdd.encode pkg#name in
      let orig = getv pkg#version in
      let newv =
        match target with
        |`Eq v -> getv v
        |`Hi v -> (getv v) + 1
        |`Lo v |`In (_,v) -> (getv v) - 1
      in
      let p = Cudf.lookup_package universe (name,orig) in
      let number = Debian.Evolution.string_of_range target in
      if not (Cudf.mem_package universe (p.Cudf.package,newv)) then
        (dummy p number newv)::l
      else l
    ) [] migrationlist
  in
  let to_remove = 
    List.fold_left (fun acc ((pkg,_),_) -> 
      let name = CudfAdd.encode pkg#name in
      let orig = getv pkg#version in
      let p = Cudf.lookup_package universe (name,orig) in
      p::acc
    ) broken migrationlist 
  in
  let universe_subset = exclude pkgset to_remove in
  Cudf.load_universe (to_add@universe_subset)
;;

let add h k v =
  try let l = Hashtbl.find h k in l := v::!l
  with Not_found -> Hashtbl.add h k (ref [v])
;;

let evalsel getv target constr =
  let evalsel v = function
    |(`Eq,w) ->  v = (getv w)
    |(`Geq,w) -> v >= (getv w)
    |(`Leq,w) -> v <= (getv w)
    |(`Gt,w) ->  v > (getv w)
    |(`Lt,w) ->  v < (getv w)
    |(`Neq,w) -> v <> (getv w)
  in
  match target with
  |`Hi v -> evalsel ((getv v) + 1) constr
  |`Lo v -> evalsel ((getv v) - 1) constr
  |`Eq v -> evalsel (getv v) constr
  |`In (v1,v2) -> evalsel ((getv v2) - 1) constr
;;

let strip v = Version.compose (Version.strip_epoch v)

let version_of_target ?(strip=(fun x -> x)) getv = function
  |`Eq v -> getv (strip v)
  |`Hi v -> (getv (strip v)) + 1
  |`Lo v |`In (_,v) -> (getv (strip v)) - 1
;;

let lesser_or_equal getv target equivs v =
  let v_le_target =
    match Version.decompose v with
    |Version.Native(_,_,_) ->
        (* in this case the reference version is without epoch,
         * hence no aligmement of the target. We want to exclude
         * this version if it is less or equal then the reference
         * version OR if the stripped target if less then the 
         * reference version. The idea is to avoid to upgrade to
         * any epoch:version, but only to epoch:version with
         * version > reference version *)
        if (version_of_target getv target) <= (getv v) then true
        else (version_of_target ~strip getv target) < (getv v)
    |_ -> 
        (* in this case the target is going to be aligned and 
         * we want to make sure that the stripped target version is
         * greater or equal then the stripped reference version *)
        (version_of_target ~strip getv target) <= (getv (strip v))
  in v_le_target || (List.mem (`Eq v) equivs) 
  (* ignore targets equivalent to status quo *)
;;

let pp tables pkg =
  let v =
    try Cudf.lookup_package_property pkg "number"
    with Not_found ->
      if (pkg.Cudf.version mod 2) = 1 then
        let (_,_,v) =
          Debian.Debcudf.get_real_version tables
          (pkg.Cudf.package,pkg.Cudf.version)
        in v
      else if pkg.Cudf.version = 0 then
        (* this is a dependency without constraint *)
        ""
      else
        fatal "Package %s without debian version" 
        (CudfAdd.string_of_package pkg)
  in
  let l =
    List.filter_map (fun k ->
      try Some(k,(Cudf.lookup_package_property pkg k,true))
      with Not_found -> None
    ) ["architecture";"source";"sourcenumber";"equivs"]
  in (pkg.Cudf.package,None,v,l)

(* repository are real packages, 
 * packagelist are cudf packages, 
 * cluster are real packages,
 * future are cudf packages *)
let challenged 
  ?(failure=true)
  ?(explain=false)
  ?(minimal=true)
  ?(downgrades=false) 
  ?(broken=false) 
  ?(cluster=false)
  ?(clusterlist=None) 
  repository =
  
  (* distribution specific *)
  let print_cluster = cluster in
  let worktable = ref [] in
  let clusters = Debian.Debutil.cluster repository in
  let version_acc = ref [] in
  let constraints_table = Debian.Evolution.constraints repository in
  let cluster_iter (sn,sv) l =
    List.iter (fun (version,realversion,cluster) ->
      let (versionlist, constr) = 
        Debian.Evolution.all_ver_constr constraints_table cluster 
      in
      version_acc := versionlist @ !version_acc;
      worktable := ((sn,sv,realversion),(cluster,versionlist,constr))::!worktable
    ) l
  in

  if Option.is_none clusterlist then
    Hashtbl.iter cluster_iter clusters
  else
    List.iter (fun (sn,_,sv) ->
      begin try
        let l = Hashtbl.find clusters (sn,sv) in
        cluster_iter (sn,sv) l
      with Not_found -> fatal "Cluster %s %s is not correctly specified" sn sv end
    ) (Option.get clusterlist) 
  ;

  (* cudf part *)
  let versionlist = Util.list_unique !version_acc in
  let tables = Debian.Debcudf.init_tables ~step:2 ~versionlist repository in
  let getv v = Debian.Debcudf.get_cudf_version tables ("",v) in
  let pp = pp tables in
  let pkglist = List.map (Debian.Debcudf.tocudf tables) repository in
  let universe = Cudf.load_universe pkglist in
  let brokenlist = Depsolver.find_broken universe in
  let pkgset = pkgset universe in

  Util.Progress.set_total predbar (List.length !worktable);

  info "Total versions: %d" (List.length versionlist);

  (* computing *)
  let results = 
    let map f l =
#ifdef HASPARMAP 
      let ncores = OptParse.Opt.get Options.ncores in
      match OptParse.Opt.opt Options.chunksize with
        None ->     
        Parmap.parmap ~ncores f (Parmap.L l)
      | Some chunksize ->       
        Parmap.parmap ~ncores ~chunksize f (Parmap.L l)
#else
      List.map f l
#endif
    in
    map (fun ((sn,sv,version),(cluster,vl,constr)) ->
      let starttime = Unix.gettimeofday() in
      let cluster_results = ref [] in
      Util.Progress.progress predbar;
      debug "\nSource: %s %s" sn sv;
      if sv <> version then debug "Subscluter: %s %s" sn version;
      debug "Clustersize: %d" (List.length cluster);
      debug "Versions: %s" (String.concat ";" vl);
      debug "Constraints: %s" (String.concat " ; " (
        List.map (fun (c,v) -> Printf.sprintf "%s" v) constr
        )
      );

      (* compute discriminants of vl
         We use ~bottom:true to include in the discriminants a representative 
         of the versions below the smalles element of vl
       *)
      let discr = Debian.Evolution.discriminant ~bottom:true (evalsel getv) vl constr in
      debug "Discriminants: %d" (List.length discr);
      if print_cluster then begin
        let pp_item fmt pkg = 
          let pp_io_property fmt (n, s) = Format.fprintf fmt "%s: %s@," n s in
          Cudf_printer.pp_package_gen pp_io_property fmt pkg
        in
        let pp_list = Diagnostic.pp_list pp_item in
        let cudf_cluster = 
          List.map (fun pkg -> 
            let name = CudfAdd.encode pkg#name in
            let (pn,pv) = (name, getv pkg#version) in
            Cudf.lookup_package universe (pn,pv) 
          ) cluster
        in
        Format.printf "@[<v 1>clusters:@,%a@]@," pp_list cudf_cluster
      end;
      List.iter (function 
        (* remove this one to show results that are equivalent to do nothing *)
        | (target,equiv) when not(downgrades) && 
            (lesser_or_equal getv target equiv version) ->
              debug "Target: %s" (Debian.Evolution.string_of_range target);
              debug "Equiv: %s" (String.concat " , " (
                List.map (Debian.Evolution.string_of_range) equiv
                ));
              debug "ignored"
        | (target,equiv) ->
            debug "Target: %s" (Debian.Evolution.string_of_range target);
            debug "Equiv: %s" (String.concat " , " (
              List.map (Debian.Evolution.string_of_range) equiv
              ));

            let migrationlist = Debian.Evolution.migrate cluster target in
            let future = upgrade tables pkgset universe brokenlist migrationlist in
            let callback d = 
              let fmt = Format.std_formatter in
              if broken then Diagnostic.fprintf ~pp ~failure ~explain ~minimal fmt d 
            in
            if broken then Format.printf "distcheck: @,";
            let i = Depsolver.univcheck ~callback future in
            if broken then Format.printf "@.";

            debug "Broken: %d" i;
            cluster_results := (((sn,sv,version),(target,equiv)),i)::!cluster_results ;
      ) discr;
      debug "<%s,%s> : %f" sn sv (Unix.gettimeofday() -. starttime); 
      !cluster_results
    ) !worktable
  in List.concat results
;;

let latest pkglist =
  let h = Hashtbl.create (List.length pkglist) in
  List.iter (fun p ->
    try 
      let q = Hashtbl.find h p#name in
      if (Version.compare p#version q#version) > 0 then
        Hashtbl.replace h p#name p
      else () 
    with Not_found -> Hashtbl.add h p#name p
  ) pkglist;
  Hashtbl.fold (fun _ v acc -> v::acc) h []
;;

let main () =
  let args = OptParse.OptParser.parse_argv Options.options in
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose);
  StdDebug.enable_bars (OptParse.Opt.get Options.progress) ["challenged"] ;
  StdDebug.enable_timers (OptParse.Opt.get Options.timers) [];
  Util.Debug.disable "Depsolver_int";
  let clusterlist = OptParse.Opt.opt Options.checkonly in 
  let broken = OptParse.Opt.get Options.brokenlist in
  let cluster = OptParse.Opt.get Options.cluster in
  let downgrades = OptParse.Opt.get Options.downgrades in
  let l = Debian.Packages.input_raw args in
  let l = if OptParse.Opt.get Options.latest then latest l else l in
  let pred = challenged ~downgrades ~broken ~cluster ~clusterlist l in
  List.iter (fun (((sn,sv,version),(target,equiv)),broken) ->
    Format.printf "cluster: %s %s@." sn version;
    if sv <> version then Format.printf "subclusterof: %s %s@." sn sv;
    Format.printf "target: %s@." (Debian.Evolution.string_of_range target);
    Format.printf "equivs: %s@," (String.concat " , " (
      List.map (Debian.Evolution.string_of_range) equiv
    ));
    Format.printf "breaks: %d@." broken;
    Format.printf "---@."
  ) pred;
  0
;;

StdUtils.if_application 
~alternatives:["dose-challenged";"dose3-challenged";"edos-challenged";"deb-challenged"]
__label main ;;
