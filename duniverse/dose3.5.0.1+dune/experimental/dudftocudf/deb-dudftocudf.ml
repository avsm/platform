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

include Util.Logging(struct let label = __FILE__ end) ;;

module Deb = Debian.Packages

module L = Xml.LazyList 

module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse
  let description = "Convert Debian Dudf files to Cudf format"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let outdir = StdOpt.str_option ()
  let problemid = StdOpt.str_option ()
  let archdefault = StdOpt.str_option ()

  open OptParser ;;
  add options ~short_name:'o' ~long_name:"outdir" ~help:"Output directory" outdir;
  add options ~short_name:'a' ~long_name:"arch" ~help:"Default architecture" archdefault;
  add options                 ~long_name:"id" ~help:"Problem id" problemid;
end

(* ========================================= *)

module AptPref = struct

  type criteria = {
    origin : string option;
    component : string option;
    release_version : string option;
    label : string option;
    archive : string option
  }

  let print_criteria = function
    { origin = o ;
      component = c ;
      release_version = v ;
      label = l ;
      archive = a
    } -> 
      let f = Option.may (Printf.eprintf "%s\n") in
      f o ; f c ; f v ; f l ; f a
  ;;

  type priority = int
  type generic = criteria
  type specific = {
    name : string ;
    version : string option ;
    criteria : criteria ;
  }

  type preferences = {
    target_release : string option ;
    specific : (specific * priority) list;
    generic  : (generic * priority) list
  }

  let map_criteria criteria = function
    |"v",v -> {criteria with release_version = Some v }
    |"c",v -> {criteria with component = Some v}
    |"o",v -> {criteria with origin = Some v}
    |"l",v -> {criteria with label = Some v}
    |"a",v -> {criteria with archive = Some v}
    |s,v -> fatal "Unknon Criteria %s %s" s v

  let dummypref = { target_release = None ; specific = [] ; generic = [] }
  let dummycriteria = {
    origin = None ; component = None ; 
    release_version = None ; 
    label = None ; archive = None
  }
  let dummyspec = { name = "undef" ; version = None ; criteria = dummycriteria }

  let mapf preferences = function
    { Debian.Apt.Pref.package = pkg ; pin = pin ; pin_priority = priority } ->
      match pkg with
      |Debian.Apt.Pref.Star ->
          begin match pin with
          |Debian.Apt.Pref.Version s -> fatal "Uhmmm pin with Verions %s" s
          |Debian.Apt.Pref.Origin origin -> begin
              warning "origin is not currectly supported" ;
              let c = { dummycriteria with origin = Some origin } in
              {preferences with generic = (c, priority) :: preferences.generic}
          end
          |Debian.Apt.Pref.Release criteria -> 
              let c = List.fold_left map_criteria dummycriteria criteria in
              {preferences with generic = (c, priority) :: preferences.generic}
          end
      |Debian.Apt.Pref.Package name ->
          begin match pin with
          |Debian.Apt.Pref.Version version -> 
              let s = { dummyspec with name = name ; version = Some version } in
              {preferences with specific = (s, priority) :: preferences.specific }
          |Debian.Apt.Pref.Origin origin -> begin
              warning "Warning : origin is not currectly supported" ;
              let c = { dummycriteria with origin = Some origin } in
              let s = { dummyspec with name = name ; criteria = c} in
              {preferences with specific = (s, priority) :: preferences.specific}
          end
          |Debian.Apt.Pref.Release criteria ->
              let c = List.fold_left map_criteria dummycriteria criteria in
              let s = { dummyspec with name = name ; criteria = c } in
              {preferences with specific = (s, priority) :: preferences.specific}
          end
  ;;

  let parse ?tr s =
    let ch = IO.input_string s in
    let l = Debian.Apt.parse_preferences_in ch in
    let pref = List.fold_left mapf dummypref l in
    { pref with target_release = tr }

  let match_criteria constr c =
       (c.origin = None || c.origin = constr.origin) 
    && (c.release_version = None || c.release_version = constr.release_version) 
    && (c.component = None || c.component = constr.component)
    && (c.archive = None || c.archive = constr.archive)
    && (c.label = None || c.label = constr.label)
  ;;

  let match_version constr c = 
    if (Option.is_none constr) || (Option.is_none c) then false
    else
      let s = Str.global_replace (Str.regexp "\\*") "\\.*" (Option.get constr) in
      let version_re = Str.regexp s in
      Str.string_match version_re (Option.get c) 0
  ;;

  let match_specific constr c =
       (c.name = constr.name)
    && ((c.version = None) 
          || (c.version = constr.version)
          || (match_version c.version constr.version) )
    && (match_criteria constr.criteria c.criteria)
  ;;

  let find_specific constr l = 
    List.find_all (fun (c,_) -> match_specific constr c) l

  let find_generic constr l = 
    List.find_all (fun (c,_) -> match_criteria constr c) l

  let find_max l = List.fold_left (fun a (_,b) -> max a b) 0 l

  let get_priority pref info pkg =
    let number = Cudf.lookup_package_property pkg "number" in
    let constr = { dummyspec with name = pkg.Cudf.package ; version = Some number } in
    match (find_specific constr pref.specific, info) with
    |[], None -> None
    |[], Some info ->
        begin
          let constr = {dummycriteria with archive = Some(info.Debian.Release.suite)} in
          match find_generic constr pref.generic with
          |[] -> None 
          |l -> Some(find_max l)
        end
    |l,_ -> Some(find_max l)

  let assign_priority preferences info package =
    match get_priority preferences info package with
    |None ->
        (* XXXXXXXXX *)
      begin match preferences.target_release,info with
      |(_, None) ->
          if package.Cudf.installed then 100 else 500
      |(None, Some info) ->
          let na = info.Debian.Release.notauto in
          let up = info.Debian.Release.autoup in
          let ins = package.Cudf.installed in
          begin match na,up,ins with
          |true,false,_ -> 1 
          |true,true,_ -> 100 
          |false,_,true -> 100 
          |false,_,false -> 500
          end
      |Some tr, Some info ->
          let na = info.Debian.Release.notauto in
          let up = info.Debian.Release.autoup in
          let ins = package.Cudf.installed in
          begin match na,up,ins with
          |true,false,_ -> 1 
          |true,true,_ -> 100 
          |false,_,true -> 100 
          |false,_,false when not (tr = info.Debian.Release.suite ) -> 500
          |false,_,false -> 990
          end
      end
    |Some p -> p

  let max_priority = function
    |[] -> fatal "Empty max_priority"
    |l ->
        let (i,p) =
          List.fold_left (fun (a,p) pkg ->
            let b = int_of_string (Cudf.lookup_package_property pkg "pin-priority") in
            if a >= b then (a,p) else (b,pkg)
          ) (min_int,Cudf.default_package) l
        in if i = min_int then fatal "Cannot find a max_priority" else p

end

(* ========================================= *)

let guess_default_arch rl =
  let guess = Hashtbl.create 5 in
  List.iter (function 
    |("apt",fname,_,_) -> begin
      let re = Str.regexp ".*_binary-\\(.*\\)_Packages.*$" in
      try 
        let _ = Str.search_backward re fname (String.length fname) in
        let a = Str.matched_group 1 fname in
        try 
          let i = Hashtbl.find guess a in
          i := !i + 1
        with Not_found ->
          Hashtbl.add guess a (ref 1)
      with Not_found -> info "No Arch guess from %s" fname
    end
    |(_,_,_,_) -> ()
  ) rl;
  match Hashtbl.length guess with
  |0 -> warning "No default architecture found" ; None
  |1 -> Some(fst(List.hd(Hashtbl.fold (fun k v l -> (k,!v)::l) guess [])))
  |_ ->
      let cmp x y = (snd x) - (snd y) in
      let l = Hashtbl.fold (fun k v l -> (k,!v)::l) guess [] in
      Some(fst(List.hd (List.sort ~cmp l)))
;;

let make_universe pl =
  let fl = ref [] in
  let (packagelist,releaselist) =
    List.partition (function 
      |("apt",_,_,_) -> true
      |("apt-release",_,_,_) -> false
      |(s,_,_,_) -> fatal "Unknown file type %s" s
    ) pl
  in
  let universe = 
    List.flatten (
      List.map (fun (_,fname,_,cdata) ->
        let i = Str.search_backward (Str.regexp "_") fname (String.length fname) in
        let s = Str.string_before fname i in
        let release = 
          let ch = IO.input_string cdata in
          let r = Debian.Release.parse_release_in fname ch in
          let _ = IO.close_in ch in
          match r with Some r -> r | None -> assert false
        in
        let cl =
          List.find_all (fun (_,fname,_,_) ->
            Str.string_match (Str.regexp ("^"^s^".*_Packages$")) fname 0
          ) packagelist
        in
        List.map (fun (_,fname,_,cdata) ->
          fl := fname :: !fl ;
          (release,cdata)
        ) cl
      ) releaselist
    )
  in
  let without_release = 
    let l = 
      List.find_all (fun (_,fname,_,_) ->
        not(List.mem fname !fl)
      ) packagelist
    in 
    List.map (fun (_,fname,_,cdata) ->
      warning "Package List without Release. %s" fname;
      (Debian.Release.default_release,cdata)
    ) l
  in
  universe @ without_release
;;

let has_children nodelist tag =
  try match nodelist with
    |t::_ when (Xml.tag t) = tag -> true
    |_ -> false
  with Xml.Not_element(_) -> false
;;

let parsepackagelist = function
  |(Some "apt",Some fname,url,_,[inc]) when has_children [inc] "include" ->
      let href = Xml.attrib inc "href" in
      ("apt",fname,url,Dudfxml.pkgget ~compression:Dudfxml.Bz2 href)
  |(Some "apt-release",Some fname,url,_,[inc]) when has_children [inc] "include" ->
      let href = Xml.attrib inc "href" in
      ("apt-release",fname,url,Dudfxml.pkgget href)
  |(Some t,Some fname,url,_,[cdata]) -> (t,fname,url,Xml.cdata cdata)
  |(Some t,Some fname,url,_,[]) -> (t,fname,url,"")
  |(Some t,Some fname,url,_,_) ->
      fatal "Unknown format for package-list element %s %s" t fname
  |_ -> fatal "Completly unknown format for package-list element"
;;

(* ========================================= *)

open Dudfxml.XmlDudf

let main () =
  let progressbar = Util.Progress.create "debdudf" in
  Random.self_init () ;
  let input_file =
    match OptParse.OptParser.parse_argv Options.options with
    |[h] -> h
    |_ -> (Printf.eprintf "too many arguments" ; exit 1)
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose) ;
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) ["debdudf"];
  Boilerplate.all_quiet (OptParse.Opt.get Options.quiet);

  info "parse xml";

  let dudfdoc = Dudfxml.parse input_file in

  if not(Pcre.pmatch ~rex:(Pcre.regexp "[Dd]ebian") dudfdoc.distribution) then begin
    Printf.eprintf
    "Input dudf document not in debian's dudf format (but %s)\n" dudfdoc.distribution;
    exit 1
  end;

  info "convert to dom ... ";

  let uid = dudfdoc.uid in
  let status =
    match dudfdoc.problem.packageStatus.st_installer with
    |[status] -> Xml.fold (fun a x -> a^(Xml.cdata x)) "" status
    |_ -> Printf.eprintf "Warning: wrong status" ; ""
  in
  let _package_list = 
    List.map (fun pl -> parsepackagelist pl) 
    dudfdoc.problem.packageUniverse
  in

  let packagelist = make_universe _package_list in
  let action = dudfdoc.problem.action in
  let preferences = AptPref.parse dudfdoc.problem.desiderata in

  let infoH = Hashtbl.create 1031 in
  let extras_property = [
    ("Size", ("size", `Nat (Some 0)));
    ("Installed-Size", ("installedsize", `Nat (Some 0)));
    ("Maintainer", ("maintainer", `String (Some "")))]
  in
  let extras = List.map (fun(f,_) -> (f,None)) extras_property in

  info "parse all packages";
  Util.Progress.set_total progressbar (List.length packagelist);
  let archs = 
    if OptParse.Opt.is_set Options.archdefault then
      [OptParse.Opt.get Options.archdefault]
    else
      match guess_default_arch _package_list with
      |None -> (warning "No default Arch. Unable to guess"; [])
      |Some s -> info "Guessed Default Arch %s" s ; [s]
  in
  let all_packages =
    List.fold_left (fun acc (release,contents) ->
      Util.Progress.progress progressbar ;
      let ch = IO.input_string contents in
      let l = Deb.input_raw_ch ~archs ~extras ch in
      let _ = IO.close_in ch in
      List.fold_left (fun s pkg -> 
        Hashtbl.add infoH (pkg.Deb.name,pkg.Deb.version) release ;
        Deb.Set.add pkg s
      ) acc l
    ) Deb.Set.empty packagelist
  in
  Util.Progress.reset progressbar;

  info "installed packages";
  let installed_packages =
    let ch = IO.input_string status in
    let l = Deb.input_raw_ch ~filter:Deb.status_filter ch in
    let _ = IO.close_in ch in
    l
  in

  info "union";
  let l = Deb.merge installed_packages (Deb.Set.elements all_packages) in
  let tables = Debian.Debcudf.init_tables l in
  let priorities = ref [] in
  let add_pin_priority v pkg =
    if not(List.mem v !priorities) then priorities := v :: !priorities ;
    let k = "pin-priority-"^(string_of_int v) in
    { pkg with Cudf.pkg_extra = (k,`Int 1) :: pkg.Cudf.pkg_extra } 
  in

  let add_suite info pkg =
    match info with
    |None -> pkg
    |Some info ->
        let s = 
          if Filename.check_suffix info.Debian.Release.fname "_InRelease" then
            Filename.chop_suffix info.Debian.Release.fname "_InRelease"
          else if Filename.check_suffix info.Debian.Release.fname "_Release" then
            Filename.chop_suffix info.Debian.Release.fname "_Release"
          else info.Debian.Release.suite
        in
        let distribsuite = ("distribsuite",`String s) in
        let suite = ("suite",`String info.Debian.Release.suite) in
        { pkg with Cudf.pkg_extra = suite :: distribsuite :: pkg.Cudf.pkg_extra }
  in

  info "convert";
  Util.Progress.set_total progressbar (List.length l); 
  let options = 
    { Debian.Debcudf.default_options with 
      Debian.Debcudf.extras_opt = extras_property }
  in
  let pl =
    List.map (fun pkg ->
      Util.Progress.progress progressbar ;
      let info =
        try 
          Some(Hashtbl.find infoH (pkg.Deb.name,pkg.Deb.version)) 
        with Not_found -> None
      in
      let cudfpkg = Debian.Debcudf.tocudf ~options tables pkg in
      let priority = AptPref.assign_priority preferences info cudfpkg in
      let cudfpkg = add_pin_priority priority cudfpkg in
      let cudfpkg = add_suite info cudfpkg in
      cudfpkg
    ) l
  in
  Util.Progress.reset progressbar;

  let universe = Cudf.load_universe pl in

  info "request";
  let request =
    let mapver = function
      |`Pkg p -> (p,None)
      |`PkgVer (p,v) -> begin
          try (p,Some(`Eq,Debian.Debcudf.get_cudf_version tables (p,v)))
          with Not_found ->
            fatal "There is no version %s of package %s" p v
      end
      |`PkgDst (p,d) ->
          let l = Cudf.lookup_packages universe p in
          let pkg = 
            try
              List.find (fun pkg ->
                let number = Cudf.lookup_package_property pkg "number" in
                let info = Hashtbl.find infoH (pkg.Cudf.package,number) in
                info.Debian.Release.suite = d
              ) l
            with Not_found -> begin
              warning "There is no package %s in release %s" p d;
              AptPref.max_priority l
            end
          in
          let number = Cudf.lookup_package_property pkg "number" in
          (pkg.Cudf.package,
            Some(`Eq,
              Debian.Debcudf.get_cudf_version tables (pkg.Cudf.package,number)
            )
          )
    in
    let request_id =
      if OptParse.Opt.is_set Options.problemid then OptParse.Opt.get Options.problemid
      else if uid <> "" then uid
      else (string_of_int (Random.bits ()))
    in
    let parsed_action =
      match dudfdoc.metaInstaller.name with
      |"apt-get" -> Debian.Apt.parse_request_apt action
      |"aptitude" -> Debian.Apt.parse_request_aptitude action
      |s -> fatal "Unsupported meta installer %s" s
    in
    match parsed_action with
    |Debian.Apt.Upgrade (Some (suite))
    |Debian.Apt.DistUpgrade (Some (suite)) -> 
        let il = List.fold_left (fun acc pkg -> `PkgDst (pkg.Deb.name,suite) :: acc) [] installed_packages in
        let l = List.map mapver il in
        { Cudf.request_id = request_id ; install = l ; remove = [] ; upgrade = [] ; req_extra = [] ; }
    |Debian.Apt.Upgrade None 
    |Debian.Apt.DistUpgrade None -> 
        let il = List.fold_left (fun acc pkg -> (`Pkg pkg.Deb.name) :: acc) [] installed_packages in
        let l = List.map mapver il in
        { Cudf.request_id = request_id ; install = [] ; remove = [] ; upgrade = l ; req_extra = [] ; }
(*    |Debian.Apt.Install l ->
        let l = List.map mapver l in
        { Cudf.request_id = request_id ; install = l ; remove = [] ; upgrade = [] ; req_extra = [] ; } 
    |Debian.Apt.Remove l -> 
        let l = List.map (fun (`Pkg p) -> (p,None) ) l in
        { Cudf.request_id = request_id ; install = [] ; remove = l ; upgrade = [] ; req_extra = [] ;}
*)
  in

  let oc =
    if OptParse.Opt.is_set Options.outdir then begin
      let dirname = OptParse.Opt.get Options.outdir in
      let file =
        let s = Filename.basename input_file in
        try Filename.chop_extension s with Invalid_argument _ -> s
      in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
      open_out (Filename.concat dirname (file^".cudf"))
    end else stdout
  in
  let preamble =
    let l = List.map snd extras_property in
    let pl =
      List.fold_left (fun l' v -> 
        ("pin-priority-"^(string_of_int v),(`Int (Some 0)))::l'
      ) l !priorities
    in
    let suite = ("suite",`String (Some "local")) in
    let distribsuite = ("distribsuite",`String (Some "unknown")) in
    CudfAdd.add_properties Debian.Debcudf.preamble (suite :: distribsuite :: pl)
  in
  Cudf_printer.pp_cudf oc (preamble, universe, request)
;;

main ();;

