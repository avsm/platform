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

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let load_list_timer = Util.Timer.create "Load" ;;
let deb_load_list_timer = Util.Timer.create "Load.Debian" ;;
let deb_load_source_timer = Util.Timer.create "Load.DebianSource" ;;

(* a list of the raw package types for all input except Cudf itself *)
(* currently, only Deb and DebSrc are used *)
type rawpackage =
  |Deb of Debian.Packages.package
  |DebSrc of Debian.Sources.source
  |Pef of Pef.Packages.package
  |Opam of Opam.Packages.package
  |Npm of Npm.Packages.package
  |Edsp of Debian.Packages.package
  |Csw of Csw.Packages.package
#ifdef HASRPM
  |Rpm of Rpm.Packages.package
#endif


(** read a debian Packages file - compressed or not *)
let read_deb ?filter ?(extras=[]) fname =
  Debian.Packages.input_raw ?filter ~extras [fname]

(* fll = file list list
 * dll = deb packages list list 
 * cll = cudf package list list
 *)
let deb_load_list options ?(status=[]) ?(raw=false) dll =
  Util.Timer.start deb_load_list_timer;
  let noindep = options.Debian.Debcudf.drop_bd_indep in
  let profiles = options.Debian.Debcudf.profiles in
  let pkgll = List.map (List.map (function
      | Deb p -> p
      | DebSrc p ->
          if Option.is_none options.Debian.Debcudf.native then
            fatal "--deb-native-arch was not specified while treating Debian Sources File";
        let buildarch = Option.get options.Debian.Debcudf.native in
        let hostarch = Option.get options.Debian.Debcudf.host in
        Debian.Sources.src2pkg ~noindep ~profiles buildarch hostarch p
      | _ -> fatal "cannot handle input"
    )) dll 
  in
  let pkgl = List.flatten pkgll in
  let pkgl = if status = [] then pkgl else Debian.Packages.merge status pkgl in
  let tables = Debian.Debcudf.init_tables ~options pkgl in
  let from_cudf (p,i) = Debian.Debcudf.get_real_version tables (p,i) in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let cll = 
    List.map (fun l ->
      List.map (Debian.Debcudf.tocudf tables ~options) (Debian.Packages.merge status l)
    ) pkgll
  in
  (* if requested, connect all cudf packages representing binary packages to
   * the cudf packages representing the source package they each build from,
   * respectively *)
  let cll = if options.Debian.Debcudf.builds_from then begin
      let univ =
        Cudf.load_universe (CudfAdd.Cudf_set.elements (
            List.fold_right (
              List.fold_right CudfAdd.Cudf_set.add)
              cll CudfAdd.Cudf_set.empty))
      in
      List.map2 (List.map2 (fun cudfpkg debpkg ->
          match debpkg with
          | Deb _ ->
            let srcpkg = try Debian.Sources.get_src_package univ cudfpkg
              with Debian.Sources.NotfoundSrc ->
                failwith (Printf.sprintf "cannot find source for binary package %s"
                            (CudfAdd.string_of_package cudfpkg))
            in
            (* connect to source package as "builds-from" *)
            let srcdep = (srcpkg.Cudf.package,Some(`Eq,srcpkg.Cudf.version)) in
            { cudfpkg with Cudf.depends = [srcdep] :: cudfpkg.Cudf.depends }
          | DebSrc _ -> cudfpkg
          | _ -> failwith "impossible"
        )) cll dll
    end else cll
  in
  let preamble = Debian.Debcudf.preamble in
  let request = Cudf.default_request in
  let rawll = if raw && status = [] then Some dll else None in
  let global_constraints = Debian.Debcudf.get_essential ~options tables in
  let l = (preamble,cll,request,from_cudf,to_cudf,rawll,global_constraints) in
  Util.Timer.stop deb_load_list_timer l
      
let npm_load_list file =
  let (request,pkglist) = Npm.Packages.input_raw file in
  let tables = Pef.Pefcudf.init_tables Versioning.SemverNode.compare pkglist in
  let from_cudf (p,i) = Pef.Pefcudf.get_real_version tables (p,i) in
  let to_cudf (p,v) = (p, Pef.Pefcudf.get_cudf_version tables (p,v)) in
  let cl = List.map (Pef.Pefcudf.tocudf tables) pkglist in
  let preamble = Npm.Npmcudf.preamble in
  let request = Cudf.default_request in
  (*
  let request = Npm.Npmcudf.requesttocudf tables (Cudf.load_universe cl) request in
  *)
  (preamble,[cl;[]],request,from_cudf,to_cudf,None,[])

let opam_load_list ?options file =
  let (request,pkglist) = Opam.Packages.input_raw file in
  let tables = Pef.Pefcudf.init_tables Versioning.Debian.compare pkglist in
  let from_cudf (p,i) = Pef.Pefcudf.get_real_version tables (p,i) in
  let to_cudf (p,v) = (p, Pef.Pefcudf.get_cudf_version tables (p,v)) in
  let options =
    match options with
    |None -> {
      Opam.Opamcudf.default_options with
      Opam.Opamcudf.switch = request.Opam.Packages.switch;
      switches = request.Opam.Packages.switches;
      profiles = request.Opam.Packages.profiles }
    |Some opt -> opt
  in
  let cl = List.flatten (List.map (Opam.Opamcudf.tocudf ~options tables) pkglist) in
  let preamble = Opam.Opamcudf.preamble in
  let request = Opam.Opamcudf.requesttocudf tables (Cudf.load_universe cl) request in
  (preamble,[cl;[]],request,from_cudf,to_cudf,None,[])

let pef_load_list ?compare options dll =
  let compare = match compare with Some c -> c |None -> Versioning.Debian.compare in
  let extras = [("maintainer",("maintainer",`String None))] in
  let pkglist = List.flatten dll in
  let tables = Pef.Pefcudf.init_tables compare pkglist in
  let from_cudf (p,i) = Pef.Pefcudf.get_real_version tables (p,i) in
  let to_cudf (p,v) = (p,Pef.Pefcudf.get_cudf_version tables (p,v)) in
  let cll =
    List.map (fun l ->
      List.map (Pef.Pefcudf.tocudf ~extras tables) l
    ) dll
  in
  let preamble = Pef.Pefcudf.preamble in
  let request = Cudf.default_request in
  (preamble,cll,request,from_cudf,to_cudf,None,[])

let csw_load_list dll =
  let pkglist = List.flatten dll in
  let tables = Csw.Cswcudf.init_tables pkglist in
  let from_cudf (p,i) = (p,None,Csw.Cswcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p, Csw.Cswcudf.get_cudf_version tables (p,v)) in
  let cll = 
    List.map (fun l ->
      List.map (Csw.Cswcudf.tocudf tables) l
    ) dll
  in
  let preamble = Csw.Cswcudf.preamble in
  let request = Cudf.default_request in
  (preamble,cll,request,from_cudf,to_cudf,None,[])
 
let edsp_load_list options file =
  let (request,pkglist) = Debian.Edsp.input_raw file in
  let (native_arch,foreign_archs) =
    StdUtils.get_architectures
      request.Debian.Edsp.architecture
      request.Debian.Edsp.architectures
      options.Debian.Debcudf.native
      (match options.Debian.Debcudf.foreign with [] -> None | l -> Some l)
  in
  let options = { 
    options with 
    Debian.Debcudf.native = native_arch;
    Debian.Debcudf.foreign = foreign_archs
  } in
  let tables = Debian.Debcudf.init_tables ~options pkglist in
  let preamble =
    let l = List.map snd Debian.Edsp.extras_tocudf in
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
          pkg#name pkg#version pkg#architecture;
        None
      end
    ) pkglist
  in
  let request = Debian.Edsp.requesttocudf tables (Cudf.load_universe cudfpkglist) request in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let from_cudf (p,i) = Debian.Debcudf.get_real_version tables (p,i) in
  let global_constraints = Debian.Debcudf.get_essential ~options tables in
  (preamble,[cudfpkglist;[]],request,from_cudf,to_cudf,None,global_constraints)

let edsp_load_universe options file =
  let (pr,l,r,f,t,w,e) = edsp_load_list options file in
  (pr,Cudf.load_universe (List.hd l), r, f, t, w,e)

(** transform a list of debian control stanza into a cudf universe *)
let deb_load_universe options ?(raw=false) l =
  let (pr,cll,r,f,t,w,e) = deb_load_list options ~raw l in
  (pr,Cudf.load_universe (List.flatten cll), r, f, t, w,e)

(** transform a list of rpm control stanza into a cudf packages list *)
let rpm_load_list dll =
#ifdef HASRPM
  let tables =  Rpm.Rpmcudf.init_tables (List.flatten dll) in
  let cll = 
    List.map (fun l -> 
      List.map (Rpm.Rpmcudf.tocudf tables) l 
    ) dll
  in
  (* Rpm.Rpmcudf.clear tables; *)
  let from_cudf (p,i) = (p,None,string_of_int i) in
  let to_cudf (p,v) = (p,Rpm.Rpmcudf.get_cudf_version tables (p,v)) in
  let preamble = Rpm.Rpmcudf.preamble in
  let request = Cudf.default_request in
  (preamble,cll,request,from_cudf,to_cudf,None,[])
#else
  failwith "librpm not available. re-configure with --with-rpm"
#endif

(** transform a list of rpm control stanza into a cudf universe *)
let rpm_load_universe l =
  let (pr,cll,r,f,t,w,_) = rpm_load_list [l] in
  (pr,Cudf.load_universe (List.flatten cll), r, f, t, w)

(** parse a cudf file and return a triple (preamble,package list,request
    option). If the package is not valid returns an empty list of packages *)
let parse_cudf doc =
  try
    let p = Cudf_parser.from_IO_in_channel (Input.open_file doc) in
    Cudf_parser.parse p
  with
  |Input.File_empty -> None, [], None
  |Cudf_parser.Parse_error (msg, loc) ->
    fatal "Error while parsing CUDF from %s (%s): %s" doc (Format822.string_of_loc loc) msg ;
  |Cudf.Constraint_violation _ as exn ->
    fatal "Error while loading CUDF from %s: %s" doc (Printexc.to_string exn)

(** parse a cudf file and return a triple (preamble,universe,request option).
    If the package is not valid return an empty list of packages *)
let load_cudf doc = 
  let ch = Input.open_file doc in
  let l = 
    try
      let p = Cudf_parser.from_IO_in_channel ch in
      Cudf_parser.load p
    with
    |Input.File_empty -> None, Cudf.load_universe [], None
    |Cudf_parser.Parse_error (msg, loc) ->
      fatal "Error while parsing CUDF from %s (%s): %s" doc (Format822.string_of_loc loc) msg ;
    |Cudf.Constraint_violation _ as exn -> begin
      fatal "Error while loading CUDF file %s:\n%s" doc (Printexc.to_string exn)
    end 
  in
  Input.close_ch ch;
  l
;;

let cudf_load_list file =
  let preamble, pkglist ,request =
    match parse_cudf file with
    |None, pkglist, None -> Cudf.default_preamble, pkglist, Cudf.default_request
    |None , pkglist, Some req -> Cudf.default_preamble, pkglist, req
    |Some p , pkglist, None -> p, pkglist, Cudf.default_request
    |Some p , pkglist, Some req -> p, pkglist, req
  in
  let from_cudf (p,i) = (p,None,string_of_int i) in
  let to_cudf (p,v) = (p,int_of_string v) in
  (preamble,[pkglist;[]],request,from_cudf,to_cudf,None,[])

let cudf_load_universe file =
  let (pr,l,r,f,t,w,_) = cudf_load_list file in
  (pr,Cudf.load_universe (List.hd l), r, f, t, w,[])

let unpack_l expected l = List.fold_left (fun acc (t,(_,_,_,_,f),_) ->
    if t = expected then f::acc
    else fatal "cannot handle input %s" (Url.scheme_to_string t)
  ) [] l

let unpack expected = function
  | (t,(_,_,_,_,f),_) when t = expected -> f
  | _ -> "cannot handle input"

let deb_parse_input options ?(status=[]) ?(raw=false) urilist =
  let archs = 
    if not(Option.is_none options.Debian.Debcudf.native) then
      (Option.get options.Debian.Debcudf.native) :: options.Debian.Debcudf.foreign 
    else []
  in
  let dll = 
    List.map (fun l ->
      List.fold_left (fun acc (t,(_,_,_,_,f),_) ->
          match t with
          | `Deb -> List.fold_left (fun acc p -> (Deb p)::acc) acc (Debian.Packages.input_raw ~archs [f])
          | `DebSrc -> List.fold_left (fun acc p -> (DebSrc p)::acc) acc (Debian.Sources.input_raw ~archs [f])
          | _ -> fatal "cannot handle input"
          ) [] l
    ) urilist
  in
  deb_load_list options ~status ~raw dll

let pef_parse_input ?compare options urilist =
  let extras = [("maintainer",None)] in
  let dll = 
    List.map (fun l ->
        let filelist = unpack_l `Pef l in
        Pef.Packages.input_raw ~extras filelist
    ) urilist
  in
  pef_load_list ?compare options dll

let npm_parse_input ?options urilist =
  match urilist with
  |[[p]] when (unpack `Npm p) = "-" -> fatal "no stdin for npm yet"
  |[[p]] -> npm_load_list (unpack `Npm p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more than one npm request file specified on the command line";
    let p = List.hd (List.flatten l) in 
    npm_load_list (unpack `Npm p)
;;

let opam_parse_input ?options urilist =
  match urilist with
  |[[p]] when (unpack `Opam p) = "-" -> fatal "no stdin for opam yet"
  |[[p]] -> opam_load_list ?options (unpack `Opam p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more than one opam request file specified on the command line";
    let p = List.hd (List.flatten l) in 
    opam_load_list ?options (unpack `Opam p)
;;

let csw_parse_input urilist =
  let dll = 
    List.map (fun l ->
        let filelist = unpack_l `Csw l in
        Csw.Packages.input_raw filelist
    ) urilist
  in
  csw_load_list dll

let cudf_parse_input urilist =
  match urilist with
  |[[p]] when (unpack `Cudf p) = "-" -> fatal "no stdin for cudf yet"
  |[[p]] -> cudf_load_list (unpack `Cudf p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more than one cudf specified on the command line";
    let p = List.hd (List.flatten l) in 
    cudf_load_list (unpack `Cudf p)
;;

let edsp_parse_input options urilist =
  match urilist with
  |[[p]] when (unpack `Edsp p) = "-" -> fatal "no stdin for edsp yet"
  |[[p]] -> edsp_load_list options (unpack `Edsp p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more than one cudf specified on the command line";
    let p = List.hd (List.flatten l) in 
    edsp_load_list options (unpack `Edsp p)
;;

(* Check that all uris are of type that is an instance of scheme *)
(* If yes return that instance of scheme, and the list of paths  *)
(* in uris.                                                      *)
(** parse a list of uris of the same type and return a cudf packages list *)
let parse_input ?(options=None) ?(raw=false) ?compare urilist =
  let filelist = List.map (List.map Input.parse_uri) urilist in
  match Input.guess_format urilist, options with
  |`Cudf, None -> cudf_parse_input filelist

  |`Deb, None
  |`DebSrc, None -> deb_parse_input Debian.Debcudf.default_options ~raw filelist
  |`Pef, None -> pef_parse_input ?compare Debian.Debcudf.default_options filelist

  |`Deb, Some (StdOptions.Deb opt)
  |`DebSrc, Some (StdOptions.Deb opt) -> deb_parse_input opt ~raw filelist
  
(*  |`Edsp, Some (StdOptions.Edsp opt) -> edsp_parse_input opt filelist *)
  |`Edsp, _ -> edsp_parse_input Debian.Debcudf.default_options filelist
  |`Opam, _ -> opam_parse_input filelist
  |`Npm, _ -> npm_parse_input filelist
(* |`Opam, Some (StdOptions.Opam options) -> opam_parse_input ~options filelist *)

  |`Pef, Some (StdOptions.Pef opt) -> pef_parse_input ?compare opt filelist

  |`Csw, None -> csw_parse_input filelist

  |`Hdlist, None -> 
#ifdef HASRPM 
      let dll = 
        List.map (fun l ->
          let filelist = unpack_l `Hdlist l in
          Rpm.Packages.Hdlists.input_raw filelist
        ) filelist 
      in
      rpm_load_list dll
#else
    fatal "hdlist Not supported. re-configure with --with-rpm"
#endif

  |`Synthesis, None -> 
#ifdef HASRPM 
      let dll = 
        List.map (fun l ->
          let filelist = unpack_l `Synthesis l in
          Rpm.Packages.Synthesis.input_raw filelist
        ) filelist
      in
      rpm_load_list dll
#else
    fatal "synthesis input format not supported. re-configure with --with-rpm"
#endif
    |s,_ -> fatal "%s Not supported" (Url.scheme_to_string s)
;;

let supported_formats () =
  let standard = ["cudf://";"deb://";"deb://-";"eclipse://";"pef://"] in
  let rpm = 
#ifdef HASRPM 
     ["hdlist://";"synthesis://"]
#else
     []
#endif
   in
   standard@rpm
;;

(** return a list of Debian packages from a debian source file *)
let deb_load_source ?filter ?(dropalternatives=false) ?(profiles=[]) ?(noindep=false) buildarch hostarch sourcefile =
  Util.Timer.start deb_load_source_timer;
  let l = Debian.Sources.input_raw ?filter ~archs:[hostarch] [sourcefile] in
  let r = Debian.Sources.sources2packages ~dropalternatives ~noindep ~profiles buildarch hostarch l in
  Util.Timer.stop deb_load_source_timer r
;;

(** parse and merge a list of files into a cudf package list *)
let load_list ?(options=None) ?(raw=false) ?compare urilist =
  info "Parsing and normalizing..." ;
  Util.Timer.start load_list_timer;
  let u = parse_input ~options ~raw ?compare urilist in
  Util.Timer.stop load_list_timer u
;;

(** parse and merge a list of files into a cudf universe *)
let load_universe ?(options=None) ?(raw=false) ?compare uris =
  info "Parsing and normalizing..." ;
  Util.Timer.start load_list_timer;
  let (pr,cll,r,f,t,w,e) = parse_input ~options ~raw ?compare [uris] in
  let u = (pr,Cudf.load_universe (List.flatten cll), r, f, t, w,e) in
  Util.Timer.stop load_list_timer u
;;

