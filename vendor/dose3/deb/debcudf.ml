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

(** Debian Specific Cudf conversion routines *)

module SSet = Set.Make(struct
  type t = string * Pef.Packages_types.constr option
  let compare = Pervasives.compare
end)
module OcamlHashtbl = Hashtbl

open ExtLib
open Common
open Packages
module Version = Versioning.Debian

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module SMap = Map.Make (String)

type tables = {
  virtual_table : (string,SSet.t ref) OcamlHashtbl.t; (** all names of virtual packages    *)
  unit_table : unit Util.StringHashtbl.t ;            (** all names of real packages       *)
  versions_table : int Util.StringHashtbl.t;          (** version -> int table             *)
  reverse_table : (string SMap.t) ref Util.IntHashtbl.t;
  versioned_table : unit Util.StringHashtbl.t;        (** all (versions,name) tuples. 
                                                        all versions mentioned of depends, 
                                                        pre_depends, conflict and breaks   *)
  essential_table:
    (string,Packages.package list ref) Hashtbl.t;     (** name -> packages list            *)
}

let create n = {
  virtual_table = OcamlHashtbl.create (10 * n);
  unit_table = Util.StringHashtbl.create (2 * n);
  versions_table = Util.StringHashtbl.create (10 * n);
  versioned_table = Util.StringHashtbl.create (10 *n);
  reverse_table = Util.IntHashtbl.create (10 * n);
  essential_table = Hashtbl.create (n / 100);
}

type lookup = {
  from_cudf : Cudf.package -> (string * string);
  to_cudf : (string * string) -> Cudf.package
}

type extramap = (string * (string * Cudf_types.typedecl1)) list

type options = {
  extras_opt : extramap ;
  native : string option; (* the native architecture *)
  foreign : string list ; (* list of foreign architectures *)
  host : string option;   (* the host architecture - cross compile *)
  ignore_essential : bool;
  builds_from : bool;
  drop_bd_indep : bool;
  profiles : string list; (* list of active build profiles *)
}

let default_options = {
  extras_opt = [] ;
  native = None;
  foreign = [];
  host = None;
  ignore_essential = false;
  builds_from = false;
  drop_bd_indep = false;
  profiles = [];
}

let add_name_arch n = function
  |"" -> CudfAdd.encode n
  |a -> CudfAdd.encode (Printf.sprintf "%s:%s" n a) 

let add_arch native_arch name = function
  |"all" -> add_name_arch name native_arch
  |package_arch -> add_name_arch name package_arch

(* add arch info to a vpkg 
 - if it's a :any dependency then just encode the name without arch information :
   means that this dependency/conflict can be satified by any packages
 - if it is a :native dependency then add the native architecture
 - if it is a package:arch dependency, then encode it as such
 - if the package dependency does not have any annotations :
   + if the package is architecture all, then all dependencies are interpreted as
     dependencies on native architecture packages.
   + otherwise all dependencies are satisfied by packages of the same architecture
     of the package we are considering 
*)
let add_arch_info ?(native_arch="") ?(package_arch="") = function
  |n,Some "any" -> CudfAdd.encode n
  |n,Some "native" -> add_name_arch n native_arch
  |n,Some a -> add_name_arch n a
  |n,None -> add_arch native_arch n package_arch
;;

let clear tables =
  OcamlHashtbl.clear tables.virtual_table;
  Util.StringHashtbl.clear tables.unit_table;
  Util.StringHashtbl.clear tables.versions_table;
  Util.StringHashtbl.clear tables.versioned_table;
  Util.IntHashtbl.clear tables.reverse_table;
  Hashtbl.clear tables.essential_table;
;;

(* string version -> int version Hash *)
let add_v tables k v =
  if not(Util.StringHashtbl.mem tables.versions_table k) then
    Util.StringHashtbl.add tables.versions_table k v

(* package names Set *)
let add_p table k =
  if not(Util.StringHashtbl.mem table k) then
    Util.StringHashtbl.add table k ()

(* (version,name) Set *)
let add_pairs table (v,p) =
  if not(Util.StringPairHashtbl.mem table (v,p)) then
    Util.StringPairHashtbl.add table (v,p) ()

(* name -> (name,constr) Hash *)
let add_s h k v =
  try let s = OcamlHashtbl.find h k in s := SSet.add v !s
  with Not_found -> OcamlHashtbl.add h k (ref (SSet.singleton v))
;;

(* collect names of virtual packages 
   associates to each virtual package a list of real packages *)
let init_virtual_table tables pkg =
  List.iter (fun ((name,_),constr) -> 
    add_s tables.virtual_table name (pkg#name,constr)
  ) pkg#provides

(* collect names of real packages *)
let init_unit_table tables pkg = 
  add_p tables.unit_table pkg#name

(* collect all versions mentioned anywhere in the universe, including source fields *)
let init_versions_table tables table pkg =
  let versionedtables = tables.versioned_table in
  let conj_iter l =
    List.iter (fun ((name,_),constr) ->
      add_p versionedtables name;
      match constr with
      |None -> ()
      |Some(_,version) -> add_pairs table (version,name)
    ) l
  in
  let cnf_iter ll = List.iter conj_iter ll in
  let add_source pv = function
    |(p,None) -> add_pairs table (pv,p)
    |(p,Some(v)) -> add_pairs table (v,p)
  in
  add_pairs table (pkg#version,pkg#name);
  conj_iter pkg#breaks;
  conj_iter pkg#provides;
  conj_iter pkg#conflicts ;
  conj_iter pkg#replaces;
  cnf_iter pkg#depends;
  cnf_iter pkg#pre_depends;
  cnf_iter pkg#recommends;
  add_source pkg#version pkg#source
;;

let init_tables ?(options=default_options) ?(step=1) ?(versionlist=[]) pkglist =
  let n = List.length pkglist in
  let tables = create n in 
  let temp_versions_table = Util.StringPairHashtbl.create (10 * n) in
  let ivrt = init_virtual_table tables in
  let ivt = init_versions_table tables temp_versions_table in
  let iut = init_unit_table tables in
  let iet pkg =
    if not options.ignore_essential && pkg#essential then
      CudfAdd.add_to_package_list tables.essential_table (pkg#name) pkg
  in
  List.iter (fun v -> add_pairs temp_versions_table (v,"")) versionlist; 
  List.iter (fun pkg -> iet pkg; ivt pkg; ivrt pkg ; iut pkg) pkglist ;
  let l = Util.StringPairHashtbl.fold (fun v _ acc -> v::acc) temp_versions_table [] in
  let add_reverse i (n,v) =
    try 
      let m = Util.IntHashtbl.find tables.reverse_table i in 
      m := (SMap.add n v !m)
    with Not_found ->
      let m = SMap.add n v SMap.empty in
      Util.IntHashtbl.add tables.reverse_table i (ref m)
  in
  let sl = List.sort ~cmp:(fun x y -> Version.compare (fst x) (fst y)) l in
  let rec numbers (prec,i) = function
    |[] -> ()
    |(v,n)::t ->
      if Version.equal v prec then begin
        add_v tables v i;
        if n <> "" then add_reverse i (n,v);
        numbers (prec,i) t
      end else begin
        add_v tables v (i+step);
        if n <> "" then add_reverse (i+step) (n,v);
        numbers (v,(i+step)) t
      end
  in
  (* versions start from 1 *)
  numbers ("",1) sl;
  tables
;;

let get_cudf_version tables (package,version) =
  try Util.StringHashtbl.find tables.versions_table version
  with Not_found -> begin
    warning "Package (%s,%s) does not have an associated cudf version" package version;
    raise Not_found
  end

let get_real_name name = 
  (* Remove --virtual- and architecture encoding *)
  let dn = (CudfAdd.decode name) in
  let no_virtual =
    if ExtString.String.starts_with dn "--vir"
    then ExtString.String.slice ~first:10 dn
    else dn
  in
  try
    let (n,a) = ExtString.String.split no_virtual ":" in
    if n = "src" then (a,None) else (n,Some a)
  with Invalid_string -> (no_virtual,None)

let get_real_version tables (cudfname,cudfversion) =
  let (debname,arch) = get_real_name cudfname in
  try
    if cudfversion = Util.max32int || cudfversion = Util.max32int - 1 then 
      (debname,arch,"nan")
    else
      let m = !(Util.IntHashtbl.find tables.reverse_table cudfversion) in
      try (debname,arch,SMap.find debname m) 
      with Not_found ->
        let known =
          String.concat "," (
            List.map (fun (n,v) ->
              Printf.sprintf "(%s,%s)" n v
            ) (SMap.bindings m)
          )
        in
        fatal "Unable to get real version for %s (%i)\n All Known versions for this package are %s" debname cudfversion known 
  with Not_found ->
    fatal "Package (%s,%d) does not have an associated debian version" cudfname cudfversion

let loadl ?native_arch ?package_arch tables l =
  List.flatten (
    List.map (fun ((name,_) as vpkgname,constr) ->
      let encname = add_arch_info ?native_arch ?package_arch vpkgname in
      match constr with
      |None ->
          (* Versioned virtual packages will satisfiy non versioned dependencies *)
          if (OcamlHashtbl.mem tables.virtual_table name) then
            [(encname, None);("--virtual-"^encname,Some(`Eq,Util.max32int - 1))]
          else
            [(encname, None)]
      |Some(op,v) ->
          (* Non-versioned virtual packages will not satisfy versioned dependencies. *)
          let op = Pef.Pefcudf.pefcudf_op op in
          try
            match SSet.elements !(OcamlHashtbl.find tables.virtual_table name) with
            |[] -> assert false
            |l ->
                let dl = 
                  List.filter_map (function
                    |(_,None) -> Some ("--virtual-"^encname,Some(`Eq,Util.max32int))
                    |(_,Some _) ->
                        let constr = Some(op,get_cudf_version tables (name,v)) in
                        Some ("--virtual-"^encname,constr)
                  ) l
                in
                if Util.StringHashtbl.mem tables.unit_table name then
                  let constr = Some(op,get_cudf_version tables (name,v)) in
                  (encname,constr)::dl
                else dl
          with Not_found -> 
              let constr = Some(op,get_cudf_version tables (name,v)) in
              [(encname,constr)]
    ) l
  )

let loadll ?native_arch ?package_arch tables ll =
  List.filter_map (fun l ->
    match loadl ?native_arch ?package_arch tables l with
    |[] -> None
    |l -> Some l
  ) ll

(* we add a self conflict here, because in debian each package is in conflict
   with all other versions of the same package *)
let loadlc ?native_arch ?package_arch tables name l =
  (CudfAdd.encode name, None)::(loadl ?native_arch ?package_arch tables l)

let loadlp ?native_arch ?package_arch tables l =
  List.flatten (
    List.map (fun ((name,_) as vpkgname,constr) ->
      let encname = add_arch_info ?native_arch ?package_arch vpkgname in
      match constr with
      |None -> [("--virtual-"^encname,Some(`Eq,Util.max32int - 1))]
      |Some("=",v) ->
        let constr = Some(`Eq,get_cudf_version tables (name,v)) in
        [("--virtual-"^encname,constr);(encname,constr)]
      |_ -> fatal "This should never happen : a provide can be either = or unversioned"
    ) l
  )


(* ========================================= *)

let preamble = 
  let l = [
    (* name,number,type,architecture are mandatory properties -- no default *)
    ("name", (`String None));
    ("number",(`String None));
    ("type",(`String None));
    ("architecture",(`String None));

    ("replaces",(`Vpkglist (Some [])));
    ("recommends",(`Vpkgformula (Some [])));
    ("priority",(`String (Some "")));
    ("source",(`String (Some ""))) ;
    ("sourcenumber",(`String (Some "")));
    ("sourceversion",(`Int (Some 1))) ;
    ("essential",(`Bool (Some false))) ;
    ("filename",(`String (Some "")));
    ("installedsize",(`Int (Some 0)));
    ("multiarch",(`String (Some "")));
    ("native",(`Int (Some 0)));
    ]
  in
  CudfAdd.add_properties Cudf.default_preamble l

let add_extra ?native_arch extras tables pkg =
  let name = 
    let n = 
      if String.starts_with pkg#name "src:" then
        String.sub pkg#name 4 ((String.length pkg#name) - 4)
      else pkg#name
    in
    Some ("name",`String n) 
  in
  let number = Some ("number",`String pkg#version) in
  let architecture = Some ("architecture",`String pkg#architecture) in
  let priority = Some ("priority",`String pkg#priority) in
  let essential =
    if pkg#essential then 
      Some ("essential", `Bool true) 
    else None 
  in
  let (source,sourcenumber,sourceversion) =
    let (n,v) =
      match pkg#source with
      |("",_) -> (pkg#name,pkg#version)
      |(n,None) -> (n,pkg#version)
      |(n,Some v) -> (n,v)
    in
    let cv = get_cudf_version tables ("",v) in
    Some ("source",`String n),
    Some ("sourcenumber", `String v),
    Some ("sourceversion", `Int cv)
  in
  let recommends = 
    match loadll tables pkg#recommends with
    |[] -> None
    |l -> Some ("recommends", `Vpkgformula l) 
  in
  let replaces = 
    match loadl tables pkg#replaces with
    |[] -> None
    |l -> Some ("replaces", `Vpkglist l)
  in
  let native =
    if Option.is_some native_arch && 
      (Option.get native_arch) = pkg#architecture 
    then Some ("native",`Int 1)
    else None
  in
  let extras = 
    ("Type",("type",`String None))::
    ("Filename",("filename",`String None))::
    ("Multi-Arch",("multiarch",`String None))::
    ("Installed-Size",("installedsize",`Int (Some 0)))::
    extras 
  in
  let l =
    List.filter_map (fun (debprop, (cudfprop,v)) ->
      try 
        let s = pkg#get_extra debprop in
        let typ = Cudf_types.type_of_typedecl v in
        try Some (cudfprop, Cudf_types_pp.parse_value typ s)
        with Cudf_types_pp.Type_error _ ->
          fatal "Cudf Parsing Error while converting properties %s: %s" debprop s
      with Not_found -> None
    ) extras
  in
  List.filter_map (function
    |Some (_,`Vpkglist []) -> None
    |Some (_,`Vpkgformula []) -> None
    |Some (_,`String "") -> None
    |e -> e
  )
  [priority; name; architecture; number;
  source; sourcenumber; sourceversion; 
  recommends; replaces; essential;native]@ l
;;

let add_inst inst pkg = inst || Packages.is_installed pkg 

let tocudf tables ?(options=default_options) ?(inst=false) pkg =
  let bind m f = List.flatten (List.map f m) in
  let encpkgname = CudfAdd.encode pkg#name in
  if not(Option.is_none options.native) then begin
    let native_arch = Option.get options.native in
    let package_arch =
      if Sources.is_source pkg then
        match options.host with
        |None -> native_arch          (* source package : build deps on the native arch *)
        |Some host_arch  -> host_arch (* source package : build deps on the cross arch *)
      else
        pkg#architecture              (* binary package : dependencies are package specific *)
    in
    let _name = 
      (* if the package is a source package the name does not need an
       * architecture annotation. Nobody depends on it *)
      if Sources.is_source pkg then encpkgname
      else add_arch native_arch pkg#name package_arch
    in
    let _version = get_cudf_version tables (pkg#name,pkg#version)  in
    let _provides = 
      let archlessprovide = (encpkgname,None) in
      let multiarchprovides = 
        match pkg#multiarch with
        |(`No|`Same) ->
           (* package_arch provides *)
           loadlp ~native_arch ~package_arch tables pkg#provides
        |`Foreign ->
           (* packages of same name and version of itself in all archs except its own
              each package this package provides is provided in all arches *)
           bind (native_arch::options.foreign) (function
                |arch when arch = package_arch ->
                    loadlp ~native_arch ~package_arch:arch tables pkg#provides
                |arch ->
                    (add_arch native_arch pkg#name arch,Some(`Eq,_version)) ::
                      (loadlp ~native_arch ~package_arch:arch tables pkg#provides)
              )
        |`Allowed ->
           (* archless package and arch: any package *)
           (* all provides as arch: any *)
           (* package_arch provides *)
           let any = (CudfAdd.encode (pkg#name^":any"),None) in
           let l = 
             loadlp ~native_arch ~package_arch tables (
               bind pkg#provides (fun ((name,_), c) ->
                   [((name, Some "any"), c);
                   ((name,None),c)]
               )
             )
           in any::l
      in archlessprovide :: multiarchprovides
    in
    let _conflicts = 
      let originalconflicts = pkg#breaks @ pkg#conflicts in
      (* self conflict *)
      let sc = (add_arch native_arch pkg#name package_arch,None) in
      let multiarchconstraints = 
        match pkg#multiarch with
        |(`No|`Foreign|`Allowed) -> 
            (* conflict with all other packages with differents archs *)
            let mac = (encpkgname,None) in
            [sc; mac] 
        |`Same -> 
            (* conflict with packages of same name but different arch and version*)
            let masc =
              List.filter_map (function
                |arch when arch = package_arch -> None
                |arch -> Some(add_arch native_arch pkg#name arch,Some(`Neq,_version))
              ) (native_arch::options.foreign)
            in
            sc :: masc 
      in
      let multiarchconflicts =
        let selfconflict = function
	  | ((n,a),None) -> (n = pkg#name) || List.exists (fun ((p,_),_) -> p=n) pkg#provides 
	  | ((n,a),_)    -> (n = pkg#name)
	in
        let realpackage = Util.StringHashtbl.mem tables.unit_table in
        match pkg#multiarch with
        |(`No|`Foreign|`Allowed) -> 
            bind (native_arch::options.foreign) (fun arch ->
                loadl ~native_arch ~package_arch:arch tables originalconflicts
              )
        |`Same -> 
            (* XXX : Duplicated conflicts ! *)
            bind (native_arch::options.foreign) (fun arch ->
              let l =
                bind originalconflicts (fun ((n,a),c) ->
                  (*
                  debug "M-A-Same: examining pkg %s, conflicting with package %s (self confl = %b)" pkg.name n (selfconflict ((n,a),c));
                  *)
                  match realpackage n, selfconflict ((n,a),c) with
                  |true,false  -> [((n,a),c)]  (* real conflict *)
                  |true, true  -> []           (* self conflict on real package, drop it *)
                  |false,false ->
                      begin match c with 
                      |None -> [((n,a),None)] (* virtual conflict *)
                      |_ -> []                (* real conflict on non-existent package, drop it *)
                      end
                  |false, true ->             (* a virtual package and a self conflict *)
                      begin
                        (*debug "M-A-Same: pkg %s has a self-conflict via virtual package: %s" pkg#name n;*)
                        try
                          List.filter_map (fun (pn,_) ->
                            if pn <> pkg#name then begin
                              (*debug "M-A-Same: adding conflict on real package %s for %s" pn pkg#name; *)
                              Some((pn,a),None)
                            end else None
                          ) (SSet.elements !(OcamlHashtbl.find tables.virtual_table n))
                        with Not_found -> []
                      end
                )
              in
              loadl ~native_arch ~package_arch:arch tables l
            )
      in
      multiarchconflicts @ multiarchconstraints
    in
    let _depends = 
      loadll ~native_arch ~package_arch tables (pkg#pre_depends @ pkg#depends)
    in
    let _keep pkg =
      if (package_arch <> native_arch && package_arch <> "all") then
        `Keep_none
      else if Packages.is_on_hold pkg && Packages.is_installed pkg then
        `Keep_version
      else
        `Keep_none
    in
    { Cudf.default_package with
      Cudf.package = _name ;
      Cudf.version = _version ;
      Cudf.keep = _keep pkg ;
      Cudf.depends = _depends ;
      Cudf.conflicts = _conflicts ;
      Cudf.provides = _provides ;
      Cudf.installed = add_inst inst pkg ;
      Cudf.pkg_extra = add_extra ~native_arch options.extras_opt tables pkg ;
    }
  end else
    (* :any and :native are not yet allowed in the Debian archive because of
     * wanna-build not supporting it but at least :native is required to be
     * removed because of build-essential:native
     * :any and :native can safely be removed as only packages of native
     * architecture are considered here anyways
     * XXX : in the future, dependencies on :$arch will be introduced (for example
     * for building cross compilers) they have to be handled here as well *)
    { Cudf.default_package with
      Cudf.package = encpkgname ;
      Cudf.version = get_cudf_version tables (pkg#name,pkg#version) ;
      Cudf.keep = if Packages.is_on_hold pkg then `Keep_version else `Keep_none ;
      Cudf.depends = loadll tables (pkg#pre_depends @ pkg#depends); 
      Cudf.conflicts = loadlc tables pkg#name (pkg#breaks @ pkg#conflicts) ;
      Cudf.provides = loadlp tables pkg#provides ;
      Cudf.installed = add_inst inst pkg; 
      Cudf.pkg_extra = add_extra options.extras_opt tables pkg ;
    }

let get_essential ?(options=default_options) tables =
  Hashtbl.fold (fun name { contents = l } acc ->
    ([(name,None)],List.map (tocudf tables ~options) l)::acc
  ) tables.essential_table []

let load_list ?options l =
  let timer = Util.Timer.create "Debian.ToCudf" in
  Util.Timer.start timer;
  let tables = init_tables ?options l in
  let pkglist = List.map (tocudf tables ?options) l in
  clear tables;
  Util.Timer.stop timer pkglist

let load_universe ?options l =
  let timer = Util.Timer.create "Debian.ToCudf" in
  let pkglist = load_list ?options l in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ
