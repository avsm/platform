(**************************************************************************************)
(*  Copyright (C) 2015 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2015 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** Opam/Pef format conversion routines *)

open ExtLib
open Common

#define __label __FILE__ 
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

type options = {
  switch : string ; (* the active switch *)
  switches : string list ; (* list of available switches *)
  profiles : string list ; (* list of build profiles *)
  depopts : bool ; (* add depopts as hard dependencies *)
}

let default_options = {
  switch = "";
  switches = [];
  profiles = [];
  depopts = false;
}

let preamble = 
  let l = [
    ("depopts",(`Vpkgformula (Some [])));
    ("number",(`String None));
    ("active",(`Int None));
    ("version-lag",(`Nat (Some 0)));
    ("switch",(`String None));
    ]
  in
  CudfAdd.add_properties Cudf.default_preamble l

(* version-lag is the distance from the most recent package *)
let add_extra extras tables (switch,activeswitch) pkg =
  let number = ("number",`String pkg#version) in
  let switchprop = ("switch",`String switch) in
  let activeswitch =
    let n = if switch = activeswitch then 1 else 0 in
    ("active",`Int n)
  in
  let version_lag =
    let l = Hashtbl.find tables.Pef.Pefcudf.versions_table pkg#name in
    let count,i =
      List.fold_left (fun (i,r) v ->
        if v = pkg#version then i,i else i+1, r
      ) (0,0) l
    in
    ("version-lag", `Int (count - i))
  in
  let l =
    List.filter_map (fun (debprop, (cudfprop,v)) ->
      let debprop = String.lowercase debprop in
      let cudfprop = String.lowercase cudfprop in
      try 
        let s = List.assoc debprop pkg#extras in
        let typ = Cudf_types.type_of_typedecl v in
        try Some (cudfprop, Cudf_types_pp.parse_value typ s)
        with Cudf_types_pp.Type_error _ ->
          fatal "Cudf Parsing Error while converting properties %s: %s" debprop s
      with Not_found -> None
    ) extras
  in
  let depopts = ("depopts", `Vpkgformula (Pef.Pefcudf.loadll tables pkg#depopts)) in
  List.filter_map (function
    |(_,`Vpkglist []) -> None
    |(_,`Vpkgformula []) -> None
    |e -> Some e
  )
  [switchprop; number; depopts; activeswitch; version_lag] @ l
;;

let decode name =
  let extract_name x =
    try String.sub x 0 (String.index x ':') with
    | Not_found -> x
  in
  CudfAdd.decode (extract_name name)

(* each package generates more than one cudf package. One for active switch
   that is not declaclare not available by the package . Each package is 
   translated differently considering the profiles associated to each dependency *)
let tocudf tables ?(options=default_options) ?(extras=[]) pkg =
  let all_switches = (List.unique (options.switch::options.switches)) in
  if options.switch = "" then warning "Now default switch specified in opam -> cudf conversion";
  List.fold_left (fun acc switch ->
    (* include this package if it is not declared as not available and if it is
     * used in some dependency. Otherwise there is no point to include it *)
    if List.mem "all" pkg#switch || List.mem switch pkg#switch then
      let depends = if options.depopts then pkg#depends@pkg#depopts else pkg#depends in
      let keep =
        if List.mem switch pkg#baselist then `Keep_package else 
        if List.mem switch pkg#pinnedlist then `Keep_version 
        else `Keep_none
      in
      let cudfpkg = 
        { Cudf.default_package with
          Cudf.package = CudfAdd.encode (pkg#name^":"^switch);
          version = Pef.Pefcudf.get_cudf_version tables (pkg#name,pkg#version);
          installed = List.mem switch pkg#installedlist;
          keep; 
          depends = Pef.Pefcudf.loadll tables ~arch:switch ~archs:all_switches depends;
          conflicts = Pef.Pefcudf.loadlc tables ~arch:switch ~archs:all_switches pkg#conflicts;
          provides = Pef.Pefcudf.loadlp tables ~arch:switch ~archs:all_switches pkg#provides;
          pkg_extra = add_extra extras tables (switch,options.switch) pkg;
        }
      in (cudfpkg::acc)
    else acc
  ) [] all_switches

(* convert an opam vpkg list into a cudf vpkg list *)
let encode_vpkglist tables switch vpkglist =
  let to_cudf (p,v) = (p,Pef.Pefcudf.get_cudf_version tables (p,v)) in
  List.map (fun (vpkgname,constr) ->
    let vpkgname =
      match vpkgname with
      |(n,None) -> (n,Some switch)
      |_ -> vpkgname
    in
    Pef.Pefcudf.pefvpkg to_cudf (vpkgname,constr)
  ) vpkglist

(** convert a opam request to a cudf request *)
let requesttocudf tables universe request =
  let remove = List.map (fun (n,_) -> (n,None)) in
  let switch = request.Packages.switch in
  if request.Packages.dist_upgrade then
    let to_upgrade = function
      |[] ->
        let filter pkg = pkg.Cudf.installed in
        let l = Cudf.get_packages ~filter universe in
        List.map (fun pkg -> (pkg.Cudf.package,None)) l
      |l -> encode_vpkglist tables switch l
    in
    {Cudf.default_request with
    Cudf.request_id = "Opam";
    Cudf.upgrade = to_upgrade request.Packages.install;
    Cudf.remove = remove (encode_vpkglist tables switch request.Packages.remove);
    }
  else
    {Cudf.default_request with
    Cudf.request_id = "Opam";
    Cudf.install = encode_vpkglist tables switch request.Packages.install;
    Cudf.remove = remove (encode_vpkglist tables switch request.Packages.remove);
    Cudf.upgrade = encode_vpkglist tables switch request.Packages.upgrade;
    }

let load_list ?(options=default_options) compare l =
  let timer = Util.Timer.create "Opam.ToCudf" in
  Util.Timer.start timer;
  let tables = Pef.Pefcudf.init_tables compare l in
  let pkglist = List.flatten (List.map (tocudf ~options tables) l) in
  Pef.Pefcudf.clear tables;
  Util.Timer.stop timer pkglist

let load_universe ?(options=default_options) compare l =
  let pkglist = load_list ~options compare l in
  let timer = Util.Timer.create "Opam.ToCudf" in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ
