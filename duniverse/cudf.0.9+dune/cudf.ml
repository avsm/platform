(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009-2012  Stefano Zacchiroli <zack@upsilon.cc>            *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

open ExtLib
open Printf

open Cudf_types
open Cudf_types_pp

exception Constraint_violation of string

type package = {
  package : pkgname ;
  version : version ;
  depends : vpkgformula ;
  conflicts : vpkglist ;
  provides : veqpkglist ;
  installed : bool ;
  was_installed : bool ;
  keep : enum_keep ;
  pkg_extra : typed_value stanza ;
}

type request = {
  request_id : string ;
  install : vpkglist ;
  remove : vpkglist ;
  upgrade : vpkglist ;
  req_extra : typed_value stanza ;
}

type preamble = {
  preamble_id : string ;
  property : typedecl ;
  univ_checksum: string ;
  status_checksum: string ;
  req_checksum: string ;
}

type cudf_doc = preamble option * package list * request
type cudf_item =
    [ `Preamble of preamble | `Package of package | `Request of request ]
type universe = {
  id2pkg: ((string * int), package) Hashtbl.t;	(** <name, ver> -> pkg *)
  name2pkgs: (string, package list ref) Hashtbl.t; (** name -> pkg list ref *)
  uid2pkgs: (int, package) Hashtbl.t; (** int uid -> pkg *)
  id2uid: ((pkgname * version), int) Hashtbl.t; (** <name, ver> -> int uid *)
  features: (string, (package * version option) list ref) Hashtbl.t;
  (** feature -> avail feature versions
      Each available feature is reported as a pair 
      <owner, provided version>, where owner is the package
      providing it. Provided version "None" means "all possible
      versions" *)
  mutable univ_size : int;
  mutable inst_size : int;
}
type cudf = preamble * universe * request
type solution = preamble * universe

let universe_size univ = univ.univ_size
let installed_size univ = univ.inst_size

let (=%) pkg1 pkg2 =
  pkg1.package = pkg2.package && pkg1.version = pkg2.version

let (<%) pkg1 pkg2 =
  Pervasives.compare (pkg1.package, pkg1.version) (pkg2.package, pkg2.version)

let (>%) pkg1 pkg2 =
  Pervasives.compare (pkg2.package, pkg2.version) (pkg1.package, pkg1.version)

let default_preamble = {
  preamble_id = "" ;
  property = [] ;
  univ_checksum = "" ;
  status_checksum = "" ;
  req_checksum = "" ;
}

let default_package = {
  package = "" ;
  version = 0 ;
  depends = [] ;
  conflicts = [] ;
  provides = [] ;
  installed = false ;
  was_installed = false ;
  keep = `Keep_none ;
  pkg_extra = [] ;
}

let default_request = {
  request_id = "" ;
  install = [] ;
  remove = [] ;
  upgrade = [] ;
  req_extra = [] ;
}

let empty_universe ?(size=1023) () =
  { id2pkg = Hashtbl.create size ;
    uid2pkgs = Hashtbl.create size;
    id2uid = Hashtbl.create size;
    name2pkgs = Hashtbl.create size;
    features = Hashtbl.create size;
    univ_size = 0 ; inst_size = 0 ;
  }

let add_to_hash_list h n p =
  try let l = Hashtbl.find h n in l := p :: !l
  with Not_found -> Hashtbl.add h n (ref [p])

let get_hash_list h n = try !(Hashtbl.find h n) with Not_found -> []

(** process all features (i.e., Provides) provided by a given package
    and fill with them a given feature table *)
let expand_features pkg features =
    List.iter
      (function
        | name, None -> add_to_hash_list features name (pkg, None)
        | name, Some (_, ver) -> add_to_hash_list features name (pkg, (Some ver)))
      pkg.provides

let add_package_aux univ pkg uid =
  let id = pkg.package, pkg.version in
  if Hashtbl.mem univ.id2pkg id then
    raise (Constraint_violation (sprintf "duplicate package: <%s, %d>" pkg.package pkg.version))
  else begin
    Hashtbl.add univ.uid2pkgs uid pkg;
    Hashtbl.add univ.id2uid id uid;
    Hashtbl.add univ.id2pkg id pkg;
    add_to_hash_list univ.name2pkgs pkg.package pkg;
    expand_features pkg univ.features;
    univ.univ_size <- univ.univ_size + 1;
    if pkg.installed then
      univ.inst_size <- univ.inst_size + 1
  end

let add_package univ pkg =
  let uid = (Hashtbl.length univ.uid2pkgs) + 1 in
  add_package_aux univ pkg uid

let remove_package univ id =
  if not (Hashtbl.mem univ.id2pkg id) then ()
  else begin
    let uid = Hashtbl.find univ.id2uid id in
    let p = Hashtbl.find univ.uid2pkgs uid in

    let l = Hashtbl.find univ.name2pkgs p.package in
    l := List.remove !l p;
    if List.length !l = 0 then
      Hashtbl.remove univ.name2pkgs p.package;

    List.iter
      (function
      | name, None ->
          let l = Hashtbl.find univ.features name in
          l := List.remove !l (p, None);
          if List.length !l = 0 then
            Hashtbl.remove univ.features name
      | name, Some (_, ver) ->
          let l = Hashtbl.find univ.features name in
          l := List.remove !l (p, (Some ver));
          if List.length !l = 0 then
            Hashtbl.remove univ.features name)
      p.provides;

    Hashtbl.remove univ.uid2pkgs uid;
    Hashtbl.remove univ.id2uid id;
    Hashtbl.remove univ.id2pkg id;

    univ.univ_size <- univ.univ_size - 1;
    if p.installed then
      univ.inst_size <- univ.inst_size - 1;
  end

let load_universe pkgs =
  let size = List.length pkgs in
  let univ = empty_universe ~size () in
  let uid = ref 0 in
  List.iter
    (fun pkg ->
      add_package_aux univ pkg !uid;
      incr uid)
    pkgs;
  univ

let package_by_uid univ = Hashtbl.find univ.uid2pkgs
let uid_by_package univ pkg =
  Hashtbl.find univ.id2uid (pkg.package, pkg.version)

let lookup_package univ = Hashtbl.find univ.id2pkg
let mem_package univ = Hashtbl.mem univ.id2pkg

let iter_packages f univ = Hashtbl.iter (fun _id pkg -> f pkg) univ.id2pkg
let iteri_packages f univ = Hashtbl.iter (fun _id pkg -> f _id pkg) univ.uid2pkgs

let fold_packages f init univ =
  Hashtbl.fold (fun _id pkg acc -> f acc pkg) univ.id2pkg init

let iter_packages_by_name f univ =
  Hashtbl.iter (fun n { contents = l } -> f n l) univ.name2pkgs

let fold_packages_by_name f a univ =
  Hashtbl.fold (fun n { contents = l } a -> f a n l) univ.name2pkgs a

let package_names univ = List.of_enum (Hashtbl.keys univ.name2pkgs)

let get_packages ?filter univ =
  match filter with
    | None -> fold_packages (fun acc pkg -> pkg :: acc) [] univ
    | Some test ->
	fold_packages
	  (fun acc pkg -> if test pkg then pkg :: acc else acc)
	  [] univ

let (|=) v = function
  | None -> true
  | Some (`Eq, v') -> v = v'
  | Some (`Neq, v') -> v <> v'
  | Some (`Geq, v') -> v >= v'
  | Some (`Gt, v') -> v > v'
  | Some (`Leq, v') -> v <= v'
  | Some (`Lt, v') -> v < v'

let version_matches = (|=)

let status univ =
  let univ' = empty_universe () in
  Hashtbl.iter
    (fun id pkg -> match pkg with
    | { installed = true } ->
      Hashtbl.add univ'.id2pkg id pkg;
      add_to_hash_list univ'.name2pkgs pkg.package pkg;
      expand_features pkg univ'.features
    | _ -> ())
    univ.id2pkg;
  univ'.inst_size <- univ.inst_size;
  univ'.univ_size <- univ.inst_size; (* as we filtered on installed pkgs *)
  univ'

let lookup_packages ?(filter=None) univ pkgname = 
  let packages = get_hash_list univ.name2pkgs pkgname in
    match filter with
	None -> packages
      | Some _ as pred -> List.filter (fun p -> p.version |= pred) packages

let get_installed univ pkgname =
  List.filter (fun { installed = i } -> i) (lookup_packages univ pkgname)

let mem_installed ?(include_features = true) ?(ignore = fun _ -> false)
    univ (name, constr) =
  let pkg_filter = fun pkg -> not (ignore pkg) in
  let mem_feature constr =
    let feats = get_hash_list univ.features name in
      List.exists
	(function
           | owner_pkg, _ when not owner_pkg.installed -> false
	   | owner_pkg, None -> pkg_filter owner_pkg
	   | owner_pkg, Some v -> pkg_filter owner_pkg && v |= constr)
	feats in
  let pkgs = List.filter pkg_filter (get_installed univ name) in
    List.exists (fun pkg -> pkg.version |= constr) pkgs
    || (include_features && mem_feature constr)

let who_provides ?(installed=true) univ (pkgname, constr) =
  List.filter
    (function 
      |pkg , _ when not pkg.installed && installed -> false
      |_, None -> true 
      | _, Some v -> v |= constr
    )
    (get_hash_list univ.features pkgname)

let lookup_typed_package_property pkg = function
  | "package" -> `Pkgname pkg.package
  | "version" -> `Posint pkg.version
  | "depends" -> `Vpkgformula pkg.depends
  | "conflicts" -> `Vpkglist pkg.conflicts
  | "provides" -> `Veqpkglist pkg.provides
  | "installed" -> `Bool pkg.installed
  | "keep" -> `Enum (keep_enums, string_of_keep pkg.keep)
  | prop_name -> List.assoc prop_name pkg.pkg_extra

let lookup_typed_request_property req = function
  | "request" -> `String req.request_id
  | "install" -> `Vpkglist req.install
  | "remove" -> `Vpkglist req.remove
  | "upgrade" -> `Vpkglist req.upgrade
  | prop_name -> List.assoc prop_name req.req_extra

let lookup_typed_preamble_property pre = function
  | "preamble" -> `String pre.preamble_id
  | "property" -> `Typedecl pre.property
  | "univ-checksum" -> `String pre.univ_checksum
  | "status-checksum" -> `String pre.status_checksum
  | "req-checksum" -> `String pre.req_checksum
  | _ -> raise Not_found


let lookup_package_property pkg prop =
  string_of_value (lookup_typed_package_property pkg prop)

let lookup_request_property req prop =
  string_of_value (lookup_typed_request_property req prop)

let lookup_preamble_property pre prop =
  string_of_value (lookup_typed_preamble_property pre prop)

let lookup_package_typedecl ?(extra = []) prop =
  List.assoc prop (Cudf_conf.package_typedecl @ extra)
