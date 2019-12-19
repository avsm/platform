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

(** Representation of a parsed source description item. all fields are string *)

open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

class source ?(name=("Package",None)) ?(version=("Version",None)) 
  ?(architecture=("Architecture",None)) ?(build_depends=("Build-Depends",None)) 
  ?(build_depends_indep=("Build-Depends-Indep",None)) ?(build_depends_arch=("Build-Depends-Arch",None))
  ?(build_conflicts=("Build-Conflicts",None)) ?(build_conflicts_indep=("Build-Conflicts-Indep",None))
  ?(build_conflicts_arch=("Build-Conflicts-Arch",None)) par = object

  val name : (string * Pef.Packages_types.name) =
    let f = Pef.Packages.parse_s ~required:true Pef.Packages.parse_name in
    Pef.Packages.get_field_value f par name

  val version : (string * Pef.Packages_types.version) =
    let f = Pef.Packages.parse_s ~required:true Pef.Packages.parse_version in
    Pef.Packages.get_field_value f par version

  val architecture : (string * Pef.Packages_types.architecture list) =
    let f = Pef.Packages.parse_s ~required:true Pef.Packages.parse_archlist in
    Pef.Packages.get_field_value f par architecture

  val build_depends : (string * Pef.Packages_types.builddepsformula) =
    let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepsformula in
    Pef.Packages.get_field_value f par build_depends

  val build_depends_indep : (string * Pef.Packages_types.builddepsformula) =
    let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepsformula in
    Pef.Packages.get_field_value f par build_depends_indep

  val build_depends_arch : (string * Pef.Packages_types.builddepsformula) =
    let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepsformula in
    Pef.Packages.get_field_value f par build_depends_arch

  val build_conflicts : (string * Pef.Packages_types.builddepslist) =
    let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepslist in
    Pef.Packages.get_field_value f par build_conflicts

  val build_conflicts_indep : (string * Pef.Packages_types.builddepslist) =
    let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepslist in
    Pef.Packages.get_field_value f par build_conflicts_indep

  val build_conflicts_arch : (string * Pef.Packages_types.builddepslist) =
    let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepslist in
    Pef.Packages.get_field_value f par build_conflicts_arch

  method name = snd name
  method version = snd version
  method architecture = snd architecture
  method build_depends = snd build_depends
  method build_depends_indep = snd build_depends_indep
  method build_depends_arch = snd build_depends_arch
  method build_conflicts = snd build_conflicts
  method build_conflicts_indep = snd build_conflicts_indep
  method build_conflicts_arch = snd build_conflicts_arch

  method pp oc =
    Pef.Printer.pp_string_wl oc name;
    Pef.Printer.pp_string_wl oc version;
    Pef.Printer.pp_string_list_wl ~sep:" " oc architecture;

    Pef.Printer.pp_builddepformula_wl oc build_depends;
    Pef.Printer.pp_builddeplist_wl oc build_conflicts;
    Pef.Printer.pp_builddepformula_wl oc build_depends_indep;
    Pef.Printer.pp_builddeplist_wl oc build_conflicts_indep;
    Pef.Printer.pp_builddepformula_wl oc build_depends_arch;
    Pef.Printer.pp_builddeplist_wl oc build_conflicts_arch;
    Printf.fprintf oc "\n"

end

(* Relationships between source and binary packages
 * http://www.debian.org/doc/debian-policy/ch-relationships.html
 * Build-Depends, Build-Depends-Indep, Build-Conflicts, Build-Conflicts-Indep
*)
let parse_package_stanza filter buildarchlist par =
  let p () =
    let pkg = new source par in
    let matchsource sourcearchlist buildarchlist = 
      List.exists (fun arch ->
        List.exists(fun source ->
          Architecture.src_matches_arch source arch
        ) sourcearchlist
      ) buildarchlist
    in
    let sourcearchlist = pkg#architecture in
    if buildarchlist = [] then pkg
    else if matchsource sourcearchlist buildarchlist then pkg
    else raise (Pef.Packages.IgnorePackage "Source Architecture Mismatch")
  in
  begin try
    if Option.is_none filter then Some (p ())
    else if (Option.get filter) par then Some(p ())
    else None
  with
  |Pef.Packages.IgnorePackage s -> begin
    let n = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_name "Package" par in
    let v = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_version "Version" par in
    let a = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_string "Architecture" par in
    debug "Ignoring Source Package (%s,%s,%s) : %s" n v a s;
    None end
  |Format822.ParseError (cl,f,err) -> begin
      let n = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_name "Package" par in
      let v = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_version "Version" par in
      let a = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_string "Architecture" par in
      let c = Printf.sprintf "Parser Error in Source Package (%s,%s,%s)" n v a in
      raise (Format822.ParseError (c::cl,f,err) ) end
  end
;;

(** parse a debian Sources file from channel *)
let parse_sources_in ?filter ?(archs=[]) fname ic =
  info "Parsing Sources file %s..." fname;
  let stanza_parser = parse_package_stanza filter archs in
  Format822.parse_from_ch (Pef.Packages.packages_parser fname stanza_parser) ic
;;

(** parse a debian Sources file. 
 [~archs] determines which which architectures should be considered while
 parsing the Souces file. if ~arch is [] then all archs are cosidered *)
let input_raw ?filter ?(archs=[]) =
  let module Set = Set.Make(struct type t = source let compare = compare end) in
  let module M = Format822.RawInput(Set) in
  M.input_raw (parse_sources_in ?filter ~archs)
;;

let sep = ":" ;;

(* as per policy, if the first arch restriction contains a !
 * then we assume that all archs on the lists are bang-ed.
 * cf: http://www.debian.org/doc/debian-policy/ch-relationships.html 7.1 *)
let matcharch hostarch = function
  | [] -> true
  | ((true,_)::_) as al ->
    List.exists (fun (_,a) -> Architecture.src_matches_arch a hostarch) al
  | ((false,_)::_) as al ->
    List.for_all (fun (_,a) -> not(Architecture.src_matches_arch a hostarch)) al
;;

(* the nested build profiles formula is given in disjunctive normal form *)
let matchprofile profiles = function
  | [] -> true
  | ll -> List.exists (List.for_all (fun (c,p) -> c = (List.mem p profiles))) ll
;;

(* given archs, profile and a dependency with an architecture and profile list,
 * decide whether to select or drop that dependency *)
let select hostarch profiles (v,al,pl) =
  if matcharch hostarch al && matchprofile profiles pl then Some v else None
;;

(* XXX src2pkg could be skip using the same trick we use in opam, 
 * where dependencies are giltered at parsing time *)
(* the package name is encodes as src:<package-name> *)
let src2pkg ?(dropalternatives=false) ?(profiles=[]) ?(noindep=false) ?(src="src") buildarch hostarch srcpkg =
  let conflicts l = List.filter_map (select hostarch profiles) l in
  (* imitate sbuild behaviour and drop all alternatives except those that have
   * the same name as the first. Search for RESOLVE_ALTERNATIVES in
   * lib/Sbuild/ResolverBase.pm in the sbuild sources *)
  let dropalt l = match l with
    | [] -> []
    | [p] -> [p]
    | hd::tl -> List.filter (fun p -> fst p = fst hd) l
  in
  let depends ll =
    List.filter_map (fun l ->
        match List.filter_map (select hostarch profiles) l with
        |[] -> None
        |l -> Some (if dropalternatives then dropalt l else l)
      ) ll
  in
  let extras_profiles  = match profiles with [] -> [] | _ -> [("profiles", String.join " " profiles)] in
  let depends_indep   = if noindep then [] else srcpkg#build_depends_indep in
  let conflicts_indep = if noindep then [] else srcpkg#build_conflicts_indep in
  (* when crossbuilding (host != build), implicitly depend on build-essential
   * and crossbuild-essential-$hostarch. When compiling natively, implicitly
   * depend on build-essential *)
  let build_essential = if buildarch<>hostarch then
      [[(("build-essential", Some buildarch), None)];[(("crossbuild-essential-"^hostarch, Some buildarch), None)]]
    else
      [[(("build-essential", Some buildarch), None)]]
  in
  new Packages.package ~name:("",Some(src ^ sep ^ srcpkg#name)) ~version:("",Some(srcpkg#version))
  ~architecture:("",Some(String.concat "," srcpkg#architecture))
  ~source:("",Some (srcpkg#name, Some srcpkg#version)) 
  ~depends:("",Some (build_essential @ (depends (depends_indep @ srcpkg#build_depends @ srcpkg#build_depends_arch))))
  ~conflicts:("",Some (conflicts (conflicts_indep @ srcpkg#build_conflicts @ srcpkg#build_conflicts_arch)))
  ~extras:([],Some(extras_profiles @ [("Type",src)])) []
;;

(* given archs, profile and a dependency with an architecture and profile list,
 * decide whether to select or drop that dependency *)
(** transform a list of sources packages into dummy binary packages.
  * This function preserve the order *)
let sources2packages ?(dropalternatives=false) ?(profiles=[]) ?(noindep=false) ?(src="src") buildarch hostarch =
  List.map (src2pkg ~dropalternatives ~profiles ~noindep ~src buildarch hostarch)
;;

(** Check if a package is of "Type" source as encoded by the function sources2packages *)
let is_source ?(src="src") pkg = List.mem ("Type", src) pkg#extras;;

exception MismatchSrc of Cudf.package list
exception NotfoundSrc

(**
 [get_src_package universe binpkg] returns the source package associate
 with the given binary package.
 
 precondition : the package has "type" bin and the universe contains
 packages of "type" src encoded with sources2packages.
 
 Raise MismatchSrc if there exists a source package with the same name
 but with a different version . Raise NotfoundSrc if the univese does not
 contain either a source package associated with the binary package or
 a source package with the same name but different version.
*)
let get_src_package universe binpkg =
  let sn = CudfAdd.encode ("src:"^(CudfAdd.get_property "source" binpkg)) in
  let sv = int_of_string (CudfAdd.get_property "sourceversion" binpkg) in
  try Cudf.lookup_package universe (sn,sv)
  with Not_found -> begin
    let name = CudfAdd.decode sn in
    let number = CudfAdd.get_property "sourcenumber" binpkg in
    match Cudf.lookup_packages universe sn with
    |[] -> begin
        debug "Cannot find source package %s %s associated to the binary package %s"
        name number (CudfAdd.string_of_package binpkg);
        raise NotfoundSrc
    end
    |othersl -> begin
      let othersrcnumbers = 
        List.map (fun othersrc ->
          CudfAdd.get_property "number" othersrc
        ) othersl
      in
      debug "Cannot find source package %s %s associated to the binary package %s"
      name number (CudfAdd.string_of_package binpkg);
      debug "There exist other versions (%s) of the source package %s in the repository"
      (String.concat " , " othersrcnumbers) name;
      raise (MismatchSrc othersl)
    end
  end
;;

(** Returns an hash table that associates source packages (encoded by the
    function sources2packages) to binary packages in the universe. It is
    possible that a source package is not associated with any binary
    packages.
*)
let srcbin_table universe =
  let h = CudfAdd.Cudf_hashtbl.create (Cudf.universe_size universe) in
  let aux binpkg = 
    if CudfAdd.get_property "type" binpkg = "bin" then begin
      try
        let srcpkg = get_src_package universe binpkg in
        try let l = CudfAdd.Cudf_hashtbl.find h srcpkg in l := binpkg::!l
        with Not_found -> CudfAdd.Cudf_hashtbl.add h srcpkg (ref [binpkg])
      with
      |NotfoundSrc -> () (* this binary is not associated to any src *)
      |MismatchSrc sl ->
          (* we add the src to the table, but we do not associate to any
             binary *)
        List.iter (fun srcpkg -> 
          if not(CudfAdd.Cudf_hashtbl.mem h srcpkg) then
            CudfAdd.Cudf_hashtbl.add h srcpkg (ref [])
        ) sl
    end
  in
  Cudf.iter_packages aux universe ;
  h
;;

(** Returns the list of binary packages associated to a package of "Type" source
    encoded by the function sources2packages. The table h associated each source
    with a list of binaries *)
let get_bin_packages h srcpkg =
  try !(CudfAdd.Cudf_hashtbl.find h srcpkg)
  with Not_found -> begin
    let sn = CudfAdd.decode srcpkg.Cudf.package in
    let sv = CudfAdd.get_property "number" srcpkg in
    debug "Source package %s %s not associated with any binary package" sn sv;
    raise Not_found
  end
;;

(** Returns the set of binary packages generated by the packages in srclist.
    The function get_bin_packages gets a source package and returns the set
    of packages associated to it. If the source package is not known, then
    it is ignored. *)
let binset get_bin_packages srclist =
  List.fold_left (fun acc srcpkg ->
    try CudfAdd.Cudf_set.union acc (CudfAdd.to_set (get_bin_packages srcpkg))
    with Not_found -> acc
  ) CudfAdd.Cudf_set.empty srclist
;;
