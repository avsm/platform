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
open ExtString
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

type tables = {
  units : (Packages.name, (int * (Packages.rel * string)) list) Hashtbl.t;
  files : ((string * string), string) Hashtbl.t;
  fileconflicts : ((string * string), (string * string))  Hashtbl.t;
}

let create n = {
  (* all real packages associated with all versions *)
  units = Hashtbl.create (2 * n);
  (* all files associated to a package that are mentioned as a conflict or depends *)
  files = Hashtbl.create (2 * n);
  fileconflicts = Hashtbl.create (10 * n);
}

let clear tables =
  Hashtbl.clear tables.units;
  Hashtbl.clear tables.files;
  Hashtbl.clear tables.fileconflicts;
;;

let init_tables pkglist =
  let tables = create (List.length pkglist) in

  (* temp_units is the list of all versions in provides or packages *)
  let temp_units = Hashtbl.create (List.length pkglist) in
  (* all avalaible files *)
  let temp_files = Hashtbl.create (10 * (List.length pkglist)) in

  let add_list t k = function
    |None -> ()
    |Some(`Eq,v) ->
      let l =
        try Hashtbl.find t k with Not_found ->
        (let l = ref [] in Hashtbl.add t k l; l)
      in
      l := (`Eq,v) :: !l
    |Some(sel,v) ->
      let l =
        try Hashtbl.find t k with Not_found ->
        (let l = ref [] in Hashtbl.add t k l; l)
      in
      warning "provide with disequality for package %s" k; 
      l := (sel,v) :: !l
  in

  let add_units (n,s) = add_list temp_units n s in

  let add_files filename =
    List.iter (fun (n,v) -> 
      Hashtbl.add tables.files (n,v) filename
    ) (Hashtbl.find_all temp_files filename)
  in

  List.iter (fun pkg ->
    add_units (pkg.Packages.name,Some(`Eq,pkg.Packages.version));
    List.iter add_units pkg.Packages.provides ;
    List.iter (fun file ->
      Hashtbl.add temp_files file (pkg.Packages.name,pkg.Packages.version)
    ) pkg.Packages.files
  ) pkglist
  ;

  List.iter (fun pkg ->
    List.iter (fun file ->
      List.iter (fun (n,v) ->
        let (pn,pv) = (pkg.Packages.name,pkg.Packages.version) in
        if (n,v) <> (pn,pv) then
          Hashtbl.add tables.fileconflicts (pn,pv) (n,v)
      ) (Hashtbl.find_all temp_files file)
    ) pkg.Packages.files ;

    List.iter (fun (name,sel) -> add_files name) pkg.Packages.conflicts ;
    List.iter (fun (name,sel) -> add_files name) pkg.Packages.depends 
  ) pkglist
  ;

  let initialid = 2 in
  let cmp (_,v1) (_,v2) = Version.compare v1 v2 in
  let order l = List.unique (List.sort ~cmp:cmp l) in
  Hashtbl.iter (fun name {contents = l1} ->
    let vl = order l1 in
    let (_,m) =
      List.fold_left (fun (i,acc) k ->
        (i+1,(i,k)::acc)
      ) (initialid,[]) vl
    in
    Hashtbl.add tables.units name m ;
  ) temp_units;

  Hashtbl.clear temp_units;
  Hashtbl.clear temp_files;

  tables
;;

(* rpmdsCompare, rpmds.c *)
(* The order here is important: the comparison is not symmetric! *)
(* return true if the first constraint overlap the second, false otherwise *)
let compare_constr (r1,v1) (r2,v2) = 
  match r1, r2 with
  |`ALL, _ | _, `ALL |(`Lt | `Leq), (`Lt | `Leq) | (`Gt | `Geq), (`Gt | `Geq) -> true
  |(`Eq | `Geq | `Gt), `Lt | `Gt, (`Eq | `Leq) -> Version.compare v1 v2 < 0
  |`Lt, (`Eq | `Geq | `Gt) | (`Eq | `Leq), `Gt -> Version.compare v1 v2 > 0
  |`Eq, `Leq | `Geq, `Leq | `Geq, `Eq -> Version.compare v1 v2 <= 0
  |`Leq, `Eq | `Leq, `Geq | `Eq, `Geq -> Version.compare v1 v2 >= 0
  |`Eq, `Eq -> Version.compare v1 v2 = 0

let get_cudf_version tables (n,v) =
  try
    let l = Hashtbl.find tables.units n in
    fst(List.find (fun (i,(_,v1)) -> v = v1) l)
  with Not_found -> 1

let expand tables l =
  let aux tables (n,s) =
    try
      let l = Hashtbl.find tables.units n in
      List.filter_map (fun (i,s1) -> if compare_constr s1 s then Some i else None) l
    with Not_found -> [1]
  in
  List.flatten (
    List.map (function
      |(name,None) -> [CudfAdd.encode name,None]
      |(name,Some sel) ->
        match
          List.map (fun v ->
            (CudfAdd.encode name,Some(`Eq,v))
          ) (aux tables (name,sel))
        with [] -> [CudfAdd.encode name,Some(`Eq,1)] |l -> l
    ) l
  )

let load_provides (nm,vr) = expand ;;
let load_conflicts = expand ;;
let load_depends tables = List.map (expand tables) ;;

let load_filesprovides tables (name,version) =
  List.unique (
    List.map (fun n -> (CudfAdd.encode n,None))
    (Hashtbl.find_all tables.files (name,version))
  )

let load_fileconflicts tables (name,version) = 
  List.map (fun (n,v) -> (CudfAdd.encode n,Some(`Eq,get_cudf_version tables (n,v)))
  ) (Hashtbl.find_all tables.fileconflicts (name,version))

(* ========================================= *)

type extramap = (string * (string * Cudf_types.typedecl1)) list

let preamble =
  (* number is a mandatory property -- no default *)
  let l = [ ("number",(`String None)) ] in
  CudfAdd.add_properties Cudf.default_preamble l

let add_extra extras tables pkg =
  let number = ("number",`String pkg.Packages.version) in
  number :: extras

let add_keep pkg =
  try
    if List.assoc "essential" pkg.Packages.extras = "true" then `Keep_package
    else `Keep_none
  with Not_found -> `Keep_none

let tocudf tables ?(extras=[]) ?(inst=false) pkg =
  let (n,v) = (pkg.Packages.name,pkg.Packages.version) in
  (* we remove dependencies on files provided by the same package, dependencies
   * on the package itself and dependencies on packages provided by the same
   * package *)
  let depends = 
    let _deps = pkg.Packages.obsoletes @ pkg.Packages.provides in
    List.filter_map (fun l ->
      match List.filter (fun (n,s) ->
        not(n = pkg.Packages.name) &&
        not (List.exists(fun (x,_) -> x = n ) _deps) && 
        not(List.mem n (
          Hashtbl.find_all tables.files (pkg.Packages.name,pkg.Packages.version))
        )
      ) l with
      |[] -> None 
      |l -> Some l
    ) (List.map (fun d -> [d]) pkg.Packages.depends)
  in
  let name = CudfAdd.encode pkg.Packages.name in
  let version = get_cudf_version tables (n,v) in
  { Cudf.default_package with
    Cudf.package = name ;
    Cudf.version = version ;
    Cudf.keep = add_keep pkg;
    Cudf.depends = List.unique ( load_depends tables depends ) ;
    Cudf.conflicts = List.unique (
      (load_conflicts tables pkg.Packages.conflicts) (* @
      (load_fileconflicts tables (n,v)) *)
    );
    Cudf.provides = List.unique (
      (* (load_provides (n,v) tables pkg.Packages.obsoletes) @ *)
      (load_provides (n,v) tables pkg.Packages.provides) @
      (load_filesprovides tables (n,v)) 
    );
    Cudf.installed = inst;
    Cudf.pkg_extra = add_extra extras tables pkg ;
  }

let load_list l =
  let timer = Util.Timer.create "load_list" in
  Util.Timer.start timer;
  let tables =  init_tables l in
  let pkglist = List.map (tocudf tables) l in
  clear tables;
  Util.Timer.stop timer pkglist

let load_universe l =
  let pkglist = load_list l in
  let timer = Util.Timer.create "load_universe" in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ
