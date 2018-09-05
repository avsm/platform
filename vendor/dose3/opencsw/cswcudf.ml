
open ExtLib
open Common
module Version = Versioning.Debian

type tables = {
  units : (Packages.name, (int * string) list) Hashtbl.t;
  reverse : (int, string) Hashtbl.t;
}

let create n = {
  (* all real packages associated with all versions *)
  units = Hashtbl.create (2 * n);
  reverse = Hashtbl.create (2 * n);
}

let clear tables =
  Hashtbl.clear tables.units;
  Hashtbl.clear tables.reverse;
;;

let init_tables pkglist =
  let size = List.length pkglist in
  let tables = create size in
  let temp_units = Hashtbl.create (2 * size) in

  List.iter (fun pkg ->
    let (n,v) = (pkg.Packages.name, pkg.Packages.version) in
    CudfAdd.add_to_package_list temp_units n v
  ) pkglist
  ;

  let initialid = 2 in
  let cmp v1 v2 = Version.compare v1 v2 in
  let order l = List.unique (List.sort ~cmp:cmp l) in
  Hashtbl.iter (fun name {contents = l1} ->
    let vl = order l1 in
    let (_,m) =
      List.fold_left (fun (i,acc) k ->
        Hashtbl.add tables.reverse i k;
        (i+1,(i,k)::acc)
      ) (initialid,[]) vl
    in
    Hashtbl.add tables.units name m ;
  ) temp_units;

  tables
;;

let add_extra _ _ p = [] ;;

let get_cudf_version tables (n,v) =
  try
    let l = Hashtbl.find tables.units n in
    fst(List.find (fun (_,v1) -> v = v1) l)
  with Not_found -> 1

let get_real_version tables (n,i) = Hashtbl.find tables.reverse i 

let preamble = Cudf.default_preamble 

let tocudf tables ?(extras=[]) pkg =
  let (n,v) = (pkg.Packages.name,pkg.Packages.version) in
  let name = CudfAdd.encode pkg.Packages.name in
  let version = get_cudf_version tables (n,v) in
  { Cudf.default_package with
    Cudf.package = name ;
    Cudf.version = version ;
    Cudf.depends = List.map (fun n -> [(CudfAdd.encode n,None)]) pkg.Packages.depends ;
    Cudf.conflicts = List.map (fun n -> (CudfAdd.encode n,None)) pkg.Packages.conflicts ;
    Cudf.pkg_extra = add_extra extras tables pkg ;
  }
