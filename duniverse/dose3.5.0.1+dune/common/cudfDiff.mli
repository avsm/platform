
(** a tuple holding the packages to be installed and the packages to be removed *)
type changeset = (CudfAdd.Cudf_set.t * CudfAdd.Cudf_set.t)

(** associates to each package name a change_set containing for each package name
    the associated versions of this package to be installed and removed *)
type difference = (Cudf_types.pkgname,changeset) ExtLib.Hashtbl.t

(** return a changeset containing all the packages to be installed and/or removed *)
val make_solution : universe:Cudf.universe -> solution:Cudf.universe -> changeset

(** return the difference table where packages are indexed by package name *)
val make_difference : universe:Cudf.universe -> solution:Cudf.universe -> difference

(** The list of packages to install, remove, etc *)
type summary = {
  install : Cudf.package list;
  remove : Cudf.package list;
  upgrade : (Cudf.package * Cudf.package) list;
  downgrade : (Cudf.package * Cudf.package) list;
  notchange : Cudf.package list;
}

(* Given a cudf universe and difference set returns a global summary of the packages
   to be installed and/or removed *)
val make_summary : Cudf.universe -> difference -> summary
