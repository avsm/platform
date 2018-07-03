
(** Return the source package associated to a binary package *)
val get_source : Packages.package -> ( Pef.Packages_types.name * Pef.Packages_types.version )

(** [cluster package list] returns a hashtbl that maps
    (source,sourceversion) -> to a packages list 

    the idea is : if the normalized version of the package is equal to the
    source version, then add it to the table indexed by source version,
    otherwise add it to the table indexed by package version actually it should
    be sourceversion -> list of list of clusters grouped by version
    (source,sourceversion) -> (version, realversion, package list)
*)
val cluster : Packages.package list -> 
  (Pef.Packages_types.name * Pef.Packages_types.version, 
		(Pef.Packages_types.version * Pef.Packages_types.version * Packages.package list) list) ExtLib.Hashtbl.t

