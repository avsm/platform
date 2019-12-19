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

(** Strong Dependencies *)

(** [strongdeps u l] build the strong dependency graph of all packages in 
    [l] wrt the universe [u] *)
val strongdeps : ?transitive:bool ->
  Cudf.universe -> Cudf.package list -> Defaultgraphs.PackageGraph.G.t

(** [strongdeps_univ u] build the strong dependency graph of 
    all packages in the universe [u] *)
val strongdeps_univ : ?transitive:bool -> 
  Cudf.universe -> Defaultgraphs.PackageGraph.G.t

(** compute the impact set of the node [q], that is the list of all 
    packages [p] that strong depends on [q] *)
val impactset :
  Defaultgraphs.PackageGraph.G.t -> Cudf.package -> Cudf.package list

(** compute the conjunctive dependency graph *)
val conjdeps_univ : Cudf.universe -> Defaultgraphs.PackageGraph.G.t

(** compute the conjunctive dependency graph considering only packages 
    in [pkglist] *)
val conjdeps :
  Cudf.universe -> Cudf.package list -> Defaultgraphs.PackageGraph.G.t
