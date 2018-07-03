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
open Common
module Version = Versioning.Debian

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let get_source pkg =
  match pkg#source with
  |("",None) -> (pkg#name, pkg#version)
  |(n,None) -> (n, pkg#version)
  |(n,Some v) -> (n,v)

(** [group_by_source universe] returns a hashtbl that maps
    (source,sourceversion) -> to a packages list *)
(* the idea is : if the normalized version of the package is equal to
 * the source version, then add it to the table indexed by source version,
 * otherwise add it to the table indexed by package version *)
(* actually it should be sourceversion -> list of list of clusters grouped by
 * version *)
(* (source,sourceversion) -> [= packageversion -> (ref[pkg],realversion) =] *)
let cluster packagelist =
  let th = Hashtbl.create (List.length packagelist) in
  List.iter (fun pkg ->
    let packageversion = Version.compose (Version.strip_epoch_binnmu pkg#version) in
    let realversion = Version.compose (Version.strip_epoch pkg#version) in
    let (source, sourceversion) = get_source pkg in
    try
      let h = Hashtbl.find th (source,sourceversion) in
      try let (l,hi_v) = Hashtbl.find h packageversion in 
      l := pkg :: !l;
      let new_hi =
        if (Version.compare hi_v realversion) < 0
        then hi_v 
        else realversion
      in       
      (* keep the highest version of the cluster handy *)
      Hashtbl.replace h packageversion (l,new_hi)
      with Not_found -> 
        (* found the source, but not the package version *)
        Hashtbl.add h packageversion (ref[pkg],realversion)
    with Not_found -> begin 
      (* didn't found the source *)
      let h = Hashtbl.create 17 in
      Hashtbl.add h packageversion (ref[pkg],realversion);
      Hashtbl.add th (source,sourceversion) h
    end
  ) packagelist ;
  let h = Hashtbl.create (List.length packagelist) in
  let i = ref 0 in
  Hashtbl.iter (fun (s,v) thv ->
    let l = Hashtbl.fold (fun v ({contents=l},rv) acc -> (v,rv,l)::acc) thv [] in
    i := !i + (List.length l);
    Hashtbl.add h (s,v) l
  ) th;
  info "Packages: %d" (List.length packagelist);
  info "Source Clusters: %d" (Hashtbl.length h);
  info "Binary (effective) Clusters: %d" !i;
  h
;;
