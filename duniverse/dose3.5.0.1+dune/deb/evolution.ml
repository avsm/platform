(**************************************************************************************)
(*  Copyright (C) 2011 Pietro Abate                                                   *)
(*  Copyright (C) 2011 Mancoosi Project                                               *)
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

type range = [
    `Hi of string
  | `In of (string * string)
  | `Lo of string 
  | `Eq of string
]

let string_of_range = function
  |`Hi v -> Printf.sprintf "%s < ." v
  |`Lo v -> Printf.sprintf ". < %s" v
  |`Eq v -> Printf.sprintf "= %s" v
  |`In (v1,v2) -> Printf.sprintf "%s < . < %s" v1 v2
;;

(* returns a list of ranges w.r.t. the list of versions vl *)
(* the range is a [ ... [ kind of interval *)
let range ?(bottom=false) vl =
  let l = List.sort ~cmp:(fun v1 v2 -> Version.compare v2 v1) vl in
  let rec aux acc = function
    |(None,[]) -> acc
    |(None,a::t) -> aux ((`Hi a)::acc) (Some a,t)
    |(Some b,a::t) -> aux ((`In (a,b))::(`Eq b)::acc) (Some a,t)
    |(Some b,[]) when bottom = false -> (`Eq b)::acc 
    |(Some b,[]) -> (`Lo b)::(`Eq b)::acc 
  in
  aux [] (None,l)
;;

(** [discriminants ?bottom ?ascending evalsel vl constraints]
   returns the discriminants of the versions [vl] w.r.t.
   the [constraints], using [evalsel] to determine whether a
   a version satisfy a constraint.
   For each discriminant, a canonical representative is given,
   as well as the list of all other equivalent versions.
   @param bottom set to true includes a version strictly smaller than all [vl] 
   @param highest chooses the highest version as representative, if set to true,
                      and the lowest otherwise.
 *)
let discriminant ?(bottom=false) ?(highest=true) evalsel vl constraints =
  let eval_constr = Hashtbl.create 17 in
  let constr_eval = Hashtbl.create 17 in
  let candidates = range ~bottom vl in
  List.iter (fun target ->
    let eval = List.map (evalsel target) constraints in
    try
      let v_rep = Hashtbl.find eval_constr eval in
      let l = Hashtbl.find constr_eval v_rep in
      Hashtbl.replace constr_eval v_rep (target::l)
    with Not_found -> begin
      Hashtbl.add eval_constr eval target;
      Hashtbl.add constr_eval target []
    end
  ) (if highest then List.rev candidates else candidates) ;
  (Hashtbl.fold (fun k v acc -> (k,v)::acc) constr_eval [])
;;

let add_unique h k v =
  try
    let vh = Hashtbl.find h k in
    if not (Hashtbl.mem vh v) then
      Hashtbl.add vh v ()
  with Not_found -> begin
    let vh = Hashtbl.create 17 in
    Hashtbl.add vh v ();
    Hashtbl.add h k vh
  end

(* collect dependency information *)
let conj_iter t l =
  List.iter (fun ((name,_),sel) ->
    match sel with
    |None -> add_unique t name None
    |Some(c,v) -> add_unique t name (Some(Pef.Pefcudf.pefcudf_op c,v))
  ) l
let cnf_iter t ll = List.iter (conj_iter t) ll

(** [constraints universe] returns a map between package names
    and an ordered list of constraints where the package name is
    mentioned *)
let constraints packagelist =
  let constraints_table = Hashtbl.create (List.length packagelist) in
  List.iter (fun pkg ->
    (* add_unique constraints_table pkg.Packages.name None; *)
    conj_iter constraints_table pkg#conflicts ;
    conj_iter constraints_table pkg#breaks ;
    conj_iter constraints_table pkg#provides ;
    cnf_iter constraints_table pkg#depends;
    cnf_iter constraints_table pkg#pre_depends
  ) packagelist
  ;
  let h = Hashtbl.create (List.length packagelist) in
  let elements hv =
    let cmp (_,v1) (_,v2) = Version.compare v2 v1 in
    List.sort ~cmp (
      Hashtbl.fold (fun k _ acc ->
        match k with
        |None -> acc 
        |Some k -> k::acc
      ) hv []
    )
  in
  Hashtbl.iter (fun n hv -> Hashtbl.add h n (elements hv)) constraints_table;
  h
;;

let all_constraints table pkgname =
  try (Hashtbl.find table pkgname)
  with Not_found -> []
;;

(* return a new target rebased accordingly to the epoch of the base version *)
let align version target =
  match Version.decompose version with
  |Version.NonNative("",_,_,_) 
  |Version.Native("",_,_) -> target
  |Version.Native(pe,_,_)
  |Version.NonNative(pe,_,_,_) ->
    let rebase v =
      match Version.decompose v with
      |Version.Native(e,u,b) -> Version.compose (Version.Native(pe,u,b))
      |Version.NonNative(_,u,r,b) -> Version.compose (Version.NonNative(pe,u,r,b))
    in
    match target with
    |`Eq v -> `Eq (rebase v)
    |`Hi v -> `Hi (rebase v)
    |`Lo v -> `Lo (rebase v)
    |`In (v,w) -> `In (rebase v,rebase w)
;;

(* all versions mentioned in a list of constraints *)
let all_versions constr = Util.list_unique (List.map (snd) constr) ;;

let migrate packagelist target =
  List.map (fun pkg -> ((pkg,target),(align pkg#version target))) packagelist
;;

let extract_epochs vl =
  Util.list_unique (
    List.fold_left (fun acc v ->
      (Version.extract_epoch v) :: acc
    ) [] vl
  )
;;

let add_normalize vl =
  List.fold_left (fun acc v ->
    match Version.decompose v with
    |Version.NonNative(_,u,r,b) ->
        let n1 = Version.compose (Version.NonNative("",u,r,"")) in
        let n2 = Version.compose (Version.NonNative("",u,r,b)) in
        n1::n2::v::acc
    |Version.Native(_,u,b) ->
        let n1 = Version.compose (Version.Native("",u,"")) in
        let n2 = Version.compose (Version.Native("",u,b)) in
        n1::n2::v::acc
  ) [] vl
;;

let add_epochs el vl =
  List.fold_left (fun acc1 e ->
    List.fold_left (fun acc2 v ->
      match Version.decompose v with
      |Version.Native("",u,b) ->
          let n = Version.compose (Version.Native(e,u,b)) in
          n::v::acc2
      |Version.NonNative("",u,r,b) ->
          let n = Version.compose (Version.NonNative(e,u,r,b)) in
          n::v::acc2
      | _ -> v::acc2
    ) acc1 vl
  ) [] el
;;

let all_ver_constr constraints_table cluster =
  let (versionlist, constr) =
    List.fold_left (fun (_vl,_cl) pkg ->
      let pn = pkg#name in
      let pv = pkg#version in
      let constr = all_constraints constraints_table pn in
      let vl = pv::(all_versions constr) in
      (vl @ _vl,constr @ _cl)
    ) ([],[]) cluster
  in
  let all_epochs = extract_epochs versionlist in
  let all_norm = add_normalize versionlist in
  let versionlist = add_epochs all_epochs all_norm in
  (Util.list_unique versionlist,Util.list_unique constr)
;;

