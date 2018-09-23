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

module OcamlHash = Hashtbl ;;
open ExtLib ;;
open Common ;;

#define __label __FILE__
let _label =  __label ;;
include Util.Logging(struct let label = _label end) ;;

type reason_int =
  |DependencyInt of (int * Cudf_types.vpkg list * int list)
  |MissingInt of (int * Cudf_types.vpkg list)
  |ConflictInt of (int * int * Cudf_types.vpkg)

type result_int =
  |SuccessInt of (?all:bool -> unit -> int list)
  |FailureInt of (unit -> reason_int list)

type request_int = int list

(** One un-installability reason for a package *)
type reason =
  |Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list) 
  (** Not strictly a un-installability, Dependency (a,vpkglist,pkglist) is used
      to recontruct the the dependency path from the root package to the
      offending un-installable package *)
  |Missing of (Cudf.package * Cudf_types.vpkg list) 
  (** Missing (a,vpkglist) means that the dependency
      [vpkglist] of package [a] cannot be satisfied *)
  |Conflict of (Cudf.package * Cudf.package * Cudf_types.vpkg) 
  (** Conflict (a,b,vpkg) means that the package [a] is in conflict
      with package [b] because of vpkg *)

(** The request provided to the solver.
    Check the installability of one package or the
    coinstallability of a list of packages *)
type request = Cudf.package list

(** The result of an installability query *)
type result =
  |Success of (?all:bool -> unit -> Cudf.package list) 
  (** If successfull returns a function that will
      return the installation set for the given query. Since
      not all packages are tested for installability directly, the
      installation set might be empty. In this case, the solver can
      be called again to provide the real installation set 
      using the parameter [~all:true] *)
  |Failure of (unit -> reason list) 
  (** If unsuccessful returns a function containing the list of reason *)

type diagnosis = { result : result ; request : request }

let reason map universe =
  let from_sat = CudfAdd.inttopkg universe in
  let globalid = map#vartoint (Cudf.universe_size universe) in
  List.filter_map (function
    |DependencyInt(i,vl,il) when i = globalid -> None
    |MissingInt(i,vl) when i = globalid ->
        fatal "the package encoding global constraints can't be missing (uid %d)" i
    |ConflictInt(i,j,vpkg) when i = globalid || j = globalid ->
        fatal "the package encoding global constraints can't be in conflict (uid %d - %d)" i j

    |DependencyInt(i,vl,il) -> Some (
        Dependency(from_sat (map#inttovar i),vl,List.map (fun i -> from_sat (map#inttovar i)) il)
    )
    |MissingInt(i,vl) -> Some (
        Missing(from_sat (map#inttovar i),vl)
    )
    |ConflictInt(i,j,vpkg) -> Some (
        Conflict(from_sat (map#inttovar i),from_sat (map#inttovar j),vpkg)
    )
  )

let result map universe result =
  let from_sat = CudfAdd.inttopkg universe in
  let globalid = map#vartoint (Cudf.universe_size universe) in
  match result with
  |SuccessInt f_int ->
      Success (fun ?(all=false) () ->
        List.filter_map (function
          |i when i = globalid -> None
          |i -> Some ({(from_sat (map#inttovar i)) with Cudf.installed = true})
        ) (f_int ~all ())
      )
  |FailureInt f -> Failure (fun () ->
      reason map universe (f ()))
;;

let request universe result =
  List.map (CudfAdd.inttopkg universe) result
;;

(* XXX here the threatment of result and request is not uniform.
 * On one hand indexes in result must be processed with map#inttovar 
 * as they represent indexes associated with the solver.
 * On the other hand the indexes in result represent cudf uid and
 * therefore do not need to be processed.
 * Ideally the compiler should make sure that we use the correct indexes
 * but we should annotate everything making packing/unpackaing handling
 * a bit too heavy *)
let diagnosis map universe res req =
  let result = result map universe res in
  let request = request universe req in
  { result = result ; request = request }

module ResultHash = OcamlHash.Make (
  struct
    type t = reason

    let equal v w = match (v,w) with
    |Missing (_,v1),Missing (_,v2) -> v1 = v2
    |Conflict(i1,j1,_),Conflict (i2,j2,_) -> i1 = i2 && j1 = j2
    |_ -> false

    let hash = function
      |Missing (_,vpkgs) -> OcamlHash.hash vpkgs
      |Conflict (i,j,_) -> OcamlHash.hash (i,j)
      |_ -> assert false
  end
)

type summary = {
  mutable missing : int;
  mutable conflict : int;
  mutable unique_missing : int;
  mutable unique_conflict : int;
  mutable unique_selfconflict : int;
  summary : (Cudf.package list ref) ResultHash.t;
  statistic : ((int * int),int ref) Hashtbl.t
}

let default_result n = {
  missing = 0;
  conflict = 0;
  unique_missing = 0;
  unique_conflict = 0;
  unique_selfconflict = 0;
  summary = ResultHash.create n;
  statistic = Hashtbl.create 17
}

let pp_out_version fmt = Format.fprintf fmt "output-version: 1.2@.";;

let pp_package ?(source=false) ?(fields=false) pp fmt pkg =
  let (p,a,v,fieldlist) = pp pkg in
  Format.fprintf fmt "package: %s@," p;
  Format.fprintf fmt "version: %s" v;
  List.iter (function
    |(("source"|"sourcenumber"|"type"|"essential"),_) -> ()
    |(k,(v,true)) -> Format.fprintf fmt "@,%s: %s" k v
    |(k,(v,false)) when fields = true -> Format.fprintf fmt "@,%s: %s" k v
    |(k,(v,_)) -> ()
  ) fieldlist;
  try
    if fst(List.assoc "essential" fieldlist) = "true" then
      Format.fprintf fmt "@,essential: true"
  with Not_found -> ();
  try
    if fst(List.assoc "type" fieldlist) = "src" then
      Format.fprintf fmt "@,type: src"
  with Not_found -> ();
  if source then
    try
      let source = fst(List.assoc "source" fieldlist) in
      let sourceversion = 
        try "(= "^(fst(List.assoc "sourcenumber" fieldlist))^")" 
        with Not_found -> ""
      in begin
        Format.fprintf fmt "@,source: %s %s" source sourceversion;
      end;
    with Not_found -> ()
;;

let pp_dependency pp ?(label="depends") fmt (i,vpkgs) =
  Format.fprintf fmt "%a" (pp_package pp) i;
  if vpkgs <> [] then
    Format.fprintf fmt "@,%s: %a" label (CudfAdd.pp_vpkglist pp) vpkgs;
;;

let rec pp_list pp fmt = function
  |[h] -> Format.fprintf fmt "@[<v 1>-@,%a@]" pp h
  |h::t ->
      (Format.fprintf fmt "@[<v 1>-@,%a@]@," pp h ;
      pp_list pp fmt t)
  |[] -> ()
;;

let rec pp_collection pp fmt = function
  |[h] -> Format.fprintf fmt "@[<v 1>%a@]" pp h
  |h::t ->
      (Format.fprintf fmt "@[<v 1>%a@]@," pp h ;
      pp_collection pp fmt t)
  |[] -> ()
;;

(** Build a SyntacticDependencyGraph from the solver output. *)
let build_explanation_graph ?(addmissing=false) root l =
  let open Defaultgraphs.SyntacticDependencyGraph in
  let add_node value =
    G.V.create (PkgV.Pkg {
      value;
      root = (CudfAdd.equal value root)
      }
    )
  in
  let gr = G.create () in
  (* we add the root, there might be a problem on related to packages
     not related to the root, but failing to install because of a 
     global constraint (like conflitting essential packages) *)
  G.add_vertex gr (add_node root);
  let c = ref 0 in
  (* remove duplicate dependencies/reasons. XXX with long
     list of packages Hashtbl.hash could give wrong results *)
  let dup_reasons_table = Hashtbl.create 10 in
  let dup_or_table = Hashtbl.create 10 in
  List.iter (function
    |e when Hashtbl.mem dup_reasons_table e -> ()
    |e -> begin
      Hashtbl.add dup_reasons_table e ();
      match e with
      (* a dependency is direct only if there is only 
         one vpkg and only one package *)
      |Dependency(pkg,[vpkg],[p]) ->
          let vpid = add_node pkg in
          let vp = add_node p in
          add_edge gr vpid (PkgE.DirDepends [vpkg]) vp
      |Dependency(pkg,vpkgs,l) ->
          let vpid = add_node pkg in
          let vor =
            try Hashtbl.find dup_or_table (pkg.Cudf.package,vpkgs)
            with Not_found -> begin
              let vor = G.V.create (PkgV.Or !c) in incr c;
              Hashtbl.add dup_or_table (pkg.Cudf.package,vpkgs) vor;
              vor
            end
          in
          add_edge gr vpid (PkgE.OrDepends vpkgs) vor;
          List.iter (fun p ->
            let vp = add_node p in
            add_edge gr vor (PkgE.OrDepends vpkgs) vp
          ) l;
          if addmissing then begin
            let s =
              List.fold_left (fun acc p ->
                CudfAdd.StringSet.add p.Cudf.package acc
              ) CudfAdd.StringSet.empty l
            in
            let missingvpkgs =
              List.fold_left (fun acc (n,c) ->
                if not(CudfAdd.StringSet.mem n s) then ((n,c)::acc) else acc
              ) [] vpkgs
            in
            (* we add this node if a package depends disjuctively on one
               or more packages that exists in the repository, but are not
               installable, and one that do not exists in the repository. For
               the latter we add a missing node to the graph. *)
            if List.length missingvpkgs > 0 then begin
              let vp = G.V.create (PkgV.Missing missingvpkgs) in incr c;
              add_edge gr vor (PkgE.MissingDepends missingvpkgs) vp
            end
          end
      |Missing(pkg,vpkgs) ->
          let vpid = add_node pkg in
          let vp = G.V.create (PkgV.Missing vpkgs) in
          add_edge gr vpid (PkgE.MissingDepends vpkgs) vp
      |Conflict(pkg_i,pkg_j,vpkg) ->
          if not (CudfAdd.equal pkg_i pkg_j) then begin
            let vi = add_node pkg_i in
            let vj = add_node pkg_j in
            incr c;
            (* if (G.V.compare vi vj) > 0 then *)
              add_edge gr vi (PkgE.Conflict vpkg) vj
(* 
            else 
              add_edge gr vj PkgE.Conflict vi
              *)
          end
      end
  ) l;
  gr
;;

(** condense nodes in the graph *)
let cmp_ne x y =
  let open Defaultgraphs.SyntacticDependencyGraph in
  let rec cmplist v1 v2 =
    match v1, v2 with
    |[],[] -> 0
    |[],_ -> 1
    |_,[] -> -1
    |x::xs,y::ys ->
        let c = G.V.compare x y in
        if c = 0 then cmplist xs ys
        else c
  in
  let cmp (n1,sl1,pl1) (n2,sl2,pl2) =
    let c = Pervasives.compare n1 n2 in
    if c = 0 then
      let c1 = cmplist sl1 sl2 in
      if c1 = 0 then cmplist pl1 pl2
      else c1
    else c
  in
  cmp x y

let groupby cmp filter l =
  List.fold_left (fun map v ->
    let k = filter v in
    let pl =
      try PMap.find k map
      with Not_found -> [v] 
    in
    PMap.add k (v :: pl) map
  ) (PMap.create cmp) l

let name_and_edges gr v =
  let open Defaultgraphs.SyntacticDependencyGraph in
  let sl = List.sort ~cmp:G.V.compare (G.succ gr v) in
  let pl = List.sort ~cmp:G.V.compare (G.pred gr v) in
  let n =
    match v with
    |PkgV.Pkg { value = p } -> p.Cudf.package
    |PkgV.Or i -> Printf.sprintf "Or%d" i
    |PkgV.Missing _ -> "Missing"
    |PkgV.Set _ -> assert false
  in
  (n,sl,pl)

let in_conflict gr x y =
  let open Defaultgraphs.SyntacticDependencyGraph in
  try
    let e = 
      if G.V.compare x y > 0 then
        G.find_edge gr x y
      else
        G.find_edge gr y x
    in
    match (!(G.E.label e)) with
    |PkgE.Conflict _ -> true
    |_ -> false
  with Not_found -> false

let condense_graph gr =
  let open Defaultgraphs.SyntacticDependencyGraph in
  let module Visit = Graph.Traverse.Dfs(G) in
  let h = Hashtbl.create 17 in
  let getlist =
    List.filter_map (function
      |PkgV.Pkg { value = p ; root = false } as v -> Some( p, v ) 
      |_ -> None
    )
  in
  Visit.postfix (function
    |((PkgV.Missing _)|(PkgV.Or _)) as v  ->
      PMap.iter (fun (name,sl,pl) l ->
        match getlist l with
        |[] -> ()
        |(_,hd)::[] -> Hashtbl.add h hd (List.hd l,sl,pl)
        |pkgl -> 
          let vpid = G.V.create (PkgV.Set (CudfAdd.to_set (List.map fst pkgl))) in
          List.iter (fun (_,v) -> Hashtbl.add h v (vpid,sl,pl)) pkgl
      ) (groupby cmp_ne (name_and_edges gr) (G.pred gr v));
      (* a missing does not have any succ, so it's safe to assume the second
       * loop interests only Or nodes *)
      PMap.iter (fun (name,sl,pl) l ->
        match getlist l with
        |[] -> ()
        |(_,hd)::[] -> Hashtbl.add h hd (List.hd l,sl,pl)
        |pkgl -> 
          if List.for_all (fun (_,src) ->
            (List.for_all (in_conflict gr src) sl) ||
            (List.for_all (in_conflict gr src) pl)
          ) pkgl then begin
            let vpid = G.V.create (PkgV.Set (CudfAdd.to_set (List.map fst pkgl))) in
            List.iter (fun (_,v) -> Hashtbl.add h v (vpid,sl,pl)) pkgl
          end
      ) (groupby cmp_ne (name_and_edges gr) (G.succ gr v))
    |_ -> ()
  ) gr;
  let e_to_remove = ref [] in
  G.iter_vertex (fun v ->
    try
      let (vpid, sl, pl) = Hashtbl.find h v in
      List.iter (fun dst ->
        let e = G.find_edge gr v dst in
        let dst = 
          try let (c,_,_) = Hashtbl.find h dst in c
          with Not_found -> dst
        in
        add_edge gr vpid (!(G.E.label e)) dst;
        e_to_remove := e::!e_to_remove;
      ) sl;
      List.iter (fun src ->
        let e = G.find_edge gr src v in
        let src = 
          try let (c,_,_) = Hashtbl.find h src in c
          with Not_found -> src
        in
        add_edge gr src (!(G.E.label e)) vpid;
        e_to_remove := e::!e_to_remove;
      ) pl;
    with Not_found -> ()
  ) gr;
  List.iter (G.remove_edge_e gr) !e_to_remove;
  Hashtbl.iter (fun k _ -> G.remove_vertex gr k) h;
  gr
;;

let print_dot ?(pp=CudfAdd.default_pp) ?(condense=false) ?(addmissing=false) ?dir = 
  Defaultgraphs.SyntacticDependencyGraph.default_pp := pp;
  let open Defaultgraphs.SyntacticDependencyGraph in function
  |{result = Success _ } -> fatal "Cannot build explanation graph on Success"
  |{result = Failure f; request = [r] } ->
      let fmt =
        let n = Printf.sprintf "%s.%s.dot"
          (CudfAdd.decode r.Cudf.package)
          (CudfAdd.string_of_version r)
        in
        let f =
          if Option.is_none dir then n
          else Filename.concat (Option.get dir) n
        in
        let oc = open_out f in
        Format.formatter_of_out_channel oc;
      in
      let gr =
        let g = build_explanation_graph ~addmissing r (f ()) in
        if condense then condense_graph g else g
      in
      DotPrinter.print fmt gr
  |_ ->
    warning "Tryin to build explanation graph for a Coinst (not implemented yet)"
;;

let print_error ?(condense=false) ?(minimal=false) pp root fmt l =
  let module DG = Defaultgraphs.SyntacticDependencyGraph in
  let get_package v = 
    match v with
    |DG.PkgV.Pkg { DG.value = p } -> [p]
    |DG.PkgV.Set s -> CudfAdd.Cudf_set.elements s
    |_ -> raise Not_found
  in
  let pp_package_list ?(source=false) pp fmt pkgl =
    if List.length pkgl = 1 then 
      pp_package ~source pp fmt (List.hd pkgl) 
    else begin
      let fl = List.map pp pkgl in
      let (n,_,_,_) = List.hd fl in
      Format.fprintf fmt "package: %s@," n;
      Format.fprintf fmt "versions: %s" (String.concat "," (List.map (fun (_,_,v,_) -> v) fl))
    end
  in
  let pp_dependency_list pp ?(label="depends") fmt (i,vpkgs) =
    Format.fprintf fmt "%a" (pp_package_list pp) i;
    if vpkgs <> [] then
      Format.fprintf fmt "@,%s: %a" label (CudfAdd.pp_vpkglist pp) vpkgs;
  in
  let pp_dependencies pp fmt pl = 
    let pp_dependency pp fmt ((src,label,_) : DG.G.E.t) =
      try
        let l = 
          match src with
          |DG.PkgV.Pkg { DG.value = p } -> [p]
          |DG.PkgV.Set l -> (CudfAdd.Cudf_set.elements l)
          |_ -> raise Not_found
        in
        let vpkgs = 
          match !label with
          |DG.PkgE.OrDepends vpkgs
          |DG.PkgE.DirDepends vpkgs
          |DG.PkgE.MissingDepends vpkgs -> vpkgs
          |_ -> []
        in
        if List.length l > 0 then begin
          Format.fprintf fmt "%a" (pp_package_list pp) l;
          if vpkgs <> [] then
            Format.fprintf fmt "@,depends: %a" (CudfAdd.pp_vpkglist pp) (List.unique vpkgs);
        end
      with Not_found -> ()
    in
    let filter (src,_,_) = match src with (DG.PkgV.Pkg _ |DG.PkgV.Set _) -> true | _ -> false in
    let rec aux fmt = function
      |[path] -> Format.fprintf fmt "@[<v 1>-@,@[<v 1>depchain:@,%a@]@]" (pp_list (pp_dependency pp)) path
      |path::pathlist ->
          (Format.fprintf fmt "@[<v 1>-@,@[<v 1>depchain:@,%a@]@]@," (pp_list (pp_dependency pp)) path;
          aux fmt pathlist)
      |[] -> ()
    in
    aux fmt [(List.filter filter pl)]
  in
  let module DJ = Graph.Path.Dijkstra(Defaultgraphs.SyntacticDependencyGraph.G)(
    struct
      open Defaultgraphs.SyntacticDependencyGraph
      type label = G.E.label
      type t = int
      type edge = G.E.t
      let weight e = match G.E.label e with { contents = PkgE.Conflict _ } -> 1000 | _ -> 0
      let compare = Pervasives.compare
      let add = (+)
      let zero = 0
    end) 
  in
  let gr =
    let g = build_explanation_graph ~addmissing:false root l  in
    if condense then condense_graph g else g
  in
  let vroot = DG.G.V.create (DG.PkgV.Pkg { value = root ; DG.root = true }) in
  let pp_reason_conflicts fmt (vi,vj,vpkg) =
    let (i,j) = get_package vi, get_package vj in 
    Format.fprintf fmt "@[<v 1>conflict:@,";
    Format.fprintf fmt "@[<v 1>pkg1:@,%a@," (pp_package_list ~source:true pp) i;
    Format.fprintf fmt "unsat-conflict: %a@]@," (CudfAdd.pp_vpkglist pp) [vpkg];
    Format.fprintf fmt "@[<v 1>pkg2:@,%a@]" (pp_package_list ~source:true pp) j;
    if not minimal then begin
      let pl1 = try fst(DJ.shortest_path gr vroot vi) with Not_found -> [] in
      let pl2 = try fst(DJ.shortest_path gr vroot vj) with Not_found -> [] in
      if pl1 <> [] then
        Format.fprintf fmt "@,@[<v 1>depchain1:@,%a@]" (pp_dependencies pp) pl1;
      if pl2 <> [] then
        Format.fprintf fmt "@,@[<v 1>depchain2:@,%a@]" (pp_dependencies pp) pl2;
      Format.fprintf fmt "@]"
    end else
      Format.fprintf fmt "@,@]"
  in
  let pp_reason_missing fmt (vi,vpkgs) =
    let i = try get_package vi with Not_found -> assert false in
    Format.fprintf fmt "@[<v 1>missing:@,";
    Format.fprintf fmt "@[<v 1>pkg:@,%a@]" 
      (pp_dependency_list ~label:"unsat-dependency" pp) (i,List.unique vpkgs);
    if not minimal then begin
      let (pl,_) = DJ.shortest_path gr vroot vi in
      if pl <> [] then begin
        Format.fprintf fmt "@,@[<v 1>depchains:@,%a@]" (pp_dependencies pp) pl;
        Format.fprintf fmt "@]"
      end else
        Format.fprintf fmt "@]"
    end else
      Format.fprintf fmt "@]"
  in
  (* here I'm realying on the order induced by iter_edges_e to
     list the reasons. This is undertermined and can change arbitrarly *)
  let conflicts = ref [] in
  let missing =  ref [] in
  Defaultgraphs.SyntacticDependencyGraph.G.iter_edges_e (fun (src,label,dst) ->
    match !label with
    |DG.PkgE.Conflict vpkg -> conflicts := (src,dst,vpkg)::!conflicts
    |DG.PkgE.MissingDepends vpkgs -> missing := (src,vpkgs)::!missing
    |_ -> ()
  ) gr;
  pp_list pp_reason_conflicts fmt !conflicts;
  if List.length !conflicts > 0 && List.length !missing > 0 then Format.fprintf fmt "@,";
  pp_list pp_reason_missing fmt !missing
;;

(* XXX unplug your imperative brain and rewrite this as a tail recoursive
 * function ! *)
let minimize roots l =
  let module H = Hashtbl in
  let h = H.create (List.length l) in
  List.iter (fun p -> H.add h p.Cudf.package p) l;
  let acc = H.create 1023 in
  let rec visit pkg =
    if not (H.mem acc pkg) then begin
      H.add acc pkg ();
      List.iter (fun vpkgformula ->
        List.iter (fun (name,constr) ->
          try
            let p = H.find h name in
            if Cudf.version_matches p.Cudf.version constr then
              visit p
          with Not_found -> ()
        ) vpkgformula
      ) pkg.Cudf.depends
    end
  in
  begin match roots with 
  |[r] -> visit r
  |rl -> List.iter visit l end;
  H.fold (fun k _ l -> k::l) acc []
;;

let get_installationset ?(minimal=false) = function
  |{result = Success f ; request = req} -> 
     let s = f ~all:true () in
     if minimal then minimize req s else s
  |{result = Failure _ } -> raise Not_found
;;

let is_solution = function
  |{result = Success _ } -> true
  |{result = Failure _ } -> false
;;

let fprintf ?(pp=CudfAdd.default_pp) ?(failure=false) ?(success=false) ?(explain=false) 
  ?(minimal=false) ?(condense=false) fmt d = 
  match d with
  |{result = Success f; request = req } when success ->
      Format.fprintf fmt "@[<v 1>-@,";
      begin match req with
      |[] -> Format.fprintf fmt "@[<v>consistent@]@,"
      |[r] -> 
          Format.fprintf fmt "@[<v>%a@]@," (pp_package ~source:true ~fields:true pp) r;
          if minimal then
            Format.fprintf fmt "success: %a@," CudfAdd.pp_package r
      |rl -> 
          Format.fprintf fmt "coinst: %s@," 
          (String.concat " , " (List.map CudfAdd.string_of_package rl));
          if minimal then
            Format.fprintf fmt "success: %s@,"
            (String.concat " , " (List.map CudfAdd.string_of_package rl))
      end;
      Format.fprintf fmt "status: ok@,";
      if explain then begin
       let is = get_installationset ~minimal d in
       if is <> [] then begin
         Format.fprintf fmt "@[<v 1>installationset:@," ;
         Format.fprintf fmt "@[<v>%a@]" (pp_list (pp_package pp)) is;
         Format.fprintf fmt "@]"
       end
      end;
      Format.fprintf fmt "@]@,"
  |{result = Failure f; request = [r] } when failure ->
      Format.fprintf fmt "@[<v 1>-@,";
      Format.fprintf fmt "@[<v>%a@]@," (pp_package ~source:true ~fields:true pp) r;
      if minimal then
        Format.fprintf fmt "failure: %a@," CudfAdd.pp_package r;
      Format.fprintf fmt "status: broken@,";
      if explain then begin
       Format.fprintf fmt "@[<v 1>reasons:@,";
       Format.fprintf fmt "@[<v>%a@]" (print_error ~minimal ~condense pp r) (f ());
       Format.fprintf fmt "@]"
      end;
      Format.fprintf fmt "@]@,"
  |{result = Failure f; request = rl } when failure -> 
       Format.fprintf fmt "@[<v 1>-@,";
       Format.fprintf fmt "coinst: %s@," (String.concat " , " (List.map CudfAdd.string_of_package rl));
       if minimal then
         Format.fprintf fmt "failure: %s@,"
         (String.concat " , " (List.map CudfAdd.string_of_package rl));
       Format.fprintf fmt "status: broken@,";
       Format.fprintf fmt "@]@,";
       if explain then begin
         Format.fprintf fmt "@[<v 1>reasons:@,";
         List.iter (fun r -> 
           Format.fprintf fmt "@[<v>%a@]@," (print_error ~minimal ~condense pp r) (f ());
         ) rl;
        Format.fprintf fmt "@]@,"
       end;
  |_ -> ()
;;

let printf ?(pp=CudfAdd.default_pp) ?(failure=false) ?(success=false) ?(explain=false) d =
  fprintf ~pp ~failure ~success ~explain Format.std_formatter d

let collect results d = 
  let add h k v =
    try let l = ResultHash.find h k in l := v :: !l
    with Not_found -> ResultHash.add h k (ref [v])
  in
  match d with 
  |{result = Failure (f) ; request = [r] } -> 
      let conflicts = ref 0 in
      let missing = ref 0 in
      List.iter (fun reason ->
        match reason with
        |Conflict (i,j,_) ->
            if not (Cudf.( r =% i || r =% j )) then
              add results.summary reason r;
            results.conflict <- results.conflict + 1;
            conflicts := !conflicts + 1
        |Missing (i,vpkgs) ->
            if not (Cudf.( r =% i )) then
              add results.summary reason r;
            results.missing <- results.missing + 1;
            missing := !missing + 1
        |_ -> ()
      ) (f ());
      let id = (!conflicts,!missing) in
      begin try incr (Hashtbl.find results.statistic id)
      with Not_found -> Hashtbl.add results.statistic id (ref 1) end
  |_  -> ()
;;

let pp_summary_row explain pp fmt = function
  |(Conflict (i,j,_),pl) ->
      Format.fprintf fmt "@[<v 1>conflict:@,";
      Format.fprintf fmt "@[<v 1>pkg1:@,%a@]@," (pp_package pp) i;
      Format.fprintf fmt "@[<v 1>pkg2:@,%a@]@," (pp_package pp) j;
      Format.fprintf fmt "@[<v 1>breaks: %d@]" (List.length pl);
      if explain then begin
        Format.fprintf fmt "@,@[<v 1>packages:@," ;
        pp_list (pp_package ~source:true pp) fmt pl;
        Format.fprintf fmt "@]"
      end;
      Format.fprintf fmt "@]"
  |(Missing (i,vpkgs) ,pl) -> 
      Format.fprintf fmt "@[<v 1>missing:@,";
      Format.fprintf fmt "@[<v 1>pkg:@,%a@]@,"
        (pp_dependency ~label:"unsat-dependency" pp) (i,vpkgs);
      (* Format.fprintf fmt "@[<v 1>unsat-dependency: %a@]@," (pp_vpkglist pp) vpkgs; *)
      Format.fprintf fmt "@[<v 1>breaks: %d@]" (List.length pl);
      if explain then begin
        Format.fprintf fmt "@,@[<v 1>packages:@," ;
        pp_list (pp_package ~source:true pp) fmt pl;
        Format.fprintf fmt "@]"
      end;
      Format.fprintf fmt "@]"
  |_ -> ()
;;

let pp_summary ?(pp=CudfAdd.default_pp) ?(explain=false) () fmt result = 
  let l =
    ResultHash.fold (fun k v acc -> 
      let l1 = Util.list_unique !v in
      begin match k with
        |Conflict(i,j,_) ->
            let (pi,_,vi,_) = pp i in
            let (pj,_,vj,_) = pp j in
            result.unique_conflict <- result.unique_conflict + 1;
            if pi = pj then
              result.unique_selfconflict <- result.unique_selfconflict + 1;
        |Missing(_,_) -> result.unique_missing <- result.unique_missing +1;
        |_ -> ()
      end;
      if List.length l1 > 1 then (k,l1)::acc else acc 
    ) result.summary [] 
  in
  let l = List.sort ~cmp:(fun (_,l1) (_,l2) -> (List.length l2) - (List.length l1)) l in

  Format.fprintf fmt "@[";
  Format.fprintf fmt "missing-packages: %d@." result.missing;
  Format.fprintf fmt "conflict-packages: %d@." result.conflict;
  Format.fprintf fmt "unique-missing-packages: %d@." result.unique_missing;
  Format.fprintf fmt "unique-conflict-packages: %d@." result.unique_conflict;
  Format.fprintf fmt "unique-self-conflicting-packages: %d@." result.unique_selfconflict;
  Format.fprintf fmt "@[<v 1>conflict-missing-ratio:@,";
  let mcl = Hashtbl.fold (fun k v acc -> (k,v)::acc) result.statistic [] in
  pp_collection (fun fmt ((c,m),i) -> Format.fprintf fmt "%d-%d: %d" c m !i) fmt mcl; 
  Format.fprintf fmt "@]";
  Format.fprintf fmt "@]@.";

  Format.fprintf fmt "@[<v 1>summary:@," ;
  pp_list (pp_summary_row explain pp) fmt l;
  Format.fprintf fmt "@]@."
;;

