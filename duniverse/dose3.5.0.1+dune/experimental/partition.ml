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

open Cudf
open ExtLib
open Common
open Algo

type res = Yes | No | Unknown

module Boilerplate = BoilerplateNoRpm

let info fmt = Util.make_info "edsp-cudf" fmt
let warning fmt = Util.make_warning "edsp-cudf" fmt
let debug fmt = Util.make_debug "edsp-cudf" fmt
let fatal fmt = Util.make_fatal "edsp-cudf" fmt

module Options = struct
  open OptParse

  let description = "Partition the dependency graph in clusters of coinstallable packages"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  open OptParser

end

(* XXX to refactor in Borilerplate.ml *)
let parse uri =
  Printf.eprintf "Parsing and normalizing...%!" ;
  let timer = Common.Util.Timer.create "Parsing and normalizing" in
  Common.Util.Timer.start timer;
  let pkglist =
    match Input.parse_uri uri with
    |(Url.Deb,(_,_,_,_,file),_) -> begin
      let l = Debian.Packages.input_raw [file] in
      let tables = Debian.Debcudf.init_tables l in
      List.map (Debian.Debcudf.tocudf tables) l
    end
    |(Url.Cudf,(_,_,_,_,file),_) -> begin
      let _, l, _ = Boilerplate.parse_cudf file in l
    end
    |_ -> failwith "Not supported"
  in
  ignore(Common.Util.Timer.stop timer ());
  Printf.eprintf "done\n%!" ;
  pkglist
;;

(* ----------------------------------- *)

(* unlabelled indirected graph *)
(* module UG = Persistent.Graph.Concrete(PkgV) *)
module IG = Defaultgraphs.IntPkgGraph.G
module UG = Graph.Imperative.Graph.Concrete(Defaultgraphs.IntPkgGraph.PkgV)

module N = Graph.Oper.Neighbourhood(UG)
module O = Graph.Oper.Make(Graph.Builder.I(UG))
module S = N.Vertex_Set
module GO = Defaultgraphs.GraphOper(UG)

module DisplayF (G : Graph.Sig.I) =
  struct
    include G
    let vertex_name v = Printf.sprintf "\"%d\"" v

    let graph_attributes = fun _ -> []
    let get_subgraph = fun _ -> None

    let default_edge_attributes = fun _ -> []
    let default_vertex_attributes = fun _ -> []

    let vertex_attributes v = []

    let edge_attributes e = []
  end
module Display = DisplayF(UG)
module D = Graph.Graphviz.Dot(Display)


type 'a llist = 'a cell Lazy.t
and 'a cell = LList of 'a * 'a llist | Empty

exception LListEmpty

let empty = lazy(Empty)
let push e l = lazy(LList(e,l))

let hd s =
    match Lazy.force s with
    | LList (hd, _) -> hd
    | Empty -> raise LListEmpty

let tl s =
    match Lazy.force s with
    | LList (_, tl) -> tl
    | Empty -> raise LListEmpty

let rec append s1 s2 =
    lazy begin
        match Lazy.force s1 with
        | LList (hd, tl) -> LList (hd, append tl s2)
        | Empty -> Lazy.force s2
    end

let rec map f s =
    lazy begin
        match Lazy.force s with
        | LList (hd, tl) -> LList (f hd, map f tl)
        | Empty -> Empty
    end

let rec iter f s =
    begin
        match Lazy.force s with
        | LList (hd, tl) -> f hd ; iter f tl
        | Empty -> ()
    end

let rec flatten ss =
    lazy begin
        match Lazy.force ss with
        | Empty -> Empty
        | LList (hd, tl) ->
            match Lazy.force hd with
            | LList (hd2, tl2) -> LList (hd2, flatten (lazy (LList (tl2, tl))))
            | Empty -> Lazy.force (flatten tl)
    end

let rec filter_map f s =
    lazy begin
        match Lazy.force s with
        | LList (hd, tl) ->
                begin match f hd with
                |Some y -> LList (y, filter_map f tl)
                |None -> Lazy.force(filter_map f tl)
                end
        | Empty -> Empty
    end

let rec of_list = function
    | [] -> lazy(Empty)
    | hd :: tl -> lazy (LList (hd, of_list tl))

let reverse =
    let rec loop stack = function
        | LList (hd, tl) -> loop (hd :: stack) (Lazy.force tl)
        | Empty -> stack
    in
    fun s ->
        of_list (loop [] (Lazy.force s))

let is_empty s =
    match Lazy.force s with
    |Empty -> true
    |_ -> false

exception Forall
let for_all f e = 
  if is_empty e then true
  else
    try (iter (fun a -> if f a then () else raise Forall) e ; true)
    with Forall -> false

let bind m f = flatten (map f m)
let return a = push a empty

let rec allsets m l =
  if m = 0 then return [] else
  match l with
  |[] -> empty
  |h::t -> append (map (fun g -> h::g) (allsets (m - 1) t)) (allsets m t)
;;

let rec range acc a b =
  if a > b then acc
  else range (push a acc) (a+1) b
;;

let rec permutation = function 
  |[] -> return []
  |h::t ->
      bind (permutation t) (fun t1 ->
        map (fun h1 -> (h1 :: t1)) h
      )

let rec bronKerbosch2 gr r p x =
  let n v = N.set_from_vertex gr v in
  let rec fold acc (p,x,s) =
    if S.is_empty s then acc
    else
      let v = S.choose s in
      let rest = S.remove v s in
      let r' = S.union r (S.singleton v) in
      let p' = S.inter p (n v) in
      let x' = S.inter x (n v) in
      let acc' = (push (r',p',x') acc) in
      fold acc' (S.remove v p, S.add v x, rest)
  in
  if (S.is_empty p) && (S.is_empty x) then (push r empty)
  else
    let s = S.union p x in
    let u = S.choose s in
    let l = fold empty (p,x,S.diff p (n u)) in
    flatten (map (fun (r',p',x') -> bronKerbosch2 gr r' p' x') l)
;;

let choose gr cands s =
  let n v = N.set_from_vertex gr v in
  fst (
    S.fold (fun u (pivot, pivot_score) ->
      let score = S.cardinal (S.inter (n u) cands) in
      if score > pivot_score then (u, score)
      else (pivot, pivot_score)
    ) s (-1, -1)
  )

let rec bronKerbosch_pivot gr clique cands nots =
  let n v = N.set_from_vertex gr v in
  let rec fold acc (cands,nots,s) =
    if S.is_empty s then acc
    else
      let pivot = choose gr cands s in
      let rest = S.remove pivot s in
      let clique' = S.add pivot clique in
      let cands' = S.inter cands (n pivot) in
      let nots' = S.inter nots (n pivot) in
      let acc' = (push (clique',cands',nots') acc) in
      fold acc' (S.remove pivot cands, S.add pivot nots, rest)
  in
  if (S.is_empty cands) && (S.is_empty nots) then (push clique empty)
  else
    let s = S.union cands nots in
    let pivot = choose gr cands s in
    let rest = S.diff cands (n pivot) in
    let l = fold empty (S.remove pivot cands,S.add pivot nots,rest) in
    flatten (map (fun (clique',cands',nots') -> bronKerbosch_pivot gr clique' cands' nots') l)

let max_independent_sets gr =
  let cgr = O.complement gr in
  let r = S.empty in
  let p = UG.fold_vertex S.add cgr S.empty in
  let x = S.empty in
  bronKerbosch_pivot cgr r p x
;;

let conflictgraph mdf =
  let index = mdf.Mdf.index in
  let g =UG.create () in
  for i=0 to (Array.length index - 1) do
    let pkg = index.(i) in
    let conflicts = List.map snd pkg.Mdf.conflicts in
    List.iter (UG.add_edge g i) conflicts
  done;
  g
;;

let filter gr cc =
  let g = UG.create () in
  List.iter (fun v1 ->
    UG.iter_succ (fun v2 ->
      UG.add_edge g v1 v2
    ) gr v1
  ) cc
  ;
  g
;;

(** connected components. *)
(* XXX : not the most efficient/elegant way, isn't it ? 
    I should do a visit with marking and remove the hashtbl. *)
let connected_components graph =
  let module Dfs = Graph.Traverse.Dfs(UG) in
  let h = Hashtbl.create (UG.nb_vertex graph) in
  let l = ref [] in
  let cc graph id =
    let l = ref [] in
    let collect id = l := id :: !l in
    Dfs.prefix_component collect graph id;
    !l
  in
  UG.iter_vertex (fun v ->
    if not(Hashtbl.mem h v) then begin
      Hashtbl.add h v ();
      match cc graph v with
      |[] -> ()
      |c ->
          begin
            List.iter (fun x -> Hashtbl.add h x ()) c ;
            l := c :: !l
          end
    end
  ) graph ;
  !l
;;

(* associate a connected component to each conflict node *)
let conflict_table cc =
  let h = Hashtbl.create (2 * List.length cc) in
  List.iter (fun l ->
    List.iter (fun v ->
      Hashtbl.add h v (ref l)
    ) l
  ) cc
  ;
  h
;;

(* associate a list of connected components to each package *)
(* we add only packages that are in the reverse dependencies of
 * a conflict. all the others are not interesting for this analysis *)
let package_table a mdf reverse cg ct =
  let h = Hashtbl.create (UG.nb_vertex cg) in
  let rev_clo pid = Depsolver_int.reverse_dependency_closure reverse [pid] in
  UG.iter_vertex (fun v ->
    List.iter (function
      |p when (List.length mdf.Mdf.index.(p).Mdf.conflicts > 0) -> a.(p) <- No
      |p ->
        try
          let l = Hashtbl.find h p in
          try l := (Hashtbl.find ct v)::!l
          with Not_found -> assert false
        with Not_found -> Hashtbl.add h p (ref [Hashtbl.find ct v])
    ) (rev_clo v)
  ) cg
  ;
  let l = Hashtbl.fold (fun v k acc -> 
    let l = List.sort ~cmp:(fun x y -> (List.length !x) - (List.length !y)) !k in
    (v, ref(List.unique l))::acc ) h [] 
  in
  List.sort ~cmp:(fun (_,k1) (_,k2) -> 
(*    let x1 = List.fold_left (fun acc x -> acc + (List.length !x)) 0 !k1 in
    let x2 = List.fold_left (fun acc x -> acc + (List.length !x)) 0 !k2 in
    x1 - x2
*)
    (List.length !k1) - (List.length !k2)
  ) l 
;;

let install solver _ ll = 
  let aux l = 
    Depsolver_int.S.reset solver.Depsolver_int.constraints;
    match Depsolver_int.solve solver (Diagnostic_int.Lst l) with
    |Diagnostic_int.Failure _ -> false
    |Diagnostic_int.Success _ -> true
  in
  let s = List.fold_left S.union S.empty ll in
  let l = S.elements s in
  aux l
;;

let reverse_table reverse =
  let h = Hashtbl.create (2000) in
  fun xl ->
    try Hashtbl.find h xl
    with Not_found -> begin
      let cs =
        List.fold_left (fun s v ->
          List.fold_right S.add reverse.(v) s
        ) S.empty !xl
      in
      Hashtbl.add h xl cs ;
      cs
    end

let not_buddy a mdf p =
  (List.length mdf.Mdf.index.(p).Mdf.conflicts > 0) || a.(p) = No

let build_cc_list mdf pt a p =
  let dcl = Depsolver_int.dependency_closure mdf [p] in
  List.fold_left (fun acc i ->
    try
      if a.(i) = Yes then acc
      else (!(snd(List.find (fun (j,_) -> i = j) pt))) @ acc
    with Not_found -> acc
  ) [] dcl
;;

let to_set l = List.fold_right S.add l S.empty

let build_bdd g =
  let vars = Hashtbl.create (UG.nb_vertex g) in
  let find x =
    try Hashtbl.find vars x 
    with Not_found -> begin
      let v = Buddy.bdd_pos (Buddy.bdd_newvar ()) in
      Hashtbl.add vars x v ; v
    end
  in
  Buddy.bdd_varblockall ();
  let bdd = 
    UG.fold_edges (fun x y b ->
      Buddy.bdd_and (Buddy.bdd_or (find x) (find y)) b
    ) g Buddy.bdd_true
  in
  Buddy.bdd_reorder ();
  bdd
;;

let buddy_check solver mdf cg a p ll =
  let size = List.length ll in
  let dcl = Depsolver_int.dependency_closure ~conjunctive:true mdf [p] in 
  if List.exists (not_buddy a mdf) dcl then false else
  let dcl = Strongdeps_int.stronglist (Strongdeps_int.strongdeps mdf [p]) p in
  if List.exists (not_buddy a mdf) dcl then false else 
  for_all (fun m ->
    for_all (fun l ->
(*      Common.Util.print_info "All sets of size %d (%d)%!" m (List.length l); 
      List.iter (fun xl ->
        Common.Util.print_info "-> %s" (String.concat " , " (List.map string_of_int !xl))
      ) l ;
  *)
      let gl = List.map (fun xl -> (filter cg !xl)) l in
(*      if List.exists (fun g -> UG.nb_vertex g > 70) gl then begin
        List.iteri (fun i g ->
          let outch = open_out (Printf.sprintf "%d%d.dot" p i) in
          (* D.output_graph outch g ; *)
          Buddy.bdd_fprintdot outch (build_bdd g);
          close_out outch;
          Printf.eprintf "DOOOOONE\n%!";
        ) gl ; 
        (* (hard := (p,ll) :: !hard ; *)  false
      end
      else i*) begin
        let sgl = List.sort ~cmp:(fun c1 c2 -> (UG.nb_vertex c1) - (UG.nb_vertex c2)) gl in
        let misl =
          List.map (fun g ->
            let e = 
              filter_map (fun s ->
                (* let sl = (S.elements s) in
                Common.Util.print_info "%s" (String.concat " , " (List.map string_of_int sl)); *)
                if install solver a [s] then Some s else None
              ) (max_independent_sets g)
            in if is_empty e then assert false else e
          ) sgl
        in
        for_all (fun e -> 
          install solver a ((S.singleton p)::e)
        ) (permutation misl)
      end
    ) (allsets m ll)
  ) (reverse (range empty 1 size))
;;

let cmp l1 l2 = (List.length l2) - (List.length l1)

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Buddy.bdd_init ();
  let pkglist = match posargs with [uri] -> parse uri | _ -> assert false in
  let mdf = Mdf.load_from_list pkglist in
  let maps = mdf.Mdf.maps in
  let index = mdf.Mdf.index in
  let solver = Depsolver_int.init_solver index in
  let reverse_t = Depsolver_int.reverse_dependencies mdf in
  let cg = conflictgraph mdf in
  let cc = connected_components cg in
  Printf.eprintf "conflict graph = vertex : %d , edges : %d\n"
  (UG.nb_vertex cg) (UG.nb_edges cg);
  Printf.eprintf "connected components = n. %d , largest : %d\n"
  (List.length cc) (List.length (List.hd (List.sort ~cmp:cmp cc)));
  let ct = conflict_table cc in
  let a = Array.make (Array.length index) Unknown in
  let pt = package_table a mdf reverse_t cg ct in
  (* XXX mark all packages with no conflicts to Yes *)
  let c = ref 0 in
  List.iter (function 
    |(p,_) when a.(p) = Yes -> incr c;
    |(p,{contents=ll}) -> begin
      let ll = List.unique (build_cc_list mdf pt a p) in 
      incr c;
      Printf.printf
        "(%d of %d) package %s (# components %d) (# cone %d) %!\n"
        !c (List.length pt) 
        (CudfAdd.string_of_package (maps.CudfAdd.map#inttovar p)) (List.length ll)
        (List.length (Depsolver_int.dependency_closure mdf [p]))
      ;
      if buddy_check solver mdf cg a p ll then begin
        a.(p) <- Yes ;
        Printf.printf "%s is always installable\n%!" 
          (CudfAdd.string_of_package (maps.CudfAdd.map#inttovar p))
      end
      else begin
        a.(p) <- No;
        Printf.printf "eliminates at least one mis\n%!"
      end
    end
  ) pt 
  ;
  let buddy = Array.fold_left (fun acc v -> if v = Yes then acc + 1 else acc) 0 a in
  let unknown = Array.fold_left (fun acc v -> if v = Unknown then acc + 1 else acc) 0 a in
  let notbuddy = Array.fold_left (fun acc v -> if v = No then acc + 1 else acc) 0 a in
  let all = 
    let l = ref S.empty in 
    Array.iteri (fun i v -> if v = Yes then l := S.add i !l) a ; !l  
  in
  
  if install solver a [all] then 
    print_endline "Success" 
  else 
    print_endline "error"; 
  
  Printf.printf "Total : %d , not buddy : %d - buddy : %d\n%!" 
  (List.length pkglist)
  notbuddy
  ( buddy + unknown )
;;

main () ;;
