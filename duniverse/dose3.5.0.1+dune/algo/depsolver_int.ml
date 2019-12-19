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

let progressbar_init = Util.Progress.create "Depsolver_int.init_solver"
let progressbar_univcheck = Util.Progress.create "Depsolver_int.univcheck"

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module R = struct type reason = Diagnostic.reason_int end
module S = EdosSolver.M(R)

type solver = {
  constraints : S.state;
  map : Util.projection;
  globalid : (bool * bool) * int
}

type global_constraints = (Cudf_types.vpkglist * int list) list

type dep_t = 
  ((Cudf_types.vpkg list * S.var list) list * 
   (Cudf_types.vpkg * S.var list) list ) 
and pool = dep_t array
and t = [`SolverPool of pool | `CudfPool of (bool * pool)]

type result =
  |Success of (unit -> int list)
  |Failure of (unit -> Diagnostic.reason_int list)

(* cudf uid -> cudf uid array . Here we assume cudf uid are sequential
   and we can use them as an array index *)
let init_pool_univ ~global_constraints univ =
  (* the last element of the array *)
  let size = Cudf.universe_size univ in
  let keep = Hashtbl.create 200 in
  let pool = 
    (* here I initalize the pool to size + 1, that is I reserve one spot
     * to encode the global constraints associated with the universe.
     * However, since they are global, I've to add the at the end, after
     * I have analyzed all packages in the universe. *)
    Array.init (size + 1) (fun uid ->
      try
        if uid = size then ([],[])  (* the last index *)
        else begin
          let pkg = Cudf.package_by_uid univ uid in
          let dll = 
            List.map (fun vpkgs ->
              (vpkgs, CudfAdd.resolve_vpkgs_int univ vpkgs)
            ) pkg.Cudf.depends 
          in
          let cl = 
            List.filter_map (fun vpkg ->
              match CudfAdd.resolve_vpkg_int univ vpkg with
              |[] -> None
              |l -> Some (vpkg, l)
            ) pkg.Cudf.conflicts
          in
          if pkg.Cudf.installed then begin
            match pkg.Cudf.keep with
            |`Keep_none -> ()
            |`Keep_package ->
              List.iter (fun id ->
                CudfAdd.add_to_package_list keep (pkg.Cudf.package,None) id
              ) (CudfAdd.resolve_vpkg_int univ (pkg.Cudf.package,None));
            |`Keep_version ->
              CudfAdd.add_to_package_list keep 
                (pkg.Cudf.package,Some (`Eq, pkg.Cudf.version)) uid;
            |`Keep_feature ->
              List.iter (function
                |(name,None) ->
                  List.iter (fun id ->
                    CudfAdd.add_to_package_list keep (name,None) id
                  ) (CudfAdd.resolve_vpkg_int univ (name,None));
                |(name,Some(`Eq,v)) ->
                  List.iter (fun id ->
                    CudfAdd.add_to_package_list keep (name,Some(`Eq,v)) id
                  ) (CudfAdd.resolve_vpkg_int univ (name,Some(`Eq,v)));
              ) pkg.Cudf.provides
          end ;
          (dll,cl)
        end
      with Not_found ->
        fatal "Package uid (%d) not found during solver pool initialization. Packages uid must have no gaps in the given universe" uid
    )
  in
  let keep_dll =
    Hashtbl.fold (fun cnstr {contents = l} acc ->
      ([cnstr],l) :: acc
    ) keep global_constraints
  in
  pool.(size) <- (keep_dll,[]);
  (`CudfPool (keep_dll <> [],pool))

(** this function creates an array indexed by solver ids that can be 
    used to init the edos solver *)
let init_solver_pool map (`CudfPool (keep_constraints,cudfpool)) closure =
  let convert (dll,cl) =
    let sdll = 
      List.map (fun (vpkgs,uidl) ->
        (vpkgs,List.map map#vartoint uidl)
      ) dll
    in
    let scl = 
    (* ignore conflicts that are not in the closure.
     * if nobody depends on a conflict package, then it is irrelevant.
     * This requires a leap of faith in the user ability to build an
     * appropriate closure. If the closure is wrong, you are on your own *)
      List.map (fun (vpkg,uidl) ->
        let l =
          List.filter_map (fun uid -> 
            try Some(map#vartoint uid)
            with Not_found -> begin
              debug "Dropping Conflict %s" (Cudf_types_pp.string_of_vpkg vpkg) ;
              None
            end
          ) uidl
        in
        (vpkg, l)
      ) cl
    in (sdll,scl)
  in

  let solverpool = 
    Array.init (List.length closure) (fun sid ->
      convert (cudfpool.(map#inttovar sid))
    )
  in
  (`SolverPool solverpool)

(** initalise the sat solver. operate only on solver ids *)
let init_solver_cache ?(buffer=false) ?(explain=true) (`SolverPool varpool) =
  let num_conflicts = ref 0 in
  let num_disjunctions = ref 0 in
  let num_dependencies = ref 0 in
  let if_explain l = if explain then l else [] in
  let varsize = Array.length varpool in

  let add_depend constraints vpkgs pkg_id l =
    let lit = S.lit_of_var pkg_id false in 
    if (List.length l) = 0 then 
      S.add_rule constraints [|lit|] (if_explain [Diagnostic.MissingInt(pkg_id,vpkgs)])
    else begin
      let lits = List.map (fun id -> S.lit_of_var id true) l in
      num_disjunctions := !num_disjunctions + (List.length lits);
      S.add_rule constraints (Array.of_list (lit :: lits))
        (if_explain [Diagnostic.DependencyInt (pkg_id, vpkgs, l)]);
      if (List.length lits) > 1 then
        S.associate_vars constraints (S.lit_of_var pkg_id true) l;
    end
  in

  let conflicts = Util.IntPairHashtbl.create (varsize / 10) in
  let add_conflict constraints vpkg (i,j) =
    if i <> j then begin
      let pair = (min i j,max i j) in
      (* we get rid of simmetric conflicts *)
      if not(Util.IntPairHashtbl.mem conflicts pair) then begin
        incr num_conflicts;
        Util.IntPairHashtbl.add conflicts pair ();
        let p = S.lit_of_var i false in
        let q = S.lit_of_var j false in
        S.add_rule constraints [|p;q|] (if_explain [Diagnostic.ConflictInt(i,j,vpkg)]);
      end
    end
  in

  let exec_depends constraints pkg_id dll =
    List.iter (fun (vpkgs,dl) ->
      incr num_dependencies;
      add_depend constraints vpkgs pkg_id dl
    ) dll
  in 

  let exec_conflicts constraints pkg_id cl =
    List.iter (fun (vpkg,l) ->
      List.iter (fun id ->
        add_conflict constraints vpkg (pkg_id, id)
      ) l
    ) cl
  in

  Util.Progress.set_total progressbar_init varsize ;
  let constraints = S.initialize_problem ~buffer varsize in

  Array.iteri (fun id (dll,cl) ->
    Util.Progress.progress progressbar_init;
    exec_depends constraints id dll;
    exec_conflicts constraints id cl
  ) varpool;

  Util.IntPairHashtbl.clear conflicts;

  debug "n. disjunctions %d" !num_disjunctions;
  debug "n. dependencies %d" !num_dependencies;
  debug "n. conflicts %d" !num_conflicts;

  S.propagate constraints ;
  constraints

(** low level call to the sat solver
  
    @param tested: optional int array used to cache older results
*)
let solve ?tested ~explain solver request =
  S.reset solver.constraints;

  let result solve collect var =
    (* Real call to the SAT solver *)
    if solve solver.constraints var then begin
      if explain then (
        let l = S.assignment_true solver.constraints in
        if not(Option.is_none tested) then
          List.iter (fun i -> (Option.get tested).(i) <- true) l;
        Diagnostic.SuccessInt(fun ?(all=false) () -> l)
      ) else (
        if not(Option.is_none tested) then (
          let l = S.assignment_true solver.constraints in
          List.iter (fun i -> (Option.get tested).(i) <- true) l);
        Diagnostic.SuccessInt(fun ?(all=false) () -> [])
      )
    end else
      if explain then
        Diagnostic.FailureInt(fun () -> collect solver.constraints var)
      else
        Diagnostic.FailureInt(fun () -> [])
  in
  match request,solver.globalid with
  |[],((false,false),_) -> Diagnostic.SuccessInt(fun ?(all=false) () -> [])
  |[],(((_,true)|(true,_)),gid) -> result S.solve S.collect_reasons (solver.map#vartoint gid)
  |[i],((false,false),_) -> result S.solve S.collect_reasons (solver.map#vartoint i)
  |l,((false,false),_) ->
      let il = List.map solver.map#vartoint l in
      result S.solve_lst S.collect_reasons_lst il
  |l,(_,gid) ->
      let il = List.map solver.map#vartoint (gid :: l) in
      result S.solve_lst S.collect_reasons_lst il

(* this function is used to "distcheck" a list of packages. The id is a cudfpool index *)
let pkgcheck callback explain solver tested id =
  let res =
    Util.Progress.progress progressbar_univcheck;
    if not(tested.(id)) then
      solve ~tested ~explain solver [id]
    else begin
      (* this branch is true only if the package was previously
         added to the tested packages and therefore it is installable
         if all = true then the solver is called again to provide the list
         of installed packages despite the fact the the package was already
         tested. This is done to provide one installation set for each package
         in the universe *)
      if explain then
        let f ?(all=false) () =
          if all then begin
            match solve solver ~explain [id] with
            |Diagnostic.SuccessInt(f_int) -> f_int ()
            |Diagnostic.FailureInt _ -> assert false (* impossible *)
          end else []
        in Diagnostic.SuccessInt(f)
      (* avoid to allocate anything on the stack if not stricly needed *)
      else Diagnostic.SuccessInt(fun ?(all=false) () -> [])
    end
  in
  match callback, res with
  |None, Diagnostic.SuccessInt _ -> true
  |None, Diagnostic.FailureInt _ -> false
  |Some f, Diagnostic.SuccessInt _ -> ( f (res,[id]) ; true )
  |Some f, Diagnostic.FailureInt _ -> ( f (res,[id]) ; false )

(** low level constraint solver initialization
 
    @param buffer debug buffer to print out debug messages
    @param univ cudf package universe
*)
let init_solver_univ ~global_constraints ?(buffer=false) ?(explain=true) univ =
  let map = new Util.identity in
  (* here we convert a cudfpool in a varpool. The assumption
   * that cudf package identifiers are contiguous is essential ! *)
  let `CudfPool (keep_constraints,pool) = init_pool_univ ~global_constraints univ in
  let varpool = `SolverPool pool in
  let constraints = init_solver_cache ~buffer ~explain varpool in
  let gid = Cudf.universe_size univ in
  let global_constraints = global_constraints <> [] in
  { constraints = constraints ; map = map;
    globalid = ((keep_constraints,global_constraints),gid) }

(** low level constraint solver initialization
 
    @param buffer debug buffer to print out debug messages
    @param pool dependencies and conflicts array idexed by package id
    @param closure subset of packages used to initialize the solver
*)
(* pool = cudf pool - closure = dependency clousure . cudf uid list *)
let init_solver_closure ~global_constraints ?(buffer=false) 
  (`CudfPool (keep_constraints,cudfpool)) closure =
  let gid = Array.length cudfpool - 1 in
  let global_constraints = global_constraints <> [] in
  let map = new Util.intprojection (List.length closure) in
  List.iter map#add closure;
  let varpool =
    init_solver_pool map
      (`CudfPool (keep_constraints,cudfpool)) closure
  in
  let constraints = init_solver_cache ~buffer varpool in
  { constraints ; map = map;
    globalid = ((keep_constraints,global_constraints),gid) }

(** return a copy of the state of the solver *)
let copy_solver solver =
  { solver with constraints = S.copy solver.constraints }

(***********************************************************)

(** [reverse_dependencies index] return an array that associates to a package id
    [i] the list of all packages ids that have a dependency on [i].

    @param mdf the package universe
*)
let reverse_dependencies univ =
  let size = Cudf.universe_size univ in
  let reverse = Array.create size [] in
  Cudf.iteri_packages (fun i p ->
    List.iter (fun ll ->
      List.iter (fun q ->
        let j = CudfAdd.pkgtoint univ q in
        if i <> j then
          if not(List.mem i reverse.(j)) then
            reverse.(j) <- i::reverse.(j)
      ) ll
    ) (CudfAdd.who_depends univ p)
  ) univ;
  reverse

let dependency_closure_cache ?(maxdepth=max_int) 
  ?(conjunctive=false) (`CudfPool (_,cudfpool)) idlist =
  let queue = Queue.create () in
  let globalid = (Array.length cudfpool - 1) in
  let visited = Hashtbl.create (2 * (List.length idlist)) in
  List.iter (fun e -> Queue.add (e,0) queue) (CudfAdd.normalize_set (globalid::idlist));
  while (Queue.length queue > 0) do
    let (id,level) = Queue.take queue in
    if not(Hashtbl.mem visited id) && level < maxdepth then begin
      Hashtbl.add visited id ();
      let (l,_) = cudfpool.(id) in
      List.iter (function
        |(_,[i]) when conjunctive = true ->
          if not(Hashtbl.mem visited i) then
            Queue.add (i,level+1) queue
        |(_,dsj) when conjunctive = false ->
          List.iter (fun i ->
            if not(Hashtbl.mem visited i) then
              Queue.add (i,level+1) queue
          ) dsj
        |_ -> ()
      ) l
    end
  done;
  Hashtbl.fold (fun k _ l -> k::l) visited []

(*    XXX : elements in idlist should be included only if because
 *    of circular dependencies *)
(** return the dependency closure of the reverse dependency graph.
    The visit is bfs.    

    @param maxdepth the maximum cone depth (infinite by default)
    @param index the package universe
    @param idlist a subset of [index]

    This function use a memoization strategy.
*)
let reverse_dependency_closure ?(maxdepth=max_int) reverse =
  let h = Hashtbl.create (Array.length reverse) in
  let cmp : int -> int -> bool = (=) in
  fun idlist ->
    try Hashtbl.find h (idlist,maxdepth)
    with Not_found -> begin
      let queue = Queue.create () in
      let visited = Hashtbl.create (List.length idlist) in
      List.iter (fun e -> Queue.add (e,0) queue) (List.unique ~cmp idlist);
      while (Queue.length queue > 0) do
        let (id,level) = Queue.take queue in
        if not(Hashtbl.mem visited id) && level < maxdepth then begin
          Hashtbl.add visited id ();
          List.iter (fun i ->
            if not(Hashtbl.mem visited i) then
              Queue.add (i,level+1) queue
          ) reverse.(id)
        end
      done;
      let result = Hashtbl.fold (fun k _ l -> k::l) visited [] in
      Hashtbl.add h (idlist,maxdepth) result;
      result
    end
