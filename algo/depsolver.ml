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
open CudfAdd

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

type solver = Depsolver_int.solver
let timer_solver = Util.Timer.create "Algo.Depsolver.solver" 
let timer_init = Util.Timer.create "Algo.Depsolver.init" 

let load ?(global_constraints=[]) universe =
  let global_constraints =
    List.map (fun (vpkg,l) ->
      (vpkg,List.map (CudfAdd.pkgtoint universe) l)
    ) global_constraints
  in
  Depsolver_int.init_solver_univ ~global_constraints universe

(** [univcheck ?callback universe] check all packages in the
    universe for installability 

    @return the number of packages that cannot be installed
*)
let univcheck ?(global_constraints=[]) ?callback ?(explain=true) universe =
  let aux ?callback univ =
    let global_constraints =
      List.map (fun (vpkg,l) ->
        (vpkg,List.map (CudfAdd.pkgtoint universe) l)
      ) global_constraints
    in
    Util.Timer.start timer_init;
    let solver = Depsolver_int.init_solver_univ ~global_constraints ~explain univ in
    Util.Timer.stop timer_init ();
    Util.Timer.start timer_solver;
    let failed = ref 0 in
    (* This is size + 1 because we encode the global constraint of the
     * universe as a package that must be tested like any other *)
    let size = (Cudf.universe_size univ) + 1 in
    let tested = Array.make size false in
    Util.Progress.set_total Depsolver_int.progressbar_univcheck size ;
    let check = Depsolver_int.pkgcheck callback explain solver tested in
    (* we do not test the last package that encodes the global constraints
     * on the universe as it is tested all the time with all other packages. *)
    for id = 0 to size - 2 do if not(check id) then incr failed done;
    Util.Timer.stop timer_solver !failed
  in
  let map = new Common.Util.identity in
  match callback with
  |None -> aux universe
  |Some f ->
    let callback_int (res,req) = f (Diagnostic.diagnosis map universe res req) in
    aux ~callback:callback_int universe
;;

(** [listcheck ?callback universe pkglist] check if a subset of packages 
    un the universe are installable.

    @param pkglist list of packages to be checked
    @return the number of packages that cannot be installed
*)
let listcheck ?(global_constraints=[]) ?callback ?(explain=true) universe pkglist =
  let aux ?callback univ idlist =
    let global_constraints =
      List.map (fun (vpkg,l) ->
        (vpkg,List.map (CudfAdd.pkgtoint universe) l)
      ) global_constraints
    in
    Util.Timer.start timer_init;
    let solver = Depsolver_int.init_solver_univ ~global_constraints ~explain univ in
    Util.Timer.stop timer_init ();
    Util.Timer.start timer_solver;
    let failed = ref 0 in
    let size = (Cudf.universe_size univ) + 1 in
    let tested = Array.make size false in
    Util.Progress.set_total Depsolver_int.progressbar_univcheck size ;
    let check = Depsolver_int.pkgcheck callback explain solver tested in
    begin match (fst solver.Depsolver_int.globalid) with
    |(false,false) ->
        List.iter (fun id -> if not(check id) then incr failed) idlist
    |_ -> 
        let gid = snd solver.Depsolver_int.globalid in
        List.iter (function
          |id when id = gid -> ()
          |id ->if not(check id) then incr failed
        ) idlist 
    end;
    Util.Timer.stop timer_solver !failed
  in
  let idlist = List.map (CudfAdd.pkgtoint universe) pkglist in
  let map = new Common.Util.identity in
  match callback with
  |None -> aux universe idlist
  |Some f ->
      let callback_int (res,req) = f (Diagnostic.diagnosis map universe res req) in
      aux ~callback:callback_int universe idlist
;;

let univcheck_lowmem ?(global_constraints=[]) ?callback ?(explain=true) universe =
  let pkglist = Cudf.get_packages universe in
  let keeplist = List.flatten (List.map snd global_constraints) in
  (* Split the universe in 10 subuniverses of size 1/10 *)
  let chunkssize = ((Cudf.universe_size universe) / 20) + 1 in
  (* The set of packages that must be present in each universe *)
  let keepset = CudfAdd.to_set (CudfAdd.cone universe keeplist) in 
  debug "univcheck lowmem make paritions chunksize=%d" chunkssize;
  let partitions keepset l =
    let rec aux (univ,tested) = function
      |[] -> (univ,[])
      |pkg::rest ->
        if not(Cudf_set.mem pkg tested || Cudf_set.mem pkg univ) then begin
          let pkgcone = CudfAdd.to_set(CudfAdd.cone universe [pkg]) in
          let newuniv = Cudf_set.union pkgcone univ in
          (*debug "Old Universe %d" (Cudf_set.cardinal univ);*)
          (*debug "Cone %d" (Cudf_set.cardinal pkgcone);*)
          (*debug "Cone+Keep %d" (Cudf_set.cardinal newuniv);*)
          if Cudf_set.cardinal newuniv >= chunkssize then begin
            (newuniv,rest)
          end else aux (newuniv,tested) rest
        end else aux (univ,tested) rest
    in
    let rec make lr count =
	Enum.make
	  ~next:(fun () ->
	    match !lr with
	    | (_,[]) -> raise Enum.No_more_elements
	    | (tested,t) ->
              decr count;
              let (newuniv,rest) = aux (keepset,tested) t in
              let totest = Cudf_set.diff newuniv tested in
              let tested = Cudf_set.union tested totest in
              (*debug "New Universe %d" (Cudf_set.cardinal newuniv);*)
              (*debug "Tested %d" (Cudf_set.cardinal tested);*)
              (*debug "ToTest %d" (Cudf_set.cardinal totest);*)
              lr := (tested,rest);
              (newuniv,totest)
	  )
	  ~count:(fun () ->
	    if !count < 0 then count := List.length (snd !lr);
	    !count
	  )
	  ~clone:(fun () ->
	    make (ref !lr) (ref !count)
	  )
    in
    make (ref (CudfAdd.Cudf_set.empty,l)) (ref (-1))
  in
  Enum.fold (fun (su,stt) acc ->
    (*debug "univcheck lowmem run : %d" (CudfAdd.Cudf_set.cardinal su);*)
    (*debug "univcheck lowmem totest : %d" (CudfAdd.Cudf_set.cardinal stt);*)
    let l = CudfAdd.Cudf_set.elements su in
    let u = Cudf.load_universe l in
    let pkglist = CudfAdd.Cudf_set.elements stt in
    let b = listcheck ~global_constraints ?callback ~explain u pkglist in
    b+acc
  ) 0 (partitions keepset pkglist)
;;

let edos_install_cache univ cudfpool pkglist =
  let idlist = List.map (CudfAdd.pkgtoint univ) pkglist in
  let closure = Depsolver_int.dependency_closure_cache cudfpool idlist in
  let solver = Depsolver_int.init_solver_closure ~global_constraints:[] cudfpool closure in
  let res = Depsolver_int.solve solver  ~explain:true idlist in
  Diagnostic.diagnosis solver.Depsolver_int.map univ res idlist
;;

let edos_install ?(global_constraints=[]) universe pkg =
  let global_constraints =
    List.map (fun (vpkg,l) ->
      (vpkg,List.map (CudfAdd.pkgtoint universe) l)
    ) global_constraints
  in
  let cudfpool = Depsolver_int.init_pool_univ ~global_constraints universe in
  edos_install_cache universe cudfpool [pkg]

let edos_coinstall ?(global_constraints=[]) universe pkglist =
  let global_constraints =
    List.map (fun (vpkg,l) ->
      (vpkg,List.map (CudfAdd.pkgtoint universe) l)
    ) global_constraints
  in
  let cudfpool = Depsolver_int.init_pool_univ ~global_constraints universe in
  edos_install_cache universe cudfpool pkglist
;;

let edos_coinstall_prod ?(global_constraints=[]) universe ll =
  let global_constraints =
    List.map (fun (vpkg,l) ->
      (vpkg,List.map (CudfAdd.pkgtoint universe) l)
    ) global_constraints
  in
  let cudfpool = Depsolver_int.init_pool_univ ~global_constraints universe in
  let return a = [a] in
  let bind m f = List.flatten (List.map f m) in
  let rec permutation = function
    |[] -> return []
    |h::t ->
        bind (permutation t) (fun t1 ->
          List.map (fun h1 -> h1 :: t1) h
        )
  in
  List.map (edos_install_cache universe cudfpool) (permutation ll)
;;

let is_consistent univ =
  match Cudf_checker.is_consistent univ with
  |true, None ->
      { Diagnostic.request = [] ;
        result =
          Diagnostic.Success (fun ?(all=false) () ->
            if all then
              Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) univ
            else []
          )
      }
  |false, Some  `Unsat_dep (nv,vpkgformula) ->
      let pkg = Cudf.lookup_package univ nv in
      { Diagnostic.request = [pkg] ;
        result =
          Diagnostic.Failure (fun () ->
            List.map (fun vpkglist ->
              Diagnostic.Missing(pkg,vpkglist)
            ) vpkgformula
          )
      }
  |false, Some `Conflict (nv,vpkglist) ->
      let pkg1 = Cudf.lookup_package univ nv in
      { Diagnostic.request = [pkg1] ;
        result =
          Diagnostic.Failure (fun () ->
            List.flatten (
              List.map (fun vpkg ->
                List.map (fun pkg2 ->
                  Diagnostic.Conflict (pkg1,pkg2,vpkg)
                ) (CudfAdd.who_provides univ vpkg)
              ) vpkglist
            )
          )
      }
  |(true|false),_ -> fatal "Bug in Cudf_checker.is_consistent"

let trim ?(global_constraints=[])  universe =
  let trimmed_pkgs = ref [] in
  let callback d =
    if Diagnostic.is_solution d then
      match d.Diagnostic.request with
      |[p] -> trimmed_pkgs := p::!trimmed_pkgs
      |_ -> assert false
  in
  ignore (univcheck ~global_constraints ~callback universe);
  Cudf.load_universe !trimmed_pkgs
;;

let trimlist ?(global_constraints=[])  universe pkglist =
  let trimmed_pkgs = ref [] in
  let callback d =
    if Diagnostic.is_solution d then
      match d.Diagnostic.request with
      |[p] -> trimmed_pkgs := p::!trimmed_pkgs
      |_ -> assert false
  in
  ignore (listcheck ~global_constraints ~callback universe pkglist);
  !trimmed_pkgs
;;

let find_broken ?(global_constraints=[])  universe =
  let broken_pkgs = ref [] in
  let callback d =
    if not (Diagnostic.is_solution d) then
      match d.Diagnostic.request with
      |[p] -> broken_pkgs := p::!broken_pkgs
      |_ -> assert false
  in
  ignore (univcheck ~global_constraints ~callback universe);
  !broken_pkgs
;;

let callback_aux acc d =
  match d.Diagnostic.request with
  |[p] when (Diagnostic.is_solution d) -> 
      acc := p::!acc
  |[p] ->
      warning "Package %s is not installable" (CudfAdd.string_of_package p)
  |_ -> ()
;;

let find_installable ?(global_constraints=[]) universe =
  let acc = ref [] in
  let callback = callback_aux acc in
  ignore (univcheck ~global_constraints ~callback universe);
  !acc
;;

let find_listinstallable ?(global_constraints=[]) universe pkglist =
  let acc = ref [] in
  let callback = callback_aux acc in
  ignore (listcheck ~global_constraints ~callback universe pkglist);
  !acc
;;

let find_listbroken ?(global_constraints=[]) universe pkglist =
  let broken_pkgs = ref [] in
  let callback d =
    if not (Diagnostic.is_solution d) then
      match d.Diagnostic.request with
      |[p] -> broken_pkgs := p::!broken_pkgs
      |_ -> assert false
  in
  ignore (listcheck ~global_constraints ~callback universe pkglist);
  !broken_pkgs
;;

(** [dependency_closure index l] return the union of the dependency closure of
    all packages in [l] .

    @param maxdepth the maximum cone depth (infinite by default)
    @param conjunctive consider only conjunctive dependencies (false by default)
    @param universe the package universe
    @param pkglist a subset of [universe]
*)
let dependency_closure ?(global_constraints=[]) ?maxdepth ?conjunctive universe pkglist =
  let global_constraints =
    List.map (fun (vpkg,l) ->
      (vpkg, List.map (CudfAdd.pkgtoint universe) l)
      ) global_constraints
  in
  let idlist =
    let l = List.map (CudfAdd.pkgtoint universe) pkglist in
    (List.flatten (List.map snd global_constraints)) @ l
  in
  let pool = Depsolver_int.init_pool_univ ~global_constraints universe in
  let l = Depsolver_int.dependency_closure_cache ?maxdepth ?conjunctive pool idlist in
  let size = Cudf.universe_size universe in
  List.filter_map (fun p ->
    if p <> size then
      Some (CudfAdd.inttopkg universe p)
    else None
  ) l

let reverse_dependencies univ =
  let rev = Depsolver_int.reverse_dependencies univ in
  let h = Cudf_hashtbl.create (Array.length rev) in
  Array.iteri (fun i l ->
    Cudf_hashtbl.add h 
      (CudfAdd.inttopkg univ i) 
      (List.map (CudfAdd.inttopkg univ) l)
  ) rev ;
  h

let reverse_dependency_closure ?maxdepth univ pkglist =
  let idlist = List.map (CudfAdd.pkgtoint univ) pkglist in
  let reverse = Depsolver_int.reverse_dependencies univ in
  let closure = Depsolver_int.reverse_dependency_closure ?maxdepth reverse idlist in
  List.map (CudfAdd.inttopkg univ) closure

type enc = Cnf | Dimacs

let output_clauses ?(global_constraints=[]) ?(enc=Cnf) universe =
  let global_constraints =
    List.map (fun (vpkg,l) ->
      (vpkg,List.map (CudfAdd.pkgtoint universe) l)
    ) global_constraints
  in
  let solver = Depsolver_int.init_solver_univ ~global_constraints ~buffer:true universe in
  let clauses = Depsolver_int.S.dump solver.Depsolver_int.constraints in
  let size = Cudf.universe_size universe in
  let buff = Buffer.create size in
  let to_cnf dump =
    let str (v, p) =
      if (abs v) != size then (* the last index *)
        let pkg = (CudfAdd.inttopkg universe) (abs v) in
        let pol = if p then "" else "!" in
        Printf.sprintf "%s%s-%d" pol pkg.Cudf.package pkg.Cudf.version
      else ""
    in
    List.iter (fun l ->
      List.iter (fun var -> Printf.bprintf buff " %s" (str var)) l;
      Printf.bprintf buff "\n"
    ) dump
  in
  let to_dimacs dump =
    let str (v, p) =
      if v != size then
        if p then Printf.sprintf "%d" v else Printf.sprintf "-%d" v 
      else ""
    in
    let varnum = size in
    let closenum = (List.length clauses) in
    Printf.bprintf buff "p cnf %d %d\n" varnum closenum;
    List.iter (fun l ->
      List.iter (fun var -> Printf.bprintf buff " %s" (str var)) l;
      Printf.bprintf buff " 0\n"
    ) dump
  in
  if enc = Cnf then to_cnf clauses ;
  if enc = Dimacs then to_dimacs clauses;
  Buffer.contents buff
;;

type solver_result =
  |Sat of (Cudf.preamble option * Cudf.universe)
  |Unsat of Diagnostic.diagnosis option
  |Error of string

let dummy_request =
  { Cudf.default_package with
    Cudf.package = "dose-dummy-request";
    version = 1;
  }

(* add a version constraint to ensure name is upgraded *)
let upgrade_constr universe name = 
  match Cudf.get_installed universe name with
  |[] -> name,None
  |[p] -> name,Some(`Geq,p.Cudf.version)
  |pl ->
      let p = List.hd(List.sort ~cmp:Cudf.(>%) pl) 
      in (name,Some(`Geq,p.Cudf.version))

let check_request_using
  ?call_solver ?criteria ?(dummy=dummy_request) ?(explain=false) (pre,universe,request) =
  let intSolver ?(explain=false) universe request =
    let deps = 
      let il = request.Cudf.install in
      (* we preserve the user defined constraints, while adding the upgrade constraint *)
      let ulc =
        List.filter (function (_,Some _) -> true | _ -> false) request.Cudf.upgrade 
      in
      let ulnc =
        List.map (fun (name,_) ->
          upgrade_constr universe name
        ) request.Cudf.upgrade
      in
      let l = il @ ulc @ ulnc in
      debug "request consistency (install %d) (upgrade %d) (remove %d) (# %d)"
        (List.length request.Cudf.install) 
        (List.length request.Cudf.upgrade)
        (List.length request.Cudf.remove)
        (Cudf.universe_size universe);
        (List.map (fun j -> [j]) l)
    in
    let dummy =
      { dummy with
        Cudf.depends = deps@dummy.Cudf.depends;
        conflicts = request.Cudf.remove@dummy.Cudf.conflicts
      }
    in
    (* XXX it should be possible to add a package to a cudf document ! *)
    let pkglist = Cudf.get_packages universe in
    let universe = Cudf.load_universe (dummy::pkglist) in
    (dummy,edos_install universe dummy)
  in
  match call_solver with
  | None ->
    let (dummy,d) = intSolver universe request in
    if Diagnostic.is_solution d then
      let is = List.remove (Diagnostic.get_installationset d) dummy in
      Sat (Some pre,Cudf.load_universe is)
    else
      if explain then Unsat (Some d) else Unsat None
  | Some call_solver ->
    try Sat(call_solver (pre,universe,request)) with
    |CudfSolver.Unsat when not explain -> Unsat None
    |CudfSolver.Unsat when explain ->
        Unsat (Some (snd(intSolver ~explain universe request)))
    |CudfSolver.Error s -> Error s
;;

(** check if a cudf request is satisfiable. we do not care about
    universe consistency . We try to install a dummy package *)
let check_request ?cmd ?criteria ?dummy ?explain cudf =
  let call_solver =
    match cmd with
    | Some cmd ->
        let criteria = Option.default "-removed,-new" criteria in
        Some (CudfSolver.execsolver cmd criteria)
    | None -> None
  in
  check_request_using ?call_solver ?dummy ?explain cudf
;;

type depclean_result =
  (Cudf.package *
    (Cudf_types.vpkglist * Cudf_types.vpkg * Cudf.package list) list *
    (Cudf_types.vpkg * Cudf.package list) list
  )

(** Depclean. Detect useless dependencies and/or conflicts 
    to missing or broken packages *)
let depclean ?(global_constraints=[]) ?(callback=(fun _ -> ())) universe pkglist =
  let global_constraints =
    List.map (fun (vpkg,l) ->
      (vpkg,List.map (CudfAdd.pkgtoint universe) l)
    ) global_constraints
  in
  let cudfpool = Depsolver_int.init_pool_univ ~global_constraints universe in
  let is_broken =
    let cache = Hashtbl.create (Cudf.universe_size universe) in
    fun pkg -> 
      try Hashtbl.find cache pkg with 
      |Not_found ->
        let r = edos_install_cache universe cudfpool [pkg] in
        let res = not(Diagnostic.is_solution r) in
        Hashtbl.add cache pkg res;
        res
  in
  let enum_conf univ pkg =
    List.map (fun vpkg ->
      match CudfAdd.who_provides univ vpkg with
      |[] -> (vpkg,[])
      |l -> (vpkg,l)
    ) pkg.Cudf.conflicts
  in
  (* for each vpkglist in the depends field create a new vpkgformula
     where for each vpkg, only one one possible alternative is considered.
     We will use this revised vpkgformula to check if the selected alternative
     is a valid dependency *)
  let enum_deps univ pkg =
    let rec aux before acc = function
      |vpkglist :: after ->
          let l =
            List.map (fun vpkg ->
              match CudfAdd.who_provides univ vpkg with
              |[] -> (vpkglist,vpkg,[],[])
              |l -> (vpkglist,vpkg,before@[[vpkg]]@after,l)
            ) vpkglist
          in
          aux (before@[vpkglist]) (l::acc) after
      |[] -> List.flatten acc
    in aux [] [] pkg.Cudf.depends
  in
  (* if a package is in conflict with another package that is broken or missing,
     then the conflict can be removed *)
  let test_conflict l = 
    List.fold_left (fun acc -> function
      |(vpkg,[]) -> (vpkg,[])::acc
      |(vpkg,l) -> acc 
      (* if the conflict is with a broken package, 
         it is still a valid conflict *)
    ) [] l 
  in
  (* if a package p depends on a package that make p uninstallable, then it 
     can be removed. If p depends on a missing package, the dependency can
     be equally removed *)
  let test_depends univ (`CudfPool (_,pool)) pkg l =
    List.fold_left (fun acc -> function
      |(vpkglist,vpkg,_,[]) -> (vpkglist,vpkg,[])::acc
      |(vpkglist,vpkg,depends,l) ->
        let pkgid = Cudf.uid_by_package univ pkg in
        let (pkgdeps,pkgconf) = pool.(pkgid) in
        let dll =
          List.map (fun vpkgs ->
            (vpkgs, CudfAdd.resolve_vpkgs_int univ vpkgs)
          ) depends
        in
        let _ = pool.(pkgid) <- (dll,pkgconf) in
        let res = edos_install_cache univ cudfpool [pkg] in
        let _ = pool.(pkgid) <- (pkgdeps,pkgconf) in
        if not(Diagnostic.is_solution res) then (vpkglist,vpkg,l)::acc else acc
    ) [] l
  in
  List.filter_map (fun pkg ->
    if not(is_broken pkg) then begin
      let resdep = test_depends universe cudfpool pkg (enum_deps universe pkg) in
      let resconf = test_conflict (enum_conf universe pkg) in
      match resdep,resconf with
      |[],[] -> None
      |_,_ -> (callback(pkg,resdep,resconf) ; Some(pkg,resdep,resconf))
    end else None
  ) pkglist

(* Build a graph of install/remove actions (optionally including dependent packages *)
(* code freely adapted from opam/src/solver/opamCudf.ml *)
(* module AG = Defaultgraphs.ActionGraph *)
let installation_graph ~solution:soluniv (install,remove) =
  let module PG = Defaultgraphs.PackageGraph in
  let module PO = Defaultgraphs.GraphOper(PG.G) in
  let module Topo = Graph.Topological.Make(PG.G) in
  let module S = CudfAdd.Cudf_set in
  let packageset = S.union install remove in
  let packagelist = S.elements packageset in

  (* transitively add recompilations *)
  let remove, install =
    let g =
      let filter p = p.Cudf.installed || S.mem p packageset in
      let l = Cudf.get_packages ~filter soluniv in
      PO.O.mirror (PG.dependency_graph_list soluniv l)
    in
    Topo.fold (fun p (rm,inst) ->
	let actionned p = S.mem p rm || S.mem p inst in
	if not (actionned p) && List.exists actionned (PG.G.pred g p)
	then S.add p rm, S.add p inst
	else rm, inst
    ) g (remove, install)
  in

  let g = Defaultgraphs.ActionGraph.G.create () in
  S.iter (fun p -> Defaultgraphs.ActionGraph.G.add_vertex g (Defaultgraphs.ActionGraph.PkgV.Remove p)) remove;
  S.iter (fun p -> Defaultgraphs.ActionGraph.G.add_vertex g (Defaultgraphs.ActionGraph.PkgV.Install p)) install;

  (* reinstalls and upgrades: remove first *)
  S.iter (fun p1 ->
    try
      let same_name_as_p1 p2 = p1.Cudf.package = p2.Cudf.package in
      let p2 = S.choose (S.filter same_name_as_p1 install) in
      Defaultgraphs.ActionGraph.G.add_edge g (Defaultgraphs.ActionGraph.PkgV.Remove p1) (Defaultgraphs.ActionGraph.PkgV.Install p2)
    with Not_found -> ()
  ) remove;
  
  (* uninstall order *)
  PG.G.iter_edges (fun p1 p2 ->
    Defaultgraphs.ActionGraph.G.add_edge g (Defaultgraphs.ActionGraph.PkgV.Remove p1) (Defaultgraphs.ActionGraph.PkgV.Remove p2)
  ) (PG.dependency_graph_list soluniv (S.elements remove));

  (* install order *)
  PG.G.iter_edges (fun p1 p2 ->
    if S.mem p1 install && S.mem p2 install then
      Defaultgraphs.ActionGraph.G.add_edge g (Defaultgraphs.ActionGraph.PkgV.Install p2) (Defaultgraphs.ActionGraph.PkgV.Install p1)
    else if S.mem p1 install && S.mem p2 remove then
      Defaultgraphs.ActionGraph.G.add_edge g (Defaultgraphs.ActionGraph.PkgV.Remove p2) (Defaultgraphs.ActionGraph.PkgV.Install p1)
  ) (PG.dependency_graph_list soluniv packagelist);

  (* conflicts *)
  PG.UG.iter_edges (fun p1 p2 ->
    if S.mem p1 remove && S.mem p2 install then
      Defaultgraphs.ActionGraph.G.add_edge g (Defaultgraphs.ActionGraph.PkgV.Remove p1) (Defaultgraphs.ActionGraph.PkgV.Install p2)
    else if S.mem p2 remove && S.mem p1 install then
      Defaultgraphs.ActionGraph.G.add_edge g (Defaultgraphs.ActionGraph.PkgV.Remove p2) (Defaultgraphs.ActionGraph.PkgV.Install p1)
  ) (PG.conflict_graph_list soluniv packagelist);
  g
;;
