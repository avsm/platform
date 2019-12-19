(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Roberto Di Cosmo <roberto@dicosmo.org>                         *)
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

module Options = struct
  open OptParse

  let verbose = StdOpt.incr_option ()
  let upgradeonly = StdOpt.store_true ()

  let description = "Analyse impact of version change on the impact set of packages"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'v' ~help:"Print information (can be repeated)" verbose;
  add options ~short_name:'u' ~long_name:"upgradeonly" ~help:"Do not analyse version changes corresponding to downgrades" upgradeonly;
end

(* ----------------------------------- *)


let predbar = Util.Progress.create "Strongpred"
let debug fmt = Util.make_debug "Strongpred" fmt
let info fmt = Util.make_info "Strongpred" fmt
let warning fmt = Util.make_warning "Strongpred" fmt

(* collect all possible versions *)
let init_versions_table table =
  let add name version =
    try let l = Hashtbl.find table name in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
  in
  let conj_iter =
    List.iter (fun (name,sel) ->
      match sel with
      |None -> ()
      |Some(_,version) -> add name version
    )
  in
  let cnf_iter =
    List.iter (fun disjunction ->
      List.iter (fun (name,sel) ->
        match sel with
        |None -> ()
        |Some(_,version) -> add name version
      ) disjunction
    )
  in
  fun pkg ->
    add pkg.Cudf.package pkg.Cudf.version;
    conj_iter pkg.Cudf.conflicts ;
    conj_iter (pkg.Cudf.provides :> Cudf_types.vpkglist) ;
    cnf_iter pkg.Cudf.depends
;;

(* build a mapping between package names and a maps old version -> new version *)
let build_table universe =
  let version_table = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.iter_packages (init_versions_table version_table) universe;
  let h = Hashtbl.create (Cudf.universe_size universe) in
  Hashtbl.iter
    (fun k {contents=l} ->
       let c = ref 1 in
       let hv = Hashtbl.create (List.length l) in
	 List.iter
	   (fun n ->
	      (* Printf.eprintf "%s : %d -> %d\n" k n (2 * !c); *)
	      Hashtbl.add hv n (2 * !c);
	      c := !c + 1
	   )
	   (List.sort (List.unique l));
	 Hashtbl.add h k hv
    )
    version_table;
  h

(* map the old universe in a new universe where all versions are even *)
let renumber universe = 
  let add table name version =
    try let l = Hashtbl.find table name in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
  in
  let h = build_table universe in
  let rh = Hashtbl.create (Cudf.universe_size universe) in
  let conj_map hv =
    List.map (fun (name,sel) ->
      match sel with
      |None -> (name,sel)
      |Some(c,v) -> begin
        let hv = Hashtbl.find h name in
        add rh name ((c :> Cudf_types.relop),Hashtbl.find hv v);
        (name,Some(c,Hashtbl.find hv v))
      end
    )
  in
  let cnf_map h =
    List.map (fun disjunction ->
      List.map (fun (name,sel) ->
        match sel with
        |None -> (name,sel)
        |Some(c,v) -> begin
          (* Printf.eprintf "->>>>>>>> %s %d\n" name v; *)
          let hv = Hashtbl.find h name in
          add rh name (c,Hashtbl.find hv v);
          (name,Some(c,Hashtbl.find hv v))
        end 
      ) disjunction
    )
  in
  let pkglist = 
    Cudf.fold_packages (fun acc pkg ->
      let hv = try Hashtbl.find h pkg.Cudf.package with Not_found -> assert false in
      let p = 
        { pkg with
          Cudf.version = (try Hashtbl.find hv pkg.Cudf.version with Not_found -> assert false);
          Cudf.depends = (try cnf_map h pkg.Cudf.depends with Not_found -> assert false);
          Cudf.conflicts = (try conj_map h pkg.Cudf.conflicts with Not_found -> assert false);
          Cudf.provides = (try conj_map h pkg.Cudf.provides with Not_found -> assert false)
        }
      in p::acc
    ) [] universe 
  in (rh,pkglist)

let string_of_relop = function
  |`Eq -> "="
  |`Neq -> "!="
  |`Geq -> ">="
  |`Gt -> ">"
  |`Leq -> "<="
  |`Lt -> "<"

let string_of_package p =
  Printf.sprintf "%s - %d" (CudfAdd.string_of_package p) p.Cudf.version

let prediction universe =
  (* print_endline "------------------------------------------";
  print_endline "Original universe";
  Format.printf "%a@\n" Cudf_printer.pp_universe universe; *)
  let (version_table,pkglist) = renumber universe in
  let size = Cudf.universe_size universe in
  let res = Hashtbl.create size in

  (* print_endline "------------------------------------------";
  print_endline "Universe 1.1 ";
  Format.printf "%a@\n" Cudf_printer.pp_packages pkglist; *)
  (*
  let pp_list fmt l =
    List.iter (fun (c,v) ->
      Format.printf "(%s %d)@," (string_of_relop c) v
    ) (List.unique l)
  in
  Hashtbl.iter (fun k { contents = l} ->
    Format.printf "@[<v 1>%s%a@]@\n" k pp_list l
  ) version_table;
  print_endline "------------------------------------------";
  *)

  let universe = Cudf.load_universe pkglist in
  let graph = Strongdeps.strongdeps_univ universe in
  let changed h p =
    try incr (Hashtbl.find h p) 
    with Not_found -> Hashtbl.add h p (ref 1)
  in
  let mem_package univ (p,v) =
    try ignore(Cudf.lookup_package univ (p,v)); true
    with Not_found -> false
  in
  (* function to create a dummy package with a given version and name *)
  let create_dummy univ p  v = 
    let offset = (if p.Cudf.version > v then "-1" else "+1") in
    let n = 
      try (Cudf.lookup_package_property p "number")^offset
      with Not_found -> Printf.sprintf "%d%s" p.Cudf.version offset
    in
    {Cudf.default_package with
     Cudf.package = p.Cudf.package;
     version = v;
     pkg_extra = [("number",`String n)] 
   }
  in
  (* discriminants takes a list of version selectors and provide a minimal list 
     of versions v1,...,vn s.t. all possible combinations of the valuse of the
     version selectors are exhibited *)
  let evalsel v = function
      (`Eq,v') -> v=v'
    | (`Geq,v') -> v>=v'
    | (`Leq,v') -> v<=v'
    | (`Gt,v') -> v>v'
    | (`Lt,v') -> v<v'
    | (`Neq,v') -> v<>v'
  in
  let discriminants sels =
    let rawvl = List.unique (List.map snd sels) in
    let minv,maxv= List.fold_left (fun (mi,ma) v -> (min mi v,max ma v)) (max_int,min_int) rawvl in
    let h = Hashtbl.create 17 in
    let h' = Hashtbl.create 17 in
    (* perform the loop from hi to lo version, to make sure
       that = selectors are properly analysed even when 
       ignoring downgrades *)
    (* for w = minv-1 to maxv+1 do *)
    for offs = 0 to (maxv-minv+2) do
      let w = maxv+1-offs in
      let row = List.map (evalsel w) sels in
      if not (Hashtbl.mem h row) then 
        (Hashtbl.add h row w; Hashtbl.add h' w row);
    done;
    Hashtbl.fold (fun k v acc -> k::acc) h' [], h'
    (* FIXME: need also to return the row associated to *any* version
       to be able, later, to avoid computing on version which have
       the same row as a version existing in the repository
     *)
  in
  Util.Progress.set_total predbar size;
  Cudf.iter_packages 
    (fun p ->
      match
        try Hashtbl.find version_table p.Cudf.package
        with Not_found -> ref []
      with 
        (* If no version of p is explicitly dependend upon, then *)
     (* changing the version of p does not change its impact set *)
      |{ contents = [] } -> Printf.printf "Skipping package %s : no version selector mentions it, so IS(p) is invariant.\n" (CudfAdd.string_of_package p)
      |{ contents = l } ->
          Printf.printf "Analysing package %s\n" (CudfAdd.string_of_package p);
          let sels = List.unique l in
          let vl,explain = discriminants sels in
          let isp = Strongdeps.impactset graph p in
          let sizeisp = List.length isp in
          let (pl,_) = List.partition (fun z -> not(Cudf.(=%) z p)) pkglist in
          List.iter 
            (fun v ->
              (* FIXME: prove the following; if (p,v) and (p,w) are in U, and
                 q implies (p,v); then q is not installable when (p,w) replaces (p,v) *)
              if p.Cudf.version = v then Printf.printf " ignoring base version %d of this package.\n" v
              else
              if p.Cudf.version > v && (OptParse.Opt.get Options.upgradeonly) then  Printf.printf " ignoring version %d of this package: it is a downgrade\n" v
              else
              if mem_package universe (p.Cudf.package,v) then
                Printf.printf "If we replace %s with version %d, then all its impact set becomes uninstallable.\n"
                  (string_of_package p) v
              else
                let dummy=create_dummy universe p v in
                Util.Progress.progress predbar;
                let u = Cudf.load_universe (dummy::pl) in
                let s = Depsolver.load u in
                let broken =
                  List.fold_left
                    (fun acc q ->
                      let d = Depsolver.edos_install s q in
                      if not(Diagnostic.is_solution d) then  (* record in res the changes for the version of p in dummy *)
                            (changed res dummy; q::acc) else acc
                    ) [] isp in
                let nbroken=List.length broken in
                Printf.printf " Changing version of %s from %d to %d breaks %d/%d (=%f percent) of its Impact set.\n"
                  (string_of_package p) p.Cudf.version v nbroken sizeisp (float (nbroken * 100)  /. (float sizeisp));
                Printf.printf " Version %d valuates the existing version selectors as follows:\n  " v;
                List.iter (fun (op,v) -> Printf.printf "(%s,%d) " (string_of_relop op) v) sels; print_newline();
                List.iter (fun v -> Printf.printf "%b " v) (List.map (evalsel v) sels); print_newline();
                Printf.printf " The broken packages in IS(%s) are:\n" (string_of_package p);
                    List.iter (fun q -> Printf.printf "  - %s\n" (string_of_package q)) broken;
            ) vl
    ) universe;
  Util.Progress.reset predbar;
  res 
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars true ["Strongdeps_int.main";"Strongdeps_int.conj"];

  let (universe,_,_) = Boilerplate.load_universe posargs in
  prediction universe
(*
  let outch = open_out "pred.table" in
  List.iter (fun (p,diff,s,d) ->
    let pkg = CudfAdd.print_package p in
    Printf.fprintf outch "%s , %d, %d, %d\n" pkg s d diff
  ) (List.sort ~cmp:(fun (_,x,_,_) (_,y,_,_) -> y - x) l);
  close_out outch
*)
;;

main();;


(* garbage collector *)

(* a function to dump the results of dsicriminants:

  Hashtbl.iter (fun v row -> Printf.printf "Version %d gives " v; List.iter (fun b -> Printf.printf "%b " b) row;print_newline()) expl;; 

Example:

# let vl,expl = discriminants [(`Eq,2);(`Eq,4)];;
val vl : int list = [4; 2; 1]
val expl : (int, bool list) Hashtbl.t = <abstr>
# Hashtbl.iter (fun v row -> Printf.printf "Version %d gives " v; List.iter (fun b -> Printf.printf "%b " b) row;print_newline()) expl;;
Version 1 gives false false 
Version 2 gives true false 
Version 4 gives false true 


*)
