(**************************************************************************************)
(*  Copyright (C) 2013 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009,2010 Mancoosi Project                                          *)
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
open Algo

include Util.Logging(struct let label = __FILE__ end) ;;

module Options = struct
  open OptParse
  let description = "cudf solver using facile"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let request = Boilerplate.incr_str_list ()

  open OptParser
  add options ~long_name:"request" ~help:"Installation Request (can be repeated)" request;

  include Boilerplate.InputOptions
  Boilerplate.InputOptions.add_options ~default:["outfile";"latest";"trim"] options ;;

  include Boilerplate.DistribOptions;;
  Boilerplate.DistribOptions.add_options options ;;

end;;

open Facile
open Easy

(* -------------------------------- *)

let varstack = Stack.create () ;;

(* given an array of boolean variables and a list of indexes in this array,
 * returns a constraint that is true if all variables are true *)
let bigand arr l =
  Arith.sum_fd (Array.of_list (List.map (Array.get arr) l))
    =~ i2e (List.length l) 
;;

(* given an array of boolean variables and a list of indexes in this array,
 * returns a constraint that is true if at least one variable is true *)
let bigor arr l = 
  Arith.sum_fd (Array.of_list (List.map (Array.get arr) l)) >=~ i2e 1
;;

(* return a constraint that is true if the variable with index i is true *)
let truev v = fd2e v =~ i2e 1 ;;
(* return a constraint that is false if the variable with index i is false *)
let falsev v = fd2e v =~ i2e 0 ;; 

let trueav arr i  = truev arr.(i) ;;
let falseav arr i  = falsev arr.(i) ;;

let removepred arr vp rid =
  let cardvp = List.length vp in
  let posc =
    (Arith.sum_fd (Array.of_list (List.map (Array.get arr) vp))) 
      +~ fd2e rid >=~ i2e 1
  in
  let negc =
    (Arith.sum_fd (Array.of_list (List.map (Array.get arr) vp)))
      +~ i2e cardvp *~ fd2e rid <=~ i2e cardvp
  in
  let c = posc &&~~ negc in
  Cstr.fprint stdout c;
  print_newline ();
  Cstr.post c
;;

let newpred arr vp rid =
  let posc = 
    (Arith.sum_fd (Array.of_list (List.map (Array.get arr) vp))) 
      -~ fd2e rid >=~ i2e 0
  in
  let negc =
    (Arith.sum_fd (Array.of_list (List.map (Array.get arr) vp))) 
      -~ i2e (List.length vp) *~ fd2e rid <=~ i2e 0
  in
  let c = posc &&~~ negc in
  List.iter (fun i ->
    Var.Fd.delay [Var.Fd.on_subst] arr.(i) c;
  ) vp;
  Cstr.fprint stdout c;
  print_newline ();
  Cstr.post c
;;

let newpred_cstr v l =
  let e = (Arith.sum_fd (Array.of_list l)) in
  let c = (Reify.xor (e >~ i2e 0) ( e =~ i2e 0)) in
  let name = "newpred" in
  let fprint oc =
    Printf.fprintf oc "%s: -> %s \n" name (String.concat " | "
    (List.filter_map (fun v -> if not (Fd.is_bound v) then Some (Fd.name v) else None) l)) in
  let delay ct = 
    List.iter (fun w -> Var.Fd.delay [Var.Fd.on_subst] w ct) (v::l)
  and update _ =
    match Fd.value v with
    |Val 0 -> (List.iter (fun w -> Fd.unify w 0) l ; true)
    |Val 1 -> (Printf.printf "update " ; fprint stdout ; true)
    |Unk _ -> 
        begin
          if Cstr.is_solved c then
            
            if List.exists (fun w -> Fd.value w = Val 1) l then
              ( List.iter (fun w -> if Fd.value w = Val 1 then () else Fd.unify w 0) l;
                Printf.printf "update "; fprint stdout ; Fd.unify v 1 ; true)
            else 
              (Printf.printf "update "; fprint stdout ; Fd.unify v 0 ; true)
          else false
        end
    |Val _ -> assert false
  and init () =
    List.iter (fun w -> Var.Fd.delay [Var.Fd.on_subst] w c) l;
    Cstr.post c; 
  in
  Cstr.create ~name ~fprint ~init update delay
;;

let changedpred arr (ivp,uvp) rid =
  let posc =
    ((List.fold_left (fun a i -> (trueav arr i) &&~~ a) Cstr.one ivp) &&~~
    (List.fold_left (fun a i -> (falseav arr i) &&~~ a) Cstr.one uvp))
      =>~~ (fd2e rid =~ i2e 0)
  in
  let negc = 
    ((List.fold_left (fun a i -> (fd2e arr.(i) =~ i2e 0) ||~~ a) Cstr.zero ivp) &&~~
    (List.fold_left (fun a i -> (fd2e arr.(i) =~ i2e 1) ||~~ a) Cstr.zero uvp))
      =>~~ (fd2e rid =~ i2e 1)
  in
  let c = posc &&~~ negc in
  Cstr.fprint stdout c;
  print_newline ();
  Cstr.post c
;;

let build_costraints global_constraints univ req =
  let cudfpool = Depsolver_int.init_pool_univ global_constraints univ in
  let (depsarray,confarray) = CudfAdd.compute_pool univ in
  let size = Cudf.universe_size univ in
  let buff = Buffer.create size in

  let module Iset = Set.Make(struct type t = int let compare = (-) end) in
  let to_set l = List.fold_right Iset.add l Iset.empty in

  let searchvars =
    (CudfAdd.resolve_vpkgs_int univ req.Cudf.install) @
    (CudfAdd.resolve_vpkgs_int univ req.Cudf.remove) @
    (CudfAdd.resolve_vpkgs_int univ req.Cudf.upgrade)
  in

  let installed, keep =
    let keep = Hashtbl.create 200 in
    let l =
      List.filter_map (fun id ->
        let pkg = Cudf.package_by_uid univ id in

        if pkg.Cudf.keep = `Keep_package then
          CudfAdd.add_to_package_list keep pkg.Cudf.package id;

        if pkg.Cudf.installed then Some id else None
      ) (Util.range 0 (size-1))
    in
    (l, (Hashtbl.fold (fun _ { contents = l } acc -> l::acc) keep [] ))
  in

  let upperboundinstall =
    let l = (
      (CudfAdd.resolve_vpkgs_int univ req.Cudf.install) @ 
      (CudfAdd.resolve_vpkgs_int univ req.Cudf.upgrade) @
      (List.flatten keep))
    in
    let dc = Depsolver_int.dependency_closure_cache cudfpool l in
    let h = Hashtbl.create (List.length l) in
    List.iter (fun i ->
      let p = Cudf.package_by_uid univ i in
      if not p.Cudf.installed then begin
        let n = (Cudf.package_by_uid univ i).Cudf.package in
        Hashtbl.replace h n ()
      end
    ) dc
    ;
    Hashtbl.length h
  in
(*
  let strongdepsinstall = 
    (* XXX something is wrong here ... I know *)
    List.fold_left (fun acc vpkg ->
      let ids = CudfAdd.resolve_vpkg_int univ vpkg in
      let sl = 
        List.map (fun id ->
          let p = Cudf.package_by_uid univ id in
          let g = Algo.Strongdeps.conjdeps univ [p] in
          let l = Algo.Strongdeps_int.stronglist g p in
          Printf.printf "stronglist %s = %d\n" p.Cudf.package (List.length l);
          to_set (List.map (fun p -> Cudf.uid_by_package univ p) l)
        ) ids
      in
      let s = if List.length sl = 1 then List.hd sl else 
        if List.length sl = 0 then Iset.empty
        else List.fold_left (fun acc ss ->
        Iset.inter acc ss) (List.hd sl) sl 
      in
      Iset.union acc s
    ) Iset.empty (req.Cudf.install @ req.Cudf.upgrade)
  in

  Printf.printf "strong\n";
  Iset.iter (fun i -> 
    let p = Cudf.package_by_uid univ i in
    Printf.printf "%s\n" (CudfAdd.string_of_package p)
  ) strongdepsinstall;

  let installlowerbound =
    Iset.fold (fun i acc ->
      let p = Cudf.package_by_uid univ i in
      if not p.Cudf.installed then acc + 1 else acc
    ) strongdepsinstall 0
  in
*)

  (* depclsure is composed of all versions of the packages 
   * mentioned packages in the request, plus all installed packages,
   * plus all keep packages *)
  let depclsure =
    let l = (searchvars @ installed @ (List.flatten keep)) in
    let dc = Depsolver_int.dependency_closure_cache cudfpool l in
    let dcn = 
      List.flatten (
        List.map (fun i -> 
          List.map (Cudf.uid_by_package univ) (
            Cudf.lookup_packages univ
              (Cudf.package_by_uid univ i).Cudf.package
          )
        ) dc
      )
    in
    to_set (Depsolver_int.dependency_closure_cache cudfpool dcn)
  in

  (* atleastinst : names id where at lest one version of a package is installed
   * noneinst : names id where none versions are installed *)
  let atleastinst, names, namessize, noneinst, namearr =
    let h = Hashtbl.create (Cudf.universe_size univ) in
    Iset.iter (fun i ->
      let p = Cudf.package_by_uid univ i in
      if not (Hashtbl.mem h p.Cudf.package) then
        let l = List.map (Cudf.uid_by_package univ) (Cudf.lookup_packages univ p.Cudf.package) in
        Hashtbl.add h p.Cudf.package l
    ) depclsure;

    let a = Array.make (Hashtbl.length h) "" in
    let i = ref 0 in
    Hashtbl.iter (fun k _ -> Array.set a !i k ; incr i) h;

    Hashtbl.fold (fun k vl (atl,l,j,nonel,a) ->
      if not (Iset.is_empty (Iset.inter (to_set vl) depclsure)) then
        let pl = List.map (Cudf.package_by_uid univ) vl in
        if List.exists (fun p -> p.Cudf.installed) pl then
          (j::atl,k::l,j+1,nonel,a)
        else
          (atl,k::l,j+1,j::nonel,a)
      else (atl,l,j+1,nonel,a)
    ) h ([],[],0,[],a)
  in

  List.iter (fun i -> 
    Printf.printf "%s\n" 
    (CudfAdd.string_of_package (Cudf.package_by_uid univ i))
  ) (Iset.elements depclsure);

  let noneinst =
    List.filter_map (fun i ->
      let name = namearr.(i) in
      let pkgversions = List.map (Cudf.uid_by_package univ) (Cudf.lookup_packages univ name) in
      if Iset.is_empty (Iset.inter (to_set pkgversions) depclsure) then None
      else Some i
    ) noneinst
  in

  let pkgmap = Depsolver_int.init_map (Iset.elements depclsure) univ in
  let newpkgmap = Depsolver_int.init_map noneinst univ in
  let size = Iset.cardinal depclsure in
  let vartoint i = pkgmap#vartoint i in
  let inttovar i = pkgmap#inttovar i in

  let pkgarr = 
    Array.init size (fun i ->
      Var.Fd.create ~name:(CudfAdd.string_of_package (Cudf.package_by_uid univ (inttovar i))
    ) Domain.boolean) 
  in
  let removedarr = Array.init namessize (fun i -> Var.Fd.create ~name:namearr.(i) Domain.boolean) in
  let changedarr = Array.init namessize (fun i -> Var.Fd.create ~name:namearr.(i) Domain.boolean) in
  let newarr     = Array.init (List.length noneinst) (fun i -> Var.Fd.create ~name:("new:"^namearr.(i)) Domain.boolean) in

(*
  Iset.iter (fun i ->
    (* all the strong dependencies of the request are set to true *)
    Var.Fd.unify pkgarr.(vartoint i) 1;
    List.iter (fun j ->
      (* all the packages that conflicts with the above are set to false *)
      if Iset.mem j depclsure then 
        Var.Fd.unify pkgarr.(vartoint j) 0;
    ) confarray.(i)
    (* all the packages that strong dependes on the above are also set to false *)
  ) strongdepsinstall ;
  *)

  let bigor_cstr l li = 
    let c = Arith.sum_fd (Array.of_list l) >=~ i2e 1 in
    let name = "bigor" in
    let fprint oc =
      Printf.fprintf oc "%s: -> %s \n" name (String.concat " | "
      (List.filter_map (fun v -> if not (Fd.is_bound v) then Some (Fd.name v) else None) l)) in
    let delay ct = 
      List.iter (fun w -> Var.Fd.delay [Var.Fd.on_subst] w ct) l
    and update _ = (fprint stdout; Cstr.post c ; true)
    and check () =
      if (List.exists (fun w -> Fd.value w = Val 1) l) then true
      else raise Cstr.DontKnow
    in
    Cstr.create ~name ~fprint ~check update delay
  in 

  let depends_disj_cstr v l li = 
    let c = Arith.sum_fd (Array.of_list l) >=~ i2e 1 in
    let name = "depends disjunctive" in
    let fprint oc =
      Printf.fprintf oc "%s: %a -> %s \n" name Fd.fprint v (String.concat " | " 
      (List.filter_map (fun v -> if not (Fd.is_bound v) then Some (Fd.name v) else None) l)) in
    let delay ct = Var.Fd.delay [Var.Fd.on_subst] v ct
    and update _ =
      match Fd.value v with
      |Val 1 -> (
        Printf.printf "update ";
        fprint stdout ; 
        List.iter (fun x -> Stack.push x varstack) li ;
        List.iter (fun w -> Var.Fd.delay [Var.Fd.on_subst] w c) l;
        Cstr.post c ; 
        true)
      |Val 0 -> true
      |Unk _ -> false 
      |Val _ -> assert false
    and check () =
      if (Fd.value v = Val 1) then
        if (List.exists (fun w -> Fd.value w = Val 1) l) then true
        else if (List.for_all (fun w -> Fd.value w = Val 0) l) then false
        else raise Cstr.DontKnow
      else
        raise Cstr.DontKnow
    in
    Cstr.create ~name ~fprint ~check update delay
  in 

  let depends_conj_cstr v l = 
    let c = Arith.sum_fd (Array.of_list l) >=~ i2e (List.length l) in
    let name = "depends conjunctive" in
    let fprint oc =
      Printf.fprintf oc "%s: %a -> %s \n" name Fd.fprint v (String.concat " & " 
      (List.filter_map (fun v -> if not (Fd.is_bound v) then Some (Fd.name v) else None) l)) in
    let delay ct = Var.Fd.delay [Var.Fd.on_subst] v ct
    and update _ =
      match Fd.value v with
      |Val 1 -> (Printf.printf "update "; fprint stdout; (List.iter (fun w -> Var.Fd.unify w 1) l) ; true)
      |Val 0 -> true
      |Unk _ -> false
      |Val _ -> assert false
    and check () =
      if (Fd.value v = Val 1) && (List.for_all (fun w -> Fd.value w = Val 1) l) then true
      else raise Cstr.DontKnow
    in
    Cstr.create ~name ~fprint ~check update delay
  in 

  let conflict_cstr v w =
    let name = "conflicts" in
    let fprint c =
      Printf.fprintf c "%s: %a ~ %a\n" name Fd.fprint v Fd.fprint w
    in
    let delay ct =
      Var.Fd.delay [Var.Fd.on_subst] v ct;
      (* Var.Fd.delay [Var.Fd.on_subst] w ct; *)
    and update _ =
      match Fd.value v, Fd.value w with
      |Val 1, (Val 0 | Unk _) -> (Printf.printf "update "; fprint stdout; Var.Fd.unify w 0 ; true)
      |(Val 0 | Unk _), Val 1 -> (Printf.printf "update "; fprint stdout; Var.Fd.unify v 0 ; true)
      |Val 0, Val 0 -> true
      |_, _ -> false
    and check () =
      match Fd.value v, Fd.value w with
      |Val 1, Val 0 -> true
      |Val 0, Val 1 -> true
      |Val 0, Val 0 -> true
      |Val 1, Val 1 -> false
      | _ -> raise Cstr.DontKnow
    in
    Cstr.create ~priority:Cstr.immediate ~name ~fprint ~check update delay
  in


  print_endline "install:";
  if req.Cudf.install <> [] then begin
    List.iter (fun ((n,_) as vpkg) ->
      (* this constraint is true if at least one variable is true *)
      let l = List.map (fun i -> pkgarr.(vartoint i)) (CudfAdd.resolve_vpkg_int univ vpkg) in
      Printf.printf "install %s: \"%d\"\n" n (List.length l);
      if List.length l = 1 then begin
        Var.Fd.unify (List.hd l) 1;
        Printf.fprintf stdout "var: %a\n" Fd.fprint (List.hd l)
      end else begin
        (* let c = bigor_cstr l in *)
        let c = Arith.sum_fd (Array.of_list l) >~ i2e 0 in
        List.iter (fun w -> Var.Fd.delay [Var.Fd.on_subst] w c) l;
        Cstr.fprint stdout c;
        print_newline ();
        Cstr.post c
      end
    ) req.Cudf.install
  end;

  print_endline "remove:";
  if req.Cudf.remove <> [] then begin
    let l = List.map vartoint (CudfAdd.resolve_vpkgs_int univ req.Cudf.remove) in
    List.iter (fun i -> Var.Fd.unify pkgarr.(i) 0) l
  end;

  print_endline "upgrade";
  if req.Cudf.upgrade <> [] then begin
    List.iter (fun ((name,_) as vpkg) ->
      let pkginstlist = List.map (Cudf.uid_by_package univ) (Cudf.get_installed univ name) in
      let l = 
        List.filter_map (fun id ->
          if not (List.mem id pkginstlist) then Some(pkgarr.(vartoint id)) else None
        ) (CudfAdd.resolve_vpkg_int univ vpkg)
      in
      (* let c = bigor_cstr l in *)
      if List.length l = 1 then begin
        Var.Fd.unify (List.hd l) 1;
        Printf.fprintf stdout "var: %a\n" Fd.fprint (List.hd l)
      end else begin
        (* XXX : include only those pkgs that have a version that is equal
           or greater then the version currently installed, if any *) 
        let c = Arith.sum_fd (Array.of_list l) >=~ i2e 1 in
        List.iter (fun w -> Var.Fd.delay [Var.Fd.on_subst] w c) l;
        Cstr.fprint stdout c;
        print_newline ();
        Cstr.post c
      end
    ) req.Cudf.upgrade
  end;

  print_endline "conflicts:";
  (* add conflicts to the constraints store *)
  Array.iteri (fun id cl ->
    if Iset.mem id depclsure && List.length cl > 0 then begin
      let j = vartoint id in
      List.iter (fun ii ->
        if Iset.mem ii depclsure then begin 
          let i = vartoint ii in
          let c = conflict_cstr pkgarr.(i) pkgarr.(j) in
          Cstr.fprint stdout c;
          print_newline ();
          Cstr.post c
        end
      ) cl
    end
  ) confarray ;

  print_endline "dependencies:";
  (* add dependencies *)
  Array.iteri (fun id dll ->
    if Iset.mem id depclsure then begin
      let conj,disj = List.partition (fun l -> List.length l = 1) dll in
      let j = vartoint id in
      if List.length (List.flatten conj) > 0 then begin
        let l = List.map vartoint (List.flatten conj) in
        let c = depends_conj_cstr pkgarr.(j) (List.map (Array.get pkgarr) l) in
        Cstr.fprint stdout c;
        print_newline ();
        Cstr.post c;
      end;

      List.iter (fun dl ->
        if List.length dl > 0 then begin
          let l = List.map vartoint dl in
          let c = depends_disj_cstr pkgarr.(j) (List.map (Array.get pkgarr) l) l in
          Cstr.fprint stdout c;
          print_newline ();
          Cstr.post c
        end
      ) disj;
    end
  ) depsarray;

  (*
  print_endline "keep";
  List.iter (fun dl ->
    if List.length dl > 0 then begin
      let l = List.map vartoint dl in
      let c = bigor pkgarr l in
      Cstr.fprint stdout c;
      print_newline ();
      Cstr.post c
    end
  ) keep;
  *)
  print_endline "criteria";
  (*
  List.iteri (fun nameid name ->
    print_endline name;
    let pkgversions = CudfAdd.sort (Cudf.lookup_packages univ name) in
    if pkgversions <> [] && Iset.mem (Cudf.uid_by_package univ (List.hd pkgversions)) depclsure then begin
      let keepflag = List.exists (fun p -> p.Cudf.keep <> `Keep_none) pkgversions in
      let installedflag = List.exists (fun p -> p.Cudf.installed) pkgversions in
      List.iter (fun i -> Printf.printf "+> %d\n" (Cudf.uid_by_package univ i)) pkgversions;
      let pkgversionsids = List.map (fun i -> vartoint (Cudf.uid_by_package univ i)) (List.rev pkgversions) in
      print_endline "2";
      List.iter (fun i -> Printf.printf "-> %d\n" i) pkgversionsids;
      Printf.printf "pkgarrsize %d\n" size;
   
      (*
      if installedflag then
        if keepflag then
          Fd.unify removedarr.(nameid) 0
        else ( print_endline "removed";
          removepred pkgarr pkgversionsids removedarr.(nameid);
        );
        *)
   
          (*
      if keepflag && installedflag && List.length pkgversions = 1 then
        Fd.unify changedarr.(nameid) 0
      else
        let ivp,uvp = List.partition (fun i -> (Cudf.package_by_uid univ i).Cudf.installed) pkgversionsids in
        changedpred pkgarr (ivp,uvp) changedarr.(nameid) ;
  *)
   (*
      if keepflag && installedflag && List.length pkgversions = 1 then
        Printf.bprintf buff "constraint nupkg[%d] = false;\n" (nameid+1)
      else
        Printf.bprintf buff "constraint nupred (%d,{%s},nupkg,pkg);\n" (nameid+1)
        (String.concat "," (List.map (fun p -> string_of_int
        (vartoint(Cudf.uid_by_package univ p))) pkgversions));
        *)
    end
  ) (List.rev names);
  *)

  List.iter (fun nameid ->
    let name = namearr.(nameid) in
    print_endline name;
    let pkgversions = CudfAdd.sort (Cudf.lookup_packages univ name) in
    if pkgversions <> [] && Iset.mem (Cudf.uid_by_package univ (List.hd pkgversions)) depclsure then begin
      let keepflag = List.exists (fun p -> p.Cudf.keep <> `Keep_none) pkgversions in
      let installedflag = List.exists (fun p -> p.Cudf.installed) pkgversions in
      List.iter (fun i -> Printf.printf "+> %d\n" (Cudf.uid_by_package univ i)) pkgversions;
      let pkgversionsids = List.map (fun i -> vartoint (Cudf.uid_by_package univ i)) (List.rev pkgversions) in
      print_endline "2";
      List.iter (fun i -> Printf.printf "-> %d\n" i) pkgversionsids;
      Printf.printf "pkgarrsize %d\n" size;
   
      if keepflag then
        Fd.unify newarr.(newpkgmap#vartoint nameid) 1
      else
        (print_endline "new"; print_int nameid; print_int namessize;
        let c = newpred_cstr newarr.(newpkgmap#vartoint nameid) (List.map (fun i -> pkgarr.(i)) pkgversionsids) in
        Cstr.post c
      )
    end
  ) noneinst;


  print_endline "sums";
  (* let removedcriteria = Arith.sum_fd ( Array.of_list ( List.map (fun i ->
    * removedarr.(i)) atleastinst)) in *)
  let newcriteria = Arith.sum_fd ( Array.of_list ( List.map (fun i -> newarr.(newpkgmap#vartoint i)) noneinst)) in
  let changedcriteria = Arith.sum_fd changedarr in

  print_endline "cost";
  let costremoved = Var.Fd.interval 0 (List.length atleastinst) in
  let costnew = Var.Fd.interval 0 (List.length noneinst) in
  Var.Fd.fprint stdout costnew;
  print_newline ();
  Var.Fd.fprint stdout (Arith.e2fd newcriteria);
  print_newline ();

  let c = (fd2e costnew =~ newcriteria) in
  Cstr.fprint stdout c;
  print_newline ();
  Cstr.post c;

  let best = ref [||] in
  let print_sol s =
    let ins,r,u = (ref 0,ref 0,ref 0) in
    Array.iteri (fun i v ->
      let p = Cudf.package_by_uid univ (inttovar i) in
      match Fd.value v with
      |Val 0 when p.Cudf.installed -> ( incr r;
         Printf.printf "remove : %s\n" (CudfAdd.string_of_package p))
      |Val 1 when not p.Cudf.installed -> ( incr ins;
         Printf.printf "install : %s\n" (CudfAdd.string_of_package p))
      |Unk _ -> incr u
      |Val i when i > 1 -> assert false
      |_ -> () 
    ) !best;
    Printf.printf "%s: installed: %d , remove: %d, unknown: %d\n" s !ins !r !u;
  in

  print_endline "goal";
  let choose = Domain.max in 
  let goalplain = Goals.Array.forall (Goals.instantiate choose) newarr in
  let goalrec =
    let rec select a =
      try
        let i = Stack.pop varstack in
        Printf.printf "select %s\n" (CudfAdd.string_of_package (Cudf.package_by_uid univ (inttovar i)));
        match Fd.value a.(i) with
        |Val _ -> select a
        |Unk _ -> i
      with Stack.Empty -> raise Not_found
    in
    
    Goals.Array.forall (* ~select:(Goals.Array.not_instantiated_fd) *) (Goals.instantiate choose) newarr 
  in
  let labelnew = Goals.Array.labeling newarr in
  let labelpkg = Goals.Array.labeling pkgarr in

  let newarrconstr best =
    let l = Array.fold_left (fun acc v -> if Fd.value v = Val 1 then v::acc else
      acc) [] newarr in
    let c =  Arith.sum_fd ( Array.of_list l ) =~ i2e best in
    Cstr.post (c =>~~ (fd2e costnew =~ i2e best))
  in
  let goalmin = 
    Goals.minimize (labelnew &&~ labelpkg) costnew
          (fun c -> best := Array.copy pkgarr; newarrconstr c ; print_sol (Printf.sprintf "Best value so far : %d" c))
  in
  let goal = (goalmin ||~ (Goals.atomic (fun _ -> print_sol "Intermediate solution"))) &&~ labelpkg
  in
(*
  let s = Goals.solve ~control:(fun i -> print_sol (Printf.sprintf "Backtrack %d" i)) goal in
  *)
  let s = Goals.solve ~control:(fun i -> print_endline (Printf.sprintf "Backtrack %d" i)) goal in
  match !best,s with
  | [||],false -> prerr_endline "No solution"
  | _,_ | _,true -> print_sol "Final solution"

;;

let loadl to_cudf l =
  List.flatten (
    List.map (fun ((name,aop),sel) ->
      let encname =
        let n = match aop with Some a -> name^":"^a | None -> name in
        CudfAdd.encode n
      in
      match CudfAdd.cudfop sel with
      |None -> [(encname, None)]
      |Some(op,v) ->
          [(encname,Some(op,snd(to_cudf (encname,v))))]
    ) l
  )
;;

let parse_request to_cudf l =
  let open Debian in
  let parse acc s =
    if String.starts_with s "install: " then
      let rs = String.strip (snd (String.split s " ")) in
      let f = Packages.lexbuf_wrapper Packages_parser.vpkglist_top in
      { acc with Cudf.install = loadl to_cudf (f (Format822.dummy_loc,rs)) }
    else if String.starts_with s "remove: " then
      let rs = String.strip (snd (String.split s " ")) in
      let f = Packages.lexbuf_wrapper Packages_parser.vpkglist_top in
      { acc with Cudf.remove = loadl to_cudf (f (Format822.dummy_loc,rs)) } 
    else if String.starts_with s "upgrade: " then
      let rs = String.strip (snd (String.split s " ")) in
      let f = Packages.lexbuf_wrapper Packages_parser.vpkglist_top in
      { acc with Cudf.upgrade = loadl to_cudf (f (Format822.dummy_loc,rs)) }
    else acc
  in
  List.fold_left parse Cudf.default_request l
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let (input_type,implicit) =
    if OptParse.Opt.is_set Options.inputtype then 
      (Url.scheme_of_string (OptParse.Opt.get Options.inputtype),true)
    else
      (Input.guess_format [posargs],false)
  in
  let options = Options.set_options input_type in

  Boilerplate.enable_debug(OptParse.Opt.get Options.verbose);
  Boilerplate.all_quiet (OptParse.Opt.get Options.quiet);

  let global_constraints = not(OptParse.Opt.get Options.deb_ignore_essential) in

  let (fg,bg) = Options.parse_cmdline (input_type,implicit) posargs in
  let (preamble,pkgll,request,from_cudf,to_cudf) = Boilerplate.load_list ~options [fg;bg] in
  let request = 
    let l = OptParse.Opt.get Options.request in
    if l <> [] then parse_request to_cudf l else request 
  in
  let (fg_pkglist, bg_pkglist) = match pkgll with [fg;bg] -> (fg,bg) | _ -> assert false in
  let universe =
    let s = CudfAdd.to_set (fg_pkglist @ bg_pkglist) in
    let u = 
      if OptParse.Opt.get Options.latest then
        Cudf.load_universe (CudfAdd.latest (CudfAdd.Cudf_set.elements s))
      else 
        Cudf.load_universe (CudfAdd.Cudf_set.elements s)
    in
    if OptParse.Opt.get Options.trim then Depsolver.trim ~global_constraints u else u
  in

  let plist = Cudf.get_packages universe in

  let u = Cudf.load_universe plist in
  let doc = (preamble,u,request) in
  let oc =
    if OptParse.Opt.is_set Options.outfile then
      open_out (OptParse.Opt.get Options.outfile)
    else stdout
  in
  build_costraints global_constraints universe request;
  close_out oc;
;;

main ();;
