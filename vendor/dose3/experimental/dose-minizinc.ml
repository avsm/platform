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
  let description = "Cudf to minizinc encoding"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let request = Boilerplate.incr_str_list ()

  open OptParser
  add options                 ~long_name:"request" ~help:"Installation Request (can be repeated)" request;

  include Boilerplate.InputOptions
  Boilerplate.InputOptions.add_options ~default:["outfile";"latest";"trim"] options ;;

  include Boilerplate.DistribOptions;;
  Boilerplate.DistribOptions.add_options options ;;

end;;

(* -------------------------------- *)

let dependency_sat = "
predicate conflicts (int : id, set of Universe : conj, array[int] of var bool : pkg) =
  forall (i in conj) ( not (pkg[id] /\\ pkg[i]) )
;

predicate conjunction (int : id, set of Universe : conj, array[int] of var bool : pkg) =
  pkg[id] -> forall (i in conj) (pkg[i])
;

predicate disjunction (int : id, set of Universe : disj, array[int] of var bool : pkg) =
  pkg[id] -> exists (i in disj) (pkg[i])
;
";;

let dependency_mip = "
predicate conflicts (int : id, set of Universe : conj, array[int] of var bool : pkg) =
  card(conj) * bool2int(pkg[id]) + sum (i in conj) (bool2int(pkg[i])) <= card(conj)
;

predicate conjunction (int : id, set of Universe : conj, array[int] of var bool : pkg) =
  - card(conj) * bool2int(pkg[id]) + sum (i in conj) (bool2int(pkg[i])) >= 0
;

predicate disjunction (int : id, set of Universe : disj, array[int] of var bool : pkg) =
   - bool2int(pkg[id]) + sum (i in disj) (bool2int(pkg[i])) >= 0
;"
;;

let removed_sat = "
predicate removedpred (int : id, set of int : Vp, array[int] of var bool : remvpkg, array[int] of var bool : pkg) =
  (forall (i in Vp) (pkg[i] = false) -> remvpkg[id] ) /\\
  (exists (i in Vp) (pkg[i] = true) -> remvpkg[id] = false )
;
";;

let removed_mip = "
predicate removedpred (int : id, set of int : Vp, array[int] of var bool : remvpkg, array[int] of var bool : pkg) = 
  (bool2int(remvpkg[id]) + sum([bool2int(pkg[i]) | i in Vp]) 
  >= 1) /\\ 

  ( card(Vp) * bool2int(remvpkg[id])
  + sum([bool2int(pkg[i]) | i in Vp])
  <= card(Vp));
";;

let changed_sat = "
predicate changedpred (int : id, set of int : Vp, array[int] of var bool : chanpkg, array[int] of var bool : pkg) =
  let {
    set of Packages : IVp = {p | p in Vp where instpkg[p] = true},
    set of Packages : UVp = {p | p in Vp where instpkg[p] = false}
  } in
  ((forall (i in IVp) (pkg[i] = true))  /\\
    (forall (i in UVp) (pkg[i] = false)) -> chanpkg[id] = false ) /\\

  ((exists (i in IVp) (pkg[i] = false )) \\/
    (exists (i in UVp) (pkg[i] = true)) -> chanpkg[id] = true )
;
";;

let changed_mip = "
predicate changedpred (int : id, set of int : Vp, array[int] of var bool : chanpkg, array[int] of var bool : pkg) =
  let {
    set of Packages : IVp = {p | p in Vp where instpkg[p] = true},
    set of Packages : UVp = {p | p in Vp where instpkg[p] = false}
  } in
  (- bool2int(chanpkg[id])
  - sum([bool2int(pkg[i]) | i in IVp])
  + sum([bool2int(pkg[i]) | i in UVp])
  >= - card(IVp)) /\\ 
  (- card(Vp) * bool2int(chanpkg[id])
  - sum([bool2int(pkg[i]) | i in IVp])
  + sum([bool2int(pkg[i]) | i in UVp])
  <= - card(IVp));
";;

let newp_sat = "
predicate newpred (int : id, set of int : Vp, array[int] of var bool : newpkg, array[int] of var bool : pkg) =
    (forall (i in Vp) (pkg[i] = false) -> newpkg[id] = false ) /\\
      (exists (i in Vp) (pkg[i] = true) -> newpkg[id] )
;
";;

let newp_mip = "
predicate newpred (int : id, set of int : Vp, array[int] of var bool : newpkg, array[int] of var bool : pkg) = 
  (- bool2int(newpkg[id])
  + sum([bool2int(pkg[i]) | i in Vp])
  >= 0) /\\ 

  (- card(Vp) * bool2int(newpkg[id])
  + sum([bool2int(pkg[i]) | i in Vp])
  <= 0);
";;

let notuptodate_sat =  "
predicate nupred (int : id, set of int : Vp, array[int] of var bool : nupkg, array[int] of var bool : pkg) = 
  let { Packages : SupVp = min(Vp) } in
  ( (exists (i in Vp diff {SupVp}) (pkg[i] = true) /\\ pkg[SupVp] = false ) -> nupkg[id] ) /\\
  ( (forall (i in Vp diff {SupVp}) (pkg[i] = false) /\\ pkg[SupVp] = true ) -> nupkg[id] =false )
;
";;

let notuptodate_mip = "
predicate nupred (int : id, set of int : Vp, array[int] of var bool : nupkg, array[int] of var bool : pkg) = 
  let { Packages : SupVp = min(Vp) } in
  (- card(Vp) * bool2int(nupkg[id])
   - (card(Vp) - 1) * bool2int(pkg[SupVp])
   + sum (i in (Vp diff {SupVp})) (bool2int(pkg[i]))
  <= 0) /\\ 
  (- card(Vp) * bool2int(nupkg[id])
   - (card(Vp) - 1) * bool2int(pkg[SupVp])
   + sum (i in (Vp diff {SupVp})) (bool2int(pkg[i]))
  >= - card(Vp) +1);
";;

let preamble = "
%%declarations

int: univsize ;
int: pnamesize ;
int: criteriasize ;

set of 1..univsize : Universe = 1..univsize;
set of 1..(univsize-1) : Packages = 1..(univsize-1);
set of 1..pnamesize : Pnames = 1..pnamesize;
set of 1..criteriasize : Criteria = 1..criteriasize;
set of 1..(univsize-1) : Closure ;

array[Pnames] of var bool : remvpkg;
array[Pnames] of var bool : chanpkg;
array[Pnames] of var bool : newpkg;
array[Pnames] of var bool : nupkg;

array[Universe] of var bool : pkg;
array[Packages] of bool : instpkg;
array[Criteria] of var int: criteria;
";;

let optimization = "
int : w4 = 1;
int : w3 = ub(w4 * criteria[4]) + 1;
int : w2 = ub(w3 * criteria[3] + criteria[4]) + 1 ;
int : w1 = ub(w2 * criteria[2] + w3 * criteria[3] + criteria[4]) + 1;
array[Criteria] of int : weigths = [w1, w2, - w3, w4];

%%pretty print
array[Packages] of string: realpname;

output 
[ if fix(pkg[id]) then show (\"install: \" ++ realpname[id] ++ \"\\n\") else \"\" endif | id in Packages] ++
[ \"removed:\"     ++ show(criteria[1]) ++ \"\\n\" ++
  \"changed:\"     ++ show(criteria[2]) ++ \"\\n\" ++
  \"new:\"         ++ show(criteria[3]) ++ \"\\n\" ++
  \"notuptodate:\" ++ show(criteria[4]) ++ \"\\n\" ];
";;

let solve = "
ann : searchann  = 
      bool_search(
        [pkg[i] | i in Closure ],
        input_order, indomain_min, complete);

solve :: searchann minimize sum (i in Criteria) (weigths[i] * criteria[i]);
";;

let predicates_sat =
  Printf.sprintf "%s\n%s\n%s\n%s\n%s\n"
  dependency_sat
  removed_sat
  changed_sat
  newp_sat
  notuptodate_sat
;;

let satencoding =
  Printf.sprintf "%s\n%s\n%s\n%s\n" 
  predicates_sat
  preamble
  optimization
  solve
;;

let output_minizinc ?(global_constraints=true) ?(criteria=[1;2;3;4;]) (pre,univ,req) = 
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

  let depclsure = to_set (
    Depsolver_int.dependency_closure_cache cudfpool (searchvars @ installed @ (List.flatten keep))
    )
  in
  let pkgmap = Depsolver_int.init_map (Iset.elements depclsure) univ in
  let size = (Iset.cardinal depclsure) in
  let vartoint i = (pkgmap#vartoint i) + 1 in

  (* atleastinst : at lest one version of a package is installed
   * noneinst : none versions are installed *)
  let atleastinst, names, namessize, noneinst =
    let h = Hashtbl.create (Cudf.universe_size univ) in
    Cudf.iter_packages (fun p ->
      if not (Hashtbl.mem h p.Cudf.package) then
        let l = List.map (Cudf.uid_by_package univ) (Cudf.lookup_packages univ p.Cudf.package) in
        Hashtbl.add h p.Cudf.package l
    ) univ;
    Hashtbl.fold (fun k vl (atl,l,i,nonel) -> 
      let j = i + 1 in
      if not (Iset.is_empty (Iset.inter (to_set vl) depclsure)) then
        let pl = List.map (Cudf.package_by_uid univ) vl in
        if List.exists (fun p -> p.Cudf.installed) pl then
          (j::atl,k::l,j,nonel)
        else
          (atl,k::l,j,j::nonel)
      else (atl,l,j,nonel)
    ) h ([],[],0,[])
  in

  Printf.bprintf buff "%s\n" satencoding;

  Printf.bprintf buff "univsize = %d;\n" (size+1);
  Printf.bprintf buff "pnamesize = %d;\n" (List.length names);
  Printf.bprintf buff "criteriasize = %d;\n" (List.length criteria);

  Printf.bprintf buff "Closure = {%s};"
  (String.concat "," (List.map string_of_int (List.map vartoint (searchvars @ installed @ (List.flatten keep)))));

  Printf.bprintf buff "
criteria = [
%% Package names such that at least one version is installed
%%  sum([bool2int(remvpkg[id]) | id in [i | i in Pnames where exists (q in pname[i]) (instpkg[q] = true)]]),
  %s,
  sum([bool2int(chanpkg[id]) | id in Pnames]),
%% Package names such that at none of the versions are installed
%%  sum([bool2int(newpkg[i]) | i in Pnames where (sum ([bool2int(instpkg[q]) | q in pname[i]]) = 0)]),
  %s,
  sum([bool2int(nupkg[id]) | id in Pnames]),
];\n" 
(if List.length atleastinst > 0 then 
  Printf.sprintf "sum([bool2int(remvpkg[id]) | id in {%s}])" 
  (String.concat "," (List.map (fun i -> string_of_int (i+1)) atleastinst)) 
 else "0")
(if List.length noneinst > 0 then
  Printf.sprintf "sum([bool2int(newpkg[i]) | i in {%s}])"
  (String.concat "," (List.map (fun i -> string_of_int (i+1)) noneinst)) 
 else "0")
;

  Printf.bprintf buff "
%% the number of changed names cannot be larger then the number of
%% new packages + removed packages + notuptodate packages
constraint criteria[2] >= criteria[1] + criteria[3] + criteria[4];
constraint criteria[2] <= card(Pnames);
constraint criteria[2] >= 0;

%% the number of removed names cannot be larger then the number
%% of packages currently installed.
constraint criteria[1] <= %d ;
constraint criteria[1] >= 0;
"
(List.length atleastinst)
;


  Printf.bprintf buff "\n%%data\n";
  Printf.bprintf buff "realpname = [%s];\n\n" (
    String.concat "," (
      List.map 
      (Printf.sprintf "\"%s\"") (
        List.map (fun id -> 
          CudfAdd.string_of_package (Cudf.package_by_uid univ (pkgmap#inttovar id))
        ) (Util.range 0 (size-1))
      )
    )
  );

  Printf.bprintf buff "instpkg = [%s];\n\n" (
    String.concat "," (
      List.map (fun id ->
        let pkg = Cudf.package_by_uid univ (pkgmap#inttovar id) in
        string_of_bool pkg.Cudf.installed
      ) (Util.range 0 (size-1))
    )
  );

  Printf.bprintf buff "\n%%constraints\n";
  List.iteri (fun nameid name ->
    let pkgversions = CudfAdd.sort (Cudf.lookup_packages univ name) in
    let keepflag = List.exists (fun p -> p.Cudf.keep <> `Keep_none) pkgversions in
    let installedflag = List.exists (fun p -> p.Cudf.installed) pkgversions in

    if installedflag then
      if keepflag then
        Printf.bprintf buff "constraint remvpkg[%d] = false;\n" (nameid+1)
      else
        Printf.bprintf buff "constraint removedpred (%d,{%s},remvpkg,pkg);\n" (nameid+1)
        (String.concat "," (List.map (fun p -> string_of_int
        (vartoint (Cudf.uid_by_package univ p))) pkgversions));

    if keepflag && installedflag && List.length pkgversions = 1 then
      Printf.bprintf buff "constraint chanpkg[%d] = false;\n" (nameid+1)
    else
      Printf.bprintf buff "constraint changedpred (%d,{%s},chanpkg,pkg);\n" (nameid+1)
      (String.concat "," (List.map (fun p -> string_of_int
      (vartoint (Cudf.uid_by_package univ p))) pkgversions));

    if keepflag && installedflag && List.length pkgversions = 1 then
      Printf.bprintf buff "constraint nupkg[%d] = false;\n" (nameid+1)
    else
      Printf.bprintf buff "constraint nupred (%d,{%s},nupkg,pkg);\n" (nameid+1)
      (String.concat "," (List.map (fun p -> string_of_int
      (vartoint(Cudf.uid_by_package univ p))) pkgversions));

    if keepflag then
      Printf.bprintf buff "constraint newpkg[%d] = true;\n" (nameid+1)
    else
      Printf.bprintf buff "constraint newpred (%d,{%s},newpkg,pkg);\n" (nameid+1)
      (String.concat "," (List.map (fun p -> string_of_int
      (vartoint(Cudf.uid_by_package univ p))) pkgversions))

  ) (List.rev names);

  Array.iteri (fun id cl ->
    if Iset.mem id depclsure && List.length cl > 0 then
      try
        Printf.bprintf buff "constraint conflicts (%d, {%s},pkg);\n"
        (vartoint id)
        (String.concat "," (List.map (fun i -> string_of_int
        (vartoint i)) cl))
      with Not_found -> () (* if the conflict is outside the depclsure, we ignore it *)
  ) confarray ;

  Array.iteri (fun id dll ->
    if Iset.mem id depclsure then begin
      let conj,disj = List.partition (fun l -> List.length l = 1) dll in
      if List.length (List.flatten conj) > 0 then
        Printf.bprintf buff "constraint conjunction (%d, {%s},pkg);\n"
        (vartoint id)
        (String.concat "," (List.map (fun i -> string_of_int
        (vartoint i)) (List.flatten conj)));

      List.iter (fun dl ->
        if List.length dl > 0 then
          Printf.bprintf buff "constraint disjunction (%d, {%s},pkg);\n"
          (vartoint id)
          (String.concat "," (List.map (fun i -> string_of_int
          (vartoint i)) (List.rev dl)));
      ) disj;
    end
  ) depsarray;

  let (dll,_) = (Depsolver_int.strip_cudf_pool cudfpool).(size) in
  if List.length dll > 0 then
    Printf.bprintf buff "constraint pkg[%d] -> (%s);\n" size (
      String.concat " /\\ " (
        List.map (fun dl ->
          Printf.sprintf "( %s )" (
            String.concat " \\/ " (List.map (fun i -> 
              Printf.sprintf "pkg[%d]"
            (vartoint i)) dl) 
          )
        ) keep
      )
    );

  Printf.bprintf buff "\n%%keep packages\n";
  Printf.bprintf buff "constraint pkg[%d] = true;\n" size;

  Printf.bprintf buff "\n%%request\n";

  if req.Cudf.install <> [] then begin
    Printf.bprintf buff "%%install\n";
    Printf.bprintf buff "constraint %s;\n" (
      String.concat " /\\ " (
        List.map (fun vpkg ->
          Printf.sprintf "( %s )" (
            String.concat " \\/ " (
              List.map (fun id ->
                Printf.sprintf "pkg[%d] = true" (vartoint id)
              ) (CudfAdd.resolve_vpkg_int univ vpkg)
            )
          )
        ) req.Cudf.install
      )
    )
  end;

  if req.Cudf.remove <> [] then begin
    Printf.bprintf buff "%%remove\n";
    Printf.bprintf buff "constraint %s;\n" (
      String.concat " /\\ " (
        List.map (fun id ->
          Printf.sprintf "(pkg[%d] = false)" (vartoint id+1)
        ) (CudfAdd.resolve_vpkgs_int univ req.Cudf.remove)
      )
    )
  end;

  if req.Cudf.upgrade <> [] then begin
    Printf.bprintf buff "%%upgrade\n";
    Printf.bprintf buff "constraint %s;\n" (
      String.concat " /\\ " (
        List.map (fun ((name,_) as vpkg) ->
          let pkginstlist = List.map (Cudf.uid_by_package univ) (Cudf.get_installed univ name) in
          Printf.sprintf "( %s )" (
            String.concat " \\/ " (
              List.filter_map (fun id ->
                if not (List.mem id pkginstlist) then
                  (* what if I cannot upgrade any packages ? *)
                  Some (Printf.sprintf "pkg[%d]" (vartoint id+1))
                else None
              ) (CudfAdd.resolve_vpkg_int univ vpkg)
            )
          )
        ) req.Cudf.upgrade
      )
    );
  end;

  Buffer.contents buff
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
  Printf.fprintf oc "%s" (output_minizinc ~global_constraints doc);
  close_out oc;
;;

main ();;
