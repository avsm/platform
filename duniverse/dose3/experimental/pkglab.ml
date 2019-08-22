(*

(run-caml "/home/dicosmo/mancoosi/trunk/dose3/experimental/pkglab.top -I /home/dicosmo/mancoosi/trunk/dose3/_build/algo -I /home/dicosmo/mancoosi/trunk/dose3/_build/common -I /home/dicosmo/mancoosi/trunk/dose3/_build/libcudf -I /home/dicosmo/mancoosi/trunk/dose3/_build/experimental -I /home/dicosmo/mancoosi/trunk/dose3/_build/applications/boilerplates -I /usr/lib/ocaml/extLib -I /usr/lib/ocaml/ocamlgraph")

 *)

open Common
open CudfAdd
open Algo

module Package =
struct
	type t = Cudf.package

	let compare = Cudf.(<%)
end;;
module PS = Set.Make(Package);;
open PS;;

let list_to_set l =
	List.fold_left (fun acc p -> PS.add p acc) PS.empty l;;

let set_to_list s =
	PS.elements s;;

(*** [load] reads a source of package metadata from [url] and returns a universe
     together with the efficient internal representation (in mdf format) 
     which is necessary to quickly perform operations on the dependency graph
*)

(*** [univ_ctx] holds all relevant data structures for manipulating a package universe 
     
*)

type univ_ctx = {univ:Cudf.universe;mdf:Common.Mdf.universe;solcudf:Algo.Depsolver.solver;solmdf:Algo.Depsolver_int.solver};;

let load url =
begin
	let (univ, _, _) = Boilerplate.load_universe [url] in
	let mdf = Mdf.load_from_universe univ in
	let slv_mdf = Depsolver_int.init_solver mdf.Mdf.index in
        let slv_cudf = Depsolver.load ~check:false univ in
	{univ=univ; mdf=mdf; solcudf=slv_cudf; solmdf=slv_mdf}
end;;

let original_version p =
	Cudf.lookup_package_property p "number";;

let get_packages ?filter {univ=u} =
	list_to_set (Cudf.get_packages ?filter u);;

let search_packages {univ=u} re =
begin
  let rex = Pcre.regexp re in
  list_to_set (Cudf.get_packages 
    ~filter:(fun p -> Pcre.pmatch ~rex (Cudf_types_pp.string_of_pkgname p.Cudf.package))
		u)
end;;

let trim {univ=u; mdf=mdf} = 
begin
	let trimmed_pkgs = ref [] in
	let callback (res, req) =
	begin
		match res with
		| Diagnostic_int.Success _ ->
			begin
				match req with
				| Diagnostic_int.Sng p ->
						trimmed_pkgs := (mdf.Mdf.maps.map#inttovar p)::!trimmed_pkgs
				| _ -> assert false
			end
		| Diagnostic_int.Failure _ -> ()
	end in
  ignore (Depsolver_int.univcheck ~callback mdf);
	let u' = Cudf.load_universe !trimmed_pkgs in
	let mdf' = Mdf.load_from_universe u' in
	let slv' = Depsolver_int.init_solver mdf'.Mdf.index in
	let slv'' = Depsolver.load ~check:false u' in
  {univ=u'; mdf=mdf'; solcudf=slv'';solmdf=slv'}
end;;

let cone ?maxdepth ?conjunctive {mdf=mdf} ps =
begin
  let maps = mdf.Mdf.maps in
  let idlist = List.rev_map maps.map#vartoint (set_to_list ps) in
  let closure =
		Depsolver_int.dependency_closure ?maxdepth ?conjunctive mdf idlist in
  list_to_set (List.rev_map maps.map#inttovar closure)
end;;

let rev_cone ?maxdepth {mdf=mdf} ps =
begin
  let maps = mdf.Mdf.maps in
  let idlist = List.rev_map maps.map#vartoint (set_to_list ps) in
  let reverse = Depsolver_int.reverse_dependencies mdf in
  let closure =
		Depsolver_int.reverse_dependency_closure ?maxdepth reverse idlist in
  list_to_set (List.rev_map maps.map#inttovar closure)
end;;

let filter_packages f {univ=u} =
	Cudf.fold_packages (fun acc p ->
		if f p then PS.add p acc
		else acc
	) PS.empty u;;

let provides_set {univ=u; mdf=mdf} p =
	List.fold_left (fun acc (pn, pv) ->
		List.fold_left (fun acc' p' ->
			PS.add p' acc'
		) acc (mdf.Mdf.maps.who_provides (pn, (pv :> Cudf_types.constr)))
	) PS.empty p.Cudf.provides;;

let conflicts_set {univ=u} p =
	List.fold_left (fun acc (cn, cv) ->
		List.fold_left (fun acc' c ->
			PS.add c acc'
		) acc (Cudf.lookup_packages ~filter:cv u cn)
	) PS.empty p.Cudf.conflicts;;

let check {univ=u;mdf=mdf;solmdf=slv} (p:PS.elt) =
begin
	let req = Diagnostic_int.Sng (mdf.Mdf.maps.map#vartoint p) in
	let res = Depsolver_int.solve slv req in
		match res with
		| Diagnostic_int.Success _ -> true
		| Diagnostic_int.Failure _ -> false
end;;	

let check_together {univ=u;mdf=mdf;solmdf=slv} s =
begin
	let req = Diagnostic_int.Lst (List.rev_map mdf.Mdf.maps.map#vartoint (set_to_list s)) in
	let res = Depsolver_int.solve slv req in
		match res with
		| Diagnostic_int.Success _ -> true
		| Diagnostic_int.Failure _ -> false
end;;

let install {univ=u;mdf=mdf;solmdf=slv} s =
begin
	let req = Diagnostic_int.Lst (List.rev_map mdf.Mdf.maps.map#vartoint (set_to_list s)) in
	let res = Depsolver_int.solve slv req in
		match res with
		| Diagnostic_int.Success f -> list_to_set (List.rev_map mdf.Mdf.maps.map#inttovar
				(f ~all:false ()))
		| Diagnostic_int.Failure _ -> PS.empty
end;;

let is_strong_dep {mdf=mdf;solmdf=slv} (p1:PS.elt) (p2:PS.elt) =
begin
	let i1 = mdf.Mdf.maps.map#vartoint p1 in
	let i2 = mdf.Mdf.maps.map#vartoint p2 in
	Strongdeps_int.strong_depends slv i1 i2
end;;

(* TODO: add
 - impactset
 - strongconflicts
 - enregistrement pour stateful time optimization
 *) 


(* dependency graphs *)

module G = Defaultgraphs.PackageGraph.G
module O = Defaultgraphs.GraphOper(G)
module S = Statistics.Make(G)

let depgraph (u, _) = Defaultgraphs.PackageGraph.dependency_graph u;;

type depgraphstats = 
    {nvertex:int;
     nedges:int;
     avgin:float;
     avgout:float;
     nsccomps:int;
     nwccomps:int;
     avgsccomp:float;
     avgwccomp:float;
     clustcoeff:float;
     avgshortpath:float;
   density:float;
   avgtwostepreach:float;
   centralityOutDeg:float;
   centralityInDeg:float;
   };;

let stats gr = 
  {nvertex=G.nb_vertex gr;
   nedges=G.nb_edges gr;
   avgin=S.averageInDegree gr;
   avgout=S.averageOutDegree gr;
   nsccomps=S.numberComponentsSC gr;
   nwccomps=S.numberComponentsWC gr;
   avgsccomp=S.averageComponentsSC gr;
   avgwccomp=S.averageComponentsWC gr;
   clustcoeff=S.clustering gr;
   avgshortpath=S.averageShortestPathLength gr;
   density=S.density gr;
   avgtwostepreach=S.averageTwoStepReach gr;
   centralityOutDeg=S.centralityOutDegree gr;
   centralityInDeg=S.centralityInDegree gr;
 };;


let saveplot h outfile =
  let out = open_out outfile in
  Printf.fprintf out "#count degree\n" ;
  Hashtbl.iter (fun n i -> Printf.fprintf out "%d %d\n" i n ) h;
  close_out out
;;

let saveplot2 h outfile =
  let out = open_out outfile in
  Printf.fprintf out "#count in_degree out_degree\n" ;
  Hashtbl.iter (fun (n1, n2) i -> Printf.fprintf out "%d %d %d\n" i n1 n2) h;
  close_out out
;;

let scatterplots prefix gr = 
  saveplot2 (S.scatteredPlotBoth gr) (prefix^"degree.data");
  saveplot (S.scatteredPlotIn gr) (prefix^"indegree.data");
  saveplot (S.scatteredPlotOut gr) (prefix^"outdegree.data")
;;


(* examples:
(* load a universe from a Debian Packages file *)
let u = load "deb:///var/lib/apt/lists/ftp.fr.debian.org_debian_dists_squeeze_main_binary-amd64_Packages";;
(* all packages whose names starts with ocaml *)
let ocamlunits= search_packages u "^ocaml";;
(* union of the cones of 2 OCaml packages *)
let a = PS.choose ocamlunits;;
let b = PS.choose (PS.remove a ocamlunits);;
let c12 = cone u (PS.add b (PS.singleton a));;
(* packages with self conflicts on provides *)
let self_confl_provides = filter_packages (fun p -> not (PS.is_empty (PS.inter (provides_set u p) (conflicts_set u p)))) u;;
(* compute list of packages with the size of their cones *)
let pkgl = get_packages u;;

let cl = let t = Timer.create "seq" in
  Timer.enable "seq"; Timer.start t;
  let l = List.map (fun p -> (p,cone u (PS.singleton p))) (PS.elements pkgl)
  in Timer.stop t (); Timer.pp_timer Format.std_formatter t;
     l
;;

let cl' = parmap (fun p -> (p,cone u (PS.singleton p))) (PS.elements pkgl) ~ncores:8;;

let conesizes = List.map 
   (fun (p,cone) -> 
   (p.Cudf.package,original_version p, PS.cardinal cone)) 
   cl;;

(* in reverse order of cone size *)

let conesizes' = List.sort (fun (_,_,n) (_,_,n') -> n'-n) conesizes;;

(* let's compute the install size of each of the packages, computed by Jerome's solver *)

let inst_list = List.map (fun p -> (p,install u (PS.singleton p))) (PS.elements pkgl);;

let inst_sizes = List.map 
   (fun (p,is) -> (p.Cudf.package,original_version p, PS.cardinal is)) inst_list
;;

(* in reverse order of install size *)

let inst_sizes' = List.sort (fun (_,_,n) (_,_,n') -> n'-n) inst_sizes;;

(* check some packages *)

 *)

