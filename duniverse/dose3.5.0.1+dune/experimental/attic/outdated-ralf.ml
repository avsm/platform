(****************************************************************************)
(*  Copyright (C) 2010, 2011 Ralf Treinen <treinen@pps.jussieu.fr>          *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU General Public License as published by     *)
(* the Free Software Foundation, either version 3 of the License, or        *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU General Public License for more details.                             *)
(*                                                                          *)
(* You should have received a copy of the GNU General Public License        *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(****************************************************************************)

open Debian
open Common
open Algo
open Diagnostic

let debug fmt = Util.make_debug "" fmt
let info fmt = Util.make_info "" fmt
let warning fmt = Util.make_warning "" fmt

let debug_switch = false

module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse
  let description =
    "Report packages that aren't installable in any futures of a repository"
  let options = OptParser.make ~description 

  include Boilerplate.MakeOptions(struct let options = options end)

  let explain = StdOpt.store_true ()
  let architecture = StdOpt.str_option ()
  let cudf_output = StdOpt.str_option ()

  open OptParser
  add options ~short_name:'e' ~long_name:"explain"
    ~help:"Explain the results" explain;
  add options ~short_name:'a' ~long_name:"architecture"
    ~help:"Set the default architecture" architecture;
  add options ~long_name:"cudf-to"
    ~help:"Dump CUDF to file" cudf_output;
end

let sourcename_of_package p = 
  try
    match List.assoc "source" p.Cudf.pkg_extra with
	`String s -> s
      | _ -> failwith "CUDF: source field of wrong type"
  with
      Not_found -> failwith "CUDF: source field missing"
;;

let sourceversion_of_package p = 
  try
    match List.assoc "sourceversion" p.Cudf.pkg_extra with
	`String s -> s
      | _ -> failwith "CUDF: source version field of wrong type"
  with
      Not_found -> failwith "CUDF: source version missing"
;;

let debversion_of_package p = 
  try
    match List.assoc "number" p.Cudf.pkg_extra with
	`String s -> s
      | _ -> failwith "debian version of wrong type"
  with
      Not_found -> failwith
	("CUDF: debian version missing for package \""^p.Cudf.package^
	    "\", version "^(string_of_int p.Cudf.version))
;;


(* normalize a debian version string by removing epoch and bin-NMU *)
let normalize s =
  let chop_binnmu s =
  (* chops a possible bin-NMU suffix from a debian version string *)
    try
      Str.string_before s
	(Str.search_backward (Str.regexp "\\+b[0-9]+$") s ((String.length s)-1))
    with
	Not_found -> s
  and chop_epoch s =
  (* chops a possible epoch from a debian version string *)
    if Str.string_match (Str.regexp "[0-9]+:") s 0
    then Str.string_after s (Str.match_end ())
    else s
  in
  chop_epoch (chop_binnmu s)
;;

(* get the epoch of a debian version (including the ":" separator) if it  *)
(* carries one, otherwise the empty string.                               *)
let epoch debversion =
  if Str.string_match (Str.regexp "[0-9]+:") debversion 0
  then Str.matched_string debversion
  else ""
;;

(**************************************************************************)

(* The cluster of a binary package p is the pair (s,w) where              *)
(* -s is the source package of p;                                         *)
(* -w is the debian version of p without epoch and bin-nmu.               *)

let cluster_of package = (
  sourcename_of_package package, normalize (debversion_of_package package)
);;

(* add to a package the constraints that synchronise it with its cluster *)
let synchronise_package cluster_size_table package  =
  let (s,w) as cluster = cluster_of package
  in
  if Hashtbl.find cluster_size_table cluster > 1
  then
    let clustername = "src/"^s^"/"^w
    and clusterversion = 1
    in
    {package with
      Cudf.provides = 
	(clustername, Some (`Eq, clusterversion))::package.Cudf.provides;
      Cudf.conflicts =
	(clustername, Some (`Neq, clusterversion))::package.Cudf.conflicts; 
       }
  else package
;;

(****************************************************************************)

(* renumber all packages mentioned in the package list in any of the fields *)
(* version, conflicts, depends, provides.                                   *)
(* translation is a hash table mapping a package name to an association     *)
(* list that maps old version numbers to new version numbers.               *)
let renumber_packages translation current_version_per_package package_list =
  let translate_version package version =
    List.assoc version (Hashtbl.find translation package)
  in
  let translate_constrained = function
    | (name,None) as c -> c
    | (name,Some(relation,version)) ->
      try
	(name,Some(relation,translate_version name version))
      with
	  (* a version may not be in the translation list, this happens *)
	  (* exactly when we have a relation to an existing package     *)
	  (* and the version in the constraint is <= then the currently *)
	  (* existing version of that package.                          *)
	  Not_found ->
	    try 
	      let existing_version =
		Hashtbl.find current_version_per_package name
	      in 
	      if existing_version = version
	      then
		(* existing versions of packages that are mentionend *)
		(* in constraints are always mapped to 1             *)
		(name,Some(relation,1))
	      else
		begin
		  assert (version < existing_version);
		  match relation with
		    | `Geq | `Gt | `Neq -> (name,None)
		    | `Leq | `Lt | `Eq  -> (name,Some(`Eq,0))
		end
	    with Not_found -> assert false
	      
  in
  List.map
    (fun p -> { p with 
      Cudf.version = translate_version p.Cudf.package p.Cudf.version;
      Cudf.conflicts = List.map translate_constrained p.Cudf.conflicts;
      Cudf.provides = List.map translate_constrained p.Cudf.provides;
      Cudf.depends = List.map (List.map translate_constrained) p.Cudf.depends;
     }
    )
    package_list
;;

(* like List.map, but the function to be applied gets as first argument *)
(* the position in the list (starting on 0), and as second argument the *)
(* current list element.                                                *)
(* listmap_with_index f [x0; ... ; xn] = [f 0 x0; ... ; f n xn]         *) 
let listmap_with_index f = 
  let rec listmap_with_index_aux i = function
    | [] -> []
    | h::r -> (f i h)::(listmap_with_index_aux (i+1) r)
  in listmap_with_index_aux 0
;;

(* returns the index of the first occurrence of an element in a list,  *)
(* starting on 0. Failure if it does not occur in the list.            *)
(* index_in_list xi [x0; ... ; xi; ... ; xn] = i                       *)
let rec index_in_list x = function
  | h::r -> if h=x then 0 else 1+(index_in_list x r)
  | [] -> failwith ("Can't find position in list: "^x)
;;

(****************************************************************************)

let main () =
  let posargs =
    let args = OptParse.OptParser.parse_argv Options.options in
    match args with
    |[] -> ["deb://-"]
    |l -> List.map ((^) "deb://") l
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];
  
  let default_arch = OptParse.Opt.opt Options.architecture in

(****************************************************************************)
(* 1: read the universe, and retain a list of packages that contains only   *)
(* the latest version of any package. In the sequele we will only use this  *)
(* list of packages. The restriction to the latest version of a package     *)
(* guarantees that we can build functions from binary package names to      *)
(* package properties by looking at a specific package with a given name.   *)

  let (original_universe, original_from_cudf,_original_to_cudf) =
    Boilerplate.load_universe ~default_arch posargs in

  (* maps each binary package name to the latest cudf version *)
  let orig_version_per_binname =
    Hashtbl.create (Cudf.universe_size original_universe) in

  (* list of packages, contains only the latest version of each package.    *)
  let purged_package_list =

      let cruft = Hashtbl.create ((Cudf.universe_size original_universe)/100)
      (* maps each obsolete (package name, package cudf version) to the *)
      (* newer debian version *)
      in
      Cudf.iter_packages
	(fun p ->
	  let name = p.Cudf.package
	  and cudf_version = p.Cudf.version
	  and deb_version = debversion_of_package p
	  in begin
	    try
	      let old_cudf_version = Hashtbl.find orig_version_per_binname name
	      in 
	      if old_cudf_version < cudf_version 
	      then begin
		Hashtbl.add cruft (name,old_cudf_version) deb_version;
		Hashtbl.replace orig_version_per_binname name cudf_version;
	      end
	      else if cudf_version < old_cudf_version
	      then
		let old_deb_version = debversion_of_package
		  (Cudf.lookup_package original_universe
		     (name,old_cudf_version))
		in Hashtbl.add cruft (name,cudf_version) old_deb_version
	      else assert false
	    with Not_found -> Hashtbl.add orig_version_per_binname name cudf_version;
	  end)
	original_universe;
      
      Cudf.fold_packages
	(fun acc p ->
	  if  Hashtbl.mem cruft (p.Cudf.package,p.Cudf.version)
	  then acc
	  else p::acc)
	[]
	original_universe

  in
  let orig_debian_version_of_binname binname =
    let cudf_version =
      Hashtbl.find orig_version_per_binname binname 
    in snd(original_from_cudf(binname,cudf_version))
    
  in
  
  let number_current_packages = List.length purged_package_list
  
  in
     
(**************************************************************************)
(* 2: gather various informations associated to binary package names      *)
(* and to clusters. A cluster is identified by a source package name      *)
(* plus a debian version string.                                          *)

  let size_per_cluster = Hashtbl.create (number_current_packages/2)
  (* associates to each cluster its size.                                *)
  and cluster_per_binname = Hashtbl.create number_current_packages
  (* associates to each binary package name its cluster.                 *)
  in
  List.iter
    (fun package ->
      let cluster = cluster_of package
      in
      Hashtbl.add cluster_per_binname package.Cudf.package cluster;
      try let oldcount = Hashtbl.find size_per_cluster cluster
	  in Hashtbl.replace size_per_cluster cluster (oldcount+1)
      with Not_found -> Hashtbl.add size_per_cluster cluster 1
    )
    purged_package_list;
  
  let referred_versions_per_binname =
    Hashtbl.create (number_current_packages/2)
  (* associates to each binary package name the list of original cudf    *)
  (* versions that are used in constraints mentioning that package, and  *)
  (* that are strictly newer than the current package. If a              *)
  (* package is never mentionend in a constraint than that package name  *)
  (* is not bound in that table. If a package is ony mentionend without  *)
  (* version constraint then this package name is bound to [].           *)
  and referred_debversions_per_binname =
    Hashtbl.create number_current_packages
  (* associates to each binary package name the list of debian           *)
  (* versions that are used in constraints mentioning that package, and  *)
  (* that are strictly newer than the current package. If a              *)
  (* package is never mentionend in a constraint that that package name  *)
  (* is not bound in that table. If a package is ony mentionend without  *)
  (* version constraint then this package name is bound to [].           *)
  and referred_debversions_per_cluster =
    Hashtbl.create (Hashtbl.length size_per_cluster)
  (* associates to each cluster c the list of debian versions v where    *)
  (* - there is some constraint on a package p with debian version w     *)
  (* - c is the cluster of p                                             *)
  (* - w is strictly newer than the current version of p                 *)
  (* - v is the normalized form of w (without epoch and binnmu)          *)
  (* when there is no such (p,v) for cluster c then c the table contains *)
  (* no binding for c.                                                   *)
  in
  let add table name version =
    try let l = Hashtbl.find table name	in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
  and add_without_version table name =
    if not (Hashtbl.mem table name)
    then Hashtbl.add table name (ref [])
  and iter_constraints f package =
    (* iterate f over all constraints that ocur in the package *)
    begin
      List.iter 
	(function clause -> List.iter f clause)
	package.Cudf.depends;
      List.iter f package.Cudf.conflicts
    end
  in
  List.iter
    (iter_constraints
       (fun (package,constr) ->
	 match constr with
	   | None ->
	     add_without_version referred_versions_per_binname package;
	     add_without_version referred_debversions_per_binname package
	   | Some(_relation,version) ->
	     try
	       let current_version =
		 Hashtbl.find orig_version_per_binname package
	       in
	       if version > current_version then
		 let debversion=
		   snd(original_from_cudf(package,version))
		 in
		 begin
		   add referred_versions_per_binname package version;
		   add referred_debversions_per_binname package debversion;
		   add referred_debversions_per_cluster
		     (Hashtbl.find cluster_per_binname package)
		     (normalize debversion)
		 end
	     with
		 Not_found ->
		   (* the package is missing, it hence does not belong to *)
		   (* any cluster.                                        *)
		   add referred_versions_per_binname package version
       ))
    purged_package_list;
  
  (* Complete the list of referred debian versions per cluster by adding *)
  (* debian versions that are "indirectly referred". This happens when a *)
  (* debian version of another package in the sqme cluster is referred.  *)
  (* Both binary packages may have different epchs, we hence have to     *)
  (* translate the debian versions by adapting epochs.                   *)
  Hashtbl.iter
    (fun binname versions ->
      versions := !versions @
	(try
	   let debv = orig_debian_version_of_binname binname
	   in
	   let ep = epoch debv
	   and ndebv = normalize debv
	   in
	   List.map
	     (fun s -> ep^s)
	     (List.filter
		(fun v -> Debian.Version.compare v ndebv > 0)
		(!(Hashtbl.find referred_debversions_per_cluster
		     (Hashtbl.find cluster_per_binname binname))))
	 with
	     Not_found -> []))
    referred_debversions_per_binname;
      
  (* Finally, sort the different lists of referred vesions in ascending  *)
  (* order.                                                              *)
  Hashtbl.iter
    (fun _package versions ->
      let l = ExtLib.List.unique 
	(ExtLib.List.sort ~cmp:Debian.Version.compare (!versions ))
      in versions := l)
    referred_debversions_per_cluster;

  Hashtbl.iter
    (fun _package versions ->
      let l = ExtLib.List.unique 
	(ExtLib.List.sort ~cmp:Debian.Version.compare (!versions ))
      in versions := l)
    referred_debversions_per_binname;

  Hashtbl.iter
    (fun _package versions ->
      versions := (ExtLib.List.unique (ExtLib.List.sort (!versions))))
    referred_versions_per_binname;

  if debug_switch then begin
    print_string "debian versions per cluster:\n";
    Hashtbl.iter
      (fun (s,v) versions ->
	print_string s;
	print_char ':';
	print_string v;
	print_string ": ";
	List.iter
          (fun v -> print_string v; print_string ", ")
          (!versions);
	print_newline ()
      )
      referred_debversions_per_cluster;
    print_string "debian versions per binary package name:\n";
    Hashtbl.iter
      (fun s versions ->
	print_string s;
	print_string ": ";
	List.iter
          (fun v -> print_string v; print_string ", ")
          (!versions);
	print_newline ()
      )
      referred_debversions_per_binname;
  end;

(**************************************************************************)
(* 3: determine how packages have to be renumbered, and what are the new  *)
(* packages that have to be created.                                      *)

  let translation_table = Hashtbl.create number_current_packages
  (* associates to every binary package name (existing or missing) an     *)
  (* association list that associates to each original cudf version the   *)
  (*  new cudf version.                                                   *)
  in
  Hashtbl.iter 
    (fun pname cudf_versions ->
      if Hashtbl.mem orig_version_per_binname pname
      then 
	(* the package currenly exists, w have to look at its cluster *)
	Hashtbl.add translation_table pname
	  (
	    (* the existing version of the package gets renumbered to 1 *)
	    ((Hashtbl.find orig_version_per_binname pname),1)::
	      (* for each of the mentionend cudf versions we have to find  *)
	      (* the corresponding debian version. Then we look up where   *)
              (* that debian version ocurs in the list of all referred     *)
	      (* debian versions of that package.                          *)
	      (let referred_debian_versions =
		 Hashtbl.find referred_debversions_per_binname pname
	       in
	       (List.map
		  (fun cudf_version ->
		    let debian_version =
		      snd(original_from_cudf(pname,cudf_version))
		    in let pos = index_in_list
			 debian_version
			 !referred_debian_versions
		       in (cudf_version,2*pos+3)
		  )
		 !cudf_versions)))
      else
	(* the package does currently not exist, we just map the mentionend *)
	(* numbers to 2,4,6,...                                             *)
	Hashtbl.add translation_table pname
	  (listmap_with_index (fun i x -> x,(2*i)+2) (!cudf_versions))
    )
    referred_versions_per_binname;
  
  let new_cudf_to_debian = Hashtbl.create number_current_packages
  (* associates to every binary package an association list thap maps   *)
  (* each new cudf version number to the (pseudo) debian version string *)
  (* that it represents (either a debian version mentionend in the      *)
  (* the original repository, or an interval of these).                 *)
  and cluster_versions_per_binname = Hashtbl.create (number_current_packages/2)
  (* associates to every binary package name that is part of a nontrivial *)
  (* cluster an association list that maps cudf versions of binary package *)
  (* to cudf versions of clusters.                                         *)
  in
  Hashtbl.iter
    (fun package_name translations ->
      Hashtbl.add new_cudf_to_debian package_name
	(if Hashtbl.mem orig_version_per_binname package_name
	 then (* we have a package with that name in the universe *)
	    let current_debian_version =
	      orig_debian_version_of_binname package_name
	    and deb_versions =
	      try !(Hashtbl.find
		      referred_debversions_per_binname package_name)
	      with Not_found -> []
	    in 
	    let rec f current_cudf previous_debian_version = function
	      | h::r -> 
		(current_cudf,"("^previous_debian_version^".."^h^")")
		::(current_cudf+1,h)
		::(f (current_cudf+2) h r)
	      | [] ->
		[(current_cudf,"("^previous_debian_version^"..)")]
	    in
	    (1,current_debian_version)
	    ::(f 2 current_debian_version deb_versions)
	 else (* there is no package with that name in the universe *)
	    if translations=[]
	    then [(1,"(..)")]
	    else
	      let (highest_debian_version, accu) =
		List.fold_left
		  (fun
		    (previous_debian_version, accu)
		    (old_cudf_version, new_cudf_version) ->
		      let debian_version =
			snd(original_from_cudf(package_name,old_cudf_version))
		      in
		      (debian_version,
		       ((new_cudf_version-1),
			("("^previous_debian_version^".."^debian_version^")"))
		       ::(new_cudf_version,debian_version)
		       ::accu))
		  ("",[])
		  translations
	      in
	      (2*(List.length translations)+1,"("^highest_debian_version^"..)")
	      ::accu
	))
    translation_table;

  Hashtbl.iter
    (fun package_name deb_versions ->
      if
	(try (Hashtbl.find size_per_cluster 
		(Hashtbl.find cluster_per_binname package_name)) > 1
	 with Not_found -> false)
      then (* the package belongs to a nontrivial cluster *)
	Hashtbl.add cluster_versions_per_binname package_name
	  (let rec f
	      current_cudf
	      previous_cluster_cudf
	      previous_cluster_deb = function
		| [] -> [(current_cudf, previous_cluster_cudf+1)]
		| h::r ->
		  if (normalize h)=previous_cluster_deb
		  then (current_cudf,previous_cluster_cudf)
		    :: (current_cudf+1,previous_cluster_cudf)
		    :: (f (current_cudf+2) previous_cluster_cudf
			  previous_cluster_deb r)
		  else (current_cudf,previous_cluster_cudf+1)
		    :: (current_cudf+1,previous_cluster_cudf+2)
		    :: (f (current_cudf+2) (previous_cluster_cudf+2)
			  (normalize h) r)
	   in
	   ((1,1)::
	       (f 2 1
		  (normalize (orig_debian_version_of_binname package_name))
		  !deb_versions))
	  )
    )
    referred_debversions_per_binname;

(*      
  if true then begin
    print_string "\nCluster versions::\n";
    Hashtbl.iter
      (fun package translations ->
	print_string package;
	print_char ' ';
	(List.iter
	   (fun (cudf,deb) ->
	     print_int cudf; print_char '=';print_int deb;print_char ' ')
	   translations);
	print_newline ())
      cluster_versions_per_binname;
  end;
*)

(*************************************************************************)
(* 4: Create the new list of packages, containing the original ones with *)
(* renumbered versions, and the new ones.                                *)

  let completed_package_list =
    let make_package package_name cudfv debianv provides conflicts =
      {Cudf.package = package_name;
       Cudf.version = cudfv;
       Cudf.depends = [];
       Cudf.conflicts = (package_name,None)::conflicts;
       Cudf.provides = provides;
       Cudf.installed = false;
       Cudf.was_installed = false;
       Cudf.keep = `Keep_none;
       Cudf.pkg_extra = [("number",`String debianv)] }
    in
    Hashtbl.fold
      (fun package_name translations accu ->
	if Hashtbl.mem orig_version_per_binname package_name
	then
	  let (srcname,srcversion) as cluster = 
	    (Hashtbl.find cluster_per_binname package_name)
	  in
	  let pseudosrcname="src/"^srcname^"/"^srcversion
	  in
	  (listmap_with_index
	     (if (Hashtbl.find size_per_cluster cluster) = 1
	      then fun _i (cudf_version, debian_version) ->
		make_package package_name cudf_version debian_version [] []
	      else
		 fun i (cudf_version, debian_version) ->
		   let src_version = 
		     List.assoc cudf_version
		       (Hashtbl.find cluster_versions_per_binname package_name)
		   in
		   make_package
		     package_name cudf_version debian_version
		     [(pseudosrcname, Some (`Eq, src_version))]
		     [(pseudosrcname, Some (`Neq, src_version))])
	     (List.filter (fun (cudf,_) -> cudf > 1) translations))
	  @accu
	else
	  (List.map
	     (fun (cudf_version, debian_version) ->
	       (make_package package_name cudf_version debian_version [] []))
	     translations)
	  @accu)
      new_cudf_to_debian
      (List.map
	 (synchronise_package size_per_cluster)
	 (renumber_packages translation_table orig_version_per_binname
	    purged_package_list))
      
  in

(***************************************************************************)
(* 5: Run the solver on the complete set of packages.                      *)

(*
  List.iter
    (fun p -> begin
      print_newline ();
      print_string p.Cudf.package;
      print_newline ();
      print_int p.Cudf.version;
      print_newline ();
    end
    )
    completed_package_list;
  
*)

  let universe = Cudf.load_universe completed_package_list
  in
  
  begin
    if OptParse.Opt.is_set Options.cudf_output then
      let ch = open_out (OptParse.Opt.get Options.cudf_output)
      in begin
	output_string ch "preamble: \nproperty: number: string\n\n";
        Cudf_printer.pp_packages ch (List.sort compare completed_package_list);
	close_out ch
      end
  end;

  let pp pkg =
    let name = pkg.Cudf.package
    and cudf_v = pkg.Cudf.version in
    let deb_v =
      if cudf_v = 0
      then "out of the past ..."
      else
	try List.assoc cudf_v (Hashtbl.find new_cudf_to_debian name)
	with Not_found -> debversion_of_package pkg 
    in 
    let l = 
      ExtLib.List.filter_map (fun k ->
        try Some(k,Cudf.lookup_package_property pkg k)
        with Not_found -> None
      ) ["source";"sourceversion"]
    in (name,deb_v,l)
  in

  info "Solving..." ;
  let timer = Util.Timer.create "Solver" in
  Util.Timer.start timer;
  let explain = OptParse.Opt.get Options.explain in
  let fmt = Format.std_formatter in
  Format.fprintf fmt "@[<v 1>report:@,";
  let callback =
    Diagnostic.fprintf ~pp ~failure:true ~success:false ~explain fmt in
  let i = Depsolver.univcheck ~callback universe 
  in
    ignore(Util.Timer.stop timer ());

    Format.fprintf fmt "@]@.";
    Format.fprintf fmt "total-packages: %d\n" (Cudf.universe_size universe);
    Format.fprintf fmt "broken-packages: %d\n" i;
    if OptParse.Opt.is_set Options.architecture then
      Format.fprintf fmt "architecture: %s\n"
	(OptParse.Opt.get Options.architecture)
;;

main () ;;

