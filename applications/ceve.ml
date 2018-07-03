(**************************************************************************************)
(*  Copyright (C) 2009,2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                *)
(*                          and Jaap Boender <boender@pps.jussieu.fr>                 *)
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
open Doseparse

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module DGraph = Defaultgraphs.SyntacticDependencyGraph
module PGraph = Defaultgraphs.PackageGraph

module Options = struct
  open OptParse
  let description = "Ceve - integrated metadata parser and transformer"
  let options = OptParser.make ~description
  include StdOptions.MakeOptions(struct let options = options end)

  let otypes = ["cnf";"dimacs";"cudf";"deb";"debsrc";"dot";"gml";"grml";"table"]
  let out_option ?default ?(metavar = Printf.sprintf "<%s>" (String.concat "|" otypes)) () =
    let corce s = if List.mem s otypes then s else raise Not_found in
    let error _ s = Printf.sprintf "%s format not supported" s in
    Opt.value_option metavar default corce error

  let gtypes = ["syn";"pkg";"conj";"strdeps";"strcnf";"dom"]
  let grp_option ?default ?(metavar = Printf.sprintf "<%s>" (String.concat "|" gtypes)) () =
    let corce s = if List.mem s gtypes then s else raise Not_found in
    let error _ s = Printf.sprintf "%s format not supported" s in
    Opt.value_option metavar default corce error

  let cone = StdOptions.vpkglist_option ()
  let reverse_cone = StdOptions.vpkglist_option ()
  let cone_maxdepth = StdOpt.int_option ()
  let out_type = out_option ~default:"cnf" ()
  let grp_type = grp_option ~default:"syn" ()
  let request = StdOptions.incr_str_list ()

  open OptParser

  include StdOptions.InputOptions ;;
  StdOptions.InputOptions.add_options ~default:["latest";"trim";"inputtype";"compare"] options ;;
  StdOptions.InputOptions.add_option options ~short_name:'c' ~long_name:"cone" ~help:"dependency cone" cone;;
  StdOptions.InputOptions.add_option options ~short_name:'r' ~long_name:"rcone" ~help:"reverse dependency cone" reverse_cone;;
  StdOptions.InputOptions.add_option options                 ~long_name:"depth" ~help:"max depth - in conjunction with cone" cone_maxdepth;;
  StdOptions.InputOptions.add_option options                 ~long_name:"request" ~help:"Installation Request (can be repeated)" request;;

  include StdOptions.OutputOptions ;;
  StdOptions.OutputOptions.add_options options ;;
  StdOptions.OutputOptions.add_option options ~short_name:'G' ~help:"Graph output type format. Default syn" grp_type;;
  StdOptions.OutputOptions.add_option options ~short_name:'T' ~help:"Output type format. Default cnf" out_type;;

  include StdOptions.DistribOptions ;;
  StdOptions.DistribOptions.add_debian_options options ;;
  StdOptions.DistribOptions.add_opam_options options ;;

end;;

(* -------------------------------- *)

let loadl to_cudf l =
  List.flatten (
    List.map (fun ((name,aop),sel) ->
      let encname =
        let n = match aop with Some a -> name^":"^a | None -> name in
        CudfAdd.encode n
      in
      match sel with
      |None -> [(encname, None)]
      |Some(op,v) ->
          [(encname,Some(Pef.Pefcudf.pefcudf_op op,snd(to_cudf (encname,v))))]
    ) l
  )
;;

let parse_request to_cudf l =
  let pkgs_of s = (* convert request into list of packages *) 
    let rs = String.strip (snd (String.split s " ")) in
    let field = ("ceve pkg req",(Format822.dummy_loc,rs)) in
    let v = Pef.Packages.lexbuf_wrapper Pef.Packages_parser.vpkglist_top field in
    loadl to_cudf v 
  in              
  let parse acc s =
    if String.starts_with s "install: " then
      { acc with Cudf.install = pkgs_of s}
    else if String.starts_with s "remove: " then
      { acc with Cudf.remove = pkgs_of s}
    else if String.starts_with s "upgrade: " then
      { acc with Cudf.upgrade = pkgs_of s}
    else acc
  in
  List.fold_left parse Cudf.default_request l
;;

let nr_conflicts univ =
  let open Cudf in
  Cudf.fold_packages (fun acc p ->
    let cfl = List.filter (fun x -> not(x =% p))
      (List.flatten (List.rev_map (CudfAdd.who_provides univ) p.conflicts)) in
    debug "%s: %d conflicts" (Cudf_types_pp.string_of_pkgname p.package)
      (List.length cfl);
    List.iter (fun c ->
      debug "- %s" (Cudf_types_pp.string_of_pkgname c.package)
    ) cfl;
    acc + (List.length cfl)
  ) 0 univ
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

  StdDebug.enable_debug(OptParse.Opt.get Options.verbose);
  StdDebug.all_quiet (OptParse.Opt.get Options.quiet);

  (* return raw package lists if the output type is deb or debsrc *)
  let raw =
    match OptParse.Opt.get Options.out_type with
    |"deb" | "debsrc" -> true
    | _ -> false
  in

  let (fg,bg) = Options.parse_cmdline (input_type,implicit) posargs in
  let (preamble,pkgll,request,from_cudf,to_cudf,rawll,global_constraints) =
    StdLoaders.load_list ~options ~raw [fg;bg]
  in
  let request = 
    let l = OptParse.Opt.get Options.request in
    if l <> [] then parse_request to_cudf l else request 
  in
  let (fg_pkglist, bg_pkglist) =
    match pkgll with [fg;bg] -> (fg,bg) | _ -> assert false
  in
  let universe =
    let s = CudfAdd.to_set (fg_pkglist @ bg_pkglist) in
    let u = 
      let sl = CudfAdd.Cudf_set.elements s in
      if OptParse.Opt.is_set Options.latest then
        let l = CudfAdd.latest ~n:(OptParse.Opt.get Options.latest) sl in
        Cudf.load_universe l
      else 
        Cudf.load_universe sl
    in
    if OptParse.Opt.get Options.trim then Depsolver.trim  u else u
  in
  let get_cudfpkglist ((n,a),c) =
    let (name,filter) = Pef.Pefcudf.pefvpkg to_cudf ((n,a),c) in
    CudfAdd.who_provides universe (name,filter)
  in

  let pkg_cone () =
    List.unique (List.fold_left (fun acc (p,c) ->
      let l = get_cudfpkglist (p,c) in
      if OptParse.Opt.is_set Options.cone_maxdepth then
        let md = OptParse.Opt.get Options.cone_maxdepth in
        (Depsolver.dependency_closure ~global_constraints ~maxdepth:md universe l) @ acc
      else
        (Depsolver.dependency_closure ~global_constraints universe l) @ acc
    ) [] (OptParse.Opt.get Options.cone))
  in
  let pkg_reverse_cone () =
    List.unique (List.fold_left (fun acc (p,c) ->
      let l = get_cudfpkglist (p,c) in
      if OptParse.Opt.is_set Options.cone_maxdepth then
        let md = OptParse.Opt.get Options.cone_maxdepth in
        (Depsolver.reverse_dependency_closure ~maxdepth:md universe l) @ acc
      else
        (Depsolver.reverse_dependency_closure universe l) @ acc
    ) [] (OptParse.Opt.get Options.reverse_cone))
  in

  let plist =
    if OptParse.Opt.is_set Options.cone then 
      pkg_cone ()
    else if OptParse.Opt.is_set Options.reverse_cone then
      pkg_reverse_cone ()
    else 
      Cudf.get_packages universe
  in

  (* Main iteration . Everything happens here *)
  List.iter (fun l ->
    let u = Cudf.load_universe l in
    let doc = (preamble,u,request) in
    let oc =
      if OptParse.Opt.is_set Options.outfile then 
        open_out (OptParse.Opt.get Options.outfile)
      else stdout
    in
    begin
      let t = OptParse.Opt.get Options.out_type in
      if List.mem t ["dot";"gml";"grml"] then
        let g = OptParse.Opt.get Options.grp_type in
        if not(List.mem g ["pkg";"strdeps";"conj";"dom";"syn"]) then
          info "Option -G %s is not compatible with format %s. Ingored" g t
    end;
    begin match OptParse.Opt.get Options.out_type with
    |"cnf" -> Printf.fprintf oc "%s" (Depsolver.output_clauses ~enc:Depsolver.Cnf u)
    |"dimacs" -> Printf.fprintf oc "%s" (Depsolver.output_clauses ~enc:Depsolver.Dimacs u)
    |"cudf" -> Cudf_printer.pp_cudf oc doc
    |"deb"|"debsrc" as t -> begin
      (* fill hashtable with mapping from cudf id to Debian.Packages and
       * Debian.Sources if the output type is deb or debsrc *)
      let cudftodeb_table = Hashtbl.create 30000 in
      (* limit the mapping in the hashtable to the packages in plist *)
      let plist_set = CudfAdd.to_set plist in
      begin match rawll with
      | Some dll ->
        List.iter2 (List.iter2 (fun cudfpkg debpkg ->
            if CudfAdd.Cudf_set.mem cudfpkg plist_set then
              let id = (cudfpkg.Cudf.package,cudfpkg.Cudf.version) in
              Hashtbl.add cudftodeb_table id debpkg
          )) pkgll dll
      | None -> fatal "If -T is deb or debsrc, then the raw list must not be None" end;
      let cudf2deb cudfpkg =
        let id = (cudfpkg.Cudf.package,cudfpkg.Cudf.version) in
        try
          Hashtbl.find cudftodeb_table id
        with Not_found ->
          fatal "cannot find cudf package the mapping - is the input deb:// or debsrc://?"
      in
      if t = "deb" then
        List.iter (function
          | StdLoaders.Deb p -> p#pp oc
          | StdLoaders.DebSrc p -> ()
          | _ -> assert false
        ) (List.map cudf2deb l)
      else if t = "debsrc" then
        List.iter (function
          | StdLoaders.Deb p -> ()
          | StdLoaders.DebSrc p -> p#pp oc
          | _ -> assert false
        ) (List.map cudf2deb l)
    end;
    |"table" ->
      Printf.fprintf oc "%d\t%d\t%d\n"
      (Cudf.universe_size u) 
      (Defaultgraphs.SyntacticDependencyGraph.G.nb_edges 
        (Defaultgraphs.SyntacticDependencyGraph.dependency_graph u))
      (nr_conflicts u)
    |("dot" | "gml" | "grml") as t -> 
      let fmt = Format.formatter_of_out_channel oc in
      begin match OptParse.Opt.get Options.grp_type with
        |"syn" ->
          let g = Defaultgraphs.SyntacticDependencyGraph.dependency_graph u in
          begin match t with
          |"dot" -> Defaultgraphs.SyntacticDependencyGraph.DotPrinter.print fmt g
          |"gml" -> Defaultgraphs.SyntacticDependencyGraph.GmlPrinter.print fmt g
          |"grml" -> Defaultgraphs.SyntacticDependencyGraph.GraphmlPrinter.print fmt g
          | _ -> assert false
          end
        |("pkg" | "strdeps" | "conj"| "dom") as gt ->
          let g =
            begin match gt with
            |"pkg" -> Defaultgraphs.PackageGraph.dependency_graph u
            |"strdeps" -> begin
              let g = Strongdeps.strongdeps_univ u in
              (* if input are Debian packages and --deb-ignore-essential was
               * not given, connect all binary packages to the Essential:yes
               * packages *)
              if not(OptParse.Opt.get Options.deb_ignore_essential) && 
                (input_type = `Deb || input_type = `DebSrc) then begin
                let essential = List.filter CudfAdd.is_essential l in
                Defaultgraphs.PackageGraph.G.iter_edges
                  (Defaultgraphs.PackageGraph.G.add_edge g)
                  (Strongdeps.strongdeps u essential);
                Defaultgraphs.PackageGraph.G.iter_vertex
                  (fun v -> List.iter
                      (Defaultgraphs.PackageGraph.add_edge ~transitive:true g v)
                      essential) g
              end;
              g end
            |"conj" -> Strongdeps.conjdeps_univ u
            |"dom" ->
              let g = Strongdeps.strongdeps_univ ~transitive:true universe in
              Dominators.dominators_tarjan g
            |_ -> assert false
            end
          in
          begin match t with
          |"dot" -> Defaultgraphs.PackageGraph.DotPrinter.print fmt g
          |"gml" -> Defaultgraphs.PackageGraph.GmlPrinter.print fmt g
          |"grml" -> Defaultgraphs.PackageGraph.GraphmlPrinter.print fmt g
          |_ -> fatal "Option -T %s not supported together with -G %s" t gt
          end
        |s -> fatal "Opation %s not supported" s
      end
    |t -> fatal "-T %s format unknown." t
    end ;
    close_out oc;
  ) [plist]
;;

StdUtils.if_application ~alternatives:["dose-ceve";"ceve"] __label (fun () -> main (); 0) ;;
