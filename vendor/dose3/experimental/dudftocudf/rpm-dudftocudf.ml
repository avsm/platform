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

module L = Xml.LazyList 

module Options = struct
  open OptParse
  let description = "Convert Rpm-based Dudf files to Cudf format"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let outdir = StdOpt.str_option ()
  let problemid = StdOpt.str_option ()
  let distribution = StdOpt.str_option () ;;

  let () = 
    match Filename.basename(Sys.argv.(0)) with
    |"caixa-dudftocudf" -> Opt.set distribution "caixa"
    |"mandriva-dudftocudf" -> Opt.set distribution "mandriva"
    |_ -> ()
  ;;

  open OptParser ;;
  add options ~short_name:'o' ~long_name:"outdir" ~help:"Output directory" outdir;
  add options                 ~long_name:"distr" ~help:"[caixa | mandriva]" distribution;
  add options                 ~long_name:"id" ~help:"Problem id" problemid;
end

let debug fmt = Util.make_debug "Rpm-dudf" fmt
let info fmt = Util.make_info "Rpm-dudf" fmt
let warning fmt = Util.make_warning "Rpm-dudf" fmt

(* ========================================= *)

let rel_of_string = function
  |"<<" | "<" -> `Lt
  |"<=" -> `Leq
  |"=" | "==" -> `Eq
  |">=" -> `Geq
  |">>" | ">" -> `Gt
  |"ALL" -> `ALL
  |s -> (Printf.eprintf "Invalid op %s" s ; assert false)

let parse_vpkg vpkg =
  match Pcre.full_split ~rex:(Pcre.regexp " <=| >=| =| <| >") vpkg with
  |(Pcre.Text n)::_ when String.starts_with n "rpmlib(" -> None
  |[Pcre.Text n] -> Some(String.strip n,(`ALL,""))
  |[Pcre.Text n;Pcre.Delim sel;Pcre.Text v] ->
      Some(String.strip n,(rel_of_string (String.strip sel), String.strip v))
  |_ -> (Printf.eprintf "%s\n%!" vpkg ; assert false)

let parse_string = function
  |Json_type.String s -> s
  |Json_type.Int i -> string_of_int i
  |Json_type.Null -> ""
  |Json_type.Bool s -> string_of_bool s
  |_ as s -> (Printf.eprintf "%s\n%!" (Json_io.string_of_json s) ; assert false)

let to_list = function
  |Json_type.Array l ->
      List.map (function Json_type.String s -> s | _ -> assert false) l
  |_ -> assert false

(*
So in the current version of CM DUDF, it's roughly this:

[[package1], [package2],...]

Each package is an array with this structure:
["name", epoch, "version", "release", [requires,...],
[provides, ...], [conflicts, ...], [obsoletes,...], size, essential].

The dependency arrays should really be filled with triples as (NAME,
FLAG, VERSION) but at the moment they are still in a single string
separated by spaces.

Any element that's not applicable (usually epoch) is expressed as null,
apart from the dependency arrays which appear as [].

Ex :
[
  "x11-font-bh-75dpi",
  null,
  "1.0.0",
  "7mdv2009.1",
  ["/bin/sh","mkfontscale","mkfontdir"],
  ["x11-font-bh-75dpi = 1.0.0-7mdv2009.1"],
  ["xorg-x11-75dpi-fonts <= 6.9.0"],
  [],
  3335361,
  false
]

*)
let read_status str = 
  let aux = function
    |Json_type.Array packagelist ->
        List.filter_map (function
          |Json_type.Array l ->
              let a = Array.of_list l in
              begin try
                (* let epoch = let s = parse_string a.(1) in if s = "" then "0" else s in
                let version = parse_string a.(2) in
                let release = parse_string a.(3) in *)
                let epoch = match parse_string a.(1) with "" -> "" |s -> s^":" in
                let version = parse_string a.(2) in
                let release = match parse_string a.(3) with "" -> ""|s -> "-"^s in
                Some 
                {
                  Rpm.Packages.name = parse_string a.(0);
                  (* Rpm.Packages.version = Printf.sprintf "%s:%s-%s" epoch version release; *)
                  Rpm.Packages.version = epoch^version^release;
                  Rpm.Packages.depends = [List.filter_map parse_vpkg (to_list a.(4))];
                  Rpm.Packages.conflicts = List.filter_map parse_vpkg (to_list a.(6));
                  Rpm.Packages.provides = List.filter_map parse_vpkg (to_list a.(5));
                  Rpm.Packages.obsoletes = List.filter_map parse_vpkg (to_list a.(7));
                  Rpm.Packages.files = [];
                  Rpm.Packages.extras = [
                    ("size", parse_string a.(8));
                    ("essential", parse_string a.(9))
                  ]
                }
              with Invalid_argument("index out of bounds") -> (
                warning "%s" (Json_io.string_of_json (Json_type.Array l));
                None)
              end
          |_ -> assert false
        ) packagelist
    |_ -> assert false
  in aux (Json_io.json_of_string str)

(* ========================================= *)

let has_children nodelist tag =
  try match nodelist with
    |t::_ when (Xml.tag t) = tag -> true
    |_ -> false
  with Xml.Not_element(_) -> false
;;

let mem_include l = List.exists (function ("include",_) -> true |_ -> false) l
let parsepackagelist = function
  |(Some ("synthesis_hdlist" as t),None,None,attrl,[]) when mem_include attrl ->
      let (_,href) = List.find (function ("include",_) -> true |_ -> false) attrl in
      (t,"","", Dudfxml.pkgget (* ~compression:Dudfxml.Cz *) href)
  |(Some ("synthesis_hdlist" as t),_,_,_,[cdata]) -> (t,"","",Xml.cdata cdata)
  |(Some t,_,_,_,_) ->
      (Printf.eprintf "Warning : Unknown format for package-list element %s \n" t; exit 1)
  |(None,_,_,_,_) -> assert false
;;

(* ========================================= *)

open Dudfxml.XmlDudf

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  Random.self_init () ;
  let input_file =
    match OptParse.OptParser.parse_argv Options.options with
    |[h] -> h
    |_ -> (Printf.eprintf "too many arguments" ; exit 1)
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  info "parse xml";

  let dudfdoc = Dudfxml.parse input_file in

  if not(OptParse.Opt.is_set Options.distribution) then
    if Pcre.pmatch ~rex:(Pcre.regexp "[Cc]aixa") dudfdoc.distribution then 
      OptParse.Opt.set Options.distribution "caixa"
    else if Pcre.pmatch ~rex:(Pcre.regexp "[Mm]andriva") dudfdoc.distribution then
      OptParse.Opt.set Options.distribution "mandriva"
    else begin
      Printf.eprintf
      "Unsupported dudf format (%s)\n" dudfdoc.distribution;
      exit 1
    end;

  let uid = dudfdoc.uid in
  let status =
    match dudfdoc.problem.packageStatus.st_installer with
    |[status] -> Xml.fold (fun a x -> a^(Xml.cdata x)) "" status
    |_ -> Printf.eprintf "Warning: wrong status" ; ""
  in
  let packagelist = 
    List.map (fun pl -> parsepackagelist pl) dudfdoc.problem.packageUniverse
  in
  let action = dudfdoc.problem.action in
  (* let preferences = dudfdoc.problem.desiderata in

  let extras_property = [
    ("Size", ("size", `Nat (Some 0)));
    ("Installed-Size", ("installedsize", `Nat (Some 0)));
    ("Maintainer", ("maintainer", `String None))]
  in
  let extras = List.map fst extras_property in
  *)

  info "parse universe";
  let all_packages =
    List.fold_left (fun acc (_,_,_,contents) ->
      let ch = IO.input_string contents in
      let l = Rpm.Packages.Synthesis.parse_packages_in (fun x -> x) ch in
      let _ = IO.close_in ch in
      List.fold_right Rpm.Packages.Set.add l acc
    ) Rpm.Packages.Set.empty packagelist
  in
  info "universe : %d" (Rpm.Packages.Set.cardinal all_packages);

  info "parse package status";
  let installed_packages =
    let l = read_status status in
    List.fold_left (fun s pkg -> Rpm.Packages.Set.add pkg s) Rpm.Packages.Set.empty l
  in
  info "status : %d" (Rpm.Packages.Set.cardinal installed_packages);

  let l = Rpm.Packages.Set.elements (Rpm.Packages.Set.union all_packages installed_packages) in
  info "union : %d" (List.length l);
  let tables = Rpm.Rpmcudf.init_tables l in

  let installed =
    let h = Hashtbl.create 1031 in
    Rpm.Packages.Set.iter (fun pkg ->
      Hashtbl.add h (pkg.Rpm.Packages.name,pkg.Rpm.Packages.version) ()
    ) installed_packages
    ;
    h
  in
  
  info "convert";
  let pl =
    List.map (fun pkg ->
      let inst = Hashtbl.mem installed (pkg.Rpm.Packages.name,pkg.Rpm.Packages.version) in
      Rpm.Rpmcudf.tocudf tables ~inst:inst pkg
    ) l
  in
  info "cudf packages %d" (List.length pl);

  let universe = Cudf.load_universe pl in

  (* duplicated code from deb-dudfcudf *)
  info "request";
  let request =
    match OptParse.Opt.get Options.distribution with
    |"caixa" ->
        begin
          let mapver = function
            |`Pkg p -> (p,None)
            |`PkgVer (p,v) -> begin
                try (p,Some(`Eq,Rpm.Rpmcudf.get_cudf_version tables (p,v)))
                with Not_found -> failwith (Printf.sprintf "There is no version %s of package %s" p v)
            end
            |`PkgDst (p,d) ->
                  failwith (Printf.sprintf "There is no package %s in release %s " p d)
          in
          let request_id =
            if OptParse.Opt.is_set Options.problemid then OptParse.Opt.get Options.problemid
            else if uid <> "" then uid
            else (string_of_int (Random.bits ()))
          in
          let parsed_action =
            match dudfdoc.metaInstaller.name with
            |"apt" -> Debian.Apt.parse_request_apt action
            |s -> failwith("Unsupported meta installer "^s)
          in
          match parsed_action with
          |Debian.Apt.Upgrade (Some (suite))
          |Debian.Apt.DistUpgrade (Some (suite)) ->
              let il = Rpm.Packages.Set.fold (fun pkg acc -> `PkgDst (pkg.Rpm.Packages.name,suite) :: acc) installed_packages [] in
              let l = List.map mapver il in
              { Cudf.request_id = request_id ; install = l ; remove = [] ; upgrade = [] ; req_extra = [] ; }
          |Debian.Apt.Install l ->
              let l = List.map mapver l in
              { Cudf.request_id = request_id ; install = l ; remove = [] ; upgrade = [] ; req_extra = [] ; }
          |Debian.Apt.Remove l ->
              let l = List.map (fun (`Pkg p) -> (p,None) ) l in
              { Cudf.request_id = request_id ; install = [] ; remove = l ; upgrade = [] ; req_extra = [] ;}
          |Debian.Apt.Upgrade None ->
              { Cudf.request_id = request_id ; install = [] ; remove = [] ; upgrade = [] ; req_extra = [] ; }
          |Debian.Apt.DistUpgrade None ->
              { Cudf.request_id = request_id ; install = [] ; remove = [] ; upgrade = [] ; req_extra = [] ; }
        end
    |"mandriva" -> Cudf.default_request
    |_ -> assert false
  in

  info "dump";
  let oc =
    if OptParse.Opt.is_set Options.outdir then begin
      let dirname = OptParse.Opt.get Options.outdir in
      let file =
        let s = Filename.basename input_file in
        try Filename.chop_extension s with Invalid_argument _ -> s
      in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
      open_out (Filename.concat dirname (file^".cudf"))
    end else stdout
  in
  let preamble = Rpm.Rpmcudf.preamble in
  Cudf_printer.pp_cudf (Format.formatter_of_out_channel oc) (preamble, universe, request)
;;

main ();;

