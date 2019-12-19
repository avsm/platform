(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  ADD authors here                                     *)
(*                                                                        *)
(*  Contributor(s):  ADD minor contributors here                          *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common
open Algo
open Doseparse

module Options = struct
  open OptParse
  open OptParser
  let description = ""
  let options = OptParser.make ~description
  include StdOptions.MakeOptions(struct let options = options end)

  include StdOptions.DistcheckOptions ;;
  StdOptions.DistcheckOptions.add_options options ;;

  include StdOptions.InputOptions ;;
  StdOptions.InputOptions.add_options options ;;

  include StdOptions.OutputOptions ;;
  StdOptions.OutputOptions.add_options options ;;

  include StdOptions.DistribOptions ;;
  StdOptions.DistribOptions.add_options options ;;

end


#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

(* implicit prefix of resources derived from name of executable *)
(* (input_format * add_format ?) *)
let guess_format t l =
  match Filename.basename(Sys.argv.(0)) with
  |"debcheck"|"dose-debcheck" -> (`Deb, true)
  |"eclipsecheck"|"dose-eclipsecheck" -> (`Pef, true)
  |"rpmcheck"|"dose-rpmcheck" -> (`Synthesis,true)
  |_ when OptParse.Opt.is_set t ->
      (Url.scheme_of_string (OptParse.Opt.get t),true)
  |_ -> (Input.guess_format [l], false)
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let inputlist = posargs@(OptParse.Opt.get Options.foreground) in
  let (input_type,implicit) = guess_format Options.inputtype inputlist in

  StdDebug.enable_debug (OptParse.Opt.get Options.verbose);
  StdDebug.enable_timers (OptParse.Opt.get Options.timers) ["Solver";"Load"];
  StdDebug.enable_bars (OptParse.Opt.get Options.progress)
    ["Depsolver_int.univcheck";"Depsolver_int.init_solver"] ;
  StdDebug.all_quiet (OptParse.Opt.get Options.quiet);

  let options = Options.set_options input_type in
  let (fg,bg) = Options.parse_cmdline (input_type,implicit) posargs in

  let (preamble,pkgll,_,from_cudf,to_cudf) = StdLoaders.load_list ~options [fg;bg] in
  let (fg_pkglist, bg_pkglist) = match pkgll with [fg;bg] -> (fg,bg) | _ -> assert false in
  let fg_pkglist =
    if OptParse.Opt.get Options.latest then CudfAdd.latest fg_pkglist
    else fg_pkglist
  in
  let universe =
    let s = CudfAdd.to_set (fg_pkglist @ bg_pkglist) in
    Cudf.load_universe (CudfAdd.Cudf_set.elements s)
  in
  let pp = CudfAdd.pp ~fields:[] from_cudf in

  let callback (pkg,deps,conf) =
    Format.printf "Some dependencies of the package %s can be revised :\n" (CudfAdd.string_of_package pkg);
    List.iter (function
      |(vpkglist,vpkg,[]) ->
        Format.printf "The dependency %a from [%a] refers to a missing package therefore useless\n" 
        (Diagnostic.pp_vpkg pp) vpkg (Diagnostic.pp_vpkglist pp) vpkglist
      |(vpkglist,vpkg,l) ->
          List.iter (fun bpkg ->
            Format.printf "The dependency %a from [%a] refers to a broken package (%a) : the dependency can be more strict\n" 
            (Diagnostic.pp_vpkg pp) vpkg
            (Diagnostic.pp_vpkglist pp) vpkglist 
            CudfAdd.pp_package bpkg
          ) l
    ) deps;
    Format.printf "Some conflict of the package %s can be revised :\n" (CudfAdd.string_of_package pkg);
    List.iter (function
      |(vpkg,[]) ->
        Format.printf "The conflict %a refers to a missing package therefore useless\n" 
        (Diagnostic.pp_vpkg pp) vpkg
      |(vpkg,l) ->
          List.iter (fun bpkg ->
            Format.printf "The conflict %a refers to a broken package (%a): this conflict can be more precise\n" 
            (Diagnostic.pp_vpkg pp) vpkg
            CudfAdd.pp_package bpkg
          ) l
    ) conf;
    Printf.printf "\n"
  in

  ignore(Depsolver.depclean ~callback universe fg_pkglist)
;;

main ();;

