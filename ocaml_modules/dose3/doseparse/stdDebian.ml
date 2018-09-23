(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
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

let vpkg_option ?default ?(metavar = " <vpkg>") () =
  let parse_vpkg s =
    let _loc = Format822.dummy_loc in
    Pef.Packages.parse_vpkg ("cmdline <vpkg>",(_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkg (fun _ s -> Printf.sprintf "invalid vpackage '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (<c> v)" *)
let vpkglist_option ?default ?(metavar = " <vpkglst>") () =
  let parse_vpkglist s =
    let _loc = Format822.dummy_loc in
    Pef.Packages.parse_vpkglist ("cmdline <vpkglst>",(_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "invalid vpackage list '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (= v)" *)
let pkglist_option ?default ?(metavar = " <pkglst>") () =
  let parse_vpkglist s =
    let _loc = Format822.dummy_loc in
    List.map (function
      |((n,a),Some("=",v)) -> (n,a,v)
      |((n,a),None) ->
          raise (Format822.ParseError ([],s,"you must specify a version" ))
      |_ -> raise (Format822.ParseError ([],s,""))
    ) (Pef.Packages.parse_vpkglist ("cmdline <pkglst>",(_loc,s)))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "invalid package list '%s'" s)
;;

let pkglist tables universe vpkglist =
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  List.flatten (
    List.map (fun ((n,a),c) ->
      let (name,filter) = Pef.Pefcudf.pefvpkg to_cudf ((n,a),c) in
      Cudf.lookup_packages ~filter universe name
    ) vpkglist
  )
;;
