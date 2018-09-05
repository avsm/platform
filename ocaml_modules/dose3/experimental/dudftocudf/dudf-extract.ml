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
module Boilerplate = BoilerplateNoRpm

include Util.Logging(struct let label = __FILE__ end) ;;

module Deb = Debian.Packages

module L = Xml.LazyList

module Options = struct
  open OptParse
  let description = "Extract all dudf row components"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)
end

open Dudfxml.XmlDudf

(*
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
*)

let has_children nodelist tag =
  try match nodelist with
    |t::_ when (Xml.tag t) = tag -> true
    |_ -> false
  with Xml.Not_element(_) -> false
;;

let parsepackagelist = function
  |(Some "apt",Some fname,url,_,[inc]) when has_children [inc] "include" ->
      let href = Xml.attrib inc "href" in
      ("apt",fname,url,Dudfxml.pkgget ~compression:Dudfxml.Bz2 href)
  |(Some "apt-release",Some fname,url,_,[inc]) when has_children [inc] "include" ->
      let href = Xml.attrib inc "href" in
      ("apt-release",fname,url,Dudfxml.pkgget href)
  |(Some t,Some fname,url,_,[cdata]) -> (t,fname,url,Xml.cdata cdata)
  |(Some t,Some fname,url,_,[]) -> (t,fname,url,"")
  |(Some t,Some fname,url,_,_) ->
      fatal "Unknown format for package-list element %s %s" t fname
  |_ -> fatal "Completly unknown format for package-list element"
;;

let main () =
  let input_file =
    match OptParse.OptParser.parse_argv Options.options with
    |[h] -> h
    |_ -> fatal "too many arguments"
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  let dirname =
    let s = Filename.basename input_file in
    try Filename.chop_extension s with Invalid_argument _ -> s
  in
  if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
  let dudfdoc = Dudfxml.parse input_file in
  let status =
    match dudfdoc.problem.packageStatus.st_installer with
    |[status] -> Xml.fold (fun a x -> a^(Xml.cdata x)) "" status
    |_ -> Printf.eprintf "Warning: wrong status" ; ""
  in
  let packagelist =
    List.map (fun pl -> parsepackagelist pl) dudfdoc.problem.packageUniverse
  in
  let action = dudfdoc.problem.action in
  let preferences = dudfdoc.problem.desiderata in

  List.iter (fun (fname,s) ->
    let oc = open_out (Filename.concat dirname fname) in
    let fmt = (Format.formatter_of_out_channel oc) in
    Format.fprintf fmt "%s@." s;
    close_out oc;
  ) [("status",status);("action",action);("preferences",preferences)] ;

  List.iter (fun (_,fname,_,s) ->
    let filename = Filename.basename fname in
    let oc = open_out (Filename.concat dirname filename) in
    let fmt = (Format.formatter_of_out_channel oc) in
    Format.fprintf fmt "%s@." s;
    close_out oc;
  ) packagelist
;;

main ();;
