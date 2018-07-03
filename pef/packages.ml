(**************************************************************************************)
(*  Copyright (C) 2015 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2015 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** Representation of a PEF stanza. *)

module Pcre = Re_pcre
open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

exception IgnorePackage of string

type parse_extras_f = (string -> Format822.stanza -> string)

let lexbuf_wrapper type_parser v =
  Format822.lexbuf_wrapper type_parser Packages_lexer.token_deb v
let parse_name v = lexbuf_wrapper Packages_parser.pkgname_top v
let parse_version v = lexbuf_wrapper Packages_parser.version_top v
let parse_vpkg v = lexbuf_wrapper  Packages_parser.vpkg_top v
let parse_vpkglist v = lexbuf_wrapper Packages_parser.vpkglist_top v
let parse_vpkgformula v = lexbuf_wrapper Packages_parser.vpkgformula_top v
let parse_archlist v = lexbuf_wrapper Packages_parser.archlist_top v
let parse_builddepslist v = lexbuf_wrapper Packages_parser.builddepslist_top v
let parse_builddepsformula v = lexbuf_wrapper Packages_parser.builddepsformula_top v

(* assume n is lowercase *)
(* Using lists in this case is faster then using 
 * specialized Maps or Hashtables : tested ! *)
let rec assoc (n : string) = function
  |(k,v)::_ when k = n -> v
  |(k,_)::t -> assoc n t
  |[] -> raise Not_found

let parse_s ?default ?(required=false) f label par =
  try let (_loc,s) = (assoc label par) in f (label,(_loc,s))
  with Not_found ->
    match required,default with
    |false,None -> raise Not_found (* for extra labels *)
    |true,None -> raise (Format822.ParseError ([],label,"This label is required."))
    |_,Some d -> d (* required or not, I take into consideration the default *)

let parse_string (_,(_,s)) = s
let parse_int (_,(_,s)) = int_of_string s
let parse_string_opt (_,(_,s)) = match s with "" -> None | _ -> Some s

let blank_regexp = Pcre.regexp "[ \t]+" ;;
let comma_regexp = Pcre.regexp "[ \t]*,[ \t]*" ;;
let parse_string_list ?(rex=blank_regexp) (_,(_,s)) =
  match Pcre.split ~rex s with
  |[] -> raise Not_found
  |l -> l

(* parse and convert to a specific type *)
let parse_bool (label,(_,s)) = match s with
  |("Yes"|"yes"|"True" |"true") -> true
  |("No" |"no" |"False"|"false") -> false (* this one usually is not there *)
  |s -> raise (Format822.Type_error (label ^ " - wrong value : "^ s))
;;

let parse_bool_s v = string_of_bool (parse_bool v)
let parse_int_s (_,(_,s)) = string_of_int (int_of_string s)

(* parse extra labels parse_f returns a string *)
let parse_e extras par =
  List.filter_map (fun (label, p) ->
    try begin
      match p with
      |None -> Some(label,parse_s parse_string label par)
      |Some parse_f -> Some (label,parse_f label par)
    end with Not_found -> None
  ) extras

let get_field_value ~parse ~par ~field:(label,value) =
  let res = 
    if Option.is_none value then parse label par
    else Option.get value
  in (label,res)

(** strip down version of the debian package format *)
class package 
  ?(name=("Package",None)) ?(version=("Version",None)) 
  ?(installed=("Installed",Some false)) ?(depends=("Depends",None))
  ?(conflicts=("Conflicts",None)) ?(provides=("Provides",None)) 
  ?(recommends=("Recommends",None)) ?(extras=([],None)) par = object

  val name : (string * Packages_types.name) =
    let parse = parse_s ~required:true parse_name in
    get_field_value ~parse ~par ~field:name

  val version : (string * Packages_types.version) =
    let parse = parse_s ~required:true parse_version in
    get_field_value ~parse ~par ~field:version

  val installed : (string * Packages_types.installed) =
    let parse = parse_s ~default:false parse_bool in
    get_field_value ~parse ~par ~field:installed

  val depends : (string * Packages_types.vpkgformula) =
    let parse = parse_s ~default:[] parse_vpkgformula in
    get_field_value ~parse ~par ~field:depends

  val conflicts : (string * Packages_types.vpkglist) =
    let parse = parse_s ~default:[] parse_vpkglist in
    get_field_value ~parse ~par ~field:conflicts

  val provides : (string * Packages_types.vpkglist) =
    let parse = parse_s ~default:[] parse_vpkglist in
    get_field_value ~parse ~par ~field:provides

  val recommends : (string * Packages_types.vpkgformula) =
    let parse = parse_s ~default:[] parse_vpkgformula in
    get_field_value ~parse ~par ~field:recommends

  val extras : (string * string) list =
    match extras with
    |([],None) -> []
    |(extras,None) -> parse_e extras par
    |([],Some l) -> l
    |(extras,Some l) -> l@(parse_e extras par)
  
  method name = snd name
  method version = snd version
  method installed = snd installed
  method depends = snd depends
  method conflicts = snd conflicts
  method provides = snd provides
  method recommends = snd recommends
  method extras = extras

  method add_extra k v = {< extras = (k,v)::extras >}
  method get_extra k = assoc k extras
  method set_extras v = {< extras = v >}

  method set_installed v = {< installed = (fst installed,v) >}

  method pp oc =
    Printer.pp_string_wl oc name;
    Printer.pp_string_wl oc version;
    Printer.pp_vpkglist_wl oc provides;
    Printer.pp_vpkgformula_wl oc depends;
    Printer.pp_vpkglist_wl oc conflicts;
    Printer.pp_vpkgformula_wl oc recommends;
    Printf.fprintf oc "\n"

end

let parse_package_stanza ~filter ~extras par =
  let p () = new package ~extras:(extras,None) par in
  if Option.is_none filter then Some (p ())
  else if (Option.get filter) par then Some(p ())
  else None

let rec packages_parser fname stanza_parser p =
  let rec packages_parser_aux fname stanza_parser acc p =
    let filename = ("Filename",(Format822.dummy_loc,Filename.basename fname)) in
    match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
    |None -> acc
    |Some stanza -> begin
      match stanza_parser (filename::stanza) with
      |None -> packages_parser_aux fname stanza_parser acc p
      |Some st -> packages_parser_aux fname stanza_parser (st::acc) p
    end
  in
  packages_parser_aux fname stanza_parser [] p

let parse_packages_in ?filter ?(extras=[]) fname ic =
  info "Parsing 822 file %s..." fname;
  try
    let stanza_parser = parse_package_stanza ~filter ~extras in
    Format822.parse_from_ch (packages_parser fname stanza_parser) ic
  with Format822.ParseError (cl,label,errmsg) ->
    fatal "Filename %s\n %s\n %s : %s" fname (String.concat "\n " cl) label errmsg

module Set = struct
  let pkgcompare p1 p2 = compare (p1#name,p1#version) (p2#name,p2#version)
  include Set.Make(struct 
    type t = package
    let compare (x:t) (y:t) = pkgcompare x y
  end)
end

let input_raw ?(extras=[]) = 
  let module M = Format822.RawInput(Set) in
  M.input_raw (parse_packages_in ~extras)

let input_raw_in ?(extras=[]) = 
  let module M = Format822.RawInput(Set) in
  M.input_raw_in (parse_packages_in ~extras)
