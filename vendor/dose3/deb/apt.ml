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

(** Apt command line parsing *)

module Pcre = Re_pcre
open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let blank_regexp = Pcre.regexp "[ \t]+" ;;

(* parse the output of "dpkg -l" *)
let parse_inst ch =
  let h = Hashtbl.create 1000 in
  try
    while true do
      let s = (input_line ch) in
      match Pcre.split ~rex:blank_regexp s with
      |status::name::version::_ when status = "ii"-> Hashtbl.add h (name,version) ()
      |_ -> ()
    done ;
    h
  with End_of_file -> h

let parse_inst_from_cmd cmd =
  let ch = Unix.open_process_in cmd in
  let h = parse_inst ch in
  let _ = close_in ch in
  h

let parse_inst_from_file file =
  let ch = open_in file in
  let h = parse_inst ch in
  let _ = close_in ch in
  h

(**********************************************************************)

(* parse the a popcon file *)
let parse_popcon s =
  match Pcre.split ~rex:blank_regexp s with
  |rank::name::inst::_ -> (int_of_string rank,name,int_of_string inst)
  |_ -> fatal "Parse error %s\n" s

(*****************************************************)

(*
1. apt-get install PACKAGE ...
2. apt-get install PACKAGE=VERSION ...
3. apt-get install PACKAGE/RELEASE ...
4. apt-get remove PACKAGE ...
5. apt-get upgrade ...
6. apt-get dist-upgrade ...

"-t RELEASE" option 
*)

type apt_req =
  |Install of Pef.Packages_types.vpkgreq list
  |Remove of Pef.Packages_types.vpkgreq list
  |Upgrade of Pef.Packages_types.suite option
  |DistUpgrade of Pef.Packages_types.suite option

let parse_req label s = 
  let _loc = Format822.dummy_loc in
  Pef.Packages.lexbuf_wrapper Pef.Packages_parser.request_top (label,(_loc,s))

let parse_pkg_req suite s =
  let (r,((n,a),c),s) = parse_req "apt req suite" s in
  begin match suite with
  |None -> (r,((n,a),c),s)
  |Some suite -> (r,((n,a),c),Some suite)
  end
;;

(** parse a string containing an apt-get command line 
    @return a data structure containing the request *)
let parse_request_apt s =
  if not (String.exists s "apt-get") then fatal "Not a valid apt-get command" ;
  let s = String.slice ~first:((String.find s "apt-get")) s in
  let suite = ref None in
  (* XXX we parse a lot of options, but we do not handle them ... *)
  let options = [
    ("-t", Arg.String (fun l -> suite := Some(l)), "");
    ("-s", Arg.Unit (fun _ -> ()), "");
    ("-y", Arg.Unit (fun _ -> ()), "");
    ("-v", Arg.Unit (fun _ -> ()), "");
    ("-f", Arg.Unit (fun _ -> ()), "");
    ("-o", Arg.String (fun _ -> ()), "");
    ("--solver", Arg.String (fun _ -> ()), "");
    ("--no-install-recommends", Arg.Unit (fun _ -> ()), "");
    ("--install-recommends", Arg.Unit (fun _ -> ()), "");
    ("--no-upgrade", Arg.Unit (fun _ -> ()), "");
    ("--no-remove", Arg.Unit (fun _ -> ()), "");
    ] 
  in
  let reqlist = ref [] in
  let anon s = reqlist := s :: !reqlist in
  begin
    begin try Arg.parse_argv ~current:(ref 0) (Array.of_list (Pcre.split ~rex:blank_regexp s)) options anon ""
    with Arg.Bad s -> fatal "%s" s end ;
    match List.rev !reqlist with
    |"install" :: tl -> Install(List.map (parse_pkg_req !suite) tl)
    |"remove" :: tl -> Remove(List.map (parse_req "apt req remove") tl)
    |["upgrade"] -> Upgrade(!suite)
    |["dist-upgrade"] -> DistUpgrade(!suite)
    |_ -> fatal "Bad apt request '%s'" s
  end
;;

let parse_request_aptitude s =
  if not (String.exists s "aptitude") then fatal "Not a valid aptitude command" ;
  let s = String.slice ~first:((String.find s "aptitude")) s in
  let suite = ref None in
  (* XXX we parse a lot of options, but we do not handle them ... *)
  let options = [
    ("-t", Arg.String (fun l -> suite := Some(l)), ""); (* default suite *)
    ("-s", Arg.Unit (fun _ -> ()), "");
    ("-y", Arg.Unit (fun _ -> ()), "");
    ("-v", Arg.Unit (fun _ -> ()), "");
    ("--full-resolver", Arg.Unit (fun _ -> ()), "");
    ("--safe-resolver", Arg.Unit (fun _ -> ()), "");
    ("-f", Arg.Unit (fun _ -> ()), ""); (* fix-broken *)
    ("-r", Arg.Unit (fun _ -> ()), ""); (* with-reccomends *)
    ("--with-recommends", Arg.Unit (fun _ -> ()), "");
    ("-R", Arg.Unit (fun _ -> ()), ""); (* without-reccomends *)
    ("--without-recommends", Arg.Unit (fun _ -> ()), "");
    ] 
  in
  let reqlist = ref [] in
  let anon s = reqlist := s :: !reqlist in
  begin
    begin try Arg.parse_argv ~current:(ref 0) (Array.of_list (Pcre.split ~rex:blank_regexp s)) options anon ""
    with Arg.Bad s -> fatal "%s" s end ;
    match List.rev !reqlist with
    |"install" :: tl -> Install(List.map (parse_pkg_req !suite) tl)
    |"remove" :: tl -> Remove(List.map (parse_req "apt req remove") tl)
    |["upgrade"] | ["safe-upgrade"] | ["dist-upgrade"] -> Upgrade(!suite)
    |["full-upgrade"] -> DistUpgrade(!suite)
    |_ -> fatal "Bad aptitude request '%s'" s
  end
;;

(*****************************************************)

(** for details on the apt_preferences format :
    man apt_preferences *)
module Pref = struct

  type pin_t =
    |Release of (string * string) list
    |Origin of string
    |Version of string 

  type package_t = Package of string | Star

  type pin_priority_t = int

  type apt_preferences = {
    package : package_t;
    pin : pin_t ;
    pin_priority : pin_priority_t
  }

end

let comma_regexp = Pcre.regexp "[ \t]*,[ \t]*" ;;
let eq_regexp = Pcre.regexp "[ \t]*=[ \t]*" ;;
let di_regexp = Pcre.regexp "[0-9.]+" ;;
let al_regexp = Pcre.regexp "[a-zA-Z]+" ;; 

let parse_pref_labels s =
  List.map (fun s' ->
    match Pcre.split ~rex:eq_regexp s' with
    |[v] when (Pcre.pmatch ~rex:di_regexp v) -> ("v",v)
    |[v] when (Pcre.pmatch ~rex:al_regexp v) -> ("a",v)
    |[l;v] -> (l,v)
    |_ -> fatal "To many '=' in label %s" s
  ) (Pcre.split ~rex:comma_regexp s)

let general_regexp = Pcre.regexp "^[ \t]*[*][ \t]*$" ;;

let parse_pref_package (_,(_,s)) =
  if Pcre.pmatch ~rex:general_regexp s then Pref.Star
  else Pref.Package (Pef.Packages.parse_name ("apt pref",(Format822.dummy_loc,s)))

let pin_regexp = Pcre.regexp "^([A-Za-z]+)[ \t]+(.*)$" ;;

let parse_pin (_,(_,s)) =
  try
    let substrings = Pcre.exec ~rex:pin_regexp s
    in
    match Pcre.get_substring substrings 1 with
    |"release" -> Pref.Release (parse_pref_labels (Pcre.get_substring substrings 2))
    |"version" -> Pref.Version (Pcre.get_substring substrings 2) 
    |"origin"  -> Pref.Origin  (Pcre.get_substring substrings 2)
    |s -> fatal "Unknown pin type %s" s
  with Not_found -> fatal "Unknown pin format %s" s

let parse_preferences_stanza par =
  {
    Pref.package = Pef.Packages.parse_s ~required:true parse_pref_package "Package" par;
    pin = Pef.Packages.parse_s ~required:true parse_pin "Pin" par;
    pin_priority = Pef.Packages.parse_s ~required:true Pef.Packages.parse_int "Pin-Priority" par;
  }

let rec preferences_parser stanza_parser acc p =
  match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
  |None -> acc
  |Some stanza -> 
      let st = stanza_parser stanza in
      preferences_parser stanza_parser (st::acc) p

(** parse the apt_preferences file *)
let parse_preferences_in ic =
  Format822.parse_from_ch (preferences_parser parse_preferences_stanza []) ic
