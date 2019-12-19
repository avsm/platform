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

module Pcre = Re_pcre
open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

type name = string
type version = string
type rel = [ `Lt | `Leq | `Eq | `Geq | `Gt | `ALL ]
type vpkg = (string * (rel * string) option)

type package = {
  name : name ;
  version : version;
  depends : vpkg list;
  conflicts : vpkg list;
  obsoletes : vpkg list;
  provides : vpkg list;
  files : string list;  (* the file and whether it is a directory *)
  extras : (string * string) list;
}

let default_package = {
  name = "";
  version = "";
  depends = [];
  conflicts = [];
  obsoletes = [];
  provides = [];
  files = [];
  extras = [];
}

let string_of_rel = function
  | `Lt -> "<"
  | `Leq  -> "<="
  | `Eq -> "="
  | `Geq  -> ">="
  | `Gt -> ">"
  | `ALL -> "ALL"

module Set = Set.Make(struct
  type t = package
  let compare p1 p2 = compare (p1.name,p1.version) (p2.name,p2.version) 
end)

let input_raw_priv parse_packages files =
  let timer = Util.Timer.create "Rpm.Packages.input_raw" in
  Util.Timer.start timer;
  if List.length files > 1 then info "Merging input lists" ;
  let s =
    List.fold_left (fun acc f ->
      info "Parsing %s..." f;
      let l = parse_packages (fun x -> x) f in
      List.fold_left (fun s x -> Set.add x s) acc l
    ) Set.empty files
  in
  info "total packages %n" (Set.cardinal s);
  Util.Timer.stop timer (Set.elements s)

module Hdlists = struct

  open Hdlists

  let parse_packages_fields par =
    try
      Some (
        {
          name = get_string "Package" par;
          version = get_string "Version" par;
          depends = get_deplist ~opt:true "Requires" par;
          conflicts = get_deplist ~opt:true "Conflicts" par;
          obsoletes = get_deplist ~opt:true "Obsoletes" par;
          provides = get_deplist ~opt:true "Provides" par;
          files = get_list ~opt:true "Files" par;
          extras = [];
        }
      )
    with Not_found -> None

  let parse_packages f filename =
    let t = _open_in filename in
    let parse_packages_rec = parse_822_iter parse_packages_fields in
    let l = parse_packages_rec f t in
    _close_in t ;
    l

  let input_raw ?(extras=[]) files = input_raw_priv parse_packages files
end

module Synthesis = struct

(* spec : http://wiki.mandriva.com/en/Format_of_synthesis.hdlist.cz_index *)

  open ExtLib
  open Common

  let rel_of_string = function
    |"<<" | "<" -> `Lt
    |"<=" -> `Leq
    |"=" | "==" -> `Eq
    |">=" -> `Geq
    |">>" | ">" -> `Gt
    |"ALL" -> `ALL
    |s -> (Printf.eprintf "Invalid op %s" s ; assert false)

  let parse_op = function
    |"*" -> None
    |"ALL" -> None
    |sel ->
        try Some(Scanf.sscanf sel "%s %s" (fun op v -> (rel_of_string op,v)))
        with End_of_file -> (Printf.eprintf "Invalid op %s" sel ; assert false)

  let parse_vpkg vpkg =
    try Scanf.sscanf vpkg "%[^[][%[^]]]" (fun n sel -> (n,parse_op sel))
    with End_of_file -> (vpkg,None)

  let parse_deps l = List.unique (List.map parse_vpkg l)

  let parse_info pkg = function
    |[nvra;epoch;size;group] ->
        let ra = String.rindex nvra '.' in (* arch *)
        let vr = String.rindex_from nvra (ra-1) '-' in (* release *)
        let nv = String.rindex_from nvra (vr-1) '-' in (* version *)
        let name = String.sub nvra 0 nv in
        let version = String.sub nvra (nv+1) (vr-nv-1) in
        let release = String.sub nvra (vr+1) (ra-vr-1) in
        let arch = String.sub nvra (ra+1) (String.length nvra-ra-1) in
        let version =
          if epoch <> "0" then Printf.sprintf "%s:%s-%s" epoch version release
          else Printf.sprintf "%s-%s" version release
        in
        { pkg with 
          name = name ;
          version = version ;
          extras = ("arch",arch) :: pkg.extras
        }
    |_ -> assert false

  exception Eof

  let rec parse_paragraph pkg ch =
    let line =
      try IO.read_line ch
      with IO.No_more_input -> raise Eof | End_of_file -> assert false
    in
    try
      match List.tl (Pcre.split ~rex:(Pcre.regexp "@") line) with
      |"provides"::l -> parse_paragraph {pkg with provides = parse_deps l} ch
      |"requires"::l -> parse_paragraph {pkg with depends = parse_deps l} ch
      |"obsoletes"::l -> parse_paragraph {pkg with obsoletes = parse_deps l} ch
      |"conflicts"::l -> parse_paragraph {pkg with conflicts = parse_deps l} ch
      |"summary"::l -> parse_paragraph pkg ch
      |"filesize"::l -> parse_paragraph pkg ch
      |"suggests"::l -> parse_paragraph pkg ch
      |"info"::l -> parse_info pkg l
      |s::l -> ((warning "Unknown field %s" s); parse_paragraph pkg ch)
      |_ -> assert false
    with End_of_file -> assert false

  let rec parse_packages_rec acc ch =
    try
      let par = parse_paragraph default_package ch in
      parse_packages_rec (par::acc) ch
    with Eof -> acc

  let parse_packages_in f ch =
    parse_packages_rec [] ch

  let parse_packages f filename =
    try
      let ch = Input.open_file filename in 
      let l = parse_packages_rec [] ch in
      Input.close_ch ch; l
    with Input.File_empty -> []

  let input_raw ?(extras=[]) files = input_raw_priv parse_packages files

end
