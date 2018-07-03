(**************************************************************************************)
(*  Copyright (C) 2009-2015 Pietro Abate <pietro.abate@pps.univ-paris-diderot.fr>     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** this functions follow the semantic versioning specification http://semver.org/ 
 * and node's https://docs.npmjs.com/misc/semver.
 * *)

module Pcre = Re_pcre

open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

type raw_version = (string * string * string * string list * string list)
type ident = S of string | N of int
type version = {
  major: int;
  minor: int;
  patch: int;
  pre: ident list;
  build: string list;
}

let compose_raw (major,minor,patch,pre,build) =
  let str = 
    match (major,minor,patch) with
    |"","","" -> Printf.sprintf "*"
    |_,"","" -> Printf.sprintf "%s" major
    |_,_,"" -> Printf.sprintf "%s.%s" major minor
    |_,_,_ -> Printf.sprintf "%s.%s.%s" major minor patch
  in
  let str_pre l = String.concat "." l in
  let str_build l = String.concat "." l in
  match pre,build with
  |[],[] -> str
  |l,[] -> Printf.sprintf "%s-%s" str (str_pre l)
  |[],l -> Printf.sprintf "%s+%s" str (str_build l)
  |lp,lb -> Printf.sprintf "%s-%s+%s" str (str_pre lp) (str_build lb)

let convert ((major,minor,patch,pre,build) as raw) = 
  (* if x = "" then this is intepreted as a partial version and converted to 0 *)
  let c_int = function
    |"" -> 0
    |("x" | "X") ->
        let composed = compose_raw raw in
        raise (Invalid_argument (Printf.sprintf "'%s': Conversion Error: 'X' or 'x' cannot be converted to an integer" composed))
    |s ->
        try int_of_string s
        with Failure _ ->
          let composed = compose_raw raw in
          raise (Invalid_argument (Printf.sprintf "%s: Conversion Error: \"%s\" cannot be converted to an integer" composed s))
  in
  let c_pre x = try N (int_of_string x) with Failure _ -> S x in
  { major = c_int major;
    minor = c_int minor;
    patch = c_int patch;
    pre   = List.map c_pre pre;
    build
  }

let compose v =
  let str = Printf.sprintf "%d.%d.%d" v.major v.minor v.patch in
  let str_pre l =
      String.concat "." (List.map (function N i -> string_of_int i | S s -> s) l)
  in
  let str_build l = String.concat "." l in
  match v.pre,v.build with
  |[],[] -> str
  |l,[] -> Printf.sprintf "%s-%s" str (str_pre l)
  |[],l -> Printf.sprintf "%s+%s" str (str_build l)
  |lp,lb -> Printf.sprintf "%s-%s+%s" str (str_pre lp) (str_build lb)

let rex = Pcre.regexp (
  "^\\s*[v=]*\\s*" ^ (* optional version identifier *)
  "([0-9]+|[xX*])(\\.([0-9]+|[xX*])?(\\.([0-9]+|[xX*])?)?)?" ^ (* 3-dotted notation *)
  "(?:-((?:[a-zA-Z0-9]+|[a-zA-Z0-9-])(?:\\.[a-zA-Z0-9]+|[a-zA-Z0-9-])*))?" ^ (* pre release *)
  "(?:\\+([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*))?\\s*$" (* build indentifier *)
)

let sep_re = Pcre.regexp "\\."

let parse_raw_version version =
  try
    let parsed = Pcre.extract rex version in
    let pre   = Pcre.split sep_re parsed.(6) in
    let build = Pcre.split sep_re parsed.(7) in
    (parsed.(1),parsed.(3),parsed.(5),pre,build)
  with Not_found ->
    raise (Invalid_argument (Printf.sprintf "%s: Parsing Error. Invalid Version" version))

let parse_version version =
  try
    convert (parse_raw_version version)
  with
    Invalid_argument _ ->
      raise (Invalid_argument (Printf.sprintf "%s: Conversion Error. Invalid Version" version))

(*Compare  two elements of the prerelease part of a version*)
let compare_pre = function
  | (N x1, N y1) -> Pervasives.compare x1 y1
  | (S _, N _)   -> 1
  | (N _, S _)   -> -1
  | (S s1, S s2) -> String.compare s1 s2

(* 1. Not having a prerelease is > that having one
   2. We compare each pre-release, the one with with less elements win or the
      one with a hight element.  *)
let compare_pre (l1,l2) =
  let lenl1 = List.length l1 in
  let lenl2 = List.length l2 in
  if lenl1 = 0 && lenl2 = 0 then 0
  else if lenl1 <> 0 && lenl2 = 0 then -1
  else if lenl1 = 0 && lenl2 <> 0 then 1
  else if lenl1 = 0 && lenl2 = 0 then 0
  else
    let rec check acc = function
      |[],[] -> acc
      |(x1::l1,x2::[]) when l1 <> [] -> 1
      |(x1::[],x2::l2) when l2 <> [] -> -1
      |(x1::l1,x2::l2) when x1 = x2 -> check 0 (l1,l2) 
      |(x1::_,x2::_) -> compare_pre (x1,x2)
      |_,_ -> assert false
    in check 0 (l1,l2)

let compare_version x y =
  let res x = if x = 0 then 0 else if x < 0 then -1 else 1 in
  let c1 = Pervasives.compare x.major y.major in
  if c1 <> 0 then res c1
  else
    let c2 = Pervasives.compare x.minor y.minor in
    if c2 <> 0 then res c2
  else
    let c3 = Pervasives.compare x.patch y.patch in
    if c3 <> 0 then res c3
  else
    compare_pre (x.pre,y.pre)

let parse_and_compare x y =
  if x = y then 0 else
    let v1 = parse_version x in
    let v2 = parse_version y in
    compare_version v1 v2

let compare x y = parse_and_compare x y
let equal x y = (compare x y) = 0
