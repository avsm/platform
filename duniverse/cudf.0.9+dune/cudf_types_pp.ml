(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009-2012  Stefano Zacchiroli <zack@upsilon.cc>            *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

open ExtLib
open Printf

open Cudf_types

(* note: Type_error <> Cudf_types.Type_error, this one is not located *)
exception Type_error of typ * typed_value

let lexbuf_wrapper type_parser typ =
  fun s ->
    try
      type_parser Cudf_type_lexer.token_cudf (Lexing.from_string s)
    with Cudf_types.Syntax_error (_msg, loc) ->
      raise (Type_error (typ, `String s))

let lexbuf_wrapper' type_parser =
  fun s ->
    type_parser Cudf_type_lexer.token_cudf (Lexing.from_string s)

let parse_int = lexbuf_wrapper Cudf_type_parser.int_top `Int
let parse_ident = lexbuf_wrapper Cudf_type_parser.ident_top `Ident
let parse_pkgname = lexbuf_wrapper Cudf_type_parser.pkgname_top `Pkgname
let parse_vpkg = lexbuf_wrapper Cudf_type_parser.vpkg_top `Vpkg
let parse_vpkglist = lexbuf_wrapper Cudf_type_parser.vpkglist_top `Vpkglist
let parse_vpkgformula =
  lexbuf_wrapper Cudf_type_parser.vpkgformula_top `Vpkgformula
let parse_typedecl = lexbuf_wrapper Cudf_type_parser.typedecl_top `Typedecl

let parse_qstring = lexbuf_wrapper' Cudf_type_parser.qstring_top
let parse_type = lexbuf_wrapper' Cudf_type_parser.type_top


(** DEFCON 4, use with care!

    Rationale: to avoid duplicating code we have the cast checks enclosed only
    in the [cast] function. After having used it however, we will have to
    extract the contained typed value. To avoid writing several functions
    extracting the appropriate value and [assert false] everywhere else we
    cheat with [Obj.magic].
*)
let unbox v = snd (Obj.magic v: 'a * 'b)

let cast' typ v =
  try
    cast typ v
  with Cudf_types.Type_error _ -> raise (Type_error (typ, v))

let parse_posint s: int = unbox (cast' `Posint (`Int (parse_int s)))
let parse_nat s: int = unbox (cast' `Nat (`Int (parse_int s)))
let parse_bool s: bool = unbox (cast' `Bool (`Ident (parse_ident s)))
let parse_veqpkg s: veqpkg = unbox (cast' `Veqpkg (`Vpkg (parse_vpkg s)))
let parse_veqpkglist s: veqpkglist =
  unbox (cast' `Veqpkglist (`Vpkglist (parse_vpkglist s)))

let parse_enum ~enums s =
  match cast' (`Enum enums) (`Ident (parse_ident s)) with
    | `Enum (_, i) -> i
    | _ -> assert false

let parse_keep = function
  | "version" -> `Keep_version
  | "feature" -> `Keep_feature
  | "package" -> `Keep_package
  | "none" -> `Keep_none
  | i -> raise (Type_error (Cudf_types.keep_type, `Ident i))

let parse_string s =
  let type_error () = raise (Type_error (`String, `String s)) in
  (try ignore (String.index s '\n') ; type_error () with Not_found -> ());
  (try ignore (String.index s '\r') ; type_error () with Not_found -> ());
  s

let parse_value ty s =
  match ty with
    | `Int -> `Int (parse_int s)
    | `Posint -> `Posint (parse_posint s)
    | `Nat -> `Nat (parse_nat s)
    | `Bool -> `Bool (parse_bool s)
    | `String -> `String (parse_string s)
    | `Enum l -> `Enum (l, parse_enum l s)
    | `Pkgname -> `Pkgname (parse_pkgname s)
    | `Ident -> `Ident (parse_ident s)
    | `Vpkg -> `Vpkg (parse_vpkg s)
    | `Vpkglist -> `Vpkglist (parse_vpkglist s)
    | `Vpkgformula -> `Vpkgformula (parse_vpkgformula s)
    | `Veqpkg -> `Veqpkg (parse_veqpkg s)
    | `Veqpkglist -> `Veqpkglist (parse_veqpkglist s)
    | `Typedecl -> `Typedecl (parse_typedecl s)

(** Pretty printers *)

let string_of_int = Pervasives.string_of_int
let string_of_posint = string_of_int
let string_of_nat = string_of_int
let string_of_bool = Pervasives.string_of_bool

let string_of_keep = function
    `Keep_version -> "version"
  | `Keep_package -> "package"
  | `Keep_feature -> "feature"
  | `Keep_none -> "none"

let string_of_pkgname pkgname = pkgname
let string_of_version = string_of_int

let string_of_relop = function
    `Eq -> "="
  | `Neq -> "!="
  | `Geq -> ">="
  | `Gt -> ">"
  | `Leq -> "<="
  | `Lt -> "<"

let string_of_vpkg = function
    (name, None) -> name
  | (name, Some (relop, v)) -> sprintf "%s %s %d" name (string_of_relop relop) v

let string_of_list string_of_item sep l =
  let buf = Buffer.create 1023 in
  let rec aux = function
    | [] -> assert false
    | [last] -> (* last item, no trailing sep *)
        Buffer.add_string buf (string_of_item last)
    | item :: tl -> (* at least one item in tl *)
        Buffer.add_string buf (string_of_item item);
        Buffer.add_string buf sep;
        aux tl in
  let _ = 
    match l with
      | [] -> ()
      | [sole] -> Buffer.add_string buf (string_of_item sole)
      | _ -> aux l in
  Buffer.contents buf

let string_of_vpkglist = string_of_list string_of_vpkg " , "

(** ASSUMPTION: formula is in CNF *)
let rec string_of_vpkgformula = function
  | [] -> "true!"
  | [ [] ] -> "false!"
  | [] :: _ ->
      eprintf "malformed vpkgformula: `[] :: _' ; aborting\n%!";
      assert false
  | fmla ->
      let string_of_OR = string_of_list string_of_vpkg " | " in
      let string_of_AND = string_of_list string_of_OR " , " in
      string_of_AND fmla

let string_of_veqpkglist l = string_of_vpkglist (l :> vpkglist)
let string_of_veqpkg = string_of_vpkg

let string_of_type = function
  | `Int -> "int"
  | `Posint -> "posint"
  | `Nat -> "nat"
  | `Bool -> "bool"
  | `String -> "string"
  | `Enum enums -> sprintf "enum[%s]" (String.concat "," enums)
  | `Pkgname -> "pkgname"
  | `Ident -> "ident"
  | `Vpkg -> "vpkg"
  | `Vpkgformula -> "vpkgformula"
  | `Vpkglist -> "vpkglist"
  | `Veqpkg -> "veqpkg"
  | `Veqpkglist -> "veqpkglist"
  | `Typedecl -> "typedecl"

let rec string_of_typedecl' (name, decl1) =
  let string_escape =
    String.replace_chars
      (function '"' -> "\\\"" | '\\' -> "\\\\" | c -> String.of_char c) in
  match value_of_typedecl decl1 with
    | None -> sprintf "%s: %s" name (string_of_type (type_of_typedecl decl1))
    | Some (`String s) -> sprintf "%s: string = [\"%s\"]" name (string_escape s)
    | Some v ->
        sprintf "%s: %s = [%s]"
	  name (string_of_type (type_of_typedecl decl1)) (string_of_value v)

and string_of_value (v: typed_value) = match v with
  | (`Int i | `Posint i | `Nat i) -> string_of_int i
  | `Bool b -> string_of_bool b
  | (`String s | `Pkgname s | `Ident s | `Enum (_, s)) -> s
  | `Vpkg p -> string_of_vpkg p
  | `Veqpkg p -> string_of_vpkg p
  | `Vpkglist l -> string_of_vpkglist l
  | `Veqpkglist l -> string_of_veqpkglist l
  | `Vpkgformula f -> string_of_vpkgformula f
  | `Typedecl d -> string_of_typedecl d

and string_of_typedecl decl = string_of_list string_of_typedecl' ", " decl

