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

type version = int
type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]
type constr = (relop * version) option

type pkgname = string
type vpkg = pkgname * constr
type vpkglist = vpkg list
type vpkgformula = vpkg list list
type veqpkg = pkgname * ([`Eq] * version) option
type veqpkglist = veqpkg list
type enum_keep = [`Keep_version | `Keep_package | `Keep_feature | `Keep_none ]

type typ =
    [ `Int | `Posint | `Nat | `Bool | `String | `Enum of string list
    | `Pkgname | `Ident
    | `Vpkg | `Vpkgformula | `Vpkglist | `Veqpkg | `Veqpkglist
    | `Typedecl ]
type typedecl1 =
    [ `Int of int option | `Posint of int option | `Nat of int option
    | `Bool of bool option | `String of string option
    | `Pkgname of string option | `Ident of string option
    | `Enum of string list * string option | `Vpkg of vpkg option
    | `Vpkgformula of vpkgformula option | `Vpkglist of vpkglist option
    | `Veqpkg of veqpkg option | `Veqpkglist of veqpkglist option
    | `Typedecl of typedecl option ]
and typedecl = (string * typedecl1) list
type typed_value =
    [ `Int of int | `Posint of int | `Nat of int | `Bool of bool
    | `String of string | `Pkgname of string | `Ident of string
    | `Enum of string list * string | `Vpkg of vpkg
    | `Vpkgformula of vpkgformula | `Vpkglist of vpkglist
    | `Veqpkg of veqpkg | `Veqpkglist of veqpkglist
    | `Typedecl of typedecl ]

type 'ty stanza = (string * 'ty) list

type loc = Lexing.position * Lexing.position
let dummy_loc: loc = Lexing.dummy_pos, Lexing.dummy_pos
let extend_loc (r1_start, _r1_end) (_r2_start, r2_end) = (r1_start, r2_end)
let loc_of_lexbuf b = (b.Lexing.lex_start_p, b.Lexing.lex_curr_p)

exception Parse_error_822 of string * loc	(* <msg, loc> *)
exception Syntax_error of string * loc		(* <msg, loc> *)
exception Type_error of typ * typed_value * loc	(* <type, literal, loc> *)


let keep_enums = ["version"; "package"; "feature"; "none"]
let keep_type = `Enum keep_enums

let type_of_typedecl = function
  | `Int _ -> `Int
  | `Posint _ -> `Posint
  | `Nat _ -> `Nat
  | `Bool _ -> `Bool
  | `String _ -> `String
  | `Pkgname _ -> `Pkgname
  | `Ident _ -> `Ident
  | `Enum (enums, _) -> `Enum enums
  | `Vpkg _ -> `Vpkg
  | `Vpkgformula _ -> `Vpkgformula
  | `Vpkglist _ -> `Vpkglist
  | `Veqpkg _ -> `Veqpkg
  | `Veqpkglist _ -> `Veqpkglist
  | `Typedecl _ -> `Typedecl

let typedecl_of_type = function
  | `Int -> `Int None
  | `Posint -> `Posint None
  | `Nat -> `Nat None
  | `Bool -> `Bool None
  | `String -> `String None
  | `Pkgname -> `Pkgname None
  | `Ident -> `Ident None
  | `Enum enums -> `Enum (enums, None)
  | `Vpkg -> `Vpkg None
  | `Vpkgformula -> `Vpkgformula None
  | `Vpkglist -> `Vpkglist None
  | `Veqpkg -> `Veqpkg None
  | `Veqpkglist -> `Veqpkglist None
  | `Typedecl -> `Typedecl None

let typedecl_of_value = function
  | `Int n -> `Int (Some n)
  | `Posint n -> `Posint (Some n)
  | `Nat n -> `Nat (Some n)
  | `Bool b -> `Bool (Some b)
  | `String s -> `String (Some s)
  | `Pkgname s -> `Pkgname (Some s)
  | `Ident s -> `Ident (Some s)
  | `Enum (enums, s) -> `Enum (enums, Some s)
  | `Vpkg p -> `Vpkg (Some p)
  | `Vpkgformula f -> `Vpkgformula (Some f)
  | `Vpkglist l -> `Vpkglist (Some l)
  | `Veqpkg p -> `Veqpkg (Some p)
  | `Veqpkglist l -> `Veqpkglist (Some l)
  | `Typedecl l -> `Typedecl (Some l)

let value_of_typedecl = function
  | `Int (Some v) -> Some (`Int v)
  | `Posint (Some v) -> Some (`Posint v)
  | `Nat (Some v) -> Some (`Nat v)
  | `Bool (Some v) -> Some (`Bool v)
  | `String (Some v) -> Some (`String v)
  | `Pkgname (Some v) -> Some (`Pkgname v)
  | `Ident (Some v) -> Some (`Ident v)
  | `Enum (enums, (Some v)) -> Some (`Enum (enums, v))
  | `Vpkg (Some v) -> Some (`Vpkg v)
  | `Vpkgformula (Some v) -> Some (`Vpkgformula v)
  | `Vpkglist (Some v) -> Some (`Vpkglist v)
  | `Veqpkg (Some v) -> Some (`Veqpkg v)
  | `Veqpkglist (Some v) -> Some (`Veqpkglist v)
  | `Typedecl (Some v) -> Some (`Typedecl v)
  | _ -> None

let type_of_value = function
  | `Int n -> `Int
  | `Posint n -> `Posint
  | `Nat n -> `Nat
  | `Bool b -> `Bool
  | `String s -> `String
  | `Pkgname s -> `Pkgname
  | `Ident s -> `Ident
  | `Enum (enums, s) -> `Enum enums
  | `Vpkg p -> `Vpkg
  | `Vpkgformula f -> `Vpkgformula
  | `Vpkglist l -> `Vpkglist
  | `Veqpkg p -> `Veqpkg
  | `Veqpkglist l -> `Veqpkglist
  | `Typedecl l -> `Typedecl

let rec cast typ v =
  let type_error () = raise (Type_error (typ, v, dummy_loc)) in
  match typ, v with
    | `Posint, `Int n when n > 0 -> `Posint n
    | `Nat, `Int n when n >= 0 -> `Nat n
    | `Bool, `Ident "true" -> `Bool true
    | `Bool, `Ident "false" -> `Bool false
    | `Pkgname, `Vpkgformula [[(pkg, None)]] -> `Pkgname pkg
    | `Pkgname, (`Int n | `Posint n | `Nat n) -> `Pkgname (string_of_int n)
    | `Pkgname, `Ident i-> `Pkgname i
    | (`Vpkg | `Veqpkg | `Vpkglist | `Veqpkglist),
      (`Int n | `Posint n | `Nat n) ->
	cast typ (`Vpkgformula [[string_of_int n, None]])
    | (`Vpkg | `Veqpkg | `Vpkglist | `Veqpkglist), `Ident i ->
	cast typ (`Vpkgformula [[i, None]])
    | `Vpkg, `Vpkgformula [[vpkg]] -> `Vpkg vpkg
    | (`Vpkglist | `Veqpkglist),
      (`Vpkgformula [] (* "true!" *) | `Vpkgformula [ [] ] (* "false!" *)) ->
        type_error ()
    | `Vpkglist, `Vpkgformula f ->
	if List.exists (function _ :: _ :: _ -> true | _ -> false) f then
	  type_error ()	(* there are OR-ed deps *)
	else
	  `Vpkglist (List.map (function [vpkg] -> vpkg | _ -> assert false) f)
    | `Veqpkg, `Vpkgformula [[ (_, (Some (`Eq, _) | None)) as vpkg ]] ->
	`Veqpkg vpkg
    | `Veqpkglist, `Vpkgformula f ->
	`Veqpkglist
	  (List.fold_right
	     (fun or_deps veqpkgs ->
		match or_deps with
		  | [ (_, (Some (`Eq, _) | None)) as vpkg ] -> vpkg :: veqpkgs
		  | _ -> type_error ())
	     f [])
    | `Veqpkg, `Vpkg ((_, (Some (`Eq, _) | None)) as vpkg) -> `Veqpkg vpkg
    | `Veqpkglist, `Vpkglist l ->
	`Veqpkglist
	  (List.fold_right
	     (fun vpkg veqpkgs ->
		match vpkg with
		  | (_, (Some (`Eq, _) | None)) as vpkg -> vpkg :: veqpkgs
		  | _ -> type_error ())
	     l [])
    | `Enum enums, `Ident i when List.mem i enums -> `Enum (enums, i)
    | `Vpkgformula, `Ident i -> `Vpkgformula [[i, None]]
    | `Vpkgformula, `Int n -> `Vpkgformula [[string_of_int n, None]]
    | typ, v when type_of_value v = typ -> v	(* identity cast *)
    | _ -> type_error ()

let rec is_eq_formula f =
  not (List.exists
	 (fun vpkgs ->
	    List.exists
	      (function
		 | (_, Some ((`Neq | `Geq | `Gt | `Leq | `Lt), _)) -> true
		 | _ -> false)
	      vpkgs)
	 f)

