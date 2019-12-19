(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009-2015  Stefano Zacchiroli <zack@upsilon.cc>            *)
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

open Cudf
open Cudf_types

type cudf_parser = {
  lexbuf: Lexing.lexbuf ;
  fname: string ;
  mutable typedecl: Cudf_conf.stanza_typedecl ;
  priv_in_chan: in_channel option;
  (* in_channel to be closed upon close() invocation, to avoid leaving up to
     OCaml GC when to close it. Will be set only if it is Cudf_parser itself
     who has created the in_channel, e.g., upon Cudf_parser.from_file *)
}

type loc_map = (string * loc) list

exception Parse_error of string * loc
let parse_error loc msg = raise (Parse_error (msg, loc))

let from_in_channel ?(typedecl=Cudf_conf.stanza_typedecl) ic =
  { lexbuf = Lexing.from_channel ic ;
    typedecl = typedecl ;
    fname = "" ;
    priv_in_chan = None ;
  }

let from_IO_in_channel ?(typedecl=Cudf_conf.stanza_typedecl) ic =
  let f s n = try IO.input ic s 0 n with IO.No_more_input -> 0 in
  { lexbuf = Lexing.from_function f;
    typedecl = typedecl ;
    fname = "" ;
    priv_in_chan = None ;
  }

let from_file ?(typedecl=Cudf_conf.stanza_typedecl) fname =
  (* Syntax  error with OCaml 3.10.2:
   * { from_in_channel ?typedecl (open_in fname)
   *   with fname = fname } *)
  let ic = open_in fname in
  { lexbuf = Lexing.from_channel ic ;
    typedecl = typedecl ;
    fname = fname ;
    priv_in_chan = Some ic ;
  }

let close p =
  match p.priv_in_chan with
  | None -> ()
  | Some ic -> close_in ic

let parse_stanza p =
  try
    (match Cudf_822_parser.stanza_822 Cudf_822_lexer.token_822 p.lexbuf with
      | Some stanza ->
	  List.fold_right	(* split loc_map from (string * loc) stanzas *)
    		  (* non tail recursive, but should be ok: stanzas are short *)
	    (fun (k, (loc, v)) (locs, stanza) ->
	       (k, loc) :: locs, (k, v) :: stanza)
	    stanza ([], [])
      | None -> raise End_of_file)
  with Parse_error_822 (msg, loc) -> raise (Syntax_error (msg, loc))

let loc_lookuper locs =
  (fun p -> try List.assoc p locs
   with Not_found -> prerr_endline "non located property" ; assert false)

let type_check_stanza ?locs stanza types =
  let lookup_loc =
    match locs with
      | None -> (fun p -> dummy_loc)
      | Some locs -> loc_lookuper locs in
  let typed_stanza =
    List.map
      (fun (k, v) ->
	try
	  let decl = List.assoc k types in
	  let typed_v = Cudf_types_pp.parse_value (type_of_typedecl decl) v in
	  k, typed_v
	with
	  | Not_found ->
	    parse_error (lookup_loc k)
	      (sprintf "unexpected property \"%s\" in this stanza" k)
	  | Cudf_types_pp.Type_error (typ, v) ->	(* localize type errors *)
	    raise (Cudf_types.Type_error (typ, v, lookup_loc k)))
      stanza in
  let defaults, missing = (* deal with missing properties *)
    List.fold_left
      (fun (defaults, missing) (name, ty1) ->
	match value_of_typedecl ty1, List.mem_assoc name typed_stanza with
	  | None, true -> defaults, missing             (* mandatory, present *)
	  | None, false -> defaults, (name :: missing)  (* mandatory, missing *)
	  | Some v, true -> defaults, missing           (* optional,  present *)
	  | Some v, false ->                            (* optional,  missing *)
	    (name, v) :: defaults, missing)
      ([], []) types
  in
  if missing <> [] then begin
    let loc = match stanza with [] -> dummy_loc | (k,_) :: _ -> lookup_loc k in
    parse_error loc (sprintf "missing mandatory properties: %s"
		       (String.concat ", " missing))
  end;
  typed_stanza @ defaults

(** Cast a typed stanza starting with "package: " to a {!Cudf.package}.
    ASSUMPTION: type checking of the stanza has already happend, in particular
    all extra properties have already been checked for allowance. *)
let bless_package stanza =
  let p = default_package in	(* assumption: should be completely overrode *)
  let rec aux p = function
    | ("package", `Pkgname v) :: tl -> aux { p with package = v } tl
    | ("version", `Posint v) :: tl -> aux { p with version = v } tl
    | ("depends", `Vpkgformula v) :: tl -> aux { p with depends = v } tl
    | ("conflicts", `Vpkglist v) :: tl -> aux { p with conflicts = v } tl
    | ("provides", `Veqpkglist v) :: tl -> aux { p with provides = v } tl
    | ("installed", `Bool v) :: tl -> aux { p with installed = v } tl
    | ("was-installed", `Bool v) :: tl -> aux { p with was_installed = v } tl
    | ("keep", `Enum (_, v)) :: tl ->
	aux { p with keep = Cudf_types_pp.parse_keep v } tl
    | (k, (v: typed_value)) :: tl ->
	aux { p with pkg_extra = (k, v) :: p.pkg_extra } tl
    | [] -> p
  in
  let p' = aux p stanza in
  { p' with pkg_extra = List.rev p'.pkg_extra }

(** Cast a typed stanza starting with "preamble: " to a {!Cudf.preamble}
    ASSUMPTION: as per {!Cudf_parser.bless_package} above. *)
let bless_preamble stanza =
  let p = default_preamble in	(* assumption: should be completely overrode *)
  let rec aux p = function
    | ("preamble", `String v) :: tl -> aux { p with preamble_id = v } tl
    | ("property", `Typedecl v) :: tl -> aux { p with property = v } tl
    | ("univ-checksum", `String v) :: tl -> aux { p with univ_checksum = v } tl
    | ("status-checksum", `String v) :: tl -> aux { p with status_checksum = v } tl
    | ("req-checksum", `String v) :: tl -> aux { p with req_checksum = v } tl
    | [] -> p
    | _ -> assert false
  in
  aux p stanza

(** Cast a typed stanza starting with "request: " to a {!Cudf.request}.
    ASSUMPTION: as per {!Cudf_parser.bless_package} above. *)
let bless_request stanza =
  let r = default_request in	(* assumption: should be completely overrode *)
  let rec aux r = function
    | ("request", `String v) :: tl -> aux { r with request_id = v } tl
    | ("install", `Vpkglist v) :: tl -> aux { r with install = v } tl
    | ("remove", `Vpkglist v) :: tl -> aux { r with remove = v } tl
    | ("upgrade", `Vpkglist v) :: tl -> aux { r with upgrade = v } tl
    | (k, (v: typed_value)) :: tl ->
	aux { r with req_extra = (k, v) :: r.req_extra } tl
    | [] -> r
  in
  let r' = aux r stanza in
  { r' with req_extra = List.rev r'.req_extra }

let parse_item' p =
  let locs, stanza =
    try
      parse_stanza p
    with Syntax_error (msg, loc) -> parse_error loc msg in
  let lookup_loc = loc_lookuper locs in
  let typed_stanza =
    match stanza with
      | [] -> eprintf "empty stanza\n%!"; assert false
      | (postmark, _) :: _ ->
	  (try
	     type_check_stanza ~locs stanza (List.assoc postmark p.typedecl)
	   with Not_found ->
	     parse_error (lookup_loc postmark)
	       (sprintf "Unknown stanza type, starting with \"%s\" postmark."
		  postmark)) in
  let item =
    match typed_stanza with
      | [] -> assert false
      | ("preamble", _) :: _ ->
	  let preamble = bless_preamble typed_stanza in
	  p.typedecl <-	(* update type declaration for "package" stanza *)
	    (let pkg_typedecl =
	       (List.assoc "package" p.typedecl) @ preamble.property in
	     ("package", pkg_typedecl)
	     :: List.remove_assoc "package" p.typedecl);
	  `Preamble preamble
      | ("package", _) :: _ -> `Package (bless_package typed_stanza)
      | ("request", _) :: _ -> `Request (bless_request typed_stanza)
      | _ -> assert false in
  (locs, item)

let parse_item p = snd (parse_item' p)

let get_postmark = function
  | `Package pkg -> pkg.package
  | `Request req -> req.request_id
  | `Preamble pre -> pre.preamble_id

let parse p =
  let pre, pkgs, req = ref None, ref [], ref None in
  let rec aux_pkg () =
    match parse_item' p with
      | locs, `Package pkg -> pkgs := pkg :: !pkgs ; aux_pkg ()
      | locs, `Request req' -> req := Some req'	(* stop recursion after req *)
      | locs, `Preamble pre ->
	  parse_error (loc_lookuper locs pre.preamble_id) "late preamble"
  in
  let parse () =
    try
      (match parse_item p with	(* parse first item *)
	 | `Preamble pre' ->
	     pre := Some pre' ;
	     (try aux_pkg () with End_of_file -> ())
	 | `Package pkg ->
	     pkgs := [pkg] ;
	     (try aux_pkg () with End_of_file -> ())
	 | `Request req' -> req := Some req')
    with End_of_file -> ()
  in
  (try
     parse () ;
   with Cudf_types.Type_error (typ, v, loc) ->
     parse_error loc
       (sprintf
	  ("a value of type \"%s\" was expected, but \"%s\" has been found")
	  (Cudf_types_pp.string_of_type typ)
	  (Cudf_types_pp.string_of_value v)));
  (try	(* check for forbidden trailing content *)
     let locs, item = parse_item' p in
     parse_error (loc_lookuper locs (get_postmark item))
       "trailing stanzas after final request stanza"
   with End_of_file -> ());
  (!pre, List.rev !pkgs, !req)
  

let load p =
  let pre, pkgs, req = parse p in
  (pre, load_universe pkgs, req)

let load_solution p univ =
  let pre, sol_pkgs, _ = parse p in
  let expand_package pkg =
    let old_pkg =
      try
	lookup_package univ (pkg.package, pkg.version)
      with Not_found ->
	parse_error dummy_loc
	  (sprintf "unknown package (%s,%d) found in solution"
	     pkg.package pkg.version)
    in
    { old_pkg with installed = pkg.installed } in
  let sol_univ = load_universe (List.map expand_package sol_pkgs) in
  pre, sol_univ

let parser_wrapper ?typedecl fname f =
  let ic = open_in fname in
  let p = from_in_channel ?typedecl ic in
  finally (fun () -> close_in ic ; close p) f p

let parse_from_file ?typedecl fname = parser_wrapper ?typedecl fname parse
let load_from_file ?typedecl fname = parser_wrapper ?typedecl fname load
let load_solution_from_file fname univ =
  parser_wrapper fname (fun p -> load_solution p univ)
