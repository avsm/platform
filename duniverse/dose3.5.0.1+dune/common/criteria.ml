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
open Criteria_types

module Pcre = Re_pcre

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let lexbuf_wrapper type_parser v =
  Format822.lexbuf_wrapper type_parser Criteria_lexer.token v
let parse_criteria v = lexbuf_wrapper Criteria_parser.criteria_top v

(* Cudf field names are much more restrictive than deb822 field names which
  is why the deb822 field name has to be sanitized.
  The first eight hex chars of the md5sum of the fieldname, the match type
  and the search string are appended because:
  - sanitizing deb822 field names might make them not unique anymore
  - regexp may contain mostly special characters that would otherwise all
    be deleted, creating a non-unique field name
  - regexp might be very long but cutting of the regex might make the
    result non-unique
  - a hash solves all these problems because it contains only valid
    characters while being unique for any input (minus unlikely collisions)
 
  Cudf properties are identifiers as per cudf spec and must start with
  a lowercase latin letter, followed by lowercase latin letters, dashes
  or arabic numerals and must be of length one or greater.
  We restrict ourselves to US ASCII characters because checking for
  latin characters would be hard. We also ignore the length restriction
  and the special restriction for the beginning of a property because
  our new field below automatically has a sufficient length and starts
  with a lowercase latin character because of the "x-" prefix *)
let invalidchars = Pcre.regexp "[^0-9a-z-]"

(* replace all possibly illegal letters by a dash
  Get the first eight hex digits of the md5sum of the fieldname, the match
  type and the search string.
*)
let makefield ?(sep="=") fieldname regex =
  let regexhash =
    let s = fieldname^sep^regex in
    String.sub (Digest.to_hex (Digest.string s)) 0 8 
  in
  let sanitize s =
    let s = String.lowercase s in
    Pcre.substitute ~rex:invalidchars ~subst:(fun _ -> "-") s
  in
  Printf.sprintf "x-%s-%s" (sanitize fieldname) regexhash

let is_misc2012 = function
  |"mccs-cbc" | "mccs-lpsolve" -> false
  |"aspcud" | "packup" -> true
  | _ -> true (* we assume true by default *)

let to_string ?(solver="dumb") criteria =
  let pr = Printf.sprintf in
  let string_of_set = function
    | Solution -> "solution"
    | Changed -> "changed"
    | New -> "new"
    | Removed -> "removed"
    | Up -> "up"
    | Down -> "down"
  in
  if is_misc2012 solver then
    let l =
      List.map (fun pred ->
        let pred, crit =
          match pred with
          | Maximize s -> "+", s
          | Minimize s -> "-", s
        in
        let critstr =
          match crit with
          | Count (set, None) ->
              pr "count(%s)" (string_of_set set)
          | Count (set, Some (field, r)) ->
            let sep, r = match r with ExactMatch r -> ("=", r) | Regexp r -> ("~", r) in
            pr "sum(%s,%s)" (string_of_set set) (makefield ~sep field r)
          | Sum (set, attr) ->
              pr "sum(%s,%s)" (string_of_set set) attr
          | Unsatrec set -> pr "unsat_recommends(%s)" (string_of_set set)
          | Aligned (set, attr1, attr2) ->
              pr "aligned(%s,%s,%s)" (string_of_set set) attr1 attr2
          | NotUptodate set ->
              pr "notuptodate(%s)" (string_of_set set)
        in
        pred ^ critstr
      ) criteria
    in
    String.concat "," l
  else
    fatal "Solver Specific Optimizations (%s) are not recognized." solver

(* compile the regex so that this doesn't need to be done later *)
(* TODO: the regex should probably be multiline? *)
let iter f =
  List.iter (function
    | Minimize (Count(_,Some(fieldname,regex))) 
    | Maximize (Count(_,Some(fieldname,regex))) ->
      let regexstring, sep, compiled_re = match regex with
        | Regexp r -> (r, "~", Some(Pcre.regexp r))
        | ExactMatch r -> (r, "=", None)
      in
      let cudffieldname = makefield ~sep fieldname regexstring in
      f (cudffieldname,fieldname,regexstring,compiled_re)
    | _ -> ()
  )

let default_criteria = 
  let minnew = Minimize(Count(New,None)) in
  let minrem = Minimize(Count(Removed,None)) in
  let minupto = Minimize(NotUptodate(Solution)) in
  let minch = Minimize(Count(Changed,None)) in
  let minunsat = Minimize(Unsatrec(Solution)) in
  [
  "upgrade", [minnew;minrem;minupto];
  "dist-upgrade", [minupto;minnew];
  "install", [minrem;minch];
  "remove", [minrem;minch];
  "paranoid", [minrem;minch];
  "trendy", [minrem;minupto;minunsat;minnew];
]
