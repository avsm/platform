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

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let to_string_with_label (k,v) =
  if v <> "" then Printf.sprintf "%s: %s" k v else ""

let string_of_vpkg = function
  |((name,None),None) -> name
  |((name,Some arch),None) -> Printf.sprintf "%s:%s" name arch
  |((name,Some arch),Some(op,ver)) -> Printf.sprintf "%s:%s (%s %s)" name arch op ver
  |((name,None),Some(op,ver)) -> Printf.sprintf "%s (%s %s)" name op ver

let string_of_vpkglist vpkglist =
  Util.string_of_list ~sep:", " string_of_vpkg vpkglist

let string_of_vpkgformula vpkgformula =
  let string_of_OR = Util.string_of_list ~sep:" | " string_of_vpkg in
  let string_of_AND = Util.string_of_list ~sep:", " string_of_OR in
  string_of_AND vpkgformula

let string_of_builddep (vpkg,archfilter,buildfilter) =
  let string_of_filter l =
    String.concat " " (
      List.map (fun (b,s) ->
        if b then s else "!"^s
      ) l
    )
  in
  let string_of_bpformula ll =
    String.concat " " (
      List.map (fun l ->
          Printf.sprintf "<%s>" (string_of_filter l)
        ) ll
    )
  in
  match archfilter,buildfilter with
  |[],[] -> string_of_vpkg vpkg
  |_,[] -> Printf.sprintf "%s [%s]" (string_of_vpkg vpkg) (string_of_filter archfilter)
  |[],_ -> Printf.sprintf "%s %s" (string_of_vpkg vpkg) (string_of_bpformula buildfilter)
  |_,_ ->
      Printf.sprintf "%s [%s] %s"
        (string_of_vpkg vpkg) 
        (string_of_filter archfilter)
        (string_of_bpformula buildfilter)

let string_of_builddepformula builddepformula =
  let string_of_OR = Util.string_of_list ~sep:" | " string_of_builddep in
  let string_of_AND = Util.string_of_list ~sep:", " string_of_OR in
  string_of_AND builddepformula

let string_of_builddeplist builddeplist =
  Util.string_of_list ~sep:", " string_of_builddep builddeplist

let string_of_vpkgreq = function
  | None,vpkg,None -> string_of_vpkg vpkg
  | None,vpkg,Some suite -> Printf.sprintf "%s/%s" (string_of_vpkg vpkg) suite
  | Some Packages_types.I, vpkg, None -> Printf.sprintf "+%s" (string_of_vpkg vpkg)
  | Some Packages_types.R, vpkg, None -> Printf.sprintf "-%s" (string_of_vpkg vpkg)
  | Some Packages_types.I, vpkg, Some suite -> Printf.sprintf "+%s/%s" (string_of_vpkg vpkg) suite
  | Some Packages_types.R, vpkg, Some suite -> Printf.sprintf "-%s/%s" (string_of_vpkg vpkg) suite

(** *)

let pp_function oc ~tostring (k,v) =
  match tostring v with
  |"" -> ()
  |s -> Printf.fprintf oc "%s: %s" k s

let pp_string_list ?(sep=", ") oc (k,v) =
  if List.length v > 0 then
    Printf.fprintf oc "%s: %s" k (Util.string_of_list ~sep (fun s -> s) v)

let pp_vpkg oc vpkg = Printf.fprintf oc "%s" (string_of_vpkg vpkg)
let pp_vpkglist oc vpkglist = Printf.fprintf oc "%s" (string_of_vpkglist vpkglist)
let pp_vpkgformula oc vpkgformula =
  Printf.fprintf oc "%s" (string_of_vpkgformula vpkgformula)

let pp_builddep oc builddep =
  Printf.fprintf oc "%s" (string_of_builddep builddep)
let pp_builddepformula oc builddepformula =
  Printf.fprintf oc "%s" (string_of_builddepformula builddepformula)
let pp_builddeplist oc builddeplist =
    Printf.fprintf oc "%s" (string_of_builddeplist builddeplist)

(** _wl -> with label *)

let pp_string_wl oc (k,v) =
  if v <> "" then Printf.fprintf oc "%s\n" (to_string_with_label (k,v))
let pp_bool_wl oc (k,v) =
  if v then
    Printf.fprintf oc "%s\n" (to_string_with_label (k,string_of_bool v))
let pp_yes_wl oc (k,v) =
  if v then
    Printf.fprintf oc "%s\n" (to_string_with_label (k,(if v then "yes" else "no")))

let pp_list_wl_aux f oc = function
  |(k,[]) -> ()
  |(k,v) -> Printf.fprintf oc "%s: %a\n" k f v

let pp_vpkglist_wl = pp_list_wl_aux pp_vpkglist
let pp_vpkgformula_wl = pp_list_wl_aux pp_vpkgformula
let pp_builddeplist_wl = pp_list_wl_aux pp_builddeplist
let pp_builddepformula_wl = pp_list_wl_aux pp_builddepformula

let pp_function_wl oc ~tostring (k,v) =
  let s = tostring v in
  if s <> "" then Printf.fprintf oc "%s\n" (to_string_with_label (k,s))
let pp_string_list_wl ?(sep=", ") oc (k,v) =
  if List.length v > 0 then
    Printf.fprintf oc "%a\n" (pp_string_list ~sep) (k,v)

