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

open Cudf
open Cudf_types
open Cudf_types_pp


let pp_property out (n, s) = fprintf out "%s: %s\n" n s
let pp_io_property out (n, s) = IO.printf out "%s: %s\n" n s

let pp_sep out = output_char out '\n'
let pp_io_sep out = IO.write out '\n'

let pp_package_gen ~pp_property out pkg =
  let pp = pp_property out in
  pp ("package", string_of_pkgname pkg.package);
  pp ("version", string_of_version pkg.version);
  if pkg.depends <> default_package.depends then
    pp ("depends", string_of_vpkgformula pkg.depends);
  if pkg.conflicts <> default_package.conflicts then
    pp ("conflicts", string_of_vpkglist pkg.conflicts);
  if pkg.provides <> default_package.provides then
    pp ("provides", string_of_vpkglist (pkg.provides :> vpkg list));
  if pkg.installed <> default_package.installed then
    pp ("installed", string_of_bool pkg.installed);
  if pkg.was_installed <> default_package.was_installed then
    pp ("was-installed", string_of_bool pkg.was_installed);
  if pkg.keep <> default_package.keep then
    pp ("keep", string_of_keep pkg.keep);
  List.iter (fun (k, v) -> pp (k, string_of_value v)) pkg.pkg_extra


let pp_request_gen ~pp_property out req =
  let pp = pp_property out in
  pp ("request", req.request_id);
  if req.install <> default_request.install then
    pp ("install", string_of_vpkglist req.install);
  if req.remove <> default_request.remove then
    pp ("remove", string_of_vpkglist req.remove);
  if req.upgrade <> default_request.upgrade then
    pp ("upgrade", string_of_vpkglist req.upgrade);
  List.iter (fun (k, v) -> pp (k, string_of_value v)) req.req_extra


let pp_preamble_gen ~pp_property out pre =
  let pp = pp_property out in
  pp ("preamble", pre.preamble_id);
  if pre.property <> default_preamble.property then
    pp ("property", string_of_typedecl pre.property);
  if pre.univ_checksum <> default_preamble.univ_checksum then
    pp ("univ-checksum", pre.univ_checksum);
  if pre.status_checksum <> default_preamble.status_checksum then
    pp ("status-checksum", pre.status_checksum);
  if pre.req_checksum <> default_preamble.req_checksum then
    pp ("req-checksum", pre.req_checksum)

let pp_universe_gen ~pp_package ~pp_sep out univ =
  iter_packages (fun pkg -> pp_package out pkg; pp_sep out) univ

let pp_packages_gen ~pp_package ~pp_sep out pkgs =
  List.iter (fun pkg -> pp_package out pkg; pp_sep out) pkgs

let pp_cudf_gen ~pp_preamble ~pp_universe ~pp_request ~pp_sep out
    (pre, univ, req) =
  pp_preamble out pre;
  pp_sep out;
  pp_universe out univ;
  pp_request out req

let pp_doc_gen ~pp_preamble ~pp_packages ~pp_request ~pp_sep out (pre, pkgs, req) =
  Option.may (fun pre -> pp_preamble out pre; pp_sep out) pre;
  pp_packages out pkgs;
  pp_request out req

let pp_solution_gen ~pp_preamble ~pp_universe ~pp_sep out (pre, univ) =
  pp_preamble out pre;
  pp_sep out;
  pp_universe out univ

let pp_item_gen ~pp_package ~pp_request ~pp_preamble out = function
  | `Package pkg -> pp_package out pkg
  | `Request req -> pp_request out req
  | `Preamble pre -> pp_preamble out pre


(** {6 Pretty print to standard output channels} *)

let pp_package out p = pp_package_gen ~pp_property out p
let pp_request out r = pp_request_gen ~pp_property out r
let pp_preamble out p = pp_preamble_gen ~pp_property out p
let pp_universe out u = pp_universe_gen ~pp_package ~pp_sep out u
let pp_packages out p = pp_packages_gen ~pp_package ~pp_sep out p
let pp_cudf out c =
  pp_cudf_gen ~pp_preamble ~pp_universe ~pp_request ~pp_sep out c
let pp_doc out d =
  pp_doc_gen ~pp_preamble ~pp_packages ~pp_request ~pp_sep out d
let pp_solution out s = pp_solution_gen ~pp_preamble ~pp_universe ~pp_sep out s
let pp_item out i = pp_item_gen ~pp_package ~pp_request ~pp_preamble out i

(** {6 Pretty print to abstract output channels} *)

let pp_io_package out p = pp_package_gen ~pp_property:pp_io_property out p
let pp_io_request out r = pp_request_gen ~pp_property:pp_io_property out r
let pp_io_preamble out p = pp_preamble_gen ~pp_property:pp_io_property out p
let pp_io_universe out u =
  pp_universe_gen ~pp_package:pp_io_package ~pp_sep:pp_io_sep out u
let pp_io_packages out p =
  pp_packages_gen ~pp_package:pp_io_package ~pp_sep:pp_io_sep out p
let pp_io_cudf out c =
  pp_cudf_gen ~pp_preamble:pp_io_preamble
    ~pp_universe:pp_io_universe ~pp_request:pp_io_request ~pp_sep:pp_io_sep
    out c
let pp_io_doc out d =
  pp_doc_gen ~pp_preamble:pp_io_preamble ~pp_packages:pp_io_packages
    ~pp_request:pp_io_request ~pp_sep:pp_io_sep
    out d
let pp_io_solution out s =
  pp_solution_gen ~pp_preamble:pp_io_preamble ~pp_universe:pp_io_universe
    ~pp_sep:pp_io_sep out s
let pp_io_item out i =
  pp_item_gen ~pp_package:pp_io_package ~pp_request:pp_io_request
    ~pp_preamble:pp_io_preamble
    out i

