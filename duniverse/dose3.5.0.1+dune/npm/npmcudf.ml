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

(** Opam/Pef format conversion routines *)

open ExtLib
open Common

#define __label __FILE__ 
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let preamble =
  let l = [
    ("number",(`String None));
    ]
  in
  CudfAdd.add_properties Cudf.default_preamble l

(* version-lag is the distance from the most recent package *)
let add_extra extras tables pkg =
  let number = ("number",`String pkg#version) in
  number :: extras

let load_list l =
  let timer = Util.Timer.create "Npm.ToCudf" in
  Util.Timer.start timer;
  let tables = Pef.Pefcudf.init_tables Versioning.SemverNode.compare l in
  let pkglist = List.map (Pef.Pefcudf.tocudf tables) l in
  Pef.Pefcudf.clear tables;
  Util.Timer.stop timer pkglist

let load_universe l =
  let pkglist = load_list l in
  let timer = Util.Timer.create "Npm.ToCudf" in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ

