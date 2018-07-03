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

(** Synthesis rpm parser *)

open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let progressbar = Util.Progress.create "Rpm.Parse.Hdlists.parse_822_iter" ;;
Util.Progress.set_total progressbar 8000 (* estimate *) ;

exception Hdlist_Eof
let _ = Callback.register_exception "hdlist.eof" (Hdlist_Eof) ;;

type ic
type field = string
type constr = ( int * string )
type data = 
  | S of string                       (* name, version, ... *)
  | L of string list                  (* filepaths *)
  | D of (string * constr option ) list    (* dependencies *)
type stanza = ( field * data ) list

(* bindings to the librpm *)
external _open_in : string -> ic = "rpm_open_hdlist"
external _close_in : ic -> unit = "rpm_close_hdlist"
external parse_paragraph : ic -> stanza = "rpm_parse_paragraph"

let decode_flags f =
  match f land 15 with
  | 0 -> `ALL
  | 2 -> `Lt
  |10 -> `Leq
  | 8 -> `Eq
  |12 -> `Geq
  | 4 -> `Gt
  |_ -> fatal "Wrong flag %d" (f land 15)

let string_of_rel = function
  | `Lt -> "<"
  | `Leq  -> "<="
  | `Eq -> "="
  | `Geq  -> ">="
  | `Gt -> ">"
  | `ALL -> "ALL"

let get_string ?(opt=true) field par =
  try
    match List.assoc field par with
    |S s -> s
    |_ -> assert false
  with Not_found ->
    if opt then "" else raise Not_found
;;

let get_list ?(opt=true) field par =
  try
    match List.assoc field par with
    |L sl -> sl
    |_ -> assert false
  with Not_found ->
    if opt then [] else raise Not_found
;;

let get_deplist ?(opt=true) field par =
  try
    match List.assoc field par with
    |D sl -> 
        List.map (function
          |(n,None) -> (n,None)
          |(n,Some(i,v)) -> (n,Some((decode_flags i),v))
        ) sl
    |_ -> assert false
  with Not_found ->
    if opt then [] else raise Not_found
;;

let dump_raw ppf s par =
  List.iter (function
    |(f,S data) -> (
      Format.fprintf ppf "%s\n%s: %s\n@." s f data
    )
    |(f,L datalist) -> (
      Format.fprintf ppf "%s\n%s: %s\n@." s f (String.concat "," datalist)
    )
    |(f,D datalist) -> (
      List.iter (function
        |(n,None) -> Format.fprintf ppf "%s: %s\n@." f n
        |(n,Some(i,v)) -> 
            let op = string_of_rel (decode_flags i) in
            Format.fprintf ppf "%s: %s %s %s \n@." f n op v
      ) datalist
    )
  ) par
;;

let parse_822_iter parse f ch =
  let acc = ref [] in
  try 
    while true do
      let par = parse_paragraph ch in
      match parse par with
      |None -> ()
      |Some p -> acc := p :: !acc
    done ; !acc
  with Hdlist_Eof -> !acc
;;

let dump ppf fname =
  let ch = _open_in fname in
  try 
    while true do
      Util.Progress.progress progressbar ;
      let par = parse_paragraph ch in
      dump_raw ppf "" par
    done 
  with Hdlist_Eof -> _close_in ch
;;
