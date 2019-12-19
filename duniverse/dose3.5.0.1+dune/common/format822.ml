(**************************************************************************************)
(*  Copyright (C) 2009-2015 Pietro Abate <pietro.abate@pps.jussieu.fr>                *)
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

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

type loc = Lexing.position * Lexing.position
type value = (loc * string)
type field = (string * value)
type stanza = field list
type doc = stanza list

let dummy_loc: loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Parse_error_822 of string
exception Syntax_error of string
exception Type_error of string
(** ParseError context list, field name * error *)
exception ParseError of string list * string * string

let error lexbuf msg =
  let curr = lexbuf.Lexing.lex_curr_p in
  let start = lexbuf.Lexing.lex_start_p in
  if curr.Lexing.pos_fname = "" then
    Printf.sprintf "character %d-%d: %s."
      (start.Lexing.pos_cnum - start.Lexing.pos_bol)
      (curr.Lexing.pos_cnum - curr.Lexing.pos_bol)
      msg
  else
    Printf.sprintf
      "File %S, line %d, character %d-%d: %s."
      curr.Lexing.pos_fname
      start.Lexing.pos_lnum
      (start.Lexing.pos_cnum - start.Lexing.pos_bol)
      (curr.Lexing.pos_cnum - curr.Lexing.pos_bol)
      msg

let raise_error lexbuf c =
  let msg = Printf.sprintf "Unexpected token : '%c'" c in
  raise (Parse_error_822 (error lexbuf msg))

let error_wrapper t f lexer lexbuf =
  let syntax_error msg =
    raise (Syntax_error (Printf.sprintf "%s (%s)" (error lexbuf msg) t))
  in
  try f lexer lexbuf with
  |Parsing.Parse_error -> syntax_error "parse error"
  |Parse_error_822 s -> syntax_error s
  |Failure _m when String.starts_with _m "lexing" -> syntax_error "lexer error"
  |Type_error _ -> syntax_error "type error"

(* here the _loc is taken from the the caller and not from the parser *)
let lexbuf_wrapper type_parser type_lexer (label,(_loc,s)) =
  try type_parser type_lexer (Lexing.from_string s) with
  |Syntax_error (m) ->
      let msg = Printf.sprintf "Field %s has a wrong value (%s): '%s'" label m s in
      raise (ParseError ([],label,msg))
  |Parsing.Parse_error ->
      let msg = Printf.sprintf "Field %s has a wrong value: '%s'" label s in
      raise (ParseError ([],label,msg))

let string_of_loc (start_pos, end_pos) =
  let line { Lexing.pos_lnum = l } = l in
  if line start_pos = line end_pos then
    Printf.sprintf "line: %d" (line start_pos)
  else
    Printf.sprintf "lines: %d-%d" (line start_pos) (line end_pos)

type f822_parser = { lexbuf: Lexing.lexbuf ; fname: string }

let from_channel ic =
  let f s n = try IO.input ic s 0 n with IO.No_more_input -> 0 in
  { lexbuf = Lexing.from_function f ; fname = "from-input-channel" }

(* since somebody else provides the channel, we do not close it here *)
let parser_wrapper_ch ic _parser = _parser (from_channel ic)

let parse_from_ch _parser ic =
  try parser_wrapper_ch ic _parser with 
  |Syntax_error (msg) -> fatal "%s" msg
  |Parse_error_822 (msg) -> fatal "%s" msg

let timer = Util.Timer.create "Format822" 

module RawInput ( Set : Set.S ) = struct

  let input_raw parse files =
    Util.Timer.start timer;
    if List.length files > 1 then info "Merging repositories" ;
    let s =
      List.fold_left (fun acc file ->
        try
          let ch =
           match file with
           (* XXX not sure about this, maybe it should be an option
            * insted of "-" ...  *)
           |"-" -> IO.input_channel stdin 
           |_   -> Input.open_file file
          in 
          let l = parse file ch in
          let _ = Input.close_ch ch in
          List.fold_left (fun s x -> Set.add x s) acc l
        with Input.File_empty -> acc
      ) Set.empty files
    in
    Parsing.clear_parser ();
    info "total packages %n" (Set.cardinal s);
    Util.Timer.stop timer (Set.elements s)

  let input_raw_in parse ch =
    Util.Timer.start timer;
    let s =
      let l = parse "" ch in
      List.fold_left (fun s x -> Set.add x s) Set.empty l
    in
    info "total packages %n" (Set.cardinal s);
    Util.Timer.stop timer (Set.elements s)
end
