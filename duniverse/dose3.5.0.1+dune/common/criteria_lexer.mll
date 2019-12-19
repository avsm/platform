(**************************************************************************************)
(*  Copyright (C) 2011 Pietro Abate                                                   *)
(*  Copyright (C) 2011 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

{
  open Criteria_parser

  let get_regexp lexbuf =
    let open Lexing in
    let c = Lexing.lexeme_char lexbuf 2 in (* the delimiter can be any character *)
    (* find the terminating delimiter *)
    let endpos =
      try Bytes.index_from lexbuf.lex_buffer (lexbuf.lex_start_pos + 3) c with
      |Invalid_argument _ ->
          raise (Format822.Syntax_error (
            Format822.error lexbuf "String too short"))
      | Not_found ->
          raise (Format822.Syntax_error (
            Format822.error lexbuf (Printf.sprintf "cannot find: %c" c)))
    in
    let len = endpos - (lexbuf.lex_start_pos + 3) in
    let s = Bytes.sub_string lexbuf.lex_buffer (lexbuf.lex_start_pos + 3) len in
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_start_pos + ((String.length s)+4);
    s

}

let lower_letter = [ 'a' - 'z' ]
let upper_letter = [ 'A' - 'Z' ]
let letter = lower_letter | upper_letter
let digit = [ '0' - '9' ]
let blank = [ ' ' '\t' ]
let blanks = blank+
let symbols = ['-' '+' '.' '_' '~']
let ident = (letter | digit) (letter | digit | symbols)*

rule token = parse
  | "count"             { COUNT }
  | "sum"               { SUM }
  | "unsat_recommends"  { UNSATREC }
  | "aligned"           { ALIGNED }
  | "notuptodate"       { NOTUPTODATE }
  | "solution"          { SOLUTION }
  | "changed"           { CHANGED }
  | "new"               { NEW }
  | "removed"           { REMOVED }
  | "up"                { UP }
  | "down"              { DOWN }
  | ":="                { EXACT (get_regexp lexbuf) }
  | ":~"                { REGEXP (get_regexp lexbuf) }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ','                 { COMMA }
  | ident as s          { IDENT s }
  | blank+              { token lexbuf }
  | eof                 { EOL }
  | _ as c              { Format822.raise_error lexbuf c }
