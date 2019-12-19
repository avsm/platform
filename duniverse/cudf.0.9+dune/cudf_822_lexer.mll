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

(** Lexer for CUDF 822 surface syntax *)

{
  open Cudf_types
  open Cudf_822_parser

  let get_range { Lexing.lex_start_p = start_pos;
		  Lexing.lex_curr_p = end_pos } =
    (start_pos, end_pos)

  (* Lexing.new_line is only available in OCaml 3.11 or greater *)
  (* let lexing_new_line = Lexing.new_line *)
  let lexing_new_line lexbuf =
    let lcp = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { lcp with
	  Lexing.pos_lnum = lcp.Lexing.pos_lnum + 1;
	  Lexing.pos_bol = lcp.Lexing.pos_cnum; }

}

let lower_letter = [ 'a' - 'z' ]
let digit = [ '0' - '9' ]
let blank = [ ' ' '\t' ]
let ident = lower_letter (lower_letter | digit | '-')*

rule token_822 = parse
  | (ident as field) ':' ' '
    ([^'\n']* as rest)		{ FIELD(field, (get_range lexbuf, rest)) }
  | ' ' ([^'\n']* as rest)	{ CONT(get_range lexbuf, rest) }
  | '#' [^'\n']* ('\n'|eof)	{ token_822 lexbuf }
  | blank* '\n'			{ lexing_new_line lexbuf;
				  EOL }
  | eof				{ EOF }
  | _				{ raise (Parse_error_822
					   ("unexpected RFC 822 token",
					    (lexbuf.Lexing.lex_start_p,
					     lexbuf.Lexing.lex_curr_p))) }
