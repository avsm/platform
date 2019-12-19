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

(** Lexer for CUDF values *)

{
  open Cudf_types
  open Cudf_type_parser
}

let lower_letter = [ 'a' - 'z' ]
let upper_letter = [ 'A' - 'Z' ]
let letter = lower_letter | upper_letter
let digit = [ '0' - '9' ]
let blank = [ ' ' '\t' ]
let blanks = blank+
let ident = lower_letter (lower_letter | digit | '-')*
let pkgname = (letter | digit | ['-' '+' '.' '/' '@' '(' ')' '%'])+

rule token_cudf = parse
  | "true!"		{ VPKGTRUE }
  | "false!"		{ VPKGFALSE }
  | ident as s		{ IDENT s }
  | '+'? (digit+ as s)		{ POSINT s }
  | '-' digit+ as s	{ NEGINT s }
  | pkgname as s	{ PKGNAME s }
  | (">=" | "<=") as op	{ RELOP op }
  | "!=" as op		{ RELOP op }
  | ('>' | '<') as op	{ RELOP (String.make 1 op) }
  | '['			{ LBRACKET }
  | ']'			{ RBRACKET }
  | '('			{ LPAREN }
  | ')'			{ RPAREN }
  | ','			{ COMMA }
  | '|'			{ PIPE }
  | ':'			{ COLON }
  | '='			{ EQ }
  | '"'			{ let buf = Buffer.create 11 in
			  qstring buf lexbuf }
  | blank+		{ token_cudf lexbuf }
  | eof			{ EOL } (* single-line parsing: EOF means in fact EOL *)

and qstring buf = parse
  | "\\\""				{ Buffer.add_string buf "\"";
					  qstring buf lexbuf }
  | "\\\\"				{ Buffer.add_string buf "\\";
					  qstring buf lexbuf }
  | '"'					{ QSTRING (Buffer.contents buf) }
  | [^ '\n' '\r' '\\' '"']+ as s	{ Buffer.add_string buf s;
					  qstring buf lexbuf }
  | _					{ raise (Parse_error_822
						   ("unexpected end of quoted string",
						    (lexbuf.Lexing.lex_start_p,
						     lexbuf.Lexing.lex_curr_p))) }
