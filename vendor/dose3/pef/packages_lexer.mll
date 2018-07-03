(*****************************************************************************)
(*  Copyright (C) 2009-2015 Pietro Abate <pietro.abate@pps.jussieu.fr>       *)
(*  Copyright (C) 2009 Mancoosi Project                                      *)
(*                                                                           *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

{
  open Common
  open Packages_parser

}

let lower_letter = [ 'a' - 'z' ]
let upper_letter = [ 'A' - 'Z' ]
let letter = lower_letter | upper_letter
let digit = [ '0' - '9' ]
let blank = [ ' ' '\t' ]
let blanks = blank+
let symbols = ['-' ':' '+' '.' '_' '~']
let ident = (letter | digit) (letter | digit | symbols)*

rule token_deb = parse
  | (">=" | "<=") as op { RELOP op }
  | (">>" | "<<") as op { RELOP op }
  | "!=" as op          { RELOP op }
  | '<'                 { LT }
  | '>'                 { GT }
  | '='                 { EQ }
  | ':'                 { COLON }
  | '/'                 { SLASH }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '['                 { LBRACKET }
  | ']'                 { RBRACKET }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ','                 { COMMA }
  | '|'                 { PIPE }
  | '!'                 { BANG }
  | ident as s          { IDENT s }
  | blank+              { token_deb lexbuf }
  | eof                 { EOL } (* single-line parsing: EOF means in fact EOL *)
  | _ as c              { Format822.raise_error lexbuf c }
