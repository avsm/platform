(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009-2011  Stefano Zacchiroli <zack@pps.jussieu.fr>        *)
(*                                                                           *)
(*  Adapted for Dose3                                                        *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

(*
Debian Policy : https://www.debian.org/doc/debian-policy/ch-controlfields.html

Each paragraph consists of a series of data fields. Each field consists of the
field name followed by a colon and then the data/value associated with that
field. The field name is composed of US-ASCII characters excluding control
characters, space, and colon (i.e., characters in the ranges 33-57 and 59-126,
inclusive). Field names must not begin with the comment character, #, nor with
the hyphen character, -.
*)

{
  open Format822_parser

  let get_range { Lexing.lex_start_p = start_pos;
                  Lexing.lex_curr_p = end_pos } =
    (start_pos, end_pos)

  let raise_error lexbuf c =
    let msg = Printf.sprintf "unexpected RFC 822 token : '%c'" c in
    raise (Format822.Parse_error_822 (Format822.error lexbuf msg))
}

let lower_letter = [ 'a' - 'z' ]
let upper_letter = [ 'A' - 'Z' ]
let letter = lower_letter | upper_letter
let digit = [ '0' - '9' ]
let blank = [ ' ' '\t' ]
let ident = ( [ '!' - '9'] | [';' - '~' ] )+
let identnosharphypen = ( '!' | '"' | [ '$' - ',' ] | [ '.' - '9'] | [';' - '~' ] )
(* conform to the Debian Policy *)
let fieldname = (identnosharphypen ident)

rule token_822 = parse
  | "-----BEGIN PGP SIGNED MESSAGE-----"               { PGPHEAD }
  | "-----BEGIN PGP SIGNATURE-----"                    { pgpsignature lexbuf }
  | '#' [^'\n']* ('\n'|eof)                            { token_822 lexbuf }
  | (fieldname as field) ':' blank* ([^'\n']* as rest) { FIELD(field, (get_range lexbuf, rest)) }
  | blank ([^'\n']* as rest)                           { CONT(get_range lexbuf, rest) }
  | blank* '\n'                                        { Lexing.new_line lexbuf; BLANKLINE }
  | eof                                                { EOF }
  | _ as c                                             { raise_error lexbuf c }
and pgpsignature = parse
    | "-----END PGP SIGNATURE-----"                    { token_822 lexbuf    }
    | _                                                { pgpsignature lexbuf }
