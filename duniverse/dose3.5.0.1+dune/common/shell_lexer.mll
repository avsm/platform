(**************************************************************************************)
(*  Copyright (C) 2015 Johannes Schauer <j.schauer@email.de>                          *)
(*  Copyright (C) 2015 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

{
  exception UnknownShellEscape of string
  exception UnmatchedChar of char
  let buf_from_str str =
    let buf = Buffer.create 16 in
    Buffer.add_string buf str;
    buf
}

let safechars = [^ '"' ''' '\\' ' ' '\t']+
let space = [ ' ' '\t' ]+

rule shell_command argv = parse
 | space         { shell_command argv lexbuf }
 | safechars     { uquote argv (buf_from_str (Lexing.lexeme lexbuf)) lexbuf }
 | '\\' '"'      { uquote argv (buf_from_str "\"") lexbuf }
 | '\\' '''      { uquote argv (buf_from_str "'") lexbuf }
 | '\\' '\\'     { uquote argv (buf_from_str "\\") lexbuf }
 | '\\' ' '      { uquote argv (buf_from_str " ") lexbuf }
 | '\\' _ as c   { raise (UnknownShellEscape c) }
 | '"'           { dquote argv (Buffer.create 16) lexbuf }
 | '''           { squote argv (Buffer.create 16) lexbuf }
 | _ as c        { raise (UnmatchedChar c) }
 | eof { List.rev argv }
and uquote argv buf = parse
 | (space|eof) { shell_command ((Buffer.contents buf)::argv) lexbuf }
 | '\\' '"'    { Buffer.add_string buf "\""; uquote argv buf lexbuf }
 | '\\' '''    { Buffer.add_string buf "'"; uquote argv buf lexbuf }
 | '\\' '\\'   { Buffer.add_string buf "\\"; uquote argv buf lexbuf }
 | '\\' ' '    { Buffer.add_string buf " "; uquote argv buf lexbuf }
 | '\\' _ as c { raise (UnknownShellEscape c) }
 | '"'         { dquote argv buf lexbuf }
 | '''         { squote argv buf lexbuf }
 | safechars   { Buffer.add_string buf (Lexing.lexeme lexbuf); uquote argv buf lexbuf }
 | _ as c      { raise (UnmatchedChar c) }
and dquote argv buf = parse
 | '"' (space|eof) { shell_command ((Buffer.contents buf)::argv) lexbuf }
 | '"' '"'         { dquote argv buf lexbuf }
 | '"' '''         { squote argv buf lexbuf }
 | '"'             { uquote argv buf lexbuf }
 | '\\' '"'        { Buffer.add_string buf "\""; dquote argv buf lexbuf }
 | '\\' '\\'       { Buffer.add_string buf "\\"; dquote argv buf lexbuf }
 | '\\' _ as c     { raise (UnknownShellEscape c) }
 | [^ '"' '\\' ]+  { Buffer.add_string buf (Lexing.lexeme lexbuf); dquote argv buf lexbuf }
 | _ as c          { raise (UnmatchedChar c) }
and squote argv buf = parse
 | ''' (space|eof) { shell_command ((Buffer.contents buf)::argv) lexbuf }
 | ''' '''         { squote argv buf lexbuf }
 | ''' '"'         { dquote argv buf lexbuf }
 | '''             { uquote argv buf lexbuf }
 | [^ ''' ]+       { Buffer.add_string buf (Lexing.lexeme lexbuf); squote argv buf lexbuf }
 | _ as c          { raise (UnmatchedChar c) }

{
  (** given a (possibly quoted) command string, parse it into an argument vector *)
  let parse_string str =
    let lexbuf = Lexing.from_string str in
    shell_command [] lexbuf
}
