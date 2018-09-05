/*****************************************************************************/
/*  libCUDF - CUDF (Common Upgrade Description Format/ manipulation library  */
/*  Copyright (C/ 2009-2011  Stefano Zacchiroli <zack@pps.jussieu.fr>        */
/*                                                                           */
/*  Adapted for Dose3 pietro.abate@pps.jussieu.fr                            */
/*                                                                           */
/*  This library is free software: you can redistribute it and/or modify     */
/*  it under the terms of the GNU Lesser General Public License as           */
/*  published by the Free Software Foundation, either version 3 of the       */
/*  License, or (at your option/ any later version.  A special linking       */
/*  exception to the GNU Lesser General Public License applies to this       */
/*  library, see the COPYING file for more information.                      */
/*****************************************************************************/

%{

open ExtLib

let extend_loc (r1_start, _) (_, r2_end) = (r1_start, r2_end)

let join (loc, v) l =
  let rec aux (startloc,acc) = function
    |[] -> (startloc,acc)
    |[(endloc,v)] -> (extend_loc startloc endloc, List.rev (v::acc))
    |(_,v)::tl -> aux (startloc,v::acc) tl
  in
  let r,vl = aux (loc, [v]) l in
  (r,String.concat "" vl)

%}

%token <string * (Format822.loc * string)> FIELD
%token <Format822.loc * string> CONT
%token BLANKLINE EOF
%token PGPHEAD
%type <Format822.doc> doc_822
%type <Format822.stanza option> stanza_822
%type <Format822.stanza option> doc_822_sign
%start doc_822 doc_822_sign stanza_822

%%

doc_822_sign:
  | PGPHEAD blanklines field BLANKLINE stanza_822 { $5 }
  | stanza_822 { $1 }
;

doc_822:
  | stanzas             { $1 }
  | blanklines stanzas  { $2 }
;

stanza_822:
  | stanza            { Some $1 }
  | blanklines stanza { Some $2 }
  | blanklines EOF    { None }
  | EOF               { None }
;

blanklines:
  | BLANKLINE            {}
  | BLANKLINE blanklines {}
;

stanzas:
  | stanza EOF                { [ $1 ] }
  | stanza blanklines stanzas { $1 :: $3 }
;

stanza:
  | fields      { $1 }
;

fields:
  | field               { [ $1 ] }
  | field fields        { $1 :: $2 }
;

field:
  | FIELD BLANKLINE           { $1 }
  | FIELD BLANKLINE linecont  { let k, v = $1 in (k, (join v $3)) }
;

linecont:
  | CONT BLANKLINE            { [ $1 ]}
  | CONT BLANKLINE linecont   { $1 :: $3 }
;

%%

let error_wrapper f lexer lexbuf =
  let syntax_error msg =
    raise (Format822.Syntax_error (Format822.error lexbuf msg))
  in
  try f lexer lexbuf with
  |Parsing.Parse_error -> syntax_error "RFC 822 parse error"
  |Failure _m -> syntax_error "RFC 822 lexer error"
  |_ -> syntax_error "RFC 822 error"

let doc_822 = error_wrapper doc_822
let stanza_822 = error_wrapper stanza_822
