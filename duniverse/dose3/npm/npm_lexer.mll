{
open Common
open Npm_parser


}

let blank = [ ' ' '\t' ]
let hypen = blank+ '-' blank+
let ident = ['0'-'9' 'A'-'Z' 'a'-'z' '.' '+' '-' 'v' '=' '*']+

rule token = parse
  | "||"                { OR }
  | hypen               { HYPHEN }
  | ('>' | '<')   as op { RELOP (Char.escaped op) }
  | (">=" | "<=") as op { RELOP op }
  | ("==")              { RELOP "=" }
  | ("!=" | "=" ) as op { RELOP op }
  | "(?!\\.)*(?!\\.)"   { STAR }
  | "~"                 { TILDE }
  | "^"                 { CARET }
  | "\""                { QUOTE }

  | ':'                 { COLON }
  | ','                 { COMMA }
  | "{"                 { LCURLY }
  | "}"                 { RCURLY }

  | ident as s          { IDENT s }
  | eof                 { EOL }
  | blank+              { token lexbuf }
  | _ as c              { Format822.raise_error lexbuf c }
