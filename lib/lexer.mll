{

  let pp_position ppf lexbuf =
    let p = Lexing.lexeme_start_p lexbuf in
    Fmt.pf ppf
      "File \"%s\", line %d, character %d"
      p.Lexing.pos_fname p.Lexing.pos_lnum
      (p.Lexing.pos_cnum - p.Lexing.pos_bol)

  (* TODO: better error reporting *)
  let err lexbuf fmt =
    Fmt.kstrf (fun str ->
        Fmt.failwith "%a: %s" pp_position lexbuf str
      ) fmt

 let commands s = Astring.String.cuts ~sep:"\\\n  > " s
}

let eol = '\n' | eof
let ws = ' ' | '\t'
let cmd = [^'\n' '\\']+ ("\\\n  > " [^'\n' '\\']+)*
let digit = ['0' - '9']

rule file = parse
 | eof { [] }
 | "<-- non-deterministic" ws* eol        { `Non_det `Output  :: file lexbuf }
 | "<-- non-deterministic [skip]" ws* eol { `Non_det `Command :: file lexbuf }
 | "<--" ([^'\n']* as str) eol { err lexbuf "invalid pre-condition: %s" str }
 | "--> exit" ws+ (digit + as str) eol { `Exit (int_of_string str) :: file lexbuf }
 | "-->"  ([^'\n']* as str) eol { err lexbuf "invalid post-condition: %s" str }
 | "### " ([^'\n']* as str) eol { `Part str    :: file lexbuf }
 | "  " ws* "..." ws* eol       { `Ellipsis    :: file lexbuf }
 | "  $ " (cmd as str) eol      { `Command (commands str) :: file lexbuf }
 | "  " ([^'\n']* as str) eol   { `Output  str :: file lexbuf }
 | ([^'\n']* as str) eol        { `Comment str :: file lexbuf }
