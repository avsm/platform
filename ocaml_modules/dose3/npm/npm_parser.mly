%{

(*
range-set  ::= range ( logical-or range ) *
logical-or ::= ( ' ' ) * '||' ( ' ' ) *

range      ::= hyphen | simple ( ' ' simple ) * | ''

hyphen     ::= partial ' - ' partial

simple     ::= primitive | partial | tilde | caret
tilde      ::= '~' partial
caret      ::= '^' partial

primitive  ::= ( '<' | '>' | '>=' | '<=' | '=' | ) partial

partial    ::= xr ( '.' xr ( '.' xr qualifier ? )? )?
xr         ::= 'x' | 'X' | '*' | nr
nr         ::= '0' | ['1'-'9'] ( ['0'-'9'] ) *
qualifier  ::= ( '-' pre )? ( '+' build )?
pre        ::= parts
build      ::= parts
parts      ::= part ( '.' part ) *
part       ::= nr | [-0-9A-Za-z]+

*)

open Versioning

type atom = (string * SemverNode.version) option

let incr_str x = string_of_int ((int_of_string x) + 1)
let range v1 v2 = [
  (Some (">=", SemverNode.compose v1));
  (Some ("<", SemverNode.compose v2)) ]

let normalize_version version =
  try
    match SemverNode.parse_raw_version version with
    | (("x"|"X"|"*"), "", "", pre, build) ->
        let v1 = SemverNode.convert ("0","0","0",pre,build) in
        [(Some (">=", SemverNode.compose v1))]
    | (("x"|"X"|"*"), ("x"|"X"|"*"), ("x"|"X"|"*"), pre, build) ->
        let v1 = SemverNode.convert ("0","0","0",pre,build) in
        [(Some (">=", SemverNode.compose v1))]
    | (("x"|"X"|"*"), ("x"|"X"|"*"), "", pre, build) ->
        let v1 = SemverNode.convert ("0","0","0",pre,build) in
        [(Some (">=", SemverNode.compose v1))]
    |(x1, ("x"|"*"), "", pre, build) ->
        let v1 = SemverNode.convert (x1,"0","0",pre,build) in
        let v2 = SemverNode.convert (incr_str x1,"0","0",pre,build) in
        range v1 v2

    |(x1, ("x"|"X"|"*"|""), ("x"|"X"|"*"|""), pre, build) ->
        let v1 = SemverNode.convert (x1,"0","0",pre,build) in
        let v2 = SemverNode.convert (incr_str x1,"0","0",pre,build) in
        range v1 v2

    |(x1, x2, ("x"|"X"|"*"|""), pre, build) ->
        let v1 = SemverNode.convert (x1,x2,"0",pre,build) in
        let v2 = SemverNode.convert (x1,incr_str x2,"0",pre,build) in
        range v1 v2

    |(("x"|"X"|"*"|" "), _, _, pre, build) ->
        let v1 = SemverNode.convert ("0","0","0",pre,build) in
        [(Some (">=", SemverNode.compose v1))]

    | (x1, x2, x3, _, _) as v->
        [Some ("=", SemverNode.(compose (convert v)))]
  with Invalid_argument _ ->
    [Some ("=", version)]

let normalize_tilde version =
  match SemverNode.parse_raw_version version with
  | (x1,(""|"*"|"x"|"X"),(""|"*"|"x"|"X"),pre,build) ->
    let v1 = SemverNode.convert (x1,"0","0",pre,build) in
    let v2 = SemverNode.convert (incr_str x1,"0","0",[],[]) in
    range v1 v2
 
  | (x1,x2,(""|"*"|"x"|"X"),pre,build) ->
    let v1 = SemverNode.convert (x1,x2,"0",pre,build) in
    let v2 = SemverNode.convert (x1,incr_str x2,"0",[],[]) in
    range v1 v2

  | (x1,x2,x3,pre,build) as parsed ->
    let v1 = SemverNode.convert parsed in
    let v2 = SemverNode.convert (x1,incr_str x2,"0",[],[]) in
    range v1 v2

let normalize_caret version =
  let caret_normal version =
    let parsed = SemverNode.parse_raw_version version in
    let two_zeros = ref false in
    let parsed_list =
      match parsed with
      | (x1,("x"|"X"|"*"|""),_,_,_) -> two_zeros := true; [x1; "0"; "0"]
      | (x1,x2,("x"|"X"|"*"|""),_,_) -> [x1; x2; "0"]
      | (x1,x2,x3,_,_) -> [x1; x2; x3]
    in
    let (major,minor,patch) =
      let found = ref false in
      let check_update n = if !found then "0" else (found := true; incr_str n) in
      let generate_range x = if x <> "0" then check_update x else "0" in
      let generated = List.map generate_range parsed_list in
      if List.for_all (fun x -> x = "0") generated then
        if !two_zeros then ("1", "0", "0") 
        else ("0", "1", "0")
        else
          match generated with
          |[x1;x2;x3] -> (x1,x2,x3)
          |_ -> assert false
    in
    let v1 =
      match parsed_list with
      | [x1;x2;x3] -> SemverNode.convert (x1, x2, x3, [], [])
      | _          -> assert false
    in
    let v2 = SemverNode.convert (major,minor,patch,[],[]) in
    range v1 v2
  in
  let caret_star =
    let v1 = SemverNode.convert ("0","0","0",[],[]) in
    [(Some (">=", SemverNode.compose v1))]
  in
  if version = "*"
    then caret_star
    else caret_normal version


let normalize_hypen version1 version2 =
  let v1 = SemverNode.parse_version version1 in
  let v2 = SemverNode.parse_version version2 in
  range v1 v2

let normalize_primitive op version =
  try
    let v = SemverNode.parse_version version in
    [Some (op, SemverNode.compose v)]
  with Invalid_argument _ ->
    [List.nth (normalize_version version) 0]

let to_cnf ll =
  let return a = [a] in
  let bind m f = List.flatten (List.map f m) in
  let rec permutation = function
    |[] -> return []
    |h::t ->
        bind (permutation t) (fun t1 ->
          List.map (fun h1 -> h1 :: t1) h
        )
  in
  permutation ll

let normalize_depend name constr =
  List.map (fun conj ->
    List.map (fun c -> ((name,None),c)) conj
  ) (to_cnf constr)

%}

%left TILDE CARET
%right OR

%token EOL
%token OR HYPHEN STAR TILDE CARET 
%token COMMA COLON QUOTE
%token LCURLY RCURLY
%token 
%token <string> IDENT
%token <string> RELOP

%type <Pef.Packages_types.vpkg list list> depend_top
%type <Pef.Packages_types.vpkgformula> depends_top
%type <Pef.Packages_types.vpkgformula> dependlist_top

%start depends_top dependlist_top depend_top
%%

depends_top: depends EOL   { $1 } ;

depend_top: depend EOL     { $1 } ;
dependlist_top: dependlist EOL   { $1 } ;

depends: LCURLY dependlist RCURLY { $2 } ;

dependlist:
  depend                  { $1 }
  | depend COMMA dependlist { $1 @ $3 }
;

depend:
  name COLON QUOTE rangelist QUOTE { normalize_depend $1 $4 (* vpkgformula *) }
;

name: QUOTE IDENT QUOTE     { $2 }
;

rangelist:
  |                         { [] }
  | simplelist              { [$1] }
  | simplelist OR rangelist { $1 :: $3 (*vpkg list list *) }
;

simplelist:
  |                          { [] }
  | partial                  { $1 }
  | partial simplelist       { $1 @ $2 (* vpkg list *) }
;

partial: 
    IDENT range       { match $2 with
                        |None    -> normalize_version $1 (* vpkg list *)
                        |Some v2 -> normalize_hypen $1 v2
                      }
  | TILDE IDENT       { normalize_tilde $2 }
  | CARET IDENT       { normalize_caret $2 }
  | RELOP IDENT       { normalize_primitive $1 $2 }
  | STAR              { [None] }
;

range:
    HYPHEN IDENT      { Some $2 }
  |                   { None }

%%

open Common

let dependlist_top = Format822.error_wrapper "dependlist" dependlist_top
let depends_top = Format822.error_wrapper "depends" depends_top
let depend_top = Format822.error_wrapper "depend" depend_top


