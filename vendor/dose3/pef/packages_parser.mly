
%{

open ExtLib
open Common

(* hash cons table *)
let h = Util.StringHashtbl.create 10000

let parse_vpkgname name =
  try
    match String.split name ":" with
    |n,"any" -> (n,Some "any")
    |n,"native" -> (n,Some "native")
    |n,"" -> raise Parsing.Parse_error
    |n,a -> (n,Some a)
  with ExtString.Invalid_string -> (name,None)

%}

%token <string> IDENT VIDENT STRING RELOP
%token LBRACKET RBRACKET LPAREN RPAREN LT GT
%token COMMA PIPE EQ BANG 
%token PLUS MINUS COLON SLASH
%token EOL

%type <Packages_types.name> pkgname_top
%type <Packages_types.version> version_top 

%type <Packages_types.architecture list> archlist_top
%type <Packages_types.source> source_top 

%type <Packages_types.vpkgname> vpkgname_top
%type <Packages_types.vpkg> vpkg_top
%type <Packages_types.vpkglist> vpkglist_top
%type <Packages_types.vpkgformula> vpkgformula_top

%type <Packages_types.builddepsformula> builddepsformula_top
%type <Packages_types.builddepslist> builddepslist_top

%type <Packages_types.vpkgreq> request_top
%type <Packages_types.vpkgreq list> requestlist_top

%start pkgname_top version_top
%start source_top
%start vpkgname_top vpkg_top vpkglist_top vpkgformula_top
%start builddepsformula_top builddepslist_top
%start request_top requestlist_top archlist_top

%%

pkgname_top: pkgname EOL { $1 } ;
version_top: version EOL { $1 } ;
source_top: source EOL { $1 } ;

vpkgname_top: vpkgname EOL { $1 } ;
vpkg_top: vpkg EOL { $1 } ;

vpkglist_top: vpkglist EOL { $1 } ;
vpkgformula_top: vpkgformula EOL { $1 } ;

builddepsformula_top: builddepsformula EOL { $1 } ;
builddepslist_top: builddepslist EOL { $1 } ;

requestlist_top: reqlist EOL { $1 } ;
request_top: request EOL { $1 } ;
archlist_top: archlist EOL { $1 } ;

/**************************************/ 

pkgname: IDENT { $1 } ;
version: IDENT { Util.hashcons h $1 } ;

source:
  |IDENT                        { ($1,None) }
  |IDENT LPAREN version RPAREN  { ($1,Some ($3)) }

relop:
  | RELOP       { $1 }
  | LT          { "<" }
  | GT          { ">" }
  | EQ          { "=" }
;

/**************************************/ 

vpkgname: IDENT { parse_vpkgname $1 } ;

constr:
  |                            { None }
  |LPAREN relop version RPAREN { Some ($2, $3) }
;

vpkg: vpkgname constr { ($1, $2) } ;

vpkglist:
  |             { [] }
  | vpkglist_ne { $1 }
;

vpkglist_ne:
  | vpkg                        { [ $1 ] }
  | vpkg COMMA vpkglist_ne      { $1 :: $3 }
;

vpkgformula:
  |                               { [ ] }
  | or_formula                    { [ $1 ] }
  | or_formula COMMA vpkgformula  { $1 :: $3 }
;

or_formula:
  |                             { [ ] }
  | vpkg                        { [ $1 ] }
  | vpkg PIPE or_formula        { $1 :: $3 }
;

/**************************************/ 

buidldep:
  |vpkg                                 { ($1,[],[]) }
  |vpkg LBRACKET buildarchlist RBRACKET { ($1,$3,[]) }
  |vpkg buildprofileformula             { ($1,[],$2) }
  |vpkg LBRACKET buildarchlist RBRACKET buildprofileformula { ($1,$3,$5) }
;

builddepslist:
  |                  { [] }
  | builddepslist_ne { $1 }
;

builddepslist_ne:
  |                                  { [ ] }
  | buidldep                         { [ $1 ] }
  | buidldep COMMA builddepslist_ne  { $1 :: $3 }
;

builddepsformula:
  |                                                { [ ] }
  | builddeps_or_formula                           { [ $1 ] }
  | builddeps_or_formula COMMA builddepsformula    { $1 :: $3 }
;

builddeps_or_formula:
  |                                      { [ ] }
  | buidldep                             { [ $1 ] }
  | buidldep PIPE builddeps_or_formula   { $1 :: $3 }
;

/**************************************/ 

buildarch:
  | BANG IDENT             { (false,$2) }
  | IDENT                  { (true,$1)  }
;

buildarchlist:
  |                  { [] }
  | buildarchlist_ne { $1 }
;

buildarchlist_ne:
  | buildarch                  { [ $1 ] }
  | buildarch buildarchlist_ne { $1 :: $2 }
;

/**************************************/ 

buildprofileformula:
  |                        { [] }
  | buildprofileformula_ne { $1 }
;

buildprofileformula_ne:
  | LT buildprofilelist GT { [ $2 ] }
  | LT buildprofilelist GT buildprofileformula_ne { $2 :: $4 }
;

buildprofile:
  | BANG IDENT             { (false,$2) }
  | IDENT                  { (true,$1)  }
;

buildprofilelist:
  |                     { [] }
  | buildprofilelist_ne { $1 }
;

buildprofilelist_ne:
  |                                   { [ ] }
  | buildprofile                      { [ $1 ] }
  | buildprofile buildprofilelist_ne  { $1 :: $2 }
;

/**************************************/ 

archlist:
  |             { [] }
  | archlist_ne { $1 }
;

archlist_ne:
  | IDENT                   { [ $1 ] }
  | IDENT archlist_ne       { $1 :: $2 }
;

/**************************************/ 

request:
  | PLUS req_aux        { let (vpkg,suite) = $2 in (Some Packages_types.I,vpkg,suite) }
  | MINUS req_aux       { let (vpkg,suite) = $2 in (Some Packages_types.R,vpkg,suite) }
  | req_aux             { let (vpkg,suite) = $1 in (None,vpkg,suite) }
;

req_aux:
  |vpkgname              { (($1,None),None) }
  |vpkgname EQ version   { (($1,Some("=",$3)),None) }
  |vpkgname SLASH IDENT  { (($1,None),Some $3) }
;

reqlist:
  |            { [] }
  | reqlist_ne { $1 }
;

reqlist_ne:
  | request                 { [ $1 ] }
  | request reqlist_ne      { $1 :: $2 }
;

%%

let pkgname_top = Format822.error_wrapper "pkgname" pkgname_top
let version_top = Format822.error_wrapper "version" version_top
let vpkg_top = Format822.error_wrapper "vpkg" vpkg_top
let vpkglist_top = Format822.error_wrapper "vpkglist" vpkglist_top
let vpkgformula_top = Format822.error_wrapper "vpkgformula" vpkgformula_top
let source_top = Format822.error_wrapper "source" source_top
let request_top = Format822.error_wrapper "request" request_top
let requestlist_top = Format822.error_wrapper "requestlist" requestlist_top
