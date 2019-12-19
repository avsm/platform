%{

(* 
ASPCUD accepted criteria

      Default: none
      Valid:   none, paranoid, -|+<crit>(,-|+<crit>)*
        <crit>: count(<set>) | sum(<set>,<attr>) | unsat_recommends(<set>)
              | aligned(<set>,<attr>,<attr>) | notuptodate(<set>)
        <attr>: CUDF attribute name
        <set> : solution | changed | new | removed | up | down
      For backwards compatibility: 
        new              = count(new)
        removed          = count(removed)
        changed          = count(changed)
        notuptodate      = notuptodate(solution)
        unsat_recommends = unsat_recommends(solution)
        sum(name)        = sum(name,solution)
*)
open Criteria_types

%}

%token <string> IDENT
%token LPAREN RPAREN 
%token COMMA 
%token <string> REGEXP
%token <string> EXACT
%token PLUS MINUS
%token EOL
%token COUNT SUM UNSATREC ALIGNED NOTUPTODATE 
%token SOLUTION CHANGED NEW REMOVED UP DOWN 

%type <Criteria_types.criteria> criteria_top

%start criteria_top

%%

criteria_top: criteria EOL { $1 }

criteria: 
    predicate { [$1] } 
  | predicate COMMA criteria { $1 :: $3 }

predicate:
    PLUS crit { Maximize($2) } 
  | MINUS crit { Minimize($2) }

crit: 
    COUNT LPAREN set RPAREN { Count($3,None) }
  | COUNT LPAREN set COMMA field RPAREN { Count($3,Some $5) }
  | SUM LPAREN set COMMA attr RPAREN { Sum($3,$5) }
  | UNSATREC LPAREN set RPAREN { Unsatrec($3) }
  | UNSATREC { Unsatrec(Solution) }
  | ALIGNED LPAREN set COMMA attr COMMA attr RPAREN { Aligned($3,$5,$7) }
  | NOTUPTODATE LPAREN set RPAREN { NotUptodate($3) }
  | NOTUPTODATE { NotUptodate(Solution) }
  | NEW { Count(New,None) }
  | REMOVED { Count(Removed,None) }
  | CHANGED { Count(Changed,None) }

attr: IDENT { $1 }

set:
    SOLUTION { Solution }
  | CHANGED { Changed }
  | NEW { New }
  | REMOVED { Removed }
  | UP { Up }
  | DOWN { Down }

field:
    IDENT EXACT { ($1,ExactMatch($2)) }
  | IDENT REGEXP { ($1,Regexp($2)) }

%%

let criteria_top = Format822.error_wrapper "criteria" criteria_top
