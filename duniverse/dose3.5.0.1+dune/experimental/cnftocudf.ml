(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common
open DoseparseNoRpm

let help = "
Input specification : 

Conjunctive dependencies :

!a b
!a c

package: a
version: 1
depends: b , c

package: b
version: 1

-----
Disjunctive dependencies :

!a b c

package: a
version: 1
depends: b | c

-----
Conflicts:

!a !b

package: a
version: 1
conflicts: b

package: b
version: 1

Negations can be either specified with the 
the character '!' or '-' before the package name

versions can be associated to a package as follows :
!a-2 b

package: a
version: 2
depends b

package: b
version: 1
"

module Options = struct
  open OptParse
  let options = OptParser.make ~description:("create a cudf document from a cnf formula" ^ help)
  include StdOptions.MakeOptions(struct let options = options end)
  open OptParser
end

include Util.Logging(struct let label = __FILE__ end) ;;

type var = (string * int)
type lit = Neg of var | Pos of var
type cnf =
  | Deps of lit list
  | Confl of lit * lit

let split ?(neg=false) s =
  try
    let s = 
      if neg then
        String.sub s 1 ((String.length s) - 1)
      else s
    in
    let idx = 
      try (String.rindex s '-') 
      with Not_found -> 0
    in
    if idx = 0 then (s,1)
    else
      let len = (String.length s) - (idx + 1) in
      let n = String.sub s 0 idx in
      let v = String.sub s (idx + 1) len in
      try
        (n,int_of_string v)
      with Failure "int_of_string" ->
        (s,1)
  with Not_found ->
    failwith (s ^ " is not a valid literal")
;;

let parse_line s =
  let lits = 
    List.map (function 
      |s when s.[0] = '!' || s.[0] = '-' -> Neg (split ~neg:true s)
      |s -> Pos (split s)
    ) (String.nsplit s " ")
  in
  match lits with
  |[Neg c1 ; Neg c2] -> Confl (Neg c1,Neg c2)
  |Neg p::tl -> Deps lits
  |_ -> fatal "input error on line %s" s
;;

let parse_cnf ic =
  let l = ref [] in
  try while true do 
    let line = ExtString.String.strip (input_line ic) in
    if line <> "" then
      l := (parse_line line)::!l
  done; assert false
  with End_of_file -> !l
;;

let add h n p =
  try let l = Hashtbl.find h n in l := p :: !l
  with Not_found -> Hashtbl.add h n (ref [p])
;;

let convert cnf =
  let module CH = CudfAdd.Cudf_hashtbl in
  let littable = Hashtbl.create (2 * (List.length cnf)) in
  List.iter (fun clause -> 
    match clause with
    |Confl (Neg a , Neg b) ->
        add littable a clause;
        add littable b clause
    |Deps ((Neg a):: l) ->
        add littable a clause;
        List.iter (function
          |Pos a | Neg a ->
              if not(Hashtbl.mem littable a) then 
                add littable a (Deps [])
        ) l
    |Deps [] -> ()
    |_ -> assert false
  ) cnf;
  let cudftable = CH.create (2 * (List.length cnf)) in
  let newp (n,v) = {Cudf.default_package with Cudf.package = n ; version = v } in
  Hashtbl.iter (fun (n,v) { contents = l } ->
    List.iter (function
      |Deps [] ->
          let p = newp (n,v) in
          if CH.mem cudftable p then ()
          else CH.add cudftable p p
      |Deps (Neg (n,v) :: l) ->
          let p = newp (n,v) in
          let p = 
            if CH.mem cudftable p then 
              CH.find cudftable p
            else p
          in
          let deps =
            List.map (function
              |Pos (n,v) -> (n,Some (`Eq, v)) 
              | Neg _ -> assert false
            ) l
          in
          let p = { p with Cudf.depends = deps :: p.Cudf.depends } in
          CH.replace cudftable p p
      |Confl (Neg (n1,v1),Neg(n2,v2)) ->
          let p1 = newp (n1,v1) in
          let p2 = newp (n2,v2) in
          let p1 = 
            if CH.mem cudftable p1 then 
              CH.find cudftable p1
            else p1
          in
          let p2 = 
            if CH.mem cudftable p2 then 
              CH.find cudftable p1
            else p2
          in
          let add_conflict p (n,v) =
           (* if p.Cudf.package != n && p.Cudf.version != v then *)
              let contr = (n,Some (`Eq, v)) in
              {p with Cudf.conflicts = contr :: p.Cudf.conflicts }
            (* else p *)
          in
          let p1 = add_conflict p1 (n2,v2) in
          let p2 = add_conflict p2 (n1,v1) in
          CH.replace cudftable p1 p1;
          CH.replace cudftable p2 p2
      |_ -> assert false
    ) l
  ) littable
  ;
  CH.fold (fun _ p acc -> p::acc) cudftable []
;;

let main () =

  let args = OptParse.OptParser.parse_argv Options.options in
  
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose);
  StdDebug.enable_bars (OptParse.Opt.get Options.progress) [] ;
  StdDebug.enable_timers (OptParse.Opt.get Options.timers) [];

  let ic =
    if List.length args > 0 then 
      open_in (List.hd args) 
    else fatal "you must specify an input file"
  in
  
  let pkglist = convert (parse_cnf ic) in
  
  if not(ic = stdin) then close_in ic;

  Cudf_printer.pp_packages stdout pkglist;

;;

main ();;

