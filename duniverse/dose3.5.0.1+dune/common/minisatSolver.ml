
module M (X : EdosSolver.S) = struct

  module X = X
  type var = Minisat.var
  type lit = Minisat.lit
  type state = Minisat.solver
  type value = Minisat.value
  
  let lit_of_var var = function
    |true -> Minisat.pos_lit var
    |false -> Minisat.neg_lit var

  let initialize_problem
    ?(print_var = (fun fmt -> Format.fprintf fmt "%d"))
    ?(buffer=false) n =
      new Minisat.solver

  let copy _ = failwith "Not Implemented in Minisat"
  let propagate state = state#simplify ()
  let protect _ = failwith "Not Implemented in Minisat"
  let rest _ = failwith "Not Implemented in Minisat"

  let assignment state = state#model
  let add_un_rule state lit _ = state#add_clause [lit]
  let add_bin_rule state l1 l2 _ = state#add_clause [l1;l2]
  let add_rule state la _ = state#add_clause (Array.to_list la)
  let associate_vars _ _ = failwith "Not Implemented in Minisat"
  let solve state var = state#solve_with_assumption [Minisat.pos_lit var]
  let solve_lst state vl = state#solve_with_assumption (List.map Minisat.pos_lit vl)
  let collect_reasons state v = failwith "Not Implemented in Minisat"
  let collect_reasons_lst state vl = failwith "Not Implemented in Minisat"
  let dump _ = failwith "Not Implemented in Minisat"
  let debug _ = failwith "Not Implemented in Minisat"
end
