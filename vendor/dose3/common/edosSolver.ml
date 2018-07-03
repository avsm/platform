(***************************************************************************************)
(*  Copyright (C) 2005-2009 Jerome Vouillon                                            *)
(*  Minor modifications :                                                              *)
(*       Pietro Abate <pietro.abate@pps.jussieu.fr>                                    *)
(*       Jaap Boender <boender@pps.jussieu.fr>                                         *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)


module type S = sig
  type reason
end

module type T = sig
  module X : S
  type state
  type var = int
  type lit
  val lit_of_var : var -> bool -> lit
  val initialize_problem :
    ?print_var:(Format.formatter -> int -> unit) -> 
      ?buffer: bool -> int -> state
  val copy : state -> state
  val propagate : state -> unit
  val protect : state -> unit
  val reset : state -> unit
  type value = True | False | Unknown
  val assignment : state -> value array
  val assignment_true : state -> var list
  val add_rule : state -> lit array -> X.reason list -> unit
  val associate_vars : state -> lit -> var list -> unit
  val solve_all : (state -> unit) -> state -> var -> bool
  val solve : state -> var -> bool
  val solve_lst : state -> var list -> bool
  val collect_reasons : state -> var -> X.reason list
  val collect_reasons_lst : state -> var list -> X.reason list
  val dump : state -> (int * bool) list list
  val debug : bool -> unit
  val stats : state -> unit
end

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module IntHash = 
  Hashtbl.Make (struct
    type t = int
    let equal = (=)
    let hash i = i
  end)

open ExtLib

let (@) l1 l2 =
  let rec geq = function
    |[],[] -> true
    |_::_,[] -> true
    |[],_::_ -> false
    |_::r1,_::r2 -> geq (r1,r2)
  in 
  if geq (l1,l2) then List.append l2 l1 else List.append l1 l2

module M (X : S) = struct

  module X = X

  let debug = ref false
  let buffer = ref false

  (* Variables *)
  type var = int

  (* Literals *)
  type lit = int

  (* A clause is an array of literals *)
  type clause =
    { lits : lit array;
      all_lits : lit array;
      reasons : X.reason list }

  type value = True | False | Unknown

  module LitMap =
    Map.Make (struct type t = int let compare (x : int) y = compare x y end)

  type state =
    { (* Indexed by var *)
      st_assign : value array;
      st_assign_true : unit IntHash.t;
      st_reason : clause option array;
      st_level : int array;
      st_seen_var : int array;
      st_refs : int array;
      st_pinned : bool array;
      (* Indexed by lit *)
      st_simpl_prop : clause LitMap.t array;
      st_watched : clause list array;
      st_associated_vars : var list array;
      (* Queues *)
      mutable st_trail : lit list;
      mutable st_trail_lim : lit list list;
      st_prop_queue : lit Queue.t;
      (* Misc *)
      mutable st_cur_level : int;
      mutable st_min_level : int;
      mutable st_seen : int;
      mutable st_var_queue_head : var list;
      st_var_queue : var Queue.t;
      mutable st_cost : int; (* Total computational cost so far *)
      st_print_var : Format.formatter -> int -> unit;
      mutable st_coherent : bool;
      mutable st_buffer : (int * bool) list list;
    }

  let copy_clause p =
      let n = Array.length p in
      let a = Array.make n None in
      Array.iteri (fun i c ->
          let copy = function
              |None -> None
              |Some cl -> Some ({ cl with lits = Array.copy cl.lits })
          in
          Array.set a i (copy c)
      ) p
      ;
      a

  let copy_simpl_prop p =
      let n = Array.length p in
      let a = Array.make n LitMap.empty in
      Array.iteri (fun i l ->
          let copy cl = { cl with lits = Array.copy cl.lits } in
          let l' =  LitMap.map (fun clause -> copy clause) l in
          Array.set a i l'
      ) p
      ;
      a

  let copy_watched p =
      let n = Array.length p in
      let a = Array.make n [] in
      Array.iteri (fun i l ->
          let copy cl = { cl with lits = Array.copy cl.lits } in
          let l' =  List.map (fun clause -> copy clause) l in
          Array.set a i l'
      ) p
      ;
      a

  let copy st =
    { st_assign = Array.copy st.st_assign;
      st_assign_true = IntHash.copy st.st_assign_true;
      st_reason = copy_clause st.st_reason;
      st_level = Array.copy st.st_level;
      st_seen_var = Array.copy st.st_seen_var;
      st_refs = Array.copy st.st_refs;
      st_pinned = Array.copy st.st_pinned;
      st_simpl_prop = copy_simpl_prop st.st_simpl_prop;
      st_watched = copy_watched st.st_watched;
      st_associated_vars = Array.copy st.st_associated_vars;
      st_trail = st.st_trail;
      st_trail_lim = st.st_trail_lim;
      st_prop_queue = Queue.copy st.st_prop_queue;
      st_cur_level = st.st_cur_level;
      st_min_level = st.st_min_level;
      st_seen = st.st_seen;
      st_var_queue_head = st.st_var_queue_head;
      st_var_queue = Queue.copy st.st_var_queue;
      st_cost = st.st_cost;
      st_print_var = st.st_print_var;
      st_coherent = st.st_coherent;
      st_buffer = st.st_buffer;
    }

  (****)
 
  let charge st x = st.st_cost <- st.st_cost + x
  let get_bill st = st.st_cost

  (****)

  let pin_var st x = st.st_pinned.(x) <- true

  let unpin_var st x = st.st_pinned.(x) <- false

  let enqueue_var st x =
    charge st 1;
    pin_var st x;
    Queue.push x st.st_var_queue

  let requeue_var st x =
    pin_var st x;
    st.st_var_queue_head <- x :: st.st_var_queue_head

  (* Returns -1 if no variable remains *)
  let rec dequeue_var st =
    let x =
      match st.st_var_queue_head with
        x :: r -> st.st_var_queue_head <- r; x
      | []     -> try Queue.take st.st_var_queue with Queue.Empty -> -1
    in
    if x = -1 then x else begin
      unpin_var st x;
      if st.st_refs.(x) = 0 || st.st_assign.(x) <> Unknown then
        dequeue_var st
      else
        x
    end

  (****)

  let var_of_lit p = p lsr 1
  let pol_of_lit p = p land 1 = 0
  let lit_of_var v s = if s then v + v else v + v + 1
  let lit_neg p = p lxor 1

  let val_neg v =
    match v with
      True    -> False
    | False   -> True
    | Unknown -> Unknown

  let val_of_bool b = if b then True else False

  let val_of_lit st p =
    let v = st.st_assign.(var_of_lit p) in
    if pol_of_lit p then v else val_neg v

  (****)

  let print_val ch v =
    Format.fprintf ch "%s"
      (match v with True -> "True" | False -> "False" | Unknown -> "Unknown")

  let print_lits st ch lits =
    Format.fprintf ch "{";
    Array.iter
      (fun p ->
         if pol_of_lit p then
           Format.fprintf ch " +%a" st.st_print_var (var_of_lit p)
         else
           Format.fprintf ch " -%a" st.st_print_var (var_of_lit p))
      lits;
    Format.fprintf ch " }"

  let print_rule st ch r = print_lits st ch r.lits

  (****)

  let store st r =
    let clause = 
      Array.fold_left (fun acc p ->
         if pol_of_lit p then
            (var_of_lit p, true)::acc
         else
            (var_of_lit p, false)::acc
      ) [] r.lits 
    in
    st.st_buffer <- (clause::st.st_buffer)

  (* we reverse the list because we store literals in reverse order *)
  let dump st = List.rev_map (fun x -> List.rev x) st.st_buffer

  (****)

  exception Conflict of clause option

  let enqueue st p reason =
    charge st 1;
    if !debug then begin
      match reason with
        Some r -> Format.eprintf "Applying rule %a@." (print_rule st) r
      | _ -> ()
    end;
    match val_of_lit st p with
      False ->
        if !debug then begin
          if pol_of_lit p then
            Format.eprintf "Cannot install %a@." st.st_print_var (var_of_lit p)
          else
            Format.eprintf "Already installed %a@."
              st.st_print_var (var_of_lit p)
        end;
        raise (Conflict reason)
    | True ->
        ()
    | Unknown ->
        if !debug then begin
          if pol_of_lit p then
            Format.eprintf "Installing %a@." st.st_print_var (var_of_lit p)
          else
            Format.eprintf "Should not install %a@."
              st.st_print_var (var_of_lit p);
        end;
        let x = var_of_lit p in
        st.st_assign.(x) <- val_of_bool (pol_of_lit p);
        if st.st_assign.(x) = True then IntHash.add st.st_assign_true x ();
        st.st_reason.(x) <- reason;
        st.st_level.(x) <- st.st_cur_level;
        st.st_trail <- p :: st.st_trail;
        List.iter
          (fun x ->
             charge st 1;
             let refs = st.st_refs.(x) in
             if refs = 0 then enqueue_var st x;
             st.st_refs.(x) <- st.st_refs.(x) + 1)
          st.st_associated_vars.(p);
        Queue.push p st.st_prop_queue

  let rec find_not_false st lits i l =
    if i = l then -1 else
    if val_of_lit st lits.(i) <> False then i else
    find_not_false st lits (i + 1) l

  let propagate_in_clause st r p =
    charge st 1;
    let p' = lit_neg p in
    if r.lits.(0) = p' then begin
      r.lits.(0) <- r.lits.(1);
      r.lits.(1) <- p'
    end;
    if val_of_lit st r.lits.(0) = True then
      st.st_watched.(p) <- r :: st.st_watched.(p)
    else begin
      let i = find_not_false st r.lits 2 (Array.length r.lits) in
      if i = -1 then begin
        st.st_watched.(p) <- r :: st.st_watched.(p);
        enqueue st r.lits.(0) (Some r)
      end else begin
        r.lits.(1) <- r.lits.(i);
        r.lits.(i) <- p';
        let p = lit_neg r.lits.(1) in
        st.st_watched.(p) <- r :: st.st_watched.(p)
      end
    end

  let propagate st =
    try
      while not (Queue.is_empty st.st_prop_queue) do
        charge st 1;
        let p = Queue.take st.st_prop_queue in
        LitMap.iter (fun p r -> enqueue st p (Some r)) st.st_simpl_prop.(p);
        let l = ref (st.st_watched.(p)) in
        st.st_watched.(p) <- [];
        begin try
          while
            match !l with
              r :: rem ->
                l := rem;
                propagate_in_clause st r p;
                true
            | [] ->
                false
          do () done
        with Conflict _ as e ->
          st.st_watched.(p) <- !l @ st.st_watched.(p);
          raise e
        end
      done
    with Conflict _ as e ->
      Queue.clear st.st_prop_queue;
      raise e

  (****)

  let raise_level st =
    st.st_cur_level <- st.st_cur_level + 1;
    st.st_trail_lim <- st.st_trail :: st.st_trail_lim;
    st.st_trail <- []

  let assume st p =
    raise_level st;
    enqueue st p None

  let protect st =
    propagate st;
    raise_level st;
    st.st_min_level <- st.st_cur_level

  let undo_one st p =
    let x = var_of_lit p in
    if !debug then Format.eprintf "Cancelling %a@." st.st_print_var x;
    if st.st_assign.(x) = True then IntHash.remove st.st_assign_true x;
    st.st_assign.(x) <- Unknown;
    st.st_reason.(x) <- None;
    st.st_level.(x) <- -1;
    List.iter
      (fun x -> charge st 1; st.st_refs.(x) <- st.st_refs.(x) - 1)
      st.st_associated_vars.(p);
    if st.st_refs.(x) > 0 && not st.st_pinned.(x) then enqueue_var st x

  let cancel st =
    st.st_cur_level <- st.st_cur_level - 1;
    List.iter (fun p -> undo_one st p) st.st_trail;
    match st.st_trail_lim with
      []     -> assert false
    | l :: r -> st.st_trail <- l; st.st_trail_lim <- r

  let reset st =
    if !debug then Format.eprintf "Reset@.";
    while st.st_trail_lim <> [] do cancel st done;
    for i = 0 to Array.length st.st_refs - 1 do
      st.st_refs.(i) <- 0;
      st.st_pinned.(i) <- false
    done;
    st.st_var_queue_head <- [];
    st.st_min_level <- 0;
    Queue.clear st.st_var_queue;
    st.st_coherent <- true

  (****)

  let rec find_next_lit st =
    match st.st_trail with
      [] ->
        assert false
    | p :: rem ->
        st.st_trail <- rem;
        if st.st_seen_var.(var_of_lit p) = st.st_seen then
          let reason = st.st_reason.(var_of_lit p) in
          undo_one st p;
          (p, reason)
        else begin
          undo_one st p;
          find_next_lit st
        end

  let analyze st conflict =
    st.st_seen <- st.st_seen + 1;
    let counter = ref 0 in
    let learnt = ref [] in
    let bt_level  = ref 0 in
    let reasons = ref [] in
    let r = ref conflict in
    while
      if !debug then begin
        Array.iter
          (fun p ->
             Format.eprintf "%d:%a (%b/%d) "
               p print_val (val_of_lit st p)
               (st.st_reason.(var_of_lit p) <> None)
               st.st_level.(var_of_lit p))
          !r.lits;
        Format.eprintf "@."
      end;
      reasons := !r.reasons @ !reasons;
      for i = 0 to Array.length !r.all_lits - 1 do
        let p = !r.all_lits.(i) in
        let x = var_of_lit p in
        if  st.st_seen_var.(x) <> st.st_seen then begin
          assert (val_of_lit st p = False);
          st.st_seen_var.(x) <- st.st_seen;
          let level = st.st_level.(x) in
          if level = st.st_cur_level then begin
            incr counter
          end else (* if level > 0 then *) begin
            learnt := p :: !learnt;
            bt_level := max level !bt_level
          end
        end
      done;

      let (p, reason) = find_next_lit st in
      decr counter;
      if !counter  = 0 then
        learnt := lit_neg p :: !learnt
      else
        begin match reason with
          Some r' -> r := r'
        | None    -> assert false
        end;
      !counter > 0
    do () done;
    if !debug then begin
      List.iter
        (fun p ->
           Format.eprintf "%d:%a/%d "
             p print_val (val_of_lit st p) st.st_level.(var_of_lit p))
        !learnt;
      Format.eprintf "@."
    end;
    (Array.of_list !learnt, !reasons, !bt_level)

  let find_highest_level st lits =
    let level = ref (-1) in
    let i = ref 0 in
    Array.iteri
      (fun j p ->
         if st.st_level.(var_of_lit p) > !level then begin
           level := st.st_level.(var_of_lit p);
           i := j
         end)
      lits;
    !i

  let backjump f st r =
    let (learnt, reasons, level) = analyze st r in
    let level = max st.st_min_level level in
    while st.st_cur_level > level do cancel st done;
    assert (val_of_lit st learnt.(0) = Unknown);
    let rule = { lits = learnt; all_lits = learnt; reasons = reasons } in
    if !debug then Format.eprintf "Learning %a@." (print_rule st) rule;
    if Array.length learnt > 1 then begin
      let i = find_highest_level st learnt in
      assert (i > 0);
      let p' = learnt.(i) in
      learnt.(i) <- learnt.(1);
      learnt.(1) <- p';
      let p = lit_neg learnt.(0) in
      let p' = lit_neg p' in
      st.st_watched.(p) <- rule :: st.st_watched.(p);
      st.st_watched.(p') <- rule :: st.st_watched.(p')
    end;
    enqueue st learnt.(0) (Some rule);
    st.st_cur_level > st.st_min_level && f st

  let val_of = function
    |True -> true
    |False -> false 
    |Unknown -> assert false 

  (* find all solutions *)
  let rec solve_all_rec callback st =
    match try propagate st; None with Conflict r -> Some r with
      None ->
        let x = dequeue_var st in
        if x < 0 then begin
          (* we do something with the solution that we just found *)
          callback st ; 
          if st.st_cur_level = 0 then begin
            (* we exhausted the search space *)
            if !debug then Format.eprintf "Search Completed.@."; 
            true
          end else begin
            if !debug then Format.eprintf "Solution found.@."; 
            (* we remove this solution from the search space and backjump *)
            let assignment =
              (* XXX : I should keep trace of this list incrementally *)
              let acc = ref [] in
              for v = 0 to (Array.length st.st_assign) - 1 do
                match st.st_assign.(v) with
                |True -> acc := (lit_of_var v true)::!acc
                |False -> acc := (lit_of_var v false)::!acc
                |Unknown -> ()
              done;
              !acc
            in
            let m = Array.of_list (List.map lit_neg assignment) in
            let r = { lits = m; all_lits = m; reasons = [] } in
            backjump (solve_all_rec callback) st r;
          end
        end else
          begin
            (* we didn't find any solution yet *)
            assume st (lit_of_var x false);
            solve_all_rec callback st
          end
    | Some r ->
        let r =
          match r with
            None   -> assert false
          | Some r -> r
        in
        (* we found a conflict *)
        backjump (solve_all_rec callback) st r

  (* find one solution *)
  let rec solve_rec st =
    match try propagate st; None with Conflict r -> Some r with
      None ->
        let x = dequeue_var st in
        x < 0 ||
        begin
          assume st (lit_of_var x false);
          solve_rec st
        end
    | Some r ->
        let r =
          match r with
            None   -> assert false
          | Some r -> r
        in
        backjump solve_rec st r

  let rec solve_aux ?callback st x =
    let s = 
      if Option.is_none callback then
        solve_rec
      else
        solve_all_rec (Option.get callback)
    in
    assert (st.st_cur_level = st.st_min_level);
    propagate st;
    try
      let p = lit_of_var x true in
      assume st p;
      assert (st.st_cur_level = st.st_min_level + 1);
      if s st then begin
        protect st;
        true
      end else
        solve_aux st ?callback x
    with Conflict _ ->
      st.st_coherent <- false;
      false

  let solve st x = solve_aux st x
  let solve_all callback st x = solve_aux ~callback st x

  let rec solve_lst_rec st l0 l =
    match l with
      [] ->
        true
    | x :: r ->
        protect st;
        List.iter (fun x -> enqueue st (lit_of_var x true) None) l0;
        propagate st;
        if solve st x then begin
          if r <> [] then reset st;
          solve_lst_rec st (x :: l0) r
        end else
          false

  let solve_lst st l = solve_lst_rec st [] l

  let debug b = debug := b
  let set_buffer b = buffer := b

  let initialize_problem 
    ?(print_var = (fun fmt -> Format.fprintf fmt "%d")) ?(buffer=false) n =
      if buffer then set_buffer true;
      (* Remove Gc settings for the moment as they are not adapted to small
         opam repositories
      Gc.set { (Gc.get()) with
        Gc.minor_heap_size = 4 * 1024 * 1024; (*4M*)
        Gc.major_heap_increment = 32 * 1024 * 1024; (*32M*)
        Gc.max_overhead = 150;
      } ;
      *)
      { st_assign = Array.make n Unknown;
        st_assign_true = IntHash.create n;
        st_reason = Array.make n None;
        st_level = Array.make n (-1);
        st_seen_var = Array.make n (-1);
        st_refs = Array.make n 0;
        st_pinned = Array.make n false;
        (* to each literal, positive or negative, 
         * we associate the list of rules where it appears *)
        st_simpl_prop = Array.make (2 * n) LitMap.empty;
        st_watched = Array.make (2 * n) [];
        (* to each literal we associate the list of assiciated variables *)
        st_associated_vars = Array.make (2 * n) [];
        st_trail = [];
        st_trail_lim = [];
        st_prop_queue = Queue.create ();
        st_cur_level = 0;
        st_min_level = 0;
        st_seen = 0;
        st_var_queue_head = [];
        st_var_queue = Queue.create ();
        st_cost = 0;
        st_print_var = print_var;
        st_coherent = true;
        st_buffer = [];
      }

  let insert_simpl_prop st r p p' =
    let p = lit_neg p in
    if not (LitMap.mem p' st.st_simpl_prop.(p)) then
      st.st_simpl_prop.(p) <- LitMap.add p' r st.st_simpl_prop.(p)

  let add_bin_rule st lits p p' reasons =
    let r = { lits = [|p; p'|]; all_lits = lits; reasons = reasons } in
    if !buffer then store st r ;
    insert_simpl_prop st r p p';
    insert_simpl_prop st r p' p

  let add_un_rule st lits p reasons =
    let r = { lits = [|p|]; all_lits = lits; reasons = reasons } in
    if !buffer then store st r ;
    enqueue st p (Some r)

  let add_rule st lits reasons =
    let is_true = ref false in
    let all_lits = Array.copy lits in
    let j = ref 0 in
    for i = 0 to Array.length lits - 1 do
      match val_of_lit st lits.(i) with
        True    -> is_true := true
      | False   -> ()
      | Unknown -> lits.(!j) <- lits.(i); incr j
    done;
    let lits = Array.sub lits 0 !j in
    if not !is_true then
    match Array.length lits with
      0 -> assert false
    | 1 -> add_un_rule st all_lits lits.(0) reasons
    | 2 -> add_bin_rule st all_lits lits.(0) lits.(1) reasons
    | _ -> let rule =
             { lits = lits; all_lits = all_lits; reasons = reasons } in
           let p = lit_neg rule.lits.(0) in let p' = lit_neg rule.lits.(1) in
           if !buffer then store st rule ;
           assert (val_of_lit st p <> False);
           assert (val_of_lit st p' <> False);
           st.st_watched.(p) <- rule :: st.st_watched.(p);
           st.st_watched.(p') <- rule :: st.st_watched.(p')

  let associate_vars st lit l =
    st.st_associated_vars.(lit) <- l @ st.st_associated_vars.(lit)

  let rec collect_rec st x l =
    if st.st_seen_var.(x) = st.st_seen then l else begin
      st.st_seen_var.(x) <- st.st_seen;
      match st.st_reason.(x) with
        None ->
          l
      | Some r ->
          r.reasons @
          Array.fold_left
            (fun l p -> collect_rec st (var_of_lit p) l) l r.all_lits
    end

  let collect_reasons st x =
    st.st_seen <- st.st_seen + 1;
    collect_rec st x []

  let collect_reasons_lst st l =
    st.st_seen <- st.st_seen + 1;
    let x = List.find (fun x -> st.st_assign.(x) = False) l in
    collect_rec st x []

  let assignment st = st.st_assign
  let assignment_true st =
    IntHash.fold (fun k _ acc -> k::acc ) st.st_assign_true []

  let stats st =
    let (t,f,u) =
      Array.fold_left(fun (t,f,u) -> function 
        |True -> (t+1,f,u) | False -> (t,f+1,u) | Unknown -> (t,f,u+1)
      ) (0,0,0) st.st_assign
    in
    Format.eprintf "Variables %d@." (Array.length st.st_assign);
    Format.eprintf "st_assign: True: %d False: %d Unknown: %d@." t f u;
    Format.eprintf "st_associated_vars %d@." (Array.length st.st_associated_vars);
    Format.eprintf "st_cost %d@." st.st_cost

end
