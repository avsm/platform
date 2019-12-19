(* Copyright (C) 2005-2011 Jerome Vouillon
 * GPL 2 
 * code taken from http://coinst.irill.org/
 * *)

open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let print_list fmt pr sep l =
  match l with
    []     -> ()
  | x :: r -> pr fmt x; List.iter (fun x -> Format.fprintf fmt "%s%a" sep pr x) r

module Package = struct
  type t = int
  let print univ fmt i = CudfAdd.pp_package fmt (CudfAdd.inttopkg univ i)
  let compare (x : int) y = compare x y
end

module PSet = Set.Make (Package)
let print_set fmt pr sep l = print_list fmt pr sep (PSet.elements l)
let pset_of_lst l = List.fold_left (fun s x -> PSet.add x s) PSet.empty l
let pset_map f s = pset_of_lst (List.map f (PSet.elements s))

module PTbl = struct
  type 'a t = 'a array
  let create size v = Array.make size v
  let init size f = Array.init size f
  let get a i = a.(i)
  let set a i v = a.(i) <- v
  let iteri f a = Array.iteri (fun i v -> f i v) a
  let map = Array.map
  let mapi f a = Array.mapi (fun i v -> f i v) a
  let foldi f a acc = 
    snd (Array.fold_right (fun v (i,acc) -> (i+1,f i v acc)) a (0,acc))
  let fold = Array.fold_right
end

module Disj  = struct
  type t = PSet.t
  let print univ fmt l =
    if PSet.is_empty l then Format.fprintf fmt "MISSING"
    else print_set fmt (Package.print univ) " | " l

  let implies = PSet.subset
  let equiv = PSet.equal
  let lit = PSet.singleton
  let lit_disj l = List.fold_right PSet.add l PSet.empty
  let _false = PSet.empty
  let disj = PSet.union
  let disjl l = List.fold_left disj _false l
  let iter s f = PSet.iter f s
  let cut d p d' = assert (PSet.mem p d); PSet.union (PSet.remove p d) d'
  let fold = PSet.fold
  let for_all = PSet.for_all
  let exists = PSet.exists
  let implies1 = PSet.mem
  let to_lit l = if PSet.cardinal l = 1 then Some (PSet.choose l) else None
  let to_lits l = l
  let filter = PSet.filter

  let normalize d =  pset_map (fun i -> i) d
  let compare = PSet.compare
end

module CSet = Set.Make(Disj)

module Formula  = struct
  type t = Disj.t list
  let print univ fmt = print_list fmt (Disj.print univ) ", "

  let of_disj d = [d]
  let lit p = of_disj (Disj.lit p)

  let lit_disj l = of_disj (Disj.lit_disj l)

  let implies1 l1 y = List.exists (fun x -> Disj.implies x y) l1
  let implies l1 l2 =
    List.for_all (fun y -> implies1 l1 y) l2
  
  let equiv l1 l2 =
    List.for_all (fun y -> List.exists (fun x -> Disj.equiv x y) l1) l2 &&
    List.for_all (fun y -> List.exists (fun x -> Disj.equiv x y) l2) l1

  let _true = []
  let conj1 l x =
    if implies1 l x then l else
    x :: List.filter (fun y -> not (Disj.implies x y)) l
  let conj l1 l2 = List.fold_left conj1 l1 l2
  let conjl l = List.fold_left conj _true l

  let _false = of_disj (Disj._false)
  let disj l1 l2 =
    List.fold_left
      (fun l x -> List.fold_left (fun l y -> conj1 l (Disj.disj x y)) l l2)
      _true l1
  let disjl l = List.fold_left disj _false l
  let iter l f = List.iter f l
  let fold f l = List.fold_right f l
  let filter = List.filter
  let exists = List.exists
  let map = List.map

  let normalize f =
    let f = List.map Disj.normalize f in
    let f = List.sort ~cmp:PSet.compare f in
    f
end

module Conflict  = struct
  type t = PSet.t PTbl.t
  let create size = PTbl.create size PSet.empty
  let has c p1 = not (PSet.is_empty (PTbl.get c p1))
  let check c p1 p2 = PSet.mem p1 (PTbl.get c p2)
  let add c p1 p2 =
    PTbl.set c p1 (PSet.add p2 (PTbl.get c p1));
    PTbl.set c p2 (PSet.add p1 (PTbl.get c p2))
  let remove c p1 p2 =
    PTbl.set c p1 (PSet.remove p2 (PTbl.get c p1));
    PTbl.set c p2 (PSet.remove p1 (PTbl.get c p2))
  let iter c f =
    PTbl.iteri (fun i s -> PSet.iter (fun j -> if i < j then f i j) s) c
  let iter_on_packages c f = PTbl.iteri f c
  let of_package = PTbl.get

  let exists c f p = PSet.exists f (PTbl.get c p)
  let for_all c f p = PSet.for_all f (PTbl.get c p)
end

let simplify_formula confl f =
  Formula.filter
    (fun d ->
       Disj.for_all
         (fun p ->
            Conflict.exists confl (fun q -> not (Disj.implies1 q d)) p) d)
    f

let filter_conflicts confl p f =
  Formula.map
    (fun d -> Disj.filter (fun q -> not (Conflict.check confl p q)) d) f

let filter_conflicts confl p f =
  Formula.fold
    (fun d nf ->
       Formula.conj nf
         (Formula.of_disj
            (Disj.filter
               (fun q ->
                  not (PSet.exists (fun r -> Formula.implies1 f (Disj.lit r))
                         (Conflict.of_package confl q)))
               d)))
    f Formula._true

let rec flatten_deps tbl deps conflicts visited l =
  Formula.fold
    (fun d (l, r) ->
       let (l', r') =
         Disj.fold
           (fun i (l, r) ->
              let (l', r') = flatten_dep tbl deps conflicts visited i in
              (Formula.disj l' l, PSet.union r r')) d (Formula._false, r)
       in
       (Formula.conj l' l, r'))
    l (Formula._true, PSet.empty)

and flatten_dep tbl deps conflicts visited i =
  try
    (Hashtbl.find tbl i, PSet.empty)
  with Not_found ->
    let res =
      if List.mem i visited then
        (Formula._true, PSet.singleton i)
      else begin
        let (l, r) =
          flatten_deps tbl deps conflicts (i :: visited) (PTbl.get deps i)
        in
        let l = simplify_formula conflicts l in
        let r = PSet.remove i r in
        if Conflict.has conflicts i then
          (Formula.conj (Formula.lit i) l, r)
        else
          (l, r)
      end
    in
    (* Only cache the result if it is unconditionally true *)
    if PSet.is_empty (snd res) then Hashtbl.add tbl i (fst res);
    res

let flatten_dependencies size deps confl =
  let tbl = Hashtbl.create 17 in
  PTbl.init size (fun p -> fst (flatten_dep tbl deps confl [] p))

let remove_self_conflicts deps confl =
  let s = ref PSet.empty in
  PTbl.iteri
    (fun p f ->
       if Formula.exists (fun d -> 
         match Disj.to_lit d with Some q -> 
           Conflict.check confl p q | None -> false
          ) f 
        then
         s := PSet.add p !s)
    deps;
  PTbl.map
    (fun f ->
       Formula.fold
         (fun d f ->
            let d = Disj.filter (fun q -> not (PSet.mem q !s)) d in
            Formula.conj (Formula.of_disj d) f)
         f Formula._true)
    deps
;;

let remove_redundant_conflicts deps confl =
  let conj_deps p =
    let f = PTbl.get deps p in
    Formula.fold
      (fun d s -> match Disj.to_lit d with Some p -> PSet.add p s | None -> s)
      f PSet.empty
  in
  Conflict.iter confl
    (fun p1 p2 ->
       let d1 = conj_deps p1 in
       let d2 = conj_deps p2 in
       if
         PSet.exists
           (fun q1 ->
              PSet.exists
                (fun q2 ->
                   (p1 <> q1 || p2 <> q2) &&
                   (p1 <> q2 || p2 <> q1) &&
                   Conflict.check confl q1 q2)
                d2)
           d1
       then begin
         Conflict.remove confl p1 p2
       end);
  let try_remove_conflict p1 p2 =
    let f1 = PTbl.get deps p1 in
    let d2 = conj_deps p2 in
    if
      Formula.exists
        (fun d1 ->
           Disj.for_all
             (fun q1 ->
                PSet.exists
                  (fun q2 ->
                     (p1 <> q1 || p2 <> q2) &&
                     (p1 <> q2 || p2 <> q1) &&
                     Conflict.check confl q1 q2)
                  d2)
             d1)
        f1
    then begin
      Conflict.remove confl p1 p2
    end
  in
  Conflict.iter confl try_remove_conflict;
  Conflict.iter confl (fun p1 p2 -> try_remove_conflict p2 p1);
  (* We may now be able to remove some dependencies *)
  PTbl.map (simplify_formula confl) deps
;;

let maybe_remove deps confl p f d =
  Disj.exists (fun q ->
    Conflict.for_all confl (fun r ->
      Formula.exists (fun d' ->
        Disj.implies d' d && not (Disj.implies1 q d')
        ) (PTbl.get deps r)
      ) q
    ) d
;;

let is_composition deps p f d =
  Formula.exists (fun d' ->
    not (Disj.equiv d d') && not (Disj.equiv (Disj.lit p) d') && (
      Formula.exists (fun d'' -> Disj.implies d d'') (
        Disj.fold (fun p f -> 
          Formula.disj (PTbl.get deps p) f
        ) d' Formula._false
      )
    )
  ) f
;;

let rec remove_deps deps confl =
  let changed = ref false in
  let deps =
    PTbl.mapi (fun p f ->
       Formula.filter (fun d ->
         let b =
           not (maybe_remove deps confl p f d) || is_composition deps p f d
         in
         if not b then changed := true;
         b
       ) f
    ) deps
  in
  if !changed then
    remove_deps deps confl
  else deps
;;


(*******************************************************************)

let repository universe =
  let cmp : int -> int -> bool = (=) in
  let size = Cudf.universe_size universe in
  let confl = Conflict.create size in
  let deps = PTbl.create size Formula._true in
  let c = CudfAdd.init_conflicts universe in
  Cudf.iteri_packages (fun i p1 ->
    List.iter (fun p2 ->
      let j = CudfAdd.pkgtoint universe p2 in
      Conflict.add confl i j
    ) (CudfAdd.who_conflicts c universe p1);

    let dll = 
      List.map (fun disjunction ->
        let dl =
          List.fold_left (fun l2 vpkg ->
            let l = CudfAdd.who_provides universe vpkg in
            List.fold_left (fun acc i -> (CudfAdd.pkgtoint universe i)::acc) l2 l
          ) [] disjunction
        in 
        Formula.lit_disj (List.unique ~cmp dl)
      ) p1.Cudf.depends
    in
    PTbl.set deps i (Formula.conjl dll)
  ) universe;
  (deps,confl)
;;

let flatten_repository size (deps,confl) =
  let flatten_deps = flatten_dependencies size deps confl in
  let flatten_deps = remove_self_conflicts flatten_deps confl in
  let flatten_deps = remove_redundant_conflicts flatten_deps confl in
  let flatten_deps = flatten_dependencies size flatten_deps confl in
  let flatten_deps = remove_deps flatten_deps confl in
  (flatten_deps,confl)
;;
