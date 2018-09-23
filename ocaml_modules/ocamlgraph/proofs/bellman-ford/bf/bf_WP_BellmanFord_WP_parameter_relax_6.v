(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require int.Int.

(* Why3 assumption *)
Definition unit  := unit.

Parameter qtmark : Type.

Parameter at1: forall (a:Type), a -> qtmark -> a.
Implicit Arguments at1.

Parameter old: forall (a:Type), a -> a.
Implicit Arguments old.

(* Why3 assumption *)
Definition implb(x:bool) (y:bool): bool := match (x,
  y) with
  | (true, false) => false
  | (_, _) => true
  end.

Parameter map : forall (a:Type) (b:Type), Type.

Parameter get: forall (a:Type) (b:Type), (map a b) -> a -> b.
Implicit Arguments get.

Parameter set: forall (a:Type) (b:Type), (map a b) -> a -> b -> (map a b).
Implicit Arguments set.

Axiom Select_eq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) -> ((get (set m a1 b1)
  a2) = b1).

Axiom Select_neq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (~ (a1 = a2)) -> ((get (set m a1 b1)
  a2) = (get m a2)).

Parameter const: forall (b:Type) (a:Type), b -> (map a b).
Set Contextual Implicit.
Implicit Arguments const.
Unset Contextual Implicit.

Axiom Const : forall (b:Type) (a:Type), forall (b1:b) (a1:a),
  ((get (const b1:(map a b)) a1) = b1).

(* Why3 assumption *)
Inductive list (a:Type) :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Set Contextual Implicit.
Implicit Arguments Nil.
Unset Contextual Implicit.
Implicit Arguments Cons.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint length (a:Type)(l:(list a)) {struct l}: Z :=
  match l with
  | Nil => 0%Z
  | (Cons _ r) => (1%Z + (length r))%Z
  end.
Unset Implicit Arguments.

Axiom Length_nonnegative : forall (a:Type), forall (l:(list a)),
  (0%Z <= (length l))%Z.

Axiom Length_nil : forall (a:Type), forall (l:(list a)),
  ((length l) = 0%Z) <-> (l = (Nil :(list a))).

Parameter set1 : forall (a:Type), Type.

Parameter mem: forall (a:Type), a -> (set1 a) -> Prop.
Implicit Arguments mem.

(* Why3 assumption *)
Definition infix_eqeq (a:Type)(s1:(set1 a)) (s2:(set1 a)): Prop :=
  forall (x:a), (mem x s1) <-> (mem x s2).
Implicit Arguments infix_eqeq.

Axiom extensionality : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)),
  (infix_eqeq s1 s2) -> (s1 = s2).

(* Why3 assumption *)
Definition subset (a:Type)(s1:(set1 a)) (s2:(set1 a)): Prop := forall (x:a),
  (mem x s1) -> (mem x s2).
Implicit Arguments subset.

Axiom subset_trans : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a))
  (s3:(set1 a)), (subset s1 s2) -> ((subset s2 s3) -> (subset s1 s3)).

Parameter empty: forall (a:Type), (set1 a).
Set Contextual Implicit.
Implicit Arguments empty.
Unset Contextual Implicit.

(* Why3 assumption *)
Definition is_empty (a:Type)(s:(set1 a)): Prop := forall (x:a), ~ (mem x s).
Implicit Arguments is_empty.

Axiom empty_def1 : forall (a:Type), (is_empty (empty :(set1 a))).

Parameter add: forall (a:Type), a -> (set1 a) -> (set1 a).
Implicit Arguments add.

Axiom add_def1 : forall (a:Type), forall (x:a) (y:a), forall (s:(set1 a)),
  (mem x (add y s)) <-> ((x = y) \/ (mem x s)).

Parameter remove: forall (a:Type), a -> (set1 a) -> (set1 a).
Implicit Arguments remove.

Axiom remove_def1 : forall (a:Type), forall (x:a) (y:a) (s:(set1 a)), (mem x
  (remove y s)) <-> ((~ (x = y)) /\ (mem x s)).

Axiom subset_remove : forall (a:Type), forall (x:a) (s:(set1 a)),
  (subset (remove x s) s).

Parameter union: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments union.

Axiom union_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (union s1 s2)) <-> ((mem x s1) \/ (mem x s2)).

Parameter inter: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments inter.

Axiom inter_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (inter s1 s2)) <-> ((mem x s1) /\ (mem x s2)).

Parameter diff: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments diff.

Axiom diff_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (diff s1 s2)) <-> ((mem x s1) /\ ~ (mem x s2)).

Axiom subset_diff : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)),
  (subset (diff s1 s2) s1).

Parameter choose: forall (a:Type), (set1 a) -> a.
Implicit Arguments choose.

Axiom choose_def : forall (a:Type), forall (s:(set1 a)), (~ (is_empty s)) ->
  (mem (choose s) s).

Parameter all: forall (a:Type), (set1 a).
Set Contextual Implicit.
Implicit Arguments all.
Unset Contextual Implicit.

Axiom all_def : forall (a:Type), forall (x:a), (mem x (all :(set1 a))).

Parameter cardinal: forall (a:Type), (set1 a) -> Z.
Implicit Arguments cardinal.

Axiom cardinal_nonneg : forall (a:Type), forall (s:(set1 a)),
  (0%Z <= (cardinal s))%Z.

Axiom cardinal_empty : forall (a:Type), forall (s:(set1 a)),
  ((cardinal s) = 0%Z) <-> (is_empty s).

Axiom cardinal_add : forall (a:Type), forall (x:a), forall (s:(set1 a)),
  (~ (mem x s)) -> ((cardinal (add x s)) = (1%Z + (cardinal s))%Z).

Axiom cardinal_remove : forall (a:Type), forall (x:a), forall (s:(set1 a)),
  (mem x s) -> ((cardinal s) = (1%Z + (cardinal (remove x s)))%Z).

Axiom cardinal_subset : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)),
  (subset s1 s2) -> ((cardinal s1) <= (cardinal s2))%Z.

Parameter vertex : Type.

Parameter vertices: (set1 vertex).

Parameter edges: (set1 (vertex* vertex)%type).

(* Why3 assumption *)
Definition edge(x:vertex) (y:vertex): Prop := (mem (x, y) edges).

Axiom edges_def : forall (x:vertex) (y:vertex), (mem (x, y) edges) -> ((mem x
  vertices) /\ (mem y vertices)).

Parameter s: vertex.

Axiom s_in_graph : (mem s vertices).

Axiom vertices_cardinal_pos : (0%Z <  (cardinal vertices))%Z.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint infix_plpl (a:Type)(l1:(list a)) (l2:(list a)) {struct l1}: (list
  a) :=
  match l1 with
  | Nil => l2
  | (Cons x1 r1) => (Cons x1 (infix_plpl r1 l2))
  end.
Unset Implicit Arguments.

Axiom Append_assoc : forall (a:Type), forall (l1:(list a)) (l2:(list a))
  (l3:(list a)), ((infix_plpl l1 (infix_plpl l2
  l3)) = (infix_plpl (infix_plpl l1 l2) l3)).

Axiom Append_l_nil : forall (a:Type), forall (l:(list a)), ((infix_plpl l
  (Nil :(list a))) = l).

Axiom Append_length : forall (a:Type), forall (l1:(list a)) (l2:(list a)),
  ((length (infix_plpl l1 l2)) = ((length l1) + (length l2))%Z).

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint mem1 (a:Type)(x:a) (l:(list a)) {struct l}: Prop :=
  match l with
  | Nil => False
  | (Cons y r) => (x = y) \/ (mem1 x r)
  end.
Unset Implicit Arguments.

Axiom mem_append : forall (a:Type), forall (x:a) (l1:(list a)) (l2:(list a)),
  (mem1 x (infix_plpl l1 l2)) <-> ((mem1 x l1) \/ (mem1 x l2)).

Axiom mem_decomp : forall (a:Type), forall (x:a) (l:(list a)), (mem1 x l) ->
  exists l1:(list a), exists l2:(list a), (l = (infix_plpl l1 (Cons x l2))).

(* Why3 assumption *)
Inductive path : vertex -> (list vertex) -> vertex -> Prop :=
  | Path_empty : forall (x:vertex), (path x (Nil :(list vertex)) x)
  | Path_cons : forall (x:vertex) (y:vertex) (z:vertex) (l:(list vertex)),
      (edge x y) -> ((path y l z) -> (path x (Cons x l) z)).

Axiom path_right_extension : forall (x:vertex) (y:vertex) (z:vertex) (l:(list
  vertex)), (path x l y) -> ((edge y z) -> (path x (infix_plpl l (Cons y
  (Nil :(list vertex)))) z)).

Axiom path_trans : forall (x:vertex) (y:vertex) (z:vertex) (l1:(list vertex))
  (l2:(list vertex)), (path x l1 y) -> ((path y l2 z) -> (path x
  (infix_plpl l1 l2) z)).

Axiom empty_path : forall (x:vertex) (y:vertex), (path x (Nil :(list vertex))
  y) -> (x = y).

Parameter weight: vertex -> vertex -> Z.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint path_weight(l:(list vertex)) (dst:vertex) {struct l}: Z :=
  match l with
  | Nil => 0%Z
  | (Cons x Nil) => (weight x dst)
  | (Cons x ((Cons y _) as r)) => ((weight x y) + (path_weight r dst))%Z
  end.
Unset Implicit Arguments.

Axiom path_weight_right_extension : forall (x:vertex) (y:vertex) (l:(list
  vertex)), ((path_weight (infix_plpl l (Cons x (Nil :(list vertex))))
  y) = ((path_weight l x) + (weight x y))%Z).

Axiom path_in_vertices : forall (v1:vertex) (v2:vertex) (l:(list vertex)),
  (mem v1 vertices) -> ((path v1 l v2) -> (mem v2 vertices)).

Axiom simple_path : forall (v:vertex) (l:(list vertex)), (path s l v) ->
  exists lqt:(list vertex), (path s lqt v) /\
  ((length lqt) <  (cardinal vertices))%Z.

(* Why3 assumption *)
Inductive t  :=
  | Finite : Z -> t 
  | Infinite : t .

(* Why3 assumption *)
Definition add1(x:t) (y:t): t :=
  match x with
  | Infinite => Infinite
  | (Finite x1) =>
      match y with
      | Infinite => Infinite
      | (Finite y1) => (Finite (x1 + y1)%Z)
      end
  end.

(* Why3 assumption *)
Definition lt(x:t) (y:t): Prop :=
  match x with
  | Infinite => False
  | (Finite x1) =>
      match y with
      | Infinite => True
      | (Finite y1) => (x1 <  y1)%Z
      end
  end.

(* Why3 assumption *)
Definition le(x:t) (y:t): Prop := (lt x y) \/ (x = y).

Axiom Refl : forall (x:t), (le x x).

Axiom Trans : forall (x:t) (y:t) (z:t), (le x y) -> ((le y z) -> (le x z)).

Axiom Antisymm : forall (x:t) (y:t), (le x y) -> ((le y x) -> (x = y)).

Axiom Total : forall (x:t) (y:t), (le x y) \/ (le y x).

(* Why3 assumption *)
Inductive ref (a:Type) :=
  | mk_ref : a -> ref a.
Implicit Arguments mk_ref.

(* Why3 assumption *)
Definition contents (a:Type)(v:(ref a)): a :=
  match v with
  | (mk_ref x) => x
  end.
Implicit Arguments contents.

(* Why3 assumption *)
Definition t1 (a:Type) := (ref (set1 a)).

(* Why3 assumption *)
Definition distmap  := (map vertex t).

(* Why3 assumption *)
Definition inv1(m:(map vertex t)) (pass:Z) (via:(set1 (vertex*
  vertex)%type)): Prop := forall (v:vertex), (mem v vertices) -> match (get m
  v) with
  | (Finite n) => (exists l:(list vertex), (path s l v) /\ ((path_weight l
      v) = n)) /\ ((forall (l:(list vertex)), (path s l v) ->
      (((length l) <  pass)%Z -> (n <= (path_weight l v))%Z)) /\
      forall (u:vertex) (l:(list vertex)), (path s l u) ->
      (((length l) <  pass)%Z -> ((mem (u, v) via) -> (n <= ((path_weight l
      u) + (weight u v))%Z)%Z)))
  | Infinite => (forall (l:(list vertex)), (path s l v) ->
      (pass <= (length l))%Z) /\ forall (u:vertex), (mem (u, v) via) ->
      ((get m u) = Infinite)
  end.

(* Why3 assumption *)
Definition inv2(m:(map vertex t)) (via:(set1 (vertex* vertex)%type)): Prop :=
  forall (u:vertex) (v:vertex), (mem (u, v) via) -> (le (get m v)
  (add1 (get m u) (Finite (weight u v)))).

Require Import Classical.
Require Import Why3.

(* Why3 goal *)
Theorem WP_parameter_relax : forall (u:vertex), forall (v:vertex),
  forall (pass:Z), forall (via:(set1 (vertex* vertex)%type)), forall (m:(map
  vertex t)), ((1%Z <= pass)%Z /\ ((mem (u, v) edges) /\ ((~ (mem (u, v)
  via)) /\ (inv1 m pass via)))) -> (match (get m
  u) with
  | Infinite => False
  | (Finite x) => match (get m
      v) with
      | Infinite => True
      | (Finite y) => ((x + (weight u v))%Z <  y)%Z
      end
  end -> (match (get m
  u) with
  | (Finite nu) => exists lu:(list vertex), (path s lu u) /\
      (((path_weight lu u) = nu) /\ (path s (infix_plpl lu (Cons u
      (Nil :(list vertex)))) v))
  | Infinite => False
  end -> forall (m1:(map vertex t)), (m1 = (set m v match (get m
  u) with
  | Infinite => Infinite
  | (Finite x) => (Finite (x + (weight u v))%Z)
  end)) -> (inv1 m1 pass (add (u, v) via)))).
intuition.
destruct (get m u) as [] _eqn; intuition.
destruct H1 as (lu, (h1, (h2, h3))).
red.
intros.
destruct (classic (v0 = v)).
subst v0 m1.
rewrite Select_eq; auto.
exists ((infix_plpl lu (Cons u Nil))); intuition.
rewrite <- h2.
apply path_weight_right_extension; auto.
subst m1; rewrite Select_neq; auto.
red in H5.
generalize (H5 v0); intuition.
Qed.


