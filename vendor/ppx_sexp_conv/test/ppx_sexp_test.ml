open Ppx_sexp_conv_lib
open Conv

module Sum_and_polymorphic_variants = struct
  type poly =
    [ `No_arg
    | `One_arg of int
    | `One_tuple of (int * string)
    | `Two_args of int * string
    ]
  [@@deriving sexp]

  let%test_unit _ =
    List.iter (fun (value, sexp) ->
      assert (sexp_of_poly value = sexp);
      assert (poly_of_sexp sexp = value);
    ) [
      `No_arg,             Sexp.Atom "No_arg";
      `One_arg 1,          Sexp.(List [Atom "One_arg"; Atom "1"]);
      `One_tuple (1, "a"), Sexp.(List [Atom "One_tuple"; List [Atom "1"; Atom "a"]]);
      `Two_args (1, "a"),  Sexp.(List [Atom "Two_args";  List [Atom "1"; Atom "a"]]);
    ]

  type nominal =
    | No_arg
    | One_arg of int
    | One_tuple of (int * string)
    | Two_args of int * string
  [@@deriving sexp]

  let%test_unit _ =
    List.iter (fun (value, sexp) ->
      assert (sexp_of_nominal value = sexp);
      assert (nominal_of_sexp sexp = value);
    ) [
      No_arg,             Sexp.Atom "No_arg";
      One_arg 1,          Sexp.(List [Atom "One_arg"; Atom "1"]);
      One_tuple (1, "a"), Sexp.(List [Atom "One_tuple"; List [Atom "1"; Atom "a"]]);
      Two_args (1, "a"),  Sexp.(List [Atom "Two_args"; Atom "1"; Atom "a"]);
    ]
end

module Records = struct
  type t =
    { a : int
    ; b : (float * string) list option
    }
  [@@deriving sexp]

  let%test_unit _ =
    let t = { a = 2; b = Some [(1., "a"); (2.3, "b")] } in
    let sexp = Sexplib.Sexp.of_string "((a 2)(b (((1 a)(2.3 b)))))" in
    assert (t_of_sexp sexp = t);
    assert (sexp_of_t t = sexp);
  ;;
end

module Inline_records = struct
  type t =
    | A of { a : int
           ; b : (float * string) list option
           }
    | B of int
  [@@deriving sexp]

  let%test_unit _ =
    let t = A { a = 2; b = Some [(1., "a"); (2.3, "b")] } in
    let sexp = Sexplib.Sexp.of_string "(A (a 2)(b (((1 a)(2.3 b)))))" in
    assert (t_of_sexp sexp = t);
    assert (sexp_of_t t = sexp);
  ;;
end

module User_specified_conversion = struct
  type my_float = float
  let sexp_of_my_float n = Sexp.Atom (Printf.sprintf "%.4f" n)
  let my_float_of_sexp = float_of_sexp

  let%test_unit _ =
    let my_float : my_float = 1.2 in
    let sexp = Sexp.Atom "1.2000" in
    assert (my_float_of_sexp sexp = my_float);
    assert (sexp_of_my_float my_float = sexp);
  ;;
end

module Exceptions : sig
  exception E0 [@@deriving sexp]
  exception E1 of string [@@deriving sexp]
  exception E2 of string * int [@@deriving sexp]
  exception E_tuple of (string * int) [@@deriving sexp]
  exception E_record of {a:string; b:int} [@@deriving sexp]
end = struct
  exception E0 [@@deriving sexp]
  exception E1 of string [@@deriving sexp]
  exception E2 of string * int [@@deriving sexp]
  exception E_tuple of (string * int) [@@deriving sexp]
  exception E_record of {a:string; b:int} [@@deriving sexp]
  let%test_unit _ =
    let cases =
      [ E0, "ppx_sexp_test.ml.Exceptions.E0"
      ; E1 "a", "(ppx_sexp_test.ml.Exceptions.E1 a)"
      ; E2 ("b", 2), "(ppx_sexp_test.ml.Exceptions.E2 b 2)"
      ; E_tuple ("c", 3), "(ppx_sexp_test.ml.Exceptions.E_tuple(c 3))"
      ; E_record {a="c"; b= 3}, "(ppx_sexp_test.ml.Exceptions.E_record(a c)(b 3))"
      ]
    in
    List.iter (fun (exn, sexp_as_str) ->
      let sexp = Sexplib.Sexp.of_string sexp_as_str in
      assert ([%sexp_of: exn] exn = sexp);
    ) cases
  ;;
end

module Abtract_types_are_allowed_in_structures : sig
  type t [@@deriving sexp]
end = struct
  type t [@@deriving sexp]
end

module Manifest_types = struct
  type a = { t : int }
  type b = a = { t : int }
  [@@deriving sexp]
end

module Uses_of_exn = struct
  type t = int * exn
  [@@deriving sexp_of]
end

module Function_types : sig
  type t1 = int -> unit [@@deriving sexp]
  type t2 = label:int -> ?optional:int -> unit -> unit [@@deriving sexp]
end = struct
  type t1 = int -> unit
  [@@deriving sexp]

  type t2 = label:int -> ?optional:int -> unit -> unit
  [@@deriving sexp]
end

module No_unused_rec = struct
  type r = { r : int } [@@deriving sexp]
end

module Field_name_should_not_be_rewritten = struct
  open No_unused_rec
  type nonrec r = { r : r }
  let _ = fun (r : r) -> r.r
end

module Polymorphic_variant_inclusion = struct
  type sub1 = [ `C1 | `C2 ]
  [@@deriving sexp]
  type 'b sub2 = [ `C4 | `C5 of 'b ]
  [@@deriving sexp]

  type ('a, 'b) t =
    [ sub1
    | `C3 of [ `Nested of 'a ]
    | 'b sub2
    | `C6 ] option
  [@@deriving sexp]

  let%test_unit _ =
    let cases : ((string * string, float) t * _) list =
      [ None, "()"
      ; Some `C1, "(C1)"
      ; Some `C2, "(C2)"
      ; Some (`C3 (`Nested ("a", "b"))), "((C3 (Nested (a b))))"
      ; Some `C4, "(C4)"
      ; Some (`C5 1.5), "((C5 1.5))"
      ; Some `C6, "(C6)"
      ]
    in
    List.iter (fun (t, sexp_as_str) ->
      let sexp = Sexplib.Sexp.of_string sexp_as_str in
      assert ([%of_sexp: (string * string, float) t] sexp = t);
      assert ([%sexp_of: (string * string, float) t] t = sexp);
    ) cases
  ;;

  type sub1_alias = sub1
  [@@deriving sexp_poly]
  type u = [ `A | sub1_alias | `D ]
  [@@deriving sexp]

  let%test_unit _ =
    let cases : (u * _) list =
      [ `A, "A"
      ; `C1, "C1"
      ; `C2, "C2"
      ; `D, "D"
      ]
    in
    List.iter (fun (u, sexp_as_str) ->
      let sexp = Sexplib.Sexp.of_string sexp_as_str in
      assert ([%of_sexp: u] sexp = u);
      assert ([%sexp_of: u] u = sexp);
    ) cases
  ;;
end

module Polymorphic_record_field = struct
  type 'x t =
    { poly : 'a 'b. 'a list
    ; maybe_x : 'x option
    }
  [@@deriving sexp]
  let%test_unit _ =
    let t x = { poly = []; maybe_x = Some x } in
    let sexp = Sexplib.Sexp.of_string "((poly ())(maybe_x (1)))" in
    assert (t_of_sexp int_of_sexp sexp = t 1);
    assert (sexp_of_t sexp_of_int (t 1) = sexp);
  ;;
end

module No_unused_value_warnings : sig end = struct
  module No_warning : sig
    type t = [ `A ] [@@deriving sexp]
  end = struct
    type t = [ `A ] [@@deriving sexp]
  end
  module Empty = struct
  end
  module No_warning2(X : sig type t [@@deriving sexp] end) = struct
  end
  (* this one can't be handled (what if Empty was a functor, huh?) *)
  (* module No_warning3(X : sig type t with sexp end) = Empty *)
  module type S = sig
    type t = [ `A ] [@@deriving sexp]
  end
  module No_warning4 : S = struct
    type t = [ `A ] [@@deriving sexp]
  end
  module No_warning5 : S = ((struct
    type t = [ `A ] [@@deriving sexp]
  end : S) : S)

  module Nested_functors
    (M1 : sig type t [@@deriving sexp] end)
    (M2 : sig type t [@@deriving sexp] end) = struct
  end

  let () =
    let module M : sig
      type t [@@deriving sexp]
    end = struct
      type t [@@deriving sexp]
    end in
    ()

  module Include = struct
    include (struct
      type t = int [@@deriving sexp]
    end : sig
      type t [@@deriving sexp]
    end with type t := int)
  end
end

module Default = struct
  type t = {
    a : int [@default 2];
  } [@@deriving sexp]
  let%test _ = Sexp.(List [List [Atom "a"; Atom "1"]]) = sexp_of_t { a = 1 }
  let%test _ = Sexp.(List [List [Atom "a"; Atom "2"]]) = sexp_of_t { a = 2 }
  let%test _ = t_of_sexp (Sexp.(List [List [Atom "a"; Atom "1"]])) = { a = 1 }
  let%test _ = t_of_sexp (Sexp.(List [List [Atom "a"; Atom "2"]])) = { a = 2 }
  let%test _ = t_of_sexp (Sexp.(List [])) = { a = 2 }
end

module Type_alias = struct
  (* checking that the [as 'a] is supported and ignored in signatures, that it still
     exports the sexp_of_t__ when needed *)
  module B : sig
    type a = [ `A ]
    type t = ([`A] as 'a) constraint 'a = a
    [@@deriving sexp]
  end = struct
    type a = [ `A ] [@@deriving sexp]
    type t = [ `A ] [@@deriving sexp]
  end
  let%test _ = Sexp.to_string (B.sexp_of_t `A) = "A"
  let%test _ = `A = B.t_of_sexp (Sexplib.Sexp.of_string "A")

  module B2 = struct
    type t = [ B.t | `B ] [@@deriving sexp]
  end

  module C : sig
    type t = (int as 'a)
    [@@deriving sexp]
  end = struct
    type t = int
    [@@deriving sexp]
  end

  module D : sig
    type t = 'a constraint 'a = int
    [@@deriving sexp]
  end = struct
    type t = int
    [@@deriving sexp]
  end
end

module Tricky_variants = struct
  (* Checking that the generated code compiles (there used to be a problem with subtyping
     constraints preventing proper generalization). *)
  type t = [ `a ] [@@deriving sexp]
  type 'a u = [ t | `b of 'a ] * int [@@deriving sexp]
end

module Drop_default = struct
  type t = {
    a : int [@default 2] [@sexp_drop_default];
  } [@@deriving sexp]
  let%test _ = Sexp.(List [List [Atom "a"; Atom "1"]]) = sexp_of_t { a = 1 }
  let%test _ = Sexp.(List []) = sexp_of_t { a = 2 }
  let%test _ = t_of_sexp (Sexp.(List [List [Atom "a"; Atom "1"]])) = { a = 1 }
  let%test _ = t_of_sexp (Sexp.(List [List [Atom "a"; Atom "2"]])) = { a = 2 }
  let%test _ = t_of_sexp (Sexp.(List [])) = { a = 2 }
end

module Drop_if = struct
  type t = {
    a : int [@default 2] [@sexp_drop_if fun x -> x mod 2 = 0]
  } [@@deriving sexp]
  let%test _ = Sexp.(List [List [Atom "a"; Atom "1"]]) = sexp_of_t { a = 1 }
  let%test _ = Sexp.(List []) = sexp_of_t { a = 2 }
  let%test _ = Sexp.(List [List [Atom "a"; Atom "3"]]) = sexp_of_t { a = 3 }
  let%test _ = Sexp.(List []) = sexp_of_t { a = 4 }
  let%test _ = t_of_sexp (Sexp.(List [List [Atom "a"; Atom "1"]])) = { a = 1 }
  let%test _ = t_of_sexp (Sexp.(List [List [Atom "a"; Atom "2"]])) = { a = 2 }
  let%test _ = t_of_sexp (Sexp.(List [List [Atom "a"; Atom "3"]])) = { a = 3 }
  let%test _ = t_of_sexp (Sexp.(List [List [Atom "a"; Atom "4"]])) = { a = 4 }
  let%test _ = t_of_sexp (Sexp.(List [])) = { a = 2 }

  type u = {
    a : int [@sexp_drop_if fun x ->
      (* pa_type_conv used to drop parens altogether, causing type errors in the
         following code *)
      let pair = (x, 2) in
      match Some pair with
      | None -> true
      | Some (x, y) -> x = y
    ]
  } [@@deriving sexp]
end

module Omit_nil = struct
  type natural_option = int

  let sexp_of_natural_option i =
    if i >= 0 then sexp_of_int i
    else sexp_of_unit ()

  let natural_option_of_sexp = function
    | Sexp.List [] -> -1
    | sexp -> int_of_sexp sexp

  let check sexp_of_t t_of_sexp str t =
    let sexp = Sexplib.Sexp.of_string str in
    assert (sexp = sexp_of_t t);
    assert (t_of_sexp sexp = t);
  ;;

  type t =
    { a : natural_option [@sexp.omit_nil] }
  [@@deriving sexp]
  let%test_unit _ = check sexp_of_t t_of_sexp "()" { a = -1 }
  let%test_unit _ = check sexp_of_t t_of_sexp "((a 1))" { a = 1 }

  type t2 =
    | A of { a : int list [@sexp.omit_nil] }
  [@@deriving sexp]
  let%test_unit _ = check sexp_of_t2 t2_of_sexp "(A)" (A { a = [] })
  let%test_unit _ = check sexp_of_t2 t2_of_sexp "(A (a (1)))" (A { a = [ 1 ] })

end

module No_unused_rec_warning = struct
  type r = { field : r -> unit }
  [@@deriving sexp_of]
end

module True_and_false = struct
  type t =
  | True
  | False
  [@@deriving sexp]

  let%test _ = Sexp.to_string (sexp_of_t True) = "True"
  let%test _ = Sexp.to_string (sexp_of_t False) = "False"
  let%test _ = True = t_of_sexp (Sexplib.Sexp.of_string "True")
  let%test _ = False = t_of_sexp (Sexplib.Sexp.of_string "False")
  let%test _ = True = t_of_sexp (Sexplib.Sexp.of_string "true")
  let%test _ = False = t_of_sexp (Sexplib.Sexp.of_string "false")

  type u =
  | True of int
  | False of int
  [@@deriving sexp]

  let%test _ = Sexp.to_string (sexp_of_u (True 1)) = "(True 1)"
  let%test _ = Sexp.to_string (sexp_of_u (False 2)) = "(False 2)"
  let%test _ = True 1 = u_of_sexp (Sexplib.Sexp.of_string "(True 1)")
  let%test _ = False 2 = u_of_sexp (Sexplib.Sexp.of_string "(False 2)")
  let%test _ = True 1 = u_of_sexp (Sexplib.Sexp.of_string "(true 1)")
  let%test _ = False 2 = u_of_sexp (Sexplib.Sexp.of_string "(false 2)")

  exception True [@@deriving sexp]
  let%test _ = "ppx_sexp_test.ml.True_and_false.True" = Sexp.to_string (sexp_of_exn True)

  exception False of int [@@deriving sexp]
  let%test _ = "(ppx_sexp_test.ml.True_and_false.False 1)" = Sexp.to_string (sexp_of_exn (False 1))

  type v = [ `True | `False of int ] [@@deriving sexp]
  let%test _ = Sexp.to_string (sexp_of_v `True) = "True"
  let%test _ = Sexp.to_string (sexp_of_v (`False 2)) = "(False 2)"
end

module Gadt = struct
  let is_eq sexp str =
    let sexp2 = Sexplib.Sexp.of_string str in
    if sexp <> sexp2 then begin
      Printf.printf "%S vs %S\n%!" (Sexp.to_string sexp) str;
      assert false
    end

  (* plain type without argument *)
  type 'a s = Packed : 'a s [@@deriving sexp_of]
  let%test_unit _ = is_eq ([%sexp_of: int s] Packed) "Packed"

  (* two kind of existential variables *)
  type 'a t = Packed : 'a * _ * 'b sexp_opaque -> 'a t [@@deriving sexp_of]
  let%test_unit _ = is_eq ([%sexp_of: int t] (Packed (2, "asd", 1.))) "(Packed 2 _ <opaque>)"

  (* plain type with argument *)
  type 'a u = A : 'a -> 'a u [@@deriving sexp_of]
  let%test_unit _ = is_eq ([%sexp_of: int u] (A 2)) "(A 2)"

  (* recursive *)
  type v = A : v option -> v [@@deriving sexp_of]
  let%test_unit _ = is_eq ([%sexp_of: v] (A (Some (A None)))) "(A((A())))"

  (* implicit existential variable *)
  type w = A : 'a * int * ('a -> string) -> w [@@deriving sexp_of]
  let%test_unit _ = is_eq ([%sexp_of: w] (A (1., 2, string_of_float))) "(A _ 2 <fun>)"

  (* tricky variable naming *)
  type 'a x = A : 'a -> 'b x [@@deriving sexp_of]
  let%test_unit _ = is_eq ([%sexp_of: int x] (A 1.)) "(A _)"

  (* interaction with inline record *)
  type _ x2 = A : { x : 'c } -> 'c x2 [@@deriving sexp_of]
  let%test_unit _ = is_eq ([%sexp_of: int x2] (A { x = 1 })) "(A (x 1))"

  (* unused but colliding variables *)
  type (_, _) y = A : ('a, 'a) y [@@deriving sexp_of]
  let%test_unit _ = is_eq ([%sexp_of: (int, int) y] A) "A"

  (* making sure we're not reversing parameters *)
  type (_, _) z = A : ('a * 'b) -> ('a, 'b) z [@@deriving sexp_of]
  let%test_unit _ = is_eq ([%sexp_of: (int, string) z] (A (1, "a"))) "(A (1 a))"

  (* interaction with universal quantifiers *)
  type _ z2 = A : { x : 'c. 'c option } -> 'c z2 [@@deriving sexp_of]
  let%test_unit _ = is_eq ([%sexp_of: unit z2] (A { x = None })) "(A (x ()))"
end

module Anonymous_variable = struct
  type _ t = int [@@deriving sexp]
  let%test _ = Sexp.to_string ([%sexp_of: _ t] 2) = "2"
  let%test _ = [%of_sexp: _ t] (Sexplib.Sexp.of_string "2") = 2

  (* making sure we don't generate signatures like (_ -> Sexp.t) -> _ t -> Sexp.t which
     are too general *)
  module M : sig
    type _ t [@@deriving sexp]
  end = struct
    type 'a t = 'a [@@deriving sexp]
  end
end

module Record_field_disambiguation = struct

  type a = { fl: float; b : b }
  and b = { fl: int }
  [@@deriving sexp]

end

module Private = struct
  type t = private int [@@deriving sexp_of]

  type ('a, 'b) u = private t [@@deriving sexp_of]

  type ('a, 'b, 'c) v = private ('a, 'b) u [@@deriving sexp_of]
end

module Nonregular_types = struct
  type 'a nonregular =
    | Leaf of 'a
    | Branch of ('a * 'a) nonregular
  [@@deriving sexp]

  type 'a variant = [ `A of 'a ] [@@deriving sexp]
  type ('a, 'b) nonregular_with_variant =
    | Branch of ([ | 'a list variant ], 'b) nonregular_with_variant
  [@@deriving sexp]
end

module Magic_types = struct
  type t =
    { sexp_array : int sexp_array
    ; sexp_list : int sexp_list
    ; sexp_option : int sexp_option
    ; sexp_bool : sexp_bool
    }
  [@@deriving sexp]

  let sexp = Sexplib.Sexp.of_string "()"
  let t =
    { sexp_array = [||]
    ; sexp_list = []
    ; sexp_option = None
    ; sexp_bool = false
    }
  let%test _ = t_of_sexp sexp = t
  let%test _ = sexp_of_t t = sexp

  let sexp =
    Sexplib.Sexp.of_string "((sexp_array (1 2))\
                     (sexp_list (3 4))\
                     (sexp_option 5)\
                     (sexp_bool))"
  let t =
    { sexp_array = [|1; 2|]
    ; sexp_list = [3; 4]
    ; sexp_option = Some 5
    ; sexp_bool = true
    }
  let%test _ = t_of_sexp sexp = t
  let%test _ = sexp_of_t t = sexp


  type u =
    | A of int sexp_list
  [@@deriving sexp]
  type v =
    [ `A of int sexp_list ]
  [@@deriving sexp]
  let sexp = Sexplib.Sexp.of_string "(A 1 2 3)"
  let u = A [1; 2; 3]
  let v = `A [1; 2; 3]
  let%test _ = u_of_sexp sexp = u
  let%test _ = sexp_of_u u = sexp
  let%test _ = v_of_sexp sexp = v
  let%test _ = sexp_of_v v = sexp
end

module Variance = struct
  type (+'a, -'b, 'c, +_, -_, _) t [@@deriving sexp]
end

module Clash = struct
  (* Same name for type-var and type-name; must be careful when introducing rigid type names. *)
  type 'hey hey = Hey of 'hey [@@deriving sexp]
  type 'hey rigid_hey = Hey of 'hey [@@deriving sexp]
  type ('foo,'rigid_foo) foo = Foo of 'foo [@@deriving sexp]
  type 'rigid_bar rigid_rigid_bar = Bar [@@deriving sexp]
end

module Applicative_functor_types = struct
  module Bidirectional_map = struct
    type ('k1, 'k2) t
    module S(K1 : sig type t end)(K2 : sig type t end) = struct
      type nonrec t = (K1.t, K2.t) t
    end
    module type Of_sexpable = sig
      type t [@@deriving of_sexp]
    end
    let s__t_of_sexp
          (type k1 k2)
          (module K1 : Of_sexpable with type t = k1)
          (module K2 : Of_sexpable with type t = k2)
          (_ : Sexp.t) : (k1, k2) t = assert false
  end
  module Int = struct
    type t = int [@@deriving of_sexp]
  end
  module String = struct
    type t = string [@@deriving of_sexp]
  end
  module M : sig
    type t = Bidirectional_map.S(String)(Int).t [@@deriving of_sexp]
  end = struct
    type t = Bidirectional_map.S(String)(Int).t [@@deriving of_sexp]
  end
end

module Type_extensions = struct
  let _ = ([%sexp_of: int] : [%sexp_of: int])
  let _ = ([%of_sexp: int] : [%of_sexp: int])
end
