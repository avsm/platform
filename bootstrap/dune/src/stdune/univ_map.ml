module Eq = struct
  type ('a, 'b) t = T : ('a, 'a) t

  let cast (type a) (type b) (T : (a, b) t) (x : a) : b = x
end

module Key = struct
  module Witness = struct
    type 'a t = ..
  end

  module type T = sig
    type t
    type 'a Witness.t += T : t Witness.t
    val id : int
    val name : string
    val sexp_of_t : t -> Usexp.t
  end

  type 'a t = (module T with type t = 'a)

  let next = ref 0

  let create (type a) ~name sexp_of_t =
    let n = !next in
    next := n + 1;
    let module M = struct
      type t = a
      type 'a Witness.t += T : t Witness.t
      let id = n
      let sexp_of_t = sexp_of_t
      let name = name
    end in
    (module M : T with type t = a)

  let id (type a) (module M : T with type t = a) = M.id

  let eq (type a) (type b)
        (module A : T with type t = a)
        (module B : T with type t = b) : (a, b) Eq.t =
    match A.T with
    | B.T -> Eq.T
    | _ -> assert false
end

module Binding = struct
  type t = T : 'a Key.t * 'a -> t
end

type t = Binding.t Int.Map.t

let empty = Int.Map.empty
let is_empty = Int.Map.is_empty

let add (type a) t (key : a Key.t) x =
  let (module M) = key in
  let data = Binding.T (key, x) in
  Int.Map.add t M.id data

let mem t key = Int.Map.mem t (Key.id key)

let remove t key = Int.Map.remove t (Key.id key)

let find t key =
  match Int.Map.find t (Key.id key) with
  | None -> None
  | Some (Binding.T (key', v)) ->
    let eq = Key.eq key' key in
    Some (Eq.cast eq v)

let find_exn t key =
  match Int.Map.find t (Key.id key) with
  | None -> failwith "Univ_map.find_exn"
  | Some (Binding.T (key', v)) ->
    let eq = Key.eq key' key in
    Eq.cast eq v

let singleton key v = Int.Map.singleton (Key.id key) (Binding.T (key, v))

let superpose = Int.Map.superpose

let sexp_of_t (t : t) =
  let open Usexp in
  List (
    Int.Map.to_list t
    |> List.map ~f:(fun (_, (Binding.T (key, v))) ->
      let (module K) = key in
      List
        [ atom_or_quoted_string K.name
        ; K.sexp_of_t v
        ]))
