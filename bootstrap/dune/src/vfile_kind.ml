open Import

module Id = struct
  type 'a tag = ..

  module type S = sig
    type t
    type 'a tag += X : t tag
  end

  type 'a t = (module S with type t = 'a)

  let create (type a) () =
    let module M = struct
      type t = a
      type 'a tag += X : t tag
    end in
    (module M : S with type t = a)

  let eq (type a) (type b)
        (module A : S with type t = a)
        (module B : S with type t = b)
    : (a, b) eq option =
    match A.X with
    | B.X -> Some Eq
    | _   -> None
end

module type S = sig
  type t

  val id : t Id.t

  val load : Path.t -> t
  val to_string : t -> string
end

type 'a t = (module S with type t = 'a)

let eq (type a) (type b)
      (module A : S with type t = a)
      (module B : S with type t = b) =
  Id.eq A.id B.id

module Make
    (T : sig
       type t
       val t : t Sexp.To_sexp.t
       val name : string
     end)
  : S with type t = T.t =
struct
  type t = T.t

  (* XXX dune dump should make use of this *)
  let _t = T.t

  module P = Utils.Persistent(struct
      type nonrec t = t
      let name = "VFILE_KIND-" ^ T.name
      let version = 1
    end)

  let id = Id.create ()

  let to_string x = P.to_out_string x

  let load path = Option.value_exn (P.load path)
end

