open! Import

type t = Byte | Native

let all = [Byte; Native]

let t =
  let open Sexp.Of_sexp in
  enum
    [ "byte"   , Byte
    ; "native" , Native
    ]

let choose byte native = function
  | Byte   -> byte
  | Native -> native

let compiled_unit_ext = choose ".cmo" ".cmx"
let compiled_lib_ext = choose ".cma" ".cmxa"
let plugin_ext = choose ".cma" ".cmxs"

let variant = choose Variant.byte Variant.native

let cm_kind = choose Cm_kind.Cmo Cmx

let exe_ext = choose ".bc" ".exe"

let of_cm_kind : Cm_kind.t -> t = function
  | Cmi | Cmo -> Byte
  | Cmx -> Native

module Dict = struct
  type 'a t =
    { byte   : 'a
    ; native : 'a
    }

  let get t = function
    | Byte   -> t.byte
    | Native -> t.native

  let of_func f =
    { byte   = f ~mode:Byte
    ; native = f ~mode:Native
    }

  let map2 a b ~f =
    { byte   = f a.byte   b.byte
    ; native = f a.native b.native
    }

  let make_both x =
    { byte   = x
    ; native = x
    }

  module Set = struct
    type nonrec t = bool t

    let all =
      { byte   = true
      ; native = true
      }

    let to_list t =
      let l = [] in
      let l = if t.native then Native :: l else l in
      let l = if t.byte   then Byte   :: l else l in
      l

    let of_list l =
      { byte   = List.mem Byte   ~set:l
      ; native = List.mem Native ~set:l
      }

    let t = Sexp.Of_sexp.(map (list t) ~f:of_list)

    let is_empty t = not (t.byte || t.native)

    let iter t ~f =
      if t.byte   then f Byte;
      if t.native then f Native
  end
end
