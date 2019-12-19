(** [Codes] expands some functions based on CamomileLibrary.USet,
    a module implements Sets of Unicode characters, implemented as sets of intervals.
*)

(** [USet.t] *)
type t

(** {3 extended functions} *)

(** based on add_range, [add_ranges l s] add a list of ranges [l] to [s]. *)
val add_ranges :
  (CamomileLibrary.UChar.t * CamomileLibrary.UChar.t) list -> t -> t

(** [tuple_to_range tuple] convert [tuple] to a UChar range *)
val tuple_to_range :
  int * int -> CamomileLibrary.UChar.t * CamomileLibrary.UChar.t

(** [of_tuple_list l] convert int tuple list [l] to a [USet.t] *)
val of_tuple_list : (int * int) list -> t

(** {3 Below are type signatures of the original [CamomileLibrary.USet] module} *)

val empty : t
val is_empty : t -> bool
val mem : CamomileLibrary.UChar.t -> t -> bool
val add : CamomileLibrary.UChar.t -> t -> t
val add_range :
  CamomileLibrary.UChar.t -> CamomileLibrary.UChar.t -> t -> t
val singleton : CamomileLibrary.UChar.t -> t
val remove : CamomileLibrary.UChar.t -> t -> t
val remove_range :
  CamomileLibrary.UChar.t -> CamomileLibrary.UChar.t -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val compl : t -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val subset : t -> t -> bool
val from : CamomileLibrary.UChar.t -> t -> t
val after : CamomileLibrary.UChar.t -> t -> t
val until : CamomileLibrary.UChar.t -> t -> t
val before : CamomileLibrary.UChar.t -> t -> t
val iter : (CamomileLibrary.UChar.t -> unit) -> t -> unit
val iter_range :
  (CamomileLibrary.UChar.t -> CamomileLibrary.UChar.t -> unit) -> t -> unit
val fold : (CamomileLibrary.UChar.t -> 'a -> 'a) -> t -> 'a -> 'a
val fold_range :
  (CamomileLibrary.UChar.t -> CamomileLibrary.UChar.t -> 'a -> 'a) ->
  t -> 'a -> 'a
val for_all : (CamomileLibrary.UChar.t -> bool) -> t -> bool
val exists : (CamomileLibrary.UChar.t -> bool) -> t -> bool
val filter : (CamomileLibrary.UChar.t -> bool) -> t -> t
val partition : (CamomileLibrary.UChar.t -> bool) -> t -> t * t
val cardinal : t -> int
val elements : t -> CamomileLibrary.UChar.t list
val ranges : t -> (CamomileLibrary.UChar.t * CamomileLibrary.UChar.t) list
val min_elt : t -> CamomileLibrary.UChar.t
val max_elt : t -> CamomileLibrary.UChar.t
val choose : t -> CamomileLibrary.UChar.t
val uset_of_iset : CamomileLibrary.Private.ISet.t -> t
val iset_of_uset : t -> CamomileLibrary.Private.ISet.t

