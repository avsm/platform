(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

val signature_of_summary : Env.summary -> Types.signature_item option

val summary_prev : Env.summary -> Env.summary option

val summary_module_ident_opt : Env.summary -> Ident.t option

val dest_tstr_eval : Typedtree.structure -> Typedtree.expression

val labels_of_application :
  prefix:string -> Typedtree.expression -> (string * Types.type_expr) list

val select_open_node :
  ('a * Browse_raw.node) list ->
  (Path.t * ('a * Browse_raw.node) list) option

val texp_function_cases : Typedtree.expression_desc -> Typedtree.case list

val const_string : string * string option -> Asttypes.constant

val dummy_type_scheme : Types.type_desc -> Types.type_expr

val ctype_instance : Env.t -> Types.type_expr -> Types.type_expr

val si_modtype_opt : Types.signature_item -> Types.module_type option

module Pattern : sig
  open Asttypes

  type pattern = Typedtree.pattern

  type desc_view =
    | Tpat_any
    | Tpat_var of Ident.t * string loc
    | Tpat_alias of pattern * Ident.t * string loc
    | Tpat_constant of constant
    | Tpat_tuple of pattern list
    | Tpat_construct of
        Longident.t loc * Types.constructor_description * pattern list
    | Tpat_variant of label * pattern option * Types.row_desc ref
    | Tpat_record of
        (Longident.t loc * Types.label_description * pattern) list *
        closed_flag
    | Tpat_array of pattern list
    | Tpat_or of pattern * pattern * Types.row_desc option
    | Tpat_lazy of pattern
    | Tpat_exception of pattern

  val view : pattern -> desc_view

  exception Not_supported

  val update_desc_exn : pattern -> desc_view -> pattern
  (** will raise [Not_supported] if the desc doesn't exist in that version of
      OCaml. *)
end
