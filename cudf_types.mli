(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009-2012  Stefano Zacchiroli <zack@upsilon.cc>            *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

(** CUDF type library

    Implement core CUDF types (see CUDF spec. §2.2.2).

    For parsing and pretty printing of CUDF types see {!Cudf_types_pp}.
*)


(** {5 CUDF types} *)

type version = int	(* required to be non 0 *)
type relop = [`Eq | `Neq | `Geq | `Gt | `Leq | `Lt]
type constr = (relop * version) option


(** {6 CUDF spec. types} *)

type pkgname = string
type vpkg = pkgname * constr
type vpkglist = vpkg list
type enum_keep = [`Keep_version | `Keep_package | `Keep_feature | `Keep_none ]

(** CNF formula. Inner lists are OR-ed, outer AND-ed.
    E.g.:
    - "Depends: foo, baz | baz"		-->	[ [ foo ] ; [ bar ; baz ] ]
    - "Depends: true!"			-->	[ ]
    - "Depends: false!"			-->	[ [] ]
*)
type vpkgformula = vpkg list list

type veqpkg = pkgname * ([`Eq] * version) option
type veqpkglist = veqpkg list

(** CUDF types *)
type typ =
    [ `Int | `Posint | `Nat | `Bool | `String | `Enum of string list
    | `Pkgname | `Ident
    | `Vpkg | `Vpkgformula | `Vpkglist | `Veqpkg | `Veqpkglist
    | `Typedecl ]

val keep_type : typ
val keep_enums : string list

(** (Single) type declaration: each variant denotes a type, its argument the
    default value, None if missing *)
type typedecl1 =
    [ `Int of int option
    | `Posint of int option
    | `Nat of int option
    | `Bool of bool option
    | `String of string option
    | `Pkgname of string option
    | `Ident of string option
    | `Enum of string list * string option	(** enums, default enum *)
    | `Vpkg of vpkg option
    | `Vpkgformula of vpkgformula option
    | `Vpkglist of vpkglist option
    | `Veqpkg of veqpkg option
    | `Veqpkglist of veqpkglist option
    | `Typedecl of typedecl option
    ]

and typedecl = (string * typedecl1) list

(** Typed value in the value space of all CUDF types *)
type typed_value =
    [ `Int of int
    | `Posint of int
    | `Nat of int
    | `Bool of bool
    | `String of string
    | `Pkgname of string
    | `Ident of string
    | `Enum of string list * string
    | `Vpkg of vpkg
    | `Vpkgformula of vpkgformula
    | `Vpkglist of vpkglist
    | `Veqpkg of veqpkg
    | `Veqpkglist of veqpkglist
    | `Typedecl of typedecl
    ]


(** {5 Manipulation of typed values} *)

(** extract the type of a (single) type declaration *)
val type_of_typedecl : typedecl1 -> typ

(** Create a (single) type declaration having as default value the given typed
    value (i.e. apply the "Some" monad to typed values) *)
val typedecl_of_value : typed_value -> typedecl1

(** Extract the default value from a type declaration (or return [None]) *)
val value_of_typedecl : typedecl1 -> typed_value option

(** Create a (single) type declaration with no default value *)
val typedecl_of_type : typ -> typedecl1

(** @return the type of a given value *)
val type_of_value : typed_value -> typ

(** [cast ty v] attempt a runtime cast of a given (typed) value to a different
    type.

    @raise Type_error if casting is not possible *)
val cast: typ -> typed_value -> typed_value


(** {6 CUDF syntactic types}

    Types used in parsing, before values are injected into the CUDF type
    system. *)

(** RFC-822-like stanza, i.e. an associative list mapping property names to
    property values.

    Values are typed according to the type variable ['ty]. Usually, libCUDF
    uses either [string stanza] (for untyped stanzas) or
    [Cudf_types.typed_value stanza] (for typed stanzas). *)
type 'ty stanza = (string * 'ty) list


(**/**)

(** {5 Parsing helpers}

    Used internally for pasring, generally otherwise uninteresting.
*)

(** Range in a file being parsed *)
type loc = Lexing.position * Lexing.position

(** Dummy location, pointing nowhere, going nowhere, ... *)
val dummy_loc : loc

(** [extend_range (p1, _) (_, p2)] return [(p1, p2)] *)
val extend_loc : loc -> loc -> loc

(** Get file range corresponding to the last read token *)
val loc_of_lexbuf : Lexing.lexbuf -> loc

(**/**)


(** {5 Various errors} *)

(** Error while parsing RFC822-like syntax of CUDF documents.

    arguments: error message and file range, respectively. *)
exception Parse_error_822 of string * loc

(** Syntax error while parsing some literal value

    arguments: error message and file range, respectively *)
exception Syntax_error of string * loc

(** Type error: mismatch between typed value and expected type

    arguments: expected type, found value *)
exception Type_error of typ * typed_value * loc


(** {5 Accessors, predicates, etc.} *)

(** Check whether a formula uses only equality tests over package versions. *)
val is_eq_formula : vpkgformula ->  bool

