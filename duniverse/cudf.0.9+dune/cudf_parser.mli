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

(** Parser for CUDF related documents *)

open Cudf
open Cudf_types

(** a CUDF parser opened on some input source *)
type cudf_parser

(** Error during parsing (syntax error, type error, ...). Arguments are error
    message and error location. *)
exception Parse_error of string * loc

(** create a CUDF parser reading data from an input channel

    @param typedecl (initial) per-stanza and per-property type declarations to
    be used while parsing. Default: {!Cudf_conf.stanza_typedecl}
*)
val from_in_channel : ?typedecl:Cudf_conf.stanza_typedecl ->
  in_channel -> cudf_parser

(** create a CUDF parser reading data from an Extlib input channel

    @param typedecl (initial) per-stanza and per-property type declarations to
    be used while parsing. Default: {!Cudf_conf.stanza_typedecl}
*)
val from_IO_in_channel : ?typedecl:Cudf_conf.stanza_typedecl ->
  IO.input -> cudf_parser

(** create a CUDF parser reading data from a file

    @param typedecl as per {!Cudf_parser.from_in_channel}
*)
val from_file :  ?typedecl:Cudf_conf.stanza_typedecl ->
  string -> cudf_parser

(** Dispose a CUDF parser.

    Afterwards, the parser should not be used any longer *)
val close : cudf_parser -> unit


(** {6 Full CUDF document parsing}

    "parse_*" functions offer plain syntax parsing, with no semantic
    interpretation of what is being parsed. "load_*" functions offer
    the latter, hence also checking for semantic constraints (such as
    the lack of key duplication).

    All full parsing function are granted to raise only
    {!Cudf_parser.Parse_error}; finer grained exception are mapped to it.
*)

(** parse a CUDF document (or a universe) as a whole

    @return a triple [preamble, packages, request] where preamble and request
    are returned only if actually met in the parsed document. Note that a
    document with no request part is not a valid CUDF document (but might still
    be used to represent solver solutions, for instance).

    @raise Parse_error when an error during parsing is encountered (might be a
    syntax error, a type error, ..)
*)
val parse : cudf_parser -> preamble option * package list * request option

(** same as {!Cudf_parser.parse}, but additionally loads the package
    list as an abstract {!Cudf.universe}.

    {b Note}: to load compact universes (i.e. only containing package names,
    versions, and installed status) that will be tested as solutions you should
    use {!Cudf_parser.load_solution} instead: the present function does not
    expand missing metadata with respect to the initial status.

    @raise Parse_error as {!Cudf_parser.parse} does
    @raise Cudf.Constraint_violation as {!Cudf.load_universe} does
*)
val load : cudf_parser -> preamble option * universe * request option

(** Load a solution wrt to a given CUDF document, whose universe is given.

    Solution format is as per Appendix B of CUDF 2.0 spec

    @raise Parse_error as {!Cudf_parser.parse} does *)
val load_solution : cudf_parser -> universe -> preamble option * universe

(** Shorthand: parse a file given its name *)
val parse_from_file : ?typedecl:Cudf_conf.stanza_typedecl ->
  string -> preamble option * package list * request option

(** Shorthand: load from a file given its name *)
val load_from_file : ?typedecl:Cudf_conf.stanza_typedecl ->
  string -> preamble option * universe * request option

(** Shorthand: load a solution from a file given its name *)
val load_solution_from_file : string -> universe -> preamble option * universe


(** {6 Item-by-item CUDF parsing} *)

(** Parse the next information item (either a package description, a user
    request, or a preamble) from the given input channel.

    Beware that parsing is stateful; in particular when the preamble is parsed,
    the list of allowed properties for future package stanzas is internally
    updated.
*)
val parse_item : cudf_parser -> cudf_item

(** {6 Low-level parsing functions}

    The following parsing function usually raise fine grained exceptions such
    as {!Cudf_types.Syntax_error} and {!Cudf_types.Type_error}.
*)

type loc_map = (string * loc) list

(** Parse a file stanza (i.e., a RFC822-like stanza, with the notable
    simplification that all field/value pairs are one-liners). Strip
    any heading blanks lines leading to the first available
    field/value pair.

    @return an associative list mapping field name to field values and a
    location map mapping field names to locations

    @raise End_of_file if no other stanza is available due to reached EOF

    @raise Cudf_types.Syntax_error when a syntax error is encountered
*)
val parse_stanza : cudf_parser -> loc_map * string stanza

(** Type check an untyped stanza according to a given set of type declarations.
    Also take care of default values, adding missing properties where needed;
    fail if a required property is missing.

    @param loc location map from prop name to locations, default is None
    (i.e. use dummy locations)

    @raise Syntax_error if a property does not match its declared type; this
    exception is also raised when an undeclared property is encountered

    @raise Type_error when a property has a value of the wrong type
*)
val type_check_stanza : ?locs:loc_map ->
  string stanza -> typedecl -> typed_value stanza
