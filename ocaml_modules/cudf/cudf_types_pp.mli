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

(** CUDF type library: parsing and pretty printing

    Implement parsing and pretty printing of CUDF types (see CUDF spec. §2.2.2).

    For the actual CUDF type definition see {!module: Cudf_types}.

    For pretty printing of macro-components see {!module: Cudf_printer}.
*)

open Cudf_types

(** {5 Errors} *)

exception Type_error of typ * typed_value

(** {5 Parsers} *)

(** {6 Public types}

    All parsing function are granted to raise only {!Cudf_types_pp.Type_error},
    lower lever exception (e.g. syntax errors) are wrapped into it *)

val parse_int : string -> int
val parse_posint : string -> int
val parse_nat : string -> int
val parse_bool : string -> bool
val parse_string : string -> string
val parse_pkgname : string -> pkgname
val parse_ident : string -> string
val parse_enum : enums:string list -> string -> string
val parse_vpkg : string -> vpkg
val parse_vpkglist : string -> vpkglist
val parse_vpkgformula : string -> vpkgformula
val parse_veqpkg : string -> veqpkg
val parse_veqpkglist : string -> veqpkglist
val parse_typedecl : string -> typedecl

(** {6 Parsing of other CUDF entities}

    Mostly for application relying on CUDF conventions *)

(** Parse a quoted string, enclosed by double quotes as it happens within the
    "property" property of preamble stanzas. The only place where such strings
    are allowed in CUDF are within type declarations; see
    {!Cudf_types_pp.parse_typedecl}.

    @return the parsed string after having resolved escaping and removed
    surrounding double quotes

    @raise Cudf_types.Syntax_error when the quoted string cannot be parsed
*)
val parse_qstring : string -> string

(** Parse a CUDF type expression.

    At present it can be either a typename or an enum with its values.

    @raise Cudf_types.Syntax_error when the given string is not a valid type
    expression
*)
val parse_type : string -> typ

(** Parse the enum value corresponding to the "keep" core property of package
    stanzas. Shorthand to avoid parsing the corresponding `Enum and then
    casting to {!Cudf_types.enum_keep} *)
val parse_keep : string -> enum_keep

(** generic, type-based parsing *)
val parse_value : typ -> string -> typed_value


(** {5 Pretty printers} *)

(** {6 Pretty print to string} *)

val string_of_int : int -> string
val string_of_posint : int -> string
val string_of_nat : int -> string
val string_of_bool : bool -> string
val string_of_keep : enum_keep -> string
val string_of_pkgname : pkgname -> string
val string_of_version : version -> string
val string_of_vpkg : vpkg -> string
val string_of_vpkglist : vpkglist -> string
val string_of_vpkgformula : vpkgformula -> string
val string_of_veqpkg : veqpkg -> string
val string_of_veqpkglist : veqpkglist -> string
val string_of_typedecl : typedecl -> string

val string_of_type : typ -> string
val string_of_value : typed_value -> string

