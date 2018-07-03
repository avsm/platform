(**************************************************************************************)
(*  Copyright (C) 2015 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2015 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open Common

(** {2 Exceptions} *)

(** IgnorePackage error message *)
exception IgnorePackage of string

(** {2 Common Parsing Functions} *)

val parse_name : Format822.field -> Packages_types.name
val parse_version : Format822.field -> Packages_types.version
val parse_vpkg : Format822.field -> Packages_types.vpkg
val parse_vpkglist : Format822.field -> Packages_types.vpkglist
val parse_vpkgformula : Format822.field -> Packages_types.vpkgformula
val parse_archlist : Format822.field -> Packages_types.architecture list
val parse_builddepslist : Format822.field -> Packages_types.builddepslist
val parse_builddepsformula : Format822.field -> Packages_types.builddepsformula

val parse_string : Format822.field -> string
val parse_string_opt : Format822.field -> string option
val parse_string_list : ?rex:Re.re -> Format822.field -> string list

val parse_int : Format822.field -> int
val parse_int_s : Format822.field -> string

val parse_bool : Format822.field -> bool
val parse_bool_s : Format822.field -> string

(** {2 Generic Parsing Functions} *)

val lexbuf_wrapper :
  ((Lexing.lexbuf -> Packages_parser.token) -> Lexing.lexbuf -> 'a) ->
  Format822.field -> 'a

(* [assoc label stanza] returns the value associated with label a in stanza.
   Raise [Not_found] if there is no value associated with label in stanza. *)
val assoc : string -> Format822.stanza -> Format822.value

val blank_regexp : Re.re
val comma_regexp : Re.re

(** Parsing function for extra values. An extra value can only be a string.
    Ex. [parse_s ?required:true parse_string] *)
type parse_extras_f = (string -> Format822.stanza -> string)

val parse_e :
  (string * parse_extras_f option) list ->
    Format822.stanza -> (string * string) list

(** parse_s is a generic function used to extract and parse a field from a
    stanza and cast it to a value. [?default] assign a default value if the
    field is absent.  The function raise [ParseError] if [?required] is true
    ( default false ) and the field is absent and no default is given. 
    [parse_s] gets a parsing function, a label and a stanza and returns the 
    value associated to the label. *)
val parse_s : ?default:'a -> ?required:bool ->
  (Format822.field -> 'a) -> string -> Format822.stanza -> 'a

(** get_field_value is a generic function used to extract and parse values
    from a Format822.stanza. It gets a parsing function [parse], a
    stanza [par] and a tuple [field] where the first element is the label
    associated to the value to be parsed and the second element is a parsed
    value. If the parsed value is not none, the the function returns it 
    directly together with the associated label. Otherwise the function will
    use the parsing function to extract the value from the stanza. This function
    is used to initialize a [package] object with certains defaults values without
    parsing the entire stanza. *)
val get_field_value:
  parse:(string -> Format822.stanza -> 'a) -> 
    par:Format822.stanza -> 
      field:(string * 'a option) -> (string * 'a)

(** {2 Generic Parsing Functions} *)

(** Representation of a PEF package. This object gets a stanza 
    (a list of list of fields) and return a pef object. Each field
    can be directly initialized using the optional arguments, providing
    the name of the field and an optional value. If the value is None, then
    the value is computed by parsing the corresponding field in the 
    822 stanza. Otherwise, the field is initialized using the given value (and
    ignoring the value in the 822 stanza).

    Extra fields can be parsed and added to the stanza.
    The first element of [extras] is a list of tuples where the first element
    is a label indentifing a field and the second element is a parsing
    function. If the second element of [extras] is not None, then the list
    of (field,value) is append to the list of parsed extras from the 822 stanza.
*)
class package :
  ?name:string * Packages_types.name option ->
  ?version:string * Packages_types.version option ->
  ?installed:string * Packages_types.installed option ->
  ?depends:string * Packages_types.vpkgformula option ->
  ?conflicts:string * Packages_types.vpkglist option ->
  ?provides:string * Packages_types.vpkglist option ->
  ?recommends:string * Packages_types.vpkgformula option ->
  ?extras:(
    (string * parse_extras_f option) list * 
    (string * string) list option) -> Format822.stanza ->
  object ('a)

    (** {4 Access methods } *)
    method name : Packages_types.name
    method version : Packages_types.version
    method conflicts : Packages_types.vpkglist
    method depends : Packages_types.vpkgformula
    method provides : Packages_types.vpkglist
    method recommends : Packages_types.vpkgformula
    method installed : Packages_types.installed
    method extras : (string * string) list

    (** {4 low level val . Used in subclasses} *)
    val name : (string * Packages_types.name)
    val version : (string * Packages_types.version)
    val conflicts : (string * Packages_types.vpkglist)
    val depends : (string * Packages_types.vpkgformula)
    val provides : (string * Packages_types.vpkglist)
    val recommends : (string * Packages_types.vpkgformula)
    val installed : (string * Packages_types.installed)

    (** {4 get/set specific fields of the object} *)
    method get_extra : string -> string
    method add_extra : string -> string -> 'a
    method set_extras : (string * string) list -> 'a
    method set_installed : Packages_types.installed -> 'a

    (* Print the object as a 822 stanza to the given channel *)
    method pp : out_channel -> unit
  end

(* [parse_package_stanza filter extras par]. Stanzas are filterd out
   according to the predicate [filter]. 
   
   Extra fields can be parsed and added to the stanza.
   [extras] is a list of tuples where the first element is a label
   indentifing a field and the second element is a parsing function. Ex : 
   
     [extras:[("extrafield", Some(parse_s parse_string))]] *)
val parse_package_stanza :
  filter:(Format822.stanza -> bool) option ->
    extras:(string * (string -> Format822.stanza -> string) option) list ->
      Format822.stanza -> package option

(** Read n files from disk and return the list of all unique packages. Extras have the
    same format as in [parse_package_stanza] *)
val input_raw :
  ?extras:(string * (string -> Format822.stanza -> string) option) list ->
    string list -> package list

(** same as [parse_package_stanza] but read packages stanzas from the given IO channel *)
val parse_packages_in :
  ?filter:(Format822.stanza -> bool) ->
    ?extras:(string * (string -> Format822.stanza -> string) option) list ->
      string -> IO.input -> package list

(** [input_raw_in] behaves as [input_raw] but read the packages stanzas from
    the given IO channel *)
val input_raw_in :
  ?extras:(string * (string -> Format822.stanza -> string) option) list ->
  IO.input -> package list

(** {2 Low Level Parsing Functions} *)

(* [packages_parser fname stanza_parser f822_parser].
   Parse the entire file [fname] parsing the raw file using [f822_parser] 
   while filtering out unwanted stanzas using the [stanza_parser]. *)
val packages_parser : string -> 
  (Format822.stanza -> 'package option) ->
    Format822.f822_parser -> 
      'package list
