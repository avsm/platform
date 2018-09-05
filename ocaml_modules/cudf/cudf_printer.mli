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

(** Pretty printing of CUDF macro-components (documents, stanzas, ...)

    For pretty printing of micro-components see {!module: Cudf_types_pp}.
*)

open Cudf

(** {6 Pretty print to standard output channels} *)

val pp_cudf : out_channel -> cudf -> unit
val pp_doc : out_channel -> cudf_doc -> unit
val pp_solution : out_channel -> solution -> unit
val pp_item : out_channel -> cudf_item -> unit
val pp_package : out_channel -> package -> unit
val pp_preamble : out_channel -> preamble -> unit
val pp_request : out_channel -> request -> unit
val pp_packages : out_channel -> package list -> unit
val pp_universe : out_channel -> universe -> unit


(** {6 Pretty print to abstract output channels}
    
    Note: you can write to string using these methods using the following
    pattern:

    [let o = IO.output_string () in ... Cudf_printer.pp_* o ...; IO.close_out o]
*)

val pp_io_cudf : 'a IO.output -> cudf -> unit
val pp_io_doc : 'a IO.output -> cudf_doc -> unit
val pp_io_solution : 'a IO.output -> solution -> unit
val pp_io_item : 'a IO.output -> cudf_item -> unit
val pp_io_package : 'a IO.output -> package -> unit
val pp_io_preamble : 'a IO.output -> preamble -> unit
val pp_io_request : 'a IO.output -> request -> unit
val pp_io_packages : 'a IO.output -> package list -> unit
val pp_io_universe : 'a IO.output -> universe -> unit


(** {6 Generic, higher-order pretty printers}

    Usually, you shouldn't need those and you should be well served by the
    above printers.

    To bootstrap usage of the generic printers, you'll need to provide a
    pp_property argument --- that takes a property as a pair of name/value
    strings and print them on a generic output --- and then proceed composing
    generic printers together.
*)

val pp_package_gen :
  pp_property:('out -> string * string -> unit) ->
    'out -> Cudf.package -> unit

val pp_request_gen :
  pp_property:('out -> string * string -> unit) ->
    'out -> Cudf.request -> unit

val pp_preamble_gen :
  pp_property:('out -> string * string -> unit) ->
    'out -> Cudf.preamble -> unit

val pp_universe_gen :
  pp_package:('out -> Cudf.package -> unit) ->
  pp_sep:('out -> unit) ->
    'out -> Cudf.universe -> unit

val pp_packages_gen :
  pp_package:('out -> Cudf.package -> unit) ->
  pp_sep:('out -> unit) ->
    'out -> Cudf.package list -> unit

val pp_cudf_gen :
  pp_preamble:('out -> Cudf.preamble -> unit) ->
  pp_universe:('out -> Cudf.universe -> unit) ->
  pp_request:('out -> Cudf.request -> unit) ->
  pp_sep:('out -> unit) ->
    'out -> Cudf.cudf -> unit

val pp_doc_gen :
  pp_preamble:('out -> Cudf.preamble -> unit) ->
  pp_packages:('out -> Cudf.package list -> unit) ->
  pp_request:('out -> Cudf.request -> unit) ->
  pp_sep:('out -> unit) ->
    'out -> Cudf.cudf_doc -> unit

val pp_solution_gen :
  pp_preamble:('out -> Cudf.preamble -> unit) ->
  pp_universe:('out -> Cudf.universe -> unit) ->
  pp_sep:('out -> unit) ->
    'out -> Cudf.solution -> unit

val pp_item_gen :
  pp_package:('out -> Cudf.package -> unit) ->
  pp_request:('out -> Cudf.request -> unit) ->
  pp_preamble:('out -> Cudf.preamble -> unit) ->
    'out -> Cudf.cudf_item -> unit
