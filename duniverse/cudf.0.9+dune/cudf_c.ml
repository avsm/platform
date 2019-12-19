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

let () =
  Callback.register "parse_from_file"
    (Cudf_parser.parse_from_file ?typedecl:None);
  Callback.register "load_from_file"
    (Cudf_parser.load_from_file ?typedecl:None);
  Callback.register "load_solution_from_file"
    Cudf_parser.load_solution_from_file;
  Callback.register "lookup_package_property" Cudf.lookup_package_property;
  Callback.register "lookup_request_property" Cudf.lookup_request_property;
  Callback.register "lookup_preamble_property" Cudf.lookup_preamble_property;
  Callback.register "universe_size" Cudf.universe_size;
  Callback.register "installed_size" Cudf.installed_size;
  Callback.register "is_consistent" Cudf_checker.is_consistent;
  Callback.register "is_solution" Cudf_checker.is_solution;
  Callback.register "load_universe" Cudf.load_universe;

