(**************************************************************************************)
(*  Copyright (C) 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2010 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open Common

let run () =
  let latency ?(reps=4) s f = Benchmark.latency1 ~name:s (Int64.of_int reps) f in 

  let ch = Input.open_file "tests/Packages" in
  let extras_properties = [
      ("maintainer", ("maintainer", `String None));
      ("size", ("size", `Nat None));
      ("installed-Size", ("installedsize", `Nat None))
    ]
  in
  let extras = List.map fst extras_properties in

  let ipr_list = Packages.parse_packages_in ~extras:extras (fun x -> x) ch in
  let tables = Debcudf.init_tables ipr_list in
  let cudf_list = List.map (Debcudf.tocudf ~extras:extras_properties tables) ipr_list in
  let universe = Cudf.load_universe cudf_list in

  let test_ipr_list =
    latency ~reps:20 "Packages.parse_packages_in" (fun () ->
      Packages.parse_packages_in ~extras:extras (fun x -> x) ch
    )
  in
  let test_tables =
   latency ~reps:10 "Debcudf.init_tables" (fun () ->
    Debcudf.init_tables ipr_list
   )
  in 
  let test_cudf_list =
    latency "Debcudf.tocudf" (fun () ->
      List.map (Debcudf.tocudf ~extras:extras_properties tables) ipr_list
    )
  in
  let test_universe =
    latency ~reps:20 "Cudf.load_universe" (fun () ->
      Cudf.load_universe cudf_list
    )
  in
  let test_maps =
    latency ~reps:10 "CudfAdd.build_maps" (fun () ->
      CudfAdd.build_maps universe
    )
  in
  List.fold_left Benchmark.merge [] [
    (* test_ipr_list (); *)
    test_tables ();
    test_cudf_list ();
    test_universe ();
    test_maps ()
  ] 
;;

ExtBenchmark.main run ;;

