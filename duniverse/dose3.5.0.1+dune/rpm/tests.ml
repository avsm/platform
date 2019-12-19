(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open OUnit
open Common
open Algo

let test_dir = "tests/rpm"

let hdlist = Filename.concat test_dir "hdlist" ;;
let row_rpm = Rpm.Packages.Hdlists.input_raw [hdlist] ;;
let tables =  Rpm.Rpmcudf.init_tables row_rpm ;;
let pkglist = List.map (Rpm.Rpmcudf.tocudf tables) row_rpm ;;
let universe = Cudf.load_universe pkglist ;;
let solver = Depsolver.load universe ;;

let test_install pl =
  match Depsolver.edos_coinstall solver pl with
  |{ Diagnostic.result = Diagnostic.Success _ } -> true
  |{ Diagnostic.result = Diagnostic.Failure _ } -> false

let test_scenario01 =
  "scenarion 01" >:: (fun _ ->
    let test01 = Cudf.lookup_package universe ("test01",2) in
    let test02 = Cudf.lookup_package universe ("test02",2) in
    assert_equal true (test_install [test01;test02])
    )

let test_scenario02 =
  "scenarion 02" >:: (fun _ ->
    let test03 = Cudf.lookup_package universe ("test03",2) in
    let test04 = Cudf.lookup_package universe ("test04",2) in
    assert_equal true (test_install [test03;test04])
    )

let test_scenario03 =
  "scenarion 03" >:: (fun _ ->
    let test03 = Cudf.lookup_package universe ("test03",2) in
    let test05 = Cudf.lookup_package universe ("test05",2) in
    assert_equal true (test_install [test03;test05])
    )

(* skip scenarion04 *)

let test_scenario05 =
  "scenarion 05" >:: (fun _ ->
    let test07 = Cudf.lookup_package universe ("test07",2) in
    let test08 = Cudf.lookup_package universe ("test08",2) in
    assert_equal true (test_install [test07;test08])
    )

let test_scenario06 =
  "scenarion 06" >:: (fun _ ->
    let test03 = Cudf.lookup_package universe ("test03",2) in
    let test09 = Cudf.lookup_package universe ("test09",2) in
    assert_equal true (test_install [test03;test09])
    )

let test_scenario07 =
  "scenarion 07" >:: (fun _ ->
    let test10 = Cudf.lookup_package universe ("test10",2) in
    let test11 = Cudf.lookup_package universe ("test11",2) in
    assert_equal true (test_install [test10;test11])
    )

let test_scenario08 =
  "scenarion 08" >:: (fun _ ->
    let test05 = Cudf.lookup_package universe ("test05",2) in
    let test12 = Cudf.lookup_package universe ("test12",2) in
    assert_equal true (test_install [test05;test12])
    )

let test_scenario09 =
  "scenarion 09" >:: (fun _ ->
    let test03 = Cudf.lookup_package universe ("test03",2) in
    let test13 = Cudf.lookup_package universe ("test13",2) in
    assert_equal true (test_install [test03;test13])
    )

let test_scenario10 =
  "scenarion 10" >:: (fun _ ->
    let test14 = Cudf.lookup_package universe ("test14",2) in
    let test15 = Cudf.lookup_package universe ("test15",2) in
    assert_equal false (test_install [test14;test15])
    )

let test_scenario11 =
  "scenarion 11" >:: (fun _ ->
    let test11 = Cudf.lookup_package universe ("test11",2) in
    let test16 = Cudf.lookup_package universe ("test16",2) in
    assert_equal true (test_install [test11;test16])
    )


let test_compare = 
  let test_epoch =
    "epoch" >:: (fun _ ->
      assert_equal 1 (Rpm.Version.compare "1:2.0-0.20100203-1xcm14" "20031225")
    )
  in
  let test_overlap =
    "overlap" >:: (fun _ ->
      assert_equal false (
        Rpm.Rpmcudf.compare_constr
        (`Eq, "1:2.0-0.20100203-1xcm14") 
        (`Lt, "20031225")
      )
    )
  in
  "compare" >::: [
    test_epoch;
    test_overlap
  ]
;;

let test_install =
  "install test" >::: [
    test_scenario01;
    test_scenario02;
    test_scenario03;
    test_scenario05;
    test_scenario06;
    test_scenario07;
    test_scenario08;
    test_scenario09;
    test_scenario10;
    test_scenario11;
    test_compare;
  ]

let all = 
  "all tests" >::: [ test_install ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
