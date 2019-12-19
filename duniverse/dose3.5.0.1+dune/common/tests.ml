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

module Pcre = Re_pcre

open OUnit
open Common

let returns_result ?(printer=(fun _ -> "(PRINTER NOT SPECIFIED)")) function_to_test expected_result =
  (fun args () -> assert_equal ~printer (function_to_test args) expected_result)
and raises_failure function_to_test failure_text =
  (fun args () -> assert_raises (Failure failure_text) (fun () -> function_to_test args) )

let test_dir = "tests/common"
let cudf_dir = "tests/cudf"
let f_legacy = Filename.concat cudf_dir "legacy.cudf"

let test_deb_local =
  "deb local" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "deb://Packages.gz" 
    in assert_equal true (protocol = `Deb && path = "Packages.gz")
  ) 
;;

let test_deb_path =
  "deb path" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "deb:///var/lib/Packages.gz" 
    in assert_equal true (protocol = `Deb && path = "/var/lib/Packages.gz")
  )
;;

let test_hdlist = 
  "hdlist" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "hdlist://path/to/file" 
    in assert_equal true (protocol = `Hdlist && path = "path/to/file")
  )
;;

let test_synth =
  "synthesis" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "synthesis://path/to/file" 
    in assert_equal true (protocol = `Synthesis && path = "path/to/file")
  )
;;

let test_cudf = 
  "cudf" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "cudf://path/to/file" 
    in assert_equal true (protocol = `Cudf && path = "path/to/file")
  )
;;

let parse_uri =
  "parse uri" >::: [
    test_deb_local;
    test_deb_path;
    test_hdlist;
    test_synth;
    test_cudf;
  ]

module S = CudfAdd.Cudf_set

let (universe,request) =
  let (_,pkglist,request) = Cudf_parser.parse_from_file f_legacy in
  (Cudf.load_universe pkglist,Option.get request)

let toset l =
  List.fold_right (fun e set ->
    S.add (Cudf.lookup_package universe e) set
  ) l S.empty

let engine_provides_set = S.empty
let engine_conflicts_set = S.empty
let electric_engine2 = Cudf.lookup_package universe ("electric-engine",2) 

let test_who_conflicts =
  "who_conflict" >:: (fun _ ->
    let conflicts = CudfAdd.init_conflicts universe in
    let l = CudfAdd.who_conflicts conflicts universe electric_engine2 in
    let engine_conflicts_set = toset 
      [ ("electric-engine",1);
        ("gasoline-engine",1);
        ("gasoline-engine",2); ]
    in
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal engine_conflicts_set set)
  )

let cone_cases =
  let open Cudf in
  let printer xs =
    List.fold_left (fun acc x -> Printf.sprintf "%s, %s" acc x.package) "" xs
  in
  let univ_ls = Cudf.fold_packages (fun acc x -> x :: acc) [] universe in
  let cone input = CudfAdd.cone universe input in
  let pedal_ls = [Cudf.package_by_uid universe 3] in
  let engine_ls = [Cudf.package_by_uid universe 8] in
  let engine_cone = [Cudf.package_by_uid universe 18; Cudf.package_by_uid universe 8] in
  [
    ("cone universe", univ_ls, (fun c () -> assert_equal (List.length (cone c)) (List.length univ_ls)));
    ("cone pedal", pedal_ls, (fun c () -> assert_equal (cone c) pedal_ls ~printer:printer));
    ("cone engine", engine_ls, (fun c () -> assert_equal (cone c) engine_cone ~printer:printer));
  ]


let criteria_parse =
  let function_to_test crt = Criteria.parse_criteria ("test",(Format822.dummy_loc,crt)) in
  let printer s = Criteria.to_string s in
  let returns = returns_result ~printer function_to_test in
  [
    ("parse upgrade", "-count(new),-count(removed),-notuptodate(solution)", returns 
      (List.assoc "upgrade" Criteria.default_criteria));
    ("parse trendy", "-count(removed),-notuptodate(solution),-unsat_recommends(solution),-count(new)", returns 
      (List.assoc "trendy" Criteria.default_criteria));
    ("parse count exact", "-count(solution,APT-Release:=/experimental/)", returns 
      Criteria_types.([Minimize(Count(Solution,Some("APT-Release",ExactMatch "experimental")))]));
    ("parse count regexp", "-count(solution,APT-Release:~/stable|unstable/)", returns 
      Criteria_types.([Minimize(Count(Solution,Some("APT-Release",Regexp "stable|unstable")))]))
  ]

let test_who_provides =
  "who_provides" >:: (fun _ ->
    let l = CudfAdd.who_provides universe ("electric-engine",None) in
    let engine_provides_set = toset 
      [ ("electric-engine",1);
        ("electric-engine",2); ]
    in
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal engine_provides_set set)
  )

let test_who_provides_versioned =
  "who_provides versioned" >:: (fun _ ->
    let l = CudfAdd.who_provides universe ("electric-engine",Some(`Eq, 1)) in
    let engine_provides_set = toset 
      [ ("electric-engine",1) ]
    in
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal engine_provides_set set)
  )

let test_lookup_packages =
  "look up packages" >::: [
    test_who_conflicts;
    test_who_provides;
    test_who_provides_versioned;
  ]

let (test_encode, test_decode) =
  (* Some useful very long strings for testing encoding and decoding. *)
(*  let a_lot_of =  (* a huge number *)
    (Pcre.config_match_limit + 111)
  in
  *)
  let a_lot_of_a           = String.make 10000 'a'
  (* This test takes too much time... 
  and a_lot_of_pipes       = String.make a_lot_of '|' in
  let a_lot_of_hexed_pipes = ExtLib.String.replace_chars (fun _ -> "%7c") a_lot_of_pipes *)
  in
  (* List of triplets: (test_name, decoded_string, encoded_string) *)
  let encode_decode_triplets = [
    ("empty", "", "");
    ("single \"allowed\" character", "a", "a");
    ("single \"not allowed\" character", "|", "%7c");
    ("single percent character", "%", "%25");
    ("several \"allowed\" characters", "abcdef", "abcdef");
    ("several \"not allowed\" characters", "[_|?]", "%5b%5f%7c%3f%5d");
    ("several percent characters", "%%%%%%", "%25%25%25%25%25%25");
    ("several mixed characters", "a[b_c|d?e]f", "a%5bb%5fc%7cd%3fe%5df");
    ("all ASCII characters in range 32-126 (i.e. normal)",
     " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~",
     "%20%21%22%23%24%25%26%27()%2a+%2c-./0123456789%3a%3b%3c%3d%3e%3f@ABCDEFGHIJKLMNOPQRSTUVWXYZ%5b%5c%5d%5e%5f%60abcdefghijklmnopqrstuvwxyz%7b%7c%7d%7e");
    
    ("all ASCII characters",
     "\000\001\002\003\004\005\006\007\b\t\n\011\012\r\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255",
     "%00%01%02%03%04%05%06%07%08%09%0a%0b%0c%0d%0e%0f%10%11%12%13%14%15%16%17%18%19%1a%1b%1c%1d%1e%1f%20%21%22%23%24%25%26%27()%2a+%2c-./0123456789%3a%3b%3c%3d%3e%3f@ABCDEFGHIJKLMNOPQRSTUVWXYZ%5b%5c%5d%5e%5f%60abcdefghijklmnopqrstuvwxyz%7b%7c%7d%7e%7f%80%81%82%83%84%85%86%87%88%89%8a%8b%8c%8d%8e%8f%90%91%92%93%94%95%96%97%98%99%9a%9b%9c%9d%9e%9f%a0%a1%a2%a3%a4%a5%a6%a7%a8%a9%aa%ab%ac%ad%ae%af%b0%b1%b2%b3%b4%b5%b6%b7%b8%b9%ba%bb%bc%bd%be%bf%c0%c1%c2%c3%c4%c5%c6%c7%c8%c9%ca%cb%cc%cd%ce%cf%d0%d1%d2%d3%d4%d5%d6%d7%d8%d9%da%db%dc%dd%de%df%e0%e1%e2%e3%e4%e5%e6%e7%e8%e9%ea%eb%ec%ed%ee%ef%f0%f1%f2%f3%f4%f5%f6%f7%f8%f9%fa%fb%fc%fd%fe%ff");
    
    ("several characters out of range 32-126 (i.e. not usual)", "\031\127\213", "%1f%7f%d5");
    ("huge string of \"allowed\" characters", a_lot_of_a, a_lot_of_a);
    (* This test takes too much time... 
    ("huge string of \"not allowed\" characters", a_lot_of_pipes, a_lot_of_hexed_pipes); *)
    ("path", "/bin/bash__", "/bin/bash%5f%5f")
  ]
  in
  (* From each triplet we generate two test cases, one for
     the function encode and one for the function decode. *)
  let (encode_tests_cases, decode_tests_cases) =
    List.split (
      List.map (fun (test_name, decoded_string, encoded_string) -> 
          ("encoding " ^ test_name) >:: (fun _ -> 
            assert_equal 
              (CudfAdd.encode decoded_string) 
              encoded_string
              ~msg:("\ndecoded_string is        = " ^ decoded_string ^ 
                    "\nencoded_string is        = " ^ (CudfAdd.encode decoded_string) ^
                    "\nencoded_string should be = " ^ encoded_string)
              ),
          
          ("decoding " ^ test_name) >:: (fun _ ->
            assert_equal
              (CudfAdd.decode encoded_string)
              decoded_string
              ~msg:("\nencoded_string is        = " ^ encoded_string ^ 
                    "\ndecoded_string is        = " ^ (CudfAdd.decode encoded_string) ^
                    "\ndecoded_string should be = " ^ decoded_string)
              )
      ) encode_decode_triplets
    )
  in
  (* We have two test suites: one for testing encoding and one for testing decoding. *)
  ("name mangling encoding" >::: encode_tests_cases,
   "name mangling decoding" >::: decode_tests_cases)
;;

(*

  let test_realversionmap

*)

let test_shell_lexer =
  let test_triplets = [
    ("simple", "foo bar baz", ["foo"; "bar"; "baz"]);
    ("simple quoted", "\"foo\" 'bar'", ["foo"; "bar"]);
    ("simple escaped", "foo\\ bar foo\\\\bar foo\\\"bar foo\\'bar", ["foo bar"; "foo\\bar"; "foo\"bar"; "foo'bar"]);
    ("composed", "foo\"bar\"'baz'", ["foobarbaz"]);
    ("dquote escaped", "\"foo\\\"bar\"", ["foo\"bar"]);
    ("uquote starts with escape", "\\\"foo \\'bar \\ baz \\\\blub", ["\"foo"; "'bar"; " baz"; "\\blub"]);
    ("dquote starts with escape", "\"\\\"foo\" \"\\\\bar\"", ["\"foo"; "\\bar"]);
    ("uquote ends with escape", "foo\\\" bar\\' baz\\  blub\\\\", ["foo\""; "bar'"; "baz "; "blub\\"]);
    ("dquote ends with escape", "\"foo\\\"\" \"bar\\\\\"", ["foo\""; "bar\\"]);
    ("compose uquote dquote", "foo\"bar\"baz", ["foobarbaz"]);
    ("compose uquote squote", "foo'bar'baz", ["foobarbaz"]);
    ("compose dquote squote", "\"foo\"'bar'\"baz\"", ["foobarbaz"]);
    ("empty", "\"\" ''", [""; ""]);
    ("compose empty", "\"\"''\"\"\"\"''''", [""]);
    ("multispace", "foo    bar   \t   baz", ["foo"; "bar"; "baz"]);
  ] in
  "lexing tests" >::: (
    List.map (fun (test_name, exec_string, argv) ->
        ("shell_lexer " ^ test_name) >:: (fun _ ->
            let l = Shell_lexer.parse_string exec_string in
            assert_equal (List.length l) (List.length argv)
              ~msg:(Printf.sprintf
                      "Expected argument vector of length %d but got one of length %d"
                      (List.length argv) (List.length l));
            List.iter2 (assert_equal
              ~msg:(Printf.sprintf
                      "String to parse:\n\t%s\nExpected:\n\t%s\nBut got:\n\t%s\n"
                      exec_string (String.concat "\n\t" argv)
                      (String.concat "\n\t" l))) l argv
          )
      ) test_triplets
  )

let criteria_to_string =
  let function_to_test crt = 
    Criteria.to_string (List.assoc crt Criteria.default_criteria) in
  let printer s = s in
  let returns = returns_result ~printer function_to_test in
  [
    ("to_string upgrade", "upgrade", returns "-count(new),-count(removed),-notuptodate(solution)");
    ("to_string trendy", "trendy", returns "-count(removed),-notuptodate(solution),-unsat_recommends(solution),-count(new)");
  ]

let criteria_parse =
  let function_to_test crt = Criteria.parse_criteria ("test",(Format822.dummy_loc,crt)) in
  let printer s = Criteria.to_string s in
  let returns = returns_result ~printer function_to_test in
  [
    ("parse upgrade", "-count(new),-count(removed),-notuptodate(solution)", returns 
      (List.assoc "upgrade" Criteria.default_criteria));
    ("parse trendy", "-count(removed),-notuptodate(solution),-unsat_recommends(solution),-count(new)", returns 
      (List.assoc "trendy" Criteria.default_criteria));
    ("parse count exact", "-count(solution,APT-Release:=/experimental/)", returns 
      Criteria_types.([Minimize(Count(Solution,Some("APT-Release",ExactMatch "experimental")))]));
    ("parse count regexp", "-count(solution,APT-Release:~/stable|unstable/)", returns 
      Criteria_types.([Minimize(Count(Solution,Some("APT-Release",Regexp "stable|unstable")))]))
  ]

let dummy name version =
  {Cudf.default_package with
   Cudf.package = name;
   version = version;
  }

module PkgSetTest = OUnitDiff.SetMake (struct
  type t = Cudf.package
  let compare = CudfAdd.compare
  let pp_printer = CudfAdd.pp_package 
  let pp_print_sep = OUnitDiff.pp_comma_separator
end)

let returns_result_pkgset function_to_test expected_result =
  (fun args () -> PkgSetTest.assert_equal (function_to_test args) expected_result)

let latest_cases =
  let a1,a2,a3,a4 = dummy "a" 1, dummy "a" 2, dummy "a" 3, dummy "a" 4 in
  let b4,b2 = dummy "b" 4, dummy "b" 2 in
  let univ = [a1;a2;a3;a4;b4;b2] in
  let returns = returns_result_pkgset (fun u -> PkgSetTest.of_list (CudfAdd.latest u)) in
  let returns_nth n = returns_result_pkgset (fun u -> PkgSetTest.of_list (CudfAdd.latest ~n u)) in
  [
    ("latest", univ, returns (PkgSetTest.of_list [a4;b4])) ;
    ("latest 3", univ, returns_nth 3 (PkgSetTest.of_list [a4;a2;a3;b4;b2])) ;
    ("latest 2", univ, returns_nth 2 (PkgSetTest.of_list [a3;a4;b4;b2])) ;
  ]
  
let make_test_cases triplets =
  List.map ( fun (test_name, input, assert_function) -> test_name >:: assert_function input ) triplets

let all = 
  "all tests" >::: [
    parse_uri ;
    test_encode;
    test_decode;
    test_lookup_packages;
    test_shell_lexer;
    "criteria" >::: make_test_cases criteria_to_string;
    "criteria" >::: make_test_cases criteria_parse;
    "cone" >::: make_test_cases cone_cases;
    "cudfadd" >::: make_test_cases latest_cases;
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
