
open Common
open OUnit

let returns_result ?(printer=(fun _ -> "(FIXME)")) function_to_test expected_result =
  (fun args () -> assert_equal ~printer (function_to_test args) expected_result)
and raises_failure function_to_test failure_text =
  (fun args () -> assert_raises (Failure failure_text) (fun () -> function_to_test args))

let printer x = Pef.Printer.string_of_vpkgformula x

let concat_ands v =
  if v = [] then "foo" else
  String.concat " , " (
    List.map (fun v ->
      if v = "" then "foo" 
      else Printf.sprintf "foo (%s)" v
    ) v) 

let parse_pef_vpkgformula v =
  Pef.Packages.parse_vpkgformula ("depends", (Format822.dummy_loc,concat_ands v))


let parse_pef_vpkgformula_or v =
  let str vl =
    if vl = [] then "foo" else
    String.concat " | " (List.map (fun v -> concat_ands v) vl)
  in
  Pef.Packages.parse_vpkgformula ("depends", (Format822.dummy_loc,str v))

let test_parse_basic =
  let function_to_test v =
    let str s = Printf.sprintf "\"foo\" : \"%s\"" s in
    Packages.parse_depend ("depends",(Format822.dummy_loc,str v)) in
  let returns result =
    let result = parse_pef_vpkgformula result in
    returns_result ~printer function_to_test result 
  in
  returns, [
    ("1.2.3",["= 1.2.3"]);
    (" 1.2.3 ",["= 1.2.3"]);
    (" 2.2.3-4 ",["= 2.2.3-4"]);
    (" 3.2.3-pre ",["= 3.2.3-pre"]);
    ("v5.2.3", ["= 5.2.3"]);
    (" v8.2.3 ",["= 8.2.3"]);
    ("\t13.2.3",["= 13.2.3"]);
    ("=21.2.3",["= 21.2.3"]);
    ("v=34.2.3",["= 34.2.3"]);
    ("*",[">= 0.0.0"]);
    ("0.0.1",["= 0.0.1"]);
    ("== 0.5.1",["= 0.5.1"]);
    ("= 0.5.1",["= 0.5.1"]);
    ("*.*.*",[">= 0.0.0"]);
  ]
;;

let test_parse_caret =
  let function_to_test v =
    let str s = Printf.sprintf "\"foo\" : \"%s\"" s in
    Packages.parse_depend ("depends",(Format822.dummy_loc,str v)) in
  let returns result v =
    let result = parse_pef_vpkgformula_or result in
    returns_result ~printer function_to_test result v
  in
  returns, [
    ("^1.2",[[">= 1.2.0"; "< 2.0.0"]]);
    ("^1.2.x",[[">=1.2.0"; "< 2.0.0"]]);
    ("^0.0.x",[[">=0.0.0"; "< 0.1.0"]]);
    ("^0.0",[[">=0.0.0"; "< 0.1.0"]]);
    ("^1.1.*",[[">= 1.1.0";"< 2.0.0"]]);
    ("^*", [[">= 0.0.0"]]);
  ]

let test_parse_ranges =
  let function_to_test v =
    let str s = Printf.sprintf "\"foo\" : \"%s\"" s in
    Packages.parse_depend ("depends",(Format822.dummy_loc,str v)) in
  let returns result =
    let result = parse_pef_vpkgformula result in
    returns_result ~printer function_to_test result 
  in
  returns, [
    ("1.x",[">= 1.0.0";"< 2.0.0"]);
    ("0.x",[">= 0.0.0";"< 1.0.0"]);
    ("x",[">= 0.0.0"]);
    ("1",[">= 1.0.0";"< 2.0.0"]);
    ("0.1",[">= 0.1.0";"< 0.2.0"]);
    ("0.1.",[">= 0.1.0";"< 0.2.0"]);
    ("0.",[">= 0.0.0";"< 1.0.0"]);
    ("x.0.0",[">= 0.0.0"]);
    ("X.0.0",[">= 0.0.0"]);
    ("2.X.X",[">= 2.0.0";"< 3.0.0"]);
    ("0.1.X",[">= 0.1.0";"< 0.2.0"]);
    ("0.1.x",[">= 0.1.0";"< 0.2.0"]);
    ("0.1.*",[">= 0.1.0";"< 0.2.0"]);
    ("2.*.*",[">= 2.0.0";"< 3.0.0"]);
  ]

let test_parse_tilde =
  let function_to_test v =
    let str s = Printf.sprintf "\"foo\" : \"%s\"" s in
    Packages.parse_depend ("depends",(Format822.dummy_loc,str v)) in
  let returns result =
    let result = parse_pef_vpkgformula result in
    returns_result ~printer function_to_test result 
  in
  returns, [
    ("~1.2.3",[">=1.2.3";"<1.3.0"]);
    ("~1.2",[">=1.2.0";"<1.3.0"]);
    ("~1",[">=1.0.0";"<2.0.0"]);
    ("~0.2.3",[">=0.2.3";"<0.3.0"]);
    ("~0.2",[">=0.2.0";"<0.3.0"]);
    ("~0",[">=0.0.0";"<1.0.0"]);
    ("~1.2.3-beta.2",[">=1.2.3-beta.2";"<1.3.0 "]);
    ("~1.1.*",[">= 1.1.0";"< 1.2.0"]);
  ]

let test_parse_simplelist =
  let function_to_test v =
    let str s = Printf.sprintf "\"foo\" : \"%s\"" s in
    Packages.parse_depend ("depends",(Format822.dummy_loc,str v)) in
  let returns result v =
    let result = parse_pef_vpkgformula result in
    returns_result ~printer function_to_test result v
  in
  returns, [
    (">1.2.0 <1.2.3",["> 1.2.0";"< 1.2.3"]);
  ]

let test_parse_orlist =
  let function_to_test v =
    let str s = Printf.sprintf "\"foo\" : \"%s\"" s in
    Packages.parse_depend ("depends",(Format822.dummy_loc,str v)) in
  let returns result v =
    let result = parse_pef_vpkgformula_or result in
    returns_result ~printer function_to_test result v
  in
  returns, [
    (">=1.2.7 <2.0.0",[[">= 1.2.7"; "< 2.0.0"]]);
    ("1.2.7 || 2.0.0",[["= 1.2.7"];["= 2.0.0"]]);
    ("1.2.7 || <2.0.0",[["= 1.2.7"];["< 2.0.0"]]);
    ("1.2.7 || >=1.2.9 || <2.0.0",[["= 1.2.7"];[">= 1.2.9"]; ["< 2.0.0"]]);
    ("1.2.7 || >=1.2.9 <2.0.0",[["= 1.2.7"];[">= 1.2.9"; "= 1.2.7"]; ["< 2.0.0"]]);
    ("1.2.7",[["= 1.2.7"]]);
    ("1.2.7 || >=1.2.9",[["= 1.2.7"];[">= 1.2.9"]]);
    ("1.2.7 >=1.2.9",[["= 1.2.7"; ">= 1.2.9"]]);
    (">= 0.1.x",[[">= 0.1.0";]]);
    (">= 0.9.*",[[">= 0.9.0";]]);
  ]


let make_test_cases_parse (assert_function,triplets) =
  List.map ( fun (v,result) -> v >:: assert_function result v) triplets


let suite = 
  "suite" >::: [ 
    "test basic " >::: make_test_cases_parse test_parse_basic;
    "test tilde" >::: make_test_cases_parse test_parse_tilde;
    "test caret" >::: make_test_cases_parse test_parse_caret;
    "test ranges" >::: make_test_cases_parse test_parse_ranges;
    "test simplelist" >::: make_test_cases_parse test_parse_simplelist;
    "test orlist" >::: make_test_cases_parse test_parse_orlist;
  ]

let main () = OUnit.run_test_tt_main suite ;;
main ()
