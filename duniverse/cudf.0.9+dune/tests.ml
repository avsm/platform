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

open ExtLib
open OUnit
open Printf

open Cudf_types
open Cudf

let cudf_test_path name = sprintf "./tests/%s.cudf" name

let good_cudfs = [	(* CUDF whose parsing must suceed *)
  "empty-vpkglist-default" ;
  "empty-vpkglist-explicit" ;
  "comment-within-stanza" ;
  "comment-at-eof" ;
  "only-request" ;
]
let bad_cudfs = [	(* CUDF whose parsing must fail (@ location) *)
  "line-111", (111, 111) ;
  "miss-mandatory-prop", (9, 9) ;
  "default-type-check-error", (2, 2) ;
  "dup-property", (5, 6) ;
  "missing-stanza-sep-1", (5, 6) ;
  "missing-stanza-sep-2", (3, 3) ;
]
let consistent_univs = [	(* CUDF whose status is expected to be consistent *)
  "assert-true" ;
]
let inconsistent_univs = [	(* CUDF whose status has some broken dep *)
  "assert-false" ;
]
let good_pkgs = [	(* universes whose parsing must suceed *)
  "conflict-comma-sep" ;
  "plus-in-pkgname" ;
  "empty" ;
]
let bad_pkgs = [	(* universes whose parsing must fail (@ location) *)
  "multiline-error", (93, 95) ;
]
let good_prob_sol = [	(* pairs cudf/sol, with sol being a good solution *)
  "legacy", "legacy-sol" ;
  "fresher", "fresher-sol-good" ;
  "upgrade-singleton", "upgrade-singleton-sol-good" ;
  "keep", "keep-sol-good";
  "virt-upgrade", "virt-upgrade-sol-good";
  "keep-uninst", "keep-uninst-good";
]
let bad_prob_sol = [	(* pairs cudf/sol, with sol being a bad solution *)
  "fresher", "fresher-sol-bad" ;
  "upgrade-singleton", "upgrade-singleton-sol-bad" ;
  "keep", "keep-sol-bad";
  "keep", "keep-sol-bad2";
  "keep", "keep-sol-bad3";
  "virt-upgrade", "virt-upgrade-sol-bad";
  "virt-upgrade", "virt-upgrade-sol-bad2";
  "virt-upgrade", "virt-upgrade-sol-bad3";
  "unknown-pkg", "unknown-pkg.sol";
]

(** {5 Helpers} *)

(** {6 OUnit helpers}
    i.e., missing stuff which should better be integrated into OUnit *)

let assert_no_exn f = assert_equal true (try f () ; true with _ -> false)
let assert_exn f = assert_equal true (try f () ; false with _ -> true)

let assert_raises' ?(cmp = (=)) ~exn f =
  assert_equal true (try f () ; false with exn' -> cmp exn exn')

(** {6 CUDF helpers} *)

let parse_test ~parse_fun name =
  let ic = open_in (cudf_test_path name) in
  let p = Cudf_parser.from_in_channel ic in
  let out = parse_fun p in
  close_in ic;
  out

let parse_cudf_wrapper p =
  match Cudf_parser.parse p with
    | pre, pkgs, Some req -> pre, pkgs, req
    | pre, pkgs, None -> raise (Cudf_parser.Parse_error ("", dummy_loc))
let parse_pkgs_wrapper p =
  match Cudf_parser.parse p with
    | pre, pkgs, Some req -> raise (Cudf_parser.Parse_error ("", dummy_loc))
    | pre, pkgs, None -> pkgs
let load_cudf_wrapper p =
  match Cudf_parser.load p with
    | pre, pkgs, Some req -> pre, pkgs, req
    | pre, pkgs, None -> raise (Cudf_parser.Parse_error ("", dummy_loc))
let load_pkgs_wrapper p =
  match Cudf_parser.load p with
    | pre, pkgs, Some req -> raise (Cudf_parser.Parse_error ("", dummy_loc))
    | pre, pkgs, None -> pkgs

let parse_cudf_test = parse_test ~parse_fun:parse_cudf_wrapper
let parse_pkgs_test = parse_test ~parse_fun:parse_pkgs_wrapper
let load_cudf_test = parse_test ~parse_fun:load_cudf_wrapper
let load_univ_test = parse_test ~parse_fun:load_pkgs_wrapper

(** {!OUnit.assert_equal} proxy for {!Cudf_types.typed_value} values *)
let assert_equal_tyval ?cmp ?pp_diff ?msg expected real =
  let printer = Cudf_types_pp.string_of_value in
  OUnit.assert_equal ?cmp ~printer ?pp_diff ?msg expected real

let assert_equal_string ?cmp ?pp_diff ?msg expected real =
  let printer s = s in
  OUnit.assert_equal ?cmp ~printer ?pp_diff ?msg expected real

let string_of_int_list l =
  sprintf "[%s]" (String.concat ";" (List.map string_of_int l))

(** {5 Test builders} *)

let good_parse ~parse_fun name = TestCase (fun _ ->
  assert_no_exn (fun () -> parse_test ~parse_fun name))

let bad_parse ~parse_fun name (l1, l2) = TestCase (fun _ ->
  assert_raises'
    ~cmp:(fun e1 e2 ->
	    match e1, e2 with
	      | Cudf_parser.Parse_error _,
		Cudf_parser.Parse_error (_msg, (loc1, loc2)) ->
		  loc1.Lexing.pos_lnum = l1 && loc2.Lexing.pos_lnum = l2
	      | _ -> false)
    ~exn:(Cudf_parser.Parse_error ("", dummy_loc))
    (fun () -> parse_test ~parse_fun name))

let good_solution prob_name sol_name = TestCase (fun _ ->
  let (_,univ,req), sol = load_cudf_test prob_name, load_univ_test sol_name in
    sprintf "problem with correct solution: (%s,%s)" prob_name sol_name @?
    fst (Cudf_checker.is_solution (univ,req) sol))

let bad_solution prob_name sol_name = TestCase (fun _ ->
  let (_,univ,req), sol = load_cudf_test prob_name, load_univ_test sol_name in
    sprintf "problem with correct solution: (%s,%s)" prob_name sol_name @?
    not (fst (Cudf_checker.is_solution (univ,req) sol)))

(** {5 Test suites} *)

(** {6 Big suites} *)

let good_cudf_parse_suite =
  "parsing of good CUDFs" >::: List.map
      (fun n -> n >: good_parse ~parse_fun:parse_cudf_wrapper n)
      good_cudfs

let bad_cudf_parse_suite =
  "parsing of bad CUDFs" >::: List.map
      (fun (n, (l1, l2)) -> n >:
	 bad_parse ~parse_fun:parse_cudf_wrapper n (l1, l2))
      bad_cudfs

let good_pkgs_parse_suite =
  "parsing of good package universes" >::: List.map
      (fun n -> n >: good_parse ~parse_fun:parse_pkgs_wrapper n)
      good_pkgs

let bad_pkgs_parse_suite =
  "parsing of bad package universes" >::: List.map
      (fun (n, (l1, l2)) -> n >:
	 bad_parse ~parse_fun:parse_pkgs_wrapper n (l1,l2))
      bad_pkgs

(** {6 Regression tests} *)

let value_parse_suite =
  let value_parse_ok (desc, typ, s, v) = desc >: TestCase (fun _ ->
    assert_equal_tyval (Cudf_types_pp.parse_value typ s) v) in
  let value_parse_ko (desc, typ, s) = desc >: TestCase (fun _ ->
    assert_raises'
      ~cmp:(fun e1 e2 ->
	      match e1, e2 with
		| Cudf_types_pp.Type_error _, Cudf_types_pp.Type_error _ -> true
		| _ -> e1 = e2)
      ~exn:(Cudf_types_pp.Type_error (`Int, `Int ~-1))
      (fun () -> Cudf_types_pp.parse_value typ s))
  in
  "value parsing" >::: [
    "good" >::: List.map value_parse_ok [
      "int 1", `Int, "1", `Int 1 ;
      "int -1", `Int, "-1", `Int ~-1 ;
      "posint", `Posint, "1", `Posint 1 ;
      "nat 0", `Nat, "0", `Nat 0 ;
      "bool true", `Bool, "true", `Bool true ;
      "bool false", `Bool, "false", `Bool false ;
      "string", `String, "asfkjg 1%!@$% aaa", `String "asfkjg 1%!@$% aaa" ;
      "pkgname", `Pkgname, "foo", `Pkgname "foo" ;
      "pkgname /", `Pkgname, "/bin/bash", `Pkgname "/bin/bash" ;
      "pkgname @", `Pkgname, "libfoo@bar", `Pkgname "libfoo@bar" ;
      "pkgname ()", `Pkgname, "libfoo(bar)", `Pkgname "libfoo(bar)" ;
      "ident", `Ident, "foo", `Ident "foo" ;
      "ident -", `Ident, "foo-bar", `Ident "foo-bar" ;
      "ident num", `Ident, "foo12", `Ident "foo12" ;
      "enum", `Enum ["foo";"bar";"baz"], "foo",
        `Enum(["foo";"bar";"baz"], "foo") ;
      "keep", keep_type, "feature", `Enum (keep_enums, "feature") ;
      "vpkg dumb", `Vpkg, "foo", `Vpkg ("foo", None) ;
      "vpkg", `Vpkg, "foo > 1", `Vpkg ("foo", Some (`Gt, 1)) ;
      "fmla vpkg", `Vpkgformula, "foo", `Vpkgformula [["foo", None]] ;
      "fmla true", `Vpkgformula, "true!", `Vpkgformula [] ;
      "fmla false", `Vpkgformula, "false!", `Vpkgformula [ [] ] ;
      "fmla and", `Vpkgformula, "foo, bar > 1",
        `Vpkgformula [ ["foo", None] ; ["bar", Some (`Gt, 1)] ] ;
      "fmla or", `Vpkgformula, "foo < 7 | bar",
        `Vpkgformula [ ["foo", Some (`Lt, 7) ; "bar", None] ] ;
      "fmla cnf", `Vpkgformula, "foo | bar, quux | baz | sup",
        `Vpkgformula [ ["foo",None; "bar",None] ;
		       ["quux",None; "baz",None; "sup",None] ] ; 
      "vpkgs nil", `Vpkglist, "", `Vpkglist [] ;
      "vpkgs one", `Vpkglist, "foo", `Vpkglist ["foo", None] ;
      "vpkgs cons", `Vpkglist, "foo != 1, bar",
        `Vpkglist [ "foo", Some (`Neq, 1) ; "bar", None ] ;
      "veqpkg", `Veqpkg, "foo = 7", `Veqpkg ("foo", Some (`Eq, 7)) ;
      "veqpkgs", `Veqpkglist, "foo = 7", `Veqpkglist [("foo", Some (`Eq, 7))] ;
      "typedecl", `Typedecl, "foo: vpkgformula = [ foo, bar | baz ]",
        `Typedecl ["foo",
		   `Vpkgformula (Some [["foo",None];["bar",None;"baz",None]])] ;
      "typedecls", `Typedecl, "foo: int, bar: string = [\"baz quux\"]",
        `Typedecl ["foo", `Int None ; "bar", `String (Some "baz quux")] ;
      "typedecl enum", `Typedecl, "p: enum[a,b,c]",
        `Typedecl ["p", `Enum (["a"; "b"; "c"], None)] ;
      "typedecl enum def", `Typedecl, "p: enum[a,b,c] = [a]",
        `Typedecl ["p", `Enum (["a"; "b"; "c"], Some "a")] ;
      "typedecl pkgname ident", `Typedecl, "p: pkgname = [ pkg ]",
        `Typedecl ["p", `Pkgname (Some "pkg")] ;
      "typedecl pkgname int", `Typedecl, "p: pkgname = [ 1 ]",
        `Typedecl ["p", `Pkgname (Some "1")] ;
      "typedecl vpkg ident", `Typedecl, "p: vpkg = [ pkg ]",
        `Typedecl ["p", `Vpkg (Some ("pkg", None))] ;
      "typedecl vpkg int", `Typedecl, "p: vpkg = [ 1 ]",
        `Typedecl ["p", `Vpkg (Some ("1", None))] ;
      "typedecl veqpkg int", `Typedecl, "p: veqpkg = [ 1 ]",
        `Typedecl ["p", `Veqpkg (Some ("1", None))] ;
      "typedecl vpkgs ident", `Typedecl, "l: vpkglist = [ pkg ]",
        `Typedecl ["l", `Vpkglist (Some [("pkg", None)])] ;
      "typedecl vpkgs int", `Typedecl, "l: vpkglist = [ 1 ]",
        `Typedecl ["l", `Vpkglist (Some [("1", None)])] ;
      "typedecl veqpkgs int", `Typedecl, "l: veqpkglist = [ 1 ]",
        `Typedecl ["l", `Veqpkglist (Some [("1", None)])] ;
    ] ;
    "bad" >::: List.map value_parse_ko [
      "int garbage", `Int, "78 gotcha" ;
      "posint neg", `Posint, "-1" ;
      "posint zero", `Posint, "0" ;
      "nat neg", `Nat, "-1" ;
      "bool", `Bool, "xxx" ;
      "bool", `Bool, "foo" ;
      "bool garbage", `Bool, "true gotcha" ;
      "string \\n", `String, "foo\nbar" ;
      "string \\r", `String, "foo\rbar" ;
      "pkgname !", `Pkgname, "foo!bar" ;
      "pkgname !", `Pkgname, "foo!bar" ;
      "ident numstart", `Ident, "12foo" ;
      "ident caps", `Ident, "foAo" ;
      "ident symb", `Ident, "fo/o" ;
      "enum", `Enum ["foo"], "bar" ;
      "keep", keep_type, "foo" ;
      "empty fmla", `Vpkgformula, "" ;
      "vpkg garbage", `Vpkg, "foo > 1 gotcha" ;
      "vpkgs trail", `Vpkglist, "foo ," ;
      "veqpkg", `Veqpkg, "foo > 1" ;
      "enum bad def", `Typedecl, "p: enum[a,b,c] = [z]" ;
    ] ;
  ]

let property_access_suite =
  let pre, univ, req = load_cudf_test "legacy" in
  let pre = Option.get pre in
  let pkg = lookup_package univ ("gasoline-engine", 1) in
  let pre_ty (p, v) =
    p >:: (fun _ -> assert_equal_tyval v (lookup_typed_preamble_property pre p)) in
  let pkg_ty (p, v) =
    p >:: (fun _ -> assert_equal_tyval v (lookup_typed_package_property pkg p)) in
  let req_ty (p, v) =
    p >:: (fun _ -> assert_equal_tyval v (lookup_typed_request_property req p)) in
  let pre_raw (p, v) =
    p >:: (fun _ -> assert_equal_string v (lookup_preamble_property pre p)) in
  let pkg_raw (p, v) =
    p >:: (fun _ -> assert_equal_string v (lookup_package_property pkg p)) in
  let req_raw (p, v) =
    p >:: (fun _ -> assert_equal_string v (lookup_request_property req p)) in
  "property access" >::: [
    "raw" >::: [
      "preamble" >::: List.map pre_raw [
	"univ-checksum", "8c6d8b4d0cf7027cd523ad095d6408b4901ac31c";
	"status-checksum", "6936ce910eb716ad97190393f80c14ab04d95b3d";
	"req-checksum", "17259225eaf63642f9ab99a627b9857a5b27c5f7";
      ] ;
      "package" >::: List.map pkg_raw [
	"package", "gasoline-engine";
	"version", "1";
	"depends", "turbo";
	"provides", "engine";
	"conflicts", "engine , gasoline-engine";
	"installed", "true";
      ] ;
      "request" >::: List.map req_raw [
	"request",
	"http://www.example.org/8f46e388-042f-415e-8aab-df4eeb974444.dudf";
	"install", "bicycle , electric-engine = 1";
	"upgrade", "door , wheel > 2";
      ] ;
    ] ;
    "typed" >::: [
      "preamble" >::: List.map pre_ty [
	"univ-checksum", (`String "8c6d8b4d0cf7027cd523ad095d6408b4901ac31c");
	"status-checksum", (`String "6936ce910eb716ad97190393f80c14ab04d95b3d");
	"req-checksum", (`String "17259225eaf63642f9ab99a627b9857a5b27c5f7");
      ] ;
      "package" >::: List.map pkg_ty [
	"package", (`Pkgname "gasoline-engine");
	"version", (`Posint 1);
	"depends", (`Vpkgformula [["turbo", None]]);
	"provides", (`Veqpkglist ["engine", None]);
	"conflicts", (`Vpkglist ["engine", None; "gasoline-engine", None]);
	"installed", (`Bool true);
      ] ;
      "request" >::: List.map req_ty [
	"request",
	(`String "http://www.example.org/8f46e388-042f-415e-8aab-df4eeb974444.dudf");
	"install", (`Vpkglist ["bicycle", None;
			       "electric-engine", Some (`Eq, 1)]);
	"upgrade", (`Vpkglist ["door", None;
			       "wheel", Some (`Gt, 2)]);
      ] ;
    ]
  ]

let value_pp_suite =
  let value_pp_ok (desc, v, s) = desc >: TestCase (fun _ ->
    assert_equal_string (Cudf_types_pp.string_of_value v) s) in
  let decl_pp_ok (desc, v, s) = desc >: TestCase (fun _ ->
    assert_equal_string (Cudf_types_pp.string_of_typedecl v) s) in
  "value pretty printing" >::: [
    "good value" >::: List.map value_pp_ok [
    ] ;
    "good decl" >::: List.map decl_pp_ok [
      "default string",
        ["source", `String (Some "")],
        "source: string = [\"\"]" ;
      "default string escape",
        ["source", `String (Some "\"")],
        "source: string = [\"\\\"\"]" ;
    ] ;
    "bad vpkgformula" >:: (fun () ->
      assert_exn (fun () -> (* should "assert false" *)
	Cudf_types_pp.string_of_vpkgformula [ []; [] ]))
  ]

let cudf_pp_suite =
  (** check that the pretty printing roundtrip (parse -> pp -> parse) returns
      the same document than plain parsing *)
  let cudf_pp_roundtrip name =
    name >: TestCase (fun _ ->
      let (pre, univ, req) as doc = parse_cudf_test name in
      let fname, oc = Filename.open_temp_file "libcudf-test." ".cudf" in
      finally
	(fun () -> Sys.remove fname)
	(fun () ->
	  Cudf_printer.pp_doc oc doc;
	  close_out oc;
	  let doc' = Cudf_parser.parse_from_file fname in
	  let doc = pre, univ, Some req in
	  assert_equal doc doc')
	())
  in
  (** check that pretty printing of a document returns some canonical pretty
      printed document (hence, this test is more fragile than
      [cudf_pp_roundtrip] above) *)
  let cudf_pp_canonical name =
    name >: TestCase (fun _ ->
      let (pre, univ, req) as doc = parse_cudf_test name in
      let fname, oc = Filename.open_temp_file "libcudf-test." ".cudf" in
      finally
	(fun () -> Sys.remove fname)
	(fun () ->
	  let expected_pp_file = cudf_test_path (sprintf "%s.pp" name) in
	  let expected_pp = input_file expected_pp_file in
	  Cudf_printer.pp_doc oc doc;
	  close_out oc;
	  let actual_pp = input_file fname in
	  if expected_pp != actual_pp then
	    ignore (Sys.command (sprintf "diff %s %s" expected_pp_file fname));
	  assert_equal expected_pp actual_pp)
	())
  in
  "cudf pretty printing" >::: [
    "roundtrip" >::: List.map cudf_pp_roundtrip [
      "legacy"
    ];
    "canonical" >::: List.map cudf_pp_canonical [
      "legacy"
    ];
  ]

let misc_parse_suite =
  "misc parsing" >::: [
    "qstring" >::: [
      "base" >:: (fun () ->
        assert_equal_string (Cudf_types_pp.parse_qstring "\"foo\"") "foo") ;
      "escape \"" >:: (fun () ->
        assert_equal_string (Cudf_types_pp.parse_qstring "\"fo\\\"o\"") "fo\"o") ;
      "escape \\" >:: (fun () ->
        assert_equal_string (Cudf_types_pp.parse_qstring "\"fo\\\\o\"") "fo\\o") ;
      "dangling \"" >:: (fun () -> "unexpected parse success" @?
        (try ignore (Cudf_types_pp.parse_qstring "\"fo\"o\"") ; false
	 with _ -> true)) ;
      "typename ident" >:: (fun () ->
        assert_equal ~printer:Cudf_types_pp.string_of_type
	  (Cudf_types_pp.parse_type "ident") `Ident) ;
    ];
    "pkg comparison" >::: [
      "=%" >:: (fun () ->
        let pkg1 = { default_package with installed = true } in
        let pkg2 = { default_package with installed = false } in
	assert_equal ~cmp:(=%) pkg1 pkg2);
      "<%" >:: (fun () ->
        let pkg1 = { default_package with installed = true } in
        let pkg2 = { default_package with installed = false } in
	let l1 = [ pkg1 ; pkg2 ] in
	let l2 = [ pkg2 ; pkg1 ] in
	let rec pkgs_eq pkgs1 pkgs2 =
	  match pkgs1, pkgs2 with
	  | [], [] -> true
	  | p1::t1, p2::t2 -> (p1 =% p2) && pkgs_eq t1 t2
	  | _ -> assert false in
        assert_equal ~cmp:pkgs_eq
	  (List.sort ~cmp:(<%) l1)
	  (List.sort ~cmp:(<%) l2));
    ];
  ]

let or_dep =
  "disjunctive dependencies" >:: (fun () ->
    assert_equal ~printer:Cudf_types_pp.string_of_vpkgformula
      (lookup_package (load_univ_test "or-dep") ("electric-engine", 1)).depends
      [["solar-collector", None; "huge-battery", None]])

let parse_reg_suite =
  "regression tests - parsing" >::: [
    or_dep ;
  ]

(** {6 New feature tests}
    i.e., kinda test-driven development *)

let status_filtering =
  "status projection" >:: (fun () ->
    "status projection returned an \"installed: false\" package" @?
      let _, univ, _ = load_cudf_test "legacy" in
      List.for_all
        (fun { installed = i } -> i)
        (get_packages (status univ)))

let status_sizes =
  "status projection size" >:: (fun _ ->
    let _, univ, _ = load_cudf_test "legacy" in
    let status = status univ in
    let assert_equal' x y = assert_equal ~printer:string_of_int x y in
    assert_equal' 6 (universe_size status);
    assert_equal' 6 (installed_size status);
    assert_equal' (installed_size univ) (installed_size status))

let inst_version_lookup =
  "lookup installed versions" >:: (fun () ->
    let univ = load_univ_test "multi-versions" in
    let versions pkg = List.map (fun p -> p.version) (get_installed univ pkg) in
      assert_equal ~printer:string_of_int_list
	(List.sort (versions "gasoline-engine")) [1; 2];
      assert_equal ~printer:string_of_int_list (versions "battery") [3];
      assert_equal ~printer:string_of_int_list (versions "not-installed") [];
      assert_equal ~printer:string_of_int_list (versions "not-existent") [])

let mem_installed =
  "check whether an installation satisfy a package constraint" >:: (fun () ->
    let _, univ, _ = load_cudf_test "legacy" in
    let mem = mem_installed ~include_features:true univ in
    let mem' = mem_installed ~include_features:false univ in
      "'car' unsatisfied" @? mem ("car", None);
      "'car = 1' unsatisfied" @? mem ("car", Some (`Eq, 1));
      "'car > 1' satisfied'" @? not (mem ("car", Some (`Gt, 1)));
      "'car >= 1' unsatisfied" @? mem ("car", Some (`Leq, 1));
      "'engine' unsatisfied w features" @? mem ("engine", None);
      "'engine' satisfied w/o features" @? not (mem' ("engine", None));
  )

let mem_package =
  "Cudf.mem_package" >:: (fun () ->
    let _, univ, _ = load_cudf_test "legacy" in
    let mem = mem_package univ in
      "<car,1> available" @? mem ("car", 1);
      "<car,2> unavailable" @? not (mem ("car", 2));
      "<bicycle,7> available" @? mem ("bicycle", 7);
      "<bicycle,8> unavailable" @? not (mem ("bicycle", 8));
      "<zuff,1> unavailable" @? not (mem ("zuff", 1));
  )

let satisfy_formula =
  "check formula satisfaction" >:: (fun () ->
    let _, univ, _ = load_cudf_test "legacy" in
    let sat f = fst (Cudf_checker.satisfy_formula univ f) in
      "true unsatisfied (WTF?)" @? sat [];
      "conjunction unsatisfied" @? sat [["battery", None]; ["wheel", None]];
      "disjunction unsatisfied" @?
	sat [["solar-collector", None; "wheel", None]];
      "unsat formula satisfied" @?
	not (sat [["wheel", Some (`Gt, 2); "tire", None]]);
  )

let disjoint =
  "check package disjunction (i.e., conflicts)" >:: (fun () ->
    let _, univ, _ = load_cudf_test "legacy" in
    let disj ps = fst (Cudf_checker.disjoint univ ps) in
      "missing package reported as existing" @? disj ["fubar", None];
      "undetected conflict" @? not (disj ["door", Some (`Eq, 1)]);
      "undetected partial conflict" @?
	not (disj ["door", Some (`Gt, 1); "turbo", None]);
  )

let self_conflicts =
  "check self-conflicts" >:: (fun () ->
    let consist u = fst (Cudf_checker.is_consistent u) in
      "direct self-conflict" @? consist (load_univ_test "direct-self-conflict");
      "indirect self-conflict" @?
	consist (load_univ_test "indirect-self-conflict"))

let consistency =
  "check universe consistency" >::: [
    "legacy example consistency" >:: (fun () ->
      let _, univ, _ = load_cudf_test "legacy" in
	"inconsistent legacy example" @? fst (Cudf_checker.is_consistent univ))
  ]

let univ_sizes =
  let univ = lazy (let _, univ, _ = load_cudf_test "legacy" in univ) in
    "check universe size measuring" >::: [
      "total size" >::
	(fun () -> assert_equal ~printer:string_of_int
	  (universe_size (Lazy.force univ)) 20);
      "installed size" >::
	(fun () -> assert_equal ~printer:string_of_int
	  (installed_size (Lazy.force univ)) 6);
    ]

let univ_manipulation =
  let univ = (let _, univ, _ = load_cudf_test "legacy" in univ) in
  let car = lookup_package univ ("car", 1) in
  "universe manipulations" >::: [
    "remove package" >::
      (fun () ->
        remove_package univ ("car", 1);
        assert_equal false (Cudf.mem_package univ ("car", 1))
      );
    "add package" >::
      (fun () ->
        add_package univ car;
        assert_equal car (lookup_package univ ("car", 1))
      );
  ]

let default_value =
  let univ = lazy (let _, univ, _ = load_cudf_test "legacy" in univ) in
    "default value of opt prop" >::: [
      "bugs" >::
	(fun () ->
	  let car = lookup_package (Lazy.force univ) ("car", 1) in
	  let bugs = List.assoc "bugs" car.pkg_extra in
	  assert_equal_tyval (`Int 0) bugs)
    ]

let good_solution_suite = "good solutions" >:::
  List.map (fun (prob, sol) -> good_solution prob sol) good_prob_sol

let bad_solution_suite = "bad solutions" >:::
  List.map (fun (prob, sol) -> bad_solution prob sol) bad_prob_sol

let consistency_suite = "consistent universes" >:::
  List.map
    (fun u -> TestCase (fun () -> "consistent" @?
       fst (Cudf_checker.is_consistent (load_univ_test u))))
    consistent_univs

let inconsistency_suite = "inconsistent universes" >:::
  List.map
    (fun u -> TestCase (fun () -> "inconsistent" @?
       not (fst (Cudf_checker.is_consistent (load_univ_test u)))))
    inconsistent_univs

let typedecl_lookup = "type declaration lookup" >:::
  let pre, _univ, _req = load_cudf_test "legacy" in
  let pre = Option.get pre in
  let test_typedecl (prop, typedecl1) =
    prop >:: (fun _ ->
      assert_equal ~printer:Std.dump (* TODO use/write a better printer *)
	typedecl1
	(lookup_package_typedecl ~extra:pre.property prop)) in
  List.map
    test_typedecl
    [ "package", `Pkgname None;
      "version", `Posint None;
      "depends", `Vpkgformula (Some []);
      "conflicts", `Vpkglist (Some []);
      "provides", `Veqpkglist (Some []);
      "installed", `Bool (Some false);
      "was-installed", `Bool (Some false);
      "keep", `Enum (keep_enums, Some "none");
      "suite", `Enum (["stable"; "testing"; "unstable"], Some "stable");
      "bugs", `Int (Some 0);
      "description", `String (Some "no description");
    ]

(** {6 Test suites} *)

let feature_suite =
  "new feature tests" >::: [
    status_filtering ;
    status_sizes ;
    inst_version_lookup ;
    mem_installed ;
    mem_package ;
    satisfy_formula ;
    disjoint ;
    self_conflicts ;
    consistency ;
    univ_sizes ;
    univ_manipulation ;
    default_value ;
    typedecl_lookup ;
  ]

(*
let test_encode =
  "encode" >:: (fun () ->
    let s = "@/bin/*-+" in
    assert_equal (Cudf_types.encode s) "@/bin/%2a-+"
  )
;;

let test_decode =
  "encode" >:: (fun () ->
    let s = "@/bin/%2a-+" in
    assert_equal (Cudf_types.decode s) "@/bin/*-+"
  )
;;

let encoding_suite =
  "encoding / decoding tests" >::: [
    test_encode;
    test_decode
  ]
*)

(** {5 Assemble and run tests} *)

let all =
  "all tests" >::: [
    value_parse_suite ;
    value_pp_suite ;
    property_access_suite ;
    cudf_pp_suite ;
    misc_parse_suite ;
    good_cudf_parse_suite ;
    bad_cudf_parse_suite ;
    good_pkgs_parse_suite ;
    bad_pkgs_parse_suite ;
    consistency_suite ;
    inconsistency_suite ;
    good_solution_suite ;
    bad_solution_suite ;
    parse_reg_suite ;
    feature_suite ;
    (* encoding_suite ; *)
  ]

