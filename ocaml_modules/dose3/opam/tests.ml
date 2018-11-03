
open ExtLib
open OUnit
open Common
module Version = Versioning.Debian

let returns_result ?(printer=(fun _ -> "(PRINTER NOT SPECIFIED)")) function_to_test expected_result =
  (fun args () -> assert_equal ~printer (function_to_test args) expected_result)
and raises_failure function_to_test failure_text =
  (fun args () -> assert_raises (Failure failure_text) (fun () -> function_to_test args) )

let parse_depends =
  let function_to_test options par =
    let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepsformula in
    Opam.Packages.vpkgformula_filter options (f "depends" par)
  in
  let returns options = returns_result (function_to_test options) in
  [
    ("depends simple", ["depends",(Common.Format822.dummy_loc,"a")], returns ("system",[],[]) ([[("a",None),None]]));
    ("depends filter arch", ["depends",(Common.Format822.dummy_loc,"a [arch]")], returns ("arch",[],[]) ([[("a",None),None]]));
    ("depends filter arch neg", ["depends",(Common.Format822.dummy_loc,"a [arch]")], returns ("system",[],[]) ([]));
  ]

let version_lag =
  let (request,packagelist) = Opam.Packages.input_raw "tests/opam/version_lag.opam" in
  let options = {
    Opam.Opamcudf.default_options with 
    Opam.Opamcudf.switch = request.Opam.Packages.switch } 
  in
  let universe = Opam.Opamcudf.load_universe ~options Version.compare packagelist in
  let pkgA1 = Cudf.lookup_package universe ("A%3asystem",1) in
  let pkgA2 = Cudf.lookup_package universe ("A%3asystem",2) in
  let pkgA3 = Cudf.lookup_package universe ("A%3asystem",3) in
  let pkgA4 = Cudf.lookup_package universe ("A%3asystem",4) in
  assert_equal "3" (Cudf.lookup_package_property pkgA1 "version-lag");
  assert_equal "2" (Cudf.lookup_package_property pkgA2 "version-lag");
  assert_equal "1" (Cudf.lookup_package_property pkgA3 "version-lag");
  assert_equal "0" (Cudf.lookup_package_property pkgA4 "version-lag");
;;

module NV = struct
  type t = (Cudf_types.pkgname * Cudf_types.version)
  let compare = compare
  let pp_printer fmt (n,v) = Format.fprintf fmt "(\"%s\",%d)" n v
  let pp_print_sep fmt () = Format.fprintf fmt ";"
end

module ListNV = OUnitDiff.ListSimpleMake(NV);;

let returns_result_list function_to_test expected_result =
  (fun args () -> ListNV.assert_equal (function_to_test args) expected_result)

let opamcudf_filter =
  let (_,packagelist) = Opam.Packages.input_raw "tests/opam/filter_universe.opam" in
  let function_to_test (switch,switches) =
    let options = {
      Opam.Opamcudf.default_options with 
      Opam.Opamcudf.switch = switch; switches } 
    in
    let l = Opam.Opamcudf.load_list ~options Version.compare packagelist in
    List.map (fun p -> (CudfAdd.decode p.Cudf.package,p.Cudf.version)) l
  in
  let returns = returns_result_list function_to_test in
  [
    ("load switch:sw1, switches:[]", ("sw1",[]), returns 
      [("f:sw1",1); ("e:sw1",1); ("d:sw1",1); ("c:sw1",1); ("a:sw1",1)]);
    ("load switch:sw2, switches:[]", ("sw2",[]), returns
      [("f:sw2",1); ("e:sw2",1); ("d:sw2",1); ("c:sw2",1); ("b:sw2",1)]);
    ("load switch:sw1, switches:[sw2]", ("sw1",["sw2"]), returns 
      [("f:sw2",1); ("f:sw1",1); ("e:sw2",1); ("e:sw1",1); ("d:sw2",1); ("d:sw1",1);
        ("c:sw2",1); ("c:sw1",1); ("b:sw2",1); ("a:sw1",1)] ) ;
    ("load switch:sw2, switches:[sw1]", ("sw2",["sw1"]), returns 
      [("f:sw1",1); ("f:sw2",1); ("e:sw1",1); ("e:sw2",1); ("d:sw1",1); ("d:sw2",1);
        ("c:sw1",1); ("c:sw2",1); ("b:sw2",1); ("a:sw1",1)] );
  ]

let opamcudf_installed =
  let (_,packagelist) = Opam.Packages.input_raw "tests/opam/filter_universe.opam" in
  let function_to_test (switch,switches) =
    let options = {
      Opam.Opamcudf.default_options with 
      Opam.Opamcudf.switch = switch; switches } 
    in
    let l = Opam.Opamcudf.load_list ~options Version.compare packagelist in
    List.filter_map (fun p ->
      if p.Cudf.installed then 
        Some(CudfAdd.decode p.Cudf.package,p.Cudf.version) 
      else None
    ) l
  in
  let returns = returns_result_list function_to_test in
  [
    ("installed switch:sw1", ("sw1",[]), returns 
      [("d:sw1",1);("c:sw1",1)]);
    ("installed switch:sw2, switches:[]", ("sw2",[]), returns
      [("c:sw2",1)]);
    ("installed switch:sw1, switches:[sw2]", ("sw1",["sw2"]), returns
      [("d:sw1",1);("c:sw2",1);("c:sw1",1)])
  ]

module NVK = struct
  type t = (Cudf_types.pkgname * Cudf_types.version * Cudf_types.enum_keep)
  let compare = compare
  let pp_printer fmt (n,v,k) =
    Format.fprintf fmt "(\"%s\",%d,%s)" n v (Cudf_types_pp.string_of_keep k)
  let pp_print_sep fmt () = Format.fprintf fmt ";"
end

module ListNVK = OUnitDiff.ListSimpleMake(NVK);;

let returns_result_nvk function_to_test expected_result =
  (fun args () -> ListNVK.assert_equal (function_to_test args) expected_result)

let opamcudf_pinned =
  let (_,packagelist) = Opam.Packages.input_raw "tests/opam/filter_universe.opam" in
  let function_to_test (switch,switches) =
    let options = {
      Opam.Opamcudf.default_options with 
      Opam.Opamcudf.switch = switch; switches } 
    in
    let l = Opam.Opamcudf.load_list ~options Version.compare packagelist in
    List.filter_map (fun p ->
      if p.Cudf.keep = `Keep_version || p.Cudf.keep = `Keep_package then 
        Some(CudfAdd.decode p.Cudf.package,p.Cudf.version,p.Cudf.keep) 
      else None
    ) l
  in
  let returns = returns_result_nvk function_to_test in
  [
    ("pinned switch:sw1", ("sw1",[]), returns 
      [("e:sw1",1,`Keep_version)]);
    ("pinned switch:sw2, switches:[]", ("sw2",[]), returns
      [("f:sw2",1,`Keep_version);("e:sw2",1,`Keep_package)]);
    ("pinned switch:sw1, switches:[sw2]", ("sw1",["sw2"]), returns
      [("f:sw2",1,`Keep_version);("e:sw2",1,`Keep_package);("e:sw1",1,`Keep_version)])
  ]

let make_test_cases triplets =
  List.map ( fun (test_name, input, assert_function) -> test_name >:: assert_function input ) triplets

let all =
  "all tests" >::: [
    "test depends" >::: make_test_cases parse_depends;
    "test filters" >::: make_test_cases opamcudf_filter;
    "test installed" >::: make_test_cases opamcudf_installed;
    "test pinned" >::: make_test_cases opamcudf_pinned;
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()

