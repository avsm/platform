(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2011 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                            *)
(*  Contributions 2011 Ralf Treinen <ralf.treinen@pps.jussieu.fr>             *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

open OUnit
open Common
module Version = Versioning.Debian

let test_dir = "tests"

let f_packages = Filename.concat test_dir "DebianPackages/Packages.bz2" ;;
let f_release = Filename.concat test_dir "DebianPackages/Release" ;;
let f_discriminants = Filename.concat test_dir "deb/discriminants" ;;
let f_essential = Filename.concat test_dir "deb/essential" ;;

(* Useful test functions *)

let returns_result ?(printer=(fun _ -> "(FIXME)")) function_to_test expected_result =
  (fun args () -> assert_equal ~printer (function_to_test args) expected_result)
and raises_failure function_to_test failure_text =
  (fun args () -> assert_raises (Failure failure_text) (fun () -> function_to_test args) )

(* let ch = Input.open_file f_packages ;; *)
(* Extension of "bracket_tmpfile" function, filling
    the temporary file with lines from the given list. *)
let bracket_tmpfile_filled lines (test_fun : string -> unit)  =
  bracket_tmpfile
    (fun (file, ch) ->
      List.iter (fun line ->
        output_string ch (line ^ "\n")
      ) lines;
      close_out ch;
      test_fun file
    )
;;

(* Makes a list of test cases from a list of triplets:
    (test_name, string_to_parse, assert_function) *)
let make_test_cases triplets =
  List.map (fun (test_name, input, assert_function) ->
    test_name >:: assert_function input
  ) triplets



(* XXX TODO:
  * add test for default arch
  * add test for extras
  * add test for parsing errors
  * add test for Ingore Packages
  * add test for status and merge
  * *)

let extras_properties = [
  ("Maintainer", ("maintainer", `String None));
  ("Size", ("size", `Nat None));
  ("Installed-Size", ("installedsize", `Nat None))
];;

let extras = List.map fst extras_properties ;;
let options = { Debcudf.default_options with Debcudf.extras_opt = extras_properties } ;;

let packagelist = Packages.input_raw [f_packages] ;;
let tables = Debcudf.init_tables packagelist ;;
let cudf_list = List.map (Debcudf.tocudf tables ~options) packagelist ;; 
let universe = Cudf.load_universe cudf_list ;;

(* version comparison ****************************************************************************)

let version_test_cases = [
  ("1.2-5.6","3.4-5.8",-1);      (* easy *)
  ("1:2-3","2:2-3",-1);          (* period comparison - equal *)
  ("1:2-3","1:2-3",0);           (* period comparison - less *)
  ("2:2-3","1:2-3",1);           (* period comparison - greater *)
  ("0:1.2-3","1.2-3",0);         (* period =0 when missing *)
  ("000001:2-3","2:1",-1);       (* leading 0 in period *)
  ("00:1","0000:1",0);           (* leading 0 in period *)
  ("1",":1",0);                  (* epoch separator but no epoch *)
  ("1a","1c",-1);                (* character ordering *)
  ("1z","1A",1);                 (* character ordering *)
  ("1Z","1.",-1);                (* character ordering *)
  ("1.","1-",1);                 (* character ordering *)
  ("1-","1+",-1);                (* character ordering *)
  ("1~~","1~~a",-1);             (* tilde - example from policy *)
  ("1~~a","1~",-1);              (* tilde - example from policy *)
  ("1~","1",-1);                 (* tilde - example from policy *)
  ("1","1a",-1);                 (* tilde - example from policy *)
  ("000","00",0);                (* numerical comparison - zeros *)
  ("1a000","1a",0);              (* empty string in numerical part counts as 0 *)
  ("1-000","1",0);               (* empty string in numerical part counts as 0 *)
  ("1.23","1.23~",1);            (* tilde after numerical part *)
  ("1.2+a.3","1.2+a.3",0);       (* alternating lexical and numerical *)
  ("1.2+a.3","1.2+aa.3",1);      (* alternating lexical and numerical *)
  ("1.2+a.3","1.2+a~.3",1);      (* alternating lexical and numerical *)
  ("05","000001",1);             (* skiping leading zeros *)
  ("1a","1a00000~",1);
  ("2:1","3:1",-1);              (* hierarchy of parts *)
  ("2:1.1.1","2:1.1.2",-1);      (* hierarchy of parts *)
  ("2:1.1-1.1","2:1.1-1.2",-1);  (* hierarchy of parts *)
];;

let dpkg_compare x y =
  let c1 = Printf.sprintf "dpkg --compare-versions %s lt %s" x y in
  let c2 = Printf.sprintf "dpkg --compare-versions %s eq %s" x y in
  if (Sys.command c1) = 0 then -1
  else if (Sys.command c2) = 0 then 0
  else 1
;;

let test_version_comparison = 
  "debian version comparison" >::: [
    "" >:: (fun _ ->
      (* we might want to execute these tests also on a non debian machine *)
      let debian_machine = ref true in
      List.iter (fun (v1,v2,res) ->
        let dose_cmp = Version.compare v1 v2 in
        let dpkg_cmp = if !debian_machine then dpkg_compare v1 v2 else res in
        if dose_cmp <> dpkg_cmp then begin
          Printf.eprintf "error version comparison %s %s\n" v1 v2;
          Printf.eprintf "dpkg says %d\n" dpkg_cmp;
          Printf.eprintf "dose says %d\n" dose_cmp
        end;
        assert_equal dose_cmp dpkg_cmp
      ) version_test_cases
    )
  ]
;;

(* architecture matching *****************************************************************)

let architecture_test_cases = [
  ("all", "i386", true);               (* all matches everything *)
  ("any", "kfreebsd-amd64",true);      (* any matches everything *)
  ("amd64", "i386", false);            (* pattern and arch do not split *)
  ("toaster", "toaster", true);        
  ("hurd-amd64", "hurd-amd64", true);  (* pattern and arch split *)
  ("hurd-amd64", "netbsd-amd64", false);   
  ("hurd-amd64", "hurd-i386", false);
  ("hurd-amd64", "netbsd-i386", false);
  ("hurd-amd64", "amd64", false);      (* pattern splits, arch doesn't *)
  ("hurd-amd64", "i386", false);
  ("linux-amd64", "amd64", true);
  ("linux-amd64", "i386", false);
  ("amd64", "hurd-amd64", false);      (* arch splits,patten doesn't *)
  ("amd64", "hurd-i386", false);
  ("amd64", "linux-amd64", true);
  ("amd64", "linux-i386", false);
  ("any-amd64", "hurd-amd64", true);   (* OS pattern *)
  ("any-amd64", "linux-amd64", true);
  ("any-amd64", "hurd-i386", false);
  ("any-amd64", "linux-i386", false);
  ("hurd-any", "hurd-alpha", true);    (* CPU pattern *)
  ("linux-any", "linux-alpha", true);
  ("hurd-any", "netbsd-alpha", false);
  ("linux-any", "netbsd-alpha", false);
  ("any-any", "linux-i386", true);     (* OS and CPU pattern *)
  ("any-any", "hurd-i386", true);
  ("any-any", "amd64", true);
  ("any-arm", "armhf", true);          (* arch name is not equal cpu name *)
  ("any-amd64", "x32", true);
  ("any-powerpc", "powerpcspe", true)
];;

let test_architecture_matching =
  "debian architecture matching" >::: [
    "" >:: (fun _ ->
      List.iter
	(fun (source,arch,expected) ->
	  let result = Architecture.src_matches_arch source arch  in
	  if result <> expected
	  then
	    begin
	      Printf.printf "error matching architecture %s against %s\n" source arch;
	      Printf.printf "found %b, should be %b\n" result expected
	    end;
	  assert_equal result expected
	)
	architecture_test_cases
    )
  ]
;;

(*****************************************************************************************)

let test_version = 
  "debian version parsing" >::: [
    "splitting all" >:: (fun _ ->
      let v = "1:1.4-5+b1" in
      assert_equal (Version.decompose v) (Version.NonNative ("1","1.4","5","b1"))
    );
    "normalize all" >:: (fun _ ->
      let v = "1:1.4-5+b1" in
      assert_equal (Version.strip_epoch_binnmu v) (Version.NonNative ("","1.4","5",""))
    );
    "concat all" >:: (fun _ ->
      let v = "1:1.4-5+b1" in
      assert_equal (Version.compose (Version.decompose v)) v
    );
    "splitting partial 1" >:: (fun _ ->
      let v = "1.4-5+b1" in
      assert_equal (Version.decompose v) (Version.NonNative ("","1.4","5","b1"))
    );
    "normalize partial 1" >:: (fun _ ->
      let v = "1.4-5+b1" in
      assert_equal (Version.strip_epoch_binnmu v) (Version.NonNative ("","1.4","5",""))
    );
    "splitting partial 2" >:: (fun _ ->
      let v = "1.4" in
      assert_equal (Version.decompose v) (Version.Native ("","1.4",""))
    );
    "normalize partial 2" >:: (fun _ ->
      let v = "1.4" in
      assert_equal (Version.strip_epoch_binnmu v) (Version.Native ("","1.4",""))
    );
    "splitting partial 3" >:: (fun _ ->
      let v = "0" in
      assert_equal (Version.decompose v) (Version.Native ("","0",""))
    );
    "normalize partial 3" >:: (fun _ ->
      let v = "0" in
      assert_equal (Version.strip_epoch_binnmu v) (Version.Native ("","0",""))
    );
    "splitting partial 4" >:: (fun _ ->
      let v = "1.1+b6" in
      assert_equal (Version.decompose v) (Version.Native ("","1.1","b6"))
    );
    "normalize partial 4" >:: (fun _ ->
      let v = "1.1+b6" in
      assert_equal (Version.strip_epoch_binnmu v) (Version.Native ("","1.1",""))
    );
  ]
;;

let test_realname =
  let function_to_test = Debcudf.get_real_name in
  let returns = returns_result function_to_test
  in
  [ ("none", "name", returns ("name",None) );
    ("virtual simple", "--virtual-name", returns ("name",None) );
    ("virtual arch", "--virtual-name:arch", returns ("name",Some "arch") );
    ("virtual src", "--virtual-src:name", returns ("name",None) );
  ] 

let string_of_relop = function
  |`Eq -> "="
  |`Neq -> "!="
  |`Geq -> ">="
  |`Gt -> ">"
  |`Leq -> "<="
  |`Lt -> "<"
;;

let rec assert_delay_stub l =
  let acc = ref l in
  fun e ->
    match !acc with
    |[] -> assert_failure "OUnit: not equal"
    |h::tl -> begin
        acc := tl;
        assert_equal e h
    end
;;

module PkgSetTest = OUnitDiff.SetMake (struct
  type t = (string * string)
  let compare = compare
  let pp_printer ppf (p,v) = Format.fprintf ppf "(%s,%s)" p v
  let pp_print_sep = OUnitDiff.pp_comma_separator
end)

module SubClusterSetTest = OUnitDiff.SetMake (struct
  type t = (string * string * PkgSetTest.t)
  let compare = compare
  let pp_printer ppf (p,v,s) = Format.fprintf ppf "(%s,%s,{%a})" p v PkgSetTest.pp_printer s
  let pp_print_sep = OUnitDiff.pp_comma_separator
end)

module ClusterSetTest = OUnitDiff.SetMake (struct
  type t = (string * string * SubClusterSetTest.t)
  let compare = compare
  let pp_printer ppf (p,v,s) = Format.fprintf ppf "(%s,%s,{%a})" p v SubClusterSetTest.pp_printer s
  let pp_print_sep = OUnitDiff.pp_comma_separator
end)


let test_cluster =
  let packagelist = Packages.input_raw [f_discriminants] in
  let clusters = Debutil.cluster packagelist in
  "cluster" >::: [
    "groups" >:: (fun _ -> 
      let of_list ll = 
        ClusterSetTest.of_list (
          List.map (fun (p,v,l) -> (p,v,
            SubClusterSetTest.of_list (
              List.map (fun (v,rv,cl) ->
                (v,rv,PkgSetTest.of_list cl)
              ) l
            ))
          ) ll 
        )
      in
      let expected = of_list [
        (* sourcename, sourceversion, [ list of clusters
         * cluster version (normalized), real version, [list of packages in the cluster]
         * ] 
         *)
        ("bb","1",["1","1",[("bb","1")]]);
        ("aa","1",["1","1",[("aa","1")]]);
        ("ee_source","1",[
          "1","1",[("gg","1");("ee","1")]]);
        ("ee_source","2",["2","2",[("ff","2")]]);
        ("cc_source","1",[
          "1","1",[("dd","1")];
          "2","2",[("cc","2")]
          ]);
        ]
      in
      let result =
        ClusterSetTest.of_list (
          Hashtbl.fold (fun (sn, sv) l acc ->
            (sn,sv,SubClusterSetTest.of_list (
              List.map (fun (v,rv,cluster) -> 
                let cl = List.map(fun pkg -> (pkg#name,pkg#version)) cluster in
                (v,rv,PkgSetTest.of_list cl)
              ) l
            ))::acc
          ) clusters []
        )
      in
      ClusterSetTest.assert_equal expected result
    ); 
  ]
;;

let test_evolution =
  let packagelist = Packages.input_raw [f_discriminants] in
  let constraints_table = Evolution.constraints packagelist in
  (* let clusters = Debutil.cluster packagelist in *)
  "evolution" >::: [
    "constraints" >:: (fun _ ->
      let constr = Evolution.all_constraints constraints_table "cc" in
      (* List.iter (fun (c,v) -> Printf.printf "(%s %s)\n" (string_of_relop c) v ) constr;
       * *)
      assert_equal [(`Eq,"4");(`Lt,"3")] constr
    );
    "constraints empty" >:: (fun _ ->
      let constr = Evolution.all_constraints constraints_table "hh" in
      (*
      List.iter (fun (c,v) -> Printf.printf "(%s %s)\n" (string_of_relop c) v ) constr;
      *)
      assert_equal [] constr
    );
    "versions" >:: (fun _ ->
      let vl = Evolution.all_versions [(`Gt,"3"); (`Eq,"3"); (`Lt,"4")] in
      (* List.iter (Printf.printf "-<< %s <<") vl; *)
      assert_equal ["4";"3"] vl
    );
    "range (1)" >:: (fun _ ->
      let rl = Evolution.range ["3.4";"76"] in
      (* List.iter (fun r -> Printf.printf "%s\n" (Evolution.string_of_range r)) rl; *)
      assert_equal [(`Eq "3.4");(`In ("3.4","76"));(`Eq "76");(`Hi "76")] rl
    );
    "range (2)" >:: (fun _ ->
      let rl = Evolution.range ["1"] in
      (* List.iter (fun r -> Printf.printf "%s\n" (Evolution.string_of_range r)) rl; *)
      assert_equal [(`Eq "1");(`Hi "1")] rl
    );
    "range bottom (1)" >:: (fun _ ->
      let rl = Evolution.range ~bottom:true ["3";"4"] in
      (* List.iter (fun r -> Printf.printf "%s\n" (Evolution.string_of_range r)) rl; *)
      assert_equal [(`Lo "3");(`Eq "3");(`In ("3","4"));(`Eq "4");(`Hi "4")] rl
    );
    "range bottom (2)" >:: (fun _ ->
      let rl = Evolution.range ~bottom:true ["1"] in
      (* List.iter (fun r -> Printf.printf "%s\n" (Evolution.string_of_range r)) rl; *)
      assert_equal [(`Lo "1");(`Eq "1");(`Hi "1")] rl
    );
    (*
    "evalsel" >:: (fun _ ->
      assert_equal false (Evolution.evalsel Version.compare ((`Eq "3.4"),(`Gt,"76")));
      assert_equal false (Evolution.evalsel Version.compare ((`In ("3.4","76")),(`Gt,"76")));
      assert_equal false (Evolution.evalsel Version.compare ((`Eq "76"),(`Gt,"76")));
      (*
      assert_equal true (Evolution.evalsel Version.compare ((`Hi "76"),(`Gt,"76")));
      *)
    );
    "discriminants simple" >:: (fun _ ->
      let assert_delay = assert_delay_stub [ ] in
      let constr = [(`Gt,"76")] in
      let vl = ["3.4";"76"] in
      let discr = Evolution.discriminant evalsel vl constr in
      List.iter (fun (target,equiv) -> 
        Printf.eprintf "(3) %s\n%!" (Evolution.string_of_range target);
        List.iter (fun k ->
          Printf.eprintf "(3) e %s\n%!" (Evolution.string_of_range k);
        ) equiv;
      ) discr;
      List.iter (fun (target,equiv) -> assert_delay (target,equiv)) discr 
    );
    "discriminant (single)" >:: (fun _ ->
      let assert_delay = assert_delay_stub [ (`Lo "1",[`Hi "1"]); (`Eq "1",[]) ] in
      let constr = Evolution.all_constraints constraints_table "bb" in
      let vl = Evolution.all_versions constr in
      let discr = Evolution.discriminant ~bottom:true vl constr in
      (*
      List.iter (fun (target,equiv) -> 
        Printf.eprintf "(3) %s\n%!" (Evolution.string_of_range target);
        List.iter (fun k ->
          Printf.eprintf "(3) e %s\n%!" (Evolution.string_of_range k);
        ) equiv;
      ) discr;
      *)
      List.iter (fun (target,equiv) -> assert_delay (target,equiv)) discr 
    ); 
    "discriminant (cluster)" >:: (fun _ ->
      let assert_delay = 
        assert_delay_stub [
          ("bb","1","1",[(`Eq "1",[]);(`Hi "1",[])]);
          ("aa","1","1",[]);
          ("ee_source","2","2",[]);
          ("ee_source","1","1",[]);
          ("cc_source","1","2",[
            (`Eq "4",[]);
            (`In ("2","3"),[]);
            (`Eq "2",[`Hi "4";`In ("3","4");`Eq "3"]);
            ]);
          ("cc_source","1","1",[
            (`In ("1","3"),[]);
            (`Eq "1",[`Hi "3";`Eq "3"])
            ]);
        ]
      in
      Hashtbl.iter (fun (sourcename, sourceversion) l ->
        Printf.eprintf "(2)cluster (%s,%s)\n%!" sourcename sourceversion; 
        List.iter (fun (version,cluster) ->
          let filter x =
            match Version.split version, Version.split x with
            |(_,v,_,_),(_,w,_,_) -> (Version.compare v w) <= 0
          in
          let l = Evolution.discriminants ~filter constraints_table cluster in
          Printf.eprintf "(2)v : %s\n%!" version;
          List.iter (fun (target,equiv) ->
            Printf.eprintf "(2)d : %s\n%!" (Evolution.string_of_range target);
            List.iter (fun target ->
            Printf.eprintf "(2)d : e %s\n%!" (Evolution.string_of_range target);
            ) equiv;
            Printf.eprintf "(2)d : ----\n%!"
          ) l;
          assert_delay (sourcename,sourceversion,version,l);
        ) l
      ) clusters;
      assert_equal true true
    );
    *)
    "align (with epoch)" >:: (fun _ ->
      let r = Evolution.align "1:3.4+b5" (`In ("3.5","3.6")) in
      assert_equal r (`In ("1:3.5","1:3.6"))
    );
    "align (without epoch 1)" >:: (fun _ ->
      let r = Evolution.align "3.4+b5" (`In ("2:3.5","2:3.6")) in
      assert_equal r (`In ("2:3.5","2:3.6"))
    );
    "align (without epoch 2)" >:: (fun _ ->
      let r = Evolution.align "3.4+b5" (`In ("3.5","3.6")) in
      assert_equal r (`In ("3.5","3.6"))
    );
(*
    "migration" >:: (fun _ ->
      Hashtbl.iter (fun (sourcename, sourceversion) h ->
        Hashtbl.iter (fun version cluster ->
          let migrationlist = Debian.Evolution.migrate cluster (`Lo "1:3") in
          List.iter (fun ((pkg,target),newtarget) -> 
            Printf.eprintf "%s %s\n%!" pkg.Packages.name pkg.Packages.version;
            Printf.eprintf "old %s\n%!" (Evolution.string_of_range target);
            Printf.eprintf "new %s\n%!" (Evolution.string_of_range newtarget)
          ) migrationlist
        ) h
      ) clusters
    );
*)
  ]
;;

let test_multiarch = 
  "test multiarch" >::: [
    "multi arch same provide-conflicts" >:: (fun _ -> 
      (*
      let f = Filename.concat test_dir "deb/edsp/multiarch-same-provides.edsp" in
      let (request,pkglist) = Edsp.input_raw f in
      let tables = Debcudf.init_tables pkglist in
      let cudf_pkglist = List.map (fun pkg -> Edsp.tocudf tables pkg) pkglist in
      let universe = Cudf.load_universe cudf_pkglist in
      let cudf_request = Edsp.requesttocudf tables universe request in
*) ()
    );
  ] 
;;

let test_numbering = 
  "test numbering" >::: [
    "sequence" >:: (fun _ -> 
      try
        let debconf = Cudf.lookup_package universe ("debconf",32) in
        assert_equal debconf.Cudf.version 32
      with Not_found -> assert_failure "debconf version mismatch"
    );
    "get real version" >:: (fun _ -> ());
  ] 
;;

let test_virtual = 
  "test virtual" >::: [
    "provides" >:: (fun _ -> 
      try
        let v = Debcudf.get_cudf_version tables ("ssmtp","2.62-3") in
        let ssmtp = Cudf.lookup_package universe ("ssmtp",v) in
        let vpkg = ("--virtual-mail-transport-agent",None) in
        let provides = CudfAdd.who_provides universe vpkg in
        assert_equal true (List.exists (Cudf.(=%) ssmtp) provides)
      with Not_found -> assert_failure "ssmtp version mismatch"
    );
    "virtual real" >:: (fun _ -> ())
  ]

let test_conflicts =
  "test conflict" >::: [
    "self conflict" >:: (fun _ -> 
      try
        let v = Debcudf.get_cudf_version tables ("ssmtp","2.62-3") in
        let ssmtp = Cudf.lookup_package universe ("ssmtp",v) in
        assert_equal true (List.mem (ssmtp.Cudf.package,None) ssmtp.Cudf.conflicts)
      with Not_found -> assert_failure "ssmtp version mismatch"
    );
  ]

let test_mapping =
  "test deb -> cudf mapping" >::: [
(*    test_numbering ; *)
    test_virtual;
    test_multiarch
  ]

(* Parsing tests *)

(* parse_inst *)

(* List of triplets: (test_name, file_lines, expected_result) *)
let parse_inst_triplets =
  (* The standard expected result. *)
  let result =
  [ (("name1", "version1"), ());
    (("name2", "version2"), ());
    (("name3", "version3"), ());
    (("name4", "version4"), ()) ]
  in
  (* List of triplets. *)
  [ ("simple",
     [ "ii name1 version1";
       "ii name2 version2";
       "ii name3 version3";
       "ii name4 version4" ],
     result);
    ("varied blanks and comments",
     [ "ii name1 version1 blah blah blah";
       "ii 	 name2 	       version2		 blah blah blah";
       "ii   name3    version3  blah blah blah";
       "ii name4                         version4 blah blah blah" ],
     result);
    ("errors with something else than \"ii\" at the beginning",
     [ "ii name1 version1 blah blah blah";
       "jj errname1 errversion1 blah blah blah";
       "ii name2 version2 blah blah blah";
       "ii name3 version3 blah blah blah";
       "kk errname2 errversion2 blah blah blah";
       "ii name4 version4 blah blah blah" ],
     result);
    ("errors with no \"ii\" at all",
     [ "err1";
       "err2 err3";
       "";
       "err4 err5 err6";
       "ii name1 version1";
       "err7 err8";
       "ii name2 version2";
       "";
       "ii name3 version3";
       "ii name4 version4" ],
     result);
    ("varying number of fields",
     [ "";
       "ii";
       "ii errname1";
       "ii name1 version1";
       "ii name2 version2 blah";
       "ii name3 version3 blah blah";
       "ii name4 version4 blah blah blah" ],
     result)
  ]

let list_of_hashtbl ht = 
  List.sort compare (ExtLib.List.of_enum (ExtLib.Hashtbl.enum ht))

let test_parse_inst = 
  let parse_inst_test_cases =
    List.map (fun (test_name, file_lines, expected_result) ->
      test_name >::
      bracket_tmpfile_filled file_lines
	( fun file -> assert_equal
	    (list_of_hashtbl (Apt.parse_inst_from_file file))
	    expected_result )
	) parse_inst_triplets
  in
  "test parse_inst" >::: parse_inst_test_cases


(* parse_popcon *)
let parse_popcon_triplets =
  let function_to_test = Apt.parse_popcon in
  let returns = returns_result function_to_test
  and raises  = raises_failure function_to_test
  in
  [ ("simple",      "123 name1 456",      returns (123, "name1", 456) );
    ("more fields", "123 name1 456 err1", returns (123, "name1", 456) );
    ("wrong int 1", "err1 name1 456",     raises "int_of_string" );
    ("wrong int 2", "123 name1 err2",     raises "int_of_string" )
  ] 

(* parse_pkg_req *)
let parse_pkg_req_triplets =
  let function_to_test = (fun (suite, s) -> Apt.parse_pkg_req suite s) in
  let returns = returns_result ~printer:Pef.Printer.string_of_vpkgreq function_to_test
(*  and raises  = raises_failure function_to_test *)
  in
  [ ("suite name=1.2", 
     ( (Some "suite"), "name=1.2"), 
     returns (None, (("name", None), Some ("=", "1.2")), Some "suite"));

    ("suite +name=1.2", 
     ( (Some "suite"), "+name=1.2"), 
     returns (Some Pef.Packages_types.I, (("name", None), Some ("=", "1.2")), Some "suite"));

    ("suite -name=1.2", 
     ( (Some "suite"), "-name=1.2"), 
     returns (Some Pef.Packages_types.R, (("name", None), Some ("=", "1.2")), Some "suite"));

    ("suite name/suite1", 
     ( (Some "suite"), "name/suite1"), 
     returns (None, (("name", None), None), Some "suite"));

    ("none name/suite1", 
     ( None, "name/suite1"), 
     returns (None, (("name", None), None), Some "suite1"));

    ("none name", 
     ( None, "name"), 
     returns (None, (("name", None), None), None));

    ("none +name", 
     ( None, "+name"), 
     returns (Some Pef.Packages_types.I, (("name", None), None), None));

    ("none -name", 
     ( None, "-name"), 
     returns (Some Pef.Packages_types.R, (("name", None), None), None));

    ("suite name", 
     ( Some "suite", "name"), 
     returns (None, (("name", None), None), Some "suite"));

  ]

(* parse_pref_labels *)
let parse_pref_labels_triplets =
  let function_to_test = Apt.parse_pref_labels in
  let returns = returns_result function_to_test
(*  and raises  = raises_failure function_to_test *)
  in
  [ ("simple single num",      "123",         returns [("v", "123")]);
    ("simple single alpha",    "abc",         returns [("a", "abc")]);
    ("simple pair",            "123=abc",     returns [("123", "abc")]);
    ("complicated single num", "123.456.789", returns [("v", "123.456.789")]);
    ("many nums",              "123,456,789", returns [("v", "123"); ("v", "456"); ("v", "789")]);
    ("many alphas",            "abc,def,ghi", returns [("a", "abc"); ("a", "def"); ("a", "ghi")]);
    ("many pairs",             "1=a,2=b,3=c", returns [("1", "a"); ("2", "b"); ("3", "c")]);
  ]

(* parse_pref_package *)
let parse_pref_package_triplets =
  let function_to_test = (fun s -> Apt.parse_pref_package ("parse_pref_package_triplets ",((),s))) in
  let returns = returns_result function_to_test
(*  and raises  = raises_failure function_to_test *)
  in
  [ ("asterisk 1", "*",          returns Apt.Pref.Star);
    ("asterisk 2", "    *     ", returns Apt.Pref.Star);
    ("name 1",     "name1",      returns (Apt.Pref.Package (Pef.Packages.parse_name ("parse_pref_package_triplets",(Format822.dummy_loc, "name1"))))); ]

(* parse_pin *)
let parse_pin_triplets =
  let function_to_test = (fun s -> Apt.parse_pin ("parse_pin_triplets",((),s))) in
  let returns = returns_result function_to_test
(*  and raises  = raises_failure function_to_test *)
  in
  [ ("release 1", "release name1", returns (Apt.Pref.Release (Apt.parse_pref_labels "name1")));
    ("version 1", "version name1", returns (Apt.Pref.Version "name1"));
    ("origin 1",  "origin name1",  returns (Apt.Pref.Origin "name1")); ]

let test_parsing =
  "test_parsing" >::: [
    test_parse_inst;
    "test parse_popcon"       >::: make_test_cases parse_popcon_triplets;
    "test parse_pkg_req"      >::: make_test_cases parse_pkg_req_triplets;
    "test parse_pref_labels"  >::: make_test_cases parse_pref_labels_triplets;
    "test parse_pref_package" >::: make_test_cases parse_pref_package_triplets;
    "test_parse_pin"          >::: make_test_cases parse_pin_triplets;
  ]

let select_deps =
  let function_to_test = (fun (archs,profile,dep) -> Sources.select archs profile dep) in
  let printer = function None -> "None" | Some s -> s in
  let returns = returns_result ~printer function_to_test in
  (* test architecture restrictions and profile restrictions *)
  (* testname archlist profilelist pkg   archlist           restrictionformula      return *)
  [ ("00",  ("amd64", [],         ("foo", [],                [])),                   returns (Some "foo"));
    ("01",  ("amd64", [],         ("foo", [],                [[(true,"stage1")]])),  returns None);
    ("02",  ("amd64", [],         ("foo", [],                [[(false,"stage1")]])), returns (Some "foo"));
    ("03",  ("amd64", [],         ("foo", [(true,"amd64")],  [])),                   returns (Some "foo"));
    ("04",  ("amd64", [],         ("foo", [(true,"amd64")],  [[(true,"stage1")]])),  returns None);
    ("05",  ("amd64", [],         ("foo", [(true,"amd64")],  [[(false,"stage1")]])), returns (Some "foo"));
    ("06",  ("amd64", [],         ("foo", [(false,"amd64")], [])),                   returns None);
    ("07",  ("amd64", [],         ("foo", [(false,"amd64")], [[(true,"stage1")]])),  returns None);
    ("08",  ("amd64", [],         ("foo", [(false,"amd64")], [[(false,"stage1")]])), returns None);
    ("09",  ("amd64", ["stage1"], ("foo", [],                [])),                   returns (Some "foo"));
    ("10",  ("amd64", ["stage1"], ("foo", [],                [[(true,"stage1")]])),  returns (Some "foo"));
    ("11",  ("amd64", ["stage1"], ("foo", [],                [[(false,"stage1")]])), returns None);
    ("12",  ("amd64", ["stage1"], ("foo", [(true,"amd64")],  [])),                   returns (Some "foo"));
    ("13",  ("amd64", ["stage1"], ("foo", [(true,"amd64")],  [[(true,"stage1")]])),  returns (Some "foo"));
    ("14",  ("amd64", ["stage1"], ("foo", [(true,"amd64")],  [[(false,"stage1")]])), returns None);
    ("15",  ("amd64", ["stage1"], ("foo", [(false,"amd64")], [])),                   returns None);
    ("16",  ("amd64", ["stage1"], ("foo", [(false,"amd64")], [[(true,"stage1")]])),  returns None);
    ("17",  ("amd64", ["stage1"], ("foo", [(false,"amd64")], [[(false,"stage1")]])), returns None);
    ("18",  ("i386",  [],         ("foo", [],                [])),                   returns (Some "foo"));
    ("19",  ("i386",  [],         ("foo", [],                [[(true,"stage1")]])),  returns None);
    ("20",  ("i386",  [],         ("foo", [],                [[(false,"stage1")]])), returns (Some "foo"));
    ("21",  ("i386",  [],         ("foo", [(true,"amd64")],  [])),                   returns None);
    ("22",  ("i386",  [],         ("foo", [(true,"amd64")],  [[(true,"stage1")]])),  returns None);
    ("23",  ("i386",  [],         ("foo", [(true,"amd64")],  [[(false,"stage1")]])), returns None);
    ("24",  ("i386",  [],         ("foo", [(false,"amd64")], [])),                   returns (Some "foo"));
    ("25",  ("i386",  [],         ("foo", [(false,"amd64")], [[(true,"stage1")]])),  returns None);
    ("26",  ("i386",  [],         ("foo", [(false,"amd64")], [[(false,"stage1")]])), returns (Some "foo"));
    ("27",  ("i386",  ["stage1"], ("foo", [],                [])),                   returns (Some "foo"));
    ("28",  ("i386",  ["stage1"], ("foo", [],                [[(true,"stage1")]])),  returns (Some "foo"));
    ("29",  ("i386",  ["stage1"], ("foo", [],                [[(false,"stage1")]])), returns None);
    ("30",  ("i386",  ["stage1"], ("foo", [(true,"amd64")],  [])),                   returns None);
    ("31",  ("i386",  ["stage1"], ("foo", [(true,"amd64")],  [[(true,"stage1")]])),  returns None);
    ("32",  ("i386",  ["stage1"], ("foo", [(true,"amd64")],  [[(false,"stage1")]])), returns None);
    ("33",  ("i386",  ["stage1"], ("foo", [(false,"amd64")], [])),                   returns (Some "foo"));
    ("34",  ("i386",  ["stage1"], ("foo", [(false,"amd64")], [[(true,"stage1")]])),  returns (Some "foo"));
    ("35",  ("i386",  ["stage1"], ("foo", [(false,"amd64")], [[(false,"stage1")]])), returns None);
  (* test architectures restrictions with more than one architecture *)
    ("36",  ("amd64", [], ("foo", [(true, "amd64");(true, "i386")], [])),   returns (Some "foo"));
    ("37",  ("amd64", [], ("foo", [(true, "i386");(true, "amd64")], [])),   returns (Some "foo"));
    ("38",  ("amd64", [], ("foo", [(false, "amd64");(false, "i386")], [])), returns None);
    ("39",  ("amd64", [], ("foo", [(false, "i386");(false, "amd64")], [])), returns None);
  (* test restriction formula with only one restriction list with more than one
   * restriction and more than one profile active at a time. *)
    ("40",  ("amd64", [],                   ("foo", [], [[(false, "stage1")]])),                    returns (Some "foo"));
    ("41",  ("amd64", [],                   ("foo", [], [[(true, "stage1")]])),                     returns None);
    ("42",  ("amd64", [],                   ("foo", [], [[(false, "stage1");(false, "nocheck")]])), returns (Some "foo"));
    ("43",  ("amd64", [],                   ("foo", [], [[(true, "stage1");(true, "nocheck")]])),   returns None);
    ("44",  ("amd64", [],                   ("foo", [], [[(false, "stage1");(true, "nocheck")]])),  returns None);
    ("45",  ("amd64", [],                   ("foo", [], [[(true, "nocheck");(false, "stage1")]])),  returns None);
    ("46",  ("amd64", ["stage1"],           ("foo", [], [[(false, "stage1")]])),                    returns None);
    ("47",  ("amd64", ["stage1"],           ("foo", [], [[(true, "stage1")]])),                     returns (Some "foo"));
    ("48",  ("amd64", ["stage1"],           ("foo", [], [[(false, "stage1");(false, "nocheck")]])), returns None);
    ("49",  ("amd64", ["stage1"],           ("foo", [], [[(true, "stage1");(true, "nocheck")]])),   returns None);
    ("50",  ("amd64", ["stage1"],           ("foo", [], [[(false, "stage1");(true, "nocheck")]])),  returns None);
    ("51",  ("amd64", ["stage1"],           ("foo", [], [[(true, "nocheck");(false, "stage1")]])),  returns None);
    ("52",  ("amd64", ["nocheck"],          ("foo", [], [[(false, "stage1")]])),                    returns (Some "foo"));
    ("53",  ("amd64", ["nocheck"],          ("foo", [], [[(true, "stage1")]])),                     returns None);
    ("54",  ("amd64", ["nocheck"],          ("foo", [], [[(false, "stage1");(false, "nocheck")]])), returns None);
    ("55",  ("amd64", ["nocheck"],          ("foo", [], [[(true, "stage1");(true, "nocheck")]])),   returns None);
    ("56",  ("amd64", ["nocheck"],          ("foo", [], [[(false, "stage1");(true, "nocheck")]])),  returns (Some "foo"));
    ("57",  ("amd64", ["nocheck"],          ("foo", [], [[(true, "nocheck");(false, "stage1")]])),  returns (Some "foo"));
    ("58",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(false, "stage1")]])),                    returns None);
    ("59",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "stage1")]])),                     returns (Some "foo"));
    ("60",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(false, "stage1");(false, "nocheck")]])), returns None);
    ("61",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "stage1");(true, "nocheck")]])),   returns (Some "foo"));
    ("62",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(false, "stage1");(true, "nocheck")]])),  returns None);
    ("63",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "nocheck");(false, "stage1")]])),  returns None);
  (* test restriction formulas with more than one restriction list *)
  (* false condition <nodoc> last *)
    ("64",  ("amd64", [],                   ("foo", [], [[(false, "stage1")]; [(true, "nodoc")]])),                    returns (Some "foo"));
    ("65",  ("amd64", [],                   ("foo", [], [[(true, "stage1")]; [(true, "nodoc")]])),                     returns None);
    ("66",  ("amd64", [],                   ("foo", [], [[(false, "stage1");(false, "nocheck")]; [(true, "nodoc")]])), returns (Some "foo"));
    ("67",  ("amd64", [],                   ("foo", [], [[(true, "stage1");(true, "nocheck")]; [(true, "nodoc")]])),   returns None);
    ("68",  ("amd64", [],                   ("foo", [], [[(false, "stage1");(true, "nocheck")]; [(true, "nodoc")]])),  returns None);
    ("69",  ("amd64", [],                   ("foo", [], [[(true, "nocheck");(false, "stage1")]; [(true, "nodoc")]])),  returns None);
    ("70",  ("amd64", ["stage1"],           ("foo", [], [[(false, "stage1")]; [(true, "nodoc")]])),                    returns None);
    ("71",  ("amd64", ["stage1"],           ("foo", [], [[(true, "stage1")]; [(true, "nodoc")]])),                     returns (Some "foo"));
    ("72",  ("amd64", ["stage1"],           ("foo", [], [[(false, "stage1");(false, "nocheck")]; [(true, "nodoc")]])), returns None);
    ("73",  ("amd64", ["stage1"],           ("foo", [], [[(true, "stage1");(true, "nocheck")]; [(true, "nodoc")]])),   returns None);
    ("74",  ("amd64", ["stage1"],           ("foo", [], [[(false, "stage1");(true, "nocheck")]; [(true, "nodoc")]])),  returns None);
    ("75",  ("amd64", ["stage1"],           ("foo", [], [[(true, "nocheck");(false, "stage1")]; [(true, "nodoc")]])),  returns None);
    ("76",  ("amd64", ["nocheck"],          ("foo", [], [[(false, "stage1")]; [(true, "nodoc")]])),                    returns (Some "foo"));
    ("77",  ("amd64", ["nocheck"],          ("foo", [], [[(true, "stage1")]; [(true, "nodoc")]])),                     returns None);
    ("78",  ("amd64", ["nocheck"],          ("foo", [], [[(false, "stage1");(false, "nocheck")]; [(true, "nodoc")]])), returns None);
    ("79",  ("amd64", ["nocheck"],          ("foo", [], [[(true, "stage1");(true, "nocheck")]; [(true, "nodoc")]])),   returns None);
    ("80",  ("amd64", ["nocheck"],          ("foo", [], [[(false, "stage1");(true, "nocheck")]; [(true, "nodoc")]])),  returns (Some "foo"));
    ("81",  ("amd64", ["nocheck"],          ("foo", [], [[(true, "nocheck");(false, "stage1")]; [(true, "nodoc")]])),  returns (Some "foo"));
    ("82",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(false, "stage1")]; [(true, "nodoc")]])),                    returns None);
    ("83",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "stage1")]; [(true, "nodoc")]])),                     returns (Some "foo"));
    ("84",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(false, "stage1");(false, "nocheck")]; [(true, "nodoc")]])), returns None);
    ("85",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "stage1");(true, "nocheck")]; [(true, "nodoc")]])),   returns (Some "foo"));
    ("86",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(false, "stage1");(true, "nocheck")]; [(true, "nodoc")]])),  returns None);
    ("87",  ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "nocheck");(false, "stage1")]; [(true, "nodoc")]])),  returns None);
  (* false condition <nodoc> first *)
    ("88",  ("amd64", [],                   ("foo", [], [[(true, "nodoc")]; [(false, "stage1")]])),                    returns (Some "foo"));
    ("89",  ("amd64", [],                   ("foo", [], [[(true, "nodoc")]; [(true, "stage1")]])),                     returns None);
    ("90",  ("amd64", [],                   ("foo", [], [[(true, "nodoc")]; [(false, "stage1");(false, "nocheck")]])), returns (Some "foo"));
    ("91",  ("amd64", [],                   ("foo", [], [[(true, "nodoc")]; [(true, "stage1");(true, "nocheck")]])),   returns None);
    ("92",  ("amd64", [],                   ("foo", [], [[(true, "nodoc")]; [(false, "stage1");(true, "nocheck")]])),  returns None);
    ("93",  ("amd64", [],                   ("foo", [], [[(true, "nodoc")]; [(true, "nocheck");(false, "stage1")]])),  returns None);
    ("94",  ("amd64", ["stage1"],           ("foo", [], [[(true, "nodoc")]; [(false, "stage1")]])),                    returns None);
    ("95",  ("amd64", ["stage1"],           ("foo", [], [[(true, "nodoc")]; [(true, "stage1")]])),                     returns (Some "foo"));
    ("96",  ("amd64", ["stage1"],           ("foo", [], [[(true, "nodoc")]; [(false, "stage1");(false, "nocheck")]])), returns None);
    ("97",  ("amd64", ["stage1"],           ("foo", [], [[(true, "nodoc")]; [(true, "stage1");(true, "nocheck")]])),   returns None);
    ("98",  ("amd64", ["stage1"],           ("foo", [], [[(true, "nodoc")]; [(false, "stage1");(true, "nocheck")]])),  returns None);
    ("99",  ("amd64", ["stage1"],           ("foo", [], [[(true, "nodoc")]; [(true, "nocheck");(false, "stage1")]])),  returns None);
    ("100", ("amd64", ["nocheck"],          ("foo", [], [[(true, "nodoc")]; [(false, "stage1")]])),                    returns (Some "foo"));
    ("101", ("amd64", ["nocheck"],          ("foo", [], [[(true, "nodoc")]; [(true, "stage1")]])),                     returns None);
    ("102", ("amd64", ["nocheck"],          ("foo", [], [[(true, "nodoc")]; [(false, "stage1");(false, "nocheck")]])), returns None);
    ("103", ("amd64", ["nocheck"],          ("foo", [], [[(true, "nodoc")]; [(true, "stage1");(true, "nocheck")]])),   returns None);
    ("104", ("amd64", ["nocheck"],          ("foo", [], [[(true, "nodoc")]; [(false, "stage1");(true, "nocheck")]])),  returns (Some "foo"));
    ("105", ("amd64", ["nocheck"],          ("foo", [], [[(true, "nodoc")]; [(true, "nocheck");(false, "stage1")]])),  returns (Some "foo"));
    ("106", ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "nodoc")]; [(false, "stage1")]])),                    returns None);
    ("107", ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "nodoc")]; [(true, "stage1")]])),                     returns (Some "foo"));
    ("108", ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "nodoc")]; [(false, "stage1");(false, "nocheck")]])), returns None);
    ("109", ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "nodoc")]; [(true, "stage1");(true, "nocheck")]])),   returns (Some "foo"));
    ("110", ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "nodoc")]; [(false, "stage1");(true, "nocheck")]])),  returns None);
    ("111", ("amd64", ["stage1";"nocheck"], ("foo", [], [[(true, "nodoc")]; [(true, "nocheck");(false, "stage1")]])),  returns None);
  ]

let test_sources_input = "
Package: source1
Version: 0.1-1
Architecture: any
Build-Depends: bin1, bin2:any, bin3:native

Package: source2
Version: 0.1-1
Architecture: any
Build-Depends: bin1 [amd64] <!stage1>, bin2 | bin3 <stage1>, bin4 [!amd64] <!stage1>

Package: source3
Version: 0.1-1
Architecture: any
Build-Depends: bin1, bin2
Build-Depends-Indep: bin3
"
;;

let test_sources2packages =
  let data = IO.input_string test_sources_input in
  let packagelist = Sources.parse_sources_in "" data in
  let hostarch = "amd64" in
  let buildarch = "amd64" in
  let sources = Sources.sources2packages ~profiles:["stage1"] buildarch hostarch packagelist in
  let function_to_test src =
    let src = List.find (fun s -> s#name = src) sources in
    src#depends
  in
  let returns = returns_result ~printer:Pef.Printer.string_of_vpkgformula function_to_test in
  [
    (
      "any/native", "src:source1", returns [
        [(("build-essential", Some hostarch), None)];
        [(("bin1", None), None)];
        [(("bin2", Some "any"), None)];
        [(("bin3", Some "native"), None)]
      ]
    );
    (
      "stage1", "src:source2", returns [
        [(("build-essential", Some hostarch), None)];
        [
          (("bin2", None), None);
          (("bin3", None), None)
        ]
      ]
    );
    (
      "indep", "src:source3", returns [
        [(("build-essential", Some hostarch), None)];
        [(("bin3",None), None)];
        [(("bin1",None), None)];
        [(("bin2",None), None)]
      ]
    )
  ]
;;

(*
 pkgA provides pkgB | pkgC depends on pkgB | dpkg  | dose3
--------------------+----------------------+-------+-------
      unversioned   |      unversioned     | sat   | sat
      unversioned   |        (= 20)        | unsat | unsat
      unversioned   |        (<= 10)       | unsat | unsat
      unversioned   |        (>= 20)       | unsat | unsat
--------------------+----------------------+-------+-------
        (= 10)      |      unversioned     | sat   | unsat  ***
        (= 10)      |        (= 20)        | unsat | unsat
        (= 10)      |        (<= 10)       | sat   | sat
        (= 10)      |        (>= 20)       | unsat | unsat
--------------------+----------------------+-------+-------
        (= 20)      |      unversioned     | sat   | unsat  ***
        (= 20)      |        (= 20)        | sat   | sat
        (= 20)      |        (<= 10)       | unsat | unsat
        (= 20)      |        (>= 20)       | sat   | sat
--------------------+----------------------+-------+-------
    (= 10) (= 20)   |      unversioned     | sat   | unsat  ***
    (= 10) (= 20)   |        (= 20)        | sat   | sat
    (= 10) (= 20)   |        (<= 10)       | sat   | sat
    (= 10) (= 20)   |        (>= 20)       | sat   | sat


 pkgA provides pkgB | pkgC conflicts with pkgB | dpkg  | dose3
--------------------+--------------------------+-------+-------
      unversioned   |        unversioned       | unsat |  sat   ***
      unversioned   |          (= 20)          | sat   |  sat
      unversioned   |          (<= 10)         | sat   |  sat
      unversioned   |          (>= 20)         | sat   |  sat
--------------------+--------------------------+-------+-------
        (= 10)      |        unversioned       | unsat |  sat   ***
        (= 10)      |          (= 20)          | sat   |  sat
        (= 10)      |          (<= 10)         | unsat |  sat   ***
        (= 10)      |          (>= 20)         | sat   |  sat
--------------------+--------------------------+-------+-------
        (= 20)      |        unversioned       | unsat |  sat   ***
        (= 20)      |          (= 20)          | unsat |  sat   ***
        (= 20)      |          (<= 10)         | sat   |  sat
        (= 20)      |          (>= 20)         | unsat |  sat   ***
--------------------+--------------------------+-------+-------
    (= 10) (= 20)   |        unversioned       | unsat |  sat   ***
    (= 10) (= 20)   |          (= 20)          | unsat |  sat   ***
    (= 10) (= 20)   |          (<= 10)         | unsat |  sat   ***
    (= 10) (= 20)   |          (>= 20)         | unsat |  sat   ***
*)

let test_versioned_provides =
  let unversioned = [(("B",None),None)] in
  let eqversioned10 = [(("B",None),Some("=","10"))] in
  let eqversioned20 = [(("B",None),Some("=","20"))] in
  let leqversioned = [(("B",None),Some("<=","10"))] in
  let geqversioned = [(("B",None),Some(">=","20"))] in
  let function_to_test univ =
    let r = Algo.Depsolver.edos_coinstall univ (Cudf.get_packages univ) in
    Algo.Diagnostic.is_solution r
  in
  let printer r = if r then "unsat" else "sat" in
  let pr_list = Util.string_of_list ~sep:", " Pef.Printer.string_of_vpkg in
  let returns = returns_result ~printer function_to_test in
  let res = 
    List.map (fun (provides,relation,result) ->
      List.map (fun (provides,relation,result,polarity) ->
        let a =
          new Packages.package
            ~name:("Package",Some "A")
            ~version:("Version",Some "1")
            ~architecture:("Architecture",Some "arch1")
            ~provides:("Provides",Some provides)
            ~extras:([],Some [("Type","bin")]) []
        in
        let c =
          if polarity then
            new Packages.package
              ~name:("Package",Some "C")
              ~version:("Version",Some "1")
              ~architecture:("Architecture",Some "arch1")
              ~depends:("Depends",Some [relation])
              ~extras:([],Some [("Type","bin")]) []
          else
            new Packages.package
              ~name:("Package",Some "C")
              ~version:("Version",Some "1")
              ~architecture:("Architecture",Some "arch1")
              ~conflicts:("Conflicts",Some relation)
              ~extras:([],Some [("Type","bin")]) []
        in
        let options = {options with Debcudf.native = Some "arch1" } in
        let tables = Debcudf.init_tables [a;c] in
        let pkga = (Debcudf.tocudf ~options tables a) in
        let pkgc = (Debcudf.tocudf ~options tables c) in
        let univ = Cudf.load_universe [pkga;pkgc] in
        let s =
          let us =
            let o = IO.output_string () in
            Cudf_printer.pp_io_preamble o Debcudf.preamble;
            IO.printf o "\n";
            Cudf_printer.pp_io_universe o univ;
            IO.close_out o
          in
          Printf.sprintf "\nA Provides:%s / C %s:%s\n%s"
          (pr_list provides)
          (if polarity then "Depends" else "Conflicts")
          (pr_list relation) us 
        in
        (s,univ,returns result)
      ) [(provides,relation,result,true);(provides,relation,not result,false)]
    )
    [ 
      (unversioned,unversioned,true);
      (unversioned,eqversioned20,false);
      (unversioned,leqversioned,false);
      (unversioned,geqversioned,false);

      (eqversioned10,unversioned,true);
      (eqversioned10,eqversioned20,false);
      (eqversioned10,leqversioned,true);
      (eqversioned10,geqversioned,false);

      (eqversioned20,unversioned,true);
      (eqversioned20,eqversioned20,true);
      (eqversioned20,leqversioned,false);
      (eqversioned20,geqversioned,true);

      (eqversioned10@eqversioned20,unversioned,true);
      (eqversioned10@eqversioned20,eqversioned20,true);
      (eqversioned10@eqversioned20,leqversioned,true);
      (eqversioned10@eqversioned20,geqversioned,true);
    ]
  in List.flatten res
;;

let test_sources =
  "test_sources" >::: [
    "test select" >::: make_test_cases select_deps;
    "test sources2packages" >::: make_test_cases test_sources2packages;
  ]
;;

module PKG = struct
  type t = Cudf.package
  let compare = CudfAdd.compare
  let pp_printer fmt n =
    Format.fprintf fmt "(\"%s\",%d)" n.Cudf.package n.Cudf.version
  let pp_print_sep fmt () = Format.fprintf fmt ";"
end

module SETPKG = struct
  module S = OUnitDiff.SetMake(PKG);;
  type t = S.t
  let compare = S.compare
  let pp_printer fmt n = S.pp_printer fmt n
  let pp_print_sep fmt () = Format.fprintf fmt ";"
end

module SetPKG = OUnitDiff.SetMake(SETPKG);;

let test_essential_constraints =
  "test essential" >:: (fun _ ->
    let f = f_essential in
    let archs = [] in
    let options = Debcudf.default_options in
    let pkgl = Packages.input_raw ~archs [f] in
    let tables = Debcudf.init_tables pkgl in
    let essential =
      List.map (fun (_,l) ->
        SETPKG.S.of_list l
      ) (Debcudf.get_essential ~options tables)
    in
    let cc = Debcudf.tocudf ~options tables (List.find (fun p -> p#name = "cc" && p#version = "1") pkgl) in
    let dd = Debcudf.tocudf ~options tables (List.find (fun p -> p#name = "dd" && p#version = "1") pkgl) in
    SetPKG.assert_equal
      (SetPKG.of_list [SETPKG.S.of_list [cc]; SETPKG.S.of_list [dd]])
      (SetPKG.of_list essential)
  )
;;

let all = 
  "all tests" >::: [ 
    test_parsing;
    "test versioned provides" >::: make_test_cases test_versioned_provides;
    test_mapping ;
    test_conflicts;
    "test real name" >::: make_test_cases test_realname;
    test_version;
    test_cluster;
    test_evolution;
    test_version_comparison;
    test_architecture_matching;
    test_sources;
    test_essential_constraints;
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
