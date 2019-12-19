open Common
open Cudf
open Cudf_types_pp
open Rpm
open ExtLib

let enable_debug () =
  Util.set_verbosity Common.Util.Summary
;;

module Options = struct
  open OptParse
  let debug = StdOpt.store_true ()
  let synth = StdOpt.str_option ()
  let hdlist = StdOpt.str_option ()

  let description = "Check a synthesis hdlist agaist an hdlist"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Do debugging" debug;
  add options ~short_name:'s' ~long_name:"synthesis" ~help:"Path of synthesis hdlist" synth;
  add options ~long_name:"hdlist" ~help:"Path of hdlist" hdlist;
end;;

let _ =
begin
  at_exit (fun () -> Util.dump Format.err_formatter);
  ignore (OptParse.OptParser.parse_argv Options.options);
  if OptParse.Opt.get Options.debug then enable_debug ();
 
  Printf.eprintf "Parsing...%!";
  let hu = Rpm.Rpmcudf.load_universe (Rpm.Packages.Hdlists.input_raw [OptParse.Opt.get Options.hdlist]) in
  let su = Rpm.Rpmcudf.load_universe (Rpm.Packages.Synthesis.input_raw [OptParse.Opt.get Options.synth]) in
  let h_map = CudfAdd.build_maps hu in
  let s_map = CudfAdd.build_maps su in
  Printf.eprintf "done.\n%!";

  (* check all packages in the synthesis hdlist *)
  Cudf.iter_packages (fun sp ->
    (* find any file provides in sp *)
    List.iter (fun (n, v) ->
      if n.[0] = '/' then
      begin
        let s_provides =
          s_map.CudfAdd.who_provides ((n, v) :> Cudf_types.vpkg) in
        let h_provides =
          h_map.CudfAdd.who_provides ((n, v) :> Cudf_types.vpkg) in
        let slist =
          List.map (fun pn ->
            string_of_pkgname pn.package)
          (List.unique (List.sort s_provides)) in
        let hlist =
          List.map (fun pn ->
            string_of_pkgname pn.package)
          (List.unique (List.sort h_provides)) in
        (* if the lengths are different, there is a problem *)
        if List.length slist <> List.length hlist
        then
          Printf.printf "ERROR: for package %s-%s, file dependency/conflict on %s:\nsynthesis says: %s\nhdlist says:%s\n"
          (string_of_pkgname sp.package) (string_of_version sp.version)
          (string_of_pkgname n) (String.concat ", " slist)
          (String.concat ", " hlist)
        (* otherwise, check if the lists have the same elements *)
        else if not
          (List.fold_left2 (fun acc s h -> acc && s = h) true slist hlist)
        then
          Printf.printf "ERROR: for package %s-%s, file dependency/conflict on %s:\nsynthesis says: %s\nhdlist says:%s\n"
          (string_of_pkgname sp.package) (string_of_version sp.version)
          (string_of_pkgname n) (String.concat ", " slist)
          (String.concat ", " hlist)
      end
    ) ((List.flatten sp.depends) @ sp.conflicts)
  ) su
end;;
