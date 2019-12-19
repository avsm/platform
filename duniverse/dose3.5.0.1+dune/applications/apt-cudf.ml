(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common
open Debian
open DoseparseNoRpm
open Criteria_types

module Pcre = Re_pcre

include Util.Logging(struct let label = "apt-cudf backend" end) ;;

module Options = struct
  open OptParse
  let description = "apt-get backend (EDSP > 0.4)"
  let options = OptParser.make ~description
  include StdOptions.MakeOptions(struct let options = options end)

  let dump = StdOpt.store_true ()
  let noop = StdOpt.store_true ()
  let solver = StdOpt.str_option ()
  let criteria = StdOptions.criteria_option ()
  let criteria_plain = StdOpt.str_option ()
  let explain = StdOpt.store_true ()
  let conffile = StdOpt.str_option ~default:"/etc/apt-cudf.conf" ()
  let native_arch = StdOpt.str_option ()
  let foreign_archs = StdOptions.str_list_option ()

  open OptParser ;;
  add options ~long_name:"conf" ~help:"configuration file (default:/etc/apt-cudf.conf)" conffile;
  add options ~long_name:"dump" ~help:"dump the cudf universe and solution" dump;
  add options ~long_name:"noop" ~help:"Do nothing" noop;
  add options ~short_name:'s' ~long_name:"solver" ~help:"external solver" solver;
  add options ~short_name:'c' ~long_name:"criteria" ~help:"optimization criteria in extended MISC syntax" criteria;
  add options ~long_name:"criteria-plain" ~help:"optimization criteria passed unmangled to the solver" criteria_plain;
  add options ~short_name:'e' ~long_name:"explain" ~help:"print installation summary" explain;
  add options ~long_name:"native-arch" ~help:"Native architecture" native_arch;
  add options ~long_name:"foreign-archs" ~help:"Foreign architectures" foreign_archs;

end

let doseexit () =
  if OptParse.Opt.is_set Options.solver then (
    Format.printf "Error: %s" (OptParse.Opt.get Options.solver);
    if OptParse.Opt.is_set Options.criteria_plain then
      Format.printf " \"%s\"" (OptParse.Opt.get Options.criteria_plain);
    Format.printf "@."
  )
;;

let fatal fmt =
  Printf.kprintf (fun s ->
    doseexit ();
    Format.printf "Message: %s@." s;
    exit 1
  ) fmt
;;

let print_progress ?i msg =
  Format.printf "Progress: %s@." (Util.timestamp ());
  if not(Option.is_none i) then
    Format.printf "Percentage: %d@." (Option.get i);
  if msg <> "" then
    Format.printf "Message: %s@." msg
;;

let rec input_all_lines acc chan =
  try input_all_lines ((input_line chan)::acc) chan
  with End_of_file -> acc

let solver_dir = 
  try Sys.getenv("CUDFSOLVERS") 
  with Not_found -> "/usr/share/cudf/solvers"
;;

let apt_get_cmdline = 
  try Sys.getenv("APT_GET_CUDF_CMDLINE")
  with Not_found -> ""
;;

let pp_pkg fmt (p,univ) =
  try
    let pkg = Hashtbl.find univ (p.Cudf.package,p.Cudf.version) in
    let apt_id = List.assoc "APT-ID" pkg#extras in
    Format.fprintf fmt "%s\n" apt_id;
    Format.fprintf fmt "Package: %s\n" pkg#name;
    Format.fprintf fmt "Version: %s\n" pkg#version;
    Format.fprintf fmt "Architecture: %s\n" pkg#architecture;
  with Not_found -> fatal "apt-cudf internal error"

let pp_pkg_list fmt (l,univ) =
  try 
    Format.fprintf fmt "%s" (
      String.concat ", "
      (List.map (fun p ->
        let pkg = Hashtbl.find univ (p.Cudf.package,p.Cudf.version) in
        Printf.sprintf "%s=%s/%s" 
        pkg#name 
        pkg#version
        pkg#architecture
      ) l)
    )
  with Not_found -> fatal "apt-cudf internal error"
;;

let pp_pkg_list_tran fmt (l,univ) = pp_pkg_list fmt (List.map snd l,univ) ;;

type critopt =
  | PlainCrit of string
  | ExtCrit of Criteria_types.criteria

(* apt-cudf.conf example :

solver: mccs-cbc , mccs-lpsolve
upgrade: -lex[-new,-removed,-notuptodate]
dist-upgrade: -lex[-notuptodate,-new,-removed]
install: -lex[-removed,-changed]
remove: -lex[-removed,-changed]
trendy: -lex[-removed,-notuptodate,-unsat_recommends,-new]
paranoid: -lex[-removed,-changed]

solver: *
upgrade: -new,-removed,-notuptodate
dist-upgrade: -notuptodate,-new,-removed
install: -removed,-changed
remove: -removed,-changed
trendy: -removed,-notuptodate,-unsat_recommends,-new
paranoid: -removed,-changed
*)
let parse_conf_file fname =
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  try
    let stanzas = Cudf_822_parser.doc_822 Cudf_822_lexer.token_822 lexbuf in
    let r = 
      List.flatten (
        List.map (fun stanza -> 
          let (_,sl) = List.assoc "solver" stanza in
          let l = List.map (fun (k, (_loc, v)) -> (k,v)) stanza in
          List.filter_map (fun s -> 
            let x = ExtString.String.strip s in
            if x = "" then None else
            let mapl =
              List.filter_map (function
                |("solver",_) -> None
                |(k,v) when x = "*" ->
                  let _loc = Format822.dummy_loc in
                  let field =
                    let str = Printf.sprintf "conf file stanza \"%s\" field \"%s\"" x k in
                    (str,(_loc,v))
                  in
                  let c = Criteria.parse_criteria field in
                  Some(x, ExtCrit c)
                |(k,v) -> Some(k, PlainCrit v)
              ) l
            in
            Some(x,mapl)
          ) (ExtString.String.nsplit sl ",")
        ) stanzas 
      )
    in
    close_in ic; r
  with Cudf_types.Parse_error_822 (msg, _) ->
    fatal "%s" (Format822.error lexbuf msg)
;;

(* the priority to choose criteria is:
  1. --criteria-plain
  2. --criteria
  3. EDSP Preferences field
  4. default criteria for given solver in /etc/apt-cudf.conf and EDSP action
  5. default criteria for "*" default solver in /etc/apt-cudf.conf and EDSP action
  6. default criteria as hardcoded above for given EDSP action
  7. hardcoded paranoid criteria *)
let choose_criteria ~criteria ~criteria_plain ~conffile solver request =
  let conf =
    if Sys.file_exists conffile then
      parse_conf_file conffile
    else []
  in
  let default_criteria =
    let critnames = List.map fst Criteria.default_criteria in
    (* test if this stanza has all the required fields *)
    try 
      let c = List.assoc solver conf in
      List.iter (fun f -> ignore(List.assoc f c)) critnames;
      c
    with Not_found ->
      try 
        let c = List.assoc "*" conf in
        List.iter (fun f -> ignore(List.assoc f c)) critnames;
        c
      with Not_found ->
        List.map (fun (k,v) -> (k,ExtCrit v))
        Criteria.default_criteria
  in
  if OptParse.Opt.is_set Options.criteria_plain then
    PlainCrit (OptParse.Opt.get Options.criteria_plain)
  else if OptParse.Opt.is_set Options.criteria then
    if Criteria.is_misc2012 solver then
      ExtCrit (OptParse.Opt.get Options.criteria)
    else
      fatal "You specified --criteria, but the solver does not recognize the MISC 2012 optimization language. Please specifify the optimization criteria using --criteria-plain"
  (* try parsing the Preferences field of the EDSP document *)
  else if request.Edsp.preferences <> "" then
    if Criteria.is_misc2012 solver then
      let _loc = Format822.dummy_loc in
      let field = ("EDSP Preferences",(_loc,request.Edsp.preferences)) in
      let c = Criteria.parse_criteria field in
      ExtCrit c
    else
      PlainCrit request.Edsp.preferences
  else if not (Criteria.is_misc2012 solver) then
      fatal "The solver does not recognize the MISC 2012 optimization language. Please specifify the optimization criteria using --criteria-plain"
  else if request.Edsp.upgrade then
    List.assoc "upgrade" default_criteria
  else if request.Edsp.distupgrade then
    List.assoc "dist-upgrade" default_criteria
  else if request.Edsp.install <> [] then
    List.assoc "install" default_criteria
  else if request.Edsp.remove <> [] then
    List.assoc "remove" default_criteria
  else
    List.assoc "paranoid" default_criteria
;;

let parse_solver_spec filename =
  let (exec, version) = (ref "", ref "") in
  begin try
    if Sys.file_exists filename then begin
      let ic = open_in filename in
      while true do
        let l = input_line ic in
        if String.starts_with l "exec: " then
          exec := String.strip (snd (String.split l " "))
        else if String.starts_with l "cudf-version: " then
          Scanf.sscanf l "cudf-version: %s " (fun s -> version := s);
      done;
      close_in ic
    end else
      fatal "Solver %s not found" filename
  with
    | Sys_error _ -> fatal "cannot parse CUDF solver specification %s" filename
    | End_of_file -> ()
    | Scanf.Scan_failure err ->
      fatal "parse error while reading CUDF solver specification %s: %s"
        filename err
  end;
  if !exec = "" || !version = "" then
    fatal "incomplete CUDF solver specification %s" filename;
  if not (String.exists !exec "$in" && String.exists !exec "$out"
          && String.exists !exec "$pref") then
    fatal
      "Incomplete solver specification %s: one or more of $in, $out, $pref is missing in exec line"
      filename;
  (!exec,!version)
;;

let main () =
  let timer1 = Util.Timer.create "parsing" in
  let timer2 = Util.Timer.create "conversion" in
  let timer3 = Util.Timer.create "solution" in
  let args = OptParse.OptParser.parse_argv Options.options in
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose);
  StdDebug.enable_bars (OptParse.Opt.get Options.progress) [] ;
  StdDebug.enable_timers (OptParse.Opt.get Options.timers)
  ["parsing";"cudfio";"conversion";"solver";"solution"];
  StdDebug.all_quiet (OptParse.Opt.get Options.quiet);
  if apt_get_cmdline <> "" then
    debug "APT_GET_CUDF_CMDLINE=%s" apt_get_cmdline;
  debug "CUDFSOLVERS=%s" solver_dir;
  (* debug "TMPDIR=%s" waiting for ocaml 4.0 *)

  if OptParse.Opt.is_set Options.criteria && 
  OptParse.Opt.is_set Options.criteria_plain then
    fatal "--criteria cannot be specified together with --criteria-plain";

  let ch = 
    match args with 
    |[] -> (IO.input_channel stdin)
    |file::_ -> Input.open_file file 
  in
  
  Util.Timer.start timer1;
  let (request,pkglist) = Edsp.input_raw_ch ch in

  let (native_arch,foreign_archs) = 
    StdUtils.get_architectures
      request.Edsp.architecture
      request.Edsp.architectures
      (OptParse.Opt.opt Options.native_arch) 
      (OptParse.Opt.opt Options.foreign_archs)
  in

  let request =
    match apt_get_cmdline with
    |"" -> request
    |_ -> begin
      let apt_req = Apt.parse_request_apt apt_get_cmdline in
      Edsp.from_apt_request native_arch 
        {request with Edsp.install = []; remove = []} apt_req
    end
  in

  Util.Timer.stop timer1 ();
  
  if args <> [] then Input.close_ch ch;

  let solver =
    if OptParse.Opt.is_set Options.solver then
      OptParse.Opt.get Options.solver
    else
      Filename.basename(Sys.argv.(0))
  in

(* Hashtable storing any new cudf fields necessary for the extended count()
  criteria syntax.
  mapping from cudf field name to 3-type containing the EDSP fieldname, the
  regex plain text and the optional compiled regex

  since we have an extended MISC criteria we need to see if it uses the
  extension to the count criteria and if yes, fill the regexfield hash
  with the correct values *)
  let regexfields = Hashtbl.create (10) in
  let criteria =
    let criteria = OptParse.Opt.opt Options.criteria in
    let criteria_plain = OptParse.Opt.opt Options.criteria_plain in
    let conffile = OptParse.Opt.get Options.conffile in
    match choose_criteria ~criteria ~criteria_plain ~conffile solver request with
    |ExtCrit c ->
      Criteria.iter (fun (cudffieldname,fieldname,regexstring,compiled_re) ->
        let hashtblval = (fieldname,regexstring,compiled_re) in
        try 
          (* mapping already exists in hashtable *)
          begin match Hashtbl.find regexfields cudffieldname with
          |v when v = hashtblval -> ()
          |(_,plain,r) ->
            let sep = match r with Some _ -> "~" | None -> "=" in
            fatal "Hash collision: md5(%s%s%s)[:8] = md5(%s%s%s)[:8]"
              fieldname sep plain fieldname sep regexstring
          end
        with Not_found ->
          Hashtbl.add regexfields cudffieldname hashtblval
      ) c;
      Criteria.to_string ~solver c
    |PlainCrit c -> c
  in
  (* small hack to avoid another global variable. 
   * We set criteria_plain to relect to actual criteria used
   * by the solver and to print it in case of error *)
  OptParse.Opt.set Options.criteria_plain criteria;

  Util.Timer.start timer2;
  let tables = Debcudf.init_tables pkglist in
  let default_preamble =
    let l = List.map snd Edsp.extras_tocudf in
    (* add any additional regex fields to the cudf preamble *)
    let l = Hashtbl.fold (fun k _ acc ->
        (k, `Int (Some 0))::acc
      ) regexfields l in
    CudfAdd.add_properties Debcudf.preamble l
  in

  let options = {
    Debcudf.default_options with 
    Debcudf.native = native_arch;
    Debcudf.foreign = foreign_archs }
  in 
  let univ = Hashtbl.create (2*(List.length pkglist)-1) in
  let cudfpkglist = 
    List.filter_map (fun pkg ->
      let p = Edsp.tocudf tables ~options pkg in
      (* for each regex, check if the current package has the requested field
       * and if yes, check if the regex matches *)
      let p = Hashtbl.fold (fun cudffield (edspfield, plain, compiledre) acc ->
          match
            try Some (pkg#get_extra edspfield)
            with Not_found -> None
          with
          | None -> acc (* field not found in this edsp package *)
          | Some fieldvalue -> begin
              let matches = match compiledre with
                | None when String.exists fieldvalue plain -> true (* plain text search *)
                | Some compiledre
                  when Pcre.pmatch ~rex:compiledre fieldvalue -> true (* regex match *)
                | _ -> false
              in
              match matches with
              | true ->
                  { acc with Cudf.pkg_extra = (cudffield, `Int 1) :: acc.Cudf.pkg_extra }
              | false -> acc (* no match *)
            end
        ) regexfields p 
      in
      if not(Hashtbl.mem univ (p.Cudf.package,p.Cudf.version)) then begin
        Hashtbl.add univ (p.Cudf.package,p.Cudf.version) pkg;
        Some p
      end else begin
        warning "Duplicated package (same version, name and architecture) : (%s,%s,%s)" 
          pkg#name pkg#version pkg#architecture;
        None
      end
    ) pkglist 
  in
  let universe = 
    try Cudf.load_universe cudfpkglist
    with Cudf.Constraint_violation s ->
      fatal "(CUDF) Malformed universe %s" s
  in

  (*
  let universe = 
    let initialsize = (2 * (List.length pkglist) - 1 ) in
    let univcache = Cudf.empty_universe ~size:initialsize () in
    List.iter (fun pkg ->
      let p = Edsp.tocudf tables ~options pkg in
      Hashtbl.add univ (p.Cudf.package,p.Cudf.version) pkg;
      try Cudf.add_package univcache p
      with Cudf.Constraint_violation s ->
(*        warning "Possibly duplicated package (same version, name and architecture) : (%s,%s,%s)"
          pkg.Packages.name pkg.Packages.version pkg.Packages.architecture;
          *)
        fatal "(CUDF) Malformed universe %s" s
    ) pkglist ;
    univcache
  in
  *)

  let cudfdump = Filename.temp_file "apt-cudf-universe" ".cudf" in
  if OptParse.Opt.get Options.dump then begin
    info "Dump cudf universe in %s\n" cudfdump;
    let oc = open_out cudfdump in
    Cudf_printer.pp_preamble oc default_preamble;
    Printf.fprintf oc "\n";
    Cudf_printer.pp_universe oc universe;
    close_out oc
  end;

  let cudf_request = Edsp.requesttocudf tables universe request in
  let cudf = (default_preamble,universe,cudf_request) in
  Util.Timer.stop timer2 ();

  if OptParse.Opt.get Options.dump then begin
    info "Append cudf request to %s\n" cudfdump;
    let oc = open_out_gen 
      [Open_wronly; Open_append; Open_creat; Open_text]
      0o666 cudfdump 
    in
    Printf.fprintf oc "\n";
    (* write regex field name mapping as comment *)
    Hashtbl.iter (fun k (f,plain,re) ->
        match re with
        | Some _ -> Printf.fprintf oc "# %s -- %s:~/%s/\n" k f plain
        | None -> Printf.fprintf oc "# %s -- %s:=/%s/\n" k f plain
      ) regexfields;
    Cudf_printer.pp_request oc cudf_request;
    close_out oc
  end;

  (* do nothing. *)
  if OptParse.Opt.get Options.noop then begin
    info "Noop & Exit.";
    exit 0
  end;

  let exec_pat = fst (parse_solver_spec (Filename.concat solver_dir solver)) in

  let solpre,soluniv = 
    let dummy = { Algo.Depsolver.dummy_request with
      Cudf.depends =
        List.map (fun (_,pkglist) ->
          List.map (fun pkg ->
            (pkg.Cudf.package,Some(`Eq,pkg.Cudf.version))
          ) pkglist
        ) (Debcudf.get_essential tables) }
    in
    let explain = false in
    match Algo.Depsolver.check_request ~cmd:exec_pat ~dummy ~criteria ~explain cudf with
    |Algo.Depsolver.Error s -> fatal "%s" s
    |Algo.Depsolver.Unsat None -> begin
      doseexit ();
      Format.printf "Message: (UNSAT) No Solutions according to the given preferences@.";
      exit 0
    end
    |Algo.Depsolver.Unsat Some d -> begin
      doseexit ();
      Format.printf "Message: (UNSAT) No Solutions according to the given preferences@.";
      exit 0
    end
    |Algo.Depsolver.Sat s -> s
  in

  if OptParse.Opt.get Options.dump then begin
    let cudfsol = Filename.temp_file "apt-cudf-solution" ".cudf" in
    info "Dump cudf solution in %s\n" cudfsol;
    let oc = open_out cudfsol in
    Cudf_printer.pp_preamble oc default_preamble;
    Printf.fprintf oc "\n";
    Cudf_printer.pp_universe oc soluniv;
    close_out oc
  end;

  Util.Timer.start timer3;
  let empty = ref true in
  let cache = CudfAdd.Cudf_hashtbl.create 1023 in
  (* In Debian (and thus for apt and dpkg), packages can only be installed in a
   * single version at a time. Thus, any up or downgrades always implicitly
   * remove the old version. Therefore, if a package installation request just
   * changes the version of the package, we remember this in a hash table such
   * that we do not generate a removal request for that package as well. As a
   * result, every package (name) only shows up exactly once in the solution.
   * Package removals are not explicitly shown for upgrades. *)
  let notremoved = Util.StringHashtbl.create 1023 in

  let (install,remove) = CudfDiff.make_solution ~universe ~solution:soluniv in
  CudfAdd.Cudf_set.iter (fun pkg ->
    CudfAdd.Cudf_hashtbl.add cache pkg ();
    Util.StringHashtbl.add notremoved pkg.Cudf.package ();
    Format.printf "Install: %a@." pp_pkg (pkg,univ)
  ) install;

  (* Print also all packages that are were requested, but don't show up in the 
   * diff because already installed *)
  List.iter (fun (n,c) ->
    List.iter (fun pkg -> 
      empty := false;
      if CudfAdd.Cudf_hashtbl.mem cache pkg then () 
      else begin
        CudfAdd.Cudf_hashtbl.add cache pkg ();
        Format.printf "Install: %a@." pp_pkg (pkg,univ);
      end
    ) (CudfAdd.who_provides soluniv (n,c))
  ) cudf_request.Cudf.install;

  CudfAdd.Cudf_set.iter (fun p ->
    if Util.StringHashtbl.mem notremoved p.Cudf.package then ()
    else Format.printf "Remove: %a@." pp_pkg (p,univ)
  ) remove;

  Util.Timer.stop timer3 ();

  if OptParse.Opt.get Options.explain then begin
    let open CudfDiff in
    let diff = make_difference ~universe ~solution:soluniv in
    let summary = make_summary universe diff in
    Format.printf "Summary: " ;
    if summary.install <> [] then
      Format.printf "%d to install " (List.length summary.install);
    if summary.remove <> [] then
      Format.printf "%d to remove " (List.length summary.remove);
    if summary.upgrade <> [] then
      Format.printf "%d to upgrade " (List.length summary.upgrade);
    if summary.downgrade <> [] then
      Format.printf "%d to downgrade " (List.length summary.downgrade);
    if summary.notchange <> [] then
      Format.printf "%d not changed " (List.length summary.notchange);

    Format.printf " @.";

    if summary.install <> [] then
      Format.printf "Installed: %a@." pp_pkg_list (summary.install,univ);
    if summary.remove <> [] then 
      Format.printf "Removed: %a@." pp_pkg_list (summary.remove,univ);
    if summary.upgrade <> [] then 
      Format.printf "Upgraded: %a@." pp_pkg_list_tran (summary.upgrade,univ);
    if summary.downgrade <> [] then 
      Format.printf "Downgraded: %a@." pp_pkg_list_tran (summary.downgrade,univ);
    if summary.notchange <> [] && (OptParse.Opt.get Options.verbose) >= 1 then 
      Format.printf "UnChanged: %a@." pp_pkg_list (summary.notchange,univ);

  end;

  if !empty then 
    print_progress ~i:100 "No packages removed or installed";
;;

main ();;

