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

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

module type Ot = sig
  val options :
    ?usage:string ->
    ?status:int ->
    ?version:string ->
    ?suppress_usage:bool ->
    ?suppress_help:bool ->
    ?prog:string ->
    ?formatter:OptParse.Formatter.t -> unit -> OptParse.OptParser.t
end

(* *************************************** *)

let vpkg_option ?default ?(metavar = " <vpkg>") () =
  let parse_vpkg s = 
    let _loc = Format822.dummy_loc in
    Pef.Packages.parse_vpkg ("cmdline <vpkg>",(_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkg (fun _ s -> Printf.sprintf "Invalid vpackage '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (<c> v)" *)
let vpkglist_option ?default ?(metavar = " <vpkglst>") () =
  let parse_vpkglist s = 
    let _loc = Format822.dummy_loc in
    Pef.Packages.parse_vpkglist ("cmdline <vpkglst>",(_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "Invalid vpackage list '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (= v)" *)
let pkglist_option ?default ?(metavar = " <pkglst>") () =
  let parse_vpkglist s = 
    let _loc = Format822.dummy_loc in
    List.map (function
      |((n,a),Some("=",v)) -> (n,a,v)
      |((n,a),None) ->
          raise (Format822.ParseError ([],s,"you must specify a version" ))
      |_ -> raise (Format822.ParseError ([],s,""))
    ) (Pef.Packages.parse_vpkglist ("cmdline <pkglst>",(_loc,s)))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "Invalid package list '%s'" s)
;;

(* this is a ,-separated list of optimization criteria *)
let criteria_option ?default ?(metavar = " <criteria>") () =
  let parse_criteria s =
    let _loc = Format822.dummy_loc in
    Criteria.parse_criteria ("cmdline <criteria>",(_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_criteria (fun _ s -> Printf.sprintf "Invalid criteria list '%s'" s)
;;

(* *************************************** *)

let incr_str_list ?(default=Some []) ?(metavar = " <str>") =
  let acc = ref [] in 
  let coerce s = acc := s :: !acc ; !acc in
  fun () ->
  OptParse.Opt.value_option metavar default coerce 
  (fun _ s -> Printf.sprintf "Invalid String '%s'" s)
;;

(* this is a ,-separated list of strings *)
let str_list_option ?default ?(metavar = " <strlst>") =
  let sep = "," in
  let coerce s = ExtString.String.nsplit s sep in
  fun () ->
    OptParse.Opt.value_option metavar default coerce
    (fun _ s -> Printf.sprintf "Invalid String '%s'" s)
;;

(* *************************************** *)

module MakeOptions(O : Ot) = struct
  open OptParse ;;

  let verbose = StdOpt.incr_option ()
  let quiet = StdOpt.store_true ()
  let progress = StdOpt.store_true ()
  let timers = StdOpt.store_true ()
  let options = O.options ~status:64 ~version:VersionInfo.version () ;;

  open OptParser ;;
  add options ~short_name:'v' ~long_name:"verbose" ~help:"print additional information" verbose;
  add options ~long_name:"progress" ~help:"print progress bars" progress;
  add options ~long_name:"timers" ~help:"print timing information" timers;
  add options ~long_name:"quiet" ~help:"do no print any messages" quiet;

end

let create_group group descr options =
    if not (Option.is_none !group) then
      Option.get !group
    else
      let g = OptParse.OptParser.add_group options descr in
      let _ = group := Some g in
      g
;;

module DistcheckOptions = struct
  open OptParse ;;

  let success = StdOpt.store_true ()
  let failure = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let minimal = StdOpt.store_true ()
  let condense = StdOpt.store_true ()
  let summary = StdOpt.store_true ()

  let default_options = [
    "success";
    "failure";
    "explain";
    "explain-minimal";
    "explain-condense";
    "summary"
  ]

  let group = ref None 
  let descr = "Distcheck Options"

  let add_options ?(default=default_options) options =
    let open OptParser in 
    if List.length default > 0 then begin
      let group = create_group group descr options in
      if List.mem "explain" default then
        add options ~group ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
      if List.mem "explain-minimal" default then
        add options ~group ~short_name:'m' ~long_name:"explain-minimal" ~help:"Do not print dependency chains of results" minimal;
      if List.mem "explain-condense" default then
        add options ~group ~short_name:'c' ~long_name:"explain-condense" ~help:"Compress explanation graph" condense;
      if List.mem "failure" default then
        add options ~group ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failure;
      if List.mem "success" default then
        add options ~group ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" success;
      if List.mem "summary" default then
        add options ~group ~long_name:"summary" ~help:"Show Failures Summary" summary;
    end
  ;;

  let add_option ?short_name ?long_name ?help options =
    let open OptParser in
    let group = create_group group descr options in
    add options ~group ?short_name ?long_name ?help 
  ;;

end

module OutputOptions = struct
  open OptParse ;;

  let outfile = StdOpt.str_option ()
  let outdir = StdOpt.str_option ()
  let dot = StdOpt.store_true ()

  let default_options = [
    "outfile";
    "outdir";
    "dot"
  ]

  let group = ref None 
  let descr = "Output Options"

  let add_options ?(default=default_options) options =
    let open OptParser in 
    if List.length default > 0 then begin
      let group = create_group group descr options in
      if List.mem "outfile" default then
        add options ~group ~short_name:'o' ~long_name:"outfile"
        ~help:"Redirect the output to a file (default stdout)" outfile;
      if List.mem "outdir" default then
        add options ~group ~short_name:'d' ~long_name:"outdir" 
        ~help:"Set the output directory (default current directory)" outdir;
      if List.mem "dot" default then
        add options ~group ~long_name:"dot"
        ~help:"Save the explanation graph (one for each package) in dot format" dot;
    end
  ;;

  let add_option ?short_name ?long_name ?help options =
    let open OptParser in
    let group = create_group group descr options in
    add options ~group ?short_name ?long_name ?help 
  ;;

end

module InputOptions = struct
  open OptParse ;;

  let itypes = (List.map Url.scheme_to_string Url.supported_input_types)
  let in_option ?default ?(metavar = Printf.sprintf "<%s>" (String.concat "|" itypes)) () =
    let coerce s = if List.mem s itypes then s else raise Not_found in
    let supported = String.concat ", " itypes in
    let error _ s = Printf.sprintf "input format \"%s\" not supported. Must be one of: %s" s supported in
    Opt.value_option metavar default coerce error

  let vtypes = Versioning.Utils.supported_formats
  let comp_option ?default ?(metavar = Printf.sprintf "<%s>" (String.concat "|" vtypes)) () =
    let coerce s = if List.mem s vtypes then s else raise Not_found in
    let supported = String.concat ", " vtypes in
    let error _ s = Printf.sprintf "comparison function \"%s\" not supported. Must be one of: %s" s supported in
    Opt.value_option metavar default coerce error

  let trim = StdOpt.store_true ()
  let latest = StdOpt.int_option ()
  let checkonly = StdDebian.vpkglist_option ()
  let background = incr_str_list ()
  let foreground = incr_str_list ()
  let inputtype = in_option ()
  let compare = comp_option ()

  let default_options = [
    (* "trim"; *)
    "inputtype";
    "latest";
    "checkonly";
    "bg";
    "fg";
    "compare";
  ]

  let group = ref None
  let descr = "Input Options"

  (** give a list of positional arguments returns two list of resources,
      foreground and background. Positional arguments are assumed to be 
      foreground resources. *)
  let parse_cmdline (it,im) posargs = 
    let add_format t = List.map (fun s -> (Url.scheme_to_string t)^"://"^s) in
    let fg = OptParse.Opt.get foreground in
    let bg = OptParse.Opt.get background in
    let fg = (if List.length (posargs@fg) = 0 then ["-"] else posargs)@fg in
    if im then
      (add_format it fg, add_format it bg)
    else
      (fg,bg)
  ;;

  let add_options ?(default=default_options) options =
    let open OptParser in 
    if List.length default > 0 then begin
      let group = create_group group descr options in
      if List.mem "inputtype" default then
        add options ~group ~short_name:'t' ~help:"Set the input type format" inputtype;
      if List.mem "checkonly" default then
        add options ~group ~long_name:"checkonly" 
        ~help:"Check only these packages" checkonly;
      if List.mem "trim" default then
        add options ~group ~long_name:"trim" 
        ~help:"Consider only installable packages" trim;
      if List.mem "latest" default then
        add options ~group ~long_name:"latest" 
        ~help:"Consider only the latest N versions of each package" latest;
      if List.mem "fg" default then
        add options ~group ~long_name:"fg"
        ~help:("Additional Packages lists that are checked and used "^
               "for resolving dependencies (can be repeated)") foreground;
      if List.mem "bg" default then
        add options ~group ~long_name:"bg"
        ~help:("Additional Packages lists that are NOT checked but used "^
               "for resolving dependencies (can be repeated)") background;
      if List.mem "compare" default then
        add options ~group ~long_name:"compare"
        ~help:"When used with a pef input type, selects a comparison function" compare;

    end
  ;;

  let add_option ?short_name ?long_name ?help options =
    let open OptParser in
    let group = create_group group descr options in
    add options ~group ?short_name ?long_name ?help 
  ;;

end

type options =
  |Deb of Debian.Debcudf.options
  |Pef of Debian.Debcudf.options
  |Opam of Opam.Opamcudf.options
  |Edsp of Debian.Debcudf.options
  |Csw
  |Rpm
  |Cudf

module DistribOptions = struct
  open OptParse ;;

  let deb_native_arch = StdOpt.str_option ()
  let deb_foreign_archs = str_list_option ()
  let deb_host_arch = StdOpt.str_option ()
  let deb_ignore_essential = StdOpt.store_true ()
  let deb_builds_from = StdOpt.store_true ()
  let deb_drop_bd_indep = StdOpt.store_true ()
  let deb_profiles = str_list_option ()

  let opam_switch = StdOpt.str_option ~default:"system" ()
  let opam_switches = str_list_option ()
  let opam_profiles = str_list_option ()

  let default_options = [
    "deb-native-arch";
    "deb-host-arch";
    "deb-foreign-archs";
    "deb-ignore-essential";
    "deb-builds-from";
    "deb-drop-b-d-indep";
    "deb-profiles";
    "opam-switch";
    "opam-switches";
    "opam-profiles"
  ]

  let set_deb_options () =
    let native = Opt.opt deb_native_arch in
    let host =
      if Opt.is_set deb_host_arch then begin
        (* if host arch is set, native arch must be set *)
        if Option.is_none native then
          fatal "you must specify at least the native architecture" ;
        Opt.opt deb_host_arch
      end
      else native
    in
    let foreign =
      (* if host arch is set, it is an implicit foreign arch *)
      if Opt.is_set deb_foreign_archs then begin
        let f = Opt.get deb_foreign_archs in
        if Opt.is_set deb_host_arch then
          (Option.get host)::f
        else
          f
      end else begin
        if Opt.is_set deb_host_arch then
          [Option.get host]
        else
          []
      end
    in
    let profiles =
      if Opt.is_set deb_profiles then
        Opt.get deb_profiles
      else
        try String.nsplit (Sys.getenv "DEB_BUILD_PROFILES") " "
        with Not_found -> []
    in
    {
      Debian.Debcudf.default_options with
      Debian.Debcudf.native = native;
      foreign = foreign;
      host = host;
      ignore_essential = Opt.get deb_ignore_essential;
      builds_from = Opt.get deb_builds_from;
      drop_bd_indep = Opt.get deb_drop_bd_indep;
      profiles = profiles;
    }
  ;;

  let set_opam_options () =
    let switch = Opt.get opam_switch in
    let switches =
      if Opt.is_set opam_switches then
        Opt.get opam_switches
      else []
    in
    let profiles =
      if Opt.is_set opam_profiles then
        Opt.get opam_profiles
      else []
    in
    { Opam.Opamcudf.default_options with
      Opam.Opamcudf.switch = switch;
      switches = switches;
      profiles = profiles;
    }
  ;;

  let set_default_options = function
    |`Deb -> Some (
      Deb { 
        Debian.Debcudf.default_options with
        Debian.Debcudf.ignore_essential = true
      })
    |`Edsp -> Some (
      Edsp { 
        Debian.Debcudf.default_options with
        Debian.Debcudf.ignore_essential = true
      })
    |`Pef -> Some (Pef Debian.Debcudf.default_options)
    |`Opam -> Some (Opam Opam.Opamcudf.default_options)
    |_ -> None

  let set_options = function
    |`Deb |`DebSrc -> Some (Deb (set_deb_options ()))
    |`Edsp -> Some (Edsp (set_deb_options ()))
    |`Pef -> Some (Pef Debian.Debcudf.default_options)
    |`Opam -> Some (Opam (set_opam_options ()))
    |_ -> None
  ;;

  let deb_group =
    let g = ref None in
    fun options ->
      match !g with
      |Some group -> group
      |None -> begin
          let group = OptParser.add_group options "Debian Specific Options" in
          g := Some group;
          group
      end

  let opam_group =
    let g = ref None in
    fun options ->
      match !g with
      |Some group -> group
      |None -> begin
          let group = OptParser.add_group options "Opam Specific Options" in
          g := Some group;
          group
      end

  let add_debian_options ?(default=default_options) options =
    let open OptParser in
    if List.length default > 0 then begin
      let group = deb_group options in
      if List.mem "deb-native-arch" default then
        add options ~group ~long_name:"deb-native-arch"
          ~help:"Native architecture" deb_native_arch;
      if List.mem "deb-host-arch" default then
        add options ~group ~long_name:"deb-host-arch" 
          ~help:"Native/cross compile host architecture, defaults to native architecture" deb_host_arch;
      if List.mem "deb-foreign-archs" default then
        add options ~group ~long_name:"deb-foreign-archs" 
          ~help:"Foreign architectures in addition to native and host architectures" deb_foreign_archs;
      if List.mem "deb-ignore-essential" default then
        add options ~group ~long_name:"deb-ignore-essential" 
          ~help:"Ignore Essential Packages" deb_ignore_essential;
      if List.mem "deb-builds-from" default then
        add options ~group ~long_name:"deb-builds-from"
          ~help:"Add builds-from relationship of binary packages on source packages as dependency" deb_builds_from;
      if List.mem "deb-drop-b-d-indep" default then
        add options ~group ~long_name:"deb-drop-b-d-indep"
          ~help:"Drop the Build-Depends-Indep field from source packages (build no Architecture:all packages)" deb_drop_bd_indep;
      if List.mem "deb-profiles" default then
        add options ~group ~long_name:"deb-profiles"
          ~help:"comma separated list of activated build profiles" deb_profiles;
    end

  let add_opam_options ?(default=default_options) options =
    let open OptParser in
    if List.length default > 0 then begin
      let group = opam_group options in
      if List.mem "opam-switch" default then
        add options ~group ~long_name:"opam-switch"
          ~help:"Active Switch" opam_switch;
      if List.mem "opam-switches" default then
        add options ~group ~long_name:"opam-switches"
          ~help:"Available Switches" opam_switches;
      if List.mem "opam-profiles" default then
        add options ~group ~long_name:"opam-profiles"
          ~help:"Build Profiles" opam_profiles;

    end

  let add_option ?group ?short_name ?long_name ?help options v =
    let open OptParser in
    match group with
    |None -> add options ?short_name ?long_name ?help v
    |Some group -> add options ~group ?short_name ?long_name ?help v
  ;;

end
