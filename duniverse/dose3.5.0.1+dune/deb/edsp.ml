(**************************************************************************************)
(*  Copyright (C) 2011 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2011 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** Representation of a apt-get <-> solvers protocol edsp > 0.4 *)

module Pcre = Re_pcre
open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

type request = {
  request : string;
  install : Pef.Packages_types.vpkg list;
  remove : Pef.Packages_types.vpkg list;
  architecture : Pef.Packages_types.architecture option;
  architectures : Pef.Packages_types.architectures;
  autoremove : bool;
  upgrade : bool;
  distupgrade : bool;
  strict_pin : bool;
  preferences: string;
  cmdline: string;
}

let default_request = {
  request = "";
  install = [];
  remove = [];
  architecture = None;
  architectures = [];
  autoremove = false;
  upgrade = false;
  distupgrade = false;
  strict_pin = false;
  preferences = "";
  cmdline = ""
}

(* convert a apt command line request to edsp request *)
let from_apt_request arch request = function
  |Apt.Install vpkgreqlist ->
      List.fold_left (fun acc -> function
        |(Some Pef.Packages_types.I, ((n,None),c), _) -> {acc with install = ((n,arch),c) :: acc.install}
        |(Some Pef.Packages_types.R, ((n,None),c), _) -> {acc with remove =  ((n,arch),c) :: acc.remove}
        |(None, ((n,None),c), _) -> {acc with install = ((n,arch),c) :: acc.install}

        |(Some Pef.Packages_types.I, vpkg, _) -> {acc with install = vpkg :: acc.install}
        |(Some Pef.Packages_types.R, vpkg, _) -> {acc with remove = vpkg :: acc.remove}
        |(None, vpkg, _) -> {acc with install = vpkg :: acc.install}
      ) request vpkgreqlist
  |Apt.Remove vpkgreqlist ->
      List.fold_left (fun acc -> function
        |(Some Pef.Packages_types.I, ((n,None),c), _) -> {acc with install = ((n,arch),c) :: acc.install}
        |(Some Pef.Packages_types.R, ((n,None),c), _) -> {acc with remove = ((n,arch),c) :: acc.remove}
        |(None, ((n,None),c), _) -> {acc with remove = ((n,arch),c) :: acc.remove}

        |(Some Pef.Packages_types.I, vpkg, _) -> {acc with install = vpkg :: acc.install}
        |(Some Pef.Packages_types.R, vpkg, _) -> {acc with remove = vpkg :: acc.remove}
        |(None, vpkg, _) -> {acc with remove = vpkg :: acc.remove}
      ) request vpkgreqlist
  |Apt.Upgrade _ -> {request with upgrade = true }
  |Apt.DistUpgrade _ -> {request with distupgrade = true}
;;

let parse_req (label,(loc,s)) = 
  let aux v = Pef.Packages.lexbuf_wrapper Pef.Packages_parser.vpkg_top (label,v) in
  let l = Pcre.split ~rex:Apt.blank_regexp s in 
  List.map (fun s -> aux (loc,s)) l

let parse_edsp_version (label,(_,s)) =
  match String.nsplit s " " with
  |["EDSP";s] when (float_of_string s) >= 0.4 -> s
  |_ -> raise (Format822.ParseError ([],label,"Invalid EDSP version."))

let parse_request_stanza par =
  (* request must be parse before any other fields *)
  let request = Pef.Packages.parse_s ~required:true parse_edsp_version "Request" par in
  {
    request = request; 
    install = Pef.Packages.parse_s ~default:[] parse_req "Install" par;
    remove = Pef.Packages.parse_s ~default:[] parse_req "Remove" par;
    upgrade = Pef.Packages.parse_s ~default:false Pef.Packages.parse_bool "Upgrade" par;
    architecture = Pef.Packages.parse_s ~default:None Pef.Packages.parse_string_opt "Architecture" par;
    architectures = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_string_list "Architectures" par;
    distupgrade = Pef.Packages.parse_s ~default:false Pef.Packages.parse_bool "Dist-Upgrade" par;
    autoremove = Pef.Packages.parse_s ~default:false Pef.Packages.parse_bool "Autoremove" par;
    strict_pin = Pef.Packages.parse_s ~default:true Pef.Packages.parse_bool "Strict-Pinning" par;
    preferences = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Preferences" par;
    cmdline = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Command-Line" par;
  }
;;

let parse_installed = Pef.Packages.parse_s Pef.Packages.parse_bool_s 
let parse_hold = Pef.Packages.parse_s Pef.Packages.parse_bool_s
let parse_apt_id = Pef.Packages.parse_s ~required:true Pef.Packages.parse_string
let parse_apt_pin = Pef.Packages.parse_s ~required:true Pef.Packages.parse_int_s
let parse_automatic = Pef.Packages.parse_s Pef.Packages.parse_bool_s
let parse_candidate = Pef.Packages.parse_s Pef.Packages.parse_bool_s
let parse_section = Pef.Packages.parse_s Pef.Packages.parse_string

(* (field,opt,err,multi,parsing function) *)
let extras = [
  ("Installed", Some parse_installed);
  ("Hold", Some parse_hold);
  ("APT-ID", Some parse_apt_id);
  ("APT-Pin", Some parse_apt_pin);
  ("APT-Candidate", Some parse_candidate);
  ("APT-Automatic", Some parse_automatic);
  ("Section", Some parse_section);
  ("APT-Release", None);
  ]

(* parse the entire file while filtering out unwanted stanzas *)
let rec packages_parser ?(request=false) (req,acc) p =
  (* if strict pin = true we filter packages that are installed and
     apt-candidate *)
  let filter par = 
    let match_field f p =
      try Pef.Packages.parse_bool (f,(Pef.Packages.assoc f p))
      with Not_found -> false
    in
    let inst () = match_field "Installed" par in 
    let candidate () = match_field "APT-Candidate" par in
    ((inst ()) || (candidate ()))
  in
  let archs =
    if Option.is_none req.architecture then []
    else (Option.get req.architecture)::req.architectures 
  in
  match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
  |None -> (req,acc) (* end of file *)
  |Some stanza when request = true -> 
      let req = parse_request_stanza stanza in
      packages_parser (req,acc) p
  |Some stanza when req.strict_pin = true -> begin
    match (Packages.parse_package_stanza (Some(filter)) archs extras stanza) with
    |None -> packages_parser (req,acc) p
    |Some st -> packages_parser (req,st::acc) p
  end
  |Some stanza when req.strict_pin = false -> begin
    match (Packages.parse_package_stanza None archs extras stanza) with
    |None -> assert false (* this is not possible in this branch *)
    |Some st -> packages_parser (req,st::acc) p
  end
  |_ -> assert false
;;

let input_raw_ch ic =
  Format822.parse_from_ch (
    packages_parser ~request:true (default_request,[])
  ) ic
;;

let input_raw file =
  try
    let ch =
      match file with
      |"-" -> IO.input_channel stdin
      |_   -> Input.open_file file
    in
    let l = input_raw_ch ch in
    let _ = Input.close_ch ch in
    l
  with Input.File_empty -> (default_request,[])
;;

let extras_tocudf =
  [
  ("Hold", ("hold", `Bool (Some false)));
  ("APT-Pin", ("apt-pin", `Int None));
  ("APT-ID", ("apt-id", `String None));
  ("APT-Candidate", ("apt-candidate", `Bool (Some false)));
  ("APT-Automatic", ("apt-automatic", `Bool (Some false)));
  ("Section", ("section", `String (Some ""))); 
  ]
;;

let is_installed pkg =
  try
    let _loc = Format822.dummy_loc in
    let v = pkg#get_extra "Installed" in
    Pef.Packages.parse_bool ("Installed",(_loc,v))
  with Not_found -> false

let is_on_hold pkg =
  try
    let _loc = Format822.dummy_loc in
    let v = pkg#get_extra "Hold" in
    (Pef.Packages.parse_bool ("Hold",(_loc,v)))
  with Not_found -> false

let tocudf tables ?(options=Debcudf.default_options) ?(inst=false) pkg =
  let options = { options with Debcudf.extras_opt = extras_tocudf } in
  let pkg = 
    if is_installed pkg then
      let s = 
        if is_on_hold pkg then "hold ok installed" 
        else "install ok installed"
      in
      pkg#add_extra "Status" s
    else pkg
  in
  Debcudf.tocudf tables ~options pkg
;;

(* Only one version of a package can be installed at a given time.
   Hence, when a remove request is issued without version constraint,
   we return (candidate.Cudf.package,None) that designates the only
   package installed.
 *)
let requesttocudf tables universe request =
  let to_cudf (p,v) = (p,Debcudf.get_cudf_version tables (p,v)) in
  let get_candidate (name,constr) =
    try
      List.find
        (fun pkg ->
          try (Cudf.lookup_package_property pkg "apt-candidate") = "true"
          with Not_found -> false)
        (CudfAdd.who_provides universe (name,constr))
    with Not_found ->
      fatal "Package %s does not have a suitable candidate" name
  in
  let select_packages ?(remove=false) l =
    List.map (fun ((n,a),c) ->
      let (name,constr) = Pef.Pefcudf.pefvpkg to_cudf ((n,a),c) in
      if remove then (name,None)
      else begin
        match constr, request.strict_pin with
        |None, false -> (name, None)
        |_, _ -> (name,Some(`Eq,(get_candidate (name,constr)).Cudf.version))
        (* FIXME: when apt will accept version constraints different from `Eq,
           we will need to pass them through. *)
      end
    ) l
  in
  if request.upgrade || request.distupgrade then
    let to_upgrade = function
      |[] ->
        let filter pkg = pkg.Cudf.installed in
        let l = Cudf.get_packages ~filter universe in
        List.map (fun pkg -> (pkg.Cudf.package,None)) l
      |l -> select_packages l
    in
    {Cudf.default_request with
    Cudf.request_id = request.request;
    Cudf.upgrade = to_upgrade request.install;
    Cudf.remove = select_packages ~remove:true request.remove;
    }
  else
    {Cudf.default_request with
    Cudf.request_id = request.request;
    Cudf.install = select_packages request.install;
    Cudf.remove = select_packages ~remove:true request.remove;
    }
;;

