(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2011 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
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

(** Representation of a debian release files *) 

open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

type release = {
  fname : string;
  origin : string;
  label : string;
  suite : string;
  version: string;
  codename : string;
  date: string;
  architecture: string;
  component : string;
  notauto: bool;
  autoup: bool;
  description: string;
  md5sums: (string * string * string) list;
  sha1: (string * string * string) list;
  sha256: (string * string * string) list
}

let default_release = {
  fname = "";
  origin = "";
  label = "";
  suite = "";
  version = "";
  codename = "";
  date = "";
  architecture = "";
  component = "";
  notauto = false;
  autoup = false;
  description = "";
  md5sums = [];
  sha1 = [];
  sha256 = []
}

let parse_release_stanza fname par = {
  fname = Filename.basename (fname);
  origin = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Origin" par;
  label = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Label" par;
  suite = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Suite" par;
  version = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Version" par;
  codename = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Codename" par;
  date = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Date" par;
  architecture = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Architectures" par;
  component = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Components" par;
  notauto = Pef.Packages.parse_s ~default:false Pef.Packages.parse_bool "NotAutomatic" par;
  autoup = Pef.Packages.parse_s ~default:false Pef.Packages.parse_bool "ButAutomaticUpgrades" par;
  description = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "Description" par;
  md5sums = [];
  sha1 = [];
  sha256 = []
}

let release_parser stanza_parser fname p =
  match
  Format822_parser.doc_822_sign 
    Format822_lexer.token_822 p.Format822.lexbuf 
  with 
  |Some st -> Some (stanza_parser fname st)
  |None -> None

let parse_release_in fname ic =
  Format822.parse_from_ch (release_parser parse_release_stanza fname) ic
