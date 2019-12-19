(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2010,2011 Ralf Treinen <ralf.treinen@pps.jussieu.fr>        *)
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

module Pcre = Re_pcre
open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

(* first column of /usr/share/dpkg/cputable *)
(* the line numbers correspond to the line numbers in /usr/share/dpkg/cputable
 * to be quickly able to find changes *)
let cpulist = ref [
(* lines 17..23 *)  "i386"; "ia64"; "alpha"; "amd64"; "armeb"; "arm"; "arm64";
(* lines 24..29 *)  "avr32"; "hppa"; "m32r"; "m68k"; "mips"; "mipsel";
(* lines 30..34 *)  "mips64"; "mips64el"; "or1k"; "powerpc"; "powerpcel";
(* lines 35..41 *)  "ppc64"; "ppc64el"; "s390"; "s390x"; "sh3"; "sh3eb"; "sh4";
(* lines 42..44 *)  "sh4eb"; "sparc"; "sparc64" ]

(* from /usr/share/dpkg/triplettable
 *
 * the line numbers correspond to the line numbers in
 * /usr/share/dpkg/triplettable to be quickly able to find changes
 *
 *   debian triplet (abi,os,cpu)      debian arch *)
let triplettable = ref [
  (("uclibceabi","linux","arm"),     "uclibc-linux-armel"); (* line 6  *)
  (("uclibc","linux","<cpu>"),       "uclibc-linux-<cpu>");
  (("musleabihf","linux","arm"),     "musl-linux-armhf");
  (("musl","linux","<cpu>"),         "musl-linux-<cpu>");
  (("gnueabihf","linux","arm"),      "armhf");              (* line 10 *)
  (("gnueabi","linux","arm"),        "armel");
  (("gnuabin32","linux","mips64el"), "mipsn32el");
  (("gnuabin32","linux","mips64"),   "mipsn32");
  (("gnuabi64","linux","mips64el"),  "mips64el");
  (("gnuabi64","linux","mips64"),    "mips64");             (* line 15 *)
  (("gnuspe","linux","powerpc"),     "powerpcspe");
  (("gnux32","linux","amd64"),       "x32");
  (("gnuhardened1","linux","<cpu>"), "hardened1-linux-<cpu>");
  (("gnu","linux","<cpu>"),          "<cpu>");
  (("gnu","kfreebsd","<cpu>"),       "kfreebsd-<cpu>");
  (("gnu","knetbsd","<cpu>"),        "knetbsd-<cpu>");      (* line 20 *)
  (("gnu","kopensolaris","<cpu>"),   "kopensolaris-<cpu>");
  (("gnu","hurd","<cpu>"),           "hurd-<cpu>");
  (("bsd","dragonflybsd","<cpu>"),   "dragonflybsd-<cpu>");
  (("bsd","freebsd","<cpu>"),        "freebsd-<cpu>");
  (("bsd","openbsd","<cpu>"),        "openbsd-<cpu>");      (* line 25 *)
  (("bsd","netbsd","<cpu>"),         "netbsd-<cpu>");
  (("bsd","darwin","<cpu>"),         "darwin-<cpu>");
  (("sysv","solaris","<cpu>"),       "solaris-<cpu>");
  (("uclibceabi","uclinux","arm"),   "uclinux-armel");
  (("uclibc","uclinux","<cpu>"),     "uclinux-<cpu>");      (* line 30 *)
  (("tos","mint","m68k"),            "mint-m68k");
  (("gnu","linux","<cpu>"),          "linux-<cpu>") (* this entry is not from /usr/share/dpkg/triplettable *)
  (* the "linux-" prefix is commented in scripts/Dpkg/Arch.pm with "XXX: Might disappear in the future, not sure yet." *)
]

let debarch_to_debtriplet = Hashtbl.create ((List.length !triplettable)*(List.length !cpulist))
let triplettable_done = ref false

let mangle_cpu_placeholder ((abi,os,cpu),debarch) =
  if cpu = "<cpu>" then begin
    List.iter (fun c ->
        let dt = (abi,os,c) in
        let _,da = String.replace ~str:debarch ~sub:"<cpu>" ~by:c in
        Hashtbl.replace debarch_to_debtriplet da dt
      ) !cpulist
  end else begin
    Hashtbl.replace debarch_to_debtriplet debarch (abi,os,cpu)
  end
;;

let read_triplettable ?(ttfile=None) ?(ctfile=None) () =
  if !triplettable_done && ttfile = None && ctfile = None then () else begin
    (* if cputable file was given, overwrite built-in table *)
    begin match ctfile with
      | Some fn -> begin
          cpulist := [];
          let ic = open_in fn in
          (* to stay most compatible with dpkg, it would be best to use its
           * regex from from scripts/Dpkg/Arch.pm to parse this file.
           * Unfortunately Re.pcre doesnt support look-ahead/look-behind
           * assertions *)
          let aux line =
            if line.[0] = '#' || not (String.contains line '\t') then ()
            else begin
              let spacei = String.index line '\t' in
              let cpu = String.sub line 0 spacei in
              if not (List.mem cpu !cpulist) then cpulist := cpu::!cpulist;
            end;
          in
          List.iter aux (Std.input_list ic);
          close_in ic;
        end
      | None -> () end;
    (* if triplettable was given, overwrite built-in table, otherwise parse
     * built-in table *)
    begin match ttfile with
      | Some fn -> begin
          (* this is an implicit assumption of dpkg *)
          mangle_cpu_placeholder (("gnu","linux","<cpu>"), "linux-<cpu>");
          let ic = open_in fn in
          (* to stay most compatible with dpkg, it would be best to use its
           * regex from from scripts/Dpkg/Arch.pm to parse this file.
           * Unfortunately Re.pcre doesnt support look-ahead/look-behind
           * assertions *)
          let aux line =
            if line.[0] = '#' || not (String.contains line '\t') then ()
            else begin
              let spaceli = String.index line '\t' in
              let spaceri = String.rindex line '\t' in
              let debtriplet = String.sub line 0 spaceli in
              let debarch = String.sub line (spaceri+1) ((String.length line)-spaceri-1) in
              match String.nsplit debtriplet "-" with
              | [abi;os;cpu] -> mangle_cpu_placeholder ((abi,os,cpu),debarch)
              | _ -> fatal "Cannot parse debtriplet: %s" debtriplet
            end
          in
          List.iter aux (Std.input_list ic);
          close_in ic;
        end
      | None -> begin
          List.iter mangle_cpu_placeholder !triplettable;
        end
    end;
    triplettable_done := true;
  end
;;

let src_matches_arch alias real =
  read_triplettable ();
  if alias=real || alias="any" || alias="all" then true else begin
    let real = Hashtbl.find_option debarch_to_debtriplet real in
    let alias = match String.nsplit alias "-" with
      | ["any";os;cpu] ->  Some ("any",os,cpu)
      | [abi;"any";cpu] -> Some (abi,"any",cpu)
      | [abi;os;"any"] ->  Some (abi,os,"any")
      | ["any";cpu] ->     Some ("any","any",cpu)
      | [os;"any"] ->      Some ("any",os,"any")
      | ["any"] ->         Some ("any","any","any")
      | _ -> begin
          (* only look up in the table if none of the parts is "any" *)
          Hashtbl.find_option debarch_to_debtriplet alias
        end
    in
    match real,alias with
    | Some (r1,r2,r3), Some (a1,a2,a3) ->
      ((a1=r1 || a1="any") && (a2=r2 || a2="any") && (a3=r3 || a3="any"))
    | _ -> false
  end
;;
