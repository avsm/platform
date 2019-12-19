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

let get_architectures native_edsp foreign_edsp native_opt foreign_opt =
  let cmd = "apt-config dump" in
  let arch = ref None in
  let archs = ref [] in
  let aux () =
    let out = Std.input_list (Unix.open_process_in cmd) in
    List.iter (fun s ->
      let key, value =  ExtString.String.split s " " in
      if key = "APT::Architecture" then
        arch := Some(ExtString.String.slice ~first: 1 ~last:(-2) value)
      else if key = "APT::Architectures::" || key = "APT::Architectures" then
        let s = ExtString.String.slice ~first:1 ~last:(-2) value in
        if s <> "" then
          archs := (ExtString.String.slice ~first:1 ~last:(-2) value)::!archs
    ) out;
    debug "Automatically set native as %s and foreign archs as %s" (Option.get !arch) (String.concat "," !archs);
  in
  let (na,fa) =
    match (native_edsp,foreign_edsp),(native_opt,foreign_opt) with
    |(None,[]),(None,None)    -> aux () ; (!arch,List.filter ((<>) (Option.get !arch)) !archs) (* EDSP 0.4 + no options *)
    |(_,l),(Some a,None)      -> (Some a,l)  (* EDSP 0.5 + overrride options *)
    |(Some a,_),(None,Some l) -> (Some a,l)  (* EDSP 0.5 + overrride options *)
    |(_,_),(Some a,Some l)    -> (Some a,l)  (* EDSP 0.5 / 0.4 + overrride options *)
    |(Some a,l),(None,None)   -> (Some a,l)  (* EDSP 0.5 + no options *)
    |(None,[]),(None,_)       -> fatal "Native arch is missing while Foregin archs are specified"
    |_,_ -> fatal "Unable to compute native and foreign arch information"
  in
  begin match (na,fa) with
  |(Some a, l) when (native_edsp,foreign_edsp) = (None,[]) || (native_edsp,foreign_edsp) = (Some a, l) ->
      notice "Setting Native Architecture to %s and Foreign Architectures to %s" a (String.concat "," l)
  |(Some a, l) ->
      info "Overriding EDSP. Setting Native Architecture to %s and Foreign Architectures to %s" a (String.concat "," l)
  |_ -> fatal "Error Setting Native Architecture"
  end;
  (na,fa)
;;

let pp_versions_table fmt (from_cudf, pkglist) =
  List.iter (fun pkg ->
    let (p,v) = from_cudf (pkg.Cudf.package,pkg.Cudf.version) in
    Format.fprintf fmt "%s=%d=%s@." p pkg.Cudf.version v
  ) pkglist

(* exit code policy : 
Exit codes 0-63 indicate a normal termination of the program, codes
64-127 indicate abnormal termination of the program (such as parse
errors, I/O errors).

In case of normal program termination:
- exit code 0 indicates that all foreground packages are found
  installable;
- exit code 1 indicates that at least one foreground package is found
  uninstallable.
*)
let if_application ?(alternatives=[]) filename main =
  let normalize f = 
    let bf = Filename.basename f in
    try
      if String.ends_with bf ".p.byte" then
        String.slice ~last:(String.find bf ".p.byte") bf
      else if String.ends_with bf ".p.native" then
        String.slice ~last:(String.find bf ".p.native") bf
      else
        Filename.chop_extension bf
    with Invalid_argument _ -> bf
  in
  let names = List.map normalize (filename::alternatives) in
  let invoked_as = normalize Sys.argv.(0) in
  if List.exists ((=) invoked_as) names then 
    try (if main () = 0 then exit(0) else exit(1)) with
      |Unix.Unix_error(err, _, arg) -> begin
          Printf.eprintf "%s %s" (Unix.error_message err) arg;
          Pervasives.exit(64) end
      |exn -> begin
          Printexc.print_backtrace stderr; 
          Printf.eprintf "The applications raised this exception : ";
          Printf.eprintf "%s\n" (Printexc.to_string exn);
          Pervasives.exit(64) end
  else begin
    Printf.eprintf "you are using %s as a module and not as an executable\n" Sys.argv.(0);
    Printf.eprintf "%s can be run as an exactable if named : %s\n" Sys.argv.(0) 
    (ExtString.String.join " , " names)
  end
