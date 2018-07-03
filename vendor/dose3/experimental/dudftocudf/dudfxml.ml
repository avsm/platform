(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

module L = Xml.LazyList 

(* ========================================= *)

(** all elements any* are distribution specific *)
module XmlDudf = struct 
  type dudfxml = {
    timestamp : string ;
    uid : string ;
    distribution : string ;
    installer : dudfinstaller ;
    metaInstaller : dudfinstaller ;
    problem : dudfproblem ;
    outcome : dudfoutcome ;
    comment : Xml.xml list; (** any* *)
  }
  and dudfinstaller = {
    name : string ;
    version : string ;
  }
  and dudfproblem = {
    packageStatus : dudfstatus;
    packageUniverse : dudfuniverse list;
    action : string ;
    desiderata : string ;
  }
  and dudfstatus = {
    st_installer : Xml.xml list; (** any* *)
    st_metaInstaller : Xml.xml list (** any* *)
  }
  and attribute = string option 
  and dudfuniverse =
    (attribute * attribute * attribute * Xml.attribute list * Xml.xml list) (** any* *)
  and dudfoutcome =
    |Success of dudfstatus
    |Failure of string

  let dummydudfprob = { 
    packageStatus = { 
      st_installer = [] ;
      st_metaInstaller = []
    };
    packageUniverse = [];
    action = "";
    desiderata = "";
  }

  let dummydudfinst = {name = ""; version = ""}

  let dummydudf = {
    timestamp = "";
    uid = "";
    distribution = "";
    installer = dummydudfinst ;
    metaInstaller = dummydudfinst ;
    problem = dummydudfprob ;
    outcome = Failure "" ;
    comment = []
  }
end

(* ========================================= *)

let _ = Curl.global_init Curl.CURLINIT_GLOBALALL ;;

let curlget ch url =
  let writer conn accum data =
    Buffer.add_string accum data;
    String.length data
  in
  let save ch content = Buffer.output_buffer ch content in
  let code_from h =
    let res = Curl.getinfo h Curl.CURLINFO_RESPONSE_CODE in
    match res with
    |Curl.CURLINFO_Long(i) -> i
    |_ -> failwith "Expected long value for HTTP code"
  in
  let errorBuffer = ref "" in
  let print_info connection =
    info "Download %s (time : %f)"
      (Curl.get_effectiveurl connection)
      (Curl.get_totaltime connection)
  in
  begin try
    let connection = Curl.init () in
    let result = Buffer.create 16384 in
    Curl.set_errorbuffer connection errorBuffer;
    Curl.set_writefunction connection (writer connection result);
    Curl.set_followlocation connection true;
    Curl.set_url connection url;
    Curl.perform connection;
    begin match code_from connection with
    |200 ->
        begin
          print_info connection;
          save ch result; flush ch;
          Curl.cleanup connection
        end
    |code -> raise (Failure (Printf.sprintf "HTTP %d" code))
    end;
    Curl.global_cleanup ()
    with
    |Curl.CurlException (reason, code, str) ->
        (warning "Error while downloading %s\n%s" url !errorBuffer;
         Curl.global_cleanup ();
         raise (Failure "")
         )
    |Failure s ->
        (warning "Caught exception while downloading %s\n%s" url s ;
        Curl.global_cleanup ();
        raise (Failure "")
        )
  end

type compression = Bz2 | Cz

let pkgget ?(cachedir=".dudf") ?compression url =
  if not(Sys.file_exists cachedir) then Unix.mkdir cachedir 0o777 ;
  let filename =
    let fname = Digest.to_hex (Digest.string url) in
    let f = 
      match compression with
      |Some Bz2 when not(Filename.check_suffix fname "bz2") -> (fname^".bz2")
      |Some Cz when not(Filename.check_suffix fname "cz") -> (fname^".cz")
      |_ -> fname
    in
    Filename.concat cachedir f
  in
  if not(Sys.file_exists filename) then begin
    let outch = open_out filename in
    try curlget outch url
    with Failure _ -> begin close_out outch; Unix.unlink filename; exit 1 end ;
    close_out outch;
  end;
  if (Unix.stat filename).Unix.st_size = 0 then ""
  else begin
    let inch = Input.open_file filename in
    let s = IO.read_all inch in
    Input.close_ch inch;
    s
  end
;;

(* ========================================= *)

open XmlDudf

let attrib_opt x att = try Some(Xml.attrib x att) with Xml.Not_attribute _ -> None

let get_children node tag =
  List.find (fun n ->
    try (Xml.tag n) = tag with Xml.Not_element _ -> false
  ) (Xml.children node)

let content_to_string node = Xml.fold (fun a x -> a^(Xml.to_string x)) "" node

let parse input_file =
  let xdata = XmlParser.parse_ch (Input.open_file input_file) in
  let dudfstatus node =
    { st_installer = 
      (try Xml.children (get_children node "installer") with Not_found -> []);
      st_metaInstaller = 
        (try Xml.children (get_children node "meta-installer") with Not_found -> [])
    }
  in
  let dudfuniverse node =
    Xml.fold (fun universe n ->
      info "   %s" (Xml.tag n);
      if (Xml.tag n) = "package-list" then begin
        let (fmt,filename,url,l) = 
          List.fold_left (fun (fmt,filename,url,l) -> function
            |("format",v) -> (Some v, filename,url,l)
            |("filename",v) -> (fmt,Some v,url,l)
            |("url",v) -> (fmt,filename,Some v,l)
            |a -> (fmt,filename,url,a::l)
          ) (None,None,None,[]) (Xml.attribs n)
        in
        (fmt,filename,url,l,Xml.children n)::universe
      end
      else 
        fatal "Warning : Unknown element \"%s\"" (Xml.tag n)
    ) [] node
  in
  let dudfoutcome node =
    match Xml.attrib node "result" with
    |"success" ->
        begin try Success (dudfstatus (get_children node "package-status"))
        with Not_found -> (
          warning "Missing installer status on success"; 
          Success { st_installer = [] ; st_metaInstaller = [] }
        ) end
    |"failure" -> Failure (content_to_string node)
    |s -> fatal "Unknown result \"%s\"" s
  in
  let dudfproblem node =
    Xml.fold (fun dudf node ->
      info "  %s" (Xml.tag node);
      match Xml.tag node with
      |"package-status" -> {dudf with packageStatus = dudfstatus node }
      |"package-universe" -> {dudf with packageUniverse = dudfuniverse node }
      |"action" -> {dudf with action = content_to_string node }
      |"desiderata" -> {dudf with desiderata = content_to_string node }
      |s -> (warning "Unknown element \"%s\"" s ; dudf)
    ) dummydudfprob node
  in
  let dudfinstaller node =
    Xml.fold (fun i node ->
      info "  %s" (Xml.tag node);
      match Xml.tag node with
      |"name" -> {i  with name = content_to_string node }
      |"version" -> { i with version = content_to_string node }
      |s -> (warning "Unknown element \"%s\"" s ; i)
    ) dummydudfinst node
  in
  Xml.fold (fun dudf node -> 
    info " %s" (Xml.tag node);
    match Xml.tag node with
    |"distribution" -> { dudf with distribution = content_to_string node }
    |"timestamp" -> {dudf with timestamp = content_to_string node}
    |"uid" -> {dudf with uid = content_to_string node}
    |"installer" -> {dudf with installer = dudfinstaller node}
    |"meta-installer" -> {dudf with metaInstaller = dudfinstaller node}
    |"problem" -> {dudf with problem = dudfproblem node }
    |"outcome" -> {dudf with outcome = dudfoutcome node }
    |"comment" -> {dudf with comment = Xml.children node }
    |s -> (warning "Warning : Unknown elemenet \"%s\"" s ; dudf)
  ) dummydudf xdata
