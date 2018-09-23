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

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let gzip_open_file file =
#ifdef HASZIP
  let ch = Gzip.open_in file in
  let input_char ch = try Gzip.input_char ch with End_of_file -> raise IO.No_more_input in
  let read ch = try Gzip.input ch with End_of_file -> raise IO.No_more_input in
  IO.create_in
  ~read:(fun () -> input_char ch)
  ~input:(read ch)
  ~close:(fun () -> Gzip.close_in ch)
#else
    fatal "gzip not supported. re-configure with --with-zip"
#endif
;;

let bzip_open_file file =
#ifdef HASBZ2
  (* workaround to avoid segfault :
   * http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=602170 *)
  let _ = Bz2.version in 
  let s = " " in
  let ch = Bz2.open_in (open_in file) in
  let input_char ch = 
    try ignore (Bz2.read ch s 0 1) ; s.[0]
    with End_of_file -> raise IO.No_more_input
  in
  let read ch s pos len =
    try Bz2.read ch (Bytes.unsafe_to_string s) pos len 
    with End_of_file -> raise IO.No_more_input
  in
  IO.create_in
  ~read:(fun () -> input_char ch)
  ~input:(read ch)
  ~close:(fun () -> Bz2.close_in ch)
#else
    fatal "bzip not supported. re-configure with --with-bz2"
#endif
;;

let std_open_file file = IO.input_channel (open_in file)
let open_ch ch = IO.input_channel ch
let close_ch ch = IO.close_in ch

exception File_empty

let open_file file =
  if not (Sys.file_exists file) then
    fatal "Input file %s does not exists." file
  else if (Unix.stat file).Unix.st_size = 0 then (
    warning "Input file %s is empty" file;
    raise File_empty)
  else begin
    let openfun = try
        let ch = open_in file in
        let openfun = match input_byte ch with
          (* gzip magic is 0x1f 0x8b *)
          | 0x1f -> (match input_byte ch with
              | 0x8b -> gzip_open_file
              | _ -> std_open_file)
          (* bz2 magic is "BZh" *)
          | 0x42 -> (match input_byte ch with
              | 0x5a -> (match input_byte ch with
                  | 0x68 -> bzip_open_file
                  | _ -> std_open_file)
              | _ -> std_open_file)
          (* xz magic is 0xfd "7zXZ" *)
          | 0xfd -> (match input_byte ch with
              | 0x37 -> (match input_byte ch with
                  | 0x7a -> (match input_byte ch with
                      | 0x58 -> (match input_byte ch with
                          | 0x5a -> fatal "xz not supported."
                          | _ -> std_open_file)
                      | _ -> std_open_file)
                  | _ -> std_open_file)
              | _ -> std_open_file)
          | _ -> std_open_file
        in
        close_in ch;
        openfun
      with End_of_file -> std_open_file 
    in
    openfun file
  end
;;

let parse_uri s =
  let url = Url.of_string s in
  let path =  url.Url.path
  in
  (url.Url.scheme,(None,None,None,None,path),None)

let guess_format urilist =
  match List.flatten urilist with
  |uri::_ -> let (p,_,_) = parse_uri uri in p
  |_ -> fatal "Impossible to guess input format"
