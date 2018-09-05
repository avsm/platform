(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open DocOck
open TestCommon

let read_file cmt =
  match read_cmt (fun name _ -> name) cmt with
  | Not_an_interface ->
      raise (Error(cmt, "not an interface"))
  | Wrong_version ->
      raise (Error(cmt, "wrong OCaml version"))
  | Corrupted ->
      raise (Error(cmt, "corrupted"))
  | Not_a_typedtree ->
      raise (Error(cmt, "not a typedtree"))
  | Not_an_implementation ->
      raise (Error(cmt, "not an implementation"))
  | Ok intf -> intf

let main () =
  let files = get_files "cmt" in
    try
      test read_file (List.rev files);
      exit 0
    with Error(file, msg) ->
      prerr_endline (file ^ ": " ^ msg);
      exit 1

let () = main ()
