(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let exec = Filename.basename Sys.executable_name
let log_err f = Format.eprintf ("%s: " ^^ f ^^ "@?") exec

let urify u = (* Detects if u is simply a file path *)
  try match Sys.file_exists u with
  | false -> u
  | true ->
      let u =
        if not (Filename.is_relative u) then u else
        Filename.concat (Sys.getcwd ()) u
      in
      Format.sprintf "file://%s" u
  with Sys_error _ -> u

let browse background prefix browser uris =
  let rec loop = function
  | [] -> 0
  | uri :: uris ->
      let uri = urify uri in
      match Webbrowser.reload ~background ~prefix ?browser uri with
      | Error (`Msg e) -> log_err "%s" e; 1
      | Ok () -> loop uris
  in
  loop uris

(* Command line interface *)

open Cmdliner

let uris =
  let doc = "URI to open or reload. If URI is an existing file path
             a corresponding file:// URI is opened."
  in
  Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"URI")

let doc = "Open and reload URIs in web browsers"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,$(tname)) command opens or reloads URIs specified
        on the command line.";
    `S "BUGS";
    `P "This program is distributed with the OCaml webbrowser library.
        See $(i,%%PKG_HOMEPAGE%%) for contact information."; ]

let cmd =
  let info = Term.info "browse" ~doc ~man in
  let t = Term.(const browse $ Webbrowser_cli.background $
                Webbrowser_cli.prefix $ Webbrowser_cli.browser $ uris)
  in
  (t, info)

let () = match Term.eval cmd with
| `Error _ -> exit 1
| `Ok v -> exit v
| _ -> exit 0

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
