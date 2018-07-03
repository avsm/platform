(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Cmdliner
open Rresult
open Bos

let browser =
  let env = Arg.env_var "BROWSER" in
  let browser =
    let parse s = match OS.Env.cmd s with
    | Error (`Msg e) -> `Error e
    | Ok cmd -> `Ok (Some cmd)
    in
    let pp ppf = function
    | None -> Format.fprintf ppf "OS specific fallback"
    | Some cmd -> Cmd.pp ppf cmd
    in
    parse, pp
  in
  let doc =
    "The WWW browser command $(docv) to use. The value may be interpreted
     and massaged depending on the OS. On Darwin it is sufficient to
     specify the name of a known existing browser; if absent the
     application that handles the 'http' URI scheme is used.
     Complaints and help to improve support are gladly taken
     at $(i,%%PKG_ISSUES%%/1)."
  in
  let docv = "CMD" in
  Arg.(value & opt browser None & info ["b"; "browser"] ~env ~doc ~docv)

let prefix =
  let doc = "Reload the first browser tab which has the URI as a prefix
             (rather than the exact URI)."
  in
  Arg.(value & flag & info ["p"; "prefix"] ~doc)

let background =
  let doc = "Reload URI but keep the browser application in the background."in
  Arg.(value & flag & info ["g"; "background"] ~doc)

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
