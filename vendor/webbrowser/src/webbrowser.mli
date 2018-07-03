(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Open and reload URIs in web browsers.

    {2 Caveat}

    Trying to load and reload URIs from the command line in a
    consistant manner across browsers and operating systems seems to
    be a hopeless endeavour.

    In particular the reload strategy mentioned below—useful to
    edit websites, write API documentation or web applications with
    {!Js_of_ocaml}—is an indication of what {e should} be done for
    what is believed to be the best user experience. However do not
    expect this work in all contexts (currently it only fully works
    with Chrome on Darwin and it is not even glitchless there).

    If you know how to improve or extend the support for particular
    browsers and platforms get in touch
    {{:https://github.com/dbuenzli/webbrowser/issues/1}here}.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(**  {1 Browser} *)

open Bos
open Rresult

val reload :
  ?background:bool -> ?prefix:bool -> ?browser:Cmd.t -> string ->
  (unit, R.msg) result
(** [reload ~background ~prefix ~browser uri] tries to reload or open
    the URI [uri] or an URI prefixed by [uri] if prefix is [true]
    (defaults to [false]) in browser [browser] (if unspecified a
    platform dependent procedure is invoked to determine the user
    preference).

    If [background] is [true] (defaults to [false]), the browser
    application should be kept in the background, only the reload
    should occur. If [false] the browser application and reloaded
    window should be brought into focus.

    The reload should always lead to the reload of a single browser
    tab found as follows.
    {ol
    {- Repeat from the frontmost browser window to the backmost one until
       a tab to reload is found:
       {ol
       {- If the window's current tab's URI is [uri] (or is prefixed by [uri]
          when  [prefix] is [true]), reload this tab.}
       {- If the window has one or more tab whose URI is [uri] (or is prefixed
          by [uri] when [prefix] is [true]), pick the left most one, make it
         current in the window and reload it.}}}
     {- If no tab was found, get the frontmost window. If the current tab
        has no URI, use that tab with [uri] otherwise create a new tab
        with [uri] and make it current for the window.}} *)

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
