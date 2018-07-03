(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos
open Rresult
open Astring

let err_no_exec () =
  R.error_msg "No browser executable specified or found. Help wanted \
               to expand support, see %%PKG_ISSUES%%/1"

(* Generic reload via direct command invocation *)

let cmd_reload cmd ~uri =
  OS.Cmd.must_exist cmd >>= fun cmd -> OS.Cmd.run Cmd.(cmd % uri)

(* Darwin reload via JavaScript Automation -- n.b. only since 10.10. *)

let run_jxa_script script args out =
  let c = Cmd.(v "osascript" % "-l" % "JavaScript" % "-" %% args) in
  OS.Cmd.must_exist c >>= fun c -> OS.Cmd.(in_string script |> run_io c |> out)

let pp_using_jxa ppf browser =
  Fmt.pf ppf "@[JavaScript@ for@ Automation@ with %S@]" browser

let darwin_jxa_reload ~background ~prefix ~appid ~uri =
  let script =
"
function is_equal (s0, s1) { return s0 === s1; }
function is_prefix (prefix, str)
{ return str && str.lastIndexOf (prefix, 0) === 0; }

function reload (background, pred, appid, uri)
{
  var app = Application (appid);
  var api =
      (appid === 'com.google.chrome') ?
      { win_create : function (uri)
        { var win = app.Window (); win.make (); win.activeTab.url = uri; },
        tab_active : function (w) { return w.activeTab; },
        tab_activate : function (w, t, tidx) { w.activeTabIndex = tidx + 1;},
        tab_reload : function (t) { app.reload (t); }} :
      (appid === 'com.apple.safari') ?
      { win_create : function (uri)
        { var win = app.Document (); win.make (); win.url = uri; },
        tab_active : function (w) { return w.currentTab; },
        tab_activate : function (w, t, tidx) { w.currentTab = t; },
        tab_reload : function (t)
        { app.doJavaScript ('window.location.reload()', { in : t }); } } :
      null;

  function activate_window (w) { w.index = 1; }
  function new_tab (uri)
  {
    if (app.windows.length == 0 || !app.windows[0].visible())
    { api.win_create (uri); }
    else {
      var current = api.tab_active (app.windows[0]).url ();
      if (is_equal (current, '') ||
          is_equal (current, 'chrome://newtab/') ||
          is_equal (current, undefined))
        api.tab_active (app.windows[0]).url = uri;
      else
        app.windows[0].tabs.push(app.Tab ({ url : uri }));
    }
  }

 function reload_and_activate_existing_tab (pred, uri)
  // returns true if a tab was found
  {
    for (var w = 0; w < app.windows.length; w++)
    {
      var win = app.windows[w];
      if (win.visible () /* For Safari */) {
        var tab = api.tab_active (win);
        if (pred (uri, tab.url()))
        { api.tab_reload (tab); activate_window (win); return true; }

        for (var t = 0; t < win.tabs.length; t++)
        {
          tab = win.tabs[t];
          if (pred (uri, tab.url ()))
          { api.tab_reload (tab); api.tab_activate (win, tab, t);
            activate_window (win);
            return true; }
        }
      }
    }
    return false;
  }

  if (!background) app.activate();
  if (!reload_and_activate_existing_tab (pred, uri)) new_tab (uri);
}

function run(argv)
{
  var background = (argv[0] == 'true');
  var pred = (argv[1] == 'true') ? is_prefix : is_equal;
  var appid = argv[2];
  var uri = argv[3];
  reload (background, pred, appid, uri);
}
"
  in
  run_jxa_script script
    Cmd.(v (string_of_bool background) % string_of_bool prefix % appid % uri)
    OS.Cmd.to_null

(* Darwin reload via open(1) *)

let darwin_open_reload ~background ~is_appid ~browser ~uri =
  let open_cmd = Cmd.(v "open" %% on background (v "-g")) in
  let app = Cmd.(v (if is_appid then "-b" else "-a") % browser) in
  OS.Cmd.run Cmd.(open_cmd %% app % uri)

(* Darwin reload *)

let darwin_default_browser () =
  let script =
    "ObjC.import('CoreServices');
     var h = $.LSCopyDefaultHandlerForURLScheme($('http'));
     $.CFStringGetCStringPtr (h, 0);"
  in
  run_jxa_script script Cmd.empty OS.Cmd.to_string

let darwin_browser = function
| None -> darwin_default_browser () >>= fun id -> Ok (`Appid id)
| Some browser ->
    match Cmd.line_exec browser with
    | None -> err_no_exec ()
    | Some exec ->
        match String.Ascii.lowercase exec with
        | ("firefox" | "org.mozilla.firefox") ->
            Ok (`Appid "org.mozilla.firefox")
        | ("safari" | "com.apple.safari") ->
            Ok (`Appid "com.apple.safari")
        | ("google chrome" | "chrome" | "com.google.chrome") ->
            Ok (`Appid "com.google.chrome")
        | ("opera" | "com.operasoftware.opera") ->
            Ok (`Appid "com.operasoftware.opera")
        | exec when Cmd.line_args browser = [] &&
                    not (String.is_infix ~affix:Fpath.dir_sep exec) ->
            Ok (`App exec)
        | _ ->
            Ok (`Cmd browser)

let darwin_reload ~background ~prefix ~browser ~uri =
  darwin_browser browser
  >>= function
  | `Appid ("com.google.chrome" | "com.apple.safari" as appid) ->
      darwin_jxa_reload ~background ~prefix ~appid ~uri
  | `Appid browser ->
      darwin_open_reload ~background ~is_appid:true ~browser ~uri
  | `App browser ->
      darwin_open_reload ~background ~is_appid:false ~browser ~uri
  | `Cmd line ->
      cmd_reload line ~uri

(* Linux reload via xdg-open(1) *)

let try_linux_xdg_open_reload ~uri =
  let cmd = Cmd.(v "xdg-open" % uri) in
  OS.Cmd.exists cmd >>= function
  | false -> err_no_exec ()
  | true -> OS.Cmd.run cmd

(* Linux reload *)

let linux_reload ~background ~prefix ~browser ~uri = match browser with
| None -> try_linux_xdg_open_reload ~uri
| Some cmd -> cmd_reload cmd ~uri

(* Unknown platform reload *)

let unknown_reload os ~background ~prefix ~browser ~uri = match browser with
| None -> err_no_exec ()
| Some cmd -> cmd_reload cmd ~uri

(* Reload *)

let os () =
  if Sys.win32 then Ok "Windows" else
  OS.Cmd.(run_out Cmd.(v "uname" % "-s") |> to_string)

let reload ?(background = false) ?(prefix = false) ?browser uri =
  begin
    os () >>= function
    | "Darwin" -> darwin_reload ~background ~prefix ~browser ~uri
    | "Linux" -> linux_reload ~background ~prefix ~browser ~uri
    | os -> unknown_reload os ~background ~prefix ~browser ~uri
  end
  |> R.reword_error_msg (fun msg -> R.msgf "browser reload: %s" msg)

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
