#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "webbrowser" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/webbrowser.mllib";
       Pkg.mllib ~cond:cmdliner "src/webbrowser_cli.mllib";
       Pkg.bin ~cond:cmdliner "test/browse"; ]
