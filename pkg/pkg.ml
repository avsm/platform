#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let jsoo = Conf.with_pkg "js_of_ocaml"
let cmdliner = Conf.with_pkg "cmdliner"
let fmt = Conf.with_pkg "fmt"
let lwt = Conf.with_pkg "lwt"

let () =
  Pkg.describe "logs" @@ fun c ->
  let jsoo = Conf.value c jsoo in
  let cmdliner = Conf.value c cmdliner in
  let fmt = Conf.value c fmt in
  let lwt = Conf.value c lwt in
  Ok [ Pkg.mllib "src/logs.mllib";
       Pkg.mllib ~cond:fmt "src/logs_fmt.mllib";
       Pkg.mllib ~cond:jsoo "src/logs_browser.mllib";
       Pkg.mllib ~cond:cmdliner "src/logs_cli.mllib";
       Pkg.mllib ~cond:lwt "src/logs_lwt.mllib";
       Pkg.mllib ~cond:fmt ~api:[] "src/logs_top.mllib";
       Pkg.lib "src/logs_top_init.ml";
       Pkg.doc "test/tool.ml";
       Pkg.doc "test/tags.ml"; ]
