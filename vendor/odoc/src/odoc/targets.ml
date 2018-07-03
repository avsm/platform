(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
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

open StdLabels

let for_compile_step ~output input =
  let name =
    Fs.File.to_string input
    |> Filename.basename
    |> Filename.chop_extension
    |> fun s -> s ^ ".odoc"
  in
  [Fs.File.create ~directory:output ~name]

let unit ~env ~output:_root_dir input =
  let unit = Compilation_unit.load input in
  let env = Env.build env (`Unit unit) in
  let odoctree = Xref.resolve (Env.resolver env) unit in
  let odoctree = Xref.expand (Env.expander env) odoctree in
  let root = Compilation_unit.root odoctree in
  let package = root.package in
  let targets = Html.List_targets.unit ~package odoctree in
  (* CR-someday trefis: have [List_targets] return a tree instead of
     postprocessing. *)
  List.map targets ~f:(fun path ->
    let directory = Fs.Directory.of_string path in
    Fs.File.create ~directory ~name:"index.html"
  )

let index ~output:_ _ = []
