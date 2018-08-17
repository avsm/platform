(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support

open Markup__Common
open Markup
module Kstream = Markup__Kstream

let start_element name = `Start_element (("", name), [])

let ok = wrong_k "failed"

type dom =
  | Text of string
  | Element of string * dom list

let tests = [
  ("utility.content" >:: fun _ ->
    "<?xml version='1.0'?><!DOCTYPE html><!--blah--><p>foo</p><?bar baz?>"
    |> string
    |> parse_xml
    |> signals
    |> content
    |> write_xml
    |> to_string
    |> assert_equal "<p>foo</p>");

  ("utility.strings_to_bytes" >:: fun _ ->
    ["foo"; "bar"]
    |> Kstream.of_list
    |> Markup__Utility.strings_to_bytes
    |> fun s ->
      Kstream.to_list s ok (assert_equal ['f'; 'o'; 'o'; 'b'; 'a'; 'r']));

  ("utility.tree" >:: fun _ ->
    [start_element "a";
     `Comment "blah";
     `Text ["foo"];
     start_element "b";
     `Text ["bar"];
     `End_element;
     `Text ["baz"];
     `End_element]
    |> of_list
    |> tree
      ~text:(fun ss -> Text (String.concat "" ss))
      ~element:(fun (_, name) _ children -> Element (name, children))
    |> assert_equal
      (Some
        (Element ("a",
          [Text "foo"; Element ("b", [Text "bar"]); Text "baz"]))));

  ("utility.tree.empty" >:: fun _ ->
    []
    |> of_list
    |> tree ~text:ignore ~element:(fun _ _ _ -> ())
    |> assert_equal None);

  ("utility.tree.reread" >:: fun _ ->
    let signals =
      [start_element "p";
       `End_element;
       start_element "p";
       `End_element]
      |> of_list
    in

    tree signals ~text:ignore ~element:(fun _ _ _ -> ()) |> ignore;

    signals |> to_list |> List.length |> assert_equal 2);

  ("utility.from_tree" >:: fun _ ->
    let dom =
      Element ("p", [Text "foo"; Element ("em", [Text "bar"]); Text "baz"]) in
    dom
    |> from_tree (function
      | Element (name, children) -> `Element (("", name), [], children)
      | Text s -> `Text s)
    |> to_list
    |> assert_equal [
      start_element "p";
      `Text ["foo"];
      start_element "em";
      `Text ["bar"];
      `End_element;
      `Text ["baz"];
      `End_element]);

  ("utility.text" >:: fun _ ->
    [`Xml {Markup.version = "1.0"; encoding = None; standalone = None};
     `Comment "blah";
     `Text ["foo"];
     start_element "a";
     `Text ["bar"; "baz"];
     `End_element]
    |> of_list
    |> text
    |> to_string
    |> assert_equal "foobarbaz");

  ("utility.trim" >:: fun _ ->
    [`Text ["\n"];
     start_element "a";
     `Text ["\n  content\n  "];
     `End_element;
     `Text ["\n\n"];
     start_element "a";
     `Text ["\n"; "\n"];
     `End_element;
     `Text [" "; "\n"; " more content "; "\n"; "\n"]]
    |> of_list
    |> trim
    |> to_list
    |> assert_equal [
      start_element "a";
      `Text ["content"];
      `End_element;
      start_element "a";
      `End_element;
      `Text ["more content"]]);

  ("utility.normalize_text" >:: fun _ ->
    [`Text [""];
     start_element "a";
     `Text ["foo"];
     `Text ["bar"];
     `End_element;
     `Text ["foo"; "bar"];
     `Text ["baz"; ""; "quux"]]
    |> of_list
    |> normalize_text
    |> to_list
    |> assert_equal [
      start_element "a";
      `Text ["foo"; "bar"];
      `End_element;
      `Text ["foo"; "bar"; "baz"; "quux"]]);

  ("utility.pretty_print" >:: fun _ ->
    [`Text ["foo"];
     start_element "a";
     `Text [" bar "];
     `Text [" baz "];
     start_element "b";
     `Text ["quux"];
     `End_element;
     `End_element]
    |> of_list
    |> pretty_print
    |> to_list
    |> assert_equal [
      `Text ["foo"; "\n"];
      start_element "a";
      `Text ["\n"; "  "; "bar "; " baz"; "\n"; "  "];
      start_element "b";
      `Text ["\n"; "    "; "quux"; "\n"; "  "];
      `End_element;
      `Text ["\n"];
      `End_element;
      `Text ["\n"]])
]
