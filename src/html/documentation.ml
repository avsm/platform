(*
 * Copyright (c) 2016, 2017 Thomas Refis <trefis@janestreet.com>
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



module Comment = Model.Comment
module Html = Tyxml.Html

type flow = Html_types.flow5_without_header_footer
type phrasing = Html_types.phrasing
type non_link_phrasing = Html_types.phrasing_without_interactive



module Reference = struct
  module Id = Html_tree.Relative_link.Id

  open Model.Paths

  let rec render_resolved : type a. a Reference.Resolved.t -> string =
    fun r ->
      let open Reference.Resolved in
      match r with
      | Identifier id -> Identifier.name id
      | SubstAlias(_, r) -> render_resolved r
      | Module (r, s) -> render_resolved r ^ "." ^ s
      | Canonical (_, Reference.Resolved r) -> render_resolved r
      | Canonical (p, _) -> render_resolved p
      | ModuleType (r, s) -> render_resolved r ^ "." ^ s
      | Type (r, s) -> render_resolved r ^ "." ^ s
      | Constructor (r, s) -> render_resolved r ^ "." ^ s
      | Field (r, s) -> render_resolved r ^ "." ^ s
      | Extension (r, s) -> render_resolved r ^ "." ^ s
      | Exception (r, s) -> render_resolved r ^ "." ^ s
      | Value (r, s) -> render_resolved r ^ "." ^ s
      | Class (r, s) -> render_resolved r ^ "." ^ s
      | ClassType (r, s) -> render_resolved r ^ "." ^ s
      | Method (r, s) ->
        (* CR trefis: do we really want to print anything more than [s] here?  *)
        render_resolved r ^ "." ^ s
      | InstanceVariable (r, s) ->
        (* CR trefis: the following makes no sense to me... *)
        render_resolved r ^ "." ^ s
      | Label (r, s) -> render_resolved r ^ ":" ^ s

  let rec ref_to_string : type a. a Reference.t -> string = function
    | Reference.Root (s, _) -> s
    | Reference.Dot (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Module (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.ModuleType (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Type (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Constructor (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Field (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Extension (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Exception (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Value (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Class (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.ClassType (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Method (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.InstanceVariable (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Label (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Resolved r -> render_resolved r


  (* This is the entry point. stop_before is false on entry, true on recursive
     call. *)
  let rec to_html
      : type a.
        ?text:(non_link_phrasing Html.elt) ->
        stop_before:bool ->
        a Reference.t ->
          phrasing Html.elt =

    fun ?text ~stop_before ref ->
      let span' (txt : phrasing Html.elt list) : phrasing Html.elt =
        Html.span txt ~a:[ Html.a_class ["xref-unresolved"]
                  ; Html.a_title (Printf.sprintf "unresolved reference to %S"
                                (ref_to_string ref))
                  ]
      in
      let open Reference in
      match ref with
      | Root (s, _) ->
        begin match text with
        | None -> Html.code [Html.pcdata s]
        | Some s -> (span' [(s :> phrasing Html.elt)] :> phrasing Html.elt)
        end
      | Dot (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Module (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | ModuleType (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Type (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Constructor (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Field (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Extension (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Exception (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Value (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Class (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | ClassType (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Method (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | InstanceVariable (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Label (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Resolved r ->
        (* IDENTIFIER MUST BE RENAMED TO DEFINITION. *)
        let id = Reference.Resolved.identifier r in
        let txt : non_link_phrasing Html.elt =
          match text with
          | None -> Html.code [Html.pcdata (render_resolved r)]
          | Some s -> s
        in
        begin match Id.href ~stop_before id with
        | exception Id.Not_linkable -> (txt :> phrasing Html.elt)
        | exception exn ->
          (* FIXME: better error message *)
          Printf.eprintf "Id.href failed: %S\n%!" (Printexc.to_string exn);
          (txt :> phrasing Html.elt)
        | href ->
          Html.a ~a:[ Html.a_href href ] [txt]
        end

  and unresolved_parts_to_html
      : type a.
        ?text:(non_link_phrasing Html.elt) ->
        ((phrasing Html.elt list) -> (phrasing Html.elt)) ->
        a Reference.t ->
        string ->
          (phrasing Html.elt) =
    fun ?text span' parent s ->
      match text with
      | Some s -> (span' [(s :> phrasing Html.elt)] :> phrasing Html.elt)
      | None ->
        let tail = [ Html.pcdata ("." ^ s) ] in
        span' (
          match to_html ~stop_before:true parent with
          | content -> content::tail
        )
end



let style_to_combinator = function
  | `Bold -> Html.b
  | `Italic -> Html.i
  | `Emphasis -> Html.em
  | `Superscript -> Html.sup
  | `Subscript -> Html.sub



let leaf_inline_element
    : Comment.leaf_inline_element -> ([> non_link_phrasing ] Html.elt) option =
  function
  | `Space -> Some (Html.pcdata " ")
  | `Word s -> Some (Html.pcdata s)
  | `Code_span s -> Some (Html.code [Html.pcdata s])
  | `Raw_markup (`Html, s) -> Some (Html.Unsafe.data s)

let rec non_link_inline_element
    : 'a. Comment.non_link_inline_element ->
        (([> non_link_phrasing ] as 'a) Html.elt) option =
  function
  | #Comment.leaf_inline_element as e -> leaf_inline_element e
  | `Styled (style, content) ->
    Some ((style_to_combinator style) (non_link_inline_element_list content))

and non_link_inline_element_list :
    'a. _ -> ([> non_link_phrasing ] as 'a) Html.elt list = fun elements ->
  List.fold_left (fun html_elements ast_element ->
    match non_link_inline_element ast_element.Model.Location_.value with
    | None -> html_elements
    | Some e -> e::html_elements)
    [] elements
  |> List.rev

let link_content_to_html =
  non_link_inline_element_list



let rec inline_element : Comment.inline_element -> (phrasing Html.elt) option =
  function
  | #Comment.leaf_inline_element as e ->
    (leaf_inline_element e :> (phrasing Html.elt) option)
  | `Styled (style, content) ->
    Some ((style_to_combinator style) (inline_element_list content))
  | `Reference (path, content) ->
    (* TODO Rework that ugly function. *)
    (* TODO References should be set in code style, if they are to code
            elements. *)
    let content =
      match content with
      | [] -> None
      | _ -> Some (Html.span (non_link_inline_element_list content))
    in
    Some (Reference.to_html ?text:content ~stop_before:false path)
  | `Link (target, content) ->
    let content =
      match content with
      | [] -> [Html.pcdata target]
      | _ -> non_link_inline_element_list content
    in
    Some (Html.a ~a:[Html.a_href target] content)

and inline_element_list elements =
  List.fold_left (fun html_elements ast_element ->
    match inline_element ast_element.Model.Location_.value with
    | None -> html_elements
    | Some e -> e::html_elements)
    [] elements
  |> List.rev



let rec nestable_block_element
    : 'a. Comment.nestable_block_element -> ([> flow ] as 'a) Html.elt =
  function
  | `Paragraph [{value = `Raw_markup (`Html, s); _}] -> Html.Unsafe.data s
  | `Paragraph content -> Html.p (inline_element_list content)
  | `Code_block s -> Html.pre [Html.code [Html.pcdata s]]
  | `Verbatim s -> Html.pre [Html.pcdata s]
  | `Modules ms ->
    let items = List.map (Reference.to_html ~stop_before:false) ms in
    let items = (items :> (Html_types.li_content Html.elt) list) in
    let items = List.map (fun e -> Html.li [e]) items in
    Html.ul items
  | `List (kind, items) ->
    let items =
      items
      |> List.map begin function
        | [{Model.Location_.value = `Paragraph content; _}] ->
          (inline_element_list content :> (Html_types.li_content Html.elt) list)
        | item ->
          nested_block_element_list item
        end
    in
    let items = List.map Html.li items in

    match kind with
    | `Unordered -> Html.ul items
    | `Ordered -> Html.ol items

and nestable_block_element_list elements =
  elements
  |> List.map Model.Location_.value
  |> List.map nestable_block_element

and nested_block_element_list elements =
  (nestable_block_element_list elements :> (Html_types.flow5 Html.elt) list)



let tag : Comment.tag -> ([> flow ] Html.elt) option = function
  | `Author s ->
    Some (Html.(dl [
      dt [pcdata "author"];
      dd [pcdata s]]))
  | `Deprecated content ->
    Some (Html.(dl [
      dt [pcdata "deprecated"];
      dd (nested_block_element_list content)]))
  | `Param (name, content) ->
    Some (Html.(dl [
      dt [pcdata "parameter "; pcdata name];
      dd (nested_block_element_list content)]))
  | `Raise (name, content) ->
    Some (Html.(dl [
      dt [pcdata "raises "; pcdata name];
      dd (nested_block_element_list content)]))
  | `Return content ->
    Some (Html.(dl [
      dt [pcdata "returns"];
      dd (nested_block_element_list content)]))
  | `See (kind, target, content) ->
    let target =
      match kind with
      | `Url -> Html.a ~a:[Html.a_href target] [Html.pcdata target]
      | `File -> Html.code [Html.pcdata target]
      | `Document -> Html.pcdata target
    in
    Some (Html.(dl [
      dt [pcdata "see "; target];
      dd (nested_block_element_list content)]))
  | `Since s ->
    Some (Html.(dl [
      dt [pcdata "since"];
      dd [pcdata s]]))
  | `Before (version, content) ->
    Some (Html.(dl [
      dt [pcdata "before "; pcdata version];
      dd (nested_block_element_list content)]))
  | `Version s ->
    Some (Html.(dl [
      dt [pcdata "version"];
      dd [pcdata s]]))
  | `Canonical _ | `Inline | `Open | `Closed ->
    None



let block_element
    : 'a. Comment.block_element -> (([> flow ] as 'a) Html.elt) option =
  function
  | #Comment.nestable_block_element as e ->
    Some (nestable_block_element e)

  | `Heading (level, label, content) ->
    (* TODO Simplify the id/label formatting. *)
    let attributes =
      let Model.Paths.Identifier.Label (_, label) = label in
      [Html.a_id label]
    in
    let a = attributes in

    let content =
      (non_link_inline_element_list content :> (phrasing Html.elt) list) in
    let content =
      let Model.Paths.Identifier.Label (_, label) = label in
      let anchor =
        Html.a ~a:[Html.a_href ("#" ^ label); Html.a_class ["anchor"]] [] in
      anchor::content
    in

    let element =
      match level with
      | `Title -> Html.h1 ~a content
      | `Section -> Html.h2 ~a content
      | `Subsection -> Html.h3 ~a content
      | `Subsubsection -> Html.h4 ~a content
    in
    Some element

  | `Tag t ->
    tag t

let block_element_list elements =
  List.fold_left (fun html_elements block ->
    match block_element block with
    | Some e -> e::html_elements
    | None -> html_elements)
    [] elements
  |> List.rev



let first_to_html = function
  | {Model.Location_.value = `Paragraph _ as first_paragraph; _}::_ ->
    begin match block_element first_paragraph with
    | Some element -> [element]
    | None -> []
    end
  | _ -> []

let to_html docs =
  block_element_list (List.map Model.Location_.value docs)

let has_doc docs =
  docs <> []
