module Html = Tyxml.Html

val to_html :
  ?root_uri:string ->
  ?syntax:Html_tree.syntax ->
  Model.Comment.docs ->
    ([> Html_types.flow5_without_header_footer ] Html.elt) list

val first_to_html :
  ?root_uri:string ->
  ?syntax:Html_tree.syntax ->
  Model.Comment.docs ->
    ([> Html_types.flow5_without_header_footer ] Html.elt) list
(** Converts the first paragraph (i.e. everything up to the first blank line) to
    html. *)

val link_content_to_html :
  Model.Comment.link_content ->
    ([> Html_types.phrasing_without_interactive ] Html.elt) list

val has_doc : Model.Comment.docs -> bool
