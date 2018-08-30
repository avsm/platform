open! Stdune
open! Import

open Dsexp.Template

type t =
  { template : Dsexp.Template.t
  ; syntax_version : Syntax.Version.t
  }

let make_text ?(quoted=false) loc s =
  { template =
      { parts = [Text s]
      ; quoted
      ; loc
      }
  ; syntax_version = (1, 0)
  }

let literal ~quoted ~loc s =
  { parts = [Text s]
  ; quoted
  ; loc
  }

(* This module implements the "old" template parsing that is only used in jbuild
   files *)
module Jbuild : sig
  val parse : string -> loc:Loc.t -> quoted:bool -> Dsexp.Template.t
end = struct
  type var_syntax = Parens | Braces
  module Token = struct
    type t =
      | String of string
      | Open   of var_syntax
      | Close  of var_syntax

    let tokenise s =
      let len = String.length s in
      let sub i j = String.sub s ~pos:i ~len:(j - i) in
      let cons_str i j acc = if i = j then acc else String (sub i j) :: acc in
      let rec loop i j =
        if j = len
        then cons_str i j []
        else
          match s.[j] with
          | '}' -> cons_str i j (Close Braces :: loop (j + 1) (j + 1))
          | ')' -> cons_str i j (Close Parens :: loop (j + 1) (j + 1))
          | '$' when j + 1 < len -> begin
              match s.[j + 1] with
              | '{' -> cons_str i j (Open Braces :: loop (j + 2) (j + 2))
              | '(' -> cons_str i j (Open Parens :: loop (j + 2) (j + 2))
              | _   -> loop i (j + 1)
            end
          | _ -> loop i (j + 1)
      in
      loop 0 0

    let to_string = function
      | String s     -> s
      | Open  Braces -> "${"
      | Open  Parens -> "$("
      | Close Braces -> "}"
      | Close Parens -> ")"
  end
  (* Remark: Consecutive [Text] items are concatenated. *)
  let rec of_tokens
    : Loc.t -> Token.t list -> part list = fun loc -> function
    | [] -> []
    | Open a :: String s :: Close b :: rest when a = b ->
      let (name, payload) =
        match String.lsplit2 s ~on:':' with
        | None -> (s, None)
        | Some (n, p) -> (n, Some p)
      in
      Var { loc
          ; name
          ; payload
          ; syntax =
              begin match a with
              | Parens -> Dollar_paren
              | Braces -> Dollar_brace
              end
          } :: of_tokens loc rest
    | token :: rest ->
      let s = Token.to_string token in
      match of_tokens loc rest with
      | Text s' :: l -> Text (s ^ s') :: l
      | l -> Text s :: l

  let parse s ~loc ~quoted =
    { parts = of_tokens loc (Token.tokenise s)
    ; loc
    ; quoted
    }
end

let dparse =
  let open Dsexp.Of_sexp in
  let jbuild =
    raw >>| function
    | Template _ as t ->
      Exn.code_error "Unexpected dune template from a jbuild file"
        [ "t", Dsexp.to_sexp (Dsexp.Ast.remove_locs t)
        ]
    | Atom(loc, A s) -> Jbuild.parse s ~loc ~quoted:false
    | Quoted_string (loc, s) -> Jbuild.parse s ~loc ~quoted:true
    | List (loc, _) -> Dsexp.Of_sexp.of_sexp_error loc "Atom expected"
  in
  let dune =
    raw >>| function
    | Template t -> t
    | Atom(loc, A s) -> literal ~quoted:false ~loc s
    | Quoted_string (loc, s) -> literal ~quoted:true ~loc s
    | List (loc, _) -> Dsexp.Of_sexp.of_sexp_error loc "Unexpected list"
  in
  let template_parser = Stanza.Of_sexp.switch_file_kind ~jbuild ~dune in
  let%map syntax_version = Syntax.get_exn Stanza.syntax
  and template = template_parser
  in
  {template; syntax_version}

let loc t = t.template.loc

let syntax_version t = t.syntax_version

let virt_syntax = (1, 0)

let virt ?(quoted=false) pos s =
  let template = Jbuild.parse ~quoted ~loc:(Loc.of_pos pos) s in
  {template; syntax_version = virt_syntax}

let virt_var ?(quoted=false) pos s =
  assert (String.for_all s ~f:(function ':' -> false | _ -> true));
  let loc = Loc.of_pos pos in
  let template =
    { parts =
        [Var { payload = None
             ; name = s
             ; syntax = Percent
             ; loc
             }]
    ; loc
    ; quoted
    }
  in
  {template; syntax_version = virt_syntax}

let virt_text pos s =
  let template = { parts = [Text s];  loc = Loc.of_pos pos;  quoted = true } in
  {template; syntax_version = virt_syntax}

let concat_rev = function
  | [] -> ""
  | [s] -> s
  | l -> String.concat (List.rev l) ~sep:""

module Mode = struct
  type _ t =
    | Single : Value.t t
    | Many : Value.t list t

  let string
    : type a. a t -> string -> a
    = fun t s ->
      match t with
      | Single -> Value.String s
      | Many -> [Value.String s]

  let value
    : type a. a t -> Value.t list -> a option
    = fun t s ->
      match t, s with
      | Many, s -> Some s
      | Single, [s] -> Some s
      | Single, _ -> None
end

module Partial = struct
  type nonrec 'a t =
    | Expanded of 'a
    | Unexpanded of t
end

let invalid_multivalue (v : var) x =
  Errors.fail v.loc "Variable %s expands to %d values, \
                   however a single value is expected here. \
                   Please quote this atom."
    (string_of_var v) (List.length x)

module Var = struct
  type t = var

  let loc (t : t) = t.loc

  let name { name; _ } = name

  let full_name t =
    match t.payload with
    | None -> t.name
    | Some v -> t.name ^ ":" ^ v

  let payload t = t.payload

  let to_string = string_of_var

  let to_sexp t = Sexp.To_sexp.string (to_string t)

  let with_name t ~name =
    { t with name }

  let is_macro t = Option.is_some t.payload

  let describe t =
    to_string
      (match t.payload with
       | None   -> t
       | Some _ -> { t with payload = Some ".." })
end

let partial_expand
  : 'a.t
  -> mode:'a Mode.t
  -> dir:Path.t
  -> f:(Var.t -> Syntax.Version.t -> Value.t list option)
  -> 'a Partial.t
  = fun ({template; syntax_version} as t) ~mode ~dir ~f ->
    let commit_text acc_text acc =
      let s = concat_rev acc_text in
      if s = "" then acc else Text s :: acc
    in
    let rec loop acc_text acc items =
      match items with
      | [] ->
        begin match acc with
        | [] ->
          Partial.Expanded (Mode.string mode (concat_rev acc_text))
        | _  ->
          let template = {template with parts = List.rev (commit_text acc_text acc)} in
          Unexpanded {template; syntax_version}
        end
      | Text s :: items -> loop (s :: acc_text) acc items
      | Var var as it :: items ->
        begin match f var syntax_version with
        | Some ([] | _::_::_ as e) when not template.quoted ->
          invalid_multivalue var e
        | Some t ->
          loop (Value.L.concat ~dir t :: acc_text) acc items
        | None -> loop [] (it :: commit_text acc_text acc) items
        end
    in
    match template.parts with
    | [] -> Partial.Expanded (Mode.string mode "")
    | [Text s] -> Expanded (Mode.string mode s)
    | [Var var] when not template.quoted ->
      begin match f var syntax_version with
      | None -> Partial.Unexpanded t
      | Some e -> Expanded (
        match Mode.value mode e with
        | None -> invalid_multivalue var e
        | Some s -> s)
      end
    | _ -> loop [] [] template.parts

type 'a expander = Var.t -> Syntax.Version.t -> 'a

let expand t ~mode ~dir ~f =
  match
    partial_expand t ~mode ~dir ~f:(fun var syntax_version ->
      match f var syntax_version with
      | None ->
        begin match var.syntax with
        | Percent ->
          if Var.is_macro var then
            Errors.fail var.loc "Unknown macro %s" (Var.describe var)
          else
            Errors.fail var.loc "Unknown variable %S" (Var.name var)
        | Dollar_brace
        | Dollar_paren -> Some [Value.String (string_of_var var)]
        end
      | s -> s)
  with
  | Partial.Expanded s -> s
  | Unexpanded _ -> assert false (* we are expanding every variable *)

let partial_expand t ~mode ~dir ~f = partial_expand t ~mode ~dir ~f

let dgen { template; syntax_version = _ } = Dsexp.Template template

let to_sexp t = Dsexp.to_sexp (dgen t)

let is_var { template; syntax_version = _ } ~name =
  match template.parts with
  | [Var n] -> name = Var.full_name n
  | _ -> false

let text_only t =
  match t.template.parts with
  | [Text s] -> Some s
  | _ -> None

let remove_locs t =
  { t with template = Dsexp.Template.remove_locs t.template
  }
