(*
 * lTerm_read_line.ml
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDefault.Camomile
open Lwt_react
open LTerm_geom
open LTerm_style
open LTerm_key

let return, (>>=) = Lwt.return, Lwt.(>>=)

exception Interrupt
type prompt = LTerm_text.t
type history = Zed_utf8.t list

(* +-----------------------------------------------------------------+
   | Completion                                                      |
   +-----------------------------------------------------------------+ *)

let common_prefix_one a b =
  let rec loop ofs =
    if ofs = String.length a || ofs = String.length b then
      String.sub a 0 ofs
    else
      let ch1, ofs1 = Zed_utf8.unsafe_extract_next a ofs
      and ch2, ofs2 = Zed_utf8.unsafe_extract_next b ofs in
      if ch1 = ch2 && ofs1 = ofs2 then
        loop ofs1
      else
        String.sub a 0 ofs
  in
  loop 0

let common_prefix = function
  | [] -> ""
  | word :: rest -> List.fold_left common_prefix_one word rest

let lookup word words = List.filter (fun word' -> Zed_utf8.starts_with word' word) words
let lookup_assoc word words = List.filter (fun (word', _) -> Zed_utf8.starts_with word' word) words

(* +-----------------------------------------------------------------+
   | Actions                                                         |
   +-----------------------------------------------------------------+ *)

type action =
  | Edit of LTerm_edit.action
  | Interrupt_or_delete_next_char
  | Complete
  | Complete_bar_next
  | Complete_bar_prev
  | Complete_bar_first
  | Complete_bar_last
  | Complete_bar
  | History_prev
  | History_next
  | History_search_prev
  | History_search_next
  | Accept
  | Clear_screen
  | Prev_search
  | Next_search
  | Cancel_search
  | Break
  | Suspend
  | Edit_with_external_editor

let doc_of_action = function
  | Edit action -> LTerm_edit.doc_of_action action
  | Interrupt_or_delete_next_char -> "interrupt if at the beginning of an empty line, or delete the next character."
  | Complete -> "complete current input."
  | Complete_bar_next -> "go to the next possible completion in the completion bar."
  | Complete_bar_prev -> "go to the previous possible completion in the completion bar."
  | Complete_bar_first -> "go to the beginning of the completion bar."
  | Complete_bar_last -> "go to the end of the completion bar."
  | Complete_bar -> "complete current input using the completion bar."
  | History_prev -> "go to the previous entry of the history."
  | History_next -> "go to the next entry of the history."
  | History_search_prev -> "go to the previous entry of the history that matches the start of the current line."
  | History_search_next -> "go to the next entry of the history that matches the start of the current line."
  | Accept -> "accept the current input."
  | Clear_screen -> "clear the screen."
  | Prev_search -> "search backward in the history."
  | Next_search -> "search forward in the history."
  | Cancel_search -> "cancel search mode."
  | Break -> "cancel edition."
  | Suspend -> "suspend edition."
  | Edit_with_external_editor -> "edit input with external editor command."

let actions = [
  Interrupt_or_delete_next_char, "interrupt-or-delete-next-char";
  Complete, "complete";
  Complete_bar_next, "complete-bar-next";
  Complete_bar_prev, "complete-bar-prev";
  Complete_bar_first, "complete-bar-first";
  Complete_bar_last, "complete-bar-last";
  Complete_bar, "complete-bar";
  History_prev, "history-prev";
  History_next, "history-next";
  History_search_prev, "history-search-prev";
  History_search_next, "history-search-next";
  Accept, "accept";
  Clear_screen, "clear-screen";
  Prev_search, "prev-search";
  Next_search, "next-search";
  Cancel_search, "cancel-search";
  Break, "break";
  Suspend, "suspend";
  Edit_with_external_editor, "edit-with-external-editor";
]

let actions_to_names = Array.of_list (List.sort (fun (a1, _) (a2, _) -> Pervasives.compare a1 a2) actions)
let names_to_actions = Array.of_list (List.sort (fun (_, n1) (_, n2) -> Pervasives.compare n1 n2) actions)

let action_of_name x =
  let rec loop a b =
    if a = b then
      Edit (LTerm_edit.action_of_name x)
    else
      let c = (a + b) / 2 in
      let action, name = Array.unsafe_get names_to_actions c in
      match Pervasives.compare x name with
        | d when d < 0 ->
            loop a c
        | d when d > 0 ->
            loop (c + 1) b
        | _ ->
            action
  in
  loop 0 (Array.length names_to_actions)

let name_of_action x =
  let rec loop a b =
    if a = b then
      raise Not_found
    else
      let c = (a + b) / 2 in
      let action, name = Array.unsafe_get actions_to_names c in
      match Pervasives.compare x action with
        | d when d < 0 ->
            loop a c
        | d when d > 0 ->
            loop (c + 1) b
        | _ ->
            name
  in
  match x with
    | Edit x -> LTerm_edit.name_of_action x
    | _ -> loop 0 (Array.length actions_to_names)

module Bindings = Zed_input.Make (LTerm_key)

let bindings = ref Bindings.empty

let bind seq actions = bindings := Bindings.add seq actions !bindings
let unbind seq = bindings := Bindings.remove seq !bindings

let () =
  bind [{ control = false; meta = false; shift = false; code = Home }] [Edit (LTerm_edit.Zed Zed_edit.Goto_bot)];
  bind [{ control = false; meta = false; shift = false; code = End }] [Edit (LTerm_edit.Zed Zed_edit.Goto_eot)];
  bind [{ control = false; meta = false; shift = false; code = Up }] [History_prev];
  bind [{ control = false; meta = false; shift = false; code = Down }] [History_next];
  bind [{ control = false; meta = false; shift = false; code = Tab }] [Complete];
  bind [{ control = false; meta = false; shift = false; code = Enter }] [Accept];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'b') }] [Edit (LTerm_edit.Zed Zed_edit.Prev_char)];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'f') }] [Edit (LTerm_edit.Zed Zed_edit.Next_char)];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'h') }] [Edit (LTerm_edit.Zed Zed_edit.Delete_prev_char)];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'c') }] [Break];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'z') }] [Suspend];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'm') }] [Accept];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'l') }] [Clear_screen];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'r') }] [Prev_search];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 's') }] [Next_search];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'd') }] [Interrupt_or_delete_next_char];
  bind [{ control = false; meta = true; shift = false; code = Char(UChar.of_char 'p') }] [History_prev];
  bind [{ control = false; meta = true; shift = false; code = Char(UChar.of_char 'n') }] [History_next];
  bind [{ control = false; meta = true; shift = false; code = Left }] [Complete_bar_prev];
  bind [{ control = false; meta = true; shift = false; code = Right }] [Complete_bar_next];
  bind [{ control = false; meta = true; shift = false; code = Home }] [Complete_bar_first];
  bind [{ control = false; meta = true; shift = false; code = End }] [Complete_bar_last];
  bind [{ control = false; meta = true; shift = false; code = Tab }] [Complete_bar];
  bind [{ control = false; meta = true; shift = false; code = Down }] [Complete_bar];
  bind [{ control = false; meta = true; shift = false; code = Enter }] [Edit (LTerm_edit.Zed Zed_edit.Newline)];
  bind [{ control = false; meta = false; shift = false; code = Escape }] [Cancel_search];
  bind [{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'x') }
       ;{ control = true; meta = false; shift = false; code = Char(UChar.of_char 'e') }]
    [Edit_with_external_editor]

(* +-----------------------------------------------------------------+
   | The read-line engine                                            |
   +-----------------------------------------------------------------+ *)

let is_prefix ~prefix s =
  String.length prefix <= String.length s &&
  (let i = ref 0 in
   while !i < String.length prefix && s.[!i] = prefix.[!i] do incr i done;
   !i = String.length prefix
  )

let history_find predicate history =
  let rec history_find_ skipped = function
    | [] -> None
    | x :: xs ->
      if predicate x then
        Some (skipped, x, xs)
      else
        history_find_ (x :: skipped) xs
  in
  history_find_ [] history

let search_string str sub =
  let rec equal_at a b =
    (b = String.length sub) || (String.unsafe_get str a = String.unsafe_get sub b) && equal_at (a + 1) (b + 1)
  in
  let rec loop ofs idx =
    if ofs + String.length sub > String.length str then
      None
    else
      if equal_at ofs 0 then
        Some idx
      else
        loop (Zed_utf8.unsafe_next str ofs) (idx + 1)
  in
  loop 0 0

let macro = Zed_macro.create []

type mode =
  | Edition
  | Search
  | Set_counter
  | Add_counter

type completion_state = {
  start : int; (* Beginning of the word being completed *)
  index : int; (* Index of the selected in [words]      *)
  count : int; (* Length of [words]                     *)
  words : (Zed_utf8.t * Zed_utf8.t) list;
}

let no_completion = {
  start = 0;
  index = 0;
  words = [];
  count = 0;
}

type direction = Forward | Backward

type search_status =
  { before : Zed_utf8.t list
  ; after  : Zed_utf8.t list
  ; match_ : (Zed_utf8.t * int) option
  }

class virtual ['a] engine ?(history = []) ?(clipboard = LTerm_edit.clipboard) ?(macro = macro) () =
  let edit : unit Zed_edit.t = Zed_edit.create ~clipboard () in
  let context = Zed_edit.context edit (Zed_edit.new_cursor edit) in
  let mode, set_mode = S.create Edition in
  let user_completion_state, set_completion_state = E.create () in
  let reset_completion_state =
    E.when_
      (S.map (fun mode -> mode = Edition) mode)
      (E.select [
         E.stamp (Zed_edit.changes edit                                    ) no_completion;
         E.stamp (S.changes (Zed_cursor.position (Zed_edit.cursor context))) no_completion;
       ])
  in
  let completion_state =
    S.hold ~eq:(==) no_completion (E.select [reset_completion_state; user_completion_state])
  in
  let completion_words = S.map ~eq:(==) (fun c -> c.words) completion_state in
  let completion_index = S.map          (fun c -> c.index) completion_state in
  let history, set_history = S.create (history, []) in
  let message, set_message = S.create None in
  let history_prefix, set_history_prefix =
    let ev, send = E.create () in
    let edit_changes = Zed_edit.changes edit in
    let edit_changes = E.map (fun _ -> Zed_edit.text edit) edit_changes in
    let prefix = S.hold Zed_rope.empty (E.select [ev; edit_changes]) in
    prefix, send
  in
object(self)
  method virtual eval : 'a
  method edit = edit
  method context = context
  method show_box = true
  method mode = mode
  method history = history
  method message = message
  method clipboard = clipboard
  method macro = macro

  (* The event which occurs when completion need to be recomputed. *)
  val mutable completion_event = E.never

  (* Save for when setting the macro counter. *)
  val mutable save = (0, Zed_rope.empty)

  method set_completion ?(index=0) start words =
    let count = List.length words in
    if index < 0 || index > max 0 (count - 1) then
      invalid_arg
        "LTerm_read_line.set_completion: \
         index out of bounds compared to words.";
    set_completion_state { start; index; count; words }

  initializer
    completion_event <- (
      E.map (fun _ ->
        (* We can't execute it right now as the user might call [set_completion]
          immediatly. *)
        Lwt.pause () >>= fun () ->
        self#completion;
        Lwt.return_unit)
        reset_completion_state
    );
    self#completion

  method input_prev =
    Zed_rope.before (Zed_edit.text edit) (Zed_edit.position context)

  method input_next =
    Zed_rope.after (Zed_edit.text edit) (Zed_edit.position context)

  method completion_words = completion_words
  method completion_index = completion_index
  method completion = self#set_completion 0 []

  method complete =
    let comp = S.value completion_state in
    let prefix_length = Zed_edit.position context - comp.start in
    match comp.words with
      | [] ->
          ()
      | [(completion, suffix)] ->
          Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after completion prefix_length));
          Zed_edit.insert context (Zed_rope.of_string suffix)
      | (completion, _suffix) :: rest ->
          let word = List.fold_left (fun acc (word, _) -> common_prefix_one acc word) completion rest in
          Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after word prefix_length))

  (* The event which search for the string in the history. *)
  val mutable search_event = E.never

  val mutable search_status = None

  initializer
    let reset_search _ =
      search_status <- None;
      self#search Backward
    in
    search_event <-
      E.map reset_search
        (E.when_ (S.map (fun mode -> mode = Search) mode)
           (Zed_edit.changes edit))

  method private search direction =
    let do_search direction =
      let set_status other_entries entries match_ =
        let before, after =
          match direction with
          | Backward -> (other_entries, entries)
          | Forward  -> (entries, other_entries)
        in
        search_status <- Some { before; after; match_ }
      in
      let input = Zed_rope.to_string (Zed_edit.text edit) in
      let rec loop other_entries entries =
        match entries with
        | [] ->
          set_status other_entries entries None;
          set_message (Some(LTerm_text.of_string "Reverse search: not found"))
        | entry :: rest ->
          match search_string entry input with
          | Some pos -> begin
              match search_status with
              | Some { match_ = Some (entry', _); _ } when entry = entry' ->
                loop (entry :: other_entries) rest
              | _ ->
                set_status other_entries rest (Some (entry, pos));
                let txt = LTerm_text.of_string entry in
                for i = pos to pos + Zed_rope.length (Zed_edit.text edit) - 1 do
                  let ch, style = txt.(i) in
                  txt.(i) <- (ch, { style with underline = Some true })
                done;
                set_message
                  (Some (Array.append (LTerm_text.of_string "Reverse search: ") txt))
            end
          | None ->
            loop (entry :: other_entries) rest
      in
      match search_status with
      | None ->
        let hist = fst (S.value history) in
        loop []
          (match direction with
           | Backward -> hist
           | Forward  -> List.rev hist)
      | Some { before; after; match_ } ->
        let other_entries, entries =
          match direction with
          | Backward -> (before, after)
          | Forward  -> (after, before)
        in
        let other_entries =
          match match_ with
          | None -> other_entries
          | Some (entry, _) -> entry :: other_entries
        in
        loop other_entries entries
    in
    match S.value mode with
    | Search -> do_search direction
    | Edition ->
      let text = Zed_edit.text edit in
      Zed_edit.goto context 0;
      Zed_edit.remove context (Zed_rope.length text);
      let prev, next = S.value history in
      set_history (Zed_rope.to_string text :: (List.rev_append next prev), []);
      search_status <- None;
      set_mode Search;
      do_search direction
    | _ ->
      ()

  method insert ch =
    Zed_edit.insert context (Zed_rope.singleton ch)

  method send_action action =
    if action <> Edit LTerm_edit.Stop_macro then Zed_macro.add macro action;
    match action with
      | (Complete | Complete_bar | Accept) when S.value mode = Search -> begin
          set_mode Edition;
          set_message None;
          match search_status with
            | Some { match_ = Some (entry, _pos); _ } ->
                search_status <- None;
                Zed_edit.goto context 0;
                Zed_edit.remove context (Zed_rope.length (Zed_edit.text edit));
                Zed_edit.insert context (Zed_rope.of_string entry)
            | Some { match_ = None; _ } | None ->
                ()
        end

      | Edit (LTerm_edit.Zed action) ->
          Zed_edit.get_action action context

      | Interrupt_or_delete_next_char ->
          if Zed_rope.is_empty (Zed_edit.text edit) then
            raise Interrupt
          else
            Zed_edit.delete_next_char context

      | Complete when S.value mode = Edition ->
          self#complete

      | Complete_bar_next when S.value mode = Edition ->
          let comp = S.value completion_state in
          if comp.index < comp.count - 1 then
            set_completion_state { comp with index = comp.index + 1 }

      | Complete_bar_prev when S.value mode = Edition ->
          let comp = S.value completion_state in
          if comp.index > 0 then
            set_completion_state { comp with index = comp.index - 1 }

      | Complete_bar_first when S.value mode = Edition ->
          let comp = S.value completion_state in
          if comp.index > 0 then
            set_completion_state { comp with index = 0 }

      | Complete_bar_last when S.value mode = Edition ->
          let comp = S.value completion_state in
          if comp.index < comp.count - 1 then
            set_completion_state { comp with index = comp.count - 1 }

      | Complete_bar when S.value mode = Edition ->
          let comp = S.value completion_state in
          if comp.words <> [] then begin
            let prefix_length = Zed_edit.position context - comp.start in
            let completion, suffix = List.nth comp.words comp.index in
            Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after completion prefix_length));
            Zed_edit.insert context (Zed_rope.of_string suffix)
          end

      | History_prev when S.value mode = Edition ->begin
          let prev, next = S.value history in
          match prev with
            | [] ->
                ()
            | line :: rest ->
                let text = Zed_edit.text edit in
                set_history (rest, Zed_rope.to_string text :: next);
                Zed_edit.goto context 0;
                Zed_edit.remove context (Zed_rope.length text);
                Zed_edit.insert context (Zed_rope.of_string line)
        end

      | History_next when S.value mode = Edition -> begin
          let prev, next = S.value history in
          match next with
            | [] ->
                ()
            | line :: rest ->
                let text = Zed_edit.text edit in
                set_history (Zed_rope.to_string text :: prev, rest);
                Zed_edit.goto context 0;
                Zed_edit.remove context (Zed_rope.length text);
                Zed_edit.insert context (Zed_rope.of_string line)
        end

      | History_search_prev when S.value mode = Edition -> begin
          let prev, next = S.value history in
          let text = Zed_rope.to_string @@ Zed_edit.text edit in
          let prefix = S.value history_prefix in
          match history_find (is_prefix ~prefix:(Zed_rope.to_string prefix)) prev with
          | None ->
            ()
          | Some (not_matched, line, rest) ->
            set_history (rest, not_matched @ text :: next);
            Zed_edit.goto context 0;
            Zed_edit.delete_next_line context;
            Zed_edit.insert context (Zed_rope.of_string line);
            set_history_prefix prefix
        end

      | History_search_next when S.value mode = Edition -> begin
          let prev, next = S.value history in
          let prefix = S.value history_prefix in
          match history_find (is_prefix ~prefix:(Zed_rope.to_string prefix)) next with
          | None ->
            ()
          | Some (not_matched, line, rest) ->
            let text = Zed_rope.to_string @@ Zed_edit.text edit in
            set_history (not_matched @ text :: prev, rest);
            Zed_edit.goto context 0;
            Zed_edit.delete_next_line context;
            Zed_edit.insert context (Zed_rope.of_string line);
            set_history_prefix prefix
        end

      | Prev_search -> self#search Backward
      | Next_search -> self#search Forward

      | Cancel_search ->
          if S.value mode = Search then begin
            set_mode Edition;
            set_message None
          end

      | Edit LTerm_edit.Start_macro when S.value mode = Edition ->
          Zed_macro.set_recording macro true

      | Edit LTerm_edit.Stop_macro ->
          Zed_macro.set_recording macro false

      | Edit LTerm_edit.Cancel_macro ->
          Zed_macro.cancel macro

      | Edit LTerm_edit.Play_macro ->
          Zed_macro.cancel macro;
          List.iter self#send_action (Zed_macro.contents macro)

      | Edit LTerm_edit.Insert_macro_counter ->
          Zed_edit.insert context (Zed_rope.of_string (string_of_int (Zed_macro.get_counter macro)));
          Zed_macro.add_counter macro 1

      | Edit LTerm_edit.Set_macro_counter when S.value mode = Edition ->
          let text = Zed_edit.text edit in
          save <- (Zed_edit.position context, text);
          Zed_edit.goto context 0;
          Zed_edit.remove context (Zed_rope.length text);
          set_mode Set_counter;
          set_message (Some (LTerm_text.of_string "Enter a value for the macro counter."))

      | Edit LTerm_edit.Add_macro_counter when S.value mode = Edition ->
          let text = Zed_edit.text edit in
          save <- (Zed_edit.position context, text);
          Zed_edit.goto context 0;
          Zed_edit.remove context (Zed_rope.length text);
          set_mode Add_counter;
          set_message (Some (LTerm_text.of_string "Enter a value to add to the macro counter."))

      | Accept -> begin
          match S.value mode with
            | Edition | Search ->
                ()
            | Set_counter ->
                let pos, text = save in
                save <- (0, Zed_rope.empty);
                (try
                   Zed_macro.set_counter macro (int_of_string (Zed_rope.to_string (Zed_edit.text edit)))
                 with Failure _ ->
                   ());
                Zed_edit.goto context 0;
                Zed_edit.remove context (Zed_rope.length (Zed_edit.text edit));
                Zed_edit.insert context text;
                Zed_edit.goto context pos;
                set_mode Edition;
                set_message None
            | Add_counter ->
                let pos, text = save in
                save <- (0, Zed_rope.empty);
                (try
                   Zed_macro.add_counter macro (int_of_string (Zed_rope.to_string (Zed_edit.text edit)))
                 with Failure _ ->
                   ());
                Zed_edit.goto context 0;
                Zed_edit.remove context (Zed_rope.length (Zed_edit.text edit));
                Zed_edit.insert context text;
                Zed_edit.goto context pos;
                set_mode Edition;
                set_message None
        end

      | Break ->
          raise Sys.Break

      | Edit (LTerm_edit.Custom f) ->
          f ()

      | _ ->
          ()

  method stylise last =
    let txt = LTerm_text.of_rope (Zed_edit.text edit) in
    let pos = Zed_edit.position context in
    if not last && Zed_edit.get_selection edit then begin
      let mark = Zed_cursor.get_position (Zed_edit.mark edit) in
      let a = min pos mark and b = max pos mark in
      for i = a to b - 1 do
        let ch, style = txt.(i) in
        txt.(i) <- (ch, { style with underline = Some true })
      done;
    end;
    (txt, pos)
end

class virtual ['a] abstract = object
  method virtual eval : 'a
  method virtual send_action : action -> unit
  method virtual insert : UChar.t -> unit
  method virtual edit : unit Zed_edit.t
  method virtual context : unit Zed_edit.context
  method virtual clipboard : Zed_edit.clipboard
  method virtual macro : action Zed_macro.t
  method virtual stylise : bool -> LTerm_text.t * int
  method virtual history : (Zed_utf8.t list * Zed_utf8.t list) signal
  method virtual message : LTerm_text.t option signal
  method virtual input_prev : Zed_rope.t
  method virtual input_next : Zed_rope.t
  method virtual completion_words : (Zed_utf8.t * Zed_utf8.t) list signal
  method virtual completion_index : int signal
  method virtual set_completion : ?index:int -> int -> (Zed_utf8.t * Zed_utf8.t) list -> unit
  method virtual completion : unit
  method virtual complete : unit
  method virtual show_box : bool
  method virtual mode : mode signal
end

(* +-----------------------------------------------------------------+
   | Predefined classes                                              |
   +-----------------------------------------------------------------+ *)

class read_line ?history () = object(self)
  inherit [Zed_utf8.t] engine ?history ()
  method eval = Zed_rope.to_string (Zed_edit.text self#edit)
end

class read_password () = object(self)
  inherit [Zed_utf8.t] engine () as super

  method! stylise last =
    let text, pos = super#stylise last in
    for i = 0 to Array.length text - 1 do
      let _ch, style = text.(i) in
      text.(i) <- (UChar.of_char '*', style)
    done;
    (text, pos)

  method eval = Zed_rope.to_string (Zed_edit.text self#edit)

  method! show_box = false

  method! send_action = function
    | Prev_search | Next_search -> ()
    | action -> super#send_action action
end

type 'a read_keyword_result =
  | Rk_value of 'a
  | Rk_error of Zed_utf8.t

class ['a] read_keyword ?history () = object(self)
  inherit ['a read_keyword_result] engine ?history ()

  method keywords = []

  method eval =
    let input = Zed_rope.to_string (Zed_edit.text self#edit) in
    try Rk_value(List.assoc input self#keywords) with Not_found -> Rk_error input

  method! completion =
    let word = Zed_rope.to_string self#input_prev in
    let keywords = List.filter (fun (keyword, _value) -> Zed_utf8.starts_with keyword word) self#keywords in
    self#set_completion 0 (List.map (fun (keyword, _value) -> (keyword, "")) keywords)
end

(* +-----------------------------------------------------------------+
   | Running in a terminal                                           |
   +-----------------------------------------------------------------+ *)

let newline = UChar.of_char '\n'
let vline = LTerm_draw.({ top = Light; bottom = Light; left = Blank; right = Blank })
let reverse_style = { LTerm_style.none with LTerm_style.reverse = Some true }
let default_prompt = LTerm_text.of_string "# "

let rec drop count l =
  if count <= 0 then
    l
  else match l with
    | [] -> []
    | _ :: l -> drop (count - 1) l

(* Computes the position of the cursor after printing the given styled
   string:
   - [pos] is the current cursor position
     (it may be at column [max-column + 1])
   - [text] is the text to display
   - [start] is the start of the chunk to display in [text]
   - [stop] is the end of the chunk to display in [text]
*)
let rec compute_position cols pos text start stop =
  if start = stop then
    pos
  else
    let ch, _style = text.(start) in
    if ch = newline then
      compute_position cols { row = pos.row + 1; col = 0 } text (start + 1) stop
    else if pos.col = cols then
      compute_position cols { row = pos.row + 1; col = 1 } text (start + 1) stop
    else
      compute_position cols { pos with col = pos.col + 1 } text (start + 1) stop

(* Return the "real" position of the cursor, i.e. on the screen. *)
let real_pos cols pos =
  if pos.col = cols then
    { row = pos.row + 1; col = 0 }
  else
    pos

let rec get_index_of_last_displayed_word column columns index words =
  match words with
    | [] ->
        index - 1
    | (word, _suffix) :: words ->
        let column = column + Zed_utf8.length word in
        if column <= columns - 1 then
          get_index_of_last_displayed_word (column + 1) columns (index + 1) words
        else
          index - 1

let draw_styled ctx row col str =
  let size = LTerm_draw.size ctx in
  let rec loop row col idx =
    if idx < Array.length str then begin
      let ch, style = Array.unsafe_get str idx in
      if ch = newline then
        loop (row + 1) 0 (idx + 1)
      else begin
        let point = LTerm_draw.point ctx row col in
        point.LTerm_draw.char <- ch;
        LTerm_draw.set_style point style;
        let col = col + 1 in
        if col = size.cols then
          loop (row + 1) 0 (idx + 1)
        else
          loop row col (idx + 1)
      end
    end
  in
  loop row col 0

let unsafe_get matrix row col =
  Array.unsafe_get (Array.unsafe_get matrix row) col

let draw_styled_with_newlines matrix cols row col str =
  let rec loop row col idx =
    if idx < Array.length str then begin
      let ch, style = Array.unsafe_get str idx in
      if ch = newline then begin
        (unsafe_get matrix row col).LTerm_draw.char <- newline;
        loop (row + 1) 0 (idx + 1)
      end else begin
        let row, col =
          if col = cols then
            (row + 1, 0)
          else
            (row, col)
        in
        let point = unsafe_get matrix row col in
        point.LTerm_draw.char <- ch;
        LTerm_draw.set_style point style;
        loop row (col + 1) (idx + 1)
      end
    end
  in
  loop row col 0

let styled_newline = [|(newline, LTerm_style.none)|]

class virtual ['a] term term =
  let size, set_size = S.create (LTerm.size term) in
  let event, set_prompt = E.create () in
  let prompt = S.switch (S.hold ~eq:( == ) (S.const default_prompt) event) in
  let key_sequence, set_key_sequence = S.create [] in
object(self)
  inherit ['a] abstract
  method size = size
  method prompt = prompt
  method set_prompt prompt = set_prompt prompt

  val mutable visible = true
    (* Whether the read-line instance is currently visible. *)

  val mutable displayed = false
    (* Whether the read-line instance is currently displayed on the
       screen. *)

  val mutable draw_queued = false
    (* Whether a draw operation has been queued, in which case it is
       not necessary to redraw. *)

  val mutable cursor = { row = 0; col = 0 }
    (* The position of the cursor. *)

  val mutable completion_start = S.const 0
    (* Index of the first displayed word in the completion bar. *)

  val mutable height = 0
    (* The height of the displayed material. *)

  val mutable resolver = None
  (* The current resolver for resolving input sequences. *)

  val mutable running = true

  initializer
    completion_start <- (
      S.fold
        (fun start (words, index, columns) ->
           if index < start then
             (* The cursor is before the left margin. *)
             let count = List.length words in
             let rev_index = count - index - 1 in
             count - get_index_of_last_displayed_word 1 columns rev_index (drop rev_index (List.rev words)) - 1
           else if index > get_index_of_last_displayed_word 1 columns start (drop start words) then
             (* The cursor is after the right margin. *)
             index
           else
             start)
        0
        (S.changes
           (S.l3
              (fun words index size -> (words, index, size.cols))
              self#completion_words
              self#completion_index
              size))
    )

  method key_sequence = key_sequence

  method completion_start = completion_start

  val draw_mutex = Lwt_mutex.create ()

  method private queue_draw_update =
    if draw_queued then
      return ()
    else begin
      (* Wait a bit in order not to draw too often. *)
      draw_queued <- true;
      Lwt.pause () >>= fun () ->
      draw_queued <- false;
      Lwt_mutex.with_lock draw_mutex (fun () ->
        if running then
          self#draw_update
        else
          return ())
    end

  method draw_update =
    let size = S.value size in
    if visible && size.rows > 0 && size.cols > 0 then begin
      let styled, position = self#stylise false in
      let prompt = S.value prompt in
      (* Compute the position of the cursor after displaying the
         prompt. *)
      let pos_after_prompt = compute_position size.cols { row = 0; col = 0 } prompt 0 (Array.length prompt) in
      (* Compute the position of the cursor after displaying the
         input before the cursor. *)
      let pos_after_before = compute_position size.cols pos_after_prompt styled 0 position in
      (* Compute the position of the cursor after displaying the
         input. *)
      let pos_after_styled = compute_position size.cols pos_after_before styled position (Array.length styled) in
      (* Compute the position of the cursor after displaying the
         newline used to end the input. *)
      let pos_after_newline = compute_position size.cols pos_after_styled styled_newline 0 1 in
      (* The real position of the cursor on the screen. *)
      let pos_cursor = real_pos size.cols pos_after_before in
      (* Height of prompt+input. *)
      let prompt_input_height = max (pos_cursor.row + 1) pos_after_newline.row in
      let matrix =
        if self#show_box && size.cols > 2 then
          match S.value self#message with
            | Some msg ->
                (* Compute the height of the message. *)
                let message_height = (compute_position (size.cols - 2) { row = 0; col = 0 } msg 0 (Array.length msg)).row + 1 in
                (* The total height of the displayed text. *)
                let total_height = prompt_input_height + message_height + 2 in

                (* Create the matrix for the rendering. *)
                let matrix_size = { cols = size.cols + 1; rows = if displayed then max total_height height else total_height } in
                let matrix = LTerm_draw.make_matrix matrix_size in

                (* Update the height parameter. *)
                height <- total_height;

                (* Draw the prompt and the input. *)
                draw_styled_with_newlines matrix size.cols 0 0 prompt;
                draw_styled_with_newlines matrix size.cols pos_after_prompt.row pos_after_prompt.col styled;
                draw_styled_with_newlines matrix size.cols pos_after_styled.row pos_after_styled.col styled_newline;

                let ctx = LTerm_draw.sub (LTerm_draw.context matrix matrix_size) {
                  row1 = 0;
                  col1 = 0;
                  row2 = matrix_size.rows;
                  col2 = size.cols;
                } in

                (* Draw a frame for the message. *)
                LTerm_draw.draw_frame ctx {
                  row1 = prompt_input_height;
                  col1 = 0;
                  row2 = total_height;
                  col2 = size.cols;
                } LTerm_draw.Light;
                for row = prompt_input_height to total_height - 1 do
                  (unsafe_get matrix row size.cols).LTerm_draw.char <- newline
                done;

                (* Draw the message. *)
                let ctx = LTerm_draw.sub ctx {
                  row1 = prompt_input_height + 1;
                  col1 = 1;
                  row2 = total_height - 1;
                  col2 = size.cols - 1;
                } in
                draw_styled ctx 0 0 msg;

                matrix

            | None ->
                let comp_start = S.value self#completion_start in
                let comp_index = S.value self#completion_index in
                let comp_words = drop comp_start (S.value self#completion_words) in

                (* The total height of the displayed text. *)
                let total_height = prompt_input_height + 3 in

                (* Create the matrix for the rendering. *)
                let matrix_size = { cols = size.cols + 1; rows = if displayed then max total_height height else total_height } in
                let matrix = LTerm_draw.make_matrix matrix_size in

                (* Update the height parameter. *)
                height <- total_height;

                (* Draw the prompt and the input. *)
                draw_styled_with_newlines matrix size.cols 0 0 prompt;
                draw_styled_with_newlines matrix size.cols pos_after_prompt.row pos_after_prompt.col styled;
                draw_styled_with_newlines matrix size.cols pos_after_styled.row pos_after_styled.col styled_newline;

                let ctx = LTerm_draw.sub (LTerm_draw.context matrix matrix_size) {
                  row1 = 0;
                  col1 = 0;
                  row2 = matrix_size.rows;
                  col2 = size.cols;
                } in

                (* Draw a frame for the completion. *)
                LTerm_draw.draw_frame ctx {
                  row1 = prompt_input_height;
                  col1 = 0;
                  row2 = total_height;
                  col2 = size.cols;
                } LTerm_draw.Light;
                for row = prompt_input_height to total_height - 1 do
                  (unsafe_get matrix row size.cols).LTerm_draw.char <- newline
                done;

                (* Draw the completion. *)
                let ctx = LTerm_draw.sub ctx {
                  row1 = prompt_input_height + 1;
                  col1 = 1;
                  row2 = total_height - 1;
                  col2 = size.cols - 1;
                } in

                let rec loop idx col = function
                  | [] ->
                      ()
                  | (word, _suffix) :: words ->
                      let len = Zed_utf8.length word in
                      LTerm_draw.draw_string ctx 0 col word;
                      (* Apply the reverse style if this is the selected word. *)
                      if idx = comp_index then
                        for col = col to min (col + len - 1) (size.cols - 2) do
                          LTerm_draw.set_style (LTerm_draw.point ctx 0 col) reverse_style
                        done;
                      (* Draw a separator. *)
                      LTerm_draw.draw_piece ctx 0 (col + len) vline;
                      let col = col + len + 1 in
                      if col < size.cols - 2 then loop (idx + 1) col words
                in
                loop comp_start 0 comp_words;

                matrix

        else begin
          let total_height = prompt_input_height in
          let matrix_size = { cols = size.cols + 1; rows = if displayed then max total_height height else total_height } in
          let matrix = LTerm_draw.make_matrix matrix_size in
          height <- total_height;
          draw_styled_with_newlines matrix size.cols 0 0 prompt;
          draw_styled_with_newlines matrix size.cols pos_after_prompt.row pos_after_prompt.col styled;
          matrix
        end
      in
      LTerm.hide_cursor term >>= fun () ->
      begin
        if displayed then
          (* Go back to the beginning of displayed text. *)
          LTerm.move term (-cursor.row) (-cursor.col)
        else
          return ()
      end >>= fun () ->
      (* Display everything. *)
      LTerm.print_box_with_newlines term matrix >>= fun () ->
      (* Update the cursor. *)
      cursor <- pos_cursor;
      (* Move the cursor to the right position. *)
      LTerm.move term (cursor.row - Array.length matrix + 1) cursor.col >>= fun () ->
      LTerm.show_cursor term >>= fun () ->
      LTerm.flush term >>= fun () ->
      displayed <- true;
      return ()
    end else
      return ()

  method draw_success =
    let size = S.value size in
    if size.rows > 0 && size.cols > 0 then begin
      let styled, _position = self#stylise true in
      let prompt = S.value prompt in
      (if displayed then
         LTerm.move term (-cursor.row) (-cursor.col) >>= fun () ->
         LTerm.clear_screen_next term
       else
         return ()) >>= fun () ->
      LTerm.fprints term prompt >>= fun () ->
      LTerm.fprintls term styled
    end else
      return ()

  method draw_failure =
    self#draw_success

  method hide =
    if visible then begin
      visible <- false;
      Lwt_mutex.lock draw_mutex >>= fun () ->
      Lwt.finalize (fun () ->
        let size = S.value size in
        if displayed && size.rows > 0 && size.cols > 0 then
          let matrix_size = { cols = size.cols + 1; rows = height } in
          let matrix = LTerm_draw.make_matrix matrix_size in
          for row = 0 to height - 1 do
            (unsafe_get matrix row 0).LTerm_draw.char <- newline
          done;
          LTerm.move term (-cursor.row) (-cursor.col) >>= fun () ->
          LTerm.print_box_with_newlines term matrix >>= fun () ->
          LTerm.move term (1 - Array.length matrix) 0 >>= fun () ->
          cursor <- { row = 0; col = 0 };
          height <- 0;
          displayed <- false;
          return ()
        else
          return ())
        (fun () ->
          Lwt_mutex.unlock draw_mutex;
          return ())
    end else
      return ()

  method show =
    if not visible then begin
      visible <- true;
      self#queue_draw_update
    end else
      return ()

  val mutable mode = None

  val mutable local_bindings = Bindings.empty
  method bind keys actions = local_bindings <- Bindings.add keys actions local_bindings

  (* The main loop. *)
  method private loop =
    LTerm.read_event term >>= fun ev ->
    match ev with
      | LTerm_event.Resize size ->
          set_size size;
          self#loop
      | LTerm_event.Key key -> begin
          let res =
            match resolver with
              | Some res -> res
              | None ->
                Bindings.resolver
                  [ Bindings.pack (fun x -> x) local_bindings
                  ; Bindings.pack (fun x -> x) !bindings
                  ; Bindings.pack (List.map (fun x -> Edit x)) !LTerm_edit.bindings
                  ]
          in
          match Bindings.resolve key res with
            | Bindings.Accepted actions ->
                resolver <- None;
                set_key_sequence [];
                self#exec actions
            | Bindings.Continue res ->
                resolver <- Some res;
                set_key_sequence (S.value key_sequence @ [key]);
                self#loop
            | Bindings.Rejected ->
                set_key_sequence [];
                if resolver = None then
                  match key with
                    | { control = false; meta = false; shift = false; code = Char ch } ->
                        Zed_macro.add self#macro (Edit (LTerm_edit.Zed (Zed_edit.Insert ch)));
                        self#insert ch
                    | { code = Char ch; _ } when LTerm.windows term && UChar.code ch >= 32 ->
                        (* Windows reports Shift+A for A, ... *)
                        Zed_macro.add self#macro (Edit (LTerm_edit.Zed (Zed_edit.Insert ch)));
                        self#insert ch
                    | _ ->
                        ()
                else
                  resolver <- None;
                self#loop
        end
      | _ ->
          self#loop

  method create_temporary_file_for_external_editor =
    Filename.temp_file "lambda-term" ".txt"

  method external_editor =
    try
      Sys.getenv "EDITOR"
    with Not_found -> "vi"

  method private exec = function
    | Accept :: _ when S.value self#mode = Edition ->
        Zed_macro.add self#macro Accept;
        return self#eval
    | Clear_screen :: actions ->
        Zed_macro.add self#macro Clear_screen;
        LTerm.clear_screen term >>= fun () ->
        LTerm.goto term { row = 0; col = 0 } >>= fun () ->
        displayed <- false;
        self#queue_draw_update >>= fun () ->
        self#exec actions
    | Edit LTerm_edit.Play_macro :: actions ->
        Zed_macro.cancel self#macro;
        self#exec (Zed_macro.contents macro @ actions)
    | Suspend :: actions ->
        if Sys.win32 then
          self#exec actions
        else begin
          let is_visible = visible in
          self#hide >>= fun () ->
          LTerm.flush term >>= fun () ->
          begin
            match mode with
              | Some mode ->
                  LTerm.leave_raw_mode term mode
              | None ->
                  return ()
          end >>= fun () ->
          Unix.kill (Unix.getpid ()) Sys.sigtstp;
          begin
            match LTerm.is_a_tty term with
              | true ->
                  LTerm.enter_raw_mode term >>= fun m ->
                  mode <- Some m;
                  return ()
              | false ->
                  return ()
          end >>= fun () ->
          (if is_visible then self#show else return ()) >>= fun () ->
          self#exec actions
        end
    | Edit_with_external_editor :: actions -> begin
        let is_visible = visible in
        self#hide >>= fun () ->
        LTerm.flush term >>= fun () ->
        begin
          match mode with
          | Some mode ->
            LTerm.leave_raw_mode term mode
          | None ->
            return ()
        end >>= fun () ->
        let temp_fn = self#create_temporary_file_for_external_editor in
        let input = Zed_rope.to_string (Zed_edit.text self#edit) in
        Lwt_io.with_file ~mode:Output temp_fn (fun oc -> Lwt_io.write_line oc input)
        >>= fun () ->
        let editor = self#external_editor in
        Printf.ksprintf Lwt_unix.system "%s %s" editor (Filename.quote temp_fn)
        >>= fun status ->
        (if status <> WEXITED 0 then
           Lwt_io.eprintf "`%s %s' exited with status %d\n"
             editor temp_fn
             (match status with
              | WEXITED n -> n
              | _         -> 255)
         else
           Lwt.try_bind
             (fun () -> Lwt_io.with_file ~mode:Input temp_fn Lwt_io.read)
             (fun s  ->
                let s = Zed_utf8.rstrip s in
                Zed_edit.goto_bot self#context;
                Zed_edit.replace self#context (Zed_rope.length (Zed_edit.text self#edit))
                  (Zed_rope.of_string s);
                Lwt.return ())
             (function
               | Unix.Unix_error (err, _, _) ->
                 Lwt_io.eprintf "%s: %s\n" temp_fn (Unix.error_message err)
               | exn -> Lwt.fail exn)
        )
        >>= fun () ->
        begin
          match LTerm.is_a_tty term with
          | true ->
            LTerm.enter_raw_mode term >>= fun m ->
            mode <- Some m;
            return ()
          | false ->
            return ()
        end
        >>= fun () ->
        (if is_visible then self#show else return ())
        >>= fun () ->
        self#exec actions
      end
    | action :: actions ->
        self#send_action action;
        self#exec actions
    | [] ->
        self#loop

  method run =
    (* Update the size with the current size. *)
    set_size (LTerm.size term);

    running <- true;

    (* Redraw everything when needed. *)
    let event =
      E.map_p
        (fun () -> if running then self#queue_draw_update else return ())
        (E.select [
           E.stamp (S.changes size) ();
           Zed_edit.update self#edit [Zed_edit.cursor self#context];
           E.stamp (S.changes prompt) ();
           E.stamp (S.changes self#completion_words) ();
           E.stamp (S.changes self#completion_index) ();
           E.stamp (S.changes self#completion_start) ();
           E.stamp (S.changes self#message) ();
         ])
    in

    begin
      match LTerm.is_a_tty term with
        | true ->
            LTerm.enter_raw_mode term >>= fun m ->
            mode <- Some m;
            return ()
        | false ->
            return ()
    end >>= fun () ->

    begin
      Lwt.finalize (fun () ->
          Lwt.catch (fun () ->
              (* Go to the beginning of line otherwise all offset
                 calculation will be false. *)
              LTerm.fprint term "\r" >>= fun () ->
              self#queue_draw_update >>= fun () ->
              self#loop)
            (fun exn ->
              running <- false;
              E.stop event;
              Lwt_mutex.with_lock draw_mutex (fun () -> self#draw_failure) >>= fun () ->
              Lwt.fail exn))
        (fun () ->
          match mode with
            | Some mode ->
                LTerm.leave_raw_mode term mode
            | None ->
                return ())
    end >>= fun result ->
    running <- false;
    E.stop event;
    Lwt_mutex.with_lock draw_mutex (fun () -> self#draw_success) >>= fun () ->
    return result
end
