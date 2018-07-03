(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std
open Sturgeon_stub
open Cursor

module I = Parser_raw.MenhirInterpreter

type kind =
  | ML
  | MLI
  (*| MLL | MLY*)

module Dump = struct
  let token = Parser_printer.print_token

  let symbol = Parser_printer.print_symbol

  let position pos =
    let l1, c1 = Lexing.split_pos pos in
    Printf.sprintf "%d:%d" l1 c1

  let item k (prod, pos) =
    if not (is_closed k) then (
      let lhs = Parser_printer.print_symbol (I.lhs prod) in
      let rec insert_dot pos = function
        | [] -> ["."]
        | xs when pos = 0 -> "." :: xs
        | x :: xs -> x :: insert_dot (pos - 1) xs
      in
      let rhs =
        I.rhs prod
        |> List.map ~f:Parser_printer.print_symbol
        |> insert_dot pos
        |> String.concat ~sep:" "
      in
      printf k "%s ::= %s\n" lhs rhs
    )

  let print_state k state =
    if not (is_closed k) then (
      let items = I.items state in
      printf k "LR(1) state %d:\n" (I.number state);
      List.iter ~f:(item k) items
    )

  let element k (I.Element (state, _, startp, endp)) =
    if not (is_closed k) then (
      printf k "From %s to %s, " (position startp) (position endp);
      print_state k state
    )

  let env k env =
    match I.top env with
    | None -> text k "Initial state."
    | Some env -> element k env

  let stack k env =
    let rec aux = function
    | None -> text k "Initial state."
    | Some env ->
      Option.iter ~f:(element k) (I.top env);
      aux (I.pop env)
    in
    aux (Some env)
end

module R = Mreader_recover.Make
    (I)
    (struct
      include Parser_recover

      let default_value loc x =
        Default.default_loc := loc;
        default_value x

      let guide (type a) : a I.symbol -> bool = function
        | I.T I.T_BEGIN -> true
        | _ -> false

      let token_of_terminal = Parser_printer.token_of_terminal

      let nullable = Parser_explain.nullable
    end)
   (Dump)

type 'a step =
  | Correct of 'a I.checkpoint
  | Recovering of 'a R.candidates

type tree = [
  | `Interface of Parsetree.signature
  | `Implementation of Parsetree.structure
]

type steps =[
  | `Signature of (Parsetree.signature step * Mreader_lexer.triple) list
  | `Structure of (Parsetree.structure step * Mreader_lexer.triple) list
]

type t = {
  kind: kind;
  tree: tree;
  steps: steps;
  errors: exn list;
  lexer: Mreader_lexer.t;
}

let eof_token = (Parser_raw.EOF, Lexing.dummy_pos, Lexing.dummy_pos)

let errors_ref = ref []

let resume_parse nav =
  let rec normal acc tokens = function
    | I.InputNeeded env as checkpoint ->
      let token, tokens = match tokens with
        | token :: tokens -> token, tokens
        | [] -> eof_token, []
      in
      check_for_error acc token tokens env (I.offer checkpoint token)

    | I.Shifting (_,env,_) | I.AboutToReduce (env,_) as checkpoint ->
      begin match I.resume checkpoint with
        | checkpoint' -> normal acc tokens checkpoint'
        | exception exn ->
          Msupport.raise_error exn;
          let token = match acc with
            | [] -> assert false
              (* Parser raised error before parsing anything *)
            | (_, token) :: _ -> token
          in
          enter_error acc token tokens env
      end

    | I.Accepted v -> acc, v

    | I.Rejected | I.HandlingError _ ->
      assert false

  and check_for_error acc token tokens env = function
    | I.HandlingError _ ->
      enter_error acc token tokens env

    | I.Shifting _ | I.AboutToReduce _ as checkpoint ->
      begin match I.resume checkpoint with
        | checkpoint' -> check_for_error acc token tokens env checkpoint'
        | exception exn ->
          Msupport.raise_error exn;
          enter_error acc token tokens env
      end

    | checkpoint ->
      normal ((Correct checkpoint, token) :: acc) tokens checkpoint

  and enter_error acc token tokens env =
    R.dump nav ~wrong:token ~rest:tokens env;
    let candidates = R.generate null env in
    let explanation =
      Mreader_explain.explain env token
        candidates.R.popped candidates.R.shifted
    in
    errors_ref := Mreader_explain.Syntax_explanation explanation :: !errors_ref;
    recover acc (token :: tokens) candidates

  and recover acc tokens candidates =
    let token, tokens = match tokens with
      | token :: tokens -> token, tokens
      | [] -> eof_token, []
    in
    let acc' = ((Recovering candidates, token) :: acc) in
    match R.attempt null candidates token with
    | `Fail ->
      if tokens = [] then
        match candidates.R.final with
        | None -> failwith "Empty file"
        | Some v -> acc', v
      else
        recover acc tokens candidates
    | `Accept v -> acc', v
    | `Ok (checkpoint, _) ->
      normal ((Correct checkpoint, token) :: acc) tokens checkpoint
  in
  fun acc tokens -> function
  | Correct checkpoint -> normal acc tokens checkpoint
  | Recovering candidates -> recover acc tokens candidates

let seek_step steps tokens =
  let rec aux acc = function
    | (step :: steps), (token :: tokens) when snd step = token ->
      aux (step :: acc) (steps, tokens)
    | _, tokens -> acc, tokens
  in
  aux [] (steps, tokens)

let parse initial nav steps tokens initial_pos =
  let acc, tokens = seek_step steps tokens in
  let step =
    match acc with
    | (step, _) :: _ -> step
    | [] -> Correct (initial initial_pos)
  in
  let acc, result = resume_parse nav acc tokens step in
  List.rev acc, result

let run_parser warnings nav lexer previous kind =
  Msupport.catch_errors warnings errors_ref @@ fun () ->
  let tokens = Mreader_lexer.tokens lexer in
  let initial_pos = Mreader_lexer.initial_position lexer in
  match kind with
  | ML  ->
    let steps = match previous with
      | `Structure steps -> steps
      | _ -> []
    in
    let steps, result =
      let state = Parser_raw.Incremental.implementation in
      parse state nav steps tokens initial_pos in
    `Structure steps, `Implementation result
  | MLI ->
    let steps = match previous with
      | `Signature steps -> steps
      | _ -> []
    in
    let steps, result =
      let state = Parser_raw.Incremental.interface in
      parse state nav steps tokens initial_pos in
    `Signature steps, `Interface result

let null_frame =
  {Widget.Nav. body = null; title = null; nav = Widget.Nav.make "" ignore}

let make warnings lexer kind =
  errors_ref := [];
  let steps, tree = run_parser warnings null_frame lexer `None kind in
  let errors = !errors_ref in
  errors_ref := [];
  {kind; steps; tree; errors; lexer}

let result t = t.tree

let errors t = t.errors

let dump_stack t k tok =
  let find l =
    match List.find ~f:(fun (_, tok') -> tok = tok') l with
    | exception Not_found -> text k "No parser"
    | Correct (I.InputNeeded env), _ ->
      text k "Ready for next token:\n";
      Dump.stack k env
    | Correct (I.Accepted _), _ -> text k "Accepted"
    | Recovering candidates, _ ->
      text k "Recovering from:\n";
      begin match candidates.R.candidates with
        | [] -> text k "Nothing"
        | c :: _ -> Dump.stack k c.R.env
      end
    | _ -> assert false
  in
  match t.steps with
  | `Signature l -> find l
  | `Structure l -> find l
