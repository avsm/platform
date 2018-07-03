(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Bos_setup

type t = {
  user  : string option;
  remote: string option;
  local : Fpath.t option;
}

let of_yaml_exn str = (* ouch *)
  let lines = String.cuts ~empty:false ~sep:"\n" str in
  let dict () =
    List.map (fun line ->
        match String.cut ~sep:":" line with
        | Some (k, v) -> String.trim k, String.trim v
        | _ -> failwith "invalid format"
      ) lines
  in
  let dict = dict () in
  let find k = try Some (List.assoc k dict) with Not_found -> None in
  let valid = ["user"; "remote"; "local"] in
  List.iter (fun (k, _) ->
      if not (List.mem k valid) then
        Fmt.failwith "%S is not a valid configuration key." k
    ) dict;
  let local = match find "local" with
  | None    -> None
  | Some v  ->
      match Fpath.of_string v with
      | Ok x    -> Some x
      | Error _ -> None
  in
  { user = find "user"; remote = find "remote"; local }

let of_yaml str =
  try Ok (of_yaml_exn str)
  with Failure s -> R.error_msg s

let read_string default ~descr =
  let read () =
    match read_line () with
    | "" -> None
    | s  -> print_newline (); Some s
    | exception End_of_file ->
        print_newline ();
        None
    | exception (Sys.Break as e) ->
        print_newline ();
        raise e
  in
  Fmt.pr "@[<h-0>%s@.[press ENTER to use '%a']@]\n%!"
    (String.trim descr)
    Fmt.(styled `Bold string) default;
  match read () with
  | None   -> default
  | Some s -> s

let create_config ~user ~remote_repo ~local_repo pkgs file =
  Fmt.pr
    "%a does not exist!\n\
     Please answer a few questions to help me create it for you:\n\n%!"
    Fpath.pp file;
  (match user with Some u -> Ok u | None ->
    let pkg = List.hd pkgs in
    Pkg.distrib_user_and_repo pkg >>= fun (u, _) ->
    Ok u)
  >>= fun default_user ->
  let user = read_string default_user ~descr:"What is your GitHub ID?" in
  let default_remote = match remote_repo with
  | Some r -> r
  | None   -> strf "https://github.com/%s/opam-repository" user
  in
  let default_local = match local_repo with
  | Some r -> Ok r
  | None ->
      match OS.Env.var "HOME" with
      | None   -> R.error_msg "$HOME is undefined"
      | Some d -> Ok (Fpath.(v d / "git" / "opam-repository") |> Fpath.to_string)
  in
  default_local >>= fun default_local ->
  let remote = read_string default_remote
      ~descr:"What is your fork of ocaml/opam-repository? \
              (you should have write access)."
  in
  let local = read_string default_local
      ~descr:"Where on your filesystem did you clone that repository?"
  in
  Fpath.of_string local >>= fun local ->
  let v = strf "user: %s\nremote: %s\nlocal: %a\n" user remote Fpath.pp local in
  OS.Dir.create Fpath.(parent file) >>= fun _ ->
  OS.File.write file v >>= fun () ->
  Ok { user = Some user; remote = Some remote; local = Some local }

let v ~user ~remote_repo ~local_repo pkgs =
  match OS.Env.var "HOME" with
  | None   -> R.error_msg "$HOME is undefined"
  | Some d ->
      let file = Fpath.(v d / ".dune" / "release.yml") in
      OS.File.exists file >>= fun exists ->
      if exists then OS.File.read file >>= of_yaml
      else create_config ~user ~remote_repo ~local_repo pkgs file


let reset_terminal : (unit -> unit) option ref = ref None
let cleanup () = match !reset_terminal with None -> () | Some f -> f ()
let () = at_exit cleanup

let no_stdin_echo f =
  let open Unix in
  let attr = tcgetattr stdin in
  let reset () = tcsetattr stdin TCSAFLUSH attr in
  reset_terminal := Some reset;
  tcsetattr stdin TCSAFLUSH
    { attr with
      c_echo = false; c_echoe = false; c_echok = false; c_echonl = true; };
  let v = f () in
  reset ();
  reset_terminal := None;
  v

let get_token () =
  let rec aux () =
    match read_line () with
    | "" -> aux ()
    | s  -> s
    | exception End_of_file ->
        print_newline ();
        aux ()
    | exception (Sys.Break as e) ->
        print_newline ();
        raise e
  in
  no_stdin_echo aux

let token ~dry_run () =
  match OS.Env.var "HOME" with
  | None   -> R.error_msg "$HOME is undefined"
  | Some d ->
      let file = Fpath.(v d / ".dune" / "github.token") in
      OS.File.exists file >>= fun exists ->
      if exists then Ok file
      else if dry_run then Ok Fpath.(v "${token}")
      else (
        Fmt.pr
          "%a does not exist!\n\
           \n\
           To create a new token, please visit:\n\
           \n\
          \   https://github.com/settings/tokens/new\n\
           \n\
           And create a token with a nice name and and the %a scope only.\n\
           \n\
           Copy the token@ here: %!"
          Fpath.pp file
          Fmt.(styled `Bold string) "public_repo";
        let token = get_token () in
        OS.Dir.create Fpath.(parent file) >>= fun _ ->
        OS.File.write ~mode:0o600 file token >>= fun () ->
        Ok file
      )
