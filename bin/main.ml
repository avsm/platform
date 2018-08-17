let src = Logs.Src.create "cram"
module Log = (val Logs.src_log src : Logs.LOG)

open Astring

let read_lines file =
  let ic = open_in file in
  let r = ref [] in
  try while true do r := input_line ic :: !r done; assert false
  with End_of_file ->
    close_in ic;
    List.rev !r

(* From jbuilder's stdlib *)
let ansi_color_strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop i =
    if i = len then
      Buffer.contents buf
    else
      match str.[i] with
      | '\027' -> skip (i + 1)
      | c      -> Buffer.add_char buf c; loop (i + 1)
  and skip i =
    if i = len then
      Buffer.contents buf
    else
      match str.[i] with
      | 'm' -> loop (i + 1)
      | _   -> skip (i + 1)
  in
  loop 0


(* http://tldp.org/LDP/abs/html/here-docs.html *)
let use_heredoc t =
  String.cut (List.hd t.Craml.command) ~sep:"<<" <> None

let command_of_test t =
  if not (use_heredoc t) then
    String.concat ~sep:" " t.Craml.command
  else
    String.concat ~sep:"\n" t.Craml.command

let run_test temp_file t =
  let cmd = command_of_test t in
  Log.info (fun l -> l "exec: %S" cmd);
  let fd = Unix.openfile temp_file [O_WRONLY; O_TRUNC] 0 in
  let pid = Unix.create_process "sh" [| "sh"; "-c"; cmd |] Unix.stdin fd fd in
  Unix.close fd;
  match snd (Unix.waitpid [] pid) with
  | WEXITED n -> n
  | _ -> 255

let run () non_deterministic expect_test =
  Craml.run expect_test ~f:(fun file_contents items ->
      let temp_file = Filename.temp_file "cram" ".output" in
      at_exit (fun () -> Sys.remove temp_file);
      let buf = Buffer.create (String.length file_contents + 1024) in
      let ppf = Format.formatter_of_buffer buf in
      List.iter (function
          | Craml.Line l -> Craml.pp_line ppf l
          | Craml.Test t ->
            match non_deterministic, t.Craml.non_deterministic with
            | false, `Command ->
              (* the command is non-deterministic so skip everything *)
              List.iter (Craml.pp_line ppf) t.Craml.lines
            | false, `Output ->
              (* the command's output is non-deterministic; run it but
                 keep the old output. *)
              let _ = run_test temp_file t in
              List.iter (Craml.pp_line ppf) t.Craml.lines
            | _ ->
              let n = run_test temp_file t in
              let lines = read_lines temp_file in
              let output =
                let output = List.map (fun x -> `Output x) lines in
                if Craml.equal_output output t.output then t.output else output
              in
              Fmt.pf ppf "  $ %a\n" Craml.pp_command t.Craml.command;
              List.iter (function
                  | `Ellipsis    -> Fmt.pf ppf "  ...\n"
                  | `Output line -> Fmt.pf ppf "  %s\n" (ansi_color_strip line)
                ) output;
              Craml.pp_exit_code ppf n
        ) items;
      Format.pp_print_flush ppf ();
      Buffer.contents buf)

open Cmdliner

let non_deterministic =
  let doc = "Run non-deterministic tests." in
  Arg.(value & flag & info ["non-deterministic"; "n"] ~doc)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let expect_test =
  let doc = "The test to run." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FILE")

let cmd =
  let doc = "A simple tool for to run CRAM tests." in
  let exits = Term.default_exits in
  Term.(pure run $ setup_log $ non_deterministic $ expect_test),
  Term.info "cram" ~version:"%%VERSION%%" ~doc ~exits

let () = Term.(exit @@ eval cmd)
