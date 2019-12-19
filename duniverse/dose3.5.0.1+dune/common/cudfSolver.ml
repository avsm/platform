(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

module Pcre = Re_pcre

open ExtLib

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

(* FIXME: unify with deb/apt.ml *)
let blank_regexp = Pcre.regexp "[ \t]+" ;;

let check_fail file =
  let ic = open_in file in
  try begin
    let l = input_line ic in
    try (close_in ic ; l = "FAIL")
    with Scanf.Scan_failure _ -> (close_in ic ; false)
  end with End_of_file -> (close_in ic ; false)
;;

let prng = lazy(Random.State.make_self_init ());;

(* bits and pieces borrowed from ocaml stdlib/filename.ml *)
let mktmpdir prefix suffix =
  let temp_dir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  let temp_file_name temp_dir prefix suffix =
    let rnd = (Random.State.bits (Lazy.force prng)) land 0xFFFFFF in
    Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)
  in
  let rec try_name counter =
    let name = temp_file_name temp_dir prefix suffix in
    try
      Unix.mkdir name 0o700;
      name
    with Unix.Unix_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in try_name 0

let rmtmpdir path =
  begin try
    Sys.remove (Filename.concat path "in-cudf");
    Sys.remove (Filename.concat path "out-cudf")
  with e -> warning "Cannot remove %s/{in-cudf,out-cudf}" path end;
  begin try
    Unix.rmdir path
  with e ->
    warning "cannot delete temporary directory %s - not empty?" path;
    raise e
  end
;;

let rec input_all_lines acc chan =
  try input_all_lines ((input_line chan)::acc) chan
  with End_of_file -> acc
;;

(** Solver "exec:" line. Contains three named wildcards to be interpolated:
   "$in", "$out", and "$pref"; corresponding to, respectively, input CUDF
   document, output CUDF universe, user preferences. *)

(* remove all characters disallowed in criteria *)
(* TODO: should this really be done? *)
let sanitize s = Pcre.substitute ~rex:(Pcre.regexp "[^\\[\\]+()a-z0-9,\"-]") ~subst:(fun _ -> "") s;;

let interpolate_solver_pat exec cudf_in cudf_out pref =
  let argv = try Shell_lexer.parse_string exec with
    | Shell_lexer.UnknownShellEscape s -> fatal "Unknown shell escape character: %s" s
    | Shell_lexer.UnmatchedChar c -> fatal "Unmatched character: %c" c
  in
  (* assoc list mapping from wildcard to value *)
  let mapping = [("$in", cudf_in); ("$out", cudf_out); ("$pref", sanitize pref)] in
  (* test if the exec string contains all wildcards *)
  let contains_all_wildcards = List.for_all (fun (w,_) -> List.mem w argv) mapping in
  if not contains_all_wildcards then
    fatal "solver exec string must contain $in, $out and $pref";
  List.map (fun a -> try List.assoc a mapping with Not_found -> a) argv
;;

exception Error of string
exception Unsat

let raise_error fmt =
  Printf.kprintf (fun s ->
    raise (Error s)
  ) fmt
;;

let check_exit_status cmd = function
  |Unix.WEXITED 0   -> ()
  |Unix.WEXITED i   -> raise_error "command '%s' failed with code %d" cmd i
  |Unix.WSIGNALED i -> raise_error "command '%s' killed by signal %d" cmd i
  |Unix.WSTOPPED i  -> raise_error "command '%s' stopped by signal %d" cmd i
;;

let timer3 = Util.Timer.create "cudfio" ;;
let timer4 = Util.Timer.create "solver" ;;

let try_set_close_on_exec fd =
  try Unix.set_close_on_exec fd; true with Invalid_argument _ -> false

let open_proc_full cmd env input output error toclose =
  let cloexec = List.for_all try_set_close_on_exec toclose in
  match Unix.fork() with
  | 0 -> Unix.dup2 input Unix.stdin; Unix.close input;
         Unix.dup2 output Unix.stdout; Unix.close output;
         Unix.dup2 error Unix.stderr; Unix.close error;
         if not cloexec then List.iter Unix.close toclose;
         begin try Unix.execvpe (List.hd cmd) (Array.of_list cmd) env
         with _ -> exit 127
         end
  | id -> id

(* bits and pieces borrowed from ocaml stdlib/filename.ml *)
let open_process argv env =
  let (in_read, in_write) = Unix.pipe() in
  let fds_to_close = ref [in_read;in_write] in
  try
    let (out_read, out_write) = Unix.pipe() in
    fds_to_close := out_read::out_write:: !fds_to_close;
    let (err_read, err_write) = Unix.pipe() in
    fds_to_close := err_read::err_write:: !fds_to_close;
    let inchan = Unix.in_channel_of_descr in_read in
    let outchan = Unix.out_channel_of_descr out_write in
    let errchan = Unix.in_channel_of_descr err_read in
    let pid = open_proc_full argv env out_read in_write err_write [in_read; out_write; err_read] in
    Unix.close out_read;
    Unix.close in_write;
    Unix.close err_write;
    (inchan, outchan, errchan,pid)
  with e ->
    List.iter Unix.close !fds_to_close;
    raise e
;;

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let close_process (inchan, outchan, errchan,pid) =
  close_in inchan;
  begin try close_out outchan with Sys_error _ -> () end;
  close_in errchan;
  snd(waitpid_non_intr pid)
;;

(** [execsolver] execute an external cudf solver.
    exec_pat : execution string
    cudf : a cudf document (preamble, universe, request)
    criteria : optimization criteria
*)
let execsolver exec_pat criteria cudf = 
  let (_,universe,_) = cudf in

  let tmpdir = mktmpdir "tmp.apt-cudf." "" in
  let aux () =
    let solver_in = Filename.concat tmpdir "in-cudf" in
    Unix.mkfifo solver_in 0o600;
    let solver_out = Filename.concat tmpdir "out-cudf" in
    let argv = interpolate_solver_pat exec_pat solver_in solver_out criteria in
    let command = String.join " " argv in

    notice "Exec: %s" command;

    (* Tell OCaml we want to capture SIGCHLD                       *)
    (* In case the external solver fails before reading its input, *)
    (* this will raise a Unix.EINTR error which is captured below  *)
    let eintr_handl = Sys.signal Sys.sigchld (Sys.Signal_handle (fun _ -> ())) in

    let env = Unix.environment () in
    let (cin,cout,cerr,pid) = open_process argv env in

    Util.Timer.start timer3;
    begin
      try
        let solver_in_fd = Unix.openfile solver_in [Unix.O_WRONLY;Unix.O_SYNC] 0 in
        let oc = Unix.out_channel_of_descr solver_in_fd in
        Cudf_printer.pp_cudf oc cudf;
        close_out oc
      with Unix.Unix_error (Unix.EINTR,_,_) ->
        info "Interrupted by EINTR while executing command '%s'" command
    end;
    Util.Timer.stop timer3 ();
    (* restore previous behaviour on sigchild *)
    Sys.set_signal Sys.sigchld eintr_handl;

    Util.Timer.start timer4;
    let lines_cin = input_all_lines [] cin in
    let lines = input_all_lines lines_cin cerr in
    let exit_code = close_process (cin,cout,cerr,pid) in
    check_exit_status command exit_code;
    notice "\n%s" (String.concat "\n" lines);
    Util.Timer.stop timer4 ();

    if not(Sys.file_exists solver_out) then
      raise_error "(CRASH) Solution file not found"
    else if check_fail solver_out then
      raise Unsat
    else 
      try begin
        try Cudf_parser.load_solution_from_file solver_out universe with
        |Cudf_parser.Parse_error _
        |Cudf.Constraint_violation _ ->
          raise_error "(CRASH) Solution file contains an invalid solution"
      end with Cudf.Constraint_violation s ->
        raise_error "(CUDF) Malformed solution: %s" s ;
  in
  let res = aux () in
  rmtmpdir tmpdir;
  res
;;
