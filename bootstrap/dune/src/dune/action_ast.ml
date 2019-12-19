open! Stdune
open Import
open Dune_lang.Decoder

let () =
  let module M = Stanza in
  ()

module Outputs = struct
  include Action_intf.Outputs

  let to_string = function
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | Outputs -> "outputs"
end

module Inputs = struct
  include Action_intf.Inputs

  let to_string = function
    | Stdin -> "stdin"
end

module type Target_intf = sig
  include Dune_lang.Conv.S

  val is_dev_null : t -> bool
end

module Make
    (Program : Dune_lang.Conv.S)
    (Path : Dune_lang.Conv.S)
    (Target : Target_intf)
    (String : Dune_lang.Conv.S)
    (Ast : Action_intf.Ast
             with type program := Program.t
             with type path := Path.t
             with type target := Target.t
             with type string := String.t) =
struct
  include Ast

  let translate_to_ignore fn output action =
    if Target.is_dev_null fn then
      Ignore (output, action)
    else
      Redirect_out (output, fn, action)

  let decode =
    let path = Path.decode in
    let string = String.decode in
    let target = Target.decode in
    Dune_lang.Decoder.fix (fun t ->
        sum
          [ ( "run"
            , let+ prog = Program.decode
              and+ args = repeat String.decode in
              Run (prog, args) )
          ; ( "with-accepted-exit-codes"
            , Dune_lang.Syntax.since Stanza.syntax (2, 0)
              >>> let+ codes = Predicate_lang.decode_one Dune_lang.Decoder.int
                  and+ t = located t in
                  match t with
                  | _, ((Run _ | Bash _ | System _) as t) ->
                    With_accepted_exit_codes (codes, t)
                  | loc, _ ->
                    User_error.raise ~loc
                      [ Pp.textf
                          "with-accepted-exit-codes can only be used with \
                           \"run\", \"bash\" or \"system\""
                      ] )
          ; ( "dynamic-run"
            , Dune_lang.Syntax.since Action_plugin.syntax (0, 1)
              >>> let+ prog = Program.decode
                  and+ args = repeat String.decode in
                  Dynamic_run (prog, args) )
          ; ( "chdir"
            , let+ dn = path
              and+ t = t in
              Chdir (dn, t) )
          ; ( "setenv"
            , let+ k = string
              and+ v = string
              and+ t = t in
              Setenv (k, v, t) )
          ; ( "with-stdout-to"
            , let+ fn = target
              and+ t = t in
              translate_to_ignore fn Stdout t )
          ; ( "with-stderr-to"
            , let+ fn = target
              and+ t = t in
              translate_to_ignore fn Stderr t )
          ; ( "with-outputs-to"
            , let+ fn = target
              and+ t = t in
              translate_to_ignore fn Outputs t )
          ; ( "with-stdin-from"
            , Dune_lang.Syntax.since Stanza.syntax (2, 0)
              >>> let+ fn = path
                  and+ t = t in
                  Redirect_in (Stdin, fn, t) )
          ; ("ignore-stdout", t >>| fun t -> Ignore (Stdout, t))
          ; ("ignore-stderr", t >>| fun t -> Ignore (Stderr, t))
          ; ("ignore-outputs", t >>| fun t -> Ignore (Outputs, t))
          ; ("progn", repeat t >>| fun l -> Progn l)
          ; ( "echo"
            , let+ x = string
              and+ xs = repeat string in
              Echo (x :: xs) )
          ; ("cat", path >>| fun x -> Cat x)
          ; ( "copy"
            , let+ src = path
              and+ dst = target in
              Copy (src, dst) )
          ; ( "copy#"
            , let+ src = path
              and+ dst = target in
              Copy_and_add_line_directive (src, dst) )
          ; ( "copy-and-add-line-directive"
            , let+ src = path
              and+ dst = target in
              Copy_and_add_line_directive (src, dst) )
          ; ("system", string >>| fun cmd -> System cmd)
          ; ("bash", string >>| fun cmd -> Bash cmd)
          ; ( "write-file"
            , let+ fn = target
              and+ s = string in
              Write_file (fn, s) )
          ; ( "diff"
            , let+ diff = Diff.decode path ~optional:false in
              Diff diff )
          ; ( "diff?"
            , let+ diff = Diff.decode path ~optional:true in
              Diff diff )
          ; ( "cmp"
            , let+ diff = Diff.decode_binary path in
              Diff diff )
          ])

  let rec encode =
    let open Dune_lang in
    let program = Program.encode in
    let string = String.encode in
    let path = Path.encode in
    let target = Target.encode in
    function
    | Run (a, xs) -> List (atom "run" :: program a :: List.map xs ~f:string)
    | With_accepted_exit_codes (pred, t) ->
      List
        [ atom "with-accepted-exit-codes"
        ; Predicate_lang.encode Dune_lang.Encoder.int pred
        ; encode t
        ]
    | Dynamic_run (a, xs) ->
      List (atom "run_dynamic" :: program a :: List.map xs ~f:string)
    | Chdir (a, r) -> List [ atom "chdir"; path a; encode r ]
    | Setenv (k, v, r) -> List [ atom "setenv"; string k; string v; encode r ]
    | Redirect_out (outputs, fn, r) ->
      List
        [ atom (sprintf "with-%s-to" (Outputs.to_string outputs))
        ; target fn
        ; encode r
        ]
    | Redirect_in (inputs, fn, r) ->
      List
        [ atom (sprintf "with-%s-from" (Inputs.to_string inputs))
        ; path fn
        ; encode r
        ]
    | Ignore (outputs, r) ->
      List [ atom (sprintf "ignore-%s" (Outputs.to_string outputs)); encode r ]
    | Progn l -> List (atom "progn" :: List.map l ~f:encode)
    | Echo xs -> List (atom "echo" :: List.map xs ~f:string)
    | Cat x -> List [ atom "cat"; path x ]
    | Copy (x, y) -> List [ atom "copy"; path x; target y ]
    | Symlink (x, y) -> List [ atom "symlink"; path x; target y ]
    | Copy_and_add_line_directive (x, y) ->
      List [ atom "copy#"; path x; target y ]
    | System x -> List [ atom "system"; string x ]
    | Bash x -> List [ atom "bash"; string x ]
    | Write_file (x, y) -> List [ atom "write-file"; target x; string y ]
    | Rename (x, y) -> List [ atom "rename"; target x; target y ]
    | Remove_tree x -> List [ atom "remove-tree"; target x ]
    | Mkdir x -> List [ atom "mkdir"; path x ]
    | Digest_files paths ->
      List [ atom "digest-files"; List (List.map paths ~f:path) ]
    | Diff { optional; file1; file2; mode = Binary } ->
      assert (not optional);
      List [ atom "cmp"; path file1; path file2 ]
    | Diff { optional = false; file1; file2; mode = _ } ->
      List [ atom "diff"; path file1; path file2 ]
    | Diff { optional = true; file1; file2; mode = _ } ->
      List [ atom "diff?"; path file1; path file2 ]
    | Merge_files_into (srcs, extras, into) ->
      List
        [ atom "merge-files-into"
        ; List (List.map ~f:path srcs)
        ; List (List.map ~f:string extras)
        ; target into
        ]

  let run prog args = Run (prog, args)

  let chdir path t = Chdir (path, t)

  let setenv var value t = Setenv (var, value, t)

  let with_stdout_to path t = Redirect_out (Stdout, path, t)

  let with_stderr_to path t = Redirect_out (Stderr, path, t)

  let with_outputs_to path t = Redirect_out (Outputs, path, t)

  let with_stdin_from path t = Redirect_in (Stdin, path, t)

  let ignore_stdout t = Ignore (Stdout, t)

  let ignore_stderr t = Ignore (Stderr, t)

  let ignore_outputs t = Ignore (Outputs, t)

  let progn ts = Progn ts

  let echo s = Echo s

  let cat path = Cat path

  let copy a b = Copy (a, b)

  let symlink a b = Symlink (a, b)

  let copy_and_add_line_directive a b = Copy_and_add_line_directive (a, b)

  let system s = System s

  let bash s = Bash s

  let write_file p s = Write_file (p, s)

  let rename a b = Rename (a, b)

  let remove_tree path = Remove_tree path

  let mkdir path = Mkdir path

  let digest_files files = Digest_files files

  let diff ?(optional = false) ?(mode = Diff.Mode.Text) file1 file2 =
    Diff { optional; file1; file2; mode }
end
