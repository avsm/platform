let src = Logs.Src.create "cram"
module Log = (val Logs.src_log src : Logs.LOG)

type line = S.line

type nd = [`Command | `Output | `False]

type test = {
  part: string option;
  non_deterministic: nd;
  command: string list;
  output: [`Output of string | `Ellipsis] list;
  lines: line list;
  exit_code: int;
}

type item =
  | Test of test
  | Line of line

type t = item list

let dump_string ppf s = Fmt.pf ppf "%S" s

let dump_nd ppf = function
  | `Command -> Fmt.string ppf "`Command"
  | `Output  -> Fmt.string ppf "`Output"
  | `False   -> Fmt.string ppf "`False"

let dump_line ppf (l:line) = match l with
  | `Output s         -> Fmt.pf ppf "`Output %S" s
  | `Part s           -> Fmt.pf ppf "`Part %S" s
  | `Command c        -> Fmt.pf ppf "`Command %a" Fmt.(Dump.list dump_string) c
  | `Ellipsis         -> Fmt.pf ppf "`Ellipsis"
  | `Non_det `Output  -> Fmt.pf ppf "`Non_det `Output"
  | `Non_det `Command -> Fmt.pf ppf "`Not_det `Command"
  | `Comment s        -> Fmt.pf ppf "`Comment %S" s
  | `Exit i           -> Fmt.pf ppf "`Exit %d" i

let dump_lines = Fmt.Dump.list dump_line

let dump_test ppf t =
  Fmt.pf ppf
    "{@[part: %a;@ non_deterministic: %a;@ command: %a;@ output: %a;@ \
     lines: %a;@ exit_code: %d@]}"
    Fmt.(Dump.option string) t.part
    dump_nd t.non_deterministic
    Fmt.(Dump.list dump_string) t.command
    dump_lines (t.output :> line list)
    dump_lines t.lines
    t.exit_code

let dump_item ppf = function
  | Test t -> Fmt.pf ppf "Test %a" dump_test t
  | Line l -> Fmt.pf ppf "Line @[%a@]" dump_line l

let dump_items = Fmt.Dump.list dump_item

let fold (l:S.line list) =
  let rec output ls acc k = function
    | `Ellipsis as l  :: t -> output (l:: t) (`Ellipsis :: acc) k t
    | `Output s as l  :: t -> output (l::ls) (`Output s :: acc) k t
    | `Exit i         :: l -> k i (List.rev ls) (List.rev acc) l
    | l                    -> k 0 (List.rev ls) (List.rev acc) l
  and command lines part k = function
    | []                   -> k (List.map (fun l -> Line l) lines)
    | `Comment _ as l :: t -> command lines part (fun ls -> k (Line l :: ls)) t
    | `Part p as l :: t    -> command lines (Some p) (fun ls -> k (Line l :: ls)) t
    | `Command s as l :: t -> create (l :: lines) `False part s k t
    | (`Non_det nd as d) :: (`Command s as l) :: t ->
      create (l :: d :: lines) (nd :> nd) part s k t
    | (`Non_det _ | `Output _ | `Ellipsis | `Exit _ as l) :: _ ->
      Fmt.failwith "malformed input: '%a'" dump_line l
  and create ls non_deterministic part s k t =
    output ls [] (fun exit_code lines output rest ->
        let c = { exit_code; lines; part; non_deterministic;
                  command = s; output } in
        command [] part (fun rest ->
            k (Test c :: rest)
          ) rest
      ) t
  in
  Log.debug (fun m -> m "lines: @[%a@]" dump_lines l);
  command [] None (fun x -> x) l

let parse_lexbuf l = Lexer.file l |> fold
let parse_file f = Lexer.file (snd (Common.init f)) |> fold

let run n ~f =
  Common.run_expect_test n ~f:(fun c l ->
      let items = parse_lexbuf l in
      Log.debug (fun l -> l "run @[%a@]" dump_items items);
      f c items
    )

let part n t =
  match
    List.filter (function
        | Line _ -> false
        | Test t -> t.part = Some n
      ) t
  with
  | [] -> None
  | l  -> Some l

let pp_command = Fmt.(list ~sep:(unit "\\\n  > ") string)

let pp_line ?(hide=false) ppf line =
  let pp_meta ppf fmt =
    Fmt.kstrf (fun str ->
        if not hide then Fmt.string ppf str
      ) fmt
  in
  match line with
  | `Output s         -> Fmt.pf ppf "  %s\n" s
  | `Part s           -> Fmt.pf ppf "### %s\n" s
  | `Command c        -> Fmt.pf ppf "  $ %a\n" pp_command c
  | `Ellipsis         -> Fmt.pf ppf "  ...\n"
  | `Non_det `Output  -> pp_meta ppf "<-- non-deterministic\n"
  | `Non_det `Command -> pp_meta ppf "<-- non-deterministic [skip]\n"
  | `Comment s        -> pp_meta ppf "%s\n" s
  | `Exit i           -> pp_meta ppf "--> exit %d" i

let pp ?hide ppf t =
  List.iter (function
      | Line l -> pp_line ?hide ppf l
      | Test t -> List.iter (pp_line ?hide ppf) t.lines
    ) t

let to_string ?hide t =
  Fmt.to_to_string (pp ?hide) t

let pp_exit_code ppf n =
  if n <> 0 then Fmt.pf ppf "%a\n" (pp_line ~hide:false) (`Exit n)

type output = [`Output of string | `Ellipsis]

let equal_output a b =
  let rec aux x y = match x, y with
    | [], []  | [`Ellipsis], _   | _, [`Ellipsis]  -> true
    | (`Ellipsis::a as x), (_::b as y) | (_::b as y), (`Ellipsis::a as x) ->
      aux x b || (* n+ matches: skip y's head *)
      aux a y    (* 0  match  : skip x's head *)
    | a::b, h::t -> a = h && aux b t
    | _ -> false
  in
  aux a b

module Html = struct

  let pp_command ppf c = Fmt.(list ~sep:(unit "\\\n  ") string) ppf c

  let pp_line ppf (line:S.line) =
    match line with
    | `Output s  -> Fmt.pf ppf ">%s\n" s
    | `Part _    -> assert false
    | `Command c -> Fmt.pf ppf "%a\n" pp_command c
    | `Ellipsis  -> Fmt.pf ppf "  ...\n"
    | `Exit i    -> Fmt.pf ppf "[exit %d]" i
    | `Non_det _
    | `Comment _ -> ()


  let pp ppf t =
    List.iter (function
        | Line l -> pp_line ppf l
        | Test t -> List.iter (pp_line ppf) t.lines
      ) t
end

let to_html t = Fmt.to_to_string Html.pp t
