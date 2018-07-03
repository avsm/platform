(**************************************************************************************)
(*  Copyright (C) 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2010 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib

type benchmark = float * (string, Benchmark.t list) Hashtbl.t

let string_of_date ut =
  let tm = Unix.gmtime ut in
  Printf.sprintf "%02d/%d/%d-%02d:%02d"
    tm.Unix.tm_mday
    (tm.Unix.tm_mon + 1)
    (tm.Unix.tm_year + 1900)
    tm.Unix.tm_hour
    tm.Unix.tm_min
;;

(* -------------------------------------- *)

let rec pp_list ?(sep="") pp_element fmt = function
  |[h] -> Format.fprintf fmt "%a" pp_element h
  |h::t ->
      Format.fprintf fmt "%a%s@,%a"
      pp_element h sep (pp_list ~sep pp_element) t
  |[] -> ()

let pp_header widths fmt header =
  let first_row = Array.map (fun x -> String.make (x + 1) ' ') widths in
  Array.iteri (fun j cell ->
    Format.pp_set_tab fmt ();
    for z=0 to (String.length header.(j)) - 1 do cell.[z] <- header.(j).[z] done;
    Format.fprintf fmt "%s" cell
  ) first_row

let pp_row pp_cell fmt row =
  Array.iteri (fun j cell ->
    Format.pp_print_tab fmt ();
    Format.fprintf fmt "%a" pp_cell cell
  ) row

let pp_tables pp_row fmt (header,table) =
  (* we build with the largest length of each column of the
   * table and header *)
  let widths = Array.create (Array.length table.(0)) 0 in
  Array.iter (fun row ->
    Array.iteri (fun j cell ->
      widths.(j) <- max (String.length cell) widths.(j)
    ) row
  ) table;
  Array.iteri (fun j cell ->
    widths.(j) <- max (String.length cell) widths.(j)
  ) header;

  (* open the table box *)
  Format.pp_open_tbox fmt ();

  (* print the header *)
  Format.fprintf fmt "%a@\n" (pp_header widths) header;
  (* print the table *)
  Array.iter (pp_row fmt) table;

  (* close the box *)
  Format.pp_close_tbox fmt ()
;;

(* Parsing *)

let parse_test s =
  (* 1283502697.79 WALL ( 0.14 usr +  0.04 sys =  0.18 CPU) @ 27.17/s (n=5) *)
  let d_re = "[0-9]+" in
  let t_re = Printf.sprintf "[ \\t]*\\(%s\\.%s\\)" d_re d_re in
  let s_re = Str.regexp (
    Printf.sprintf
    "^%s WALL (%s usr \\+ %s sys = %s CPU) @ %s/s (n=\\(%s\\))$"
      t_re t_re t_re t_re t_re d_re
    )
  in
  let ex n s = float_of_string (Str.matched_group n s) in
  if Str.string_match s_re s 0 then 
      { Benchmark.wall = ex 1 s;
        utime = ex 2 s;
        stime = ex 3 s;
        cutime = 0.;
        cstime = 0.;
        iters = Int64.of_string (Str.matched_group 6 s)
      }
  else
    failwith (Printf.sprintf "invalid test %s" s)

let parse_sample s =
  let s_re = Str.regexp "^\\([a-zA-Z0-9_.]+\\) : \\(.*\\)$" in
  if Str.string_match s_re s 0 then
    let fname = Str.matched_group 1 s in
    let sl = Str.split (Str.regexp ",") (Str.matched_group 2 s) in
    (fname, List.map parse_test sl)
  else
    failwith (Printf.sprintf "invalid sample %s" s)

(* "parse_date s" extracts the date from a date-string of a specific format.
    e.g. parse_date "date 12345" = 12345 *)
let parse_date s =
  let date_regexp = Pcre.regexp "^date ([0-9]+)$" in
  try
    let substrings = Pcre.exec ~rex:date_regexp s in
    float_of_string(Pcre.get_substring substrings 1)
  with Not_found -> failwith (Printf.sprintf "invalid date %s" s)

let parse_benchmark filename =
  let ic = open_in filename in
  let d = parse_date (input_line ic) in
  let h = Hashtbl.create 17 in
  begin try while true do
    let (f,l) = parse_sample (input_line ic) in
    Hashtbl.add h f l
  done with End_of_file -> close_in ic end;
  (d,h)

(* -------------------------------------- *)

(* Printing *)
let pp_benchmark fmt (ut,h) =
  let pp_t fmt t = Format.fprintf fmt "%s" (Benchmark.to_string ~fdigits:6 t) in
  Format.fprintf fmt "date %.f@." ut;
  Hashtbl.iter (fun s tl ->
    Format.fprintf fmt "%s : @[<h>%a@]@," s (pp_list ~sep:"," pp_t) tl
  ) h
;;

let save_benchmark ?(dirname=".benchmarks") (ut,h) =
  let fname = Printf.sprintf "%.f.bench" ut in
  if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
  let file = (Filename.concat dirname (Filename.basename fname)) in
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a" pp_benchmark (ut,h) ;
  close_out oc
;;

let by_date x y = int_of_float ((fst x) -. (fst y))

let parse_benchmarks ?(days=7) ?(dirname=".benchmarks") () =
  let l = ref [] in
  let a = Sys.readdir dirname in
  if Array.length a > 0 then
    for i=0 to (min days ((Array.length a)-1)); do
      let fname = a.(i) in
      let file = (Filename.concat dirname (Filename.basename fname)) in
      let (date,h) = parse_benchmark file in
      l := (date,h)::!l
    done;
  List.sort ~cmp:by_date !l

module StringSet = Set.Make(String)

let pp_benchmarks fmt data =
  if List.length data = 0 then 
    Format.fprintf fmt "Sample Set empty, nothing to print"
  else begin 
    let error = 0.001 in
    let fa =
      Array.of_list (
        StringSet.elements (
          List.fold_left (fun s (_,h) ->
            Hashtbl.fold (fun k _ s -> StringSet.add k s) h s
          ) StringSet.empty data
        )
      )
    in
    let func_size = Array.length fa in
    let data_size = List.length data in
    let pp_cell fmt e = Format.fprintf fmt "%s" e in
    let h = Array.init (func_size+1) (function 0 -> "Date" |n -> fa.(n-1)) in
    let t = Array.make_matrix (List.length data) (func_size+1) "" in
    let last = Array.make func_size max_float in
    let diff a b = (abs_float(a -. b)) > error in
    List.iteri (fun i (ut,h) ->
      (* we need to consider the list from the less recent to the more recent, but
       * then I we want to print the from the most recent to the less recent *)
      let i = data_size - i -1 in
      t.(i).(0) <- string_of_date ut;
      for j = 0 to func_size-1 do
        let avg = 
          try
            match Hashtbl.find h fa.(j) with
            |[] -> "n/a"
            |h::_ -> begin
              (* XXX : I should only compare up to 3 decimal digits *)
                let a = h.Benchmark.utime /. Int64.to_float(h.Benchmark.iters) in
                let res = 
                  if diff last.(j) a && last.(j) > 0. && a > last.(j) then
                    Printf.sprintf "%.03f(*)" a
                  else 
                    Printf.sprintf "%.03f" a
                in
                if a < last.(j) then last.(j) <- a;
                res
            end
          with Not_found -> "X"
        in
        t.(i).(j+1) <- avg
      done
    ) data;
    pp_tables (pp_row pp_cell) fmt (h,t)
  end
;;

let make_benchmark l =
  let h = Hashtbl.create (List.length l) in
  List.iter (fun (s,sl) -> Hashtbl.add h s sl) l;
  (Unix.time(),h)

module Options = struct
  open OptParse

  let verbose = StdOpt.incr_option ()
  let run = StdOpt.store_true ()
  let save = StdOpt.store_false ()
  let show = StdOpt.store_false ()


  let description = ""
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'v' ~help:"Print information (can be repeated)" verbose;
  add options ~short_name:'r' ~long_name:"run" ~help:"run all tests" run;
  add options ~long_name:"nosave" ~help:"not save test results" save;
  add options ~long_name:"noshow" ~help:"not show test results" show;
end

let main run =
  ignore(OptParse.OptParser.parse_argv Options.options);

  if OptParse.Opt.get Options.run then begin
    let b = make_benchmark (run ()) in
    if OptParse.Opt.get Options.save then
      save_benchmark b
    else if not(OptParse.Opt.get Options.show) then
      Format.printf "%a@." pp_benchmarks [b]
  end ;
  (* this will also read the new benchmark *)
  if OptParse.Opt.get Options.show then
    let l = parse_benchmarks () in
    Format.printf "%a@." pp_benchmarks l
;;

