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

(* a time-stamped collection of samples *)
type benchmark 

(* create a time-stamped collection of samples *)
val make_benchmark : Benchmark.samples -> benchmark

(** parse a string representing a value of type Benchmark.t
    Benchmark.to_string ~fdigits:6 (Benchmark.make (Int64.of_int 4))
    46.194004 WALL (45.626852 usr + 0.144009 sys = 45.770861 CPU) @ 0.087392/s (n=4)
 *)
val parse_test : string -> Benchmark.t

(** parse a string representing a sample of the form :
    func_name : 46.194004 WALL (45.626852 usr + 0.144009 sys = 45.770861 CPU) @ 0.087392/s (n=4)
  *)
val parse_sample : string -> string * Benchmark.t list

(** parse a benchmark file of the form :

date 1283531842
fname1 : 43.240758 WALL (43.222701 usr + 0.012001 sys = 43.234702 CPU) @ 0.092518/s (n=4)
fname2 : 46.194004 WALL (45.626852 usr + 0.144009 sys = 45.770861 CPU) @ 0.087392/s (n=4)
fname3 : 43.600401 WALL (43.358710 usr + 0.028002 sys = 43.386712 CPU) @ 0.092194/s (n=4)

 *)
val parse_benchmark : string -> benchmark

(** save a benchmark *)
val save_benchmark : ?dirname:string -> benchmark -> unit

(** parse all benchmarks in the benchmark's directory (.benchmark by default) *) 
val parse_benchmarks : ?days:int -> ?dirname:string -> unit -> benchmark list

(** pretty print a [Benchmark.sample] *)
val pp_benchmark : Format.formatter -> benchmark -> unit

(** pretty print a table 
    highlight difference bigger then [error]
 *)
val pp_benchmarks : Format.formatter -> benchmark list -> unit

val main : ( unit -> Benchmark.samples ) -> unit
