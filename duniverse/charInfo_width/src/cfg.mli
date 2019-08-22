(** {2 Cfg.widthTable}

  The configuration file contains width info that can be loaded by [load_from_path path_of_config_file]. The config syntax is a subset of OCaml. You can define several codes set in it:
  - unprintable
  - combining
  - w2
  - w3
  - w4
  - w5
  - w6

  The type of there value is [(int * int) list].

  We don't have to define all of the values and the sequence of definition doesn't matter.

  Here is a sample config file:
{[
  let unprintable= [ (888, 0x379)(* dec, hex *); (0b1110001011, 0o1613)(* bin, oct *) ]
  let w2= [(0x01c4, 0x01cc)] (* Ǆ  , ǌ  *)(* (* nested comments *) *)
]}
*)

type widthTable = {
  unprintable : Codes.t; (** set contains unprintable characters *)
  combining : Codes.t; (** set contains combinging characters *)
  w2 : Codes.t; (** set contains characters of width 2 *)
  w3 : Codes.t; (** set contains characters of width 3 *)
  w4 : Codes.t; (** set contains characters of width 4 *)
  w5 : Codes.t; (** set contains characters of width 5 *)
  w6 : Codes.t; (** set contains characters of width 6 *)
}

type t = widthTable

val load_from_string : string -> (t, int) Result.result
(** [load_from_string str] parse configurations in string [str] *)

val load_from_path : string -> (t, int) Result.result
(** [load_from_path path] open and loads the config file from [path] *)

val union : t -> t -> t
(** [widthTable union] *)

