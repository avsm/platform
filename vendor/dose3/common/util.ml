(*****************************************************************************)
(*  Copyright (C) 2009  <pietro.abate@pps.jussieu.fr>                        *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

(** return a unique identifier *)
let uuid () =
  let rand =
    let s = Random.State.make_self_init () in
    fun () -> Random.State.bits s
  in
  Digest.to_hex (Digest.string (string_of_int (rand ())))

(* This algorithm runs in O(n) . does not preserve ordering - 
   returns elements in reverse order *)
(* XXX it would be nice to add a comparison function here... *)
let list_unique l = 
  let seen = Hashtbl.create (List.length l) in
  let rec add acc = function
    |hd :: tl when not (Hashtbl.mem seen hd) ->
        begin
          Hashtbl.add seen hd ();
          add (hd :: acc) tl
        end
    |_ :: tl -> add acc tl
    |[] -> acc
  in
  add [] l

(* standard memoization function *)
let memo f =
  let h = Hashtbl.create 1023 in
  fun i ->
    try Hashtbl.find h i
    with Not_found -> begin
      let r = f i in
      Hashtbl.add h i r ;
      r
    end

let timestamp () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
;;

let max32int =
  if Int32.to_int(Int32.max_int) < 0 then
    max_int
  else Int32.to_int(Int32.max_int)
;;

type label = string

module type Messages = sig
  type t
  val create: ?enabled:bool -> label -> t
  val eprintf: ?raw:bool -> t -> ('a, unit, string, unit) format4 -> 'a
  val enable : label -> unit
  val disable : label -> unit
  val all_disabled : unit -> unit
  val all_enabled : unit -> unit
  val avalaible : unit -> label list
  val is_enabled : label -> bool
end

(** Debug messages are printed immediately on stderr. 
 * They can enabled or disabled (default) *)
module MakeMessages(X : sig val label : string end) = struct
  type t = {
    label : string;
    mutable enabled : bool
  } 
  let messages = Hashtbl.create 10

  let clean label =
    try 
      let s = Filename.chop_extension (Filename.basename label) in
      String.capitalize s
    with Invalid_argument _ -> label

  let create ?(enabled=false) label =
    if label = "" then begin
      Printf.eprintf "Logging Label Empty\n";
      Pervasives.exit (64);
    end else if not (Hashtbl.mem messages label) then
      let t = { label = clean label ; enabled = enabled } in
      Hashtbl.add messages (clean label) t ;
      t
    else begin
      Printf.eprintf "The label (%s) %s already exists\n" X.label label;
      Pervasives.exit (64);
    end

  let eprintf ?(raw=false) t fmt =
    Printf.ksprintf (
      if t.enabled then begin
        (fun s -> 
          if raw then
            Printf.eprintf "(%s)%s: %s" X.label t.label s
          else
            Printf.eprintf "(%s)%s: %s\n%!" X.label t.label s
        )
      end else ignore
    ) fmt

  let onoff s b =
    try let t = Hashtbl.find messages s in t.enabled <- b
    with Not_found ->
      Printf.eprintf "Warning: debug label %s not found\n" s

  let all_enabled () = Hashtbl.iter (fun _ t -> t.enabled <- true) messages
  let all_disabled () = Hashtbl.iter (fun k t -> t.enabled <- false) messages
  let enable s = onoff s true
  let disable s = onoff s false

  let avalaible () = Hashtbl.fold (fun k _ acc -> k::acc) messages []
  let is_enabled s =
    try let t = Hashtbl.find messages s in t.enabled
    with Not_found -> begin
      Printf.eprintf "Warning: debug label %s not found\n" s;
      false
    end

end

(* this way we can have the same label for different messages *)
module Info = MakeMessages(struct let label = "I" end)
module Notice = MakeMessages(struct let label = "N" end)
module Warning = MakeMessages(struct let label = "W" end)
module Debug = MakeMessages(struct let label = "D" end)

module Logging(X : sig val label : string end) = struct

  let it = Info.create X.label
  let info fmt = Info.eprintf it fmt

  let nt = Notice.create X.label
  let notice fmt = Notice.eprintf nt fmt

(* warning is enabled by default *)
  let wt = Warning.create ~enabled:true X.label
  let warning fmt = Warning.eprintf wt fmt

  let dt = Debug.create X.label
  let debug fmt = Debug.eprintf dt fmt

  let fatal fmt = 
    let l = Printf.sprintf "Fatal error in module %s: " X.label in
    Printf.kprintf (fun s ->
      Printf.eprintf "%s\n %s\n%!" l s; 
      Pervasives.exit (64)
    ) fmt
end

#define __label __FILE__
let label =  __label ;;
include Logging(struct let label = label end) ;;

(** Printf bars are printed immediately on stderr.
 * they can be enabled or disabled (default) *)
module Progress = struct
  type t = {
    name : string ;
    buffer : Buffer.t ;
    mutable total : int ;
    mutable perc : int ;
    mutable rotation : int ;
    mutable enabled : bool ;
    mutable unbounded : bool ;
  }

  let columns = 75 
  let full = " %100.0\n" 
  let rotate = "|/-\\"
  let bars = Hashtbl.create 10

  let create ?(enabled=false) ?(total=0) ?(unbounded=false) s =
    let c = {
      name = s;
      buffer = Buffer.create columns ;
      total = if unbounded then 100 else total ;
      perc = 0 ;
      rotation = 0 ;
      enabled = enabled ;
      unbounded = unbounded }
    in
    Hashtbl.add bars s c;
    c

  let enable s =
    try let t = Hashtbl.find bars s in t.enabled <- true
    with Not_found -> warning "Progress Bar %s not found" s

  let disable s =
    try let t = Hashtbl.find bars s in t.enabled <- false
    with Not_found -> warning "Progress Bar %s not found" s

  let available () = Hashtbl.fold (fun k _ acc -> k::acc) bars []

  let set_total c total =
    if not c.unbounded then c.total <- total
    else warning "%s is an unbounded progress bar. Cannot set total" c.name

  let reset c =
    (* if c.enabled then Printf.eprintf "\n%!"; *)
    Buffer.clear c.buffer;
    c.perc <- 0;
    c.rotation <- 0

  let progress ?(i=1) c =
    if c.enabled then begin
      c.perc <- c.perc + i;
      Buffer.clear c.buffer;
      Buffer.add_char c.buffer '\r';
      Buffer.add_string c.buffer c.name;
      let f = floor (1000.0 *. (float c.perc) /. (float c.total)) in
      let f = f /. 10.0 in
      if f = 100.0 && not c.unbounded then 
        Buffer.add_string c.buffer full
      else begin
        c.rotation <- (1 + c.rotation) land 3;
        Printf.bprintf c.buffer "%c %%%4.1f" rotate.[c.rotation] f
      end ;
      Printf.eprintf "%s%!" (Buffer.contents c.buffer)
    end

end

(** Timers are printed all together when the function dump is called.
 * they can be enabled or disabled (default) *)
module Timer = struct
  type t = {
    name: string;
    mutable total : float;
    mutable last  : float;
    mutable is_in : bool;
    mutable enabled : bool;
  }

  let timers = Hashtbl.create 10
  let gettimeofday = ref (fun _ -> 0.)
  let () = gettimeofday := Unix.gettimeofday

  let pp_timer fmt c =
    if c.total = 0. then
      Format.fprintf fmt "Timer %s: n/a@." c.name
    else
      Format.fprintf fmt "Timer %s: %f.@." c.name c.total

  let dump fmt () =
    Hashtbl.iter (fun _ c -> if c.enabled then pp_timer fmt c) timers

  let create ?(enabled=false) s =
    let c = { 
      name = s;
      total = 0.;
      last = 0.;
      is_in = false ;
      enabled = enabled } 
    in
    Hashtbl.add timers s c;
    c

  let enable s =
    try let t = Hashtbl.find timers s in t.enabled <- true
    with Not_found -> warning "Timer %s not found" s

  let available () = Hashtbl.fold (fun k _ acc -> k::acc) timers []

  let start c =
    c.is_in <- true;
    c.last <- !gettimeofday()

  let stop c x =
    assert(c.is_in);
    c.is_in <- false;
    c.total <- c.total +. (!gettimeofday() -. c.last);
    x
end

let pp_process_time fmt () =
  let pt = Unix.times () in
  Printf.fprintf fmt "Process time (user):  %5.2f\n%!" pt.Unix.tms_utime;
  Printf.fprintf fmt "Process time (sys):   %5.2f\n%!" pt.Unix.tms_stime
;;

module StringHashtbl = Hashtbl.Make (
  struct
    type t = string
    let equal (a : string) (b : string) = (a = b)
    let hash s = Hashtbl.hash s
  end
)

let hits = ref 0;;
let miss = ref 0;;
let hashcons table x =
  try 
    let s = StringHashtbl.find table x in
    incr hits;
    s 
  with Not_found ->
    incr miss;
    StringHashtbl.add table x x; x
;;

module StringPairHashtbl = Hashtbl.Make (
  struct
    type t = string * string
    let equal (a : string * string) (b : string * string) = (a = b)
    let hash s = Hashtbl.hash s
  end
)

module IntHashtbl = Hashtbl.Make (
  struct
    type t = int
    let equal (a : int) (b : int) = (a = b)
    let hash i = Hashtbl.hash i
  end
)

module IntPairHashtbl = Hashtbl.Make (
  struct
    type t = int * int
    let equal (a : int * int) (b : int * int) = (a = b)
    let hash i = Hashtbl.hash i
  end
)

let range i j =
 let rec aux acc n =
   if n < i then acc else aux (n::acc) (n-1)
 in aux [] j
;;

let string_of_list ?(delim=("","")) ?(sep=",") string_of_item l =
  let buf = Buffer.create 1023 in
  let rec aux = function
    | [] -> assert false
    | [last] -> (* last item, no trailing sep *)
        Buffer.add_string buf (string_of_item last)
    | item :: tl -> (* at least one item in tl *)
        Buffer.add_string buf (string_of_item item);
        Buffer.add_string buf sep;
        aux tl 
  in
  Buffer.add_string buf (fst delim);
  begin match l with
    | [] -> ()
    | [sole] -> Buffer.add_string buf (string_of_item sole)
    | _ -> aux l 
  end;
  Buffer.add_string buf (snd delim);
  Buffer.contents buf
;;

class type projection =
  object
    method add : int -> unit
    method inttovar : int -> int
    method vartoint : int -> int
    method size : int
  end

(** associate a sat solver variable to a package id *)
class intprojection size = object

  val vartoint = IntHashtbl.create (2 * size)
  val inttovar = Array.make size 0
  val size = size
  val mutable counter = 0

  (** add a package id to the map *)
  method add v =
    if (size = 0) then assert false ;
    if (counter > size - 1) then assert false;
    IntHashtbl.add vartoint v counter;
    inttovar.(counter) <- v;
    counter <- counter + 1

  (** given a package id return a sat solver variable 
      raise Not_found if the package id is not known *)
  method vartoint v = IntHashtbl.find vartoint v

  (* given a sat solver variable return a package id *)
  method inttovar i =
    if (i >= size) then fatal "out of boundary i = %d size = %d" i size;
    inttovar.(i)

  method size = size
end

class identity = object
  method add (v : int) =
    warning "There is no point of adding element to the identity projection"; ()
  method vartoint (v : int) = v
  method inttovar (v : int) = v
  method size =
    warning "This is the identity projetion. size unknown"; 0
end

