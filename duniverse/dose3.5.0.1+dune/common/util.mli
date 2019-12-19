(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** return a unique identifier based on random numbers *)
val uuid: unit -> string

(** return a list of unique elements. This algorithm runs in
    O(n) but is not stable . elements are returned in reverse order *)
val list_unique : 'a list -> 'a list

(** A generic memoization function. To use with care as it allocates
    an hashtbl storing all results that will be released only on exit *)
val memo : ('a -> 'b) -> 'a -> 'b

val timestamp : unit -> string

val max32int : int

(** Debug, ProgressBars, Timers and Loggers *)

type label = string

module type Messages = sig
  type t
  (** create a new message handle with label [t] . 
   * Printing is disabled per default *)
  val create: ?enabled:bool -> label -> t

  (** Print the message on [stderr] if the Util module has been
   * set to verbose using the function [make_verbose] and
   * either the handle [t] is enable or all handles were enabled with
   * the function [all_enabled] *)
  val eprintf: ?raw : bool -> t -> ('a, unit, string, unit) format4 -> 'a

  (** [enable l] the handle with label [l] *)
  val enable : label -> unit

  (** [disable l] the handle with label [l] *)
  val disable : label -> unit

  (** disable all handles in the module *)
  val all_disabled : unit -> unit

  (** enable all handles in the module *)
  val all_enabled : unit -> unit

  (** return the list of all labels known to the module *)
  val avalaible : unit -> label list

  val is_enabled : label -> bool
end

(** Debug, Info and Warning messages are printed immediately on stderr. 
 * Info messages are enabled per default. Debug and Warning messages
 * must be enabled explicitely *)
module Debug : Messages
module Warning : Messages
module Info : Messages
module Notice : Messages

(** Ex : To use the Message framework, you should declare three functions
 * at the begin of each module as:

  let debug fmt = Util.make_debug "MyModuleLabel" fmt
  let info fmt = Util.make_info "MyModuleLabel" fmt
  let warning fmt = Util.make_warning "MyModuleLabel" fmt

  and then use these function as

  debug "this is a message string %s" "a string"

  To enable this handle, from the main program use the function
  
  Debug.enable "MyModuleLabel"

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

*)

module Logging :
  functor (X : sig val label : string end) ->
    sig
      val it : Info.t
      val info : ('a, unit, string, unit) format4 -> 'a
      val nt : Notice.t
      val notice : ('a, unit, string, unit) format4 -> 'a
      val wt : Warning.t
      val warning : ('a, unit, string, unit) format4 -> 'a
      val dt : Debug.t
      val debug : ('a, unit, string, unit) format4 -> 'a
      val fatal : ('a, unit, string, 'b) format4 -> 'a
    end

(** ProgressBars are printed immediately on stderr. 
 * To be used, the **must** be created outside the functions where
 * they are used.
 * They can enabled or disabled (default) *)
module Progress : sig
  type t
    
  (** [create "barname"] : create new a progress bar labelled "barname". 
      The progress bar is disabled by default *)
  val create: ?enabled:bool -> ?total:int -> ?unbounded:bool -> label -> t

  (** [enable "barname"] : enable the progress bar with label "barname" *)
  val enable : label -> unit

  (** [disable "barname"] : disable the progress bar with label "barname" *)
  val disable : label -> unit
  
  (** [set_total bar 10] : set the max width of the progress bar to 10 units *)
  val set_total : t -> int -> unit
  
  (** increment the progress bar of [i] units *)
  val progress : ?i:int -> t -> unit

  (** reset the progress bar *)
  val reset: t -> unit

  (** return the labels of all available progress bar *)
  val available: unit -> label list
end

(** Timers are printed all at once by the [dump] function on stderr. 
 * They can enabled or disabled (default) *)
module Timer : sig
  type t
    
  (** [create s] create and register a new logger named [s] *)
  val create: ?enabled:bool -> string -> t

  (** [enable "barname"] : enable the progress bar with label "barname" *)
  val enable : label -> unit
 
  val pp_timer: Format.formatter -> t -> unit

  (** print all timers that are enabled *)
  val dump: Format.formatter -> unit -> unit

  val start: t -> unit

  val stop: t -> 'a -> 'a

  (** return the labels of all available progress bar *)
  val available : unit -> label list
end

module IntHashtbl : Hashtbl.S with type key = int
module IntPairHashtbl : Hashtbl.S with type key = int * int
module StringHashtbl : Hashtbl.S with type key = string
module StringPairHashtbl : Hashtbl.S with type key = string * string

(** hash consing for strings *)
val hashcons: string StringHashtbl.t -> string -> string
val hits : int ref
val miss : int ref

val range : int -> int -> int list

val string_of_list :
  ?delim:(string * string) -> ?sep: string -> 
    ('a -> string) -> 'a list -> string

(** associate a sat solver variable to a package id *)
class type projection =
  object
    (** add a package id to the map *)
    method add : int -> unit
    (** given a package id return a sat solver variable 
        raise Not_found if the package id is not known *)
    method inttovar : int -> int
    (** given a sat solver variable return a package id *)
    method vartoint : int -> int
    method size : int
  end

(** identity projection *)
class identity : projection

(** [intprojection n] integer projection of size [n] *)
class intprojection : int -> projection
