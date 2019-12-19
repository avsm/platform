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

open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let enable_debug = function
  |0 -> () (* only warning messages : default *)
  |1 -> Util.Info.all_enabled ()
  |2 -> 
      begin
        Util.Info.all_enabled ();
        Util.Notice.all_enabled ()
      end
  |_ ->
      begin
        Util.Info.all_enabled () ;
        Util.Notice.all_enabled ();
        Util.Debug.all_enabled ()
      end
;;

let all_quiet t =
  if t then begin
    Util.Info.all_disabled ();
    Util.Notice.all_disabled ();
    Util.Warning.all_disabled ();
    Util.Debug.all_disabled ();
    List.iter Util.Progress.disable (Util.Progress.available ())
  end
;;

let enable_bars verbose l =
  if verbose then List.iter Util.Progress.enable l

let enable_timers verbose l = 
  at_exit (Util.Timer.dump Format.err_formatter);
  if verbose then List.iter Util.Timer.enable l
;;

