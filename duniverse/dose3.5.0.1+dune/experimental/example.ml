(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  ADD authors here                                     *)
(*                                                                        *)
(*  Contributor(s):  ADD minor contributors here                          *)
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
open Algo
open Doseparse

module Options = struct
  open OptParse
  let options = OptParser.make ~description:"add a decription here"
  include StdOptions.MakeOptions(struct let options = options end)

  let fail = StdOpt.store_true () 

  open OptParser ;;
  add options ~short_name:'f' ~long_name:"fail" ~help:"exit with a failoure" fail;

end

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let main () =

  let args = OptParse.OptParser.parse_argv Options.options in
  
  (* enable info / warning / debug information *)
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose);
  
  (* enable a selection of progress bars *)
  StdDebug.enable_bars (OptParse.Opt.get Options.progress) [] ;

  (* enable a selection of timers *)
  StdDebug.enable_timers (OptParse.Opt.get Options.timers) [];
  StdDebug.all_quiet (OptParse.Opt.get Options.quiet);

  info "print some verbose info";
  warning "print some warnings";
  debug "print some debug";
  
  List.iter (Printf.printf "Arg : %s\n") args;
  
  if OptParse.Opt.get Options.fail then
    fatal "this is a fatal error"
  ;

  (* add here the rest *)
;;

main ();;

