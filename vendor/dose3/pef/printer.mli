(**************************************************************************************)
(*  Copyright (C) 2015 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2015 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

val string_of_vpkg : Packages_types.vpkg -> string
val string_of_vpkglist : Packages_types.vpkglist -> string
val string_of_vpkgformula : Packages_types.vpkgformula -> string

val string_of_builddep : Packages_types.builddep -> string
val string_of_builddeplist : Packages_types.builddepslist -> string
val string_of_builddepformula : Packages_types.builddepsformula -> string

val string_of_vpkgreq : Packages_types.vpkgreq -> string

val pp_vpkg : out_channel -> Packages_types.vpkg -> unit
val pp_vpkglist : out_channel -> Packages_types.vpkglist -> unit
val pp_vpkgformula : out_channel -> Packages_types.vpkgformula -> unit

val pp_builddep : out_channel -> Packages_types.builddep -> unit
val pp_builddepformula : out_channel -> Packages_types.builddepsformula -> unit
val pp_builddeplist : out_channel -> Packages_types.builddepslist -> unit

val pp_string_list : ?sep:string -> out_channel -> string * string list -> unit
val pp_function : out_channel -> tostring:('a -> string) -> string * 'a -> unit

val pp_string_wl: out_channel -> string * string -> unit
val pp_bool_wl: out_channel -> string * bool -> unit
val pp_yes_wl: out_channel -> string * bool -> unit

val pp_vpkglist_wl : out_channel -> string * Packages_types.vpkglist -> unit
val pp_vpkgformula_wl : out_channel -> string * Packages_types.vpkgformula -> unit
val pp_builddepformula_wl : out_channel -> string * Packages_types.builddepsformula -> unit
val pp_builddeplist_wl : out_channel -> string * Packages_types.builddepslist -> unit

val pp_function_wl : out_channel -> tostring:('a -> string) -> string * 'a -> unit
val pp_string_list_wl : ?sep:string -> out_channel -> string * string list -> unit
