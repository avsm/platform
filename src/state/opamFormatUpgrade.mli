(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** This modules handles the conversion from older repository and package
    versions to the current one *)

open OpamTypes

(** Raised when the opam root has been updated to a newer format, and further
    action (opam init/update) is needed. *)
exception Upgrade_done of OpamFile.Config.t

(** The latest version of the opam root format, that normal operation of this
    instance of opam requires *)
val latest_version: OpamVersion.t

(** Runs the upgrade from its current format to the latest version for the opam
   root at the given directory. A global write lock must be supplied. If an
   upgrade has been done, raises [Upgrade_done updated_config]. *)
val as_necessary: OpamSystem.lock -> dirname -> OpamFile.Config.t -> unit

(** Converts the opam file format, including rewriting availabillity conditions
    based on OCaml-related variables into dependencies. The filename is used to
    report errors *)
val opam_file_from_1_2_to_2_0:
  ?filename:OpamFile.OPAM.t OpamFile.t -> OpamFile.OPAM.t -> OpamFile.OPAM.t

(** Runs the opam file format from the file's format to current. Supplying
    [filename] enables additional notification messages *)
val opam_file:
  ?quiet:bool -> ?filename:OpamFile.OPAM.t OpamFile.t ->
  OpamFile.OPAM.t -> OpamFile.OPAM.t
