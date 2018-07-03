(* Copyright (c) 2017-2018 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** Manipulate, parse and generate OCaml version strings.

    These are of the form [major.minor.patch+extra], where the
    [patch] and [extra] fields are optional.  *)

type t
(** Type of an OCaml version string *)

val v : ?patch:int -> ?extra:string -> int -> int -> t
(** [v ?patch ?extra major minor] will construct an OCaml
    version string with the appropriate parameters.  The
    [patch] and [extra] indicators are optional, but it is
    conventional to include a [patch] value of 0 for most
    recent OCaml releases. *)

(** {2 Parsers and serializers} *)

val to_string : ?sep:char -> t -> string
(** [to_string ?sep t] will convert the version [t] into
    a human-readable representation.  The [sep] will default
    to [+] which is the normal representation of extra
    version strings, but can be changed to another character
    by supplying [sep].  One such usecase is to generate
    Docker container tags from OCaml version strings, where
    only dashes and alphanumeric characters are allowed. *)

val of_string : string -> (t, [> `Msg of string ]) result
(** [of_string t] will parse the version string in [t].
    The return value is compatible with the {!Result}
    combinators defined in the [rresult] library. *)

val of_string_exn : string -> t
(** [of_string_exn t] behaves as {!of_string} but raises
    [Invalid_argument] if the string cannot be parsed. *)

val compare : t -> t -> int
(** [compare a b] is the comparison function for two OCaml
    version strings. Returns [-1] if [a<b], [0] if they are
    equal and [1] if [a>b]. Comparison is done using integer
    comparison for the major, minor and patch versions, and
    lexical comparison for any extra version strings present. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] will output a human-readable version string of
    [t] to the [fmt] formatter. *)

(** {2 Architecture Support }
    These definitions cover the CPU architectures that OCaml
    runs and is supported on. *)

type arch = [ `X86_64 | `Aarch64 | `Ppc64le ]
(** Type of CPU architectures.
    TODO: This is currently an incomplete list, and lists just
    those used by the opam test systems. Contributions welcome
    to complete it. *)

val arches : arch list
(** [arches] is an enumeration of all of the possible {!arch} values. *)

val string_of_arch : arch -> string
(** [string_of_arch arch] will convert [arch] into a human-readable
    CPU architecture string.  The result will follow the
    {{:https://golang.org/doc/install/source#environment}GOARCH}
    convention used by Golang. *)

val arch_of_string : string -> (arch, [> `Msg of string ]) result
(** [arch_of_string t] will parse the architecture string in [t].
    The return value is compatible with the {!Result}
    combinators defined in the [rresult] library. This function
    is liberal and will attempt to understand variants of the
    same architecture.  For example, both [aarch64] and [arm64]
    are parsed into {!`Aarch64}. *)

val arch_of_string_exn: string -> arch
(** [arch_of_string_exn t] is the same as {!arch_of_string},
    except that it raises [Invalid_argument] in case of error. *)

(** {2 Accessors} *)

val major : t -> int
(** [major t] will return the major version number of an OCaml
    release.  For example, [of_string "4.03.0" |> major] will
    return [4]. *)

val minor : t -> int
(** [minor t] will return the minor version number of an OCaml
    release.  For example, [of_string "4.03.0" |> minor] will
    return [3]. *)

val patch : t -> int option
(** [patch t] will return the patch version number of an OCaml
    release.  For example, [of_string "4.03.0" |> minor] will
    return [Some 0]. *)

val extra : t -> string option
(** [extra t] will return the additional information string of
    an OCaml release.
    For example, [of_string "4.03.0+flambda" |> extra] will
    return [Some "flambda"]. *)

val with_variant : t -> string option -> t
(** [with_variant t extra] will return a fresh value with
    the extra version information in [t] to
    [extra], and remove it if [None] is supplied. *)

val with_patch : t -> int option -> t
(** [with_patch t patch] will return a fresh value with
    the patch number in [t] to [patch], and remove it if [None]
    is supplied. *)

val without_patch : t -> t
(** [without_patch t] is as {!with_patch} [t None]. It removes
    the least significant number from the version string.
    This is useful for the Docker OCaml containers, which are
    all named without a patch number and compiled using the
    latest patch release (e.g. [4.06] instead of [4.06.1]). *)

(** {2 Constants } *)
 
val sys_version : t
(** [sys_version] is the version of OCaml that this library is
    currently compiled with, which will be the same as
    {!Sys.ocaml_version}. *)

(** Values representing official releases of OCaml. *)
module Releases : sig

  val v4_00_1 : t
  (** Version 4.00.1 *)

  val v4_00 : t
  (** Latest release in the 4.00.x series *)

  val v4_01_0 : t
  (** Version 4.01.0 *)

  val v4_01 : t
  (** Latest release in the 4.01.x series *)

  val v4_02_0 : t
  (** Version 4.02.0 *)

  val v4_02_1 : t
  (** Version 4.02.1 *)

  val v4_02_2 : t
  (** Version 4.02.2 *)

  val v4_02_3 : t
  (** Version 4.02.3 *)

  val v4_02 : t
  (** Latest release in the 4.02.x series *)

  val v4_03_0 : t
  (** Version 4.03.0 *)

  val v4_03 : t
  (** Latest release in the 4.03.x series *)

  val v4_04_0 : t
  (** Version 4.04.0 *)

  val v4_04_1 : t
  (** Version 4.04.1 *)

  val v4_04_2 : t
  (** Version 4.04.2 *)

  val v4_04 : t
  (** Latest release in the 4.04.x series *)

  val v4_05_0 : t
  (** Version 4.05.0 *)

  val v4_05 : t
  (** Latest release in the 4.05.x series *)

  val v4_06_0 : t
  (** Version 4.06.0 *)

  val v4_06_1 : t
  (** Version 4.06.1 *)

  val v4_06 : t
  (** Latest release in the 4.06.x series *)

  val all_patches : t list
  (** [all_patches] is an enumeration of all OCaml releases, including every patch release.
    To get the major and minor releases with the latest patch version, use {!all} instead. *)

  val all : t list
  (** [all] is an enumeration of all the OCaml releases, with the latest patch versions in
    each major and minor release. *)

  val dev : t list
  (** Enumeration of the latest development OCaml releases.
    This is usually just one, but may include two active dev
    versions if a release branch has just been cut. *)

  val latest: t
  (** [latest] is the most recent stable release of OCaml. *)

  val recent : t list
  (** [recent] is the last five releases of OCaml, with each at the latest patch level.
    This is the set that is most reliably tested in the opam package repository. *)

  val recent_with_dev : t list
  (** [recent_with_dev] are the last four stable releases of OCaml and the latest
    development branches. *)
end

(** {2 Feature Selection} *)

(** Determine which release a feature or architecture first appeared in. *)
module Since : sig
 
  val bytes: t
  (** [bytes] is the release that the {!Bytes} module first appeared in. *)

  val arch : arch -> t 
  (** [arch a] will return the first release of OCaml that the architecture
    was reasonably stably supported on. *)
end

(** Test whether a release has a given feature. *)
module Has : sig

  val bytes : t -> bool
  (** [bytes t] will return {!true} if that release has a {!bytes} type.
    Note that opam provides a [bytes] compatibility package for older releases. *)

  val arch : arch -> t -> bool
  (** [arch a t] will return {!true} if architecture [a] is supported on release [t]. *)
end

(** Opam compiler switches.
    These are available from the public {{:https://github.com/ocaml/opam-repository}opam-repository}. *)
module Opam : sig

  val variants : t -> string list
  (** [variants t] lists the compiler variants that are available in
    opam for [t] strings. This is returned as a string that can be passed to
    {!with_variant} to form a full version (or use {!switches} as a convenience). *)

  val default_variant : t -> string option
  (** [default_variant t] returns [Some v] if a variant exists by
    default for version [t].  This is typically true for development
    versions of the compiler that have a branch name such as [trunk]
    appended to their switch name.  {!default_switch} combines this into a
    full OCaml version. *)

  val default_switch : t -> t
  (** [default_switch t] is the name of the switch of the stock version of [t].
    This is normally just the version number, but can include an extra version
    string such as [trunk] for development versions of the compiler. *)

  val switches : t -> t list
  (** [switches t] lists all the available opam switches for compiler version [t].
    This list includes the default compiler, and any available variants such as
    [flambda]. *)

  val variant_switches : t -> t list
  (** [variant_switches t] lists all the non-default switch versions available
    for compiler version [t].  This filters out the default variant of the compiler. *)

  (** Opam 2.0 functions *)
  module V2 : sig
    val package : t -> string
    (** [package t] returns the opam2 package for that compiler version. *)
  end
end
