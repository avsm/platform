(* Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
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

(** Data structures for accessing results of opam2 package build results *)

(** Index of all the opam2 builds. The [Index] module contains types for the
    opam2 bulk build results. *)
module Index : sig
  (** [deps] is a list of package dependencies, where each entry is a tuple of
      [name, version, status] *)
  type deps = (string * string * [`Fail | `Ok | `Skipped]) list
  [@@deriving sexp]

  (** [result] represents the exit result of an [opam] invocation. The raw exit
      codes are parsed to look for special opam exit codes, which indicate
      errors such as the internal solver failing to find a result. *)
  type result =
    [ `Ok
    | `Fail of int * deps
    | `Depfail of deps
    | `Uninstallable of string list
    | `No_sources of string list
    | `Solver_failure ]
  [@@deriving sexp]

  val pp_result : Format.formatter -> result -> unit
  (** [pp_result ppf result] will print a human-readable result to formatter
      [ppf] *)

  (** [params] represents some of the build parameters that opam packages are
      tested against. These include the CPU architecture, OS distribution and
      OCaml compiler version. *)
  type params =
    { arch: Dockerfile_distro.arch  (** CPU architecture *)
    ; distro: Dockerfile_distro.t  (** Operating system distribution *)
    ; ov: Ocaml_version.t  (** OCaml compiler version *) }
  [@@deriving sexp]

  (** [metadata] contains the results and parameters for a single build run of
      a package and version. *)
  type metadata =
    { rev: string
          (** opam-repository git revision hash that this run was built against *)
    ; params: params  (** build {!params} for this run *)
    ; build_result: result
          (** result of the execution of this set of parameters *)
    ; duration: float
          (** wall clock time of the duration of the build in Unix seconds *)
    ; log: string list
          (** list of log lines. This is blank unless there is an error *) }
  [@@deriving sexp]

  (** [pkg] collects all the metadata for all versions of a given opam package. *)
  type pkg =
    { name: string  (** opam package name *)
    ; mutable maintainers: string list
          (** list of maintainers associated with this package. As a heuristic,
              all of the maintainers listed for all versions are bundled
              together here for simplicity. *)
    ; mutable tags: string list
          (** list of tags associated with this package. As a heuristic, all of
              the tags listed of all versions are bundled together here for
              simplicity. *)
    ; mutable versions: (string * metadata list) list
          (** list of a tuple of opam package version and the list of build
              results. There is a list of results since there are usually
              multiple different runs for every version, to test it against
              different compiler and CPU/OS combinations. *) }
  [@@deriving sexp]

  (** [t] contains a version number of the metadata system, which is set to
      {!current_version} for the latest builds. Older clients can parse the
      sexpression to determine if they need to upgrade for a newer format. The
      [CHANGES.md] file in the Obi source code repository tracks metadata
      version number bumps. *)
  type t = {version: int; packages: pkg list} [@@deriving sexp]

  val current_version : int
  (** [current_version] is a monotonically increasing number for the version of
      Obi in use. *)

  (** [maintainers] is an association list of the package name (including
      version string) to the list of maintainers registered for that package.
      As a heuristic, all of the maintainers listed for all versions are
      bundled here for simplicity. *)
  type maintainers = (string * string list) list [@@deriving sexp]

  (** [tags] is an association list of the package name (including version
      string) to the list of tags registered for that package. As a heuristic,
      all of the tags listed for all versions are bundled here for simplicity. *)
  type tags = (string * string list) list [@@deriving sexp]
end

(** Results of individual builds for a particular parameters. This is not
    intended for direct use by users, as the results are collected and
    aggregated into the {!Index} module which is easier to consume by tools. *)
module Builds : sig
  (** [build_result] records the results of a single job build. *)
  type build_result =
    { code: [`Signaled of int | `Exited of int]
    ; actions: string
    ; start_time: float
    ; end_time: float }
  [@@deriving sexp]

  (** [pkg] records the results of the runs for every version of a package. *)
  type pkg = {name: string; versions: (string * build_result) list}
  [@@deriving sexp]

  (** [params] is the configuration for a run. *)
  type params =
    { arch: Dockerfile_distro.arch
    ; distro: Dockerfile_distro.t
    ; ov: Ocaml_version.t }
  [@@deriving sexp]

  (** [batch] represents the results of a bulk build with one configuration
      against a single opam-repository revision. *)
  type batch =
    { rev: string  (** opam-repository git revision *)
    ; params: params
    ; pkgs: pkg list }
  [@@deriving sexp]
end

module VersionCompare : sig
  val compare : string -> string -> int
end

module OpamJsonActions : sig
  val installs : Ezjsonm.value -> Index.deps
end
