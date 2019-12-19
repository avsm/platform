(** Information about a package defined in the workspace *)

open! Stdune

module Name : sig
  type t

  val of_string : string -> t

  val parse_string_exn : Loc.t * string -> t

  val opam_fn : t -> string

  val version_fn : t -> string

  include Interned.S with type t := t

  include Dune_lang.Conv.S with type t := t

  module Infix : Comparator.OPS with type t = t

  val to_dyn : t -> Dyn.t

  val of_basename : string -> t option
end

module Dependency : sig
  module Op : sig
    type t =
      | Eq
      | Gte
      | Lte
      | Gt
      | Lt
      | Neq
  end

  module Constraint : sig
    module Var : sig
      type t =
        | QVar of string
        | Var of string
    end

    type t =
      | Bvar of Var.t
      | Uop of Op.t * Var.t
      | And of t list
      | Or of t list
  end

  type t =
    { name : Name.t
    ; constraint_ : Constraint.t option
    }

  val opam_depend : t -> OpamParserTypes.value

  val to_dyn : t -> Dyn.t

  val decode : t Dune_lang.Decoder.t
end

module Kind : sig
  type has_opam = bool

  type t =
    | Dune of has_opam
    | Opam
end

module Source_kind : sig
  type t =
    | Github of string * string
    | Url of string

  val to_dyn : t Dyn.Encoder.t

  val pp : Format.formatter -> t -> unit

  val decode : t Dune_lang.Decoder.t
end

module Info : sig
  type t

  val source : t -> Source_kind.t option

  val license : t -> string option

  val authors : t -> string list option

  val homepage : t -> string option

  val bug_reports : t -> string option

  val documentation : t -> string option

  val maintainers : t -> string list option

  val empty : t

  val to_dyn : t Dyn.Encoder.t

  val decode :
       ?since:Dune_lang.Syntax.Version.t
    -> unit
    -> t Dune_lang.Decoder.fields_parser

  val superpose : t -> t -> t
end

type t =
  { name : Name.t
  ; loc : Loc.t
  ; synopsis : string option
  ; description : string option
  ; depends : Dependency.t list
  ; conflicts : Dependency.t list
  ; depopts : Dependency.t list
  ; info : Info.t
  ; path : Path.Source.t
  ; version : string option
  ; kind : Kind.t
  ; tags : string list
  ; deprecated_package_names : Loc.t Name.Map.t
  }

val file : dir:Path.t -> name:Name.t -> Path.t

val decode : dir:Path.Source.t -> t Dune_lang.Decoder.t

val opam_file : t -> Path.Source.t

val meta_file : t -> Path.Source.t

val deprecated_meta_file : t -> Name.t -> Path.Source.t

val to_dyn : t -> Dyn.t

val hash : t -> int

val is_opam_file : Path.t -> bool
