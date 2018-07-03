
open Common

val parse_multiarch : string * ('a * string) -> Pef.Packages_types.multiarch
val parse_source : Format822.field -> Pef.Packages_types.source
val parse_binarylist : Format822.field -> Pef.Packages_types.vpkglist

class package :
  ?name:string * Pef.Packages_types.name option ->
  ?version:string * Pef.Packages_types.version option ->
  ?depends:string * Pef.Packages_types.vpkgformula option ->
  ?conflicts:string * Pef.Packages_types.vpkglist option ->
  ?provides:string * Pef.Packages_types.vpkglist option ->
  ?recommends:string * Pef.Packages_types.vpkgformula option ->
  ?architecture:string * Pef.Packages_types.architecture option ->
  ?multiarch:string * Pef.Packages_types.multiarch option ->
  ?source:string * Pef.Packages_types.source option ->
  ?essential:string * bool option ->
  ?extra_source_only:string * bool option ->
  ?priority:string * string option ->
  ?pre_depends:string * Pef.Packages_types.vpkgformula option ->
  ?suggests:string * Pef.Packages_types.vpkgformula option ->
  ?enhances:string * Pef.Packages_types.vpkgformula option ->
  ?breaks:string * Pef.Packages_types.vpkglist option ->
  ?replaces:string * Pef.Packages_types.vpkglist option ->
  ?extras:(string * Pef.Packages.parse_extras_f option) list *
          (string * string) list option ->
  Format822.stanza ->
  object ('a)
    (** {4 Access methods } *)
    method name : Pef.Packages_types.name
    method version : Pef.Packages_types.version
    method conflicts : Pef.Packages_types.vpkglist
    method depends : Pef.Packages_types.vpkgformula
    method provides : Pef.Packages_types.vpkglist
    method recommends : Pef.Packages_types.vpkgformula
    method installed : Pef.Packages_types.installed
    method extras : (string * string) list

		(** {4 Debian specific methods } *)
    method architecture : Pef.Packages_types.architecture
    method breaks : Pef.Packages_types.vpkglist
    method enhances : Pef.Packages_types.vpkgformula
    method essential : bool
    method extra_source_only : bool
    method extras : (string * string) list
    method multiarch : Pef.Packages_types.multiarch
    method pre_depends : Pef.Packages_types.vpkgformula
    method priority : string
    method replaces : Pef.Packages_types.vpkglist
    method source : Pef.Packages_types.name * Pef.Packages_types.version option
    method suggests : Pef.Packages_types.vpkgformula

    (** {4 low level val } *)
    val name : (string * Pef.Packages_types.name)
    val version : (string * Pef.Packages_types.version)
    val conflicts : (string * Pef.Packages_types.vpkglist)
    val depends : (string * Pef.Packages_types.vpkgformula)
    val provides : (string * Pef.Packages_types.vpkglist)
    val recommends : (string * Pef.Packages_types.vpkgformula)
    val installed : (string * Pef.Packages_types.installed)

		(** {4 Debian specific val } *)
    val architecture : string * Pef.Packages_types.architecture
    val breaks : string * Pef.Packages_types.vpkglist
    val enhances : string * Pef.Packages_types.vpkgformula
    val essential : string * bool
    val extra_source_only : string * bool
    val multiarch : string * Pef.Packages_types.multiarch
    val pre_depends : string * Pef.Packages_types.vpkgformula
    val priority : string * string
    val replaces : string * Pef.Packages_types.vpkglist
    val source : string * (Pef.Packages_types.name * Pef.Packages_types.version option)
    val suggests : string * Pef.Packages_types.vpkgformula

    (** {4 get/set specific fields of the object} *)
    method get_extra : string -> string
    method add_extra : string -> string -> 'a
    method set_extras : (string * string) list -> 'a
    method set_installed : Pef.Packages_types.installed -> 'a

    (** {4 Debian specific methods } *)
    method set_essential : bool -> 'a
    method set_multiarch : Pef.Packages_types.multiarch -> 'a

    (* Print the object as a 822 stanza to the given channel *)
    method pp : out_channel -> unit

    method pp : out_channel -> unit
  end

val parse_package_stanza :
  (Format822.stanza -> bool) option ->
  Pef.Packages_types.architecture list ->
  (string * Pef.Packages.parse_extras_f option) list ->
  Format822.stanza -> package option

val parse_packages_in :
  ?filter:(Format822.stanza -> bool) ->
  ?archs:Pef.Packages_types.architecture list ->
  ?extras:(string * Pef.Packages.parse_extras_f option) list ->
  string -> IO.input -> package list

val merge : package list -> package list -> package list

val is_installed : package -> bool
val is_on_hold : package -> bool
val default_extras : (string * 'a option) list

val input_raw :
  ?filter:(Format822.stanza -> bool) ->
  ?archs:Pef.Packages_types.architecture list ->
  ?extras:(string * Pef.Packages.parse_extras_f option) list ->
  string list -> package list

val input_raw_in :
  ?filter:(Format822.stanza -> bool) ->
  ?archs:Pef.Packages_types.architecture list ->
  ?extras:(string * Pef.Packages.parse_extras_f option) list ->
  IO.input -> package list
