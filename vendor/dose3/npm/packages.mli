
type request = {
  install : Pef.Packages_types.vpkg list;
  remove : Pef.Packages_types.vpkg list;
  upgrade : Pef.Packages_types.vpkg list;
  preferences : string;
}

val default_request : request

val parse_request_stanza : Common.Format822.stanza -> request

val parse_dependlist : Common.Format822.field -> Pef.Packages_types.vpkgformula
val parse_depends : Common.Format822.field -> Pef.Packages_types.vpkgformula
val parse_depend : Common.Format822.field -> Pef.Packages_types.vpkg list list

class package :
  ?name:string * Pef.Packages_types.name option ->
  ?version:string * Pef.Packages_types.version option ->
  ?depends:string * Pef.Packages_types.vpkgformula option ->
  ?conflicts:string * Pef.Packages_types.vpkglist option ->
  ?provides:string * Pef.Packages_types.vpkglist option ->
  ?extras:(string * Pef.Packages.parse_extras_f option) list *
          (string * string) list option ->
  Common.Format822.stanza ->
  object ('a)
    method name : Pef.Packages_types.name
    method version : Pef.Packages_types.version
    method conflicts : Pef.Packages_types.vpkglist
    method depends : Pef.Packages_types.vpkgformula
    method provides : Pef.Packages_types.vpkglist
    method installed : Pef.Packages_types.installed
    method recommends : Pef.Packages_types.vpkgformula
    method extras : (string * string) list

    method get_extra : string -> string
    method set_extras : (string * string) list -> 'a
    method set_installed : Pef.Packages_types.installed -> 'a
    method add_extra : string -> string -> 'a

    method pp : out_channel -> unit
  end

val parse_package_stanza : 
  ?extras:(string * Pef.Packages.parse_extras_f option) list ->
  Common.Format822.stanza -> package option

val packages_parser : ?request:bool -> request * package list ->
  Common.Format822.f822_parser -> request * package list

val input_raw_in : IO.input -> request * package list
val input_raw : string -> request * package list

