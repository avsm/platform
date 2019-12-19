type name = string
type version = string
type architecture = string
type architectures = architecture list
type buildprofile = string
type vpkgname = (string * architecture option)
type multiarch = [ `Foreign | `Allowed | `No | `Same ]
type source = (name * version option)
type relop = string
type constr = (relop * version)
type installed = bool

type vpkg = (vpkgname * constr option)
type vpkglist = vpkg list
type vpkgformula = vpkg list list

type builddep = (vpkg * (bool * architecture) list * (bool * buildprofile) list list)
type builddepslist = builddep list
type builddepsformula = builddep list list

type action = I | R
type suite = string
type vpkgreq = (action option * vpkg * suite option)
