

open ExtLib
open Common
module Boilerplate = BoilerplateNoRpm

let debug fmt = Util.make_debug __FILE__ fmt
let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt

module Options = struct
  open OptParse
  let description = "Convert a cudf to debian (Packages,status,Request)"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let architecture = StdOpt.str_option ~default:"amd64" ()

  open OptParser
  add options ~short_name:'a' ~long_name:"arch" ~help:"Set architecture" architecture;
end

let pp_pkgname fmt name = Format.fprintf fmt "%s" name
let pp_version fmt ver = Format.fprintf fmt "%d" ver

let string_of_relop = function
  |`Eq -> "="
  |`Neq -> "!="
  |`Geq -> ">="
  |`Gt -> ">>"
  |`Leq -> "<="
  |`Lt -> "<<"

let pp_vpkg fmt (c : Cudf_types.vpkg) = match c with
  |(name, None) -> pp_pkgname fmt name
  |(name, Some (relop, v)) ->
      Format.fprintf fmt "%a (%s %a)" 
      pp_pkgname name (string_of_relop relop) pp_version v

let pp_list fmt ~pp_item ~sep l =
  let rec aux fmt = function
    | [] -> assert false
    | [last] -> (* last item, no trailing sep *)
        Format.fprintf fmt "@,%a" pp_item last
    | vpkg :: tl -> (* at least one package in tl *)
        Format.fprintf fmt "@,%a%s" pp_item vpkg sep ;
        aux fmt tl
  in
  match l with
  | [] -> ()
  | [sole] -> pp_item fmt sole
  | _ -> Format.fprintf fmt "@[<hv>%a@]" aux l

let pp_vpkglist fmt = pp_list fmt ~pp_item:pp_vpkg ~sep:" , "

(** ASSUMPTION: formula is in CNF *)
let rec pp_vpkgformula fmt = function
  | [] -> assert false
  | [ [] ] -> assert false
  | fmla ->
      let pp_or fmt = pp_list fmt ~pp_item:pp_vpkg ~sep:" | " in
      let pp_and fmt = pp_list fmt ~pp_item:pp_or ~sep:" , " in
      pp_and fmt fmla

let pp_property fmt (n, s) = Format.fprintf fmt "%s: %s@." n s

let buf = Buffer.create 1024
let buf_formatter =
  let fmt = Format.formatter_of_buffer buf in
    Format.pp_set_margin fmt max_int;
    fmt

let string_of pp arg =
  Buffer.clear buf;
  ignore(pp buf_formatter arg);
  Format.pp_print_flush buf_formatter ();
  Buffer.contents buf

let string_of_vpkg = string_of pp_vpkg
let string_of_vpkglist = string_of pp_vpkglist
let string_of_vpkgformula = string_of pp_vpkgformula

let string_of_value (v : Cudf_types.typed_value ) = match v with
  | (`Int i | `Posint i | `Nat i) -> Cudf_types_pp.string_of_int i
  | `Bool b -> Cudf_types_pp.string_of_bool b
  | (`String s | `Pkgname s | `Ident s | `Enum (_, s)) -> s
  | `Vpkg p -> string_of_vpkg p
  | `Vpkglist l -> string_of_vpkglist l
  | `Vpkgformula f -> string_of_vpkgformula f
  | _ -> assert false

let pp_package fmt pkg =
  let pp = pp_property fmt in
  pp ("Package", Cudf_types_pp.string_of_pkgname pkg.package);
  pp ("Version", Cudf_types_pp.string_of_version pkg.version);
  pp ("Architecture", OptParse.Opt.get Options.architecture);
  if pkg.depends <> Cudf.default_package.depends then
    pp ("Depends", string_of_vpkgformula pkg.depends);
  if pkg.conflicts <> Cudf.default_package.conflicts then
    pp ("Conflicts", string_of_vpkglist pkg.conflicts);
  if pkg.provides <> Cudf.default_package.provides then
    pp ("Provides", string_of_vpkglist (pkg.provides :> Cudf_types.vpkg list));
  if pkg.installed <> Cudf.default_package.installed then
    pp ("Status", "install ok installed");
  if pkg.keep = `Keep_package then
    pp ("Essential", "yes");
  let p = Cudf_types_pp.string_of_pkgname pkg.package in
  let v = Cudf_types_pp.string_of_version pkg.version in
  pp ("Filename", "/var/fake"^p^v);
  List.iter (fun (k, v) -> pp (k, string_of_value v)) pkg.pkg_extra

let pp_packages fmt =
  List.iter (fun pkg -> Format.fprintf fmt "%a@." pp_package pkg)
;;

let pp_request fmt req =
  let inst = 
    List.map (function
      |(name,None) -> (name,None)
      |(name,Some(`Eq,v)) -> (name,Some(`Eq,v))
      |_ -> assert false
    ) (req.install)
  in
  let rem = 
    List.map (function 
      |(name,None) -> (name^"-",None)
      |(name,Some(`Eq,v)) -> (name^"-",Some(`Eq,v))
      |_ -> assert false
    ) req.remove
  in
  let all = inst @ rem in
  let pp_vpkg fmt (c : Cudf_types.vpkg) = match c with
    |(name, None) -> pp_pkgname fmt name
    |(name, Some (relop, v)) ->
        Format.fprintf fmt "%a%s%a"
        pp_pkgname name (string_of_relop relop) pp_version v
  in
  let pp_vpkglist fmt = pp_list fmt ~pp_item:pp_vpkg ~sep:" " in
  let string_of_vpkglist = string_of pp_vpkglist in
  Format.fprintf fmt "%s@." (string_of_vpkglist all)
;;

let convert universe req =
  let vr_RE = Pcre.regexp "(.*)--virtual" in
  let f1 (pkgname,constr) =
    if Pcre.pmatch ~rex:vr_RE pkgname then begin 
      let s = Pcre.exec ~rex: vr_RE pkgname in
      (Pcre.get_substring s 1, constr) 
    end
    else (pkgname,constr)
  in
  let f2 name (pkgname,constr) =
    not(Pcre.pmatch ~rex:vr_RE pkgname) && 
    not (name = pkgname && constr = None)
  in
  let (status,pkglist)  = 
    Cudf.fold_packages (fun (status,pkglist) p ->
      let p' = 
        {p with
        provides = List.map f1 p.provides;
        conflicts = List.filter (f2 p.package) p.conflicts;
        depends = 
          List.filter_map (fun l' ->
            match List.filter (f2 p.package) l' with |[] -> None |l -> Some l
          ) p.depends;
        pkg_extra =
          List.filter_map (function 
            |("recommends",`Vpkgformula l) ->
                begin match (List.map (List.filter (f2 p.package)) l) with
                |[] -> None
                |l -> Some ("Recommends", `Vpkgformula l) end
            |("replaces",`Vpkglist l) ->
                begin match (List.filter (f2 p.package) l) with
                |[] -> None
                |l -> Some ("Replaces", `Vpkglist l) end
            |(s,v) -> None
          ) p.pkg_extra }
      in
      if p.installed then (p'::status,pkglist) else (status,p'::pkglist)
    ) ([],[]) universe
  in
  let reqlist = ref [] in
  let newreq = 
    { Cudf.default_request with
      Cudf.install = 
        List.filter_map (function 
          |(name,None) -> Some(name,None)
          |(name,Some(`Eq,ver)) -> Some(name,Some(`Eq,ver))
          |(name,constr) -> begin
            let l = 
                List.map (fun p -> 
                  (p.Cudf.package,Some(`Eq,p.Cudf.version))
                ) (Cudf.lookup_packages ~filter:constr universe name) 
            in
            let p = {
              Cudf.default_package with 
              Cudf.package = "dummy_"^name ; 
              Cudf.version = 1;
              Cudf.depends = [l]} 
            in
            reqlist := p::!reqlist ; 
            Some("dummy_"^name,None)
          end
        ) (req.install @ req.upgrade);
      Cudf.remove =
        List.flatten (
            List.map (function 
            |(name,None) -> [(name,None)]
            |(name,Some(`Eq,ver)) -> [(name,Some(`Eq,ver))]
            |(name,constr) ->
              List.map (fun p -> (p.Cudf.package,Some(`Eq,p.Cudf.version))
              ) (Cudf.lookup_packages ~filter:constr universe name)
          ) (req.remove)
        )
    }
  in
  (status,!reqlist @ pkglist,newreq)
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);

  let (status,pkglist,request) =
    match posargs with
    |[u] ->
        begin
          info "Converting file %s" u ;
          match Boilerplate.load_cudf u with
          |(_,univ,None) -> begin
              Printf.eprintf "This is Cudf universe, not a Cudf document. Request missing\n" ; 
              exit 1
            end
          |(_,univ,Some(req)) -> convert univ req
        end
    |_ -> (Printf.eprintf "You must specify one cudf document\n" ; exit 1)
  in

  let status_oc = open_out "status" in
  let packages_oc = open_out "Packages" in
  let request_oc = open_out "Request" in
  let status_ofr = Format.formatter_of_out_channel status_oc in
  let packages_ofr = Format.formatter_of_out_channel packages_oc in
  let request_ofr = Format.formatter_of_out_channel request_oc in

  pp_request request_ofr request;
  close_out request_oc;

  pp_packages packages_ofr pkglist;
  close_out packages_oc ;

  pp_packages status_ofr status;
  close_out status_oc
;;

main ();;
