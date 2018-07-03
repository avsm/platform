(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

module Pcre = Re_pcre
(* Remember the original hashtable module from Ocaml standard library,
    whose name will be overriden by opening Extlib. *)
module OCAMLHashtbl = Hashtbl

open ExtLib

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let equal = Cudf.(=%)
let compare = Cudf.(<%)

let sort ?(asc=false) l =
  let cmp = if asc then (fun a b -> compare b a) else compare in
  List.sort ~cmp l
let hash p = Hashtbl.hash (p.Cudf.package,p.Cudf.version)

module Cudf_hashtbl =
  OCAMLHashtbl.Make(struct
    type t = Cudf.package
    let equal = equal
    let hash = hash
  end)

module Cudf_set =
  Set.Make(struct
    type t = Cudf.package
    let compare = compare
  end)

let to_set l = List.fold_right Cudf_set.add l Cudf_set.empty

(** Encode - Decode *)

(* Specialized hashtable for encoding strings efficiently. *)
module EncodingHashtable =
  OCAMLHashtbl.Make(struct
    type t = string
    let equal = (=)
    let hash = fun s -> Char.code (s.[0])
  end)

(* Specialized hashtable for decoding strings efficiently. *)
module DecodingHashtable =
  OCAMLHashtbl.Make(struct
    type t = string
    let equal = (=)
    let hash = (fun s -> (Char.code s.[1]) * 1000 + (Char.code s.[2]) )
  end)

(*  "hex_char char" returns the ASCII code of the given character
    in the hexadecimal form, prefixed with the '%' sign.
    e.g. hex_char '+' = "%2b" *)
let hex_char char = Printf.sprintf "%%%02x" (Char.code char);;

(* "init_hashtables" initializes the two given hashtables to contain:

    - enc_ht: Precomputed results of applying the function "hex_char"
    to all possible ASCII chars.
    e.g. EncodingHashtable.find enc_ht "+" = "%2b"

    - dec_ht: An inversion of enc_ht.
    e.g. DecodingHashtable.find dec_ht "%2b" = "+" 
*)
let init_hashtables enc_ht dec_ht =
  let n = ref 255 in
  while !n >= 0 do
    let schr = String.make 1 (Char.chr !n) in
    let hchr = Printf.sprintf "%%%02x" !n in
    EncodingHashtable.add enc_ht schr hchr;
    DecodingHashtable.add dec_ht hchr schr;
    decr n;
  done

(* Create and initialize twin hashtables,
   one for encoding and one for decoding. *)
let enc_ht = EncodingHashtable.create 256;;
let dec_ht = DecodingHashtable.create 256;;
init_hashtables enc_ht dec_ht;;

(* encode *)
let encode_single s = EncodingHashtable.find enc_ht s;;
let not_allowed_regexp = Pcre.regexp "[^a-zA-Z0-9@/+().-]";;

let encode s =
  Pcre.substitute ~rex:not_allowed_regexp ~subst:encode_single s

(* decode *)
let decode_single s = DecodingHashtable.find dec_ht s;;
let encoded_char_regexp = Pcre.regexp "%[0-9a-f][0-9a-f]";;

let decode s =
  Pcre.substitute ~rex:encoded_char_regexp ~subst:decode_single s

(** Pretty Printing *)

let string_of pp arg =
  ignore(pp Format.str_formatter arg);
  Format.flush_str_formatter ()

let pp_version fmt pkg =
  try Format.fprintf fmt "%s" (decode (Cudf.lookup_package_property pkg "number"))
  with Not_found -> Format.fprintf fmt "%d" pkg.Cudf.version

let pp_package fmt pkg =
  Format.fprintf fmt "%s (= %a)" (decode pkg.Cudf.package) pp_version pkg

let string_of_version = string_of pp_version
let string_of_package = string_of pp_package

type pp = Cudf.package -> string * string option * string * (string * (string * bool)) list

(** [default_pp] default package printer. If the version of the package is
  * a negative number, the version version if printed as "nan" *)
let default_pp pkg =
  let v = if pkg.Cudf.version > 0 then string_of_version pkg else "nan" in
  (pkg.Cudf.package,None,v,[])

let pp from_cudf ?(fields=[]) ?(decode=decode) pkg =
  let (p,a,v) = from_cudf (pkg.Cudf.package,pkg.Cudf.version) in
  let default_fields = ["architecture";"source";"sourcenumber";"essential";"type"] in
  let f b l acc =
    List.fold_left (fun acc k ->
      try (k,(decode(Cudf.lookup_package_property pkg k),b))::acc
      with Not_found -> acc
    ) acc l
  in
  let l = (f false fields (f true default_fields [])) in
  (p,a,v,l)

let pp_vpkg pp fmt vpkg =
  let string_of_relop = function
      `Eq -> "="
    | `Neq -> "!="
    | `Geq -> ">="
    | `Gt -> ">"
    | `Leq -> "<="
    | `Lt -> "<"
  in
  let dummy p v = {Cudf.default_package with Cudf.package = p ; version = v} in
  match vpkg with
  |(p,None) ->
      begin match pp (dummy p Util.max32int) with
      |(p,None,_,_) -> Format.fprintf fmt "%s" p
      |(p,Some a,_,_) -> Format.fprintf fmt "%s:%s" p a end
  |(p,Some(c,v)) ->
      match pp (dummy p v) with
      |(p,None,("nan" | ""),_) -> Format.fprintf fmt "%s" p
      |(p,None,v,_) -> Format.fprintf fmt "%s (%s %s)" p (string_of_relop c) v
      |(p,Some a,("nan" | ""),_) -> Format.fprintf fmt "%s:%s" p a
      |(p,Some a,v,_) -> Format.fprintf fmt "%s:%s (%s %s)" p a (string_of_relop c) v

let pp_vpkglist pp fmt =
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
    | _ -> Format.fprintf fmt "@[<h>%a@]" aux l
  in
  pp_list fmt ~pp_item:(pp_vpkg pp) ~sep:" | "

module StringSet = Set.Make(String)

let add_to_package_list h n p =
  try let l = Hashtbl.find h n in l := p :: !l
  with Not_found -> Hashtbl.add h n (ref [p])

let get_package_list h n = try !(Hashtbl.find h n) with Not_found -> []

let pkgnames universe =
  Cudf.fold_packages (fun names pkg ->
    StringSet.add pkg.Cudf.package names
  ) StringSet.empty universe

let pkgnames_ universe =
  let h = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.iter_packages (fun pkg ->
    add_to_package_list h pkg.Cudf.package pkg
  ) universe

let add_properties preamble l =
  List.fold_left (fun pre prop ->
    {pre with Cudf.property = prop :: pre.Cudf.property }
  ) preamble l

let get_property prop pkg =
   try Cudf.lookup_package_property pkg prop
   with Not_found -> begin
     warning "%s missing" prop;
     raise Not_found
   end

let is_essential pkg =
  try Cudf.lookup_package_property pkg "essential" = "true"
  with Not_found -> false

let realversionmap pkglist =
  let h = Hashtbl.create (5 * (List.length pkglist)) in
  List.iter (fun pkg ->
    Hashtbl.add h (pkg.Cudf.package,string_of_version pkg) pkg
  ) pkglist ;
  h

let pkgtoint universe p =
  try Cudf.uid_by_package universe p
  with Not_found-> begin
    warning 
    "package %s is not associate with an integer in the given universe"
    (string_of_package p);
    raise Not_found
  end

let inttopkg = Cudf.package_by_uid

let normalize_set (l : int list) = 
  List.rev (List.fold_left (fun results x ->
    if List.mem x results then results
    else x::results) [] l
  )

(* vpkg -> pkg list *)
let who_provides univ (pkgname,constr) = 
  let pkgl = Cudf.lookup_packages ~filter:constr univ pkgname in
  let prol = Cudf.who_provides ~installed:false univ (pkgname,constr) in
  let filter = function 
    |(p,None) -> Some p
    |(p,Some v) when Cudf.version_matches v constr -> Some p
    |_ -> None
  in
  pkgl @ (List.filter_map filter prol)

(* vpkg -> id list *)
let resolve_vpkg_int univ vpkg =
  List.map (Cudf.uid_by_package univ) (who_provides univ vpkg)

(* vpkg list -> id list *)
let resolve_vpkgs_int univ vpkgs =
  normalize_set (List.flatten (List.map (resolve_vpkg_int univ) vpkgs))

(* vpkg list -> pkg list *)
let resolve_deps univ vpkgs =
  List.map (Cudf.package_by_uid univ) (resolve_vpkgs_int univ vpkgs)

(* pkg -> pkg list list *)
let who_depends univ pkg = 
  List.map (resolve_deps univ) pkg.Cudf.depends

type ctable = (int, int list ref) ExtLib.Hashtbl.t

let who_conflicts conflicts_packages univ pkg = 
  if (Hashtbl.length conflicts_packages) = 0 then
    debug "Either there are no conflicting packages in the universe or you
CudfAdd.init_conflicts was not invoked before calling CudfAdd.who_conflicts";
  let i = Cudf.uid_by_package univ pkg in
  List.map (Cudf.package_by_uid univ) (get_package_list conflicts_packages i)

let init_conflicts univ =
  let conflict_pairs = Hashtbl.create 1023 in
  let conflicts_packages = Hashtbl.create 1023 in
  Cudf.iteri_packages (fun i p ->
    List.iter (fun n ->
      let pair = (min n i, max n i) in
      if n <> i && not (Hashtbl.mem conflict_pairs pair) then begin
        Hashtbl.add conflict_pairs pair ();
        add_to_package_list conflicts_packages i n;
        add_to_package_list conflicts_packages n i
      end
    )
    (resolve_vpkgs_int univ p.Cudf.conflicts)
  ) univ;
  conflicts_packages

(* here we assume that the id given by cudf is a sequential and dense *)
let compute_pool universe = 
  let size = Cudf.universe_size universe in
  let conflicts = init_conflicts universe in
  let c = Array.init size (fun i -> get_package_list conflicts i) in
  let d =
    Array.init size (fun i ->
      let p = Cudf.package_by_uid universe i in
      List.map (resolve_vpkgs_int universe) p.Cudf.depends
    )
  in
  (d,c)

let cudf_op = function
  |("<<" | "<") -> `Lt
  |(">>" | ">") -> `Gt
  |"<=" -> `Leq
  |">=" -> `Geq
  |"=" -> `Eq
  |"!=" -> `Neq
  |c -> fatal "Unknown operator: %s" c

let cudf_constr = function
  |None -> None
  |Some("ALL",_) -> None
  |Some(c,v) -> Some(cudf_op c,v)

let latest ?(n=1) pkglist =
  let h = Hashtbl.create (List.length pkglist) in
  List.iter (fun p ->
    add_to_package_list h p.Cudf.package p
  ) pkglist;
  Hashtbl.fold (fun _ { contents = l } acc ->
    if List.length l <= n then l@acc
    else (fst(List.split_nth n (sort ~asc:true l)))@acc
  ) h []

let cone universe pkgs =
  let l = ref [] in
  let queue = Queue.create () in
  let visited = Hashtbl.create (2 * (List.length pkgs)) in
  List.iter (fun pkg -> Queue.add (Cudf.uid_by_package universe pkg) queue) pkgs;
  while (Queue.length queue > 0) do
    let id = Queue.take queue in
    let pkg = Cudf.package_by_uid universe id in
    if not(Hashtbl.mem visited id) then begin
      l := pkg :: !l;
      Hashtbl.add visited id ();
      List.iter (fun vpkgs ->
        match resolve_vpkgs_int universe vpkgs with
        |[i] when not(Hashtbl.mem visited i) -> Queue.add i queue;
        |dsj ->
          List.iter (fun i ->
            if not(Hashtbl.mem visited i) then Queue.add i queue;
          ) dsj
      ) pkg.Cudf.depends
    end
  done;
  !l
