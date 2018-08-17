(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

let loadpath     = ref []

let last_location = ref Location.none

let log_section = "locate"

let log title msg = Logger.log log_section title msg
let logf title fmt = Logger.logf log_section title fmt
let logfmt title fmt = Logger.logfmt log_section title fmt

let erase_loadpath ~cwd ~new_path k =
  let str_path_list =
    List.map new_path ~f:(function
      | "" ->
        (* That's the cwd at the time of the generation of the cmt, I'm
            guessing/hoping it will be the directory where we found it *)
        log "erase_loadpath" cwd;
        cwd
      | x ->
        log "erase_loadpath" x;
        x
    )
  in
  let_ref loadpath str_path_list k

let restore_loadpath ~config k =
  log "restore_loadpath" "Restored load path";
  let_ref loadpath (Mconfig.cmt_path config) k

module Fallback = struct
  let fallback = ref None

  let get () = !fallback

  let set loc =
    logfmt "Fallback.set" (fun fmt -> Location.print_loc fmt loc);
    fallback := Some loc

  let reset () = fallback := None

  let is_set () = !fallback <> None
end

module File : sig
  type t = private
    | ML   of string
    | MLL  of string
    | MLI  of string
    | CMT  of string
    | CMTI of string

  val ml : string -> t
  val mli : string -> t
  val cmt : string -> t
  val cmti : string -> t

  val of_filename : string -> t option

  val alternate : t -> t

  val name : t -> string

  val with_ext : ?src_suffix_pair:(string * string) -> t -> string

  val explain_not_found :
    ?doc_from:string -> string -> t -> [> `File_not_found of string ]
end = struct
  type t =
    | ML   of string
    | MLL  of string
    | MLI  of string
    | CMT  of string
    | CMTI of string

  let file_path_to_mod_name f =
    Misc.unitname (Filename.basename f)

  let ml   s = ML   (file_path_to_mod_name s)
  let mll  s = MLL  (file_path_to_mod_name s)
  let mli  s = MLI  (file_path_to_mod_name s)
  let cmt  s = CMT  (file_path_to_mod_name s)
  let cmti s = CMTI (file_path_to_mod_name s)

  let of_filename fn =
    match Misc.rev_string_split ~on:'.' fn with
    | []
    | [ _ ] -> None
    | ext :: _ ->
      let ext = String.lowercase ext in
      Some (
        match ext with
        | "cmti" -> cmti fn
        | "cmt"  -> cmt fn
        | "mll"  -> mll fn
        | _ -> if Filename.check_suffix ext "i" then mli fn else ml fn
      )

  let alternate = function
    | ML  s
    | MLL s -> MLI s
    | MLI s -> ML s
    | CMT s  -> CMTI s
    | CMTI s -> CMT s

  let name = function
    | ML name
    | MLL name
    | MLI name
    | CMT name
    | CMTI name -> name

  let ext src_suffix_pair = function
    | ML _  -> fst src_suffix_pair
    | MLI _  -> snd src_suffix_pair
    | MLL _ -> ".mll"
    | CMT _ -> ".cmt"
    | CMTI _ -> ".cmti"

  let with_ext ?(src_suffix_pair=(".ml",".mli")) t =
    name t ^ ext src_suffix_pair t

  let explain_not_found ?(doc_from="") str_ident path =
    let msg =
      match path with
      | ML file ->
        sprintf "'%s' seems to originate from '%s' whose ML file could not be \
                 found" str_ident file
      | MLL file ->
        sprintf "'%s' seems to originate from '%s' whose MLL file could not be \
                 found" str_ident file
      | MLI file ->
        sprintf "'%s' seems to originate from '%s' whose MLI file could not be \
                 found" str_ident file
      | CMT file ->
        sprintf "Needed cmt file of module '%s' to locate '%s' but it is not \
                 present" file str_ident
      | CMTI file when file <> doc_from ->
        sprintf "Needed cmti file of module '%s' to locate '%s' but it is not \
                 present" file str_ident
      | CMTI _ ->
        sprintf "The documentation for '%s' originates in the current file, \
                 but no cmt is available" str_ident
    in
    `File_not_found msg
end

module Preferences : sig
  val set : [ `ML | `MLI ] -> unit

  val src : string -> File.t
  val build : string -> File.t

  val is_preferred : string -> bool
end = struct
  let prioritize_impl = ref true

  let set choice =
    prioritize_impl :=
      match choice with
      | `ML -> true
      | _ -> false

  let src   file = if !prioritize_impl then File.ml  file else File.mli  file
  let build file = if !prioritize_impl then File.cmt file else File.cmti file

  let is_preferred fn =
    match File.of_filename fn with
    | Some ML _ -> !prioritize_impl
    | Some MLI _ -> not !prioritize_impl
    | _ -> false
end

module File_switching : sig
  val reset : unit -> unit

  val move_to : digest:Digest.t -> string -> unit

  val where_am_i : unit -> string option

  val source_digest : unit -> Digest.t option
end = struct
  type t = {
    last_file_visited : string;
    digest : Digest.t;
  }

  let last_file_visited t = t.last_file_visited
  let digest t = t.digest

  let state = ref None

  let reset () = state := None

  let move_to ~digest file =
    logf "File_switching.move_to" "%s" file;
    state := Some { last_file_visited = file ; digest }

  let where_am_i () = Option.map !state ~f:last_file_visited

  let source_digest () = Option.map !state ~f:digest
end


module Utils = struct
  let is_builtin_path = function
    | Path.Pident id ->
      let f (_, i) = Ident.same i id in
      List.exists Predef.builtin_idents ~f
      || List.exists Predef.builtin_values ~f
    | _ -> false

  let is_ghost_loc { Location. loc_ghost; _ } = loc_ghost

  (* Reuse the code of [Misc.find_in_path_uncap] but returns all the files
     matching, instead of the first one.
     This is only used when looking for ml files, not cmts. Indeed for cmts we
     know that the load path will only ever contain files with uniq names (in
     the presence of packed modules we refine the loadpath as we go); this in
     not the case for the "source path" however.
     We therefore get all matching files and use an heuristic at the call site
     to choose the appropriate file. *)
  let find_all_in_path_uncap ?src_suffix_pair ~with_fallback path file =
    let name = File.with_ext ?src_suffix_pair file in
    let uname = String.uncapitalize name in
    let fallback, ufallback =
      let alt = File.alternate file in
      let fallback = File.with_ext ?src_suffix_pair alt in
      fallback, String.uncapitalize fallback
    in
    let try_file dirname basename acc =
      if Misc.exact_file_exists ~dirname ~basename
      then Misc.canonicalize_filename (Filename.concat dirname basename) :: acc
      else acc
    in
    let try_dir acc dirname =
      let acc = try_file dirname uname acc in
      let acc = try_file dirname name acc in
      let acc =
        if with_fallback then
          let acc = try_file dirname ufallback acc in
          let acc = try_file dirname fallback acc in
          acc
        else
          acc
      in
      acc
    in
    List.fold_left ~f:try_dir ~init:[] path

  let find_all_matches ~config ?(with_fallback=false) file =
    let files =
      List.concat_map ~f:(fun synonym_pair ->
        find_all_in_path_uncap ~src_suffix_pair:synonym_pair ~with_fallback
          (Mconfig.source_path config) file
      ) Mconfig.(config.merlin.suffixes)
    in
    List.uniq files ~cmp:String.compare

  let find_file_with_path ~config ?(with_fallback=false) file path =
    if File.name file = Misc.unitname Mconfig.(config.query.filename) then
      Some Mconfig.(config.query.filename)
    else
      let attempt_search src_suffix_pair =
        let fallback =
          if with_fallback then
            Some (File.with_ext ~src_suffix_pair (File.alternate file))
          else
            None
        in
        let fname = File.with_ext ~src_suffix_pair file in
        try Some (Misc.find_in_path_uncap ?fallback path fname)
        with Not_found -> None
      in
      try
        Some (List.find_map Mconfig.(config.merlin.suffixes) ~f:attempt_search)
      with Not_found ->
        None

  let find_file ~config ?with_fallback (file : File.t) =
    find_file_with_path ~config ?with_fallback file @@
        match file with
        | ML  _ | MLI _  | MLL _ -> Mconfig.source_path config
        | CMT _ | CMTI _         -> !loadpath
end

module Context = struct
  type t =
    | Constructor of Types.constructor_description
      (* We attach the constructor description here so in the case of
        disambiguated constructors we actually directly look for the type
        path (cf. #486, #794). *)
    | Expr
    | Label of Types.label_description (* Similar to constructors. *)
    | Module_path
    | Module_type
    | Patt
    | Type
    | Unknown

  let to_string = function
    | Constructor cd -> Printf.sprintf "constructor %s" cd.cstr_name
    | Expr -> "expression"
    | Label lbl -> Printf.sprintf "record field %s" lbl.lbl_name
    | Module_path -> "module path"
    | Module_type -> "module type"
    | Patt -> "pattern"
    | Type -> "type"
    | Unknown -> "unknown"
end

exception Cmt_cache_store of Typedtrie.t

let trie_of_cmt root =
  let open Cmt_format in
  let cached = Cmt_cache.read root in
  logf "browse_cmts" "inspecting %s" root ;
  begin match cached.Cmt_cache.location_trie with
  | Cmt_cache_store _ ->
    let digest =
      (* [None] only for packs, and we wouldn't have a trie if the cmt was for a
         pack. *)
      Option.get cached.cmt_infos.cmt_source_digest
    in
    File_switching.move_to ~digest root;
    log "browse_cmts" "trie already cached"
  | Not_found ->
    let trie_of_nodes nodes =
      let digest =
        (* [None] only for packs. *)
        Option.get cached.cmt_infos.cmt_source_digest
      in
      File_switching.move_to ~digest root;
      let trie =
        Typedtrie.of_browses (List.map ~f:Browse_tree.of_node nodes)
      in
      cached.location_trie <- Cmt_cache_store trie
    in
    Option.iter ~f:trie_of_nodes (
      match cached.Cmt_cache.cmt_infos.cmt_annots with
      | Packed (_, _)       -> None
      | Interface intf      -> Some [Browse_raw.Signature intf]
      | Implementation impl -> Some [Browse_raw.Structure impl]
      | Partial_interface parts
      | Partial_implementation parts ->
        log "browse_cmt" "working from partial cmt(i)";
        let env = cached.cmt_infos.cmt_initial_env in
        let nodes =
          Array.to_list parts
          |> List.map ~f:(Mbrowse.node_of_binary_part env)
        in
        Some nodes
    )
  | _ -> assert false
  end;
  cached.cmt_infos, cached.location_trie

type locate_result =
  | Found of Location.t * string option
  | File_not_found of File.t
  | Other_error (* FIXME *)

let rec locate ~config ~context path trie : locate_result =
  match Typedtrie.find ~remember_loc:Fallback.set ~context trie path with
  | Typedtrie.Found (loc, doc_opt) -> Found (loc, doc_opt)
  | Typedtrie.Resolves_to (new_path, state) ->
    begin match Namespaced_path.head_exn new_path with
    | Ident (_, `Mod) ->
      logf "locate" "resolves to %s" (Namespaced_path.to_unique_string new_path);
      from_path ~config ~context:(Typedtrie.Resume state) new_path
    | _ ->
      logf "locate" "new path (%s) is not a real path"
        (Namespaced_path.to_unique_string new_path);
      logfmt "locate (typedtrie dump)" (fun fmt -> Typedtrie.dump fmt trie);
      Other_error (* incorrect path *)
    end

and from_path ~config ~context path : locate_result =
  log "from_path" (Namespaced_path.to_unique_string path) ;
  match Namespaced_path.head_exn path with
  | Ident (fname, `Mod) ->
    let path = Namespaced_path.peal_head_exn path in
    let fname = Namespaced_path.Id.name fname in
    let file = Preferences.build fname in
    let browse_cmt cmt_file =
      let cmt_infos, trie = trie_of_cmt cmt_file in
      match trie, Namespaced_path.head path with
      | Not_found, None ->
        Other_error (* Trying to stop on a packed module... *)
      | Not_found, Some _ ->
        log "from_path" "Saw packed module => erasing loadpath" ;
        erase_loadpath ~cwd:(Filename.dirname cmt_file)
          ~new_path:cmt_infos.cmt_loadpath
          (fun () -> from_path ~context ~config path)
      | Cmt_cache_store _, None ->
        (* We found the module we were looking for, we can stop here. *)
        let pos_fname =
          match cmt_infos.cmt_sourcefile with
          | None   -> fname
          | Some f -> f
        in
        let pos = Lexing.make_pos ~pos_fname (1, 0) in
        let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=true } in
        (* TODO: retrieve "ocaml.text" floating attributes? *)
        Found (loc, None)
      | Cmt_cache_store trie, Some _ ->
        locate ~config ~context path trie
      | _, _ -> assert false
    in
    begin match Utils.find_file ~config ~with_fallback:true file with
    | Some cmt_file -> browse_cmt cmt_file
    | None ->
      (* The following is ugly, and deserves some explanations:
           As can be seen above, when encountering packed modules we override
           the loadpath by the one used to create the pack.
           This means that if the cmt files haven't been moved, we have access
           to the cmt file of every unit included in the pack.
           However, we might not have access to any other cmt (e.g. if others
           paths in the loadpath reference only cmis of packs).
           (Note that if we had access to other cmts, there might be conflicts,
           and the paths order would matter unless we have reliable digests...)
           Assuming we are in such a situation, if we do not find something in
           our "erased" loadpath, it could mean that we are looking for a
           persistent unit, and that's why we restore the initial loadpath. *)
      restore_loadpath ~config (fun () ->
        match Utils.find_file ~config ~with_fallback:true file with
        | Some cmt_file -> browse_cmt cmt_file
        | None ->
          logf "from_path" "failed to locate the cmt[i] of '%s'" fname;
          File_not_found file
      )
    end
  | _ ->
    Other_error (* type error, [from_path] should only be called on modules *)

let path_and_loc_of_cstr desc _ =
  let open Types in
  match desc.cstr_tag with
  | Cstr_extension (path, _) -> path, desc.cstr_loc
  | _ ->
    match desc.cstr_res.desc with
    | Tconstr (path, _, _) -> path, desc.cstr_loc
    | _ -> assert false

let path_and_loc_from_label desc env =
  let open Types in
  match desc.lbl_res.desc with
  | Tconstr (path, _, _) ->
    let typ_decl = Env.find_type path env in
    path, typ_decl.Types.type_loc
  | _ -> assert false

type find_source_result =
  | Found of string
  | Not_found of File.t
  | Multiple_matches of string list

let find_source ~config loc =
  let fname = loc.Location.loc_start.Lexing.pos_fname in
  let with_fallback = loc.Location.loc_ghost in
  let file =
    match File.of_filename fname with
    | Some file -> file
    | None ->
      (* no extension? we have to decide. *)
      Preferences.src fname
  in
  let filename = File.name file in
  let initial_path =
    match File_switching.where_am_i () with
    | None -> fname
    | Some s -> s
  in
  let dir = Filename.dirname initial_path in
  let dir =
    match Mconfig.(config.query.directory) with
    | "" -> dir
    | cwd -> Misc.canonicalize_filename ~cwd dir
  in
  match Utils.find_all_matches ~config ~with_fallback file with
  | [] ->
    logf "find_source" "failed to find %S in source path (fallback = %b)"
       filename with_fallback ;
    logf "find_source" "looking for %S in %S" (File.name file) dir ;
    begin match Utils.find_file_with_path ~config ~with_fallback file [dir] with
    | Some source -> Found source
    | None ->
      logf "find_source" "Trying to find %S in %S directly" fname dir;
      try Found (Misc.find_in_path [dir] fname)
      with _ -> Not_found file
    end
  | [ x ] -> Found x
  | files ->
    logf (sprintf "find_source(%s)" filename)
      "multiple matches in the source path : %s"
      (String.concat ~sep:" , " files);
    try
      match File_switching.source_digest () with
      | None ->
        logf "find_source"
          "... no source digest available to select the right one" ;
        raise Not_found
      | Some digest ->
        logf "find_source"
          "... trying to use source digest to find the right one" ;
        logf "find_source" "Source digest: %s" (Digest.to_hex digest) ;
        Found (
          List.find files ~f:(fun f ->
            let fdigest = Digest.file f in
            logf "find_source" "  %s (%s)" f (Digest.to_hex fdigest) ;
            fdigest = digest
          )
        )
    with Not_found ->
      logf "find_source" "... using heuristic to select the right one" ;
      logf "find_source" "we are looking for a file named %s in %s" fname dir ;
      let rev = String.reverse (Misc.canonicalize_filename ~cwd:dir fname) in
      let lst =
        List.map files ~f:(fun path ->
          let path' = String.reverse path in
          let priority = (String.common_prefix_len rev path') * 2 +
                          if Preferences.is_preferred path
                          then 1
                          else 0
          in
          priority, path
        )
      in
      let lst =
        (* TODO: remove duplicates in [source_path] instead of using
          [sort_uniq] here. *)
        List.sort_uniq ~cmp:(fun ((i:int),s) ((j:int),t) ->
          let tmp = compare j i in
          if tmp <> 0 then tmp else
          match compare s t with
          | 0 -> 0
          | n ->
            (* Check if we are referring to the same files.
                Especially useful on OSX case-insensitive FS.
                FIXME: May be able handle symlinks and non-existing files,
                CHECK *)
            match File_id.get s, File_id.get t with
            | s', t' when File_id.check s' t' ->
              0
            | _ -> n
        ) lst
      in
      match lst with
      | (i1, _) :: (i2, _) :: _ when i1 = i2 ->
        Multiple_matches files
      | (_, s) :: _ -> Found s
      | _ -> assert false

(* Well, that's just another hack.
   [find_source] doesn't like the "-o" option of the compiler. This hack handles
   Jane Street specific use case where "-o" is used to prefix a unit name by the
   name of the library which contains it. *)
let find_source ~config loc =
  match find_source ~config loc with
  | Found _ as result -> result
  | failure ->
    let fname = loc.Location.loc_start.Lexing.pos_fname in
    match
      let i = String.first_double_underscore_end fname in
      let pos = i + 1 in
      let fname = String.sub fname ~pos ~len:(String.length fname - pos) in
      let loc =
        let lstart = { loc.Location.loc_start with Lexing.pos_fname = fname } in
        { loc with Location.loc_start = lstart }
      in
      find_source ~config loc
    with
    | Found _ as result -> result
    | _ -> failure
    | exception _ -> failure

let recover _ =
  match Fallback.get () with
  | None -> assert false
  | Some loc -> `Found (loc, None)

module Env_lookup : sig

  val with_context
     : Context.t
    -> Longident.t
    -> Env.t
    -> (Path.t * Namespaced_path.t * Location.t) option

   val label
     : Longident.t
     -> Env.t
    -> (Path.t * Namespaced_path.t * Location.t) option

end = struct

  let namespaces : Context.t -> _ = function
    | Type          -> [ `Type ; `Mod ; `Modtype ; `Constr ; `Labels ; `Vals ]
    | Module_type   -> [ `Modtype ; `Mod ; `Type ; `Constr ; `Labels ; `Vals ]
    | Expr | Patt   -> [ `Vals ; `Mod ; `Modtype ; `Constr ; `Labels ; `Type ]
    | Unknown       -> [ `Vals ; `Type ; `Constr ; `Mod ; `Modtype ; `Labels ]
    | Label _       -> [ `Labels; `Mod ]
    | Constructor _ -> [ `Constr; `Mod ]
    | Module_path   -> [ `Mod ]

  exception Found of (Path.t * Namespaced_path.t * Location.t)

  let with_context (ctxt : Context.t) ident env =
    try
      List.iter (namespaces ctxt) ~f:(fun namespace ->
        try
          match namespace with
          | `Constr ->
            log "lookup" "lookup in constructor namespace" ;
            let cd =
              match ctxt with
              | Constructor cd -> cd
              | _ -> Env.lookup_constructor ident env
            in
            let path, loc = path_and_loc_of_cstr cd env in
            (* TODO: Use [`Constr] here instead of [`Type] *)
            raise (Found (path, Namespaced_path.of_path ~namespace:`Type path, loc))
          | `Mod ->
            log "lookup" "lookup in module namespace" ;
            let path = Env.lookup_module ~load:true ident env in
            let md = Env.find_module path env in
            raise (Found (path, Namespaced_path.of_path ~namespace:`Mod path, md.Types.md_loc))
          | `Modtype ->
            log "lookup" "lookup in module type namespace" ;
            let path, mtd = Env.lookup_modtype ident env in
            raise (Found (path, Namespaced_path.of_path ~namespace:`Modtype path, mtd.Types.mtd_loc))
          | `Type ->
            log "lookup" "lookup in type namespace" ;
            let path = Env.lookup_type ident env in
            let typ_decl = Env.find_type path env in
            raise (Found (path, Namespaced_path.of_path ~namespace:`Type path, typ_decl.Types.type_loc))
          | `Vals ->
            log "lookup" "lookup in value namespace" ;
            let path, val_desc = Env.lookup_value ident env in
            raise (Found (path, Namespaced_path.of_path ~namespace:`Vals path, val_desc.Types.val_loc))
          | `Labels ->
            log "lookup" "lookup in label namespace" ;
            let lbl =
              match ctxt with
              | Label lbl -> lbl
              | _ -> Env.lookup_label ident env
            in
            let path, loc = path_and_loc_from_label lbl env in
            (* TODO: Use [`Labels] here instead of [`Type] *)
            raise (Found (path, Namespaced_path.of_path ~namespace:`Type path, loc))
        with Not_found -> ()
      ) ;
      logf "lookup" "   ... not in the environment" ;
      None
    with Found x ->
      Some x

  let label ident env =
    try
      let label_desc = Env.lookup_label ident env in
      let path, loc = path_and_loc_from_label label_desc env in
      (* TODO: Use [`Labels] here *)
      Some (path, Namespaced_path.of_path ~namespace:`Type path, loc)
    with Not_found ->
      None
end

let locate ~config ~ml_or_mli ~path ~lazy_trie ~pos ~str_ident loc =
  File_switching.reset ();
  Fallback.reset ();
  Preferences.set ml_or_mli;
  logf "locate"
    "present in the environment, walking up the typedtree looking for '%s'"
    (Namespaced_path.to_unique_string path);
  try
    if not (Utils.is_ghost_loc loc) then Fallback.set loc;
    let lazy trie = lazy_trie in
    match locate ~config ~context:(Initial pos) path trie with
    | Found (loc, doc) -> `Found (loc, doc)
    | Other_error
    | File_not_found _ when Fallback.is_set () -> recover str_ident
    | Other_error -> `Not_found (str_ident, File_switching.where_am_i ())
    | File_not_found f -> File.explain_not_found str_ident f
  with
  | _ when Fallback.is_set () -> recover str_ident
  | Not_found -> `Not_found (str_ident, File_switching.where_am_i ())

(* Only used to retrieve documentation *)
let from_completion_entry ~config ~lazy_trie ~pos (namespace, path, loc) =
  let str_ident = Path.name path in
  let tagged_path = Namespaced_path.of_path ~namespace path in
  locate ~config ~ml_or_mli:`MLI ~path:tagged_path ~pos ~str_ident loc
    ~lazy_trie

let from_longident ~config ~env ~lazy_trie ~pos ctxt ml_or_mli lid =
  let ident, is_label = Longident.keep_suffix lid in
  let str_ident = String.concat ~sep:"." (Longident.flatten ident) in
  match
    if not is_label then
      Env_lookup.with_context ctxt ident env
    else
      Env_lookup.label ident env
  with
  | None -> `Not_in_env str_ident
  | Some (path, tagged_path, loc) ->
    if Utils.is_builtin_path path then
      `Builtin
    else
      locate ~config ~ml_or_mli ~path:tagged_path ~lazy_trie ~pos ~str_ident loc

(* Distinguish between "Mo[d]ule.Constructor" and "Module.Cons[t]ructor" *)
let cursor_on_constructor_name ~cursor:pos
      ~cstr_token:{ Asttypes.loc; txt = lid } cd =
  match lid with
  | Longident.Lident _ -> true
  | _ ->
    let end_offset = loc.loc_end.pos_cnum in
    let constr_pos =
      { loc.loc_end
        with pos_cnum = end_offset - String.length cd.Types.cstr_name }
    in
    Lexing.compare_pos pos constr_pos >= 0

let inspect_pattern ~pos ~lid p =
  let open Typedtree in
  let open Context in
  logfmt "inspect_context"
    (fun fmt -> Format.fprintf fmt "current pattern is: %a"
                  (Printtyped.pattern 0) p);
  match p.pat_desc with
  | Tpat_any when Longident.last lid = "_" -> None
  | Tpat_var (_, str_loc) when (Longident.last lid) = str_loc.txt ->
    None
  | Tpat_alias (_, _, str_loc)
    when (Longident.last lid) = str_loc.txt ->
    (* Assumption: if [Browse.enclosing] stopped on this node and not on the
       subpattern, then it must mean that the cursor is on the alias. *)
    None
  | Tpat_construct (lid_loc, cd, _)
    when cursor_on_constructor_name ~cursor:pos ~cstr_token:lid_loc cd
         && (Longident.last lid) = (Longident.last lid_loc.txt) ->
    (* Assumption: if [Browse.enclosing] stopped on this node and not on the
       subpattern, then it must mean that the cursor is on the constructor
       itself.  *)
      Some (Constructor cd)
  | _ ->
    Some Patt

let inspect_expression ~pos ~lid e : Context.t =
  match e.Typedtree.exp_desc with
  | Texp_construct (lid_loc, cd, _)
    when cursor_on_constructor_name ~cursor:pos ~cstr_token:lid_loc cd
         && (Longident.last lid) = (Longident.last lid_loc.txt) ->
    Constructor cd
  | _ ->
    Expr

let inspect_context browse lid pos : Context.t option =
  match Mbrowse.enclosing pos browse with
  | [] ->
    logf "inspect_context" "no enclosing around: %a" Lexing.print_position pos;
    Some Unknown
  | enclosings ->
    let open Browse_raw in
    let node = Browse_tree.of_browse enclosings in
    logf "inspect_context" "current node is: %s"
      (string_of_node node.Browse_tree.t_node);
    match node.Browse_tree.t_node with
    | Pattern p -> inspect_pattern ~pos ~lid p
    | Value_description _
    | Type_declaration _
    | Extension_constructor _
    | Module_binding_name _
    | Module_declaration_name _ ->
      None
    | Module_expr _
    | Open_description _ -> Some Module_path
    | Module_type _ -> Some Module_type
    | Core_type _ -> Some Type
    | Record_field (_, lbl, _)
      when (Longident.last lid) = lbl.lbl_name ->
      (* if we stopped here, then we're on the label itself, and whether or not
         punning is happening is not important *)
      Some (Label lbl)
    | Expression e -> Some (inspect_expression ~pos ~lid e)
    | _ ->
      Some Unknown

let from_string ~config ~env ~local_defs ~pos switch path =
  let browse = Mbrowse.of_typedtree local_defs in
  let lazy_trie = lazy (Typedtrie.of_browses ~local_buffer:true
                          [Browse_tree.of_browse browse]) in
  let lid = Longident.parse path in
  match inspect_context [browse] lid pos with
  | None ->
    log "from_string" "already at origin, doing nothing" ;
    `At_origin
  | Some ctxt ->
    logf "inspect_context" "inferred context: %s" (Context.to_string ctxt);
    logf "from_string" "looking for the source of '%s' (prioritizing %s files)"
      path (match switch with `ML -> ".ml" | `MLI -> ".mli") ;
    let_ref loadpath (Mconfig.cmt_path config) @@ fun () ->
    match
      from_longident ~config ~pos ~env ~lazy_trie ctxt switch lid
    with
    | `File_not_found _ | `Not_found _ | `Not_in_env _ as err -> err
    | `Builtin -> `Builtin path
    | `Found (loc, _) ->
      match find_source ~config loc with
      | Found src -> `Found (Some src, loc.Location.loc_start)
      | Not_found f -> File.explain_not_found path f
      | Multiple_matches lst ->
        let matches = String.concat lst ~sep:", " in
        `File_not_found (
          sprintf "Several source files in your path have the same name, and \
                   merlin doesn't know which is the right one: %s"
            matches
        )


let get_doc ~config ~env ~local_defs ~comments ~pos =
  let browse = Mbrowse.of_typedtree local_defs in
  let lazy_trie = lazy (Typedtrie.of_browses ~local_buffer:true
                          [Browse_tree.of_browse browse]) in
  fun path ->
  let_ref loadpath (Mconfig.cmt_path config) @@ fun () ->
  let_ref last_location Location.none @@ fun () ->
  match
    match path with
    | `Completion_entry entry -> from_completion_entry ~config ~pos ~lazy_trie entry
    | `User_input path ->
      let lid = Longident.parse path in
      begin match inspect_context [browse] lid pos with
      | None ->
        `Found ({ Location. loc_start=pos; loc_end=pos ; loc_ghost=true }, None)
      | Some ctxt ->
        logf "get_doc" "looking for the doc of '%s'" path ;
        from_longident ~config ~pos ~env ~lazy_trie ctxt `MLI lid
      end
  with
  | `Found (_, Some doc) ->
    `Found doc
  | `Found (loc, None) ->
    let comments =
      match File_switching.where_am_i () with
      | None -> List.rev comments
      | Some cmt_path ->
        let {Cmt_cache. cmt_infos; _ } = Cmt_cache.read cmt_path in
        cmt_infos.Cmt_format.cmt_comments
    in
    logfmt "get_doc" (fun fmt ->
        Format.fprintf fmt "looking around %a inside: [\n"
          Location.print_loc !last_location;
        List.iter comments ~f:(fun (c, l) ->
            Format.fprintf fmt "  (%S, %a);\n" c
              Location.print_loc l);
        Format.fprintf fmt "]\n"
      );
    begin match
      Ocamldoc.associate_comment comments loc !last_location
    with
    | None, _     -> `No_documentation
    | Some doc, _ -> `Found doc
    end
  | `Builtin ->
    begin match path with
    | `User_input path -> `Builtin path
    | `Completion_entry (_, path, _) -> `Builtin (Path.name path)
    end
  | `File_not_found _
  | `Not_found _
  | `Not_in_env _ as otherwise -> otherwise
