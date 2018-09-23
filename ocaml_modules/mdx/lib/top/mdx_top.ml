(*
 * Copyright (c) 2017 Frédéric Bour
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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
 *)

let redirect ~f =
  let stdout_backup = Unix.dup Unix.stdout in
  let stderr_backup = Unix.dup Unix.stdout in
  let filename = Filename.temp_file "mdx" "stdout" in
  let fd_out = Unix.openfile filename Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.dup2 fd_out Unix.stderr;
  let ic = open_in filename in
  let read_up_to = ref 0 in
  let capture buf =
    flush stdout;
    flush stderr;
    let pos = Unix.lseek fd_out 0 Unix.SEEK_CUR in
    let len = pos - !read_up_to in
    read_up_to := pos;
    Buffer.add_channel buf ic len
  in
  Misc.try_finally (fun () -> f ~capture)
    (fun () ->
       close_in_noerr ic;
       Unix.close fd_out;
       Unix.dup2 stdout_backup Unix.stdout;
       Unix.dup2 stderr_backup Unix.stderr;
       Unix.close stdout_backup;
       Unix.close stderr_backup;
       Sys.remove filename)

module Lexbuf = struct

  open Lexing

  type t = {
    contents: string;
    lexbuf  : lexbuf;
  }

  let toplevel_fname = "//toplevel//"

  let shift_toplevel_position ~start pos = {
    pos_fname = toplevel_fname;
    pos_lnum = pos.pos_lnum - start.pos_lnum + 1;
    pos_bol  = pos.pos_bol  - start.pos_cnum - 1;
    pos_cnum = pos.pos_cnum - start.pos_cnum ;
  }

  let shift_toplevel_location ~start loc =
    let open Location in
    {loc with loc_start = shift_toplevel_position ~start loc.loc_start;
              loc_end = shift_toplevel_position ~start loc.loc_end}

  let semisemi_action =
    let lexbuf = Lexing.from_string ";;" in
    match Lexer.token lexbuf with
    | Parser.SEMISEMI ->
      lexbuf.Lexing.lex_last_action
    | _ -> assert false

  let shift_location_error start =
    let open Location in
    let rec aux (error : Location.error) =
      {error with sub = List.map aux error.sub;
                  loc = shift_toplevel_location ~start error.loc}
    in
    aux

  let position_mapper start =
    let open Ast_mapper in
    let start = {start with pos_fname = toplevel_fname} in
    let location mapper loc =
      shift_toplevel_location ~start (default_mapper.location mapper loc)
    in
    {default_mapper with location}

end

module Phrase = struct

  open Lexing
  open Parsetree

  type t = {
    doc      : Lexbuf.t;
    startpos : position;
    endpos   : position;
    parsed   : (toplevel_phrase, exn) result;
  }

  let result t = t.parsed
  let start t = t.startpos

  let parse lines =
    let contents = String.concat " " lines in
    let lexbuf = Lexing.from_string contents in
    let startpos = lexbuf.Lexing.lex_start_p in
    let parsed = match Parse.toplevel_phrase lexbuf with
      | phrase -> Ok phrase
      | exception exn ->
        let exn = match Location.error_of_exn exn with
          | None -> raise exn
          | Some `Already_displayed -> raise exn
          | Some (`Ok error) ->
            Location.Error (Lexbuf.shift_location_error startpos error)
        in
        if lexbuf.Lexing.lex_last_action <> Lexbuf.semisemi_action then begin
          let rec aux () = match Lexer.token lexbuf with
            | Parser.SEMISEMI | Parser.EOF -> ()
            | exception Lexer.Error (_, _) -> ()
            | _ -> aux ()
          in
          aux ();
        end;
        Error exn
    in
    let endpos = lexbuf.Lexing.lex_curr_p in
    { doc = { lexbuf; contents}; startpos; endpos; parsed }

  let ends_by_semi_semi c = match List.rev c with
    | h::_ ->
      let len = String.length h in
      len > 2 && h.[len-1] = ';' && h.[len-2] = ';'
    | _ -> false

  let parse lines =
    let lines = if ends_by_semi_semi lines then lines else lines @ [";;"] in
    match parse lines with
    | exception End_of_file -> None
    | t -> Some t

  let is_findlib_directive =
    let findlib_directive = function
      | "require" | "use" | "camlp4o" | "camlp4r" | "thread" -> true
      | _ -> false
    in
    function
    | { parsed = Ok (Ptop_dir (dir, _)); _ } -> findlib_directive dir
    | _ -> false

end

open Parsetree

module Rewrite = struct

  type t = {
    typ    : Longident.t;
    witness: Longident.t;
    runner : Longident.t;
    rewrite: Warnings.loc -> expression -> expression;
    mutable preload: string option;
  }

  (* Rewrite Lwt.t expressions to Lwt_main.run <expr> *)
  let lwt =
    let typ = Longident.parse "Lwt.t" in
    let runner = Longident.parse "Lwt_main.run" in
    let witness = Longident.parse "Lwt.return" in
    let preload = Some "lwt.unix" in
    let open Ast_helper in
    let rewrite loc e =
      with_default_loc loc (fun () ->
          Exp.apply (Exp.ident (Location.mkloc runner loc))
            [(Asttypes.Nolabel, e)]
        )
    in
    { typ; runner; rewrite; witness; preload }

  (* Rewrite Async.Defered.t expressions to
     Async.Thread_safe.block_on_async_exn (fun () -> <expr>). *)
  let async =
    let typ = Longident.parse "Async.Deferred.t" in
    let runner = Longident.parse "Async.Thread_safe.block_on_async_exn" in
    let witness = runner in
    let preload = None in
    let open Ast_helper in
    let rewrite loc e =
      let punit =
        Pat.construct (Location.mkloc (Longident.Lident "()") loc) None in
      with_default_loc loc @@ fun () ->
      Exp.apply
        (Exp.ident (Location.mkloc runner loc))
        [(Asttypes.Nolabel, Exp.fun_ Asttypes.Nolabel None punit e)]
    in
    { typ; runner; rewrite; witness; preload }

  let normalize_type_path env path =
    match Env.find_type path env with
    | { Types.type_manifest = Some ty; _ } -> begin
        match Ctype.expand_head env ty with
        | { Types.desc = Types.Tconstr (path, _, _); _ } -> path
        | _ -> path
      end
    | _ -> path

  let is_persistent_value env longident =
    let rec is_persistent_path = function
      | Path.Pident id -> Ident.persistent id
      | Path.Pdot (p, _, _) -> is_persistent_path p
      | Path.Papply (_, p) -> is_persistent_path p
    in
    try is_persistent_path (fst (Env.lookup_value longident env))
    with Not_found -> false

  let apply ts env pstr_item path e =
    let rec aux = function
      | []   -> pstr_item
      | h::t ->
        let ty = normalize_type_path env (Env.lookup_type h.typ env) in
        if Path.same ty (normalize_type_path env path) then (
          let loc = pstr_item.Parsetree.pstr_loc in
          { Parsetree.pstr_desc = Parsetree.Pstr_eval (h.rewrite loc e, []);
            Parsetree.pstr_loc = loc }
        ) else
          aux t
    in
    aux ts

  let item ts env pstr_item tstr_item =
    match pstr_item.Parsetree.pstr_desc, tstr_item.Typedtree.str_desc with
    | (Parsetree.Pstr_eval (e, _),
       Typedtree.Tstr_eval ({ Typedtree.exp_type = typ; _ }, _)) ->
      begin match (Ctype.repr typ).Types.desc with
        | Types.Tconstr (path, _, _) -> apply ts env pstr_item path e
        | _ -> pstr_item
      end
    | _ -> pstr_item

  let active_rewriters () =
    List.filter (fun t ->
        is_persistent_value !Toploop.toplevel_env t.witness
      ) [lwt; async]

  let phrase phrase =
    let is_eval = function
      | { pstr_desc = Pstr_eval _; _ } -> true
      | _ -> false
    in
    match phrase with
    | Ptop_def pstr when List.exists is_eval pstr ->
      let ts = active_rewriters () in
      if ts = [] then phrase
      else (
        Env.reset_cache_toplevel ();
        let snap = Btype.snapshot () in
        let pstr =
          try
            let tstr, _tsg, env =
              Typemod.type_structure !Toploop.toplevel_env pstr Location.none
            in
            List.map2 (item ts env) pstr tstr.Typedtree.str_items
          with _ ->
            pstr
        in
        Btype.backtrack snap;
        Ptop_def pstr
      )
    | _ -> phrase

  let preload verbose ppf =
    let require pkg =
      let p = Ptop_dir ("require", Pdir_string pkg) in
      let _ = Toploop.execute_phrase verbose ppf p in
      ()
    in
    match active_rewriters () with
    | [] -> ()
    | ts ->
      let ts =
        List.fold_left (fun acc t -> match t.preload with
            | None   -> acc
            | Some x -> t.preload <- None; x :: acc
          ) [] ts
      in
      List.iter (fun pkg ->
          if verbose then require pkg
          else redirect ~f:(fun ~capture:_ -> require pkg)
        ) ts

end

type t = {
  mutable verbose: bool;
  mutable silent : bool;
  verbose_findlib: bool;
}

let toplevel_exec_phrase t ppf p = match Phrase.result p with
  | Error exn -> raise exn
  | Ok phrase ->
    Warnings.reset_fatal ();
    let mapper = Lexbuf.position_mapper (Phrase.start p) in
    let phrase = match phrase with
      | Ptop_def str -> Ptop_def (mapper.Ast_mapper.structure mapper str)
      | Ptop_dir _ as x -> x
    in
    let phrase = match phrase with
      | Ptop_dir _ as x -> x
      | Ptop_def s -> Ptop_def (Pparse.apply_rewriters_str ~tool_name:"mdx" s)
    in
    Rewrite.preload t.verbose_findlib ppf;
    let phrase = Rewrite.phrase phrase in
    if !Clflags.dump_parsetree then Printast. top_phrase ppf phrase;
    if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
    Env.reset_cache_toplevel ();
    Toploop.execute_phrase t.verbose ppf phrase

type var_and_value = V : 'a ref * 'a -> var_and_value

let protect_vars =
  let set_vars l = List.iter (fun (V (r, v)) -> r := v) l in
  fun vars ~f ->
    let backup = List.map (fun (V (r, _)) -> V (r, !r)) vars in
    set_vars vars;
    Misc.try_finally f (fun () -> set_vars backup)

let capture_compiler_stuff ppf ~f =
  protect_vars
    [ V (Location.formatter_for_warnings , ppf) ]
    ~f

let rec ltrim = function
  | "" :: t -> ltrim t
  | l       -> l

let trim_line str =
  let len = String.length str in
  if len = 0 then str else
    let trim_from = if str.[0] = '\n' then 1 else 0 in
    let trim_to = if str.[len - 1] = '\n' then len - 1 else len in
    String.sub str trim_from (trim_to - trim_from)

let rtrim l = List.rev (ltrim (List.rev l))
let trim l = ltrim (rtrim (List.map trim_line l))

let eval t cmd =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let exec_code ~capture phrase =
    let lines = ref [] in
    let capture () =
      capture buf;
      match Buffer.contents buf with
      | "" -> ()
      | s  -> Buffer.clear buf; lines := s :: !lines
    in
    let out_phrase' = !Oprint.out_phrase in
    let out_phrase ppf phr = match phr with
      | Outcometree.Ophr_exception _ -> out_phrase' ppf phr
      | _ ->
        capture ();
        out_phrase' ppf phr;
        capture ();
    in
    Oprint.out_phrase := out_phrase;
    let restore () = Oprint.out_phrase := out_phrase' in
    begin match toplevel_exec_phrase t ppf phrase with
      | (_ : bool) -> restore ()
      | exception exn ->
        restore ();
        Location.report_exception ppf exn
    end;
    Format.pp_print_flush ppf ();
    capture ();
    if t.silent || (not t.verbose_findlib && Phrase.is_findlib_directive phrase)
    then []
    else trim (List.rev !lines)
  in
  redirect ~f:(fun ~capture ->
      capture_compiler_stuff ppf ~f:(fun () ->
          match Phrase.parse cmd with
          | Some t -> exec_code ~capture t
          | None   -> []
        ))


let all_show_funs = ref []

let section_env = "Environment queries"

let std_out = lazy (Format.formatter_of_out_channel stdout)

let show_prim to_sig ppf lid =
  let env = !Toploop.toplevel_env in
  let loc = Location.none in
  try
    let s =
      match lid with
      | Longident.Lident s -> s
      | Longident.Ldot (_,s) -> s
      | Longident.Lapply _ ->
          Format.fprintf ppf "Invalid path %a@." Printtyp.longident lid;
          raise Exit
    in
    let id = Ident.create_persistent s in
    let sg = to_sig env loc id lid in
    Printtyp.wrap_printing_env env (fun () ->
        Format.fprintf ppf "@[%a@]@." Printtyp.signature sg
      )
  with
  | Not_found -> Format.fprintf ppf "@[Unknown element.@]@."
  | Exit -> ()

let reg_show_prim name to_sig doc =
  let lazy ppf = std_out in
  all_show_funs := to_sig :: !all_show_funs;
  Toploop.add_directive
    name
    (Directive_ident (show_prim to_sig ppf))
    { section = section_env; doc }

let show_val () =
  reg_show_prim "show_val"
    (fun env loc id lid ->
       let _path, desc = Typetexp.find_value env loc lid in
       [ Sig_value (id, desc) ]
    )
    "Print the signature of the corresponding value."

let show_type () =
  reg_show_prim "show_type"
    (fun env loc id lid ->
       let _path, desc = Typetexp.find_type env loc lid in
       [ Sig_type (id, desc, Trec_not) ]
    )
    "Print the signature of the corresponding type constructor."

let show_exception () =
  reg_show_prim "show_exception"
    (fun env loc id lid ->
       let desc = Typetexp.find_constructor env loc lid in
       if not (Ctype.equal env true [desc.cstr_res] [Predef.type_exn]) then
         raise Not_found;
       let ret_type =
         if desc.cstr_generalized then Some Predef.type_exn
         else None
       in
       let ext =
         { ext_type_path = Predef.path_exn;
           ext_type_params = [];
           ext_args = Cstr_tuple desc.cstr_args;
           ext_ret_type = ret_type;
           ext_private = Asttypes.Public;
           Types.ext_loc = desc.cstr_loc;
           Types.ext_attributes = desc.cstr_attributes; }
       in
         [Sig_typext (id, ext, Text_exception)]
    )
    "Print the signature of the corresponding exception."

let show_module () =
  let open Types in
  let trim_signature = function
    | Mty_signature sg ->
      Mty_signature (List.map (function
            Sig_module (id, md, rs) ->
            Sig_module (id, {md with md_attributes =
                                       (Location.mknoloc "...", Parsetree.PStr [])
                                       :: md.md_attributes},
                        rs)
          (*| Sig_modtype (id, Modtype_manifest mty) ->
              Sig_modtype (id, Modtype_manifest (trim_modtype mty))*)
          | item -> item)
          sg)
    | mty -> mty
  in
  reg_show_prim "show_module"
    (fun env loc id lid ->
       let rec accum_aliases path acc =
         let md = Env.find_module path env in
         let acc =
           Sig_module (id, {md with md_type = trim_signature md.md_type},
                       Trec_not) :: acc in
         match md.md_type with
         | Mty_alias(_, path) -> accum_aliases path acc
         | Mty_ident _ | Mty_signature _ | Mty_functor _ ->
           List.rev acc
       in
       let path, _ = Typetexp.find_module env loc lid in
       accum_aliases path []
    )
    "Print the signature of the corresponding module."

let show_module_type () =
  reg_show_prim "show_module_type"
    (fun env loc id lid ->
       let _path, desc = Typetexp.find_modtype env loc lid in
       [ Sig_modtype (id, desc) ]
    )
    "Print the signature of the corresponding module type."

let show_class () =
  reg_show_prim "show_class"
    (fun env loc id lid ->
       let _path, desc = Typetexp.find_class env loc lid in
       [ Sig_class (id, desc, Trec_not) ]
    )
    "Print the signature of the corresponding class."

let show_class_type () =
  reg_show_prim "show_class_type"
    (fun env loc id lid ->
       let _path, desc = Typetexp.find_class_type env loc lid in
       [ Sig_class_type (id, desc, Trec_not) ]
    )
    "Print the signature of the corresponding class type."

let show env loc id lid =
  let sg =
    List.fold_left
      (fun sg f -> try (f env loc id lid) @ sg with _ -> sg)
      [] !all_show_funs
  in
  if sg = [] then raise Not_found else sg

let show () =
  let lazy pp = std_out in
  Toploop.add_directive "show" (Directive_ident (show_prim show pp))
    {
      section = section_env;
      doc = "Print the signatures of components \
             from any of the categories below.";
    }

let verbose t =
  Toploop.add_directive "verbose"
    (Toploop.Directive_bool (fun x -> t.verbose <- x))
    { section = section_env ; doc = "Be verbose" }

let silent t = Toploop.add_directive "silent"
    (Toploop.Directive_bool (fun x -> t.silent <- x))
    { section = section_env; doc = "Be silent" }

module Linked = struct
  include (Topdirs : sig end)
  include (Ephemeron : sig end)
  include (Uchar : sig end)
  include (Condition : sig end)
end

(* BLACK MAGIC: patch field of a module at runtime *)
let monkey_patch (type a) (type b) (m: a) (prj: unit -> b) (v : b) =
  let m = Obj.repr m in
  let v = Obj.repr v in
  let v' = Obj.repr (prj ()) in
  if v' == v then () else (
    try
      for i = 0 to Obj.size m - 1 do
        if Obj.field m i == v' then (
          Obj.set_field m i v;
          if Obj.repr (prj ()) == v then raise Exit;
          Obj.set_field m i v';
        )
      done;
      invalid_arg "monkey_patch: field not found"
    with Exit -> ()
  )

let patch_env () =
  let module M = struct
    module type T = module type of Env
    let field () = Env.without_cmis
    let replacement f x = f x
    let () = monkey_patch (module Env : T) field replacement
  end
  in ()

let init ~verbose:v ~silent:s ~verbose_findlib () =
  Clflags.real_paths := false;
  Toploop.set_paths ();
  Compmisc.init_path true;
  Toploop.toplevel_env := Compmisc.initial_env ();
  Sys.interactive := false;
  patch_env ();
  Topfind.don't_load_deeply [
    "unix"; "findlib.top"; "findlib.internal"; "compiler-libs.toplevel"
  ];
  Topfind.add_predicates ["byte"; "toploop"];
  let t = { verbose=v; silent=s; verbose_findlib } in
  show ();
  show_val ();
  show_type ();
  show_module ();
  show_module_type ();
  show_exception ();
  show_class ();
  show_class_type ();
  verbose t;
  silent t;
  t