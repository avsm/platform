open! Stdune
open Dune_lang.Decoder

(* workspace files use the same version numbers as dune-project files for
   simplicity *)
let syntax = Stanza.syntax

let env_field =
  field "env" ~default:Dune_env.Stanza.empty
    (Dune_lang.Syntax.since syntax (1, 1) >>> Dune_env.Stanza.decode)

module Context = struct
  module Target = struct
    type t =
      | Native
      | Named of Context_name.t

    let t =
      map string ~f:(function
        | "native" -> Native
        | s -> Named (Context_name.parse_string_exn (Loc.none, s)))

    let add ts x =
      match x with
      | None -> ts
      | Some t ->
        if List.mem t ~set:ts then
          ts
        else
          ts @ [ t ]
  end

  module Common = struct
    type t =
      { loc : Loc.t
      ; profile : Profile.t
      ; targets : Target.t list
      ; env : Dune_env.Stanza.t
      ; toolchain : Context_name.t option
      ; name : Context_name.t
      ; host_context : Context_name.t option
      ; paths : (string * Ordered_set_lang.t) list
      ; fdo_target_exe : Path.t option
      ; disable_dynamically_linked_foreign_archives : bool
      }

    let fdo_suffix t =
      match t.fdo_target_exe with
      | None -> ""
      | Some file ->
        let name, _ = Path.split_extension file in
        "-fdo-" ^ Path.basename name

    let t ~profile =
      let+ env = env_field
      and+ targets =
        field "targets" (repeat Target.t) ~default:[ Target.Native ]
      and+ profile = field "profile" Profile.decode ~default:profile
      and+ host_context =
        field_o "host"
          (Dune_lang.Syntax.since syntax (1, 10) >>> Context_name.decode)
      and+ toolchain =
        field_o "toolchain"
          (Dune_lang.Syntax.since syntax (1, 5) >>> Context_name.decode)
      and+ disable_dynamically_linked_foreign_archives =
        field ~default:false "disable_dynamically_linked_foreign_archives"
          (Dune_lang.Syntax.since syntax (2, 0) >>> bool)
      and+ fdo_target_exe =
        let f file =
          let ext = Filename.extension file in
          if ext = ".exe" then
            Path.(relative root file)
          else
            User_error.raise
              [ Pp.textf
                  "`fdo %s` expects executable filename ending with .exe \
                   extension, not %s. \n\
                   Please specify the name of the executable to optimize, \
                   including path from <root>."
                  file ext
              ]
        in
        field_o "fdo" (Dune_lang.Syntax.since syntax (2, 0) >>> map string ~f)
      and+ paths =
        let f l =
          match
            Env.Map.of_list (List.map ~f:(fun ((loc, s), _) -> (s, loc)) l)
          with
          | Ok _ -> List.map ~f:(fun ((_, s), x) -> (s, x)) l
          | Error (var, _, loc) ->
            User_error.raise ~loc
              [ Pp.textf
                  "the variable %S can appear at most once in this stanza." var
              ]
        in
        field "paths" ~default:[]
          ( Dune_lang.Syntax.since Stanza.syntax (1, 12)
          >>> map ~f (repeat (pair (located string) Ordered_set_lang.decode))
          )
      and+ loc = loc in
      Option.iter host_context ~f:(fun _ ->
          match targets with
          | [ Target.Native ] -> ()
          | _ ->
            User_error.raise ~loc
              [ Pp.text
                  "`targets` and `host` options cannot be used in the same \
                   context."
              ]);
      { targets
      ; profile
      ; loc
      ; env
      ; name = Context_name.default
      ; host_context
      ; toolchain
      ; paths
      ; fdo_target_exe
      ; disable_dynamically_linked_foreign_archives
      }
  end

  module Opam = struct
    type t =
      { base : Common.t
      ; switch : Context_name.t
      ; root : string option
      ; merlin : bool
      }

    let t ~profile ~x =
      let+ switch = field "switch" Context_name.decode
      and+ name = field_o "name" Context_name.decode
      and+ root = field_o "root" string
      and+ merlin = field_b "merlin"
      and+ base = Common.t ~profile in
      let default =
        (* TODO this needs proper error handling with locations *)
        let name = Context_name.to_string switch ^ Common.fdo_suffix base in
        Context_name.parse_string_exn (Loc.none, name)
      in
      let name = Option.value ~default name in
      let base = { base with targets = Target.add base.targets x; name } in
      { base; switch; root; merlin }
  end

  module Default = struct
    type t = Common.t

    let t ~profile ~x =
      let+ common = Common.t ~profile
      and+ name =
        field_o "name"
          ( Dune_lang.Syntax.since syntax (1, 10)
          >>= fun () -> Context_name.decode )
      in
      let default =
        (* TODO proper error handling with locs *)
        let name =
          Context_name.to_string common.name ^ Common.fdo_suffix common
        in
        Context_name.parse_string_exn (Loc.none, name)
      in
      let name = Option.value ~default name in
      { common with targets = Target.add common.targets x; name }
  end

  type t =
    | Default of Default.t
    | Opam of Opam.t

  let loc = function
    | Default x -> x.loc
    | Opam x -> x.base.loc

  let host_context = function
    | Default { host_context; _ }
    | Opam { base = { host_context; _ }; _ } ->
      host_context

  let t ~profile ~x =
    sum
      [ ("default", fields (Default.t ~profile ~x) >>| fun x -> Default x)
      ; ("opam", fields (Opam.t ~profile ~x) >>| fun x -> Opam x)
      ]

  let env = function
    | Default d -> d.env
    | Opam o -> o.base.env

  let name = function
    | Default d -> d.name
    | Opam o -> o.base.name

  let targets = function
    | Default x -> x.targets
    | Opam x -> x.base.targets

  let all_names t =
    let n = name t in
    n
    :: List.filter_map (targets t) ~f:(function
         | Native -> None
         | Named s -> Some (Context_name.target n ~toolchain:s))

  let default ?x ?profile () =
    Default
      { loc = Loc.of_pos __POS__
      ; targets = [ Option.value x ~default:Target.Native ]
      ; profile = Option.value profile ~default:Profile.default
      ; name = Context_name.default
      ; host_context = None
      ; env = Dune_env.Stanza.empty
      ; toolchain = None
      ; paths = []
      ; fdo_target_exe = None
      ; disable_dynamically_linked_foreign_archives = false
      }
end

type t =
  { merlin_context : Context_name.t option
  ; contexts : Context.t list
  ; env : Dune_env.Stanza.t
  }

include Dune_lang.Versioned_file.Make (struct
  type t = unit
end)

let () = Lang.register syntax ()

let bad_configuration_check map =
  let find_exn loc name host =
    match Context_name.Map.find map host with
    | Some host_ctx -> host_ctx
    | None ->
      User_error.raise ~loc
        [ Pp.textf "Undefined host context '%s' for '%s'."
            (Context_name.to_string host)
            (Context_name.to_string name)
        ]
  in
  let check elt =
    Context.host_context elt
    |> Option.iter ~f:(fun host ->
           let name = Context.name elt in
           let loc = Context.loc elt in
           let host_elt = find_exn loc name host in
           Context.host_context host_elt
           |> Option.iter ~f:(fun host_of_host ->
                  User_error.raise ~loc:(Context.loc host_elt)
                    [ Pp.textf
                        "Context '%s' is both a host (for '%s') and a target \
                         (for '%s')."
                        (Context_name.to_string host)
                        (Context_name.to_string name)
                        (Context_name.to_string host_of_host)
                    ]))
  in
  Context_name.Map.iter map ~f:check

let top_sort contexts =
  let key = Context.name in
  let map =
    Context_name.Map.of_list_map_exn contexts ~f:(fun x -> (key x, x))
  in
  let deps def =
    match Context.host_context def with
    | None -> []
    | Some ctx -> [ Context_name.Map.find_exn map ctx ]
  in
  bad_configuration_check map;
  match Context_name.Top_closure.top_closure ~key ~deps contexts with
  | Ok topo_contexts -> topo_contexts
  | Error _ -> assert false

let t ?x ?profile:cmdline_profile () =
  let* () = Dune_lang.Versioned_file.no_more_lang in
  let* env = env_field in
  let* profile = field "profile" Profile.decode ~default:Profile.default in
  let profile = Option.value cmdline_profile ~default:profile in
  let+ contexts = multi_field "context" (Context.t ~profile ~x) in
  let defined_names = ref Context_name.Set.empty in
  let merlin_context =
    List.fold_left contexts ~init:None ~f:(fun acc ctx ->
        let name = Context.name ctx in
        if Context_name.Set.mem !defined_names name then
          User_error.raise ~loc:(Context.loc ctx)
            [ Pp.textf "second definition of build context %S"
                (Context_name.to_string name)
            ];
        defined_names :=
          Context_name.Set.union !defined_names
            (Context_name.Set.of_list (Context.all_names ctx));
        match (ctx, acc) with
        | Opam { merlin = true; _ }, Some _ ->
          User_error.raise ~loc:(Context.loc ctx)
            [ Pp.text "you can only have one context for merlin" ]
        | Opam { merlin = true; _ }, None -> Some name
        | _ -> acc)
  in
  let contexts =
    match contexts with
    | [] -> [ Context.default ?x ~profile () ]
    | _ -> contexts
  in
  let merlin_context =
    match merlin_context with
    | Some _ -> merlin_context
    | None ->
      if
        List.exists contexts ~f:(function
          | Context.Default _ -> true
          | _ -> false)
      then
        Some Context_name.default
      else
        None
  in
  { merlin_context; contexts = top_sort (List.rev contexts); env }

let t ?x ?profile () = fields (t ?x ?profile ())

let default ?x ?profile () =
  { merlin_context = Some Context_name.default
  ; contexts = [ Context.default ?x ?profile () ]
  ; env = Dune_env.Stanza.empty
  }

let load ?x ?profile p =
  let x = Option.map x ~f:(fun s -> Context.Target.Named s) in
  Io.with_lexbuf_from_file p ~f:(fun lb ->
      if Dune_lexer.eof_reached lb then
        default ?x ?profile ()
      else
        let first_line = Dune_lang.Versioned_file.First_line.lex lb in
        parse_contents lb first_line ~f:(fun _lang -> t ?x ?profile ()))

let default ?x ?profile () =
  let x = Option.map x ~f:(fun s -> Context.Target.Named s) in
  default ?x ?profile ()

let filename = "dune-workspace"
