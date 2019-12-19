open! Stdune
module Context = Dune.Context
module Workspace = Dune.Workspace
module Dune_project = Dune.Dune_project
module Vcs = Dune.Vcs

type checked =
  | In_build_dir of (Context.t * Path.Source.t)
  | In_install_dir of (Context.t * Path.Source.t)
  | In_source_dir of Path.Source.t
  | External of Path.External.t

let check_path contexts =
  let contexts =
    Dune.Context_name.Map.of_list_exn
      (List.map contexts ~f:(fun c -> (c.Context.name, c)))
  in
  fun path ->
    let internal_path () =
      User_error.raise
        [ Pp.textf "This path is internal to dune: %s"
            (Path.to_string_maybe_quoted path)
        ]
    in
    let context_exn ctx =
      match Dune.Context_name.Map.find contexts ctx with
      | Some context -> context
      | None ->
        User_error.raise
          [ Pp.textf "%s refers to unknown build context: %s"
              (Path.to_string_maybe_quoted path)
              (Dune.Context_name.to_string ctx)
          ]
          ~hints:
            (User_message.did_you_mean
               (Dune.Context_name.to_string ctx)
               ~candidates:
                 ( Dune.Context_name.Map.keys contexts
                 |> List.map ~f:Dune.Context_name.to_string ))
    in
    match path with
    | External e -> External e
    | In_source_tree s -> In_source_dir s
    | In_build_dir path -> (
      match Dune.Dpath.analyse_target path with
      | Other _ -> internal_path ()
      | Alias (_, _) -> internal_path ()
      | Install (name, src) -> In_install_dir (context_exn name, src)
      | Regular (name, src) -> In_build_dir (context_exn name, src) )
