(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup
open Dune_release

let get_pkg_dir pkg =
  Pkg.build_dir pkg
  >>= fun bdir -> Pkg.distrib_filename ~opam:true pkg
  >>= fun fname -> Ok Fpath.(bdir // fname)

let rec descr = function
| []   -> Ok 0
| h::t ->
    Pkg.opam_descr h >>= fun d ->
    Logs.app (fun m -> m "%s" (Opam.Descr.to_string d));
    if t <> [] then Logs.app (fun m -> m "---\n");
    descr t

module D = struct
  let distrib_uri = "${distrib_uri}"
end

let format_upgrade ~dry_run ~url ~opam_f pkg opam dir =
  let opam_t = OpamFile.OPAM.read_from_string opam in
  let url = OpamFile.URL.read_from_string url in
  match OpamVersion.to_string (OpamFile.OPAM.opam_version opam_t) with
  | "2.0" ->
      let file x = OpamFile.make (OpamFilename.of_string (Fpath.to_string x)) in
      let opam_t = OpamFile.OPAM.with_url url opam_t in
      if not dry_run then
        OpamFile.OPAM.write_with_preserved_format
          ~format_from:(file opam_f)
          (file Fpath.(dir / "opam"))
          opam_t;
      Ok ()
  | "1.0"|"1.1"|"1.2" ->
      Pkg.opam_descr pkg >>= fun descr ->
      let descr =
        OpamFile.Descr.read_from_string (Opam.Descr.to_string descr)
      in
      let opam =
        opam_t
        |> OpamFormatUpgrade.opam_file_from_1_2_to_2_0
        |> OpamFile.OPAM.with_url url
        |> OpamFile.OPAM.with_descr descr
        |> OpamFile.OPAM.write_to_string
      in
      Sos.write_file ~dry_run Fpath.(dir / "opam") opam
  | s -> Fmt.kstrf (fun x -> Error (`Msg x)) "invalid opam version: %s" s

let pkg ~dry_run pkg =
  let log_pkg dir =
    Logs.app (fun m -> m "Wrote opam package %a" Text.Pp.path dir)
  in
  let warn_if_vcs_dirty () =
    Cli.warn_if_vcs_dirty "The opam package may be inconsistent with the \
                           distribution."
  in
  get_pkg_dir pkg >>= fun dir ->
  Pkg.opam pkg >>= fun opam_f ->
  OS.File.read opam_f >>= fun opam ->
  OS.Path.exists opam_f >>= fun exists ->
  Pkg.distrib_file ~dry_run pkg >>= fun distrib_file ->
  (if dry_run && not exists then Ok D.distrib_uri else Pkg.distrib_uri pkg)
  >>= fun uri ->
  Opam.Url.with_distrib_file ~dry_run ~uri distrib_file >>= fun url ->
  OS.Dir.exists dir >>= fun exists ->
  (if exists then Sos.delete_dir ~dry_run dir else Ok ()) >>= fun () ->
  OS.Dir.create dir >>= fun _ ->
  format_upgrade ~dry_run ~url ~opam_f pkg opam dir >>= fun () ->
  log_pkg dir; (if not dry_run then warn_if_vcs_dirty () else Ok ())

let github_issue = Re.(compile @@ seq [
    group (compl [alpha]);
    group (seq [char '#'; rep1 digit]);
  ])

let rewrite_github_refs user repo msg =
  Re.replace github_issue msg
    ~f:(fun s ->
        let x = Re.Group.get s 1 in
        let y = Re.Group.get s 2 in
        Fmt.strf "%s%s/%s%s" x user repo y)

let rec pp_list pp ppf = function
  | []    -> ()
  | [x]   -> pp ppf x
  | [x;y] -> Fmt.pf ppf "%a and %a" pp x pp y
  | h::t  -> Fmt.pf ppf "%a, %a" pp h (pp_list pp) t

let rec list_map f = function
| []   -> Ok []
| h::t -> f h >>= fun h -> list_map f t >>= fun t -> Ok (h :: t)

let submit ~dry_run local_repo remote_repo pkgs auto_open =
  Config.token ~dry_run () >>= fun token ->
  List.fold_left (fun acc pkg ->
      get_pkg_dir pkg
      >>= fun pkg_dir -> Sos.dir_exists ~dry_run pkg_dir
      >>= function
      | true  ->
          Logs.app (fun m -> m "Submitting %a" Text.Pp.path pkg_dir);
          acc
      | false ->
          Logs.err (fun m ->
              m "Package@ %a@ does@ not@ exist. Did@ you@ forget@ \
                 to@ invoke 'dune-release opam pkg' ?" Fpath.pp pkg_dir);
          Ok 1
    ) (Ok 0) pkgs
  >>= fun _ ->
  let pkg = List.hd pkgs in
  Pkg.version pkg >>= fun version ->
  list_map Pkg.name pkgs >>= fun names ->
  let title = strf "[new release] %a (%s)" (pp_list Fmt.string) names version in
  Pkg.publish_msg pkg >>= fun changes ->
  Pkg.distrib_user_and_repo pkg >>= fun (distrib_user, repo) ->
  let changes = rewrite_github_refs distrib_user repo changes in
  let msg = strf "%s\n\n%s\n" title changes in
  Opam.prepare ~dry_run ~msg ~local_repo ~remote_repo ~version names
  >>= fun branch ->
  (* open a new PR *)
  Pkg.opam_descr pkg >>= fun (syn, _) ->
  Pkg.opam_homepage pkg >>= fun homepage ->
  Pkg.opam_doc pkg >>= fun doc ->
  let pp_link name ppf = function
  | None -> () | Some h -> Fmt.pf ppf "- %s: <a href=%S>%s</a>\n" name h h
  in
  let pp_space ppf () =
    if homepage <> None || doc <> None then Fmt.string ppf "\n"
  in
  let msg =
    strf "%s\n\n%a%a%a##### %s"
      syn
      (pp_link "Project page") homepage
      (pp_link "Documentation") doc
      pp_space ()
      changes
  in
  let user =
    match Github.user_from_remote remote_repo with
    | Some user -> user
    | None -> distrib_user
  in
  Github.open_pr ~token ~dry_run ~title ~distrib_user ~user ~branch msg >>= function
  | `Already_exists -> Logs.app (fun m ->
      m "\nThe existing pull request for %a has been automatically updated."
        Fmt.(styled `Bold string) (distrib_user ^ ":" ^ branch));
      Ok 0
  | `Url url ->
      let msg () =
        Logs.app (fun m -> m "A new pull-request has been created at %s\n" url);
        Ok 0
      in
      if not auto_open then msg ()
      else
      let auto_open =
        if OpamStd.Sys.(os () = Darwin) then "open" else "xdg-open"
      in
      match Sos.run ~dry_run Cmd.(v auto_open % url) with
      | Ok ()   -> Ok 0
      | Error _ -> msg ()

let field pkgs field = match field with
| None -> Logs.err (fun m -> m "Missing FIELD positional argument"); Ok 1
| Some field ->
    let rec loop = function
    | []   -> Ok 0
    | h::t ->
        Pkg.opam_field h field >>= function
        | Some v -> Logs.app (fun m -> m "%s" (String.concat ~sep:" " v)); loop t
        | None ->
            Pkg.opam h >>= fun opam ->
            Logs.err (fun m -> m "%a: field %s is undefined" Fpath.pp opam field);
            Ok 1
    in
    loop pkgs

(* Command *)

let opam () dry_run build_dir local_repo remote_repo user keep_v
    opam distrib_uri distrib_file tag
    name pkg_names version pkg_descr
    readme change_log publish_msg action field_name no_auto_open
  =
  begin
    Config.keep_v keep_v >>= fun keep_v ->
    Config.auto_open (not no_auto_open) >>= fun auto_open ->
    let distrib_file =
      let pkg =
        Pkg.v ?name ?opam ?tag ?version ?distrib_file
          ?distrib_uri ~dry_run:false ~keep_v ()
      in
      Pkg.distrib_archive_path pkg
    in
    let pkgs =
      Pkg.infer_pkg_names pkg_names >>= fun pkg_names ->
      let pkg_names = List.map (fun n -> Some n) pkg_names in
      distrib_file >>| fun distrib_file ->
      List.map (fun name ->
          Pkg.v ~dry_run
            ?build_dir ?name ?version ?opam ?tag
            ?opam_descr:pkg_descr ~keep_v
            ?distrib_uri ~distrib_file
            ?readme ?change_log ?publish_msg ()
        ) pkg_names
    in
    pkgs >>= fun pkgs ->
    match action with
    | `Descr -> descr pkgs
    | `Pkg ->
        List.fold_left (fun acc p ->
            match acc, pkg ~dry_run p with
            | Ok i, Ok () -> Ok i
            | (Error _ as e), _ | _, (Error _ as e) -> e

          ) (Ok 0) pkgs
    | `Submit ->
        Config.v ~user ~local_repo ~remote_repo pkgs >>= fun config ->
        (match local_repo with
        | Some r -> Ok Fpath.(v r)
        | None   ->
            match config.local with
            | Some r -> Ok r
            | None   -> R.error_msg "Unknown local repository.")
        >>= fun local_repo ->
        (match remote_repo with
        | Some r -> Ok r
        | None ->
            match config.remote with
            | Some r -> Ok r
            | None   -> R.error_msg "Unknown remote repository.")
        >>= fun remote_repo ->
        submit ~dry_run local_repo remote_repo pkgs auto_open
    | `Field -> field pkgs field_name
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  let action = [ "descr", `Descr; "pkg", `Pkg; "submit", `Submit;
                 "field", `Field ]
  in
  let doc = strf "The action to perform. $(docv) must be one of %s."
      (Arg.doc_alts_enum action)
  in
  let action = Arg.enum action in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")

let field =
  let doc = "the field to output ($(b,field) action)" in
  Arg.(value & pos 1 (some string) None & info [] ~doc ~docv:"FIELD")

let no_auto_open =
  let doc = "Do not open a browser to view the new pull-request." in
  Arg.(value & flag & info ["no-auto-open"] ~doc)

let user =
  let doc =
    "the name of the GitHub account where to push new opam-repository branches."
  in
  Arg.(value & opt (some string) None & info ["u"; "user"] ~doc ~docv:"USER")

let local_repo =
  let doc = "Location of the local fork of opam-repository" in
  let env = Arg.env_var "DUNE_RELEASE_LOCAL_REPO" in
  Arg.(value & opt (some string) None
       & info ~env ["l"; "--local-repo"] ~doc ~docv:"PATH")

let remote_repo =
  let doc = "Location of the remote fork of opam-repository" in
  let env = Arg.env_var "DUNE_RELEASE_REMOTE_REPO" in
  Arg.(value & opt (some string) None
       & info ~env ["r"; "--remote-repo"] ~doc ~docv:"URI")

let pkg_descr =
  let doc = "The opam descr file to use for the opam package. If absent and
             the opam file name (see $(b,--pkg-opam)) has a `.opam`
             extension, uses an existing file with the same path but a `.descr`
             extension. If the opam file name is `opam` uses a `descr`
             file in the same directory. If these files are not found
             a description is extracted from the the readme (see
             option $(b,--readme)) as follow: the first marked up
             section of the readme is extracted, its title is parsed
             according to the pattern '\\$(NAME) \\$(SEP) \\$(SYNOPSIS)',
             the body of the section is the long description. A few
             lines are filtered out: lines that start with either
             'Home page:', 'Contact:' or '%%VERSION'."
  in
  let docv = "FILE" in
  Arg.(value & opt (some Cli.path_arg) None & info ["pkg-descr"] ~doc ~docv)

let doc = "Interaction with opam and the OCaml opam repository"
let sdocs = Manpage.s_common_options
let envs = [  ]

let man_xrefs = [`Main; `Cmd "distrib" ]
let man =
  [ `S Manpage.s_synopsis;
    `P "$(mname) $(tname) [$(i,OPTION)]... $(i,ACTION)";
    `S Manpage.s_description;
    `P "The $(tname) command provides a few actions to interact with
        opam and the OCaml opam repository.";
    `S "ACTIONS";
    `I ("$(b,descr)",
        "extract and print an opam descr file. This is used by the
         $(b,pkg) action. See the $(b,--pkg-descr) option for details.");
    `I ("$(b,pkg)",
        "create an opam package description for a distribution.
         The action needs a distribution archive to operate, see
         dune-release-distrib(1) or the $(b,--dist-file) option.");
    `I ("$(b,submit)",
        "submits a package created with the action $(b,pkg) the OCaml
         opam repository. This requires configuration to be created
         manually first, see $(i, dune-release help files) for more
         details.");
    `I ("$(b,field) $(i,FIELD)",
        "outputs the field $(i,FIELD) of the package's opam file."); ]

let cmd =
  let info = Term.info "opam" ~doc ~sdocs ~envs ~man ~man_xrefs in
  let t = Term.(pure opam $ Cli.setup $ Cli.dry_run $ Cli.build_dir $
                local_repo $ remote_repo $
                user $ Cli.keep_v $
                Cli.dist_opam $ Cli.dist_uri $ Cli.dist_file $ Cli.dist_tag $
                Cli.dist_name $ Cli.pkg_names $ Cli.pkg_version $
                pkg_descr $ Cli.readme $ Cli.change_log $ Cli.publish_msg $
                action $ field $ no_auto_open)
  in
  (t, info)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
