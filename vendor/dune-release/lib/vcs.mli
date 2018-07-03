(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** VCS repositories. *)

(** {1 VCS} *)

open Rresult

(** {1:vcsops Version control system repositories} *)

type kind = [ `Git | `Hg ]
(** The type for version control systems (VCS). *)

val pp_kind : Format.formatter -> kind -> unit
(** [pp_kind ppf k] prints an unspecified representation of [k] on [ppf]. *)

type commit_ish = string
(** The type for symbols resolving to a commit. The module uses
    ["HEAD"] for specifying the current checkout; use this symbol even
    if the underlying VCS is [`Hg]. *)

type t
(** The type for version control systems repositories. *)

val kind : t -> kind
(** [kind r] is [r]'s VCS kind. *)

val dir : t -> Fpath.t
(** [dir r] is [r]'s repository directory. *)

val cmd : t -> Bos.Cmd.t
(** [cmd r] is the base VCS command to use to act on [r].

    {b Warning} Prefer the functions below to remain VCS
    independent. *)

val find : ?dir:Fpath.t -> unit -> (t option, R.msg) result
(** [find ~dir ()] looks for a VCS repository in working directory
    [dir] (not the repository directory like [.git], default is guessed
    automatically). *)

val get : ?dir:Fpath.t -> unit -> (t, R.msg) result
(** [get] is like {!find} but returns an error if no VCS was found. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf r] prints an unspecified representation of [r] on [ppf]. *)

(** {1:state Repository state} *)

val is_dirty : t -> (bool, R.msg) result
(** [is_dirty r] is [Ok true] iff the working tree of [r] has
    uncommited changes. *)

val not_dirty : t -> (unit, R.msg) result
(** [not_dirty] is [Ok ()] iff the working directory of [r] is not
    dirty and an error that enjoins to stash or commit otherwise. *)

val file_is_dirty : t -> Fpath.t -> (bool, R.msg) result
(** [file_id_dirty r f] is [Ok true] iff [f] has uncommited changes. *)

val head : ?dirty:bool -> t -> (string, R.msg) result
(** [head ~dirty r] is the HEAD commit identifier of the repository
    [r]. If [dirty] is [true] (default), and indicator is appended to
    the commit identifier if the working tree of [r] {!is_dirty}. *)

val commit_id : ?dirty:bool -> ?commit_ish:commit_ish -> t -> (string, R.msg) result
(** [commit_id ~dirty ~commit_ish r] is the object name (identifier)
    of [commit_ish] (defaults to ["HEAD"]). If [commit_ish] is ["HEAD"]
    and [dirty] is [true] (default) and indicator is appended to the
    identifier if the working tree is dirty. *)

val commit_ptime_s : ?commit_ish:commit_ish -> t -> (int, R.msg) result
(** [commit_ptime_s t ~commit_ish] is the POSIX time in seconds of
    commit [commit_ish] (defaults to ["HEAD"]) of repository [r]. *)

val describe : ?dirty:bool -> ?commit_ish:commit_ish -> t -> (string, R.msg) result
(** [describe ~dirty ~commit_ish r] identifies [commit_ish] (defaults
    to ["HEAD"]) using tags from the repository [r]. If [commit_ish] is
    ["HEAD"] and [dirty] is [true] (default) an indicator is appended
    to the identifier if the working tree is dirty. *)

val tags : t -> (string list, R.msg) result
(** [tags r] is the list of tags in the repo [r]. *)

val tag_exists: dry_run:bool -> t -> string -> bool
val branch_exists: dry_run:bool -> t -> string -> bool

val changes :
  ?until:commit_ish -> t -> after:commit_ish -> ((string * string) list, R.msg) result
(** [changes r ~after ~until] is the list of commits with their
    one-line message from commit-ish [after] to commit-ish [until]
    (defaults to ["HEAD"]). *)

val tracked_files : ?tree_ish:string -> t -> (Fpath.t list, R.msg) result
(** [tracked_files ~tree_ish r] are the files tracked by the tree
    object [tree_ish] (defaults to ["HEAD"]). *)

(** {1:ops Repository operations} *)

val clone : dry_run:bool -> ?force:bool -> ?branch:string -> dir:Fpath.t ->
  t -> (unit, R.msg) result
(** [clone ~dir r] clones [r] in directory [dir]. *)

val checkout : dry_run:bool ->
  ?branch:string -> t -> commit_ish:commit_ish -> (unit, R.msg) result
(** [checkout r ~branch commit_ish] checks out [commit_ish]. Checks
    out in a new branch [branch] if provided. *)

val commit_files : dry_run:bool ->
  ?msg:string -> t -> Fpath.t list -> (unit, R.msg) result
(** [commit_files r ~msg files] commits the file [files] with message
    [msg] (if unspecified the VCS should prompt). *)

val tag :
  dry_run:bool -> ?force:bool -> ?sign:bool -> ?msg:string -> ?commit_ish:string -> t ->
  string -> (unit, R.msg) result
(** [tag r ~force ~sign ~msg ~commit_ish t] tags [commit_ish] with [t]
    and message [msg] (if unspecified the VCS should prompt).  if
    [sign] is [true] (defaults to [false]) signs the tag ([`Git] repos
    only).  If [force] is [true] (default to [false]) doesn't fail if
    the tag already exists. *)

val delete_tag : dry_run:bool -> t -> string -> (unit, R.msg) result
(** [delete_tag r t] deletes tag [t] in repo [r]. *)

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
