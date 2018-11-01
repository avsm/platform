<<<<<<< HEAD
let user_from_remote_test_case =
  let test () =
    let check repo_uri expected =
      Alcotest.(check (option string))
        repo_uri
        expected
        (Dune_release.Github.user_from_remote repo_uri)
    in
    check "git@github.com:username/repo.git" (Some "username");
    check "git@github.com:user-name/repo.git" (Some "user-name");
    check "git@github.com:user-name-123/repo.git" (Some "user-name-123");
    check "git@github.com:123/repo.git" (Some "123");
    (* Same as above but without the .git part *)
    check "git@github.com:username/repo" (Some "username");
    check "git@github.com:user-name/repo" (Some "user-name");
    check "git@github.com:user-name-123/repo" (Some "user-name-123");
    check "git@github.com:123/repo" (Some "123");
    check "wrong" None;
    check "https://github.com/username/repo.git" None;
    ()
  in
  ("user_from_remote", `Quick, test)

let suite =
  ( "Github"
  , [ user_from_remote_test_case
    ]
  )
=======
let user_from_remote () =
  let check repo_uri expected =
    Alcotest.(check (option string))
      repo_uri
      expected
      (Dune_release.Github.user_from_remote repo_uri)
  in
  check "git@github.com:username/repo.git" (Some "username");
  check "git@github.com:user-name/repo.git" (Some "user-name");
  check "git@github.com:user-name-123/repo.git" (Some "user-name-123");
  check "git@github.com:123/repo.git" (Some "123");
  (* Same as above but without the .git part *)
  check "git@github.com:username/repo" (Some "username");
  check "git@github.com:user-name/repo" (Some "user-name");
  check "git@github.com:user-name-123/repo" (Some "user-name-123");
  check "git@github.com:123/repo" (Some "123");
  check "wrong" None;
  check "https://github.com/username/repo.git" None

let suite =
  "Github", [
    "user_from_remote", `Quick, user_from_remote;
]
>>>>>>> 6c0d22059a376f2e5e7fcfdde3014740a747ec3a
