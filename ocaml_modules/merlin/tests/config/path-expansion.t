A simple name is not expanded

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx test1 | \
  > jq '.value.ocaml.ppx'
  sh: test1: command not found
  [
    {
      "workdir": "tests/config",
      "workval": "test1"
    }
  ]

Neither is an absolute path

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx /test2 | \
  > jq '.value.ocaml.ppx'
  sh: /test2: No such file or directory
  [
    {
      "workdir": "tests/config",
      "workval": "/test2"
    }
  ]

But relative names are

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx ./test3 | \
  > jq '.value.ocaml.ppx'
  sh: ./test3: No such file or directory
  [
    {
      "workdir": "tests/config",
      "workval": "./test3"
    }
  ]
