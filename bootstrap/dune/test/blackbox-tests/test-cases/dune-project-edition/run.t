  $ [ -e dune-project ] || echo File does not exist
  File does not exist
  $ mkdir src
  $ echo '(rule (alias runtest) (action (progn)))' >  src/dune
  $ dune build
  Info: Creating file dune-project with this contents:
  | (lang dune 2.0)
  $ cat dune-project
  (lang dune 2.0)

Test that using menhir automatically update the dune-project file

  $ echo '(library (name x)) (menhir (modules x))' >> src/dune
  $ dune build @install
  Info: Appending this line to dune-project: (using menhir 2.0)
  $ cat dune-project
  (lang dune 2.0)
  (using menhir 2.0)
