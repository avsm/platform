  $ dune runtest --root absolute-path
  Entering directory 'absolute-path'
  File "jbuild", line 3, characters 16-24:
  Error: Invalid alias!
  Tried to reference path outside build dir: "/foo/bar"
  [1]
  $ dune runtest --root outside-workspace
  Entering directory 'outside-workspace'
  File "jbuild", line 4, characters 16-39:
  Error: path outside the workspace: ./../../../foobar from default
  [1]
