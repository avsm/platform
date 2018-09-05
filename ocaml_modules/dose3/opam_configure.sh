opam init -y
eval `opam config env`
opam switch -y 4.02.0
eval `opam config env`
opam install -y extlib camlbz2 camlzip ocamlgraph extlib ounit re cudf cppo
