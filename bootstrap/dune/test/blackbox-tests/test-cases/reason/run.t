  $ dune build @runtest @install-file --display short
         refmt bar.re.ml
      ocamldep .rlib.objs/bar.re.ml.d
      ocamldep pp/.reasononlypp.eobjs/reasononlypp.ml.d
        ocamlc pp/.reasononlypp.eobjs/reasononlypp.{cmi,cmo,cmt}
      ocamlopt pp/.reasononlypp.eobjs/reasononlypp.{cmx,o}
      ocamlopt pp/reasononlypp.exe
  reasononlypp cppome.pp.re
         refmt cppome.pp.re.ml
      ocamldep .rlib.objs/cppome.pp.re.ml.d
      ocamldep ppx/.reasonppx.objs/reasonppx.ml.d
        ocamlc ppx/.reasonppx.objs/reasonppx.{cmi,cmo,cmt}
      ocamlopt ppx/.reasonppx.objs/reasonppx.{cmx,o}
      ocamlopt ppx/reasonppx.{a,cmxa}
      ocamlopt .ppx/jbuild/reasonppx@rlib/ppx.exe
           ppx foo.pp.ml
      ocamldep .rlib.objs/foo.pp.ml.d
         refmt hello.re.ml
           ppx hello.re.pp.ml
      ocamldep .rlib.objs/hello.re.pp.ml.d
         refmt pped.re.ml
      ocamldep .rlib.objs/pped.re.ml.d
        ocamlc .rlib.objs/rlib.{cmi,cmo,cmt}
      ocamlopt .rlib.objs/rlib.{cmx,o}
      ocamldep .rlib.objs/bar.mli.d
        ocamlc .rlib.objs/rlib__Bar.{cmi,cmti}
      ocamlopt .rlib.objs/rlib__Bar.{cmx,o}
         refmt foo.re.mli
           ppx foo.re.pp.mli
      ocamldep .rlib.objs/foo.re.pp.mli.d
        ocamlc .rlib.objs/rlib__Foo.{cmi,cmti}
      ocamlopt .rlib.objs/rlib__Foo.{cmx,o}
         refmt hello.re.mli
           ppx hello.re.pp.mli
      ocamldep .rlib.objs/hello.re.pp.mli.d
        ocamlc .rlib.objs/rlib__Hello.{cmi,cmti}
      ocamlopt .rlib.objs/rlib__Hello.{cmx,o}
         refmt pped.re.mli
      ocamldep .rlib.objs/pped.re.mli.d
        ocamlc .rlib.objs/rlib__Pped.{cmi,cmti}
      ocamlopt .rlib.objs/rlib__Pped.{cmx,o}
  reasononlypp cppome.pp.rei
         refmt cppome.pp.re.mli
      ocamldep .rlib.objs/cppome.pp.re.mli.d
        ocamlc .rlib.objs/rlib__Cppome.{cmi,cmti}
      ocamlopt .rlib.objs/rlib__Cppome.{cmx,o}
      ocamlopt rlib.{a,cmxa}
      ocamlopt rlib.cmxs
  reasononlypp rbin.pp.re
         refmt rbin.pp.re.ml
      ocamldep .rbin.eobjs/rbin.pp.re.ml.d
        ocamlc .rbin.eobjs/rbin.{cmi,cmo,cmt}
      ocamlopt .rbin.eobjs/rbin.{cmx,o}
      ocamlopt rbin.exe
          rbin alias runtest
  Cppome
  hello world
  Bar
  Foo
        ocamlc .rlib.objs/rlib__Bar.{cmo,cmt}
        ocamlc .rlib.objs/rlib__Foo.{cmo,cmt}
        ocamlc .rlib.objs/rlib__Hello.{cmo,cmt}
        ocamlc .rlib.objs/rlib__Pped.{cmo,cmt}
        ocamlc .rlib.objs/rlib__Cppome.{cmo,cmt}
        ocamlc rlib.cma
  lib: [
    "_build/install/default/lib/rlib/META" {"META"}
    "_build/install/default/lib/rlib/bar.mli" {"bar.mli"}
    "_build/install/default/lib/rlib/bar.re" {"bar.re"}
    "_build/install/default/lib/rlib/cppome.re" {"cppome.re"}
    "_build/install/default/lib/rlib/cppome.rei" {"cppome.rei"}
    "_build/install/default/lib/rlib/foo.ml" {"foo.ml"}
    "_build/install/default/lib/rlib/foo.rei" {"foo.rei"}
    "_build/install/default/lib/rlib/hello.re" {"hello.re"}
    "_build/install/default/lib/rlib/hello.rei" {"hello.rei"}
    "_build/install/default/lib/rlib/opam" {"opam"}
    "_build/install/default/lib/rlib/pped.re" {"pped.re"}
    "_build/install/default/lib/rlib/pped.rei" {"pped.rei"}
    "_build/install/default/lib/rlib/rlib$ext_lib" {"rlib$ext_lib"}
    "_build/install/default/lib/rlib/rlib.cma" {"rlib.cma"}
    "_build/install/default/lib/rlib/rlib.cmi" {"rlib.cmi"}
    "_build/install/default/lib/rlib/rlib.cmt" {"rlib.cmt"}
    "_build/install/default/lib/rlib/rlib.cmx" {"rlib.cmx"}
    "_build/install/default/lib/rlib/rlib.cmxa" {"rlib.cmxa"}
    "_build/install/default/lib/rlib/rlib.cmxs" {"rlib.cmxs"}
    "_build/install/default/lib/rlib/rlib.dune" {"rlib.dune"}
    "_build/install/default/lib/rlib/rlib.ml-gen" {"rlib.ml-gen"}
    "_build/install/default/lib/rlib/rlib__Bar.cmi" {"rlib__Bar.cmi"}
    "_build/install/default/lib/rlib/rlib__Bar.cmt" {"rlib__Bar.cmt"}
    "_build/install/default/lib/rlib/rlib__Bar.cmti" {"rlib__Bar.cmti"}
    "_build/install/default/lib/rlib/rlib__Bar.cmx" {"rlib__Bar.cmx"}
    "_build/install/default/lib/rlib/rlib__Cppome.cmi" {"rlib__Cppome.cmi"}
    "_build/install/default/lib/rlib/rlib__Cppome.cmt" {"rlib__Cppome.cmt"}
    "_build/install/default/lib/rlib/rlib__Cppome.cmti" {"rlib__Cppome.cmti"}
    "_build/install/default/lib/rlib/rlib__Cppome.cmx" {"rlib__Cppome.cmx"}
    "_build/install/default/lib/rlib/rlib__Foo.cmi" {"rlib__Foo.cmi"}
    "_build/install/default/lib/rlib/rlib__Foo.cmt" {"rlib__Foo.cmt"}
    "_build/install/default/lib/rlib/rlib__Foo.cmti" {"rlib__Foo.cmti"}
    "_build/install/default/lib/rlib/rlib__Foo.cmx" {"rlib__Foo.cmx"}
    "_build/install/default/lib/rlib/rlib__Hello.cmi" {"rlib__Hello.cmi"}
    "_build/install/default/lib/rlib/rlib__Hello.cmt" {"rlib__Hello.cmt"}
    "_build/install/default/lib/rlib/rlib__Hello.cmti" {"rlib__Hello.cmti"}
    "_build/install/default/lib/rlib/rlib__Hello.cmx" {"rlib__Hello.cmx"}
    "_build/install/default/lib/rlib/rlib__Pped.cmi" {"rlib__Pped.cmi"}
    "_build/install/default/lib/rlib/rlib__Pped.cmt" {"rlib__Pped.cmt"}
    "_build/install/default/lib/rlib/rlib__Pped.cmti" {"rlib__Pped.cmti"}
    "_build/install/default/lib/rlib/rlib__Pped.cmx" {"rlib__Pped.cmx"}
  ]
