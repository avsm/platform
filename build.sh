#!/bin/sh -ex

WITH_OCAML=0

if [ -x "$(command -v ocamlc)" ]; then
  if [ "$(ocamlc -version)" != "4.07.0" ]; then
      echo 'OCaml compiler detected is not 4.07.0, so building local version'
      WITH_OCAML=1
  fi
else
  echo 'OCaml compiler not detected, building a local version of 4.07.0' 
  WITH_OCAML=1
fi

if [ -x "$(command -v gmake)" ]; then
  echo 'Using gmake instead of make'
  MAKE=gmake
else
  MAKE=make
fi

if [ $WITH_OCAML -eq 1 ]; then
  PREFIX="`pwd`/_obj"
  cd bootstrap/ocaml
  ./configure --prefix $PREFIX 
  $MAKE world.opt
  $MAKE install
  export PATH=$PREFIX/bin:$PATH
  cd ../..
fi

export LWT_FORCE_LIBEV_BY_DEFAULT=no
cd bootstrap/dune
$MAKE
cd ../..
./bootstrap/dune/_build/install/default/bin/dune build --profile=release @cli
cp bootstrap/dune/_build/install/default/bin/dune _build/default/output/
cp bootstrap/dune/_build/install/default/bin/jbuilder _build/default/output/
