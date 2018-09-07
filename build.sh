#!/bin/sh -ex

WITH_OCAML=0

MODE=$1
shift

if [ "$MODE" != "native" ]; then
  echo Forcing a local compiler to be built for $MODE
  WITH_OCAML=1
elif [ -x "$(command -v ocamlc)" ]; then
  if [ "$(ocamlc -version)" != "4.07.0" ]; then
    echo 'OCaml compiler detected is not 4.07.0, so building local version'
    WITH_OCAML=1
  else
    echo 'Using system OCaml 4.07 compiler'
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
  if [ ! -e bootstrap/ocaml/Makefile -a -d .git ] ; then
    git submodule update --init bootstrap/ocaml
  fi
  cd bootstrap/ocaml
  case MODE in
  bytecode-only)
    ./configure --prefix $PREFIX --no-native-compiler
    $MAKE world
    ;;
  flambda)
    ./configure --prefix $PREFIX --flambda
    $MAKE world.opt
    ;;
  *)
    ./configure --prefix $PREFIX
    $MAKE world.opt
    ;;
  esac
  $MAKE install
  export PATH=$PREFIX/bin:$PATH
  cd ../..
fi

if [ ! -e bootstrap/dune/Makefile -a -d .git ] ; then
  git submodule update --init bootstrap/dune
fi
cd bootstrap/dune
$MAKE
cd ../..
./bootstrap/dune/_build/install/default/bin/dune build --profile=release --debug-dependency-path @cli
cp bootstrap/dune/_build/install/default/bin/dune _build/default/output/
cp bootstrap/dune/_build/install/default/bin/jbuilder _build/default/output/
