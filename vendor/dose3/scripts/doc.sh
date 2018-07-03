#!/bin/bash

# this script the documentation of all dose submodules using ocppack

OCAMLEXT=${1:-native}
OCPPACK=_build/scripts/pack.$OCAMLEXT

echo "Cleaning up before"
for i in deb common opam pef algo versioning ; do
  F=$(ls $i/*.mlpack) ;
  rm -f ${F%.mlpack}.ml
  rm -f ${F%.mlpack}.mli
done

echo "Create packs"
for i in deb common opam pef algo versioning ; do
  echo -n "$i..."
  F=$(ls $i/*.mlpack) ;
  ML=$(cat $F | sed -e "s/\(.\)\(.*\)/_build\/$i\/\L\1\E\2.ml/" |  tr '\n' ' ') ;
  MML=
  for f in $ML; do
    if echo $f | grep -v -E "_lexer|_parser" ; then
      MML="$MML $f"
    fi
  done > /dev/null
  $OCPPACK -no-ml -mli -pp 'cppo -D '\''OCAMLGRAPHVERSION 186'\''' -o ${F%.mlpack}.ml $MML ;
  if [ -f $i/.ocamldoc.txt ]; then
    cat $i/.ocamldoc.txt ${F%.mlpack}.mli > ${F%.mlpack}.mli.tmp
    mv ${F%.mlpack}.mli.tmp ${F%.mlpack}.mli
    rm -f ${F%.mlpack}.mli.tmp
  fi
done

i=doseparse
echo "$i..."
F=$(ls $i/doseparse.mlpack) ;
ML=$(cat $F | sed -e "s/\(.\)\(.*\)/_build\/$i\/\L\1\E\2.ml/" |  tr '\n' ' ') ;
MML=
for f in $ML; do
  if echo $f | grep -v -E "_lexer|_parser" ; then
    MML="$MML $f"
  fi
done > /dev/null
$OCPPACK -pp 'cppo -D '\''OCAMLGRAPHVERSION 186'\''' -o ${F%.mlpack}.ml $MML ;
if [ -f $i/.ocamldoc.txt ]; then
  cat $i/.ocamldoc.txt ${F%.mlpack}.ml > ${F%.mlpack}.ml.tmp
  mv ${F%.mlpack}.ml.tmp ${F%.mlpack}.ml
  rm -f ${F%.mlpack}.ml.tmp
fi


echo 

ocamlbuild dose3.docdir/index.html dose3.docdir/index.dot

echo "Cleaning up after"
for i in deb common opam pef algo versioning ; do
  F=$(ls $i/*.mlpack) ;
  rm -f ${F%.mlpack}.ml
  rm -f ${F%.mlpack}.mli
done
