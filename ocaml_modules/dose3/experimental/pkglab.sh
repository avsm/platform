#!/bin/sh
init=/tmp/pkglab.$$.ml
ocamlfind query -a-format -predicates native unix extlib str pcre bz2 zip ocamlgraph cudf dose3 | sed -e 's/^/#load "/' -e 's/cmxa$/cmxs";;/' > $init
cat >> $init <<EOF
#load "boilerplate.cmxs";;
#use "pkglab.ml";;
EOF
if [ -f `which ledit` ]
then
	ledit  ocamlnat `ocamlfind query -i-format -predicates native bz2 zip ocamlgraph dose3 cudf pcre extlib` \
	-init $init
else
	ocamlnat `ocamlfind query -i-format -predicates native bz2 zip ocamlgraph dose3 cudf pcre extlib` \
         -init $init
fi	
rm $init
