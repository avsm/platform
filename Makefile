PACKAGES=opam-devel dune-release utop odoc merlin ocp-indent ocamlformat ocamlfind mdx
PINS=

build:
	./build.sh native

bytecode-only:
	./build.sh bytecode-only

flambda:
	./build.sh flambda

quick:
	./bootstrap/dune/_build/install/default/bin/dune build --profile=release @cli

doc:
	dune build --profile=release @doc

clean:
	rm -rf _build _obj
	rm -rf vendor/dune/_build

DUNIVERSE?=duniverse

vendor-dune:
	rm -rf bootstrap/dune
	opam source dune --dir=bootstrap/dune
	git add bootstrap/dune
	git commit -m 'update dune vendor' bootstrap/dune

init:
	$(DUNIVERSE) init $(PACKAGES)
