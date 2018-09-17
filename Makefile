PACKAGES=opam-devel dune-release utop bun odoc merlin ocp-indent ocamlformat ocamlfind craml
PINS=ocp-indent odoc tyxml ocamlformat merlin lwt ocamlfind markup mccs,https://github.com/AltGr/ocaml-mccs.git dune-release

build:
	./build.sh native

bytecode-only:
	./build.sh bytecode-only

flambda:
	./build.sh flambda

quick:
	cd vendor/lwt && ocaml src/util/configure.ml -use-libev false
	cd vendor/markup && ocaml src/configure.ml
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

debug-opam:
	$(DUNIVERSE) opam $(PINS:%=--pin %) $(PACKAGES) -v

debug-dune:
	$(DUNIVERSE) lock $(DEBUG)

git-lock:
	$(DUNIVERSE) git-lock $(PINS:%=--pin %) $(PACKAGES) $(DEBUG)

git-pull:
	$(DUNIVERSE) git-pull -v $(DEBUG)

git-merge:
	$(DUNIVERSE) git-merge -v $(DEBUG)
