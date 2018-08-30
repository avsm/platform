PACKAGES=opam-devel dune-release utop bun odoc merlin ocp-indent ocamlformat dune ocamlfind
PINS=ocp-indent odoc tyxml ocamlformat merlin lwt dune ocamlfind

build:
	cd bootstrap/dune && $(MAKE)
	cd vendor/lwt && ocaml src/util/configure.ml
	cd vendor/markup && ocaml src/configure.ml
	./bootstrap/dune/_build/install/default/bin/dune build --profile=release @cli

doc:
	dune build --profile=release @doc

clean:
	rm -rf _build
	rm -rf vendor/dune/_build

DUNIVERSE?=duniverse

v-lock:
	$(DUNIVERSE) vendor-lock $(PINS:%=--pin %) $(PACKAGES) -v

v-pull:
	$(DUNIVERSE) vendor-pull -vv

v-merge:
	$(DUNIVERSE) vendor-merge -v
