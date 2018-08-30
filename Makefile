PACKAGES=opam-devel dune-release utop bun odoc merlin ocp-indent ocamlformat
PINS=ocp-indent odoc tyxml ocamlformat merlin lwt

build:
	cd vendor/lwt && ocaml src/util/configure.ml -use-libev false
	cd vendor/markup && ocaml src/configure.ml
	dune build --profile=release @cli

doc:
	dune build --profile=release @doc

clean:
	dune clean

DUNIVERSE?=duniverse

v-lock:
	$(DUNIVERSE) vendor-lock $(PINS:%=--pin %) $(PACKAGES) -v

v-pull:
	$(DUNIVERSE) vendor-pull -v

v-merge:
	$(DUNIVERSE) vendor-merge -v
