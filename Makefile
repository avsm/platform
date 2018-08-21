PACKAGES=opam-devel dune-release utop bun odoc merlin ocp-indent ocamlformat
PINS=ocp-indent odoc tyxml ocamlformat merlin ppx_tools_versioned

build:
	cd vendor/lwt && ocaml src/util/configure.ml -use-libev false
	cd vendor/markup && ocaml src/configure.ml
	dune build --profile=release @cli

doc:
	dune build --profile=release @doc

clean:
	dune clean

v-lock:
	duniverse vendor-lock $(PINS:%=--pin %) $(PACKAGES) -v

v-pull:
	duniverse vendor-pull -v

v-merge:
	duniverse vendor-merge -v
