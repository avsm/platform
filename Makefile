PACKAGES=opam-devel dune-release utop bun odoc merlin ocp-indent ocamlformat
PINS=ocp-indent odoc tyxml ocamlformat merlin

v-lock:
	duniverse vendor-lock $(PINS:%=--pin %) $(PACKAGES) -v

v-pull:
	duniverse vendor-pull -v

v-merge:
	duniverse vendor-merge -v
