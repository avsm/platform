PACKAGES=opam-devel dune-release utop bun odoc merlin ocp-indent
PINS=ocp-indent odoc tyxml ocamlformat merlin ppx_tools_versioned
INSTALLS=vendor/odoc/odoc.install vendor/dune-release/dune-release.install vendor/utop/utop.install vendor/bun/bun.install

build:
	cd vendor/lwt && ocaml src/util/configure.ml -use-libev false
	dune build --profile=release $(INSTALLS)

v-lock:
	duniverse vendor-lock $(PINS:%=--pin %) $(PACKAGES) -v

v-pull:
	duniverse vendor-pull -v

v-merge:
	duniverse vendor-merge -v
