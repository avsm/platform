PACKAGES=opam-devel dune-release utop bun odoc merlin ocp-indent 
PINS=ocp-indent odoc tyxml ocamlformat merlin ppx_tools_versioned
INSTALLS=vendor/ocp-indent/ocp-indent.install vendor/opam-core/opam-client.install vendor/merlin/merlin.install vendor/odoc/odoc.install vendor/dune-release/dune-release.install vendor/utop/utop.install vendor/bun/bun.install vendor/opam-ci/opam-ci.install

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
