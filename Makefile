PACKAGES=opam-devel dune-release utop bun odoc merlin ocp-indent ocamlformat ocamlfind 
PINS=ocp-indent odoc tyxml ocamlformat merlin lwt ocamlfind markup mccs,https://github.com/avsm/ocaml-mccs.git,fix-warnings

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

v-lock:
	$(DUNIVERSE) vendor-lock $(PINS:%=--pin %) $(PACKAGES) -vv

v-pull:
	$(DUNIVERSE) vendor-pull -vv

v-merge:
	$(DUNIVERSE) vendor-merge -v
	sed -i 's/-default-configuration true/-use-libev false/g' vendor/lwt/src/unix/dune && git commit -m 'patch lwt libev' vendor/lwt/src/unix/dune
