PACKAGES=opam-devel dune-release utop bun odoc merlin ocp-indent ocamlformat ocamlfind
PINS=ocp-indent odoc tyxml ocamlformat merlin lwt ocamlfind ocp-index

build:
	cd bootstrap/ocaml && ./configure --prefix `pwd`/../../_obj && $(MAKE) -j $(J) world.opt && $(MAKE) install
	cd bootstrap/dune && env PATH=`pwd`/../../_obj/bin:$$PATH $(MAKE)
	cd vendor/lwt && env PATH=`pwd`/../../_obj/bin:$$PATH ocaml src/util/configure.ml -use-libev false
	cd vendor/markup && env PATH=`pwd`/../../_obj/bin:$$PATH ocaml src/configure.ml
	env PATH=`pwd`/_obj/bin:$$PATH ./bootstrap/dune/_build/install/default/bin/dune build --profile=release @cli
	cp bootstrap/dune/_build/install/default/bin/dune _build/default/output/
	cp bootstrap/dune/_build/install/default/bin/jbuilder _build/default/output/

doc:
	dune build --profile=release @doc

clean:
	rm -rf _build _obj
	rm -rf vendor/dune/_build

DUNIVERSE?=duniverse

v-lock:
	$(DUNIVERSE) vendor-lock $(PINS:%=--pin %) $(PACKAGES) -v

v-pull:
	$(DUNIVERSE) vendor-pull -vv

v-merge:
	$(DUNIVERSE) vendor-merge -v
