#this is a forward reference to the target all below
all: all

#SHELL=/bin/bash
include Makefile.config

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz

#VERBOSE := -classic-display
OBFLAGS := $(VERBOSE) -j 10 -no-links -cflags -warn-error,FPSXY
APPFLAGS := $(VERBOSE) -j 10
#OBFLAGS := $(OBFLAGS) -tag debug
#OBFLAGS := $(OBFLAGS) -tag profile
#OBFLAGS := $(OBFLAGS) -classic-display

addnotrpm:
	@if [ ! -s doseparseNoRpm ]; then $(LN) doseparse doseparseNoRpm ; fi
	@cd doseparse ; \
		if [ ! -s doseparseNoRpm.mlpack ]; then \
			$(LN) doseparse.mlpack doseparseNoRpm.mlpack ; \
		fi ; \
	cd -

myocamlbuild.ml: myocamlbuild.ml.pp
	cppo $(CPPOFLAGS) $< -o $@

all: libs apps man

apps: addnotrpm myocamlbuild.ml itarget $(BYTELIBS) $(OPTLIBS) 
	$(OCAMLBUILD) $(APPFLAGS) applications/apps.otarget

libs: addnotrpm myocamlbuild.ml itarget $(BYTELIBS) $(OPTLIBS) $(CMXSLIBS) $(ALIBS)

cleandoselib:
	rm -Rf $(DOSELIBS)

itarget:
	@rm -f applications/apps.itarget
	@for i in $(TARGETS); do echo $$i >> applications/apps.itarget; done
	@$(shell \
		for lib in $(LIBNAMES); do \
			libname=`basename "$$lib"` ;\
			dirname=`dirname "$$lib"` ;\
			rm -f $$dirname/$$libname.itarget ;\
			for ext in $(SUFFIX); do \
				echo "$$libname.$$ext" >> $$dirname/$$libname.itarget; \
			done;\
		done)

$(DOSELIBS)/cudf.%:
	$(OCAMLBUILD) $(OBFLAGS) cudf/cudf.$*
	@mkdir -p $(DOSELIBS)
	@cp _build/cudf/*.cmi $(DOSELIBS)
	@rm _build/cudf/*.cmi
	@for i in _build/cudf/cudf.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/common.%: common/*.ml common/*.mli
	$(OCAMLBUILD) $(OBFLAGS) common/common.otarget
	@mkdir -p $(DOSELIBS)
	@for i in _build/common/common.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
		rm $$i ;\
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/versioning.%: versioning/*.ml versioning/*.mli
	$(OCAMLBUILD) $(OBFLAGS) versioning/versioning.otarget
	@mkdir -p $(DOSELIBS)
	@for i in _build/versioning/versioning.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
		rm $$i ;\
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/algo.%: algo/*.ml algo/*.mli $(DOSELIBS)/common.%
	$(OCAMLBUILD) $(OBFLAGS) algo/algo.otarget
	@for i in _build/algo/algo.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
		rm $$i ;\
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/debian.%: deb/*.ml deb/*.mli $(DOSELIBS)/pef.%
	$(OCAMLBUILD) $(OBFLAGS) deb/debian.otarget
	@for i in _build/deb/debian.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
		rm $$i ;\
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/opam.%: opam/*.ml opam/*.mli $(DOSELIBS)/pef.%
	$(OCAMLBUILD) $(OBFLAGS) opam/opam.otarget
	@for i in _build/opam/opam.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
		rm $$i ;\
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/npm.%: npm/*.ml npm/*.mli $(DOSELIBS)/versioning.% $(DOSELIBS)/pef.%
	$(OCAMLBUILD) $(OBFLAGS) npm/npm.otarget
	@for i in _build/npm/npm.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
		rm $$i ;\
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/rpm.%: rpm/*.ml $(DOSELIBS)/algo.%
	$(OCAMLBUILD) $(OBFLAGS) rpm/rpm.otarget
	@for i in _build/rpm/rpm.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
		rm $$i ;\
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/pef.%: pef/*.ml pef/*.mli
	$(OCAMLBUILD) $(OBFLAGS) pef/pef.otarget
	@for i in _build/pef/pef.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
		rm $$i ;\
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/csw.%: opencsw/*.ml $(DOSELIBS)/versioning.%
	$(OCAMLBUILD) $(OBFLAGS) opencsw/csw.otarget
	@for i in _build/opencsw/csw.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
		rm $$i ;\
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/doseparse.%: $(DOSELIBS)/pef.% $(DOSELIBS)/debian.%
	$(OCAMLBUILD) $(OBFLAGS) doseparse/doseparse.otarget
	@for i in _build/doseparse/doseparse.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
		rm $$i ;\
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx $(DOSELIBS)/*.ml ; \
	  fi ; \
	done

$(DOSELIBS)/doseparseNoRpm.%: $(DOSELIBS)/pef.% $(DOSELIBS)/debian.%
	$(OCAMLBUILD) $(OBFLAGS) doseparseNoRpm/doseparseNoRpm.otarget
	@for i in _build/doseparseNoRpm/doseparseNoRpm.*; do \
	  if [ -e $$i ]; then \
			cp $$i $(DOSELIBS) ;\
			rm $$i ;\
			rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ;\
	  fi ; \
	done

clean:
	$(OCAMLBUILD) -clean
	@$(shell \
		for lib in $(LIBNAMES); do \
			libname=`basename "$$lib"` ;\
			dirname=`dirname "$$lib"` ;\
			rm -f $$dirname/$$libname.itarget ;\
		done)
	rm -f myocamlbuild.ml
	rm -f applications/apps.itarget
	rm -f doseparseNoRpm doseparse/doseparseNoRpm.mlpack
	cd doc && $(MAKE) clean

distclean: clean
	rm -Rf Makefile.config aclocal.m4 config.log config.status autom4te.cache/
	rm -f algo/algo.mlpack
	rm -f common/versionInfo.ml
	rm -f db/db.mlpack
	rm -f _tags META applications/dose-tests.list dose3.odocl

test: apps
ifeq (libs,$(word 2,$(MAKECMDGOALS)))
	$(MAKE) testlib
else
ifdef group
	applications/dose-tests.py --rungroup $(group) applications/dose-tests.list
else 
ifdef unit
	applications/dose-tests.py --runtest $(unit) applications/dose-tests.list
else
	$(MAKE) testlib
	applications/dose-tests.py applications/dose-tests.list
endif
endif
endif

testlib: 
	echo $(TESTS)
	@for i in $(TESTS); do\
		echo "#######START TESTING $$i" ;\
		$(OCAMLBUILD) $(APPFLAGS) $$i/tests.$(OCAMLEXT) || exit 1;\
		./tests.$(OCAMLEXT) || exit 1;\
	done

# stuff not not put in a distribution tarball
DIST_EXCLUDE = cudf tests $(wildcard */tests) experimental doc/webpages

INSTALL_STUFF_ = META
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.cma _build/doselibs/*.cmi)
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.cmxa _build/doselibs/*.cmxs)
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.a)
#INSTALL_STUFF_ += $(wildcard _build/*/*.mli)
INSTALL_STUFF_ += $(wildcard _build/rpm/*.so)

exclude_cudf = $(wildcard _build/doselibs/*cudf* _build/cudf/*)
INSTALL_STUFF = $(filter-out $(exclude_cudf), $(INSTALL_STUFF_))

installlib: META installcudf
	@test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	@test -d $(LIBDIR)/stublibs || mkdir -p $(LIBDIR)/stublibs
	@$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)
	@echo "Install dose librairies to $(LIBDIR)"

install: installlib
	@cd _build/applications ; \
	install -d $(BINDIR) ; \
	for f in $$(ls *.$(OCAMLEXT)) ; do \
		install $(INSTALLOPTS) $$f $(BINDIR)/$${f%.$(OCAMLEXT)}$(EXE) ; \
	done
	@$(LN) $(BINDIR)/distcheck $(BINDIR)/debcheck$(EXE)
	@$(LN) $(BINDIR)/distcheck $(BINDIR)/rpmcheck$(EXE)
	@$(LN) $(BINDIR)/distcheck $(BINDIR)/eclipsecheck$(EXE)
	@echo "Install dose binaries to $(BINDIR)"

uninstalllib:
	@$(OCAMLFIND) remove -destdir $(LIBDIR) $(NAME)
	@echo "Uninstall dose librairies from $(LIBDIR)"

uninstall: uninstalllib uninstallcudf
	@for f in $$(ls *.$(OCAMLEXT)) ; do \
	  rm -f $(BINDIR)/$${f%.$(OCAMLEXT)}$(EXE) ; \
	done
	@rm -f $(BINDIR)/debcheck$(EXE) $(BINDIR)/rpmcheck$(EXE) $(BINDIR)/eclipsecheck$(EXE)
	@echo "Uninstall dose binaries from $(BINDIR)"

dist: ./$(DIST_TARBALL)
./$(DIST_TARBALL):
	@if [ -d ./$(DIST_DIR)/ ] ; then rm -rf ./$(DIST_DIR)/ ; fi
	@if [ -d ./$(DIST_TARBALL) ] ; then rm -f ./$(DIST_TARBALL) ; fi
	@mkdir ./$(DIST_DIR)/ ; git archive --format=tar HEAD | tar -x -C ./$(DIST_DIR)/
	@for f in $(DIST_EXCLUDE) ; do rm -rf ./$(DIST_DIR)/$$f; done
	@tar czf ./$(DIST_TARBALL) ./$(DIST_DIR)
	@rm -rf ./$(DIST_DIR)
	@echo "Distribution tarball: ./$(DIST_TARBALL)"

changelog:
	dch -c CHANGES --package $(NAME) -v $(VERSION)

credits:
	@git log --pretty=format:'%aN        %aE' | LC_ALL=C sort -u | awk -F'\t' '{printf("\t%s <%s>\n",$$1,$$2)}';

doc: all
	$(OCAMLBUILD) -package unix scripts/pack.$(OCAMLEXT)
	scripts/doc.sh $(OCAMLEXT)
	dot -Grotate=0 -Tsvg -o dose3.docdir/index.svg dose3.docdir/index.dot
	(cd doc && $(MAKE) all)

man:
	cd doc/manpages && $(MAKE)

upload: doc
	(cd doc && $(MAKE) upload)
	rsync -avz -O dose3.docdir/ scm.gforge.inria.fr:/home/groups/dose/htdocs/API/

.PHONY: \
	common algo pef versioning debian rpm csw doseparseNoRpm doseparse \
	all clean top-level headers test tags install uninstall dist doc man
