include Makefile.config

NAME = cudf

ifeq ("$(shell (ocamlc -config 2>/dev/null || ocamlopt -config) | fgrep os_type)","os_type: Win32")
EXE=.exe
OCAMLLIBDIR := $(shell cygpath $(OCAMLLIBDIR))
else
EXE=
endif

LIBS = _build/cudf.cma
LIBS_OPT = _build/cudf.cmxa
PROGS = _build/main_cudf_check _build/main_cudf_parse_822
PROGS_BYTE = $(addsuffix .byte,$(PROGS))
PROGS_OPT = $(addsuffix .native,$(PROGS))
DOC = doc/cudf-check.1
RESULTS = $(DOC) $(LIBS) $(PROGS_BYTE) _build/cudf_c.cmo
RESULTS_OPT = $(DOC) $(LIBS_OPT) $(PROGS_OPT) _build/cudf_c.cmx
SOURCES = $(wildcard *.ml *.mli *.mll *.mly)
C_LIB_DIR = c-lib
C_LIB_SOURCES = $(wildcard $(C_LIB_DIR)/*.c $(C_LIB_DIR)/*.h)

OCAMLBUILD = ocamlbuild
OBFLAGS =
OCAMLFIND = ocamlfind

ifeq ($(DESTDIR),)
INSTALL = $(OCAMLFIND) install
UNINSTALL = $(OCAMLFIND) remove
else
DESTDIR:=$(DESTDIR)/
INSTALL = $(OCAMLFIND) install -destdir $(DESTDIR)$(OCAMLLIBDIR)
UNINSTALL = $(OCAMLFIND) remove -destdir $(DESTDIR)$(OCAMLLIBDIR)
endif

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz

all: $(RESULTS)
opt: $(RESULTS_OPT)
$(RESULTS): $(SOURCES)
$(RESULTS_OPT): $(SOURCES)

doc/cudf-check.1: doc/cudf-check.pod
	$(MAKE) -C doc/

.PHONY: c-lib c-lib-opt doc
c-lib:
	make -C $(C_LIB_DIR) all
c-lib-opt:
	make -C $(C_LIB_DIR) opt

clean:
	make -C $(C_LIB_DIR) clean
	make -C doc/ clean
	$(OCAMLBUILD) $(OBFLAGS) -clean
	rm -rf $(NAME)-*.gz $(NAME)_*.gz $(NAME)-*/

_build/%:
	$(OCAMLBUILD) $(OBFLAGS) $*
	@touch $@

# top-level: _build/cudf.cma _build/tests.cmo
top-level: _build/cudf.cma
	ledit ocaml -I ./_build/ -init ./.ocamlinit-cudf

headers: header.txt .headache.conf
	headache -h header.txt -c .headache.conf $(SOURCES) $(C_LIB_SOURCES)

test: test.byte
	./$< -verbose
	@echo
c-lib-test:
	make -C $(C_LIB_DIR) test
test.byte: $(SOURCES)
	ocamlbuild $@

tags: TAGS
TAGS: $(SOURCES)
	otags $^

INSTALL_STUFF = META
INSTALL_STUFF += $(wildcard _build/*.cma _build/*.cmxa _build/cudf.a)
INSTALL_STUFF += $(wildcard _build/cudf_*.cmi) $(wildcard _build/*.mli)
INSTALL_STUFF += $(wildcard _build/cudf_*.cmx _build/cudf_*.o _build/cudf_*.a)
INSTALL_STUFF += $(wildcard _build/cudf.o _build/cudf.cmx _build/cudf.cmi)

install:
	test -d $(DESTDIR)$(OCAMLLIBDIR) || mkdir -p $(DESTDIR)$(OCAMLLIBDIR)
	$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)
	test -d $(DESTDIR)$(BINDIR) || mkdir -p $(DESTDIR)$(BINDIR)
	for p in $(notdir $(PROGS)) ; do \
		tgt=`echo $$p | sed -e 's/^main.//' -e 's/_/-/g'`$(EXE) ; \
		if [ -f _build/$$p.native ] ; then \
			cp _build/$$p.native $(DESTDIR)$(BINDIR)/$$tgt ; \
		else \
			cp _build/$$p.byte $(DESTDIR)$(BINDIR)/$$tgt ; \
		fi ; \
		echo "Installed $(DESTDIR)$(BINDIR)/$$tgt" ; \
	done
	if [ -f $(C_LIB_DIR)/cudf.o ] ; then \
		$(MAKE) -C c-lib/ -e install ; \
	fi

uninstall:
	$(UNINSTALL) $(NAME)
	for p in $(notdir $(PROGS)) ; do \
		tgt=`echo $$p | sed -e 's/^main.//' -e 's/_/-/g'`$(EXE) ; \
		if [ -f $(DESTDIR)$(BINDIR)/$$tgt ] ; then \
			rm $(DESTDIR)$(BINDIR)/$$tgt ; \
		fi ; \
		echo "Removed $(DESTDIR)$(BINDIR)/$$tgt" ; \
	done
	-rmdir -p $(DESTDIR)$(OCAMLLIBDIR) $(DESTDIR)$(BINDIR)

dist: ./$(DIST_TARBALL)
./$(DIST_TARBALL):
	git archive --format=tar --prefix=$(DIST_DIR)/ HEAD | gzip > $@
	@echo "Distribution tarball: ./$(DIST_TARBALL)"

rpm: ./$(DIST_TARBALL)
	rpmbuild --nodeps -ta $<

distcheck: ./$(DIST_TARBALL)
	tar xzf $<
	$(MAKE) -C ./$(DIST_DIR) all
	if which ocamlopt > /dev/null ; then $(MAKE) -C ./$(DIST_DIR) opt ; fi
	$(MAKE) -C ./$(DIST_DIR) test
	$(MAKE) -C ./$(DIST_DIR)/$(C_LIB_DIR)/ all
	$(MAKE) -C ./$(DIST_DIR) install DESTDIR=$(CURDIR)/$(DIST_DIR)/tmp
	rm -rf ./$(DIST_DIR)

doc:
	$(OCAMLBUILD) $(OBFLAGS) cudf.docdir/index.html

world: all opt c-lib c-lib-opt doc

.PHONY: all opt world clean top-level headers test tags install uninstall
.PHONY: dep rpm c-lib c-lib-opt dist doc
.NOTPARALLEL:
