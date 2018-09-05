INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

all:
	jbuilder build --dev @install

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

test:
	jbuilder runtest --dev

promote:
	jbuilder promote

clean:
	jbuilder clean

all-supported-ocaml-versions:
	jbuilder runtest --dev --workspace jbuild-workspace.dev

.PHONY: all-supported-ocaml-versions all install uninstall reinstall test clean
