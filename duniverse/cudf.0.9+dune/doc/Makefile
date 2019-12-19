include ../Makefile.config

MANPAGES = cudf-check

GEN_STUFF = $(patsubst %,%.1,$(MANPAGES))

all: $(GEN_STUFF)

%.1: %.pod
	pod2man --release $(VERSION) $< > $@

clean:
	rm -f $(GEN_STUFF)

show: cudf-check.1
	man -l $<

.PHONY: all clean show
