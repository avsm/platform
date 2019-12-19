# all pod files
PODA = $(wildcard *.pod)
# pod files per section
POD5 = apt-cudf.conf.pod
POD8 = apt-cudf-get.pod
POD1 = $(filter-out $(POD5) $(POD8),$(PODA))

# manpages to be generated
MAN1 = $(patsubst %.pod,%.1,$(POD1))
MAN5 = $(patsubst %.pod,%.5,$(POD5))
MAN8 = $(patsubst %.pod,%.8,$(POD8))
MANA = $(MAN1) $(MAN5) $(MAN8)

# manpages in HTML to be generated
HTMLA = $(patsubst %.pod,%.html,$(PODA))

all: man html

man: $(MANA)

html: $(HTMLA)

%.1: %.pod
	pod2man --section 1 --center="DOSE Tools" \
		--release "$(NAME) $(VERSION)" $*.pod > $@

%.5: %.pod
	pod2man --section 5 --center="DOSE Tools" \
		--release "$(NAME) $(VERSION)" $*.pod > $@

%.8: %.pod
	pod2man --section 8 --center="DOSE Tools" \
		--release "$(NAME) $(VERSION)" $*.pod > $@

%.html: %.pod
	pod2html $*.pod > $@

distclean clean:
	rm -f $(MANA) $(HTMLA) pod2htmd.tmp pod2htmi.tmp

.PHONY: clean distclean man html all


