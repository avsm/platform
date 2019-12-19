all: rebuild

upload:
	$(MAKE) -C ../ upload

makedoc:
	$(MAKE) -C ../../ doc

docdir: makedoc
	for i in ../../dose3.docdir/*.html ; \
		do ./extract-ocamldoc.py $$i > src/API/`basename $$i`; \
	done

manpage:
	./extract-manpages.py ../manpages/*.html

primer:
	./extract-primer.py ../debcheck.primer/debcheck-primer.html > src/tutorials/primer.mdwn

refresh:
	ikiwiki --setup local.setup --verbose --refresh

rebuild: manpage primer
	ikiwiki --setup local.setup --verbose --rebuild

clean:
	rm -Rf public_html

.phony: refresh rebuild clean
