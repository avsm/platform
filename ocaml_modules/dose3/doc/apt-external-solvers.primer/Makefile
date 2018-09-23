MAIN=apt-external-solvers
SHELL=/bin/bash
HEVEA=hevea -exec xxdate.exe
DEPS = $(wildcard *.tex) version.tex mancoosi.bib

all: html ps pdf

include ../../Makefile.config

html: $(MAIN).html 

ps: $(MAIN).ps

pdf: $(MAIN).pdf


$(MAIN).html: $(DEPS) $(MAIN).aux
	$(HEVEA) -fix $(MAIN).tex

$(MAIN).ps: $(MAIN).dvi
	dvips $<

$(MAIN).pdf: $(DEPS)
	pdflatex $(MAIN)

$(MAIN).dvi: $(DEPS)
	latex $(MAIN)

$(MAIN).aux: $(DEPS)
	latex $(MAIN)

version.tex:
	echo -n "\\def\\version{" >> $@
	echo -n $(VERSION) >> $@
	echo "}" >> $@

headers:
	headache -h headache.header -c headache.conf $(DEPS)

distclean clean:
	-rm -f $(MAIN).{dvi,aux,toc,haux,htoc,log,bbl,blg,html,ps,pdf}\
		 version.tex

.phony: clean distclean headers all ps pdf html
