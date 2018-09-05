SUBDIRS = manpages debcheck.primer apt-external-solvers.primer
SHELL=/bin/bash

all:
	@for subdir in $(SUBDIRS); do \
	  (cd $$subdir && $(MAKE)); \
	done

clean:
	@for subdir in $(SUBDIRS); do \
	  (cd $$subdir && $(MAKE) clean); \
	done

webpages:
	cd webpages && $(MAKE) rebuild

upload: webpages
	rsync -avz -O --delete --no-perms webpages/public_html/* scm.gforge.inria.fr:/home/groups/dose/htdocs/
