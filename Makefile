###########################################################################
### Variables
###

# Should gries-instrumented be in this list?
LISP_FILES := gries-helper.lisp instrument.lisp data-trace.lisp \
	load-all.lisp \
	gries.lisp gries-instrumented.lisp inv-medic.lisp
PYTHON_FILES := invariants.py util.py TextFile.py
DOC_FILES := invariants.py.doc Makefile TextFile.README daikon.html
EDG_DIR := /projects/se/people/jake/invariants/vortex/C++/front-end/release/dist
# $(EDG_DIR)/edgcpfe is distributed separately (not in the main tar file)
EDG_FILES := $(EDG_DIR)/dump_trace.h $(EDG_DIR)/dump_trace.c $(EDG_DIR)/instrumentor
DIST_DIR := /homes/gws/mernst/www/invariants-dist
# For really big files
DIST_DIR_2 := /projects/se/people/mernst/www

## Examples of better ways to get the lists:
# PERL_MODULES := $(wildcard *.pm)
# PERL_SCRIPTS := $(wildcard *.pl)
# PERL_SCRIPTS += em_analyze em_reports cppp
# PERL_MODULE_TEXI := $(patsubst %.pm,%.texi,$(PERL_MODULES))
# PERL_MODULE_INFO := $(patsubst %.pm,%.info,$(PERL_MODULES))
# PERL_MODULE_MAN := $(patsubst %.pm,%.man,$(PERL_MODULES))
# PERL_MODULE_HTML := $(patsubst %.pm,%.html,$(PERL_MODULES))


###########################################################################
### Rules
###

### Default tag
help:
	@echo "Targets:"
	@echo " tags TAGS"
	@echo " dist invariants.tar"
	@echo " dist-edg dist-edg-solaris"
	@echo " examples examples-gries"

### Tags

tags: TAGS

## As of July 1998, my Linux etags works on Python; my Solaris one doesn't.
## So I should be sure to do the make on a Linux machine. -MDE
TAGS:  $(LISP_FILES) $(PYTHON_FILES)
	etags $(LISP_FILES) $(PYTHON_FILES)

###########################################################################
### Distribution
###

## The update-link-dates script appears in ~mernst/bin/share/.

# Main distribution

dist: $(DIST_DIR)/invariants.tar.gz

$(DIST_DIR)/invariants.tar.gz: invariants.tar.gz
	cp -pf invariants.tar.gz dist/README dist/VERSION $(DIST_DIR)
	update-link-dates $(DIST_DIR)/index.html

# Also creates a directory called "dist"
invariants.tar: $(LISP_FILES) $(PYTHON_FILES) $(DOC_FILES) $(EDG_FILES) README-dist
	mkdir invariants
	cp -p $(LISP_FILES) $(PYTHON_FILES) $(DOC_FILES) invariants
	cp -p README-dist invariants/README

	# C/C++ instrumenter
	cp -p $(EDG_FILES) invariants
	cp -p $(EDG_DIR)/Makefile invariants/Makefile-sample
	echo "0" > invariants/label.txt
	# Fix permission problems with C/C++ instrumenter (due to Jake's directory)
	(cd invariants; chmod +r *; chmod -x Makefile-sample dump_trace.c dump_trace.h; chmod +x instrumentor)

	date > invariants/VERSION
	chgrp -R invariants invariants
	rm -rf invariants.tar
	tar cf invariants.tar invariants
	# After making the tar file, don't edit the (historical) distribution
	chmod -R uog-w invariants/*
	if (test -d dist-`date +'%y%m%d'`); then rm -rf dist-`date +'%y%m%d'`; fi
	mv invariants dist-`date +'%y%m%d'`
	rm dist
	ln -s dist-`date +'%y%m%d'` dist

invariants.tar.gz: invariants.tar
	rm -rf invariants.tar.gz
	gzip -c invariants.tar > invariants.tar.gz

### C front end

dist-edg: dist-edg-solaris

dist-edg-solaris: $(DIST_DIR_2)/edgcpfe-solaris

$(DIST_DIR_2)/edgcpfe-solaris: $(EDG_DIR)/edgcpfe
	cp -pf $< $@
	update-link-dates $(DIST_DIR)/index.html

### Examples

examples: examples-gries

# I made the replace examples by running the following on 4/23/99:
# /projects/null/se/people/mernst/www
# mkdir replace-TC1-traces; cp -p /projects/null/se/people/jake/invariants/test_gen/TC1/traces/* replace-TC1-traces/; tar czf replace-TC1-traces.tar.gz replace-TC1-traces; rm -rf replace-TC1-traces
# mkdir replace-TC3-traces; cp -p /projects/null/se/people/jake/invariants/test_gen/TC3/traces/* replace-TC3-traces/; tar czf replace-TC3-traces.tar.gz replace-TC3-traces; rm -rf replace-TC3-traces


GRIES_FILES := gries-instrumented.decls \
	p173-14.3.dtrace \
	p176.dtrace \
	p177-1.dtrace \
	p177-14.8.dtrace \
	p177-14.9.dtrace \
	p177-2.dtrace \
	p178-1b.dtrace \
	p180-15.1.1.dtrace \
	p184-3.dtrace \
	p187.dtrace \
	p191-2.dtrace

examples-gries: $(DIST_DIR)/examples-gries.tar.gz

$(DIST_DIR)/examples-gries.tar.gz: examples-gries.tar.gz
	cp -pf $< $@
	update-link-dates $(DIST_DIR)/index.html

examples-gries.tar.gz: $(GRIES_FILES) README-examples-gries
	mkdir examples-gries
	cp -pf $(GRIES_FILES) examples-gries
	cp -pf README-examples-gries examples-gries/README
	tar czf examples-gries.tar.gz examples-gries
	rm -rf examples-gries
