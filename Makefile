###########################################################################
### Variables
###

# Should gries-instrumented be in this list?
LISP_FILES := gries-helper.lisp instrument.lisp data-trace.lisp \
	load-all.lisp \
	gries.lisp gries-instrumented.lisp inv-medic.lisp
PYTHON_FILES := daikon.py util.py TextFile.py
DOC_FILES := daikon.py.doc Makefile TextFile.README daikon.html
EDG_DIR := /projects/se/people/jake/invariants/vortex/C++/front-end/release/dist
# $(EDG_DIR)/edgcpfe is distributed separately (not in the main tar file)
EDG_FILES := $(EDG_DIR)/dump_trace.h $(EDG_DIR)/dump_trace.c $(EDG_DIR)/instrumentor
DIST_DIR := /homes/gws/mernst/www/daikon/dist
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
	@echo " dist daikon.tar"
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

dist: $(DIST_DIR)/daikon.tar.gz

$(DIST_DIR)/daikon.tar.gz: daikon.tar.gz
	cp -pf daikon.tar.gz dist/daikon.html $(DIST_DIR)
	# Don't edit the copy of daikon.html in the distribution directory
	chmod ogu-w $(DIST_DIR) daikon.tar.gz dist/daikon.html
	update-link-dates $(DIST_DIR)/index.html

# Also creates a directory called "dist"
daikon.tar: $(LISP_FILES) $(PYTHON_FILES) $(DOC_FILES) $(EDG_FILES) README-dist
	mkdir daikon
	cp -p $(LISP_FILES) $(PYTHON_FILES) $(DOC_FILES) daikon
	cp -p README-dist daikon/README

	# C/C++ instrumenter
	cp -p $(EDG_FILES) daikon
	cp -p $(EDG_DIR)/Makefile daikon/Makefile-sample
	echo "0" > daikon/label.txt
	# Fix permission problems with C/C++ instrumenter (due to Jake's directory)
	(cd daikon; chmod +r *; chmod -x Makefile-sample dump_trace.c dump_trace.h; chmod +x instrumentor)

	date > daikon/VERSION
	chgrp -R invariants daikon
	rm -rf daikon.tar
	tar cf daikon.tar daikon
	# After making the tar file, don't edit the (historical) distribution
	chmod -R uog-w daikon/*
	if (test -d dist-`date +'%y%m%d'`); then rm -rf dist-`date +'%y%m%d'`; fi
	mv daikon dist-`date +'%y%m%d'`
	rm dist
	ln -s dist-`date +'%y%m%d'` dist

daikon.tar.gz: daikon.tar
	rm -rf daikon.tar.gz
	gzip -c daikon.tar > daikon.tar.gz

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
