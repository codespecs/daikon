###########################################################################
### Variables
###

# Should gries-instrumented be in this list?
LISP_FILES := gries-helper.lisp instrument.lisp data-trace.lisp \
	load-all.lisp \
	gries.lisp gries-instrumented.lisp inv-medic.lisp
PYTHON_FILES := invariants.py util.py
DOC_FILES := invariants.py.doc Makefile
EDG_DIR := /projects/se/people/jake/invariants/vortex/C++/front-end/release/dist
# $(EDG_DIR)/edgcpfe is distributed separately (not in the main tar file
EDG_FILES := $(EDG_DIR)/dump_trace.h $(EDG_DIR)/dump_trace.h $(EDG_DIR)/dump_trace.c $(EDG_DIR)/instrumentor

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

### Distribution

dist: invariants.tar.gz

# Also creates a directory called "dist"
invariants.tar: $(LISP_FILES) $(PYTHON_FILES) $(DOC_FILES) $(EDG_FILES) README-dist
	if (test -d dist); then rm -rf dist-`date +'%y%m%d'`; mv -f dist dist-`date +'%y%m%d'`; fi
	mkdir invariants
	cp -p $(LISP_FILES) $(PYTHON_FILES) $(DOC_FILES) invariants
	cp -p README-dist invariants/README
	# C/C++ instrumenter
	cp -p $(EDG_FILES) invariants
	cp -p $(EDG_DIR)/Makefile invariants/Makefile-sample
	echo "0" > invariants/label.txt
	rm -rf invariants.tar
	tar cvf invariants.tar invariants
	mv invariants dist
	chmod -R uog-w dist

invariants.tar.gz: invariants.tar
	rm -rf invariants.tar.gz
	gzip -c invariants.tar > invariants.tar.gz

### Tags

tags: TAGS

## As of July 1998, my Linux etags works on Python; my Solaris one doesn't.
## So I should be sure to do the make on a Linux machine. -MDE
TAGS:  $(LISP_FILES) $(PYTHON_FILES)
	etags $(LISP_FILES) $(PYTHON_FILES)
