###########################################################################
### Variables
###

# Should gries-instrumented be in this list?
LISP_FILES := lisp-front-end/gries-helper.lisp lisp-front-end/instrument.lisp lisp-front-end/data-trace.lisp \
	lisp-front-end/load-all.lisp \
	lisp-front-end/gries.lisp lisp-front-end/gries-instrumented.lisp lisp-front-end/inv-medic.lisp
PYTHON_FILES := daikon.py util.py TextFile.py
DOC_FILES := daikon.py.doc Makefile TextFile.README daikon.html
EDG_DIR := /homes/gws/mernst/research/invariants/edg/dist
# $(EDG_DIR)/edgcpfe is distributed separately (not in the main tar file)
EDG_FILES := $(EDG_DIR)/dump_trace.h $(EDG_DIR)/dump_trace.c $(EDG_DIR)/dfec $(EDG_DIR)/dfec.sh
DFEJ_DIR := /homes/gws/mernst/research/invariants/dfej

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
	@echo " dist dist-force daikon.tar"
	@echo " dist-edg dist-edg-solaris"
	@echo " dist-dfej dist-dfej-solaris"
	@echo " examples examples-gries"

### Tags

tags: TAGS

## As of July 1998, my Linux etags works on Python; my Solaris one doesn't.
## So I should be sure to do the make on a Linux machine. -MDE
TAGS:  $(LISP_FILES) $(PYTHON_FILES)
	cd Daikon; make tags
	etags $(LISP_FILES) $(PYTHON_FILES) --include=Daikon/TAGS

###########################################################################
### Distribution
###

## The update-link-dates script appears in ~mernst/bin/share/.

# Main distribution

dist: $(DIST_DIR)/daikon.tar.gz

# Is this the right way to do this?
dist-force:
	rm -f daikon.tar.gz
	make dist

$(DIST_DIR)/daikon.tar.gz: daikon.tar.gz
	# This isn't quite right:  I want the copy of daikon.html in daikon.tar.gz.
	rm -rf $(DIST_DIR)/daikon.tar.gz $(DIST_DIR)/daikon.html
	cp -pf daikon.tar.gz daikon.html $(DIST_DIR)
	# Don't edit the copy of daikon.html in the distribution directory
	chmod ogu-w $(DIST_DIR)/daikon.tar.gz $(DIST_DIR)/daikon.html
	update-link-dates $(DIST_DIR)/index.html

daikon.tar: $(LISP_FILES) $(PYTHON_FILES) $(DOC_FILES) $(EDG_FILES) README-dist
	mkdir daikon
	cp -p $(PYTHON_FILES) $(DOC_FILES) daikon
	cp -p README-dist daikon/README

	# Lisp instrumenter
	mkdir daikon/lisp-front-end
	cp -p $(LISP_FILES) daikon/lisp-front-end

	# C/C++ instrumenter
	mkdir daikon/c-front-end
	cp -p $(EDG_FILES) daikon/c-front-end
	cp -p $(EDG_DIR)/Makefile daikon/c-front-end/Makefile-sample
	echo "0" > daikon/c-front-end/label.txt
	# Fix permission problems (does this fully do the trick?)
	chmod +rw daikon/c-front-end/*

	# Java instrumenter
	# The -h option saves symbolic links as real files, to avoid problem 
	# with the fact that I've made dfej into a symbolic link.
	(cd $(DFEJ_DIR)/..; tar chf /tmp/dfej.tar dfej)
	(cd daikon; tar xf /tmp/dfej.tar; mv dfej java-front-end; rm /tmp/dfej.tar)
	# delete src/Makefile, which is created by "./configure".
	(cd daikon/java-front-end; rm -f src/Makefile; rm -rf `find . \( -name UNUSED -o -name CVS -o -name SCCS -o -name RCS -o -name jikes -o -name dfej -o -name '*.o' -o -name '*~' -o -name '.cvsignore' -o -name '*.orig' -o -name 'config.log' \) -print`)

	date > daikon/VERSION
	chgrp -R invariants daikon
	rm -rf daikon.tar
	tar cf daikon.tar daikon

	## Better yet, just blow it away.
	rm -rf daikon
	# # After making the tar file, don't edit the (historical) distribution
	# chmod -R uog-w daikon/*

	# Don't bother making a "dist-*" backup directory.
	# if (test -d dist-`date +'%y%m%d'`); then rm -rf dist-`date +'%y%m%d'`; fi
	# mv daikon dist-`date +'%y%m%d'`
	# rm dist
	# ln -s dist-`date +'%y%m%d'` dist

daikon.tar.gz: daikon.tar
	rm -rf daikon.tar.gz
	gzip -c daikon.tar > daikon.tar.gz

### Front end binaries

## C/C++ front end

dist-edg: dist-edg-solaris

dist-edg-solaris: $(DIST_DIR_2)/edgcpfe-solaris

$(DIST_DIR_2)/edgcpfe-solaris: $(EDG_DIR)/edgcpfe
	cp -pf $< $@
	update-link-dates $(DIST_DIR)/index.html

# This is an attempt to indicate that it is not rebuilt from dfec.sh.
# I seem to have to have a body in the rule.
$(EDG_DIR)/dfec: $(EDG_DIR)/dfec.sh
	@echo

## Java front end

dist-dfej: dist-dfej-solaris

dist-dfej-solaris: $(DIST_DIR_2)/dfej-solaris

$(DIST_DIR_2)/dfej-solaris: $(DFEJ_DIR)/src/dfej
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
