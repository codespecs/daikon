###########################################################################
### Variables
###

# Should gries-instrumented be in this list?
LISP_FILES := gries-helper.lisp instrument.lisp data-trace.lisp \
	load-all.lisp \
	gries.lisp gries-instrumented.lisp inv-medic.lisp
LISP_PATHS := $(addprefix lisp-front-end/,$(LISP_FILES))
PYTHON_FILES := daikon.py util.py TextFile.py
DOC_FILES := dtrace-format.txt Makefile daikon.html daikon.gif
PY_DOC_FILES := daikon.py.doc Makefile TextFile.README daikon.gif
README_FILES := README-daikon-java README-daikon1 README-dist
SCRIPT_FILES := modbit-munge.pl java-cpp lines-from
SCRIPT_PATHS := $(addprefix scripts/,$(SCRIPT_FILES))

# EDG_DIR := /homes/gws/mernst/research/invariants/edg/dist
EDG_DIR := /homes/gws/mernst/research/invariants/c-front-end
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
TAGS:  $(LISP_PATHS) $(PYTHON_FILES)
	cd daikon; $(MAKE) tags
	etags $(LISP_PATHS) $(PYTHON_FILES) --include=daikon/TAGS

###########################################################################
### Distribution
###

## The update-link-dates script appears in ~mernst/bin/share/.

# Main distribution

dist: $(DIST_DIR)/daikon.tar.gz
	$(MAKE) -n dist-dfej

# Is this the right way to do this?
dist-force:
	rm -f daikon.tar.gz
	$(MAKE) dist

$(DIST_DIR)/daikon.tar.gz: daikon.tar.gz
	# This isn't quite right:  I want the copy of daikon.html in daikon.tar.gz.
	rm -rf $(DIST_DIR)/daikon.tar.gz $(DIST_DIR)/daikon.html
	cp -pf daikon.tar.gz daikon.html $(DIST_DIR)
	# Don't edit the copy of daikon.html in the distribution directory
	chmod ogu-w $(DIST_DIR)/daikon.tar.gz $(DIST_DIR)/daikon.html
	update-link-dates $(DIST_DIR)/index.html

daikon.tar: $(LISP_PATHS) $(PYTHON_FILES) $(DOC_FILES) $(PY_DOC_FILES) $(EDG_FILES) $(README_files) examples-gries.tar.gz
	mkdir /tmp/daikon

	# Old Python implementation
	mkdir /tmp/daikon/daikon-python
	cp -p $(PYTHON_FILES) $(PY_DOC_FILES) /tmp/daikon/daikon-python
	cp -p README-daikon1 /tmp/daikon/daikon-python/README
	cp -p daikon-19991114.html /tmp/daikon/daikon-python/daikon.html

	# Current Java implementation
	cp -p $(DOC_FILES) /tmp/daikon
	cp -p README-dist /tmp/daikon/README
	tar chf /tmp/daikon-java.tar daikon
	(mkdir /tmp/daikon/java; cd /tmp/daikon/java; tar xf /tmp/daikon-java.tar; rm /tmp/daikon-java.tar)
	cp -p README-daikon-java /tmp/daikon/java/README
	# Maybe I should do  $(MAKE) doc  
	# Maybe I should do  $(MAKE) clean  which will also get rid of .class
	(cd /tmp/daikon/java; rm -rf `find . \( -name UNUSED -o -name CVS -o -name SCCS -o -name RCS -o -name '*~' -o -name '.cvsignore' -o -name '*.orig' -o -name 'config.log' -o -name '*.java-*' -o -name '*to-do' -o -name 'TAGS' \) -print`)

	# Java support files
	(cp -p java-getopt-1.0.7.tar.gz /tmp/daikon/java; cd /tmp/daikon/java; tar zxf java-getopt-1.0.7.tar.gz; rm java-getopt-1.0.7.tar.gz)
	(cd $(HOME)/java/utilMDE; $(MAKE) utilMDE.tar.gz; cd /tmp/daikon/java; tar zxf $(HOME)/java/utilMDE/utilMDE.tar.gz)
	(cp -p OROMatcher-1.1.tar.gz /tmp/daikon/java; cd /tmp/daikon/java; tar zxf OROMatcher-1.1.tar.gz; rm OROMatcher-1.1.tar.gz; ln -s OROMatcher-1.1.0a/com .)

	# Auxiliary programs
	mkdir /tmp/daikon/bin
	cp -p $(SCRIPT_PATHS) /tmp/daikon/bin

	# Lisp instrumenter
	mkdir /tmp/daikon/lisp-front-end
	cp -p $(LISP_PATHS) /tmp/daikon/lisp-front-end

	# C/C++ instrumenter
	mkdir /tmp/daikon/c-front-end
	cp -p $(EDG_FILES) /tmp/daikon/c-front-end
	cp -p $(EDG_DIR)/Makefile /tmp/daikon/c-front-end/Makefile-sample
	echo "0" > /tmp/daikon/c-front-end/label.txt
	# Fix permission problems (does this fully do the trick?)
	chmod +rw /tmp/daikon/c-front-end/*

	# Java instrumenter
	# The -h option saves symbolic links as real files, to avoid problem 
	# with the fact that I've made dfej into a symbolic link.
	(cd $(DFEJ_DIR)/..; tar chf /tmp/dfej.tar dfej)
	(cd /tmp/daikon; tar xf /tmp/dfej.tar; mv dfej java-front-end; rm /tmp/dfej.tar)
	# the subsequence rm -rf shouldn't be necessary one day, 
	# but for the time being (and just in case)...
	(cd /tmp/daikon/java-front-end; (cd src; $(MAKE) distclean); rm -rf `find . \( -name UNUSED -o -name CVS -o -name SCCS -o -name RCS -o -name jikes -o -name dfej -o -name '*.o' -o -name '*~' -o -name '.cvsignore' -o -name '*.orig' -o -name 'config.log' \) -print`)

	# Example files
	mkdir /tmp/daikon/examples
	(cp -p examples-gries.tar.gz /tmp/daikon/examples; cd /tmp/daikon/examples; tar zxf examples-gries.tar.gz; mv examples-gries gries; rm examples-gries.tar.gz)

	date > /tmp/daikon/VERSION
	chgrp -R invariants /tmp/daikon
	(cd /tmp; tar cf daikon.tar daikon)
	cp -pf /tmp/daikon.tar .

	## Better than the below dist-* directory, just blow it away.
	rm -rf /tmp/daikon /tmp/daikon.tar

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

$(DIST_DIR_2)/dfej-solaris: $(DFEJ_DIR)/src/dfej-solaris
	cp -pf $< $@
	update-link-dates $(DIST_DIR)/index.html
	cat /dev/null | mail -s "make dist-dfej   has been run" kataoka@cs.washington.edu mernst@cs.washington.edu

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
GRIES_DIR := lisp-front-end
GRIES_FILE_PATHS := $(addprefix $(GRIES_DIR)/,$(GRIES_FILES))

# Don't bother with this any longer; it's so small that I might as well
# just include it in the distribution directly.
# 
# examples-gries: $(DIST_DIR)/examples-gries.tar.gz
# 
# $(DIST_DIR)/examples-gries.tar.gz: examples-gries.tar.gz
# 	cp -pf $< $@
# 	update-link-dates $(DIST_DIR)/index.html

examples-gries.tar.gz: $(GRIES_FILE_PATHS) $(GRIES_DIR)/README-examples-gries
	mkdir examples-gries
	cp -pf $(GRIES_FILE_PATHS) examples-gries
	cp -pf $(GRIES_DIR)/README-examples-gries examples-gries/README
	tar czf examples-gries.tar.gz examples-gries
	rm -rf examples-gries
