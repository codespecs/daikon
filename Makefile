##########################################################################
### Variables
###

IMAGE_FILES := daikon-logo.gif daikon-logo.png daikon-logo.eps gui-ControlPanel.png gui-ControlPanel.eps gui-InvariantsDisplay-small.png gui-InvariantsDisplay-small.eps
IMAGE_PARTIAL_PATHS := $(addprefix images/,$(DOC_FILES))
DOC_FILES := dtrace-format.txt Makefile daikon.html $(IMAGE_PARTIAL_PATHS)
DOC_PATHS := $(addprefix doc/,$(DOC_FILES))
README_FILES := README-daikon-java README-dist
README_PATHS := $(addprefix doc/,$(README_FILES))
SCRIPT_FILES := modbit-munge.pl java-cpp.pl lines-from
SCRIPT_PATHS := $(addprefix scripts/,$(SCRIPT_FILES))
DAIKON_JAVA_FILES := $(shell find daikon \( -name '*daikon-java*' -o -name '*-cpp.java' -o -name CVS -o -name 'ReturnBytecodes.java' -o -name 'AjaxDecls.java' \) -prune -o -name '*.java' -print)

MERNST_DIR := /g2/users/mernst
# This is the current directory!  Maybe I don't need a variable for it.
INV_DIR := $(MERNST_DIR)/research/invariants

DFEJ_DIR := $(INV_DIR)/dfej
DFEC_DIR := $(INV_DIR)/dfec
# Old C front end
# EDG_DIR := $(INV_DIR)/edg/dist
# EDG_DIR := $(INV_DIR)/c-front-end
# $(EDG_DIR)/edgcpfe is distributed separately (not in the main tar file)
# EDG_FILES := $(EDG_DIR)/dump_trace.h $(EDG_DIR)/dump_trace.c $(EDG_DIR)/dfec $(EDG_DIR)/dfec.sh

DIST_DIR := $(MERNST_DIR)/www/daikon/dist
DIST_DIR_FILES := daikon-source.tar.gz daikon-jar.tar.gz daikon.html gui.html daikon.jar
# For really big files
# DIST_DIR_2 := /projects/se/people/mernst/www
DIST_DIR_2 := $(DIST_DIR)

CVS_REP := /g4/projects/invariants/.CVS/

# for "chgrp"
INV_GROUP := invariants

RM_TEMP_FILES := rm -rf `find . \( -name UNUSED -o -name CVS -o -name SCCS -o -name RCS -o -name '*.o' -o -name '*~' -o -name '.*~' -o -name '.cvsignore' -o -name '*.orig' -o -name 'config.log' -o -name '*.java-*' -o -name '*to-do' -o -name 'TAGS' -o -name '.\#*' -o -name '.deps' -o -name jikes -o -name dfej -o -name daikon-java -o -name daikon-output -o -name core -o -name '*.bak' -o -name '.nfs*' -o -name '\#*\#' \) -print`


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
	@echo " dist-dfej dist-dfej-solaris dist-dfej-linux"
	@echo " examples examples-gries"
	@echo " test"

test:
	cd tests && $(MAKE) all

### Tags

tags: TAGS

TAGS:
	cd daikon && $(MAKE) tags


###########################################################################
### Test the distribution
###

DISTTESTDIR := $(HOME)/tmp/daikon.dist

dist-test: dist-notest dist-test-no-update-dist

# A very simple test:  just verify that the distributed system compiles.
dist-test-no-update-dist:
	-rm -rf $(DISTTESTDIR)
	mkdir $(DISTTESTDIR)
	(cd $(DISTTESTDIR); tar xzf $(DIST_DIR)/daikon-source.tar.gz)
	(cd $(DISTTESTDIR)/daikon/java/daikon; CLASSPATH=$(DISTTESTDIR)/daikon/java:/g2/users/mernst/java/jdk/jre/lib/rt.jar; rm `find . -name '*.class'`; make)

cvs-test:
	-rm -rf $(HOME)/tmp/daikon.cvs
	mkdir $(HOME)/tmp/daikon.cvs
	# (cd $(HOME)/tmp/daikon.cvs; cvs -Q -d $(CVS_REP) co invariants; cvs -Q -d $(CVS_REP) co utilMDE)
	(cd $(HOME)/tmp/daikon.cvs; cvs -Q -d $(CVS_REP) co invariants)
	(cd $(HOME)/tmp/daikon.cvs/invariants/daikon; CLASSPATH=/g2/users/mernst/tmp/daikon.cvs:/g2/users/mernst/tmp/daikon.cvs/invariants:/g2/users/mernst/java/OROMatcher-1.1:/g2/users/mernst/java/getopt-1.0.8:.:/g2/users/mernst/java/jdk/jre/lib/rt.jar; make)


###########################################################################
### Distribution
###

## The update-link-dates script appears in ~mernst/bin/share/.

# Main distribution

dist: dist-test

dist-notest: $(DIST_DIR_FILES)
	$(MAKE) update-dist-dir 
	$(MAKE) -n dist-dfej

# Is this the right way to do this?
dist-force:
	-rm -f daikon-source.tar.gz daikon-jar.tar.gz
	$(MAKE) dist

update-dist-dir:
	html-update-toc daikon.html
	# This isn't quite right:  $(DIST_DIR) should hold the
	# daikon.html from daikon-source.tar.gz, not the current version.
	-cd $(DIST_DIR) && rm -rf $(DIST_DIR_FILES)
	cp -pf $(DIST_DIR_FILES) $(DIST_DIR)
	# Don't files in the distribution directory
	cd $(DIST_DIR) && chmod ogu-w $(DIST_DIR_FILES)
	update-link-dates $(DIST_DIR)/index.html

daikon.jar: $(DAIKON_JAVA_FILES)
	-rm -rf daikon.jar /tmp/daikon-jar
	mkdir /tmp/daikon-jar
	cd daikon && $(MAKE) JAVAC='javac -g -d /tmp/daikon-jar' all
	cd utilMDE && $(MAKE) JAVAC='javac -g -d /tmp/daikon-jar' all
	tar xzf java-getopt-1.0.8.tar.gz -C /tmp/daikon-jar
	tar xzf OROMatcher-1.1.tar.gz -C /tmp/daikon-jar
	mv /tmp/daikon-jar/OROMatcher-1.1.0a/com /tmp/daikon-jar
	rm -rf /tmp/daikon-jar/OROMatcher-1.1.0a
	cd /tmp/daikon-jar && jar cf $@ *
	mv /tmp/daikon-jar/$@ $@
	# rm -rf /tmp/daikon-jar

# Use this ordering because daikon-jar is made before daikon-source

daikon-jar.tar daikon-source.tar: $(DOC_PATHS) $(EDG_FILES) $(README_PATHS) $(DAIKON_JAVA_FILES) daikon.jar
	html-update-toc daikon.html

	-rm -rf /tmp/daikon
	mkdir /tmp/daikon

	cp -p $(DOC_FILES) /tmp/daikon
	cp -p README-dist /tmp/daikon/README

	# Auxiliary programs
	mkdir /tmp/daikon/bin
	cp -p $(SCRIPT_PATHS) /tmp/daikon/bin

	# C/C++ instrumenter
	mkdir /tmp/daikon/c-front-end
	cp -p $(EDG_FILES) /tmp/daikon/c-front-end
	cp -p $(EDG_DIR)/Makefile /tmp/daikon/c-front-end/Makefile-sample
	echo "0" > /tmp/daikon/c-front-end/label.txt
	# Fix permission problems (does this fully do the trick?)
	chmod +rw /tmp/daikon/c-front-end/*

	# Example files
	mkdir /tmp/daikon/examples
	(cp -p examples-gries.tar.gz /tmp/daikon/examples; cd /tmp/daikon/examples; tar zxf examples-gries.tar.gz; mv examples-gries gries; rm examples-gries.tar.gz)

	date > /tmp/daikon/VERSION
	chgrp -R $(INV_GROUP) /tmp/daikon

	# Now we are ready to make the daikon-jar distribution
	cp -p daikon.jar /tmp/daikon
	(cd /tmp; tar cf daikon-jar.tar daikon)
	cp -pf /tmp/daikon-jar.tar .

	## Now make the daikon-source distribution
	# First add some more files to the distribution

	# Daikon itself
	tar chf /tmp/daikon-java.tar --exclude daikon-java --exclude daikon-output daikon
	(mkdir /tmp/daikon/java; cd /tmp/daikon/java; tar xf /tmp/daikon-java.tar; rm /tmp/daikon-java.tar)
	cp -p README-daikon-java /tmp/daikon/java/README
	# Maybe I should do  $(MAKE) doc
	# Don't do  $(MAKE) clean  which deletes .class files
	(cd /tmp/daikon/java; $(RM_TEMP_FILES))

	# Java support files
	(cp -p java-getopt-1.0.7.tar.gz /tmp/daikon/java; cd /tmp/daikon/java; tar zxf java-getopt-1.0.7.tar.gz; rm java-getopt-1.0.7.tar.gz)
	(cd utilMDE; $(MAKE) utilMDE.tar.gz; cd /tmp/daikon/java; tar zxf utilMDE/utilMDE.tar.gz)
	(cp -p OROMatcher-1.1.tar.gz /tmp/daikon/java; cd /tmp/daikon/java; tar zxf OROMatcher-1.1.tar.gz; rm OROMatcher-1.1.tar.gz; ln -s OROMatcher-1.1.0a/com .)

	# Java instrumenter
	# The -h option saves symbolic links as real files, to avoid problem 
	# with the fact that I've made dfej into a symbolic link.
	(cd $(DFEJ_DIR)/..; tar chf /tmp/dfej.tar --exclude '*.o' --exclude 'src/dfej' --exclude 'src.tar' dfej)
	# (cd /tmp/daikon; tar xf /tmp/dfej.tar; mv dfej java-front-end; rm /tmp/dfej.tar)
	# For debugging
	(cd /tmp/daikon; tar xf /tmp/dfej.tar; mv dfej java-front-end)
	# the subsequence rm -rf shouldn't be necessary one day, 
	# but for the time being (and just in case)...
	# (cd /tmp/daikon/java-front-end; $(MAKE) distclean; (cd src; $(MAKE) distclean); $(RM_TEMP_FILES))
	(cd /tmp/daikon/java-front-end; $(MAKE) distclean; $(RM_TEMP_FILES))

	# Make the source distribution proper
	(cd /tmp; tar cf daikon-source.tar daikon)
	cp -pf /tmp/daikon-source.tar .

## This apparently does not work
# %.tar.gz : %.tar
# 	-rm -rf $@
# 	gzip -c $< > $@

daikon-source.tar.gz: daikon-source.tar
	-rm -rf $@
	gzip -c $< > $@

daikon-jar.tar.gz: daikon-jar.tar
	-rm -rf $@
	gzip -c $< > $@



### Front end binaries

## C/C++ front end

dist-dfec: dist-dfec-linux

dist-dfec-linux:
	cd $(DFEC_DIR) && $(MAKE) dfec-static
	cp -pf $(DFEC_DIR)/bin/dfec-static $(DIST_DIR)/dfec-linux
	cp -pf $(DFEC_DIR)/src/dfec $(DIST_DIR)/dfec-linux-dynamic

## Old version
# dist-edg: dist-edg-solaris
# 
# dist-edg-solaris: $(DIST_DIR_2)/edgcpfe-solaris
# 
# $(DIST_DIR_2)/edgcpfe-solaris: $(EDG_DIR)/edgcpfe
# 	cp -pf $< $@
# 	update-link-dates $(DIST_DIR)/index.html
# 
# # This is an attempt to indicate that it is not rebuilt from dfec.sh.
# # I seem to have to have a body in the rule.
# $(EDG_DIR)/dfec: $(EDG_DIR)/dfec.sh
# 	@echo

## Java front end


## Don't distribute executables for now
# dist-dfej: dist-dfej-solaris
dist-dfej: dist-dfej-linux dist-dfej-windows

dist-dfej-solaris: $(DIST_DIR)/dfej-solaris

$(DIST_DIR)/dfej-solaris: $(DFEJ_DIR)/src/dfej-solaris
	cp -pf $< $@
	update-link-dates $(DIST_DIR)/index.html
	cat /dev/null | mail -s "make dist-dfej   has been run" kataoka@cs.washington.edu mernst@lcs.mit.edu

dist-dfej-linux: $(DIST_DIR)/dfej-linux
	# First remake
	-mv -f $(DFEJ_DIR)/src/dfej $(DFEJ_DIR)/src/dfej-dynamic
	-mv -f $(DFEJ_DIR)/src/dfej-linux $(DFEJ_DIR)/src/dfej
	cd $(DFEJ_DIR)/src && $(MAKE) LDFLAGS=-static
	mv -f $(DFEJ_DIR)/src/dfej $(DFEJ_DIR)/src/dfej-linux
	mv -f $(DFEJ_DIR)/src/dfej-dynamic $(DFEJ_DIR)/src/dfej

	# Now copy it over
	cp -pf $(DFEJ_DIR)/src/dfej-linux $(DIST_DIR)/dfej-linux
	cp -pf $(DFEJ_DIR)/src/dfej $(DIST_DIR)/dfej-linux-dynamic
	update-link-dates $(DIST_DIR)/index.html
	# cat /dev/null | mail -s "make dist-dfej   has been run" kataoka@cs.washington.edu mernst@lcs.mit.edu

# To create build_mingw, I did:
# 	cd dfej && $(MAKE) distclean
# 	mkdir build_mingw_dfej
# 	setenv PATH /g2/users/mernst/bin/src/mingw32-linux-x86-glibc-2.1/cross-tools/bin:$PATH; cd build_mingw_dfej; ~mernst/research/invariants/dfej/configure --prefix=/tmp/dfej_Xmingw --host=i386-mingw32msvc
# Path must include /g2/users/mernst/bin/src/mingw32-linux-x86-glibc-2.1/cross-tools/bin


# dfej-src/build_mingw_dfej/src/dfej.exe:
# 	cd dfej-src/build_mingw_dfej; setenv PATH /g2/users/mernst/bin/src/mingw32-linux-x86-glibc-2.1/cross-tools/bin:$(PATH); $(MAKE)

mingw_exe: dfej-src/build_mingw_dfej/src/dfej.exe

## Problem:  I seem to need to move away the .o files in the source
## directory.  If they exist, then no attempt is made to build locally.
## So as a hack, move them aside and then replace them.

dfej-src/build_mingw_dfej/src/dfej.exe: dfej-src/dfej/src/*.cpp dfej-src/dfej/src/*.h
	rename .o .mingw-saved.o dfej-src/dfej/src/*.o
	cd dfej-src/build_mingw_dfej; setenv PATH /g2/users/mernst/bin/src/mingw32-linux-x86-glibc-2.1/cross-tools/bin:$(PATH); $(MAKE); 
	rename .mingw-saved.o .o dfej-src/dfej/src/*.mingw-saved.o

dist-dfej-windows: dfej-src/build_mingw_dfej/src/dfej.exe
	cp dfej-src/build_mingw_dfej/src/dfej.exe $(DIST_DIR)/dfej.exe
	update-link-dates $(DIST_DIR)/index.html

## Cross-compiling DFEJ to create a Windows executable (instructions by
## Michael Harder <mharder@MIT.EDU>):
# 1.  Get the mingw32 cross-compiler for Linux.  Details are avaliable at: 
# http://www.mingw.org/mingwfaq.shtml#faq-cross.  A pre-built version for 
# Linux is available at: 
# http://www.devolution.com/~slouken/SDL/Xmingw32/mingw32-linux-x86-glibc-2.1.tar.gz
# 
# 2.  Extract mingw32-linux-x86-glibc-2.1.tar.gz.  The cross compiler tools 
# are in cross-tools/bin.  The tools start with "i386-mingw32msvc-".  These 
# tools need to be in your path when you build the Windows binary (including
# when you configure the Windows binary).
# 
# 
# The following instructions are adapted from daikon/java-front-end/INSTALL
# 
# 3.  Make a separate directory to build the Windows executable.
# 
# mkdir build_mingw; cd build_mingw
# 
# 4.  Run the dfej configure script, with a target platform of 
# "i386-mingw32msvc".  Assume the dfej source is at ~/daikon/java-front-end.
# 
# ~/daikon/java-front-end/configure --prefix=/tmp/dfej_Xmingw 
# --host=i386-mingw32msvc
# 
# Add additional arguments to configure as desired.  I don't know what the 
# "--prefix" flag is for, but they use it in the jikes INSTALL instructions.
# 
# 5.  Run "make".  This should make the Windows binary at 
# build_mingw/src/dfej.exe.  Copy this file to a Windows machine, and run 
# it.  You should at least get the Daikon usage message.
