##########################################################################
### Variables
###

IMAGE_FILES := daikon-logo.gif daikon-logo.png daikon-logo.eps gui-ControlPanel.jpg gui-ControlPanel.eps gui-InvariantsDisplay-small.jpg gui-InvariantsDisplay-small.eps
IMAGE_PARTIAL_PATHS := $(addprefix images/,$(IMAGE_FILES))
DOC_FILES_NO_IMAGES := Makefile daikon.texinfo daikon.ps daikon.pdf daikon.html
DOC_FILES := ${DOC_FILES_NO_IMAGES} $(IMAGE_PARTIAL_PATHS)
DOC_PATHS := $(addprefix doc/,$(DOC_FILES))
README_FILES := README-daikon-java README-dist
README_PATHS := $(addprefix doc/,$(README_FILES))
SCRIPT_FILES := modbit-munge.pl modbit-munge.bat java-cpp.pl daikon.pl lines-from
SCRIPT_PATHS := $(addprefix scripts/,$(SCRIPT_FILES))
DAIKON_JAVA_FILES := $(shell find java \( -name '*daikon-java*' -o -name '*-cpp.java' -o -name CVS -o -name 'ReturnBytecodes.java' -o -name 'AjaxDecls.java' \) -prune -o -name '*.java' -print)
WWW_FILES := $(shell cd doc/www; find . \( -name '*~' -o -name CVS \) -prune -o -type f -print)
WWW_DIR := /home/httpd/html/daikon/

MERNST_DIR := /g2/users/mernst
# This is the current directory!  Maybe I don't need a variable for it.
INV_DIR := $(MERNST_DIR)/research/invariants

DFEJ_DIR := $(INV_DIR)/dfej
DFEC_DIR := $(INV_DIR)/dfec
C_RUNTIME_PATHS := front-end/c/daikon_runtime.h front-end/c/daikon_runtime.c
# Old C front end
# EDG_DIR := $(INV_DIR)/edg/dist
# EDG_DIR := $(INV_DIR)/c-front-end
# $(EDG_DIR)/edgcpfe is distributed separately (not in the main tar file)
# EDG_FILES := $(EDG_DIR)/dump_trace.h $(EDG_DIR)/dump_trace.c $(EDG_DIR)/dfec $(EDG_DIR)/dfec.sh

# DIST_DIR := $(MERNST_DIR)/www/daikon/dist
DIST_DIR := /home/httpd/html/daikon/dist
DIST_BIN_DIR := $(DIST_DIR)/binaries
# Files that appear in the top level of the distribution directory
DIST_DIR_FILES := daikon-source.tar.gz daikon-jar.tar.gz daikon-logo.gif daikon.jar
DIST_DIR_PATHS := daikon-source.tar.gz daikon-jar.tar.gz doc/images/daikon-logo.gif daikon.jar
# Location for NFS-mounted binaries
NFS_BIN_DIR := /g2/users/mernst/research/invariants/binaries

# For really big files
# DIST_DIR_2 := /projects/se/people/mernst/www
DIST_DIR_2 := $(DIST_DIR)

CVS_REP := /g4/projects/invariants/.CVS/
RTJAR := /g2/users/mernst/java/jdk/jre/lib/rt.jar

# for "chgrp"
INV_GROUP := invariants

RM_TEMP_FILES := rm -rf `find . \( -name UNUSED -o -name CVS -o -name SCCS -o -name RCS -o -name '*.o' -o -name '*~' -o -name '.*~' -o -name '.cvsignore' -o -name '*.orig' -o -name 'config.log' -o -name '*.java-*' -o -name '*to-do' -o -name 'TAGS' -o -name '.\#*' -o -name '.deps' -o -name jikes -o -name dfej -o -name dfej-linux -o -name dfej-linux-x86 -o -name daikon-java -o -name daikon-output -o -name core -o -name '*.bak' -o -name '*.rej' -o -name '*.old' -o -name '.nfs*' -o -name '\#*\#' \) -print`


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
	cd java && $(MAKE) tags


###########################################################################
### Test the distribution
###

DISTTESTDIR := /tmp/daikon.dist

# Both make and test the distribution.
# (Must make it first in order to test it!)
dist-test: dist-notest dist-test-no-update-dist

# A very simple test:  just verify that the distributed system compiles.
dist-test-no-update-dist: dist-ensure-directory-exists
	-rm -rf $(DISTTESTDIR)
	mkdir $(DISTTESTDIR)
	(cd $(DISTTESTDIR); tar xzf $(DIST_DIR)/daikon-source.tar.gz)
	(cd $(DISTTESTDIR)/daikon/java/daikon; CLASSPATH=$(DISTTESTDIR)/daikon/java:$(RTJAR); rm `find . -name '*.class'`; make)
	(cd $(DISTTESTDIR)/daikon/java && $(MAKE) junit)

# I would rather define this inside the cvs-test rule.  (In that case I
# must use "$$FOO", not $(FOO), to refer to it.)
TESTCVS=/scratch/$(USER)/daikon.cvs
TESTCVSJAVA=$(TESTCVS)/invariants/java

cvs-test:
	-rm -rf $(TESTCVS)
	mkdir -p $(TESTCVS)
	cd $(TESTCVS) && cvs -Q -d $(CVS_REP) co invariants
	cd $(TESTCVSJAVA)/daikon && make CLASSPATH=$(TESTCVSJAVA):$(TESTCVSJAVA)/lib/jakarta-oro.jar:$(TESTCVSJAVA)/lib/java-getopt.jar:$(TESTCVSJAVA)/lib/junit.jar:.:$(RTJAR)


###########################################################################
### Distribution
###

## The update-link-dates script appears in ~mernst/bin/share/.

# Main distribution

dist: dist-test

dist-ensure-directory-exists: $(DIST_DIR)

dist-notest: dist-ensure-directory-exists $(DIST_DIR_PATHS)
	$(MAKE) update-dist-dir 
	$(MAKE) -n dist-dfej

# Is this the right way to do this?
dist-force:
	-rm -f daikon-source.tar.gz daikon-jar.tar.gz
	$(MAKE) dist

update-dist-dir: dist-ensure-directory-exists
	# Would be clever to call "cvs examine" and warn if not up-to-date.
	cd java/daikon && $(MAKE) junit
	cd doc && $(MAKE) html html-chap
	# html-update-toc daikon.html
	-cd $(DIST_DIR) && rm -rf $(DIST_DIR_FILES) doc daikon_manual_html
	cp -pf $(DIST_DIR_PATHS) $(DIST_DIR)
	# This isn't quite right:  $(DIST_DIR) should hold the
	# daikon.html from daikon-source.tar.gz, not the current version.
	mkdir $(DIST_DIR)/doc
	cd doc && cp -pf $(DOC_FILES_NO_IMAGES) $(DIST_DIR)/doc
	cp -pR doc/images $(DIST_DIR)/doc
	cp -pR doc/daikon_manual_html $(DIST_DIR)/doc
	# Don't modify files in the distribution directory
	cd $(DIST_DIR) && chmod -R ogu-w $(DIST_DIR_FILES)

	update-link-dates $(DIST_DIR)/index.html

www:
	cd doc/www && cp -Ppf $(WWW_FILES) $(WWW_DIR)
	cd $(WWW_DIR) && chmod -w $(WWW_FILES)

.PHONY: www

daikon.jar: $(DAIKON_JAVA_FILES)
	-rm -rf daikon.jar /tmp/daikon-jar
	mkdir /tmp/daikon-jar
	cd java/daikon && $(MAKE) JAVAC='javac -g -d /tmp/daikon-jar' all
	cd java/utilMDE && $(MAKE) JAVAC='javac -g -d /tmp/daikon-jar' all
	## Old untarring code:
	#  tar xzf java/lib/java-getopt-1.0.8.tar.gz -C /tmp/daikon-jar
	#  tar xzf java/lib/OROMatcher-1.1.tar.gz -C /tmp/daikon-jar
	#  mv /tmp/daikon-jar/OROMatcher-1.1.0a/com /tmp/daikon-jar
	#  rm -rf /tmp/daikon-jar/OROMatcher-1.1.0a
	# jar does not seem to accept the -C argument.  MDE 6/14/2001
	# jar xf java/lib/jakarta-oro.jar -C /tmp/daikon-jar
	# jar xf java/lib/java-getopt.jar -C /tmp/daikon-jar
	# jar xf java/lib/junit.jar -C /tmp/daikon-jar
	(cd /tmp/daikon-jar; jar xf $(INV_DIR)/java/lib/jakarta-oro.jar)
	(cd /tmp/daikon-jar; jar xf $(INV_DIR)/java/lib/java-getopt.jar)
	(cd /tmp/daikon-jar; jar xf $(INV_DIR)/java/lib/junit.jar)
	cd /tmp/daikon-jar && jar cf $@ *
	mv /tmp/daikon-jar/$@ $@
	rm -rf /tmp/daikon-jar

# Use this ordering because daikon-jar is made before daikon-source

daikon-jar.tar daikon-source.tar: $(DOC_PATHS) $(EDG_FILES) $(README_PATHS) $(DAIKON_JAVA_FILES) daikon.jar
	# html-update-toc daikon.html

	-rm -rf /tmp/daikon
	mkdir /tmp/daikon

	mkdir /tmp/daikon/doc
	cp -p doc/README-dist /tmp/daikon/README
	cd doc && cp -p $(DOC_FILES_NO_IMAGES) /tmp/daikon/doc
	mkdir /tmp/daikon/doc/images
	cd doc && cp -p $(IMAGE_PARTIAL_PATHS) /tmp/daikon/doc/images
	cp -pR doc/daikon_manual_html /tmp/daikon/doc

	# Auxiliary programs
	mkdir /tmp/daikon/bin
	cp -p $(SCRIPT_PATHS) /tmp/daikon/bin

	## Front ends
	mkdir /tmp/daikon/front-end

	# C/C++ instrumenter
	mkdir /tmp/daikon/front-end/c
	cp -p $(C_RUNTIME_PATHS) /tmp/daikon/front-end/c

	# Example files
	cp -pR examples /tmp/daikon
	cd /tmp/daikon && find examples \( -name '*.java' \) -prune -o -type f -o -name CVS -print | xargs rm -rf

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
	cp -p doc/README-daikon-java /tmp/daikon/java/README
	# Maybe I should do  $(MAKE) doc
	# Don't do  $(MAKE) clean  which deletes .class files
	(cd /tmp/daikon/java; $(RM_TEMP_FILES))

	# Java support files
	(cd java/utilMDE; $(MAKE) utilMDE.tar.gz)
	cd java && tar zxf utilMDE/utilMDE.tar.gz -C /tmp/daikon/java
	tar zxf java/lib/java-getopt-1.0.8.tar.gz -C /tmp/daikon/java
	# tar zxf java/lib/OROMatcher-1.1.tar.gz -C /tmp/daikon/java
	# (cd /tmp/daikon/java; ln -s OROMatcher-1.1.0a/com .)
	tar zxf java/lib/jakarta-oro-2.0.3.tar.gz -C /tmp/daikon/java
	(cd /tmp/daikon/java; ln -s jakarta-oro-2.0.3/src/java/org .)
	unzip java/lib/junit3.7.zip -d /tmp/daikon/java
	(cd /tmp/daikon/java; ln -s junit3.7/junit .)

	# Java instrumenter
	# The -h option saves symbolic links as real files, to avoid problem 
	# with the fact that I've made dfej into a symbolic link.
	(cd $(DFEJ_DIR)/..; tar chf /tmp/dfej.tar --exclude '*.o' --exclude 'src/dfej' --exclude 'src.tar' dfej)
	# (cd /tmp/daikon; tar xf /tmp/dfej.tar; mv dfej front-end/java; rm /tmp/dfej.tar)
	# For debugging
	(cd /tmp/daikon; tar xf /tmp/dfej.tar; mv dfej front-end/java)
	# the subsequence rm -rf shouldn't be necessary one day, 
	# but for the time being (and just in case)...
	# (cd /tmp/daikon/java-front-end; $(MAKE) distclean; (cd src; $(MAKE) distclean); $(RM_TEMP_FILES))
	(cd /tmp/daikon/front-end/java; $(MAKE) distclean; $(RM_TEMP_FILES))

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
	cp -pf $(DFEC_DIR)/bin/dfec-static $(DIST_BIN_DIR)/dfec-linux-x86
	cp -pf $(DFEC_DIR)/src/dfec $(DIST_BIN_DIR)/dfec-linux-x86-dynamic
	update-link-dates $(DIST_DIR)/index.html
	cp -pf $(DFEC_DIR)/src/dfec $(NFS_BIN_DIR)


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

$(DFEJ_DIR)/src/dfej:
	cd $(DFEJ_DIR) && $(MAKE)

## Don't distribute executables for now
dist-dfej: dist-dfej-linux-x86 dist-dfej-windows

dist-dfej-solaris: $(DIST_BIN_DIR)/dfej-solaris

$(DIST_BIN_DIR)/dfej-solaris: $(DFEJ_DIR)/src/dfej-solaris
	cp -pf $< $@
	update-link-dates $(DIST_DIR)/index.html
	# cat /dev/null | mail -s "make dist-dfej   has been run" kataoka@cs.washington.edu mernst@lcs.mit.edu

dist-dfej-linux-x86: $(DFEJ_DIR)/src/dfej
	# First remake
	-mv -f $(DFEJ_DIR)/src/dfej $(DFEJ_DIR)/src/dfej-dynamic
	-mv -f $(DFEJ_DIR)/src/dfej-linux-x86 $(DFEJ_DIR)/src/dfej
	cd $(DFEJ_DIR)/src && $(MAKE) LDFLAGS=-static
	mv -f $(DFEJ_DIR)/src/dfej $(DFEJ_DIR)/src/dfej-linux-x86
	mv -f $(DFEJ_DIR)/src/dfej-dynamic $(DFEJ_DIR)/src/dfej

	# Now copy it over
	cp -pf $(DFEJ_DIR)/src/dfej-linux-x86 $(DIST_BIN_DIR)/dfej-linux-x86
	cp -pf $(DFEJ_DIR)/src/dfej $(DIST_BIN_DIR)/dfej-linux-x86-dynamic
	update-link-dates $(DIST_DIR)/index.html
	cp -pf $(DFEJ_DIR)/src/dfej $(NFS_BIN_DIR)
	# cat /dev/null | mail -s "make dist-dfej   has been run" kataoka@cs.washington.edu mernst@lcs.mit.edu

# To create build_mingw_dfej, I did:
# 	cd dfej && $(MAKE) distclean
# 	mkdir build_mingw_dfej
# 	(setenv PATH /g2/users/mernst/bin/src/mingw32-linux-x86-glibc-2.1/cross-tools/bin:$PATH; cd build_mingw_dfej; ~mernst/research/invariants/dfej/configure --prefix=/tmp/dfej_Xmingw --host=i386-mingw32msvc)
#	cd dfej && ./configure
# Path must include /g2/users/mernst/bin/src/mingw32-linux-x86-glibc-2.1/cross-tools/bin


# dfej-src/build_mingw_dfej/src/dfej.exe:
# 	cd dfej-src/build_mingw_dfej; setenv PATH /g2/users/mernst/bin/src/mingw32-linux-x86-glibc-2.1/cross-tools/bin:$(PATH); $(MAKE)

mingw_exe: dfej-src/build_mingw_dfej/src/dfej.exe

## Problem:  I seem to need to move away the .o files in the source
## directory.  If they exist, then no attempt is made to build locally.
## So as a hack, move them aside and then replace them.

dfej-src/build_mingw_dfej/src/dfej.exe: dfej-src/dfej/src/*.cpp dfej-src/dfej/src/*.h
	rename .o .mingw-saved.o dfej-src/dfej/src/*.o
	(cd dfej-src/build_mingw_dfej; export PATH=/g2/users/mernst/bin/src/mingw32-linux-x86-glibc-2.1/cross-tools/bin:${PATH}; $(MAKE))
	rename .mingw-saved.o .o dfej-src/dfej/src/*.mingw-saved.o

dist-dfej-windows: dfej-src/build_mingw_dfej/src/dfej.exe
	cp -p dfej-src/build_mingw_dfej/src/dfej.exe $(DIST_BIN_DIR)/dfej.exe
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


###########################################################################
### Utilities
###

showvars:
	@echo "DAIKON_JAVA_FILES = " $(DAIKON_JAVA_FILES)
