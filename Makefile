DAIKONDIR_DEFAULT := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))

# Put user-specific changes in your own Makefile.user file in this directory.
# Make will silently continue if Makefile.user does not exist.
-include Makefile.user

DAIKONDIR ?= ${DAIKONDIR_DEFAULT}

##########################################################################
### Variables
###

# Set the NONETWORK variable to avoid network operations.  Example:
#   make NONETWORK=true compile

# note that for right now, we are only copying the html and texinfo
# versions of the developer manual (though all other versions are built)
IMAGE_FILES := daikon-logo.gif daikon-logo.png daikon-logo.eps dfepl-flow.jpg
# old image files
# gui-ControlPanel.jpg gui-ControlPanel.eps gui-InvariantsDisplay-small.jpg gui-InvariantsDisplay-small.eps context-gui.jpg context-gui.eps
IMAGE_PARTIAL_PATHS := $(addprefix images/,$(IMAGE_FILES))
DOC_FILES_NO_IMAGES := Makefile index.html daikon.texinfo config-options.texinfo invariants-doc.texinfo daikon.ps daikon.pdf daikon.html developer.texinfo developer.html CHANGES
DOC_FILES := ${DOC_FILES_NO_IMAGES} $(IMAGE_PARTIAL_PATHS)
DOC_PATHS := $(addprefix doc/,$(DOC_FILES))
# The texinfo files are included so we can diff to see what has changed from
# release to release.  They are in the dist/doc directory, but not
# visible to the user
DOC_FILES_USER := daikon.ps daikon.pdf daikon.html developer.html CHANGES \
				  daikon.texinfo developer.texinfo config-options.texinfo \
				  invariants-doc.texinfo developer.pdf developer.ps
# EMACS_PATHS := emacs/daikon-context-gui.el
README_PATHS := README.txt README.html doc/README.txt java/README.txt
# Files that contain the (automatically updated) version number and date.
DIST_VERSION_FILES := ${README_PATHS} \
	doc/daikon.texinfo doc/developer.texinfo \
	doc/index.html doc/www/download/index.html
# Scripts, such as Perl programs.  Why not just include all of them?
# (Maybe to avoid problems with accidentally including things in the user's
# checkout that are not needed by most users, but why not include
# everything that's in repository?)
SCRIPT_FILES := Makefile \
	daikon.cshrc daikon.bashrc daikonenv.bat \
	dfepl dtrace-perl dtype-perl \
	kvasir-dtrace \
	convertcsv.pl \
	trace-untruncate trace-untruncate-fast.c trace-purge-fns.pl trace-purge-vars.pl \
	trace-add-nonces.pl \
	util_daikon.pm \
	runcluster.pl decls-add-cluster.pl extract_vars.pl dtrace-add-cluster.pl
PLUME_SCRIPT_FILES := java-cpp lines-from

## These are now in plume-lib:
# 	cygwin-runner java-cygwin \
# 	javac-xlint javac-xlint-prune \
# 	checkargs.pm
# 	sort-directory-order

SCRIPT_PATHS := $(addprefix scripts/,$(SCRIPT_FILES)) $(addprefix plume-lib/bin/,$(PLUME_SCRIPT_FILES))
# This is so troublesome that it isn't used except as a list of dependences for make commands
DAIKON_JAVA_FILES := $(shell find java \( -name '*daikon-java*' -o -name CVS  \) -prune -o -name '*.java' -print) $(shell find java/daikon -follow \( -name '*daikon-java*' -o -name CVS \) -prune -o -name '*.java' -print)
DAIKON_RESOURCE_FILES := daikon/config/example-settings.txt \
	daikon/simplify/daikon-background.txt \
	daikon/test/InvariantFormatTest.commands \
	daikon/test/SampleTester.commands \
	daikon/test/SampleTester.decls \
	daikon/test/SampleTesterGlobal.decls \
	daikon/test/SampleTester.test \
	daikon/test/varInfoNameTest.testEscForall \
	daikon/test/varInfoNameTest.testEscForall.goal \
	daikon/test/varInfoNameTest.testJML \
	daikon/test/varInfoNameTest.testJML.goal \
	daikon/test/varInfoNameTest.testParse \
	daikon/test/varInfoNameTest.testParse.goal \
	daikon/test/varInfoNameTest.testSubscript \
	daikon/test/varInfoNameTest.testSubscript.goal \
	daikon/test/galar.asm \
	daikon/test/GenericTestClass.java \
	daikon/test/dtracediff/AllTypes.dtrace.gz \
	daikon/test/dtracediff/Hanoi-badvar.dtrace.gz \
	daikon/test/dtracediff/Hanoi-mungpointers.dtrace.gz \
	daikon/test/dtracediff/Hanoi-badvalue.dtrace.gz \
	daikon/test/dtracediff/Hanoi.dtrace.gz \
	daikon/test/dtracediff/Hanoi-truncated.dtrace.gz

# Find might be cleaner, but this works.
# I don't know why, but a "-o name ." clause makes find err, so use grep instead
# WWW_FILES := $(shell cd doc/www; find . \( -name '*~' -o -name '.*~' -o -name CVS -o -name .cvsignore -o -name '.\#*' -o -name '*.bak' -o -name uw -o name . -o name .. \) -prune -o -print)
# WWW_FILES := $(shell cd doc/www; find . \( \( -name '*~' -o -name '.*~' -o -name CVS -o -name .cvsignore -o -name '.\#*' -o -name '*.bak' -o -name uw \) -prune -a -type f \) -o -print | grep -v '^.$$')
WWW_FILES := $(shell cd doc/www; find . -type f -print | egrep -v '~$$|CVS|.cvsignore|/.\#|.bak$$|uw/|pubs/')
#WWW_DIR := /home/httpd/html/daikon/
WWW_PARENT ?= /afs/csail.mit.edu/group/pag/docroot/www.pag.csail.mit.edu
WWW_DIR := $(WWW_PARENT)/daikon
INV_DIR := $(shell pwd)
# Staging area for the distribution
STAGING_DIR := $(WWW_DIR)/staging-daikon

# Files to copy to the website
WWW_DAIKON_FILES := faq.html index.html mailing-lists.html StackAr.html \
                    download/index.html download/doc/index.html


C_RUNTIME_PATHS := front-end/c/daikon_runtime.h front-end/c/daikon_runtime.cc

BCEL_DIR := $(DAIKONDIR)/java/lib/bcel.jar
DIST_DIR := $(WWW_DIR)/dist
MIT_DIR  := $(WWW_DIR)/mit
DIST_BIN_DIR := $(DIST_DIR)/binaries
DIST_PAG_BIN_DIR := /afs/csail/group/pag/projects/invariants/binaries
# Files that appear in the top level of the distribution directory
DIST_DIR_FILES := daikon.tar.gz daikon-logo.gif daikon.jar
DIST_DIR_PATHS := daikon.tar.gz daikon.zip doc/images/daikon-logo.gif daikon.jar

# Perhaps replace with `cat CVS/Root`, for remote CVS users?
CVS_REPOSITORY := /afs/csail.mit.edu/group/pag/projects/invariants/.CVS

# It seems like these should come from their standard locations (jhp)
RTJAR := $(JAVA_HOME)/jre/lib/rt.jar
TOOLSJAR := $(JAVA_HOME)/lib/tools.jar

RSYNC_AR := rsync -aR

JAVAC ?= javac -target 5

# A good alternative for Makefile.user is: hg fetch
# When disconnected from network, change this to a no-op, or (better) just
# set NONETWORK to true.
HG_PULL_U ?= hg pull -u
# Example Makefile.user line, on cygwin: HG_OPTIONS=--insecure
HG_OPTIONS ?=

JUNIT_VERSION := junit3.8.1

# for "chgrp, we need to use the number on debian, 14127 is invariants"
INV_GROUP := 14127

RM_TEMP_FILES := rm -rf `find . \( -name UNUSED -o -name CVS -o -name SCCS -o -name RCS -o -name '*.o' -o -name '*~' -o -name '.*~' -o -name '.cvsignore' -o -name '*.orig' -o -name 'config.log' -o -name '*.java-*' -o -name '*to-do' -o -name 'TAGS' -o -name '.\#*' -o -name '.deps' -o -name jikes -o -name daikon-java -o -name daikon-output -o -name core -o -name '*.bak' -o -name '*.rej' -o -name '*.old' -o -name '.nfs*' -o -name '\#*\#' \) -print`

TMPDIR ?= /scratch/${USER}


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
	@echo " compile compile-java     -- compile Java files"
	@echo " junit                    -- run unit tests"
	@echo " test                     -- run system tests"
	@echo " tags TAGS                -- make TAGS file for Emacs"
	@echo " pag-install              -- install PAG specific files "
	@echo " kvasir                   -- make Kvasir, the C front end"
	@echo "Creating the Daikon distribution:"
	@echo " daikon.tar daikon.jar    -- just makes the tar files"
	@echo " staging                  -- moves all release file to staging-dist/"
	@echo " test-staged-dist         -- tests the distribution in staging-dist/"
	@echo " staging-to-www           -- copies staging-dist/ to website"
	@echo " "
	@echo "This Makefile is for manipulating the entire Daikon system."
	@echo "Daikon source code is in the java/daikon subdirectory (but you"
	@echo "can perform basic operations like compiling it from here)."

### Compiling the code

compile: compile-java

compile-java:
	cd java && $(MAKE) all

clean-java:
	cd java && $(MAKE) clean

javadoc:
	cd java && $(MAKE) javadoc

### Kvasir (C/C++ front end)

ifeq ($(shell uname -m),x86_64)
VALGRIND_ARCH := amd64
else
VALGRIND_ARCH := x86
endif

../fjalar/auto-everything.sh:
	cd .. && hg clone ${HG_OPTIONS} https://code.google.com/p/fjalar/ fjalar
	touch $@

kvasir/fjalar/Makefile.in: ../fjalar/auto-everything.sh
	ln -nsf ../fjalar/valgrind kvasir
	touch $@

kvasir/configure: ../fjalar/auto-everything.sh
	cd ../fjalar && ./auto-everything.sh

kvasir/config.status: kvasir/fjalar/Makefile.in ../fjalar/valgrind/VEX/pub/libvex.h kvasir/configure
	cd kvasir && ./configure --prefix=`pwd`/inst

kvasir/coregrind/valgrind: kvasir/configure kvasir/config.status $(wildcard kvasir/coregrind/*.[ch])
	cd kvasir && $(MAKE) --no-print-directory

kvasir/fjalar/fjalar-$(VALGRIND_ARCH)-linux: kvasir/coregrind/valgrind $(wildcard kvasir/fjalar/*.[ch]) $(wildcard kvasir/fjalar/kvasir/*.[ch])
	cd kvasir/fjalar && $(MAKE) --no-print-directory

kvasir/inst/bin/valgrind: kvasir/coregrind/valgrind
	cd kvasir && $(MAKE) install >/dev/null

kvasir/inst/lib/valgrind/fjalar-$(VALGRIND_ARCH)-linux: kvasir/fjalar/fjalar-$(VALGRIND_ARCH)-linux
	cd kvasir/fjalar && $(MAKE) install >/dev/null

kvasir: kvasir/inst/lib/valgrind/fjalar-$(VALGRIND_ARCH)-linux kvasir/inst/bin/valgrind

build-kvasir: kvasir

### Rebuild everything; used for monthly releases, for example

rebuild-everything:
	${MAKE} -C $(DAIKONDIR)/java very-clean
	${MAKE} -C $(DAIKONDIR)/java tags compile
	${MAKE} -C $(DAIKONDIR) daikon.jar
	${MAKE} -C $(DAIKONDIR)/java dcomp_rt.jar
	${MAKE} -C $(DAIKONDIR)/doc clean
	${MAKE} -C $(DAIKONDIR)/doc
	-${MAKE} -C $(DAIKONDIR)/kvasir distclean
	${MAKE} kvasir

### Testing the code

test:
	cd tests && $(MAKE) all

junit:
	cd java && $(MAKE) junit

### Tags

tags: TAGS

TAGS:
	cd java && $(MAKE) tags

###########################################################################
### Test the distribution
###


DISTTESTDIR := ${TMPDIR}/daikon.dist
DISTTESTDIRJAVA := ${TMPDIR}/daikon.dist/daikon/java

# Test that the files in the staging area are correct.
test-staged-dist: $(STAGING_DIR)
	-rm -rf $(DISTTESTDIR)
	mkdir $(DISTTESTDIR)
	(cd $(DISTTESTDIR); tar xzf $(STAGING_DIR)/download/daikon.tar.gz)
	## First, test daikon.jar.
	(cd $(DISTTESTDIR)/daikon/java && \
	  $(MAKE) CLASSPATH=$(DISTTESTDIR)/daikon/daikon.jar junit)
	## Make sure that all of the class files are 1.5 (version 49) or earlier
	(cd $(DISTTESTDIRJAVA) && find . \( -name '*.class' \) -print | xargs -n 1 classfile_check_version 49)
	## Second, test the .java files.
	# No need to add to classpath: ":$(DISTTESTDIRJAVA)/lib/java-getopt.jar:$(DISTTESTDIRJAVA)/lib/junit.jar"
	(cd $(DISTTESTDIRJAVA)/daikon; rm `find . -name '*.class'`; make CLASSPATH=$(DISTTESTDIRJAVA):$(DISTTESTDIR)/daikon/daikon.jar:$(RTJAR):$(TOOLSJAR) all_javac)
	(cd $(DISTTESTDIR)/daikon/java && $(MAKE) CLASSPATH=$(DISTTESTDIRJAVA):$(DISTTESTDIR)/daikon/daikon.jar junit)
	# Test the main target of the makefile
	cd $(DISTTESTDIR)/daikon && make
	# test basic operation (Chicory/Daikon)
	cd $(DISTTESTDIR)/daikon/examples/java-examples/StackAr && \
	  ${JAVAC} -g `find . -name '*.java'` && \
	  java -cp .:$(DISTTESTDIR)/daikon/daikon.jar -ea daikon.Chicory \
		--daikon DataStructures/StackArTester

# I would rather define this inside the cvs-test rule.  (In that case I
# must use "$$FOO", not $(FOO), to refer to it.)
TESTCVS=${TMPDIR}/daikon.cvs
TESTCVSJAVA=$(TESTCVS)/invariants/java

cvs-test:
	-rm -rf $(TESTCVS)
	mkdir -p $(TESTCVS)
	cd $(TESTCVS) && cvs -Q -P -d $(CVS_REPOSITORY) co invariants
	cd $(TESTCVSJAVA)/daikon && make CLASSPATH=$(TESTCVSJAVA):$(TESTCVSJAVA)/lib/java-getopt.jar:$(TESTCVSJAVA)/lib/junit.jar:(TESTCVSJAVA)/lib/checkers.jar:.:$(RTJAR):$(TOOLSJAR)


###########################################################################
### Distribution
###

# Main distribution

# The staging target builds all of the files that will be distributed
# to the website in the directory $(STAGING_DIR).  This includes:
# daikon.tar.gz, daikon.zip, daikon.jar, javadoc, and the documentation.
# See the dist target for moving these files to the website.
staging: doc/CHANGES
	chmod -R +w $(STAGING_DIR)
	/bin/rm -rf $(STAGING_DIR)
	install -d $(STAGING_DIR)/download
	# Build the main tarfile for daikon
	@echo "]2;Building daikon.tar"
	$(MAKE) daikon.tar
	gzip -c ${TMPDIR}/daikon.tar > $(STAGING_DIR)/download/daikon.tar.gz
	cp -pf ${TMPDIR}/daikon.zip $(STAGING_DIR)/download/daikon.zip
	mv daikon.jar $(STAGING_DIR)/download
	# Build javadoc
	@echo "]2;Building Java doc"
	install -d $(STAGING_DIR)/download/jdoc
	cd java; make 'JAVADOC_DEST=$(STAGING_DIR)/download/jdoc' doc
	# Copy the documentation
	@echo "]2;Copying documentation"
	install -d $(STAGING_DIR)/download/doc
	cd doc && cp -pf $(DOC_FILES_USER) $(STAGING_DIR)/download/doc
	cp -pR doc/images $(STAGING_DIR)/download/doc
	cp -pR doc/daikon_manual_html $(STAGING_DIR)/download/doc
	cp -pR doc/images $(STAGING_DIR)/download/doc/daikon_manual_html
	cp -pR doc/developer_manual_html $(STAGING_DIR)/download/doc
	cd doc/www && ${RSYNC_AR} $(WWW_DAIKON_FILES) $(STAGING_DIR)
	# Build pubs and copy the results
	@echo "]2;Building Pubs"
	cd doc/www && make pubs
	install -d $(STAGING_DIR)/pubs
	cp -pR doc/www/pubs/* $(STAGING_DIR)/pubs
	# all distributed files should be readonly
	chmod -R -w $(STAGING_DIR)
	# compare new list of files in tarfile to previous list
	@echo "]2;New or removed files"
	@echo "***** New or removed files:"
	# Using $(WWW_DIR)/download/daikon.tar.gz is faster than fetching with wget but only works on the local filesystem.
	(cd ${TMPDIR} && wget http://groups.csail.mit.edu/pag/daikon/download/daikon.tar.gz && tar tzf daikon.tar.gz | sort > old_tar.txt && rm -f daikon.tar.gz)
	tar tzf $(STAGING_DIR)/download/daikon.tar.gz | sort > ${TMPDIR}/new_tar.txt
	-diff -u ${TMPDIR}/old_tar.txt ${TMPDIR}/new_tar.txt
	# Delete the tmp files
	cd ${TMPDIR} && /bin/rm -rf daikon daikon.dist daikon.tar daikon.zip \
							old_tar.txt new_tar.txt

# Copy the files in the staging area to the website.  This will copy
# all of the files in staging, but will not delete any files in the website
# that are not in staging.
staging-to-www: $(STAGING_DIR)
	(cd $(STAGING_DIR) && tar cf - .) | (cd $(WWW_DIR) && tar xfBp -)
	@echo "**Update the dates and sizes in the various index files**"
	html-update-link-dates $(DIST_DIR)/index.html
	$(MAKE) update-dist-version-file
	@echo "*****"
	@echo "Don't forget to send mail to daikon-announce and commit changes."
	@echo "(See sample messages in ~mernst/research/invariants/mail/daikon-lists.mail.)"
	@echo "*****"


# Webpages of publications that use Daikon
pubs:
	$(MAKE) -C doc/www pubs

doc/CHANGES: doc/daikon.texinfo
	@echo "******************************************************************"
	@echo "** doc/CHANGES file is not up-to-date with respect to doc files."
	@echo "** doc/CHANGES must be modified by hand."
	@echo "** Try:"
	@echo "     diff -u -s --from-file   $(WWW_DIR)/dist/doc doc/*.texinfo"
	@echo "** (or maybe  touch doc/CHANGES )."
	@echo "******************************************************************"
	@exit 1


doc-all:
	# "make" in doc directory may fail the first time, but do show output.
	-cd doc && $(MAKE) all
	cd doc && $(MAKE) all

# Perl command compresses multiple spaces to one, for first 9 days of month.
TODAY := $(shell date "+%B %e, %Y" | perl -p -e 's/  / /')

update-doc-dist-date-and-version:
	$(MAKE) update-doc-dist-date
	$(MAKE) update-doc-dist-version

# Update the documentation with a new distribution date (today).
# This is done immediately before releasing a new distribution.
update-doc-dist-date:
	perl -wpi -e 's/(Daikon version .*, released ).*(\.|<\/CENTER>)$$/$$1${TODAY}$$2/' ${DIST_VERSION_FILES}
	perl -wpi -e 'BEGIN { $$/="\n\n"; } s/(\@c Daikon version .* date\n\@center ).*(\n)/$$1${TODAY}$$2/;' doc/daikon.texinfo doc/developer.texinfo
	perl -wpi -e 's/(public final static String release_date = ").*(";)$$/$$1${TODAY}$$2/' java/daikon/Daikon.java
	touch doc/CHANGES

# Update the documentation according to the version number in VERSION.
# This isn't done as part of "make dist" because then subsequent "make www"
# would show the new version.
# I removed the dependence on "update-dist-version-file" because this rule
# is invoked at the beginning of a make.
update-doc-dist-version:
	perl -wpi -e 'BEGIN { $$/="\n\n"; } s/(Daikon version )[0-9]+(\.[0-9]+)*/$$1 . "$(shell cat doc/VERSION)"/e;' ${DIST_VERSION_FILES}
	perl -wpi -e 's/(public final static String release_version = ")[0-9]+(\.[0-9]+)*(";)$$/$$1 . "$(shell cat doc/VERSION)" . $$3/e;' java/daikon/Daikon.java
	perl -wpi -e 's/(VG_\(details_version\)\s*\(")[0-9]+(\.[0-9]+)*("\);)$$/$$1 . "$(shell cat doc/VERSION)" . $$3/e' kvasir/fjalar/mc_main.c
	cvs ci -m "Update version number for new Daikon distribution" kvasir/fjalar/mc_main.c
	touch doc/CHANGES

# Update the version number.
# This is done immediately after releasing a new version; thus, VERSION
# refers to the next version to be released, not the previously-released one.
# This isn't a part of the "update-dist-version" target because if it is,
# the "shell cat" command gets the old VERSION file.
# (Note that the last element of VERSION may be negative, such as "-1".
# This is useful in order to make the next version end with ".0".)
update-dist-version-file:
	perl -wpi -e 's/\.(-?[0-9]+)$$/"." . ($$1+1)/e' doc/VERSION

## Problem: "make -C java veryclean; make daikon.jar" fails, as does
## "make -C java clean; make daikon.jar".
## It seems that one must do "make compile" before "make daikon.jar".
# Perhaps daikon.jar shouldn't include JUnit or the test files.
daikon.jar: $(DAIKON_JAVA_FILES) $(patsubst %,java/%,$(DAIKON_RESOURCE_FILES))
	-rm -rf $@ ${TMPDIR}/daikon-jar
	install -d ${TMPDIR}/daikon-jar
	# Compile Daikon and copy the resulting class files
	# to the ${TMPDIR}/daikon-jar directory
	$(MAKE) -C java all_directly
	cd java && find . \( -name dcomp-rt \) -prune -o -name '*.class' -print \
		-exec ${RSYNC_AR} '{}' ${TMPDIR}/daikon-jar \;
	# (cd ${TMPDIR}/daikon-jar; jar xf $(INV_DIR)/java/lib/checkers.jar)
	# (cd ${TMPDIR}/daikon-jar; jar xf $(INV_DIR)/java/lib/jtb-1.1.jar)
	cd ${TMPDIR}/daikon-jar; jar xf $(INV_DIR)/java/lib/java-getopt.jar
	cd ${TMPDIR}/daikon-jar; jar xf $(INV_DIR)/java/lib/junit.jar
	cd ${TMPDIR}/daikon-jar; jar xf $(INV_DIR)/java/lib/bcel.jar
	cd ${TMPDIR}/daikon-jar; jar xf $(INV_DIR)/java/lib/commons-io.jar
	cd ${TMPDIR}/daikon-jar; jar xf $(INV_DIR)/java/lib/plume.jar
	(cd java; ${RSYNC_AR} $(DAIKON_RESOURCE_FILES) ${TMPDIR}/daikon-jar)
	cd ${TMPDIR}/daikon-jar && \
	  jar cfm $@ $(INV_DIR)/java/daikon/chicory/manifest.txt *
	mv ${TMPDIR}/daikon-jar/$@ $@
	#rm -rf ${TMPDIR}/daikon-jar

# This rule creates the files that comprise the distribution, but does
# not copy them anywhere.  (It does no compilation of its own.)
# This rule could be changed to check out a fresh version of the
# repository, then tar from there.  Then there would be no need to be so
# careful about not including extraneous files in the distribution, and one
# could make a distribution even if there were diffs in the current
# checkout.
daikon.tar daikon.zip: doc-all $(DOC_PATHS) $(EDG_FILES) $(README_PATHS) $(DAIKON_JAVA_FILES) daikon.jar java/Makefile

	-rm -rf ${TMPDIR}/daikon
	mkdir ${TMPDIR}/daikon

	mkdir ${TMPDIR}/daikon/doc
	cp -p README-dist.txt ${TMPDIR}/daikon/README.txt
	cp -p README-dist.html ${TMPDIR}/daikon/README.html
	cp -p doc/README.txt ${TMPDIR}/daikon/doc/README.txt
	cd doc && cp -p $(DOC_FILES_NO_IMAGES) ${TMPDIR}/daikon/doc
	mkdir ${TMPDIR}/daikon/doc/images
	cd doc && cp -p $(IMAGE_PARTIAL_PATHS) ${TMPDIR}/daikon/doc/images
	cp -pR doc/daikon_manual_html ${TMPDIR}/daikon/doc

	## EMACS_PATHS is currently empty.
	# # Emacs
	# mkdir ${TMPDIR}/daikon/emacs
	# cp -p $(EMACS_PATHS) ${TMPDIR}/daikon/emacs

	# Plume-lib library
	(cd plume-lib; hg archive ${TMPDIR}/daikon/plume-lib)

	# Auxiliary programs
	mkdir ${TMPDIR}/daikon/bin
	cp -p $(SCRIPT_PATHS) ${TMPDIR}/daikon/bin

	# Java example files
	mkdir ${TMPDIR}/daikon/examples
	cp -pR examples/java-examples ${TMPDIR}/daikon/examples
	# Keep .java files, delete everything else
	cd ${TMPDIR}/daikon && find examples/java-examples -name '*.java' -prune -o \( -type f -o -name CVS -o -name daikon-output -o -name daikon-java -o -name daikon-instrumented \) -print | xargs rm -rf

	# Perl example files
	mkdir ${TMPDIR}/daikon/examples/perl-examples
	cp -p examples/perl-examples/{Birthday.{pm,accessors},{test_bday,standalone}.pl} ${TMPDIR}/daikon/examples/perl-examples

	# C example files for Kvasir
	mkdir ${TMPDIR}/daikon/examples/c-examples
	mkdir ${TMPDIR}/daikon/examples/c-examples/bzip2
	cp -p examples/c-examples/bzip2/bzip2.c ${TMPDIR}/daikon/examples/c-examples/bzip2
	mkdir ${TMPDIR}/daikon/examples/c-examples/wordplay
	cp -p examples/c-examples/wordplay/wordplay.c ${TMPDIR}/daikon/examples/c-examples/wordplay
	cp -p examples/c-examples/wordplay/words.txt ${TMPDIR}/daikon/examples/c-examples/wordplay

	# chgrp -R $(INV_GROUP) ${TMPDIR}/daikon

	cp -p daikon.jar ${TMPDIR}/daikon
	# # Now we are ready to make the daikon-compiled distribution
	# (cd ${TMPDIR}; tar cf daikon-compiled.tar daikon)
	# cp -pf ${TMPDIR}/daikon-compiled.tar .

	## Now make the daikon distribution
	# First add some more files to the distribution

	cp -p Makefile-dist ${TMPDIR}/daikon/Makefile

	# Daikon itself
	(cd java; tar chf ${TMPDIR}/daikon-java.tar --exclude daikon-java --exclude daikon-output --exclude Makefile.user daikon)
	(mkdir ${TMPDIR}/daikon/java; cd ${TMPDIR}/daikon/java; tar xf ${TMPDIR}/daikon-java.tar; rm ${TMPDIR}/daikon-java.tar)
	cp -p java/README.txt ${TMPDIR}/daikon/java/README.txt
	cp -p java/Makefile ${TMPDIR}/daikon/java/Makefile
	cp -p java/Makefile-dist.user ${TMPDIR}/daikon/java/Makefile.user
	# Maybe I should do  $(MAKE) doc
	# Don't do  $(MAKE) clean  which deletes .class files
	(cd ${TMPDIR}/daikon/java; $(RM_TEMP_FILES))

	## I don't think I need the source.  The compiled versions appear in .jar files.
	## # Java support files
	## ## plume
	## (cd plume-lib; hg archive ${TMPDIR}/daikon/java)
	## ## getopt
	## (cd ${TMPDIR}/daikon/java; jar xf $(INV_DIR)/java/lib/java-getopt.jar)
	## ## intern checker
	## # (cd ${TMPDIR}/daikon/java; jar xf $(INV_DIR)/java/lib/checkers.jar)
	## ## Apache packages
	## mkdir ${TMPDIR}/daikon/java/org
	## mkdir ${TMPDIR}/daikon/java/org/apache
	## ## JTB
	## cp -pR java/jtb ${TMPDIR}/daikon/java/
	## ## BCEL
	## (cd ${TMPDIR}/daikon/java; jar xf $(INV_DIR)/java/lib/bcel.jar)
	## ## Apache commons
	## (cd ${TMPDIR}/daikon/java; jar xf $(INV_DIR)/java/lib/commons-io.jar)

	## JUnit
	# This is wrong:
	#   unzip java/lib/$(JUNIT_VERSION).zip -d ${TMPDIR}/daikon/java
	#   (cd ${TMPDIR}/daikon/java; ln -s $(JUNIT_VERSION)/junit .)
	# Need to extract a jar file in the zip file, then unjar that.
	# (src.jar only contains .java files, not .class files.)

	# JHP 9/4/2007: The lines commented out below were removed when the
    # junit .zip file was removed from the CVS repository.  This happened
    # when we updated to junit4.4.  It looks like we were compiling JUnit,
    # though its surprising that was necessary.  Now we just extract from the
    # jar file like we do for other jars that we include.
	##mkdir ${TMPDIR}/daikon/tmp-junit
	##unzip java/lib/$(JUNIT_VERSION).zip $(JUNIT_VERSION)/cpl-v10.html $(JUNIT_VERSION)/src.jar -d ${TMPDIR}/daikon/tmp-junit
	##(cd ${TMPDIR}/daikon/tmp-junit; unzip $(JUNIT_VERSION)/src.jar; rm -f $(JUNIT_VERSION)/src.jar; mv $(JUNIT_VERSION)/cpl-v10.html junit; rmdir $(JUNIT_VERSION); chmod -R +x *; find . -type f -print | xargs chmod -x; rm -rf META-INF TMP; mv junit ${TMPDIR}/daikon/java/)
	##rm -rf ${TMPDIR}/daikon/tmp-junit
	##(cd ${TMPDIR}/daikon/java/junit; ${JAVAC} -g `find . -name '*.java'`)
	## cd ${TMPDIR}/daikon/java; jar xf $(INV_DIR)/java/lib/junit.jar

	# Plume library
	## cd ${TMPDIR}/daikon/java; jar xf $(INV_DIR)/plume-lib/java/plume.jar

	## Front ends
	mkdir ${TMPDIR}/daikon/front-end

	# # C/C++ instrumenter -- now distributed separately
	# mkdir ${TMPDIR}/daikon/front-end/c
	# cp -p $(C_RUNTIME_PATHS) ${TMPDIR}/daikon/front-end/c

	# Perl front end
	# mkdir ${TMPDIR}/daikon/front-end/perl
	cp -pR front-end/perl ${TMPDIR}/daikon/front-end
	(cd ${TMPDIR}/daikon/front-end/perl; $(RM_TEMP_FILES) )

	# Kvasir C front end
	@# "rsync -C" means "copy, ignoring the same files CVS would"
	rsync -rCp ../fjalar/ ${TMPDIR}/daikon/kvasir
	@# Generate configure file
	(cd ${TMPDIR}/daikon/kvasir/valgrind; ./autogen.sh )
	@# CVS-only build script
	rm -f ${TMPDIR}/daikon/kvasir/auto-everything.sh
	@# Internal developer documentation
	rm -rf ${TMPDIR}/daikon/kvasir/valgrind/fjalar/notes
	rm -rf ${TMPDIR}/daikon/kvasir/valgrind/fjalar/trivial-tool
	(cd ${TMPDIR}/daikon/kvasir; $(RM_TEMP_FILES) )

	# Jar file needed for Chicory front end
	cp -p java/ChicoryPremain.jar ${TMPDIR}/daikon/java

	# Jar file needed for DynComp front end
	cp -p java/dcomp_premain.jar ${TMPDIR}/daikon/java

	## Tools
	cp -pR tools ${TMPDIR}/daikon
	(cd ${TMPDIR}/daikon/tools; $(RM_TEMP_FILES); rm -f kmeans/kmeans; (cd hierarchical; rm -f clgroup cluster den difftbl) )

	## Make the source distribution proper
	rm -rf `find ${TMPDIR}/daikon -name CVS`
	(cd ${TMPDIR} && chmod -R a+rX daikon)
	(cd ${TMPDIR}; tar cf daikon.tar daikon)
	cp -pf ${TMPDIR}/daikon.tar .
	rm -f ${TMPDIR}/daikon.zip
	(cd ${TMPDIR}; zip -r daikon daikon)
	cp -pf ${TMPDIR}/daikon.zip .

# Rule for daikon.tar.gz
%.gz : %
	-rm -rf $@
	gzip -c $< > $@


### Front end binaries

## (empty for now)


###########################################################################
### Utilities
###

showvars:
	@echo "DAIKON_JAVA_FILES = " $(DAIKON_JAVA_FILES)
	@echo "WWW_FILES = " $(WWW_FILES)
	@echo "DIST_DIR_PATHS = " $(DIST_DIR_PATHS)



# ## v2 is now obsolete, so there is no longer any need to perform these steps.
# # Only run the "setup" targets once.
# setup-v2-and-v3: setup-v2-and-v3-tests setup-v2-and-v3-daikon
# 
# setup-v2-and-v3-daikon:
# 	mv java/daikon daikon.ver3
# 	cvs update -d -P -r ENGINE_V2_PATCHES java/daikon
# 	mv java/daikon daikon.ver2
# 	ln -s daikon.ver3 java/daikon
# 
# setup-v2-and-v3-tests:
# 	mv tests tests.ver3
# 	cvs update -d -P -r ENGINE_V2_PATCHES tests
# 	mv tests tests.ver2
# 	ln -s tests.ver3 java/daikon
# 
# 
# # To set up the version-2 and version-3 directories:
# # Use the above setup-v2-and-v3 target after updating your invariants
# # directory so that it only contains V3 stuff.
# 
# use-%: daikon-is-symlink
# 	[ -e daikon.$* ]
# 	[ -e tests.$* ]
# 	rm -f java/daikon
# 	ln -s ../daikon.$* java/daikon
# 	$(MAKE) tags >& /dev/null &
# 	rm -f tests
# 	ln -s tests.$* tests
# 
# daikon-is-symlink:
# 	[ ! -e java/daikon ] || [ -L java/daikon ] # daikon must be symlink if it exists
# 	[ ! -e tests ] || [ -L tests ] # tests must be symlink if it exists


plume-lib:
	rm -rf java/utilMDE java/lib/utilMDE.jar
	hg clone ${HG_OPTIONS} https://code.google.com/p/plume-lib/ plume-lib

.PHONY: plume-lib-update
plume-lib-update: plume-lib
ifndef NONETWORK
	(cd plume-lib; ${HG_PULL_U} ${HG_OPTIONS})
endif

# plume.jar is now checked in.
# # The file is real, but the commands should always be re-run even if it exists.
# .PHONY: plume-lib/java/plume.jar
# plume-lib/java/plume.jar: plume-lib-update
# 	$(MAKE) -C java ../plume-lib/java/plume.jar
