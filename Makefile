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
# versions of the developer manual (though the PDF version is also built)
IMAGE_FILES := daikon-logo.gif daikon-logo.png daikon-logo.eps dfepl-flow.jpg
IMAGE_PARTIAL_PATHS := $(addprefix images/,$(IMAGE_FILES))
DOC_FILES_NO_IMAGES := Makefile index.html daikon.texinfo \
                       config-options.texinfo invariants-doc.texinfo \
                       daikon.pdf daikon.html developer.texinfo \
                       developer.html CHANGES
DOC_FILES := ${DOC_FILES_NO_IMAGES} $(IMAGE_PARTIAL_PATHS)
DOC_PATHS := $(addprefix doc/,$(DOC_FILES))

# The texinfo files are included so we can diff to see what has changed from
# release to release.  They are in the dist/doc directory, but not
# visible to the user
DOC_FILES_USER := daikon.pdf daikon.html developer.html developer.pdf \
                  daikon.texinfo developer.texinfo config-options.texinfo \
                  invariants-doc.texinfo CHANGES daikon-favicon.png \
                  valgrind-merge.pdf valgrind-merge.html valgrind-merge.texinfo
README_PATHS := README.txt README.html doc/README fjalar/README
# Files that contain the (automatically updated) version number and date.
DIST_VERSION_FILES := ${README_PATHS} doc/daikon.texinfo doc/developer.texinfo \
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

SCRIPT_PATHS := $(addprefix scripts/,$(SCRIPT_FILES)) \
                $(addprefix plume-lib/bin/,$(PLUME_SCRIPT_FILES))

# This is so troublesome that it isn't used except as a list of dependences for make commands
DAIKON_JAVA_FILES := $(shell find java -name '*daikon-java*' -prune -o -name '*.java' -print) $(shell find java/daikon -follow -name '*daikon-java*' -prune -o -name '*.java' -print)
DAIKON_RESOURCE_FILES := daikon/config/example-settings.txt \
	daikon/simplify/daikon-background.txt \
	daikon/simplify/daikon-background-defined.txt \
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
	daikon/test/GenericTestClass.java \
	daikon/test/dtracediff/AllTypes.dtrace.gz \
	daikon/test/dtracediff/Hanoi-badvar.dtrace.gz \
	daikon/test/dtracediff/Hanoi-mungpointers.dtrace.gz \
	daikon/test/dtracediff/Hanoi-badvalue.dtrace.gz \
	daikon/test/dtracediff/Hanoi.dtrace.gz \
	daikon/test/dtracediff/Hanoi-truncated.dtrace.gz

# the following is only used in show-vars 
WWW_FILES := $(shell cd doc/www; find . -type f -print | egrep -v '~$$|/.\#|.bak$$|uw/|pubs/')

WWW_PARENT ?= /cse/web/research/plse
WWW_DIR := $(WWW_PARENT)/daikon
INV_DIR := $(shell pwd)

ifeq (cygwin,$(OSTYPE))
#JAVA tools need Windows path on Windows
JAR_DIR := $(shell cygpath -m $(INV_DIR))
else
JAR_DIR := $(INV_DIR)
endif

# Staging area for the distribution
STAGING_DIR := $(WWW_DIR)/staging-daikon

# Files to copy to the website
WWW_DAIKON_FILES := faq.html index.html mailing-lists.html StackAr.html \
                    download/index.html download/doc/index.html

DIST_DIR := $(WWW_DIR)/download

# the following is only used in show-vars 
DIST_DIR_PATHS := daikon.tar.gz daikon.zip doc/images/daikon-logo.gif daikon.jar

REPOSITORY := https://code.google.com/p/daikon

## These seem to be used only by the test-staged-dist target.
# It would be nicer to automatically set JAVA_HOME, or to not need it to be set.
ifndef JAVA_HOME
$(error JAVA_HOME is not set)
endif
RTJAR := $(JAVA_HOME)/jre/lib/rt.jar
TOOLSJAR := $(JAVA_HOME)/lib/tools.jar

RSYNC_AR := rsync -aR

# A good alternative for Makefile.user is: hg fetch
# When disconnected from network, change this to a no-op, or (better) just
# set NONETWORK to true.
# HG_PULL_U ?= hg pull -u
HG_PULL_U ?= hg pull; hg merge --tool internal:merge; hg update
# Example Makefile.user line, on cygwin: HG_OPTIONS=--insecure
HG_OPTIONS ?=

# JUNIT_VERSION := junit3.8.1

RM_TEMP_FILES := rm -rf `find . \( -name UNUSED -o -name SCCS -o -name RCS -o -name '*.o' -o -name '*~' -o -name '.*~' -o -name '*.orig' -o -name 'config.log' -o -name '*.java-*' -o -name '*to-do' -o -name 'TAGS' -o -name '.\#*' -o -name '.deps' -o -name jikes -o -name daikon-java -o -name daikon-output -o -name core -o -name '*.bak' -o -name '*.rej' -o -name '*.old' -o -name '.nfs*' -o -name '\#*\#' \) -print`

TMPDIR ?= $(if $(shell if [ -d /scratch ] ; then echo true; fi),/scratch/$(USER),/tmp/$(USER))

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
# must 'make compile' before 'make doc-all'
	@echo " doc-all                  -- build all documentation"
	@echo " tags TAGS                -- make TAGS file for Emacs"
	@echo " kvasir                   -- make Kvasir, the C front end"
	@echo " very-clean               -- remove (most) all generated files"
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

very-clean:
	cd doc && $(MAKE) very-clean
	cd java && $(MAKE) very-clean
	cd plume-lib/java && $(MAKE) very-clean
	cd scripts && $(MAKE) clean
	cd tests && $(MAKE) very-clean
	-rm -rf examples/java-examples/QueueAr/DataStructures/*.class
	-rm -rf examples/java-examples/StackAr/DataStructures/*.class
	-rm -rf tests/sources/DataStructures/*.class
	-rm -rf daikon.jar daikon.tar daikon.zip

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

fjalar/valgrind/Makefile.am: ../fjalar/auto-everything.sh
	ln -nsf ../fjalar fjalar
	touch $@

fjalar/valgrind/Makefile.in: fjalar/valgrind/Makefile.am
	cd fjalar/valgrind && ./autogen.sh

fjalar/valgrind/Makefile: fjalar/valgrind/Makefile.in 
	cd fjalar/valgrind && ./configure --prefix=`pwd`/inst

fjalar/valgrind/coregrind/valgrind: fjalar/valgrind/Makefile
	cd fjalar/valgrind && $(MAKE) --no-print-directory

fjalar/valgrind/fjalar/fjalar-$(VALGRIND_ARCH)-linux: fjalar/valgrind/coregrind/valgrind
	cd fjalar/valgrind/fjalar && $(MAKE) --no-print-directory

fjalar/valgrind/inst/bin/valgrind: fjalar/valgrind/coregrind/valgrind
	cd fjalar/valgrind && $(MAKE) install >/dev/null

fjalar/valgrind/inst/lib/valgrind/fjalar-$(VALGRIND_ARCH)-linux: fjalar/valgrind/fjalar/fjalar-$(VALGRIND_ARCH)-linux
	cd fjalar/valgrind/fjalar && $(MAKE) install >/dev/null

kvasir: fjalar/valgrind/inst/lib/valgrind/fjalar-$(VALGRIND_ARCH)-linux fjalar/valgrind/inst/bin/valgrind

build-kvasir: kvasir

### Rebuild everything; used for monthly releases, for example

rebuild-everything-clean:
	${MAKE} -C ${DAIKONDIR} clean-everything
	${MAKE} -C ${DAIKONDIR} rebuild-everything

rebuild-everything-but-kvasir-clean:
	${MAKE} -C ${DAIKONDIR} clean-everything-but-kvasir
	${MAKE} -C ${DAIKONDIR} rebuild-everything-but-kvasir

rebuild-everything:
	${MAKE} -C ${DAIKONDIR} rebuild-everything-but-kvasir
	${MAKE} -C ${DAIKONDIR} rebuild-kvasir

rebuild-everything-but-kvasir:
	${MAKE} -C ${DAIKONDIR}/java tags compile
	${MAKE} -C ${DAIKONDIR} daikon.jar
	${MAKE} -C ${DAIKONDIR}/java dcomp_rt.jar
	${MAKE} -C ${DAIKONDIR}/java javadoc
	${MAKE} -C ${DAIKONDIR} doc-all

rebuild-kvasir:
	${MAKE} kvasir

clean-everything:
	${MAKE} -C ${DAIKONDIR} clean-everything-but-kvasir
	${MAKE} -C ${DAIKONDIR} clean-kvasir

clean-everything-but-kvasir:
	-rm -rf daikon.jar
	-rm -rf java/java_files.txt
	${MAKE} -C ${DAIKONDIR}/java very-clean
	${MAKE} -C ${DAIKONDIR}/doc clean

clean-kvasir:
	-${MAKE} -C ${DAIKONDIR}/fjalar/valgrind uninstall distclean 


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
	## Make sure that all of the class files are 1.7 (version 51) or earlier
	(cd $(DISTTESTDIRJAVA) && find . \( -name '*.class' \) -print | xargs -n 1 classfile_check_version 51)
	## Second, test the .java files.
	# No need to add to classpath: ":$(DISTTESTDIRJAVA)/lib/java-getopt.jar:$(DISTTESTDIRJAVA)/lib/junit.jar"
	(cd $(DISTTESTDIRJAVA)/daikon; rm `find . -name '*.class'`; make CLASSPATH=$(DISTTESTDIRJAVA):$(DISTTESTDIR)/daikon/daikon.jar:$(RTJAR):$(TOOLSJAR) all_javac)
	(cd $(DISTTESTDIR)/daikon/java && $(MAKE) CLASSPATH=$(DISTTESTDIRJAVA):$(DISTTESTDIR)/daikon/daikon.jar junit)
	# Test the main target of the makefile
	cd $(DISTTESTDIR)/daikon && make
	# test basic operation (Chicory/Daikon)
	cd $(DISTTESTDIR)/daikon/examples/java-examples/StackAr && \
	  javac -g `find . -name '*.java'` && \
	  java -cp .:$(DISTTESTDIR)/daikon/daikon.jar -ea daikon.Chicory \
		--daikon DataStructures/StackArTester

# I would rather define this inside the repository-test rule.  (In that case I
# must use "$$FOO", not $(FOO), to refer to it.)
MYTESTDIR=${TMPDIR}/test
TESTPATH=${MYTESTDIR}/daikon/java

#This is broken and under repair! (markro)
repository-test:
	-rm -rf $(MYTESTDIR)
	mkdir -p $(MYTESTDIR)
	cd $(MYTESTDIR)
	hg clone -q $(REPOSITORY) daikon
# vars for Daikon
	export DAIKONDIR=${MYTESTDIR}/daikon
	export JAVA_HOME=/usr/lib/jvm/java
	source ${DAIKONDIR}/scripts/daikon.bashrc
	cd daikon && make 


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
	chmod +w $(STAGING_DIR)/..
	/bin/rm -rf $(STAGING_DIR)
    # dummy history directory to remove checklink warnings
	install -d $(STAGING_DIR)/history
	install -d $(STAGING_DIR)/download
	# Build the main tarfile for daikon
	@echo "]2;Building daikon.tar"
	# make daikon.tar has side effect of making 'finalout' version of documents
	$(MAKE) daikon.tar
	gzip -c ${TMPDIR}/daikon.tar > $(STAGING_DIR)/download/daikon.tar.gz
	cp -pf ${TMPDIR}/daikon.zip $(STAGING_DIR)/download/daikon.zip
	cp -pf daikon.jar $(STAGING_DIR)/download
	# Build javadoc
	@echo "]2;Building Javadoc"
	install -d $(STAGING_DIR)/download/api
	cd java; make 'JAVADOC_DEST=$(STAGING_DIR)/download/api' javadoc
	# Copy the documentation
	@echo "]2;Copying documentation"
	install -d $(STAGING_DIR)/download/doc
	cd doc && cp -pf $(DOC_FILES_USER) $(STAGING_DIR)/download/doc
	cp -pR doc/images $(STAGING_DIR)/download/doc
	cp -pR doc/daikon $(STAGING_DIR)/download/doc
	cp -pR doc/developer $(STAGING_DIR)/download/doc
	cd doc/www && ${RSYNC_AR} $(WWW_DAIKON_FILES) $(STAGING_DIR)
	# Build pubs and copy the results
	@echo "]2;Building Pubs"
	cd doc/www && make pubs
	install -d $(STAGING_DIR)/pubs
	cp -pR doc/www/pubs/* $(STAGING_DIR)/pubs
	cp -p doc/images/daikon-logo.gif $(STAGING_DIR)
	# all distributed files should be readonly
	chmod -R -w $(STAGING_DIR)
	# compare new list of files in tarfile to previous list
	@echo "]2;New or removed files"
	@echo "***** New or removed files:"
	tar tzf $(WWW_DIR)/download/daikon.tar.gz | sort > ${TMPDIR}/old_tar.txt
	tar tzf $(STAGING_DIR)/download/daikon.tar.gz | sort > ${TMPDIR}/new_tar.txt
	-diff -u ${TMPDIR}/old_tar.txt ${TMPDIR}/new_tar.txt
	# Delete the tmp files
	cd ${TMPDIR} && /bin/rm -rf daikon daikon.dist daikon.tar daikon.zip \
							old_tar.txt new_tar.txt

# Copy the files in the staging area to the website.  This will copy
# all of the files in staging, but will not delete any files in the website
# that are not in staging.
staging-to-www: $(STAGING_DIR)
#copy the files
	chmod -R u+w,g+w $(WWW_DIR)
# don't trash existing history directory
	(cd $(STAGING_DIR) && tar cf - --exclude=history .) | (cd $(WWW_DIR) && tar xfBp -)
	chmod -R u-w,g-w $(WWW_DIR)
	@echo "**Update the dates and sizes in the various index files**"
# need to allow write so html-update can update	
	chmod +w $(DIST_DIR)
	chmod +w $(DIST_DIR)/index.html
	html-update-link-dates $(DIST_DIR)/index.html
	chmod -w $(DIST_DIR)
	chmod -w $(DIST_DIR)/index.html
# with new version number system, this is now done manually
#	$(MAKE) update-dist-version-file
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
	cd doc && $(MAKE) all

# Get the current release version
CUR_VER := $(shell unzip -p /cse/web/research/plse/daikon/download/daikon.zip daikon/README.txt |head -2|tail -1|perl -p -e 's/ version /\./' |perl -p -e 's/,.*//')
HISTORY_DIR := /cse/web/research/plse/daikon/history

check-for-broken-doc-links:
	checklink -q -r `grep -v '^#' ${DAIKONDIR}/plume-lib/bin/checklink-args.txt` http://plse.cs.washington.edu/daikon/staging-daikon >check.log 2>&1

save-current-release:
	@echo Saving $(CUR_VER) to history directory.
	chmod +w $(HISTORY_DIR)
	mkdir $(HISTORY_DIR)/$(CUR_VER)
	chmod -w $(HISTORY_DIR)
	cd $(HISTORY_DIR)/$(CUR_VER) && cp /cse/web/research/plse/daikon/download/daikon.zip . && unzip -p daikon.zip daikon/doc/CHANGES >CHANGES && chmod -w CHANGES .

# Perl command compresses multiple spaces to one, for first 9 days of month.
ifeq ($(origin TODAY), undefined)
TODAY := $(shell date "+%B %e, %Y" | perl -p -e 's/  / /')
endif

update-doc-dist-date-and-version:
	$(MAKE) update-doc-dist-date
	$(MAKE) update-doc-dist-version

# Update the documentation with a new distribution date (today).
# This is done immediately before releasing a new distribution.
update-doc-dist-date:
	perl -wpi -e 's/((Daikon|Fjalar) version .*, released ).*(\.|<\/CENTER>)$$/$$1${TODAY}$$3/' ${DIST_VERSION_FILES}
	perl -wpi -e 'BEGIN { $$/="\n\n"; } s/(\@c .* Daikon version .* date\n\@center ).*(\n)/$$1${TODAY}$$2/;' doc/daikon.texinfo doc/developer.texinfo
	perl -wpi -e 's/(public final static String release_date = ").*(";)$$/$$1${TODAY}$$2/' java/daikon/Daikon.java
	touch doc/CHANGES

# Update the documentation according to the version number in VERSION.
# This isn't done as part of "make dist" because then subsequent "make www"
# would show the new version.
# I removed the dependence on "update-dist-version-file" because this rule
# is invoked at the beginning of a make.
update-doc-dist-version:
	perl -wpi -e 'BEGIN { $$/="\n\n"; } s/((Daikon|Fjalar) version )[0-9]+(\.[0-9]+)*/$$1 . "$(shell cat doc/VERSION)"/e;' ${DIST_VERSION_FILES}
	perl -wpi -e 's/(public final static String release_version = ")[0-9]+(\.[0-9]+)*(";)$$/$$1 . "$(shell cat doc/VERSION)" . $$3/e;' java/daikon/Daikon.java
	perl -wpi -e 's/(VG_\(details_version\)\s*\(")[0-9]+(\.[0-9]+)*("\);)$$/$$1 . "$(shell cat doc/VERSION)" . $$3/e' fjalar/valgrind/fjalar/mc_main.c
#	cvs ci -m "Update version number for new Daikon distribution" fjalar/valgrind/fjalar/mc_main.c
	touch doc/CHANGES

# Update the version number.
# This is done immediately after releasing a new version; thus, VERSION
# refers to the next version to be released, not the previously-released one.
# This isn't a part of the "update-dist-version" target because if it is,
# the "shell cat" command gets the old VERSION file.
# (Note that the last element of VERSION may be negative, such as "-1".
# This is useful in order to make the next version end with ".0".)
update-dist-version-file:
	@perl -wpi -e 's/\.(-?[0-9]+)$$/"." . ($$1+1)/e' doc/VERSION
	@cat doc/VERSION

JAR_FILES = \
$(INV_DIR)/java/lib/java-getopt.jar \
$(INV_DIR)/java/lib/plume.jar

## Problem: "make -C java veryclean; make daikon.jar" fails, as does
## "make -C java clean; make daikon.jar".
## It seems that one must do "make compile" before "make daikon.jar".
# Perhaps daikon.jar shouldn't include JUnit or the test files.
.PHONY: jar
jar: daikon.jar
daikon.jar: $(DAIKON_JAVA_FILES) $(patsubst %,java/%,$(DAIKON_RESOURCE_FILES)) $(JAR_FILES)
	-rm -rf $@ ${TMPDIR}/daikon-jar
	install -d ${TMPDIR}/daikon-jar
	# Compile Daikon and copy the resulting class files
	# to the ${TMPDIR}/daikon-jar directory
	$(MAKE) -C java all_directly
	cd java && find . \( -name "dcomp-rt*" \) -prune -o -name '*.class' -print \
		-exec ${RSYNC_AR} '{}' ${TMPDIR}/daikon-jar \;
	# (cd ${TMPDIR}/daikon-jar; jar xf $(INV_DIR)/java/lib/checkers.jar)
	# (cd ${TMPDIR}/daikon-jar; jar xf $(INV_DIR)/java/lib/jtb-1.1.jar)

	cd ${TMPDIR}/daikon-jar; jar xf $(JAR_DIR)/java/lib/java-getopt.jar
	cd ${TMPDIR}/daikon-jar; jar xf $(JAR_DIR)/java/lib/plume.jar
	(cd java; ${RSYNC_AR} $(DAIKON_RESOURCE_FILES) ${TMPDIR}/daikon-jar)
	(cd java; ${RSYNC_AR} daikon/tools/runtimechecker/Main.doc daikon/tools/runtimechecker/InstrumentHandler.doc ${TMPDIR}/daikon-jar)
	cd ${TMPDIR}/daikon-jar && \
	  jar cfm $@ $(JAR_DIR)/java/daikon/chicory/manifest.txt *
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
	cp -p README.txt ${TMPDIR}/daikon/README.txt
	cp -p README.html ${TMPDIR}/daikon/README.html
	cp -p README.source ${TMPDIR}/daikon/README.source
	cp -p doc/README ${TMPDIR}/daikon/doc/README
	cd doc && cp -p $(DOC_FILES_NO_IMAGES) ${TMPDIR}/daikon/doc
	mkdir ${TMPDIR}/daikon/doc/images
	cd doc && cp -p $(IMAGE_PARTIAL_PATHS) ${TMPDIR}/daikon/doc/images
	cp -pR doc/daikon ${TMPDIR}/daikon/doc

	# Plume-lib library
	(cd plume-lib; git archive ${TMPDIR}/daikon/plume-lib)

	# Auxiliary programs
	mkdir ${TMPDIR}/daikon/scripts
	cp -p $(SCRIPT_PATHS) ${TMPDIR}/daikon/scripts

	# Java example files
	mkdir ${TMPDIR}/daikon/examples
	cp -pR examples/java-examples ${TMPDIR}/daikon/examples
	# Keep .java files, delete everything else
	cd ${TMPDIR}/daikon && find examples/java-examples -name '*.java' -prune -o \( -type f -o -name daikon-output -o -name daikon-java -o -name daikon-instrumented \) -print | xargs rm -rf

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
	cp -p daikon.jar ${TMPDIR}/daikon

	## Now make the daikon distribution
	# First add some more files to the distribution

	cp -p Makefile-dist ${TMPDIR}/daikon/Makefile

	# Daikon itself
	(cd java; tar chf ${TMPDIR}/daikon-java.tar --exclude daikon-java --exclude daikon-output --exclude Makefile.user daikon)
	(mkdir ${TMPDIR}/daikon/java; cd ${TMPDIR}/daikon/java; tar xf ${TMPDIR}/daikon-java.tar; rm ${TMPDIR}/daikon-java.tar)
	cp -p java/README.txt ${TMPDIR}/daikon/java/README.txt
	cp -p java/Makefile ${TMPDIR}/daikon/java/Makefile
	# Maybe I should do  $(MAKE) javadoc
	# Don't do  $(MAKE) clean  which deletes .class files
	(cd ${TMPDIR}/daikon/java; $(RM_TEMP_FILES))

	# Plume library
	## cd ${TMPDIR}/daikon/java; jar xf $(INV_DIR)/plume-lib/java/plume.jar

	## Front ends
	mkdir ${TMPDIR}/daikon/front-end

	# Perl front end
	# mkdir ${TMPDIR}/daikon/front-end/perl
	cp -pR front-end/perl ${TMPDIR}/daikon/front-end
	(cd ${TMPDIR}/daikon/front-end/perl; $(RM_TEMP_FILES) )

	# Kvasir C front end
# We use the --filter option twice with rsync to exclude unneeded files.
# The first attempts to ignore all files indicated by the contents
# of the .hgignore file.  The second ignores the .hg files.
	rsync -rp -L --filter=':- .hgignore' --filter='. rsync.ignore' fjalar ${TMPDIR}/daikon

	@# Internal developer documentation
	rm -rf ${TMPDIR}/daikon/fjalar/valgrind/fjalar/notes
	rm -rf ${TMPDIR}/daikon/fjalar/valgrind/fjalar/basic-tool
	(cd ${TMPDIR}/daikon/fjalar; $(RM_TEMP_FILES) )

	# Jar file needed for Chicory front end
	cp -p java/ChicoryPremain.jar ${TMPDIR}/daikon/java

	# Jar file needed for DynComp front end
	cp -p java/dcomp_premain.jar ${TMPDIR}/daikon/java

	## Tools
	cp -pR tools ${TMPDIR}/daikon
	(cd ${TMPDIR}/daikon/tools; $(RM_TEMP_FILES); rm -f kmeans/kmeans; (cd hierarchical; rm -f clgroup cluster den difftbl) )

	## Make the source distribution proper
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

plume-lib:
	rm -rf java/utilMDE java/lib/utilMDE.jar
#	hg clone ${HG_OPTIONS} https://code.google.com/p/plume-lib/ plume-lib
	git clone ${GIT_OPTIONS} https://github.com/mernst/plume-lib.git plume-lib

.PHONY: plume-lib-update
plume-lib-update: plume-lib
ifndef NONETWORK
	(cd plume-lib; git pull ${GIT_OPTIONS})
endif

