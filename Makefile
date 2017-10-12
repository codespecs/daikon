DAIKONDIR_DEFAULT := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))

# Put user-specific changes in your own Makefile.user file in this directory.
# Make will silently continue if Makefile.user does not exist.
-include Makefile.user

# Don't take value from user, because we want operations to apply to this directory.
# DAIKONDIR ?= ${DAIKONDIR_DEFAULT}
DAIKONDIR = ${DAIKONDIR_DEFAULT}

##########################################################################
### Variables
###

# Set the NONETWORK variable to avoid network operations.  Example:
#   make NONETWORK=true compile

# note that for right now, we are only copying the html and texinfo
# versions of the developer manual (though the PDF version is also built)
IMAGE_FILES := daikon-logo.gif daikon-logo.png daikon-logo.eps dfepl-flow.dot
IMAGE_PARTIAL_PATHS := $(addprefix images/,$(IMAGE_FILES))
DOC_FILES_NO_IMAGES := Makefile index.html daikon.texinfo \
                       config-options.texinfo invariants-doc.texinfo \
                       daikon.pdf daikon.html developer.texinfo \
                       developer.html CHANGES VERSION daikon-favicon.png
DOC_FILES := ${DOC_FILES_NO_IMAGES} $(IMAGE_PARTIAL_PATHS)
DOC_PATHS := $(addprefix doc/,$(DOC_FILES))

# The texinfo files are included so we can diff to see what has changed from
# release to release.  They are in the dist/doc directory, but not
# visible to the user.
DOC_FILES_USER := daikon.pdf daikon.html developer.html developer.pdf \
                  daikon.texinfo developer.texinfo config-options.texinfo \
                  invariants-doc.texinfo CHANGES daikon-favicon.png VERSION
README_PATHS := README doc/README fjalar/README
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

# the following is only used in the "make showvars" target
WWW_FILES := $(shell cd doc/www; find . -type f -print | egrep -v '~$$|/.\#|.bak$$|uw/|pubs/')

WWW_PARENT ?= /cse/web/research/plse
WWW_DIR := $(WWW_PARENT)/daikon
INV_DIR := $(shell pwd)

ifeq (cygwin,$(OSTYPE))
#JAVA tools need Windows path on Windows
JAR_DIR := `cygpath -wp $(INV_DIR)`
# for install-test target
QT_PATH := `cygpath -wp ../../../daikon.jar:.`
else
JAR_DIR := $(INV_DIR)
QT_PATH := ../../../daikon.jar:.
endif

# Staging area for the distribution
STAGING_DIR := $(WWW_PARENT)/staging-daikon

# Files to copy to the website, from $DAIKONDIR/doc/www/
WWW_DAIKON_FILES := faq.html index.html mailing-lists.html StackAr.html \
                    download/index.html download/doc/index.html

REPOSITORY := https://github.com/codespecs/daikon.git

## These seem to be used only by the test-staged-dist target.
# It would be nicer to automatically set JAVA_HOME, or to not need it to be set.
ifndef JAVA_HOME
$(error JAVA_HOME is not set)
endif
RTJAR := $(JAVA_HOME)/jre/lib/rt.jar
TOOLSJAR := $(JAVA_HOME)/lib/tools.jar

RSYNC_AR := rsync -aR

GIT_OPTIONS ?=

# JUNIT_VERSION := junit3.8.1

RM_TEMP_FILES := rm -rf `find . \( -name UNUSED -o -name SCCS -o -name RCS -o -name '*.o' -o -name '*~' -o -name '.*~' -o -name '*.orig' -o -name 'config.log' -o -name '*.java-*' -o -name '*to-do' -o -name 'TAGS' -o -name '.\#*' -o -name '.deps' -o -name jikes -o -name daikon-java -o -name daikon-output -o -name core -o -name '*.bak' -o -name '*.rej' -o -name '*.old' -o -name '.nfs*' -o -name '\#*\#' \) -print`

TMPDIR ?= $(if $(shell if [ -d /scratch ] ; then echo true; fi),/scratch/$(USER),/tmp/$(USER))

# For deterministic sorting
export LC_ALL=C

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
	@echo
	@echo "Targets:"
	@echo " compile compile-java     -- compile Java files"
	@echo " junit                    -- run unit tests"
	@echo " test                     -- run system tests"
# must 'make compile' before 'make doc-all'
	@echo " doc-all                  -- build all documentation"
	@echo " tags TAGS                -- make TAGS file for Emacs"
	@echo " kvasir                   -- make Kvasir, the C front end"
	@echo " very-clean               -- remove (most) all generated files"
	@echo " dyncomp-jdk              -- Make file java/dcomp_rt.jar"
	@echo
	@echo "Targets for creating the Daikon distribution:"
	@echo " daikon.tar daikon.jar    -- just makes the tar files"
	@echo " staging                  -- moves all release file to staging-daikon/"
	@echo " test-staged-dist         -- tests the distribution in staging-daikon/"
	@echo " staging-to-www           -- copies staging-daikon/ to website"
	@echo " "
	@echo "Daikon source code is in the java/daikon subdirectory (but you"
	@echo "can perform basic operations like compiling it from here)."


### Compiling the code

compile: compile-java

compile-java:
	cd java && $(MAKE) all

very-clean:
	find . -type f -name "*~" -exec rm -f {} \;
	${MAKE} -C ${DAIKONDIR} clean-everything
	-cd plume-lib/java && $(MAKE) very-clean
	cd scripts && $(MAKE) clean
	cd tests && $(MAKE) very-clean
	-rm -rf examples/java-examples/QueueAr/DataStructures/*.class
	-rm -rf examples/java-examples/StackAr/DataStructures/*.class
	-rm -rf tests/sources/DataStructures/*.class
	-rm -rf daikon-*.tar daikon-*.zip

clean-java:
	cd java && $(MAKE) clean

javadoc:
	cd java && $(MAKE) javadoc

dyncomp-jdk:
	cd java && $(MAKE) dyncomp-jdk

dcomp-jdk:
	cd java && $(MAKE) dcomp-jdk

reformat:
	cd java && $(MAKE) reformat


### Kvasir (C/C++ front end)

ifeq ($(shell uname -m),x86_64)
VALGRIND_ARCH := amd64
else
VALGRIND_ARCH := x86
endif

../fjalar:
	(cd .. && git clone ${GIT_OPTIONS} https://github.com/codespecs/fjalar.git fjalar)

fjalar/valgrind/Makefile.am:
	# If fjalar/valgrind/Makefile.am does not exist, then this must be a fresh
	# daikon repository and we need to create the parallel fjalar repository.
	$(MAKE) ../fjalar
	ln -nsf ../fjalar fjalar
	# force a build
	touch $@

fjalar/valgrind/Makefile.in: fjalar/valgrind/Makefile.am
	cd fjalar/valgrind && ./autogen.sh

fjalar/valgrind/Makefile: fjalar/valgrind/Makefile.in
	cd fjalar/valgrind && ./configure --prefix=`pwd`/inst

.PHONY: kvasir
kvasir:
	$(MAKE) fjalar/valgrind/Makefile
	$(MAKE) -C fjalar/valgrind --no-print-directory
	$(MAKE) -C fjalar/valgrind install >/dev/null

build-kvasir:
ifeq (Linux i686,$(shell uname -sm))
	$(MAKE) kvasir
else
ifeq (Linux i586,$(shell uname -sm))
	$(MAKE) kvasir
else
ifeq (Linux i486,$(shell uname -sm))
	$(MAKE) kvasir
else
ifeq (Linux i386,$(shell uname -sm))
	$(MAKE) kvasir
else
ifeq (Linux x86_64,$(shell uname -sm))
	$(MAKE) kvasir
else
	@echo "Not building Kvasir: it's only for Linux x86 and x86-64"
	@echo "and this appears to be" `uname -sm`
endif
endif
endif
endif
endif


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
	${MAKE} -C ${DAIKONDIR}/java compile
	${MAKE} -C ${DAIKONDIR} daikon.jar
	${MAKE} -C ${DAIKONDIR}/java dcomp_rt.jar
	${MAKE} -C ${DAIKONDIR}/java javadoc
	${MAKE} -C ${DAIKONDIR} doc-all

rebuild-kvasir:
	${MAKE} build-kvasir

clean-everything:
	${MAKE} -C ${DAIKONDIR} clean-everything-but-kvasir
	${MAKE} -C ${DAIKONDIR} clean-kvasir

clean-everything-but-kvasir:
	-rm -rf daikon.jar
	-rm -rf java/java_files.txt
	${MAKE} -C ${DAIKONDIR}/java very-clean
	${MAKE} -C ${DAIKONDIR}/doc very-clean

clean-kvasir:
	-${MAKE} -C ${DAIKONDIR}/fjalar/valgrind uninstall distclean


### Testing the code

test:
	cd tests && $(MAKE) all

junit:
	cd java && $(MAKE) junit

# A quick test used to verify that Chicory and Daikon
# are working properly.
quick-test:
	cd examples/java-examples/StackAr; \
	javac -g DataStructures/*.java; \
	java -cp $(QT_PATH) daikon.Chicory --daikon DataStructures.StackArTester

# Sanity check, suitable for continuous integration such as Jenkins or Travis.
nightly-test:
	$(MAKE) showvars compile daikon.jar
	$(MAKE) javadoc doc-all
	$(MAKE) dyncomp-jdk
	$(MAKE) junit test

# For systems such as Ubuntu 12.04 where makeinfo does not take the --pdf
# command-line option, don't build the PDF manual.
nightly-test-except-doc-pdf:
	$(MAKE) showvars compile daikon.jar
	$(MAKE) javadoc doc-all-except-pdf
	$(MAKE) dyncomp-jdk
	$(MAKE) junit test


### Tags

# To make a TAGS table that does not include generated files, run:
#   make -C java tags-sans-generated
TAGS: tags
.PHONY: tags
tags:
	cd java && $(MAKE) tags
	etags --include=java/TAGS


###########################################################################
### Test the distribution
###

# This is the target we use to verify that the software we are about
# to distribute runs correctly in a variety of target environments.
# Currently, we test Windows(Cygwin), Fedora and Ubuntu client machines.
distribution-check:
	$(MAKE) -C scripts
ifdef DAIKONCLASS_SOURCES
	$(MAKE) -C java
endif
	$(MAKE) -C java dcomp_rt.jar
	$(MAKE) build-kvasir
	$(MAKE) quick-test

DISTTESTDIR := ${TMPDIR}/daikon.dist
DISTTESTDIRJAVA := ${TMPDIR}/daikon.dist/daikon/java

# This is the target we use to do a sanity check of the distribution
# on the machine used to build the release - prior to running the
# 'distribution-check' target on a variety of client machines.
# - verify we can open/unpack the distribution tar file
# - run the junit verification tests on daikon.jar
# - verify the released class files are all version 7
# - verify that we can rebuild the .class files from the .java files
# - run the junit verification tests on the class files
# - run the quick-test
test-staged-dist: $(STAGING_DIR)
	## First, get and test daikon.jar.
	-rm -rf $(DISTTESTDIR)
	mkdir $(DISTTESTDIR)
	(cd $(DISTTESTDIR); tar xzf $(STAGING_DIR)/download/$(NEW_RELEASE_NAME).tar.gz)
	(cd $(DISTTESTDIR); mv $(NEW_RELEASE_NAME) daikon)
	(cd $(DISTTESTDIR)/daikon/java && \
	  $(MAKE) CLASSPATH=$(DISTTESTDIR)/daikon/daikon.jar:$(DISTTESTDIRJAVA)/lib/junit-4.12.jar junit)
	## Make sure that all of the class files are 1.7 (version 51) or earlier.
	(cd $(DISTTESTDIRJAVA) && find . \( -name '*.class' \) -print | xargs -n 1 classfile_check_version 51)
	## Test that we can rebuild the .class files from the .java files.
	(cd $(DISTTESTDIRJAVA)/daikon; rm `find . -name '*.class'`; make CLASSPATH=$(DISTTESTDIRJAVA):$(DISTTESTDIR)/daikon/daikon.jar:$(RTJAR):$(TOOLSJAR):$(DISTTESTDIRJAVA)/lib/junit-4.12.jar all_javac)
	## Test that these new .class files work properly.
	(cd $(DISTTESTDIR)/daikon/java && $(MAKE) CLASSPATH=$(DISTTESTDIRJAVA):$(DISTTESTDIR)/daikon/daikon.jar:$(DISTTESTDIRJAVA)/lib/junit-4.12.jar junit)
	## Test the main target of the makefile.
	cd $(DISTTESTDIR)/daikon && make
	## Test that we can build docs.
	cd $(DISTTESTDIR)/daikon && $(MAKE) doc-all
	## Test the basic operation of Chicory/Daikon.
	cd $(DISTTESTDIR)/daikon && $(MAKE) quick-test

# I would rather define this inside the repository-test rule.  (In that case I
# must use "$$FOO", not $(FOO), to refer to it.)
MYTESTDIR=${TMPDIR}/test
TESTPATH=${MYTESTDIR}/daikon/java

#This is broken and under repair! (markro)
repository-test:
	-rm -rf $(MYTESTDIR)
	mkdir -p $(MYTESTDIR)
	cd $(MYTESTDIR)
	git clone $(REPOSITORY) daikon
# vars for Daikon
	export DAIKONDIR=${MYTESTDIR}/daikon
	export JAVA_HOME=/usr/lib/jvm/java
	source ${DAIKONDIR}/scripts/daikon.bashrc
	cd daikon && make


validate:
	html5validator --ignore /doc/daikon.html /doc/daikon/ /doc/developer.html /doc/developer/ /java/api/ tools/hierarchical/clustering.html /tests/sources/


###########################################################################
### Distribution
###

# Main distribution

# A couple of checks to see if we can proceed with staging.
# Verify that doc/CHANGES is newer than doc/daikon.texinfo - error out if not.
# Report any uncommited files - users responsibility to act on results.
check-repo: doc/CHANGES
	git status -uno

# The staging target builds all of the files that will be distributed
# to the website in the directory $(STAGING_DIR).  This includes:
# daikon.tar.gz, daikon.zip, daikon.jar, javadoc, and the documentation.
# See the dist target for moving these files to the website.
staging:
# Our intention is that members of the plse_www group will always have
# write permission on the release directories; however, if you happen
# to be the owner of an existing file, the permissions system gives
# that priority and we must set the write bit.  These two chmod commands
# will fail if you are not the owner, but the remainder of the commands
# should work fine.
	-chmod u+w $(WWW_PARENT)
	-chmod -R u+w $(STAGING_DIR)
	/bin/rm -rf $(STAGING_DIR)
	mkdir $(STAGING_DIR)
	cp -pR $(WWW_DIR)/history $(STAGING_DIR)
	mkdir $(STAGING_DIR)/download
	$(MAKE) save-current-release
	cp -pR $(WWW_DIR)/download/inv-cvs $(STAGING_DIR)/download
	# Build the main tarfile for daikon
	@echo "]2;Building daikon.tar"
	# make daikon.tar has side effect of making documents
	# it also causes doc/CHANGES time stamp to be checked; which
	# we do not care about at this point.
	touch doc/CHANGES
	$(MAKE) daikon.tar
	gzip -c ${TMPDIR}/$(NEW_RELEASE_NAME).tar > $(STAGING_DIR)/download/$(NEW_RELEASE_NAME).tar.gz
	cp -pf ${TMPDIR}/$(NEW_RELEASE_NAME).zip $(STAGING_DIR)/download/$(NEW_RELEASE_NAME).zip
	cp -pf daikon.jar $(STAGING_DIR)/download
	# Build javadoc
	@echo "]2;Building Javadoc"
	mkdir $(STAGING_DIR)/download/api
	cd java; make 'JAVADOC_DEST=$(STAGING_DIR)/download/api' javadoc
	# Copy the documentation
	@echo "]2;Copying documentation"
	mkdir $(STAGING_DIR)/download/doc
	cd doc && cp -pf $(DOC_FILES_USER) $(STAGING_DIR)/download/doc
	cp -pR doc/images $(STAGING_DIR)/download/doc
	cp -pR doc/daikon $(STAGING_DIR)/download/doc
	cp -pR doc/developer $(STAGING_DIR)/download/doc
	cd doc/www && ${RSYNC_AR} $(WWW_DAIKON_FILES) $(STAGING_DIR)
	# Build pubs and copy the results
	@echo "]2;Building Pubs"
	cd doc/www && make pubs
	mkdir $(STAGING_DIR)/pubs
	cp -pR doc/www/pubs/* $(STAGING_DIR)/pubs
	cp -p doc/daikon-favicon.png $(STAGING_DIR)
	cp -p doc/images/daikon-logo.gif $(STAGING_DIR)
	# This command updates the dates and sizes in the various index files
	html-update-link-dates $(STAGING_DIR)/download/index.html
	# all distributed files should belong to the group plse_www and be group writable.
	# set the owner and other permissions to readonly
	chgrp -R plse_www $(STAGING_DIR)
	chmod -R g+w $(STAGING_DIR)
	-chmod -R u-w $(STAGING_DIR)
	chmod -R o-w $(STAGING_DIR)
	-chmod u-w $(WWW_PARENT)
	# compare new list of files in tarfile to previous list
	@echo "]2;New or removed files"
	@echo "***** New or removed files:"
	# need to remove the leading "daikon-<version>/" before we can compare old and new
	tar tzf $(WWW_DIR)/download/$(CUR_RELEASE_NAME).tar.gz | perl -p -e 's/^(.*?)\///' | sort > ${TMPDIR}/old_tar.txt
	tar tzf $(STAGING_DIR)/download/$(NEW_RELEASE_NAME).tar.gz | perl -p -e 's/^(.*?)\///' | sort > ${TMPDIR}/new_tar.txt
	diff -u ${TMPDIR}/old_tar.txt ${TMPDIR}/new_tar.txt || true
	# Delete the tmp files
	cd ${TMPDIR} && /bin/rm -rf daikon daikon.dist old_tar.txt new_tar.txt

# Convert the staging area to the live release.
# We do this by deleting the previous release directory and
# renaming the staging directory to be the release directory.
staging-to-www: $(STAGING_DIR)
	-chmod u+w $(WWW_PARENT)
	-chmod -R -f u+w $(WWW_DIR)
	\rm -rf $(WWW_DIR)
	\mv $(STAGING_DIR) $(WWW_DIR)
	-chmod -R -f u-w $(WWW_DIR)
	-chmod u-w $(WWW_PARENT)


# Webpages of publications that use Daikon
pubs:
	$(MAKE) -C doc/www pubs

doc/CHANGES: doc/daikon.texinfo
	@echo "******************************************************************"
	@echo "** doc/CHANGES file is not up-to-date with respect to doc files."
	@echo "** doc/CHANGES must be modified by hand."
	@echo "** Try:"
	@echo "     diff -b -u -s --from-file $(WWW_DIR)/download/doc doc/*.texinfo"
	@echo "** (or maybe  touch doc/CHANGES )."
	@echo "******************************************************************"
	@exit 1


doc-all:
	cd doc && $(MAKE) all
doc-all-except-pdf:
	cd doc && $(MAKE) all-except-pdf

# Get the current release version,
# if on the CSE filesystem where an old daikon-*.zip file exists.
ifneq ($(shell ls $(WWW_DIR)/download/daikon-*.zip 2>/dev/null),)
# As we are now including links to the previous version, we must only look at the newest one to get the correct version number.
    CUR_VER := $(shell ls -t -1 $(WWW_DIR)/download/daikon-*.zip |head -1 |perl -p -e 's/^.*download.daikon.//' |perl -p -e 's/.zip//')
    CUR_RELEASE_NAME := daikon-$(CUR_VER)
else
    CUR_RELEASE_NAME := UNKNOWN
endif
# Get the new release version.
NEW_VER := $(shell cat doc/VERSION)
NEW_RELEASE_NAME := daikon-$(NEW_VER)

check-for-broken-doc-links:
	checklink -q -r `grep -v '^#' ${DAIKONDIR}/plume-lib/bin/checklink-args.txt` http://plse.cs.washington.edu/staging-daikon  >check.log 2>&1

HISTORY_DIR := $(STAGING_DIR)/history
save-current-release:
	@echo Saving $(CUR_VER) to history directory.
	-chmod u+w $(HISTORY_DIR)
	mkdir $(HISTORY_DIR)/$(CUR_RELEASE_NAME)
	-chmod u-w $(HISTORY_DIR)
	cd $(HISTORY_DIR)/$(CUR_RELEASE_NAME) && cp -p $(WWW_DIR)/download/$(CUR_RELEASE_NAME).* . && unzip -p $(CUR_RELEASE_NAME).zip $(CUR_RELEASE_NAME)/doc/CHANGES >CHANGES && chmod o-w CHANGES .
# Create links to the previous version to help reduce confusion over a version number change.
	cd $(STAGING_DIR)/download && ln $(HISTORY_DIR)/$(CUR_RELEASE_NAME)/$(CUR_RELEASE_NAME).zip
	cd $(STAGING_DIR)/download && ln $(HISTORY_DIR)/$(CUR_RELEASE_NAME)/$(CUR_RELEASE_NAME).tar.gz

# Perl command compresses multiple spaces to one, for first 9 days of month.
ifeq ($(origin TODAY), undefined)
TODAY := $(shell date "+%B %e, %Y" | perl -p -e 's/  / /')
endif

update-and-commit-version: update-doc-dist-date-and-version
	git commit -a -m "Change version to $(NEW_VER)"
	git push
	cd fjalar && git commit -a -m "Change version to $(NEW_VER)" && git push

update-doc-dist-date-and-version:
	$(MAKE) update-doc-dist-date
	$(MAKE) update-doc-dist-version

# Update the documentation with a new distribution date (today).
# This is done immediately before releasing a new distribution.
update-doc-dist-date:
	perl -wpi -e 's/((Daikon|Fjalar) version .*, released ).*(\.|<\/CENTER>)$$/$$1${TODAY}$$3/' ${DIST_VERSION_FILES}
	perl -wpi -e 'BEGIN { $$/="\n\n"; } s/(\@c .* Daikon version .* date\n\@center ).*(\n)/$$1${TODAY}$$2/;' doc/daikon.texinfo doc/developer.texinfo
	perl -wpi -e 's/(public static final String release_date = ").*(";)$$/$$1${TODAY}$$2/' java/daikon/Daikon.java
	touch doc/CHANGES

# Update the documentation according to the version number in VERSION.
# This isn't done as part of "make dist" because then subsequent "make www"
# would show the new version.
# I removed the dependence on "update-dist-version-file" because this rule
# is invoked at the beginning of a make.
update-doc-dist-version:
	perl -wpi -e 'BEGIN { $$/="\n"; } s/((Daikon|Fjalar) version |[ \/\\]daikon-)[0-9]+(\.[0-9]+)+/$$1 . "$(NEW_VER)"/e;' ${DIST_VERSION_FILES}
	# update the version number in the release archive file names
	perl -wpi -e 's/(\-)[0-9]+(\.[0-9]+)+/$$1 . "$(NEW_VER)"/eg;' doc/www/download/index.html
	perl -wpi -e 's/(public static final String release_version = ")[0-9]+(\.[0-9]+)*(";)$$/$$1 . "$(NEW_VER)" . $$3/e;' java/daikon/Daikon.java
	perl -wpi -e 's/(VG_\(details_version\)\s*\(")[0-9]+(\.[0-9]+)*("\);)$$/$$1 . "$(NEW_VER)" . $$3/e' fjalar/valgrind/fjalar/mc_main.c
	touch doc/CHANGES

# Update the version number in file doc/VERSION
# This is done immediately after releasing a new version; thus, VERSION
# refers to the next version to be released, not the previously-released one.
# (Tip: If you want the next version to end with ".0", then before running
# this target you can set the the last element of VERSION to "-1".)
update-dist-version-file:
	@perl -wpi -e 's/\.(-?[0-9]+)$$/"." . ($$1+1)/e' doc/VERSION
	@echo -n "doc/VERSION now contains: "
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
		| sort | xargs '-I{}' ${RSYNC_AR} '{}' ${TMPDIR}/daikon-jar
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
daikon.tar daikon.zip: doc-all kvasir $(DOC_PATHS) $(EDG_FILES) $(README_PATHS) $(DAIKON_JAVA_FILES) daikon.jar java/Makefile

	-rm -rf ${TMPDIR}/daikon-* ${TMPDIR}/daikon-*.tar ${TMPDIR}/daikon-*.zip ${TMPDIR}/daikon
	mkdir ${TMPDIR}/daikon

	mkdir ${TMPDIR}/daikon/doc
	cp -p README ${TMPDIR}/daikon/README
	cp -p README.source ${TMPDIR}/daikon/README.source
	cp -p doc/README ${TMPDIR}/daikon/doc/README
	(cd doc && cp -p $(DOC_FILES_NO_IMAGES) ${TMPDIR}/daikon/doc)
	mkdir ${TMPDIR}/daikon/doc/images
	(cd doc && cp -p $(IMAGE_PARTIAL_PATHS) ${TMPDIR}/daikon/doc/images)
	cp -pR doc/daikon ${TMPDIR}/daikon/doc
	cp -pR doc/developer ${TMPDIR}/daikon/doc
	cp -pR doc/www ${TMPDIR}/daikon/doc

	# Plume-lib library
	(cd plume-lib; git archive --prefix=plume-lib/ HEAD | (cd ${TMPDIR}/daikon/ && tar xf -))

	# Auxiliary programs
	mkdir ${TMPDIR}/daikon/scripts
	cp -p $(SCRIPT_PATHS) ${TMPDIR}/daikon/scripts

	# Java example files
	mkdir ${TMPDIR}/daikon/examples
	cp -pR examples/java-examples ${TMPDIR}/daikon/examples
	# Keep .java files, delete everything else
	(cd ${TMPDIR}/daikon && find examples/java-examples -name '*.java' -prune -o \( -type f -o -name daikon-output -o -name daikon-java -o -name daikon-instrumented \) -print | xargs rm -rf)

	# Perl example files
	mkdir ${TMPDIR}/daikon/examples/perl-examples
	(cd examples/perl-examples && cp -p Birthday.accessors Birthday.pm standalone.pl test_bday.pl ${TMPDIR}/daikon/examples/perl-examples)

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

	cp -p Makefile ${TMPDIR}/daikon/Makefile

	# Daikon itself
	(cd java; tar chf ${TMPDIR}/daikon-java.tar --exclude daikon-java --exclude daikon-output --exclude Makefile.user daikon jtb lib)
	(mkdir ${TMPDIR}/daikon/java; cd ${TMPDIR}/daikon/java; tar xf ${TMPDIR}/daikon-java.tar; rm ${TMPDIR}/daikon-java.tar)
	cp -p java/README.txt ${TMPDIR}/daikon/java/README.txt
	cp -p java/Makefile ${TMPDIR}/daikon/java/Makefile
	# Maybe I should do  $(MAKE) javadoc
	# Don't do  $(MAKE) clean  which deletes .class files
	(cd ${TMPDIR}/daikon/java; $(RM_TEMP_FILES))

	# Plume library
	## (cd ${TMPDIR}/daikon/java; jar xf $(INV_DIR)/plume-lib/java/plume.jar)

	## Front ends
	mkdir ${TMPDIR}/daikon/front-end

	# Perl front end
	# mkdir ${TMPDIR}/daikon/front-end/perl
	cp -pR front-end/perl ${TMPDIR}/daikon/front-end
	(cd ${TMPDIR}/daikon/front-end/perl; $(RM_TEMP_FILES) )

	# Kvasir C front end
# We use the --filter option twice with rsync to exclude unneeded files.
# The first attempts to ignore all files indicated by the contents
# of the .gitignore file.  The second ignores the version control files.
	rsync -rptg -L --filter=':- fjalar/.gitignore' --filter='. scripts/rsync.ignore' fjalar ${TMPDIR}/daikon

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
	(cd ${TMPDIR}; mv daikon $(NEW_RELEASE_NAME))
	(cd ${TMPDIR}; tar cf $(NEW_RELEASE_NAME).tar $(NEW_RELEASE_NAME))
	cp -pf ${TMPDIR}/$(NEW_RELEASE_NAME).tar .
	(cd ${TMPDIR}; zip -r $(NEW_RELEASE_NAME).zip $(NEW_RELEASE_NAME))
	cp -pf ${TMPDIR}/$(NEW_RELEASE_NAME).zip .


### Front end binaries

## (empty for now)


###########################################################################
### Utilities
###

showvars:
	@echo "DAIKONDIR =" $(DAIKONDIR)
	@echo "DAIKONDIR_DEFAULT =" $(DAIKONDIR_DEFAULT)
	@echo "DAIKON_JAVA_FILES =" $(DAIKON_JAVA_FILES)
	@echo "WWW_FILES =" $(WWW_FILES)
	@echo "CUR_RELEASE_NAME =" $(CUR_RELEASE_NAME)
	@echo "NEW_RELEASE_NAME =" $(NEW_RELEASE_NAME)
	${MAKE} -C java showvars

plume-lib:
	rm -rf java/utilMDE java/lib/utilMDE.jar
	# Don't use an ssh URL because can't pull from it in cron jobs
	git clone ${GIT_OPTIONS} https://github.com/mernst/plume-lib.git plume-lib

.PHONY: plume-lib-update
plume-lib-update: plume-lib
ifndef NONETWORK
	# if plume-lib/.git does not exist, then directory was created
	# from a daikon archive file - cannot do a git pull.
	# The "git pull" command fails under Fedora 23, for mysterious reasons.
	if test -d plume-lib/.git ; then \
		(cd plume-lib && git pull -q ${GIT_OPTIONS}) || true; fi
endif

update-plume-jar: plume-lib-update
ifndef CHECKERFRAMEWORK
	$(error CHECKERFRAMEWORK is not set)
endif
	make -D plume-lib/java clean jar verify-plume-jar-classfile-version
	\cp -pf plume-lib/java/plume.jar java/lib/

.PHONY: git-hooks
git-hooks: .git/hooks/pre-commit .git/hooks/post-merge
.git/hooks/pre-commit: scripts/daikon.pre-commit
	(cd .git/hooks && ln -s ../../scripts/daikon.pre-commit pre-commit)
.git/hooks/post-merge: scripts/daikon.post-merge
	(cd .git/hooks && ln -s ../../scripts/daikon.post-merge post-merge)
