### User customizations

# Put user-specific changes in your own Makefile.user
#  (such as changes to UTILMDE_DIR or JAVAC or JAVAC_EXTRA_ARGS).
# Make will silently continue if file does not exist.
-include Makefile.user

DAIKONDIR ?= ../..
CHECKERS ?= ${HOME}/research/types/checker-framework/checkers
CHECKERSJAR ?= ${DAIKONDIR}/java/lib/checkers.jar
CLASSPATH:=.:${DAIKONDIR}/java/lib/junit.jar:${CHECKERSJAR}:${DAIKONDIR}/java/lib/bcel.jar:$(JDKDIR)/lib/tools.jar:svnkit.jar:ini4j-0.5.1.jar:${CLASSPATH}

JAVAC ?= javac
# Removed "-J-Xmx800m", because it is incompatible with JDK 7.
JAVAC_ARGS ?= -g -target 5 -Xlint -Xmaxwarns 1000
# When using the JSR 308 compiler, this expects checkers.jar on the classpath.
# The "-classpath" argument is only necessary when using the JSR 308 javac,
# since it's a shell command for "javac -jar ..." which ignores the
# CLASSPATH environment variable and thus must be supplied explicitly.
JAVAC_COMMAND = ${JAVAC} ${JAVAC_ARGS} ${JAVAC_ARGS}
# JAVAC ?= javac -g -Xlint:unchecked
# JAVAC ?= jikes -g +E +F
JAVA ?= java -ea

JAR ?= jar

# Leading "./" is for consistency with output of "find", so that names are
# identical and "sort" can remove duplicates.
AUTO_GENERATED_FILES := \
	./MathMDE.java \
	./LimitedSizeIntSet.java
# Call to sort is to remove duplicates, which would cause javac errors.
JAVA_FILES := $(sort $(shell find . \( -name CVS \) -prune -o -name '*.java' -print | sort) ${AUTO_GENERATED_FILES})
JAVA_LINT_FILES := $(shell find . \( -name CVS -o -name ArraysMDE.java -o -name WeakHasherMap.java -o -name Intern.java -o -name OrderedPairIterator.java -o -name WeakIdentityHashMap.java \) -prune -o -name '*.java' -print | sort)
JAVA_CPP_FILES := $(shell find . \( -name CVS \) -prune -o -name '*.java.jpp' -print | sort)
JAVA_AND_CPP_FILES := ${JAVA_FILES} ${JAVA_CPP_FILES}

jsr308_imports=checkers.interning.quals.*:checkers.nullness.quals.*

export jsr308_imports
export CLASSPATH

default: all
compile: all

all: test

compile_notest:  .class-files-timestamp

# Don't do this.  We want to create .class-files-timestamp after the
# compilation succeeds.  So, no targets for .class files in this Makefile.
# ${ALL_CLASS_FILES}: .class-files-timestamp

# This rule creates ${ALL_CLASS_FILES}.
# The timestamp file indicates when the files were last compiled.
.class-files-timestamp: ${JAVA_AND_CPP_FILES}
	${JAVAC_COMMAND} ${JAVAC_EXTRA_ARGS} ${JAVA_FILES} 2>&1
	touch .class-files-timestamp

clean:
	rm -f *.class
	rm .class-files-timestamp

very_clean: clean
	rm -f ${AUTO_GENERATED_FILES}

tags: TAGS
TAGS: ${JAVA_AND_CPP_FILES}
	etags ${JAVA_AND_CPP_FILES}

# This "set" form doesn't seem to work for me (maybe needs to be export?).
# Used to depend on targets "clean all", but I want to run this in "all".
test: .test-timestamp

# Using compile_notest instead of .class-files-timestamp as the
# prerequisite doesn't work.
.test-timestamp: .class-files-timestamp
	which java
# This is failing without the "-cp ${CLASSPATH}"; will that argument help?
	set JAVA_COMPILER=NONE; ${JAVA} -cp ${CLASSPATH} utilMDE.TestUtilMDE
	touch .test-timestamp

run-chicory:
	set JAVA_COMPILER=NONE; ${JAVA} -ea daikon.Chicory --output-dir=/tmp -- utilMDE.TestUtilMDE --shortrun

run-chicory-arrays:
	set JAVA_COMPILER=NONE; ${JAVA} -ea daikon.Chicory --ppt-select-pattern=ArraysMDE --output-dir=/tmp utilMDE.TestUtilMDE --shortrun


doc: javadoc

javadoc: $(JAVA_FILES)
	mkdir -p doc
	javadoc -d doc utilMDE
	# jdk-add-munged-anchor-names doc/utilMDE/*.html
	@echo "Optionally run jdk-index-to-alist (for Emacs support)"

showvars:
	@echo JAVA_AND_CPP_FILES = $(JAVA_AND_CPP_FILES)
	@echo JAVA_FILES = $(JAVA_FILES)
	@echo JAVA_LINT_FILES = $(JAVA_LINT_FILES)
	@echo JAVA_CPP_FILES = $(JAVA_CPP_FILES)
	@echo CLASSPATH = "$(CLASSPATH)"
	@echo jsr308_imports = $(jsr308_imports)


###########################################################################
### Automatically generated files
###

JAVA_CPP_PL = ../../scripts/java-cpp.pl
# JAVA_CPP = java-cpp.pl $< -DDEFINEDVAR > $@
# This rule does not work with make 3.77, but does work with make 3.78.
JAVA_CPP ?= (rm -f $@; ${JAVA_CPP_PL} -DDEFINEDVAR $< 2>&1 > $@ && chmod -w $@)

MathMDE.java: MathMDE.java.jpp MathMDE-gcd.java.jpp MathMDE-modulus.java.jpp MathMDE-nonmodulus.java.jpp MathMDE-help.java.jpp MathMDE-helpend.java.jpp
	$(JAVA_CPP)


###########################################################################
### Jar files and distribution
###

## TODO:  the .jar files other than utilMDE.jar should be trimmed, with
## extraneous components removed.


jar: utilMDE.jar
utilMDE.jar: .class-files-timestamp svnkit.jar ini4j-0.5.1.jar lookup.html lookup.txt
# "make clean" avoids including stray .class files from your directory.
# "make compile" runs unit tests.
	$(MAKE) clean compile
	rm -rf jar-contents
	mkdir jar-contents
	mkdir jar-contents/utilMDE
	cp -p *.class jar-contents/utilMDE
	(cd jar-contents; jar xf ../svnkit.jar)
	(cd jar-contents; jar xf ../ini4j-0.5.1.jar)
	(cd jar-contents; jar xf ${JDKDIR}/lib/tools.jar com/sun/javadoc)
	rm -rf jar-contents/meta-inf jar-contents/META-INF
# Put contents in alphabetical order.
	(cd jar-contents; jar cf ../utilMDE.jar `find * -type f | sort`)
	rm -rf jar-contents

#
# Lookup targets
#
lookup.html: Lookup.java
	javadoc -J-ea -quiet -doclet utilMDE.Lookup $< > $@
lookup.txt: lookup.html html2textrc
	html2text -rcfile html2textrc < $< > $@

lookup.jar : utilMDE.jar lookup.manifest
	rm -rf jar-contents
	mkdir jar-contents
	(cd jar-contents; jar xf ../utilMDE.jar)
	rm -rf jar-contents/meta-inf jar-contents/META-INF
	(cd jar-contents; $(JAR) cfm ../lookup.jar ../lookup.manifest `find * -type f | sort`)
	rm -rf jar-contents

install-lookup: $(pag)/software/pkg/lookup.jar
$(pag)/software/pkg/lookup.jar: lookup.jar
	cp -p $< $>

#
# TaskManager targets
#
TASKMANAGER_JAVA_FILES := $(shell find . -name '*.java')
task-manager : task_manager.jar
task_manager.jar: utilMDE.jar task_manager.manifest
	rm -rf jar-contents
	mkdir jar-contents
	(cd jar-contents; jar xf ../utilMDE.jar)
	rm -rf jar-contents/meta-inf jar-contents/META-INF
	(cd jar-contents; $(JAR) cfm ../task_manager.jar ../task_manager.manifest `find * -type f | sort`)
	rm -rf jar-contents

install-task-manager: $(pag)/software/pkg/task_manager.jar
$(pag)/software/pkg/task_manager.jar: task_manager.jar
	cp -p $< $>


# No need to do "make doc" or "make all" before running this.
# (This includes .class files, so users don't need to recompile.)
utilMDE.tar.gz: $(JAVA_AND_CPP_FILES) Makefile README
	rm -rf /tmp/utilMDE
	mkdir /tmp/utilMDE
	cp -p $(JAVA_AND_CPP_FILES) /tmp/utilMDE
	cp -p Makefile README /tmp/utilMDE
	(cd /tmp/utilMDE; make compile_notest doc)
	(cd /tmp; tar zcf utilMDE.tar.gz utilMDE)
	rm -f utilMDE.tar.gz
	mv /tmp/utilMDE.tar.gz .
	rm -rf /tmp/utilMDE



###########################################################################
### Pluggable type-checking
###

JAVAC_NULLNESS_ARGS = -processor checkers.nullness.NullnessChecker -implicit:class -Xlint:-processing
JAVAC_NULLNESS_STUBS = -sourcepath ${CHECKERS}/jdk/nullness/src

check-nullness: check-nullness-utilmde

check-nullness-utilmde: check-nullness-utilmde-withstubs

check-nullness-utilmde-withstubs:
	${JAVAC_COMMAND} ${JAVAC_NULLNESS_ARGS} ${JAVAC_NULLNESS_STUBS} ${JAVA_FILES}



###########################################################################
### Makefile bookkeeping
###

.PHONY: default all compile_notest jar clean very_clean tags test run-chicory javadoc showvars doc task-manager install-task-manager install-lookup


# Existence of this rule means that a target is deleted if has just changed
# and its rule commands exit with nonzero status.  It's needed for the
# java-cpp rules above.
# (Actually, it isn't anymore, for I arrange not to overwrite the file
# unless success.  But leave it in for now anyway.)

.DELETE_ON_ERROR:

# end
