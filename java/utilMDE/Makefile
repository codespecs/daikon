### User customizations

# Put user-specific changes in your own Makefile.user
#  (such as changes to UTILMDE_DIR or JAVAC).
# Make will silently continue if file does not exist.
-include Makefile.user

JAVA ?= java
JAVAC ?= jikes -g +E +F


JAVA_FILES := $(shell find . -name '*.java' -print)
JAVA_CPP_FILES := $(shell find . -name '*.java.jpp' -print)
ALL_JAVA_FILES := ${JAVA_FILES} ${JAVA_CPP_FILES}
AUTO_GENERATED_FILES = \
	MathMDE.java

default: all

all: all_notest test

all_javac: all_javac_notest test

all_javac_notest:  ${AUTO_GENERATED_FILES}
	javac -g ${JAVAC_EXTRA_ARGS} ${JAVA_FILES}

all_notest: ${AUTO_GENERATED_FILES}
	${JAVAC} ${JAVA_FILES}

clean:
	rm -f *.class

very_clean: clean
	rm -f ${AUTO_GENERATED_FILES}

tags:
	etags ${ALL_JAVA_FILES}

# Run Jikes unconditionally.
# This "set" form doesn't seem to work for me (maybe needs to be export?).
# Used to depend on targets "clean all", but I want to run this in "all".
test:
	set JAVA_COMPILER=NONE; ${JAVA} utilMDE.TestUtilMDE

doc: $(JAVA_FILES)
	mkdir -p doc
	javadoc -d doc utilMDE
	# makes "doc" (the target of this rule) look up to date
	touch doc
	jdk-add-munged-anchor-names-no-bak doc/utilMDE/*.html
	@echo "Run jdk-index-to-alist (see ~/bin/share/jdk-index-to-alist)"

audit:
	(cd ..; maudit utilMDE)

# No need to do "make doc" or "make all" before running this.
# (This includes .class files, so users don't need to recompile.)
utilMDE.tar.gz: $(ALL_JAVA_FILES) Makefile README
	rm -rf /tmp/utilMDE
	mkdir /tmp/utilMDE
	cp -p $(ALL_JAVA_FILES) /tmp/utilMDE
	cp -p Makefile README /tmp/utilMDE
	(cd /tmp/utilMDE; make JAVAC_EXTRA_ARGS='-classpath .:/SDG/g2/users/mernst/research/invariants/java/lib/junit.jar' all_javac_notest doc)
	(cd /tmp; tar zcf utilMDE.tar.gz utilMDE)
	rm -f utilMDE.tar.gz
	mv /tmp/utilMDE.tar.gz .
	rm -rf /tmp/utilMDE

showvars:
	@echo ALL_JAVA_FILES = $(ALL_JAVA_FILES)
	@echo JAVA_FILES = $(JAVA_FILES)
	@echo JAVA_CPP_FILES = $(JAVA_CPP_FILES)


###########################################################################
### Automatically generated files
###

JAVA_CPP = java-cpp.pl $< -DDEFINEDVAR > $@

MathMDE.java: MathMDE.java.jpp MathMDE-gcd.java.jpp MathMDE-modulus.java.jpp MathMDE-nonmodulus.java.jpp MathMDE-help.java.jpp MathMDE-helpend.java.jpp
	-mv -f $@ $@-bak
	java-cpp.pl $< > $@
	chmod -w $@


###########################################################################
###
###

# Existence of this rule means that a target is deleted if has just changed
# and its rule commands exit with nonzero status.  It's needed for the
# java-cpp rules above.
# (Actually, it isn't anymore, for I arrange not to overwrite the file
# unless success.  But leave it in for now anyway.)

.DELETE_ON_ERROR:

# end
