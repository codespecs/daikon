UTILMDE_DIR := $(HOME)/java/utilMDE
JAVA_FILES := $(shell find . \( -name '*daikon-java*' -o -name '*-cpp.java' -o -name CVS \) -prune -o -name '*.java' -print)
TAG_FILES := $(shell find . \( -name '*daikon-java*' -o -name CVS \) -prune -o -name '*.java' -print)
# Should these really be targets in the below, or should I use "make
# ${AUTO_GENERATED_FILES}"? I don't want to omit running just because these
# are up-to-date.
AUTO_GENERATED_FILES = \
	inv/scalar/LowerBound.java \
	inv/scalar/UpperBound.java \
	inv/scalar/OneOfScalar.java \
	inv/string/OneOfString.java \
	inv/sequence/OneOfSequence.java \
	inv/sequence/EltOneOf.java \
	PptSlice1.java \
	PptSlice2.java \
	PptSlice3.java

all:	all_via_daikon

all_via_daikon:	Runtime.class Diff.class ${AUTO_GENERATED_FILES}
	jikes -g +E +F Daikon.java

all_directly: ${AUTO_GENERATED_FILES}
	jikes -g +E +F ${JAVA_FILES}

all_force:	all_directly

all_via_javac: ${AUTO_GENERATED_FILES}
	javac -g ${JAVA_FILES}

all_javac:	all_via_javac

Runtime.class: Runtime.java
	jikes -g +E +F Runtime.java

Diff.class: Diff.java
	jikes -g +E +F Diff.java

# Wrong, becaue Daikon.class might be up to date even if other files aren't
# Daikon.class:
# 	jikes -g +E +F Daikon.java

# It's good to do this occasionally, because it costs little to regenerate
# .class files and it's bad to continue to use an orphaned class file
# (whose source file was renamed).  Or, run "orphaned-class-files".
clean:
	-rm -f `find . -name "*.class" -print`

dist_clean: clean
	-rm -f TAGS

very_clean: dist_clean
	-rm -f ${AUTO_GENERATED_FILES}

run:	all
	java daikon.Daikon

TAGS:	tags

# Used to include "*.py", but no such in daikon/ directory.
# I could make this ../*.py, but probably don't want to include that old code.
tags:
	etags ${TAG_FILES} $(UTILMDE_DIR)/*.java

doc: $(JAVA_FILES)
	javadoc -d doc `find -name '*.java' -exec dirname {} \; | sort | uniq | perl -p -e 's/^./daikon/g' | perl -p -e 's:/:.:g'`
	jdk-add-munged-anchor-names doc/daikon/*.html
	@echo "Run jdk-index-to-alist > ~/emacs/jdk-index.el"

lint:	jlint

jlint:
	jlint . | grep -v "Value of referenced variable '.*' may be NULL" | grep -v "Compare strings as object references" | perl -p -e 's:^daikon/::'

# "(cd $inv; maudit daikon)" does not work; I don't understand why.
# Unfortunately, this is way too much output; I need to filter it.
# Maybe write my own script to do that...
audit:
	cd $(HOME)/java && maudit daikon | perl -p -e 's/^daikon\///'

showvars:
	@echo JAVA_FILES = $(JAVA_FILES)

list_java_files:
	@echo $(JAVA_FILES)


###########################################################################

## Automatically generated files


# ":=" does not work here; the variables get expanded too soon.
# "-e" means exit immediately if any error, rather than returning 
# the status of the last command.

JAVA_CPP = (cp -pf $@ $@-bak; set -e; java-cpp $< -DDEFINEDVAR > $@-postproc; mv -f $@-postproc $@; chmod -w $@)

## Here are alternate ways of writing the same command:
## I could use "define", but that's specific to GNU Make
## I can't do this because the CPP defined variable changes.
# JAVA_CPP = (mv -f $@ $@-bak; set -e; java-cpp $< -DDEFINEDVAR > $@; chmod -w $@)
## These are used like:	$(JAVA_CPP_START) -DLOWER $(JAVA_CPP_END)
# JAVA_CPP_START = (cp -pf $@ $@-bak; set -e; java-cpp $<
# JAVA_CPP_END = > $@-postproc; mv -f $@-postproc $@; chmod -w $@)


inv/scalar/LowerBound.java: inv/scalar/Bound-cpp.java
	$(JAVA_CPP:DEFINEDVAR=LOWER)

inv/scalar/UpperBound.java: inv/scalar/Bound-cpp.java
	$(JAVA_CPP_START) -DUPPER $(JAVA_CPP_END)

inv/scalar/OneOfScalar.java: inv/OneOf-cpp.java
	$(JAVA_CPP_START) -DSCALAR $(JAVA_CPP_END)

inv/string/OneOfString.java: inv/OneOf-cpp.java
	$(JAVA_CPP_START) -DSTRING $(JAVA_CPP_END)

inv/sequence/OneOfSequence.java: inv/OneOf-cpp.java
	$(JAVA_CPP_START) -DSEQUENCE $(JAVA_CPP_END)

inv/sequence/EltOneOf.java: inv/OneOf-cpp.java
	$(JAVA_CPP_START) -DELT $(JAVA_CPP_END)

PptSlice1.java: PptSlice-cpp.java
	$(JAVA_CPP:DEFINEDVAR=ARITY1)

PptSlice2.java: PptSlice-cpp.java
	$(JAVA_CPP_START) -DARITY2 $(JAVA_CPP_END)

PptSlice3.java: PptSlice-cpp.java
	$(JAVA_CPP_START) -DARITY3 $(JAVA_CPP_END)


###########################################################################
###
###

# Existence of this rule means that a target is deleted if has just changed
# and its rule commands exit with nonzero status.  It's needed for the
# java-cpp rules above.
# (Actually, it isn't anymore, for I arrange not to overwrite the file
# unless success.  But leave it in for now anyway.)

.DELETE_ON_ERROR:
