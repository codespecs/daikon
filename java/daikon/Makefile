UTILMDE_DIR := /g2/users/mernst/java/utilMDE
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
	rm -f `find . -name "*.class" -print`

dist_clean: clean
	rm -f TAGS

very_clean: dist_clean
	rm -f ${AUTO_GENERATED_FILES}

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

inv/scalar/LowerBound.java: inv/scalar/Bound-cpp.java
	-mv -f $@ $@-bak
	java-cpp $< -DLOWER > $@
	chmod -w $@

inv/scalar/UpperBound.java: inv/scalar/Bound-cpp.java
	-mv -f $@ $@-bak
	java-cpp $< -DUPPER > $@
	chmod -w $@

inv/scalar/OneOfScalar.java: inv/OneOf-cpp.java
	-mv -f $@ $@-bak
	java-cpp $< -DSCALAR > $@
	chmod -w $@

inv/string/OneOfString.java: inv/OneOf-cpp.java
	-mv -f $@ $@-bak
	java-cpp $< -DSTRING > $@
	chmod -w $@

inv/sequence/OneOfSequence.java: inv/OneOf-cpp.java
	-mv -f $@ $@-bak
	java-cpp $< -DSEQUENCE > $@
	chmod -w $@

inv/sequence/EltOneOf.java: inv/OneOf-cpp.java
	-mv -f $@ $@-bak
	java-cpp $< -DELT > $@
	chmod -w $@

PptSlice1.java: PptSlice-cpp.java
	-mv -f $@ $@-bak
	java-cpp $< -DARITY1 > $@
	chmod -w $@

PptSlice2.java: PptSlice-cpp.java
	-mv -f $@ $@-bak
	java-cpp $< -DARITY2 > $@
	chmod -w $@

PptSlice3.java: PptSlice-cpp.java
	-mv -f $@ $@-bak
	java-cpp $< -DARITY3 > $@
	chmod -w $@

