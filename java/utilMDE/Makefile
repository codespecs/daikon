### User customizations

# Put user-specific changes in your own Makefile.user
#  (such as changes to UTILMDE_DIR or JAVAC).
# Make will silently continue if file does not exist.
-include Makefile.user

JAVAC ?= jikes -g +E +F


JAVA_FILES := $(shell find . -name '*.java' -print)
JAVA_CPP_FILES := $(shell find . -name '.java.cpp' -print)
ALL_JAVA_FILES := ${JAVA_FILES} ${JAVA_CPP_FILES}
AUTO_GENERATED_FILES = \
	MathMDE.java

all: ${AUTO_GENERATED_FILES}
	${JAVAC} ${JAVA_FILES}

all_javac: ${AUTO_GENERATED_FILES}
	javac -g ${JAVAC_EXTRA_ARGS} ${JAVA_FILES}

clean:
	rm -f *.class

very_clean: clean
	rm -f ${AUTO_GENERATED_FILES}

tags:
	etags ${ALL_JAVA_FILES}

# Run Jikes unconditionally.
# This "set" form doesn't seem to work for me.
test:	clean all
	set JAVA_COMPILER=NONE; java utilMDE.TestUtilMDE

doc: $(JAVA_FILES)
	mkdir -p doc
	javadoc -d doc utilMDE
	# makes "doc" (the target of this rule) look up to date
	touch doc
	jdk-add-munged-anchor-names-no-bak doc/utilMDE/*.html
	@echo "Run jdk-index-to-alist (see ~/bin/share/jdk-index-to-alist)"

# Old version
# # I do this in another directory to prevent javadoc from getting confused
# # by the -cpp.java files, which aren't legal Java source.
# doc: $(JAVA_FILES)
# 	rm -rf /tmp/utilMDE-doc
# 	mkdir -p /tmp/utilMDE-doc/utilMDE
# 	cp -p $(JAVA_FILES) /tmp/utilMDE-doc/utilMDE
# 	mkdir -p doc
# 	javadoc -sourcepath /tmp/utilMDE-doc -d doc utilMDE
# 	# makes "doc" (the target of this rule) look up to date
# 	touch doc
# 	jdk-add-munged-anchor-names-no-bak doc/utilMDE/*.html
# 	@echo "Run jdk-index-to-alist (see ~/bin/share/jdk-index-to-alist)"
# 	rm -rf /tmp/utilMDE-doc

audit:
	(cd ..; maudit utilMDE)

# No need to do "make doc" or "make all" before running this.
# (This includes .class files, so users don't need to recompile.)
utilMDE.tar.gz: $(JAVA_FILES) Makefile README
	rm -rf /tmp/utilMDE
	mkdir /tmp/utilMDE
	cp -p $(ALL_JAVA_FILES) /tmp/utilMDE
	cp -p Makefile README /tmp/utilMDE	
	(cd /tmp/utilMDE; make all doc)
	(cd /tmp; tar zcf utilMDE.tar.gz utilMDE)
	rm -f utilMDE.tar.gz
	mv /tmp/utilMDE.tar.gz .
	rm -rf /tmp/utilMDE

showvars:
	@echo ALL_JAVA_FILES = $(ALL_JAVA_FILES)
	@echo JAVA_FILES = $(JAVA_FILES)


###########################################################################
### Automatically generated files
###

JAVA_CPP = java-cpp.pl $< -DDEFINEDVAR > $@

MathMDE.java: MathMDE.java.cpp MathMDE-gcd.java.cpp MathMDE-modulus.java.cpp MathMDE-nonmodulus.java.cpp  MathMDE-help.java.cpp MathMDE-helpend.java.cpp
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
