JAVA_FILES := $(shell find . -name '*.java' -print)

all:
	jikes -g +E +F *.java

clean:
	rm -f *.class

tags:
	etags *.java

# Run Jikes unconditionally.
# This "set" form doesn't seem to work for me.
test:	clean all
	set JAVA_COMPILER=NONE; java utilMDE.TestUtilMDE

doc: $(JAVA_FILES)
	mkdir -p doc
	javadoc -d doc utilMDE
	touch doc
	jdk-add-munged-anchor-names doc/utilMDE/*.html
	@echo "Run jdk-index-to-alist (see ~/bin/share/jdk-index-to-alist)"

audit:
	(cd ..; maudit utilMDE)

# No need to do "make doc" or "make all" before running this.
# (This includes .class files, so users don't need to recompile.)
utilMDE.tar.gz: $(JAVA_FILES) Makefile README
	rm -rf /tmp/utilMDE
	mkdir /tmp/utilMDE
	cp -p $(JAVA_FILES) /tmp/utilMDE
	cp -p Makefile README /tmp/utilMDE	
	(cd /tmp/utilMDE; make all doc)
	(cd /tmp; tar zcf utilMDE.tar.gz utilMDE)
	rm -f utilMDE.tar.gz
	mv /tmp/utilMDE.tar.gz .
	rm -rf /tmp/utilMDE
