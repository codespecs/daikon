JAVA_FILES := $(shell find . -name '*.java' -print)

all:
	jikes -g +E +F *.java

clean:
	rm -f *.class

tags:
	etags *.java

# Run Jikes unconditionally
test:	clean all
	set JAVA_COMPILER=NONE; java utilMDE.TestUtilMDE

doc: $(JAVA_FILES)
	javadoc -d doc utilMDE
	jdk-add-munged-anchor-names doc/utilMDE/*.html

audit:
	(cd ..; maudit utilMDE)
