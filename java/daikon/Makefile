JAVA_FILES := $(shell find . -name '*.java' -print)

all:	all_via_daikon

all_via_daikon:	Runtime.class
	jikes -g +E +F Daikon.java

all_directly: 
	jikes -g +E +F ${JAVA_FILES}

all_force:	all_directly

all_javac:
	javac -g ${JAVA_FILES}

Runtime.class: Runtime.java
	jikes -g +E +F Runtime.java

# Wrong, becaue Daikon.class might be up to date even if other files aren't
# Daikon.class:
# 	jikes -g +E +F Daikon.java

# It's good to do this occasionally, because it costs little to regenerate
# .class files and it's bad to continue to use an orphaned class file
# (whose source file was renamed).  Or, run "orphaned_class_files".
clean:
	rm -f `find . -name "*.class" -print`

run:	all
	java daikon.Daikon

TAGS:	tags

tags:
	etags ${JAVA_FILES} *.py /homes/gws/mernst/java/utilMDE/*.java

doc: $(JAVA_FILES)
	javadoc -d doc daikon
	jdk-add-munged-anchor-names doc/daikon/*.html

lint:	jlint

jlint:
	jlint . | grep -v "Value of referenced variable '.*' may be NULL" | grep -v "Compare strings as object references" | perl -p -e 's:^daikon/::'

# "(cd $inv; maudit daikon)" does not work; I don't understand why.
# Unfortunately, this is way too much output; I need to filter it.
# Maybe write my own script to do that...
audit:
	cd $(HOME)/java && maudit daikon | perl -p -e 's/^daikon\///'
