JAVA_FILES := $(shell find . -name '*.java' -print)

all:	all_via_daikon

all_via_daikon:
	jikes -g +E +F Daikon.java

all_directly: 
	jikes -g +E +F ${JAVA_FILES}

all_force:	all_directly

all_javac:
	javac -g ${JAVA_FILES}

# Wrong, becaue daikon.class might be up to date even if other files aren't
daikon.class:
	jikes -g +E +F daikon.java

run:	all
	java daikon.Daikon

TAGS:	tags

tags:
	etags ${JAVA_FILES} *.py /homes/gws/mernst/java/utilMDE/*.java

doc: $(JAVA_FILES)
	javadoc -d doc daikon
	jdk-add-munged-anchor-names doc/daikon/*.html


# "(cd $inv; maudit daikon)" does not work; I don't understand why.
# Unfortunately, this is way too much output; I need to filter it.
# Maybe write my own script to do that...
audit:
	cd $(HOME)/java && maudit daikon | perl -p -e 's/^daikon\///'
