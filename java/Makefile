# This Makefile delegates most work to the daikon/ subdirectory.
# (That might change eventually, but for now, issuing commands there is a
# more direct way to get your work done.)

JAVA_FILES := $(shell find . -name '*.java' -print)

default %:
	cd daikon && $(MAKE) $@

.PHONY : doc

doc: $(JAVA_FILES)
	mkdir -p doc
	javadoc -d doc `find -name '*.java' -exec dirname {} \; | grep -v CVS | grep -v ajax-ship | grep -v old-ajax | sort | uniq | perl -p -e 's:^./::g' | perl -p -e 's:/:.:g'`
	@#jdk-add-munged-anchor-names-no-bak `find doc -name '*.html'`
	@#echo "Run jdk-index-to-alist > ~/emacs/jdk-index.el"
