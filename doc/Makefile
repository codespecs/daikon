all: all-except-pdf pdf
all-except-pdf: config-options.texinfo invariants-doc.texinfo html-chap html

TEXI2HTML ?= makeinfo --html

HTMLTOOLS ?= ../utils/html-tools

daikon.info: daikon.texinfo config-options.texinfo invariants-doc.texinfo
# Large split size fixes an apparent bug in makeinfo 4.11:
# failing searches say
#   Search failed: "  Indirect:"
# instead of proper message.
	makeinfo --split-size=1000000 $<

developer.info: developer.texinfo
	makeinfo $<

ifdef SKIP_JAVA
# If you know you haven't changed the Java code, or you don't care
# about the parts of the documentation that are automatically
# generated from it, running with SKIP_JAVA=1 will skip regenerating
# the code-derived texinfo, which in turn avoids recompiling all of
# Daikon, regenerating the splitter tests, and running the Daikon unit
# tests.
config-options.texinfo:
invariants-doc.texinfo:
else
config-options.texinfo:
	cd ../java && $(MAKE) ../doc/config-options.texinfo

invariants-doc.texinfo:
	cd ../java && $(MAKE) ../doc/invariants-doc.texinfo

# Always remake config-options.texinfo
# (We delegate to ../java/Makefile, which is more discerning)
.PHONY : config-options.texinfo
.PHONY : invariants-doc.texinfo
endif

images/%.eps: images/%.dot
	dot -Tps $< >$@

# Create a jpg version of the dfepl flow, blow up by 4X to get cleaner
# output.
images/dfepl-flow.jpg: images/dfepl-flow.eps
	pstopnm -portrait -stdout -xsize 2000 $<  \
	  | pnmcrop \
	  | pnmscale 0.25 \
	  | pnmmargin -white 20 | pnmtojpeg > images/dfepl-flow.jpg

images/%.pdf: images/%.eps
	epstopdf $< --outfile=$@

.PHONY: html
html: daikon.html developer.html

daikon.html: daikon.texinfo config-options.texinfo invariants-doc.texinfo images/dfepl-flow.jpg
	$(MAKE) daikon-html

# Just builds .html from .texinfo.
# Much faster than rebuilding the .texinfo files from Javadoc.
daikon-fast: daikon.texinfo
	$(MAKE) daikon-html

## These two rules create the directories if absent but don't update them.
../utils/html-tools:
	make -C .. update-html-tools

../utils/plume-scripts:
	make -C .. update-plume-scripts

.PHONY: daikon-html
# Does the actual work
daikon-html: ../utils/html-tools
	${TEXI2HTML} --no-split daikon.texinfo
	# Fixup 'bad' href inserted by makeinfo.
	perl -pi -e 's|dir.html#Top|index.html|;' daikon.html
	${HTMLTOOLS}/html-add-favicon . daikon-favicon.png

developer.html: developer.texinfo ../utils/html-tools
	${TEXI2HTML} --no-split $<
	# Fixup 'bad' href inserted by makeinfo.
	perl -pi -e 's|dir.html#Top|index.html|;' $@
	${HTMLTOOLS}/html-add-favicon . daikon-favicon.png

html-chap: daikon/index.html developer/index.html

# The "subdir" flag does not copy over images, so the
# pathnames become incorrect.  We need to copy them over ourselves.
daikon/index.html: daikon.texinfo config-options.texinfo invariants-doc.texinfo images/dfepl-flow.jpg ../utils/html-tools
	rm -rf daikon/*
	${TEXI2HTML} --split=chapter -o daikon $<
	# Fixup 'bad' href(s) inserted by makeinfo.
	perl -pi -e 's|../dir/index.html|../index.html|;' daikon/index.html
	perl -pi -e 's|href="CHANGELOG.md"|href="../CHANGELOG.md"|;' daikon/*.html
	mkdir daikon/images
	cp images/*.png daikon/images
	cp images/*.jpg daikon/images
	cd daikon && ../${HTMLTOOLS}/html-add-favicon . ../daikon-favicon.png

developer/index.html: developer.texinfo ../utils/html-tools
	rm -rf developer/*
	${TEXI2HTML} --split=chapter -o developer $<
	# Fixup 'bad' href inserted by makeinfo.
	perl -pi -e 's|../dir/index.html|../index.html|;' developer/index.html
	mkdir developer/images
	cp images/*.png developer/images
	cp images/*.jpg developer/images
	cd developer && ../${HTMLTOOLS}/html-add-favicon . ../daikon-favicon.png

pdf: daikon.pdf images/dfepl-flow.eps developer.pdf

daikon.pdf: daikon.texinfo config-options.texinfo invariants-doc.texinfo
	makeinfo --pdf $<

developer.pdf: developer.texinfo
	makeinfo --pdf $<

# Note that the clean target does not remove invariants-doc.texinfo or
# config-options.texinfo.

clean: texi-clean
	rm -f daikon.tmp \
		daikon.info daikon.info-[0-9] \
		daikon.log daikon.dvi daikon.pdf daikon.html
	rm -f *.bak
	rm -rf daikon/*
	rm -f developer.tmp \
		developer.info developer.info-[0-9] \
		developer.log developer.dvi developer.pdf developer.html
	rm -rf developer/*
	rm -f images/dfepl-flow.{eps,pdf,jpg}
# Obsolete files
	rm -f daikon.ps developer.ps

texi-clean:
	rm -rf daikon.aux daikon.cp daikon.cps daikon.fn daikon.fns \
		   daikon.ky  daikon.pg daikon.toc daikon.tp daikon.vr
	rm -rf developer.aux developer.cp developer.cps developer.fn developer.fns \
		   developer.ky  developer.pg developer.toc developer.tp developer.vr

very-clean: clean
	rm -rf config-options.texinfo invariants-doc.texinfo
	rm -rf www/pubs

# Remakes entire distribution; use with caution.
dist:
	cd .. && $(MAKE) dist

# "www" target wouldn't work from here, because "www" is an (existing)
# directory; but go to the parent directory and run the rule from there.
# www:
# 	cd .. && $(MAKE) www

.PHONY: find-changes spell-check
find-changes:
	diff -b -u -s --from-file /cse/web/research/plse/daikon/download/doc *.texinfo

# -u switch to hunspell is undocumented and subject to change,
# but gives a line number whereas -l does not.
spell-check:
	perl prepare-texinfo-for-spellcheck.pl < daikon.texinfo | hunspell -u -p daikon.dict
	perl prepare-texinfo-for-spellcheck.pl < developer.texinfo | hunspell -u -p daikon.dict
# For CHANGELOG.md file, only recent lines (top 100), and not text starting with
# 4+ spaces (assumed to be literal code or commands.  Permit some failures.
	@echo "The following command is allowed to have some failures not in daikon.dict"
	head -100 CHANGELOG.md | sed 's/^    .*/ /' | hunspell -u -p daikon.dict
