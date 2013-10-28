all: config-options invariants-doc html html-chap ps pdf

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
config-options:
invariants-doc:
else
config-options: ../java/daikon/util/SimpleLog.class
	cd ../java && $(MAKE) ../doc/config-options.texinfo

invariants-doc:
	cd ../java && $(MAKE) ../doc/invariants-doc.texinfo

# Always remake config-options.texinfo
# (We delegate to ../java/Makefile, which more discerning)
.PHONY : config-options
.PHONY : invariants-doc
endif

../java/daikon/util/SimpleLog.class: ../java/daikon/util/SimpleLog.java
	make -C ../java

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

html: daikon.html developer.html

daikon.html: daikon.texinfo config-options.texinfo invariants-doc.texinfo images/dfepl-flow.jpg
	makeinfo --html --no-split $<

# just builds from texinfo.  Much faster than rebuilding the files from javadoc
daikon-fast: daikon.texinfo 
	makeinfo --html --no-split $<

developer.html: developer.texinfo 
	makeinfo --html --no-split $<

html-chap: daikon_manual_html/daikon.html developer_manual_html/developer.html

# The "subdir" flag does not copy over images aren't copied, so the
# pathnames become incorrect.  I need to copy them over myself.
# Images are in the order of appearance in the manual (search for "@image",
# except daikon-logo.jpg; this list is updated by hand (unfortunately).
daikon_manual_html/daikon.html: daikon.texinfo
	rm -rf daikon_manual_html/*
# Alternatively, run "makeinfo -html", but it doesn't support "-split chapter",
# only split into nodes or --no-split
# However, as of texi2html, the cross-references seem to assume the other
# document was processed without "-split chapter", instead splitting into nodes.
	texi2html -split chapter -subdir daikon_manual_html $<
	perl -pi -e '$$/ = "\n\n"; if (m|http://www\.nongnu\.org/texi2html|) { $$_ = ""; }' daikon_manual_html/*.html

developer_manual_html/developer.html: developer.texinfo
	rm -rf developer_manual_html/*
# Alternatively, run "makeinfo -html", but it doesn't support "-split chapter",
# only split into nodes or --no-split
# However, as of texi2html, the cross-references seem to assume the other
# document was processed without "-split chapter", instead splitting into nodes.
	texi2html -split chapter -subdir developer_manual_html $<
	perl -pi -e '$$/ = "\n\n"; if (m|http://www\.nongnu\.org/texi2html|) { $$_ = ""; }' developer_manual_html/*.html

daikon.dvi: daikon.texinfo config-options.texinfo invariants-doc.texinfo
	texi2dvi $<

developer.dvi: developer.texinfo
	texi2dvi $<

ps: daikon.ps images/dfepl-flow.eps developer.ps

daikon.ps: daikon.dvi
	dvips -t letter -o $@ $<

developer.ps: developer.dvi
	dvips -t letter -o $@ $<

pdf: daikon.pdf developer.pdf

pdf-direct: daikon.texinfo daikon.dvi images/daikon-logo.pdf images/dfepl-flow.pdf
	pdftex $<

daikon.pdf: daikon.dvi images/dfepl-flow.eps
	dvipdfm $<

developer.pdf: developer.dvi
	dvipdfm $<

pdf-final: daikon.texinfo config-options.texinfo invariants-doc.texinfo developer.texinfo
	texi2dvi --command='@set final' daikon.texinfo
	dvipdfm daikon.dvi
	texi2dvi --command='@set final' developer.texinfo
	dvipdfm developer.dvi

# Note that the clean target does not remove invariants-doc.texinfo or
# config-options.texinfo.

clean: texi-clean
	rm -f daikon.tmp \
		daikon.info daikon.info-[0-9] \
		daikon.log daikon.dvi daikon.ps daikon.pdf daikon.html 
	rm -f *.bak
	rm -rf daikon_manual_html
	rm -f developer.tmp \
		developer.info developer.info-[0-9] \
		developer.log developer.dvi developer.ps developer.pdf developer.html 
	rm -rf developer_manual_html
	rm -f images/dfepl-flow.{eps,pdf,jpg}

texi-clean:
	rm -rf daikon.aux daikon.cp daikon.cps daikon.fn daikon.fns \
		   daikon.ky  daikon.pg daikon.toc daikon.tp daikon.vr
	rm -rf developer.aux developer.cp developer.cps developer.fn developer.fns \
		   developer.ky  developer.pg developer.toc developer.tp developer.vr

# Remakes entire distribution; use with caution.
dist:
	cd .. && $(MAKE) dist

# "www" target wouldn't work from here, because "www" is an (existing)
# directory; but go to the parent directory and run the rule from there.
# www:
# 	cd .. && $(MAKE) www

