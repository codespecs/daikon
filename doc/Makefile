all: config-options invariants-doc info html html-chap ps pdf


info: daikon.info developer.info

daikon.info: daikon.texinfo config-options.texinfo invariants-doc.texinfo \
			 .fix-menu
	makeinfo $<

developer.info: developer.texinfo .fix-dev-menu
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
config-options:
	cd ../java && $(MAKE) ../doc/config-options.texinfo

invariants-doc:
	cd ../java && $(MAKE) ../doc/invariants-doc.texinfo

# Always remake config-options.texinfo (../java/Makefile is more discerning)
.PHONY : config-options
.PHONY : invariants-doc
endif

# Don't want to make this a target of other rules because it updates the modification date.
# Make forces me to double the dollar signs in the Perl code.
# sh seems not to permit redirecting standard error to standard out
.fix-menu: daikon.texinfo
	@echo "Updating menus in daikon.texinfo..."
	@cp daikon.texinfo daikon.texinfo.new
	@emacs --batch daikon.texinfo.new --eval '(progn (texinfo-master-menu t) (save-buffer))' 2>&1 | grep -v "^Updating node: " | grep -v '^Updated ".*" level menu' | grep -v "^Inserting menu for" | grep -v '^Inserting chapter menu entry:'
	perl -pi -e '$$/="\n\n"; s/(::) *(\@ifhtml\n\@contents\n)/\1\n\n\2\@end ifhtml\n\@ifnothtml\n/; s/(\@end detailmenu\n)(\@end menu)/\1\@end ifnothtml\n\2/' daikon.texinfo.new
	@if cmp daikon.texinfo daikon.texinfo.new >/dev/null 2>&1; then echo "daikon.texinfo unchanged"; rm -f daikon.texinfo.new; else mv daikon.texinfo.new daikon.texinfo; fi
	touch .fix-menu

.fix-dev-menu: developer.texinfo
	@echo "Updating menus in developer.texinfo..."
	@cp developer.texinfo developer.texinfo.new
	@emacs --batch developer.texinfo.new --eval '(progn (texinfo-master-menu t) (save-buffer))' 2>&1 | grep -v "^Updating node: " | grep -v '^Updated ".*" level menu' | grep -v "^Inserting menu for" | grep -v '^Inserting chapter menu entry:'
	perl -pi -e '$$/="\n\n"; s/(::) *(\@ifhtml\n\@contents\n)/\1\n\n\2\@end ifhtml\n\@ifnothtml\n/; s/(\@end detailmenu\n)(\@end menu)/\1\@end ifnothtml\n\2/' developer.texinfo.new
	@if cmp developer.texinfo developer.texinfo.new >/dev/null 2>&1; then echo "developer.texinfo unchanged"; rm -f developer.texinfo.new; else mv developer.texinfo.new developer.texinfo; fi
	touch .fix-dev-menu

images/%.eps: images/%.dot
	dot -Tps $< >$@

images/dfepl-flow.png: images/dfepl-flow.eps
	eps2png-mono-alpha.zsh $< 500

images/%.pdf: images/%.eps
	epstopdf $< --outfile=$@

html: daikon.html developer.html

# The perl moves </ul> from after the table of contents to before it,
# preventing it from being too deeply indented.  It's needed with makeinfo 4.2.
daikon.html: daikon.texinfo config-options.texinfo invariants-doc.texinfo .fix-menu images/dfepl-flow.png
	makeinfo --html --no-split $<
	perl -pi -e '$$/ = undef; s|(<h2>Table of Contents</h2>)|</ul>\n\n$$1|; s|</ul>\n\n(<p><hr>\nNode:<a name="Introduction">Introduction</a>,)|$$1|; s|(<h2>Table of Contents</h2>)|</ul>\n\n$$1|; s|,\nPrevious:<a rel=previous href="#dir">\(dir\)</a>,\nUp:<a rel=up href="#dir">\(dir\)</a>||; s|,\nPrevious:&nbsp;<a rel="previous" accesskey="p" href="#dir">\(dir\)</a>,\nUp:&nbsp;<a rel="up" accesskey="u" href="#dir">\(dir\)</a>||;' daikon.html

# just builds from texinfo.  Much faster than rebuilding the files from
# javadoc
daikon-fast: daikon.texinfo .fix-menu
	makeinfo --html --no-split $<
	perl -pi -e '$$/ = undef; s|(<h2>Table of Contents</h2>)|</ul>\n\n$$1|; s|</ul>\n\n(<p><hr>\nNode:<a name="Introduction">Introduction</a>,)|$$1|; s|(<h2>Table of Contents</h2>)|</ul>\n\n$$1|; s|,\nPrevious:<a rel=previous href="#dir">\(dir\)</a>,\nUp:<a rel=up href="#dir">\(dir\)</a>||; s|,\nPrevious:&nbsp;<a rel="previous" accesskey="p" href="#dir">\(dir\)</a>,\nUp:&nbsp;<a rel="up" accesskey="u" href="#dir">\(dir\)</a>||;' daikon.html

developer.html: developer.texinfo .fix-menu
	makeinfo --html --no-split $<
	perl -pi -e '$$/ = undef; s|(<h2>Table of Contents</h2>)|</ul>\n\n$$1|; s|</ul>\n\n(<p><hr>\nNode:<a name="Introduction">Introduction</a>,)|$$1|; s|(<h2>Table of Contents</h2>)|</ul>\n\n$$1|; s|,\nPrevious:<a rel=previous href="#dir">\(dir\)</a>,\nUp:<a rel=up href="#dir">\(dir\)</a>||; s|,\nPrevious:&nbsp;<a rel="previous" accesskey="p" href="#dir">\(dir\)</a>,\nUp:&nbsp;<a rel="up" accesskey="u" href="#dir">\(dir\)</a>||;' developer.html

html-chap: daikon_manual_html/daikon.html developer_manual_html/developer.html

# The "subdir" flag does not copy over images aren't copied, so the
# pathnames become incorrect.  I need to copy them over myself.
# Images are in the order of appearance in the manual (search for "@image",
# except daikon-logo.jpg; this list is updated by hand (unfortunately).
daikon_manual_html/daikon.html: daikon.texinfo
	rm -rf daikon_manual_html/*
	texi2html -split chapter -subdir daikon_manual_html $<
	perl -pi -e '$$/ = "\n\n"; if (m|http://texi2html\.cvshome\.org|) { $$_ = ""; }' daikon_manual_html/daikon.html

developer_manual_html/developer.html: developer.texinfo
	rm -rf developer_manual_html/*
	texi2html -split chapter -subdir developer_manual_html $<

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

# daikon.pdf: daikon.ps
# 	ps2pdf $<

daikon.pdf: daikon.dvi images/dfepl-flow.eps
	dvipdfm $<

developer.pdf: developer.dvi
	dvipdfm $<

# If the source file below doesn't exist, you may need to check out
# the eclipse-plugins CVS module by saying "cvs checkout
# eclipse-plugins" in $INV.
daikonEclipseHelp.html: ../eclipse-plugins/workspace/DaikonUI/html/daikonHelp.html
	cp -p $< $@

# Note that the clean target does not remove invariants-doc.texinfo or
# config-options.texinfo.

clean: texi-clean
	rm -f daikon.tmp \
		daikon.info daikon.info-[0-9] \
		daikon.log daikon.dvi daikon.ps daikon.pdf daikon.html .fix-menu
	rm -f *.bak
	rm -rf daikon_manual_html
	rm -f developer.tmp developer.pg \
		developer.info developer.info-[0-9] \
		developer.log developer.dvi developer.ps developer.pdf developer.html
	rm -f images/dfepl-flow.{eps,pdf,png}

texi-clean:
	rm -rf daikon.aux daikon.cp daikon.cps daikon.fn daikon.ky daikon.pg daikon.toc daikon.tp daikon.vr
	rm -rf developer.aux developer.cp developer.cps developer.fn developer.ky \
		   developer.toc developer.tp developer.vr

# Remakes entire distribution; use with caution.
dist:
	cd .. && $(MAKE) dist

# "www" target wouldn't work from here, because "www" is an (existing)
# directory; but go to the parent directory and run the rule from there.
# www:
# 	cd .. && $(MAKE) www

www-dist:
	cd .. && $(MAKE) pag-install
pag-install:
	cd .. && $(MAKE) pag-install
