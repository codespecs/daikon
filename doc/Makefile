all: info html html-chap ps pdf


info: daikon.info

daikon.info: daikon.texinfo config-options.texinfo .fix-menu
	makeinfo $<

config-options.texinfo:
	cd ../java && $(MAKE) ../doc/config-options.texinfo

# Don't want to make this a target of other rules because it updates the modification date.
# Make forces me to double the dollar signs in the Perl code.
# sh seems not to permit redirecting standard error to standard out
.fix-menu: daikon.texinfo
	@echo "Updating menus in daikon.texinfo..."
	@emacs --batch daikon.texinfo --eval '(progn (texinfo-master-menu t) (save-buffer))' 2>&1 | grep -v "^Updating node: " | grep -v '^Updated ".*" level menu' | grep -v "^Inserting menu for" | grep -v '^Inserting chapter menu entry:'
	perl -pi -e '$$/="\n\n"; s/(::) *(\@ifhtml\n\@contents\n)/\1\n\n\2\@end ifhtml\n\@ifnothtml\n/; s/(\@end detailmenu\n)(\@end menu)/\1\@end ifnothtml\n\2/' daikon.texinfo
	touch .fix-menu

html: daikon.html

daikon.html: daikon.texinfo .fix-menu
	makeinfo --html --no-split $<

html-chap: daikon_manual_html/daikon.html

# The "subdir" flag does not copy over images aren't copied, so the
# pathnames become incorrect.  I need to copy them over myself.
# Images are in the order of appearance in the manual (search for
# "@image"; this list is updated by hand (unfortunately).
daikon_manual_html/daikon.html: daikon.texinfo
	rm -rf daikon_manual_html/*
	texi2html -split chapter -subdir daikon_manual_html $<
	mkdir daikon_manual_html/images
	cp -p images/daikon-logo.jpg images/gui-InvariantsDisplay-small.jpg images/gui-ControlPanel.jpg images/context-gui.jpg daikon_manual_html/images

daikon.dvi: daikon.texinfo
	texi2dvi $<

ps: daikon.ps

daikon.ps: daikon.dvi
	dvips -t letter -o $@ $<

pdf: daikon.pdf

# I can't get the images to convert to PDF, so this doesn't work
pdf-direct: daikon.texinfo daikon.dvi
	pdftex $<

# daikon.pdf: daikon.ps
# 	ps2pdf $<

daikon.pdf: daikon.dvi
	dvipdfm $<

clean: texi-clean
	rm -f daikon.log daikon.info daikon.info-[0-9] daikon.tmp daikon.dvi daikon.ps daikon.pdf daikon.html .fix-menu
	rm -f *.bak
	rm -rf daikon_manual_html

texi-clean:
	rm -rf daikon.aux daikon.cp daikon.cps daikon.fn daikon.ky daikon.pg daikon.toc daikon.tp daikon.vr

dist:
	cd .. && $(MAKE) dist

# "www" target doesn't work, because "www" is an (existing) directory; but
# go to the subdirectory and run the rule from there.
# www:
# 	cd .. && $(MAKE) www

www-dist:
	cd .. && $(MAKE) www
