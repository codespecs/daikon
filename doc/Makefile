all: info html html-chap ps pdf


info: daikon.info

daikon.info: daikon.texinfo
	makeinfo $<

# Can't do this automatically without first grepping the file to
# see whether the change is needed.
fix-menu:
	perl -pi -e 's/\@detailmenu/\@ifhtml\n\@contents\n\@end ifhtml\n\@ifnothtml\n\@detailmenu/; s/\@end detailmenu/\@end detailmenu\n\@end ifnothtml/' daikon.texinfo

html: daikon.html

daikon.html: daikon.texinfo
	makeinfo --html $<

html-chap: daikon_manual_html/daikon.html

# The "subdir" flag does not copy over images aren't copied, so the
# pathnames become incorrect.  I need to copy them over myself.
daikon_manual_html/daikon.html: daikon.texinfo
	rm -rf daikon_manual_html/*
	texi2html -split chapter -subdir daikon_manual_html $<
	mkdir daikon_manual_html/images
	cp -p images/gui-ControlPanel.jpg images/gui-InvariantsDisplay-small.jpg daikon_manual_html/images

daikon.dvi: daikon.texinfo
	texi2dvi $<

ps: daikon.ps

daikon.ps: daikon.dvi
	dvips -o $@ $<

pdf: pdf-via-ps

# I can't get the images to convert to PDF, so this doesn't work
pdf-direct: daikon.texinfo daikon.dvi
	pdftex $<

pdf-via-ps: daikon.ps
	ps2pdf $<
