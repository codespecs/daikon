all: info html ps


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

# The "subdir" flag does not copy over images aren't copied, so the
# pathnames become incorrect.  I need to copy them over myself.
daikon_manual_html/daikon.html: daikon.texinfo
	texi2html -split chapter -subdir daikon_manual_html $<

daikon.dvi: daikon.texinfo
	texi2dvi $<

ps: daikon.ps

daikon.ps: daikon.dvi
	dvips -o $@ $<
