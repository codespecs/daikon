all: daikon.info daikon.html daikon.ps

daikon.info: daikon.texinfo
	makeinfo $<

daikon.html: daikon.texinfo
	makeinfo --html $<

# The "subdir" flag causes images to be processed incorrectly.  (The images
# aren't copied, so the pathnames become incorrect.)
daikon_manual_html/daikon.html: daikon.texinfo
	texi2html -split section -subdir daikon_manual_html $<

daikon.dvi: daikon.texinfo
	texi2dvi $<

daikon.ps: daikon.dvi
	dvips -o $@ $<
