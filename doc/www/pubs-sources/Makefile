###########################################################################
DAIKONDIR := $(realpath $(dir $(lastword $(MAKEFILE_LIST)))../../..)
CHECKLINK ?= ${DAIKONDIR}/utils/checklink
HTMLTOOLS ?= ${DAIKONDIR}/utils/html-tools
PLUMESCRIPTS ?= ${DAIKONDIR}/utils/plume-scripts
BPHOME ?= ${DAIKONDIR}/utils/bibtex2web/lib
export BPHOME
BWBIN = ${BPHOME}/../bin
BIBDIR ?= ~/bib
# This should include every file with "usesDaikon" or "\"Invariant dectection\""
BIBFILES = ${BIBDIR}/bibstring-unabbrev.bib ${BIBDIR}/ernst.bib ${BIBDIR}/invariants.bib ${BIBDIR}/pag.bib ${BIBDIR}/soft-eng.bib ${BIBDIR}/testing.bib ${BIBDIR}/types.bib ${BIBDIR}/crossrefs.bib
###########################################################################

# Files in $pag/www/daikon/pubs-sources/
FILES_FROM_CHECKOUT := Makefile abstract-headfoot-methodology.html abstract-headfoot-technique.html abstract-headfoot-testsubject.html abstract-headfoot.html fix-homedir-tilde.pl index-headfoot.html


html: HTML

HTML: abstracts index.html

update-from-checkout:
	cd ${DAIKONDIR}/doc/www/pubs-sources; cp -pf ${FILES_FROM_CHECKOUT} ${pag}/www/daikon/pubs-sources/

# Remove generated files.
clean-abstracts:
	@touch dummy-abstract.html
	\rm -f *-abstract.html

pull-repos:
	cd ${BIBDIR} && git pull
	${MAKE} -C ${DAIKONDIR} update-libs

.PHONY: index.html
index.html: pull-repos
# Avoid error "Cannot write to index.html"
	rm -f $@
	${BWBIN}/bwconv.pl -format=bibtex,htmlpubs -outopts=withyears -hfbodyline BODY-INVARIANTDETECTION -headfoot ../pubs-sources/index-headfoot.html -to $(notdir $@)-1 -filter 'defined($$rec{"category"}) && (index($$rec{"category"}, "Specification inference") != -1) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	${BWBIN}/bwconv.pl -format=bibtex,htmlpubs -outopts=withyears -hfbodyline BODY-METHODOLOGYUSINGDAIKON -headfoot $(notdir $@)-1 -to $(notdir $@)-2 -filter 'defined($$rec{"usesdaikon"}) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	${BWBIN}/bwconv.pl -format=bibtex,htmlpubs -outopts=withyears -hfbodyline BODY-DAIKONTESTSUBJECT -headfoot $(notdir $@)-2 -to $(notdir $@) -filter 'defined($$rec{"usesdaikonastestsubject"}) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	rm -f $(notdir $@)-1 $(notdir $@)-2
	../pubs-sources/fix-homedir-tilde.pl $@

writable-abstracts:
# A chmod command fails if there are no arguments.  But this might be
# called when no *-abstract.html files exist.  So create a dummy file,
# include it on the command line, and remove it afterward.  This is gross.
# An alternative (but "--no-run-if-empty" is a GNU extension):
#   ls | egrep '-abstract\.html$' | xargs --no-run-if-empty chmod -f a+w
	@touch dummy-abstract.html
	chmod -f a+w *-abstract.html
	@\rm -f dummy-abstract.html
	@\rm -f abstracts-unsplit.html

# Creates *-abstract.html files.
abstracts: pull-repos writable-abstracts abstracts-testsubject abstracts-methodology abstracts-invariantdetection writable-abstracts

abstracts-invariantdetection:
	${BWBIN}/bwconv.pl -format=bibtex -outformat=htmlabstract -outopts=withbibtex -to abstracts-unsplit.html -linknames ${HTMLTOOLS}/html-canonical-urls -validurls ${HTMLTOOLS}/html-valid-urls -filter 'defined($$rec{"category"}) && (index($$rec{"category"}, "Specification inference") != -1) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	../pubs-sources/fix-homedir-tilde.pl abstracts-unsplit.html
	${BWBIN}/htmlabstract-split.pl \
	  -headfoot ../pubs-sources/abstract-headfoot-technique.html \
	  abstracts-unsplit.html
	\rm -f abstracts-unsplit.html

abstracts-methodology:
	${BWBIN}/bwconv.pl -format=bibtex -outformat=htmlabstract -outopts=withbibtex -to abstracts-unsplit.html -linknames ${HTMLTOOLS}/html-canonical-urls -validurls ${HTMLTOOLS}/html-valid-urls -filter 'defined($$rec{"usesdaikon"}) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	../pubs-sources/fix-homedir-tilde.pl abstracts-unsplit.html
	${BWBIN}/htmlabstract-split.pl \
	  -headfoot ../pubs-sources/abstract-headfoot-methodology.html \
	  abstracts-unsplit.html
	rm abstracts-unsplit.html

abstracts-testsubject:
	${BWBIN}/bwconv.pl -format=bibtex -outformat=htmlabstract -outopts=withbibtex -to abstracts-unsplit.html -linknames ${HTMLTOOLS}/html-canonical-urls -validurls ${HTMLTOOLS}/html-valid-urls -filter 'defined($$rec{"usesdaikonastestsubject"}) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	../pubs-sources/fix-homedir-tilde.pl abstracts-unsplit.html
	${BWBIN}/htmlabstract-split.pl -headfoot ../pubs-sources/abstract-headfoot-testsubject.html abstracts-unsplit.html
	rm abstracts-unsplit.html

.PHONY: abstracts html HTML clean-abstracts writable-abstracts
