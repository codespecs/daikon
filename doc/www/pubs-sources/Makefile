

###########################################################################

BIBDIR ?= /afs/csail.mit.edu/u/m/mernst/bib
BIBFILES = ${BIBDIR}/bibstring-unabbrev.bib ${BIBDIR}/ernst.bib ${BIBDIR}/pag.bib ${BIBDIR}/invariants.bib ${BIBDIR}/testing.bib
BPHOME ?= /afs/csail.mit.edu/u/m/mernst/bin/src/perl/bibtex2web/lib
BWBIN = ${BPHOME}/../bin
MDEWWW ?= /afs/csail.mit.edu/u/m/mernst/www
PERLLIB := $(HOME)/research/invariants/scripts:/afs/csail.mit.edu/u/m/mernst/bin/src/perl/bibtex2web/lib
###########################################################################

html: HTML

HTML: abstracts index.html

# Remove generated files.
clean-abstracts:
	rm *-abstract.html

index.html:
	${BWBIN}/bwconv.pl -format=bibtex,htmlpubs -outopts=withyears -hfbodyline BODY-INVARIANTDETECTION -headfoot ../pubs-sources/index-headfoot.html -to $(notdir $@)-1 -filter 'defined($$rec{"category"}) && ($$rec{"category"} eq "Invariant detection") && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	${BWBIN}/bwconv.pl -format=bibtex,htmlpubs -outopts=withyears -hfbodyline BODY-METHODOLOGYUSINGDAIKON -headfoot $(notdir $@)-1 -to $(notdir $@)-2 -filter 'defined($$rec{"usesdaikon"}) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	${BWBIN}/bwconv.pl -format=bibtex,htmlpubs -outopts=withyears -hfbodyline BODY-DAIKONTESTSUBJECT -headfoot $(notdir $@)-2 -to $(notdir $@) -filter 'defined($$rec{"usesdaikonastestsubject"}) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	rm -f $(notdir $@)-1 $(notdir $@)-2
	../pubs-sources/fix-homedir-tilde.pl $@

writable-abstracts:
	chmod -f +w *-abstract.html

# Creates *-abstract.html files.
abstracts: abstracts-testsubject abstracts-methodology abstracts-invariantdetection writable-abstracts

abstracts-invariantdetection:
	${BWBIN}/bwconv.pl -format=bibtex -outformat=htmlabstract -outopts=withbibtex -to abstracts-unsplit.html -linknames ${MDEWWW}/pubs/link-names -filter 'defined($$rec{"category"}) && ($$rec{"category"} eq "Invariant detection") && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	../pubs-sources/fix-homedir-tilde.pl abstracts-unsplit.html
	${BWBIN}/htmlabstract-split.pl \
	  -headfoot ../pubs-sources/abstract-headfoot-technique.html \
	  abstracts-unsplit.html
	rm abstracts-unsplit.html

abstracts-methodology:
	${BWBIN}/bwconv.pl -format=bibtex -outformat=htmlabstract -outopts=withbibtex -to abstracts-unsplit.html -linknames ${MDEWWW}/pubs/link-names -filter 'defined($$rec{"usesdaikon"}) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	../pubs-sources/fix-homedir-tilde.pl abstracts-unsplit.html
	${BWBIN}/htmlabstract-split.pl \
	  -headfoot ../pubs-sources/abstract-headfoot-methodology.html \
	  abstracts-unsplit.html
	rm abstracts-unsplit.html

abstracts-testsubject:
	${BWBIN}/bwconv.pl -format=bibtex -outformat=htmlabstract -outopts=withbibtex -to abstracts-unsplit.html -linknames ${MDEWWW}/pubs/link-names -filter 'defined($$rec{"usesdaikonastestsubject"}) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	../pubs-sources/fix-homedir-tilde.pl abstracts-unsplit.html
	${BWBIN}/htmlabstract-split.pl -headfoot ../pubs-sources/abstract-headfoot-testsubject.html abstracts-unsplit.html
	rm abstracts-unsplit.html

.PHONY: abstracts index.html
