

###########################################################################

BIBDIR ?= /g2/users/mernst/bib
BIBFILES = ${BIBDIR}/bibstring-unabbrev.bib ${BIBDIR}/ernst.bib ${BIBDIR}/pag.bib ${BIBDIR}/invariants.bib
BPHOME ?= /g2/users/mernst/bin/src/perl/bibtex2web/lib
BWBIN = ${BPHOME}/../bin
MDEWWW ?= /g2/users/mernst/www

###########################################################################

html: HTML

HTML: abstracts index.html

# Remove generated files.
clean-abstracts:
	rm *-abstract.html

index.html:
	${BWBIN}/bwconv.pl -format=bibtex,htmlpubs -outopts=withyears -headfoot ../pubs-sources/index-headfoot.html -to $(notdir $@) -filter 'defined($$rec{"usesdaikon"}) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	../pubs-sources/fix-homedir-tilde.pl $@

# Creates *-abstract.html files.
abstracts:
	${BWBIN}/bwconv.pl -format=bibtex -outformat=htmlabstract -to abstracts-unsplit.html -authorurls ${MDEWWW}/pubs/author-urls -filter 'defined($$rec{"usesdaikon"}) && ! (defined($$rec{"omitfromcv"}) || (lc($$rec{"TYPE"}) eq "lecture") || (defined($$rec{"crossrefonly"}) && ! defined($$rec{"inlined-crossref"})))' ${BIBFILES}
	../pubs-sources/fix-homedir-tilde.pl abstracts-unsplit.html
	${BWBIN}/htmlabstract-split.pl -headfoot ../pubs-sources/abstract-headfoot.html abstracts-unsplit.html
	rm abstracts-unsplit.html

.PHONY: abstracts index.html
