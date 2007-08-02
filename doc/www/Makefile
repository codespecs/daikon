default:
	@echo "Specify a target:"
	@echo "  www-dist"
	@echo "  pubs"

# Not the default to avoid accidentally installing new webpages.
www-dist:
	cd .. && $(MAKE) www-dist
pag-install:
	cd .. && $(MAKE) pag-install

pubs:
	mkdir -p pubs
	/bin/rm -f pubs/*
	cd pubs && $(MAKE) -f ../pubs-sources/Makefile html

validate:
	validate `find -name '*.html' -print`

.PHONY: pubs

###########################################################################
