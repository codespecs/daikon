default:
	@echo "Specify a target:"
	@echo "  www-dist"
	@echo "  pubs"

# Not the default to avoid accidentally installing new webpages.
# It's best to run www-dist from pag, not a client machine.
www-dist:
	cd .. && $(MAKE) www-dist

pubs:
	mkdir -p pubs
	/bin/rm -f pubs/*
	cd pubs && $(MAKE) -f ../pubs-sources/Makefile html

validate:
	html-validate `find -name '*.html' -print`

.PHONY: pubs

###########################################################################
