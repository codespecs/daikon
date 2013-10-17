default:
	@echo "Specify a target:"
	@echo "  pubs"
	@echo "  validate"

# Not the default to avoid accidentally installing new webpages.

pubs:
	mkdir -p pubs
	/bin/rm -f pubs/*
	cd pubs && $(MAKE) -f ../pubs-sources/Makefile html

validate:
	validate `find -name '*.html' -print`

.PHONY: pubs

###########################################################################
