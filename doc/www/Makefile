default:
	@echo "Specify a target:"
	@echo "  dist"
	@echo "  pubs-using"

# Not the default to avoid accidentally installing new webpages.
www-dist:
	cd .. && $(MAKE) www-dist

pubs-using:
	cd pubs-using && $(MAKE) -f ../pubs-sources/Makefile html

.PHONY: pubs-using

###########################################################################
