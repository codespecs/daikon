default:
	@echo "Specify a target:"
	@echo "  dist"
	@echo "  pubs"

# Not the default to avoid accidentally installing new webpages.
www-dist:
	cd .. && $(MAKE) www-dist

pubs:
	cd pubs && $(MAKE) -f ../pubs-sources/Makefile html

.PHONY: pubs

###########################################################################
