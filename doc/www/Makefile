default:
	@echo "Specify a target:"
	@echo "  pubs"

# Not the default to avoid accidentally installing new webpages.

pubs:
	mkdir -p pubs
	/bin/rm -f pubs/*
	${MAKE} -C pubs -f ../pubs-sources/Makefile html

.PHONY: pubs

###########################################################################
