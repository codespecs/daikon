default:
	@echo "Specify a target:"
	@echo "  dist"

# Not the default to avoid accidentally installing new webpages.
dist:
	cd .. && $(MAKE) www-dist

www: dist

www-dist: www
