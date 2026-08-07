PLUME_SCRIPTS ?= ../.utils/plume-scripts

.PHONY: all test clean default
all default: config.yml validate
# TODO: all default: config.yml config-daily.yml validate

%: %.m4 defs.m4 ../.azure/defs-common.m4 ../.azure/jobs.m4
	@chmod -f +w $@ || true
	m4 $< > $@
	${PLUME_SCRIPTS}/squeeze-blank-lines $@
	@chmod -w $@

validate: config.yml
	cd .. && circleci config validate

${PLUME_SCRIPTS}:
	mkdir -p $$(dirname $@)
	git clone --depth=1 -q https://github.com/plume-lib/plume-scripts.git $@

clean:
	rm -f config.yml
test:
