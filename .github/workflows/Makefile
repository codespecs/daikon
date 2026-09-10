PLUME_SCRIPTS ?= ../../.utils/plume-scripts

.PHONY: all test clean default
default: all
all: ci.yml
test: all

%: %.m4 ../../.azure/defs-common.m4 defs.m4 ../../.azure/jobs.m4 ${PLUME_SCRIPTS}
	m4 $< > $@
	${PLUME_SCRIPTS}/squeeze-blank-lines $@

${PLUME_SCRIPTS}:
	mkdir -p $$(dirname $@)
	git clone --depth=1 -q https://github.com/plume-lib/plume-scripts.git $@

clean:
	@rm -f ci.yml
