PLUME_SCRIPTS ?= ../.utils/plume-scripts
ifeq (,$(wildcard ${PLUME_SCRIPTS}))
mkdir -p $(dirname ${PLUME_SCRIPTS})
dummy := $(shell git clone --depth=1 -q https://github.com/plume-lib/plume-scripts.git ${PLUME_SCRIPTS})
endif

.PHONY: all default clean

all default: azure-pipelines.yml
# TODO: all default: azure-pipelines.yml azure-pipelines-daily.yml

%: %.m4 defs.m4 jobs.m4
	@chmod -f +w $@ || true
	m4 $< > $@
	${PLUME_SCRIPTS}/squeeze-blank-lines $@
	@chmod -w $@

clean:
	rm -f azure-pipelines.yml
