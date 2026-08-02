PLUME_SCRIPTS ?= ../.utils/plume-scripts

.PHONY: default all test clean

all default: azure-pipelines.yml
# TODO: all default: azure-pipelines.yml azure-pipelines-daily.yml

%: %.m4 defs.m4 defs-common.m4 jobs.m4
	@chmod -f +w $@ || true
	m4 $< > $@
	${PLUME_SCRIPTS}/squeeze-blank-lines $@
	@chmod -w $@

${PLUME_SCRIPTS}:
	mkdir -p $(dirname ${PLUME_SCRIPTS})
	git clone --depth=1 -q https://github.com/plume-lib/plume-scripts.git ${PLUME_SCRIPTS}

clean:
	rm -f azure-pipelines.yml
test:
