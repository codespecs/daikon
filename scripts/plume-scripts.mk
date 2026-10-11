# Defines PLUME_SCRIPTS and clones it if it does not exist.
# Include this file after setting DAIKONDIR.
# Set the NONETWORK variable to avoid the clone.

PLUME_SCRIPTS ?= ${DAIKONDIR}/.utils/plume-scripts

ifndef NONETWORK
ifeq (,$(wildcard ${PLUME_SCRIPTS}))
  dummy := $(shell mkdir -p "$(dir ${PLUME_SCRIPTS})" && git clone --depth=1 -q https://github.com/plume-lib/plume-scripts.git "${PLUME_SCRIPTS}")
endif
endif
