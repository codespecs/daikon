# Defines PLUME_SCRIPTS and rules to clone and update it.
# Include this file after setting DAIKONDIR.
# Set the NONETWORK variable to avoid network operations.

PLUME_SCRIPTS ?= ${DAIKONDIR}/.utils/plume-scripts

PLUME_SCRIPTS_CLONE := mkdir -p "$(dir ${PLUME_SCRIPTS})" && git clone --depth=1 -q https://github.com/plume-lib/plume-scripts.git "${PLUME_SCRIPTS}"

# Non-empty when running under continuous integration.
# Empty if is-ci.sh is unavailable (for example, because NONETWORK is set).
# A recipe that uses IS_CI should have ${PLUME_SCRIPTS} as an order-only prerequisite.
IS_CI = $(shell [ -x "${PLUME_SCRIPTS}"/is-ci.sh ] && "${PLUME_SCRIPTS}"/is-ci.sh)

# The rules below should not change the including Makefile's default goal.
plume_scripts_saved_default_goal := ${.DEFAULT_GOAL}

# Creates the directory if absent, but does not update it.
${PLUME_SCRIPTS}:
ifndef NONETWORK
	${PLUME_SCRIPTS_CLONE}
endif

.PHONY: update-plume-scripts
update-plume-scripts:
ifndef NONETWORK
	if [ -d "${PLUME_SCRIPTS}"/.git ]; then \
	  git -C "${PLUME_SCRIPTS}" pull -q --ff-only; \
	else \
	  ${PLUME_SCRIPTS_CLONE}; \
	fi
endif

.DEFAULT_GOAL := ${plume_scripts_saved_default_goal}
