# Sets DAIKONDIR to the clone of the 'daikon' project that contains this file.
# Include this file via a pathname computed from $(lastword ${MAKEFILE_LIST}),
# before the including Makefile includes any other file.

# A DAIKONDIR from the environment is ignored, so that each clone uses its own
# files even if the environment's DAIKONDIR names a different clone.
# A DAIKONDIR from the command line is respected.
# '?=' would not work, because it defines a recursively-expanded variable, and
# MAKEFILE_LIST changes as other files are included.
ifneq ($(origin DAIKONDIR),command line)
  DAIKONDIR := $(realpath $(dir $(lastword ${MAKEFILE_LIST}))..)
endif

# Make splits pathnames at spaces, so DAIKONDIR must not contain any.
ifeq ($(strip ${DAIKONDIR}),)
  $(error DAIKONDIR is empty; set it to the top-level directory of your daikon clone)
else ifneq ($(words ${DAIKONDIR}),1)
  $(error DAIKONDIR must be a directory whose pathname contains no spaces; DAIKONDIR="${DAIKONDIR}")
else ifeq ($(wildcard ${DAIKONDIR}/scripts/check-daikondir.mk),)
  $(error DAIKONDIR is not a clone of the 'daikon' project; DAIKONDIR="${DAIKONDIR}")
endif
