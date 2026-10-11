# Sets DAIKONDIR, if it is not already set, and checks that it is usable.

# DAIKONDIR is the clone of the 'daikon' project.
# '?=' would not work, because it defines a recursively-expanded variable, and
# MAKEFILE_LIST changes as other files are included.
ifeq ($(origin DAIKONDIR), undefined)
  DAIKONDIR := $(realpath $(dir $(lastword ${MAKEFILE_LIST}))..)
endif

# Make splits pathnames at spaces, so DAIKONDIR must not contain any.
ifeq ($(strip ${DAIKONDIR}),)
  $(error DAIKONDIR is empty; set it to the top-level directory of your daikon clone)
else ifneq ($(words ${DAIKONDIR}),1)
  $(error DAIKONDIR must be a directory whose pathname contains no spaces; DAIKONDIR="${DAIKONDIR}")
endif
