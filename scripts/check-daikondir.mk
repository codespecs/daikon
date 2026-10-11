# Checks that DAIKONDIR is usable.  Include this file after setting DAIKONDIR.

# Make splits pathnames at spaces, so DAIKONDIR must not contain any.
ifeq ($(strip ${DAIKONDIR}),)
  $(error DAIKONDIR is empty; set it to the top-level directory of your daikon clone)
else ifneq ($(words ${DAIKONDIR}),1)
  $(error DAIKONDIR must be a directory whose pathname contains no spaces; DAIKONDIR="${DAIKONDIR}")
endif
