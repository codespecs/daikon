NAME := rijndael
KVASIR_OPTIONS := --ppt-list-file=rijndael.ppts
DEFINES := -DMCT_ITERATIONS_OUTER=2 -DMCT_ITERATIONS_INNER=2 -DVARIABLE_TEST_SKIP_FACTOR=16 -DTABLE_SIZE=2
SOURCES := rijndaeltest-ref.c rijndael-api-ref.c rijndael-alg-ref.c
EXTRA_OUTPUTS := cbc_d_m.txt cbc_e_m.txt ecb_d_m.txt ecb_e_m.txt ecb_iv.txt \
                 ecb_tbl.txt ecb_vk.txt ecb_vt.txt

# DAIKONDIR is the clone of the 'daikon' project.
ifeq ($(origin DAIKONDIR), undefined)
  DAIKONDIR := $(realpath $(dir $(lastword ${MAKEFILE_LIST}))../../..)
endif
# Find Makefile.jdk relative to this file, not via DAIKONDIR, which may name a
# different Daikon tree.
include $(dir $(lastword ${MAKEFILE_LIST}))../../Makefile.jdk

include ../Makefile.common
