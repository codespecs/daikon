NAME := rijndael
KVASIR_OPTIONS := --ppt-list-file=rijndael.ppts
DEFINES := -DMCT_ITERATIONS_OUTER=2 -DMCT_ITERATIONS_INNER=2 -DVARIABLE_TEST_SKIP_FACTOR=16 -DTABLE_SIZE=2
SOURCES := rijndaeltest-ref.c rijndael-api-ref.c rijndael-alg-ref.c
EXTRA_OUTPUTS := cbc_d_m.txt cbc_e_m.txt ecb_d_m.txt ecb_e_m.txt ecb_iv.txt \
                 ecb_tbl.txt ecb_vk.txt ecb_vt.txt
DTRACE_DIFF := dtrace-diff-fast.pl
COUNTS_ONLY := 1

# This test seems to be non-deterministic on Ubuntu 22.10 with JDK 19.

JAVA_VERSION_STRING := $(shell javac -version 2>&1 | head -1 | cut "-d " -f2 | sed 's/-ea//')
JAVA_RELEASE_NUMBER := $(shell echo $JAVA_VERSION_STRING | sed 's/\([0-9]\+\)\(\..*\)\?\(-.*\)\?/\1/')
ifeq ($(shell expr $JAVA_RELEASE_NUMBER \>= 19),1)
  JAVA19_OR_NEWER := 1
else
  JAVA19_OR_NEWER := 0
endif

LDAIKONDIR ?= $(realpath $(dir $(lastword $(MAKEFILE_LIST)))../../..)
OS_ID := $(shell sed -n -e 's/^NAME="\(.*\)"/\1/p' /etc/os-release | cut -d " " -f1)
OS_REL := $(shell sed -n -e 's/^VERSION_ID="(.*)"/\1/' /etc/os-release)
ifeq ($(OS_ID),Ubuntu)
ifeq ($(OS_REL),22.10)
ifeq ($(JAVA19_OR_NEWER),1)
SKIP_COMP_DIFF := 1
endif
endif
endif

include ../Makefile.common
