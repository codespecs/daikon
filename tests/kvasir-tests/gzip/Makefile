NAME := gzip
SOURCES := bits.c crypt.c deflate.c getopt.c getopt1.c gzip.c inflate.c lzw.c \
           trees.c unlzh.c unlzw.c unpack.c unzip.c util.c yesno.c zip.c
INCLUDES := gzip.h config.h lzw.h revision.h tailor.h
DEFINES = -I. -DHAVE_CONFIG_H
BEFORE_RUN := cp input file; rm -f file.gz
PROGRAM_ARGS := -v1 file
AFTER_RUN := od -t x1 file.gz >>${NAME}.out
DTRACE_DIFF := dtrace-diff-fast.pl
# COMPRESS_DTRACE := 1
COUNTS_ONLY := 1

#default: summary

include ../Makefile.common

