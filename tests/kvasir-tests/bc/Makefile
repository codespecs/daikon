NAME := bc
SOURCES := bc.c execute.c getopt.c getopt1.c global.c load.c main.c number.c \
           scan.c storage.c util.c
INCLUDES := bc.h bcdefs.h config.h const.h getopt.h global.h libmath.h \
           number.h proto.h
KVASIR_OPTIONS := --ignore-globals --func-disambig-ptrs
DEFINES := -I. -DHAVE_CONFIG_H
PROGRAM_ARGS := -l pi.b

default: summary

include ../Makefile.common
