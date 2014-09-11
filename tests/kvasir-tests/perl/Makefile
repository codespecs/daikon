NAME := perl
SOURCES := av.c deb.c doio.c doop.c dump.c globals.c gv.c hv.c \
           locale.c mg.c numeric.c op.c pad.c perl.c perlapi.c \
	   perlio.c perly.c pp.c pp_ctl.c pp_hot.c pp_pack.c pp_sort.c \
	   pp_sys.c reentr.c regcomp.c regexec.c run.c scope.c sv.c \
           taint.c toke.c universal.c utf8.c util.c xsutils.c \
           miniperlmain.c
INCLUDES := EXTERN.h INTERN.h XSUB.h av.h config.h cop.h cv.h embed.h \
            embedvar.h form.h gv.h handy.h hv.h intrpvar.h iperlsys.h \
            keywords.h mg.h nostdio.h op.h opcode.h opnames.h pad.h \
            patchlevel.h perl.h perlapi.h perlio.h perliol.h perlvars.h \
            perly.h pp.h pp_proto.h proto.h reentr.h regcomp.h regexp.h \
            regnodes.h scope.h sv.h thrdvar.h thread.h unixish.h utf8.h \
            util.h warnings.h
DEFINES = -I. -DPERL_CORE -DPURIFY -DDEBUGGING -fno-strict-aliasing -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64
LIBS := -lm -lcrypt
CFLAGS := -DNEED_VA_COPY
# PROGRAM_ARGS := -e 'print "Hello, world!\n"'
PROGRAM_ARGS := sort-half.t
KVASIR_OPTIONS := --ignore-globals --nesting-depth=1
DTRACE_DIFF := dtrace-diff-fast.pl
# COMPRESS_DTRACE := 1
COUNTS_ONLY := 1

default: summary

include ../Makefile.common
