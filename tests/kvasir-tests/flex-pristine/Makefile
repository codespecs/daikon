NAME := flex-pristine
SOURCES := ccl.c dfa.c ecs.c gen.c main.c misc.c nfa.c parse.c \
	   scan.c skel.c sym.c tblcmp.c yylex.c
PROGRAM_ARGS := small.fl
EXTRA_OUTPUTS := lex.yy.c
EXTRA_JUNK := libfl.a libmain.o libyywrap.o skel.c .bootstrap
DTRACE_DIFF := dtrace-diff-fast.pl
COUNTS_ONLY := 1

default: summary

include ../Makefile.common

DEFS = -DHAVE_CONFIG_H

# You can define these to be "lex" and "libl.a" if you want to replace
# lex at your site.
FLEX = flex
FLEXLIB = libfl.a

srcdir = .

LN_S = ln -s
YACC = bison -y
AR = ar
RANLIB = ranlib

HEADERS = flexdef.h version.h

LIBSRCS = libmain.c libyywrap.c
LIBOBJS = libmain.o libyywrap.o

# which "flex" to use to generate scan.c from scan.l
# For reproducibility, use our own version of flex to make scan.c. To
# avoid bootstrapping problems, we keep the output in CVS, and there
# shouldn't often be a need to rerun it.
FLEX_EXEC = ./flex
FLEX_FLAGS = -t ${PERF_REPORT}
COMPRESSION =
PERF_REPORT = -p


${FLEX}: flexdef.h config.h version.h

remake-parse.c: parse.y
	${YACC} -d ${srcdir}/parse.y
	@sed '/extern char.*malloc/d' <y.tab.c >parse.tmp
	@mv parse.tmp parse.c
	@mv y.tab.h parse.h
	@rm -f y.tab.c

# To rebuild parse.c and parse.h, use the rule above
parse.c:
	touch parse.c

parse.h: parse.c

remake-scan.c: scan.l
	${FLEX_EXEC} ${FLEX_FLAGS} ${COMPRESSION} ${srcdir}/scan.l >scan.c
	sed s,\"${srcdir}/scan.l\",\"scan.l\", <scan.c >scan.tmp
	mv scan.tmp scan.c

# To rebuild scan.c, use the rule above
scan.c:
	touch scan.c

skel.c: flex.skl mkskel.sh
	${SHELL} ${srcdir}/mkskel.sh ${srcdir}/flex.skl >skel.c


${FLEXLIB}: ${LIBOBJS}
	${AR} cru ${FLEXLIB} ${LIBOBJS}
	-${RANLIB} ${FLEXLIB}
