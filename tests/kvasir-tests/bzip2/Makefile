NAME := bzip2
AR=ar
RANLIB=ranlib
LDFLAGS=
CUSTOM_BUILD := 1
OBJS= blocksort.o  \
      huffman.o    \
      crctable.o   \
      randtable.o  \
      compress.o   \
      decompress.o \
      bzlib.o
EXTRA_JUNK = ${OBJS} libbz2.a file.bz2 bzip2.o
BEFORE_RUN := cp small-input file; rm -f file.bz2
PROGRAM_ARGS := -vv file
AFTER_RUN := od -t x1 file.bz2 >>${NAME}.out
DTRACE_DIFF := dtrace-diff-fast.pl
COUNTS_ONLY := 1

include ../Makefile.common


bzip2: libbz2.a bzip2.o
	${CC} ${CFLAGS} ${LDFLAGS} -o bzip2 bzip2.o -L. -lbz2

libbz2.a: ${OBJS}
	rm -f libbz2.a
	${AR} cq libbz2.a ${OBJS}
	@if ( test -f ${RANLIB} -o -f /usr/bin/ranlib -o \
		-f /bin/ranlib -o -f /usr/ccs/bin/ranlib ) ; then \
		echo ${RANLIB} libbz2.a ; \
		${RANLIB} libbz2.a ; \
	fi

%.o: %.c
	${CC} ${CFLAGS} -c $< -o $@
