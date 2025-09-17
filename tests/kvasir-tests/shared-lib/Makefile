NAME := shared-lib
SOURCES := main.c
LIBS := liba.so libb.so
LIBFLAGS = -L. -Wl,-rpath . -la -lb
EXTRA_JUNK = ${LIBS}

include ../Makefile.common

liba.so: liba.c
	${CC} -fPIC ${CFLAGS} ${DEFINES} $< -shared -o $@

libb.so: libb.c
	${CC} -fPIC ${CFLAGS} ${DEFINES} $< -shared -o $@
