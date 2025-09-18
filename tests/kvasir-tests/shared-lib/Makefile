NAME := shared-lib
SOURCES := main.c
LIBS := liba.so libb.so
LIBFLAGS = -L. -Wl,-rpath . -la -lb
EXTRA_JUNK = ${LIBS}

include ../Makefile.common

%.so: %.c
	${CC} -fPIC ${CFLAGS} ${DEFINES} $< -shared -o $@
