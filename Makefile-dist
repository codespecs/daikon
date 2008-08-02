# Main Makefile for Daikon

all:
	$(MAKE) -C bin
ifdef DAIKONCLASS_SOURCES
	$(MAKE) -C java
endif
	$(MAKE) java/dcomp_rt.jar
ifeq (Linux i686,$(shell uname -sm))
	$(MAKE) kvasir
else
ifeq (Linux i586,$(shell uname -sm))
	$(MAKE) kvasir
else
ifeq (Linux i486,$(shell uname -sm))
	$(MAKE) kvasir
else
ifeq (Linux i386,$(shell uname -sm))
	$(MAKE) kvasir
else
ifeq (Linux x86_64,$(shell uname -sm))
	$(MAKE) kvasir
else
	@echo "Not building Kvasir: it's only for Linux x86 and x86-64"
	@echo "and this appears to be" `uname -sm`
endif
endif
endif
endif
endif

clean:
	$(MAKE) -C bin clean
ifdef DAIKONCLASS_SOURCES
	$(MAKE) -C java clean
endif
	$(MAKE) -C kvasir/valgrind clean

java/dcomp_rt.jar:
	$(MAKE) -C java dcomp_rt.jar

kvasir:
	cd kvasir/valgrind && ./configure --prefix=`pwd`/../inst
	cd kvasir/valgrind && make
	cd kvasir/valgrind && make install

.PHONY: kvasir
