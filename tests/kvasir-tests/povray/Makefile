NAME := povray
AMD64_WORKING := yes
CPLUSPLUS := 1
CUSTOM_BUILD := 1
PROGRAM_ARGS := +Icylinder1.pov +L./source/include +L./source/scenes/camera +Oscene.tga +FT +W2 +H2 +V +X
DTRACE_DIFF := dtrace-diff-fast.pl
COUNTS_ONLY := 1
KVASIR_OPTIONS := --ignore-globals --nesting-depth=1
EXTRA_JUNK := scene.tga *.Log
CLEAN_SCRIPT := ./clean.sh

default: summary

povray:
	./build.sh

include ../Makefile.common
