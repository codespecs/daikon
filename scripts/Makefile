HIERDIR = ../tools/hierarchical
KMDIR = ../tools/kmeans

# These are executables and/or downloaded from the Internet.
all: difftbl cluster clgroup kmeans trace-untruncate-fast checkargs.pm style-fix
.PHONY: all clean tags

clean:
	rm -f difftbl cluster clgroup kmeans trace-untruncate-fast
	${MAKE} -C ${HIERDIR} clean
	${MAKE} -C ${KMDIR} clean

TAGS: tags
tags:
	etags `find . | grep -v '~\|^\.$$\|^\./[kx]means$$'`

difftbl: ${HIERDIR}/difftbl
	cp -p $^ $@

cluster: ${HIERDIR}/cluster
	cp -p $^ $@

clgroup: ${HIERDIR}/clgroup
	cp -p $^ $@

kmeans: ${KMDIR}/kmeans
	cp -p $^ $@

trace-untruncate-fast: trace-untruncate-fast.c
	gcc -Wall -g $< -o $@

## Delegation to other Makefiles (gross)

${HIERDIR}/difftbl: ${HIERDIR}/difftbl.c
	${MAKE} -C ${HIERDIR} difftbl

${HIERDIR}/cluster: ${HIERDIR}/cluster.c
	${MAKE} -C ${HIERDIR} cluster

${HIERDIR}/clgroup: ${HIERDIR}/clgroup.c
	${MAKE} -C ${HIERDIR} clgroup

# This rule lists no dependencies, because doing so would just duplicate
# what is already in that Makefile.
${KMDIR}/kmeans:
	${MAKE} -C ${KMDIR} kmeans

checkargs.pm:
	wget https://raw.githubusercontent.com/plume-lib/html-tools/master/checkargs.pm
