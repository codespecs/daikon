HIERDIR = ../tools/hierarchical
KMDIR = ../tools/kmeans

all: difftbl cluster clgroup kmeans trace-untruncate-fast

SH_SCRIPTS = $(shell grep -r -l '^\#! \?\(/bin/\|/usr/bin/env \)sh' * | grep -v .git | grep -v "~" | grep -v addrfilter | grep -v mail-stackoverflow.sh)
BASH_SCRIPTS = $(shell grep -r -l '^\#! \?\(/bin/\|/usr/bin/env \)bash' * | grep -v .git | grep -v "~")
shell-script-style:
	shellcheck --format=gcc ${SH_SCRIPTS} ${BASH_SCRIPTS}
	checkbashisms ${SH_SCRIPTS}
showvars:
	@echo "SH_SCRIPTS=${SH_SCRIPTS}"
	@echo "BASH_SCRIPTS=${BASH_SCRIPTS}"

PYTHON_FILES=$(wildcard *.py)
python-style:
	yapf -i --style='{column_limit: 100}' ${PYTHON_FILES}
	pylint -f parseable --disable=W,invalid-name ${PYTHON_FILES}

clean:
	rm -f difftbl cluster clgroup kmeans trace-untruncate-fast
	$(MAKE) -C $(HIERDIR) clean
	$(MAKE) -C $(KMDIR) clean

TAGS: tags

tags:
	etags `find . | grep -v '~\|^\.$$\|^\./[kx]means$$'`

difftbl: $(HIERDIR)/difftbl
	cp -p $^ $@

cluster: $(HIERDIR)/cluster
	cp -p $^ $@

clgroup: $(HIERDIR)/clgroup
	cp -p $^ $@

kmeans: $(KMDIR)/kmeans
	cp -p $^ $@

trace-untruncate-fast: trace-untruncate-fast.c
	gcc -Wall -g $< -o $@

## Delegation to other Makefiles (gross)

$(HIERDIR)/difftbl: $(HIERDIR)/difftbl.c
	$(MAKE) -C $(HIERDIR) difftbl

$(HIERDIR)/cluster: $(HIERDIR)/cluster.c
	$(MAKE) -C $(HIERDIR) cluster

$(HIERDIR)/clgroup: $(HIERDIR)/clgroup.c
	$(MAKE) -C $(HIERDIR) clgroup

# This rule lists no dependencies, because doing so would just duplicate
# what is already in that Makefile.
$(KMDIR)/kmeans:
	$(MAKE) -C $(KMDIR) kmeans
