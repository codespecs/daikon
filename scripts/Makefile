HIERDIR = ../tools/hierarchical
KMDIR = ../tools/kmeans

all: difftbl cluster clgroup kmeans trace-untruncate-fast checkargs.pm
.PHONY: all


style-fix: shell-style-fix python-style-fix perl-style-fix
style-check: shell-style-check python-style-check perl-style-check
.PHONY: style-fix shell-style-fix python-style-fix perl-style-fix style-check shell-style-check python-style-check perl-style-check

perl-style:
	@for perlfile in $(shell ls *.pm *.pl); do perl -Mstrict -cw $${perlfile}; done

PYTHON_FILES=$(wildcard **/*.py)
python-style-fix:
	echo $PATH
	whereis ruff
	which ruff
	ruff --version
	ruff format ${PYTHON_FILES}
	ruff check ${PYTHON_FILES} --fix
python-style-check:
	echo $PATH
	whereis ruff
	which ruff
	ruff --version
	ruff format --check ${PYTHON_FILES}
	ruff check ${PYTHON_FILES}

SH_SCRIPTS   := $(shell grep -r -l --exclude='*~' --exclude='*.tar' --exclude=gradlew --exclude=lsb_release --exclude-dir=.git '^\#! \?\(/bin/\|/usr/bin/env \)sh'   | grep -v addrfilter | grep -v cronic-orig | grep -v mail-stackoverflow.sh)
BASH_SCRIPTS := $(shell grep -r -l --exclude='*~' --exclude='*.tar' --exclude=gradlew --exclude=lsb_release --exclude-dir=.git '^\#! \?\(/bin/\|/usr/bin/env \)bash' | grep -v addrfilter | grep -v cronic-orig | grep -v mail-stackoverflow.sh)
SHFMT_EXISTS := $(shell command -v shfmt 2> /dev/null)
shell-style-fix:
ifdef SHFMT_EXISTS
	shfmt -w -i 2 -ci -bn -sr ${SH_SCRIPTS} ${BASH_SCRIPTS}
endif
	shellcheck -x -P SCRIPTDIR --format=diff ${SH_SCRIPTS} ${BASH_SCRIPTS} | patch -p1
shell-style-check:
ifdef SHFMT_EXISTS
	shfmt -d -i 2 -ci -bn -sr ${SH_SCRIPTS} ${BASH_SCRIPTS}
endif
	shellcheck -x -P SCRIPTDIR --format=gcc ${SH_SCRIPTS} ${BASH_SCRIPTS}
	checkbashisms -l ${SH_SCRIPTS}

showvars:
	@echo "PYTHON_FILES=${PYTHON_FILES}"
	@echo "SH_SCRIPTS=${SH_SCRIPTS}"
	@echo "BASH_SCRIPTS=${BASH_SCRIPTS}"

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

checkargs.pm:
	wget https://raw.githubusercontent.com/plume-lib/html-tools/master/checkargs.pm
