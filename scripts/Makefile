HIERDIR = ../tools/hierarchical
KMDIR = ../tools/kmeans

# These are executables and/or downloaded from the Internet.
all: difftbl cluster clgroup kmeans trace-untruncate-fast checkargs.pm style-fix style-check
.PHONY: all


style-fix: shell-style-fix python-style-fix perl-style-fix
style-check: shell-style-check python-style-check python-typecheck perl-style-check
.PHONY: style-fix shell-style-fix python-style-fix perl-style-fix style-check shell-style-check python-style-check perl-style-check

perl-style:
	@for perlfile in $(shell ls *.pm *.pl); do perl -Mstrict -cw $${perlfile}; done

PYTHON_FILES:=$(wildcard *.py) $(shell grep -r -l --exclude='*.py' --exclude='*~' --exclude='*.tar' --exclude=gradlew --exclude-dir=.git '^\#! \?\(/bin/\|/usr/bin/env \)python')
PYTHON_FILES_TO_CHECK:=$(filter-out ${lcb_runner},${PYTHON_FILES})
python-style-fix:
ifneq (${PYTHON_FILES_TO_CHECK},)
	@ruff --version
	@ruff format ${PYTHON_FILES_TO_CHECK}
	@ruff -q check ${PYTHON_FILES_TO_CHECK} --fix
endif
python-style-check:
ifneq (${PYTHON_FILES_TO_CHECK},)
	@ruff --version
	@ruff -q format --check ${PYTHON_FILES_TO_CHECK}
	@ruff -q check ${PYTHON_FILES_TO_CHECK}
endif
python-typecheck:
ifneq (${PYTHON_FILES_TO_CHECK},)
	@mypy --version
	@mypy --strict --install-types --non-interactive ${PYTHON_FILES_TO_CHECK} > /dev/null 2>&1 || true
	mypy --strict --ignore-missing-imports ${PYTHON_FILES_TO_CHECK}
endif

SH_SCRIPTS   := $(shell grep -r -l --exclude='*~' --exclude='*.tar' --exclude=gradlew --exclude-dir=.git '^\#! \?\(/bin/\|/usr/bin/env \)sh'   | grep -v addrfilter | grep -v cronic-orig | grep -v mail-stackoverflow.sh)
BASH_SCRIPTS := $(shell grep -r -l --exclude='*~' --exclude='*.tar' --exclude=gradlew --exclude-dir=.git '^\#! \?\(/bin/\|/usr/bin/env \)bash' | grep -v addrfilter | grep -v cronic-orig | grep -v mail-stackoverflow.sh)
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
	@echo "PYTHON_FILES_TO_CHECK=${PYTHON_FILES_TO_CHECK}"
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
