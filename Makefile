###########################################################################
### Variables
###

# Should gries-instrumented be in this list?
LISP_FILES := gries-helper.lisp instrument.lisp data-trace.lisp \
	load-all.lisp \
	gries.lisp gries-instrumented.lisp inv-medic.lisp
PYTHON_FILES := invariants.py util.py

## Examples of better ways to get the lists:
# PERL_MODULES := $(wildcard *.pm)
# PERL_SCRIPTS := $(wildcard *.pl)
# PERL_SCRIPTS += em_analyze em_reports cppp
# PERL_MODULE_TEXI := $(patsubst %.pm,%.texi,$(PERL_MODULES))
# PERL_MODULE_INFO := $(patsubst %.pm,%.info,$(PERL_MODULES))
# PERL_MODULE_MAN := $(patsubst %.pm,%.man,$(PERL_MODULES))
# PERL_MODULE_HTML := $(patsubst %.pm,%.html,$(PERL_MODULES))


###########################################################################
### Rules
###

tags: TAGS

## As of July 1998, my Linux etags works on Python; my Solaris one doesn't.
## So I should be sure to do the make on a Linux machine. -MDE
TAGS:  $(LISP_FILES) $(PYTHON_FILES)
	etags $(LISP_FILES) $(PYTHON_FILES)

## I should add a rule for making the distribution, which will include
## documentation and sample files.
