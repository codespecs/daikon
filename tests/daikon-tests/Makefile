###########################################################################
# Read Makefile in parent directory to see what this all means

# DAIKONDIR is the clone of the 'daikon' project.
DAIKONDIR ?= $(realpath $(dir $(lastword $(MAKEFILE_LIST)))..)
SCRIPTS=$(DAIKONDIR)/scripts

# Help system
default help: help-recurse

.PHONY: regression-tests
regression-tests:
	$(MAKE) txt-diff
	$(MAKE) summary

# Targets that ascend to parent directories
results summary summary2 summary3 help-recurse:
	@$(MAKE) --no-print-directory BASE?=`pwd` -C .. $@

# Targets that descend to child directories
clean clean-output txt-diff txt-daikon-suppress-diffed txt-suppress txt-daikon txt-chicory update-goals txt-all merge stats instrument compile diffs inv-checker-diff list-targets chicory chicory-diff esc jml inv txt-daikon-diff txt-chicory-diff spinfo-static dyncomp dyncomp-diff txt-simplify txt-simplify-diff txt-csharpcontract txt-csharpcontract-diff txt-repair-diff txt-esc txt-esc-diff txt-jml-diff txt-merge-diff txt-jaif-diff txt-jaif rtc rtc-diff txt-logicalcompare txt-logicalcompare-diff:
	$(MAKE) everything-$@

cross-checker:
	$(MAKE) everything-cross-checker

###########################################################################
# Useful groupings of projects (everything, default, by source, etc.)
# e.g. "make siemens-txt-diff" or "make dsaa-inv"

# (We must use actions for these rules for 'make' to work correctly.)

# Approximate times as of July 2016:  total 58 minutes, 30 for quick, 28 for nonquick
# SQD: 6 minutes
# polycalc: 11 minutes
# special-cases: 10 minutes
# siemens: 3 minutes
# dsaa: 7 minutes
# mapquick: 9 minutes
# javautil: 6 minutes
# 6170: 5 minutes
# large: 0 minutes
# new: 1 minute

everything-%: quick-% nonquick-%
	@echo ${HR}
quick-%: SQD-% polycalc-% special-cases-% siemens-%
	@echo ${HR}
nonquick-%: dsaa-% mapquick-% javautil-% 6170-% large-% new-%
	@echo ${HR}

# Canonical groupings

SQD-%: do-StackAr-% do-QueueAr-% do-DisjSets-%
	@echo ${HR}
polycalc-%: do-RatNum-% do-RatPoly-% do-RatPolyStack-% do-RatTermVec-%
	@echo ${HR}
special-cases-%: do-encapsulate_downcast-% do-compar-% do-precedence-% do-fib-% do-hanoi-% do-param-% do-suppress01-% do-suppress02-% do-StaticInteresting-% do-pine_mdiep-% do-purity-% do-Smithnosky-%
	@echo ${HR}
siemens-%: do-print_tokens-% do-print_tokens2-% do-replace-% do-schedule-% do-schedule2-% do-tcas-% do-tot_info-% do-space-%
	@echo ${HR}
dsaa-%: do-BinaryTree-% do-CursorList-% do-LinkedList-% do-redblack-% do-BinaryHeap-%
	@echo ${HR}
mapquick-%: do-WeightedNodePath-% do-StreetNumberSet-% do-GeoSegment-% do-Graph-% # do-MapQuick-%
	@echo ${HR}
javautil-%: do-Vector-% do-ArrayList13-% do-ArrayList17-%
	@echo ${HR}
6170-%: do-FixedSizeSet-% do-TinySet-% do-Huffman-%
	@echo ${HR}
large-%: # do-Flex-% do-JML-%
	@echo ${HR}
new-%: do-KnownValues-% do-KnownValuesNewDecls-% do-baseline-% do-junit-% do-junit5-% do-ncalc-%
	@echo ${HR}

# Extra groupings

paper-%: SQD-% do-Vector-% do-StreetNumberSet-% do-GeoSegment-% do-Graph-% do-RatNum-% do-RatPoly-% do-FixedSizeSet-%
	@echo ${HR}

cross-checker-good-%: do-BinaryTree-% do-compar-% do-DisjSets-% do-encapsulate_downcast-% do-fib-% do-FixedSizeSet-% do-GeoSegment-% do-Graph-% do-hanoi-% do-param-% do-precedence-% do-print_tokens-% do-print_tokens2-% do-QueueAr-% do-RatNum-% do-RatPoly-% do-RatPolyStack-% do-RatTermVec-% do-replace-% do-schedule-% do-schedule2-% do-space-% do-StackAr-% do-StaticInteresting-% do-StreetNumberSet-% do-suppress01-% do-suppress02-% do-tcas-% do-TinySet-% do-tot_info-% do-WeightedNodePath-% do-Vector-%
	@echo ${HR}

# 'make do-SomeDiectory-target'
# e.g. 'make do-StackAr-inv'
DIR = $(word 1,$(subst -, ,$*))
ifdef PARALLEL
do-%:
	@echo exec $(MAKE) -C daikon-tests/$(DIR) $(subst $(DIR)-,,$*)
else
do-%:
	@$(MAKE) -C $(DIR) --jobs=1 $(subst $(DIR)-,,$*)
endif

HR:="=========================================================================="

###########################################################################
# Jeremy's junk

proving:
	find -name 'proving.txt' | perl -ne 'print "-" x 70, "\n", $$_, "\n"; print `cat $$_`;'

# Collecting esc-specs-paper statistics

PAPER_STATS_FILES:=StackAr/stackar.stats QueueAr/queuear.stats DisjSets/dsets.stats StreetNumberSet/sns.stats GeoSegment/geoseg.stats Graph/graph.stats RatNum/ratnum.stats RatPoly/ratpoly.stats Vector/vector.stats FixedSizeSet/fixset.stats
stats-table: paper-stats
	@echo "You should probably have already run  make merge  and possibly made fixups."
	@rm -f stats.tex
	${SCRIPTS}/esc-stats-latex.pl ${PAPER_STATS_FILES} > table-stats.tex
#	cp -p table-stats.tex ${DAIKONDIR}/papers/esc-specs-paper/
	cat table-stats.tex

stats-tables-single: paper-stats
#	${SCRIPTS}/esc-stats-latex.pl -s StackAr/stackar.stats > table-stats-stackar.tex
#	cp -p table-stats-stackar.tex ${DAIKONDIR}/papers/esc-specs-paper/
	${SCRIPTS}/esc-stats-latex.pl -s RatPoly/ratpoly.stats > table-stats-ratpoly.tex
	cp -p table-stats-ratpoly.tex ${DAIKONDIR}/papers/esc-specs-paper/

# x: stats-table stats-tables-single

-include Makefile.user
