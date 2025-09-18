###########################################################################

# Read Makefile.common for what all of this means

#MAIN_CLASS:=MapQuick.PS6PrivateTest # is uselessly long
#MAIN_CLASS:=MapQuick.DirectionsFinderTest # is "original" for esc-specs paper
MAIN_CLASS:=MapQuick.PAGTest # is "augmented" for esc-specs paper

ESC_ON:=1

include ../../Makefile.common

# Just don't want data
# ==========================================================================
# ^MapQuick		: Only want the applicaton (e.g. not JUnit)
# ^MapQuick/tigerdb"	: ImmIterator closure; probably lots of data
# Test | PrivateQueries	: Inner classes, weird crap, etc.
# StreetClassification  : Uninteresting and way too much output in depth = 2

# Instrumentation problems
# ==========================================================================
# StreetSegReader &	: public final static non-primitive init'd inline field in dtrace; not in decls
#  StreetSegIterator

# DirectionsFinder also

instrument-files-revise:
	echo \
		MapQuick/Address.java \
		MapQuick/Directions.java \
		MapQuick/InvalidDatabaseException.java \
		MapQuick/InvalidAddressException.java \
		MapQuick/KillfileReader.java \
		MapQuick/NoPathException.java \
		MapQuick/PriorityQueue.java \
		MapQuick/StreetSegmentFilter.java \
		MapQuick2/CompositeRoute.java \
		MapQuick2/ElementaryRoute.java \
		MapQuick2/GeoSegment.java \
		MapQuick2/GeoPoint.java \
		MapQuick2/Graph.java \
		MapQuick2/Path.java \
		MapQuick2/PathFinder.java \
		MapQuick2/Route.java \
		MapQuick2/RouteDirections.java \
		MapQuick2/TextUI.java \
	    > ${INST_LIST_FILE}

# 	cat ${ALL_LIST_FILE} \
# 		| egrep    "^MapQuick" \
# 		| egrep -v "^MapQuick/tigerdb" \
# 		|  grep -v "Test" | grep -v "PrivateQueries" \
# 		|  grep -v "StreetClassification" \
# 		|  grep -v "StreetSegReader" | grep -v "StreetSegIterator" \
# 	>| ${INST_LIST_FILE}

# run "make separate" to split the trace file into pieces
# run "make invs" to do inference on each
# run "make oneinv" to recominbine into one inv file
# then you may run "make txt-daikon", "make merge", etc.
separate: ${SCRATCH_FOR_DAIKON}/.run-timestamp
	cd ${SCRATCH_FOR_DAIKON} && ${GZCAT} MapQuick.dtrace.gz | trace-separate.pl

SEP_TRACES:=$(shell cd ${SCRATCH_FOR_DAIKON} && ls separate*.dtrace.gz)
SEP_INVS:=$(subst .dtrace,.inv,${SEP_TRACES})
SEP_TXTD:=$(subst .inv.gz,.txt-daikon,${SET_INVS})
SEP_TXTE:=$(subst .inv.gz,.txt-esc,${SET_INVS})

%.inv.gz: ${SCRATCH_FOR_DAIKON}/%.dtrace.gz #${DAIKON_CLASS}
	(${DAIKON} -o $@ ${DAIKON_OPTIONS} ${USER_DAIKON_OPTIONS} --no_text_output --no_show_progress \
	  --config_option daikon.Debug.show_stack_trace=true \
	  --show_progress $< ${DECLS_FILES}) || (rm -f $@ && false)

invs: ${SEP_INVS}

txts: ${SEP_TXTD} ${SEP_TXTE}

oneinv: MapQuick.inv.gz
MapQuick.inv.gz: separate*.inv.gz
	java daikon.UnionInvariants --suppress_redundant -o $@ separate*.inv.gz

# USER_MERGE_ESC_OPTIONS:=--dbg daikon.tools.jtb.MergeESC

# This screws up LOC counting, but we miss edits in non-checked files otherwise
${PROJECT_FILE}.stats:
	@${MAKE} stats-impl --no-print-directory 'STATS_FILES=`find merged/ -name "*.java"`'

sum: ${PROJECT_FILE}.stats.sum
${PROJECT_FILE}.stats.sum: ${PROJECT_FILE}.stats
	${SCRIPTS}/esc-stats-latex.pl $< > $@
	@cat $@
