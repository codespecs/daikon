### User customizations

# Put user-specific changes (such as changes to the JAVAC variable:
#   JAVAC ?= jikes -g +E +F
# ) in your own Makefile.user file in this directory.
# Make will silently continue if Makefile.user does not exist.
-include Makefile.user

### INV and DAIKONBIN need to be set early, because they are used below.

INV ?= ${DAIKONDIR}

# Test that the directory exists.  There must be a better way to do this.
INV:=$(wildcard $(INV))
ifndef INV
  $(error Environment variable INV is not set to an existing directory)
endif

# Set DAIKONBIN to scripts/ if it exists, otherwise to bin/.
ifeq "$(wildcard ${INV}/scripts)" "${INV}/scripts"
    DAIKONBIN ?= ${INV}/scripts
else
    DAIKONBIN ?= ${INV}/bin
endif


#
# Do some basic configuration checking:
#  - Make sure uname and OSTYPE match up
#  - On Windows, make sure that find is the Unix find and not the Windows find
#
ifeq (cygwin,$(OSTYPE))
  FIND_VERSION := $(shell find -version)
  ifneq (GNU,$(findstring GNU,$(FIND_VERSION)))
    $(error the find command in the path must be the Linux(GNU) find $(FIND_VERSION))
  endif
else
  UNAME := $(shell uname)
  ifeq (CYGWIN,$(findstring CYGWIN,$(UNAME)))
    $(error uname returns '$(UNAME)', but OSTYPE is not 'cygwin')
  endif
endif

# For debugging, uncomment this line, or do:  JAVAC="javac -verbose" make
# JAVAC ?= javac -verbose

JAVAC ?= javac
JAVAC_TARGET ?= -target 5 -source 5
JAVAC_ARGS ?= -g ${JAVAC_TARGET}

JAR ?= jar
ifeq (cygwin,$(OSTYPE))
  JAVA ?= $(DAIKONBIN)/java-cygwin.sh -ea -cp "${CLASSPATH}"
else
  JAVA ?= java -ea
endif

CHECKERSJAR ?= ${DAIKONDIR}/java/lib/checkers.jar

jsr308_imports = checkers.interning.quals.*:checkers.nullness.quals.*:checkers.quals.Dependent:checkers.quals.Unused:typequals.*

### end of user customizations

JAVAC_COMMAND = ${JAVAC} -Xlint -J-Xmx1024m ${JAVAC_ARGS}
# When running the checkers, it is important not to pass in "-source 5".
JAVAC_COMMAND_NO_ARGS = ${JAVAC} -Xlint -J-Xmx1024m



###########################################################################
### Variables
###

# These jar files only exist for the development version, not the
# distributed version, of Daikon.  The if clause avoids a (harmless but
# annoying) javac error message that they don't exist.
ifeq "$(wildcard $(INV)/java/lib/bcel.jar)" "$(INV)/java/lib/bcel.jar"
    CLASSPATH_JARS = :$(INV)/java/lib/bcel.jar:$(INV)/java/lib/java-getopt.jar:$(INV)/java/lib/commons-io.jar:$(INV)/java/lib/junit.jar:$(INV)/java/lib/utilMDE.jar
endif
ifeq "$(wildcard ${CHECKERSJAR})" "${CHECKERSJAR}"
    CLASSPATH_JARS2 = ${CHECKERSJAR}
endif

# Add necessary items to the classpath if they are not already there
ifneq (.:,$(findstring .:,$(subst ;,:,$(CLASSPATH))))
  CLASSPATH := .:$(CLASSPATH)
endif
ifdef DAIKON_CLASSPATH_PREFIX
  CLASSPATH := ${DAIKON_CLASSPATH_PREFIX}:${CLASSPATH}
endif
ifdef CLASSPATH_JARS
	CLASSPATH := $(CLASSPATH):$(CLASSPATH_JARS)
endif
ifdef CLASSPATH_JARS2
  ifneq (checkers.jar,$(findstring checkers.jar,$(CLASSPATH)))
    CLASSPATH := $(CLASSPATH):$(CLASSPATH_JARS2)
  endif
endif
ifneq (tools.jar, $(findstring tools.jar,$(CLASSPATH)))
  CLASSPATH := $(CLASSPATH):${JDKDIR}/lib/tools.jar:${JDKDIR}/jre/lib/rt.jar
endif

# On cygwin, translate the classpath to windows format.  Only do so if
# this has not already been done (which is determined by looking for
# semicolons (;) in the classpath
ifeq (cygwin,$(OSTYPE))
  ifneq (;,$(findstring ;,$(CLASSPATH)))
    CLASSPATH := $(shell cygpath -pw "$(CLASSPATH)")
  endif
endif

export CLASSPATH

# This command should run after the AUTO_GENERATED_FILES are generated, or
# else AUTO_GENERATED_FILES should be appended to it (without duplication).
# Therefore, this assignment uses "=" instead of ":=".
# DO NOT USE THESE in dependencies or the
# find command will get run multiple times.  USE AUTO_GENERATED_FILES instead.
# "-o -path './jakarta-oro*/src/java/examples'" does not work for non-GNU find.
# " | perl -pi -e 's/^\.\///g'" to remove leading "./"
JAVA_FILES = $(shell find . -follow \( -name '*daikon-java*' -o -name '*daikon-instrumented*' -o -name CVS -o -name typequals -o -name 'ReturnBytecodes.java' -o -name '\.\#*' \) -prune -o -name '*.java' -print | grep -v '/jakarta-oro.*/src/java/examples/' | ${DAIKONBIN}/sort-directory-order.pl)
DAIKON_JAVA_FILES = $(shell find daikon/ -name '*.java' -not -name '*\#*' | sort)

#                    $(shell find binary_variables/ -name '*.java' -print)

TAG_FILES = $(shell find . -follow \( -name '*daikon-java*' -o -name CVS -o -wholename ./daikon/util \) -prune -o \( -name '*.java' -o -name '*.java.jpp' \) -print | grep -v '/\.\#' | ${DAIKONBIN}/sort-directory-order.pl)

# When removing files from this list, add them to OLD_AUTO_GENERATED_FILES.
AUTO_GENERATED_FILES = \
	daikon/PptSlice1.java \
	daikon/PptSlice2.java \
	daikon/PptSlice3.java \
	daikon/Quant.java \
	daikon/inv/ternary/threeScalar/FunctionBinary.java \
	daikon/inv/ternary/threeScalar/FunctionBinaryFloat.java \
	daikon/inv/ternary/threeScalar/ThreeScalar.java \
	daikon/inv/ternary/threeScalar/ThreeFloat.java \
	daikon/inv/ternary/threeScalar/LinearTernaryCore.java \
	daikon/inv/ternary/threeScalar/LinearTernaryCoreFloat.java \
	daikon/inv/ternary/threeScalar/LinearTernary.java \
	daikon/inv/ternary/threeScalar/LinearTernaryFloat.java \
	daikon/inv/unary/LowerBoundCore.java \
	daikon/inv/unary/UpperBoundCore.java \
	daikon/inv/unary/LowerBoundCoreFloat.java \
	daikon/inv/unary/UpperBoundCoreFloat.java \
	daikon/inv/unary/scalar/LowerBound.java \
	daikon/inv/unary/scalar/UpperBound.java \
	daikon/inv/unary/scalar/LowerBoundFloat.java \
	daikon/inv/unary/scalar/UpperBoundFloat.java \
	daikon/inv/unary/scalar/OneOfScalar.java \
	daikon/inv/unary/scalar/OneOfFloat.java \
	daikon/inv/unary/string/OneOfString.java \
	daikon/inv/unary/scalar/NonZero.java \
	daikon/inv/unary/scalar/NonZeroFloat.java \
	daikon/inv/unary/scalar/RangeInt.java \
	daikon/inv/unary/scalar/RangeFloat.java \
	daikon/inv/unary/sequence/EltRangeInt.java \
	daikon/inv/unary/sequence/EltRangeFloat.java \
	daikon/inv/unary/sequence/OneOfSequence.java \
	daikon/inv/unary/sequence/OneOfFloatSequence.java \
	daikon/inv/unary/sequence/EltOneOf.java \
	daikon/inv/unary/sequence/EltOneOfFloat.java \
	daikon/inv/unary/sequence/EltLowerBound.java \
	daikon/inv/unary/sequence/EltLowerBoundFloat.java \
	daikon/inv/unary/sequence/EltUpperBound.java \
	daikon/inv/unary/sequence/EltUpperBoundFloat.java \
	daikon/inv/unary/sequence/NoDuplicates.java \
	daikon/inv/unary/sequence/NoDuplicatesFloat.java \
	daikon/inv/unary/sequence/SeqIndexIntEqual.java \
	daikon/inv/unary/sequence/SeqIndexIntNonEqual.java \
	daikon/inv/unary/sequence/SeqIndexIntGreaterThan.java \
	daikon/inv/unary/sequence/SeqIndexIntGreaterEqual.java \
	daikon/inv/unary/sequence/SeqIndexIntLessThan.java \
	daikon/inv/unary/sequence/SeqIndexIntLessEqual.java \
	daikon/inv/unary/sequence/SeqIndexFloatEqual.java \
	daikon/inv/unary/sequence/SeqIndexFloatNonEqual.java \
	daikon/inv/unary/sequence/SeqIndexFloatGreaterThan.java \
	daikon/inv/unary/sequence/SeqIndexFloatGreaterEqual.java \
	daikon/inv/unary/sequence/SeqIndexFloatLessThan.java \
	daikon/inv/unary/sequence/SeqIndexFloatLessEqual.java \
	daikon/inv/unary/sequence/CommonSequence.java \
	daikon/inv/unary/sequence/CommonFloatSequence.java \
	daikon/inv/unary/sequence/EltNonZero.java \
	daikon/inv/unary/sequence/EltNonZeroFloat.java \
	daikon/inv/unary/sequence/EltwiseIntComparison.java \
	daikon/inv/unary/sequence/EltwiseFloatComparison.java \
	daikon/inv/unary/sequence/EltwiseIntEqual.java \
	daikon/inv/unary/sequence/EltwiseIntLessThan.java \
	daikon/inv/unary/sequence/EltwiseIntLessEqual.java \
	daikon/inv/unary/sequence/EltwiseIntGreaterThan.java \
	daikon/inv/unary/sequence/EltwiseIntGreaterEqual.java \
	daikon/inv/unary/sequence/EltwiseFloatEqual.java \
	daikon/inv/unary/sequence/EltwiseFloatLessThan.java \
	daikon/inv/unary/sequence/EltwiseFloatLessEqual.java \
	daikon/inv/unary/sequence/EltwiseFloatGreaterThan.java \
	daikon/inv/unary/sequence/EltwiseFloatGreaterEqual.java \
	daikon/inv/unary/stringsequence/EltOneOfString.java \
	daikon/inv/unary/stringsequence/OneOfStringSequence.java \
	daikon/inv/binary/sequenceScalar/Member.java \
	daikon/inv/binary/sequenceString/MemberString.java \
	daikon/inv/binary/sequenceString/SequenceString.java \
	daikon/inv/binary/sequenceScalar/MemberFloat.java \
	daikon/inv/binary/sequenceScalar/SeqIntLessThan.java \
	daikon/inv/binary/sequenceScalar/SeqIntGreaterThan.java \
	daikon/inv/binary/sequenceScalar/SeqIntLessEqual.java \
	daikon/inv/binary/sequenceScalar/SeqIntGreaterEqual.java\
	daikon/inv/binary/sequenceScalar/SeqIntEqual.java \
	daikon/inv/binary/sequenceScalar/SeqFloatLessThan.java \
	daikon/inv/binary/sequenceScalar/SeqFloatGreaterThan.java \
	daikon/inv/binary/sequenceScalar/SeqFloatLessEqual.java \
	daikon/inv/binary/sequenceScalar/SeqFloatGreaterEqual.java \
	daikon/inv/binary/sequenceScalar/SeqFloatEqual.java \
	daikon/inv/binary/sequenceScalar/SequenceScalar.java \
	daikon/inv/binary/sequenceScalar/SequenceFloat.java \
	daikon/inv/binary/twoScalar/IntEqual.java \
	daikon/inv/binary/twoScalar/FloatEqual.java \
	daikon/inv/binary/twoScalar/IntNonEqual.java \
	daikon/inv/binary/twoScalar/FloatNonEqual.java \
	daikon/inv/binary/twoScalar/IntLessThan.java \
	daikon/inv/binary/twoScalar/FloatLessThan.java \
	daikon/inv/binary/twoScalar/IntLessEqual.java \
	daikon/inv/binary/twoScalar/FloatLessEqual.java \
	daikon/inv/binary/twoScalar/IntGreaterThan.java \
	daikon/inv/binary/twoScalar/FloatGreaterThan.java \
	daikon/inv/binary/twoScalar/IntGreaterEqual.java \
	daikon/inv/binary/twoScalar/FloatGreaterEqual.java \
	daikon/inv/binary/twoScalar/NumericInt.java \
	daikon/inv/binary/twoScalar/NumericFloat.java \
	daikon/inv/binary/twoString/StdString.java \
	daikon/inv/binary/twoScalar/LinearBinaryCore.java \
	daikon/inv/binary/twoScalar/LinearBinaryCoreFloat.java \
	daikon/inv/binary/twoScalar/LinearBinary.java \
	daikon/inv/binary/twoScalar/LinearBinaryFloat.java \
	daikon/inv/binary/twoScalar/TwoScalar.java \
	daikon/inv/binary/twoScalar/TwoFloat.java \
	daikon/inv/binary/twoString/TwoString.java \
	daikon/inv/binary/twoString/StringEqual.java \
	daikon/inv/binary/twoString/StringNonEqual.java \
	daikon/inv/binary/twoString/StringLessThan.java \
	daikon/inv/binary/twoString/StringLessEqual.java \
	daikon/inv/binary/twoString/StringGreaterThan.java \
	daikon/inv/binary/twoString/StringGreaterEqual.java \
	daikon/inv/binary/twoSequence/SeqSeqIntEqual.java \
	daikon/inv/binary/twoSequence/SeqSeqIntLessThan.java \
	daikon/inv/binary/twoSequence/SeqSeqIntGreaterThan.java \
	daikon/inv/binary/twoSequence/SeqSeqIntLessEqual.java \
	daikon/inv/binary/twoSequence/SeqSeqIntGreaterEqual.java \
	daikon/inv/binary/twoSequence/SeqSeqFloatEqual.java \
	daikon/inv/binary/twoSequence/SeqSeqFloatLessThan.java \
	daikon/inv/binary/twoSequence/SeqSeqFloatGreaterThan.java \
	daikon/inv/binary/twoSequence/SeqSeqFloatLessEqual.java \
	daikon/inv/binary/twoSequence/SeqSeqFloatGreaterEqual.java \
	daikon/inv/binary/twoSequence/SeqSeqStringEqual.java \
	daikon/inv/binary/twoSequence/SeqSeqStringLessThan.java \
	daikon/inv/binary/twoSequence/SeqSeqStringGreaterThan.java \
	daikon/inv/binary/twoSequence/SeqSeqStringLessEqual.java \
	daikon/inv/binary/twoSequence/SeqSeqStringGreaterEqual.java \
	daikon/inv/binary/twoSequence/Reverse.java \
	daikon/inv/binary/twoSequence/ReverseFloat.java \
	daikon/inv/binary/twoSequence/SubSequence.java \
	daikon/inv/binary/twoSequence/SubSequenceFloat.java \
	daikon/inv/binary/twoSequence/SuperSequence.java \
	daikon/inv/binary/twoSequence/SuperSequenceFloat.java \
	daikon/inv/binary/twoSequence/SubSet.java \
	daikon/inv/binary/twoSequence/SubSetFloat.java \
	daikon/inv/binary/twoSequence/SuperSet.java \
	daikon/inv/binary/twoSequence/SuperSetFloat.java \
	daikon/inv/binary/twoSequence/PairwiseIntEqual.java \
	daikon/inv/binary/twoSequence/PairwiseIntLessThan.java \
	daikon/inv/binary/twoSequence/PairwiseIntGreaterThan.java \
	daikon/inv/binary/twoSequence/PairwiseIntLessEqual.java \
	daikon/inv/binary/twoSequence/PairwiseIntGreaterEqual.java \
	daikon/inv/binary/twoSequence/PairwiseFloatEqual.java \
	daikon/inv/binary/twoSequence/PairwiseFloatLessThan.java \
	daikon/inv/binary/twoSequence/PairwiseFloatGreaterThan.java \
	daikon/inv/binary/twoSequence/PairwiseFloatLessEqual.java \
	daikon/inv/binary/twoSequence/PairwiseFloatGreaterEqual.java \
	daikon/inv/binary/twoSequence/PairwiseLinearBinary.java \
	daikon/inv/binary/twoSequence/PairwiseLinearBinaryFloat.java \
	daikon/inv/binary/twoSequence/PairwiseStringEqual.java \
	daikon/inv/binary/twoSequence/PairwiseStringLessThan.java \
	daikon/inv/binary/twoSequence/PairwiseStringGreaterThan.java \
	daikon/inv/binary/twoSequence/PairwiseStringLessEqual.java \
	daikon/inv/binary/twoSequence/PairwiseStringGreaterEqual.java \
	daikon/inv/binary/twoSequence/PairwiseLinearBinary.java \
	daikon/inv/binary/twoSequence/PairwiseLinearBinaryFloat.java \
	daikon/inv/binary/twoSequence/PairwiseNumericInt.java \
	daikon/inv/binary/twoSequence/PairwiseNumericFloat.java \
	daikon/inv/binary/twoSequence/PairwiseString.java \
	daikon/inv/binary/twoSequence/TwoSequence.java \
	daikon/inv/binary/twoSequence/TwoSequenceFloat.java \
	daikon/inv/binary/twoSequence/TwoSequenceString.java \
	daikon/inv/binary/twoSequence/TwoSequenceString.java \
	daikon/derive/ternary/SequenceScalarArbitrarySubsequence.java \
	daikon/derive/ternary/SequenceFloatArbitrarySubsequence.java \
	daikon/derive/ternary/SequenceStringArbitrarySubsequence.java \
	daikon/derive/ternary/SequenceScalarArbitrarySubsequenceFactory.java \
	daikon/derive/ternary/SequenceFloatArbitrarySubsequenceFactory.java \
	daikon/derive/ternary/SequenceStringArbitrarySubsequenceFactory.java \
	daikon/derive/binary/SequenceScalarSubscript.java \
	daikon/derive/binary/SequenceFloatSubscript.java \
	daikon/derive/binary/SequenceStringSubscript.java \
	daikon/derive/binary/SequenceScalarSubsequence.java \
	daikon/derive/binary/SequenceFloatSubsequence.java \
	daikon/derive/binary/SequenceStringSubsequence.java \
	daikon/derive/binary/SequenceScalarSubscriptFactory.java \
	daikon/derive/binary/SequenceFloatSubscriptFactory.java \
	daikon/derive/binary/SequenceStringSubscriptFactory.java \
	daikon/derive/binary/SequenceScalarIntersection.java \
	daikon/derive/binary/SequenceFloatIntersection.java \
	daikon/derive/binary/SequenceStringIntersection.java \
	daikon/derive/binary/SequenceScalarIntersectionFactory.java \
	daikon/derive/binary/SequenceFloatIntersectionFactory.java \
	daikon/derive/binary/SequenceStringIntersectionFactory.java \
	daikon/derive/binary/SequenceScalarUnion.java \
	daikon/derive/binary/SequenceFloatUnion.java \
	daikon/derive/binary/SequenceStringUnion.java \
	daikon/derive/binary/SequenceScalarUnionFactory.java \
	daikon/derive/binary/SequenceFloatUnionFactory.java \
	daikon/derive/binary/SequenceStringUnionFactory.java \
	daikon/derive/binary/SequencesPredicate.java \
	daikon/derive/binary/SequencesPredicateFloat.java \
	daikon/derive/binary/SequencesPredicateFactory.java \
	daikon/derive/binary/SequencesPredicateFactoryFloat.java \
	daikon/derive/binary/SequencesJoin.java \
	daikon/derive/binary/SequencesJoinFloat.java \
	daikon/derive/binary/SequencesJoinFactory.java \
	daikon/derive/binary/SequencesJoinFactoryFloat.java \
	daikon/derive/unary/SequenceInitial.java \
	daikon/derive/unary/SequenceInitialFloat.java \
	daikon/derive/unary/SequenceInitialFactory.java \
	daikon/derive/unary/SequenceInitialFactoryFloat.java \

# These files were auto-generated previously; they still might exist in
# some user's directory.
OLD_AUTO_GENERATED_FILES = \
	daikon/inv/binary/twoScalar/FloatComparisonCore.java \
	daikon/inv/binary/twoScalar/FunctionUnary.java \
	daikon/inv/binary/twoScalar/FunctionUnaryCore.java \
	daikon/inv/binary/twoScalar/FunctionUnaryCoreFloat.java \
	daikon/inv/binary/twoScalar/FunctionUnaryFloat.java \
	daikon/inv/binary/twoScalar/IntComparisonCore.java \
	daikon/inv/binary/twoSequence/PairwiseFunctionUnary.java \
	daikon/inv/binary/twoSequence/PairwiseFunctionUnaryFloat.java \
	daikon/inv/unary/sequence/SeqIndexComparison.java \
	daikon/inv/unary/sequence/SeqIndexComparisonFloat.java \
	daikon/inv/unary/sequence/SeqIndexNonEqual.java \
	daikon/inv/unary/sequence/SeqIndexNonEqualFloat.java \
	daikon/inv/binary/twoScalar/NonEqualCore.java \
	daikon/inv/binary/twoScalar/NonEqualCoreFloat.java \
	daikon/inv/binary/sequenceScalar/SequenceScalarFactory.java \
	daikon/inv/binary/sequenceScalar/SequenceFloatFactory.java \
	daikon/inv/binary/twoScalar/TwoScalarFactory.java \
	daikon/inv/binary/twoScalar/TwoFloatFactory.java \
	daikon/inv/binary/twoSequence/TwoSequenceFactory.java \
	daikon/inv/binary/twoSequence/TwoSequenceFactoryFloat.java \
	daikon/inv/unary/sequence/SingleScalarSequenceFactory.java \
	daikon/inv/unary/sequence/SingleFloatSequenceFactory.java \
	daikon/inv/ternary/threeScalar/ThreeScalarFactory.java \
	daikon/inv/binary/sequenceString/Member.java \
	daikon/inv/ternary/threeScalar/FunctionBinaryCore.java \
	daikon/inv/Functions.java \
	daikon/inv/FunctionsFloat.java \
	daikon/inv/ternary/threeScalar/FunctionBinaryCoreFloat.java


OLD_AUTO_GENERATED_CLASSES = $(subst .java,.class,${OLD_AUTO_GENERATED_FILES})


# These files cannot be generated until everything else has been compiled.
AUTO_GENERATED_FILES_2 = \
	daikon/test/split/SplitterFactoryTest.java \

AUTO_GENERATED_CLASSES_2 = $(subst .java,.class,${AUTO_GENERATED_FILES_2})

ALL_GENERATED_FILES = ${AUTO_GENERATED_FILES} ${AUTO_GENERATED_FILES_2} lib/utilMDE.jar


###########################################################################
### Variables passed down to subshells
###

export jsr308_imports
export CLASSPATH
export CHECKERSJAR
export DAIKONDIR


###########################################################################
### Compilation targets
###

default: all
compile: all

# "-S" means do not continue after errors.
all: all_stop_on_error

# Don't bother to run the unit tests if the compilation failed.
all_stop_on_error:
	$(MAKE) --stop all_directly junit

## In general, don't use this; it misses some files
all_via_daikon: chicory daikon/Daikon.class daikon/Runtime.class daikon/diff/Diff.class
	$(MAKE) all_pass2

xlint: chicory ${ALL_GENERATED_FILES}
	@${JAVAC_COMMAND} -Xlint -Xmaxwarns 1000 ${DAIKON_JAVA_FILES} 2>&1

## ${JAVA_FILES} is so long I can't see the result on the same screen as the command.
all_directly: ${AUTO_GENERATED_FILES} lib/utilMDE.jar utilMDE-import-check utilMDE-copy
	@echo ${JAVA_FILES} > java_files.txt
	${JAVAC_COMMAND} @java_files.txt
	rm -f java_files.txt
	@chmod -w ${AUTO_GENERATED_FILES}
# This creates ${AUTO_GENERATED_FILES_2}, so that shouldn't be a prerequisite.
	$(MAKE) all_pass2
# These can't be prerequisites because they depend on .class files from Daikon itself.
	$(MAKE) chicory
	$(MAKE) dyncomp

.PHONY : daikon
daikon: compile_daikon junit
compile_daikon : $(AUTO_GENERATED_FILES) lib/utilMDE.jar
	@echo $(JAVAC_COMMAND) 'daikon/*.java ...'
	@$(JAVAC_COMMAND) $(DAIKON_JAVA_FILES)
	@chmod -w ${AUTO_GENERATED_FILES}
	# $(MAKE) all_pass2

#
# Chicory java front end for Daikon
#
CHICORY_JAVA_FILES := daikon/Chicory.java daikon/chicory/*.java

# This actually requires Daikon to already be compiled.
# It would be good to make that an explicit prerequisite.
chicory : ChicoryPremain.jar
ChicoryPremain.jar : $(wildcard $(CHICORY_JAVA_FILES)) \
					 daikon/chicory/manifest.txt
	$(JAVAC_COMMAND) $(CHICORY_JAVA_FILES)
	$(JAR) cfm ChicoryPremain.jar daikon/chicory/manifest.txt \
	  daikon/chicory/ChicoryPremain.class

chicory-test : ChicoryPremain.jar
	$(JAVA) -ea daikon.Chicory --verbose --debug --output-dir=/tmp \
	  daikon.chicory.Test

#
# Dynamic Comparability (DynComp/Java)
#
DCOMP_JAVA_FILES := daikon/dcomp/*java daikon/chicory/*.java daikon/util/*.java
DCOMP_ARGS := --ppt-select-pattern=daikon.dcomp.Test
ORIG_RT ?= $(JDKDIR)/jre/lib/rt.jar
DCOMP_DIR := daikon/dcomp
JAVA_DCOMP = java -ea -esa -Xbootclasspath:dcomp_rt.jar:$(CLASSPATH)
JAVA_DCOMP = java -ea -esa -Xbootclasspath:$(DCOMP_RT):$(CLASSPATH)

# Build DynComp/Java, check the DF summary info and run a sanity test
dyncomp dcomp : utilMDE-import-check utilMDE-copy dcomp_premain.jar dcomp-test
dcomp_premain.jar : $(wildcard $(DCOMP_JAVA_FILES)) daikon/dcomp/manifest.txt
	$(JAVAC_COMMAND) $(DCOMP_JAVA_FILES)
	$(JAVA) daikon.dcomp.SummaryInfo
	$(JAR) cfm dcomp_premain.jar daikon/dcomp/manifest.txt \
	  daikon/dcomp/Premain.class

# Runs a simple sanity test on DynComp/Java without an instrumented JDK
dcomp-test : daikon/dcomp/std_dcomp_out.diff
daikon/dcomp/std_dcomp_out.diff : dcomp_premain.jar
	/bin/rm -rf /tmp/$(USER)/bin/* /tmp/$(USER)/orig/*
	$(JAVA) \
	  daikon.DynComp $(DCOMP_ARGS) \
	  --compare-sets-file=daikon/dcomp/std_dcomp_out.txt --no-jdk \
	  daikon/dcomp/Test
	-diff -uw daikon/dcomp/std_dcomp_out.goal daikon/dcomp/std_dcomp_out.txt \
	  > daikon/dcomp/std_dcomp_out.diff
	@if test ! -s daikon/dcomp/std_dcomp_out.diff ; then echo No Errors; \
	else echo Errors: more daikon/dcomp/std_dcomp_out.diff for details; fi

dyncomp-test-jdk dcomp-test-jdk : dcomp_premain.jar
	/bin/rm -rf /tmp/$(USER)/bin/* /tmp/$(USER)/orig/*
	$(JAVA_DCOMP) \
	   daikon.DynComp $(DCOMP_ARGS) \
	   --compare-sets-file=daikon/dcomp/jdk_dcomp_out.txt daikon/dcomp/Test
	-diff -u daikon/dcomp/jdk_dcomp_out.goal daikon/dcomp/jdk_dcomp_out.txt \
	  > daikon/dcomp/jdk_dcomp_out.diff
	@cat daikon/dcomp/jdk_dcomp_out.diff
	@find daikon/dcomp -name '*.diff' -printf "%8s    %p\n"

dyncomp-update-goals dcomp-update-goals:
	cp daikon/dcomp/std_dcomp_out.txt daikon/dcomp/std_dcomp_out.goal
	cp daikon/dcomp/jdk_dcomp_out.txt daikon/dcomp/jdk_dcomp_out.goal
#
# Rules to build the instrumented JDK for DynComp/Java
#
DCOMP_RT	:= dcomp-rt
#DCOMP_JDK	:= /tmp/$(USER)/dcomp-j2sdk-1.5
J2SDK15		:= /usr/lib/j2sdk1.5-sun
dyncomp-jdk dcomp-jdk   : dcomp_rt.jar
dcomp_rt.jar : dcomp_premain.jar
	/bin/rm -rf $(DCOMP_RT)
	install -d $(DCOMP_RT)
	$(JAVA) -Xmx1024m daikon.dcomp.BuildJDK "$(ORIG_RT)" $(DCOMP_RT)
	$(JAR) cmf $(DCOMP_RT)/META-INF/MANIFEST.MF dcomp_rt.jar -C $(DCOMP_RT) .

dyncomp-jdk-no-primitives dcomp-jdk-no-primitives  : dcomp_premain.jar
	/bin/rm -rf $(DCOMP_RT)
	install -d $(DCOMP_RT)
	$(JAVA) -Xmx1024m daikon.dcomp.BuildJDK --no-primitives $(ORIG_RT) $(DCOMP_RT)
	$(JAR) cmf $(DCOMP_RT)/META-INF/MANIFEST.MF dcomp_rt.jar -C $(DCOMP_RT) .

# Install the local version of the dyncomp specific rt.jar
install-dyncomp-jdk install-dcomp-jdk : dcomp_rt.jar
	cp dcomp_rt.jar $(pag)/software/arch/common/pkg/DynComp/

# Only rebuild the jar file, don't recreate the contents of $(DCOMP_RT).
# Useful if editing only one file in the jar.
dyncomp-jdk-only dcomp-jdk-only :
	$(JAR) cmf $(DCOMP_RT)/META-INF/MANIFEST.MF dcomp_rt.jar -C $(DCOMP_RT) .

# daikon.util is a copy of utilMDE.
# Copied source files are read-only, to prevent accidental changes.
# Use this file as a "canary" for copying everything.
.PHONY: utilMDE-copy
utilMDE-copy : daikon/util/README

daikon/util/README: $(wildcard utilMDE/*.java utilMDE/*.jpp utilMDE/*.manifest)\
					 utilMDE/Makefile utilMDE/README
	$(MAKE) -C utilMDE
	/bin/rm -rf daikon/util
	install -d daikon/util/
	cp utilMDE/*.java daikon/util
	chmod 444 daikon/util/*.java
	#install -t daikon/util/ -m 444 utilMDE/*.java
	install -m 444 daikon/dcomp/daikon.util.README daikon/util/README
	# change the package declarations
	perl -p -i -e 's/package utilMDE/package daikon.util/' daikon/util/*.java
	# change relevant imports (make sure to get "import static" as well)
	perl -p -i -e 's/^(import .*)utilMDE/\1daikon.util/g' daikon/util/*.java
	$(JAVAC_COMMAND) daikon/util/*.java

## Old version.  Unfortunately, it always re-made everything.
## That is unnecessarily slow, and it also produces a lot of output that
## obscures the interesting output.
# utilMDE-copy : utilMDE-import-check
# 	/bin/rm -rf daikon/util
# 	install -d daikon/util/
# 	# Must compile utilMDE in order to create .java files from .jpp files.
# 	make -C utilMDE
# 	cp utilMDE/*.java daikon/util
# 	chmod 444 daikon/util/*.java
# 	#install -t daikon/util/ -m 444 utilMDE/*.java
# 	install -m 444 daikon/dcomp/daikon.util.README daikon/util/README
# 	# change the package declarations
# 	perl -p -i -e 's/package utilMDE/package daikon.util/' daikon/util/*.java
# 	# change relevant imports (make sure to get "import static" as well)
# 	perl -p -i -e 's/^(import .*)utilMDE/\1daikon.util/g' daikon/util/*.java
# 	$(JAVAC_COMMAND) daikon/util/*.java

# Checks that daikon.util is imported by DynComp source files, rather
# than utilMDE
utilMDE-import-check :
	@echo -n "Checking that imports of utilMDE and daikon.util are correct..."
	@if [ -n "`grep '^import utilMDE' $(DCOMP_JAVA_FILES)`" ]; then \
	echo ""; \
	echo "*** ERROR: Do not import utilMDE from DynComp code. It will be"; \
	echo " instrumented when DynComp is run. Please use the (identical)"; \
	echo " daikon.util package, which will not be instrumented."; \
	echo "The following files import from utilMDE:"; \
	grep -n '^import utilMDE' $(DCOMP_JAVA_FILES); \
	else \
	echo "done"; \
	fi

#
# Web Services Matching targets
#
WSMATCH_JAVA_FILES = daikon/tools/WSMatch.java daikon/chicory/*.java
wsmatch : daikon/tools/WSMatch.class
daikon/tools/WSMatch.class : $(wildcard $(WSMATCH_JAVA_FILES)) lib/utilMDE.jar
	$(JAVAC_COMMAND) $(WSMATCH_JAVA_FILES)

## Use this when you don't want $inv/tests makefiles to restart
JAVA_FILES_EXCEPT_DAIKON = $(subst ./daikon/Daikon.java,,${JAVA_FILES})
all_except_daikon: ${AUTO_GENERATED_FILES} lib/utilMDE.jar
	@echo ${JAVAC_COMMAND} '*.java ... (except Daikon.java)'
	@${JAVAC_COMMAND} ${JAVA_FILES_EXCEPT_DAIKON}
	@chmod -w ${AUTO_GENERATED_FILES}
	$(MAKE) all_pass2

all_pass2: ${AUTO_GENERATED_CLASSES_2}
	${JAVAC_COMMAND} ${AUTO_GENERATED_FILES_2}

# all_pass2: ${AUTO_GENERATED_FILES_2}
#	${JAVAC_COMMAND} ${AUTO_GENERATED_FILES_2}

all_except: all_except_daikon

all_force:	all_directly

all_via_javac:
	$(MAKE) JAVAC='javac' all

all_javac:	all_via_javac

javac:	all_via_javac

all_via_jikes:
	$(MAKE) JAVAC='jikes +E +F' all

all_jikes:	all_via_jikes

jikes:	all_via_jikes

# These rules are undesirable, because Daikon.class (or Diff.class) might
# be up to date even if other files aren't.
daikon/Daikon.class: daikon/Daikon.java	${AUTO_GENERATED_FILES} lib/utilMDE.jar
	${JAVAC_COMMAND} daikon/Daikon.java

daikon/Diff.class: Diff.java ${AUTO_GENERATED_FILES} lib/utilMDE.jar
	${JAVAC_COMMAND} daikon/Diff.java

daikon/Runtime.class: daikon/Runtime.java lib/utilMDE.jar
	${JAVAC_COMMAND} daikon/Runtime.java


###########################################################################
### Non-compilation targets
###

# It's good to run "make clean" occasionally, because it costs little to
# regenerate .class files and it's bad to continue to use an orphaned class
# file (whose source file was renamed).  Or, run "orphaned-class-files".
# The (slight) downside is that the regenerated .class files might cause some
# Make rules to unnecessarily re-run, if the code hasn't actually changed.
clean_class_files:
	-rm -f `find . -name "*.class" -print`
	# java/daikon might be a symbolic link; the alternative is to pass
	# "-follow" to the above find command, but I don't necessarily
	# want to remove everything reachable through symbolic links.
	-rm -f `find daikon -follow -name "*.class" -print`
	-rm -f ${OLD_AUTO_GENERATED_FILES}

clean:
	${MAKE} clean_class_files
	-rm -f ChicoryPremain.jar
	-rm -rf daikon/util

dist_clean: clean
	-rm -f TAGS

veryclean: very_clean

very-clean: very_clean

very_clean: dist_clean clean-generated-files
	-rm -f dcomp_premain.jar dcomp_rt.jar

clean-generated-files:
	-rm -f ${AUTO_GENERATED_FILES} ${AUTO_GENERATED_FILES_2}

run:	all
	${JAVA} daikon.Daikon

run_gui:	all
	${JAVA} daikon.gui.InvariantsGUI /g1/users/mhao/daikon/inv_files/dsaa.inv 2>&1 | grep -v Font

etags:	tags
TAGS:	tags
tags:
	@echo etags '*.java ...'
	@etags -l java ${TAG_FILES}

# When doing certain search/replace tasks, it's better to omit generated files
tags-nogen: tags-without-generated-files
tags-without-generated-files: clean-generated-files
	@echo etags '*.java ...'
	@etags -l java ${TAG_FILES}
	$(MAKE) compile

utilMDE/utilMDE.jar:
	$(MAKE) -C utilMDE utilMDE.jar

lib/utilMDE.jar: utilMDE/utilMDE.jar
	install -d lib
	cp -p utilMDE/utilMDE.jar $@

###
### Static checks of the code
###

findbugs: .findbugs-output
.PHONY: findbugs

# Takes about 20 minutes on manioc as of June 2005.
.findbugs-output: ../daikon.jar
	$(MAKE) findbugs-nocompile

# WARNING: This does not use the class files, but the .jar file.
# So make sure it's up to date before using this
findbugs-nocompile:
	rm -f .findbugs-output
	~mernst/bin/share/findbugs -emacs -textui -property "findbugs.maskedfields.locals=true" -property "findbugs.de.comment=true" -exclude .findbugs-exclude.xml -maxHeap 750 ../daikon.jar | tee .findbugs-output

# This target is aimed at M-x compile in Emacs.  The output is misleading
# if the .findbugs-output file might be old, or the daikon.jar file is old.
findbugs-cat:
	cat .findbugs-output

../daikon.jar: ${AUTO_GENERATED_FILES} lib/utilMDE.jar compile
	cd .. && $(MAKE) daikon.jar

checkstyle: checkstyle-cs checkstyle-sun
checkstyle-cs: .checkstyle-cschecks-output
checkstyle-sun: .checkstyle-sunchecks-output
.PHONY: checkstyle checkstyle-cs checkstyle-sun

.checkstyle-sunchecks-output: ${AUTO_GENERATED_FILES} lib/utilMDE.jar
	  ${JAVA} \
	    -Dcheckstyle.header.file=/g2/users/mernst/java/checkstyle-3.4/docs/java.header \
	    -Dcheckstyle.suppressions.file=${inv}/java/.checkstyle-suppressions.xml \
	    -Dbasedir=/g2/users/mernst/java/checkstyle-3.4/docs \
	    -jar ~mernst/java/checkstyle-3.4/checkstyle-all-3.4.jar \
	    -c /g2/users/mernst/java/checkstyle-3.4/docs/sun_checks.xml \
	    -r daikon | tee .checkstyle-sunchecks-output

.checkstyle-cschecks-output: ${AUTO_GENERATED_FILES} lib/utilMDE.jar
	  ${JAVA} \
	    -Dcheckstyle.header.file=/g2/users/mernst/java/checkstyle-3.4/docs/java.header \
	    -Dcheckstyle.suppressions.file=.checkstyle-suppressions.xml \
	    -Dtranslation.severity=error \
	    -Dindent.severity=error \
	    -jar /g2/users/mernst/java/checkstyle-3.4/checkstyle-all-3.4.jar \
	    -c /g2/users/mernst/java/checkstyle-3.4/docs/checkstyle_checks.xml \
	    -r daikon | tee .checkstyle-cschecks-output


jlint:
	jlint . | grep -v "Value of referenced variable '.*' may be NULL" | grep -v "Compare strings as object references" | perl -p -e 's:^daikon/::'

# "(cd $inv; maudit daikon)" does not work; I don't understand why.
# Unfortunately, this is way too much output; I need to filter it.
# Maybe write my own script to do that...
audit:
	cd $(HOME)/java && maudit daikon | perl -p -e 's/^daikon\///'


# Pluggable type qualifier checkers

# JAVAC_INTERNING_ARGS = -processor checkers.interning.InterningChecker -implicit:class -Xlint:-processing
JAVAC_INTERNING_ARGS = -processor checkers.interning.InterningChecker -Alint=-dotequals -implicit:class -Xlint:-processing
JAVAC_NULLNESS_ARGS = -processor checkers.nullness.NullnessChecker -implicit:class -Xlint:-processing
## The "-J-Dcheckers.skipClasses='^daikon.Debug'" should really be just for rawness, not for nullness.
JAVAC_NULLNESS_STUBS = -sourcepath ${CHECKERS}/jdk/nullness/src -J-Dcheckers.skipClasses='^daikon.Debug'
JAVAC_PROTOTYPE_ARGS = -processor checkers.basic.BasicChecker -Aquals=typequals.Prototype,typequals.NonPrototype -implicit:class -Xlint:-processing
# JAVAC_PROTOTYPE_STUBS = -Astubs=typequals/prototype-stubs.astub
JAVAC_PROTOTYPE_STUBS = -J-Dcheckers.skipClasses='^java\.'
JAVAC_JAVARI_ARGS = -processor checkers.javari.JavariChecker -implicit:class -Xlint:-processing
JAVAC_JAVARI_STUBS = -sourcepath ${CHECKERS}/jdk/nullness/src
JAVAC_IGJ_ARGS = -processor checkers.igj.IGJChecker -implicit:class -Xlint:-processing
JAVAC_IGJ_STUBS = -sourcepath ${CHECKERS}/jdk/nullness/src
## Remove files that are not fully annotated, but contain the string "@Interned".
INTERNING_JAVA_FILES_SKIP = 'PptTopLevel.java|DynamicConstants.java|daikon/util'

# I should perhaps not count Intern.java, which implements interning and
# so has very many annotations.  Also, VarInfoName is obsolescent, retained
# for backward compatibility only; it is of flawed design and has 60 @Interned
#  annotations and quite a few SuppressWarnings.
INTERNING_JAVA_FILES_SKIP_COUNTS = 'Intern.java|VarInfoName.java'
## Which files to check.
INTERNING_JAVA_FILES = $(shell find . -type f -name '*.java' | egrep -v $(INTERNING_JAVA_FILES_SKIP) | xargs grep -l -i '@Interned' | sort)
INTERNING_JAVA_FILES_COUNTS = $(shell find . -type f -name '*.java' | egrep -v ${INTERNING_JAVA_FILES_SKIP}\|${INTERNING_JAVA_FILES_SKIP_COUNTS} | xargs grep -l -i '@Interned' | sort)
## Temporarily, only a subset of directories.  Expand the scope later.
NULLNESS_JAVA_FILES = \
  $(shell find daikon/ -name '*.java' -print | sort)

#   $(shell find typequals/ -name '*.java')

intern: check-interning
interning: check-interning
check-interning: ${INTERNING_JAVA_FILES}
	${JAVAC_COMMAND_NO_ARGS} ${JAVAC_INTERNING_ARGS} ${INTERNING_JAVA_FILES}

# Run the interning checker on all Java files, not just those that contain an
# @Interned annotation.
interning-all: check-interning-all
check-interning-all: ${ALL_GENERATED_FILES}
	@echo ${JAVAC_COMMAND_NO_ARGS} ${JAVAC_INTERNING_ARGS} ...
	@${JAVAC_COMMAND_NO_ARGS} ${JAVAC_INTERNING_ARGS} ${JAVA_FILES}

check-nullness: check-nullness-all

## Once this actually works, stop using the -warns version
# check-nullness-all: check-nullness-all-withstubs
check-nullness-all: check-nullness-all-withstubs-warns

check-nullness-all-nostubs: ${ALL_GENERATED_FILES} compile-typequals
	@echo ${JAVAC_COMMAND_NO_ARGS} ${JAVAC_NULLNESS_ARGS} ...
	@${JAVAC_COMMAND_NO_ARGS} ${JAVAC_NULLNESS_ARGS} ${NULLNESS_JAVA_FILES}

check-nullness-all-withstubs: ${ALL_GENERATED_FILES} compile-typequals
	@echo ${JAVAC_COMMAND_NO_ARGS} ${JAVAC_NULLNESS_ARGS} ${JAVAC_NULLNESS_STUBS} ...
	@${JAVAC_COMMAND_NO_ARGS} ${JAVAC_NULLNESS_ARGS} ${JAVAC_NULLNESS_STUBS} ${NULLNESS_JAVA_FILES}

check-nullness-all-withstubs-warns: ${ALL_GENERATED_FILES} compile-typequals
	@echo ${JAVAC_COMMAND_NO_ARGS} -Awarns -Xmaxwarns 10000 ${JAVAC_NULLNESS_ARGS} ${JAVAC_NULLNESS_STUBS} ...
	@${JAVAC_COMMAND_NO_ARGS} -Awarns -Xmaxwarns 10000 ${JAVAC_NULLNESS_ARGS} ${JAVAC_NULLNESS_STUBS} ${NULLNESS_JAVA_FILES}

check-prototype: check-prototype-all

check-prototype-all: check-prototype-all-withstubs

check-prototype-all-withstubs: ${ALL_GENERATED_FILES} compile-typequals
	@echo ${JAVAC_COMMAND_NO_ARGS} ${JAVAC_PROTOTYPE_ARGS} ${JAVAC_PROTOTYPE_STUBS} ...
	@${JAVAC_COMMAND_NO_ARGS} ${JAVAC_PROTOTYPE_ARGS} ${JAVAC_PROTOTYPE_STUBS} ${NULLNESS_JAVA_FILES}

check-igj-all-nostubs: ${ALL_GENERATED_FILES}
	@echo ${JAVAC_COMMAND_NO_ARGS} ${JAVAC_IGJ_ARGS} ...
	@${JAVAC_COMMAND_NO_ARGS} ${JAVAC_IGJ_ARGS} ${JAVA_FILES}

check-igj-all-withstubs: ${ALL_GENERATED_FILES}
	@echo ${JAVAC_COMMAND_NO_ARGS} ${JAVAC_IGJ_ARGS} ${JAVAC_IGJ_STUBS} ...
	@${JAVAC_COMMAND_NO_ARGS} ${JAVAC_IGJ_ARGS} ${JAVAC_IGJ_STUBS} ${JAVA_FILES}

check-javari-all-nostubs: ${ALL_GENERATED_FILES}
	@echo ${JAVAC_COMMAND_NO_ARGS} ${JAVAC_JAVARI_ARGS} ...
	@${JAVAC_COMMAND_NO_ARGS} ${JAVAC_JAVARI_ARGS} ${JAVA_FILES}

check-javari-all-withstubs: ${ALL_GENERATED_FILES}
	@echo ${JAVAC_COMMAND_NO_ARGS} ${JAVAC_JAVARI_ARGS} ${JAVAC_JAVARI_STUBS} ...
	@${JAVAC_COMMAND_NO_ARGS} ${JAVAC_JAVARI_ARGS} ${JAVAC_JAVARI_STUBS} ${JAVA_FILES}

TYPEQUALS_JAVA_FILES = $(wildcard typequals/*.java)
TYPEQUALS_CLASSES = $(subst .java,.class,${TYPEQUALS_JAVA_FILES})

compile-typequals: ${TYPEQUALS_CLASSES}
${TYPEQUALS_CLASSES}: ${TYPEQUALS_JAVA_FILES}
	${JAVAC_COMMAND} ${TYPEQUALS_JAVA_FILES}

# These targets run the Interning checker on just one file.  They use ${JAVAC}
# instead of ${JAVAC_COMMAND}, to display a shorter, easier-to-read command.
check-interning-varinfo:
	${JAVAC} ${JAVAC_INTERNING_ARGS} daikon/VarInfo.java
check-interning-fileio:
	${JAVAC} ${JAVAC_INTERNING_ARGS} daikon/FileIO.java

# Problem with these computations: double-counting of daikon/util/ directory.
# "tail -1" is required when wc is given multiple arguments
interning-counts:
	@echo -n "Annotated files: "
	@echo ${INTERNING_JAVA_FILES_COUNTS} | sed 's/ /\n/g' | wc -l
	#@echo ${INTERNING_JAVA_FILES_COUNTS}
	@echo -n "Lines in annotated files: "
	@wc -l ${INTERNING_JAVA_FILES_COUNTS} | tail -1
	@echo -n "@Interned annotations: "
	@grep "@Interned" ${INTERNING_JAVA_FILES_COUNTS} | wc -l
	@echo -n "Commented @Interned annotations: "
	@grep -E "//.*@Interned" ${INTERNING_JAVA_FILES_COUNTS} | wc -l
	@echo -n "@SuppressWarnings annotations: "
	@grep -n '@SuppressWarnings("interning")' ${INTERNING_JAVA_FILES_COUNTS} | wc -l
	@grep -n '@SuppressWarnings("interning")' ${INTERNING_JAVA_FILES_COUNTS}
	@echo -n "Daikon total files: "
	@echo ${DAIKON_JAVA_FILES} | sed 's/ /\n/g' | wc -l
	@echo -n "Daikon total lines: "
	@wc -l ${DAIKON_JAVA_FILES} | tail -1

# Problem with these computations: double-counting of daikon/util/ directory.
# "tail -1" is required when wc is given multiple arguments
nullness-counts:
	@echo -n "Annotated files: "
	@echo ${NULLNESS_JAVA_FILES} | sed 's/ /\n/g' | wc -l
#	@echo ${NULLNESS_JAVA_FILES}
	@echo -n "Lines in annotated files: "
	@wc -l ${NULLNESS_JAVA_FILES} | tail -1
	@echo -n "Nullness annotations: "
	@grep '@Dependent\|@NonNull\|@Nullable\|@PolyNull\|@Prototype.*result=Nullable\|@Raw' ${NULLNESS_JAVA_FILES}
	@grep '@Dependent\|@NonNull\|@Nullable\|@PolyNull\|@Prototype\|@Raw' ${NULLNESS_JAVA_FILES} | wc -l
	@echo -n "@SuppressWarnings annotations: "
	@grep -n '@SuppressWarnings("nullness")' ${NULLNESS_JAVA_FILES} | wc -l
	@grep -n '@SuppressWarnings("nullness")' ${NULLNESS_JAVA_FILES}
	@echo -n "Daikon total files: "
	@echo ${DAIKON_JAVA_FILES} | sed 's/ /\n/g' | wc -l
	@echo -n "Daikon total lines: "
	@wc -l ${DAIKON_JAVA_FILES} | tail -1


###
### Testing
###

test: tests

tests: junit
	cd ../tests && $(MAKE) all

# run a static check on currently-compiled classes
# ensure serialized classes have a date-style UID assigned
check-serialize:
#	for each non-gui class, run serialver
#	accept only "is not Serializable" or "UID = 200#####" as output
	@find daikon -follow -name 'gui' -prune -o -name '*.class' -print | perl -ne 's|\.class\n$$||; s|/|.|g; print "Checking $$_... \t"; $$_ =~ s|\$$|\\\$$|g; $$msg =`serialver -J-Djava.compiler=NONE $$_ 2>&1`; if ($$msg =~ m/is not Serializable/) { print "ok (not serialized)\n"; next; } if ($$msg =~m/UID = (200\d[01]\d[0123]\d)L;/) { print "ok ($$1)\n"; next; }  print "WARNING: incorrect: $$msg";'


.PHONY: junit
# Include CLASSPATH on command line for Cygwin compatibility.
# Include . in CLASSPATH because you can have two versions of Daikon,
# and you want to do junit tests on *this* version.
junit:
	@echo "Running unit (not system) tests..."
	${JAVA} daikon.test.MasterUnitTester

junit-all: junit
	@# Alternately, MasterUnitTester could run SplitterFactoryTest,
	@# but I'm having trouble with that, apparently due to hard-coded
	@# directory paths.  And these tests are slow, so we want them to
	@# be separate anyway.
	@echo java daikon.test.split.SplitterFactoryTest
	@${JAVA} daikon.test.split.SplitterFactoryTest

###
### Clover (code coverage)
###

CLOVER := $(HOME)/clover
CLOVER_CMD := ${JAVA} -cp $(CLOVER)/clover-1.3.1/lib/clover.jar \
									com.cenqua.clover.CloverInstr -jdk14
CLOVER_CP := $(subst $(INV)/java,$(CLOVER)/java,$(CLASSPATH))
CLOVER_CP := $(CLOVER_CP):$(CLOVER)/clover-1.3.1/lib/clover.jar

clover: all_directly
	/bin/rm -rf $(CLOVER)/java
	mkdir $(CLOVER)/java
	cd $(CLOVER)/java; ln -s $(INV)/java/jtb
	cd $(CLOVER)/java; ln -s $(INV)/java/lib
	cp Makefile $(CLOVER)/java
	mkdir $(CLOVER)/java/daikon
	(cd daikon; tar cf - .) | (cd $(CLOVER)/java/daikon; tar xfBp -)
	chmod -R a+w $(CLOVER)/java/daikon
	mkdir $(CLOVER)/java/utilMDE
	(cd utilMDE; tar cf - .) | (cd $(CLOVER)/java/utilMDE; tar xfBp -)
	chmod -R a+w $(CLOVER)/java/utilMDE
	$(CLOVER_CMD) -i $(CLOVER)/clover.db -s daikon -d $(CLOVER)/java/daikon
	$(CLOVER_CMD) -i $(CLOVER)/clover.db -s utilMDE -d $(CLOVER)/java/utilMDE
	cd $(CLOVER)/java; \
	  ${JAVAC} -classpath $(CLOVER_CP) -g -target 1.1 daikon/Daikon.java

###
### Documentation
###

.PHONY: doc

JAVADOC_DEST  = doc
JAVADOC_FLAGS = -breakiterator -quiet -linksource -noqualifier all \
				-d $(JAVADOC_DEST) \
                -linkoffline http://java.sun.com/j2se/1.5.0/docs/api/ http://java.sun.com/j2se/1.5.0/docs/api/
# javadoc 1.4 doesn't like being given package names; give all filenames explicitly instead
# JDOC_FILES = `find -follow -name '*.java' | grep -v CVS | grep -v '\#'`
JDOC_FILES = $(shell find . -follow \( -name '*daikon-java*' -o -name CVS -o -wholename ./daikon/util \) -prune -o \( -name '*.java' \) -print | grep -v '/\.\#' | grep -v typequals | sort)
#JAVADOC_PACKAGES = `find -follow -name '*.java' -exec dirname {} \; | grep -v CVS | grep -v '\#' | sort | uniq | perl -p -e 's:^./::g; s:/:.:g;'`

# Daikon packages except for util (which is just a copy of utilMDE)
DAIKON_PACKAGES := $(shell ls -F daikon | grep / | egrep -v 'CVS|util' \
				   | sed s./.. | sed s/^/daikon./ | tr '\n' ':')

javadoc: doc

# Build javadoc using the daikon package and all of subpackages of
# daikon (except daikon.util) and utilMDE.  Unfortunately, there is
# no switch to javadoc that ignores a subpackage.  Don't include the
# typequals package -- it crashes javadoc.
doc: $(AUTO_GENERATED_FILES) doc-dkconfig
	mkdir -p doc
	@rm -f java-files.txt
	find . -name "*.java" -print | grep -v typequals > java-files.txt
	javadoc -J-Xmx1000m -J-ea $(JAVADOC_FLAGS) \
		daikon @java-files.txt
	@rm -f java-files.txt
	cd doc && preplace '^ *&#64;cindex.*' ''

## Make doc with private fields for people working on source code
doc-private: $(AUTO_GENERATED_FILES)
	mkdir -p doc
	javadoc -J-Xmx1000m -J-ea $(JAVADOC_FLAGS) -private \
	  daikon -subpackages $(DAIKON_PACKAGES):utilMDE
	cd doc && preplace '^ *&#64;cindex.*' ''

## This doesn't seem necessary any more
# javadoc-lookup: doc-javadoc-lookup
# 
# ## Fix up generated .html files for use by the Emacs `javadoc-lookup' function.
# doc-javadoc-lookup: doc
# 	find doc -name '*.html' | xargs -n 100 ${DAIKONBIN}/jdk-add-munged-anchor-names
# 	echo "Run javadoc-index-to-alist > ~/.javadoc-index.el"

../doc/invariants-doc.texinfo: $(AUTO_GENERATED_FILES)
	javadoc -J-Xmx1000m -J-ea -doclet daikon.config.InvariantDoclet \
		--texinfo $@ \
		$(JDOC_FILES)

doc-invariant: ../doc/invariants-doc.texinfo

# This fails if both . and pwd are on CLASSPATH:  classes are found twice
# resulting in a duplicate class warning.
# This could depend on java/daikon/config/ParameterDoclet.class (but that
# will typically exist).
# It depends on "all", which forces compilation, to avoid errors like
#   "java.lang.ClassFormatError: Truncated class file".
## Experiment with not depending on "all". -MDE
## ../doc/config-options.texinfo: $(AUTO_GENERATED_FILES) all
../doc/config-options.texinfo: $(AUTO_GENERATED_FILES)
	javadoc -J-Xmx1000m -J-ea -doclet daikon.config.ParameterDoclet \
		--texinfo $@ \
		$(JDOC_FILES)
	perl -pi -e 's/&#64;cindex/\@cindex/g' $@


doc-dkconfig: ../doc/config-options.texinfo

# This file is no longer used; there's no point in keeping it around.
# daikon/config/configurable.txt: $(AUTO_GENERATED_FILES)
# 	javadoc -J-ea -doclet daikon.config.ParameterDoclet \
# 		--list $@ \
# 		$(JDOC_FILES)

../doc/category-options.texinfo: $(AUTO_GENERATED_FILES)
	javadoc -J-Xmx1000m -J-ea -doclet daikon.config.ParameterDocletCategory \
		--texinfo $@ \
		$(JDOC_FILES)

doc-category: ../doc/category-options.texinfo

###
### Switching between version 2 and version 3 (temporary)
###

# Recurse to the directory above this since regression tests need to be
# switched too
use-%:
	cd .. && $(MAKE) $@

###
### For debugging the Makefile.
###

showvars:
	@echo AUTO_GENERATED_FILES = ${AUTO_GENERATED_FILES}
	@echo CLASSPATH = "${CLASSPATH}"
	@echo DAIKONBIN = ${DAIKONBIN}
	@echo DAIKON_JAVA_FILES = $(DAIKON_JAVA_FILES)
	@echo INV = "${INV}"
	@echo JAVA = ${JAVA}
	@echo JAVAC = ${JAVAC}
	@echo JAVAC_ARGS = ${JAVAC_ARGS}
	@echo JAVAC_COMMAND = ${JAVAC_COMMAND}
	@echo JAVA_FILES = $(JAVA_FILES)
	@echo TAG_FILES = $(TAG_FILES)
	@echo jsr308_imports = $(jsr308_imports)

showvar_JAVA_FILES:
	@echo ${JAVA_FILES}

showvar_CLASS_FILES:
	@echo $(subst .java,.class,${JAVA_FILES})

list_java_files: showvar_JAVA_FILES


###########################################################################
### Automatically generated files
###

## First, a file not generated via java-cpp.
daikon/test/split/SplitterFactoryTest.java: daikon/test/split/SplitterFactoryTestUpdater.java
	${JAVAC_COMMAND} $<
	rm -f $@
	${JAVA} daikon.test.split.SplitterFactoryTestUpdater
	chmod -w $@

## I don't understand why this is necessary, but it seems to be
daikon/test/split/SplitterFactoryTest.class: daikon/test/split/SplitterFactoryTest.java
	${JAVAC_COMMAND} $<

# Create the auto-generated files with jpp
.PHONY: jpp
jpp: ${AUTO_GENERATED_FILES}

# Use this rule if you do not have cpp or for some other reason do not want
# to refresh the automatically generated .java files.  You should not do
# this if those files are not up-to-date (that is, if you have modified the
# .jpp files).  It is safe to do this with a distrubution that you have
# just unpacked.
avoid-jpp:
	@echo "Any recent changes in the .jpp files will not get propagated."
	touch ${AUTO_GENERATED_FILES}


# ":=" does not work here; the variables get expanded too soon.
# "-e" means exit immediately if any error, rather than returning
# the status of the last command.

# This rule does not work with make 3.77, but does work with make 3.78.
JAVA_CPP ?= (rm -f $@; ${DAIKONBIN}/java-cpp.pl -DDEFINEDVAR $< 2>&1 > $@ && chmod -w $@)
# Like the above, but also makes a backup file
# JAVA_CPP = (mv -f $@ $@-bak; set -e; ${DAIKONBIN}/java-cpp.pl -DDEFINEDVAR $< > $@; chmod -w $@)

# JAVA_CPP = (cp -pf $@ $@-bak; set -e; java-cpp $< -DDEFINEDVAR > $@-postproc; mv -f $@-postproc $@; chmod -w $@)
## Here are alternate ways of writing the same command:
## I could use "define", but that's specific to GNU Make
## I can't do this because the CPP defined variable changes.
# JAVA_CPP = (mv -f $@ $@-bak; set -e; java-cpp $< -DDEFINEDVAR > $@; chmod -w $@)
## These are used like:	$(JAVA_CPP_START) -DLOWER $(JAVA_CPP_END)
# JAVA_CPP_START = (cp -pf $@ $@-bak; set -e; java-cpp $<
# JAVA_CPP_END = > $@-postproc; mv -f $@-postproc $@; chmod -w $@)

daikon/PptSlice1.java: daikon/PptSlice.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ARITY1)

daikon/PptSlice2.java: daikon/PptSlice.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ARITY2)

daikon/PptSlice3.java: daikon/PptSlice.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ARITY3)

daikon/Quant.java: daikon/Quant.java.jpp daikon/QuantBody.java.jpp
	$(JAVA_CPP)

daikon/inv/ternary/threeScalar/FunctionBinary.java: daikon/inv/ternary/threeScalar/FunctionBinary.java.jpp daikon/inv/ternary/threeScalar/FunctionBinaryGeneric.java.jpp daikon/inv/ternary/threeScalar/FunctionBinarySymmetric.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/ternary/threeScalar/FunctionBinaryFloat.java: daikon/inv/ternary/threeScalar/FunctionBinary.java.jpp daikon/inv/ternary/threeScalar/FunctionBinaryGeneric.java.jpp daikon/inv/ternary/threeScalar/FunctionBinarySymmetric.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/ternary/threeScalar/ThreeScalar.java: daikon/inv/ternary/threeScalar/ThreeScalar.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/ternary/threeScalar/ThreeFloat.java: daikon/inv/ternary/threeScalar/ThreeScalar.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/ternary/threeScalar/LinearTernaryCore.java: daikon/inv/ternary/threeScalar/LinearTernaryCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/ternary/threeScalar/LinearTernaryCoreFloat.java: daikon/inv/ternary/threeScalar/LinearTernaryCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/ternary/threeScalar/LinearTernary.java: daikon/inv/ternary/threeScalar/LinearTernary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/ternary/threeScalar/LinearTernaryFloat.java: daikon/inv/ternary/threeScalar/LinearTernary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/LowerBoundCore.java: daikon/inv/unary/BoundCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LOWER)

daikon/inv/unary/UpperBoundCore.java: daikon/inv/unary/BoundCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=UPPER)

daikon/inv/unary/LowerBoundCoreFloat.java: daikon/inv/unary/BoundCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LOWERFLOAT)

daikon/inv/unary/UpperBoundCoreFloat.java: daikon/inv/unary/BoundCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=UPPERFLOAT)

daikon/inv/unary/scalar/NonZero.java: daikon/inv/unary/scalar/NonZero.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/inv/unary/scalar/NonZeroFloat.java: daikon/inv/unary/scalar/NonZero.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/inv/unary/scalar/RangeInt.java: daikon/inv/unary/scalar/Range.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/inv/unary/scalar/RangeFloat.java: daikon/inv/unary/scalar/Range.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/inv/unary/scalar/LowerBound.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LOWER)

daikon/inv/unary/scalar/UpperBound.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=UPPER)

daikon/inv/unary/scalar/LowerBoundFloat.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LOWERFLOAT)

daikon/inv/unary/scalar/UpperBoundFloat.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=UPPERFLOAT)

daikon/inv/unary/scalar/OneOfScalar.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/inv/unary/scalar/OneOfFloat.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/inv/unary/string/OneOfString.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/inv/unary/sequence/EltRangeInt.java: daikon/inv/unary/scalar/Range.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALARSEQ)

daikon/inv/unary/sequence/EltRangeFloat.java: daikon/inv/unary/scalar/Range.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOATSEQ)

daikon/inv/unary/sequence/OneOfSequence.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SEQUENCE)

daikon/inv/unary/sequence/OneOfFloatSequence.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOATSEQUENCE)

daikon/inv/unary/sequence/EltOneOf.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELT)

daikon/inv/unary/sequence/EltOneOfFloat.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELTFLOAT)

daikon/inv/unary/sequence/EltLowerBound.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELTLOWER)

daikon/inv/unary/sequence/EltLowerBoundFloat.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELTLOWERFLOAT)

daikon/inv/unary/sequence/EltUpperBound.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELTUPPER)

daikon/inv/unary/sequence/EltUpperBoundFloat.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELTUPPERFLOAT)

daikon/inv/unary/sequence/NoDuplicates.java: daikon/inv/unary/sequence/NoDuplicates.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/NoDuplicatesFloat.java: daikon/inv/unary/sequence/NoDuplicates.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/sequence/SeqIndexIntEqual.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUAL)

daikon/inv/unary/sequence/SeqIndexFloatEqual.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUALFLOAT)

daikon/inv/unary/sequence/SeqIndexIntNonEqual.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=NONEQUAL)

daikon/inv/unary/sequence/SeqIndexFloatNonEqual.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=NONEQUALFLOAT)

daikon/inv/unary/sequence/SeqIndexIntLessThan.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHAN)

daikon/inv/unary/sequence/SeqIndexFloatLessThan.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHANFLOAT)

daikon/inv/unary/sequence/SeqIndexIntLessEqual.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUAL)

daikon/inv/unary/sequence/SeqIndexFloatLessEqual.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUALFLOAT)

daikon/inv/unary/sequence/SeqIndexIntGreaterThan.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHAN)

daikon/inv/unary/sequence/SeqIndexFloatGreaterThan.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHANFLOAT)

daikon/inv/unary/sequence/SeqIndexIntGreaterEqual.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUAL)

daikon/inv/unary/sequence/SeqIndexFloatGreaterEqual.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUALFLOAT)

daikon/inv/unary/sequence/CommonSequence.java: daikon/inv/unary/sequence/CommonSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/CommonFloatSequence.java: daikon/inv/unary/sequence/CommonSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/sequence/EltNonZero.java: daikon/inv/unary/sequence/EltNonZero.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/EltNonZeroFloat.java: daikon/inv/unary/sequence/EltNonZero.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/sequence/EltwiseIntComparison.java: daikon/inv/unary/sequence/EltwiseIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/EltwiseFloatComparison.java: daikon/inv/unary/sequence/EltwiseIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/sequence/EltwiseIntEqual.java: daikon/inv/unary/sequence/EltwiseIntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUAL)

daikon/inv/unary/sequence/EltwiseIntLessThan.java: daikon/inv/unary/sequence/EltwiseIntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHAN)

daikon/inv/unary/sequence/EltwiseIntLessEqual.java: daikon/inv/unary/sequence/EltwiseIntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUAL)

daikon/inv/unary/sequence/EltwiseIntGreaterThan.java: daikon/inv/unary/sequence/EltwiseIntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHAN)

daikon/inv/unary/sequence/EltwiseIntGreaterEqual.java: daikon/inv/unary/sequence/EltwiseIntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUAL)

daikon/inv/unary/sequence/EltwiseFloatEqual.java: daikon/inv/unary/sequence/EltwiseIntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUALFLOAT)

daikon/inv/unary/sequence/EltwiseFloatLessThan.java: daikon/inv/unary/sequence/EltwiseIntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHANFLOAT)

daikon/inv/unary/sequence/EltwiseFloatLessEqual.java: daikon/inv/unary/sequence/EltwiseIntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUALFLOAT)

daikon/inv/unary/sequence/EltwiseFloatGreaterThan.java: daikon/inv/unary/sequence/EltwiseIntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHANFLOAT)

daikon/inv/unary/sequence/EltwiseFloatGreaterEqual.java: daikon/inv/unary/sequence/EltwiseIntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUALFLOAT)

daikon/inv/unary/stringsequence/EltOneOfString.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELTSTRING)

daikon/inv/unary/stringsequence/OneOfStringSequence.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRINGSEQUENCE)

daikon/inv/binary/sequenceScalar/Member.java: daikon/inv/binary/Member.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/inv/binary/sequenceScalar/MemberFloat.java: daikon/inv/binary/Member.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/inv/binary/sequenceString/MemberString.java: daikon/inv/binary/Member.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/inv/binary/sequenceString/SequenceString.java: daikon/inv/binary/sequenceScalar/SequenceScalar.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPESTRING)

daikon/inv/binary/sequenceScalar/SeqIntLessThan.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHAN)

daikon/inv/binary/sequenceScalar/SeqIntGreaterThan.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHAN)

daikon/inv/binary/sequenceScalar/SeqIntLessEqual.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUAL)

daikon/inv/binary/sequenceScalar/SeqIntGreaterEqual.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUAL)

daikon/inv/binary/sequenceScalar/SeqIntEqual.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUAL)

daikon/inv/binary/sequenceScalar/SeqFloatLessThan.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHANFLOAT)

daikon/inv/binary/sequenceScalar/SeqFloatGreaterThan.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHANFLOAT)

daikon/inv/binary/sequenceScalar/SeqFloatLessEqual.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUALFLOAT)

daikon/inv/binary/sequenceScalar/SeqFloatGreaterEqual.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUALFLOAT)

daikon/inv/binary/sequenceScalar/SeqFloatEqual.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUALFLOAT)

daikon/inv/binary/sequenceScalar/SequenceScalar.java: daikon/inv/binary/sequenceScalar/SequenceScalar.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/sequenceScalar/SequenceFloat.java: daikon/inv/binary/sequenceScalar/SequenceScalar.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/TwoScalar.java: daikon/inv/binary/BinaryInvariant.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/TwoFloat.java: daikon/inv/binary/BinaryInvariant.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/IntEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUAL)

daikon/inv/binary/twoScalar/FloatEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUALFLOAT)

daikon/inv/binary/twoString/TwoString.java: daikon/inv/binary/BinaryInvariant.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPESTRING)

daikon/inv/binary/twoString/StringEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUALSTRING)

daikon/inv/binary/twoScalar/IntNonEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=NONEQUAL)

daikon/inv/binary/twoScalar/FloatNonEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=NONEQUALFLOAT)

daikon/inv/binary/twoString/StringNonEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=NONEQUALSTRING)

daikon/inv/binary/twoScalar/IntLessThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHAN)

daikon/inv/binary/twoScalar/FloatLessThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHANFLOAT)

daikon/inv/binary/twoString/StringLessThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHANSTRING)

daikon/inv/binary/twoScalar/IntLessEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUAL)

daikon/inv/binary/twoScalar/FloatLessEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUALFLOAT)

daikon/inv/binary/twoString/StringLessEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUALSTRING)

daikon/inv/binary/twoScalar/IntGreaterThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHAN)

daikon/inv/binary/twoScalar/FloatGreaterThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHANFLOAT)

daikon/inv/binary/twoString/StringGreaterThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHANSTRING)

daikon/inv/binary/twoScalar/IntGreaterEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUAL)

daikon/inv/binary/twoScalar/FloatGreaterEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUALFLOAT)

daikon/inv/binary/twoString/StringGreaterEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUALSTRING)

daikon/inv/binary/twoScalar/NumericInt.java: daikon/inv/binary/twoScalar/Numeric.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/NumericFloat.java: daikon/inv/binary/twoScalar/Numeric.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoString/StdString.java: daikon/inv/binary/twoScalar/Numeric.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPESTRING)

daikon/inv/binary/twoScalar/LinearBinaryCore.java: daikon/inv/binary/twoScalar/LinearBinaryCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/LinearBinaryCoreFloat.java: daikon/inv/binary/twoScalar/LinearBinaryCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/LinearBinary.java: daikon/inv/binary/twoScalar/LinearBinary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/LinearBinaryFloat.java: daikon/inv/binary/twoScalar/LinearBinary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/TwoSequence.java: daikon/inv/binary/BinaryInvariant.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONGSEQ)

daikon/inv/binary/twoSequence/TwoSequenceFloat.java: daikon/inv/binary/BinaryInvariant.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLESEQ)

daikon/inv/binary/twoSequence/TwoSequenceString.java: daikon/inv/binary/BinaryInvariant.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPESTRINGSEQ)

daikon/inv/binary/twoSequence/SeqSeqIntEqual.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUAL)

daikon/inv/binary/twoSequence/SeqSeqIntLessThan.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHAN)

daikon/inv/binary/twoSequence/SeqSeqIntGreaterThan.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHAN)

daikon/inv/binary/twoSequence/SeqSeqIntLessEqual.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUAL)

daikon/inv/binary/twoSequence/SeqSeqIntGreaterEqual.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUAL)

daikon/inv/binary/twoSequence/SeqSeqFloatEqual.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUALFLOAT)

daikon/inv/binary/twoSequence/SeqSeqFloatLessThan.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHANFLOAT)

daikon/inv/binary/twoSequence/SeqSeqFloatGreaterThan.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHANFLOAT)

daikon/inv/binary/twoSequence/SeqSeqFloatLessEqual.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUALFLOAT)

daikon/inv/binary/twoSequence/SeqSeqFloatGreaterEqual.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUALFLOAT)

daikon/inv/binary/twoSequence/SeqSeqStringEqual.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUALSTRING)

daikon/inv/binary/twoSequence/SeqSeqStringLessThan.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHANSTRING)

daikon/inv/binary/twoSequence/SeqSeqStringGreaterThan.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHANSTRING)

daikon/inv/binary/twoSequence/SeqSeqStringLessEqual.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUALSTRING)

daikon/inv/binary/twoSequence/SeqSeqStringGreaterEqual.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUALSTRING)

daikon/inv/binary/twoSequence/Reverse.java: daikon/inv/binary/twoSequence/Reverse.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoSequence/ReverseFloat.java: daikon/inv/binary/twoSequence/Reverse.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/SubSequence.java: daikon/inv/binary/twoSequence/SubSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SUBLONG)

daikon/inv/binary/twoSequence/SuperSequence.java: daikon/inv/binary/twoSequence/SubSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SUPERLONG)

daikon/inv/binary/twoSequence/SubSequenceFloat.java: daikon/inv/binary/twoSequence/SubSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SUBDOUBLE)

daikon/inv/binary/twoSequence/SuperSequenceFloat.java: daikon/inv/binary/twoSequence/SubSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SUPERDOUBLE)

daikon/inv/binary/twoSequence/SubSet.java: daikon/inv/binary/twoSequence/SubSet.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SUB)

daikon/inv/binary/twoSequence/SubSetFloat.java: daikon/inv/binary/twoSequence/SubSet.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SUBFLOAT)

daikon/inv/binary/twoSequence/SuperSet.java: daikon/inv/binary/twoSequence/SubSet.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SUP)

daikon/inv/binary/twoSequence/SuperSetFloat.java: daikon/inv/binary/twoSequence/SubSet.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SUPFLOAT)

daikon/inv/binary/twoSequence/PairwiseIntEqual.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUAL)

daikon/inv/binary/twoSequence/PairwiseIntLessThan.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHAN)

daikon/inv/binary/twoSequence/PairwiseIntGreaterThan.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHAN)

daikon/inv/binary/twoSequence/PairwiseIntLessEqual.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUAL)

daikon/inv/binary/twoSequence/PairwiseIntGreaterEqual.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUAL)

daikon/inv/binary/twoSequence/PairwiseFloatEqual.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUALFLOAT)

daikon/inv/binary/twoSequence/PairwiseFloatLessThan.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHANFLOAT)

daikon/inv/binary/twoSequence/PairwiseFloatGreaterThan.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHANFLOAT)

daikon/inv/binary/twoSequence/PairwiseFloatLessEqual.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUALFLOAT)

daikon/inv/binary/twoSequence/PairwiseFloatGreaterEqual.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUALFLOAT)

daikon/inv/binary/twoSequence/PairwiseStringEqual.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUALSTRING)

daikon/inv/binary/twoSequence/PairwiseStringLessThan.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHANSTRING)

daikon/inv/binary/twoSequence/PairwiseStringGreaterThan.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHANSTRING)

daikon/inv/binary/twoSequence/PairwiseStringLessEqual.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUALSTRING)

daikon/inv/binary/twoSequence/PairwiseStringGreaterEqual.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp daikon/inv/suppressionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUALSTRING)

daikon/inv/binary/twoSequence/PairwiseLinearBinary.java: daikon/inv/binary/twoSequence/PairwiseLinearBinary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoSequence/PairwiseLinearBinaryFloat.java: daikon/inv/binary/twoSequence/PairwiseLinearBinary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/PairwiseNumericInt.java: daikon/inv/binary/twoScalar/Numeric.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONGSEQ)

daikon/inv/binary/twoSequence/PairwiseNumericFloat.java: daikon/inv/binary/twoScalar/Numeric.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLESEQ)

daikon/inv/binary/twoSequence/PairwiseString.java: daikon/inv/binary/twoScalar/Numeric.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPESTRINGSEQ)

daikon/derive/ternary/SequenceScalarArbitrarySubsequence.java: daikon/derive/ternary/SequenceArbitrarySubsequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/ternary/SequenceFloatArbitrarySubsequence.java: daikon/derive/ternary/SequenceArbitrarySubsequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/derive/ternary/SequenceStringArbitrarySubsequence.java: daikon/derive/ternary/SequenceArbitrarySubsequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/ternary/SequenceScalarArbitrarySubsequenceFactory.java: daikon/derive/ternary/SequenceArbitrarySubsequenceFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/ternary/SequenceFloatArbitrarySubsequenceFactory.java: daikon/derive/ternary/SequenceArbitrarySubsequenceFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/derive/ternary/SequenceStringArbitrarySubsequenceFactory.java: daikon/derive/ternary/SequenceArbitrarySubsequenceFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/SequenceScalarSubscript.java: daikon/derive/binary/SequenceSubscript.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/SequenceFloatSubscript.java: daikon/derive/binary/SequenceSubscript.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/derive/binary/SequenceStringSubscript.java: daikon/derive/binary/SequenceSubscript.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/SequenceScalarSubsequence.java: daikon/derive/binary/SequenceSubsequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/SequenceFloatSubsequence.java: daikon/derive/binary/SequenceSubsequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/derive/binary/SequenceStringSubsequence.java: daikon/derive/binary/SequenceSubsequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/SequenceScalarSubscriptFactory.java: daikon/derive/binary/SequenceSubscriptFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/SequenceFloatSubscriptFactory.java: daikon/derive/binary/SequenceSubscriptFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/derive/binary/SequenceStringSubscriptFactory.java: daikon/derive/binary/SequenceSubscriptFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/SequenceScalarIntersection.java: daikon/derive/binary/SequencesIntersection.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/SequenceFloatIntersection.java: daikon/derive/binary/SequencesIntersection.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/derive/binary/SequenceStringIntersection.java: daikon/derive/binary/SequencesIntersection.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/SequenceScalarIntersectionFactory.java: daikon/derive/binary/SequencesIntersectionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/SequenceFloatIntersectionFactory.java: daikon/derive/binary/SequencesIntersectionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/derive/binary/SequenceStringIntersectionFactory.java: daikon/derive/binary/SequencesIntersectionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/SequenceScalarUnion.java: daikon/derive/binary/SequencesUnion.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/SequenceFloatUnion.java: daikon/derive/binary/SequencesUnion.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/derive/binary/SequenceStringUnion.java: daikon/derive/binary/SequencesUnion.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/SequenceScalarUnionFactory.java: daikon/derive/binary/SequencesUnionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/SequenceFloatUnionFactory.java: daikon/derive/binary/SequencesUnionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/derive/binary/SequenceStringUnionFactory.java: daikon/derive/binary/SequencesUnionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/SequencesPredicate.java: daikon/derive/binary/SequencesPredicate.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/derive/binary/SequencesPredicateFloat.java: daikon/derive/binary/SequencesPredicate.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/derive/binary/SequencesPredicateFactory.java: daikon/derive/binary/SequencesPredicateFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/derive/binary/SequencesPredicateFactoryFloat.java: daikon/derive/binary/SequencesPredicateFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/derive/binary/SequencesJoin.java: daikon/derive/binary/SequencesJoin.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/derive/binary/SequencesJoinFloat.java: daikon/derive/binary/SequencesJoin.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/derive/binary/SequencesJoinFactory.java: daikon/derive/binary/SequencesJoinFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/derive/binary/SequencesJoinFactoryFloat.java: daikon/derive/binary/SequencesJoinFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/derive/unary/SequenceInitial.java: daikon/derive/unary/SequenceInitial.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/derive/unary/SequenceInitialFloat.java: daikon/derive/unary/SequenceInitial.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/derive/unary/SequenceInitialFactory.java: daikon/derive/unary/SequenceInitialFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/derive/unary/SequenceInitialFactoryFloat.java: daikon/derive/unary/SequenceInitialFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)


# See the showvars target; this one seems extraneous
dump :
	@echo "CLASSPATH = $(CLASSPATH)"
	@echo "CLASSPATH_JARS2 = $(CLASSPATH_JARS2)"
	@echo "DAIKONDIR = $(DAIKONDIR)"
	which java
	echo $(OSTYPE)
	echo $(PATH)
	echo $(DAIKONBIN)
	@echo "JAVAC_COMMAND = $(JAVAC_COMMAND)"
	@echo "INTERNING_JAVA_FILES = $(INTERNING_JAVA_FILES)"
	@echo "INTERNING_JAVA_FILES_COUNTS = $(INTERNING_JAVA_FILES_COUNTS)"


###########################################################################
### Delete-on-error
###

# Existence of this rule means that a target is deleted if has just changed
# and its rule commands exit with nonzero status.  It's needed for the
# java-cpp rules above.
# (Actually, it isn't anymore, for I arrange not to overwrite the file
# unless success.  But leave it in for now anyway.)
.DELETE_ON_ERROR:
