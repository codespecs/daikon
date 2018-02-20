### User customizations

# Put user-specific changes (such as changes to the JAVAC variable:
#   JAVAC ?= jikes -g +E +F
# ) in your own Makefile.user file in this directory.
# Make will silently continue if Makefile.user does not exist.
-include Makefile.user

# Don't let the user override this; it should refer to the current directory.
DAIKONDIR := $(realpath $(dir $(lastword $(MAKEFILE_LIST)))..)

# Set DAIKONBIN to scripts/ if it exists, otherwise to bin/.
ifeq "$(wildcard ${DAIKONDIR}/scripts)" "${DAIKONDIR}/scripts"
    DAIKONBIN ?= ${DAIKONDIR}/scripts
else
    DAIKONBIN ?= ${DAIKONDIR}/bin
endif

PLUMEBIN ?= ${DAIKONDIR}/utils/plume-lib/bin
HTMLTOOLS ?= ${DAIKONDIR}/utils/html-tools

USER ?= daikonbuild

#
# Do some basic configuration checking:
#  - Make sure uname and OSTYPE match up
#  - On Windows, make sure that find is the Unix find and not the Windows find
#
ifeq (cygwin,$(OSTYPE))
else
  UNAME := $(shell uname)
  ifeq (CYGWIN,$(findstring CYGWIN,$(UNAME)))
    # OSTYPE is a bash variable, not an environment variable.  Set it if necessary
    OSTYPE := cygwin
  endif
endif
ifeq (cygwin,$(OSTYPE))
  FIND_VERSION := $(shell find -version)
  ifneq (GNU,$(findstring GNU,$(FIND_VERSION)))
    $(error the find command in the path must be the Linux(GNU) find $(FIND_VERSION))
  endif
endif

# For debugging, add -verbose flag to javac command

# To run doclint, do
#   JAVADOC="~/research/types/jsr308-langtools/dist/bin/javadoc -Xdoclint:all,-missing" make javadoc
# But doclint is enabled by default starting with Java 8.
ifeq (cygwin, $(OSTYPE))
  ## Make the classpath explicit on the command line so it can be processed by *-cygwin
  JAVA ?= ${PLUMEBIN}/java-cygwin -ea -classpath "${CLASSPATH}"
  JAVAC ?= ${PLUMEBIN}/javac-cygwin -classpath "${CLASSPATH}"
  JAVADOC ?= ${PLUMEBIN}/javadoc-cygwin
  JAR ?= ${JAVA_HOME}/bin/jar
else
  JAVA ?= java -ea
  # JAVAC ?= javac -g -Xlint:unchecked
  # JAVAC ?= jikes -g +E +F
  JAVAC ?= javac
  JAVADOC ?= javadoc
  JAR ?= jar
endif

# Add flags like "-source 5" or "-target 5" to JAVAC_TARGET_FLAGS.
# That variable appears last and overrides the values we provide here.
JAVAC_ARGS ?= -g -XDignore.symbol.file -source 7 -target 7 -Werror -Xlint:-path -Xlint:-options -Xlint:-classfile ${JAVAC_TARGET_FLAGS}

INSTALL ?= /usr/bin/install

# When type-checking, must use checker.jar.
# No harm using it, if available, during ordinary compilation.
ifdef JSR308
  CHECKERFRAMEWORK ?= ${JSR308}/checker-framework
endif
ifdef CHECKERFRAMEWORK
  CHECKERJAR ?= ${CHECKERFRAMEWORK}/checker/dist/checker.jar
endif
# If actually checking, override to checker.jar.
# CHECKERJAR ?= ${DAIKONDIR}/java/lib/plume.jar
# Version of javac that supports pluggable type-checking.
JAVAC_FOR_CHECKER ?= ${CHECKERFRAMEWORK}/checker/bin/javac ${JAVACHECK_EXTRA_ARGS}

### end of user customizations

JAVAC_COMMAND = ${JAVAC} -Xlint -J-Xmx1024m ${JAVAC_ARGS}
# When running the checkers, it is important not to pass in "-source 5".
# Also avoid unqualified "-Xlint" that activates warnings about failure to use == for interned values.
JAVAC_COMMAND_FOR_CHECKER = ${JAVAC_FOR_CHECKER} -J-Xmx2048m



###########################################################################
### Variables
###

# These jar files only exist for the development version, not the
# distributed version, of Daikon.  The if clause avoids a (harmless but
# annoying) javac error message that they don't exist.
ifeq "$(wildcard ${DAIKONDIR}/java/lib/plume.jar)" "${DAIKONDIR}/java/lib/plume.jar"
    CLASSPATH_JARS = :${DAIKONDIR}/java/lib/bcel-util-all-0.0.4.jar:${DAIKONDIR}/java/lib/java-getopt.jar:${DAIKONDIR}/java/lib/options-all-0.3.1.jar:${DAIKONDIR}/java/lib/junit-4.12.jar:${DAIKONDIR}/java/lib/plume.jar
endif
ifeq "$(wildcard ${CHECKERJAR})" "${CHECKERJAR}"
    CLASSPATH_JARS2 = ${CHECKERJAR}
endif

# This probably shouldn't be unconditional??
CLASSPATH := :${DAIKONDIR}/java:$(CLASSPATH)

# Add necessary items to the classpath if they are not already there
ifneq (.:,$(findstring .:,$(subst ;,:,$(CLASSPATH))))
  CLASSPATH := .:$(CLASSPATH)
endif
# This DAIKON_CLASSPATH_PREFIX is intended to be set in Makefile.user
ifdef DAIKON_CLASSPATH_PREFIX
  CLASSPATH := ${DAIKON_CLASSPATH_PREFIX}:${CLASSPATH}
endif
ifdef CLASSPATH_JARS
  CLASSPATH := $(CLASSPATH):$(CLASSPATH_JARS)
endif
ifeq "$(wildcard ${CHECKERFRAMEWORK}/checker/build)" "${CHECKERFRAMEWORK}/checker/build"
  CLASSPATH := $(CLASSPATH):${CHECKERFRAMEWORK}/checker/build
endif
ifdef CLASSPATH_JARS2
  ifneq (${CHECKERJAR}, $(findstring ${CHECKERJAR},$(CLASSPATH)))
    CLASSPATH := $(CLASSPATH):$(CLASSPATH_JARS2)
  endif
endif
ifneq (tools.jar, $(findstring tools.jar,$(CLASSPATH)))
  ifneq "$(wildcard ${JAVA_HOME}/lib/tools.jar)" ""
    CLASSPATH := $(CLASSPATH):${JAVA_HOME}/lib/tools.jar
  endif
  ifneq "$(wildcard ${JAVA_HOME}/lib/rt.jar)" ""
    CLASSPATH := $(CLASSPATH):${JAVA_HOME}/lib/rt.jar
  endif
endif

## Not necessary due to use of *-cygwin versions of Java programs.
# # On cygwin, translate the classpath to windows format.  Only do so if
# # this has not already been done (which is determined by looking for
# # semicolons (;) in the classpath
# ifeq (cygwin,$(OSTYPE))
#   ifneq (;,$(findstring ;,$(CLASSPATH)))
#     CLASSPATH := $(shell cygpath -pw "$(CLASSPATH)")
#   endif
# endif

export CLASSPATH

# This command should run after the AUTO_GENERATED_FILES are generated, or
# else AUTO_GENERATED_FILES should be appended to it (without duplication).
# Therefore, this assignment uses "=" instead of ":=".
# DO NOT USE THESE in dependencies or the
# find command will get run multiple times.  USE AUTO_GENERATED_FILES instead.
# "-o -path './jakarta-oro*/src/java/examples'" does not work for non-GNU find.
# " | perl -pi -e 's/^\.\///g'" to remove leading "./"
SORT_DIRECTORY_ORDER = ${PLUMEBIN}/sort-directory-order
ifneq "$(wildcard ${SORT_DIRECTORY_ORDER})" "${SORT_DIRECTORY_ORDER}"
  # Until "make ../plume-lib" has been run, sort-directory-order is not available.
  SORT_DIRECTORY_ORDER = sort
endif
DAIKON_JAVA_FILES = $(shell find daikon/ -name '*.java' -not -name '*\#*' | ${SORT_DIRECTORY_ORDER})
JAVA_FILES = $(shell find . -follow \( -name '*daikon-java*' -o -name '*daikon-instrumented*' -o -name typequals -o -name 'ReturnBytecodes.java' -o -name 'SplitterFactoryTest.java' -o -name '\.\#*' \) -prune -o -name '*.java' -print | grep -v '/jakarta-oro.*/src/java/examples/' | ${SORT_DIRECTORY_ORDER})
# Copied from
JAVA_FILES_FOR_STYLE = $(shell find . -follow \( -name '*daikon-java*' -o -name '*daikon-instrumented*' -o -name typequals -o -name 'ReturnBytecodes.java' -o -name 'SplitterFactoryTest.java' -o -name 'WeakHasherMap.java' -o -name 'WeakIdentityHashMap.java' -o -name '\.\#*' -o -path './jtb' \) -prune -o -name '*.java' -print | grep -v '/jakarta-oro.*/src/java/examples/' | ${SORT_DIRECTORY_ORDER})
# Other find commands should perhaps use the recursive grep form instead
JAVA_FILES_FOR_FORMAT = $(shell grep -r --include '*.java' --exclude-dir '*daikon-java*' --exclude-dir '*daikon-instrumented*' --exclude WeakHasherMap.java --exclude WeakIdentityHashMap.java -L 'This file is automatically generated' . | grep -v "^./jtb" | grep -v "daikon/util" | ${SORT_DIRECTORY_ORDER})
#                    $(shell find binary_variables/ -name '*.java' -print)

TAG_FILES = $(shell find . -follow \( -name '*daikon-java*' -o -wholename ./daikon/util \) -prune -o \( -name '*.java' -o -name '*.java.jpp' \) -print | grep -v '/\.\#' | ${SORT_DIRECTORY_ORDER})

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
	daikon/inv/ternary/threeScalar/FunctionBinaryCoreFloat.java \
	utilMDE/MathMDE.java \
	lib/utilMDE.jar


OLD_AUTO_GENERATED_CLASSES = $(subst .java,.class,${OLD_AUTO_GENERATED_FILES})


# These files cannot be generated until everything else has been compiled.
# (But, a problem is that it can get out of date and cause the compilation
# to fail during pass 1.)
AUTO_GENERATED_FILES_2 = \
	daikon/test/split/SplitterFactoryTest.java

AUTO_GENERATED_CLASSES_2 = $(subst .java,.class,${AUTO_GENERATED_FILES_2})

# Used as a prerequisite, not for the file names, so OK to include update-libs.
ALL_GENERATED_FILES = update-libs ${AUTO_GENERATED_FILES} ${AUTO_GENERATED_FILES_2}

update-libs:
	make -C .. update-libs
.PHONY: update-libs


###########################################################################
### Variables passed down to subshells
###

export CLASSPATH
export CHECKERJAR
export DAIKONDIR


###########################################################################
### Compilation targets
###

# all_directly is not marked as phony
.PHONY: default compile all all_stop_on_error all_via_daikon
default: all
compile: all

# "-S" means do not continue after errors.
all: all_stop_on_error

# Don't bother to run the unit tests if the compilation failed.
all_stop_on_error:
# if ../.git does not exist, then directory was created from
# a daikon archive file and we cannot do git-hooks
ifneq ($(shell ls ../.git 2>/dev/null),)
	$(MAKE) -C .. git-hooks
endif
	$(MAKE) --stop all_directly junit

## In general, don't use this; it misses some files
all_via_daikon: chicory daikon/Daikon.class daikon/Runtime.class daikon/diff/Diff.class
	$(MAKE) all_pass2

xlint: chicory ${ALL_GENERATED_FILES}
	@${JAVAC_COMMAND} -Xlint -Xmaxwarns 1000 ${DAIKON_JAVA_FILES} 2>&1

## ${JAVA_FILES} is so long I can't see the result on the same screen as the command.
# Do not mark this target as .PHONY.  We want it to
# always recompile, whether recompilation is necessary or not.  (Why?)
all_directly: ../utils/plume-lib ${AUTO_GENERATED_FILES} plume-copy-and-check java_files.txt
# # If SplitterFactoryTestUpdater.java is newer than SplitterFactoryTest.java, then delete the latter.
# 	if [ daikon/test/split/SplitterFactoryTestUpdater.java -nt daikon/test/split/SplitterFactoryTest.java ] ; then rm -f daikon/test/split/SplitterFactoryTest.java; fi
	rm -f `find daikon -name "*.class" -print`
	${JAVAC_COMMAND} @java_files.txt
	rm -f java_files.txt
# on some systems chmod with symbolic arguments pays attention to umask.  so use 444 instead of -w
	@chmod 444 ${AUTO_GENERATED_FILES}
# This creates ${AUTO_GENERATED_FILES_2}, so that shouldn't be a prerequisite.
	$(MAKE) all_pass2
# These can't be prerequisites because they depend on .class files from Daikon itself.
	$(MAKE) chicory
	$(MAKE) dyncomp

java_files.txt: ${JAVA_FILES}
	@echo ${JAVA_FILES} > java_files.txt

.PHONY: plume-copy-and-check
plume-copy-and-check: update-plume-lib plume-lib-compile plume-copy

.PHONY : daikon compile_daikon
daikon: compile_daikon junit
compile_daikon : ../utils/plume-lib $(AUTO_GENERATED_FILES)
	@echo $(JAVAC_COMMAND) 'daikon/*.java ...'
	@$(JAVAC_COMMAND) $(DAIKON_JAVA_FILES)
	@chmod 444 ${AUTO_GENERATED_FILES}
	# $(MAKE) all_pass2

#
# Chicory java front end for Daikon
#
CHICORY_JAVA_FILES := daikon/Chicory.java daikon/chicory/*.java

# This actually requires Daikon to already be compiled.
# It would be good to make that an explicit prerequisite.
.PHONY: chicory
chicory : ChicoryPremain.jar
ChicoryPremain.jar : $(wildcard $(CHICORY_JAVA_FILES)) \
					 daikon/chicory/manifest.txt
	$(JAVAC_COMMAND) $(CHICORY_JAVA_FILES)
	$(JAR) cfm ChicoryPremain.jar daikon/chicory/manifest.txt \
	  daikon/chicory/ChicoryPremain.class
	$(MAKE) chicory-test

.PHONY: chicory-test
chicory-test : daikon/chicory/inv_out.diff
daikon/chicory/inv_out.diff : ChicoryPremain.jar
	cd daikon/chicory && rm -f Test.log Test.dtrace.gz Test.inv.gz Test.inv.out
	cd daikon/chicory && $(JAVA) daikon.Chicory --verbose --debug daikon.chicory.Test > Test.log
	cd daikon/chicory && $(JAVA) daikon.Daikon --no_text_output --no_show_progress Test.dtrace.gz
	cd daikon/chicory && $(JAVA) daikon.PrintInvariants Test.inv.gz > Test.inv.out
	-diff -uw daikon/chicory/Test.inv.out.goal daikon/chicory/Test.inv.out \
	  > daikon/chicory/Test.inv.out.diff
	@if test ! -s daikon/chicory/Test.inv.out.diff ; then echo No Errors; \
	else echo Errors: more daikon/chicory/Test.inv.out.diff for details; fi

#
# Dynamic Comparability (DynComp/Java; also Chicory)
#
INSTRUMENTATION_JAVA_FILES := daikon/DynComp.java daikon/dcomp/*.java daikon/chicory/*.java daikon/util/*.java
DCOMP_ARGS := --ppt-select-pattern=daikon.dcomp.Test
ORIG_RT ?= $(JAVA_HOME)/jre/lib/rt.jar
DCOMP_DIR := daikon/dcomp
JAVA_DCOMP = java -ea -esa -Xbootclasspath:$(DCOMP_RT):$(CLASSPATH)

# Build DynComp/Java, check the DF summary info and run a sanity test
.PHONY: dyncomp dcomp
dyncomp dcomp : plume-copy plume-import-check dcomp_premain.jar
dcomp_premain.jar : $(wildcard $(INSTRUMENTATION_JAVA_FILES)) daikon/dcomp/manifest.txt
	$(JAVAC_COMMAND) $(INSTRUMENTATION_JAVA_FILES)
	$(JAR) cfm dcomp_premain.jar daikon/dcomp/manifest.txt \
	  daikon/dcomp/Premain.class
	$(MAKE) dcomp-test

# Runs a simple sanity test on DynComp/Java without an instrumented JDK
# Also creates file ./Test.decls-DynComp
.PHONY: dcomp-test
dcomp-test : daikon/dcomp/std_dcomp_out.diff
daikon/dcomp/std_dcomp_out.diff : dcomp_premain.jar
	/bin/rm -rf /tmp/$(USER)/bin/* /tmp/$(USER)/orig/*
	$(JAVA) \
	  daikon.DynComp $(DCOMP_ARGS) \
	  --comparability-file=daikon/dcomp/std_dcomp_out.txt --rt-file=NONE \
	  daikon/dcomp/Test
	-diff -uw daikon/dcomp/std_dcomp_out.goal daikon/dcomp/std_dcomp_out.txt \
	  > daikon/dcomp/std_dcomp_out.diff
	@if test ! -s daikon/dcomp/std_dcomp_out.diff ; then echo No Errors; \
	else echo Errors: more daikon/dcomp/std_dcomp_out.diff for details; fi
	@rm -f Test.decls-DynComp

dyncomp-test-jdk dcomp-test-jdk : dcomp_premain.jar
	/bin/rm -rf /tmp/$(USER)/bin/* /tmp/$(USER)/orig/*
	$(JAVA_DCOMP) \
	   daikon.DynComp $(DCOMP_ARGS) \
	   --comparability-file=daikon/dcomp/jdk_dcomp_out.txt daikon/dcomp/Test
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
dcomp_rt.jar : dcomp_premain.jar $(ORIG_RT)
	/bin/rm -rf $(DCOMP_RT)
	$(INSTALL) -d $(DCOMP_RT)
# was 1590
	$(JAVA) -Xmx1500m daikon.dcomp.BuildJDK "$(ORIG_RT)" $(DCOMP_RT)
	$(JAR) cmf $(DCOMP_RT)/META-INF/MANIFEST.MF dcomp_rt.jar -C $(DCOMP_RT) .

dyncomp-jdk-no-primitives dcomp-jdk-no-primitives  : dcomp_premain.jar
	/bin/rm -rf $(DCOMP_RT)
	$(INSTALL) -d $(DCOMP_RT)
	$(JAVA) -Xmx1600m daikon.dcomp.BuildJDK --no-primitives $(ORIG_RT) $(DCOMP_RT)
	$(JAR) cmf $(DCOMP_RT)/META-INF/MANIFEST.MF dcomp_rt.jar -C $(DCOMP_RT) .

# Install the local version of the dyncomp specific rt.jar
install-dyncomp-jdk install-dcomp-jdk : dcomp_rt.jar
	cp dcomp_rt.jar $(pag)/software/arch/common/pkg/DynComp/

# Only rebuild the jar file, don't recreate the contents of $(DCOMP_RT).
# Useful if editing only one file in the jar.
dyncomp-jdk-only dcomp-jdk-only :
	$(JAR) cmf $(DCOMP_RT)/META-INF/MANIFEST.MF dcomp_rt.jar -C $(DCOMP_RT) .

# daikon.util is a copy of plume.
# Copied source files are read-only, to prevent accidental changes.
# Use the README file as a "canary" for copying everything.
# If other files change without the README changing, this won't update.
.PHONY: plume-copy
plume-copy : daikon/util/README

PLUMEJAVA=../utils/plume-lib/java
PLUME=${PLUMEJAVA}/src/plume

## These two rules create the directories if absent but don't update them.
../utils/plume-lib:
	${MAKE} -C .. update-plume-lib

../utils/html-tools:
	${MAKE} -C .. update-html-tools

# Watch out if you have another copy of plume that is earlier on your classpath.
.PHONY: update-plume-lib
update-plume-lib:
	${MAKE} -C .. update-plume-lib

# The class files are not used; this is just to sanity-check
# ... but compilation takes too long, so don't bother with the sanity-check.
plume-lib-compile:
#	${MAKE} -C ../utils/plume-lib/java compile

# # A stable version of plume.jar is checked in, so that all this is not necessary.
# lib/plume.jar: ../utils/plume-lib/java/plume.jar
# 	install -d lib
# 	cp -p $< $@
#
# # The file is real, but the commands should always be re-run even if it exists.
# .PHONY: ../utils/plume-lib/java/plume.jar
# ../utils/plume-lib/java/plume.jar: ../plume-lib
# 	$(MAKE) JAVAC=${JAVAC_FOR_CHECKER} JAVAC_TARGET_FLAGS="-target 5" -C ../utils/plume-lib/java plume.jar

daikon/util/README: update-plume-lib daikon/util/.timestamp-daikonutil

daikon/util/.timestamp-daikonutil: $(wildcard ${PLUME}/*.java ${PLUME}/*.jpp \
  ${PLUMEJAVA}/*.manifest) ${PLUMEJAVA}/Makefile
	$(MAKE) -C ${PLUMEJAVA} compile
	/bin/rm -rf daikon/util
	$(INSTALL) -d daikon/util/
	cp ${PLUME}/*.java daikon/util
	chmod 444 daikon/util/*.java
	#install -t daikon/util/ -m 444 ${PLUME}/*.java
	$(INSTALL) -m 444 daikon/dcomp/daikon.util.README daikon/util/README
	# change the package declarations
	perl -p -i -e 's/package plume/package daikon.util/' daikon/util/*.java
	# change relevant imports (make sure to get "import static" as well)
	perl -p -i -e 's/^(import .*)plume\./\1daikon.util\./g' daikon/util/*.java
	# for debugging
	@echo "DAIKONDIR = ${DAIKONDIR}"
	@echo "CLASSPATH = ${CLASSPATH}"
	$(JAVAC_COMMAND) daikon/util/*.java
	touch daikon/util/.timestamp-daikonutil

# Checks that daikon.util, rather than plume, is imported by source files
# that do instrumentation.  Should also check that daikon.util is never
# imported by non-instrumentation files.
.PHONY: plume-import-check
plume-import-check :
	@echo -n "Checking that imports of plume and daikon.util are correct..."
	@if [ -n "`grep '^import plume' $(INSTRUMENTATION_JAVA_FILES)`" ]; then \
	echo ""; \
	echo "*** ERROR: Do not import plume from DynComp code. It will be"; \
	echo " instrumented when DynComp or Chicory is run. Please use the"; \
	echo " (identical) daikon.util package, which will not be instrumented."; \
	echo "The following files import from plume:"; \
	grep -n '^import plume' $(INSTRUMENTATION_JAVA_FILES); \
	else \
	echo "done"; \
	fi

## Use this when you don't want $inv/tests makefiles to restart
JAVA_FILES_EXCEPT_DAIKON = $(subst ./daikon/Daikon.java,,${JAVA_FILES})
all_except_daikon: ../utils/plume-lib ${AUTO_GENERATED_FILES}
	@echo ${JAVAC_COMMAND} '*.java ... (except Daikon.java)'
	@${JAVAC_COMMAND} ${JAVA_FILES_EXCEPT_DAIKON}
	@chmod -w ${AUTO_GENERATED_FILES}
	$(MAKE) all_pass2

.PHONY: all_pass2
all_pass2: ${AUTO_GENERATED_CLASSES_2}

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
daikon/Daikon.class: daikon/Daikon.java	../utils/plume-lib ${AUTO_GENERATED_FILES}
	${JAVAC_COMMAND} daikon/Daikon.java

daikon/Diff.class: Diff.java ../utils/plume-lib ${AUTO_GENERATED_FILES}
	${JAVAC_COMMAND} daikon/Diff.java

daikon/Runtime.class: daikon/Runtime.java
	${JAVAC_COMMAND} daikon/Runtime.java


###########################################################################
### Cleaning (removing generated files
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
	-rm -f daikon/chicory/Test.log

clean-generated-files:
	-rm -f ${AUTO_GENERATED_FILES} ${AUTO_GENERATED_FILES_2}

# Could/should remove plume-lib here, too.
veryclean: very_clean
very-clean: very_clean
very_clean: clean-generated-files
	-rm -f TAGS
	-rm -rf dcomp_premain.jar dcomp_rt.jar $(DCOMP_RT)
	-rm -rf api
	-rm -rf daikon/dcomp/std_dcomp_out.diff daikon/dcomp/std_dcomp_out.txt


run:	all
	${JAVA} daikon.Daikon

ETAGS := $(shell command -v etags 2> /dev/null)

TAGS:	tags
.PHONY: etags tags
etags:	tags
tags: ../utils/plume-lib
ifndef ETAGS
	$(error etags is not available, please install it)
else
	@echo etags '*.java ...'
ifneq (,$(findstring Exuberant, $(shell etags --version 2>&1)))
	@etags --language-force=java ${TAG_FILES}
else
	@etags --language=java ${TAG_FILES}
endif
endif
# A TAGS file that does not contain generated files
# When doing certain search/replace tasks, it's better to omit generated files
tags-nogen: tags-without-generated-files
tags-sans-generated: tags-without-generated-files
tags-without-generated-files: clean-generated-files
	@echo etags '*.java ...'
	$(MAKE) tags
	$(MAKE) compile


###########################################################################
### Code quality (static checks of the code)
###

# See version numbers at https://github.com/google/error-prone/releases
ERRORPRONE_VERSION := 2.2.0

error-prone: errorprone
errorprone: compile errorprone-nocompile
errorprone-nocompile:
	${MAKE} lib/error_prone_ant-${ERRORPRONE_VERSION}.jar
#	java -Xbootclasspath/p:${DAIKONDIR}/java/lib/error_prone_ant-${ERRORPRONE_VERSION}.jar com.google.errorprone.ErrorProneCompiler -Werror ${JAVA_FILES_FOR_STYLE}
	java -Xbootclasspath/p:${DAIKONDIR}/java/lib/error_prone_ant-${ERRORPRONE_VERSION}.jar com.google.errorprone.ErrorProneCompiler -Xmaxwarns 100000 -Xep:ReferenceEquality:OFF -Xep:StringSplitter:OFF ${JAVA_FILES_FOR_STYLE}
# To create a patch file to apply changes:
#	\rm -f ${DAIKONDIR}/java/error-prone.patch && java -Xbootclasspath/p:${DAIKONDIR}/java/lib/error_prone_ant-${ERRORPRONE_VERSION}.jar com.google.errorprone.ErrorProneCompiler -Xmaxwarns 100000 -XepPatchChecks:MissingOverride,DefaultCharset,DeadException -XepPatchLocation:${DAIKONDIR}/java ${JAVA_FILES_FOR_STYLE}
# This didn't create the file though its output claimed it did:
#	\rm -f ${DAIKONDIR}/java/error-prone.patch && java -Xbootclasspath/p:${DAIKONDIR}/java/lib/error_prone_ant-${ERRORPRONE_VERSION}.jar com.google.errorprone.ErrorProneCompiler -Xmaxwarns 100000 -XepPatchChecks:BoxedPrimitiveConstructor -XepPatchLocation:${DAIKONDIR}/java ${JAVA_FILES_FOR_STYLE}
# To apply the patch, see instructions in the Error Prone output.

lib/error_prone_ant-${ERRORPRONE_VERSION}.jar:
	(cd lib && curl -s -O --retry 3 https://repo1.maven.org/maven2/com/google/errorprone/error_prone_ant/${ERRORPRONE_VERSION}/error_prone_ant-${ERRORPRONE_VERSION}.jar)

findbugs: .findbugs-output
.PHONY: findbugs

# Takes about 20 minutes on manioc as of June 2005.
.findbugs-output: ../daikon.jar
	$(MAKE) findbugs-nocompile

# WARNING: This does not use the class files, but the .jar file.
# So make sure it's up to date before using this
# WARNING: I don't think this will work unless you're logged on as Michael Ernst.
findbugs-nocompile:
	rm -f .findbugs-output
	~mernst/bin/share/findbugs -emacs -textui -property "findbugs.maskedfields.locals=true" -property "findbugs.de.comment=true" -exclude .findbugs-exclude.xml -maxHeap 750 ../daikon.jar | tee .findbugs-output

# This target is aimed at M-x compile in Emacs.  The output is misleading
# if the .findbugs-output file might be old, or the daikon.jar file is old.
findbugs-cat:
	cat .findbugs-output

.PHONY: jar
jar: ../daikon.jar
../daikon.jar: ../utils/plume-lib ${AUTO_GENERATED_FILES} compile
	cd .. && $(MAKE) daikon.jar

checkstyle: checkstyle-cs checkstyle-sun
checkstyle-cs: .checkstyle-cschecks-output
checkstyle-sun: .checkstyle-sunchecks-output
.PHONY: checkstyle checkstyle-cs checkstyle-sun

checkstyle:
# This invocation works on Ubuntu.
	checkstyle -c /usr/share/checkstyle/sun_checks.xml ${JAVA_FILES_FOR_STYLE}

jlint:
	jlint . | grep -v "Value of referenced variable '.*' may be NULL" | grep -v "Compare strings as object references" | perl -p -e 's:^daikon/::'

# "(cd $inv; maudit daikon)" does not work; I don't understand why.
# Unfortunately, this is way too much output; I need to filter it.
# Maybe write my own script to do that...
audit:
	cd $(HOME)/java && maudit daikon | perl -p -e 's/^daikon\///'


.PHONY: reformat check-format

# Requires Java 8
reformat:
ifneq (,$(findstring 1.8.,$(shell java -version 2>&1)))
	${MAKE} -C .. update-run-google-java-format
	@../utils/run-google-java-format/run-google-java-format.py ${JAVA_FILES_FOR_FORMAT}
endif

# Requires Java 8
check-format:
ifneq (,$(findstring 1.8.,$(shell java -version 2>&1)))
	${MAKE} -C .. update-run-google-java-format
	@../utils/run-google-java-format/check-google-java-format.py ${JAVA_FILES_FOR_FORMAT} || (echo "Try running:  make reformat" && /bin/false)
endif


###########################################################################
### Pluggable type qualifier checkers
###

# (Don't run these until after you have compiled Daikon at least once.)

# Note about CHECKER_ARGS: if this contains any -Alint option, it will
# override (not augment) any other use of -Alint in this Makefile.
# "-proc:only" prevents generation of .class files (so generated classfiles are
# version 7 rather than version 8).
CHECKER_ARGS ?= -AprintErrorStack -Xmaxerrs 10000 -Xmaxwarns 10000 -XDignore.symbol.file -Xlint:deprecation -proc:only -Astubs=${CHECKERFRAMEWORK}/checker/lib/javadoc.astub

## All of the following targets need to depend on "compile".

# Just check that the Index Checker doesn't crash -- -AsuppressWarnings="index"
# command-line argument suppresses all type-checking errors.
JAVAC_INDEX_ARGS = -processor org.checkerframework.checker.index.IndexChecker -implicit:class -Xlint:-processing -AskipDefs='${INDEX_SKIPDEFS}' -AassumeAssertionsAreEnabled -AsuppressWarnings="index"
# JAVAC_INTERNING_ARGS = -processor org.checkerframework.checker.interning.InterningChecker -implicit:class -Xlint:-processing
JAVAC_INTERNING_ARGS = -processor org.checkerframework.checker.interning.InterningChecker -Alint=-dotequals -implicit:class -Xlint:-processing -AskipDefs='${INTERNING_SKIPDEFS}'
JAVAC_SIGNATURE_ARGS = -processor org.checkerframework.checker.signature.SignatureChecker -implicit:class -Xlint:-processing -AskipDefs='${ALLSYSTEMS_SKIPDEFS}'
## The "-J-Dcheckers.skipUses='^daikon.Debug'" should really be just for rawness, not for nullness.
JAVAC_NULLNESS_RAW_ARGS = -processor org.checkerframework.checker.nullness.NullnessRawnessChecker -implicit:class -Xlint:-processing -AskipDefs='${ALLSYSTEMS_SKIPDEFS}' -J-Dcheckers.skipUses='^daikon.Debug'
JAVAC_NULLNESS_FBC_ARGS = -processor org.checkerframework.checker.nullness.NullnessChecker -implicit:class -Xlint:-processing -AskipDefs='${ALLSYSTEMS_SKIPDEFS}' -J-Dcheckers.skipUses='^daikon.Debug'
JAVAC_PROTOTYPE_ARGS = -processor org.checkerframework.common.subtyping.SubtypingChecker -Aquals=typequals.Prototype,typequals.NonPrototype -implicit:class -Xlint:-processing -J-Dcheckers.skipUses='^java\.'
JAVAC_VINDEX_ARGS = -processor org.checkerframework.common.subtyping.SubtypingChecker -Aquals=typequals.VIndexTop,typequals.ValueIndex,typequals.VarIndex,typequals.VIndexUnqualified,typequals.VIndexBottom -implicit:class -Xlint:-processing -J-Dcheckers.skipUses='^java\.'
JAVAC_REGEX_ARGS = -processor org.checkerframework.checker.regex.RegexChecker -implicit:class -Xlint:-processing -AskipDefs='${ALLSYSTEMS_SKIPDEFS}'
JAVAC_REGEX_QUAL_ARGS = -processor org.checkerframework.checker.experimental.regex_qual.RegexCheckerAdapter -implicit:class -Xlint:-processing -AskipDefs='${ALLSYSTEMS_SKIPDEFS}'
JAVAC_FORMATTER_ARGS = -processor org.checkerframework.checker.formatter.FormatterChecker -implicit:class -Xlint:-processing -AskipDefs='${ALLSYSTEMS_SKIPDEFS}'
# -AsuppressWarnings=method.guarantee.violated,override.sideeffect.invalid is temporary.
# Once the Purity Checker and -AsuggestPureMethods are fixed, they can be run on plume-lib
# and Daikon and this can be removed. The method.guarantee.violated warning is issued by
# the Lock Checker when a method calls another method with a weaker side effect guarantee.
# The override.sideeffect.invalid warning is issued by the Lock Checker when the side effect
# annotation on the overridder method is weaker than that on the overridden method.
JAVAC_LOCK_ARGS = -processor org.checkerframework.checker.lock.LockChecker -AsuppressWarnings=method.guarantee.violated,override.sideeffect.invalid -implicit:class -Xlint:-processing -AskipDefs='${ALLSYSTEMS_SKIPDEFS}'
JAVAC_JAVARI_ARGS = -processor org.checkerframework.checker.javari.JavariChecker -implicit:class -Xlint:-processing -AskipDefs='${ALLSYSTEMS_SKIPDEFS}'
JAVAC_IGJ_ARGS = -processor org.checkerframework.checker.igj.IGJChecker -implicit:class -Xlint:-processing -AskipDefs='${ALLSYSTEMS_SKIPDEFS}'
## Code in dcomp directory is not called directly; annotations would be useless.
INDEX_SKIPDEFS = ^daikon\.dcomp\|^jtb\.
INTERNING_SKIPDEFS = ^daikon\.dcomp\|^jtb\.
ALLSYSTEMS_SKIPDEFS=^jtb\.
INDEX_JAVA_FILES = ${DAIKON_JAVA_FILES} $(shell find daikon/util/ -name '*.java' | ${SORT_DIRECTORY_ORDER})
# Intern.java implements interning and so has very many annotations.  VarInfoName
# is obsolescent, retained for backward compatibility only; it is of flawed design
# and has 60 @Interned annotations and quite a few SuppressWarnings.
INTERNING_JAVA_FILES_SKIP_COUNTS = Intern.java\|VarInfoName.java\|daikon/dcomp
## Which files to check.
INTERNING_JAVA_FILES = ${DAIKON_JAVA_FILES} $(shell find daikon/util/ -name '*.java' | ${SORT_DIRECTORY_ORDER})
INTERNING_JAVA_FILES_COUNTS = $(shell find . -type f -name '*.java' | egrep -v ${INTERNING_JAVA_FILES_SKIP_COUNTS} | xargs grep -l -i '@Interned' | ${SORT_DIRECTORY_ORDER})
## Setting this to a subset of directories only works if every file has been
## previously compiled with the Java 7 compiler in annotations-in-comments
## mode, so that annotations in the files with @SuppressWarnings are respected.
NULLNESS_JAVA_FILES = \
  ${DAIKON_JAVA_FILES} \
  $(shell find daikon/util/ -name '*.java' | ${SORT_DIRECTORY_ORDER})

#  $(shell find daikon -name '*.java' -a -exec grep -L "^@SuppressWarnings.*nullness" {} \; | sort)
#   $(shell find typequals/ -name '*.java')

show_nullness_java_files:
	@echo NULLNESS_JAVA_FILES:
	@echo ${NULLNESS_JAVA_FILES}
	@echo DAIKON_JAVA_FILES:
	@echo ${DAIKON_JAVA_FILES}

## Reinstate prototype and vindex checkers later.
# check-all: check-formatter check-interning check-nullness check-regex check-signature check-prototype check-vindex
## check-regex-qual target runs extremely slowly.
# check-all: check-formatter check-interning check-nullness check-regex check-regex-qual check-signature
check-all: check-formatter check-interning check-nullness check-regex check-signature check-lock check-index
typecheck: check-all

index: check-index
check-index: daikon/util/README ${INDEX_JAVA_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_INDEX_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_INDEX_ARGS} ${CHECKER_ARGS} ${INDEX_JAVA_FILES}

# Run the index checker on all Java files, not just those in the daikon package.
# For instance, this includes jtb, which is not annotated with @Interned.
index-all: check-index-all
check-index-all: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_INDEX_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_INDEX_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

# This target runs the Index checker on just one file.
# Typical command:
# INDEX_ONEFILE="daikon/derive/binary/SequenceStringIntersection.java daikon/derive/binary/SequenceStringUnion.java daikon/inv/unary/stringsequence/CommonStringSequence.java daikon/Quant.java" make check-index-onefile
INDEX_ONEFILE ?= daikon/PptTopLevel.java
check-index-onefile: ${ALL_GENERATED_FILES} compile
	${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_INDEX_ARGS} ${CHECKER_ARGS} ${INDEX_ONEFILE}

intern: check-interning
interning: check-interning
check-interning: daikon/util/README ${INTERNING_JAVA_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_INTERNING_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_INTERNING_ARGS} ${CHECKER_ARGS} ${INTERNING_JAVA_FILES}

# Run the interning checker on all Java files, not just those in the daikon package.
# For instance, this includes jtb, which is not annotated with @Interned.
interning-all: check-interning-all
check-interning-all: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_INTERNING_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_INTERNING_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

# This target runs the Interning checker on just one file.
# Typical command:
# INTERNING_ONEFILE="daikon/derive/binary/SequenceStringIntersection.java daikon/derive/binary/SequenceStringUnion.java daikon/inv/unary/stringsequence/CommonStringSequence.java daikon/Quant.java" make check-interning-onefile
INTERNING_ONEFILE ?= daikon/PptTopLevel.java
check-interning-onefile: ${ALL_GENERATED_FILES} compile
	${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_INTERNING_ARGS} ${CHECKER_ARGS} ${INTERNING_ONEFILE}

signature: check-signature
check-signature: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_SIGNATURE_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_SIGNATURE_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

# This target runs the Signature checker on just one file.
# Typical command:
# SIGNATURE_ONEFILE="daikon/derive/binary/SequenceStringIntersection.java daikon/derive/binary/SequenceStringUnion.java daikon/inv/unary/stringsequence/CommonStringSequence.java daikon/Quant.java" make check-signature-onefile
# SIGNATURE_ONEFILE ?= daikon/chicory/Instrument.java daikon/dcomp/DCInstrument.java
SIGNATURE_ONEFILE ?= daikon/dcomp/TypeStack.java
check-signature-onefile: ${ALL_GENERATED_FILES} compile
	${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_SIGNATURE_ARGS} ${CHECKER_ARGS} ${SIGNATURE_ONEFILE}

check-nullness: daikon/util/README check-nullness-all

check-nullness-all: check-nullness-all-withstubs

check-nullness-all-nostubs: check-nullness-raw-all-nostubs check-nullness-fbc-all-nostubs

check-nullness-raw: check-nullness-raw-all-withstubs

check-nullness-raw-all-nostubs: ${ALL_GENERATED_FILES} compile-typequals
	$(MAKE) JAVAC=${JAVAC_FOR_CHECKER} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_RAW_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_RAW_ARGS} ${CHECKER_ARGS} ${NULLNESS_JAVA_FILES}

# These targets run the Nullness checker on just one file.
# Typical command:
# NULLNESS_RAW_ONEFILE="daikon/derive/binary/SequenceStringIntersection.java daikon/derive/binary/SequenceStringUnion.java daikon/inv/unary/stringsequence/CommonStringSequence.java daikon/Quant.java" make check-nullness-raw-onefile
# NULLNESS_RAW_ONEFILE ?= daikon/derive/binary/SequenceStringUnion.java
# NULLNESS_RAW_ONEFILE ?= daikon/FileIO.java
NULLNESS_RAW_ONEFILE ?= daikon/inv/Invariant.java
check-nullness-raw-onefile: ${ALL_GENERATED_FILES} compile-typequals
	${MAKE} JAVAC=${JAVAC_FOR_CHECKER} compile
	${MAKE} check-nullness-raw-onefile-norecompile
check-nullness-raw-onefile-norecompile:
	${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_RAW_ARGS} ${CHECKER_ARGS} ${NULLNESS_RAW_ONEFILE}


check-nullness-fbc-all-nostubs: ${ALL_GENERATED_FILES} compile-typequals
	${MAKE} JAVAC=${JAVAC_FOR_CHECKER} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_FBC_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_FBC_ARGS} ${CHECKER_ARGS} ${NULLNESS_JAVA_FILES}

check-nullness-all-withstubs: check-nullness-raw-all-withstubs check-nullness-fbc-all-withstubs

check-nullness-raw-all-withstubs: ${ALL_GENERATED_FILES} compile-typequals
	${MAKE} JAVAC=${JAVAC_FOR_CHECKER} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_RAW_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_RAW_ARGS} ${CHECKER_ARGS} ${NULLNESS_JAVA_FILES}

check-nullness-fbc: check-nullness-fbc-all-withstubs

check-nullness-fbc-all-withstubs: ${ALL_GENERATED_FILES} compile-typequals
	${MAKE} JAVAC=${JAVAC_FOR_CHECKER} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_FBC_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_FBC_ARGS} ${CHECKER_ARGS} ${NULLNESS_JAVA_FILES}


# Version that issues warnings, not errors.
## (Actually, it's identical to the above!  TODO: Fix that.)
check-nullness-raw-all-withstubs-warns: ${ALL_GENERATED_FILES} compile-typequals
	${MAKE} JAVAC=${JAVAC_FOR_CHECKER} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_RAW_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_RAW_ARGS} ${CHECKER_ARGS} ${NULLNESS_JAVA_FILES}

check-nullness-fbc-all-withstubs-warns: ${ALL_GENERATED_FILES} compile-typequals
	${MAKE} JAVAC=${JAVAC_FOR_CHECKER} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_FBC_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_FBC_ARGS} ${CHECKER_ARGS} ${NULLNESS_JAVA_FILES}

# These targets run the Nullness checker on just one file.
# Typical command:
# NULLNESS_ONEFILE="daikon/derive/binary/SequenceStringIntersection.java daikon/derive/binary/SequenceStringUnion.java daikon/inv/unary/stringsequence/CommonStringSequence.java daikon/Quant.java" make check-nullness-onefile
# NULLNESS_ONEFILE ?= daikon/derive/binary/SequenceStringUnion.java
# NULLNESS_ONEFILE ?= daikon/FileIO.java
NULLNESS_ONEFILE ?= daikon/Daikon.java
check-nullness-onefile: check-nullness-fbc-onefile
check-nullness-fbc-onefile: ${ALL_GENERATED_FILES} compile-typequals
	${MAKE} JAVAC=${JAVAC_FOR_CHECKER} compile
	${MAKE} check-nullness-fbc-onefile-norecompile
check-nullness-onefile-norecompile: check-nullness-fbc-onefile-norecompile
check-nullness-fbc-onefile-norecompile:
	${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_NULLNESS_FBC_ARGS} ${CHECKER_ARGS} ${NULLNESS_ONEFILE}

check-prototype: check-prototype-all

check-prototype-all: check-prototype-all-withstubs

check-prototype-all-withstubs: ${ALL_GENERATED_FILES} compile-typequals
	${MAKE} JAVAC=${JAVAC_FOR_CHECKER} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_PROTOTYPE_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_PROTOTYPE_ARGS} ${CHECKER_ARGS} ${NULLNESS_JAVA_FILES}

check-vindex: check-vindex-all

check-vindex-all: check-vindex-all-withstubs

check-vindex-all-withstubs: ${ALL_GENERATED_FILES} compile-typequals
	${MAKE} JAVAC=${JAVAC_FOR_CHECKER} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_VINDEX_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} -Xbootclasspath/p:${DAIKONDIR}/java ${JAVAC_VINDEX_ARGS} ${CHECKER_ARGS} ${NULLNESS_JAVA_FILES}

check-regex: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_REGEX_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_REGEX_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

check-regex-qual: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_REGEX_QUAL_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_REGEX_QUAL_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

check-formatter: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_FORMATTER_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_FORMATTER_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

check-lock: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${CHECKER_ARGS} ${JAVAC_LOCK_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_LOCK_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

check-igj-all-nostubs: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_IGJ_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_IGJ_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

check-igj-all-withstubs: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_IGJ_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_IGJ_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

check-javari-all-nostubs: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_JAVARI_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_JAVARI_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

check-javari-all-withstubs: ${ALL_GENERATED_FILES} compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_JAVARI_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_JAVARI_ARGS} ${CHECKER_ARGS} ${JAVA_FILES}

TYPEQUALS_JAVA_FILES = $(wildcard typequals/*.java)
TYPEQUALS_CLASSES = $(subst .java,.class,${TYPEQUALS_JAVA_FILES})

compile-typequals: ${TYPEQUALS_CLASSES}
${TYPEQUALS_CLASSES}: ${TYPEQUALS_JAVA_FILES}
	${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_ARGS} ${TYPEQUALS_JAVA_FILES}

# These targets run the Interning checker on just one file.  They use ${JAVAC}
# instead of ${JAVAC_COMMAND}, to display a shorter, easier-to-read command.
check-interning-varinfo:
	${JAVAC} ${JAVAC_INTERNING_ARGS} daikon/VarInfo.java
check-interning-fileio:
	${JAVAC} ${JAVAC_INTERNING_ARGS} daikon/FileIO.java

# "tail -1" is required when wc is given multiple arguments
interning-counts:
	@echo -n "Annotated files: "
	@echo ${INTERNING_JAVA_FILES_COUNTS} | sed 's/ /\n/g' | wc -l
#	@echo ${INTERNING_JAVA_FILES_COUNTS}
	@echo -n "Lines in annotated files: "
	@wc -l ${INTERNING_JAVA_FILES_COUNTS} | tail -1
	@echo -n "Lines with @Interned annotations: "
	@grep "@Interned" ${INTERNING_JAVA_FILES_COUNTS} | wc -l
	@echo -n "Lines with commented @Interned annotations: "
	@grep -E "//.*@Interned" ${INTERNING_JAVA_FILES_COUNTS} | wc -l
	@echo -n "@SuppressWarnings annotations: "
	@grep -n '@SuppressWarnings("interning")' ${INTERNING_JAVA_FILES_COUNTS} | wc -l
	@grep -n '@SuppressWarnings("interning")' ${INTERNING_JAVA_FILES_COUNTS}
	@echo -n "Daikon total files: "
	@echo ${DAIKON_JAVA_FILES} | sed 's/ /\n/g' | wc -l
	@echo -n "Daikon total lines: "
	@wc -l ${DAIKON_JAVA_FILES} | tail -1

# This counts annotations in comments, etc.
# "tail -1" is required when wc is given multiple arguments
nullness-counts:
	@echo -n "Annotated files (lines, blank, comments, NCSL, AESL): "
	@echo ${NULLNESS_JAVA_FILES} | xargs sclc | tail -1
	@echo -n "Total files (lines, blank, comments, NCSL, AESL): "
	@echo ${DAIKON_JAVA_FILES} | xargs sclc | tail -1
# 	@echo -n "Annotated files: "
# 	@echo ${NULLNESS_JAVA_FILES} | sed 's/ /\n/g' | wc -l
# 	@echo -n " / "
# 	@echo ${DAIKON_JAVA_FILES} | sed 's/ /\n/g' | wc -l
# #	@echo ${NULLNESS_JAVA_FILES}
# 	@echo -n "Lines in annotated files: "
# 	@echo ${NULLNESS_JAVA_FILES} | xargs wc -l | tail -1 | cut -d ' ' -f 2
# 	@echo -n " / "
# 	@echo ${DAIKON_JAVA_FILES} | xargs wc -l | tail -1 | cut -d ' ' -f 2
	@echo -n "Lines with nullness annotations: "
	@grep '@AssertNonNullIfFalse\|@AssertNonNullIfTrue\|@AssertParametersNonNull\|@KeyFor\|@LazyNonNull\|@NonNull\|@NonNullOnEntry\|@NonRaw\|@Nullable\|@PolyNull\|@PolyRaw\|@Pure\|@Raw' ${NULLNESS_JAVA_FILES} | wc -l
	@echo -n "Lines with @SuppressWarnings annotation or castNonNull method call: "
	@grep '@SuppressWarnings.*nullness\|castNonNull' ${NULLNESS_JAVA_FILES} | wc -l
	@grep -n '@SuppressWarnings.*nullness\|castNonNull' ${NULLNESS_JAVA_FILES}


JAVAC_COUNT_ARGS = -processor org.checkerframework.common.util.count.Locations -implicit:class -Xlint:-processing
possible-annos: ${ALL_GENERATED_FILES} compile-typequals compile
	@echo ${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_COUNT_ARGS} ${CHECKER_ARGS} ...
	@${JAVAC_COMMAND_FOR_CHECKER} ${JAVAC_COUNT_ARGS} ${CHECKER_ARGS} ${NULLNESS_JAVA_FILES}



###########################################################################
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
# TestPlume.java requires the locale to be US.
junit:
	@echo "Running unit (not system) tests..."
	LC_ALL=en_US.UTF-8 ${JAVA} daikon.test.MasterUnitTester
# Does not run daikon.util tests because utils/plume-lib/java/Makefile does not
# get copied over to Daikon.

junit-all: junit
	@# Alternately, MasterUnitTester could run SplitterFactoryTest,
	@# but I'm having trouble with that, apparently due to hard-coded
	@# directory paths.  And these tests are slow, so we want them to
	@# be separate anyway.
	@echo ${JAVA} daikon.test.split.SplitterFactoryTest
	@${JAVA} daikon.test.split.SplitterFactoryTest

###
### Clover (code coverage)
###

CLOVER := $(HOME)/clover
CLOVER_CMD := ${JAVA} -cp $(CLOVER)/clover-1.3.1/lib/clover.jar \
									com.cenqua.clover.CloverInstr -jdk14
CLOVER_CP := $(subst ${DAIKONDIR}/java,$(CLOVER)/java,$(CLASSPATH))
CLOVER_CP := $(CLOVER_CP):$(CLOVER)/clover-1.3.1/lib/clover.jar

clover: all_directly
	/bin/rm -rf $(CLOVER)/java
	mkdir $(CLOVER)/java
	cd $(CLOVER)/java; ln -s ${DAIKONDIR}/java/jtb
	cd $(CLOVER)/java; ln -s ${DAIKONDIR}/java/lib
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


###########################################################################
### Documentation
###

JAVADOC_DEST  = api
# ${DAIKONDIR}/java should not be in classpath, to avoid Javadoc error
# "Multiple sources of package comments found for package"
JAVADOC_FLAGS = -breakiterator -quiet -linksource -noqualifier all \
				-d $(JAVADOC_DEST) \
                -linkoffline https://docs.oracle.com/javase/8/docs/api/ https://docs.oracle.com/javase/8/docs/api/ \
                -docletpath "${CLASSPATH}" \
                -classpath "$(subst ${DAIKONDIR}/java:,${CLASSPATH_JARS}:${CLASSPATH_JARS2}:,${CLASSPATH})"
# javadoc 1.4 doesn't like being given package names; give all filenames explicitly instead
JDOC_FILES = $(shell find . -follow \( -name '*daikon-java*' -o -wholename ./daikon/util \) -prune -o \( -name '*.java' \) -print | grep -v '/\.\#' | grep -v typequals | ${SORT_DIRECTORY_ORDER})

# Daikon packages except for util (which is just a copy of plume)
DAIKON_PACKAGES := $(shell ls -F daikon | grep / | egrep -v 'util' \
				   | sed s./.. | sed s/^/daikon./ | tr '\n' ':')

javadoc: api

# Build javadoc using the daikon package and all of subpackages of
# daikon (except daikon.util) and plume.  Unfortunately, there is
# no switch to javadoc that ignores a subpackage.  Don't include the
# typequals package -- it crashes javadoc.
.PHONY: api
api: ../utils/plume-lib ../utils/html-tools $(JAVA_FILES)
	mkdir -p $(JAVADOC_DEST)
	@rm -f java-files.txt
	find . -name "*.java" -print | grep -v typequals > java-files.txt
	${JAVADOC} -J-Xmx1000m -J-ea $(JAVADOC_FLAGS) \
		@java-files.txt
	@rm -f java-files.txt
	cd $(JAVADOC_DEST) && $(PLUMEBIN)/preplace '^ *&#64;cindex.*' ''
	cp -pf ../doc/daikon-favicon.png $(JAVADOC_DEST)
	cd $(JAVADOC_DEST) && $(HTMLTOOLS)/html-add-favicon . daikon-favicon.png

## Make Javadoc with private fields for people working on source code.
## It is generated in the standard location, directory api/
api-private: ../utils/plume-lib $(AUTO_GENERATED_FILES)
	mkdir -p api
	${JAVADOC} -J-Xmx1000m -J-ea $(JAVADOC_FLAGS) -private \
	  daikon -subpackages $(DAIKON_PACKAGES):plume
	cd api && ../../utils/plume-lib/bin/preplace '^ *&#64;cindex.*' ''

## This doesn't seem necessary any more
# javadoc-lookup: doc-javadoc-lookup
#
# ## Fix up generated .html files for use by the Emacs `javadoc-lookup' function.
# doc-javadoc-lookup: api
# 	find api -name '*.html' | xargs -n 100 ${PLUMEBIN}/jdk-add-munged-anchor-names
# 	echo "Run javadoc-index-to-alist > ~/.javadoc-index.el"

../doc/invariants-doc.texinfo: ../utils/plume-lib ${JDOC_FILES} daikon/config/InvariantDoclet.class daikon/config/HtmlToTexinfo.class
	${JAVADOC} -J-Xmx1000m -J-ea -docletpath "${CLASSPATH}" -classpath "${CLASSPATH}" -doclet daikon.config.InvariantDoclet \
		--texinfo $@ \
		$(JDOC_FILES)

# This fails if both . and pwd are on CLASSPATH:  classes are found twice
# resulting in a duplicate class warning.

../doc/config-options.texinfo: ../utils/plume-lib ${JDOC_FILES} daikon/config/ParameterDoclet.class daikon/config/HtmlToTexinfo.class
	${JAVADOC} -J-Xmx1000m -J-ea -docletpath "${CLASSPATH}" -classpath "${CLASSPATH}" -doclet daikon.config.ParameterDoclet \
		--texinfo $@ \
		$(JDOC_FILES)
	perl -pi -e 's/&#64;cindex/\@cindex/g' $@

daikon/config/InvariantDoclet.class: daikon/config/InvariantDoclet.java
	${MAKE} compile

daikon/config/ParameterDoclet.class: daikon/config/ParameterDoclet.java
	${MAKE} compile

daikon/config/HtmlToTexinfo.class: daikon/config/HtmlToTexinfo.java
	${MAKE} compile


# This file is no longer used; there's no point in keeping it around.
# daikon/config/configurable.txt: $(AUTO_GENERATED_FILES)
# 	${JAVADOC} -J-ea -doclet daikon.config.ParameterDoclet \
# 		--list $@ \
# 		$(JDOC_FILES)


###########################################################################
### For debugging the Makefile.
###

showvars:
	which java
	which javac
	@echo PWD = $(shell pwd)
	@echo AUTO_GENERATED_FILES = ${AUTO_GENERATED_FILES}
	@echo CLASSPATH = "${CLASSPATH}"
	@echo CLASSPATH_JARS = $(CLASSPATH_JARS)
	@echo CLASSPATH_JARS2 = $(CLASSPATH_JARS2)
	@echo OSTYPE = $(OSTYPE)
	@echo PATH = $(PATH)
	@echo DAIKONDIR = "${DAIKONDIR}"
	@echo DAIKONBIN = ${DAIKONBIN}
	@echo PLUMEBIN = ${PLUMEBIN}
	@echo DAIKON_JAVA_FILES = $(DAIKON_JAVA_FILES)
	@echo JAVA = ${JAVA}
	@echo JAVAC = ${JAVAC}
	@echo JAVAC_ARGS = ${JAVAC_ARGS}
	@echo JAVAC_COMMAND = ${JAVAC_COMMAND}
	@echo JAVA_FILES = $(JAVA_FILES)
	@echo JAVA_FILES_FOR_STYLE = $(JAVA_FILES_FOR_STYLE)
	@echo JAVA_FILES_FOR_FORMAT = $(JAVA_FILES_FOR_FORMAT)
	@echo TAG_FILES = $(TAG_FILES)
	@echo INTERNING_JAVA_FILES = $(INTERNING_JAVA_FILES)
	@echo INTERNING_JAVA_FILES_COUNTS = $(INTERNING_JAVA_FILES_COUNTS)
	@echo JAVA_HOME = $(JAVA_HOME)

dump: showvars

showvar_JAVA_FILES:
	@echo ${JAVA_FILES}

showvar_CLASS_FILES:
	@echo $(subst .java,.class,${JAVA_FILES})

showvar_JAVA_FILES_FOR_FORMAT:
	@echo ${JAVA_FILES_FOR_FORMAT}

list_java_files: showvar_JAVA_FILES


###########################################################################
### Automatically generated files
###

## First, a file not generated via java-cpp.
daikon/test/split/SplitterFactoryTest.java: daikon/test/split/SplitterFactoryTestUpdater.java
	${JAVAC_COMMAND} $<
	rm -f $@
# Has expected errors while compiling splitter files
	${JAVA} daikon.test.split.SplitterFactoryTestUpdater > /dev/null 2>&1
	chmod a-w $@

${AUTO_GENERATED_CLASSES_2}: ${AUTO_GENERATED_FILES_2}
	${JAVAC_COMMAND} ${AUTO_GENERATED_FILES_2}

## This would be an alternative to the above rule for ${AUTO_GENERATED_CLASSES_2}.
# daikon/test/split/SplitterFactoryTest.class: daikon/test/split/SplitterFactoryTest.java
# 	${JAVAC_COMMAND} $<

# Create the auto-generated files with jpp
.PHONY: jpp
jpp: ../utils/plume-lib ${AUTO_GENERATED_FILES}

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
JAVA_CPP ?= (rm -f $@; ${PLUMEBIN}/java-cpp -DDEFINEDVAR $< 2>&1 > $@ && chmod -w $@)
# Like the above, but also makes a backup file
# JAVA_CPP = (mv -f $@ $@-bak; set -e; ${PLUMEBIN}/java-cpp -DDEFINEDVAR $< > $@; chmod -w $@)

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


###########################################################################
### Delete-on-error
###

# Existence of this rule means that a target is deleted if has just changed
# and its rule commands exit with nonzero status.  It's needed for the
# java-cpp rules above.
# (Actually, it isn't anymore, for I arrange not to overwrite the file
# unless success.  But leave it in for now anyway.)
.DELETE_ON_ERROR:
