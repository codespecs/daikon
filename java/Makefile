### User customizations

# Put user-specific changes (such as changes to the JAVAC variable:
#   JAVAC ?= jikes -g +E +F
# ) in your own Makefile.user file in this directory.
# Make will silently continue if Makefile.user does not exist.
-include Makefile.user

JAVAC ?= javac -g

### end of user customizations


###########################################################################
### Variables
###

# This command should run after the AUTO_GENERATED_FILES are generated, or
# else AUTO_GENERATED_FILES should be appended to it (without duplication).
# In practice, this is good enough, however.
# Creates a symlink to ajax if it does not exist, so the ajax .java
# files get compiled as well.
# "-o -path './jakarta-oro*/src/java/examples'" does not work for non-GNU find.
JAVA_FILES := $(shell test -d ajax-ship/ajax && test ! -L ajax && ln -s ajax-ship/ajax .; find . -follow \( -name '*daikon-java*' -o -name '*daikon-instrumented*' -o -name CVS -o -name 'ReturnBytecodes.java' -o -name 'AjaxDecls.java' -o -name 'ajax-ship' -o -name '\.\#*' \) -prune -o -name '*.java' -print | grep -v '/jakarta-oro.*/src/java/examples/')
TAG_FILES := $(shell find . -follow \( -name '*daikon-java*' -o -name CVS -o -name ajax \) -prune -o \( -name '*.java' -o -name '*.java.jpp' \) -print)

AUTO_GENERATED_FILES = \
	utilMDE/MathMDE.java \
	daikon/PptSlice1.java \
	daikon/PptSlice2.java \
	daikon/PptSlice3.java \
	daikon/inv/ternary/threeScalar/FunctionBinary.java \
	daikon/inv/ternary/threeScalar/FunctionBinaryFloat.java \
	daikon/inv/ternary/threeScalar/FunctionBinaryCore.java \
	daikon/inv/ternary/threeScalar/FunctionBinaryCoreFloat.java \
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
	daikon/inv/Functions.java \
	daikon/inv/FunctionsFloat.java \
	daikon/inv/unary/scalar/LowerBound.java \
	daikon/inv/unary/scalar/UpperBound.java \
	daikon/inv/unary/scalar/LowerBoundFloat.java \
	daikon/inv/unary/scalar/UpperBoundFloat.java \
	daikon/inv/unary/scalar/OneOfScalar.java \
	daikon/inv/unary/scalar/OneOfFloat.java \
	daikon/inv/unary/string/OneOfString.java \
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
	daikon/inv/unary/sequence/SeqIndexComparison.java \
	daikon/inv/unary/sequence/SeqIndexComparisonFloat.java \
	daikon/inv/unary/sequence/SeqIndexNonEqual.java \
	daikon/inv/unary/sequence/SeqIndexNonEqualFloat.java \
	daikon/inv/unary/sequence/SingleSequenceFactory.java \
	daikon/inv/unary/sequence/SingleFloatSequenceFactory.java \
	daikon/inv/unary/sequence/CommonSequence.java \
	daikon/inv/unary/sequence/CommonFloatSequence.java \
	daikon/inv/unary/sequence/EltNonZero.java \
	daikon/inv/unary/sequence/EltNonZeroFloat.java \
	daikon/inv/unary/sequence/EltwiseIntComparison.java \
	daikon/inv/unary/sequence/EltwiseFloatComparison.java \
	daikon/inv/unary/stringsequence/EltOneOfString.java \
	daikon/inv/unary/stringsequence/OneOfStringSequence.java \
	daikon/inv/binary/sequenceScalar/Member.java \
	daikon/inv/binary/sequenceString/Member.java \
	daikon/inv/binary/sequenceScalar/MemberFloat.java \
	daikon/inv/binary/sequenceScalar/SeqIntComparison.java \
	daikon/inv/binary/sequenceScalar/SeqFloatComparison.java \
	daikon/inv/binary/sequenceScalar/SequenceScalar.java \
	daikon/inv/binary/sequenceScalar/SequenceFloat.java \
	daikon/inv/binary/sequenceScalar/SequenceScalarFactory.java \
	daikon/inv/binary/sequenceScalar/SequenceFloatFactory.java \
	daikon/inv/binary/twoScalar/IntComparisonCore.java \
	daikon/inv/binary/twoScalar/FloatComparisonCore.java \
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
	daikon/inv/binary/twoScalar/FunctionUnaryCore.java \
	daikon/inv/binary/twoScalar/FunctionUnaryCoreFloat.java \
	daikon/inv/binary/twoScalar/FunctionUnary.java \
	daikon/inv/binary/twoScalar/FunctionUnaryFloat.java \
	daikon/inv/binary/twoScalar/LinearBinaryCore.java \
	daikon/inv/binary/twoScalar/LinearBinaryCoreFloat.java \
	daikon/inv/binary/twoScalar/LinearBinary.java \
	daikon/inv/binary/twoScalar/LinearBinaryFloat.java \
	daikon/inv/binary/twoScalar/NonEqualCore.java \
	daikon/inv/binary/twoScalar/NonEqualCoreFloat.java \
	daikon/inv/binary/twoScalar/TwoScalarFactory.java \
	daikon/inv/binary/twoScalar/TwoFloatFactory.java \
	daikon/inv/binary/twoScalar/TwoScalar.java \
	daikon/inv/binary/twoScalar/TwoFloat.java \
	daikon/inv/binary/twoSequence/SeqComparison.java \
	daikon/inv/binary/twoSequence/SeqComparisonFloat.java \
	daikon/inv/binary/twoSequence/Reverse.java \
	daikon/inv/binary/twoSequence/ReverseFloat.java \
	daikon/inv/binary/twoSequence/SubSequence.java \
	daikon/inv/binary/twoSequence/SubSequenceFloat.java \
	daikon/inv/binary/twoSequence/PairwiseIntComparison.java \
	daikon/inv/binary/twoSequence/PairwiseFloatComparison.java \
	daikon/inv/binary/twoSequence/PairwiseLinearBinary.java \
	daikon/inv/binary/twoSequence/PairwiseLinearBinaryFloat.java \
	daikon/inv/binary/twoSequence/PairwiseFunctionUnary.java \
	daikon/inv/binary/twoSequence/PairwiseFunctionUnaryFloat.java \
	daikon/inv/binary/twoSequence/TwoSequence.java \
	daikon/inv/binary/twoSequence/TwoSequenceFloat.java \
	daikon/inv/binary/twoSequence/TwoSequenceFactory.java \
	daikon/inv/binary/twoSequence/TwoSequenceFactoryFloat.java \
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

###########################################################################
### Compilation targets
###

default: all

# "-S" means do not continue after errors.
all:
	${MAKE} -S all_directly junit

## In general, don't use this; it misses some files
all_via_daikon: daikon/Daikon.class daikon/Runtime.class daikon/Diff.class

## ${JAVA_FILES} is so long I can't see the result on the same screen as the command.
# all_directly: ${AUTO_GENERATED_FILES} ajax
all_directly: ${AUTO_GENERATED_FILES}
	@echo ${JAVAC} '*.java ...'
	@${JAVAC} ${JAVA_FILES}
	@chmod -w ${AUTO_GENERATED_FILES}

## Use this when you don't want $inv/tests makefiles to restart
JAVA_FILES_EXCEPT_DAIKON := $(subst ./daikon/Daikon.java,,${JAVA_FILES})
all_except_daikon: ${JAVA_FILES_EXCEPT_DAIKON}
	@echo ${JAVAC} '*.java ... (except Daikon.java)'
	@${JAVAC} ${JAVA_FILES_EXCEPT_DAIKON}
	@chmod -w ${AUTO_GENERATED_FILES}

all_except: all_except_daikon

all_force:	all_directly

all_via_javac:
	$(MAKE) JAVAC='javac -g' all

all_javac:	all_via_javac

javac:	all_via_javac

all_via_jikes:
	$(MAKE) JAVAC='jikes -g +E +F' all

all_jikes:	all_via_jikes

jikes:	all_via_jikes

# These rules are undesirable, because Daikon.class (or Diff.class) might
# be up to date even if other files aren't.
daikon/Daikon.class: daikon/Daikon.java	${AUTO_GENERATED_FILES}
	${JAVAC} daikon/Daikon.java

daikon/Diff.class: Diff.java ${AUTO_GENERATED_FILES}
	${JAVAC} daikon/Diff.java

daikon/Runtime.class: daikon/Runtime.java
	${JAVAC} daikon/Runtime.java

# No longer needed because symlink is created when setting the
# JAVA_FILES variable
#ajax:
#	ln -s ajax-ship/ajax .


###########################################################################
### Non-compilation targets
###

# It's good to run "make clean" occasionally, because it costs little to
# regenerate .class files and it's bad to continue to use an orphaned class
# file (whose source file was renamed).  Or, run "orphaned-class-files".
clean:
	-rm -f `find . -name "*.class" -print`

dist_clean: clean
	-rm -f TAGS

very_clean: dist_clean
	-rm -f ${AUTO_GENERATED_FILES}

run:	all
	java daikon.Daikon

run_gui:	all
	java daikon.gui.InvariantsGUI /g1/users/mhao/daikon/inv_files/dsaa.inv 2>&1 | grep -v Font

TAGS:	tags

# Used to include "*.py", but no such in daikon/ directory.
# I could make this ../*.py, but probably don't want to include that old code.
tags:
	@echo etags '*.java ...'
	@etags -l java ${TAG_FILES}

## Static checks of the code

lint:	jlint

jlint:
	jlint . | grep -v "Value of referenced variable '.*' may be NULL" | grep -v "Compare strings as object references" | perl -p -e 's:^daikon/::'

# "(cd $inv; maudit daikon)" does not work; I don't understand why.
# Unfortunately, this is way too much output; I need to filter it.
# Maybe write my own script to do that...
audit:
	cd $(HOME)/java && maudit daikon | perl -p -e 's/^daikon\///'

## Testing

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
# Include CLASSPATH on command line for Cygwin compatibility
# Include . in CLASSPATH because you can have two versions of Daikon,
# and you want to do junit tests on *this* version.
junit:
	@echo java daikon.test.MasterTester
	@java -server -classpath .:${CLASSPATH}:daikon/test/ daikon.test.MasterTester

## Documentation

.PHONY: doc

JAVADOC_FLAGS = -d doc \
                -linkoffline http://java.sun.com/j2se/1.3/docs/api/ http://java.sun.com/j2se/1.3/docs/api/
# Note that this excludes Ajax
JAVADOC_PACKAGES = `find -follow -name '*.java' -exec dirname {} \; | grep -v CVS | grep -v ajax | sort | uniq | perl -p -e 's:^./::g' | perl -p -e 's:/:.:g'`

doc: $(JAVA_FILES) doc-dkconfig
	mkdir -p doc
	# This requires that jdk/lib/tools.jar be on your classpath
	javadoc $(JAVADOC_FLAGS) $(JAVADOC_PACKAGES)

## Make doc with private fields for people working on source code
doc-private: $(JAVA_FILES)
	mkdir -p doc
	javadoc $(JAVADOC_FLAGS) -private $(JAVADOC_PACKAGES)

## Fix up generated .html files for use by the Emacs `jdk-lookup' function.
doc-jdk-lookup: doc
	jdk-add-munged-anchor-names `find doc -name '*.html'`
	echo "Run jdk-index-to-alist > ~/emacs/jdk-index.el"

../doc/config-options.texinfo: $(JAVA_FILES)
	javadoc -doclet daikon.config.ParameterDoclet \
		--texinfo $@ \
		$(JAVADOC_PACKAGES)

doc-dkconfig: ../doc/config-options.texinfo

daikon/config/configurable.txt: $(JAVA_FILES)
	javadoc -doclet daikon.config.ParameterDoclet \
		--list $@ \
		$(JAVADOC_PACKAGES)

../doc/category-options.texinfo: $(JAVA_FILES)
	javadoc -doclet daikon.config.ParameterDocletCategory \
		--texinfo $@ \
		$(JAVADOC_PACKAGES)

doc-category: ../doc/category-options.texinfo

## Switching between version 2 and version 3 (temporary)

# Recurse to the directory above this since regression tests need to be
# switched too
use-%:
	cd .. && $(MAKE) $@

## For debugging the Makefile.
showvars:
	@echo JAVA_FILES = $(JAVA_FILES)
	@echo TAG_FILES = $(TAG_FILES)

list_java_files:
	@echo $(JAVA_FILES)


###########################################################################
### Automatically generated files
###

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
JAVA_CPP ?= (rm -f $@; java-cpp.pl -DDEFINEDVAR $< 2>&1 > $@ && chmod -w $@)
# Like the above, but also makes a backup file
# JAVA_CPP = (mv -f $@ $@-bak; set -e; java-cpp.pl -DDEFINEDVAR $< > $@; chmod -w $@)

# JAVA_CPP = (cp -pf $@ $@-bak; set -e; java-cpp $< -DDEFINEDVAR > $@-postproc; mv -f $@-postproc $@; chmod -w $@)
## Here are alternate ways of writing the same command:
## I could use "define", but that's specific to GNU Make
## I can't do this because the CPP defined variable changes.
# JAVA_CPP = (mv -f $@ $@-bak; set -e; java-cpp $< -DDEFINEDVAR > $@; chmod -w $@)
## These are used like:	$(JAVA_CPP_START) -DLOWER $(JAVA_CPP_END)
# JAVA_CPP_START = (cp -pf $@ $@-bak; set -e; java-cpp $<
# JAVA_CPP_END = > $@-postproc; mv -f $@-postproc $@; chmod -w $@)

utilMDE/MathMDE.java: utilMDE/MathMDE.java.jpp utilMDE/MathMDE-gcd.java.jpp utilMDE/MathMDE-modulus.java.jpp utilMDE/MathMDE-nonmodulus.java.jpp utilMDE/MathMDE-help.java.jpp utilMDE/MathMDE-helpend.java.jpp
	cd utilMDE && $(MAKE) MathMDE.java

daikon/PptSlice1.java: daikon/PptSlice.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ARITY1)

daikon/PptSlice2.java: daikon/PptSlice.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ARITY2)

daikon/PptSlice3.java: daikon/PptSlice.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ARITY3)

daikon/inv/ternary/threeScalar/FunctionBinary.java: daikon/inv/ternary/threeScalar/FunctionBinary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/ternary/threeScalar/FunctionBinaryFloat.java: daikon/inv/ternary/threeScalar/FunctionBinary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/ternary/threeScalar/FunctionBinaryCore.java: daikon/inv/ternary/threeScalar/FunctionBinaryCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/ternary/threeScalar/FunctionBinaryCoreFloat.java: daikon/inv/ternary/threeScalar/FunctionBinaryCore.java.jpp
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

daikon/inv/Functions.java: daikon/inv/Functions.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LONG)

daikon/inv/FunctionsFloat.java: daikon/inv/Functions.java.jpp
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

daikon/inv/unary/sequence/NoDuplicates.java: daikon/inv/unary/sequence/NoDuplicates.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/NoDuplicatesFloat.java: daikon/inv/unary/sequence/NoDuplicates.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/sequence/SeqIndexComparison.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/SeqIndexComparisonFloat.java: daikon/inv/unary/sequence/SeqIndexComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/sequence/SeqIndexNonEqual.java: daikon/inv/unary/sequence/SeqIndexNonEqual.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/SeqIndexNonEqualFloat.java: daikon/inv/unary/sequence/SeqIndexNonEqual.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/sequence/SingleSequenceFactory.java: daikon/inv/unary/sequence/SingleSequenceFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/SingleFloatSequenceFactory.java: daikon/inv/unary/sequence/SingleSequenceFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/sequence/CommonSequence.java: daikon/inv/unary/sequence/CommonSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/CommonFloatSequence.java: daikon/inv/unary/sequence/CommonSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/sequence/EltNonZero.java: daikon/inv/unary/sequence/EltNonZero.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/EltNonZeroFloat.java: daikon/inv/unary/sequence/EltNonZero.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/sequence/EltwiseIntComparison.java: daikon/inv/unary/sequence/EltwiseIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/unary/sequence/EltwiseFloatComparison.java: daikon/inv/unary/sequence/EltwiseIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/unary/stringsequence/EltOneOfString.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELTSTRING)

daikon/inv/unary/stringsequence/OneOfStringSequence.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRINGSEQUENCE)

daikon/inv/binary/sequenceScalar/Member.java: daikon/inv/binary/Member.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/inv/binary/sequenceString/Member.java: daikon/inv/binary/Member.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/inv/binary/sequenceScalar/MemberFloat.java: daikon/inv/binary/Member.java.jpp
	$(JAVA_CPP:DEFINEDVAR=FLOAT)

daikon/inv/binary/sequenceScalar/SeqIntComparison.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/sequenceScalar/SeqFloatComparison.java: daikon/inv/binary/sequenceScalar/SeqIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/sequenceScalar/SequenceScalar.java: daikon/inv/binary/sequenceScalar/SequenceScalar.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/sequenceScalar/SequenceFloat.java: daikon/inv/binary/sequenceScalar/SequenceScalar.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/sequenceScalar/SequenceScalarFactory.java: daikon/inv/binary/sequenceScalar/SequenceScalarFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/sequenceScalar/SequenceFloatFactory.java: daikon/inv/binary/sequenceScalar/SequenceScalarFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/IntComparisonCore.java:  daikon/inv/binary/twoScalar/IntComparisonCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/FloatComparisonCore.java:  daikon/inv/binary/twoScalar/IntComparisonCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/IntEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUAL)

daikon/inv/binary/twoScalar/FloatEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUALFLOAT)

daikon/inv/binary/twoScalar/IntNonEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=NONEQUAL)

daikon/inv/binary/twoScalar/FloatNonEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=NONEQUALFLOAT)

daikon/inv/binary/twoScalar/IntLessThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHAN)

daikon/inv/binary/twoScalar/FloatLessThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHANFLOAT)

daikon/inv/binary/twoScalar/IntLessEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUAL)

daikon/inv/binary/twoScalar/FloatLessEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUALFLOAT)

daikon/inv/binary/twoScalar/IntGreaterThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHAN)

daikon/inv/binary/twoScalar/FloatGreaterThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHANFLOAT)

daikon/inv/binary/twoScalar/IntGreaterEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUAL)

daikon/inv/binary/twoScalar/FloatGreaterEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUALFLOAT)

daikon/inv/binary/twoScalar/FunctionUnaryCore.java: daikon/inv/binary/twoScalar/FunctionUnaryCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/FunctionUnaryCoreFloat.java: daikon/inv/binary/twoScalar/FunctionUnaryCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/FunctionUnary.java: daikon/inv/binary/twoScalar/FunctionUnary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/FunctionUnaryFloat.java: daikon/inv/binary/twoScalar/FunctionUnary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/LinearBinaryCore.java: daikon/inv/binary/twoScalar/LinearBinaryCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/LinearBinaryCoreFloat.java: daikon/inv/binary/twoScalar/LinearBinaryCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/LinearBinary.java: daikon/inv/binary/twoScalar/LinearBinary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/LinearBinaryFloat.java: daikon/inv/binary/twoScalar/LinearBinary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/NonEqualCore.java: daikon/inv/binary/twoScalar/NonEqualCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/NonEqualCoreFloat.java: daikon/inv/binary/twoScalar/NonEqualCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/TwoScalarFactory.java: daikon/inv/binary/twoScalar/TwoScalarFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/TwoFloatFactory.java: daikon/inv/binary/twoScalar/TwoScalarFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoScalar/TwoScalar.java: daikon/inv/binary/twoScalar/TwoScalar.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoScalar/TwoFloat.java: daikon/inv/binary/twoScalar/TwoScalar.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/SeqComparison.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoSequence/SeqComparisonFloat.java: daikon/inv/binary/twoSequence/SeqComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/Reverse.java: daikon/inv/binary/twoSequence/Reverse.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoSequence/ReverseFloat.java: daikon/inv/binary/twoSequence/Reverse.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/SubSequence.java: daikon/inv/binary/twoSequence/SubSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoSequence/SubSequenceFloat.java: daikon/inv/binary/twoSequence/SubSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/PairwiseIntComparison.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoSequence/PairwiseFloatComparison.java: daikon/inv/binary/twoSequence/PairwiseIntComparison.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/PairwiseLinearBinary.java: daikon/inv/binary/twoSequence/PairwiseLinearBinary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoSequence/PairwiseLinearBinaryFloat.java: daikon/inv/binary/twoSequence/PairwiseLinearBinary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/PairwiseFunctionUnary.java: daikon/inv/binary/twoSequence/PairwiseFunctionUnary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoSequence/PairwiseFunctionUnaryFloat.java: daikon/inv/binary/twoSequence/PairwiseFunctionUnary.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/TwoSequence.java: daikon/inv/binary/twoSequence/TwoSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoSequence/TwoSequenceFloat.java: daikon/inv/binary/twoSequence/TwoSequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

daikon/inv/binary/twoSequence/TwoSequenceFactory.java: daikon/inv/binary/twoSequence/TwoSequenceFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPELONG)

daikon/inv/binary/twoSequence/TwoSequenceFactoryFloat.java: daikon/inv/binary/twoSequence/TwoSequenceFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=TYPEDOUBLE)

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


## I decided against this.  It gave little enough help, and it cluttered
## the directories enough (and could be slightly confusing about what those
## new types really do), that I will hold off on this for now.  Perhaps it
## would be more reasonable if all of the specialized files were in one
## Vector subdirectory (that was ignored by tags, etc.).

# This one is just for testing
IntegerVector.java: VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon -DELTVECTOR=IntegerVector -DELTTYPE=Integer")

daikon/PptVector.java: daikon/VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon -DELTVECTOR=PptVector -DELTTYPE=Ppt")

daikon/PptTopLevelVector.java: daikon/VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon -DELTVECTOR=PptTopLevelVector -DELTTYPE=PptTopLevel")

daikon/PptConditionalVector.java: daikon/VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon -DELTVECTOR=PptConditionalVector -DELTTYPE=PptConditional")

daikon/VarInfoVector.java: daikon/VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon -DELTVECTOR=VarInfoVector -DELTTYPE=VarInfo")

daikon/inv/InvariantVector.java: daikon/VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon.inv -DELTVECTOR=InvariantVector -DELTTYPE=Invariant")

daikon/derive/DerivationVector.java: daikon/ VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon.derive -DELTVECTOR=DerivationVector -DELTTYPE=Derivation")


###########################################################################
### Delete-on-error
###

# Existence of this rule means that a target is deleted if has just changed
# and its rule commands exit with nonzero status.  It's needed for the
# java-cpp rules above.
# (Actually, it isn't anymore, for I arrange not to overwrite the file
# unless success.  But leave it in for now anyway.)
.DELETE_ON_ERROR:
