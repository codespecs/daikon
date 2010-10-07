###########################################################################

# Read Makefile.common for what all of this means

# The bin/jml script did these things to run JML:
#   JMLDIR="/g4/projects/invariants/tests/jml/JML"
#   JMLOUTDIR="${HOME}/tmp"
#   JMLSRCPATH="$JMLDIR/JDK:/usr/unsup/jdk1.2.2/src:/opt/java/src"
#   CLASSPATH=".:${JMLDIR}/antlr.jar:${JMLDIR}:${CLASSPATH}"
#   java -Xmx64M -Djml.src.path="$JMLSRCPATH" -Djml.out.dir="$JMLOUTDIR" edu.iastate.cs.jml.checker.JmlParserDriver "$@"

#INSTR_FILES:=edu/iastate/cs/jml/checker/util/ArrayEnumerator.java
#INSTR_FILES:=edu/iastate/cs/jml/checker/ASTTokenTypeConverter.java
INSTR_FILES:=edu/iastate/cs/jml/checker/Modifier.java edu/iastate/cs/jml/checker/ModifierSet.java
INSTRUMENT:=explicit

JMLDIR:=/g4/projects/invariants/tests/jml/JML

JAVA_ARGS:=-Djml.src.path="${JMLDIR}/JDK:/g2/j2sdk1.3.0/src" -Djml.out.dir="${HOME}/tmp"
MAIN_CLASS:=edu.iastate.cs.jml.checker.JmlParserDriver
MAIN_ARGS:=$(INV)/java/daikon/VarInfoName.java

CLASSPATH:=${JMLDIR}/antlr.jar:${JMLDIR}:${CLASSPATH}

include ../../Makefile.common


# Where to find the sources
SOURCES:=${JMLDIR}

# Omit Simplify for now
DAIKON_OPTIONS:=$(subst --suppress_redundant,,${DAIKON_OPTIONS})
