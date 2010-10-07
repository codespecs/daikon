###########################################################################

# Read Makefile.common for what all of this means

SOURCES:=/g4/projects/invariants/tests/antlr/antlr-current
MAIN_CLASS:=antlr.Tool

ESC_ON:=1

INSTRUMENT:=infer
# INSTRUMENT:=explicit
# INSTR_FILES:=antlr/collections/impl/BitSet.java antlr/collections/impl/ASTEnumerator.java antlr/collections/impl/IndexedVector.java antlr/collections/impl/VectorEnumerator.java antlr/collections/impl/Vector.java antlr/collections/impl/LLEnumeration.java antlr/collections/impl/LList.java antlr/collections/impl/LLCell.java antlr/collections/impl/ASTArray.java antlr/collections/impl/IntRange.java antlr/collections/Enumerator.java antlr/collections/Stack.java antlr/collections/List.java antlr/collections/ASTEnumeration.java antlr/collections/AST.java


DAIKON_OPTIONS:=--var_omit_pattern="buf(fer)?\[\]"

include ../../Makefile.common

EXTRA_CLASSPATH_BEGINNING:=${COMMON_HERE}/daikon-tests/Antlr/source:

# Reflection
instrument-files-revise:
	perl -ni.bak -e "print unless m|^antlr/Tool.java|;" ${ALL_LIST_FILE}
	echo "antlr/collections/impl/BitSet.java" >| ${INST_LIST_FILE}

#GFILE:=${SOURCES}/examples/java/java/java.g
GFILE:=${SOURCES}/examples/cpp/IDL/idl.g

MAIN_ARGS:=-o ${SCRATCH_FOR_DAIKON} ${GFILE}
