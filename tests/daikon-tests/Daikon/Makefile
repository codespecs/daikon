#########################################################################

# Read Makefile.common for what all of this means

mkfile_path := $(abspath $(lastword ${MAKEFILE_LIST}))
current_dir := $(notdir $(patsubst %/,%,$(dir ${mkfile_path})))

# Basics
ESC_ON:=1
INSTRUMENT:=infer
SOURCES:=${current_dir}../../../java

# Input for daikon under test
INPUTS_DECLS:=${PWD}/inputs/print_tokens2.decls
INPUTS_DTRACE1:=${PWD}/inputs/print_tokens2.1.dtrace.gz
INPUTS_DTRACE2:=${PWD}/inputs/print_tokens2.2.dtrace.gz
INPUTS_INVGZ_REL:=inputs/print_tokens2.1.inv.gz inputs/print_tokens2.2.inv.gz

# Test diff; or test Inference
ifeq (0,1)
MAIN_CLASS:=daikon.diff.Diff
MAIN_ARGS:=${INPUTS_INVGZ_REL}
else
MAIN_CLASS:=daikon.Daikon
MAIN_ARGS:=${INPUTS_DECLS} ${INPUTS_DTRACE1} -o ${PWD}/blah.inv.gz
endif

include ../../Makefile.common


#		| grep -v 'ProglangType' \
#		| grep -v 'FileIO' \
#		| grep -v 'daikon/inv' \
#		| grep -v 'daikon/derive' \


# Just don't want data
# ==========================================================================
# Configuration		: Singleton; too much data
# ProglangType		: Way to many static fields
# simplify		: Unintersting / off?
# split			: Uninteresting?
# plume			: Not daikon

# Instrumentation problems
# ==========================================================================
# VarInfoName		: Something unpleasant about its inner class QuantHelper.
# Invariant		: Hoisting static field anon class problem
# ValueAndModified	: Init'd static fields in dtrace but not decls

instrument-files-revise:
	cat ${ALL_LIST_FILE} \
		| grep -v 'VarInfoName' \
		| grep -v 'Invariant' \
		| grep -v 'Configuration' \
		| grep -v 'ProglangType' \
		| grep -v 'daikon/simplify' \
		| grep -v 'daikon/split' \
		| grep -v 'plume' \
		>| ${INST_LIST_FILE}

# Make .inv.gz files for diff under test

${SCRATCH_FOR_DAIKON}/.exists: ${INPUTS_INVGZ_REL}

inputs/%.inv.gz: inputs/%.dtrace.gz ${INPUTS_DECLS}
	@echo
	@echo "Creating inv file for daikon.Diff input [$@] ..."
	@echo
	java daikon.Daikon --no_text_output --no_show_progress --config_option daikon.Debug.show_stack_trace=true $^ -o $@
