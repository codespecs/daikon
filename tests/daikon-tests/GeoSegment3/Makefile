###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=junit.textui.TestRunner
MAIN_ARGS:=MapQuick3.SpecificationTests
MAIN2_CLASS:=MapQuick3.SpecificationTests

# Using this, the Daikon error of 3/9/2006 does not occur.
# instrument-files-revise:
# 	@echo "MapQuick3/GeoSegment.java MapQuick3/GeoFeature.java MapQuick3/Route.java" >| ${INST_LIST_FILE}


# This has no effect, because Makefile.common overrides it.  Use instrument-files-revise instead.
CHICORY_USER_FLAGS := '--ppt-select-pattern=^ps2.GeoSegment|^ps2.GeoFeature|^ps2.Route --ppt-omit-pattern=RouteFormatter --config_option daikon.PptRelation.enable_object_user=false'

ESC_ON:=1

include ../../Makefile.common
