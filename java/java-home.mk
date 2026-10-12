# Makefile fragment that sets JAVA_HOME to a JDK, version 8 or later.  It uses
# the existing value of JAVA_HOME if that is a JDK, and otherwise the JDK that
# contains the javac on the PATH.  It also sets JAVA_VERSION_STRING_WITH_EA
# (e.g., "27-ea"), JAVA_VERSION_STRING (e.g., "27" or "1.8.0_402"), and
# JAVA_RELEASE_NUMBER (e.g., "27" or "8"), all for the JDK at JAVA_HOME.
# It is included by java/Makefile, tests/Makefile.common, and ../Makefile.

# For JDK 8, java.home is the jre/ subdirectory of the JDK, and a user's
# JAVA_HOME may be too.  "override" applies these assignments to "make JAVA_HOME=...".
JAVA_HOME_ORIGIN := $(origin JAVA_HOME)
JAVA_HOME_FROM_PATH :=
override JAVA_HOME := $(patsubst %/jre,%,$(patsubst %/,%,$(strip ${JAVA_HOME})))
# If JAVA_HOME is unset, or is a JRE that came from the environment, use the JDK
# that contains the javac on the PATH.  Any other JAVA_HOME that is not a JDK is
# probably a mistake, so it is reported below.
ifeq (${JAVA_HOME},)
  JAVA_HOME_FROM_PATH := 1
else ifeq ($(wildcard ${JAVA_HOME}/bin/javac),)
  ifneq ($(wildcard ${JAVA_HOME}/bin/java),)
    ifneq (${JAVA_HOME_ORIGIN},command line)
      $(warning JAVA_HOME="${JAVA_HOME}" is a JRE, not a JDK; using the javac on the PATH instead)
      JAVA_HOME_FROM_PATH := 1
    endif
  endif
endif
# Ask the javac on the PATH which JDK it is in.  This works even if javac is a
# wrapper script (as used by jenv, asdf, or mise) or the macOS /usr/bin/javac stub.
ifdef JAVA_HOME_FROM_PATH
  override JAVA_HOME := $(patsubst %/jre,%,$(shell javac -J-XshowSettings:properties -version 2>&1 | sed -n -e 's/^ *java\.home = //p'))
endif

# If JAVA_HOME_DEFER_ERROR is set, a problem is recorded in JAVA_HOME_ERROR
# rather than reported immediately.
JAVA_HOME_ERROR :=
# Make splits values on spaces, and many recipes use ${JAVA_HOME} unquoted.
ifneq ($(word 2,${JAVA_HOME}),)
  JAVA_HOME_ERROR := JAVA_HOME="${JAVA_HOME}" contains a space; use a path without spaces, such as a symbolic link to the JDK
else ifeq ($(if ${JAVA_HOME},$(wildcard ${JAVA_HOME}/bin/javac)),)
  ifdef JAVA_HOME_FROM_PATH
    JAVA_HOME_ERROR := No JDK found; set JAVA_HOME to a JDK or put javac on the PATH
  else
    JAVA_HOME_ERROR := JAVA_HOME="${JAVA_HOME}" is not a JDK (it has no bin/javac)
  endif
else
  # The output of `javac -version` is like "javac 27-ea", "javac 25.0.1", or
  # "javac 1.8.0_402".  It may be preceded by lines such as
  # "Picked up JAVA_TOOL_OPTIONS: ...".
  JAVA_VERSION_STRING_WITH_EA := $(shell '${JAVA_HOME}/bin/javac' -version 2>&1 | sed -n -e 's/^javac //p')
  JAVA_VERSION_STRING := $(subst -ea,,${JAVA_VERSION_STRING_WITH_EA})
  # The release number is the first component, or the second if the first is "1".
  JAVA_VERSION_COMPONENTS := $(subst ., ,$(subst -, ,${JAVA_VERSION_STRING}))
  ifeq ($(firstword ${JAVA_VERSION_COMPONENTS}),1)
    JAVA_RELEASE_NUMBER := $(word 2,${JAVA_VERSION_COMPONENTS})
  else
    JAVA_RELEASE_NUMBER := $(firstword ${JAVA_VERSION_COMPONENTS})
  endif
  ifneq ($(shell [ '${JAVA_RELEASE_NUMBER}' -ge 8 ] 2>/dev/null && echo yes),yes)
    JAVA_HOME_ERROR := Java 8 or later is required; "${JAVA_HOME}/bin/javac -version" yields "${JAVA_VERSION_STRING_WITH_EA}"
  endif
endif
ifneq (${JAVA_HOME_ERROR},)
  ifndef JAVA_HOME_DEFER_ERROR
    $(error ${JAVA_HOME_ERROR})
  endif
endif
# Commands run by make, including recursive invocations of make, use this value.
export JAVA_HOME
