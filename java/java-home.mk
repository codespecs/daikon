# Makefile fragment that sets JAVA_HOME, if it is not already set to a non-empty
# value, and checks that it is a JDK.  It also sets JAVA_VERSION_STRING_WITH_EA
# (e.g., "27-ea"), JAVA_VERSION_STRING (e.g., "27" or "1.8.0_402"), and
# JAVA_RELEASE_NUMBER (e.g., "27" or "8"), all for the JDK at JAVA_HOME.
# It is included by java/Makefile and tests/Makefile.common.
# scripts/daikon.bashrc finds JAVA_HOME in nearly the same way.

# Ask the javac on the PATH which JDK it is in.  This works even if javac is a
# wrapper script (as used by jenv, asdf, or mise) or the macOS /usr/bin/javac stub.
ifeq ($(strip ${JAVA_HOME}),)
  JAVA_HOME := $(shell javac -J-XshowSettings:properties -version 2>&1 | sed -n -e 's/^ *java\.home = //p')
endif
# For JDK 8, java.home is the jre/ subdirectory of the JDK, and a user's
# JAVA_HOME may be too.  "override" applies this to "make JAVA_HOME=...".
override JAVA_HOME := $(patsubst %/jre,%,$(patsubst %/,%,${JAVA_HOME}))
ifneq ($(shell [ -n '${JAVA_HOME}' ] && [ -x '${JAVA_HOME}/bin/javac' ] && echo yes),yes)
  $(error No JDK found at JAVA_HOME="${JAVA_HOME}"; set JAVA_HOME or put javac on the PATH)
endif
# Commands run by make, including recursive invocations of make, use this value.
export JAVA_HOME

# The output of `javac -version` is like "javac 27-ea", "javac 25.0.1", or
# "javac 1.8.0_402".  It may be preceded by lines such as
# "Picked up JAVA_TOOL_OPTIONS: ...".
# Running javac is slow, so recursive invocations of make reuse the version
# string if JAVA_HOME is unchanged.
ifneq (${DAIKON_JAVA_VERSION_HOME},${JAVA_HOME})
  JAVA_VERSION_STRING_WITH_EA :=
endif
ifeq (${JAVA_VERSION_STRING_WITH_EA},)
  JAVA_VERSION_STRING_WITH_EA := $(shell '${JAVA_HOME}/bin/javac' -version 2>&1 | sed -n -e 's/^javac //p')
endif
export DAIKON_JAVA_VERSION_HOME := ${JAVA_HOME}
export JAVA_VERSION_STRING_WITH_EA
JAVA_VERSION_STRING := $(subst -ea,,${JAVA_VERSION_STRING_WITH_EA})
# The release number is the first component, or the second if the first is "1".
JAVA_VERSION_COMPONENTS := $(subst ., ,$(subst -, ,${JAVA_VERSION_STRING}))
ifeq ($(firstword ${JAVA_VERSION_COMPONENTS}),1)
  JAVA_RELEASE_NUMBER := $(word 2,${JAVA_VERSION_COMPONENTS})
else
  JAVA_RELEASE_NUMBER := $(firstword ${JAVA_VERSION_COMPONENTS})
endif
