# Makefile fragment that sets JAVA_HOME, if it is not already set to a non-empty
# value, and defines the function is_jdk.
# It is included by java/Makefile and tests/Makefile.common.
# scripts/daikon.bashrc finds JAVA_HOME in the same way.

# $(call is_jdk,DIR) is non-empty if DIR contains a JDK.
# JDK 9 and later contain lib/jrt-fs.jar; JDK 8 contains jre/lib/rt.jar.
is_jdk = $(and $(strip $(1)),$(wildcard $(1)/bin/javac),$(wildcard $(1)/lib/jrt-fs.jar $(1)/jre/lib/rt.jar))

# The JDK that contains the javac on the PATH, or empty if the javac on the PATH
# is not (a symbolic link to) a file in a JDK.  For example, it is empty if
# javac is a wrapper script (as used by jenv, asdf, or mise) or the macOS
# /usr/bin/javac stub.
JAVAC_ON_PATH_JDK := $(patsubst %/bin/javac,%,$(filter %/bin/javac,$(realpath $(shell command -v javac 2> /dev/null))))
JAVAC_ON_PATH_JDK := $(if $(call is_jdk,${JAVAC_ON_PATH_JDK}),${JAVAC_ON_PATH_JDK})

# If javac on the PATH is in a JDK, use that JDK.  If javac is a wrapper
# script, set JAVA_HOME instead.
ifeq ($(strip ${JAVA_HOME}),)
  JAVA_HOME := ${JAVAC_ON_PATH_JDK}
endif
ifeq ($(strip ${JAVA_HOME}),)
  ifeq ($(shell uname), Darwin)
    JAVA_HOME := $(shell /usr/libexec/java_home 2> /dev/null)
  endif
endif

# Warn if the javac on the PATH is in a different JDK than JAVA_HOME, because
# some commands use the javac on the PATH.  The check is skipped if JAVA_HOME or
# the javac on the PATH is not in a JDK.  It is done once for each combination
# of JAVA_HOME and PATH, not in every recursive invocation of make.
ifneq (${DAIKON_JAVA_HOME_CHECKED},${JAVA_HOME}:${PATH})
  export DAIKON_JAVA_HOME_CHECKED := ${JAVA_HOME}:${PATH}
  ifneq (,$(and $(call is_jdk,${JAVA_HOME}),${JAVAC_ON_PATH_JDK}))
    ifneq ($(realpath ${JAVA_HOME}),${JAVAC_ON_PATH_JDK})
      $(warning JAVA_HOME is ${JAVA_HOME}, but the javac on the PATH is in ${JAVAC_ON_PATH_JDK}; make them agree)
    endif
  endif
endif

# Commands run by make, including recursive invocations of make, use this value.
export JAVA_HOME
