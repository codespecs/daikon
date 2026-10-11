# Makefile fragment that sets JAVA_HOME, if it is not already set to a non-empty
# value, and defines the function is_jdk.
# It is included by java/Makefile and tests/Makefile.common.
# scripts/daikon.bashrc finds JAVA_HOME in the same way.

# $(call is_jdk,DIR) is non-empty if DIR contains a JDK.
# JDK 9 and later contain lib/jrt-fs.jar; JDK 8 contains jre/lib/rt.jar.
is_jdk = $(and $(strip $(1)),$(wildcard $(1)/bin/javac),$(wildcard $(1)/lib/jrt-fs.jar $(1)/jre/lib/rt.jar))

# If the javac on the PATH is (a symbolic link to) a file in a JDK, use that
# JDK.  If javac is a wrapper script (as used by jenv, asdf, or mise), set
# JAVA_HOME instead.
ifeq ($(strip ${JAVA_HOME}),)
  JAVAC_ON_PATH_JDK := $(patsubst %/bin/javac,%,$(filter %/bin/javac,$(realpath $(shell command -v javac 2> /dev/null))))
  JAVA_HOME := $(if $(call is_jdk,${JAVAC_ON_PATH_JDK}),${JAVAC_ON_PATH_JDK})
endif
ifeq ($(strip ${JAVA_HOME}),)
  ifeq ($(shell uname), Darwin)
    JAVA_HOME := $(shell /usr/libexec/java_home 2> /dev/null)
  endif
endif
# Commands run by make, including recursive invocations of make, use this value.
export JAVA_HOME
