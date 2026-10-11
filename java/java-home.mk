# Makefile fragment that sets JAVA_HOME, if it is not already set to a non-empty
# value, and defines variables and functions about the JDK there.
# It is included by the top-level Makefile, java/Makefile, and tests/Makefile.common.
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

# The version of the JDK at JAVA_HOME, such as "1.8.0_462", "25.0.1", or
# "27-ea".  It is empty if JAVA_HOME does not contain a JDK.
JAVA_VERSION_STRING :=
ifneq (,$(call is_jdk,${JAVA_HOME}))
  JAVA_VERSION_STRING := $(shell '${JAVA_HOME}/bin/java' -version 2>&1 | sed -n 's/^.* version "\([^"]*\)".*/\1/p' | head -1)
  ifeq (,${JAVA_VERSION_STRING})
    $(error Cannot determine the Java version of JAVA_HOME="${JAVA_HOME}")
  endif
endif
# The major version, such as "8" or "25".  It is empty if JAVA_VERSION_STRING is.
JAVA_RELEASE_NUMBER := $(firstword $(subst ., ,$(subst -, ,$(subst +, ,$(subst _, ,$(patsubst 1.%,%,${JAVA_VERSION_STRING}))))))

# $(call java_at_least,NN) is "1" if the JDK at JAVA_HOME is version NN or
# later, and is empty otherwise (including if there is no JDK).
java_at_least = $(shell test 0${JAVA_RELEASE_NUMBER} -ge $(1) && echo 1)
