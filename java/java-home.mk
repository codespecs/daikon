# Makefile fragment that sets JAVA_HOME (if it is not already set to a non-empty
# value) and defines DAIKON_JAVA_HOME, CHECKED_JAVA_HOME, JAVA_VERSION_STRING,
# JAVA_RELEASE_NUMBER, and the function java_at_least.
# It is included by the top-level Makefile, java/Makefile, tests/Makefile,
# tests/Makefile.common, and tests/daikon-tests/Makefile.
# scripts/daikon.bashrc finds JAVA_HOME in the same way, but does not compute
# the Java version.

# $(call is_java_home,DIR) is non-empty if DIR contains a JDK or JRE.
# JDK 9 and later contain lib/jrt-fs.jar; JDK 8 contains jre/lib/rt.jar; JRE 8 contains lib/rt.jar.
is_java_home = $(and $(strip $(1)),$(wildcard $(1)/bin/java),$(wildcard $(1)/lib/jrt-fs.jar $(1)/jre/lib/rt.jar $(1)/lib/rt.jar))
# $(call is_jdk,DIR) is non-empty if DIR contains a JDK.
is_jdk = $(and $(call is_java_home,$(1)),$(wildcard $(1)/bin/javac))

# The JDK that contains the javac on the PATH, or empty if the javac on the PATH
# is not (a symbolic link to) a file in a JDK.  For example, it is empty if
# javac is a wrapper script (as used by jenv, asdf, or mise), or the macOS
# /usr/bin/javac stub.
JAVAC_ON_PATH_REALPATH := $(realpath $(shell command -v javac 2> /dev/null))
JAVAC_ON_PATH_JDK := $(patsubst %/bin/javac,%,$(filter %/bin/javac,${JAVAC_ON_PATH_REALPATH}))
JAVAC_ON_PATH_JDK := $(if $(call is_jdk,${JAVAC_ON_PATH_JDK}),${JAVAC_ON_PATH_JDK})

ifeq ($(strip ${JAVA_HOME}),)
  JAVA_HOME := ${JAVAC_ON_PATH_JDK}
endif
# Ask the JVM that runs javac for its home directory.  This is slower because it
# starts a JVM, but it works even if javac on the PATH is a wrapper script.
# For JDK 8, java.home is the "jre" subdirectory of the JDK.
ifeq ($(strip ${JAVA_HOME}),)
  JAVA_HOME := $(shell command -v javac > /dev/null 2>&1 && javac -J-XshowSettings:properties -version 2>&1 | sed -n 's:^ *java\.home = ::p' | sed 's:/jre$$::')
endif
ifeq ($(strip ${JAVA_HOME}),)
  ifeq ($(shell uname), Darwin)
    JAVA_HOME := $(shell /usr/libexec/java_home 2> /dev/null)
  endif
endif
# Recursive invocations of make use this value rather than recomputing it.
export JAVA_HOME

# DAIKON_JAVA_HOME is the value of JAVA_HOME when this file is included.  It
# does not change if a Makefile later reassigns JAVA_HOME, so the commands that
# use it always agree with the version variables below.
DAIKON_JAVA_HOME := ${JAVA_HOME}

# CHECKED_JAVA_HOME is DAIKON_JAVA_HOME, but expanding it fails if
# DAIKON_JAVA_HOME does not contain a JDK.  The check happens only when a
# variable that uses it is expanded, so targets that do not run Java (such as
# "clean") work without a JDK.
CHECKED_JAVA_HOME = $(if $(call is_jdk,${DAIKON_JAVA_HOME}),${DAIKON_JAVA_HOME},$(error No JDK found at JAVA_HOME="${DAIKON_JAVA_HOME}"; set JAVA_HOME or put javac on the PATH))

# The version of the Java installation at DAIKON_JAVA_HOME.  Examples:
# "1.8.0_462", "25.0.1", "27-ea".  It is read from the "release" file, to avoid
# starting a JVM.  If there is no "release" file (as in Debian and Ubuntu's
# OpenJDK 8), it is obtained from "java -version".  It is empty if JAVA_HOME
# does not contain a Java installation.  It is an error if JAVA_HOME contains a
# Java installation but its version cannot be determined.
JAVA_VERSION_STRING :=
ifneq (,$(call is_java_home,${DAIKON_JAVA_HOME}))
  JAVA_VERSION_STRING := $(shell sed -n 's/^JAVA_VERSION="\(.*\)"/\1/p' '${DAIKON_JAVA_HOME}/release' 2> /dev/null)
  ifeq (,${JAVA_VERSION_STRING})
    JAVA_VERSION_STRING := $(shell '${DAIKON_JAVA_HOME}/bin/java' -version 2>&1 | sed -n 's/^.* version "\([^"]*\)".*/\1/p' | head -1)
  endif
  ifeq (,${JAVA_VERSION_STRING})
    $(error Cannot determine the Java version of JAVA_HOME="${DAIKON_JAVA_HOME}")
  endif
endif
# The major version, such as "8" or "25".  It is empty if JAVA_VERSION_STRING is.
JAVA_RELEASE_NUMBER := $(firstword $(subst ., ,$(subst -, ,$(subst +, ,$(subst _, ,$(patsubst 1.%,%,${JAVA_VERSION_STRING}))))))

# $(call java_at_least,NN) is "1" if JAVA_RELEASE_NUMBER is NN or greater, and
# is empty otherwise (including if there is no Java installation).
JAVA_HOME_MK_NUMBERS := $(shell seq 1 999)
java_at_least = $(if $(filter ${JAVA_RELEASE_NUMBER},$(wordlist $(1),999,${JAVA_HOME_MK_NUMBERS})),1)

# Warn if the javac or java on the PATH is in a different JDK than JAVA_HOME,
# because some commands use the javac or java on the PATH.  Skip the check if
# JAVA_HOME does not contain a JDK (expanding CHECKED_JAVA_HOME reports that),
# or if the program on the PATH is not within a Java installation (such as a
# wrapper script, or the macOS /usr/bin/javac stub).
# The check is done once for each combination of JAVA_HOME and PATH, not in
# every recursive invocation of make.
JAVA_HOME_MK_CHECK_KEY := ${DAIKON_JAVA_HOME}:${PATH}
ifneq (${DAIKON_JAVA_HOME_CHECKED},${JAVA_HOME_MK_CHECK_KEY})
  export DAIKON_JAVA_HOME_CHECKED := ${JAVA_HOME_MK_CHECK_KEY}
  ifneq (,$(call is_jdk,${DAIKON_JAVA_HOME}))
    ifneq (,${JAVAC_ON_PATH_JDK})
      ifneq (${JAVAC_ON_PATH_REALPATH},$(realpath ${DAIKON_JAVA_HOME}/bin/javac))
        $(warning JAVA_HOME is ${DAIKON_JAVA_HOME}, but the javac on the PATH is ${JAVAC_ON_PATH_REALPATH}; make them agree)
      endif
    endif
    JAVA_ON_PATH_REALPATH := $(realpath $(shell command -v java 2> /dev/null))
    JAVA_ON_PATH_HOME := $(patsubst %/jre,%,$(patsubst %/bin/java,%,$(filter %/bin/java,${JAVA_ON_PATH_REALPATH})))
    ifneq (,$(call is_java_home,${JAVA_ON_PATH_HOME}))
      ifneq (${JAVA_ON_PATH_REALPATH},$(realpath ${DAIKON_JAVA_HOME}/bin/java))
        $(warning JAVA_HOME is ${DAIKON_JAVA_HOME}, but the java on the PATH is ${JAVA_ON_PATH_REALPATH}; make them agree)
      endif
    endif
  endif
endif
