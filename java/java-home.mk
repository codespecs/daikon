# Makefile fragment that sets JAVA_HOME (if it is not already set to a non-empty
# value), JAVA_VERSION_STRING, JAVA_RELEASE_NUMBER, and CHECKED_JAVA_HOME.
# It is included by the top-level Makefile, java/Makefile, and tests/Makefile.common.
# The same JAVA_HOME logic appears in scripts/daikon.bashrc.

# Ask the JVM that runs javac for its home directory.  This works even if javac
# on the PATH is a wrapper script (as used by jenv, asdf, or mise) rather than a
# symbolic link.  For JDK 8, java.home is the "jre" subdirectory of the JDK.
# Use ":=" so that the shell commands run only once.
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

# CHECKED_JAVA_HOME is JAVA_HOME, but expanding it fails if JAVA_HOME does not
# contain a JDK.  The check happens only when a variable that uses it is
# expanded, so targets that do not run Java (such as "clean") work without a JDK.
CHECKED_JAVA_HOME = $(if $(and ${JAVA_HOME},$(wildcard ${JAVA_HOME}/bin/javac)),${JAVA_HOME},$(error No JDK found at JAVA_HOME="${JAVA_HOME}"; set JAVA_HOME or put javac on the PATH))

# The version of the JDK at JAVA_HOME, read from its "release" file without
# starting a JVM.  Examples: "1.8.0_462", "25.0.1", "27-ea".  Each variable is
# empty if JAVA_HOME does not contain a JDK.
JAVA_VERSION_STRING := $(if $(strip ${JAVA_HOME}),$(shell sed -n 's/^JAVA_VERSION="\(.*\)"/\1/p' '${JAVA_HOME}/release' 2> /dev/null))
# The major version, such as "8" or "25".
JAVA_RELEASE_NUMBER := $(shell echo '${JAVA_VERSION_STRING}' | sed -e 's/^1\.//' -e 's/^\([0-9]*\).*/\1/')

# Warn if the javac on the PATH is in a different JDK than JAVA_HOME, because
# some commands use the javac on the PATH.  Skip the check if JAVA_HOME does not
# contain a JDK (expanding CHECKED_JAVA_HOME reports that), or if the javac on
# the PATH is not within a JDK (such as a wrapper script, or the macOS
# /usr/bin/javac stub).  A JDK is recognized by its "release" file.
# The check is done only once, not in every recursive invocation of make.
ifndef DAIKON_JAVA_HOME_CHECKED
  export DAIKON_JAVA_HOME_CHECKED := 1
  ifneq (,$(wildcard ${JAVA_HOME}/bin/javac))
    JAVAC_ON_PATH_REALPATH := $(realpath $(shell command -v javac 2> /dev/null))
    ifneq (,$(and $(filter %/bin/javac,${JAVAC_ON_PATH_REALPATH}),$(wildcard $(patsubst %/bin/javac,%,${JAVAC_ON_PATH_REALPATH})/release)))
      ifneq (${JAVAC_ON_PATH_REALPATH},$(realpath ${JAVA_HOME}/bin/javac))
        $(warning JAVA_HOME is ${JAVA_HOME}, but the javac on the PATH is ${JAVAC_ON_PATH_REALPATH}; make them agree)
      endif
    endif
  endif
endif
