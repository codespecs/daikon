# Makefile fragment that sets JAVA_HOME, if it is not already set to a non-empty value.
# It is included by java/Makefile and tests/Makefile.common.
# The same logic appears in scripts/daikon.bashrc.

JAVA_HOME_WAS_SET := $(strip ${JAVA_HOME})

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

# If the user set JAVA_HOME, warn if the javac on the PATH is in a different
# JDK, because some commands use the javac on the PATH.  A javac that is not
# within a JDK (such as a wrapper script, or the macOS /usr/bin/javac stub) is
# not checked.  A JDK is recognized by its "release" file.
ifneq (,${JAVA_HOME_WAS_SET})
  JAVAC_ON_PATH_REALPATH := $(realpath $(shell command -v javac 2> /dev/null))
  ifneq (,$(and $(filter %/bin/javac,${JAVAC_ON_PATH_REALPATH}),$(wildcard $(patsubst %/bin/javac,%,${JAVAC_ON_PATH_REALPATH})/release)))
    ifneq (${JAVAC_ON_PATH_REALPATH},$(realpath ${JAVA_HOME}/bin/javac))
      $(warning JAVA_HOME is ${JAVA_HOME}, but the javac on the PATH is ${JAVAC_ON_PATH_REALPATH}; make them agree)
    endif
  endif
endif

# $(call checked_java_home,DIR) yields DIR, or fails if DIR does not contain a JDK.
checked_java_home = $(if $(and $(1),$(wildcard $(1)/bin/javac)),$(1),$(error No JDK found at JAVA_HOME="$(1)"; set JAVA_HOME or put javac on the PATH))
