# Makefile fragment that sets JAVA_HOME, if it is not already set to a non-empty
# value, and checks it.
# It is included by java/Makefile and tests/Makefile.common.
# scripts/daikon.bashrc finds JAVA_HOME in the same way.

# Ask the javac on the PATH which JDK it is in.  This works even if javac is a
# symbolic link or a wrapper script (as used by jenv, asdf, or mise).
# For JDK 8, java.home is the jre/ subdirectory of the JDK.
ifeq ($(strip ${JAVA_HOME}),)
  JAVA_HOME := $(shell javac -J-XshowSettings:properties -version 2>&1 | sed -n -e '/^ *java\.home = /{s///;s:/jre$$::;p;}')
endif
ifeq ($(strip ${JAVA_HOME}),)
  ifeq ($(shell uname), Darwin)
    JAVA_HOME := $(shell /usr/libexec/java_home 2> /dev/null)
  endif
endif
ifeq ($(strip ${JAVA_HOME}),)
  $(error Cannot infer JAVA_HOME; set JAVA_HOME or put javac on the PATH)
endif
# JDK 9 and later contain lib/jrt-fs.jar; JDK 8 contains jre/lib/rt.jar.
# This uses the shell rather than $(wildcard ...), which does not handle spaces.
ifneq ($(shell [ -x '${JAVA_HOME}/bin/javac' ] && { [ -f '${JAVA_HOME}/lib/jrt-fs.jar' ] || [ -f '${JAVA_HOME}/jre/lib/rt.jar' ]; } && echo yes),yes)
  $(warning No JDK found at JAVA_HOME="${JAVA_HOME}")
endif
# Commands run by make, including recursive invocations of make, use this value.
export JAVA_HOME
