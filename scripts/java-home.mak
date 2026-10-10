# Sets JAVA_HOME, if it is not already set, to the JDK that contains the javac
# on the PATH.  If JAVA_HOME is already set, checks that it agrees with the
# javac on the PATH.  Requires javac to be on the PATH.

JAVAC_ON_PATH := $(shell command -v javac 2>/dev/null)
ifeq (,$(JAVAC_ON_PATH))
  $(error javac is not on the PATH)
endif
# Resolve symbolic links such as /usr/bin/javac -> /etc/alternatives/javac.
JAVAC_ON_PATH_REALPATH := $(realpath $(JAVAC_ON_PATH))

# On macOS, /usr/bin/javac is a stub that runs the javac of JAVA_HOME if it is
# set, or else of the JDK that /usr/libexec/java_home reports.
JAVAC_ON_PATH_IS_MACOS_STUB :=
ifeq ($(shell uname), Darwin)
  ifeq ($(JAVAC_ON_PATH_REALPATH), /usr/bin/javac)
    JAVAC_ON_PATH_IS_MACOS_STUB := 1
  endif
endif

ifndef JAVA_HOME
  ifdef JAVAC_ON_PATH_IS_MACOS_STUB
    JAVA_HOME := $(shell /usr/libexec/java_home 2>/dev/null)
    ifeq (,$(JAVA_HOME))
      $(error /usr/libexec/java_home did not find a JDK; set JAVA_HOME or put a JDK's bin directory on the PATH)
    endif
  else
    JAVA_HOME := $(patsubst %/bin/javac,%,$(JAVAC_ON_PATH_REALPATH))
  endif
  ifeq (,$(wildcard $(JAVA_HOME)/bin/javac))
    $(error Cannot determine JAVA_HOME from $(JAVAC_ON_PATH) (which resolves to $(JAVAC_ON_PATH_REALPATH)); set JAVA_HOME)
  endif
else ifndef JAVAC_ON_PATH_IS_MACOS_STUB
  # A javac that is not within a JDK, such as a jenv shim script, is not checked.
  ifneq ($(filter %/bin/javac,$(JAVAC_ON_PATH_REALPATH)),)
  ifneq ($(realpath $(JAVA_HOME)/bin/javac),$(JAVAC_ON_PATH_REALPATH))
    $(error JAVA_HOME is $(JAVA_HOME), but the javac on the PATH is $(JAVAC_ON_PATH_REALPATH); make them agree)
  endif
  endif
endif
