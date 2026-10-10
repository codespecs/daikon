# Sets JAVA_HOME, if it is not already set, to the JDK that contains the javac
# on the PATH.  If JAVA_HOME is already set, checks that it agrees with the
# javac on the PATH.  Requires javac to be on the PATH.
# A JDK is recognized by its "release" file.
# Paths that contain whitespace are not supported, because Make splits on it.

JAVAC_ON_PATH := $(shell command -v javac 2>/dev/null)
ifeq (,$(JAVAC_ON_PATH))
  $(error javac is not on the PATH)
endif
ifneq (1,$(words $(JAVAC_ON_PATH)))
  $(error The javac on the PATH, "$(JAVAC_ON_PATH)", contains whitespace, which is not supported)
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

# The JDK that contains the javac on the PATH, or empty if that javac is not
# within a JDK (for example, it is a wrapper script or a jenv shim).
JAVAC_ON_PATH_JDK :=
ifndef JAVAC_ON_PATH_IS_MACOS_STUB
  ifneq (,$(filter %/bin/javac,$(JAVAC_ON_PATH_REALPATH)))
    ifneq (,$(wildcard $(patsubst %/bin/javac,%,$(JAVAC_ON_PATH_REALPATH))/release))
      JAVAC_ON_PATH_JDK := $(patsubst %/bin/javac,%,$(JAVAC_ON_PATH_REALPATH))
    endif
  endif
endif

ifndef JAVA_HOME
  ifdef JAVAC_ON_PATH_IS_MACOS_STUB
    JAVA_HOME := $(shell /usr/libexec/java_home 2>/dev/null)
    ifeq (,$(JAVA_HOME))
      $(error /usr/libexec/java_home did not find a JDK; set JAVA_HOME or put a JDK's bin directory on the PATH)
    endif
  else ifdef JAVAC_ON_PATH_JDK
    JAVA_HOME := $(JAVAC_ON_PATH_JDK)
  else
    $(error Cannot determine JAVA_HOME: $(JAVAC_ON_PATH) (which resolves to $(JAVAC_ON_PATH_REALPATH)) is not within a JDK; set JAVA_HOME)
  endif
else
  ifneq (1,$(words $(JAVA_HOME)))
    $(error JAVA_HOME, "$(JAVA_HOME)", contains whitespace, which is not supported)
  endif
  ifeq (,$(wildcard $(JAVA_HOME)/bin/javac))
    $(error JAVA_HOME is $(JAVA_HOME), but $(JAVA_HOME)/bin/javac does not exist)
  endif
  # A javac that is not within a JDK is not checked.
  ifdef JAVAC_ON_PATH_JDK
    ifneq ($(realpath $(JAVA_HOME)/bin/javac),$(JAVAC_ON_PATH_REALPATH))
      $(error JAVA_HOME is $(JAVA_HOME), but the javac on the PATH is $(JAVAC_ON_PATH_REALPATH); make them agree)
    endif
  endif
endif
