# Sets JAVA_HOME, if it is not already set, and exports it so that sub-makes
# do not recompute it.  Requires javac to be on the PATH.

JAVAC_ON_PATH := $(shell command -v javac 2>/dev/null)
ifeq (,$(JAVAC_ON_PATH))
  $(error javac is not on the PATH)
endif

ifeq ($(shell uname), Darwin)
  # On macOS, /usr/bin/javac is a stub that uses JAVA_HOME, so JAVA_HOME and
  # the javac on the PATH always agree.
  ifndef JAVA_HOME
    JAVA_HOME := $(shell /usr/libexec/java_home)
    ifeq (,$(JAVA_HOME))
      $(error /usr/libexec/java_home did not find a JDK; set JAVA_HOME)
    endif
  endif
else
  # Resolve symbolic links such as /usr/bin/javac -> /etc/alternatives/javac.
  JAVAC_ON_PATH_REALPATH := $(realpath $(JAVAC_ON_PATH))
  ifndef JAVA_HOME
    JAVA_HOME := $(patsubst %/bin/javac,%,$(JAVAC_ON_PATH_REALPATH))
  else ifneq ($(realpath $(JAVA_HOME)/bin/javac),$(JAVAC_ON_PATH_REALPATH))
    $(warning JAVA_HOME is $(JAVA_HOME), but the javac on the PATH is $(JAVAC_ON_PATH_REALPATH))
  endif
endif

export JAVA_HOME
