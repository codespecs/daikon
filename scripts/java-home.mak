# Sets JAVA_HOME, if it is not already set.
# Uses ":=" so that the shell commands run only once.

ifndef JAVA_HOME
ifeq ($(shell uname), Darwin)
  JAVA_HOME := $(shell /usr/libexec/java_home)
else
ifeq ($(shell which javac > /dev/null 2>&1 && echo found || echo nonexistent), found)
  JAVA_HOME := $(shell readlink -f $(shell which javac) | sed "s:/bin/javac::")
else
  $(info Failure: Here is the output of: which javac)
  $(info $(shell which javac))
  $(error 'which javac' had exit status 1)
endif
endif
endif
