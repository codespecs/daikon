## daikon.bashrc
## Daikon initialization file for bash users.

## Optionally, you may set the following environment variables:
##   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
##        files, instead of the default, which is to use daikon.jar.  This is
##        useful if you have made changes to Daikon and compiled the .java
##        files to .class files but have not re-made the daikon.jar file.
## You should not need to edit this file.

# This logic is the same as in java-home.mak:  use the JDK that contains the
# javac on the PATH.  On macOS, /usr/bin/javac is a stub; then use the JDK that
# /usr/libexec/java_home reports.
if [ -z "$JAVA_HOME" ] ; then
  DAIKON_JAVAC_REALPATH=$(type -P javac > /dev/null && readlink -f "$(type -P javac)")
  if [ "$(uname)" = "Darwin" ] && [ "$DAIKON_JAVAC_REALPATH" = "/usr/bin/javac" ] ; then
    JAVA_HOME=$(/usr/libexec/java_home)
  elif [ -n "$DAIKON_JAVAC_REALPATH" ] ; then
    JAVA_HOME=$(dirname "$(dirname "$DAIKON_JAVAC_REALPATH")")
  fi
  unset DAIKON_JAVAC_REALPATH
  export JAVA_HOME
fi
if [ ! -d "$JAVA_HOME" ]; then
  echo "Cannot infer JAVA_HOME; please set it.  Aborting daikon.bashrc ."
  return 2
fi

if [ ${#BASH_SOURCE[@]} -eq 0 ]; then
  # Cannot infer DAIKONDIR.
  if [ -z ${DAIKONDIR+x} ]; then
    echo "Please set DAIKONDIR environment variable.  Aborting daikon.bashrc ."
    return 2
  elif [ ! -d "$DAIKONDIR" ]; then
    echo "DAIKONDIR is set to non-existent directory: $DAIKONDIR"
    echo "Please set DAIKONDIR to an existing directory.  Aborting daikon.bashrc ."
    return 2
  fi
else
  ## Note that this overrides any previous setting.
  # Mac OS does not have "-e" argument to readlink
  # export DAIKONDIR="$( readlink -e "$( dirname "${BASH_SOURCE[0]}" )/..")"
  # Code from: https://stackoverflow.com/q/59895/173852
  export DAIKONDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null && pwd )"
fi

if [ -z "$DAIKONSCRIPTS" ]; then
  if [ -d ${DAIKONDIR}/scripts ]; then
    export DAIKONSCRIPTS=${DAIKONDIR}/scripts
  else
    echo "Cannot choose a value for environment variable DAIKONSCRIPTS."
    echo "Maybe DAIKONDIR is badly set: $DAIKONDIR"
    echo "Please fix the problem.  Aborting daikon.bashrc ."
    return 2
  fi
fi

if [ -z "$PLUME_SCRIPTS" ]; then
  export PLUME_SCRIPTS=${DAIKONDIR}/.utils/plume-scripts
fi

DAIKON_CLASSPATH=${DAIKONDIR}/daikon.jar:${JAVA_HOME}/jre/lib/rt.jar:${JAVA_HOME}/lib/tools.jar
# Avoid warnings about non-existent elements on classpath
if [ -d ${DAIKONDIR}/java ]; then
  DAIKON_CLASSPATH=${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKON_CLASSPATH}
fi
export DAIKON_CLASSPATH

export PATH=${DAIKONSCRIPTS}:${PLUME_SCRIPTS}:${PATH}:${JAVA_HOME}/bin

## Indicate where to find Perl modules such as util_daikon.pm.
if [ $PERL5LIB ]; then
  export PERL5LIB=${DAIKONSCRIPTS}:${PLUME_SCRIPTS}:${PERL5LIB}
fi
if [ $PERLLIB ]; then
  export PERLLIB=${DAIKONSCRIPTS}:${PLUME_SCRIPTS}:${PERLLIB}
else
  export PERLLIB=${DAIKONSCRIPTS}:${PLUME_SCRIPTS}
fi
