## daikon.bashrc
## Daikon initialization file for bash users.

## Optionally, you may set the following environment variables:
##   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
##        files, instead of the default, which is to use daikon.jar.  This is
##        useful if you have made changes to Daikon and compiled the .java
##        files to .class files but have not re-made the daikon.jar file.
## You should not need to edit this file.

# This logic is the same as in java/java-home.mk, except that running Daikon
# requires only a JRE, so this uses the java on the PATH if there is no javac.
# For JDK 8, java.home is the jre/ subdirectory of the JDK.
# A failed detection does not modify JAVA_HOME in the user's shell.
daikon_java_home="$JAVA_HOME"
if [ -z "$daikon_java_home" ] ; then
  if [ "$(uname):$(command -v javac)" = "Darwin:/usr/bin/javac" ] ; then
    daikon_java_home=$(/usr/libexec/java_home 2> /dev/null)
  elif command -v javac > /dev/null 2>&1 ; then
    daikon_java_home=$(javac -J-XshowSettings:properties -version 2>&1 | sed -n -e '/^ *java\.home = /{s///;s:/jre$::;p;}')
  else
    daikon_java_home=$(java -XshowSettings:properties -version 2>&1 | sed -n -e '/^ *java\.home = /{s///;s:/jre$::;p;}')
  fi
fi
if [ -z "$daikon_java_home" ] || [ ! -x "$daikon_java_home/bin/java" ] ; then
  echo "No Java found at JAVA_HOME=$daikon_java_home; set JAVA_HOME or put java on the PATH.  Aborting daikon.bashrc ."
  unset daikon_java_home
  return 2
fi
export JAVA_HOME="$daikon_java_home"
unset daikon_java_home

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
