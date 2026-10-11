## daikon.bashrc
## Daikon initialization file for bash users.

## Optionally, you may set the following environment variables:
##   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
##        files, instead of the default, which is to use daikon.jar.  This is
##        useful if you have made changes to Daikon and compiled the .java
##        files to .class files but have not re-made the daikon.jar file.
## You should not need to edit this file.

# This logic is the same as in java/java-home.mk:  on macOS, use
# /usr/libexec/java_home; otherwise, ask the javac on the PATH which JDK it is
# in, falling back to resolving symbolic links to javac.
# For JDK 8, java.home is the jre/ subdirectory of the JDK.
# A failed detection does not modify JAVA_HOME in the user's shell.
daikon_java_home="$JAVA_HOME"
if [ -z "$daikon_java_home" ] ; then
  if [ "$(uname)" = "Darwin" ] ; then
    daikon_java_home=$(/usr/libexec/java_home 2> /dev/null)
  else
    daikon_java_home=$(javac -J-XshowSettings:properties -version 2>&1 | sed -n -e '/^ *java\.home = /{s///;s:/jre$::;p;}')
    if [ -z "$daikon_java_home" ] ; then
      daikon_java_home=$(readlink -f "$(command -v javac)" 2> /dev/null | sed -e 's:/bin/javac$::')
    fi
  fi
fi
# JDK 9 and later contain lib/jrt-fs.jar; JDK 8 contains jre/lib/rt.jar.
if [ -z "$daikon_java_home" ] ; then
  unset daikon_java_home
  echo "Cannot infer JAVA_HOME; please set it.  Aborting daikon.bashrc ."
  return 2
elif [ ! -x "$daikon_java_home/bin/javac" ] \
     || { [ ! -f "$daikon_java_home/lib/jrt-fs.jar" ] && [ ! -f "$daikon_java_home/jre/lib/rt.jar" ]; } ; then
  echo "No JDK found at JAVA_HOME=$daikon_java_home; please fix it.  Aborting daikon.bashrc ."
  unset daikon_java_home
  return 2
fi
export JAVA_HOME="$daikon_java_home"
unset daikon_java_home

if [ -z "${BASH_SOURCE:-}" ]; then
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
  # export DAIKONDIR="$( readlink -e "$( dirname "${BASH_SOURCE}" )/..")"
  # Code from: https://stackoverflow.com/q/59895/173852
  export DAIKONDIR="$( cd "$( dirname "${BASH_SOURCE}" )/.." >/dev/null && pwd )"
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
