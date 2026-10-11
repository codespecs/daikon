## daikon.bashrc
## Daikon initialization file for bash users.

## Optionally, you may set the following environment variables:
##   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
##        files, instead of the default, which is to use daikon.jar.  This is
##        useful if you have made changes to Daikon and compiled the .java
##        files to .class files but have not re-made the daikon.jar file.
## You should not need to edit this file.

# Find the JDK that contains the javac on the PATH.  (Daikon requires a JDK, not
# just a JRE, because it compiles splitters at run time.)  Following symlinks is
# fast, but it does not work for a wrapper script (as used by jenv, asdf, or mise)
# or for the macOS /usr/bin/javac stub, so in those cases ask the JVM instead.
# A JDK home contains a "release" file; a wrapper's directory does not.
# For JDK 8, java.home is the jre/ subdirectory of the JDK.
# A failed detection does not modify JAVA_HOME in the user's shell.
daikon_java_home="$JAVA_HOME"
# "type -P" ignores aliases such as "alias javac='javac -g'".
if [ -z "$daikon_java_home" ] && daikon_java_home=$(type -P javac) ; then
  daikon_java_home=$(readlink -f "$daikon_java_home" 2> /dev/null)
  daikon_java_home="${daikon_java_home%/bin/javac}"
  if [ ! -f "$daikon_java_home/release" ] ; then
    daikon_java_home=$(command javac -J-XshowSettings:properties -version 2>&1 | sed -n -e 's/^ *java\.home = //p')
    if [ -x "${daikon_java_home%/jre}/bin/javac" ] ; then
      daikon_java_home="${daikon_java_home%/jre}"
    fi
  fi
fi
if [ -z "$daikon_java_home" ] || [ ! -x "$daikon_java_home/bin/javac" ] ; then
  if [ -n "$JAVA_HOME" ] ; then
    echo "JAVA_HOME=$JAVA_HOME does not contain bin/javac; set it to a JDK or unset it.  Aborting daikon.bashrc ."
  else
    echo "Cannot find a JDK; set JAVA_HOME or put javac on the PATH.  Aborting daikon.bashrc ."
  fi
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
