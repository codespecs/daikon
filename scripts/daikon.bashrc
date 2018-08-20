## daikon.bashrc
## Daikon initialization file for Bourne shell (bash) users.
## (This file should be kept in synch with daikon.cshrc and daikonenv.bat.)

## Wherever you source this file, you should set two environment variables:
##   JAVA_HOME      absolute pathname of the directory containing the JDK
##                  (or "none" if you don't have it)
## Optionally, you may set the following environment variables:
##   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
##        files, instead of the default, which is to use daikon.jar.  This is
##        useful if you have made changes to Daikon and compiled the .java
##        files to .class files but have not re-made the daikon.jar file.
## You should not need to edit this file.

if [ -z "$JAVA_HOME" ]; then
  echo "JAVA_HOME environment variable is not set."
  echo "Please fix this before proceeding.  Aborting daikon.bashrc ."
  return 2
elif [ ! -d "$JAVA_HOME" -a "$JAVA_HOME" != "none" ]; then
  echo "JAVA_HOME is set to non-existent directory: $JAVA_HOME"
  echo "Please fix this before proceeding.  Aborting daikon.bashrc ."
  return 2
fi

DAIKONDIR="$( readlink -e "$( dirname "${BASH_SOURCE[0]}" )/..")"
echo "DAIKONDIR=$DAIKONDIR"

if [ -z "$DAIKONSCRIPTS" ]; then
  if [ -d ${DAIKONDIR}/scripts ]; then
    export DAIKONSCRIPTS=${DAIKONDIR}/scripts
  else
    echo "Cannot choose a value for environment variable DAIKONSCRIPTS."
    echo "Please fix this before proceeding.  Aborting daikon.bashrc ."
    return 2
  fi
fi

if [ -z "$PLUMESCRIPTS" ]; then
  export PLUMESCRIPTS=${DAIKONDIR}/utils/plume-scripts
fi

# export DAIKONCLASS_SOURCES=1
if [ $DAIKONCLASS_SOURCES ]; then
  CPADD=${DAIKONDIR}/java
else
  CPADD=${DAIKONDIR}/daikon.jar
fi

if [ ! -z "$CLASSPATH" ]; then
  # For Cygwin we need to convert CLASSPATH to Window's format
  # BUT - we assume existing CLASSPATH is already in Window's format
  if [ "$OSTYPE" == "cygwin" ]; then
    CPADD1="`cygpath -wp ${CPADD}`"
    CPADD2="`cygpath -wp ${JAVA_HOME}/jre/lib/rt.jar:${JAVA_HOME}/lib/tools.jar`"
    export CLASSPATH="${CPADD1};${CLASSPATH};${CPADD2}"
  else
    export CLASSPATH=${CPADD}:${CLASSPATH}:${JAVA_HOME}/jre/lib/rt.jar:${JAVA_HOME}/lib/tools.jar
  fi
else
  if [ -n "$PS1" ]; then echo "Warning: daikon.bashrc is setting CLASSPATH, which was previously unset"; fi
  export CLASSPATH=${CPADD}:${JAVA_HOME}/jre/lib/rt.jar:${JAVA_HOME}/lib/tools.jar
  # For Cygwin we need to convert CLASSPATH to windows format
  if [ "$OSTYPE" == "cygwin" ]; then
    export CLASSPATH="`cygpath -wp $CLASSPATH`"
  fi
fi

## Make sure the specified JDK is first on your path
export PATH=$JAVA_HOME/bin:$PATH

## Add the Daikon binaries to your path
export PATH=${DAIKONSCRIPTS}:${PLUMESCRIPTS}:${PATH}

## Indicate where to find Perl modules such as util_daikon.pm.
if [ $PERL5LIB ]; then
  export PERL5LIB=${DAIKONSCRIPTS}:${PLUMESCRIPTS}:${PERL5LIB}
fi

if [ $PERLLIB ]; then
  export PERLLIB=${DAIKONSCRIPTS}:${PLUMESCRIPTS}:${PERLLIB}
else
  export PERLLIB=${DAIKONSCRIPTS}:${PLUMESCRIPTS}
fi
