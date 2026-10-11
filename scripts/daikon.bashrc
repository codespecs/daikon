## daikon.bashrc
## Daikon initialization file for bash users.

## Optionally, you may set the following environment variables:
##   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
##        files, instead of the default, which is to use daikon.jar.  This is
##        useful if you have made changes to Daikon and compiled the .java
##        files to .class files but have not re-made the daikon.jar file.
## You should not need to edit this file.

# This logic is the same as in java/java-home.mk:  if javac on the PATH is (a
# symbolic link to) a file in a JDK, use that JDK; otherwise ask the JVM that
# runs javac for its home directory; otherwise (on macOS) use
# /usr/libexec/java_home.  "command" bypasses any alias for javac.
if [ -z "$JAVA_HOME" ] ; then
  if command -v javac > /dev/null 2>&1 ; then
    javac_realpath=$(realpath "$(command -v javac)" 2> /dev/null)
    javac_jdk=${javac_realpath%/bin/javac}
    # JDK 9 and later contain lib/jrt-fs.jar; JDK 8 contains jre/lib/rt.jar.
    if [ "$javac_jdk" != "$javac_realpath" ] && [ -x "$javac_jdk/bin/java" ] \
       && { [ -f "$javac_jdk/lib/jrt-fs.jar" ] || [ -f "$javac_jdk/jre/lib/rt.jar" ]; } ; then
      JAVA_HOME=$javac_jdk
    else
      JAVA_HOME=$(command javac -J-XshowSettings:properties -version 2>&1 | sed -n 's:^ *java\.home = ::p' | sed 's:/jre$::')
    fi
    unset javac_realpath javac_jdk
  fi
  if [ -z "$JAVA_HOME" ] && [ "$(uname)" = "Darwin" ] ; then
    JAVA_HOME=$(/usr/libexec/java_home 2> /dev/null)
  fi
fi
export JAVA_HOME
if [ ! -d "$JAVA_HOME" ]; then
  echo "Cannot infer JAVA_HOME; please set it.  Aborting daikon.bashrc ."
  return 2
fi
# Running Daikon needs only a JRE, but building Daikon needs a JDK.
if [ ! -x "$JAVA_HOME/bin/javac" ]; then
  echo "Warning: JAVA_HOME=$JAVA_HOME is not a JDK (it has no bin/javac); building Daikon requires a JDK."
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
