## daikon.bashrc
## Daikon initialization file for Bourne shell (bash) users.
## This file should be kept in synch with daikon.cshrc

## Set this directory to the directory containing "daikon".
export DAIKONPARENT=${DAIKONPARENT:-/path/to/parent/of/daikon}
export DAIKONDIR=${DAIKONDIR:-${DAIKONPARENT}/daikon}

## Set this directory to the directory containing the JDK.
export JDKDIR=${JDKDIR:-/g2/jdk}

## Set DAIKONCLASS_SOURCES if you want to run Daikon from .class files that
## you compile yourself.  Otherwise, you will run Daikon from the
## precompiled bytecode files in daikon.jar.  The latter is easier (and is
## the default), but the former permits you to modify Daikon (most users
## will not need to do so).  The former may be used only if you downloaded
## the source distribution.
# export DAIKONCLASS_SOURCES=1

if [ $DAIKONCLASS_SOURCES ]; then
  CPADD=$DAIKONDIR/java:$DAIKONDIR/java/lib/log4j.jar
else
  CPADD=$DAIKONDIR/daikon.jar
fi
if [ ! -z "$CLASSPATH ]; then
  export CLASSPATH=${CPADD}:${CLASSPATH}
else
  export CLASSPATH=${CPADD}
endif


## Add the Daikon binaries to your path
export PATH=$DAIKONDIR/bin:${JDKDIR}/bin:${PATH}

## tools.jar must be on your classpath.  Also, if you wish to use dfej, the
## Daikon front end for Java, you need to have rt.jar on your classpath.
export CLASSPATH=${CLASSPATH}:${JDKDIR}/jre/lib/rt.jar:${JDKDIR}/lib/tools.jar

## Indicates where Ajax should find its helper files such as
## main-harness.csal, tweaked-classes.zip, etc.  Given a Java program, Ajax
## determines which variables can be sensibly compared to one another.
export AJAX_DIR=$DAIKONDIR/java/ajax-ship
