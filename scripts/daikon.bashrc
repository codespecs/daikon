## daikon.bashrc
## Daikon initialization file for Bourne shell (bash) users.
## This file should be kept in synch with daikon.cshrc

## Set this directory to the directory containing "daikon".
## TODO:  set only if not already set
export DAIKONPARENT=/path/to/parent/of/daikon

## Set this directory to the directory containing the JDK.
## TODO:  set only if not already set
export JDKDIR=/g2/jdk1.3.1

## You should uncomment exactly one of the following lines, depending on
## want to run Daikon from the precompiled bytecode files in daikon.jar, or
## you want to run Daikon from .class files that you compile yourself.  The
## former is easier, but the latter permits you to modify Daikon (most
## users will not need to do so).  The latter may be used only if you
## downloaded the source distribution.
export CLASSPATH=$DAIKONPARENT/daikon/daikon.jar:${CLASSPATH}
# setenv CLASSPATH $DAIKONPARENT/daikon/java:$DAIKONPARENT/daikon/java/lib/log4j.jar:${CLASSPATH}

## Add the Daikon binaries to your path
export PATH=$DAIKONPARENT/daikon/bin:${JDKDIR}/bin:${PATH}

## tools.jar must be on your classpath.  Also, if you wish to use dfej, the
## Daikon front end for Java, you need to have rt.jar on your classpath.
export CLASSPATH=${CLASSPATH}:${JDKDIR}/jre/lib/rt.jar:${JDKDIR}/lib/tools.jar

## Indicates where Ajax should find its helper files such as
## main-harness.csal, tweaked-classes.zip, etc.  Given a Java program, Ajax
## determines which variables can be sensibly compared to one another.
export AJAX_DIR=$DAIKONPARENT/daikon/java/ajax-ship
