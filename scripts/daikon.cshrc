## daikon.cshrc
## Daikon initialization file for C shell (csh and tcsh) users.
## This file should be kept in synch with daikon.bashrc.

## Set this directory to the directory containing "daikon".
if (! $?DAIKONPARENT) setenv DAIKONPARENT /path/to/parent/of/daikon

## Set this directory to the directory containing the JDK.
if (! $?JDKDIR) setenv JDKDIR /g2/jdk

## You should uncomment exactly one of the following lines, depending on
## want to run Daikon from the precompiled bytecode files in daikon.jar, or
## you want to run Daikon from .class files that you compile yourself.  The
## former is easier, but the latter permits you to modify Daikon (most
## users will not need to do so).  The latter may be used only if you
## downloaded the source distribution.
setenv CLASSPATH $DAIKONPARENT/daikon/daikon.jar:${CLASSPATH}
# setenv CLASSPATH $DAIKONPARENT/daikon/java:$DAIKONPARENT/daikon/java/lib/log4j.jar:${CLASSPATH}

## Add the Daikon binaries to your path
set path = ($DAIKONPARENT/daikon/bin $JDKDIR/bin $path)

## tools.jar must be on your classpath.  Also, if you wish to use dfej, the
## Daikon front end for Java, you need to have rt.jar on your classpath.
setenv CLASSPATH ${CLASSPATH}:${JDKDIR}/jre/lib/rt.jar:${JDKDIR}/lib/tools.jar

## Indicates where Ajax should find its helper files such as
## main-harness.csal, tweaked-classes.zip, etc.  Given a Java program, Ajax
## determines which variables can be sensibly compared to one another.
setenv AJAX_DIR $DAIKONPARENT/daikon/java/ajax-ship
