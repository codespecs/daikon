## daikon.cshrc
## Daikon initialization file for C shell (csh and tcsh) users.
## This file should be kept in synch with daikon.bashrc.

## Set this directory to the directory containing "daikon".
if (! $?DAIKONPARENT) setenv DAIKONPARENT /path/to/parent/of/daikon
if (! $?DAIKONDIR) setenv DAIKONDIR $DAIKONPARENT/daikon

## Set this directory to the directory containing the JDK.
if (! $?JDKDIR) setenv JDKDIR /g2/jdk

## Set DAIKONCLASS_SOURCES if you want to run Daikon from .class files that
## you compile yourself.  Otherwise, you will run Daikon from the
## precompiled bytecode files in daikon.jar.  The latter is easier (and is
## the default), but the former permits you to modify Daikon (most users
## will not need to do so).  The former may be used only if you downloaded
## the source distribution.
# setenv DAIKONCLASS_SOURCES 1

if (($?DAIKONCLASS_SOURCES) && ($DAIKONCLASS_SOURCES)) then
  setenv CPADD $DAIKONDIR/java:$DAIKONDIR/java/lib/log4j.jar
else
  setenv CPADD $DAIKONDIR/daikon.jar
endif
if ($?CLASSPATH) then
  setenv CLASSPATH ${CPADD}:${CLASSPATH}
else
  setenv CLASSPATH ${CPADD}
endif

## Add the Daikon binaries to your path
set path = ($DAIKONDIR/bin $JDKDIR/bin $path)

## tools.jar must be on your classpath.  Also, if you wish to use dfej, the
## Daikon front end for Java, you need to have rt.jar on your classpath.
setenv CLASSPATH ${CLASSPATH}:${JDKDIR}/jre/lib/rt.jar:${JDKDIR}/lib/tools.jar

## Indicates where Ajax should find its helper files such as
## main-harness.csal, tweaked-classes.zip, etc.  Given a Java program, Ajax
## determines which variables can be sensibly compared to one another.
setenv AJAX_DIR $DAIKONDIR/java/ajax-ship
