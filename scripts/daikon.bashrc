## daikon.bashrc
## Daikon initialization file for Bourne shell (bash) users.
## (This file should be kept in synch with daikon.cshrc and daikonenv.bat.)

## Set DAIKONPARENT to absolute pathname of the directory containing "daikon/".
export DAIKONPARENT=${DAIKONPARENT:-/path/to/parent/of/daikon}
export DAIKONDIR=${DAIKONDIR:-${DAIKONPARENT}/daikon}
export DFECDIR=${DFECDIR:-${DAIKONDIR}/front-end/c}
export DAIKONBIN=${DAIKONBIN:-${DAIKONDIR}/bin}

## Set this directory to the directory containing the JDK.
export JDKDIR=${JDKDIR:-/g2/jdk}

## Set DAIKONCLASS_SOURCES if you want to run Daikon from .class files that
## you compile yourself.  This permits you to modify Daikon (most users
## will not need to do so).  If you do not set DAIKONCLASS_SOURCES, you will
## run Daikon from the precompiled bytecode files in daikon.jar.
# export DAIKONCLASS_SOURCES=1

if [ $DAIKONCLASS_SOURCES ]; then
  CPADD=${DAIKONDIR}/java
else
  CPADD=${DAIKONDIR}/daikon.jar
fi
if [ ! -z "$CLASSPATH" ]; then
  export CLASSPATH=${CPADD}:${CLASSPATH}
else
  export CLASSPATH=${CPADD}
fi

## tools.jar must be on your classpath.  Also, if you wish to use dfej (the
## Daikon front end for Java), rt.jar must be on your classpath.
if [ "$OSTYPE" != "darwin" ]; then
  export CLASSPATH=${CLASSPATH}:${JDKDIR}/jre/lib/rt.jar:${JDKDIR}/lib/tools.jar
else
  ## For Macintosh MacOSX users.  (This list
  ## is the system property "sun.boot.class.path".)
  export CLASSPATH=${CLASSPATH}:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/classes.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/ui.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/i18n.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/sunrsasign.jar
fi

## (ajax.jar is temporary, will be removed soon, we hope.)
if [ -e ${DAIKONDIR}/java/ajax-ship/ajax.jar ]; then
  export CLASSPATH=${CLASSPATH}:${DAIKONDIR}/java/ajax-ship/ajax.jar
fi

## Add the Daikon binaries to your path
export PATH=${DAIKONBIN}:${DAIKONDIR}/front-end/java/src:${DFECDIR}:${JDKDIR}/bin:${PATH}

## Indicate where to find Perl modules such as util_daikon.pm.
if [ $PERLLIB ]; then
  export PERLLIB=${DAIKONBIN}:${PERLLIB}
else
  export PERLLIB=${DAIKONBIN}
fi

## Indicates where Ajax should find its helper files such as
## main-harness.csal, tweaked-classes.zip, etc.  Given a Java program, Ajax
## determines which variables can be sensibly compared to one another.
export AJAX_DIR=${DAIKONDIR}/java/ajax-ship

## Indicates where Lackwit can find its libraries (and binaries).
export LACKWIT_HOME=$DFECDIR/lackwit
