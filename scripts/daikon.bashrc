## daikon.bashrc
## Daikon initialization file for Bourne shell (bash) users.
## (This file should be kept in synch with daikon.cshrc and daikonenv.bat.)

## Wherever you source this file, you should set two environment variables:
##   DAIKONPARENT   absolute pathname of the directory containing "daikon/"
##   JDKDIR         absolute pathname of the directory containing the JDK
## Optionally, you may set the following environment variables:
##   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
##        files, instead of the default, which is to use daikon.jar.
## You should not need to edit this file directly.


export JDKDIR=${JDKDIR:-/directory/containing/jdk}
export DAIKONPARENT=${DAIKONPARENT:-/path/to/parent/of/daikon}
export DAIKONDIR=${DAIKONDIR:-${DAIKONPARENT}/daikon}
export DFECDIR=${DFECDIR:-${DAIKONDIR}/front-end/c}
export DAIKONBIN=${DAIKONBIN:-${DAIKONDIR}/bin}

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

## tools.jar must be on your classpath.
if [ "$OSTYPE" != "darwin" ]; then
  export CLASSPATH=${CLASSPATH}:${JDKDIR}/jre/lib/rt.jar:${JDKDIR}/lib/tools.jar
else
  ## For Macintosh MacOSX users.  (This list
  ## is the system property "sun.boot.class.path".)
  export CLASSPATH=${CLASSPATH}:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/classes.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/ui.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/i18n.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/sunrsasign.jar
fi

## Add the Daikon binaries to your path
export PATH=${DAIKONBIN}:${DFECDIR}:${JDKDIR}/bin:${PATH}

## Indicate where to find Perl modules such as util_daikon.pm.
if [ $PERLLIB ]; then
  export PERLLIB=${DAIKONBIN}:${PERLLIB}
else
  export PERLLIB=${DAIKONBIN}
fi

## Indicates where Lackwit can find its libraries (and binaries).
export LACKWIT_HOME=$DFECDIR/lackwit
