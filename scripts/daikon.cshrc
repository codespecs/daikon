## daikon.cshrc
## Daikon initialization file for C shell (csh and tcsh) users.
## (This file should be kept in synch with daikon.bashrc and daikonenv.bat.)

## Wherever you source this file, you should set two environment variables:
##   JAVA_HOME      absolute pathname of the directory containing the JDK
##                  (or "none" if you don't have it)
## Optionally, you may set the following environment variables:
##   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
##        files, instead of the default, which is to use daikon.jar.  This is
##        useful if you have made changes to Daikon and compiled the .java
##        files to .class files but have not re-made the daikon.jar file.
## You should not need to edit this file.

if (! $?JAVA_HOME) then
  echo "JAVA_HOME environment variable is not set."
  echo "Please fix this before proceeding.  Aborting daikon.cshrc ."
  exit 2
else if (! -d $JAVA_HOME && $JAVA_HOME != "none") then
  echo "JAVA_HOME is set to non-existent directory: $JAVA_HOME"
  echo "Please fix this before proceeding.  Aborting daikon.cshrc ."
  exit 2
endif

scriptdir=`/bin/dirname $0`       # may be relative path
DAIKONDIR=`cd $scriptdir/.. && pwd`    # ensure absolute path

if (! $?DAIKONBIN) then
  if ( -d ${DAIKONDIR}/bin ) then
    setenv DAIKONBIN ${DAIKONDIR}/bin
  else if ( -d ${DAIKONDIR}/scripts ) then
    setenv DAIKONBIN ${DAIKONDIR}/scripts
  else
    echo "Cannot choose a value for environment variable DAIKONBIN."
    echo "Please fix this before proceeding.  Aborting daikon.cshrc ."
    exit 2
  endif
endif

if (! $?PLUMEBIN) then
  setenv PLUMEBIN ${DAIKONDIR}/utils/plume-lib/bin
endif

# setenv DAIKONCLASS_SOURCES 1

setenv CPADD ${DAIKONDIR}/daikon.jar
# In csh, can't use "&&" here to protect the use of the variable; use two "if"s
if ($?DAIKONCLASS_SOURCES) then
  if ($DAIKONCLASS_SOURCES) then
    setenv CPADD ${DAIKONDIR}/java
  endif
endif
if ($?CLASSPATH) then
  setenv CLASSPATH ${CPADD}:${CLASSPATH}
else
  if ( $?prompt != 0 ) echo "Warning: daikon.cshrc is setting CLASSPATH, which was previously unset"
  setenv CLASSPATH ${CPADD}
endif

## tools.jar must be on your classpath.
setenv darwinos 0
if ($?OSTYPE) then
  if ("$OSTYPE" == "darwin") then
    setenv darwinos 1
  endif
endif

if ($JAVA_HOME != "none") then
  if (! $darwinos) then
    if ($?debuglogin) echo "daikon.cshrc about to set classpath (non-Darwin)"
    if ($?debuglogin) echo "CLASSPATH: $CLASSPATH"
    setenv CLASSPATH ${CLASSPATH}:${JAVA_HOME}/jre/lib/rt.jar:${JAVA_HOME}/lib/tools.jar
  else
    if ($?debuglogin) echo "daikon.cshrc about to set classpath (Darwin)"
    ## For Macintosh MacOSX users.  (This list
    ## is the system property "sun.boot.class.path".)
    setenv CLASSPATH ${CLASSPATH}:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/classes.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/ui.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/i18n.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/sunrsasign.jar
  endif

  ## Add the requested JDK's binaries to the front of the path
  set path = ($JAVA_HOME/bin $path)
endif

if ($?debuglogin) echo "daikon.cshrc about to add scripts to path"

## Add the Daikon binaries to your path
set path = (${DAIKONBIN} ${PLUMEBIN} $path)

## Indicate where to find Perl modules such as util_daikon.pm.
if ($?PERLLIB) then
  setenv PERLLIB ${DAIKONBIN}:${PLUMEBIN}:${PERLLIB}
else
  setenv PERLLIB ${DAIKONBIN}:${PLUMEBIN}
endif
