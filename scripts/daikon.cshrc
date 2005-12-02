## daikon.cshrc
## Daikon initialization file for C shell (csh and tcsh) users.
## (This file should be kept in synch with daikon.bashrc and daikonenv.bat.)

## Wherever you source this file, you should set two environment variables:
##   DAIKONDIR      absolute pathname of the "daikon" directory
##   JDKDIR         absolute pathname of the directory containing the JDK
## Optionally, you may set the following environment variables:
##   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
##        files, instead of the default, which is to use daikon.jar.
## You should not need to edit this file directly.

if (! $?JDKDIR) then
  echo "daikon.cshrc: JDKDIR environment variable is not set"
  return 2
else if (! -d $JDKDIR); then
  echo "daikon.cshrc: JDKDIR is set to non-existent directory $JDKDIR"
  return 2
fi

if (! $?DAIKONDIR) setenv DAIKONDIR ${DAIKONPARENT}/daikon
if (! $?DAIKONDIR) then
  echo "daikon.cshrc: DAIKONDIR environment variable is not set"
  return 2
else if (! -d $DAIKONDIR); then
  echo "daikon.cshrc: DAIKONDIR is set to non-existent directory $JDKDIR"
  return 2
fi

if (! $?DAIKONBIN) then
  if ( -d ${DAIKONDIR}/bin ) then
    setenv DAIKONBIN ${DAIKONDIR}/bin
  else if ( -d ${DAIKONDIR}/scripts ) then
    setenv DAIKONBIN ${DAIKONDIR}/scripts
  else
    echo "daikon.cshrc: Cannot set DAIKONBIN"
    return 2
  endif
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
  setenv CLASSPATH ${CPADD}
endif

## tools.jar must be on your classpath.
setenv darwinos 0
if ($?OSTYPE) then
  if ("$OSTYPE" != "darwin") then
    setenv darwinos 1
  endif
endif
if (! $darwinos) then
  if ($?debuglogin) echo "daikon.cshrc about to set classpath (#1)"
  if ($?debuglogin) echo "CLASSPATH: $CLASSPATH"
  setenv CLASSPATH ${CLASSPATH}:${JDKDIR}/jre/lib/rt.jar:${JDKDIR}/lib/tools.jar
else
  ## For Macintosh MacOSX users.  (This list
  ## is the system property "sun.boot.class.path".)
  setenv CLASSPATH ${CLASSPATH}:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/classes.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/ui.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/i18n.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/sunrsasign.jar
endif

if ($?debuglogin) echo "daikon.cshrc about to set classpath"

if ($?debuglogin) echo "daikon.cshrc about to set path"

## Add the Daikon binaries to your path
set path = (${DAIKONBIN} $JDKDIR/bin $path)

## Indicate where to find Perl modules such as util_daikon.pm.
if ($?PERLLIB) then
  setenv PERLLIB ${DAIKONBIN}:${PERLLIB}
else
  setenv PERLLIB ${DAIKONBIN}
endif
