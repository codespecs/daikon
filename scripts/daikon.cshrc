## daikon.cshrc
## Daikon initialization file for C shell (csh and tcsh) users.
## (This file should be kept in synch with daikon.bashrc and daikonenv.bat.)

## Wherever you source this file, you should set two environment variables:
##   DAIKONPARENT   absolute pathname of the directory containing "daikon/"
##   JDKDIR         absolute pathname of the directory containing the JDK
## Optionally, you may set the following environment variables:
##   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
##        files, instead of the default, which is to use daikon.jar.
## You should not need to edit this file directly.


if (! $?JDKDIR) setenv JDKDIR /directory/containing/jdk
if (! $?DAIKONPARENT) setenv DAIKONPARENT /path/to/parent/of/daikon
if (! $?DAIKONDIR) setenv DAIKONDIR ${DAIKONPARENT}/daikon
if (! $?DFECDIR) setenv DFECDIR ${DAIKONDIR}/front-end/c
if (! $?DAIKONBIN) setenv DAIKONBIN ${DAIKONDIR}/bin

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

## tools.jar must be on your classpath.  Also, if you wish to use dfej (the
## Daikon front end for Java), rt.jar must be on your classpath.
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

## (ajax.jar is temporary, will be removed soon, we hope.)
if (-e ${DAIKONDIR}/java/ajax-ship/ajax.jar) then
  setenv CLASSPATH ${CLASSPATH}:${DAIKONDIR}/java/ajax-ship/ajax.jar
endif

if ($?debuglogin) echo "daikon.cshrc about to set path"

## Add the Daikon binaries to your path
set path = (${DAIKONBIN} ${DAIKONDIR}/front-end/java/src $DFECDIR $JDKDIR/bin $path)

## Indicate where to find Perl modules such as util_daikon.pm.
if ($?PERLLIB) then
  setenv PERLLIB ${DAIKONBIN}:${PERLLIB}
else
  setenv PERLLIB ${DAIKONBIN}
endif

## Indicates where Ajax should find its helper files such as
## main-harness.csal, tweaked-classes.zip, etc.  Given a Java program, Ajax
## determines which variables can be sensibly compared to one another.
setenv AJAX_DIR ${DAIKONDIR}/java/ajax-ship

## Indicates where Lackwit can find its libraries (and binaries).
setenv LACKWIT_HOME $DFECDIR/lackwit
