## daikon.cshrc
## Daikon initialization file for C shell (csh and tcsh) users.
## (This file should be kept in synch with daikon.bashrc and daikonenv.bat.)

## Set DAIKONPARENT to absolute pathname of the directory containing "daikon/".
if (! $?DAIKONPARENT) setenv DAIKONPARENT /path/to/parent/of/daikon
if (! $?DAIKONDIR) setenv DAIKONDIR ${DAIKONPARENT}/daikon
if (! $?DFECDIR) setenv DFECDIR ${DAIKONDIR}/front-end/c
if (! $?DAIKONBIN) setenv DAIKONBIN ${DAIKONDIR}/bin

## Set this directory to the directory containing the JDK.
if (! $?JDKDIR) setenv JDKDIR /g2/jdk

## Set DAIKONCLASS_SOURCES if you want to run Daikon from .class files that
## you compile yourself.  This permits you to modify Daikon (most users
## will not need to do so).  If you do not set DAIKONCLASS_SOURCES, you will
## run Daikon from the precompiled bytecode files in daikon.jar.
# setenv DAIKONCLASS_SOURCES 1

setenv CPADD ${DAIKONDIR}/daikon.jar
# In csh, can't use "&&" here to protect the use of the variable; use two "if"s
if ($?DAIKONCLASS_SOURCES) then
  if ($DAIKONCLASS_SOURCES) then
    setenv CPADD ${DAIKONDIR}/java
  endif
endif
endif
if ($?CLASSPATH) then
  setenv CLASSPATH ${CPADD}:${CLASSPATH}
else
  setenv CLASSPATH ${CPADD}
endif

## tools.jar must be on your classpath.  Also, if you wish to use dfej (the
## Daikon front end for Java), rt.jar must be on your classpath.
if ("$OSTYPE" != "darwin") then
  setenv CLASSPATH ${CLASSPATH}:${JDKDIR}/jre/lib/rt.jar:${JDKDIR}/lib/tools.jar
else
  ## For Macintosh MacOSX users.  (This list
  ## is the system property "sun.boot.class.path".)
  setenv CLASSPATH ${CLASSPATH}:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/classes.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/ui.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/i18n.jar:/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/Classes/sunrsasign.jar
endif

## (ajax.jar is temporary, will be removed soon, we hope.)
if (-e ${DAIKONDIR}/java/ajax-ship/ajax.jar) then
  setenv CLASSPATH ${CLASSPATH}:${DAIKONDIR}/java/ajax-ship/ajax.jar
endif

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
