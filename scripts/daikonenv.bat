REM daikonenv.bat
REM Set up environment variables to run Daikon in a Windows NT command window.
REM (This file should be kept in synch with daikon.bashrc and daikon.cshrc.)

echo off
REM Set DAIKONPARENT to absolute pathname of the directory containing "daikon\".
set DAIKONPARENT=d:\Daikon
set DAIKONDIR=%DAIKONPARENT%\Daikon
set DFECDIR=%DAIKONDIR%\front-end\c
set DAIKONBIN=%DAIKONDIR%\bin

REM Set this directory to the directory containing the JDK.
set JDKDIR=d:\j2sdk1.4.0

REM Set DAIKONCLASS_SOURCES if you want to run Daikon from .class files that
REM you compile yourself.  This permits you to modify Daikon (most users
REM will not need to do so).  If you do not set DAIKONCLASS_SOURCES, you will
REM run Daikon from the precompiled bytecode files in daikon.jar.
REM set DAIKONCLASS_SOURCES=1

REM For Windows, adjacent semicolons in CLASSPATH are harmless, but keep
REM the CPADD logic for parallelism with daikon.bashrc and daikon.cshrc.
if defined %DAIKONCLASS_SOURCES% (
  set CPADD=%DAIKONDIR%\java
) else (
  set CPADD=%DAIKONDIR%\daikon.jar
)
if defined %CLASSPATH% (
  set CLASSPATH=%CPADD%;%CLASSPATH%
) else (
  set CLASSPATH=%CPADD%
)

REM tools.jar must be on your classpath.  Also, if you wish to use dfej (the
REM Daikon front end for Java), rt.jar must be on your classpath.
set CLASSPATH=%CLASSPATH%;%JDKDIR%\jre\lib\rt.jar;%JDKDIR%\lib\tools.jar

REM (ajax.jar is temporary, will be removed soon, we hope.)
if exist %DAIKONDIR%\java\ajax-ship\ajax.jar (
  set CLASSPATH=%CLASSPATH%;%DAIKONDIR%\java\ajax-ship\ajax.jar
)

REM Add the Daikon binaries to your path
set PATH=%DAIKONBIN%;%DAIKONDIR%\front-end\java\src;%DFECDIR%;%JDKDIR%\bin;%PATH%

REM Indicate where to find Perl modules such as util_daikon.pm.
if defined %PERLLIB% (
  set PERLLIB=%DAIKONBIN%;%PERLLIB%
) else (
  set PERLLIB=%DAIKONBIN%
)

REM Indicates where Ajax should find its helper files such as
REM main-harness.csal, tweaked-classes.zip, etc.  Given a Java program, Ajax
REM determines which variables can be sensibly compared to one another.
set AJAX_DIR=%DAIKONDIR%\java\ajax-ship

REM Indicates where Lackwit can find its libraries (and binaries).
set LACKWIT_HOME=%DFECDIR%\lackwit
