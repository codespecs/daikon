@ECHO OFF

REM daikonenv.bat
REM Set up environment variables to run Daikon in a Windows NT command window.
REM (This file should be kept in synch with daikon.bashrc.)

REM Wherever you source this file, you should set two environment variables:
REM   JAVA_HOME      absolute pathname of the directory containing the JDK
REM Optionally, you may set the following environment variables:
REM   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
REM        files, instead of the default, which is to use daikon.jar.  This is
REM        useful if you have made changes to Daikon and compiled the .java
REM        files to .class files but have not re-made the daikon.jar file.
REM You should not need to edit this file.

if "%JAVA_HOME%"=="" (
  echo JAVA_HOME environment variable is not set.
  echo Please fix this before proceeding.  Aborting daikonenv.bat .
  exit /b 2
) else (
  if not exist "%JAVA_HOME%" (
    echo JAVA_HOME is set to non-existent directory: %JAVA_HOME%
    echo Please fix this before proceeding.  Aborting daikonenv.bat .
    exit /b 2
  )
)

SET scriptpath=%~dp0
SET DAIKONDIR=%mypath:~0,-1%

if "$DAIKONSCRIPTS"=="" (
  if exist "%DAIKONDIR%\scripts" (
    set DAIKONSCRIPTS=%DAIKONDIR%\scripts
  ) else (
    echo Cannot choose a value for environment variable DAIKONSCRIPTS.
    echo Please fix this before proceeding.  Aborting daikonenv.bat .
    exit /b 2
  )
)

if "$PLUMESCRIPTS"=="" (
  set PLUMESCRIPTS=%DAIKONDIR%\utils\plume-scripts
)

REM set DAIKONCLASS_SOURCES=1

REM For Windows, adjacent semicolons in CLASSPATH are harmless, but keep
REM the CPADD logic for parallelism with daikon.bashrc.
if not "%DAIKONCLASS_SOURCES%"=="" (
  set CPADD=%DAIKONDIR%\java
) else (
  set CPADD=%DAIKONDIR%\daikon.jar
)
if not "%CLASSPATH%"=="" (
  set CLASSPATH=%CPADD%;%CLASSPATH%
) else (
  set CLASSPATH=%CPADD%
)

REM tools.jar must be on your classpath.
set CLASSPATH=%CLASSPATH%;%JAVA_HOME%\jre\lib\rt.jar;%JAVA_HOME%\lib\tools.jar

REM Add the Daikon binaries to your path
set PATH=%DAIKONSCRIPTS%;%DAIKONDIR%\front-end\java\src;%JAVA_HOME%\bin;%PATH%

REM Indicate where to find Perl modules such as util_daikon.pm.
if not "%PERLLIB%"=="" (
  set PERLLIB=%DAIKONSCRIPTS%;%PLUMESCRIPTS%;%PERLLIB%
) else (
  set PERLLIB=%DAIKONSCRIPTS%;%PLUMESCRIPTS%
)
