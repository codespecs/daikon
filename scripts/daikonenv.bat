echo off

REM daikonenv.bat
REM Set up environment variables to run Daikon in a Windows NT command window.
REM (This file should be kept in synch with daikon.bashrc and daikon.cshrc.)

REM Wherever you source this file, you should set two environment variables:
REM   DAIKONPARENT   absolute pathname of the directory containing "daikon/"
REM   JDKDIR         absolute pathname of the directory containing the JDK
REM Optionally, you may set the following environment variables:
REM   DAIKONCLASS_SOURCES   to any value, if you want to run Daikon from .class
REM        files, instead of the default, which is to use daikon.jar.
REM You should not need to edit this file directly.


REM These are examples of the customization you might put where you source this file.
REM set DAIKONPARENT=c:\Daikon
REM set JDKDIR=C:\Program Files\Java\jdk1.5.0_05

set DAIKONDIR=%DAIKONPARENT%\Daikon
set DFECDIR=%DAIKONDIR%\front-end\c
set DAIKONBIN=%DAIKONDIR%\bin

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

REM tools.jar must be on your classpath.
set CLASSPATH=%CLASSPATH%;%JDKDIR%\jre\lib\rt.jar;%JDKDIR%\lib\tools.jar

REM Add the Daikon binaries to your path
set PATH=%DAIKONBIN%;%DFECDIR%;%JDKDIR%\bin;%PATH%

REM Indicate where to find Perl modules such as util_daikon.pm.
if defined %PERLLIB% (
  set PERLLIB=%DAIKONBIN%;%PERLLIB%
) else (
  set PERLLIB=%DAIKONBIN%
)

REM Indicates where Lackwit can find its libraries (and binaries).
set LACKWIT_HOME=%DFECDIR%\lackwit
