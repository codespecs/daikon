@echo off
REM Windows users can run this batch file to execute the modbit-munge.pl
REM Perl script.  UNIX users do not need to use this file at all.

perl -S -wpi.bak modbit-munge.pl %1
