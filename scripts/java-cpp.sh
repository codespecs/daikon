#!/bin/sh
# sh version of java-cpp (which see)
# Josh Kataoka
# Time-stamp: <2000-5-2 mernst>

# If first argument is a file, it is used as input.  Otherwise, input comes
# from standard in.  Output goes to standard out.
# (This order of arguments is gross, but I don't see how to do a pop (shift
# from the back) of $argv.

# Problem:  this errs if there is an unmatched ' in comments.

# I'm not calling this jpp because someone else has probably already taken
# that name.
# This is not a shell alias so it's sure to work in Makefiles, scripts, etc.

# Problem:  quote marks in comments can cause "unterminated character
# constant" warnings.  I don't use the -C (leave comments in) flag to cpp,
# or make DOUBLESLASHCOMMENT put the rest of the line in a string, because
# I do want substitution to occur in comments.
# Workaround:  don't have single quotes in comments.

if [ -f $1 ]
then
    filearg=$1
    shift
else
    filearg=""
fi

for arg do
    argv="${argv} ${arg}"
done

# echo filearg $filearg
# echo argv $argv

perl -p -e 's/\/\//DOUBLESLASHCOMMENT/g;' -e 's/\/\*/SLASHSTARCOMMENT/g;' -e 's/\'/SINGLEQUOTE/g;' $filearg > /tmp/java-cpp-$$-input
cpp $argv /tmp/java-cpp-$$-input > /tmp/java-cpp-$$-output
cat /tmp/java-cpp-$$-output | perl -p -e 's/DOUBLESLASHCOMMENT/\/\//g;' -e 's/SLASHSTARCOMMENT/\/\*/g;' -e 's/SINGLEQUOTE/\'/g;' -e 's/"  ?\+ "//g;' -e 's/^(package .*\.) ([^ ]*) ?;/\1\2;/;' -e 's/^# [0-9]+ ".*$//;' | perl -p -e 'use English; $INPUT_RECORD_SEPARATOR = "";' | lines-from "package"
# Problem:  doesn't propagate error codes correctly
rm /tmp/java-cpp-$$-input /tmp/java-cpp-$$-output
