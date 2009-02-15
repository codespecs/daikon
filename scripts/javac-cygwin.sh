#!/bin/bash
#
# A wrapper for calling javac from Cygwin
# Tries to modify any arguments which are unix style paths to
# windows style paths.  This includes any arguments to classpath or sourcepath
# or any arguments that begin with /
#

ME="`basename $0`"
JAVAC_EXEC="$JDKDIR/bin/javac"
ARGS=""

while [ -n "$1" ]; do
   arg="$1"
   shift
   case "$arg" in
      -cp | -classpath)
         arg="$arg' '`cygpath -p -w "$1"`"
         shift
         ;;
      -sourcepath)
         arg="$arg' '`cygpath -p -w "$1"`"
         shift
         ;;
      /*)
         arg="`cygpath -p -w "$arg"`"
         ;;
   esac
   ARGS="$ARGS '$arg'"
done

eval "set -- $ARGS"
#echo "$JAVAC_EXEC" "$@"
exec "$JAVAC_EXEC" "$@"
