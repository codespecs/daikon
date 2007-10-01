#!/bin/bash
#
# A wrapper for calling Java from Cygwin
# Tries to modify any arguments which are unix style paths to
# windows style paths.  This includes any arguments to classpath or 
# Xbootclasspath or any arguments that begin with /
#

ME="`basename $0`"
JAVA_EXEC="$JDKDIR/bin/java"
ARGS=""

while [ -n "$1" ]; do
   arg="$1"
   shift
   case "$arg" in
      -cp | -classpath)
         arg="$arg' '`cygpath -p -w "$1"`"
         shift
         ;;
      -Xbootclasspath*:*)
         arg="${arg%%:*}:`cygpath -p -w "${arg#*:}"`"
         ;;
      /*)
         arg="`cygpath -p -w "$arg"`"
         ;;
   esac
   ARGS="$ARGS '$arg'"
done

eval "set -- $ARGS"
#echo "$JAVA_EXEC" "$@"
exec "$JAVA_EXEC" "$@"
