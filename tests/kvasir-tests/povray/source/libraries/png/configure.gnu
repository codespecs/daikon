#!/bin/sh
##############################################################################
#               configure.gnu
#
# Wrapper to configure
#
# from Persistence of Vision(tm) Ray Tracer version 3.6.
# Copyright 1991-2003 Persistence of Vision Team
# Copyright 2003-2004 Persistence of Vision Raytracer Pty. Ltd.
#---------------------------------------------------------------------------
# NOTICE: This source code file is provided so that users may experiment
# with enhancements to POV-Ray and to port the software to platforms other
# than those supported by the POV-Ray developers. There are strict rules
# regarding how you are permitted to use this file. These rules are contained
# in the distribution and derivative versions licenses which should have been
# provided with this file.
#
# These licences may be found online, linked from the end-user license
# agreement that is located at http://www.povray.org/povlegal.html
#---------------------------------------------------------------------------
# This program is based on the popular DKB raytracer version 2.12.
# DKBTrace was originally written by David K. Buck.
# DKBTrace Ver 2.0-2.12 were written by David K. Buck & Aaron A. Collins.
##############################################################################

# configure.gnu for the source distribution of POV-Ray 3.6 for UNIX
# Written by Nicolas Calimet <pov4grasp@free.fr>

# get srcdir
srcdir=`echo $* | grep srcdir | sed 's,.*srcdir=\([^[:space:]]*\).*,\1,g'`
if test -z "$srcdir"; then
  srcdir="."
fi

# get cache-file
cachefile=`echo $* | grep cache-file | sed 's,.*cache-file=\([^[:space:]]*\).*,\1,g'`

# Setting MISSING can prevent configure error with cygwin and
# directories containing special caracters (such as spaces or quotes).
MISSING="\${SHELL} $srcdir/missing"
export MISSING

# Process arguments (remove VAR=VALUE args, which are exported).
# cache-file and srcdir were the last arguments, so remove all what follows
# the first VAR argument.
args=
for arg in $*; do
  arg=`echo $arg | sed 's,^[^-].*,,'`
  if test -z "$arg"; then break; fi
  args="$args $arg"
done
if test -z "`echo $args | grep cache-file`"; then
  args="--cache-file=$cachefile $args"
fi
if test -z "`echo $args | grep srcdir`"; then
  args="--srcdir=$srcdir $args"
fi

# zlib directory relative to png/
if test "$INCZ"; then
  zlibdir=$srcdir/../zlib
  CPPFLAGS="-I$zlibdir $CPPFLAGS"
fi
export CPPFLAGS

# run configure
case "$args" in
  *help*)
  ;;

  *)
  echo "configure.gnu: configuring libpng 1.2.5"
  echo "configure.gnu: running $srcdir/configure $args"
  ;;
esac
$srcdir/configure $args
echo ""
