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

# process useful arguments (VAR=VALUE args are exported)
args=
for arg in $*; do
  arg=`echo $arg | egrep "help|quiet|verbose|version|prefix|target|srcdir"`
  arg=`echo $arg | sed 's,-*help=[^[:space:]]*,--help,g'`
  args="$args $arg"
done

# GCOPTS and ENVOPTS will be used instead of CFLAGS.
# Init OPTIMIZER to avoid defaults.
CFLAGS=${CFLAGS:--O}
GCOPTS="$CFLAGS $CPPFLAGS"
ENVOPTS="$LDFLAGS"
OPTIMIZER=

### These tweaks were used for libtiff 3.5.7
# Additional settings for Cygwin/MinGW (i.e. -mno-cygwin).
# They are related to tif_jpeg.c which does some "wrong" assumptions.
# - XMD_H prevents defining INT32 in jpeg/jmorecfg.h (called in jpeglib.h).
# - The file says "The windows RPCNDR.H file defines boolean", which is
#   apparently not the case in cygwin/mingw, resulting in undefined
#   boolean typedef.  So I use a preprocessor #define for it.
#if ! test -z "`echo $* | grep 'cygwin' | egrep 'out|no'`"; then
#  ENVOPTS="$ENVOPTS -DXMD_H -Dboolean=int"
#fi

# zlib directory relative to tiff/libtiff/
if test "$INCZ"; then
  case $srcdir in
    [\/]* | ?:[\/]* )  # absolute path
    zlibdir=$srcdir/../zlib
    ;;
    *) # relative path
    zlibdir=../$srcdir/../zlib
    ;;
  esac
  ENVOPTS="-I$zlibdir $ENVOPTS"
fi

# jpeg directory relative to tiff/libtiff/
if test "$INCJPEG"; then
  case $srcdir in
    [\/]* | ?:[\/]* )  # absolute path
    jpegdir=$srcdir/../jpeg
    ;;
    *) # relative path
    jpegdir=../$srcdir/../jpeg
    ;;
  esac
  ENVOPTS="-I$jpegdir $ENVOPTS"
fi

# Variables must be set before calling configure (old-fashion configure).
export GCOPTS ENVOPTS OPTIMIZER

# run configure
case "$args" in
  *help*)
  echo "Configuration of libtiff 3.6.1:"
  echo ""
  ;;

  *)
  args="--noninteractive --with-DSO=no $args"
  echo "configure.gnu: configuring libtiff 3.6.1"
  echo "configure.gnu: running $srcdir/configure $args"
esac
$srcdir/configure $args
echo ""

# ensure all files are read-writeable
chmod -R u+rw *

# post-process libtiff/Makefile
if test -f libtiff/Makefile; then
  echo "configure.gnu: editing libtiff/Makefile"

  # libtiff/mkg3states requires port/libport.a on systems without getopt
  if test -f port/Makefile; then
    if test "`grep getopt port/Makefile`"; then
      sed 's,\(-o mkg3states\),\1 ../port/libport.a,g' libtiff/Makefile > libtiff/Makefile.tmp
      mv libtiff/Makefile.tmp libtiff/Makefile
    fi
  else
    echo "all clean:" > port/Makefile
  fi

  # do not create tiffvers.h in srcdir
  sed 's,^tiffvers\.h:.*,, ; s,${SRCDIR}/tiffvers\.h,tiffvers.h,g' libtiff/Makefile > libtiff/Makefile.tmp
  mv libtiff/Makefile.tmp libtiff/Makefile
fi
echo ""
