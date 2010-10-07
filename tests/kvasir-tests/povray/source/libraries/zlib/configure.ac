##############################################################################
#               configure.ac
#
# Process this file with autoconf to produce configure
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

# configure.ac for the source distribution of POV-Ray 3.6 for UNIX
# Written by Nicolas Calimet <pov4grasp@free.fr>

AC_PREREQ(2.57)
AC_INIT(zlib, 1.2.1)
AC_CONFIG_SRCDIR([inflate.c])
AC_CONFIG_HEADER([config.h])

AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AM_INIT_AUTOMAKE([1.7 no-dependencies])
AM_MAINTAINER_MODE

# Checks for programs.
# Inits compiler flags with -O if none given.
CFLAGS=${CFLAGS:--O}
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_RANLIB

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h malloc.h memory.h stddef.h stdlib.h string.h unistd.h utime.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_MMAP
AC_FUNC_STAT
AC_FUNC_UTIME_NULL
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memset mkdir munmap strdup strerror strrchr utime])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
