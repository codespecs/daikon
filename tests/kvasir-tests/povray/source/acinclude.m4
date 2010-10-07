##############################################################################
#               acinclude.m4
#
# Additional macros for autoconf.
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
#---------------------------------------------------------------------------
# $File: //depot/povray/3.6-release/unix/acinclude.m4 $
# $Revision: #3 $
# $Change: 3032 $
# $DateTime: 2004/08/02 18:43:41 $
# $Author: chrisc $
# $Log$
##############################################################################

# acinclude.m4 for the source distribution of POV-Ray 3.6 for UNIX
# Written by Nicolas Calimet <pov4grasp@free.fr>


# POV_ARG_ENABLE(FEATURE, HELP-STRING)
# --------------
# Basic replacement for AC_ARG_ENABLE().
# FEATURE must include the '--enable' or '--disable' prefix.
#
AC_DEFUN([POV_ARG_ENABLE],
[
  # remove enable|disable prefix and convert dash to underscore
  pov_arg_enable_feature=`echo $1 | sed 's,^.*able-,,; s,-,_,g'`

  # AC_ARG_ENABLE does only accept a litteral for feature name / help string
  eval "enable_povvar=\${enable_$pov_arg_enable_feature}"
  AC_ARG_ENABLE([povvar],
    AC_HELP_STRING([$1], [$2]),
    [case $enableval in
       yes|no|"") eval "enable_${pov_arg_enable_feature}=$enableval" ;;
       *) AC_MSG_ERROR([bad value '$enableval' for $1]) ;;
     esac
    ],
    [eval "enable_${pov_arg_enable_feature}="]
  )
])


# POV_ARG_WITH(PACKAGE, HELP-STRING)
# ------------
# Basic replacement for AC_ARG_WITH().
# PACKAGE must include the '--with' or '--without' prefix.
#
AC_DEFUN([POV_ARG_WITH],
[
  # remove with(out) prefix and convert dash to underscore
  pov_arg_with_package=`echo $1 | sed 's,^.*with.*-,,; s,-,_,g'`

  # AC_ARG_WITH does only accept a litteral for package name / help string
  eval "with_povvar=\${with_$pov_arg_with_package}"
  AC_ARG_WITH([povvar],
    AC_HELP_STRING([$1], [$2]),
    [case $withval in
       yes|no|"") eval "with_${pov_arg_with_package}=$withval" ;;
       *) AC_MSG_ERROR([bad value '$withval' for $1]) ;;
     esac
    ],
    [eval "with_${pov_arg_with_package}="]
  )
])


# POV_PROG_CXX_VERSION()
# --------------------
# Try to determine the compiler and its version.
#
AC_DEFUN([POV_PROG_CXX_VERSION],
[
  AC_REQUIRE([AC_PROG_EGREP])

  AC_PROG_CXX
  AC_PROG_CXXCPP
  AC_MSG_CHECKING([for $CXX version])

  # check for -dumpversion
  AC_TRY_COMMAND([$CXX -dumpversion < /dev/null 2> /dev/null > conftest.out])
  pov_prog_cxx_version=`cat conftest.out | $ac_cv_prog_egrep '[[0-9]]'`

  # check for -v
  if test -z "$pov_prog_cxx_version"; then
    AC_TRY_COMMAND([$CXX -v < /dev/null 2>&1 | tr A-Z a-z > conftest.out])
    pov_prog_cxx_version=`cat conftest.out | $ac_cv_prog_egrep "version" | sed 's,\(.*version[[[:space:]]]*\)\(.*\),\2,'`
  fi

  # final compiler version string
  if test -z "$pov_prog_cxx_version"; then
    AC_MSG_RESULT([unkown])
    pov_prog_cxx_version="$CXX"
  else
    AC_MSG_RESULT([$pov_prog_cxx_version])
    pov_prog_cxx_version="$CXX $pov_prog_cxx_version"
  fi

  rm -f conftest.out
])


# POV_PROG_CXX_FLAGS(FLAGS, [ACTION-IF-WORKS], [ACTION-IF-FAILS])
# ------------------
# Check whether CXX compiler supports a given set of flags, cache result,
# and update CXXFLAGS.
# Note: in principle I'd better use AC_COMPILE_IFELSE([AC_LANG_PROGRAM()])
# but I don't remember whether it works well when using multiple flags.
# I find it also safer to inspect stderr using pov_prog_cxx_flags_regexp.
# For safety, the compiler is tested with and without the tested flags
# and the corresponding standard error outputs are compared.
#
AC_DEFUN([POV_PROG_CXX_FLAGS],
[
  AC_REQUIRE([AC_PROG_EGREP])

  # Create a unique cache-id name (multiple flags are handled).
  pov_prog_cxx_flags_var=pov_cv_prog_cxx_flags`echo $1 | sed 's,[[^a-zA-Z0-9]],_,g'`

  # Create the extended regular expression to handle multiple flags.
  # For instance, "-first-flag -second-flag -X" gives the regexp:
  #   "\-f|irst-flag|\-s|econd-flag|\-X|X"
  #
  # FreeBSD and Darwin seem to have a problem with the \+ sed construct
  # (but apparently not with the \{x,y\} one).  For safety, I prefer to
  # use  [[:space:]][[:space:]]*  for  [[:space:]]\+
  pov_prog_cxx_flags_regexp=`echo $1 | sed 's,\(-.\)\([[^[:space:]]]*\),\\\\\1|\2,g; s,[[[:space:]]][[[:space:]]]*,|,g; s,\(.\)||,\1|\1|,g; s,\(.\)|$,\1|\1,'`
 
  # Cannot use AC_CACHE_CHECK due to the nature of the cache-id variable.
  AC_MSG_CHECKING([whether $CXX accepts $1])
  AC_CACHE_VAL(
    [$pov_prog_cxx_flags_var],
    [
      # Create a conftest file for CXX.
      AC_LANG_PUSH(C++)
      AC_LANG_CONFTEST([AC_LANG_PROGRAM()])

      # Compile with $CXX and inspect standard error for given flags.
      AC_TRY_COMMAND([$CXX -c conftest.$ac_ext > /dev/null 2> conftest.err0])
      AC_TRY_COMMAND([$CXX -c $1 conftest.$ac_ext > /dev/null 2> conftest.err])
      AC_TRY_COMMAND([cat conftest.err >&5])
      AC_TRY_COMMAND([$ac_cv_prog_egrep \"$pov_prog_cxx_flags_regexp\" conftest.err 2>&1 > /dev/null])
      # the command above seems to not always return the correct status, so
      # for safety I rerun it here; I keep the other one for logging purpose.
      pov_prog_cxx_flags_err=`$ac_cv_prog_egrep "$pov_prog_cxx_flags_regexp" conftest.err`
      AC_TRY_COMMAND([diff conftest.err0 conftest.err >&5])
      if test -z "$pov_prog_cxx_flags_err" \
      || test -z "`diff conftest.err0 conftest.err`"; then
        eval "$pov_prog_cxx_flags_var=yes"
      else
        eval "$pov_prog_cxx_flags_var=no"
      fi

      rm -f conftest.$ac_ext conftest.$ac_objext conftest.err conftest.err0
      AC_LANG_POP(C++)
    ]
  )
  eval "pov_prog_cxx_flags_value=\$$pov_prog_cxx_flags_var"
  AC_MSG_RESULT([$pov_prog_cxx_flags_value])

  # Update CXXFLAGS when flags are working, and run provided actions.
  if test x"$pov_prog_cxx_flags_value" = x"yes"; then
    CXXFLAGS="$CXXFLAGS $1"
    ifelse([$2],[],[:],[$2])
  else
    ifelse([$3],[],[:],[$3])
  fi
])


# POV_PROG_CC_FLAGS(FLAGS, [ACTION-IF-WORKS], [ACTION-IF-FAILS])
# -----------------
# CC version of the macro above.
#
AC_DEFUN([POV_PROG_CC_FLAGS],
[
  AC_REQUIRE([AC_PROG_EGREP])

  pov_prog_cc_flags_var=pov_cv_prog_cc_flags`echo $1 | sed 's,[[^a-zA-Z0-9]],_,g'`
  pov_prog_cc_flags_regexp=`echo $1 | sed 's,\(-.\)\([[^[:space:]]]*\),\\\\\1|\2,g; s,[[[:space:]]][[[:space:]]]*,|,g; s,\(.\)||,\1|\1|,g; s,\(.\)|$,\1|\1,'`

  AC_MSG_CHECKING([whether $CC accepts $1])
  AC_CACHE_VAL(
    [$pov_prog_cc_flags_var],
    [
      AC_LANG_PUSH(C)
      AC_LANG_CONFTEST([AC_LANG_PROGRAM()])
      AC_TRY_COMMAND([$CC -c conftest.$ac_ext > /dev/null 2> conftest.err0])
      AC_TRY_COMMAND([$CC -c $1 conftest.$ac_ext > /dev/null 2> conftest.err])
      AC_TRY_COMMAND([cat conftest.err >&5])
      AC_TRY_COMMAND([$ac_cv_prog_egrep \"$pov_prog_cc_flags_regexp\" conftest.err 2>&1 > /dev/null])
      pov_prog_cc_flags_err=`$ac_cv_prog_egrep "$pov_prog_cc_flags_regexp" conftest.err`
      AC_TRY_COMMAND([diff conftest.err0 conftest.err >&5])
      if test -z "$pov_prog_cc_flags_err" \
      || test -z "`diff conftest.err0 conftest.err`"; then
        eval "$pov_prog_cc_flags_var=yes"
      else
        eval "$pov_prog_cc_flags_var=no"
      fi

      rm -f conftest.$ac_ext conftest.$ac_objext conftest.err conftest.err0
      AC_LANG_POP(C)
    ]
  )
  eval "pov_prog_cc_flags_value=\$$pov_prog_cc_flags_var"
  AC_MSG_RESULT([$pov_prog_cc_flags_value])

  if test x"$pov_prog_cc_flags_value" = x"yes"; then
    CFLAGS="$CFLAGS $1"
    ifelse([$2],[],[:],[$2])
  else
    ifelse([$3],[],[:],[$3])
  fi
])


# POV_PROG_CXX_STATIC()
# -------------------
# Get flag for static linking with a library, cache result,
# and update LDFLAGS.
# Inspired (borrowed) from GNU libtool 1.4.2
# Changed $build_os to $host_os, and $build_cpu to $host_cpu
#
AC_DEFUN([POV_PROG_CXX_STATIC],
[
  AC_CACHE_VAL(
    [pov_cv_prog_cxx_static],
    [
      AC_MSG_CHECKING([for linker static flag])
      if test x"$GCC" = x"yes"; then
        pov_cv_prog_cxx_static="-static"
        case "$host_os" in
          aix*)
            pov_cv_prog_cxx_static="$pov_cv_prog_cxx_static -Wl,-lC"
            ;;
          irix*)  # [NC]
            pov_cv_prog_cxx_static="$pov_cv_prog_cxx_static -Wl,-Bstatic"
            ;;
        esac
      else
        case "$host_os" in
          aix3* | aix4* | aix5*)
            if text x"$host_cpu" = x"ia64"; then
              pov_cv_prog_cxx_static="-Bstatic"
            else
              pov_cv_prog_cxx_static="-bnso -bI:/lib/syscalls.exp"
            fi
            ;;
          hpux9* | hpux10* | hpux11*)
            pov_cv_prog_cxx_static="-Wl,-a -Wl,archive"
            ;;
          irix5* | irix6*)
            pov_cv_prog_cxx_static="-non_shared"
            ;;
          osf3* | osf4* | osf5*)
            pov_cv_prog_cxx_static="-non_shared"
            ;;
          sco3.2v5*)
            pov_cv_prog_cxx_static="-dn"
            ;;
          *)
            pov_cv_prog_cxx_static="-Bstatic"
            ;;
        esac
      fi
      AC_MSG_RESULT([$pov_cv_prog_cxx_static])

      # now check for working flag
      AC_MSG_CHECKING([for working '$pov_cv_prog_cxx_static' flag])
      pov_prog_cxx_static_save_ldflags="$LDFLAGS"
      LDFLAGS="$LDFLAGS $pov_cv_prog_cxx_static"
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([], [])],
        [AC_MSG_RESULT([yes])],
        [
          AC_MSG_RESULT([no])
          AC_MSG_NOTICE([static linking does not work, revert to dynamic linking])
          pov_cv_prog_cxx_static=""
          LDFLAGS="$pov_prog_cxx_static_save_ldflags"
        ]
      )
    ]
  )
])


# POV_CHECK_PATH(ENVVAR, PATH, [ACTION-IF-FOUND], [ACTION-IF-MISSING]))
# --------------
# Check whether the environment variable contains a given path, warn and
# remove it.
AC_DEFUN([POV_CHECK_PATH],
[
  AC_SUBST([$1])

  # process paths containing dots and create regexp
  pov_check_path_regexp="[[=:]]*`echo $2 | sed 's,\.,\\\\.,g'`:*"
  AC_TRY_COMMAND([echo pov_check_path_regexp = $pov_check_path_regexp > /dev/null])

  # initial and processed variable values
  eval "pov_check_path_old=\$$1"
  pov_check_path_new=`echo $pov_check_path_old | sed s,$pov_check_path_regexp,,g`
  AC_TRY_COMMAND([echo pov_check_path_old    = $pov_check_path_old > /dev/null])
  AC_TRY_COMMAND([echo pov_check_path_new    = $pov_check_path_new > /dev/null])

  AC_MSG_CHECKING([whether \$$1 contains the $2 path])
  if test x"$pov_check_path_new" != x"$pov_check_path_old"; then
    AC_MSG_RESULT([yes])
    AC_MSG_WARN([\$$1 is incorrectly set with the $2 path])
    eval $1=$pov_check_path_new
    ifelse([$3],[],[:],[$3])
  else
    AC_MSG_RESULT([no])
    ifelse([$4],[],[:],[$4])
  fi
])


# POV_CHECK_LIB(lib, required_version, search_libs, check_function, header, version_function)
# -------------
# Check whether a function is found in a set of libraries, and compare the
# library version to the required one.
#
AC_DEFUN([POV_CHECK_LIB],
[
  # check the library
  AC_SEARCH_LIBS(
    [$4],
    [$3],
    [
      # check include file
      AC_CHECK_HEADER(
        [$5],
        [
          # check library version, update LIBS
          AC_MSG_CHECKING([for lib$1 version >= $2])
          AC_RUN_IFELSE(
            [
              AC_LANG_SOURCE(
[#include <stdio.h>
#include <string.h>
#include "$5"
int main (void)
{
  const char *version = $6;
  fprintf (stderr, "%s\n", version);
  return ! (strcmp (version ? version : "", "$2") >= 0);
}]
              )
            ],
            [
              pov_check_lib_version=`eval $ac_try 2>&1`
              pov_check_lib="ok"
              AC_MSG_RESULT([$pov_check_lib_version, $pov_check_lib])
            ],
            [
              if test "$ac_status" != "1" || ! test -s conftest$ac_exeext ; then
                pov_check_lib="unknown"
                AC_MSG_RESULT([$pov_check_lib])
              else
                pov_check_lib_version=`eval $ac_try 2>&1`
                pov_check_lib="bad"
                AC_MSG_RESULT([$pov_check_lib_version, $pov_check_lib])
              fi
            ],
            [AC_MSG_RESULT([cross-compiling, forced])]
          )  # AC_RUN_IFELSE
        ],
        [pov_check_lib="no headers"]
      )  # AC_CHECK_HEADER
    ],
    [pov_check_lib="not found"],
    []
  )  # AC_SEARCH_LIBS
])


# POV_CHECK_LIBTIFF(required_version)
# -----------------
# Specialized version of the function above.
#
AC_DEFUN([POV_CHECK_LIBTIFF],
[
  # check the library
  AC_SEARCH_LIBS(
    [TIFFGetVersion],
    [tiff],
    [
      # check include file
      AC_CHECK_HEADER(
        [tiffio.h],
        [
          # check library version, update LIBS
          AC_MSG_CHECKING([for libtiff version >= $1])
          AC_RUN_IFELSE(
            [
              AC_LANG_SOURCE(
[#include <stdio.h>
#include <string.h>
#include "tiffio.h"
int main (void)
{
  char version[[81]];
  char *p, *c;
  sprintf (version, "%.79s", TIFFGetVersion ());
  p = strstr (version, "Version ") + strlen ("Version ");
  if (! p)  return 1;
  c = strchr (p, '\n');
  if (! c)  return 1;
  *c = '\0';
  fprintf (stderr, "%s\n", p);
  return ! (strcmp (p, "$1") >= 0);
}]
              )
            ],
            [
              pov_check_libtiff_version=`eval $ac_try 2>&1`
              pov_check_libtiff="ok"
              AC_MSG_RESULT([$pov_check_libtiff_version, $pov_check_libtiff])
            ],
            [
              if test "$ac_status" != "1" || ! test -s conftest$ac_exeext ; then
                pov_check_libtiff="unknown"
                AC_MSG_RESULT([$pov_check_libtiff])
              else
                pov_check_libtiff_version=`eval $ac_try 2>&1`
                pov_check_libtiff="bad"
                AC_MSG_RESULT([$pov_check_libtiff_version, $pov_check_libtiff])
              fi
            ],
            [AC_MSG_RESULT([cross-compiling, forced])]
          )  # AC_RUN_IFELSE
        ],
        [pov_check_libtiff="no headers"]
      )  # AC_CHECK_HEADER
    ],
    [pov_check_libtiff="not found"],
    []
  )  # AC_SEARCH_LIBS
])


# POV_CHECK_LIBJPEG(required_version)
# -----------------
# Specialized version.
#
AC_DEFUN([POV_CHECK_LIBJPEG],
[
  # check the library
  AC_SEARCH_LIBS(
    [jpeg_std_error],
    [jpeg],
    [
      # check include file
      AC_CHECK_HEADER(
        [jpeglib.h],
        [
          # check library version, update LIBS
          AC_MSG_CHECKING([for libjpeg version >= $1])
          AC_RUN_IFELSE(
            [
              AC_LANG_SOURCE(
[#include <stdio.h>
#include <string.h>
#include "jpeglib.h"
int main (void)
{
  char ver_string[[81]];
  sprintf (ver_string, "%d", JPEG_LIB_VERSION);
  ver_string[[1]] = (ver_string[[1]] > '0') ? ver_string[[1]] + 48 : '\0';
  fprintf (stderr, "%s\n", ver_string);
  return ! (strcmp (ver_string, "$1") >= 0);
}]
              )
            ],
            [
              pov_check_libjpeg_version=`eval $ac_try 2>&1`
              pov_check_libjpeg="ok"
              AC_MSG_RESULT([$pov_check_libjpeg_version, $pov_check_libjpeg])
            ],
            [
              if test "$ac_status" != "1" || ! test -s conftest$ac_exeext ; then
                pov_check_libjpeg="unknown"
                AC_MSG_RESULT([$pov_check_libjpeg])
              else
                pov_check_libjpeg_version=`eval $ac_try 2>&1`
                pov_check_libjpeg="bad"
                AC_MSG_RESULT([$pov_check_libjpeg_version, $pov_check_libjpeg])
              fi
            ],
            [AC_MSG_RESULT([cross-compiling, forced])]
          )  # AC_RUN_IFELSE
        ],
        [pov_check_libjpeg="no headers"]
      )  # AC_CHECK_HEADER
    ],
    [pov_check_libjpeg="not found"],
    []
  )  # AC_SEARCH_LIBS
])
