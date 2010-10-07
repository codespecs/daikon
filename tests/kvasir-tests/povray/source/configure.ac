##############################################################################
#               configure.ac
#
# Process this file with autoconf to produce the configure script.
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
# $File: //depot/povray/3.6-release/unix/configure.ac $
# $Revision: #7 $
# $Change: 3032 $
# $DateTime: 2004/08/02 18:43:41 $
# $Author: chrisc $
# $Log$
##############################################################################

# configure.ac for the source distribution of POV-Ray 3.6 for UNIX
# Written by Nicolas Calimet <pov4grasp@free.fr>

# This 'configure.ac' file is a full rewrite of the former 'configure.in'
# prepared by Mark Gordon and distributed with the official POV-Ray 3.5
# for Unix/Linux.  The hope is that the 'configure' script derived from
# 'configure.ac' will work on most *nix architectures (and not only Linux).
# 'configure.ac' comes with a set of supporting files that are needed to
# generate a fresh 'configure'.  In particular, the 'acinclude.m4' file
# defines all POV_* macros used throughout the present file.
#
# New features (among others):
# * The supporting libraries (Zlib, PNG, JPEG, and TIFF) are included in
#   the compilation process when they are either not found on the system
#   or too old.  Otherwise the POV-Ray executable is linked against the
#   shared libraries available on the system.  Full static linking is also
#   possible through the --disable-shared option.
# * Compilation flags (mostly for code optimization) are tested at
#   configure time.  For now those flags are essentially oriented towards
#   the GNU gcc compiler, but further tweaking might come along with
#   experience in other compilers (e.g. Intel C/C++).
# * It is no longer required to hand-edit the source/optout.h file to update
#   the "Compiled by..." heading message.  Instead, configure uses the
#   COMPILED_BY environment variable (required).


# Required autoconf version.
AC_PREREQ(2.57)

# Inits.
# Cannot use a variable to define the POV-Ray version string.
AC_INIT([POV-Ray], [3.6.1], [pov4grasp@free.fr], [povray])
VERSION_BASE=3.6
AC_SUBST([VERSION_BASE])
AC_DEFINE_UNQUOTED([VERSION_BASE],
  ["$VERSION_BASE"],
  [Base version number of package]
)
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_HEADERS([conf.h])
AC_CONFIG_SRCDIR([unix/unix.cpp])

# Required versions of the supporting libraries.
# Must be declared after AC_INIT.
required_libz_version="1.2.1"
required_libpng_version="1.2.5"
required_libjpeg_version="6b"
required_libtiff_version="3.6.1"

# Supported options.
POV_ARG_ENABLE([--enable-watch-cursor],
  [enable a watch cursor over the display while rendering (X Window only)]
)
POV_ARG_ENABLE([--enable-strip],
  [enable stripping all symbols from the object files (smaller binary)]
)
POV_ARG_ENABLE([--enable-debug],
  [enable compiler debugging mode]
)
POV_ARG_ENABLE([--enable-profile],
  [enable program execution profiling]
)
POV_ARG_ENABLE([--disable-optimiz],
  [disable all compiler optimizations]
)
POV_ARG_ENABLE([--disable-pipe],
  [disable usage of pipes during compilation (use temporary files instead)]
)
POV_ARG_ENABLE([--disable-shared],
  [disable linking with shared libraries (build static binary)]
)
POV_ARG_ENABLE([--disable-lib-checks],
  [disable checking for supporting image libraries (force compiling)]
)
POV_ARG_ENABLE([--disable-vsnprintf-check],
  [disable checking for the C99 vsnprintf() function (systems without this functions will fail compiling the program)]
)
# cannot use $PACKAGE_NAME instead of litteral POV-Ray
POV_ARG_ENABLE([--disable-io-restrictions],
  [disable POV-Ray's mechanism for control of I/O operations]
)

POV_ARG_WITH([--without-cygwin-dll],
  [don't link with the Cygwin DLL (MinGW required)]
)
POV_ARG_WITH([--with-svga],
  [use the SVGA library (Linux only)]
)

AC_ARG_VAR([COMPILED_BY],
  [customize the "unofficial version" message which defaults to user@machine]
)
AC_ARG_VAR([C99_COMPATIBLE_RADIOSITY],
  [adapt radiosity code to non-IEEE 754 floating point, requires ISO C99 compiler and library (see source/octree.cpp for details)]
)


###############################################################################


# Remove povray.ini unconditionally.
rm -f ./povray.ini


# Instruction message.
if test -z "$COMPILED_BY"; then
  echo "
Welcome to the $PACKAGE_NAME $PACKAGE_VERSION configure script.

This script will prepare the source distribution of $PACKAGE_NAME $PACKAGE_VERSION for Unix
for compilation on this machine.  Compiling the program yourself means
that the POV-Ray Team(tm) is not responsible for supporting this
version, no matter if it is the unmodified official POV-Ray source code
or a customized version.  Refer to the POV-Ray source license conditions
that come with this package.

In order to start configuration, you have to provide a valid contact
information that will be included in the executable and displayed
whenever POV-Ray (or a modified version of it) is started.  In case
you intend to distribute this binary, this information will help users
to contact the appropriate maintainers.  To start configuring $PACKAGE_NAME
type in the following command with your own contact information:

./configure COMPILED_BY=\"your name <email@address>\"

Please consult the INSTALL file for additional configuration and
installation instructions.
"
  exit 1
fi


# Don't use 'echo' (not saved into config.log / doesn't honor --silent).
# Don't use AC_MSG_NOTICE which prints things first.
AC_MSG_RESULT([
===============================================================================
Configure $PACKAGE_NAME version $PACKAGE_VERSION
===============================================================================

This is an unofficial version compiled by:
 $COMPILED_BY
The POV-Ray Team(tm) is not responsible for supporting this version.])

AC_DEFINE_UNQUOTED([DISTRIBUTION_MESSAGE_2],
  [" $COMPILED_BY"],
  [Who compiled this binary.]
)


###############################################################################

AC_MSG_RESULT([
Environment
-----------])

AC_CANONICAL_BUILD
AC_CANONICAL_HOST

AM_INIT_AUTOMAKE([1.7 dist-bzip2])
AM_MAINTAINER_MODE

POV_CHECK_PATH(C_INCLUDE_PATH, ".",
  [pov_warn_path="$pov_warn_path C_INCLUDE_PATH"]
)
POV_CHECK_PATH(CPLUS_INCLUDE_PATH, ".",
  [pov_warn_path="$pov_warn_path CPLUS_INCLUDE_PATH"]
)


###############################################################################

AC_MSG_RESULT([
Programs
--------])

# Check for C++ compiler and version.
# Inits compiler flags to avoid defaults such as '-g -O2'.
CXXFLAGS="$CXXFLAGS"
POV_PROG_CXX_VERSION
AC_DEFINE_UNQUOTED([COMPILER_VER],
  [" ($pov_prog_cxx_version @ $host)"],
  [Compiler version.]
)

# Check for C compiler for CFLAGS passed to supporting libraries.
# Inits compiler flags to avoid defaults such as '-g -O2'.
CFLAGS="$CFLAGS"
AC_PROG_CC
AC_PROG_CPP

# Not necessary when AM_MAINTAINER_MODE is called above.
###AC_PROG_MAKE_SET

AC_PROG_RANLIB


###############################################################################

AC_MSG_RESULT([
Language constructs and functions
---------------------------------])

AC_LANG(C++)

# Don't link with the cygwin DLL.
# Must be placed before any other header or function checks.
AC_MSG_CHECKING([whether to link with cygwin DLL])
if test x"$with_cygwin_dll" = x"no"; then
  AC_MSG_RESULT([no])
  case "$host" in
    *cygwin*)
      pov_flags="-mno-cygwin"
      pov_cygwin_save_cxxflags="$CXXFLAGS"
      pov_cygwin_save_cflags="$CFLAGS"
      POV_PROG_CXX_FLAGS([$pov_flags],
        [
          POV_PROG_CC_FLAGS([$pov_flags],
            [
              AC_CHECK_LIB([mingw32], [main],
                [
                  LDFLAGS="$LDFLAGS $pov_flags"
                  # -D__CYGWIN is required for tiffio.h (incl. in tiff_pov.cpp)
                  CPPFLAGS="$CPPFLAGS -D__CYGWIN $pov_flags" 
                ],
                [with_cygwin_dll="yes"]
              )
            ],
            [with_cygwin_dll="yes"]
          )
        ],
        [with_cygwin_dll="yes"]
      )
      if test x"$with_cygwin_dll" != x"no"; then
        AC_MSG_WARN([Cannot link without Cygwin DLL])
        CXXFLAGS="$pov_cygwin_save_cxxflags"
        CFLAGS="$pov_cygwin_save_cflags"
      fi
    ;;
  esac
else
  case "$host" in
    *cygwin*)
      AC_MSG_RESULT([yes])
    ;;
    *)
      AC_MSG_RESULT([no])
    ;;
  esac
fi

# Compiling on the mingw32 platform (e.g. using MSYS) also requires
# the -D__CYGWIN flag for tiff_pov.cpp
case "$host" in
  *mingw32*)  CPPFLAGS="$CPPFLAGS -D__CYGWIN";;
esac


# Language constructs.

AC_HEADER_TIME
AC_CHECK_HEADERS([limits.h sys/time.h unistd.h])

AC_TYPE_SIZE_T
AC_STRUCT_TM

AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long int])	# check for 64 bit support
AC_CHECK_SIZEOF([size_t])       # check for 64 bit support
AC_CHECK_SIZEOF([float])


# Checks for functions.

# the test for memcmp does not work well with C++
AC_LANG_PUSH(C)
AC_FUNC_MEMCMP
AC_LANG_POP(C)

AC_TYPE_SIGNAL
#AC_FUNC_STRFTIME
#AC_FUNC_VPRINTF

# vsnprintf <stdarg.h> (C99)
if test x"$enable_vsnprintf_check" != x"no"; then
  AC_CHECK_FUNC([vsnprintf],
    [],
    [AC_MSG_ERROR([
*** This system does not provide the C99 vsnprintf() function which
*** is required to compile $PACKAGE_NAME.  You should consider updating
*** your C library as it is too old.  Alternatively you might install
*** a specific library implementing the (v)snprintf family of functions.
*** Searching the web should give you appropriate pointers.
***
*** In case you want to try compiling $PACKAGE_NAME while your system lacks
*** this function (meaning that compilation *will* fail unless you provide
*** a replacement function), try the --disable-vsnprintf-check option.
])
    ]
  )
fi

# getcwd or its equivalent with getenv <unistd.h>
AC_LANG_PUSH(C)
AC_CHECK_FUNCS([getcwd],
  [],
  [
    AC_MSG_CHECKING([for working getenv("PWD")])
    AC_RUN_IFELSE(
      [
        AC_LANG_SOURCE(
[#include <stdlib.h>
int main(void) { return (getenv("PWD") == NULL); }]
        )
      ],
      [AC_MSG_RESULT([yes])],
      [
        AC_MSG_RESULT([no])
        AC_MSG_NOTICE([I/O restriction code will be disabled])
        enable_io_restrictions="no"
      ],
      [
        AC_MSG_RESULT([cross-compiling])
        AC_MSG_NOTICE([I/O restriction code will be disabled])
        enable_io_restrictions="no"
      ]
    )
  ]
)
AC_LANG_POP(C)

# readlink <unistd.h>
if test x"$enable_io_restrictions" != x"no"; then
  AC_CHECK_FUNCS([readlink],
    [],
    [
      AC_MSG_NOTICE([I/O restriction code will be disabled])
      enable_io_restrictions="no"
    ]
  )
fi

# gettimeofday <sys/time.h>
AC_CHECK_FUNCS([gettimeofday],
  [AC_DEFINE([PRECISION_TIMER_AVAILABLE], [1], [Precision timer.])],
  [AC_DEFINE([PRECISION_TIMER_AVAILABLE], [0], [Precision timer.])]
)


###############################################################################

AC_MSG_RESULT([
Libraries
---------])

# Link with static library, update LDFLAGS.
AC_MSG_CHECKING([whether to enable static linking])
if test x"$enable_shared" = x"no"; then
  AC_MSG_RESULT([yes])
  POV_PROG_CXX_STATIC
else
  AC_MSG_RESULT([no])
fi


# libm
AC_CHECK_LIB([m], [sin])


# asinh and friends (requires libm)
AC_CHECK_FUNCS([asinh],
  [],
  [AC_DEFINE([NEED_INVHYP], [], [Inverse hyperbolic functions.])]
)


# support for non-ieee compilers (requires libm)
if test "$C99_COMPATIBLE_RADIOSITY" \
|| test x"$ac_cv_sizeof_int" != x"4" \
|| test x"$ac_cv_sizeof_float" != x"4"; then
  AC_CHECK_FUNCS([copysign],
    [],
    [AC_MSG_ERROR([
*** Cannot find the 'copysign' function required for non-ieee platform
])]
  )
  if test -z "$C99_COMPATIBLE_RADIOSITY"; then  # auto-detection
    AC_CHECK_FUNCS([ilogbf],
      [AC_DEFINE([C99_COMPATIBLE_RADIOSITY], [2], [See source/octree.cpp for details.])],
      [AC_CHECK_FUNCS([ilogb],
         [AC_DEFINE([C99_COMPATIBLE_RADIOSITY], [4], [See source/octree.cpp for details.])],
         [AC_CHECK_FUNCS([logbf],
            [AC_DEFINE([C99_COMPATIBLE_RADIOSITY], [1], [See source/octree.cpp for details.])],
            [AC_CHECK_FUNCS([logb],
               [AC_DEFINE([C99_COMPATIBLE_RADIOSITY], [3], [See source/octree.cpp for details.])],
               [AC_MSG_ERROR([
*** Cannot find a 'logb' or equivalent function required for non-ieee platform
])]
             )]
          )]
       )]
    )
  else  # check value of C99_COMPATIBLE_RADIOSITY and function presence
    case "$C99_COMPATIBLE_RADIOSITY" in
      1) AC_CHECK_FUNCS([logbf],  [], [AC_MSG_ERROR([logbf not found])]) ;;
      2) AC_CHECK_FUNCS([ilogbf], [], [AC_MSG_ERROR([ilogbf not found])]) ;;
      3) AC_CHECK_FUNCS([logb],   [], [AC_MSG_ERROR([logb not found])]) ;;
      4) AC_CHECK_FUNCS([ilogb],  [], [AC_MSG_ERROR([ilogb not found])]) ;;
      *) AC_MSG_ERROR([Unsupported C99_COMPATIBLE_RADIOSITY value]) ;;
    esac
    AC_DEFINE_UNQUOTED([C99_COMPATIBLE_RADIOSITY],
      [$C99_COMPATIBLE_RADIOSITY],
      [See source/octree.cpp for details.]
    )
  fi
fi


# Let's try to use the -v compiler flag for better debugging.
# Backup compiler flags first.
pov_lib_save_cflags="$CFLAGS"
if ! test `$CC -v < /dev/null 2> /dev/null`; then
  CFLAGS="$CFLAGS -v"
fi
pov_lib_save_cxxflags="$CXXFLAGS"
if ! test `$CXX -v < /dev/null 2> /dev/null`; then
  CXXFLAGS="$CXXFLAGS -v"
fi


# don't even look for libvga/libX11 under Cygwin/MinGW
if test x"$with_cygwin_dll" = x"no"; then

  AC_MSG_NOTICE([X Window display will be disabled])
  AC_MSG_NOTICE([SVGAlib display will be disabled])
  AC_DEFINE([X_DISPLAY_MISSING], [], [Don't use the X Window System.])
  pov_xwin_msg="disabled"
  pov_svga_msg="disabled"

else 

  # libvga and libvgagl
  pov_svga_msg="disabled"
  if test x"$with_svga" != x"no"; then
    AC_CHECK_LIB([vga], [vga_init],
      [AC_CHECK_LIB([vgagl], [gl_setcontextvga], [pov_svga_msg="enabled"], [], [-lvga])]
    )
    if test x"$pov_svga_msg" = x"disabled"; then
      AC_MSG_NOTICE([SVGAlib display will be disabled])
    fi
  fi

  # libX11
  # Some systems seem to have libz in the X11 paths, so let's
  # check for libX11 first.

  # IRIX cannot find X headers/libraries, so set defaults.
  case "$host" in
    *irix*)
      if test x"${x_includes}" = x"NONE"; then
        x_includes="/usr/include"
      fi
      if test x"$x_libraries" = x"NONE"; then
        x_libraries=""
	# IRIX ld32/64 have weird use of -L flags (they exclude default paths)
        # Add the content of LD_LIBRARY*_PATHS before system path
        # Note that the first path is intentionnaly not prefixed with -L
        if test x"$ac_cv_sizeof_long_int" = x"4"; then
          if test "$LD_LIBRARYN32_PATH"; then
            x_libraries=`echo $LD_LIBRARYN32_PATH | sed s,:," -L",g`
          fi
          x_libraries="$x_libraries -L/usr/lib32"
        else
          if test "$LD_LIBRARY64_PATH"; then
            x_libraries=`echo $LD_LIBRARY64_PATH | sed s,:," -L",g`
          fi
          x_libraries="$x_libraries -L/usr/lib64"
        fi
      fi
    ;;
  esac

  pov_xwin_msg="disabled"
  AC_PATH_XTRA
  if test x"$have_x" = x"yes"; then
    AC_CHECK_LIB([X11], [XFlush],
      [
        # X_CFLAGS should actually be X_CPPFLAGS since it only adds -Idir
        CPPFLAGS="$X_CFLAGS $CPPFLAGS"
        LDFLAGS="$X_LIBS $LDFLAGS"
        LIBS="$X_EXTRA_LIBS $X_PRE_LIBS -lX11 $LIBS"
        pov_xwin_msg="enabled"
      ],
      [
        AC_MSG_NOTICE([X Window display will be disabled])
        AC_DEFINE([X_DISPLAY_MISSING],[1])
      ],
      [-L$x_libraries -lX11]
    )
  else
    AC_MSG_NOTICE([X Window display will be disabled])
  fi

  if test x"$pov_xwin_msg" = x"enabled"; then
    # Color icon and X Window cursor.
    AC_CHECK_LIB([Xpm], [XpmCreatePixmapFromData])
    AC_MSG_CHECKING([whether to enable the watch cursor])
    if test x"$enable_watch_cursor" = x"yes"; then
      AC_MSG_RESULT([yes])
      AC_CHECK_FUNC([XCreateFontCursor],
        [AC_DEFINE([USE_CURSOR], [], [Use a watch cursor while rendering.])],
        [AC_MSG_NOTICE([watch cursor will be disabled])]
      )
    else
      AC_MSG_RESULT([no])
    fi
  fi

  ### Note from Warp <warp@tag.povray.org> :
  # Using "-static" for static linking in Solaris causes a linkage error.
  # For some reason the X libraries in Solaris are not designed for static
  # linking. [...] So at least for Solaris the '-static' flag should
  # definitely be turned off by default.
  if test "$pov_cv_prog_cc_static" \
  && test x"$ac_cv_lib_X11_XFlush" = x"yes"; then
    case "$host" in
      sparc-*)
        AC_MSG_WARN([Using $pov_cv_prog_cc_static might cause a linkage error])
      ;;
    esac
  fi

fi  # with_cygwin_dll


AC_MSG_CHECKING([for installed supporting libraries])
if test x"$enable_lib_checks" != x"no"; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no, build them all])
fi


# Force the C compiler for testing the supporting libraries.
# This may prevent build failure on some systems.
AC_LANG_PUSH(C)


# libz
if test x"$enable_lib_checks" != x"no"; then
  POV_CHECK_LIB([z], [$required_libz_version], [z],
    [zlibVersion], [zlib.h], [zlibVersion()]
  )
fi
if test x"$pov_check_lib" != x"ok"; then
  if test x"$enable_lib_checks" != x"no"; then
    AC_MSG_NOTICE([zlib will be built and statically linked to $PACKAGE_NAME])
  fi
  AC_CONFIG_SUBDIRS([libraries/zlib])
  if test "$ac_cv_search_zlibVersion"; then
    LIBS=`echo $LIBS | sed s,$ac_cv_search_zlibVersion,,g`
  fi
  INCZ="-I\$(top_srcdir)/libraries/zlib"
  LIBZ="../libraries/zlib/libz.a"
  export INCZ		# tell libpng and libtiff to use the provided zlib.
fi
AC_SUBST([INCZ])
AC_SUBST([LIBZ])


# libpng, depends on libz
# if libz needs to be compiled, force compiling libpng as well
if test x"$pov_check_lib" = x"ok"; then
  if test x"$enable_lib_checks" != x"no"; then
    # check libpng and update ${pov_check_lib}
    POV_CHECK_LIB([png], [$required_libpng_version], [png12 png],
      [png_get_libpng_ver], [png.h], [png_get_libpng_ver(NULL)]
    )
  fi
elif test x"$enable_lib_checks" != x"no"; then
  AC_MSG_CHECKING([for libpng])
  AC_MSG_RESULT([forced by libz])
fi
if test x"$pov_check_lib" != x"ok"; then
  if test x"$enable_lib_checks" != x"no"; then
    AC_MSG_NOTICE([libpng will be built and statically linked to $PACKAGE_NAME])
  fi
  AC_CONFIG_SUBDIRS([libraries/png])
  if test "$ac_cv_search_png_get_libpng_ver"; then
    LIBS=`echo $LIBS | sed s,$ac_cv_search_png_get_libpng_ver,,g`
  fi
  INCPNG="-I\$(top_srcdir)/libraries/png"
  LIBPNG="../libraries/png/libpng.a"
fi
AC_SUBST([INCPNG])
AC_SUBST([LIBPNG])


# libjpeg
# always build it under cygwin which may use a custom jpeglib.h file
case "$host" in
  *cygwin*)
    AC_MSG_CHECKING([for libjpeg])
    AC_MSG_RESULT([forced by cygwin])
  ;;

  *)
  if test x"$enable_lib_checks" != x"no"; then
    POV_CHECK_LIBJPEG([$required_libjpeg_version])
  fi
  ;;
esac
if test x"$pov_check_libjpeg" != x"ok"; then
  if test x"$enable_lib_checks" != x"no"; then
    AC_MSG_NOTICE([libjpeg will be built and statically linked to $PACKAGE_NAME])
  fi
  AC_CONFIG_SUBDIRS([libraries/jpeg])
  if test "$ac_cv_search_jpeg_std_error"; then
    LIBS=`echo $LIBS | sed s,$ac_cv_search_jpeg_std_error,,g`
  fi
  INCJPEG="-I\$(top_srcdir)/libraries/jpeg"
  LIBJPEG="../libraries/jpeg/libjpeg.a"
  export INCJPEG	# tell libtiff to use the provided jpeg library.
else
  # create jversion.h in builddir
  if ! test -d "./source"; then
    mkdir ./source
  fi
  echo "#define JVERSION \"${pov_check_libjpeg_version}\"" > ./source/jversion.h
fi
AC_SUBST([INCJPEG])
AC_SUBST([LIBJPEG])


# libtiff, depends on libz and libjpeg
if test -z "$INCZ"  &&  test -z "$INCJPEG"; then
  if test x"$enable_lib_checks" != x"no"; then
    POV_CHECK_LIBTIFF([$required_libtiff_version])
  fi
elif test x"$enable_lib_checks" != x"no"; then
  AC_MSG_CHECKING([for libtiff])
  if test "$INCZ"; then
    AC_MSG_RESULT([forced by libz])
  else
    AC_MSG_RESULT([forced by libjpeg])
  fi
fi
if test x"$pov_check_libtiff" != x"ok"; then
  if test x"$enable_lib_checks" != x"no"; then
    AC_MSG_NOTICE([libtiff will be built and statically linked to $PACKAGE_NAME])
  fi
  AC_CONFIG_SUBDIRS([libraries/tiff])
  if test "$ac_cv_search_TIFFGetVersion"; then
    LIBS=`echo $LIBS | sed s,$ac_cv_search_TIFFGetVersion,,g`
  fi
  INCTIFF="-I\$(top_srcdir)/libraries/tiff/libtiff -I\$(top_builddir)/libraries/tiff/libtiff"
  LIBTIFF="../libraries/tiff/libtiff/libtiff.a"
fi
AC_SUBST([INCTIFF])
AC_SUBST([LIBTIFF])


# Revert to previous compiler (C++).
AC_LANG_POP(C)


# Restore initial compiler flags.
CFLAGS="$pov_lib_save_cflags"
CXXFLAGS="$pov_lib_save_cxxflags"


###############################################################################

AC_MSG_RESULT([
Compiling
---------])

# Start with no flags at all, so back them up first.
pov_comp_save_cflags="$CFLAGS"
pov_comp_save_cxxflags="$CXXFLAGS"
CFLAGS=
CXXFLAGS=


# Use pipes for communication between compilation stages.
AC_MSG_CHECKING([whether to enable pipes for communications])
if test x"$enable_pipe" = x"no"; then
  AC_MSG_RESULT([no])
else
  AC_MSG_RESULT([yes])
  pov_flags="-pipe"
  POV_PROG_CXX_FLAGS([$pov_flags],
    [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
  )
fi


# Language.

# don't use -ansi since it can make compilation fail (e.g. on cygwin).
#POV_PROG_CXX_FLAGS([-ansi], [], [POV_PROG_CXX_FLAGS([-Aa])])

# -w would prevent all warning messages, even if -W... is given later on.
# we don't want to clutter the compiling with lots of warnings.
#POV_PROG_CXX_FLAGS([-W])
#POV_PROG_CXX_FLAGS([-Wall], [], [POV_PROG_CXX_FLAGS([-fullwarn])])
POV_PROG_CXX_FLAGS([-Wno-multichar])


# I/O restrictions.
AC_MSG_CHECKING([whether to enable I/O restrictions])
if test x"$enable_io_restrictions" = x"no"; then
  AC_MSG_RESULT([no])
  AC_DEFINE([IO_RESTRICTIONS_DISABLED], [1], [I/O restrictions.])
else
  AC_MSG_RESULT([yes])
  AC_DEFINE([IO_RESTRICTIONS_DISABLED], [0], [I/O restrictions.])
fi


# Compile with debugging code.

AC_MSG_CHECKING([whether to enable debugging])

if test x"$enable_debug" = x"yes"; then
  AC_MSG_RESULT([yes])
  pov_flags="-g"
  POV_PROG_CXX_FLAGS([$pov_flags],
    [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi],
    [AC_MSG_WARN([Cannot set debugging mode])]
  )
  AC_DEFINE([MEM_STATS], [2], [Memory statistics.])
  AC_DEFINE([MEM_TRACE], [],  [Memory tracing.])
  AC_DEFINE([MEM_GUARD], [],  [Memory guard.])
  AC_DEFINE([MEM_TAG],   [],  [Memory tag.])
else
  AC_MSG_RESULT([no])
  AC_DEFINE([MEM_STATS], [1], [Memory statistics.])
fi


# Compile for profiling.

AC_MSG_CHECKING([whether to enable profiling])

if test x"$enable_profile" = x"yes"; then
  AC_MSG_RESULT([yes])
  if test -z "$pov_cv_prog_cxx_flags_g"; then
    pov_flags="-g"
    POV_PROG_CXX_FLAGS([$pov_flags],
      [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi],
      [AC_MSG_WARN([Cannot set debugging mode])]
    )
  fi
  pov_flags="-pg"
  POV_PROG_CXX_FLAGS([$pov_flags],
    [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi],
    [AC_MSG_WARN([Cannot set profiling mode])]
  )
else
  AC_MSG_RESULT([no])
fi


# Strip object files.

AC_MSG_CHECKING([whether to enable stripping])

if test x"$enable_strip" = x"yes"; then
  if test x"$enable_debug" = x"yes" || test x"$enable_profile" = x"yes"; then
    AC_MSG_RESULT([no, debugging/profiling mode is on])
  else
    AC_MSG_RESULT([yes])
    pov_flags="-s"
    POV_PROG_CXX_FLAGS([$pov_flags],
      [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi],
      [AC_MSG_WARN([Cannot set stripping mode])]
    )
  fi
else
  AC_MSG_RESULT([no])
fi


# Compile with optimizations.

AC_MSG_CHECKING([whether to enable optimizations])

if test x"$enable_optimiz" != x"no"; then

  AC_MSG_RESULT([yes])

  # don't try -fast (makes ICC fail when linking due to implied -ipo flag).
  pov_flags="-O3"
  POV_PROG_CXX_FLAGS([$pov_flags],
    [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi],
    [
      pov_flags="-O2"
      POV_PROG_CXX_FLAGS([$pov_flags],
        [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi],
        [
          pov_flags="-O"
          POV_PROG_CXX_FLAGS([$pov_flags],
            [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
          )
        ]
      )
    ]
  )

  # gcc-specific flags.
  if test x"$CXX" = x"g++"; then
    if test x"$pov_cv_prog_cxx_flags_O" = x"yes"; then  # -O was set
      for pov_flags in "-fexpensive-optimizations" "-finline-functions" "-foptimize-sibling-calls"; do
        POV_PROG_CXX_FLAGS([$pov_flags],
          [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
        )
      done
    elif test x"$pov_cv_prog_cxx_flags_O2" = x"yes"; then  # -O2 was set
      pov_flags="-finline-functions"
      POV_PROG_CXX_FLAGS([$pov_flags],
        [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
      )
    fi
  # icc-specific flags.
  elif test x"$CXX" = x"icc"; then
    pov_flags="-ip"
    POV_PROG_CXX_FLAGS([$pov_flags],
      [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
    )
  fi

  # Arch-specific tuning, alphabetically ordered for conveniency.
  case "$host" in

    alphaev*6*)
      ### Suggested by Christopher Endsley <endsley@yokuts.csustan.edu> .
      for pov_flags in "-mcpu=ev56" "-mno-soft-float"; do
        POV_PROG_CXX_FLAGS([$pov_flags],
          [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
        )
      done
      ;;

    i?86-* | k?-* | *86*64*)
      if test x"$cross_compiling" != x"yes"; then
        # head is used to handle SMP/HT systems
        # check whether 'head -n 1' is supported; otherwise use older 'head -1'
        if test -z "`head -n 1 /dev/null 2>&1`"; then
          head_1='head -n 1'
        else
          head_1='head -1'
        fi

        if test -r /proc/cpuinfo ; then  # Linux
          pov_cpuinfo=/proc/cpuinfo
        elif test -r /compat/linux/proc/cpuinfo ; then # FreeBSD using linprocfs
          pov_cpuinfo=/compat/linux/proc/cpuinfo
        elif test "`dmesg 2> /dev/null`"; then  # FreeBSD 4.9 at least
          pov_modelname=`dmesg | grep CPU | $head_1 | cut -d: -f2 | tr A-Z a-z`
          pov_cpuflags=`dmesg | grep Features | $head_1 | cut -d'<' -f2 | sed 's/[,>]/ /g' | tr A-Z a-z`
          pov_vendorid=`dmesg | grep Origin | $head_1 | cut -d'"' -f2`
        fi
      fi

      if test "$pov_cpuinfo"; then
        pov_modelname=`cat $pov_cpuinfo | grep 'model name' | $head_1 | cut -d: -f2 | tr A-Z a-z`
        pov_cpuflags=`cat $pov_cpuinfo | grep flags | $head_1 | cut -d: -f2`
        pov_cpuflags=`echo $pov_cpuflags`  # remove leading/trailing blanks
        pov_vendorid=`cat $pov_cpuinfo | grep vendor_id | $head_1 | cut -d: -f2`
        pov_vendorid=`echo $pov_vendorid`
      fi

      # log values
      AC_TRY_COMMAND([echo pov_cpuinfo   = $pov_cpuinfo   > /dev/null])
      AC_TRY_COMMAND([echo pov_modelname = $pov_modelname > /dev/null])
      AC_TRY_COMMAND([echo pov_vendorid  = $pov_vendorid  > /dev/null])
      AC_TRY_COMMAND([echo pov_cpuflags  = $pov_cpuflags  > /dev/null])

      for flag in $pov_cpuflags ; do
        case "$flag" in
          sse)
            pov_flags="-m$flag"
            POV_PROG_CXX_FLAGS([$pov_flags],
              [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
            )
            pov_flags="-mfpmath=$flag"
            POV_PROG_CXX_FLAGS([$pov_flags],
              [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
            )
            ;;
          sse?)  # sse2, sse3
            pov_flags="-m$flag"
            POV_PROG_CXX_FLAGS([$pov_flags],
              [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
            )
            ;;
        esac
      done

      if test -z "$pov_arch"; then
        case "$host" in
          i486-*)         pov_arch="i486";;
          i586-*)         pov_arch="pentium";;  # for GCC / ICC
          i686-*|i386-*)  # FreeBSD seems to only report i386
            pov_cpu=${pov_modelname:-`uname -p 2> /dev/null`}
            case "$pov_cpu" in
              # Intel
              *pentium*4*|*xeon*)  pov_arch="pentium4";;  # for GCC / ICC
              *pentium*mmx*)       pov_arch="pentium";;   # catch this first
              *pentium*m*)
                if test x"$CXX" = x"icc"; then
                  pov_arch="pentium4"
                else
                  pov_arch="pentium-m"
                fi
                ;;
              *pentium*iii*|*celeron*)  # assume all celerons belong here
                if test x"$CXX" = x"icc"; then
                  pov_arch="pentiumiii"
                else
                  pov_arch="pentium3"
                fi
                ;;
              *pentium*ii*)
                if test x"$CXX" = x"icc"; then
                  pov_arch="pentiumii"
                else
                  pov_arch="pentium2"
                fi
                ;;
              *pentium*pro*)       pov_arch="pentiumpro";;  # for GCC / ICC
              *pentium*)           pov_arch="pentium";;     # for GCC / ICC
              # AMD
              *athlon*xp*)         pov_arch="athlon-xp";;
              *athlon*mp*)         pov_arch="athlon-mp";;
              *athlon*|*duron*)    pov_arch="athlon";;
              # others
              *)                   pov_arch="i686";;  # don't support true i386
            esac
            pov_arch_fallback="i686"
            ;;
          k6-*)           pov_arch="k6"; pov_arch_fallback="i686";;
          k7-*)           pov_arch="k7"; pov_arch_fallback="i686";;
          k8-*|x86_64-*)  pov_arch="k8"; pov_arch_fallback="i686";;
          *)              pov_arch="unknown";;
        esac
      fi
      if test x"$pov_arch" != x"unknown"; then
        POV_PROG_CXX_FLAGS([-march=$pov_arch -mtune=$pov_arch],
          [
            if test "$subdirs"; then
              {
                POV_PROG_CC_FLAGS([-march=$pov_arch -mtune=$pov_arch],
                  [],
                  [POV_PROG_CC_FLAGS([-march=$pov_arch -mcpu=$pov_arch])]
                )
              }
            fi
          ],
          [
            POV_PROG_CXX_FLAGS([-march=$pov_arch -mcpu=$pov_arch],
              [
                if test "$subdirs"; then
                  { POV_PROG_CC_FLAGS([-march=$pov_arch -mcpu=$pov_arch]) }
                fi
              ],
              [
                POV_PROG_CXX_FLAGS([-march=$pov_arch],
                  [
                    if test "$subdirs"; then
                      { POV_PROG_CC_FLAGS([-march=$pov_arch]) }
                    fi
                  ],
                  [
                    if test "$pov_arch_fallback"; then
                      {
                        POV_PROG_CXX_FLAGS([-march=$pov_arch_fallback],
                          [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_arch_fallback]) }; fi]
                        )
                      }
                    fi
                  ]
                )
              ]
            )
          ]
        )
      fi

      # gcc says -malign-double makes no sense in the 64bit mode and reject it
      for pov_flags in "-malign-double" "-minline-all-stringops"; do
        POV_PROG_CXX_FLAGS([$pov_flags],
          [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
        )
      done
      ;;

    *irix*)
      pov_flags="-mips4"
      POV_PROG_CXX_FLAGS([$pov_flags],
        [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi],
        [
          pov_flags="-mips2"
          POV_PROG_CXX_FLAGS([$pov_flags],
            [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
          )
        ]
      )
      ;;

    powerpc*)
      ### Suggested by Thorsten Froehlich <thorsten@trf.de> :
      # Note that the different parameters for mcpu and mtune are intentional:
      # The PPC 7450 processor has a much longer pipeline and thus scheduling
      # for that pipeline always yields good results even on processors with
      # a shorter pipeline.  On the other hand, most PowerPC processors only
      # support the instructions also found in the PPC 750 processor (aka G3).
      # That is why it is selected in the mcpu parameter.  Using 7450 (aka G4+)
      # in mcpu would make the code incompatible with anything prior to the
      # PPC 7400 (aka G4).  To be precise, the vector instructions are new
      # in the four digit model number PowerPC processors (aka G4), but gcc
      # cannot (as of February 2003) use them automatically anyway! [trf]
      for pov_flags in "-mpowerpc" "-mcpu=750 -mtune=7450" "-mmultiple" "-mstring" "-mfused-madd"; do
        POV_PROG_CXX_FLAGS([$pov_flags],
          [if test "$subdirs"; then { POV_PROG_CC_FLAGS([$pov_flags]) }; fi]
        )
      done
      ;;

    sparc-*)
      # Commented out for now.
      #POV_PROG_CXX_FLAGS([-march=ultrasparc])
      #POV_PROG_CXX_FLAGS([-mvis])
      ;;

  esac

else

  AC_MSG_RESULT([no])

fi


# Add flags specified at the command line.
CXXFLAGS="$CXXFLAGS $pov_comp_save_cxxflags"

# Flags for supporting libraries.
CFLAGS="$CFLAGS $pov_comp_save_cflags"
export CFLAGS CPPFLAGS LDFLAGS


###############################################################################

AC_MSG_RESULT([
Makefiles
---------])

# Supporting libraries to be included in the compilation/linking process.
AC_SUBST([POVLIBS])
if test "$LIBZ"; then
  POVLIBS="zlib"
fi
if test "$LIBPNG"; then
  POVLIBS="$POVLIBS png"
fi
if test "$LIBJPEG"; then
  POVLIBS="$POVLIBS jpeg"
fi
# Does not compile all in tiff/ but only the library itself.
# This is required to avoid errors due to some dependencies in tiff/tools .
if test "$LIBTIFF"; then
  POVLIBS="$POVLIBS tiff/port tiff/libtiff"
fi

# For 'make distclean' to clean also the supporting libraries.
# It is different from POVLIBS since tiff/libtiff has no distclean target.
POVLIBSCLEAN=`echo $POVLIBS | sed -e 's,tiff/port,,g' -e 's,/libtiff,,g'`
AC_SUBST([POVLIBSCLEAN])

AC_CONFIG_FILES([\
  Makefile \
  libraries/Makefile \
  source/base/Makefile \
  source/frontend/Makefile \
  source/Makefile \
  unix/Makefile \
])


###############################################################################

if test "$subdirs"; then
  AC_CONFIG_COMMANDS([config.log],
    [AC_MSG_RESULT([
Supporting libraries
--------------------])]
  )
fi
AC_OUTPUT


###############################################################################

if test x"$enable_io_restrictions" = x"no"; then
  pov_io_msg="disabled"
else
  pov_io_msg="enabled"
fi
AC_MSG_RESULT([
===============================================================================
$PACKAGE_NAME $PACKAGE_VERSION has been configured with the following features:
  I/O restrictions: ${pov_io_msg}
  X Window display: ${pov_xwin_msg}
  SVGAlib display : ${pov_svga_msg}

Type 'make' to build, and 'make install' to install all files in the hierarchy
${prefix}])

if test "$pov_warn_path"; then
  echo "
WARNING: configure has detected that the list of search paths specified
by the following environment variables
 $pov_warn_path
erroneously contain the \".\" directory, which may cause a build failure.
In order to build POV-Ray successfully, the package has been configured
with all \".\" directory entries removed from the variables above.  You're
strongly advised to correct your configuration accordingly."
fi

AC_MSG_RESULT(
[===============================================================================
])
