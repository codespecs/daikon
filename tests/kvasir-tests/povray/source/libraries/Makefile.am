##############################################################################
#               Makefile.am
#
# Process this file with automake to produce Makefile.in
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

# Makefile.am for the source distribution of POV-Ray 3.6 for UNIX
# Written by Nicolas Calimet <pov4grasp@free.fr>

# Libraries to build.
SUBDIRS = @POVLIBS@

# Libraries to clean with 'make distclean'.
DIST_SUBDIRS = @POVLIBSCLEAN@

# Don't recurse in the subdirectories for 'make maintainer-clean'
# since some libraries might not have this target (e.g. libtiff).
# These subdirectories should be cleaned first by 'make clean'.
maintainer-clean: DIST_SUBDIRS=

# Create an empty target for 'make dist'; ignore automake warning about it.
# The whole directory is distributed through EXTRA_DIST at the top level.
distdir:
	@:

# Replaces the install target for 'make install'.
install: all

# Creates an empty install target for 'make uninstall'.
uninstall:

# Don't recurse in the subdirectories for 'make check'.
check: SUBDIRS=
