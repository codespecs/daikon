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
noinst_LIBRARIES = libbase.a

# Source files.
libbase_a_SOURCES = \
configbase.h fileinputoutput.cpp fileinputoutput.h platformbase.h pointer.h pov_err.h povms.cpp povmscpp.cpp povmscpp.h povmsgid.h povms.h processoptions.cpp processoptions.h stringutilities.cpp stringutilities.h textstreambuffer.cpp textstreambuffer.h textstream.cpp textstream.h

# Include paths for headers.
# INCZ and INCPNG required in case "png.h" is included in config.h.
AM_CPPFLAGS = \
  -I$(top_srcdir) \
  -I$(top_srcdir)/source \
  -I$(top_srcdir)/unix \
  @INCZ@ @INCPNG@
