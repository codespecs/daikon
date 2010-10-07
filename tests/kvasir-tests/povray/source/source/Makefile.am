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

# Directories to build.
SUBDIRS = base frontend

# Libraries to build.
noinst_LIBRARIES = libpovray.a

# Source files.
libpovray_a_SOURCES = \
atmosph.cpp atmosph.h bbox.cpp bbox.h bcyl.cpp bcyl.h benchmark.cpp benchmark.h bezier.cpp bezier.h blob.cpp blob.h boxes.cpp boxes.h bsphere.cpp bsphere.h camera.cpp camera.h chi2.cpp chi2.h colour.cpp colour.h colutils.cpp colutils.h cones.cpp cones.h csg.cpp csg.h discs.cpp discs.h express.cpp express.h fncode.cpp fncode.h fnintern.cpp fnintern.h fnpovfpu.cpp fnpovfpu.h fnsyntax.cpp fnsyntax.h fpmetric.cpp fpmetric.h fractal.cpp fractal.h frame.h function.cpp function.h gif.cpp gifdecod.cpp gifdecod.h gif.h hcmplx.cpp hcmplx.h hfield.cpp hfield.h histogra.cpp histogra.h iff.cpp iff.h image.cpp image.h interior.cpp interior.h isosurf.cpp isosurf.h jpeg_pov.cpp jpeg_pov.h lathe.cpp lathe.h lbuffer.cpp lbuffer.h lightgrp.cpp lightgrp.h lighting.cpp lighting.h mathutil.cpp mathutil.h matrices.cpp matrices.h media.cpp media.h mesh.cpp mesh.h normal.cpp normal.h objects.cpp objects.h octree.cpp octree.h optout.cpp optout.h parse.cpp parse.h parsestr.cpp parsestr.h parstxtr.cpp parstxtr.h pattern.cpp pattern.h pgm.cpp pgm.h photons.cpp photons.h pigment.cpp pigment.h planes.cpp planes.h png_pov.cpp png_pov.h point.cpp point.h poly.cpp polygon.cpp polygon.h poly.h polysolv.cpp polysolv.h pov_mem.cpp pov_mem.h povmsend.cpp povmsend.h povmsrec.cpp povmsrec.h povray.cpp povray.h pov_util.cpp pov_util.h ppm.cpp ppm.h prism.cpp prism.h quadrics.cpp quadrics.h quatern.cpp quatern.h rad_data.cpp radiosit.cpp radiosit.h ray.cpp ray.h rendctrl.cpp rendctrl.h render.cpp render.h renderio.cpp renderio.h sor.cpp sor.h spheres.cpp spheres.h sphsweep.cpp sphsweep.h splines.cpp splines.h statspov.cpp statspov.h super.cpp super.h targa.cpp targa.h texture.cpp texture.h tiff_pov.cpp tiff_pov.h tokenize.cpp tokenize.h torus.cpp torus.h triangle.cpp triangle.h truetype.cpp truetype.h txttest.cpp txttest.h userdisp.cpp userdisp.h userio.cpp userio.h vbuffer.cpp vbuffer.h vector.h vlbuffer.cpp vlbuffer.h warps.cpp warps.h

# Include paths for headers.
AM_CPPFLAGS = \
  -I$(top_srcdir) \
  -I$(top_srcdir)/source/base \
  -I$(top_srcdir)/source/frontend \
  -I$(top_srcdir)/unix \
  @INCZ@ @INCPNG@ @INCTIFF@ @INCJPEG@
