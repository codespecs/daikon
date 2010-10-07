/****************************************************************************
 *               statspov.cpp
 *
 * This module implements the render statistics gathering and output functions.
 *
 * from Persistence of Vision(tm) Ray Tracer version 3.6.
 * Copyright 1991-2003 Persistence of Vision Team
 * Copyright 2003-2004 Persistence of Vision Raytracer Pty. Ltd.
 *---------------------------------------------------------------------------
 * NOTICE: This source code file is provided so that users may experiment
 * with enhancements to POV-Ray and to port the software to platforms other
 * than those supported by the POV-Ray developers. There are strict rules
 * regarding how you are permitted to use this file. These rules are contained
 * in the distribution and derivative versions licenses which should have been
 * provided with this file.
 *
 * These licences may be found online, linked from the end-user license
 * agreement that is located at http://www.povray.org/povlegal.html
 *---------------------------------------------------------------------------
 * This program is based on the popular DKB raytracer version 2.12.
 * DKBTrace was originally written by David K. Buck.
 * DKBTrace Ver 2.0-2.12 were written by David K. Buck & Aaron A. Collins.
 *---------------------------------------------------------------------------
 * $File: //depot/povray/3.6-release/source/statspov.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <ctype.h>
#include <time.h>
#include <stdarg.h>
#include "frame.h"
#include "vector.h"
#include "atmosph.h"
#include "bezier.h"
#include "blob.h"
#include "bbox.h"
#include "cones.h"
#include "csg.h"
#include "discs.h"
#include "fractal.h"
#include "hfield.h"
#include "lathe.h"
#include "lighting.h"
#include "mesh.h"
#include "polysolv.h"
#include "objects.h"
#include "parse.h"
#include "point.h"
#include "poly.h"
#include "polygon.h"
#include "octree.h"
#include "quadrics.h"
#include "pgm.h"
#include "ppm.h"
#include "prism.h"
#include "radiosit.h"
#include "render.h"
#include "sor.h"
#include "spheres.h"
#include "super.h"
#include "targa.h"
#include "texture.h"
#include "torus.h"
#include "triangle.h"
#include "truetype.h"
#include "userio.h"
#include "userdisp.h"
#include "lbuffer.h"
#include "vbuffer.h"
#include "povray.h"
#include "optout.h"
#include "povms.h"
#include "povmsgid.h"
#include "povmsend.h"
#include "statspov.h"
#include "pov_util.h"
#include "pov_err.h"

BEGIN_POV_NAMESPACE

USING_POV_BASE_NAMESPACE

/*****************************************************************************
*
* FUNCTION
*
*   init_statistics
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   Initialize statistics to 0
*
* CHANGES
*
*   -
*
******************************************************************************/

void init_statistics(COUNTER *pstats)
{
  int i;

  for(i=0; i<MaxStat; i++)
    Init_Counter(pstats[i]);
}

/*****************************************************************************
*
* FUNCTION
*
*   sum_statistics
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   Add current statistics to total statistics
*
* CHANGES
*
*   -
*
******************************************************************************/

void sum_statistics(COUNTER *ptotalstats, COUNTER *pstats)
{
  int i;
  COUNTER tmp;

  for(i=0; i<MaxStat; i++)
  {
    Add_Counter(tmp,pstats[i],ptotalstats[i]);
    ptotalstats[i]=tmp;
  }
}

END_POV_NAMESPACE
