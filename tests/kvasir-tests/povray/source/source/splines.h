/****************************************************************************
 *                  splines.h
 *
 * Contributed by ???
 *
 * This module contains all defines, typedefs, and prototypes for splines.cpp.
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
 * $File: //depot/povray/3.6-release/source/splines.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef SPLINE_H
#define SPLINE_H
/* Generic header for spline modules */

#include "frame.h"

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "config.h"

BEGIN_POV_NAMESPACE

#define INIT_SPLINE_SIZE     16

#define LINEAR_SPLINE         1
#define QUADRATIC_SPLINE      2
#define NATURAL_SPLINE        3
#define CATMULL_ROM_SPLINE    4

SPLINE * Create_Spline(int Type);
SPLINE * Copy_Spline(SPLINE * Old);
void Destroy_Spline(SPLINE * Spline);
void Insert_Spline_Entry(SPLINE * Spline, DBL p, EXPRESS v);
DBL Get_Spline_Val(SPLINE * sp, DBL p, EXPRESS v, int *Terms);

END_POV_NAMESPACE

#endif

