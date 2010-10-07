/****************************************************************************
 *                  lathe.h
 *
 * This module contains all defines, typedefs, and prototypes for LATHE.CPP.
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
 * $File: //depot/povray/3.6-release/source/lathe.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef LATHE_H
#define LATHE_H

#include "bcyl.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor definitions
******************************************************************************/

#define LATHE_OBJECT (STURM_OK_OBJECT)

#define LINEAR_SPLINE    1
#define QUADRATIC_SPLINE 2
#define CUBIC_SPLINE     3
#define BEZIER_SPLINE    4

/* Generate additional lathe statistics. */

#define LATHE_EXTRA_STATS 1



/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Lathe_Struct LATHE;
typedef struct Lathe_Spline_Struct LATHE_SPLINE;
typedef struct Lathe_Spline_Entry_Struct LATHE_SPLINE_ENTRY;

struct Lathe_Spline_Entry_Struct
{
  UV_VECT A, B, C, D;  /* Coefficients of segment */
};

struct Lathe_Spline_Struct
{
  int References;             /* Count references to this structure. */
  LATHE_SPLINE_ENTRY *Entry;  /* Array of spline segments.           */
  BCYL *BCyl;                 /* bounding cylinder.                  */
};

struct Lathe_Struct
{
  OBJECT_FIELDS
  int Spline_Type;          /* Spline type (linear, quadratic ...)  */
  int Number;               /* Number of segments!!!                */
  LATHE_SPLINE *Spline;     /* Pointer to spline array              */
  DBL Height1, Height2;     /* Min./Max. height                     */
  DBL Radius1, Radius2;     /* Min./Max. radius                     */
};



/*****************************************************************************
* Global variables
******************************************************************************/




/*****************************************************************************
* Global functions
******************************************************************************/

void  Compute_Lathe_BBox (LATHE *Lathe);
void  Compute_Lathe (LATHE *Lathe, UV_VECT *P);
LATHE *Create_Lathe (void);

END_POV_NAMESPACE

#endif
