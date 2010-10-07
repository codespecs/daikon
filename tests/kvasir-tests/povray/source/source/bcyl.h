/****************************************************************************
 *                  bcyl.h
 *
 * This module contains all defines, typedefs, and prototypes for BCYL.C.
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
 * $File: //depot/povray/3.6-release/source/bcyl.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef BCYL_H
#define BCYL_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

/* Generate additional bcyl statistics. */

#define BCYL_EXTRA_STATS 1


/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct BCyl_Struct BCYL;
typedef struct BCyl_Entry_Struct BCYL_ENTRY;
typedef struct BCyl_Intersection_Struct BCYL_INT;

struct BCyl_Intersection_Struct
{
  int n;     /* Number of cylinder hit    */
  DBL d[2];  /* Intersection distance(s)  */
  DBL w[2];  /* Intersection parameter(s) */
};

struct BCyl_Entry_Struct
{
  short r1, r2;        /* Index of min/max segment radius */
  short h1, h2;        /* Index of min/max segmnet height */
};

struct BCyl_Struct
{
  int number;          /* Number of bounding cylinders.       */
  short nradius;       /* Number of different bound-radii.    */
  short nheight;       /* Number of different bound-heights.  */
  DBL *radius;         /* List of different bound-radii.      */
  DBL *height;         /* List of different bound-heights.    */
  BCYL_INT *rint;      /* BCyl intersections list.            */
  BCYL_INT *hint;      /* BCyl intersections list.            */
  BCYL_INT *intervals; /* BCyl intersection intervals.        */
  BCYL_ENTRY *entry;   /* BCyl elements.                      */
};



/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

BCYL *Create_BCyl (int, DBL *, DBL *, DBL *, DBL *);
void Destroy_BCyl (BCYL *);

int  Intersect_BCyl (BCYL *BCyl, VECTOR P, VECTOR D);

END_POV_NAMESPACE

#endif
