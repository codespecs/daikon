/****************************************************************************
 *                  triangle.h
 *
 * This module contains all defines, typedefs, and prototypes for TRIANGLE.CPP.
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
 * $File: //depot/povray/3.6-release/source/triangle.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef TRIANGLE_H
#define TRIANGLE_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define TRIANGLE_OBJECT        (PATCH_OBJECT)
#define SMOOTH_TRIANGLE_OBJECT (PATCH_OBJECT)
/* NK 1998 double_illuminate - removed +DOUBLE_ILLUMINATE from smooth_triangle */


/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Triangle_Struct TRIANGLE;
typedef struct Smooth_Triangle_Struct SMOOTH_TRIANGLE;
typedef struct Smooth_Color_Triangle_Struct SMOOTH_COLOR_TRIANGLE; /* AP */

struct Triangle_Struct
{
  OBJECT_FIELDS
  VECTOR  Normal_Vector;
  DBL     Distance;
  unsigned int  Dominant_Axis:2;
  unsigned int  vAxis:2;  /* used only for smooth triangles */
  VECTOR  P1, P2, P3;
};

struct Smooth_Triangle_Struct
{
  OBJECT_FIELDS
  VECTOR  Normal_Vector;
  DBL     Distance;
  unsigned int  Dominant_Axis:2;
  unsigned int  vAxis:2;         /* used only for smooth triangles */
  VECTOR  P1, P2, P3;
  VECTOR  N1, N2, N3, Perp;
};

struct Smooth_Color_Triangle_Struct /* AP */
{
  OBJECT_FIELDS
  VECTOR  Normal_Vector;
  DBL     Distance;
  unsigned int  Dominant_Axis:2;
  unsigned int  vAxis:2;         /* used only for smooth triangles */
  VECTOR  P1, P2, P3;
  VECTOR  N1, N2, N3, Perp;
  int magic;
  COLOUR  C1, C2, C3;
};


/*****************************************************************************
* Global variables
******************************************************************************/

extern METHODS Triangle_Methods;
extern METHODS Smooth_Triangle_Methods;



/*****************************************************************************
* Global functions
******************************************************************************/

TRIANGLE *Create_Triangle (void);
SMOOTH_TRIANGLE *Create_Smooth_Triangle (void);
int Compute_Triangle  (TRIANGLE *Triangle, int Smooth);
void Compute_Triangle_BBox (TRIANGLE *Triangle);
/* AP */
DBL Calculate_Smooth_T(VECTOR IPoint, VECTOR P1, VECTOR P2, VECTOR P3);

END_POV_NAMESPACE

#endif
