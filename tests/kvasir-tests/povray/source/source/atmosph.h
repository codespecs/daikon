/****************************************************************************
 *                  atmoshp.h
 *
 * This module contains all defines, typedefs, and prototypes for ATMOSPH.CPP.
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
 * $File: //depot/povray/3.6-release/source/atmosph.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef ATMOSPH_H
#define ATMOSPH_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

/* Define fog types. DMF */

#define ORIG_FOG    1
#define GROUND_MIST 2
#define FOG_TYPES   2

/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Fog_Struct FOG;
typedef struct Rainbow_Struct RAINBOW;
typedef struct Skysphere_Struct SKYSPHERE;

struct Fog_Struct
{
  int Type;
  DBL Distance;
  DBL Alt;
  DBL Offset;
  COLOUR Colour;
  VECTOR Up;
  TURB *Turb;
  SNGL Turb_Depth;
  FOG *Next;
};

struct Rainbow_Struct
{
  DBL Distance;
  DBL Jitter;
  DBL Angle, Width;
  DBL Arc_Angle, Falloff_Angle, Falloff_Width;
  VECTOR Antisolar_Vector;
  VECTOR Up_Vector, Right_Vector;
  PIGMENT *Pigment;
  RAINBOW *Next;
};

struct Skysphere_Struct
{
  int Count;           /* Number of pigments.       */
  PIGMENT **Pigments;  /* Pigment(s) to use.        */
  TRANSFORM *Trans;    /* Skysphere transformation. */
};



/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

void Initialize_Atmosphere_Code (void);
void Deinitialize_Atmosphere_Code (void);

void Do_Infinite_Atmosphere (RAY *Ray, COLOUR Colour);
void Do_Finite_Atmosphere (RAY *Ray, INTERSECTION *Intersection, COLOUR Colour, int Light_Source_Flag);

FOG *Create_Fog (void);
FOG *Copy_Fog (FOG *Fog);
void Destroy_Fog (FOG *Fog);

RAINBOW *Create_Rainbow (void);
RAINBOW *Copy_Rainbow (RAINBOW *Rainbow);
void Destroy_Rainbow (RAINBOW *Rainbow);

SKYSPHERE *Create_Skysphere (void);
SKYSPHERE *Copy_Skysphere (SKYSPHERE *Skysphere);
void Destroy_Skysphere (SKYSPHERE *Skysphere);
void Scale_Skysphere (SKYSPHERE *Skysphere, VECTOR Vector);
void Rotate_Skysphere (SKYSPHERE *Skysphere, VECTOR Vector);
void Translate_Skysphere (SKYSPHERE *Skysphere, VECTOR Vector);
void Transform_Skysphere (SKYSPHERE *Skysphere, TRANSFORM *Trans);

END_POV_NAMESPACE

#endif
