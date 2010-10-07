/****************************************************************************
 *                  texture.h
 *
 * This file contains defines and variables for the txt*.cpp files
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
 * $File: //depot/povray/3.6-release/source/texture.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

/* NOTE: FRAME.H contains other texture stuff. */

#ifndef TEXTURE_H
#define TEXTURE_H

#include "pattern.h"
#include "warps.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define RNDMASK       0x7FFF
#define RNDMULTIPLIER ((DBL)0.000030518509476)

/*
 * Macro to create random number in the [0; 1] range.
 */

#define FRAND() ((DBL)POV_RAND()*RNDMULTIPLIER)

#define FLOOR(x)  ((x) >= 0.0 ? floor(x) : (0.0 - floor(0.0 - (x)) - 1.0))

#define Hash3d(a,b,c) \
  hashTable[(int)(hashTable[(int)(hashTable[(int)((a) & 0xfff)] ^ ((b) & 0xfff))] ^ ((c) & 0xfff))]


/*****************************************************************************
* Global typedefs
******************************************************************************/

/*****************************************************************************
* Global variables
******************************************************************************/

#ifdef DYNAMIC_HASHTABLE
extern unsigned short *hashTable;
#else
extern unsigned short hashTable[8192];
#endif

extern DBL *frequency;               /* dmf */
extern unsigned int Number_Of_Waves; /* dmf */
extern VECTOR *Wave_Sources;         /* dmf */



/*****************************************************************************
* Global functions
******************************************************************************/

void Compute_Colour (COLOUR Colour,PIGMENT *Pigment, DBL value);
void Initialize_Noise (void);
void Free_Noise_Tables (void);
INLINE_NOISE DBL Noise (VECTOR EPoint,TPATTERN *TPat);
INLINE_NOISE void DNoise (VECTOR result, VECTOR EPoint);
DBL Turbulence (VECTOR EPoint, TURB *Turb,TPATTERN *TPat);
void DTurbulence (VECTOR result, VECTOR EPoint, TURB *Turb);
DBL cycloidal (DBL value);
DBL Triangle_Wave (DBL value);
void Transform_Textures (TEXTURE *Textures, TRANSFORM *Trans);
void Destroy_Textures (TEXTURE *Textures);
void Post_Textures (TEXTURE *Textures);
FINISH *Create_Finish (void);
FINISH *Copy_Finish (FINISH *Old);
TEXTURE *Create_PNF_Texture (void);
TEXTURE *Copy_Texture_Pointer (TEXTURE *Texture);
TEXTURE *Copy_Textures (TEXTURE *Textures);
TEXTURE *Create_Texture (void);
int Test_Opacity (TEXTURE *Texture);
TURB *Create_Turb (void);
int POV_RAND (void);
void POV_SRAND (int seed);
int POV_GET_OLD_RAND(void);

END_POV_NAMESPACE

#endif
