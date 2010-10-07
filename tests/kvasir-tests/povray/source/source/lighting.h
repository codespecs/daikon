/****************************************************************************
 *                  lighting.h
 *
 * This module contains all defines, typedefs, and prototypes for LIGHTING.CPP.
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
 * $File: //depot/povray/3.6-release/source/lighting.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef LIGHTING_H
#define LIGHTING_H

#include "point.h"

BEGIN_POV_NAMESPACE


/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

/* How many subrays to trace for dispersive media */
#define DEFAULT_DISP_NELEMS  7



/*****************************************************************************
* Global typedefs
******************************************************************************/
/*
 * List to store light colours during shadow testing
 * to avoid repeated testing with layered textures.
 */

typedef struct Light_Tested_Struct LIGHT_TESTED;

struct Light_Tested_Struct
{
  int    Tested;
  COLOUR Colour;
};




/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

void Determine_Apparent_Colour (INTERSECTION *Ray_Intersection, COLOUR Colour, RAY *Ray, DBL Weight);
void Initialize_Lighting_Code (void);
void Reinitialize_Lighting_Code (int Number_Of_Entries, TEXTURE ***Textures, DBL **Weights);
void Deinitialize_Lighting_Code (void);
int Test_Shadow (LIGHT_SOURCE *Light, DBL *Depth, RAY *Light_Source_Ray, RAY *Eye_Ray, VECTOR P, COLOUR Colour);

void do_diffuse (FINISH *Finish, RAY *Light_Source_Ray,
  VECTOR Layer_Normal, COLOUR Colour, COLOUR Light_Colour,
  COLOUR Layer_Pigment_Colour, DBL Attenuation);

void do_phong (FINISH *Finish, RAY *Light_Source_Ray,
  VECTOR Eye, VECTOR Layer_Normal, COLOUR Colour, COLOUR Light_Colour,
  COLOUR Layer_Texture_Colour);

void do_specular (FINISH *Finish, RAY *Light_Source_Ray,
  VECTOR REye, VECTOR Layer_Normal, COLOUR Colour, COLOUR Light_Colour,
  COLOUR Layer_Pigment_Colour);

void ResizeMediaMallocPools(long newSize);
void ResizeLightMallocPools(long newSize);

END_POV_NAMESPACE

#endif
