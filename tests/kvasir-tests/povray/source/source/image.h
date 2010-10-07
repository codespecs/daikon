/****************************************************************************
 *                  image.h
 *
 * This module contains all defines, typedefs, and prototypes for IMAGE.CPP.
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
 * $File: //depot/povray/3.6-release/source/image.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef IMAGE_H
#define IMAGE_H

#include "hfield.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

/* Image/Bump Map projection types. */

#define PLANAR_MAP      0
#define SPHERICAL_MAP   1
#define CYLINDRICAL_MAP 2
#define PARABOLIC_MAP   3
#define HYPERBOLIC_MAP  4
#define TORUS_MAP       5
#define PIRIFORM_MAP    6
#define OLD_MAP         7

/* Bit map interpolation types. */

#define NO_INTERPOLATION 0
#define NEAREST_NEIGHBOR 1
#define BILINEAR         2
#define CUBIC_SPLINE     3
#define NORMALIZED_DIST  4


/* NK 1998 - instead of Use_Colour_Flag */
#define USE_INDEX  0
#define USE_COLOUR 1
#define USE_ALPHA  2
/* USE_IMAGE = TRUE = 1
   USE_INDEX = FALSE = 0 */
/* NK ---- */


/*****************************************************************************
* Global typedefs
******************************************************************************/




/*****************************************************************************
* Global variables
******************************************************************************/




/*****************************************************************************
* Global functions
******************************************************************************/

DBL image_pattern (VECTOR EPoint, TPATTERN *TPattern);
int image_map (VECTOR EPoint, PIGMENT *Pigment, COLOUR colour);
TEXTURE *material_map (VECTOR IPoint, TEXTURE *Texture);
void bump_map (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal);
HF_VAL image_height_at(IMAGE *Image, int x, int y);
bool is_image_opaque(IMAGE *Image);
IMAGE *Copy_Image (IMAGE *Old);
IMAGE *Create_Image (void);
void Destroy_Image (IMAGE *Image);

END_POV_NAMESPACE

#endif
