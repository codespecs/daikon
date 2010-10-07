/****************************************************************************
 *                  pattern.h
 *
 * This module contains all defines, typedefs, and prototypes for PATTERN.CPP.
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
 * $File: //depot/povray/3.6-release/source/pattern.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

/* NOTE: FRAME.H contains other pattern stuff. */

#ifndef PATTERN_H
#define PATTERN_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define LAST_SPECIAL_PATTERN     BITMAP_PATTERN
#define LAST_NORM_ONLY_PATTERN   DENTS_PATTERN
#define LAST_INTEGER_PATTERN     HEXAGON_PATTERN

enum PATTERN_IDS
{
  NO_PATTERN = 0,
  PLAIN_PATTERN,
  AVERAGE_PATTERN,
  UV_MAP_PATTERN,
  BITMAP_PATTERN,
/* These former normal patterns require special handling.  They too
   must be kep seperate for now.*/

  WAVES_PATTERN,
  RIPPLES_PATTERN,
  WRINKLES_PATTERN,
  BUMPS_PATTERN,
/*  BUMPY1_PATTERN,
  BUMPY2_PATTERN,
  BUMPY3_PATTERN,*/
  QUILTED_PATTERN,
  FACETS_PATTERN,
  DENTS_PATTERN,
/* These patterns return integer values.  They must be kept
   together in the list.  Any new integer functions added must be added
   here and the list renumbered. */

  CHECKER_PATTERN,
  OBJECT_PATTERN,
  BRICK_PATTERN,
  HEXAGON_PATTERN,
/* These patterns return float values.  They must be kept together
   and seperate from those above. */

  BOZO_PATTERN,
  CELLS_PATTERN,
  MARBLE_PATTERN,
  WOOD_PATTERN,
  SPOTTED_PATTERN,
  AGATE_PATTERN,
  GRANITE_PATTERN,
  GRADIENT_PATTERN,
/*  PATTERN1_PATTERN,
  PATTERN2_PATTERN,
  PATTERN3_PATTERN,*/
  ONION_PATTERN,
  LEOPARD_PATTERN,
  JULIA_PATTERN,
  JULIA3_PATTERN,
  JULIA4_PATTERN,
  JULIAX_PATTERN,
  MANDEL_PATTERN,
  MANDEL3_PATTERN,
  MANDEL4_PATTERN,
  MANDELX_PATTERN,
  MAGNET1M_PATTERN,
  MAGNET1J_PATTERN,
  MAGNET2M_PATTERN,
  MAGNET2J_PATTERN,
  RADIAL_PATTERN,
  CRACKLE_PATTERN,
  SPIRAL1_PATTERN,
  SPIRAL2_PATTERN,
  PLANAR_PATTERN,
  SPHERICAL_PATTERN,
  BOXED_PATTERN,
  CYLINDRICAL_PATTERN,
  DENSITY_FILE_PATTERN,
  FUNCTION_PATTERN, 
  SLOPE_PATTERN,
  PIGMENT_PATTERN,
  IMAGE_PATTERN
};

/* Pattern flags */

#define NO_FLAGS              0
#define HAS_FILTER            1
#define FULL_BLOCKING         2
#define POST_DONE             4
#define DONT_SCALE_BUMPS_FLAG 8 /* scale bumps for normals */
#define NOISE_FLAG_1         16 /* this flag and the next one work together */
#define NOISE_FLAG_2         32 /* neither=default, 1=orig,2=range,3=perlin */
#define NOISE_FLAGS         NOISE_FLAG_1+NOISE_FLAG_2

#define Destroy_Turb(t) if ((t)!=NULL) POV_FREE(t);

#define RAMP_WAVE     0
#define SINE_WAVE     1
#define TRIANGLE_WAVE 2
#define SCALLOP_WAVE  3
#define CUBIC_WAVE    4
#define POLY_WAVE     5

/* Interpolation types. */

#define NO_INTERPOLATION        0
#define TRILINEAR_INTERPOLATION 1
#define TRICUBIC_INTERPOLATION  2  


/*****************************************************************************
* Global typedefs
******************************************************************************/


/*****************************************************************************
* Global variables
******************************************************************************/


/*****************************************************************************
* Global constants
******************************************************************************/


/*****************************************************************************
* Global functions
******************************************************************************/

DBL Evaluate_TPat (TPATTERN *TPat, VECTOR EPoint, INTERSECTION *Isection);
void Init_TPat_Fields (TPATTERN *Tpat);
void Copy_TPat_Fields (TPATTERN *New, TPATTERN *Old);
void Destroy_TPat_Fields (TPATTERN *Tpat);
void Translate_Tpattern (TPATTERN *Tpattern, VECTOR Vector);
void Rotate_Tpattern (TPATTERN *Tpattern, VECTOR Vector);
void Scale_Tpattern (TPATTERN *Tpattern, VECTOR Vector);
void Transform_Tpattern (TPATTERN *Tpattern, TRANSFORM *Trans);
DBL quilt_cubic (DBL t,DBL p1,DBL p2);
void Search_Blend_Map (DBL value,BLEND_MAP *Blend_Map,
                       BLEND_MAP_ENTRY **Prev, BLEND_MAP_ENTRY **Cur);

DENSITY_FILE *Create_Density_File (void);
DENSITY_FILE *Copy_Density_File (DENSITY_FILE *);
void Destroy_Density_File (DENSITY_FILE *);
void Read_Density_File (DENSITY_FILE *df);
int PickInCube (VECTOR tv, VECTOR p1);

END_POV_NAMESPACE

#endif
