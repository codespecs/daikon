/**************************************************************************
 *               pattern.cpp
 *
 * This module implements texturing functions that return a value to be
 * used in a pigment or normal.
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
 * $File: //depot/povray/3.6-release/source/pattern.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

/*
 * Some texture ideas garnered from SIGGRAPH '85 Volume 19 Number 3,
 * "An Image Synthesizer" By Ken Perlin.
 * Further Ideas Garnered from "The RenderMan Companion" (Addison Wesley).
 */

#include "frame.h"
#include "vector.h"
#include "matrices.h"
#include "pattern.h"
#include "povray.h"
#include "texture.h"
#include "image.h"
#include "txttest.h"
#include "colour.h"
#include "isosurf.h"
#include "pov_util.h"
#include "pigment.h"
#include "fnpovfpu.h"
#include "objects.h"

#include <algorithm>

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

#define CLIP_DENSITY(r) { if((r) < 0.0) { (r) = 1.0; } else if((r) > 1.0) { (r) = 0.0; } else { (r) = 1.0 - (r); } }

const int FRACTAL_MAX_EXPONENT = 33;


/*****************************************************************************
* Local variables
******************************************************************************/

bool BinomialCoefficientsInited = false; // GLOBAL VARIABLE
int BinomialCoefficients[((FRACTAL_MAX_EXPONENT+1)*(FRACTAL_MAX_EXPONENT+2))/2]; // GLOBAL VARIABLE


/*****************************************************************************
* Static functions
******************************************************************************/

static DBL agate_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL boxed_pattern (VECTOR EPoint);
static DBL brick_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL cells_pattern (VECTOR EPoint);
static DBL checker_pattern (VECTOR EPoint);
static DBL crackle_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL cylindrical_pattern (VECTOR EPoint);
static DBL dents_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL density_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL function_pattern (VECTOR EPoint, TPATTERN *TPat); // iso_surface - added
static DBL gradient_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL granite_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL hexagon_pattern (VECTOR EPoint);
static DBL julia_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL julia3_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL julia4_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL juliax_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL leopard_pattern (VECTOR EPoint);
static DBL magnet1m_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL magnet1j_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL magnet2m_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL magnet2j_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL mandel_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL mandel3_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL mandel4_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL mandelx_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL marble_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL object_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL onion_pattern (VECTOR EPoint);
static DBL pigment_pattern (VECTOR EPoint, TPATTERN *TPat, INTERSECTION *isect);
static DBL planar_pattern (VECTOR EPoint);
static DBL quilted_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL radial_pattern (VECTOR EPoint);
static DBL ripples_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL slope_pattern (VECTOR EPoint, TPATTERN *TPat, INTERSECTION *Intersection);
static DBL spiral1_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL spiral2_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL spherical_pattern (VECTOR EPoint);
static DBL waves_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL wood_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL wrinkles_pattern (VECTOR EPoint, TPATTERN *TPat);
static DBL object(VECTOR EPoint, TPATTERN *TPat);

static DBL fractal_exterior_color(TPATTERN *TPat, int iters, DBL a, DBL b);
static DBL fractal_interior_color(TPATTERN *TPat, int iters, DBL a, DBL b, DBL mindist2);
static TURB *Search_For_Turb(WARP *Warps);
static TURB *Copy_Turb(TURB *Old); // Unused function [AED]
static unsigned short readushort(IStream *infile);
static unsigned int readuint(IStream *infile);
static void InitializeBinomialCoefficients();


/*****************************************************************************
*
* FUNCTION
*
*   Evaluate_Pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   TPat   -- Texture pattern struct
*   Intersection - intersection structure
*   
* OUTPUT
*   
* RETURNS
*
*   DBL result usual 0.0 to 1.0 but may be 2.0 in hexagon
*   
* AUTHOR
*
*   Adapted from Add_Pigment by Chris Young
*   
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

DBL Evaluate_TPat (TPATTERN *TPat, VECTOR EPoint, INTERSECTION *Isection)
{
	DBL value = 0.0;

	/* NK 19 Nov 1999 removed Warp_EPoint call */

	switch(TPat->Type)
	{
		case AGATE_PATTERN:       value = agate_pattern      (EPoint, TPat);   break;
		case BOZO_PATTERN:
		case SPOTTED_PATTERN:
		case BUMPS_PATTERN:       value = Noise              (EPoint, TPat);   break;
		case BRICK_PATTERN:       value = brick_pattern      (EPoint, TPat);   break;
		case CELLS_PATTERN:       value = cells_pattern      (EPoint);         break;
		case CHECKER_PATTERN:     value = checker_pattern    (EPoint);         break;
		case CRACKLE_PATTERN:     value = crackle_pattern    (EPoint, TPat);   break;
		case GRADIENT_PATTERN:    value = gradient_pattern   (EPoint, TPat);   break;
		case GRANITE_PATTERN:     value = granite_pattern    (EPoint, TPat);   break;
		case HEXAGON_PATTERN:     value = hexagon_pattern    (EPoint);         break;
		case JULIA_PATTERN:       value = julia_pattern      (EPoint, TPat);   break;
		case JULIA3_PATTERN:      value = julia3_pattern     (EPoint, TPat);   break;
		case JULIA4_PATTERN:      value = julia4_pattern     (EPoint, TPat);   break;
		case JULIAX_PATTERN:      value = juliax_pattern     (EPoint, TPat);   break;
		case LEOPARD_PATTERN:     value = leopard_pattern    (EPoint);         break;
		case MAGNET1M_PATTERN:    value = magnet1m_pattern   (EPoint, TPat);   break;
		case MAGNET1J_PATTERN:    value = magnet1j_pattern   (EPoint, TPat);   break;
		case MAGNET2M_PATTERN:    value = magnet2m_pattern   (EPoint, TPat);   break;
		case MAGNET2J_PATTERN:    value = magnet2j_pattern   (EPoint, TPat);   break;
		case MANDEL_PATTERN:      value = mandel_pattern     (EPoint, TPat);   break;
		case MANDEL3_PATTERN:     value = mandel3_pattern    (EPoint, TPat);   break;
		case MANDEL4_PATTERN:     value = mandel4_pattern    (EPoint, TPat);   break;
		case MANDELX_PATTERN:     value = mandelx_pattern    (EPoint, TPat);   break;
		case MARBLE_PATTERN:      value = marble_pattern     (EPoint, TPat);   break;
		case ONION_PATTERN:       value = onion_pattern      (EPoint);         break;
		case RADIAL_PATTERN:      value = radial_pattern     (EPoint);         break;
		case SPIRAL1_PATTERN:     value = spiral1_pattern    (EPoint, TPat);   break;
		case SPIRAL2_PATTERN:     value = spiral2_pattern    (EPoint, TPat);   break;
		case WOOD_PATTERN:        value = wood_pattern       (EPoint, TPat);   break;
		case WAVES_PATTERN:       value = waves_pattern      (EPoint, TPat);   break;
		case RIPPLES_PATTERN:     value = ripples_pattern    (EPoint, TPat);   break;
		case WRINKLES_PATTERN:    value = wrinkles_pattern   (EPoint, TPat);   break;
		case DENTS_PATTERN:       value = dents_pattern      (EPoint, TPat);   break;
		case QUILTED_PATTERN:     value = quilted_pattern    (EPoint, TPat);   break;
		case FUNCTION_PATTERN:    value = function_pattern   (EPoint, TPat);   break;
		case PLANAR_PATTERN:      value = planar_pattern     (EPoint);         break;
		case BOXED_PATTERN:       value = boxed_pattern      (EPoint);         break;
		case SPHERICAL_PATTERN:   value = spherical_pattern  (EPoint);         break;
		case CYLINDRICAL_PATTERN: value = cylindrical_pattern(EPoint);         break;
		case DENSITY_FILE_PATTERN:value = density_pattern    (EPoint, TPat);   break;
		case IMAGE_PATTERN:       value = image_pattern      (EPoint, TPat);   break;
		case SLOPE_PATTERN:       value = slope_pattern      (EPoint, TPat, Isection); break;
		case PIGMENT_PATTERN:     value = pigment_pattern    (EPoint, TPat, Isection);   break;
		case OBJECT_PATTERN:      value = object_pattern     (EPoint, TPat);   break;

		default: Error("Problem in Evaluate_TPat.");
	}

	if(TPat->Frequency != 0.0)
		value = fmod(value * TPat->Frequency + TPat->Phase, 1.00001);

	/* allow negative Frequency */
	if(value < 0.0)
		value -= floor(value);

	switch(TPat->Wave_Type)
	{
		case RAMP_WAVE:
			break;
		case SINE_WAVE:
			value = (1.0 + cycloidal(value)) * 0.5;
			break;
		case TRIANGLE_WAVE:
			value = Triangle_Wave(value);
			break;
		case SCALLOP_WAVE:
			value = fabs(cycloidal(value * 0.5));
			break;
		case CUBIC_WAVE:
			value = Sqr(value) * ((-2.0 * value) + 3.0);
			break;
		case POLY_WAVE:
			value = pow(value, (DBL) TPat->Exponent);
			break;
		default:
			Error("Unknown Wave Type %d.", TPat->Wave_Type);
	}

	return value;
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void Init_TPat_Fields (TPATTERN *Tpat)
{
  Tpat->Type       = NO_PATTERN;
  Tpat->Wave_Type  = RAMP_WAVE;
  Tpat->Flags      = NO_FLAGS;
  Tpat->References = 1;
  Tpat->Exponent   = 1.0;
  Tpat->Frequency  = 1.0;
  Tpat->Phase      = 0.0;
  Tpat->Warps      = NULL;
  Tpat->Next       = NULL;
  Tpat->Blend_Map  = NULL;
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void Copy_TPat_Fields (TPATTERN *New, TPATTERN  *Old)
{
  *New = *Old;
  
  /* Copy warp chain */
  New->Warps = Copy_Warps(Old->Warps);

  New->Blend_Map = Copy_Blend_Map(Old->Blend_Map);

  /* Note, cannot copy Old->Next because we don't know what kind of
     thing this is.  It must be copied by Copy_Pigment, Copy_Tnormal etc.
  */

  /* NK 1998 - added IMAGE_PATTERN */
  if ((Old->Type == BITMAP_PATTERN) || (Old->Type == IMAGE_PATTERN))
  {
     New->Vals.Image = Copy_Image(Old->Vals.Image);
  }

  if (Old->Type == DENSITY_FILE_PATTERN)
  {
     New->Vals.Density_File = Copy_Density_File(Old->Vals.Density_File);
  }

  if (Old->Type == PIGMENT_PATTERN )
  {
    New->Vals.Pigment = Copy_Pigment(Old->Vals.Pigment);
  }

  if (Old->Type == OBJECT_PATTERN)
  {
    if(Old->Vals.Object != NULL)
    {
      New->Vals.Object = (OBJECT*)Copy_Object(Old->Vals.Object);
    }
  }

  if (Old->Type == CRACKLE_PATTERN)
  {
    if (Old->Vals.Crackle.cv != NULL)
    {
       New->Vals.Crackle.cv =(VECTOR*) POV_MALLOC( 125*sizeof(VECTOR), "crackle cache");
       New->Vals.Crackle.lastseed = 0x8000000; 
    }
  }

  if (Old->Type == FUNCTION_PATTERN)
  {
    if (Old->Vals.Function.Fn != NULL)
    {
      New->Vals.Function.Fn = (void *)Copy_Function( (FUNCTION_PTR)(Old->Vals.Function.Fn) );
    }
  }
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void Destroy_TPat_Fields(TPATTERN *Tpat)
{
  Destroy_Warps(Tpat->Warps);
  Destroy_Blend_Map(Tpat->Blend_Map);
  /* Note, cannot destroy Tpat->Next nor pattern itself because we don't
     know what kind of thing this is.  It must be destroied by Destroy_Pigment, etc.
  */

  if ((Tpat->Type == BITMAP_PATTERN) || (Tpat->Type == IMAGE_PATTERN))
  {
     Destroy_Image(Tpat->Vals.Image);
  }

  if (Tpat->Type == DENSITY_FILE_PATTERN)
  {
     Destroy_Density_File(Tpat->Vals.Density_File);
  }

  if (Tpat->Type == OBJECT_PATTERN)
  {
    if(Tpat->Vals.Object != NULL)
    {
        Destroy_Object((OBJECT *)Tpat->Vals.Object);
    }
  }

  if (Tpat->Type == CRACKLE_PATTERN)
  {
    if (Tpat->Vals.Crackle.cv != NULL)
    {
      POV_FREE( Tpat->Vals.Crackle.cv );
    }
  }

  if (Tpat->Type == PIGMENT_PATTERN)
  {
    if (Tpat->Vals.Pigment != NULL)
    {
      Destroy_Pigment( Tpat->Vals.Pigment );
    }
  }

  if (Tpat->Type == FUNCTION_PATTERN)
  {
    if (Tpat->Vals.Function.Fn != NULL)
    {
      Destroy_Function( (FUNCTION_PTR)(Tpat->Vals.Function.Fn) );
    }
  }
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

TURB *Create_Turb()
{
  TURB *New;

  New = (TURB *)POV_MALLOC(sizeof(TURB),"turbulence struct");

  Make_Vector(New->Turbulence, 0.0, 0.0, 0.0);

  New->Octaves = 6;
  New->Omega = 0.5;
  New->Lambda = 2.0;

  return(New);
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

#if 0   /* Unused function [AED] */
static TURB *Copy_Turb(TURB *Old)
{
  TURB *New;

  if (Old != NULL)
  {
    New = Create_Turb();

    *New = *Old;
  }
  else
  {
    New=NULL;
  }

  return(New);
}
#endif


/*****************************************************************************
*
* FUNCTION
*
*   Translate_Tpattern
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
* CHANGES
*
******************************************************************************/

void Translate_Tpattern(TPATTERN *Tpattern,VECTOR Vector)
{
  TRANSFORM Trans;

  if (Tpattern != NULL)
  {
    Compute_Translation_Transform (&Trans, Vector);

    Transform_Tpattern (Tpattern, &Trans);
  }
}


/*****************************************************************************
*
* FUNCTION
*
*   Rotate_Tpattern
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
* CHANGES
*
******************************************************************************/

void Rotate_Tpattern(TPATTERN *Tpattern,VECTOR Vector)
{
  TRANSFORM Trans;

  if (Tpattern != NULL)
  {
    Compute_Rotation_Transform (&Trans, Vector);

    Transform_Tpattern (Tpattern, &Trans);
  }
}


/*****************************************************************************
*
* FUNCTION
*
*   Scale_Tpattern
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
* CHANGES
*
******************************************************************************/

void Scale_Tpattern(TPATTERN *Tpattern,VECTOR Vector)
{
  TRANSFORM Trans;

  if (Tpattern != NULL)
  {
    Compute_Scaling_Transform (&Trans, Vector);

    Transform_Tpattern (Tpattern, &Trans);
  }
}


/*****************************************************************************
*
* FUNCTION
*
*   Transform_Tpattern
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
* CHANGES
*
******************************************************************************/

void Transform_Tpattern(TPATTERN *Tpattern,TRANSFORM *Trans)
{
  WARP *Temp;

  if (Tpattern != NULL)
  {
    if (Tpattern->Warps == NULL)
    {
      Tpattern->Warps = Create_Warp(TRANSFORM_WARP);
    }
    else
    {
      if (Tpattern->Warps->Warp_Type != TRANSFORM_WARP)
      {
        Temp = Tpattern->Warps;

        Tpattern->Warps = Create_Warp(TRANSFORM_WARP);

        Tpattern->Warps->Next_Warp = Temp;
        if(Tpattern->Warps->Next_Warp != NULL)
          Tpattern->Warps->Next_Warp->Prev_Warp = Tpattern->Warps;
      }
    }

    Compose_Transforms (&( ((TRANS *)(Tpattern->Warps))->Trans), Trans);
  }
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void Search_Blend_Map (DBL value,BLEND_MAP *Blend_Map,BLEND_MAP_ENTRY **Prev,BLEND_MAP_ENTRY  **Cur)
{
  BLEND_MAP_ENTRY *P, *C;
  int Max_Ent=Blend_Map->Number_Of_Entries-1;

  /* if greater than last, use last. */

  if (value >= Blend_Map->Blend_Map_Entries[Max_Ent].value)
  {
    P = C = &(Blend_Map->Blend_Map_Entries[Max_Ent]);
  }
  else
  {
    P = C = &(Blend_Map->Blend_Map_Entries[0]);

    while (value > C->value)
    {
      P = C++;
    }
  }

  if (value == C->value)
  {
    P = C;
  }

  *Prev = P;
  *Cur  = C;
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static TURB *Search_For_Turb(WARP *Warps)
{
  WARP* Temp=Warps;

  if (Temp!=NULL)
  {
    while (Temp->Next_Warp != NULL)
    {
      Temp=Temp->Next_Warp;
    }

    if (Temp->Warp_Type != CLASSIC_TURB_WARP)
    {
       Temp=NULL;
    }
  }

  return ((TURB *)Temp);
}


/*****************************************************************************
*
* FUNCTION
*
*   agate_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern is evaluated.
*   TPat   -- Texture pattern struct
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
* CHANGES
*
*   Oct 1994    : adapted from agate pigment by [CY]
*
******************************************************************************/

static DBL agate_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  register DBL noise, turb_val;
  TURB* Turb;

  Turb=Search_For_Turb(TPat->Warps);

  turb_val = TPat->Vals.Agate_Turb_Scale * Turbulence(EPoint,Turb,TPat);

  noise = 0.5 * (cycloidal(1.3 * turb_val + 1.1 * EPoint[Z]) + 1.0);

  if (noise < 0.0)
  {
    noise = 0.0;
  }
  else
  {
    noise = min(1.0, noise);
    noise = pow(noise, 0.77);
  }

  return(noise);
}


/*****************************************************************************
*
* FUNCTION
*
*   boxed_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   -
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL boxed_pattern (VECTOR EPoint)
{
  register DBL value;

  value = max(fabs(EPoint[X]), max(fabs(EPoint[Y]), fabs(EPoint[Z])));
  CLIP_DENSITY(value);

  return(value);
}


/*****************************************************************************
*
* FUNCTION
*
*   brick_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   TPat   -- Texture pattern struct
*   
* OUTPUT
*   
* RETURNS
*
*   DBL value exactly 0.0 or 1.0
*   
* AUTHOR
*
*   Dan Farmer
*   
* DESCRIPTION
*
* CHANGES
*
*   Oct 1994    : adapted from pigment by [CY]
*
******************************************************************************/

static DBL brick_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int ibrickx, ibricky, ibrickz;
  DBL brickheight, brickwidth, brickdepth;
  DBL brickmortar, mortarheight, mortarwidth, mortardepth;
  DBL brickx, bricky, brickz;
  DBL x, y, z, fudgit;

  fudgit=Small_Tolerance+TPat->Vals.Brick.Mortar;

  x =  EPoint[X]+fudgit;
  y =  EPoint[Y]+fudgit;
  z =  EPoint[Z]+fudgit;

  brickwidth  = TPat->Vals.Brick.Size[X];
  brickheight = TPat->Vals.Brick.Size[Y];
  brickdepth  = TPat->Vals.Brick.Size[Z];
  brickmortar = (DBL)TPat->Vals.Brick.Mortar;

  mortarwidth  = brickmortar / brickwidth;
  mortarheight = brickmortar / brickheight;
  mortardepth  = brickmortar / brickdepth;

  /* 1) Check mortar layers in the X-Z plane (ie: top view) */

  bricky = y / brickheight;
  ibricky = (int) bricky;
  bricky -= (DBL) ibricky;

  if (bricky < 0.0)
  {
    bricky += 1.0;
  }

  if (bricky <= mortarheight)
  {
    return(0.0);
  }

  bricky = (y / brickheight) * 0.5;
  ibricky = (int) bricky;
  bricky -= (DBL) ibricky;

  if (bricky < 0.0)
  {
    bricky += 1.0;
  }


  /* 2) Check ODD mortar layers in the Y-Z plane (ends) */

  brickx = (x / brickwidth);
  ibrickx = (int) brickx;
  brickx -= (DBL) ibrickx;

  if (brickx < 0.0)
  {
    brickx += 1.0;
  }

  if ((brickx <= mortarwidth) && (bricky <= 0.5))
  {
    return(0.0);
  }

  /* 3) Check EVEN mortar layers in the Y-Z plane (ends) */

  brickx = (x / brickwidth) + 0.5;
  ibrickx = (int) brickx;
  brickx -= (DBL) ibrickx;

  if (brickx < 0.0)
  {
    brickx += 1.0;
  }

  if ((brickx <= mortarwidth) && (bricky > 0.5))
  {
    return(0.0);
  }

  /* 4) Check ODD mortar layers in the Y-X plane (facing) */

  brickz = (z / brickdepth);
  ibrickz = (int) brickz;
  brickz -= (DBL) ibrickz;

  if (brickz < 0.0)
  {
    brickz += 1.0;
  }

  if ((brickz <= mortardepth) && (bricky > 0.5))
  {
    return(0.0);
  }

  /* 5) Check EVEN mortar layers in the X-Y plane (facing) */

  brickz = (z / brickdepth) + 0.5;
  ibrickz = (int) brickz;
  brickz -= (DBL) ibrickz;

  if (brickz < 0.0)
  {
    brickz += 1.0;
  }

  if ((brickz <= mortardepth) && (bricky <= 0.5))
  {
    return(0.0);
  }

  /* If we've gotten this far, color me brick. */

  return(1.0);
}


/*****************************************************************************
*
* FUNCTION
*
*   cells_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   John VanSickle
*
* DESCRIPTION
*
*   "cells":
*
*   New colour function by John VanSickle,
*     vansickl@erols.com
*
*   Assigns a pseudorandom value to each unit cube.  The value for the cube in
*   which the evaluted point lies is returned.
*
*   All "cells" specific source code and examples are in the public domain.
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL cells_pattern (VECTOR EPoint)
{
  int    temp;
  DBL    tf;

  temp = POV_GET_OLD_RAND(); /* save current seed */

  
  /* select a random value based on the cube from which this came. */

  /* floor the values, instead of just truncating - this eliminates duplicated cells
  around the axes */

  POV_SRAND(Hash3d((int)floor(EPoint[X]+Small_Tolerance),
                   (int)floor(EPoint[Y]+Small_Tolerance),
                   (int)floor(EPoint[Z]+Small_Tolerance)));

  tf = FRAND();

  POV_SRAND(temp);  /* restore */

  return min(tf, 1.0);
}


/*****************************************************************************
*
* FUNCTION
*
*   checker_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value exactly 0.0 or 1.0
*
* AUTHOR
*
*   POV-Team
*
* DESCRIPTION
*
* CHANGES
*   Oct 1994    : adapted from pigment by [CY]
*
******************************************************************************/

static DBL checker_pattern (VECTOR EPoint)
{
  int value;

  value = (int)(floor(EPoint[X]+Small_Tolerance) +
                floor(EPoint[Y]+Small_Tolerance) +
                floor(EPoint[Z]+Small_Tolerance));

  if (value & 1)
  {
    return (1.0);
  }
  else
  {
    return (0.0);
  }
}


/*****************************************************************************
*
* FUNCTION
*
*   crackle_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Jim McElhiney
*
* DESCRIPTION
*
*   "crackle":
*
*   New colour function by Jim McElhiney,
*     CompuServe 71201,1326, aka mcelhiney@acm.org
*
*   Large scale, without turbulence, makes a pretty good stone wall.
*   Small scale, without turbulence, makes a pretty good crackle ceramic glaze.
*   Highly turbulent (with moderate displacement) makes a good marble, solving
*   the problem of apparent parallel layers in Perlin's method.
*   2 octaves of full-displacement turbulence make a great "drizzled paint"
*   pattern, like a 1950's counter top.
*   Rule of thumb:  put a single colour transition near 0 in your colour map.
*
*   Mathematically, the set crackle(p)=0 is a 3D Voronoi diagram of a field of
*   semirandom points, and crackle(p)>0 is distance from set along shortest path.
*   (A Voronoi diagram is the locus of points equidistant from their 2 nearest
*   neighbours from a set of disjoint points, like the membranes in suds are
*   to the centres of the bubbles).
*
*   All "crackle" specific source code and examples are in the public domain.
*
* CHANGES
*   Oct 1994    : adapted from pigment by [CY]
*   Other changes: enhanced by Ron Parker, Integer math by Nathan Kopp
*
******************************************************************************/
static int IntPickInCube(int tvx, int tvy, int tvz, VECTOR  p1);

static DBL crackle_pattern (VECTOR EPoint, TPATTERN *TPat ) 
{
  int    i;
  int   thisseed;
  DBL    sum, minsum, minsum2, minsum3, tf;
  VECTOR minvec;
  VECTOR tv, dv, t1;
  int addx,addy,addz;

  VECTOR flo;
  int cvc;
  static int vali=0, vals[3];
  static int valid[125];

  DBL Metric;
  DBL Offset;

  int UseSquare;
  int UseUnity;
  int flox,floy,floz;
  /*int seed,temp;*/

  Metric = TPat->Vals.Crackle.Metric[X];
  Offset = TPat->Vals.Crackle.Offset;

  UseSquare = ( Metric == 2);
  UseUnity = ( Metric == 1);

  Assign_Vector(tv,EPoint);

  /*
   * Check to see if the input point is in the same unit cube as the last
   * call to this function, to use cache of cubelets for speed.
   */

  thisseed = PickInCube(tv, t1);

  if (thisseed != TPat->Vals.Crackle.lastseed)
  {
    /*
     * No, not same unit cube.  Calculate the random points for this new
     * cube and its 80 neighbours which differ in any axis by 1 or 2.
     * Why distance of 2?  If there is 1 point in each cube, located
     * randomly, it is possible for the closest random point to be in the
     * cube 2 over, or the one two over and one up.  It is NOT possible
     * for it to be two over and two up.  Picture a 3x3x3 cube with 9 more
     * cubes glued onto each face.
     */


    flo[X] = floor(tv[X] - EPSILON);
    flo[Y] = floor(tv[Y] - EPSILON);
    flo[Z] = floor(tv[Z] - EPSILON);
    
    Assign_Vector( (TPat->Vals.Crackle.lastcenter), flo );

    /* Now store a points for this cube and each of the 80 neighbour cubes. */

    vals[0]=25*(2+(-2))+5*(2+(-1))+2+(-1);
    vals[1]=25*(2+(-2))+5*(2+(-1))+2+(0);
    vals[2]=25*(2+(-2))+5*(2+(-1))+2+(1);

    flox = (int)flo[X];
    floy = (int)flo[Y];
    floz = (int)flo[Z];

    for (addx = -2; addx <= 2; addx++)
    {
      for (addy = -2; addy <= 2; addy++)
      {
	      for (addz = -2; addz <= 2; addz++)
	      {
	        /* For each cubelet in a 5x5 cube. */
          cvc = 25*(2+addx)+5*(2+addy)+2+addz;

          if ((abs(addx)==2)+(abs(addy)==2)+(abs(addz)==2) <= 1)
	        {
	          /* Yes, it's within a 3d knight move away. */

#define INLINE_PICK_IN_CUBE 0
#if INLINE_PICK_IN_CUBE
            /* do our own PickInCube and use as much integer math as possible */
            seed = Hash3d((flox+addx)&0xFFF,(floy+addy)&0xFFF,(floz+addz)&0xFFF);
            temp = POV_GET_OLD_RAND(); /* save current seed */
            POV_SRAND(seed);
            TPat->Vals.Crackle.cv[cvc][X] = flox+addx + FRAND();
            TPat->Vals.Crackle.cv[cvc][Y] = floy+addy + FRAND();
            TPat->Vals.Crackle.cv[cvc][Z] = floz+addz + FRAND();
            POV_SRAND(temp);  /* restore */
#else
	          IntPickInCube(flox+addx,floy+addy,floz+addz, t1);

	          TPat->Vals.Crackle.cv[cvc][X] = t1[X];
	          TPat->Vals.Crackle.cv[cvc][Y] = t1[Y];
	          TPat->Vals.Crackle.cv[cvc][Z] = t1[Z];
#endif
	          valid[cvc]=1;
	        }
	        else 
          {
            valid[cvc]=0;
	        }
	      }
      }
    }

    TPat->Vals.Crackle.lastseed = thisseed;
  }

  cvc=125;
  /*
   * Find the 2 points with the 2 shortest distances from the input point.
   * Loop invariant:  minsum is shortest dist, minsum2 is 2nd shortest
   */

  /* Set up the loop so the invariant is true:  minsum <= minsum2 */

  VSub(dv, TPat->Vals.Crackle.cv[vals[0]], tv);  
  if ( UseSquare ) 
  {
    minsum  = VSumSqr(dv);
	  if ( Offset ) minsum += Offset*Offset;
  }
  else if ( UseUnity ) 
  {
	  minsum = fabs(dv[X]) + fabs(dv[Y]) + fabs(dv[Z]);
	  if ( Offset ) minsum += Offset;
  }
  else 
  {
	  minsum = pow( fabs( dv[X] ), Metric ) +
         pow( fabs( dv[Y] ), Metric ) +
			   pow( fabs( dv[Z] ), Metric );
	  if ( Offset ) minsum += pow( Offset, Metric );
  }
  Assign_Vector( minvec, TPat->Vals.Crackle.cv[vals[0]] );
  VSub(dv, TPat->Vals.Crackle.cv[vals[1]], tv);  
  if ( UseSquare ) 
  {
    minsum2  = VSumSqr(dv);
	  if ( Offset ) minsum2 += Offset*Offset;
  }
  else if ( UseUnity ) 
  {
	  minsum2 = fabs(dv[X]) + fabs(dv[Y]) + fabs(dv[Z]);
	  if ( Offset ) minsum2 += Offset;
  }
  else 
  {
	  minsum2 = pow( fabs( dv[X] ), Metric ) +
         pow( fabs( dv[Y] ), Metric ) +
         pow( fabs( dv[Z] ), Metric );
	  if ( Offset ) minsum2 += pow( Offset, Metric );
  }
  VSub(dv, TPat->Vals.Crackle.cv[vals[2]], tv);  
  if ( UseSquare ) {
    minsum3  = VSumSqr(dv);
    if ( Offset ) minsum3 += Offset*Offset;
  }
  else if ( UseUnity ) 
  {
    minsum3 = fabs(dv[X]) + fabs(dv[Y]) + fabs(dv[Z]);
    if ( Offset ) minsum3 += Offset;
  }
  else 
  {
    minsum3 = pow( fabs( dv[X] ), Metric ) +
         pow( fabs( dv[Y] ), Metric ) +
         pow( fabs( dv[Z] ), Metric );
    if ( Offset ) minsum3 += pow( Offset, Metric );
  }

  if (minsum2 < minsum)
  {
    tf = minsum; minsum = minsum2; minsum2 = tf;
    Assign_Vector( minvec, TPat->Vals.Crackle.cv[vals[1]] );
  }
  if (minsum3 < minsum)
  {
    tf = minsum; minsum = minsum3; minsum3 = tf;
    Assign_Vector( minvec, TPat->Vals.Crackle.cv[vals[2]] );
  }
  if ( minsum3 < minsum2 ) 
  {
	  tf = minsum2; minsum2=minsum3; minsum3= tf;
  }

  /* Loop for the 81 cubelets to find closest and 2nd closest. */

  for (i = vals[2]+1; i < cvc; i++) if (valid[i])
  {
    VSub(dv, TPat->Vals.Crackle.cv[i], tv);

    if ( UseSquare ) 
    {
      sum  = VSumSqr(dv);
      if ( Offset ) sum += Offset*Offset;
    }
    else if ( UseUnity ) 
    {
      sum = fabs(dv[X]) + fabs(dv[Y]) + fabs(dv[Z]);
      if ( Offset ) sum += Offset;
    }
    else 
    {
      sum = pow( fabs( dv[X] ), Metric ) +
      pow( fabs( dv[Y] ), Metric ) +
      pow( fabs( dv[Z] ), Metric );
      if ( Offset ) sum += pow( Offset, Metric );
    }

    if (sum < minsum)
    {
      minsum3 = minsum2;
      minsum2 = minsum;
      minsum = sum;
      Assign_Vector( minvec, TPat->Vals.Crackle.cv[i] );
    }
    else if (sum < minsum2) 
    {
      minsum3 = minsum2;
      minsum2 = sum;
    }
    else if ( sum < minsum3 ) 
    {
      minsum3 = sum;
	  }
  }

  if ( TPat->Vals.Crackle.IsSolid ) 
  {
	  tf = Noise( minvec, TPat );
  }
  else if (UseSquare) 
  {
    tf = TPat->Vals.Crackle.Form[X]*sqrt(minsum) + 
      TPat->Vals.Crackle.Form[Y]*sqrt(minsum2) +
      TPat->Vals.Crackle.Form[Z]*sqrt(minsum3); 
  }
  else if ( UseUnity ) 
  {
    tf = TPat->Vals.Crackle.Form[X]*minsum + 
      TPat->Vals.Crackle.Form[Y]*minsum2 +
      TPat->Vals.Crackle.Form[Z]*minsum3; 
  }
  else 
  {
    tf = TPat->Vals.Crackle.Form[X]*pow(minsum, 1.0/Metric) + 
      TPat->Vals.Crackle.Form[Y]*pow(minsum2, 1.0/Metric) + 
      TPat->Vals.Crackle.Form[Z]*pow(minsum3, 1.0/Metric); 
  }

  return max(min(tf, 1.), 0.);
}


/*****************************************************************************
*
* FUNCTION
*
*   cylindrical_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   -
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL cylindrical_pattern (VECTOR EPoint)
{
  register DBL value;

  value = sqrt(Sqr(EPoint[X]) + Sqr(EPoint[Z]));
  CLIP_DENSITY(value);

  return(value);
}


/*****************************************************************************
*
* FUNCTION
*
*   density_pattern
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
* CHANGES
*
*   Dec 1996 : Creation.
*
******************************************************************************/

inline float intp3(float t, float fa, float fb, float fc, float fd)
{
	float b,d,e,f;

	b = (fc - fa) * 0.5;
	d = (fd - fb) * 0.5;
	e = 2.0 * (fb - fc) + b + d;
	f = -3.0 * (fb - fc) - 2.0 * b - d;

	return ((e * t + f) * t + b) * t + fb;
}

inline float intp3_2(float t, float fa, float fb, float fc, float fd)
{
	float b,e,f;

	e = fd - fc - fa + fb;
	f = fa - fb - e;
	b = fc - fa;

	return ((e * t + f) * t + b) * t + fb;
}

#define zmax(i,imax) (((i)<0)?(imax-1):((i) % (imax))) 

static DBL density_pattern(VECTOR EPoint, TPATTERN *TPat)
{
	int x, y, z;
	int x1, y1, z1;
	int x2, y2, z2;
	DBL Ex, Ey, Ez;
	DBL xx, yy, zz;
	DBL xi, yi;
	DBL f111, f112, f121, f122, f211, f212, f221, f222;
	float intpd2[4][4];
	DBL density = 0.0;
	DENSITY_FILE_DATA *Data;
	int k0, k1, k2, k3, i,j,ii,jj;

	Ex=EPoint[X];
	Ey=EPoint[Y];
	Ez=EPoint[Z];

	if((TPat->Vals.Density_File != NULL) && ((Data = TPat->Vals.Density_File->Data) != NULL))
	{
/*		if(Data->Cyclic == true) 
		{
			Ex -= floor(Ex);
			Ey -= floor(Ey);
			Ez -= floor(Ez);
		}
*/
		if((Ex >= 0.0) && (Ex < 1.0) && (Ey >= 0.0) && (Ey < 1.0) && (Ez >= 0.0) && (Ez < 1.0))
		{
			switch (TPat->Vals.Density_File->Interpolation % 10)
			{
				case NO_INTERPOLATION:
					x = (int)(Ex * (DBL)Data->Sx);
					y = (int)(Ey * (DBL)Data->Sy);
					z = (int)(Ez * (DBL)Data->Sz);

					if ((x < 0) || (x >= Data->Sx) || (y < 0) || (y >= Data->Sy) || (z < 0) || (z >= Data->Sz))
						density = 0.0;
					else
					{
						if(Data->Type == 4)
							density = (DBL)Data->Density32[z * Data->Sy * Data->Sx + y * Data->Sx + x] / (DBL)UINT_MAX;
						else if(Data->Type==2)
							density = (DBL)Data->Density16[z * Data->Sy * Data->Sx + y * Data->Sx + x] / (DBL)USHRT_MAX;
						else if(Data->Type == 1)
							density = (DBL)Data->Density8[z * Data->Sy * Data->Sx + y * Data->Sx + x] / (DBL)UCHAR_MAX;
					}
					break;
				case TRILINEAR_INTERPOLATION:
					xx = Ex * (DBL)(Data->Sx );
					yy = Ey * (DBL)(Data->Sy );
					zz = Ez * (DBL)(Data->Sz );

					x1 = (int)xx;
					y1 = (int)yy;
					z1 = (int)zz;

					x2 = (x1 + 1) % Data->Sx;
					y2 = (y1 + 1) % Data->Sy;
					z2 = (z1 + 1) % Data->Sz;

					xx -= floor(xx);
					yy -= floor(yy);
					zz -= floor(zz);

					xi = 1.0 - xx;
					yi = 1.0 - yy;

					if(Data->Type == 4)
					{
						f111 = (DBL)Data->Density32[z1 * Data->Sy * Data->Sx + y1 * Data->Sx + x1] / (DBL)UINT_MAX;
						f112 = (DBL)Data->Density32[z1 * Data->Sy * Data->Sx + y1 * Data->Sx + x2] / (DBL)UINT_MAX;
						f121 = (DBL)Data->Density32[z1 * Data->Sy * Data->Sx + y2 * Data->Sx + x1] / (DBL)UINT_MAX;
						f122 = (DBL)Data->Density32[z1 * Data->Sy * Data->Sx + y2 * Data->Sx + x2] / (DBL)UINT_MAX;
						f211 = (DBL)Data->Density32[z2 * Data->Sy * Data->Sx + y1 * Data->Sx + x1] / (DBL)UINT_MAX;
						f212 = (DBL)Data->Density32[z2 * Data->Sy * Data->Sx + y1 * Data->Sx + x2] / (DBL)UINT_MAX;
						f221 = (DBL)Data->Density32[z2 * Data->Sy * Data->Sx + y2 * Data->Sx + x1] / (DBL)UINT_MAX;
						f222 = (DBL)Data->Density32[z2 * Data->Sy * Data->Sx + y2 * Data->Sx + x2] / (DBL)UINT_MAX;
					}
					else if(Data->Type == 2)
					{
						f111 = (DBL)Data->Density16[z1 * Data->Sy * Data->Sx + y1 * Data->Sx + x1] / (DBL)USHRT_MAX;
						f112 = (DBL)Data->Density16[z1 * Data->Sy * Data->Sx + y1 * Data->Sx + x2] / (DBL)USHRT_MAX;
						f121 = (DBL)Data->Density16[z1 * Data->Sy * Data->Sx + y2 * Data->Sx + x1] / (DBL)USHRT_MAX;
						f122 = (DBL)Data->Density16[z1 * Data->Sy * Data->Sx + y2 * Data->Sx + x2] / (DBL)USHRT_MAX;
						f211 = (DBL)Data->Density16[z2 * Data->Sy * Data->Sx + y1 * Data->Sx + x1] / (DBL)USHRT_MAX;
						f212 = (DBL)Data->Density16[z2 * Data->Sy * Data->Sx + y1 * Data->Sx + x2] / (DBL)USHRT_MAX;
						f221 = (DBL)Data->Density16[z2 * Data->Sy * Data->Sx + y2 * Data->Sx + x1] / (DBL)USHRT_MAX;
						f222 = (DBL)Data->Density16[z2 * Data->Sy * Data->Sx + y2 * Data->Sx + x2] / (DBL)USHRT_MAX;
					}
					else if(Data->Type == 1)
					{
						f111 = (DBL)Data->Density8[z1 * Data->Sy * Data->Sx + y1 * Data->Sx + x1] / (DBL)UCHAR_MAX;
						f112 = (DBL)Data->Density8[z1 * Data->Sy * Data->Sx + y1 * Data->Sx + x2] / (DBL)UCHAR_MAX;
						f121 = (DBL)Data->Density8[z1 * Data->Sy * Data->Sx + y2 * Data->Sx + x1] / (DBL)UCHAR_MAX;
						f122 = (DBL)Data->Density8[z1 * Data->Sy * Data->Sx + y2 * Data->Sx + x2] / (DBL)UCHAR_MAX;
						f211 = (DBL)Data->Density8[z2 * Data->Sy * Data->Sx + y1 * Data->Sx + x1] / (DBL)UCHAR_MAX;
						f212 = (DBL)Data->Density8[z2 * Data->Sy * Data->Sx + y1 * Data->Sx + x2] / (DBL)UCHAR_MAX;
						f221 = (DBL)Data->Density8[z2 * Data->Sy * Data->Sx + y2 * Data->Sx + x1] / (DBL)UCHAR_MAX;
						f222 = (DBL)Data->Density8[z2 * Data->Sy * Data->Sx + y2 * Data->Sx + x2] / (DBL)UCHAR_MAX;
					}

					density = ((f111 * xi + f112 * xx) * yi + (f121 * xi + f122 * xx) * yy) * (1.0 - zz) +
					          ((f211 * xi + f212 * xx) * yi + (f221 * xi + f222 * xx) * yy) * zz;
					break;
				case TRICUBIC_INTERPOLATION:
				default:
					xx = Ex * (DBL)(Data->Sx);
					yy = Ey * (DBL)(Data->Sy);
					zz = Ez * (DBL)(Data->Sz);

					x1 = (int)xx;
					y1 = (int)yy;
					z1 = (int)zz;

					xx -= floor(xx);
					yy -= floor(yy);
					zz -= floor(zz);

					k0 = zmax(-1+z1, Data->Sz );
					k1 = zmax(   z1, Data->Sz );
					k2 = zmax( 1+z1, Data->Sz );
					k3 = zmax( 2+z1, Data->Sz );

					if(Data->Type == 4)
					{
						for(i = 0; i < 4; i++)
						{
							ii = zmax(i + x1 - 1, Data->Sx);
							for(j = 0; j < 4; j++)
							{
								jj = zmax(j + y1 - 1, Data->Sy);
								intpd2[i][j] = intp3(zz,
								                     Data->Density32[k0 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)UINT_MAX,
								                     Data->Density32[k1 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)UINT_MAX,
								                     Data->Density32[k2 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)UINT_MAX,
								                     Data->Density32[k3 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)UINT_MAX);
							}
						}
					}
					else if(Data->Type == 2)
					{
						for(i = 0; i < 4; i++)
						{
							ii = zmax(i + x1 - 1, Data->Sx);
							for(j = 0; j < 4; j++)
							{
								jj = zmax(j + y1 - 1, Data->Sy);
								intpd2[i][j] = intp3(zz,
								                     Data->Density16[k0 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)USHRT_MAX,
								                     Data->Density16[k1 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)USHRT_MAX,
								                     Data->Density16[k2 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)USHRT_MAX,
								                     Data->Density16[k3 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)USHRT_MAX);
							}
						}
					}
					else if(Data->Type == 1)
					{
						for(i = 0; i < 4; i++)
						{
							ii = zmax(i + x1 - 1, Data->Sx);
							for(j = 0; j < 4; j++)
							{
								jj = zmax(j + y1 - 1, Data->Sy);
								intpd2[i][j] = intp3(zz,
								                     Data->Density8[k0 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)UCHAR_MAX,
								                     Data->Density8[k1 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)UCHAR_MAX,
								                     Data->Density8[k2 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)UCHAR_MAX,
								                     Data->Density8[k3 * Data->Sy * Data->Sx + jj * Data->Sx + ii] / (DBL)UCHAR_MAX);
							}
						}
					}

					for(i = 0; i < 4; i++)
						intpd2[0][i] = intp3(yy, intpd2[i][0], intpd2[i][1],  intpd2[i][2], intpd2[i][3]);

					density = intp3(xx, intpd2[0][0], intpd2[0][1], intpd2[0][2], intpd2[0][3]);
					break;
			}
		}
		else
			density = 0.0;
	}

	return density;
}


/*****************************************************************************
*
* FUNCTION
*
*   dents_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION   : Note this pattern is only used for pigments and textures.
*                 Normals have a specialized pattern for this.
*
* CHANGES
*   Nov 1994 : adapted from normal by [CY]
*
******************************************************************************/

static DBL dents_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  DBL noise;

  noise = Noise (EPoint, TPat);

  return(noise * noise * noise);
}


/*****************************************************************************
*
* FUNCTION
*
*   function_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   
* OUTPUT
*   
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static DBL function_pattern (VECTOR EPoint, TPATTERN *TPat)
{
	DBL value;
	FPUContext *oldcontext;

	if(TPat->Vals.Function.Data == NULL)
		TPat->Vals.Function.Data = POVFPU_NewContext();

	oldcontext = POVFPU_SwitchContext((FPUContext *)(TPat->Vals.Function.Data));

	POVFPU_SetLocal(X, EPoint[X]);
	POVFPU_SetLocal(Y, EPoint[Y]);
	POVFPU_SetLocal(Z, EPoint[Z]);
 
   	value = POVFPU_Run(*((FUNCTION_PTR)(TPat->Vals.Function.Fn)));

	(void)POVFPU_SwitchContext(oldcontext);

	return ((value > 1.0) ? fmod(value, 1.0) : value);
}


/*****************************************************************************
*
* FUNCTION
*
*   gradient_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   
* OUTPUT
*   
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   Gradient Pattern - gradient based on the fractional values of
*   x, y or z, based on whether or not the given directional vector is
*   a 1.0 or a 0.0.
*   The basic concept of this is from DBW Render, but Dave Wecker's
*   only supports simple Y axis gradients.
*
* CHANGES
*
*   Oct 1994    : adapted from pigment by [CY]
*
******************************************************************************/

static DBL gradient_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  DBL Result;
  VDot( Result, EPoint, TPat->Vals.Gradient );

  /* Mod to keep within [0.0,1.0] range */
  return ((Result > 1.0) ? fmod(Result, 1.0) : Result);
}


/*****************************************************************************
*
* FUNCTION
*
*   granite_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   
* OUTPUT
*   
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   Granite - kind of a union of the "spotted" and the "dented" textures,
*   using a 1/f fractal noise function for color values. Typically used
*   with small scaling values. Should work with colour maps for pink granite.
*
* CHANGES
*
*   Oct 1994    : adapted from pigment by [CY]
*
******************************************************************************/

static DBL granite_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  register int i;
  register DBL temp, noise = 0.0, freq = 1.0;
  VECTOR tv1,tv2;

  VScale(tv1,EPoint,4.0);

  int noise_generator;
  if (TPat != NULL)
    noise_generator = (TPat->Flags & NOISE_FLAGS) >> 4;
  if (!noise_generator)
    noise_generator=opts.Noise_Generator;

  for (i = 0; i < 6 ; freq *= 2.0, i++)
  {
    VScale(tv2,tv1,freq);
    if(noise_generator==1)
    {
      temp = 0.5 - Noise (tv2, TPat);
      temp = fabs(temp);
    }
    else
    {
      temp = 1.0 - 2.0 * Noise (tv2, TPat);
      temp = fabs(temp);
      if (temp>0.5) temp=0.5;
    }

    

    noise += temp / freq;
  }

  return(noise);
}


/*****************************************************************************
*
* FUNCTION
*
*   hexagon_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value exactly 0.0, 1.0 or 2.0
*
* AUTHOR
*
*   Ernest MacDougal Campbell III
*   
* DESCRIPTION
*
*   TriHex pattern -- Ernest MacDougal Campbell III (EMC3) 11/23/92
*
*   Creates a hexagon pattern in the XZ plane.
*
*   This algorithm is hard to explain.  First it scales the point to make
*   a few of the later calculations easier, then maps some points to be
*   closer to the Origin.  A small area in the first quadrant is subdivided
*   into a 6 x 6 grid.  The position of the point mapped into that grid
*   determines its color.  For some points, just the grid location is enough,
*   but for others, we have to calculate which half of the block it's in
*   (this is where the atan2() function comes in handy).
*
* CHANGES
*
*   Nov 1992 : Creation.
*   Oct 1994 : adapted from pigment by [CY]
*
******************************************************************************/

const DBL xfactor = 0.5;         /* each triangle is split in half for the grid */
const DBL zfactor = 0.866025404; /* sqrt(3)/2 -- Height of an equilateral triangle */

static DBL hexagon_pattern (VECTOR EPoint)
{
  int xm, zm;
  int brkindx;
  DBL xs, zs, xl, zl, value = 0.0;
  DBL x=EPoint[X];
  DBL z=EPoint[Z];


  /* Keep all numbers positive.  Also, if z is negative, map it in such a
   * way as to avoid mirroring across the x-axis.  The value 5.196152424
   * is (sqrt(3)/2) * 6 (because the grid is 6 blocks high)
   */

  x = fabs(x);

  /* Avoid mirroring across x-axis. */

  z = z < 0.0 ? 5.196152424 - fabs(z) : z;

  /* Scale point to make calcs easier. */

  xs = x/xfactor;
  zs = z/zfactor;

  /* Map points into the 6 x 6 grid where the basic formula works. */

  xs -= floor(xs/6.0) * 6.0;
  zs -= floor(zs/6.0) * 6.0;

  /* Get a block in the 6 x 6 grid. */

  xm = (int) FLOOR(xs) % 6;
  zm = (int) FLOOR(zs) % 6;

  switch (xm)
  {
    /* These are easy cases: Color depends only on xm and zm. */

    case 0:
    case 5:

      switch (zm)
      {
        case 0:
        case 5: value = 0; break;

        case 1:
        case 2: value = 1; break;

        case 3:
        case 4: value = 2; break;
      }

      break;

    case 2:
    case 3:

      switch (zm)
      {
        case 0:
        case 1: value = 2; break;

        case 2:
        case 3: value = 0; break;

        case 4:
        case 5: value = 1; break;
      }

      break;

    /* These cases are harder.  These blocks are divided diagonally
     * by the angled edges of the hexagons.  Some slope positive, and
     * others negative.  We flip the x value of the negatively sloped
     * pieces.  Then we check to see if the point in question falls
     * in the upper or lower half of the block.  That info, plus the
     * z status of the block determines the color.
     */

    case 1:
    case 4:

      /* Map the point into the block at the origin. */

      xl = xs-xm;
      zl = zs-zm;

      /* These blocks have negative slopes so we flip it horizontally. */

      if (((xm + zm) % 2) == 1)
      {
        xl = 1.0 - xl;
      }

      /* Avoid a divide-by-zero error. */

      if (xl == 0.0)
      {
        xl = 0.0001;
      }

      /* Is the angle less-than or greater-than 45 degrees? */

      brkindx = (zl / xl) < 1.0;

      /* was...
       * brkindx = (atan2(zl,xl) < (45 * M_PI_180));
       * ...but because of the mapping, it's easier and cheaper,
       * CPU-wise, to just use a good ol' slope.
       */

      switch (brkindx)
      {
        case true:

          switch (zm)
          {
            case 0:
            case 3: value = 0; break;

            case 2:
            case 5: value = 1; break;

            case 1:
            case 4: value = 2; break;
          }

          break;

        case false:

          switch (zm)
          {
            case 0:
            case 3: value = 2; break;

            case 2:
            case 5: value = 0; break;

            case 1:
            case 4: value = 1; break;
          }

          break;
      }
  }

  value = fmod(value, 3.0);

  return(value);
}


/*****************************************************************************
*
* FUNCTION
*
*   julia_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL julia_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col;
  DBL a, b, cf, a2, b2, dist2, mindist2,
      cr = TPat->Vals.Fractal.Coord[U], ci = TPat->Vals.Fractal.Coord[V];

  a = EPoint[X]; a2 = Sqr(a);
  b = EPoint[Y]; b2 = Sqr(b);
  mindist2 = a2+b2;

  it_max = TPat->Vals.Fractal.Iterations;

  for (col = 0; col < it_max; col++)
  {
    b  = 2.0 * a * b + ci;
    a  = a2 - b2 + cr;

    a2 = Sqr(a);
    b2 = Sqr(b);
    dist2 = a2+b2;

    if(dist2 < mindist2) mindist2 = dist2;
    if(dist2 > 4.0)
    {
        cf = fractal_exterior_color(TPat, col, a, b);
        break;
    }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   julia3_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL julia3_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col;
  DBL a, b, cf, a2, b2, dist2, mindist2,
      cr = TPat->Vals.Fractal.Coord[U], ci = TPat->Vals.Fractal.Coord[V];

  a = EPoint[X]; a2 = Sqr(a);
  b = EPoint[Y]; b2 = Sqr(b);
  mindist2 = a2+b2;

  it_max = TPat->Vals.Fractal.Iterations;

  for (col = 0; col < it_max; col++)
  {
    b = 3.0*a2*b - b2*b + ci;
    a = a2*a - 3.0*a*b2 + cr;

    a2 = Sqr(a);
    b2 = Sqr(b);
    dist2 = a2+b2;

    if(dist2 < mindist2) mindist2 = dist2;
    if(dist2 > 4.0)
    {
        cf = fractal_exterior_color(TPat, col, a, b);
      break;
    }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   julia4_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL julia4_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col;
  DBL a, b, cf, a2, b2, dist2, mindist2,
      cr = TPat->Vals.Fractal.Coord[U], ci = TPat->Vals.Fractal.Coord[V];

  a = EPoint[X]; a2 = Sqr(a);
  b = EPoint[Y]; b2 = Sqr(b);
  mindist2 = a2+b2;

  it_max = TPat->Vals.Fractal.Iterations;

  for (col = 0; col < it_max; col++)
  {
    b = 4.0 * (a2*a*b - a*b2*b) + ci;
    a = a2*a2 - 6.0*a2*b2 + b2*b2 + cr;

    a2 = Sqr(a);
    b2 = Sqr(b);
    dist2 = a2+b2;

    if(dist2 < mindist2) mindist2 = dist2;
    if(dist2 > 4.0)
    {
        cf = fractal_exterior_color(TPat, col, a, b);
        break;
    }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   juliax_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL juliax_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col, exponent;
  DBL a, b, cf=0, x, y, dist2, mindist2,
      cr = TPat->Vals.Fractal.Coord[U], ci = TPat->Vals.Fractal.Coord[V];
  int* binomial_coeff;

  if(BinomialCoefficientsInited == false)
      InitializeBinomialCoefficients();

  a = x = EPoint[X];
  b = y = EPoint[Y];
  mindist2 = a*a+b*b;

  it_max = TPat->Vals.Fractal.Iterations;
  exponent = TPat->Vals.Fractal.Exponent;

  binomial_coeff = &BinomialCoefficients[(exponent+1)*exponent/2];

  for (col = 0; col < it_max; col++)
  {
      // Calculate (a+bi)^exponent
      DBL new_a = pow(a, exponent);
      for(int k=2; k<=exponent; k+=2)
      {
          new_a += binomial_coeff[k]*pow(a, exponent-k)*pow(b, k);
      }
      DBL new_b = 0;
      for(int l=1; l<=exponent; l+=2)
      {
          new_b += binomial_coeff[l]*pow(a, exponent-l)*pow(b, l);
      }

      a = new_a + cr;
      b = new_b + ci;

      dist2 = a*a+b*b;

      if(dist2 < mindist2) mindist2 = dist2;
      if(dist2 > 4.0)
      {
          cf = fractal_exterior_color(TPat, col, a, b);
          break;
      }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   leopard_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Scott Taylor
*
* DESCRIPTION
*
* CHANGES
*
*   Jul 1991 : Creation.
*   Oct 1994 : adapted from pigment by [CY]
*
******************************************************************************/

static DBL leopard_pattern (VECTOR EPoint)
{
  register DBL value, temp1, temp2, temp3;

  /* This form didn't work with Zortech 386 compiler */
  /* value = Sqr((sin(x)+sin(y)+sin(z))/3); */
  /* So we break it down. */

  temp1 = sin(EPoint[X]);
  temp2 = sin(EPoint[Y]);
  temp3 = sin(EPoint[Z]);

  value = Sqr((temp1 + temp2 + temp3) / 3.0);

  return(value);
}


/*****************************************************************************
*
* FUNCTION
*
*   magnet1m_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL magnet1m_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col;
  DBL a, b, cf, a2, b2, x, y, tmp, tmp1r, tmp1i, tmp2r, tmp2i, dist2, mindist2;

  x = EPoint[X];
  y = EPoint[Y];
  a = a2 = 0;
  b = b2 = 0;
  mindist2 = 10000;

  it_max = TPat->Vals.Fractal.Iterations;

  for (col = 0; col < it_max; col++)
  {
      tmp1r = a2-b2 + x-1;
      tmp1i = 2*a*b + y;
      tmp2r = 2*a + x-2;
      tmp2i = 2*b + y;
      tmp = tmp2r*tmp2r + tmp2i*tmp2i;
      a = (tmp1r*tmp2r + tmp1i*tmp2i) / tmp;
      b = (tmp1i*tmp2r - tmp1r*tmp2i) / tmp;
      b2 = b*b;
      b = 2*a*b;
      a = a*a-b2;

      a2 = Sqr(a);
      b2 = Sqr(b);
      dist2 = a2+b2;

      if(dist2 < mindist2) mindist2 = dist2;
      tmp1r = a-1;
      if(dist2 > 10000.0 || tmp1r*tmp1r+b2 < 1/10000.0)
      {
          cf = fractal_exterior_color(TPat, col, a, b);
          break;
      }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   magnet1j_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL magnet1j_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col;
  DBL a, b, cf, a2, b2, tmp, tmp1r, tmp1i, tmp2r, tmp2i, dist2, mindist2,
      cr = TPat->Vals.Fractal.Coord[U], ci = TPat->Vals.Fractal.Coord[V];

  a = EPoint[X]; a2 = Sqr(a);
  b = EPoint[Y]; b2 = Sqr(b);
  mindist2 = a2+b2;

  it_max = TPat->Vals.Fractal.Iterations;

  for (col = 0; col < it_max; col++)
  {
      tmp1r = a2-b2 + cr-1;
      tmp1i = 2*a*b + ci;
      tmp2r = 2*a + cr-2;
      tmp2i = 2*b + ci;
      tmp = tmp2r*tmp2r + tmp2i*tmp2i;
      a = (tmp1r*tmp2r + tmp1i*tmp2i) / tmp;
      b = (tmp1i*tmp2r - tmp1r*tmp2i) / tmp;
      b2 = b*b;
      b = 2*a*b;
      a = a*a-b2;

      a2 = Sqr(a);
      b2 = Sqr(b);
      dist2 = a2+b2;

      if(dist2 < mindist2) mindist2 = dist2;
      tmp1r = a-1;
      if(dist2 > 10000.0 || tmp1r*tmp1r+b2 < 1/10000.0)
      {
          cf = fractal_exterior_color(TPat, col, a, b);
          break;
      }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   magnet2m_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL magnet2m_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col;
  DBL a, b, cf, a2, b2, x, y, tmp, tmp1r, tmp1i, tmp2r, tmp2i,
      c1r, c2r, c1c2r, c1c2i, dist2, mindist2;

  x = EPoint[X];
  y = EPoint[Y];
  a = a2 = 0;
  b = b2 = 0;
  mindist2 = 10000;

  c1r = x-1; c2r = x-2;
  c1c2r = c1r*c2r-y*y;
  c1c2i = (c1r+c2r)*y;

  it_max = TPat->Vals.Fractal.Iterations;

  for (col = 0; col < it_max; col++)
  {
      tmp1r = a2*a-3*a*b2 + 3*(a*c1r-b*y) + c1c2r;
      tmp1i = 3*a2*b-b2*b + 3*(a*y+b*c1r) + c1c2i;
      tmp2r = 3*(a2-b2) + 3*(a*c2r-b*y) + c1c2r + 1;
      tmp2i = 6*a*b + 3*(a*y+b*c2r) + c1c2i;
      tmp = tmp2r*tmp2r + tmp2i*tmp2i;
      a = (tmp1r*tmp2r + tmp1i*tmp2i) / tmp;
      b = (tmp1i*tmp2r - tmp1r*tmp2i) / tmp;
      b2 = b*b;
      b = 2*a*b;
      a = a*a-b2;

      a2 = Sqr(a);
      b2 = Sqr(b);
      dist2 = a2+b2;

      if(dist2 < mindist2) mindist2 = dist2;
      tmp1r = a-1;
      if(dist2 > 10000.0 || tmp1r*tmp1r+b2 < 1/10000.0)
      {
          cf = fractal_exterior_color(TPat, col, a, b);
          break;
      }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   magnet2j_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL magnet2j_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col;
  DBL a, b, cf, a2, b2, tmp, tmp1r, tmp1i, tmp2r, tmp2i, c1r,c2r,c1c2r,c1c2i,
      cr = TPat->Vals.Fractal.Coord[U], ci = TPat->Vals.Fractal.Coord[V],
      dist2, mindist2;

  a = EPoint[X]; a2 = Sqr(a);
  b = EPoint[Y]; b2 = Sqr(b);
  mindist2 = a2+b2;

  c1r = cr-1, c2r = cr-2;
  c1c2r = c1r*c2r-ci*ci;
  c1c2i = (c1r+c2r)*ci;

  it_max = TPat->Vals.Fractal.Iterations;

  for (col = 0; col < it_max; col++)
  {
      tmp1r = a2*a-3*a*b2 + 3*(a*c1r-b*ci) + c1c2r;
      tmp1i = 3*a2*b-b2*b + 3*(a*ci+b*c1r) + c1c2i;
      tmp2r = 3*(a2-b2) + 3*(a*c2r-b*ci) + c1c2r + 1;
      tmp2i = 6*a*b + 3*(a*ci+b*c2r) + c1c2i;
      tmp = tmp2r*tmp2r + tmp2i*tmp2i;
      a = (tmp1r*tmp2r + tmp1i*tmp2i) / tmp;
      b = (tmp1i*tmp2r - tmp1r*tmp2i) / tmp;
      b2 = b*b;
      b = 2*a*b;
      a = a*a-b2;

      a2 = Sqr(a);
      b2 = Sqr(b);
      dist2 = a2+b2;

      if(dist2 < mindist2) mindist2 = dist2;
      tmp1r = a-1;
      if(dist2 > 10000.0 || tmp1r*tmp1r+b2 < 1/10000.0)
      {
          cf = fractal_exterior_color(TPat, col, a, b);
          break;
      }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   mandel_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   submitted by user, name lost (sorry)
*
* DESCRIPTION
*
*   The mandel pattern computes the standard Mandelbrot fractal pattern and
*   projects it onto the X-Y plane.  It uses the X and Y coordinates to compute
*   the Mandelbrot set.
*
* CHANGES
*
*   Oct 1994 : adapted from pigment by [CY]
*   May 2001 : updated with code from Warp [trf]
*
******************************************************************************/

static DBL mandel_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col;
  DBL a, b, cf, a2, b2, x, y, dist2, mindist2;

  a = x = EPoint[X]; a2 = Sqr(a);
  b = y = EPoint[Y]; b2 = Sqr(b);
  mindist2 = a2+b2;

  it_max = TPat->Vals.Fractal.Iterations;

  for (col = 0; col < it_max; col++)
  {
    b  = 2.0 * a * b + y;
    a  = a2 - b2 + x;

    a2 = Sqr(a);
    b2 = Sqr(b);
    dist2 = a2+b2;

    if(dist2 < mindist2) mindist2 = dist2;
    if(dist2 > 4.0)
    {
        cf = fractal_exterior_color(TPat, col, a, b);
        break;
    }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   mandel3_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL mandel3_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col;
  DBL a, b, cf, a2, b2, x, y, dist2, mindist2;

  a = x = EPoint[X]; a2 = Sqr(a);
  b = y = EPoint[Y]; b2 = Sqr(b);
  mindist2 = a2+b2;

  it_max = TPat->Vals.Fractal.Iterations;

  for (col = 0; col < it_max; col++)
  {
      b = 3.0*a2*b - b2*b + y;
      a = a2*a - 3.0*a*b2 + x;

      a2 = Sqr(a);
      b2 = Sqr(b);
      dist2 = a2+b2;

      if(dist2 < mindist2) mindist2 = dist2;
      if(dist2 > 4.0)
      {
          cf = fractal_exterior_color(TPat, col, a, b);
          break;
      }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   mandel4_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL mandel4_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col;
  DBL a, b, cf, a2, b2, x, y, dist2, mindist2;

  a = x = EPoint[X]; a2 = Sqr(a);
  b = y = EPoint[Y]; b2 = Sqr(b);
  mindist2 = a2+b2;

  it_max = TPat->Vals.Fractal.Iterations;

  for (col = 0; col < it_max; col++)
  {
      b = 4.0 * (a2*a*b - a*b2*b) + y;
      a = a2*a2 - 6.0*a2*b2 + b2*b2 + x;

      a2 = Sqr(a);
      b2 = Sqr(b);
      dist2 = a2+b2;

      if(dist2 < mindist2) mindist2 = dist2;
      if(dist2 > 4.0)
      {
          cf = fractal_exterior_color(TPat, col, a, b);
          break;
      }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   mandelx_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Nieminen Juha
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL mandelx_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  int it_max, col, exponent;
  DBL a, b, cf=0, x, y, dist2, mindist2;
  int* binomial_coeff;

  if(BinomialCoefficientsInited == false)
      InitializeBinomialCoefficients();

  a = x = EPoint[X];
  b = y = EPoint[Y];
  mindist2 = a*a+b*b;

  it_max = TPat->Vals.Fractal.Iterations;
  exponent = TPat->Vals.Fractal.Exponent;

  binomial_coeff = &BinomialCoefficients[(exponent+1)*exponent/2];

  for (col = 0; col < it_max; col++)
  {
      // Calculate (a+bi)^exponent
      DBL new_a = pow(a, exponent);
      for(int k=2; k<=exponent; k+=2)
      {
          new_a += binomial_coeff[k]*pow(a, exponent-k)*pow(b, k);
      }
      DBL new_b = 0;
      for(int l=1; l<=exponent; l+=2)
      {
          new_b += binomial_coeff[l]*pow(a, exponent-l)*pow(b, l);
      }

      a = new_a + x;
      b = new_b + y;

      dist2 = a*a+b*b;

      if(dist2 < mindist2) mindist2 = dist2;
      if(dist2 > 4.0)
      {
          cf = fractal_exterior_color(TPat, col, a, b);
          break;
      }
  }

  if(col == it_max)
      cf = fractal_interior_color(TPat, col, a, b, mindist2);

  return(cf);
}


/*****************************************************************************
*
* FUNCTION
*
*   marble_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   TPat   -- Texture pattern struct
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
* CHANGES
*
*   Oct 1994 : adapted from pigment by [CY]
*
******************************************************************************/

static DBL marble_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  register DBL turb_val;
  TURB *Turb;

  if ((Turb=Search_For_Turb(TPat->Warps)) != NULL)
  {
    turb_val = Turb->Turbulence[X] * Turbulence(EPoint,Turb,TPat);
  }
  else
  {
    turb_val = 0.0;
  }

  return(EPoint[X] + turb_val);
}


/*****************************************************************************
*
* FUNCTION
*
*   object_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   TPat   -- Texture pattern struct
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static DBL object_pattern (VECTOR EPoint, TPATTERN *TPat)
{
   if(TPat->Vals.Object != NULL)
   {
      if(Inside_Object(EPoint, TPat->Vals.Object))
         return 1.0;
      else
         return 0.0;
   }

   return 0.0;
}

/*****************************************************************************
*
* FUNCTION
*
*   onion_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Scott Taylor
*
* DESCRIPTION
*
* CHANGES
*
*   Jul 1991 : Creation.
*   Oct 1994 : adapted from pigment by [CY]
*
******************************************************************************/

static DBL onion_pattern (VECTOR EPoint)
{
  /* The variable noise is not used as noise in this function */

  register DBL noise;

/*
   This ramp goes 0-1,1-0,0-1,1-0...

   noise = (fmod(sqrt(Sqr(x)+Sqr(y)+Sqr(z)),2.0)-1.0);

   if (noise<0.0) {noise = 0.0-noise;}
*/

  /* This ramp goes 0-1, 0-1, 0-1, 0-1 ... */

  noise = (fmod(sqrt(Sqr(EPoint[X])+Sqr(EPoint[Y])+Sqr(EPoint[Z])), 1.0));

  return(noise);
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR 
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static DBL pigment_pattern (VECTOR EPoint, TPATTERN *TPat, INTERSECTION *isect)
{
	DBL value;
	COLOUR Col;
	int colour_found=false;

	if (TPat->Vals.Pigment)
		colour_found = Compute_Pigment(Col, TPat->Vals.Pigment, EPoint, isect);

	if(!colour_found)
		value = 0.0;
	else
		value = GREY_SCALE(Col);

	return value ;
}


/*****************************************************************************
*
* FUNCTION
*
*   planar_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   -
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL planar_pattern (VECTOR EPoint)
{
	register DBL value = fabs(EPoint[Y]);

	CLIP_DENSITY(value);

	return value;
}


/*****************************************************************************
*
* FUNCTION
*
*   quilted_pattern
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Dan Farmer & Chris Young
*   
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static DBL quilted_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  VECTOR value;
  DBL t;

  value[X] = EPoint[X]-FLOOR(EPoint[X])-0.5;
  value[Y] = EPoint[Y]-FLOOR(EPoint[Y])-0.5;
  value[Z] = EPoint[Z]-FLOOR(EPoint[Z])-0.5;

  t = sqrt(value[X]*value[X]+value[Y]*value[Y]+value[Z]*value[Z]);

  t = quilt_cubic(t, TPat->Vals.Quilted.Control0, TPat->Vals.Quilted.Control1);

  value[X] *= t;
  value[Y] *= t;
  value[Z] *= t;

  return((fabs(value[X])+fabs(value[Y])+fabs(value[Z]))/3.0);
}


/*****************************************************************************
*
* FUNCTION
*
*   radial_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Chris Young -- new in vers 2.0
*
* DESCRIPTION
*
* CHANGES
*
*   Oct 1994 : adapted from pigment by [CY]
*
******************************************************************************/

static DBL radial_pattern (VECTOR EPoint)
{
  register DBL value;

  if ((fabs(EPoint[X])<0.001) && (fabs(EPoint[Z])<0.001))
  {
    value = 0.25;
  }
  else
  {
    value = 0.25 + (atan2(EPoint[X],EPoint[Z]) + M_PI) / TWO_M_PI;
  }

  return(value);
}


/*****************************************************************************
*
* FUNCTION
*
*   ripples_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   TPat   -- Texture pattern struct
*
* OUTPUT
*   
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*   
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION   : Note this pattern is only used for pigments and textures.
*                 Normals have a specialized pattern for this.
*
* CHANGES
*
*   Nov 1994 : adapted from normal by [CY]
*
******************************************************************************/

static DBL ripples_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  register unsigned int i;
  register DBL length, index;
  DBL scalar =0.0;
  VECTOR point;

  for (i = 0 ; i < Number_Of_Waves ; i++)
  {
    VSub (point, EPoint, Wave_Sources[i]);
    VLength (length, point);

    if (length == 0.0)
      length = 1.0;

    index = length * TPat->Frequency + TPat->Phase;

    scalar += cycloidal(index);
  }

  scalar = 0.5*(1.0+(scalar / (DBL)Number_Of_Waves));

  return(scalar);
}


/*****************************************************************************
*
* FUNCTION
*
*   slope_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*             is evaluated.
*   TPat   -- Texture pattern struct
*   Intersection - intersection struct
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0, 0.0 if normal is NULL
*
* AUTHOR
*
*   -hdf-
*
* DESCRIPTION   :
*
*   calculates the surface slope from surface normal vector
*
* CHANGES
*
*   Apr 1998 : written by H.-D. Fink
*   May 1998 : modified by M.C. Andrews - now combines slope and 'gradient'.
*
******************************************************************************/

static DBL slope_pattern (VECTOR EPoint, TPATTERN *TPat, INTERSECTION *Isection)
{
  DBL value, value1, value2;

  if (Isection == NULL) return 0.0; /* just in case ... */

  if (TPat->Vals.Slope.Slope_Base > 0)
    /* short case 1: slope vector in x, y or z direction */
    value1 = Isection->PNormal[TPat->Vals.Slope.Slope_Base - 1];
  else if (TPat->Vals.Slope.Slope_Base < 0)
    /* short case 2: slope vector in negative x, y or z direction */
    value1 = -Isection->PNormal[-TPat->Vals.Slope.Slope_Base - 1];
  else
    /* projection slope onto normal vector */
    VDot(value1, Isection->PNormal, TPat->Vals.Slope.Slope_Vector);

  /* Clamp to 1.0. */
  /* should never be necessary since both vectors are normalized */
  if      (value1 >  1.0) value1 =  1.0;
  else if (value1 < -1.0) value1 = -1.0;

  value1 = asin(value1) / M_PI * 2;
  value1 = (value1 + 1.0) * 0.5;        /* normalize to [0..1] interval */

  /* If set, use offset and scalings for slope and altitude. */
  if (0.0 != TPat->Vals.Slope.Slope_Mod[V])
  {
    value1 = (value1 - TPat->Vals.Slope.Slope_Mod[U]) / TPat->Vals.Slope.Slope_Mod[V];
  }

  if (!TPat->Vals.Slope.Altit_Len) 
  {
    /* Clamp to 1.0. */
    if ( value1 == 1.0 )
    {
      value1= value1- EPSILON;
    }
    else
    {  
      value1 = (value1 < 0.0) ? 1.0 + fmod(value1, 1.0) : fmod(value1, 1.0);
    }    
    return value1; /* no altitude defined */
  }

  /* Calculate projection of Epoint along altitude vector */
  if (TPat->Vals.Slope.Altit_Base > 0)
    /* short case 1: altitude vector in x, y or z direction */
    value2 = EPoint[TPat->Vals.Slope.Altit_Base - 1];
  else if (TPat->Vals.Slope.Altit_Base < 0)
    /* short case 2: altitude vector in negative x, y or z direction */
    value2 = -EPoint[-TPat->Vals.Slope.Altit_Base - 1];
  else
    /* projection of Epoint along altitude vector */
    VDot(value2, EPoint, TPat->Vals.Slope.Altit_Vector);

  if (0.0 != TPat->Vals.Slope.Altit_Mod[V])
  {
    value2 = (value2 - TPat->Vals.Slope.Altit_Mod[U]) / TPat->Vals.Slope.Altit_Mod[V];
  }

  value = TPat->Vals.Slope.Slope_Len * value1 + TPat->Vals.Slope.Altit_Len * value2;

  /* Clamp to 1.0. */
  if ( value - 1.0 < EPSILON && value >= 1.0 )
  {
    /* 1.0 is a very common value to get *exactly*.  We don't want to wrap
       it to the bottom end of the map. */
    value = value - EPSILON;
  }
  else
  {
    value = (value < 0.0) ? 1.0 + fmod(value, 1.0) : fmod(value, 1.0);
  }  
  return value;

}


/*****************************************************************************
*
* FUNCTION
*
*   spiral1_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   TPat   -- Texture pattern struct
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*   Spiral whirles around z-axis.
*   The number of "arms" is defined in the TPat.
*
* CHANGES
*
*   Aug 1994 : Creation.
*   Oct 1994 : adapted from pigment by [CY]
*
******************************************************************************/

static DBL spiral1_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  DBL rad, phi, turb_val;
  DBL x = EPoint[X];
  DBL y = EPoint[Y];
  DBL z = EPoint[Z];
  TURB *Turb;

  if ((Turb=Search_For_Turb(TPat->Warps)) != NULL)
  {
    turb_val = Turb->Turbulence[X] * Turbulence(EPoint,Turb,TPat);
  }
  else
  {
    turb_val = 0.0;
  }

  /* Get distance from z-axis. */

  rad = sqrt(x * x + y * y);

  /* Get angle in x,y-plane (0...2 PI). */

  if (rad == 0.0)
  {
    phi = 0.0;
  }
  else
  {
    if (x < 0.0)
    {
      phi = 3.0 * M_PI_2 - asin(y / rad);
    }
    else
    {
      phi = M_PI_2 + asin(y / rad);
    }
  }

  return(z + rad + (DBL)TPat->Vals.Arms * phi / TWO_M_PI + turb_val);
}


/*****************************************************************************
*
* FUNCTION
*
*   spiral2_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   TPat   -- Texture pattern struct
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*   Spiral whirles around z-axis.
*   The number of "arms" is defined in the TPat.
*
* CHANGES
*
*   Aug 1994 : Creation.
*   Oct 1994 : adapted from pigment by [CY]
*
******************************************************************************/

static DBL spiral2_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  DBL rad, phi, turb_val;
  DBL x = EPoint[X];
  DBL y = EPoint[Y];
  DBL z = EPoint[Z];
  TURB *Turb;

  if ((Turb=Search_For_Turb(TPat->Warps)) != NULL)
  {
    turb_val = Turb->Turbulence[X] * Turbulence(EPoint,Turb,TPat);
  }
  else
  {
    turb_val = 0.0;
  }

  /* Get distance from z-axis. */

  rad = sqrt(x * x + y * y);

  /* Get angle in x,y-plane (0...2 PI) */

  if (rad == 0.0)
  {
    phi = 0.0;
  }
  else
  {
    if (x < 0.0)
    {
      phi = 3.0 * M_PI_2 - asin(y / rad);
    }
    else
    {
      phi = M_PI_2 + asin(y / rad);
    }
  }

  turb_val = Triangle_Wave(z + rad + (DBL)TPat->Vals.Arms * phi / TWO_M_PI +
                           turb_val);

  return(Triangle_Wave(rad) + turb_val);
}


/*****************************************************************************
*
* FUNCTION
*
*   spherical_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   -
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL spherical_pattern (VECTOR EPoint)
{
  register DBL value;

  VLength(value, EPoint);
  CLIP_DENSITY(value);

  return(value);
}


/*****************************************************************************
*
* FUNCTION
*
*   waves_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   TPat   -- Texture pattern struct
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION   : Note this pattern is only used for pigments and textures.
*                 Normals have a specialized pattern for this.
*
* CHANGES
*
*   Nov 1994 : adapted from normal by [CY]
*
******************************************************************************/

static DBL waves_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  register unsigned int i;
  register DBL length, index;
  DBL scalar = 0.0;
  VECTOR point;

  for (i = 0 ; i < Number_Of_Waves ; i++)
  {
    VSub (point, EPoint, Wave_Sources[i]);
    VLength (length, point);

    if (length == 0.0)
    {
      length = 1.0;
    }

    index = length * TPat->Frequency * frequency[i] + TPat->Phase;

    scalar += cycloidal(index)/frequency[i];
  }

  scalar = 0.2*(2.5+(scalar / (DBL)Number_Of_Waves));

  return(scalar);
}


/*****************************************************************************
*
* FUNCTION
*
*   wood_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   TPat   -- Texture pattern struct
*
* OUTPUT
*
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
* CHANGES
*
*   Oct 1994 : adapted from pigment by [CY]
*
******************************************************************************/

static DBL wood_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  register DBL length;
  VECTOR WoodTurbulence;
  VECTOR point;
  DBL x=EPoint[X];
  DBL y=EPoint[Y];
  TURB *Turb;

  if ((Turb=Search_For_Turb(TPat->Warps)) != NULL)
  {
    DTurbulence (WoodTurbulence, EPoint,Turb);
    point[X] = cycloidal((x + WoodTurbulence[X]) * Turb->Turbulence[X]);
    point[Y] = cycloidal((y + WoodTurbulence[Y]) * Turb->Turbulence[Y]);
  }
  else
  {
    point[X] = 0.0;
    point[Y] = 0.0;
  }
  point[Z] = 0.0;

  point[X] += x;
  point[Y] += y;

  /* point[Z] += z; Deleted per David Buck --  BP 7/91 */

  VLength (length, point);

  return(length);
}


/*****************************************************************************
*
* FUNCTION
*
*   wrinkles_pattern
*
* INPUT
*
*   EPoint -- The point in 3d space at which the pattern
*   is evaluated.
*   
* OUTPUT
*   
* RETURNS
*
*   DBL value in the range 0.0 to 1.0
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION   : Note this pattern is only used for pigments and textures.
*                 Normals have a specialized pattern for this.
*
* CHANGES
*
*   Nov 1994 : adapted from normal by [CY]
*
******************************************************************************/

static DBL wrinkles_pattern (VECTOR EPoint, TPATTERN *TPat)
{
  register int i;
  DBL lambda = 2.0;
  DBL omega = 0.5;
  DBL value;
  VECTOR temp;
  DBL noise;
  int noise_generator = 0;

  if (TPat != NULL)
    noise_generator = (TPat->Flags & NOISE_FLAGS) >> 4;
  if (noise_generator == 0)
    noise_generator = opts.Noise_Generator;

  if(noise_generator>1)
  {
    noise = Noise(EPoint, TPat)*2.0-0.5;
    value = min(max(noise,0.0),1.0);
  }
  else
  {
      value = Noise(EPoint, TPat);
  }

  for (i = 1; i < 10; i++)
  {
    VScale(temp,EPoint,lambda);

    if(noise_generator>1)
    {
      noise = Noise(temp, TPat)*2.0-0.5;
      value += omega * min(max(noise,0.0),1.0);
    }
    else
    {
      value += omega * Noise(temp, TPat);
    }

    lambda *= 2.0;

    omega *= 0.5;
  }

  return(value/2.0);
}


/*****************************************************************************
*
* FUNCTION
*
*   IntPickInCube(tvx,tvy,tvz, p1)
*    a version of PickInCube that takes integers for input
*
* INPUT
*
*   ?
*
* OUTPUT
*   
* RETURNS
*
*   long integer hash function used, to speed up cacheing.
*   
* AUTHOR
*
*   original PickInCube by Jim McElhiney
*   this integer one modified by Nathan Kopp
*   
* DESCRIPTION
*
*   A subroutine to go with crackle.
*
*   Pick a random point in the same unit-sized cube as tv, in a
*   predictable way, so that when called again with another point in
*   the same unit cube, p1 is picked to be the same.
*
* CHANGES
*
******************************************************************************/

static int IntPickInCube(int tvx, int tvy, int tvz, VECTOR  p1)
{
  int seed, temp;

  seed = Hash3d(tvx&0xFFF,tvy&0xFFF,tvz&0xFFF);

  temp = POV_GET_OLD_RAND(); /* save current seed */
  POV_SRAND(seed);

  p1[X] = tvx + FRAND();
  p1[Y] = tvy + FRAND();
  p1[Z] = tvz + FRAND();

  POV_SRAND(temp);  /* restore */

  return((int)seed);
}


/*****************************************************************************
*
* FUNCTION
*
*   PickInCube(tv, p1)
*
* INPUT
*
*   ?
*
* OUTPUT
*   
* RETURNS
*
*   long integer hash function used, to speed up cacheing.
*   
* AUTHOR
*
*   Jim McElhiney
*   
* DESCRIPTION
*
*   A subroutine to go with crackle.
*
*   Pick a random point in the same unit-sized cube as tv, in a
*   predictable way, so that when called again with another point in
*   the same unit cube, p1 is picked to be the same.
*
* CHANGES
*
******************************************************************************/

int PickInCube(VECTOR tv, VECTOR  p1)
{
  int seed, temp;
  VECTOR flo;

  /*
   * This uses floor() not FLOOR, so it will not be a mirror
   * image about zero in the range -1.0 to 1.0. The viewer
   * won't see an artefact around the origin.
   */

  flo[X] = floor(tv[X] - EPSILON);
  flo[Y] = floor(tv[Y] - EPSILON);
  flo[Z] = floor(tv[Z] - EPSILON);

  seed = Hash3d((int)flo[X], (int)flo[Y], (int)flo[Z]);

  temp = POV_GET_OLD_RAND(); /* save current seed */
  
  POV_SRAND(seed);

  p1[X] = flo[X] + FRAND();
  p1[Y] = flo[Y] + FRAND();
  p1[Z] = flo[Z] + FRAND();

  POV_SRAND(temp);  /* restore */

  return((int)seed);
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

const DBL INV_SQRT_3_4 = 1.154700538;
DBL quilt_cubic(DBL t, DBL p1, DBL p2)
{
	DBL it=(1-t);
	DBL itsqrd=it*it;
	/* DBL itcubed=it*itsqrd; */
	DBL tsqrd=t*t;
	DBL tcubed=t*tsqrd;
	DBL val;

	/* Originally coded as...

	val= (DBL)(itcubed*n1+(tcubed)*n2+3*t*(itsqrd)*p1+3*(tsqrd)*(it)*p2);

	re-written by CEY to optimise because n1=0 n2=1 always.

	*/

	val = (tcubed + 3.0*t*itsqrd*p1 + 3.0*tsqrd*it*p2) * INV_SQRT_3_4;

	return(val);
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static DBL fractal_exterior_color(TPATTERN *TPat, int iters, DBL a, DBL b)
{
    switch(TPat->Vals.Fractal.exterior_type)
    {
      case 0:
          return  (DBL)TPat->Vals.Fractal.efactor;
      case 1:
          return (DBL)iters / (DBL)TPat->Vals.Fractal.Iterations;
      case 2:
          return a * (DBL)TPat->Vals.Fractal.efactor;
      case 3:
          return b * (DBL)TPat->Vals.Fractal.efactor;
      case 4:
          return a*a * (DBL)TPat->Vals.Fractal.efactor;
      case 5:
          return b*b * (DBL)TPat->Vals.Fractal.efactor;
      case 6:
          return sqrt(a*a+b*b) * (DBL)TPat->Vals.Fractal.efactor;
    }
    return 0;
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static DBL fractal_interior_color(TPATTERN *TPat, int /*iters*/, DBL a, DBL b, DBL mindist2)
{
    switch(TPat->Vals.Fractal.interior_type)
    {
      case 0:
          return  (DBL)TPat->Vals.Fractal.ifactor;
      case 1:
          return sqrt(mindist2) * (DBL)TPat->Vals.Fractal.ifactor;
      case 2:
          return a * (DBL)TPat->Vals.Fractal.ifactor;
      case 3:
          return b * (DBL)TPat->Vals.Fractal.ifactor;
      case 4:
          return a*a * (DBL)TPat->Vals.Fractal.ifactor;
      case 5:
          return b*b * (DBL)TPat->Vals.Fractal.ifactor;
      case 6:
          return a*a+b*b * (DBL)TPat->Vals.Fractal.ifactor;
    }
    return 0;
}


/*****************************************************************************
*
* FUNCTION
*
*   Create_Density_File
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Create a density file structure.
*
* CHANGES
*
*   Dec 1996 : Creation.
*
******************************************************************************/

DENSITY_FILE *Create_Density_File()
{
  DENSITY_FILE *New;

  New = (DENSITY_FILE *)POV_MALLOC(sizeof(DENSITY_FILE), "density file");

  New->Interpolation = NO_INTERPOLATION;

  New->Data = (DENSITY_FILE_DATA *)POV_MALLOC(sizeof(DENSITY_FILE_DATA), "density file data");

  New->Data->References = 1;

  New->Data->Name = NULL;

  New->Data->Sx =
  New->Data->Sy =
  New->Data->Sz = 0;

  New->Data->Type = 0;

  New->Data->Density32 = NULL;
  New->Data->Density16 = NULL;
  New->Data->Density8 = NULL;

  return (New);
}


/*****************************************************************************
*
* FUNCTION
*
*   Copy_Density_File
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Copy a density file structure.
*
* CHANGES
*
*   Dec 1996 : Creation.
*
******************************************************************************/

DENSITY_FILE *Copy_Density_File(DENSITY_FILE *Old)
{
  DENSITY_FILE *New;

  if (Old != NULL)
  {
    New = (DENSITY_FILE *)POV_MALLOC(sizeof(DENSITY_FILE), "density file");

    *New = *Old;

    New->Data->References++;
  }
  else
  {
    New=NULL;
  }

  return(New);
}


/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Density_File
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Destroy a density file structure.
*
* CHANGES
*
*   Dec 1996 : Creation.
*
******************************************************************************/

void Destroy_Density_File(DENSITY_FILE *Density_File)
{
	if(Density_File != NULL)
	{
		if((--(Density_File->Data->References)) == 0)
		{
			POV_FREE(Density_File->Data->Name);

			if(Density_File->Data->Type == 4)
			{
				POV_FREE(Density_File->Data->Density32);
			}
			else if(Density_File->Data->Type == 2)
			{
				POV_FREE(Density_File->Data->Density16);
			}
			else if(Density_File->Data->Type == 1)
			{
				POV_FREE(Density_File->Data->Density8);
			}

			POV_FREE(Density_File->Data);
		}

		POV_FREE(Density_File);
	}
}

void Read_Density_File(DENSITY_FILE *df)
{
	int x, y, z, sx, sy, sz;
	IStream *file;
	unsigned long len;

	if (df == NULL)
		return;

	/* Allocate and read density file. */

	if((df != NULL) && (df->Data->Name != NULL))
	{
		if((file = Locate_File(df->Data->Name, POV_File_Data_DF3, NULL, true)) == NULL)
			Error("Cannot read media density file.");

		sx = df->Data->Sx = readushort(file);
		sy = df->Data->Sy = readushort(file);
		sz = df->Data->Sz = readushort(file);

		file->seekg(0, IOBase::seek_end);
		len = file->tellg() - 6;
		file->seekg(6);

		// figure out the data size
		if((sx * sy * sz * 4) == len)
		{
			df->Data->Type = 4;

			unsigned int *map = (unsigned int *)POV_MALLOC(sx * sy * sz * sizeof(unsigned int), "media density file data 32 bit");

			for (z = 0; z < sz; z++)
			{
				for (y = 0; y < sy; y++)
				{
					for (x = 0; x < sx; x++)
						map[z * sy * sx + y * sx + x] = readuint(file);
				}
			}

			df->Data->Density32 = map;
		}
		else if((sx * sy * sz * 2) == len)
		{
			df->Data->Type = 2;

			unsigned short *map = (unsigned short *)POV_MALLOC(sx * sy * sz * sizeof(unsigned short), "media density file data 16 bit");

			for (z = 0; z < sz; z++)
			{
				for (y = 0; y < sy; y++)
				{
					for (x = 0; x < sx; x++)
						map[z * sy * sx + y * sx + x] = readushort(file);
				}
			}

			df->Data->Density16 = map;
		}
		else if((sx * sy * sz) == len)
		{
			df->Data->Type = 1;

			unsigned char *map = (unsigned char *)POV_MALLOC(sx * sy * sz * sizeof(unsigned char), "media density file data 8 bit");
			
			for (z = 0; z < sz; z++)
			{
				for (y = 0; y < sy; y++)
					file->read((char *)(&(map[z * sy * sx + y * sx])), sizeof(unsigned char) * sx);
			}

			df->Data->Density8 = map;
		}
		else
			Error("Invalid density file size");

		if (file != NULL)
		{
			delete file;
		}
	}
}

static unsigned short readushort(IStream *infile)
{
  short i0 = 0, i1 = 0;

  if ((i0 = infile->Read_Byte ()) == EOF || (i1 = infile->Read_Byte ()) == EOF)
  {
    Error("Error reading density_file");
  }

  return (((unsigned short)i0 << 8) | (unsigned short)i1);
}

static unsigned int readuint(IStream *infile)
{
  int i0 = 0, i1 = 0, i2 = 0, i3 = 0;

  if ((i0 = infile->Read_Byte ()) == EOF || (i1 = infile->Read_Byte ()) == EOF ||
      (i2 = infile->Read_Byte ()) == EOF || (i3 = infile->Read_Byte ()) == EOF)
  {
    Error("Error reading density_file");
  }

  return (((unsigned int)i0 << 24) | ((unsigned int)i1 << 16) | ((unsigned int)i2 << 8) | (unsigned int)i3);
}

// This should be called once, either at povray start or the first time
// a fractal pattern is created:
static void InitializeBinomialCoefficients()
{
    int* ptr = BinomialCoefficients;
    *ptr = 1; ++ptr;

    for(unsigned n=1; n<=FRACTAL_MAX_EXPONENT; ++n)
    {
        *ptr = 1; ++ptr;
        for(unsigned k=1; k<n; ++k)
        {
            *ptr = *(ptr-(n+1)) + *(ptr-n); ++ptr;
        }
        *ptr = 1; ++ptr;
    }
    ptr = BinomialCoefficients+1;
    for(unsigned m=1; m<=FRACTAL_MAX_EXPONENT; ++m)
    {
        ++ptr;
        for(unsigned k=1; k<m; ++k)
        {
            if((k&2)!=0) *ptr = -(*ptr);
            ++ptr;
        }
        if((m&2)!=0) *ptr = -(*ptr);
        ++ptr;
    }

    BinomialCoefficientsInited = true;
}

END_POV_NAMESPACE
