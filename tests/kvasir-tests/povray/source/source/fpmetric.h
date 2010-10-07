/****************************************************************************
 *                  fpmetric.h
 *
 * This module contains all defines, typedefs, and prototypes for fpmetric.cpp.
 *
 * This module was written by D.Skarda&T.Bily and modified by R.Suzuki.
 * Ported to POV-Ray 3.5 by Thorsten Froehlich.
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
 * $File: //depot/povray/3.6-release/source/fpmetric.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef FPMETRIC_H
#define FPMETRIC_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define PARAMETRIC_OBJECT        (PATCH_OBJECT) 


/*****************************************************************************
* Global variables
******************************************************************************/

extern METHODS Parametric_Methods;


/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Parametric_Struct PARAMETRIC;
typedef struct PrecompParValues_Struct PRECOMP_PAR_DATA; 

struct PrecompParValues_Struct 
{
   int      use, depth;
   char     flags;

   DBL      *Low[3], *Hi[3];     /*  X,Y,Z  */
 };


struct Parametric_Struct 
{
	OBJECT_FIELDS
	FUNCTION_PTR Function[3];
	DBL umin, umax, vmin, vmax;
	DBL accuracy;
  DBL max_gradient;
	int Inverted; 

	int container_shape;
	union
	{
		struct
		{
			VECTOR center;
			DBL radius;
		} sphere;
		struct
		{
			VECTOR corner1;
			VECTOR corner2;
		} box;
	} container;

	// internal use only
	PRECOMP_PAR_DATA *PData;
	DBL last_u, last_v;
};


/*****************************************************************************
* Global functions
******************************************************************************/

PARAMETRIC  *Create_Parametric (void);
void Destroy_Parametric (OBJECT *Object);
void *Copy_Parametric (OBJECT *Object);
void Compute_Parametric_BBox (PARAMETRIC *Param);

PRECOMP_PAR_DATA *Precompute_Parametric_Values(PARAMETRIC *Par, char flags, int depth);
PRECOMP_PAR_DATA *Copy_PrecompParVal (PRECOMP_PAR_DATA *PPV);
void Destroy_PrecompParVal (PRECOMP_PAR_DATA *PPV);

END_POV_NAMESPACE

#endif
