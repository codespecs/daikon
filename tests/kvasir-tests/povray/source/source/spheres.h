/****************************************************************************
 *                  spheres.h
 *
 * This module contains all defines, typedefs, and prototypes for SPHERES.CPP.
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
 * $File: //depot/povray/3.6-release/source/spheres.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef SPHERES_H
#define SPHERES_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define SPHERE_OBJECT (BASIC_OBJECT)


/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Sphere_Struct SPHERE;

struct Sphere_Struct
{
  OBJECT_FIELDS
  VECTOR Center;
  DBL    Radius;
};


/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

SPHERE *Create_Sphere (void);
void Compute_Sphere_BBox (SPHERE *Sphere);
int Intersect_Sphere (RAY *Ray, VECTOR Center, DBL Radius2, DBL *Depth1, DBL *Depth2);
SPHERE *Copy_Sphere (OBJECT *Object);
void Transform_Sphere (OBJECT *Object, TRANSFORM *Trans);
void Destroy_Sphere (OBJECT *Object);

END_POV_NAMESPACE

#endif
