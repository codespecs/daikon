/****************************************************************************
 *                  cones.h
 *
 * This module contains all defines, typedefs, and prototypes for CONES.CPP.
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
 * $File: //depot/povray/3.6-release/source/cones.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef CONES_H
#define CONES_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define CONE_OBJECT (BASIC_OBJECT)



/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Cone_Struct CYLINDER;
typedef struct Cone_Struct CONE;

struct Cone_Struct
{
  OBJECT_FIELDS
  VECTOR apex;        /* Center of the top of the cone */
  VECTOR base;        /* Center of the bottom of the cone */
  DBL apex_radius;    /* Radius of the cone at the top */
  DBL base_radius;    /* Radius of the cone at the bottom */
  DBL dist;           /* Distance to end of cone in canonical coords */
};



/*****************************************************************************
* Global variables
******************************************************************************/




/*****************************************************************************
* Global functions
******************************************************************************/

CONE *Create_Cone (void);
CONE *Create_Cylinder (void);
void Compute_Cone_Data (OBJECT *Object);
void Compute_Cylinder_Data (OBJECT *Object);
void Compute_Cone_BBox (CONE *Cone);

END_POV_NAMESPACE

#endif
