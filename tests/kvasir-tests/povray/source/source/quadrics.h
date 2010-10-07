/****************************************************************************
 *                  quadrics.h
 *
 * This module contains all defines, typedefs, and prototypes for QUADRICS.CPP.
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
 * $File: //depot/povray/3.6-release/source/quadrics.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef QUADRICS_H
#define QUADRICS_H

#include "planes.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define QUADRIC_OBJECT (BASIC_OBJECT)



/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Quadric_Struct QUADRIC;

struct Quadric_Struct
{
  OBJECT_FIELDS
  VECTOR Square_Terms;
  VECTOR Mixed_Terms;
  VECTOR Terms;
  DBL Constant;
  bool Automatic_Bounds;
};



/*****************************************************************************
* Global variables
******************************************************************************/

extern METHODS Quadric_Methods;



/*****************************************************************************
* Global functions
******************************************************************************/

QUADRIC *Create_Quadric (void);
void Compute_Quadric_BBox (QUADRIC *Quadric, VECTOR Min, VECTOR Max);
void Compute_Plane_Min_Max (PLANE *Plane, VECTOR Min, VECTOR Max);

END_POV_NAMESPACE

#endif
