/****************************************************************************
 *                  discs.h
 *
 * This module contains all defines, typedefs, and prototypes for DISCS.CPP.
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
 * $File: //depot/povray/3.6-release/source/discs.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef DISCS_H
#define DISCS_H

BEGIN_POV_NAMESPACE


/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define DISC_OBJECT            (BASIC_OBJECT)



/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Disc_Struct DISC;

struct Disc_Struct
{
  OBJECT_FIELDS
  VECTOR center;    /* Center of the disc */
  VECTOR normal;    /* Direction perpendicular to the disc (plane normal) */
  DBL d;            /* The constant part of the plane equation */
  DBL iradius2;     /* Distance from center to inner circle of the disc */
  DBL oradius2;     /* Distance from center to outer circle of the disc */
};



/*****************************************************************************
* Global variables
******************************************************************************/




/*****************************************************************************
* Global functions
******************************************************************************/

DISC *Create_Disc (void);
void Compute_Disc (DISC *Disc);

END_POV_NAMESPACE

#endif
