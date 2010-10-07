/****************************************************************************
 *                  sor.h
 *
 * This module contains all defines, typedefs, and prototypes for SOR.CPP.
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
 * $File: //depot/povray/3.6-release/source/sor.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/



#ifndef SOR_H
#define SOR_H

#include "bcyl.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor definitions
******************************************************************************/

#define SOR_OBJECT (STURM_OK_OBJECT)

/* Generate additional surface of revolution statistics. */

#define SOR_EXTRA_STATS 1




/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Sor_Struct SOR;
typedef struct Sor_Spline_Entry_Struct SOR_SPLINE_ENTRY;
typedef struct Sor_Spline_Struct SOR_SPLINE;

struct Sor_Spline_Entry_Struct
{
  DBL A, B, C, D;
};

struct Sor_Spline_Struct
{
  int References;
  SOR_SPLINE_ENTRY *Entry;
  BCYL *BCyl;                 /* bounding cylinder.                  */
};

struct Sor_Struct
{
  OBJECT_FIELDS
  int Number;
  SOR_SPLINE *Spline;      /* List of spline segments     */
  DBL Height1, Height2;    /* Min./Max. height            */
  DBL Radius1, Radius2;    /* Min./Max. radius            */
  DBL Base_Radius_Squared; /* Radius**2 of the base plane */
  DBL Cap_Radius_Squared;  /* Radius**2 of the cap plane  */
};



/*****************************************************************************
* Global variables
******************************************************************************/




/*****************************************************************************
* Global functions
******************************************************************************/

SOR  *Create_Sor (void);
void Compute_Sor_BBox (SOR *Sor);
void Compute_Sor (SOR *Sor, UV_VECT *P);

END_POV_NAMESPACE

#endif
