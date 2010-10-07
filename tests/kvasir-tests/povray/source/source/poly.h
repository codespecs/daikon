/****************************************************************************
 *                  poly.h
 *
 * This module contains all defines, typedefs, and prototypes for POLY.CPP.
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
 * $File: //depot/povray/3.6-release/source/poly.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef POLY_H
#define POLY_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define POLY_OBJECT    (STURM_OK_OBJECT)
#define CUBIC_OBJECT   (STURM_OK_OBJECT)
#define QUARTIC_OBJECT (STURM_OK_OBJECT)

/* Number of coefficients of a three variable polynomial of order x */

#define term_counts(x) (((x)+1)*((x)+2)*((x)+3)/6)



/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Poly_Struct POLY;

struct Poly_Struct
{
  OBJECT_FIELDS
  int Order;
  DBL *Coeffs;
};



/*****************************************************************************
* Global variables
******************************************************************************/

extern METHODS Poly_Methods;



/*****************************************************************************
* Global functions
******************************************************************************/

POLY *Create_Poly (int Order);
void Compute_Poly_BBox (POLY *Poly);

END_POV_NAMESPACE

#endif
