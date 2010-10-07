/****************************************************************************
 *                  colour.h
 *
 * This module contains all defines, typedefs, and prototypes for COLOUR.CPP.
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
 * $File: //depot/povray/3.6-release/source/colour.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

/* NOTE: FRAME.H contains other colour stuff. */

#ifndef COLOUR_H
#define COLOUR_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define RED2GRAY   0.297
#define GREEN2GRAY 0.589
#define BLUE2GRAY  0.114
#define GREY_SCALE3(CR,CG,CB) (RED2GRAY*(CR) + GREEN2GRAY*(CG) + BLUE2GRAY*(CB))
#define GREY_SCALE(C) GREY_SCALE3((C)[pRED],(C)[pGREEN],(C)[pBLUE])

/*****************************************************************************
* Global typedefs
******************************************************************************/




/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

COLOUR *Create_Colour (void);
COLOUR *Copy_Colour (COLOUR Old);
BLEND_MAP_ENTRY *Create_BMap_Entries (int Map_Size);
BLEND_MAP_ENTRY *Copy_BMap_Entries (BLEND_MAP_ENTRY *Old,int Map_Size,int Type);
BLEND_MAP *Create_Blend_Map (void);
BLEND_MAP *Copy_Blend_Map (BLEND_MAP *Old);
DBL Colour_Distance (COLOUR colour1, COLOUR colour2);
DBL Colour_Distance_RGBT (COLOUR colour1, COLOUR colour2);
void Add_Colour (COLOUR result, COLOUR colour1, COLOUR colour2);
void Scale_Colour (COLOUR result, COLOUR colour, DBL factor);
void Clip_Colour (COLOUR result, COLOUR colour);
void Destroy_Blend_Map (BLEND_MAP *BMap);
DBL RGBtoHue( COLOUR c );


/*****************************************************************************
* Inline functions
******************************************************************************/

// Add a scaled colour.
//   v  = v1 + k * v2;
//   v += k * v2;
inline void CRGBAddScaledEq(COLOUR v, COLC k, const COLOUR v2)
{
	v[X] += k * v2[X];
	v[Y] += k * v2[Y];
	v[Z] += k * v2[Z];
}

// Linear combination of 2 colours. [CEY]
//   c = k1 * c1 + k2 * c2
inline void CLinComb2(COLOUR c, COLC k1, const COLOUR c1, COLC k2, const COLOUR c2)
{
	c[pRED]    = k1 * c1[pRED]    + k2 * c2[pRED];
	c[pGREEN]  = k1 * c1[pGREEN]  + k2 * c2[pGREEN];
	c[pBLUE]   = k1 * c1[pBLUE]   + k2 * c2[pBLUE];
	c[pFILTER] = k1 * c1[pFILTER] + k2 * c2[pFILTER];
	c[pTRANSM] = k1 * c1[pTRANSM] + k2 * c2[pTRANSM];
}

inline void CRGBLinComb2(COLOUR c, COLC k1, const COLOUR c1, COLC k2, const COLOUR c2)
{
	c[X] = k1 * c1[X] + k2 * c2[X];
	c[Y] = k1 * c1[Y] + k2 * c2[Y];
	c[Z] = k1 * c1[Z] + k2 * c2[Z];
}

END_POV_NAMESPACE

#endif
