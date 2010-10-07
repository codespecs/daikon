/****************************************************************************
 *                  express.h
 *
 * This module contains all defines, typedefs, and prototypes for EXPRESS.CPP.
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
 * $File: //depot/povray/3.6-release/source/express.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef EXPRESS_H
#define EXPRESS_H

BEGIN_POV_NAMESPACE


/*****************************************************************************
* Global preprocessor defines
******************************************************************************/




/*****************************************************************************
* Global typedefs
******************************************************************************/




/*****************************************************************************
* Global variables
******************************************************************************/
extern short Have_Vector;



/*****************************************************************************
* Global functions
******************************************************************************/

void Parse_Colour (COLOUR Colour);
BLEND_MAP *Parse_Blend_Map (int Blend_Type, int Pat_Type);
BLEND_MAP *Parse_Colour_Map (void);
BLEND_MAP *Parse_Blend_List (int Count, BLEND_MAP *Def_Map, int Blend_Type);
BLEND_MAP *Parse_Item_Into_Blend_List (int Blend_Type);
SPLINE *Parse_Spline (void);
DBL Parse_Float (void);
DBL Allow_Float (DBL defval);
int Allow_Vector (VECTOR Vect);
void Parse_UV_Vect (UV_VECT UV_Vect);
void Parse_Vector (VECTOR Vector);
void Parse_Vector4D (VECTOR Vector);
int Parse_Unknown_Vector (EXPRESS Express, bool allow_identifier = false, bool *had_identifier = NULL);
void Parse_Scale_Vector (VECTOR Vector);
DBL Parse_Float_Param (void);
void Parse_Float_Param2 (DBL *Val1, DBL *Val2);
void Init_Random_Generators (void);
void Destroy_Random_Generators (void);
DBL Parse_Signed_Float(void);

END_POV_NAMESPACE

#endif
