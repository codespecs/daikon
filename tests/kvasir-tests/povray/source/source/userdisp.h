/****************************************************************************
 *                  userdisp.h
 *
 * This module contains all defines, typedefs, and prototypes for userdisp.cpp
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
 * $File: //depot/povray/3.6-release/source/userdisp.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef USERDISP_H
#define USERDISP_H

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




/*****************************************************************************
* Global functions
******************************************************************************/

int POV_Std_Display_Init (int w, int h);
void POV_Std_Display_Close (void);
void POV_Std_Display_Finished (void);
void POV_Std_Display_Plot (int x, int y, unsigned int r, unsigned int g, unsigned int b, unsigned int a);
void POV_Std_Display_Plot_Rect (int x1, int y1, int x2, int y2, unsigned int r, unsigned int g, unsigned int b, unsigned int a);
void POV_Std_Display_Plot_Box (int x1,int y1,int x2,int y2, unsigned int r,unsigned int g,unsigned int b,unsigned int a);

END_POV_NAMESPACE

#endif /* USERDISP_H */
