/****************************************************************************
 *               userdisp.cpp
 *
 * This module contains non-graphic display I/O routines.
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
 * $File: //depot/povray/3.6-release/source/userdisp.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <stdarg.h>
#include "frame.h"
#include "vector.h"
#include "parse.h"
#include "povray.h"
#include "tokenize.h"
#include "userio.h"
#include "userdisp.h"

BEGIN_POV_NAMESPACE

/***************************************************************************
 *
 * Dummy display routines for non-graphic Unix ports
 *
 **************************************************************************/

static DBL Display_Width_Scale, Display_Height_Scale; // GLOBAL VARIABLE
static int Prev_X, Prev_Y; // GLOBAL VARIABLE

/****************************************************************************/
int POV_Std_Display_Init(int w, int  h)
{
  Display_Width_Scale = 78.0 / (DBL)w;
  Display_Height_Scale = 24.0 / (DBL)h;
  Prev_X = 0;
  Prev_Y = 0;
  fprintf(stderr, "");

  return true;
}

/****************************************************************************/
void POV_Std_Display_Finished()
{
  char s[3];

  fprintf(stderr, "\007\007");
  if (opts.Options & PROMPTEXIT)
  {
    fgets(s, 2, stdin);
  }
}

/****************************************************************************/
void POV_Std_Display_Close()
{
  fprintf(stderr, "");
}

/****************************************************************************/
void POV_Std_Display_Plot(int x, int y, unsigned int r, unsigned int g, unsigned int b, unsigned int)
{
  int sx = (int)(Display_Width_Scale * ((DBL)x));
  int sy = (int)(Display_Height_Scale * ((DBL)y));
  char I;
  unsigned char G[6] = { ' ', '.', 'o', '*', '@', 'M' };

  if (sy > Prev_Y)
  {
    Prev_Y++;
    
    fprintf(stderr, "");
    
    Prev_X = 0;
  }
  
  if (sx > Prev_X)
  {
    I = (int)(((DBL)r * 1.80 + (DBL)g * 3.54 + (DBL)b * 0.66) / 256.0);

    fprintf(stderr, "%c", G[(int)I]);
    
    Prev_X++;
  }
}

/****************************************************************************/
void POV_Std_Display_Plot_Rect(int x1, int y1, int x2, int y2, unsigned int r, unsigned int g, unsigned int b, unsigned int a)
{
	int x,y;

	for(y = y1; y <= y2; y++)
	{
		for(x = x1; x <= x2; x++)
			POV_Std_Display_Plot(x, y, r, g, b, a);
	}
}

/*****************************************************************************
*
* FUNCTION
*
*   POV_Std_Display_Plot_Box
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Chris Young
*   
* DESCRIPTION
*
*   Generic box drawing routine which may be overriden in POV_DRAW_BOX
*   by a platform specific routine.
*
* CHANGES
*
*   Nov 1995 : Creation.
*
******************************************************************************/
void POV_Std_Display_Plot_Box(int x1, int y1, int x2, int y2, unsigned int r, unsigned int g, unsigned int b, unsigned int a)
{
     int x,y;

     for (x = x1; x <= x2; x++)
     {
       POV_DISPLAY_PLOT(opts.Preview_RefCon, x, y1, r, g, b, a);
       POV_DISPLAY_PLOT(opts.Preview_RefCon, x, y2, r, g, b, a);
     }

     for (y = y1; y <= y2; y++)
     {
       POV_DISPLAY_PLOT(opts.Preview_RefCon, x1, y, r, g, b, a);
       POV_DISPLAY_PLOT(opts.Preview_RefCon, x2, y, r, g, b, a);
     }
}

END_POV_NAMESPACE
