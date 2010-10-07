/****************************************************************************
 *               colutils.cpp
 *
 * This module implements the utility functions for colors.
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
 * $File: //depot/povray/3.6-release/source/colutils.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <time.h>
#include "frame.h"
#include "vector.h"
#include "bbox.h"
#include "chi2.h"
#include "colour.h"
#include "interior.h"
#include "lighting.h"
#include "normal.h"
#include "objects.h"
#include "octree.h"
#include "optout.h"
#include "povray.h"
#include "radiosit.h"
#include "ray.h"
#include "render.h"
#include "targa.h"
#include "texture.h"
#include "vbuffer.h"
#include "userio.h"
#include "userdisp.h"
#include "parse.h"
#include "tokenize.h"
#include "povmsend.h"
#include "colutils.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
*
* FUNCTION
*
*   gamma_correct
*
* INPUT
*
*   Colour (an array of DBL's)
*   
* OUTPUT
*
*   The colour array
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   Adjust RGB values for overall brightness and gamma curve so that the
*   image appears a constant brightness regardless of the hardware that
*   is being used.  This can't be part of extract_colors, since
*   extract_colors is used multiple times in anti aliasing, and this has
*   to be called exactly once per pixel output.
*
*   If gamma correction is enabled, then file and display pixel values 
*   will be corrected for the current assumed_gamma, which has default
*   value 1.00, but can be set by a global_settings {assumed_gamma} in the
*   source file, and the DisplayGamma value, which can be set in the INI
*   file via Display_Gamma=n.n or defaults to DEFAULT_DISPLAY_GAMMA
*   (2.2 unless another value set in the system specific config.h files.)
*
*   If gamma correction is turned off (if no global_settings {assumed_gamma}
*   line occurrs in the scene file) then no gamma correction will be done 
*   on the pixel values.  If gamma correction is turned off, the 
*   DisplayGamma value is still relevant for PNG output files, since if 
*   gamma correction is disabled, it is presumed that the scene is 
*   "pre-corrected" for the current hardware, and a gAMA chunk with the 
*   current 1/DisplayGamma value will be output.
*
*   When DisplayGamma approximately equals assumed_gamma, it means that we
*   don't need to do any gamma correction since it is already the correct
*   brightness.  opts.GammaFactor is calculated once in tokenize.c to
*   be assumed_gamma/DisplayGamma to avoid re-calculation for each pixel.
*
* CHANGES
*
*   Apr 1995 :  Created function - Jim McElhiney
*   Oct 1995 :  Modified to do proper system gamma correction [AED]
*
******************************************************************************/

void gamma_correct(COLOUR Colour)
{
  if (opts.Options & GAMMA_CORRECT)
  {
    Colour[pRED]   = pow(Colour[pRED],  opts.GammaFactor);
    Colour[pGREEN] = pow(Colour[pGREEN],opts.GammaFactor);
    Colour[pBLUE]  = pow(Colour[pBLUE], opts.GammaFactor);
    Colour[pTRANSM]  = 1.0f - pow((1.0f - Colour[pTRANSM]), opts.GammaFactor);
  }
}


/*****************************************************************************
*
* FUNCTION
*
*   extract_colors
*
* INPUT
*
*   Colour, Red, Green, Blue, Alpha, grey
*   
* OUTPUT
*
*   Red, Green, Blue, Alpha, grey
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   Create appropriate rgba values.
*
* CHANGES
*
*   Aug 1994 : Extracted common code from Start_Tracing - Eduard Schwan
*   Jun 1995 : Alpha channel support -CEY
*
******************************************************************************/

void extract_colors(COLOUR Colour, unsigned char *Red, unsigned char  *Green, unsigned char  *Blue, unsigned char  *Alpha, DBL *grey)
{
  COLOUR ColourG;

  Clip_Colour(ColourG, Colour);
  gamma_correct(ColourG);

  if (opts.PaletteOption == GREY)
  {
    *grey = ColourG[pRED] * GREY_SCALE(ColourG);

    *Red = *Green = *Blue = (unsigned char)((*grey) * maxclr);
  }
  else
  {
    *Red   = (unsigned char)(ColourG[pRED]    * maxclr);
    *Green = (unsigned char)(ColourG[pGREEN]  * maxclr);
    *Blue  = (unsigned char)(ColourG[pBLUE]   * maxclr);
    *Alpha = (unsigned char)(ColourG[pTRANSM] * maxclr);
  }
}

/*****************************************************************************
*
* FUNCTION
*
*   extract_colors_nocorrect
*
* INPUT
*
*   Colour, Red, Green, Blue, Alpha, grey
*   
* OUTPUT
*
*   Red, Green, Blue, Alpha, grey
*   
* RETURNS
*   
* AUTHOR
*
*   Christoph Hormann
*   
* DESCRIPTION
*
*   Create appropriate rgba values without gamma correction and clipping.
*   Used for reading images in continued trace.
*
* CHANGES
*
*   Jul 2004 : Creation based on extract_colors()
*
******************************************************************************/

void extract_colors_nocorrect(COLOUR Colour, unsigned char *Red, unsigned char  *Green, unsigned char  *Blue, unsigned char  *Alpha, DBL *grey)
{
  int RedU, GreenU, BlueU, AlphaU;

  if (opts.PaletteOption == GREY)
  {
    *grey = Colour[pRED] * GREY_SCALE(Colour);

    RedU = GreenU = BlueU = (int)((*grey) * maxclr);
    AlphaU = 0;
  }
  else
  {
    RedU   = (int)(Colour[pRED]    * maxclr);
    GreenU = (int)(Colour[pGREEN]  * maxclr);
    BlueU  = (int)(Colour[pBLUE]   * maxclr);
    AlphaU = (int)(Colour[pTRANSM] * maxclr);
  }

  if (RedU > UCHAR_MAX) RedU = UCHAR_MAX;
  else if (RedU < 0) RedU = 0;

  if (GreenU > UCHAR_MAX) GreenU = UCHAR_MAX;
  else if (GreenU < 0) GreenU = 0;

  if (BlueU > UCHAR_MAX) BlueU = UCHAR_MAX;
  else if (BlueU < 0) BlueU = 0;

  if (AlphaU > UCHAR_MAX) AlphaU = UCHAR_MAX;
  else if (AlphaU < 0) AlphaU = 0;

  *Red   = RedU;
  *Green = GreenU;
  *Blue  = BlueU;
  *Alpha = AlphaU;
}


/*****************************************************************************
*
* FUNCTION
*
*   colour2photonRgbe
*
* INPUT
*
*   
* OUTPUT
*
*   
* RETURNS
*   
* AUTHOR
*
  originally float2rgb

  Bruce Walter - http://www.graphics.cornell.edu/online/formats/rgbe/

 This file contains code to read and write four byte rgbe file format
 developed by Greg Ward.  It handles the conversions between rgbe and
 pixels consisting of floats.  The data is assumed to be an array of floats.
 By default there are three floats per pixel in the order red, green, blue.
 (RGBE_DATA_??? values control this.)  Only the mimimal header reading and 
 writing is implemented.  Each routine does error checking and will return
 a status value as defined below.  This code is intended as a skeleton so
 feel free to modify it to suit your needs.

 (Place notice here if you modified the code.)
 posted to http://www.graphics.cornell.edu/~bjw/
 written by Bruce Walter  (bjw@graphics.cornell.edu)  5/26/95
 based on code written by Greg Ward
*   
* DESCRIPTION
*
*   standard conversion from float pixels to rgbe pixels.
*
* CHANGES
*
*  May 25, 20020 - incorporated into POV-Ray - Nathan Kopp
*                  For photons, our exponent will almost always be
*                  negative, so we use e+250 to get a larger range of negative
*                  exponents.
*
******************************************************************************/
void colour2photonRgbe(SMALL_COLOUR rgbe, COLOUR c)
{
  float v;
  int e;

  v = c[pRED];
  if (c[pGREEN] > v) v = c[pGREEN];
  if (c[pBLUE] > v) v = c[pBLUE];
  if (v < 1e-32) {
    rgbe[0] = rgbe[1] = rgbe[2] = rgbe[3] = 0;
  }
  else {
    v = frexp(v,&e) * 256.0/v;
    rgbe[0] = (unsigned char) (c[pRED] * v);
    rgbe[1] = (unsigned char) (c[pGREEN] * v);
    rgbe[2] = (unsigned char) (c[pBLUE] * v);
    //rgbe[3] = (unsigned char) (e + 128);
    rgbe[3] = (unsigned char) (e + 250);
  }
}

/*****************************************************************************
*
* FUNCTION
*
*   photonRgbe2colour
*
* INPUT
*
*   
* OUTPUT
*
*   
* RETURNS
*   
* AUTHOR
*
  originally float2rgb

  Bruce Walter - http://www.graphics.cornell.edu/online/formats/rgbe/

 This file contains code to read and write four byte rgbe file format
 developed by Greg Ward.  It handles the conversions between rgbe and
 pixels consisting of floats.  The data is assumed to be an array of floats.
 By default there are three floats per pixel in the order red, green, blue.
 (RGBE_DATA_??? values control this.)  Only the mimimal header reading and 
 writing is implemented.  Each routine does error checking and will return
 a status value as defined below.  This code is intended as a skeleton so
 feel free to modify it to suit your needs.

 (Place notice here if you modified the code.)
 posted to http://www.graphics.cornell.edu/~bjw/
 written by Bruce Walter  (bjw@graphics.cornell.edu)  5/26/95
 based on code written by Greg Ward
*   
* DESCRIPTION
*
*   standard conversion from rgbe to float pixels
*   note: Ward uses ldexp(col+0.5,exp-(128+8)).  However we wanted pixels 
*         in the range [0,1] to map back into the range [0,1].            
*
* CHANGES
*
*  May 25, 20020 - incorporated into POV-Ray - Nathan Kopp
*                  For photons, our exponent will almost always be
*                  negative, so we use e+250 to get a larger range of negative
*                  exponents.
*
******************************************************************************/
void photonRgbe2colour(COLOUR c, SMALL_COLOUR rgbe)
{
  float f;

  if (rgbe[3]) {   /*nonzero pixel*/
    f = ldexp(1.0,rgbe[3]-(int)(250+8));
    c[pRED] = rgbe[0] * f;
    c[pGREEN] = rgbe[1] * f;
    c[pBLUE] = rgbe[2] * f;
  }
  else
    c[pRED] = c[pGREEN] = c[pBLUE] = 0.0;
}

/*****************************************************************************
*
* FUNCTION
*
*   prepare_output_line
*
* INPUT
*
*   Line - input/output line
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Christoph Hormann
*
* DESCRIPTION
*
*   Clip and gamma correct a line for file output
*
* CHANGES
*
*   Mar 2003 : Creation.
*
******************************************************************************/

void prepare_output_line(COLOUR *Line)
{
  int i;

  for (i = 0; i <= Frame.Screen_Width ; i++)
  {
    Clip_Colour(Line[i], Line[i]);
    gamma_correct(Line[i]);
  }
}

END_POV_NAMESPACE
