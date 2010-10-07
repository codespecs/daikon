/****************************************************************************
 *                  colour.cpp
 *
 * This module implements routines to manipulate colours.
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
 * $File: //depot/povray/3.6-release/source/colour.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "vector.h"
#include "colour.h"
#include "pigment.h"
#include "normal.h"
#include "texture.h"

#include <algorithm>

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/



/*****************************************************************************
* Local typedefs
******************************************************************************/



/*****************************************************************************
* Local variables
******************************************************************************/


/*****************************************************************************
* Static functions
******************************************************************************/



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

COLOUR *Create_Colour ()
{
  COLOUR *New;

  New = (COLOUR *)POV_MALLOC(sizeof (COLOUR), "color");

  Make_ColourA (*New, 0.0, 0.0, 0.0, 0.0, 0.0);

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

COLOUR *Copy_Colour (COLOUR Old)
{
  COLOUR *New;

  if (Old != NULL)
  {
    New = Create_Colour ();

    Assign_Colour(*New,Old);
  }
  else
  {
    New = NULL;
  }

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   Aug 1995 : Use POV_CALLOC to initialize entries. [DB]
*
******************************************************************************/

BLEND_MAP_ENTRY *Create_BMap_Entries (int Map_Size)
{
  BLEND_MAP_ENTRY *New;

  New = (BLEND_MAP_ENTRY *)POV_CALLOC(Map_Size, sizeof (BLEND_MAP_ENTRY), "blend map entry");

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*
* CHANGES
*
******************************************************************************/

BLEND_MAP_ENTRY *Copy_BMap_Entries (BLEND_MAP_ENTRY *Old, int Map_Size, int  Type)
{
  int i;
  BLEND_MAP_ENTRY *New;

  if (Old != NULL)
  {
    New = Create_BMap_Entries (Map_Size);

    for (i = 0; i < Map_Size; i++)
    {
      switch (Type)
      {
        case PIGMENT_TYPE:

          New[i].Vals.Pigment = Copy_Pigment(Old[i].Vals.Pigment);

          break;

        case NORMAL_TYPE:

          New[i].Vals.Tnormal = Copy_Tnormal(Old[i].Vals.Tnormal);

          break;

        case TEXTURE_TYPE:

          New[i].Vals.Texture = Copy_Textures(Old[i].Vals.Texture);

          break;

        case COLOUR_TYPE:
        case SLOPE_TYPE:

          New[i] = Old[i];

          break;
      }
    }
  }
  else
  {
    New = NULL;
  }

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_Blend_Map
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

BLEND_MAP *Create_Blend_Map ()
{
  BLEND_MAP *New;

  New = (BLEND_MAP *)POV_MALLOC(sizeof (BLEND_MAP), "blend map");

  New->Users = 1;

  New->Number_Of_Entries = 0;

  New->Type = COLOUR_TYPE;

  New->Blend_Map_Entries = NULL;

  New->Transparency_Flag = false;

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Blend_Map
*
* INPUT
*   
* OUTPUT
*
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

BLEND_MAP *Copy_Blend_Map (BLEND_MAP *Old)
{
  BLEND_MAP *New;

  New = Old;

  /* 
   * Do not increase the users field if it is negative.
   *
   * A negative users field incicates a reference to a static
   * or global memory area in the data segment, not on the heap!
   * Thus it must not be deleted later.
   */

  if ((New != NULL) && (New->Users >= 0))
  {
    New->Users++;
  }

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Colour_Distance
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

DBL Colour_Distance (COLOUR colour1, COLOUR  colour2)
{
  return (fabs(colour1[pRED]   - colour2[pRED]) +
          fabs(colour1[pGREEN] - colour2[pGREEN]) +
          fabs(colour1[pBLUE]  - colour2[pBLUE]));
}



/*****************************************************************************
*
* FUNCTION
*
*   Colour_Distance_RGBT
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

DBL Colour_Distance_RGBT (COLOUR colour1, COLOUR  colour2)
{
  return (fabs(colour1[pRED]    - colour2[pRED]) +
          fabs(colour1[pGREEN]  - colour2[pGREEN]) +
          fabs(colour1[pBLUE]   - colour2[pBLUE]) +
          fabs(colour1[pTRANSM] - colour2[pTRANSM]));
}



/*****************************************************************************
*
* FUNCTION
*
*   Add_Colour
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Add_Colour (COLOUR result, COLOUR  colour1, COLOUR  colour2)
{
  result[pRED]    = colour1[pRED]    + colour2[pRED];
  result[pGREEN]  = colour1[pGREEN]  + colour2[pGREEN];
  result[pBLUE]   = colour1[pBLUE]   + colour2[pBLUE];
  result[pFILTER] = colour1[pFILTER] + colour2[pFILTER];
  result[pTRANSM] = colour1[pTRANSM] + colour2[pTRANSM];
}



/*****************************************************************************
*
* FUNCTION
*
*   Scale_Colour
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Scale_Colour (COLOUR result, COLOUR  colour, DBL factor)
{
  result[pRED]    = colour[pRED]    * factor;
  result[pGREEN]  = colour[pGREEN]  * factor;
  result[pBLUE]   = colour[pBLUE]   * factor;
  result[pFILTER] = colour[pFILTER] * factor;
  result[pTRANSM] = colour[pTRANSM] * factor;
}



/*****************************************************************************
*
* FUNCTION
*
*   Clip_Colour
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Clip_Colour (COLOUR result, COLOUR  colour)
{
  if (colour[pRED] > 1.0)
  {
    result[pRED] = 1.0;
  }
  else
  {
    if (colour[pRED] < 0.0)
    {
      result[pRED] = 0.0;
    }
    else
    {
      result[pRED] = colour[pRED];
    }
  }

  if (colour[pGREEN] > 1.0)
  {
    result[pGREEN] = 1.0;
  }
  else
  {
    if (colour[pGREEN] < 0.0)
    {
      result[pGREEN] = 0.0;
    }
    else
    {
      result[pGREEN] = colour[pGREEN];
    }
  }

  if (colour[pBLUE] > 1.0)
  {
    result[pBLUE] = 1.0;
  }
  else
  {
    if (colour[pBLUE] < 0.0)
    {
      result[pBLUE] = 0.0;
    }
    else
    {
      result[pBLUE] = colour[pBLUE];
    }
  }

  if (colour[pFILTER] > 1.0)
  {
    result[pFILTER] = 1.0;
  }
  else
  {
    if (colour[pFILTER] < 0.0)
    {
      result[pFILTER] = 0.0;
    }
    else
    {
      result[pFILTER] = colour[pFILTER];
    }
  }

  if (colour[pTRANSM] > 1.0)
  {
    result[pTRANSM] = 1.0;
  }
  else
  {
    if (colour[pTRANSM] < 0.0)
    {
      result[pTRANSM] = 0.0;
    }
    else
    {
      result[pTRANSM] = colour[pTRANSM];
    }
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Blend_Map
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Destroy_Blend_Map (BLEND_MAP *BMap)
{
  int i;
  
  if (BMap != NULL)
  {
    if (--(BMap->Users) == 0)
    {
      for (i = 0; i < BMap->Number_Of_Entries; i++)
      {
        switch (BMap->Type)
        {
           case PIGMENT_TYPE:
           case DENSITY_TYPE:
             Destroy_Pigment(BMap->Blend_Map_Entries[i].Vals.Pigment);
             break;

           case NORMAL_TYPE:
             Destroy_Tnormal(BMap->Blend_Map_Entries[i].Vals.Tnormal);
             break;

           case TEXTURE_TYPE:
             Destroy_Textures(BMap->Blend_Map_Entries[i].Vals.Texture);
        }
      }

      POV_FREE (BMap->Blend_Map_Entries);

      POV_FREE (BMap);
    }
  }
}


/* NK phmap & post process - this code from Mike's MSGTracer */
/*****************************************************************************
*
* FUNCTION
*
*   RGBtoHue
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Nathan Kopp
*   
* DESCRIPTION
*
*   NK phmap & post process
*   this code from Mike's MSGTracer
*
* CHANGES
*
*   -
*
******************************************************************************/

DBL RGBtoHue( COLOUR c )
 {
  DBL r, g, b;
  DBL mx, mn, delta;
  DBL h, s, v;
  DBL w;

  r = c[0];
  g = c[1];
  b = c[2];

  /* ----------- Convert to HSV -------------- */

  mx = max3(r,g,b);
  mn = min3(r,g,b);

  v = mx;
  h = 3.0;

  delta = mx-mn;
  if( delta > 0.0 && mx > 0.0 )
   {
    s = delta/mx;

    if( r == mx )
      h = (g-b)/delta;
    else if( g == mx )
      h = 2.0 + (b-r)/delta;
    else if( b == mx )
      h = 4.0 + (r-g)/delta;
   }

  h *= 60.0;
  if( h < 0.0 ) h += 360.0;


  /* ------- Convert H to wavelength --------- */

  w = h + 60.0;                         /* Split ultraviolet/red at -30 */
  if( w > 360.0 ) w -= 360.0;           

  /* should we invert??????? */
  /*w = 360.0 - w;*/                        /* Invert H, 0 = blue, 360 = red    */
  /*w = 380.0 + (780.0-380.0)*(w/360.0);*/  /* Convert to wavelength, 380-780nm */
  w = 0.0 + (1.0-0.0)*(w/360.0);  /* Convert to range, 0-1 */

  return w;
}

END_POV_NAMESPACE
