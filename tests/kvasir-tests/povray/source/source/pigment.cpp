/****************************************************************************
 *               pigment.cpp
 *
 * This module implements solid texturing functions that modify the color
 * transparency of an object's surface.
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
 * $File: //depot/povray/3.6-release/source/pigment.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

/*
   Some texture ideas garnered from SIGGRAPH '85 Volume 19 Number 3, 
   "An Image Synthesizer" By Ken Perlin.
   Further Ideas Garnered from "The RenderMan Companion" (Addison Wesley).
*/

#include "frame.h"
#include "userio.h"
#include "vector.h"
#include "texture.h"
#include "colour.h"   
#include "image.h"    
#include "matrices.h" 
#include "pigment.h"  
#include "txttest.h"
#include "parse.h" // for Warning()

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

static BLEND_MAP_ENTRY Black_White_Entries[2] /* = // GLOBAL VARIABLE
  {{0.0, false, {{0.0, 0.0, 0.0, 0.0, 0.0}}},
  {1.0, false, {{1.0, 1.0, 1.0, 0.0, 0.0}}}} */ ;

static BLEND_MAP Gray_Default_Map = // GLOBAL VARIABLE
  { -1,  2,  false, COLOUR_TYPE,  Black_White_Entries};

static BLEND_MAP_ENTRY Bozo_Entries[6] /* = // GLOBAL VARIABLE
  {{0.4, false, {{1.0, 1.0, 1.0, 0.0, 0.0}}},
   {0.4, false, {{0.0, 1.0, 0.0, 0.0, 0.0}}},
   {0.6, false, {{0.0, 1.0, 0.0, 0.0, 0.0}}},
   {0.6, false, {{0.0, 0.0, 1.0, 0.0, 0.0}}},
   {0.8, false, {{0.0, 0.0, 1.0, 0.0, 0.0}}},
   {0.8, false, {{1.0, 0.0, 0.0, 0.0, 0.0}}}} */ ;

static BLEND_MAP Bozo_Default_Map = // GLOBAL VARIABLE
  { -1,  6,  false, COLOUR_TYPE,  Bozo_Entries};

static BLEND_MAP_ENTRY Wood_Entries[2] /* = // GLOBAL VARIABLE
  {{0.6, false, {{0.666, 0.312,  0.2,   0.0, 0.0}}},
   {0.6, false, {{0.4,   0.1333, 0.066, 0.0, 0.0}}}} */ ;
    
static BLEND_MAP Wood_Default_Map = // GLOBAL VARIABLE
  { -1,  2,  false, COLOUR_TYPE,  Wood_Entries};

static BLEND_MAP_ENTRY Mandel_Entries[5] /* = // GLOBAL VARIABLE
  {{0.001, false, {{0.0, 0.0, 0.0, 0.0, 0.0}}},
   {0.001, false, {{0.0, 1.0, 1.0, 0.0, 0.0}}},
   {0.012, false, {{1.0, 1.0, 0.0, 0.0, 0.0}}},
   {0.015, false, {{1.0, 0.0, 1.0, 0.0, 0.0}}},
   {0.1,   false, {{0.0, 1.0, 1.0, 0.0, 0.0}}}} */ ;

static BLEND_MAP Mandel_Default_Map = // GLOBAL VARIABLE
  { -1,  5,  false, COLOUR_TYPE,  Mandel_Entries};

static BLEND_MAP_ENTRY Agate_Entries[6] /* = // GLOBAL VARIABLE
  {{0.0, false, {{1.0,  1.0,  1.0,  0.0, 0.0}}},
   {0.5, false, {{0.95, 0.75, 0.5,  0.0, 0.0}}},
   {0.5, false, {{0.9,  0.7,  0.5,  0.0, 0.0}}},
   {0.6, false, {{0.9,  0.7,  0.4,  0.0, 0.0}}},
   {0.6, false, {{1.0,  0.7,  0.4,  0.0, 0.0}}},
   {1.0, false, {{0.6,  0.3,  0.0,  0.0, 0.0}}}} */ ;

static BLEND_MAP Agate_Default_Map = // GLOBAL VARIABLE
  { -1,  6,  false, COLOUR_TYPE,  Agate_Entries};

static BLEND_MAP_ENTRY Radial_Entries[4] /* = // GLOBAL VARIABLE
  {{0.0,   false, {{0.0, 1.0, 1.0, 0.0, 0.0}}},
   {0.333, false, {{1.0, 1.0, 0.0, 0.0, 0.0}}},
   {0.666, false, {{1.0, 0.0, 1.0, 0.0, 0.0}}},
   {1.0,   false, {{0.0, 1.0, 1.0, 0.0, 0.0}}}} */ ;

static BLEND_MAP Radial_Default_Map = // GLOBAL VARIABLE
  { -1,  4,  false, COLOUR_TYPE,  Radial_Entries};

static BLEND_MAP_ENTRY Marble_Entries[3] /* = // GLOBAL VARIABLE
  {{0.0, false, {{0.9, 0.8,  0.8,  0.0, 0.0}}},
   {0.9, false, {{0.9, 0.08, 0.08, 0.0, 0.0}}},
   {0.9, false, {{0.0, 0.0, 0.0, 0.0, 0.0}}}} */ ;

static BLEND_MAP Marble_Default_Map = // GLOBAL VARIABLE
  { -1,  3,  false, COLOUR_TYPE,  Marble_Entries};

static BLEND_MAP_ENTRY Brick_Entries[2] /* = // GLOBAL VARIABLE
  {{0.0, false, {{0.5, 0.5,  0.5,  0.0, 0.0}}},
   {1.0, false, {{0.6, 0.15, 0.15, 0.0, 0.0}}}} */ ;

BLEND_MAP Brick_Default_Map = // GLOBAL VARIABLE
  { -1,  2,  false, COLOUR_TYPE,  Brick_Entries};

static BLEND_MAP_ENTRY Hex_Entries[3] /*= // GLOBAL VARIABLE
  {{0.0, false, {{0.0, 0.0, 1.0, 0.0, 0.0}}},
   {1.0, false, {{0.0, 1.0, 0.0, 0.0, 0.0}}},
   {2.0, false, {{1.0, 0.0, 0.0, 0.0, 0.0}}}} */;

BLEND_MAP Hex_Default_Map = // GLOBAL VARIABLE
  { -1, 3, false,COLOUR_TYPE, Hex_Entries};

BLEND_MAP Check_Default_Map = // GLOBAL VARIABLE
  { -1, 2, false,COLOUR_TYPE, Hex_Entries}; /* Yes... Hex_Entries, not Check [CY] */



/*****************************************************************************
* Static functions
******************************************************************************/
static void Do_Average_Pigments (COLOUR Colour, PIGMENT *Pigment, VECTOR EPoint,INTERSECTION *Intersect);



/*****************************************************************************
*
* FUNCTION
*
*   Create_Pigment
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
*   pointer to the created pigment
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION   : Allocate memory for new pigment and initialize it to
*                 system default values.
*
* CHANGES
*
******************************************************************************/

PIGMENT *Create_Pigment ()
{
  PIGMENT *New;

  New = (PIGMENT *)POV_MALLOC(sizeof (PIGMENT), "pigment");

  Init_TPat_Fields((TPATTERN *)New);

  Make_Colour(New->Colour, 0.0,0.0,0.0) ;

  New->Blend_Map = NULL;

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Pigment
*
* INPUT
*
*   Old -- point to pigment to be copied
*   
* RETURNS
*
*   pointer to the created pigment
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION   : Allocate memory for new pigment and initialize it to
*                 values in existing pigment Old.
*
* CHANGES
*
******************************************************************************/

PIGMENT *Copy_Pigment (PIGMENT *Old)
{
  PIGMENT *New;

  if (Old != NULL)
  {
    New = Create_Pigment ();

    Copy_TPat_Fields ((TPATTERN *)New, (TPATTERN *)Old);

    if (Old->Type == PLAIN_PATTERN)
    {
      Assign_Colour(New->Colour,Old->Colour);
    }
    New->Next = (TPATTERN *)Copy_Pigment((PIGMENT *)Old->Next);
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
*   Destroy_Pigment
*
* INPUT
*
*   pointer to pigment to destroied
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION   : free all memory associated with given pigment
*
* CHANGES
*
******************************************************************************/

void Destroy_Pigment (PIGMENT *Pigment)
{
  if (Pigment != NULL)
  {
    Destroy_Pigment((PIGMENT *)Pigment->Next);

    Destroy_TPat_Fields ((TPATTERN *)Pigment);

    POV_FREE(Pigment);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Post_Pigment
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
* CHANGES
*
******************************************************************************/

int Post_Pigment(PIGMENT *Pigment)
{
  int i, Has_Filter;
  BLEND_MAP *Map;

  if (Pigment == NULL)
  {
    Error("Missing pigment");
  }

  if (Pigment->Flags & POST_DONE)
  {
    return(Pigment->Flags & HAS_FILTER);
  }

  if (Pigment->Type == NO_PATTERN)
  {
    Pigment->Type = PLAIN_PATTERN;

    Make_Colour(Pigment->Colour, 0.0, 0.0, 0.0) ;

    Warning(150, "No pigment type given.");
  }

  Pigment->Flags |= POST_DONE;

  switch (Pigment->Type)
  {
    case PLAIN_PATTERN:

      Destroy_Warps (Pigment->Warps);

      Pigment->Warps = NULL;

      break;

    case NO_PATTERN:
    case BITMAP_PATTERN:

      break;

    default:

      if (Pigment->Blend_Map == NULL)
      {
        switch (Pigment->Type)
        {
          case BOZO_PATTERN:    Pigment->Blend_Map = &Bozo_Default_Map;  break;
          case BRICK_PATTERN:   Pigment->Blend_Map = &Brick_Default_Map; break;
          case WOOD_PATTERN:    Pigment->Blend_Map = &Wood_Default_Map;  break;
          case MANDEL_PATTERN:  Pigment->Blend_Map = &Mandel_Default_Map;break;
          case RADIAL_PATTERN:  Pigment->Blend_Map = &Radial_Default_Map;break;
          case AGATE_PATTERN:   Pigment->Blend_Map = &Agate_Default_Map; break;
          case MARBLE_PATTERN:  Pigment->Blend_Map = &Marble_Default_Map;break;
          case HEXAGON_PATTERN: Pigment->Blend_Map = &Hex_Default_Map;   break;
          case CHECKER_PATTERN: Pigment->Blend_Map = &Check_Default_Map; break;
          case AVERAGE_PATTERN: Error("Missing pigment_map in average pigment"); break;
          case OBJECT_PATTERN:  Pigment->Blend_Map = &Check_Default_Map; break;
          default:              Pigment->Blend_Map = &Gray_Default_Map;  break;
        }
      }

      break;
  }

  /* Now we test wether this pigment is opaque or not. [DB 8/94] */

  Has_Filter = false;

  if ((fabs(Pigment->Colour[pFILTER]) > EPSILON) ||
      (fabs(Pigment->Colour[pTRANSM]) > EPSILON))
  {
    Has_Filter = true;
  }

  if ((Map = Pigment->Blend_Map) != NULL)
  {
    if ((Map->Type == PIGMENT_TYPE) || (Map->Type == DENSITY_TYPE))
    {
       for (i = 0; i < Map->Number_Of_Entries; i++)
       {
         Has_Filter |= Post_Pigment(Map->Blend_Map_Entries[i].Vals.Pigment);
       }
    }
    else
    {
       for (i = 0; i < Map->Number_Of_Entries; i++)
       {
         Has_Filter |= fabs(Map->Blend_Map_Entries[i].Vals.Colour[pFILTER])>EPSILON;
         Has_Filter |= fabs(Map->Blend_Map_Entries[i].Vals.Colour[pTRANSM])>EPSILON;
       }
    }
  }

  if (Has_Filter)
  {
    Pigment->Flags |= HAS_FILTER;
  }
  
  if (Pigment->Next != NULL)
  {
    Post_Pigment((PIGMENT *)Pigment->Next);
  }

  return(Has_Filter);
}



/*****************************************************************************
*
* FUNCTION
*
*   Compute_Pigment
*
* INPUT
*
*   Pigment - Info about this pigment
*   EPoint  - 3-D point at which pattern is evaluated
*   Intersection - structure holding info about object at intersection point
*
* OUTPUT
*
*   Colour  - Resulting color is returned here.
*
* RETURNS
*
*   int - true,  if a color was found for the given point
*         false, if no color was found (e.g. areas outside an image map
*                that has the once option)
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*   Given a 3d point and a pigment, compute colour from that layer.
*   (Formerly called "Colour_At", or "Add_Pigment")
*
* CHANGES
*   Added pigment map support [CY 11/94]
*   Added Intersection parameter for UV support NK 1998
*
******************************************************************************/

int Compute_Pigment (COLOUR Colour, PIGMENT *Pigment, VECTOR EPoint, INTERSECTION *Intersect)
{
  int Colour_Found;
  VECTOR TPoint;
  DBL value;
  register DBL fraction;
  BLEND_MAP_ENTRY *Cur, *Prev;
  COLOUR Temp_Colour;
  BLEND_MAP *Blend_Map = Pigment->Blend_Map;
  UV_VECT UV_Coords;

  if (Pigment->Type <= LAST_SPECIAL_PATTERN)
  {
    Colour_Found = true;

    switch (Pigment->Type)
    {
      case NO_PATTERN:

        Make_Colour(Colour, 0.0, 0.0, 0.0);

        break;

      case PLAIN_PATTERN:

        Assign_Colour(Colour,Pigment->Colour);

        break;

      case AVERAGE_PATTERN:

        Warp_EPoint (TPoint, EPoint, (TPATTERN *)Pigment);

        Do_Average_Pigments(Colour,Pigment,TPoint,Intersect);

        break;

      case UV_MAP_PATTERN:
        if(Intersect == NULL)
           Error("The 'uv_mapping' pattern cannot be used as part of a pigment function!");

        Cur = &(Pigment->Blend_Map->Blend_Map_Entries[0]);

        if (Blend_Map->Type == COLOUR_TYPE)
        {
          Colour_Found = true;

          Assign_Colour(Colour, Cur->Vals.Colour);
        }
        else
        {
          /* Don't bother warping, simply get the UV vect of the intersection */
          UVCoord(UV_Coords, Intersect->Object, Intersect);
          TPoint[X] = UV_Coords[U];
          TPoint[Y] = UV_Coords[V];
          TPoint[Z] = 0;

          if (Compute_Pigment(Colour, Cur->Vals.Pigment,TPoint,Intersect))
          {
            Colour_Found = true;
          }
        }

        break;

      case BITMAP_PATTERN:

        Warp_EPoint (TPoint, EPoint, (TPATTERN *)Pigment);

        Make_Colour(Colour, 0.0, 0.0, 0.0);

        Colour_Found = image_map (TPoint, Pigment, Colour);

        break;

      default:

        Error("Pigment type %d not yet implemented",Pigment->Type);
    }

    return(Colour_Found);
  }

  Colour_Found = false;

  /* NK 19 Nov 1999 added Warp_EPoint */
	Warp_EPoint (TPoint, EPoint, (TPATTERN *)Pigment);
	value = Evaluate_TPat ((TPATTERN *)Pigment,TPoint,Intersect);

  Search_Blend_Map (value, Blend_Map, &Prev, &Cur);

  if (Blend_Map->Type == COLOUR_TYPE)
  {
    Colour_Found = true;

    Assign_Colour(Colour, Cur->Vals.Colour);
  }
  else
  {
    Warp_EPoint (TPoint, EPoint, (TPATTERN *)Pigment);

    if (Compute_Pigment(Colour, Cur->Vals.Pigment,TPoint,Intersect))
    {
      Colour_Found = true;
    }
  }

  if (Prev != Cur)
  {
    if (Blend_Map->Type == COLOUR_TYPE)
    {
      Colour_Found = true;

      Assign_Colour(Temp_Colour, Prev->Vals.Colour);
    }
    else
    {
      if (Compute_Pigment(Temp_Colour, Prev->Vals.Pigment, TPoint,Intersect))
      {
        Colour_Found = true;
      }
    }

    fraction = (value - Prev->value) / (Cur->value - Prev->value);

    Colour[pRED]    = Temp_Colour[pRED]    + fraction * (Colour[pRED]    - Temp_Colour[pRED]);
    Colour[pGREEN]  = Temp_Colour[pGREEN]  + fraction * (Colour[pGREEN]  - Temp_Colour[pGREEN]);
    Colour[pBLUE]   = Temp_Colour[pBLUE]   + fraction * (Colour[pBLUE]   - Temp_Colour[pBLUE]);
    Colour[pFILTER] = Temp_Colour[pFILTER] + fraction * (Colour[pFILTER] - Temp_Colour[pFILTER]);
    Colour[pTRANSM] = Temp_Colour[pTRANSM] + fraction * (Colour[pTRANSM] - Temp_Colour[pTRANSM]);
  }

  return(Colour_Found);
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
* DESCRIPTION
*
* CHANGES
*   Added Intersection parameter for UV support NK 1998
*
******************************************************************************/

static void Do_Average_Pigments (COLOUR Colour, PIGMENT *Pigment, VECTOR EPoint, INTERSECTION *Intersect)
{
   int i;
   COLOUR LC;
   BLEND_MAP *Map = Pigment->Blend_Map;
   SNGL Value;
   SNGL Total = 0.0;
   
   Make_Colour (Colour, 0.0, 0.0, 0.0);

   for (i = 0; i < Map->Number_Of_Entries; i++)
   {
     Value = Map->Blend_Map_Entries[i].value;

     Compute_Pigment (LC,Map->Blend_Map_Entries[i].Vals.Pigment,EPoint,Intersect);
     
     Colour[pRED]   += LC[pRED]   *Value;
     Colour[pGREEN] += LC[pGREEN] *Value;
     Colour[pBLUE]  += LC[pBLUE]  *Value;
     Colour[pFILTER]+= LC[pFILTER]*Value;
     Colour[pTRANSM]+= LC[pTRANSM]*Value;

     Total += Value;
   }
   Colour[pRED]   /= Total;
   Colour[pGREEN] /= Total;
   Colour[pBLUE]  /= Total;
   Colour[pFILTER]/= Total;
   Colour[pTRANSM]/= Total;
}



/*****************************************************************************
*
* FUNCTION  Make_Pigment_Entries
*
* INPUT  None
*   
* OUTPUT  Initializes default pigment blend_map values.
*   
* RETURNS  None
*   
* AUTHOR  Steve Demlow, Dec. '95
*   
* DESCRIPTION  Some pre-ANSI compilers won't auto-initialize unions, so these
*   have to be done in regular code.
*
* CHANGES
*
******************************************************************************/

void Make_Pigment_Entries()
{
  static unsigned char Made = false;

  if (Made) {
    return;
  }
  Made = true;

  Make_Blend_Map_Entry(Black_White_Entries[0] , 0.0, false, 0.0, 0.0, 0.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Black_White_Entries[1] , 1.0, false, 1.0, 1.0, 1.0, 0.0, 0.0);
  
  Make_Blend_Map_Entry(Bozo_Entries[0], 0.4, false, 1.0, 1.0, 1.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Bozo_Entries[1], 0.4, false, 0.0, 1.0, 0.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Bozo_Entries[2], 0.6, false, 0.0, 1.0, 0.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Bozo_Entries[3], 0.6, false, 0.0, 0.0, 1.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Bozo_Entries[4], 0.8, false, 0.0, 0.0, 1.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Bozo_Entries[5], 0.8, false, 1.0, 0.0, 0.0, 0.0, 0.0);
  
  Make_Blend_Map_Entry(Wood_Entries[0], 0.6, false, 0.666, 0.312,  0.2,   0.0, 0.0);
  Make_Blend_Map_Entry(Wood_Entries[1], 0.6, false, 0.4,   0.1333, 0.066, 0.0, 0.0);
  
  Make_Blend_Map_Entry(Mandel_Entries[0], 0.001, false, 0.0, 0.0, 0.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Mandel_Entries[1], 0.001, false, 0.0, 1.0, 1.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Mandel_Entries[2], 0.012, false, 1.0, 1.0, 0.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Mandel_Entries[3], 0.015, false, 1.0, 0.0, 1.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Mandel_Entries[4], 0.1,   false, 0.0, 1.0, 1.0, 0.0, 0.0);
  
  Make_Blend_Map_Entry(Agate_Entries[0], 0.0, false, 1.0,  1.0,  1.0,  0.0, 0.0);
  Make_Blend_Map_Entry(Agate_Entries[1], 0.5, false, 0.95, 0.75, 0.5,  0.0, 0.0);
  Make_Blend_Map_Entry(Agate_Entries[2], 0.5, false, 0.9,  0.7,  0.5,  0.0, 0.0);
  Make_Blend_Map_Entry(Agate_Entries[3], 0.6, false, 0.9,  0.7,  0.4,  0.0, 0.0);
  Make_Blend_Map_Entry(Agate_Entries[4], 0.6, false, 1.0,  0.7,  0.4,  0.0, 0.0);
  Make_Blend_Map_Entry(Agate_Entries[5], 1.0, false, 0.6,  0.3,  0.0,  0.0, 0.0);
  
  Make_Blend_Map_Entry(Radial_Entries[0], 0.0,   false, 0.0, 1.0, 1.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Radial_Entries[1], 0.333, false, 1.0, 1.0, 0.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Radial_Entries[2], 0.666, false, 1.0, 0.0, 1.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Radial_Entries[3], 1.0,   false, 0.0, 1.0, 1.0, 0.0, 0.0);
  
  Make_Blend_Map_Entry(Marble_Entries[0], 0.0, false, 0.9, 0.8, 0.8, 0.0, 0.0);
  Make_Blend_Map_Entry(Marble_Entries[1], 0.9, false, 0.9, 0.08, 0.08, 0.0, 0.0);
  Make_Blend_Map_Entry(Marble_Entries[2], 0.9, false, 0.0, 0.0, 0.0, 0.0, 0.0);
  
  Make_Blend_Map_Entry(Brick_Entries[0], 0.0, false, 0.5, 0.5,  0.5,  0.0, 0.0);
  Make_Blend_Map_Entry(Brick_Entries[1], 1.0, false, 0.6, 0.15, 0.15, 0.0, 0.0);
  
  Make_Blend_Map_Entry(Hex_Entries[0], 0.0, false, 0.0, 0.0, 1.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Hex_Entries[1], 1.0, false, 0.0, 1.0, 0.0, 0.0, 0.0);
  Make_Blend_Map_Entry(Hex_Entries[2], 2.0, false, 1.0, 0.0, 0.0, 0.0, 0.0);
}
END_POV_NAMESPACE

