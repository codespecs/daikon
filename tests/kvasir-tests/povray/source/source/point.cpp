/****************************************************************************
 *               point.cpp
 *
 * This module implements the point & spot light source primitive.
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
 * $File: //depot/povray/3.6-release/source/point.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "vector.h"
#include "point.h"
#include "matrices.h"
#include "objects.h"
#include "colour.h"
#include "povray.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/



/*****************************************************************************
* Local typedefs
******************************************************************************/



/*****************************************************************************
* Static functions
******************************************************************************/

static DBL cubic_spline ( DBL low,DBL high,DBL pos);
static int  All_Light_Source_Intersections (OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack);
static int  Inside_Light_Source (VECTOR point, OBJECT *Object);
static void Light_Source_Normal (VECTOR Result, OBJECT *Object, INTERSECTION *Inter);
static void Light_Source_UVCoord (UV_VECT Result, OBJECT *Object, INTERSECTION *Inter);
static void Translate_Light_Source (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Rotate_Light_Source (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Scale_Light_Source (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Transform_Light_Source (OBJECT *Object, TRANSFORM *Trans);
static void Invert_Light_Source (OBJECT *Object);
static LIGHT_SOURCE *Copy_Light_Source (OBJECT *Object);
static void Destroy_Light_Source (OBJECT *Object);

/*****************************************************************************
* Local variables
******************************************************************************/

METHODS Light_Source_Methods =
{
  All_Light_Source_Intersections,
  Inside_Light_Source, Light_Source_Normal, Light_Source_UVCoord,
  (COPY_METHOD)Copy_Light_Source,
  Translate_Light_Source, Rotate_Light_Source,
  Scale_Light_Source, Transform_Light_Source, Invert_Light_Source,
  Destroy_Light_Source
};





/*****************************************************************************
*
* FUNCTION
*
*   All_Light_Source_Intersections
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

static int All_Light_Source_Intersections (OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack)
{
  if (((LIGHT_SOURCE *)Object)->Children != NULL)
  {
    if (Ray_In_Bound (Ray, ((LIGHT_SOURCE *)Object)->Children->Bound))
    {
      if (All_Intersections (((LIGHT_SOURCE *)Object)->Children, Ray, Depth_Stack))
      {
        return(true);
      }
    }
  }

  return(false);
}



/*****************************************************************************
*
* FUNCTION
*
*   Inside_Light_Source
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

static int Inside_Light_Source (VECTOR IPoint, OBJECT *Object)
{
  if (((LIGHT_SOURCE *)Object)->Children != NULL)
  {
    if (Inside_Object (IPoint, ((LIGHT_SOURCE *)Object)->Children))
    {
      return (true);
    }
  }

  return (false);
}



/*****************************************************************************
*
* FUNCTION
*
*   Light_Source_Normal
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

static void Light_Source_Normal (VECTOR Result, OBJECT *Object, INTERSECTION *Inter)
{
  if (((LIGHT_SOURCE *)Object)->Children != NULL)
  {
    Normal (Result, ((LIGHT_SOURCE *)Object)->Children,Inter);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Translate_Light_Source
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

static void Translate_Light_Source (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans)
{
  LIGHT_SOURCE *Light = (LIGHT_SOURCE *)Object;

  VAddEq (Light->Center, Vector);
  VAddEq (Light->Points_At, Vector);

  if (Light->Children != NULL)
  {
    Translate_Object (Light->Children, Vector, Trans);
  }

  if (Light->Projected_Through_Object != NULL )
  {
    Translate_Object (Light->Projected_Through_Object, Vector, Trans );
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Rotate_Light_Source
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

static void Rotate_Light_Source (OBJECT *Object, VECTOR, TRANSFORM *Trans)
{
  Transform_Light_Source(Object, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Scale_Light_Source
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

static void Scale_Light_Source (OBJECT *Object, VECTOR, TRANSFORM *Trans)
{
  Transform_Light_Source(Object, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Transform_Light_Source
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

static void Transform_Light_Source (OBJECT *Object, TRANSFORM *Trans)
{
  DBL len;
  LIGHT_SOURCE *Light = (LIGHT_SOURCE *)Object;

  MTransPoint (Light->Center,    Light->Center,    Trans);
  MTransPoint (Light->Points_At, Light->Points_At, Trans);
  MTransPoint (Light->Axis1,     Light->Axis1,     Trans);
  MTransPoint (Light->Axis2,     Light->Axis2,     Trans);

  MTransDirection (Light->Direction, Light->Direction, Trans);

  /* Make sure direction has unit length. */

  VLength(len, Light->Direction);

  if (len > EPSILON)
  {
    VInverseScaleEq(Light->Direction, len);
  }

  if (Light->Children != NULL)
  {
    Transform_Object (Light->Children, Trans);
  }

  if (Light->Projected_Through_Object != NULL)
  {
    Transform_Object (Light->Projected_Through_Object, Trans);
  }
}
  
  


/*****************************************************************************
*
* FUNCTION
*
*   Invert_Light_Source
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

static void Invert_Light_Source (OBJECT *Object)
{
  LIGHT_SOURCE *Light = (LIGHT_SOURCE *)Object;

  if (Light->Children != NULL)
  {
    Invert_Object (Light->Children);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_Light_Source
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

LIGHT_SOURCE *Create_Light_Source ()
{
  int i;
  LIGHT_SOURCE *New;

  New = (LIGHT_SOURCE *)POV_MALLOC(sizeof (LIGHT_SOURCE), "light_source");

  INIT_OBJECT_FIELDS(New, LIGHT_OBJECT, &Light_Source_Methods)

  New->Children = NULL;

  Set_Flag(New, NO_SHADOW_FLAG);

  Make_Colour(New->Colour,    1.0, 1.0, 1.0);
  Make_Vector(New->Direction, 0.0, 0.0, 0.0);
  Make_Vector(New->Center,    0.0, 0.0, 0.0);
  Make_Vector(New->Points_At, 0.0, 0.0, 1.0);
  Make_Vector(New->Axis1,     0.0, 0.0, 1.0);
  Make_Vector(New->Axis2,     0.0, 1.0, 0.0);

  New->Coeff   = 0.0;
  New->Radius  = 0.0;
  New->Falloff = 0.0;

  New->Fade_Distance = 0.0;
  New->Fade_Power    = 0.0;

  New->Next_Light_Source    = NULL;
  New->Light_Grid           = NULL;
  New->Shadow_Cached_Object = NULL;
  New->Projected_Through_Object= NULL;
  New->blend_map            = NULL;

  New->Light_Type = POINT_SOURCE;

  New->Area_Light = false;
  New->Jitter     = false;
  New->Orient     = false;
  New->Circular   = false;
  New->Parallel   = false;
  New->Photon_Area_Light = false;

  New->Area_Size1 = 0;
  New->Area_Size2 = 0;

  New->Adaptive_Level = 100;

  New->Media_Attenuation = false;
  New->Media_Interaction = true;

  for (i = 0; i < 6; i++)
  {
    New->Light_Buffer[i] = NULL;
  }

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Light_Source
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

static LIGHT_SOURCE *Copy_Light_Source (OBJECT *Old)
{
  int i, j;
  LIGHT_SOURCE *New;
  LIGHT_SOURCE *Light = (LIGHT_SOURCE *)Old;

  New = Create_Light_Source();

  /* Copy light source. */

  *New = *(LIGHT_SOURCE *)Old;
  New->Next_Light_Source = NULL;

  New->Children = Copy_Object (((LIGHT_SOURCE *)Old)->Children);
  New->Projected_Through_Object = Copy_Object (((LIGHT_SOURCE *)Old)->Projected_Through_Object);

  if (Light->Light_Grid != NULL)
  {
    New->Light_Grid = Create_Light_Grid(Light->Area_Size1, Light->Area_Size2);

    for (i = 0; i < Light->Area_Size1; i++)
    {
      for (j = 0; j < Light->Area_Size2; j++)
      {
        Assign_Colour(New->Light_Grid[i][j], Light->Light_Grid[i][j]);
      }
    }
  }

  /* NK phmap */
  New->blend_map = Copy_Blend_Map(Light->blend_map);

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Light_Source
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

static void Destroy_Light_Source (OBJECT *Object)
{
  int i;
  LIGHT_SOURCE *Light = (LIGHT_SOURCE *)Object;

  if (Light->Light_Grid != NULL)
  {
    for (i = 0; i < Light->Area_Size1; i++)
    {
      POV_FREE(Light->Light_Grid[i]);
    }

    POV_FREE(Light->Light_Grid);
  }

  if ( Light->blend_map)
  {
    Destroy_Blend_Map(Light->blend_map);
    Light->blend_map=NULL;
  }

  Destroy_Object(Light->Children);
  Destroy_Object(Light->Projected_Through_Object);

  POV_FREE(Object);
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_Light_Grid
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

COLOUR **Create_Light_Grid (int Size1, int  Size2)
{
  int i;
  COLOUR **New;

  New = (COLOUR **)POV_MALLOC(Size1 * sizeof (COLOUR *), "area light");

  for (i = 0; i < Size1; i++)
  {
    New[i] = (COLOUR *)POV_MALLOC(Size2 * sizeof (COLOUR), "area light");
  }

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   cubic_spline
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
*   Cubic spline that has tangents of slope 0 at x == low and at x == high.
*   For a given value "pos" between low and high the spline value is returned.
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL cubic_spline(DBL low, DBL  high, DBL  pos)
{
  /* Check to see if the position is within the proper boundaries. */

  if (pos < low)
  {
    return(0.0);
  }
  else
  {
    if (pos >= high)
    {
      return(1.0);
    }
  }

  /* This never happens. [DB] */

/*
  if (high == low)
  {
    return(0.0);
  }
*/

  /* Normalize to the interval [0...1]. */

  pos = (pos - low) / (high - low);

  /* See where it is on the cubic curve. */

  return(3 - 2 * pos) * pos * pos;
}



/*****************************************************************************
*
* FUNCTION
*
*   Attenuate_Light
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
*   Jan 1995 : Added attenuation due to atmospheric scattering and light
*              source distance. Added cylindrical light source. [DB]
*
******************************************************************************/

DBL Attenuate_Light (LIGHT_SOURCE *Light, RAY *Ray, DBL Distance)
{
  DBL len, k, costheta;
  DBL Attenuation = 1.0;
  VECTOR P, V1;

  /* If this is a spotlight then attenuate based on the incidence angle. */

  switch (Light->Light_Type)
  {
    case SPOT_SOURCE:

      VDot(costheta, Ray->Direction, Light->Direction);

      if(Distance>0.0) costheta = -costheta;

      if (costheta > 0.0)
      {
        Attenuation = pow(costheta, Light->Coeff);

        if (Light->Radius > 0.0 && costheta < Light->Radius)
        {
          Attenuation *= cubic_spline(Light->Falloff, Light->Radius, costheta);
        }
      }
      else
      {
        return 0.0; //Attenuation = 0.0;
      }

      break;

    case CYLINDER_SOURCE:

      // Project light->point onto light direction
      // to make sure that we're on the correct side of the light
      VSub(V1, Ray->Initial, Light->Center);
      VDot(k, V1, Light->Direction);

      if (k > 0.0)
      {
        // Now subtract that from the light-direction.  This will
        // give us a vector showing us the distance from the
        // point to the center of the cylinder.
        VLinComb2(P, 1.0, V1, -k, Light->Direction);
        VLength(len, P);

        if (len < Light->Falloff)
        {
          DBL dist = 1.0 - len / Light->Falloff;

          Attenuation = pow(dist, Light->Coeff);

          if (Light->Radius > 0.0 && len > Light->Radius)
          {
            Attenuation *= cubic_spline(0.0, 1.0 - Light->Radius / Light->Falloff, dist);
          }
        }
        else
        {
          return 0.0; //Attenuation = 0.0;
        }
      }
      else
      {
        return 0.0; //Attenuation = 0.0;
      }

      break;
  }

  if (Attenuation > 0.0)
  {
    /* Attenuate light due to light source distance. */

    if ((Light->Fade_Power > 0.0) && (fabs(Light->Fade_Distance) > EPSILON))
    {
      Attenuation *= 2.0 / (1.0 + pow(Distance / Light->Fade_Distance, Light->Fade_Power));
    }
  }

  return(Attenuation);
}

/*****************************************************************************
*
* FUNCTION
*
*   Light_Source_UVCoord
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Nathan Kopp -- adapted from Light_Source_Normal by the POV-Ray Team
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

static void Light_Source_UVCoord (UV_VECT Result, OBJECT *Object, INTERSECTION *Inter)
{
  if (((LIGHT_SOURCE *)Object)->Children != NULL)
  {
    UVCoord (Result, ((LIGHT_SOURCE *)Object)->Children,Inter);
  }
}

END_POV_NAMESPACE
