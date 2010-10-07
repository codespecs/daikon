/****************************************************************************
 *               spheres.cpp
 *
 * This module implements the sphere primitive.
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
 * $File: //depot/povray/3.6-release/source/spheres.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "povray.h"
#include "vector.h"
#include "bbox.h"
#include "matrices.h"
#include "objects.h"
#include "spheres.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

const DBL DEPTH_TOLERANCE = 1.0e-6;



/*****************************************************************************
* Static functions
******************************************************************************/

static int All_Sphere_Intersections (OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack);
static int All_Ellipsoid_Intersections (OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack);
static int Inside_Sphere (VECTOR IPoint, OBJECT *Object);
static int Inside_Ellipsoid (VECTOR IPoint, OBJECT *Object);
static void Sphere_Normal (VECTOR Result, OBJECT *Object, INTERSECTION *Inter);
static void Sphere_UVCoord (UV_VECT Result, OBJECT *Object, INTERSECTION *Inter);
static void Ellipsoid_Normal (VECTOR Result, OBJECT *Object, INTERSECTION *Inter);
static void Translate_Sphere (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Rotate_Sphere (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Scale_Sphere (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Invert_Sphere (OBJECT *Object);



/*****************************************************************************
* Local variables
******************************************************************************/

METHODS Sphere_Methods =
{
  All_Sphere_Intersections,
  Inside_Sphere, Sphere_Normal, Sphere_UVCoord,
  (COPY_METHOD)Copy_Sphere,
  Translate_Sphere, Rotate_Sphere,
  Scale_Sphere, Transform_Sphere, Invert_Sphere,
  Destroy_Sphere
};



METHODS Ellipsoid_Methods =
{
  All_Ellipsoid_Intersections,
  Inside_Ellipsoid, Ellipsoid_Normal, Sphere_UVCoord,
  (COPY_METHOD)Copy_Sphere,
  Translate_Sphere, Rotate_Sphere,
  Scale_Sphere, Transform_Sphere, Invert_Sphere,
  Destroy_Sphere
};



/*****************************************************************************
*
* FUNCTION
*
*   All_Sphere_Intersection
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

static int All_Sphere_Intersections(OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack)
{
  register int Intersection_Found;
  DBL Depth1, Depth2;
  VECTOR IPoint;
  SPHERE *Sphere = (SPHERE *)Object;

  Intersection_Found = false;

  if (Intersect_Sphere(Ray, Sphere->Center, Sqr(Sphere->Radius), &Depth1, &Depth2))
  {
    if ((Depth1 > DEPTH_TOLERANCE) && (Depth1 < Max_Distance))
    {
      VEvaluateRay(IPoint, Ray->Initial, Depth1, Ray->Direction);

      if (Point_In_Clip(IPoint, Object->Clip))
      {
        push_entry(Depth1, IPoint, Object, Depth_Stack);

        Intersection_Found = true;
      }
    }

    if ((Depth2 > DEPTH_TOLERANCE) && (Depth2 < Max_Distance))
    {
      VEvaluateRay(IPoint, Ray->Initial, Depth2, Ray->Direction);

      if (Point_In_Clip(IPoint, Object->Clip))
      {
        push_entry(Depth2, IPoint, Object, Depth_Stack);

        Intersection_Found = true;
      }
    }
  }

  return(Intersection_Found);
}



/*****************************************************************************
*
* FUNCTION
*
*   All_Ellipsoid_Intersection
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

static int All_Ellipsoid_Intersections(OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack)
{
  register int Intersection_Found;
  DBL Depth1, Depth2, len;
  VECTOR IPoint;
  RAY New_Ray;
  SPHERE *Sphere = (SPHERE *)Object;

  /* Transform the ray into the ellipsoid's space */

  MInvTransPoint(New_Ray.Initial, Ray->Initial, ((SPHERE *)Object)->Trans);
  MInvTransDirection(New_Ray.Direction, Ray->Direction, ((SPHERE *)Object)->Trans);

  VLength(len, New_Ray.Direction);
  VInverseScaleEq(New_Ray.Direction, len);

  Intersection_Found = false;

  if (Intersect_Sphere(&New_Ray, Sphere->Center, Sqr(Sphere->Radius), &Depth1, &Depth2))
  {
    if ((Depth1 > DEPTH_TOLERANCE) && (Depth1 < Max_Distance))
    {
      VEvaluateRay(IPoint, New_Ray.Initial, Depth1, New_Ray.Direction);

      MTransPoint(IPoint, IPoint, ((SPHERE *)Object)->Trans);

      if (Point_In_Clip(IPoint, Object->Clip))
      {
        push_entry(Depth1 / len, IPoint, Object, Depth_Stack);

        Intersection_Found = true;
      }
    }

    if ((Depth2 > DEPTH_TOLERANCE) && (Depth2 < Max_Distance))
    {
      VEvaluateRay(IPoint, New_Ray.Initial, Depth2, New_Ray.Direction);

      MTransPoint(IPoint, IPoint, ((SPHERE *)Object)->Trans);

      if (Point_In_Clip(IPoint, Object->Clip))
      {
        push_entry(Depth2 / len, IPoint, Object, Depth_Stack);

        Intersection_Found = true;
      }
    }
  }

  return(Intersection_Found);
}



/*****************************************************************************
*
* FUNCTION
*
*   Intersect_Sphere
*
* INPUT
*
*   Ray     - Ray to test intersection with
*   Center  - Center of the sphere
*   Radius2 - Squared radius of the sphere
*   Depth1  - Lower intersection distance
*   Depth2  - Upper intersection distance
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

int Intersect_Sphere(RAY *Ray, VECTOR Center, DBL Radius2, DBL *Depth1, DBL  *Depth2)
{
  DBL OCSquared, t_Closest_Approach, Half_Chord, t_Half_Chord_Squared;
  VECTOR Origin_To_Center;

  Increase_Counter(stats[Ray_Sphere_Tests]);

  VSub(Origin_To_Center, Center, Ray->Initial);

  VDot(OCSquared, Origin_To_Center, Origin_To_Center);

  VDot(t_Closest_Approach, Origin_To_Center, Ray->Direction);

  if ((OCSquared >= Radius2) && (t_Closest_Approach < EPSILON))
  {
    return(false);
  }

  t_Half_Chord_Squared = Radius2 - OCSquared + Sqr(t_Closest_Approach);

  if (t_Half_Chord_Squared > EPSILON)
  {
    Half_Chord = sqrt(t_Half_Chord_Squared);

    *Depth1 = t_Closest_Approach - Half_Chord;
    *Depth2 = t_Closest_Approach + Half_Chord;

    Increase_Counter(stats[Ray_Sphere_Tests_Succeeded]);

    return(true);
  }

  return(false);
}



/*****************************************************************************
*
* FUNCTION
*
*   Inside_Sphere
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

static int Inside_Sphere(VECTOR IPoint, OBJECT *Object)
{
  DBL OCSquared;
  VECTOR Origin_To_Center;

  VSub(Origin_To_Center, ((SPHERE *)Object)->Center, IPoint);

  VDot(OCSquared, Origin_To_Center, Origin_To_Center);

  if (Test_Flag(Object, INVERTED_FLAG))
  {
    return(OCSquared > Sqr(((SPHERE *)Object)->Radius));
  }
  else
  {
    return(OCSquared < Sqr(((SPHERE *)Object)->Radius));
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Inside_Ellipsoid
*
* INPUT
*
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

static int Inside_Ellipsoid(VECTOR IPoint, OBJECT *Object)
{
  DBL OCSquared;
  VECTOR Origin_To_Center, New_Point;

  /* Transform the point into the sphere's space */

  MInvTransPoint(New_Point, IPoint, ((SPHERE *)Object)->Trans);

  VSub(Origin_To_Center, ((SPHERE *)Object)->Center, New_Point);

  VDot(OCSquared, Origin_To_Center, Origin_To_Center);

  if (Test_Flag(Object, INVERTED_FLAG))
  {
    return(OCSquared > Sqr(((SPHERE *)Object)->Radius));
  }
  else
  {
    return(OCSquared < Sqr(((SPHERE *)Object)->Radius));
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Sphere_Normal
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

static void Sphere_Normal(VECTOR Result, OBJECT *Object, INTERSECTION *Inter)
{
  VSub(Result, Inter->IPoint, ((SPHERE *)Object)->Center);

  VInverseScaleEq(Result, ((SPHERE *)Object)->Radius);
}



/*****************************************************************************
*
* FUNCTION
*
*   Ellipsoid_Normal
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

static void Ellipsoid_Normal(VECTOR Result, OBJECT *Object, INTERSECTION *Inter)
{
  VECTOR New_Point;

  /* Transform the point into the sphere's space */

  MInvTransPoint(New_Point, Inter->IPoint, ((SPHERE *)Object)->Trans);

  VSub(Result, New_Point, ((SPHERE *)Object)->Center);

  MTransNormal(Result, Result, ((SPHERE *)Object)->Trans);

  VNormalize(Result, Result);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Shere
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

SPHERE *Copy_Sphere(OBJECT *Object)
{
  SPHERE *New;

  New = Create_Sphere();

  *New = *((SPHERE *)Object);

  New->Trans = Copy_Transform(((SPHERE *)Object)->Trans);

  return(New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Translate_Sphere
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   ?
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

static void Translate_Sphere(OBJECT *Object, VECTOR Vector, TRANSFORM *Trans)
{
  SPHERE *Sphere = (SPHERE *) Object;

  if (Sphere->Trans == NULL)
  {
    VAddEq(Sphere->Center, Vector);

    Compute_Sphere_BBox(Sphere);
  }
  else
  {
    Transform_Sphere(Object, Trans);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Rotate_Sphere
*
* INPUT
*
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

static void Rotate_Sphere(OBJECT *Object, VECTOR, TRANSFORM *Trans)
{
  SPHERE *Sphere = (SPHERE *) Object;

  if (Sphere->Trans == NULL)
  {
    MTransPoint(Sphere->Center, Sphere->Center, Trans);

    Compute_Sphere_BBox(Sphere);
  }
  else
  {
    Transform_Sphere(Object, Trans);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Scale_Sphere
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

static void Scale_Sphere(OBJECT *Object, VECTOR Vector, TRANSFORM *Trans)
{
  SPHERE *Sphere = (SPHERE *) Object;

  if ((Vector[X] != Vector[Y]) || (Vector[X] != Vector[Z]))
  {
    if (Sphere->Trans == NULL)
    {
      Sphere->Methods = &Ellipsoid_Methods;

      Sphere->Trans = Create_Transform();
    }
  }

  if (Sphere->Trans == NULL)
  {
    VScaleEq(Sphere->Center, Vector[X]);

    Sphere->Radius *= fabs(Vector[X]);

    Compute_Sphere_BBox(Sphere);
  }
  else
  {
    Transform_Sphere(Object, Trans);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Invert_Sphere
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

static void Invert_Sphere(OBJECT *Object)
{
  Invert_Flag(Object, INVERTED_FLAG);
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_Sphere
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

SPHERE *Create_Sphere()
{
  SPHERE *New;

  New = (SPHERE *)POV_MALLOC(sizeof(SPHERE), "sphere");

  INIT_OBJECT_FIELDS(New, SPHERE_OBJECT, &Sphere_Methods)

  Make_Vector(New->Center, 0.0, 0.0, 0.0);

  New->Radius = 1.0;

  New->Trans = NULL;

  return(New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Transform_Sphere
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

void Transform_Sphere(OBJECT *Object, TRANSFORM *Trans)
{
  SPHERE *Sphere = (SPHERE *)Object;

  if (Sphere->Trans == NULL)
  {
    Sphere->Methods = &Ellipsoid_Methods;

    Sphere->Trans = Create_Transform();
  }

  Compose_Transforms(Sphere->Trans, Trans);

  Compute_Sphere_BBox(Sphere);
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Sphere
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   ?
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

void Destroy_Sphere(OBJECT *Object)
{
#if(DUMP_OBJECT_DATA == 1)
  Debug_Info("{ // SPHERE \n");
  DUMP_OBJECT_FIELDS(Object);
  Debug_Info("\t{ %f, %f, %f }, // Center\n", \
             (DBL)((SPHERE *)Object)->Center[X], \
             (DBL)((SPHERE *)Object)->Center[Y], \
             (DBL)((SPHERE *)Object)->Center[Z]); \
  Debug_Info("\t%f // Radius\n", (DBL)((SPHERE *)Object)->Radius);
  Debug_Info("}\n");
#endif

  Destroy_Transform(((SPHERE *)Object)->Trans);

  POV_FREE (Object);
}



/*****************************************************************************
*
* FUNCTION
*
*   Compute_Sphere_BBox
*
* INPUT
*
*   Sphere - Sphere
*   
* OUTPUT
*
*   Sphere
*   
* RETURNS
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Calculate the bounding box of a sphere.
*
* CHANGES
*
*   Aug 1994 : Creation.
*
******************************************************************************/

void Compute_Sphere_BBox(SPHERE *Sphere)
{
  Make_BBox(Sphere->BBox, Sphere->Center[X] - Sphere->Radius, Sphere->Center[Y] - Sphere->Radius,  Sphere->Center[Z] - Sphere->Radius,
    2.0 * Sphere->Radius, 2.0 * Sphere->Radius, 2.0 * Sphere->Radius);

  if (Sphere->Trans != NULL)
  {
    Recompute_BBox(&Sphere->BBox, Sphere->Trans);
  }
}

/*****************************************************************************
*
* FUNCTION
*
*   Sphere_UVCoord
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Nathan Kopp   (adapted from spherical_image_map)
*
* DESCRIPTION
*
*   Find the UV coordinates of a sphere.
*   Map a point (x, y, z) on a sphere of radius 1 to a 2-d image. (Or is it the
*   other way around?)
*
* CHANGES
*
*   -
*
******************************************************************************/
static void Sphere_UVCoord(UV_VECT Result, OBJECT *Object, INTERSECTION *Inter)
{
  DBL len, phi, theta;
  DBL x,y,z;
  VECTOR New_Point, New_Center;
  SPHERE *Sphere = (SPHERE *)Object;

  /* Transform the point into the sphere's space */
  if (Object->UV_Trans != NULL)
  {
    
    MInvTransPoint(New_Point, Inter->IPoint, Object->UV_Trans);

    if (Sphere->Trans != NULL)
      MTransPoint(New_Center, Sphere->Center, Sphere->Trans);
    else
      Assign_Vector(New_Center, Sphere->Center);

    MInvTransPoint(New_Center, New_Center, Object->UV_Trans);
  }
  else
  {
    Assign_Vector(New_Point, Inter->IPoint);
    Assign_Vector(New_Center, Sphere->Center);
  }
  x = New_Point[X]-New_Center[X];
  y = New_Point[Y]-New_Center[Y];
  z = New_Point[Z]-New_Center[Z];

  len = sqrt(x * x + y * y + z * z);

  if (len == 0.0)
    return;
  else
  {
    x /= len;
    y /= len;
    z /= len;
  }

  /* Determine its angle from the x-z plane. */
  phi = 0.5 + asin(y) / M_PI; /* This will be from 0 to 1 */


  /* Determine its angle from the point (1, 0, 0) in the x-z plane. */
  len = x * x + z * z;

  if (len > EPSILON)
  {
    len = sqrt(len);
    if (z == 0.0)
    {
      if (x > 0)
        theta = 0.0;
      else
        theta = M_PI;
    }
    else
    {
      theta = acos(x / len);
      if (z < 0.0)
        theta = TWO_M_PI - theta;
    }

    theta /= TWO_M_PI;  /* This will be from 0 to 1 */
  }
  else
    /* This point is at one of the poles. Any value of xcoord will be ok... */
    theta = 0;

  Result[U] = theta;
  Result[V] = phi;
}

END_POV_NAMESPACE
