/****************************************************************************
 *               planes.cpp
 *
 * This module implements functions that manipulate planes.
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
 * $File: //depot/povray/3.6-release/source/planes.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "povray.h"
#include "vector.h"
#include "matrices.h"
#include "objects.h"
#include "planes.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

const DBL DEPTH_TOLERANCE = 1.0e-6;

/*****************************************************************************
* Static functions
******************************************************************************/

static int   Intersect_Plane (RAY *Ray, PLANE *Plane, DBL *Depth);
static int   All_Plane_Intersections (OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack);
static int   Inside_Plane (VECTOR point, OBJECT *Object);
static void  Plane_Normal (VECTOR Result, OBJECT *Object, INTERSECTION *Inter);
static PLANE *Copy_Plane (OBJECT *Object);
static void  Translate_Plane (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void  Rotate_Plane (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void  Scale_Plane (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void  Transform_Plane (OBJECT *Object, TRANSFORM *Trans);
static void  Invert_Plane (OBJECT *Object);
static void  Destroy_Plane (OBJECT *Object);

/*****************************************************************************
* Local variables
******************************************************************************/

METHODS Plane_Methods =
{
  All_Plane_Intersections,
  Inside_Plane, Plane_Normal, Default_UVCoord,
  (COPY_METHOD)Copy_Plane,
  Translate_Plane, Rotate_Plane,
  Scale_Plane, Transform_Plane, Invert_Plane, Destroy_Plane
};


/*****************************************************************************
*
* FUNCTION
*
*   All_Plane_Intersections
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

static int All_Plane_Intersections (OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack)
{
  DBL Depth;
  VECTOR IPoint;

  if (Intersect_Plane(Ray, (PLANE *)Object, &Depth))
  {
    VEvaluateRay(IPoint, Ray->Initial, Depth, Ray->Direction);

    if (Point_In_Clip(IPoint, Object->Clip))
    {
      push_entry(Depth,IPoint,Object,Depth_Stack);

      return(true);
    }
  }

  return(false);
}



/*****************************************************************************
*
* FUNCTION
*
*   Intersect_Plane
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

static int Intersect_Plane (RAY *Ray, PLANE *Plane, DBL *Depth)
{
  DBL NormalDotOrigin, NormalDotDirection;
  VECTOR P, D;

  Increase_Counter(stats[Ray_Plane_Tests]);

  if (Plane->Trans == NULL)
  {
    VDot(NormalDotDirection, Plane->Normal_Vector, Ray->Direction);

    if (fabs(NormalDotDirection) < EPSILON)
    {
      return(false);
    }

    VDot(NormalDotOrigin, Plane->Normal_Vector, Ray->Initial);
  }
  else
  {
    MInvTransPoint(P, Ray->Initial, Plane->Trans);
    MInvTransDirection(D, Ray->Direction, Plane->Trans);

    VDot(NormalDotDirection, Plane->Normal_Vector, D);

    if (fabs(NormalDotDirection) < EPSILON)
    {
      return(false);
    }

    VDot(NormalDotOrigin, Plane->Normal_Vector, P);
  }

  *Depth = -(NormalDotOrigin + Plane->Distance) / NormalDotDirection;

  if ((*Depth >= DEPTH_TOLERANCE) && (*Depth <= Max_Distance))
  {
    Increase_Counter(stats[Ray_Plane_Tests_Succeeded]);

    return (true);
  }
  else
  {
    return (false);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Inside_Plane
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

static int Inside_Plane (VECTOR IPoint, OBJECT *Object)
{
  DBL Temp;
  VECTOR P;

  if (((PLANE *)Object)->Trans == NULL)
  {
    VDot (Temp, IPoint, ((PLANE *)Object)->Normal_Vector);
  }
  else
  {
    MInvTransPoint(P, IPoint, ((PLANE *)Object)->Trans);

    VDot (Temp, P, ((PLANE *)Object)->Normal_Vector);
  }

  return((Temp + ((PLANE *)Object)->Distance) < EPSILON);
}



/*****************************************************************************
*
* FUNCTION
*
*   Plane_Normal
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

static void Plane_Normal (VECTOR Result, OBJECT *Object, INTERSECTION *)
{
  Assign_Vector(Result,((PLANE *)Object)->Normal_Vector);

  if (((PLANE *)Object)->Trans != NULL)
  {
    MTransNormal(Result, Result, ((PLANE *)Object)->Trans);

    VNormalize(Result, Result);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Translate_Plane
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

static void Translate_Plane (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans)
{
  VECTOR Translation;
  PLANE *Plane = (PLANE *)Object;

  if (Plane->Trans == NULL)
  {
    VEvaluate (Translation, ((PLANE *)Object)->Normal_Vector, Vector);

    ((PLANE *)Object)->Distance -= Translation[X] + Translation[Y] + Translation[Z];

    Compute_Plane_BBox((PLANE *)Object);
  }
  else
  {
    Transform_Plane(Object, Trans);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Rotate_Plane
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

static void Rotate_Plane (OBJECT *Object, VECTOR, TRANSFORM *Trans)
{
  if (((PLANE *)Object)->Trans == NULL)
  {
    MTransDirection(((PLANE *)Object)->Normal_Vector, ((PLANE *)Object)->Normal_Vector, Trans);

    Compute_Plane_BBox(((PLANE *)Object));
  }
  else
  {
    Transform_Plane (Object, Trans);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Scale_Plane
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

static void Scale_Plane (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans)
{
  DBL Length;
  PLANE *Plane = (PLANE  *) Object;

  if (Plane->Trans == NULL)
  {
    VDivEq(Plane->Normal_Vector, Vector);

    VLength(Length, ((PLANE *)Object)->Normal_Vector);

    VInverseScaleEq (((PLANE *)Object)->Normal_Vector, Length);

    ((PLANE *)Object)->Distance /= Length;

    Compute_Plane_BBox(Plane);
  }
  else
  {
    Transform_Plane (Object, Trans);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Invert_Plane
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

static void Invert_Plane (OBJECT *Object)
{
  VScaleEq(((PLANE *)Object)->Normal_Vector, -1.0);

  ((PLANE *)Object)->Distance *= -1.0;
}



/*****************************************************************************
*
* FUNCTION
*
*   Transform_Plane
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

static void Transform_Plane(OBJECT *Object, TRANSFORM *Trans)
{
  PLANE *Plane = (PLANE  *) Object;

  if (Plane->Trans == NULL)
  {
    Plane->Trans = Create_Transform();
  }

  Compose_Transforms(Plane->Trans, Trans);

  Compute_Plane_BBox(Plane);
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_Plane
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

PLANE *Create_Plane()
{
  PLANE *New;

  New = (PLANE *)POV_MALLOC(sizeof (PLANE), "plane");

  INIT_OBJECT_FIELDS(New,PLANE_OBJECT,&Plane_Methods)

  Make_Vector(New->Normal_Vector, 0.0, 1.0, 0.0);

  New ->Distance = 0.0;

  New->Trans = NULL;

  return(New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Plane
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

static PLANE *Copy_Plane (OBJECT *Object)
{
  PLANE *New;

  New = Create_Plane();

  Destroy_Transform(New->Trans);

  *New = *((PLANE *)Object);

  New->Trans = Copy_Transform(((PLANE *)Object)->Trans);

  return(New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Plane
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

static void Destroy_Plane(OBJECT *Object)
{
#if(DUMP_OBJECT_DATA == 1)
  Debug_Info("{ // PLANE \n");
  DUMP_OBJECT_FIELDS(Object);
  Debug_Info("\t{ %f, %f, %f }, // Normal_Vector\n", \
             (DBL)((PLANE *)Object)->Normal_Vector[X], \
             (DBL)((PLANE *)Object)->Normal_Vector[Y], \
             (DBL)((PLANE *)Object)->Normal_Vector[Z]); \
  Debug_Info("\t%f // Distance\n", (DBL)((PLANE *)Object)->Distance);
  Debug_Info("}\n");
#endif

  Destroy_Transform(((PLANE *)Object)->Trans);

  POV_FREE(Object);
}



/*****************************************************************************
*
* FUNCTION
*
*   Compute_Plane_BBox
*
* INPUT
*
*   Plane - Plane
*   
* OUTPUT
*
*   Plane
*   
* RETURNS
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Calculate the bounding box of a plane (it's always infinite).
*
* CHANGES
*
*   Aug 1994 : Creation.
*
******************************************************************************/

void Compute_Plane_BBox(PLANE *Plane)
{
  Make_BBox(Plane->BBox, -BOUND_HUGE/2, -BOUND_HUGE/2, -BOUND_HUGE/2,
    BOUND_HUGE, BOUND_HUGE, BOUND_HUGE);

  if (Plane->Clip != NULL)
  {
    Plane->BBox = Plane->Clip->BBox;
  }
}

END_POV_NAMESPACE
