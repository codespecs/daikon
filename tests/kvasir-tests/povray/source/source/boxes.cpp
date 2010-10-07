/****************************************************************************
 *               boxes.cpp
 *
 * This module implements the box primitive.
 * This file was written by Alexander Enzmann.  He wrote the code for
 * boxes and generously provided us these enhancements.
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
 * $File: //depot/povray/3.6-release/source/boxes.cpp $
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
#include "boxes.h"
#include "matrices.h"
#include "objects.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

/* Minimal intersection depth. */

const DBL DEPTH_TOLERANCE = 1.0e-6;

/* Two values are equal if their difference is small than CLOSE_TOLERANCE. */

const DBL CLOSE_TOLERANCE = 1.0e-6;

/* Side hit. */

const int SIDE_X_0 = 1;
const int SIDE_X_1 = 2;
const int SIDE_Y_0 = 3;
const int SIDE_Y_1 = 4;
const int SIDE_Z_0 = 5;
const int SIDE_Z_1 = 6;



/*****************************************************************************
* Static functions
******************************************************************************/
static int  All_Box_Intersections (OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack);
static int  Inside_Box (VECTOR point, OBJECT *Object);
static void Box_Normal (VECTOR Result, OBJECT *Object, INTERSECTION *Inter);
static void Box_UVCoord (UV_VECT Result, OBJECT *Object, INTERSECTION *Inter);
static void Translate_Box (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Rotate_Box (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Scale_Box (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Transform_Box (OBJECT *Object, TRANSFORM *Trans);
static void Invert_Box (OBJECT *Object);



/*****************************************************************************
* Local variables
******************************************************************************/

METHODS Box_Methods =
{
  All_Box_Intersections,
  Inside_Box, Box_Normal, Box_UVCoord,
  (COPY_METHOD)Copy_Box, Translate_Box, Rotate_Box, Scale_Box, Transform_Box,
  Invert_Box, Destroy_Box
};



/*****************************************************************************
*
* FUNCTION
*
*   All_Box_Intersections
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

static int All_Box_Intersections(OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack)
{
  int Intersection_Found;
  int Side1, Side2;
  DBL Depth1, Depth2;
  VECTOR IPoint;

  Increase_Counter(stats[Ray_Box_Tests]);

  Intersection_Found = false;

  if (Intersect_Box(Ray, ((BOX *)Object)->Trans, ((BOX *)Object)->bounds[0], ((BOX *)Object)->bounds[1], &Depth1, &Depth2, &Side1, &Side2))
  {
    if (Depth1 > DEPTH_TOLERANCE)
    {
      VEvaluateRay(IPoint, Ray->Initial, Depth1, Ray->Direction);

      if (Point_In_Clip(IPoint, Object->Clip))
      {
        push_entry_i1(Depth1,IPoint,Object,Side1,Depth_Stack);

        Intersection_Found = true;
      }
    }

    VEvaluateRay(IPoint, Ray->Initial, Depth2, Ray->Direction);

    if (Point_In_Clip(IPoint, Object->Clip))
    {
      push_entry_i1(Depth2,IPoint,Object,Side2,Depth_Stack);

      Intersection_Found = true;
    }
  }

  if (Intersection_Found)
  {
    Increase_Counter(stats[Ray_Box_Tests_Succeeded]);
  }

  return (Intersection_Found);
}



/*****************************************************************************
*
* FUNCTION
*
*   Intersect_Box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   Sep 1994 : Added code to decide which side was hit in the case
*              intersection points are close to each other. This removes
*              some ugly artefacts one could observe at the corners of
*              boxes due to the usage of the wrong normal vector. [DB]
*
******************************************************************************/

int Intersect_Box(RAY *Ray, TRANSFORM *Trans, VECTOR Corner1, VECTOR Corner2, DBL *Depth1, DBL  *Depth2, int *Side1, int  *Side2)
{
  int smin = 0, smax = 0;    /* Side hit for min/max intersection. */
  DBL t, tmin, tmax;
  VECTOR P, D;

  /* Transform the point into the boxes space */

  if (Trans != NULL)
  {
    MInvTransPoint(P, Ray->Initial, Trans);
    MInvTransDirection(D, Ray->Direction, Trans);
  }
  else
  {
    Assign_Vector(P, Ray->Initial);
    Assign_Vector(D, Ray->Direction);
  }

  tmin = 0.0;
  tmax = BOUND_HUGE;

  /*
   * Sides first.
   */

  if (D[X] < -EPSILON)
  {
    t = (Corner1[X] - P[X]) / D[X];

    if (t < tmin) return(false);

    if (t <= tmax)
    {
      smax = SIDE_X_0;
      tmax = t;
    }

    t = (Corner2[X] - P[X]) / D[X];

    if (t >= tmin)
    {
      if (t > tmax) return(false);

      smin = SIDE_X_1;
      tmin = t;
    }
  }
  else
  {
    if (D[X] > EPSILON)
    {
      t = (Corner2[X] - P[X]) / D[X];

      if (t < tmin) return(false);

      if (t <= tmax)
      {
        smax = SIDE_X_1;
        tmax = t;
      }

      t = (Corner1[X] - P[X]) / D[X];

      if (t >= tmin)
      {
        if (t > tmax) return(false);

        smin = SIDE_X_0;
        tmin = t;
      }
    }
    else
    {
      if ((P[X] < Corner1[X]) || (P[X] > Corner2[X]))
      {
        return(false);
      }
    }
  }

  /*
   * Check Top/Bottom.
   */

  if (D[Y] < -EPSILON)
  {
    t = (Corner1[Y] - P[Y]) / D[Y];

    if (t < tmin) return(false);

    if (t <= tmax - CLOSE_TOLERANCE)
    {
      smax = SIDE_Y_0;
      tmax = t;
    }
    else
    {
      /*
       * If intersection points are close to each other find out
       * which side to use, i.e. is most probably hit. [DB 9/94]
       */

      if (t <= tmax + CLOSE_TOLERANCE)
      {
        if (-D[Y] > fabs(D[X])) smax = SIDE_Y_0;
      }
    }

    t = (Corner2[Y] - P[Y]) / D[Y];

    if (t >= tmin + CLOSE_TOLERANCE)
    {
      if (t > tmax) return(false);

      smin = SIDE_Y_1;
      tmin = t;
    }
    else
    {
      /*
       * If intersection points are close to each other find out
       * which side to use, i.e. is most probably hit. [DB 9/94]
       */

      if (t >= tmin - CLOSE_TOLERANCE)
      {
        if (-D[Y] > fabs(D[X])) smin = SIDE_Y_1;
      }
    }
  }
  else
  {
    if (D[Y] > EPSILON)
    {
      t = (Corner2[Y] - P[Y]) / D[Y];

      if (t < tmin) return(false);

      if (t <= tmax - CLOSE_TOLERANCE)
      {
        smax = SIDE_Y_1;
        tmax = t;
      }
      else
      {
        /*
         * If intersection points are close to each other find out
         * which side to use, i.e. is most probably hit. [DB 9/94]
         */

        if (t <= tmax + CLOSE_TOLERANCE)
        {
          if (D[Y] > fabs(D[X])) smax = SIDE_Y_1;
        }
      }

      t = (Corner1[Y] - P[Y]) / D[Y];

      if (t >= tmin + CLOSE_TOLERANCE)
      {
        if (t > tmax) return(false);

        smin = SIDE_Y_0;
        tmin = t;
      }
      else
      {
        /*
         * If intersection points are close to each other find out
         * which side to use, i.e. is most probably hit. [DB 9/94]
         */

        if (t >= tmin - CLOSE_TOLERANCE)
        {
          if (D[Y] > fabs(D[X])) smin = SIDE_Y_0;
        }
      }
    }
    else
    {
      if ((P[Y] < Corner1[Y]) || (P[Y] > Corner2[Y]))
      {
        return(false);
      }
    }
  }

  /* Now front/back */

  if (D[Z] < -EPSILON)
  {
    t = (Corner1[Z] - P[Z]) / D[Z];

    if (t < tmin) return(false);

    if (t <= tmax - CLOSE_TOLERANCE)
    {
      smax = SIDE_Z_0;
      tmax = t;
    }
    else
    {
      /*
       * If intersection points are close to each other find out
       * which side to use, i.e. is most probably hit. [DB 9/94]
       */

      if (t <= tmax + CLOSE_TOLERANCE)
      {
        switch (smax)
        {
          case SIDE_X_0 :
          case SIDE_X_1 : if (-D[Z] > fabs(D[X])) smax = SIDE_Z_0; break;

          case SIDE_Y_0 :
          case SIDE_Y_1 : if (-D[Z] > fabs(D[Y])) smax = SIDE_Z_0; break;
        }
      }
    }

    t = (Corner2[Z] - P[Z]) / D[Z];

    if (t >= tmin + CLOSE_TOLERANCE)
    {
      if (t > tmax) return(false);

      smin = SIDE_Z_1;
      tmin = t;
    }
    else
    {
      /*
       * If intersection points are close to each other find out
       * which side to use, i.e. is most probably hit. [DB 9/94]
       */

      if (t >= tmin - CLOSE_TOLERANCE)
      {
        switch (smin)
        {
          case SIDE_X_0 :
          case SIDE_X_1 : if (-D[Z] > fabs(D[X])) smin = SIDE_Z_1; break;

          case SIDE_Y_0 :
          case SIDE_Y_1 : if (-D[Z] > fabs(D[Y])) smin = SIDE_Z_1; break;
        }
      }
    }
  }
  else
  {
    if (D[Z] > EPSILON)
    {
      t = (Corner2[Z] - P[Z]) / D[Z];

      if (t < tmin) return(false);

      if (t <= tmax - CLOSE_TOLERANCE)
      {
        smax = SIDE_Z_1;
        tmax = t;
      }
      else
      {
        /*
         * If intersection points are close to each other find out
         * which side to use, i.e. is most probably hit. [DB 9/94]
         */

        if (t <= tmax + CLOSE_TOLERANCE)
        {
          switch (smax)
          {
            case SIDE_X_0 :
            case SIDE_X_1 : if (D[Z] > fabs(D[X])) smax = SIDE_Z_1; break;

            case SIDE_Y_0 :
            case SIDE_Y_1 : if (D[Z] > fabs(D[Y])) smax = SIDE_Z_1; break;
          }
        }
      }

      t = (Corner1[Z] - P[Z]) / D[Z];

      if (t >= tmin + CLOSE_TOLERANCE)
      {
        if (t > tmax) return(false);

        smin = SIDE_Z_0;
        tmin = t;
      }
      else
      {
        /*
         * If intersection points are close to each other find out
         * which side to use, i.e. is most probably hit. [DB 9/94]
         */

        if (t >= tmin - CLOSE_TOLERANCE)
        {
          switch (smin)
          {
            case SIDE_X_0 :
            case SIDE_X_1 : if (D[Z] > fabs(D[X])) smin = SIDE_Z_0; break;

            case SIDE_Y_0 :
            case SIDE_Y_1 : if (D[Z] > fabs(D[Y])) smin = SIDE_Z_0; break;
          }
        }
      }
    }
    else
    {
      if ((P[Z] < Corner1[Z]) || (P[Z] > Corner2[Z]))
      {
        return(false);
      }
    }
  }

  if (tmax < DEPTH_TOLERANCE)
  {
    return (false);
  }

  *Depth1 = tmin;
  *Depth2 = tmax;

  *Side1 = smin;
  *Side2 = smax;

  return(true);
}



/*****************************************************************************
*
* FUNCTION
*
*   Inside_Box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

static int Inside_Box(VECTOR IPoint, OBJECT *Object)
{
  VECTOR New_Point;
  BOX *box = (BOX *) Object;

  /* Transform the point into box space. */

  if (box->Trans != NULL)
  {
    MInvTransPoint(New_Point, IPoint, box->Trans);
  }
  else
  {
    Assign_Vector(New_Point,IPoint);
  }

  /* Test to see if we are outside the box. */

  if ((New_Point[X] < box->bounds[0][X]) || (New_Point[X] > box->bounds[1][X]))
  {
    return (Test_Flag(box, INVERTED_FLAG));
  }

  if ((New_Point[Y] < box->bounds[0][Y]) || (New_Point[Y] > box->bounds[1][Y]))
  {
    return (Test_Flag(box, INVERTED_FLAG));
  }

  if ((New_Point[Z] < box->bounds[0][Z]) || (New_Point[Z] > box->bounds[1][Z]))
  {
    return (Test_Flag(box, INVERTED_FLAG));
  }

  /* Inside the box. */

  return (!Test_Flag(box, INVERTED_FLAG));
}



/*****************************************************************************
*
* FUNCTION
*
*   Box_Normal
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

static void Box_Normal(VECTOR Result, OBJECT *Object, INTERSECTION *Inter)
{
  switch (Inter->i1)
  {
    case SIDE_X_0: Make_Vector(Result, -1.0,  0.0,  0.0); break;
    case SIDE_X_1: Make_Vector(Result,  1.0,  0.0,  0.0); break;
    case SIDE_Y_0: Make_Vector(Result,  0.0, -1.0,  0.0); break;
    case SIDE_Y_1: Make_Vector(Result,  0.0,  1.0,  0.0); break;
    case SIDE_Z_0: Make_Vector(Result,  0.0,  0.0, -1.0); break;
    case SIDE_Z_1: Make_Vector(Result,  0.0,  0.0,  1.0); break;

    default: Error("Unknown box side in Box_Normal().");
  }

  /* Transform the point into the boxes space. */

  if (((BOX *)Object)->Trans != NULL)
  {
    MTransNormal(Result, Result, ((BOX *)Object)->Trans);

    VNormalize(Result, Result);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Translate_Box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

static void Translate_Box(OBJECT *Object, VECTOR Vector, TRANSFORM *Trans)
{
  if (((BOX *)Object)->Trans == NULL)
  {
    VAddEq(((BOX *)Object)->bounds[0], Vector);

    VAddEq(((BOX *)Object)->bounds[1], Vector);

    Compute_Box_BBox((BOX *)Object);
  }
  else
  {
    Transform_Box(Object, Trans);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Rotate_Box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

static void Rotate_Box(OBJECT *Object, VECTOR, TRANSFORM *Trans)
{
  Transform_Box(Object, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Scale_Box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

static void Scale_Box(OBJECT *Object, VECTOR Vector, TRANSFORM *Trans)
{
  DBL temp;
  BOX *Box = (BOX *)Object;

  if (((BOX *)Object)->Trans == NULL)
  {
    VEvaluateEq(Box->bounds[0], Vector);
    VEvaluateEq(Box->bounds[1], Vector);

    if (Box->bounds[0][X] > Box->bounds[1][X])
    {
      temp = Box->bounds[0][X];

      Box->bounds[0][X] = Box->bounds[1][X];
      Box->bounds[1][X] = temp;
    }

    if (Box->bounds[0][Y] > Box->bounds[1][Y])
    {
      temp = Box->bounds[0][Y];

      Box->bounds[0][Y] = Box->bounds[1][Y];
      Box->bounds[1][Y] = temp;
    }

    if (Box->bounds[0][Z] > Box->bounds[1][Z])
    {
      temp = Box->bounds[0][Z];

      Box->bounds[0][Z] = Box->bounds[1][Z];
      Box->bounds[1][Z] = temp;
    }

    Compute_Box_BBox((BOX *)Object);
  }
  else
  {
    Transform_Box(Object, Trans);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Invert_Box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

static void Invert_Box(OBJECT *Object)
{
  Invert_Flag(Object, INVERTED_FLAG);
}



/*****************************************************************************
*
* FUNCTION
*
*   Transform_Box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

static void Transform_Box(OBJECT *Object, TRANSFORM *Trans)
{
  BOX *box = (BOX *)Object;

  if (box->Trans == NULL)
  {
    box->Trans = Create_Transform();
  }

  Compose_Transforms(box->Trans, Trans);

  Compute_Box_BBox(box);
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_Box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

BOX *Create_Box()
{
  BOX *New;

  New = (BOX *)POV_MALLOC(sizeof(BOX), "box");

  INIT_OBJECT_FIELDS(New, BOX_OBJECT, &Box_Methods)

  Make_Vector(New->bounds[0], -1.0, -1.0, -1.0);
  Make_Vector(New->bounds[1],  1.0,  1.0,  1.0);

  Make_BBox(New->BBox, -1.0, -1.0, -1.0, 2.0, 2.0, 2.0);

  New->Trans = NULL;

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

BOX *Copy_Box(OBJECT *Object)
{
  BOX *New;

  New  = Create_Box();

  /* Copy box. */

  *New = *((BOX *)Object);

  New->Trans = Copy_Transform(((BOX *)Object)->Trans);

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Alexander Enzmann
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

void Destroy_Box(OBJECT *Object)
{
  Destroy_Transform(((BOX *)Object)->Trans);

  POV_FREE (Object);
}



/*****************************************************************************
*
* FUNCTION
*
*   Compute_Box_BBox
*
* INPUT
*
*   Box - Box
*
* OUTPUT
*
*   Box
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Calculate the bounding box of a box.
*
* CHANGES
*
*   Aug 1994 : Creation.
*
******************************************************************************/

void Compute_Box_BBox(BOX *Box)
{
  // [ABX 20.01.2004] Low_Left introduced to hide BCC 5.5 bug
  BBOX_VECT& Low_Left = Box->BBox.Lower_Left;

  Assign_BBox_Vect(Low_Left, Box->bounds[0]);

  VSub(Box->BBox.Lengths, Box->bounds[1], Box->bounds[0]);

  if (Box->Trans != NULL)
  {
    Recompute_BBox(&Box->BBox, Box->Trans);
  }
}

/*****************************************************************************
*
* FUNCTION
*
*   Box_UVCoord
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Nathan Kopp, Lutz Kretzschmar
*
* DESCRIPTION
*
*        +-----+
*        ^  4  |
*        z     |
*  +-----+--x>-#--z>-+-<x--+
*  |     ^     |     |     |
*  |  1  y  5  |  2  |  6  |
*  |     |     |     |     |
*  +-----O--x>-+-----+-----+
*        |     |
*        |  3  |
*        +-----+
*
*  planes:
*  1: min x   2: max x
*  3: min y   4: max y
*  5: min z   6: max z
*
*  O : Origin
*  # : <1,1,0>
*
* CHANGES
*
*   The code was changed to use somthing similar to environmental cube mappping
*
*   1        +-----+           #
*            |     |
* V          z  4  |
*            |     |
*  .6  +--z>-+--x>-+-<z--+-<x--+
*      |     ^     |     |     |
*      |  1  y  5  |  2  |  6  |
*      |     |     |     |     |
*  .3  +-----+--x>-+-----+-----+
*            ^     |
*            z  3  |
*            |     |
*  0   O     +-----+
*  
*      0    .25    .5   .75    1
*                            U
*	
*  planes:
*  1: min x   2: max x
*  3: min y   4: max y
*  5: max z   6: min z
*
*  O : Origin of U,V map
*  # : <1,1,0>
*
******************************************************************************/

static void Box_UVCoord(UV_VECT Result, OBJECT *Object, INTERSECTION *Inter)
{
  VECTOR P, Box_Diff;
  BOX *Box = (BOX *)Object;

  /* Transform the point into the cube's space */
  if (Box->Trans != NULL)
    MInvTransPoint(P, Inter->IPoint, Box->Trans);
  else
    Assign_Vector(P, Inter->IPoint);

  VSub(Box_Diff,Box->bounds[1],Box->bounds[0]);

  /* this line moves the bottom,left,front corner of the box to <0,0,0> */
  VSubEq(P, Box->bounds[0]);
  /* this line normalizes the face offsets */
  VDivEq(P, Box_Diff);

  /* if no normalize above, then we should use Box->UV_Trans and also
     inverse-transform the bounds */

  /* The following code does a variation of cube environment mapping. All the
     textures are not mirrored when the cube is viewed from outside. */

  switch (Inter->i1)
  {
    case SIDE_X_0:
      Result[U] =               (P[Z] / 4.0);
      Result[V] = (1.0 / 3.0) + (P[Y] / 3.0);
      break;
    case SIDE_X_1:
      Result[U] = (3.0 / 4.0) - (P[Z] / 4.0);
      Result[V] = (1.0 / 3.0) + (P[Y] / 3.0);
      break;
    case SIDE_Y_0:
      Result[U] = (1.0 / 4.0) + (P[X] / 4.0);
      Result[V] =               (P[Z] / 3.0);
      break;
    case SIDE_Y_1:
      Result[U] = (1.0 / 4.0) + (P[X] / 4.0);
      Result[V] = (3.0 / 3.0) - (P[Z] / 3.0);
      break;
    case SIDE_Z_0:
      Result[U] =  1.0        - (P[X] / 4.0);
      Result[V] = (1.0 / 3.0) + (P[Y] / 3.0);
      break;
    case SIDE_Z_1:
      Result[U] = (1.0 / 4.0) + (P[X] / 4.0);
      Result[V] = (1.0 / 3.0) + (P[Y] / 3.0);
      break;

    default: Error("Unknown box side in Box_Normal().");
  }

/* 
   This is the original cube environment mapping. The texture is correct
   when viewed from inside the cube. 

  switch (Inter->i1)
  {
  case SIDE_X_0:
	  Result[U] = (1.0 / 4.0) - (P[Z] / 4.0);
	  Result[V] = (1.0 / 3.0) + (P[Y] / 3.0);
	  break;
  case SIDE_X_1:
	  Result[U] = (2.0 / 4.0) + (P[Z] / 4.0);
	  Result[V] = (1.0 / 3.0) + (P[Y] / 3.0);
	  break;
  case SIDE_Y_0:
	  Result[U] = (1.0 / 4.0) + (P[X] / 4.0);
	  Result[V] = (1.0 / 3.0) - (P[Z] / 3.0);
	  break;
  case SIDE_Y_1:
	  Result[U] = (1.0 / 4.0) + (P[X] / 4.0);
	  Result[V] = (2.0 / 3.0) + (P[Z] / 3.0);
	  break;
  case SIDE_Z_0:
	  Result[U] = (1.0 / 4.0) + (P[X] / 4.0);
	  Result[V] = (1.0 / 3.0) + (P[Y] / 3.0);
	  break;
  case SIDE_Z_1:
	  Result[U] = 1.0         - (P[X] / 4.0);
	  Result[V] = (1.0 / 3.0) + (P[Y] / 3.0);
	  break;
	  
  default: Error("Unknown box side in Box_Normal().");
  }
  */
}

END_POV_NAMESPACE
