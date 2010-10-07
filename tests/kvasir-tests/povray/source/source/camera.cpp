/****************************************************************************
 *               camera.cpp
 *
 * This module implements methods for managing the viewpoint.
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
 * $File: //depot/povray/3.6-release/source/camera.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "vector.h"
#include "camera.h"
#include "matrices.h"
#include "normal.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
*
* FUNCTION
*
*   Translate_Camera
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

void Translate_Camera(CAMERA *Camera, VECTOR Vector)
{
  VAddEq(((CAMERA *)Camera)->Location, Vector);
}



/*****************************************************************************
*
* FUNCTION
*
*   Rotate_Camera
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

void Rotate_Camera(CAMERA *Camera, VECTOR Vector)
{
  TRANSFORM Trans;
  
  Compute_Rotation_Transform(&Trans, Vector);
  
  Transform_Camera(Camera, &Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Scale_Camera
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

void Scale_Camera(CAMERA *Camera, VECTOR Vector)
{
  TRANSFORM Trans;
  
  Compute_Scaling_Transform(&Trans, Vector);
  
  Transform_Camera(Camera, &Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Transform_Camera
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

void Transform_Camera(CAMERA *Camera, TRANSFORM *Trans)
{
  MTransPoint(Camera->Location, Camera->Location, Trans);
  MTransDirection(Camera->Direction, Camera->Direction, Trans);
  MTransDirection(Camera->Up, Camera->Up, Trans);
  MTransDirection(Camera->Right, Camera->Right, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_Camera
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

CAMERA *Create_Camera()
{
  CAMERA *New;
  
  New = (CAMERA *)POV_MALLOC(sizeof (CAMERA), "camera");
  
  Make_Vector(New->Location,    0.0,  0.0, 0.0);
  Make_Vector(New->Direction,   0.0,  0.0, 1.0);
  Make_Vector(New->Up,          0.0,  1.0, 0.0);
  Make_Vector(New->Right,       1.33, 0.0, 0.0);
  Make_Vector(New->Sky,         0.0,  1.0, 0.0);
  Make_Vector(New->Look_At,     0.0,  0.0, 1.0);
  Make_Vector(New->Focal_Point, 0.0,  0.0, 1.0);

  /* Init focal blur stuff (not used by default). */
  New->Blur_Samples   = 0;
  New->Confidence     = 0.9;
  New->Variance       = 1.0 / 10000.0;
  New->Aperture       = 0.0;
  New->Focal_Distance = -1.0;

  /* Set default camera type and viewing angle. [DB 7/94] */
  New->Type = PERSPECTIVE_CAMERA;
  New->Angle = 90.0;

  /* Default view angle for spherical camera. [MH 6/99] */
  New->H_Angle = 360;
  New->V_Angle = 180;

  /* Do not perturb primary rays by default. [DB 7/94] */
  New->Tnormal = NULL;

  New->Trans = Create_Transform();

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Camera
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

CAMERA *Copy_Camera(CAMERA *Old)
{
  CAMERA *New;

  if (Old != NULL)
  {
    New = Create_Camera();

    Destroy_Tnormal(New->Tnormal);
    Destroy_Transform(New->Trans);

    *New = *Old;
    New->Tnormal = NULL; // clear in case the copy fails
    if(Old->Tnormal != NULL)
       New->Tnormal = Copy_Tnormal(Old->Tnormal);

    New->Trans = NULL; // clear in case the copy fails
    if(Old->Trans != NULL)
       New->Trans = Copy_Transform(Old->Trans);
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
*   Destroy_Camera
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
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

void Destroy_Camera(CAMERA *Camera)
{
  if (Camera != NULL)
  {
    Destroy_Tnormal(Camera->Tnormal);
    Destroy_Transform(Camera->Trans);

    POV_FREE(Camera);
  }
}

END_POV_NAMESPACE
