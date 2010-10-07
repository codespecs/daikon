/****************************************************************************
 *                  camera.h
 *
 * This module contains all defines, typedefs, and prototypes for CAMERA.CPP.
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
 * $File: //depot/povray/3.6-release/source/camera.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef CAMERA_H
#define CAMERA_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/




/* Available camera types. [DB 8/94] */

#define PERSPECTIVE_CAMERA      1
#define ORTHOGRAPHIC_CAMERA     2
#define FISHEYE_CAMERA          3
#define ULTRA_WIDE_ANGLE_CAMERA 4
#define OMNIMAX_CAMERA          5
#define PANORAMIC_CAMERA        6
#define CYL_1_CAMERA            7
#define CYL_2_CAMERA            8
#define CYL_3_CAMERA            9
#define CYL_4_CAMERA           10
#define SPHERICAL_CAMERA       11


/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Camera_Struct CAMERA;

struct Camera_Struct
{
  VECTOR Location;
  VECTOR Direction;
  VECTOR Up;
  VECTOR Right;
  VECTOR Sky;
  VECTOR Look_At;                /* Used only to record the user's preference */
  VECTOR Focal_Point;            /* Used only to record the user's preference */
  DBL Focal_Distance, Aperture;  /* ARE 9/92 for focal blur.                  */
  int Blur_Samples;              /* ARE 9/92 for focal blur.                  */
  DBL Confidence;                /* Probability for confidence test.          */
  DBL Variance;                  /* Max. variance for confidence test.        */
  int Type;                      /* Camera type.                              */
  DBL Angle;                     /* Viewing angle.                            */
  DBL H_Angle;                   /* Spherical horizontal viewing angle        */
  DBL V_Angle;                   /* Spherical verticle viewing angle          */
  TNORMAL *Tnormal;              /* Primary ray pertubation.                  */
  TRANSFORM *Trans;              /* Used only to record the user's input      */
};



/*****************************************************************************
* Global variables
******************************************************************************/




/*****************************************************************************
* Global functions
******************************************************************************/

void Translate_Camera (CAMERA *Cm, VECTOR Vector);
void Rotate_Camera (CAMERA *Cm, VECTOR Vector);
void Scale_Camera (CAMERA *Cm, VECTOR Vector);
void Transform_Camera (CAMERA *Cm, TRANSFORM *Trans);
CAMERA *Copy_Camera (CAMERA *Old);
CAMERA *Create_Camera (void);
void Destroy_Camera (CAMERA *Cm);

END_POV_NAMESPACE

#endif
