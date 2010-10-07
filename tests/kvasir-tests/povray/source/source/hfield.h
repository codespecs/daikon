/****************************************************************************
 *                  hfield.h
 *
 * This module contains all defines, typedefs, and prototypes for HFIELD.CPP.
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
 * $File: //depot/povray/3.6-release/source/hfield.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef HFIELD_H
#define HFIELD_H

#include "bbox.h"
#include "boxes.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define HFIELD_OBJECT (BASIC_OBJECT+HIERARCHY_OK_OBJECT)

#define HF_CACHE_SIZE 16

/* Generate additional height field statistics. */

#define HFIELD_EXTRA_STATS 1


/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct HField_Struct HFIELD;
typedef struct HField_Data_Struct HFIELD_DATA;
typedef struct HField_Block_Struct HFIELD_BLOCK;
typedef struct HField_Normal_Struct HFIELD_NORMAL;
typedef short HF_Normals[3];
typedef unsigned short HF_VAL;

struct HField_Normal_Struct
{
  DBL fx, fz;
  VECTOR normal;
};

struct HField_Block_Struct
{
  int xmin, xmax;
  int zmin, zmax;
  DBL ymin, ymax;
};

struct HField_Data_Struct
{
  int References;
  int cache_pos;
  int Normals_Height;  /* Needed for Destructor */
  int max_x, max_z;
  HF_VAL min_y, max_y;
  int block_max_x, block_max_z;
  int block_width_x, block_width_z;
  HF_VAL **Map;
  HF_Normals **Normals;
  HFIELD_NORMAL Normal_Cache[HF_CACHE_SIZE];
  HFIELD_BLOCK **Block;
};

struct HField_Struct
{
  OBJECT_FIELDS
  VECTOR bounding_corner1;
  VECTOR bounding_corner2;
  HFIELD_DATA *Data;
};



/*****************************************************************************
* Global variables
******************************************************************************/

extern METHODS HField_Methods;


/*****************************************************************************
* Global functions
******************************************************************************/

HFIELD *Create_HField (void);
void   Compute_HField_BBox (HFIELD *HField);
void   Compute_HField (HFIELD *H_Field, IMAGE *Image);

END_POV_NAMESPACE

#endif
