/****************************************************************************
 *                  bezier.h
 *
 * This module contains all defines, typedefs, and prototypes for BEZIER.CPP.
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
 * $File: //depot/povray/3.6-release/source/bezier.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef BEZIER_H
#define BEZIER_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define BICUBIC_PATCH_OBJECT (PATCH_OBJECT)
/* NK 1998 double_illuminate - removed +DOUBLE_ILLUMINATE from bicubic_patch */

#define BEZIER_INTERIOR_NODE 0
#define BEZIER_LEAF_NODE 1

#define MAX_PATCH_TYPE 2




/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef DBL DISTANCES[4][4];
typedef DBL WEIGHTS[4][4];
typedef struct Bicubic_Patch_Struct BICUBIC_PATCH;
typedef struct Bezier_Node_Struct BEZIER_NODE;
typedef struct Bezier_Child_Struct BEZIER_CHILDREN;
typedef struct Bezier_Vertices_Struct BEZIER_VERTICES;

struct Bezier_Child_Struct
{
  BEZIER_NODE *Children[4];
};

struct Bezier_Vertices_Struct
{
  float uvbnds[4];
  VECTOR Vertices[4];
};

struct Bezier_Node_Struct
{
  int Node_Type;      /* Is this an interior node, or a leaf */
  VECTOR Center;      /* Center of sphere bounding the (sub)patch */
  DBL Radius_Squared; /* Radius of bounding sphere (squared) */
  int Count;          /* # of subpatches associated with this node */
  void *Data_Ptr;     /* Either pointer to vertices or pointer to children */
};

struct Bicubic_Patch_Struct
{
  OBJECT_FIELDS
  int Patch_Type, U_Steps, V_Steps;
  VECTOR Control_Points[4][4];
  UV_VECT ST[4];
  VECTOR Bounding_Sphere_Center;
  DBL Bounding_Sphere_Radius;
  DBL Flatness_Value;
  DBL accuracy;
  
  BEZIER_NODE *Node_Tree;
  WEIGHTS      *Weights;
 };
  
  

/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

void Precompute_Patch_Values (BICUBIC_PATCH *Shape);
BICUBIC_PATCH *Create_Bicubic_Patch (void);
void Compute_Bicubic_Patch_BBox (BICUBIC_PATCH *Patch);
void beztype2_compute_normals (BICUBIC_PATCH *, VECTOR [4][4], DBL , VECTOR);
  
int intersect_bicubic_patch_nsk (RAY *, BICUBIC_PATCH *, ISTACK *);
void find_planes  ( VECTOR, VECTOR, VECTOR, DBL *,VECTOR,DBL *);

END_POV_NAMESPACE

#endif
