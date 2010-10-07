/****************************************************************************
 *                  mesh.h
 *
 * This module contains all defines, typedefs, and prototypes for MESH.CPP.
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
 * $File: //depot/povray/3.6-release/source/mesh.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef MESH_H
#define MESH_H

#include "bbox.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define MESH_OBJECT (PATCH_OBJECT+HIERARCHY_OK_OBJECT)


/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Mesh_Struct MESH;
typedef struct Mesh_Data_Struct MESH_DATA;
typedef struct Mesh_Triangle_Struct MESH_TRIANGLE;

struct Mesh_Struct
{
  OBJECT_FIELDS
  MESH_DATA *Data;               /* Mesh data holding triangles.    */
  long Number_Of_Textures;       /* Number of textures in the mesh.   */
  TEXTURE **Textures;            /* Array of texture references.      */
  short has_inside_vector;
};

struct Mesh_Data_Struct
{
  int References;                /* Number of references to the mesh. */
  long Number_Of_UVCoords;       /* Number of UV coords in the mesh.  */
  long Number_Of_Normals;        /* Number of normals in the mesh.    */
  long Number_Of_Triangles;      /* Number of trinagles in the mesh.  */
  long Number_Of_Vertices;       /* Number of vertices in the mesh.   */
  SNGL_VECT *Normals, *Vertices; /* Arrays of normals and vertices.   */
  UV_VECT *UVCoords;             /* Array of UV coordinates           */
  MESH_TRIANGLE *Triangles;      /* Array of triangles.               */
  BBOX_TREE *Tree;               /* Bounding box tree for mesh.       */
  VECTOR Inside_Vect;            /* vector to use to test 'inside'    */
};

struct Mesh_Triangle_Struct
{
  unsigned int Smooth:1;         /* Is this a smooth triangle.            */
  unsigned int Dominant_Axis:2;  /* Dominant axis.                        */
  unsigned int vAxis:2;          /* Axis for smooth triangle.             */
  unsigned int ThreeTex:1;       /* Color Triangle Patch.                 */
  long Normal_Ind;               /* Index of unsmoothed triangle normal.  */
  long P1, P2, P3;               /* Indices of triangle vertices.         */
  long Texture;                  /* Index of triangle texture.            */
  long Texture2, Texture3;       /* Color Triangle Patch.                 */
  long N1, N2, N3;               /* Indices of smoothed triangle normals. */
  long UV1, UV2, UV3;            /* Indicies of UV coordinate vectors     */
  SNGL Distance;                 /* Distance of triangle along normal.    */
  SNGL_VECT Perp;                /* Vector used for smooth triangles.     */
};



/*****************************************************************************
* Global variables
******************************************************************************/

extern METHODS Mesh_Methods;


/*****************************************************************************
* Global functions
******************************************************************************/

MESH *Create_Mesh (void);
int Compute_Mesh_Triangle (MESH_TRIANGLE *Triangle, int Smooth, VECTOR P1, VECTOR P2, VECTOR P3, VECTOR S_Normal);
void Compute_Mesh_BBox (MESH *Mesh);
void Init_Mesh_Triangle (MESH_TRIANGLE *Triangle);
void Build_Mesh_BBox_Tree (MESH *Mesh);
void Test_Mesh_Opacity (MESH *Blob);

void Create_Mesh_Hash_Tables (void);
void Destroy_Mesh_Hash_Tables (void);
int Mesh_Hash_Vertex (int *Number_Of_Vertices, int *Max_Vertices, SNGL_VECT **Vertices, VECTOR Vertex);
int Mesh_Hash_Normal (int *Number_Of_Normals, int *Max_Normals, SNGL_VECT **Normals, VECTOR Normal);
int Mesh_Hash_Texture (int *Number_Of_Textures, int *Max_Textures, TEXTURE ***Textures, TEXTURE *Texture);
int Mesh_Hash_UV (int *Number, int *Max, UV_VECT **Elements, UV_VECT aPoint);
int Mesh_Degenerate (VECTOR P1, VECTOR P2, VECTOR P3);
void Initialize_Mesh_Code (void);
void Deinitialize_Mesh_Code (void);
int Mesh_Interpolate(VECTOR Weights, VECTOR IPoint, MESH *m, MESH_TRIANGLE *Triangle);

END_POV_NAMESPACE

#endif
