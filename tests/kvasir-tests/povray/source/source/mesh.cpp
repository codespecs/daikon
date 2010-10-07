/****************************************************************************
 *               mesh.cpp
 *
 * This module implements primitives for mesh objects.
 *
 * This module was written by Dieter Bayer [DB].
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
 * $File: //depot/povray/3.6-release/source/mesh.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

/****************************************************************************
*
*  Explanation:
*
*    -
*
*  Syntax:
*
*    mesh
*    {
*      triangle { <CORNER1>, <CORNER2>, <CORNER3>, texture { NAME } }
*      smooth_triangle { <CORNER1>, <NORMAL1>, <CORNER2>, <NORMAL2>, <CORNER3>, <NORMAL3>, texture { NAME } }
*      ...
*      [ hierarchy FLAG ]
*    }
*
*  ---
*
*  Feb 1995 : Creation. [DB]
*
*****************************************************************************/

#include "frame.h"
#include "vector.h"
#include "bbox.h"
#include "matrices.h"
#include "objects.h"
#include "mesh.h"
#include "texture.h"
#include "povray.h"
#include "triangle.h"

#include <algorithm>

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

const DBL DEPTH_TOLERANCE = 1e-6;

#define max3_coordinate(x,y,z) ((x > y) ? ((x > z) ? X : Z) : ((y > z) ? Y : Z))

const int HASH_SIZE = 1000;

const int INITIAL_NUMBER_OF_ENTRIES = 256;


/*****************************************************************************
* Local typedefs
******************************************************************************/

typedef struct Hash_Table_Struct HASH_TABLE;
typedef struct UV_Hash_Table_Struct UV_HASH_TABLE;

struct Hash_Table_Struct
{
  int Index;
  SNGL_VECT P;
  HASH_TABLE *Next;
};

struct UV_Hash_Table_Struct
{
  int Index;
  UV_VECT P;
  UV_HASH_TABLE *Next;
};


/*****************************************************************************
* Static functions
******************************************************************************/

static int Intersect_Mesh  (RAY *Ray, MESH *Mesh, ISTACK *Depth_Stack);
static int All_Mesh_Intersections  (OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack);
static int Inside_Mesh  (VECTOR IPoint, OBJECT *Object);
static void Mesh_Normal  (VECTOR Result, OBJECT *Object, INTERSECTION *Inter);
static void Mesh_UVCoord  (UV_VECT Result, OBJECT *Object, INTERSECTION *Inter);
static MESH *Copy_Mesh  (OBJECT *Object);
static void Translate_Mesh  (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Rotate_Mesh  (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Scale_Mesh  (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
static void Transform_Mesh  (OBJECT *Object, TRANSFORM *Trans);
static void Invert_Mesh  (OBJECT *Object);
static void Destroy_Mesh  (OBJECT *Object);

static void compute_smooth_triangle (MESH_TRIANGLE *Triangle, VECTOR P1, VECTOR P2, VECTOR P3);
static int intersect_mesh_triangle (RAY *Ray, MESH *Mesh, MESH_TRIANGLE *Triangle, DBL *Depth);
static int test_hit (MESH_TRIANGLE *Triangle, MESH *Mesh, RAY *OrigRay, RAY *Ray, DBL Depth, DBL len, ISTACK *Depth_Stack);
static void smooth_mesh_normal (MESH *Mesh, VECTOR Result, MESH_TRIANGLE *Triangle, VECTOR IPoint);
static void get_triangle_bbox (MESH *Mesh, MESH_TRIANGLE *Triangle, BBOX *BBox);

static int intersect_bbox_tree (MESH *Mesh, RAY *Ray, RAY *Orig_Ray, DBL len, ISTACK *Depth_Stack);

/* NK 1998 */
static int inside_bbox_tree (MESH *Mesh, RAY *Ray);
/* NK ---- */

static void get_triangle_vertices (MESH *Mesh, MESH_TRIANGLE *Triangle, VECTOR P1, VECTOR P2, VECTOR P3);
static void get_triangle_normals (MESH *Mesh, MESH_TRIANGLE *Triangle, VECTOR N1, VECTOR N2, VECTOR N3);
static void get_triangle_uvcoords (MESH *Mesh, MESH_TRIANGLE *Triangle, UV_VECT U1, UV_VECT U2, UV_VECT U3);

static int mesh_hash (HASH_TABLE **Hash_Table,
  int *Number, int *Max, SNGL_VECT **Elements, VECTOR aPoint);



/*****************************************************************************
* Local variables
******************************************************************************/

METHODS Mesh_Methods =
{
  All_Mesh_Intersections,
  Inside_Mesh, Mesh_Normal, Mesh_UVCoord,
  (COPY_METHOD)Copy_Mesh,
  Translate_Mesh, Rotate_Mesh,
  Scale_Mesh, Transform_Mesh, Invert_Mesh, Destroy_Mesh
};

static HASH_TABLE **Vertex_Hash_Table, **Normal_Hash_Table; // GLOBAL VARIABLE
static UV_HASH_TABLE **UV_Hash_Table; // GLOBAL VARIABLE

static PRIORITY_QUEUE *Mesh_Queue; // GLOBAL VARIABLE



/*****************************************************************************
*
* FUNCTION
*
*   All_Mesh_Intersections
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static int All_Mesh_Intersections(OBJECT *Object, RAY *Ray, ISTACK *Depth_Stack)
{
  Increase_Counter(stats[Ray_Mesh_Tests]);

  if (Intersect_Mesh(Ray, (MESH *)Object, Depth_Stack))
  {
    Increase_Counter(stats[Ray_Mesh_Tests_Succeeded]);

    return(true);
  }

  return(false);
}



/*****************************************************************************
*
* FUNCTION
*
*   Intersect_Mesh
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static int Intersect_Mesh(RAY *Ray, MESH *Mesh, ISTACK *Depth_Stack)
{
  int i;
  unsigned found;
  DBL len, t;
  RAY New_Ray;

  /* Transform the ray into mesh space. */

  if (Mesh->Trans != NULL)
  {
    MInvTransPoint(New_Ray.Initial, Ray->Initial, Mesh->Trans);
    MInvTransDirection(New_Ray.Direction, Ray->Direction, Mesh->Trans);

    VLength(len, New_Ray.Direction);
    VInverseScaleEq(New_Ray.Direction, len);
  }
  else
  {
    New_Ray = *Ray;

    len = 1.0;
  }

  found = false;

  if (Mesh->Data->Tree == NULL)
  {
    /* There's no bounding hierarchy so just step through all elements. */

    for (i = 0; i < Mesh->Data->Number_Of_Triangles; i++)
    {
      if (intersect_mesh_triangle(&New_Ray, Mesh, &Mesh->Data->Triangles[i], &t))
      {
        if (test_hit(&Mesh->Data->Triangles[i], Mesh, Ray, &New_Ray, t, len, Depth_Stack))
        {
          found = true;
        }
      }
    }
  }
  else
  {
    /* Use the mesh's bounding hierarchy. */

    return(intersect_bbox_tree(Mesh, &New_Ray, Ray, len, Depth_Stack));
  }

  return(found);
}



/*****************************************************************************
*
* FUNCTION
*
*   Inside_Mesh
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Nathan Kopp & Dieter Bayer (adapted from Intersect_Mesh by Dieter Bayer)
*   
* DESCRIPTION
*
*   Shoot a ray out from this point, if the ray hits an odd number of
*     triangles, it is inside the object.  If it hits an even number
*     than it is outside the object.
*   The triangle mesh should be closed, otherwise results are unpredictable.
*
* CHANGES
*
*   October, 1998 : Creation.
*
******************************************************************************/

static int Inside_Mesh(VECTOR IPoint, OBJECT *Object)
{
  int inside, i;
  unsigned found;
  DBL len, t;
  RAY Ray, New_Ray;
  MESH *Mesh;

  Mesh = (MESH *)Object;
  if (Mesh->has_inside_vector==false)
 	  return false;

  /* shoot a ray in the chosen direction from this point */
  /*if( (fabs(Mesh->Data->Inside_Vect[X]) < EPSILON) &&
  (fabs(Mesh->Data->Inside_Vect[Y]) < EPSILON) &&
  (fabs(Mesh->Data->Inside_Vect[Z]) < EPSILON))
  {
*/	/* if no inside_vect , use X... maybe we should quit */
/* return(false); */
/*
	Make_Vector(Ray.Direction, 0, 0, 1);
  }
  else
  */
    Assign_Vector(Ray.Direction, Mesh->Data->Inside_Vect);

  Assign_Vector(Ray.Initial, IPoint);

  /* Transform the ray into mesh space. */
  if (Mesh->Trans != NULL)
  {
    MInvTransPoint(New_Ray.Initial, Ray.Initial, Mesh->Trans);
    MInvTransDirection(New_Ray.Direction, Ray.Direction, Mesh->Trans);

    VLength(len, New_Ray.Direction);
    VInverseScaleEq(New_Ray.Direction, len);
  }
  else
  {
    New_Ray = Ray;
    len = 1.0;
  }

  found = 0;

  if (Mesh->Data->Tree == NULL)
  {
    /* just step through all elements. */
    for (i = 0; i < Mesh->Data->Number_Of_Triangles; i++)
    {
      if (intersect_mesh_triangle(&New_Ray, Mesh, &Mesh->Data->Triangles[i], &t))
      {
		/* actually, this should push onto a local depth stack and
		   make sure that we don't have the same intersection point from
		   two (or three) different triangles!!!!! */
          found++;
      }
    }
    /* odd number = inside, even number = outside */
    inside = found & 1;
  }
  else
  {
    /* Use the mesh's bounding hierarchy. */
    inside = inside_bbox_tree(Mesh, &New_Ray);
  }

  if (Test_Flag(Object, INVERTED_FLAG))
  {
    inside = !inside;
  }
  return (inside);
}




/*****************************************************************************
*
* FUNCTION
*
*   Mesh_Normal
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
*   Return the normalized normal in the given point.
*
* CHANGES
*
*   Feb 1995 : Creation.
*
******************************************************************************/

static void Mesh_Normal(VECTOR Result, OBJECT *Object, INTERSECTION *Inter)
{
  VECTOR IPoint;
  MESH_TRIANGLE *Triangle;
  MESH *Mesh = (MESH *)Object;

  Triangle = (MESH_TRIANGLE *)Inter->Pointer;

  if (Triangle->Smooth)
  {
    if (Mesh->Trans != NULL)
    {
      MInvTransPoint(IPoint, Inter->IPoint, Mesh->Trans);
    }
    else
    {
      Assign_Vector(IPoint, Inter->IPoint);
    }

    smooth_mesh_normal(Mesh, Result, Triangle, IPoint);
  
    if (Mesh->Trans != NULL)
    {
      MTransNormal(Result, Result, Mesh->Trans);
    }

    VNormalize(Result, Result);
  }
  else
  {
    Assign_Vector(Result, Mesh->Data->Normals[Triangle->Normal_Ind]);

    if (Mesh->Trans != NULL)
    {
      MTransNormal(Result, Result, Mesh->Trans);

      VNormalize(Result, Result);
    }
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   smooth_mesh_normal
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
*   Remove the un-normalized normal of a smoothed triangle.
*
* CHANGES
*
*   Feb 1995 : Creation. (Derived from TRIANGLE.C)
*
******************************************************************************/

static void smooth_mesh_normal(MESH *Mesh, VECTOR Result, MESH_TRIANGLE *Triangle, VECTOR  IPoint)
{
  int axis;
  DBL u, v;
  DBL k1, k2, k3;
  VECTOR PIMinusP1, N1, N2, N3;

  get_triangle_normals(Mesh, Triangle, N1, N2, N3);

  VSub(PIMinusP1, IPoint, Mesh->Data->Vertices[Triangle->P1]);

  VDot(u, PIMinusP1, Triangle->Perp);

  if (u < EPSILON)
  {
    Assign_Vector(Result, N1);
  }
  else
  {
    axis = Triangle->vAxis;

    k1 = Mesh->Data->Vertices[Triangle->P1][axis];
    k2 = Mesh->Data->Vertices[Triangle->P2][axis];
    k3 = Mesh->Data->Vertices[Triangle->P3][axis];

    v = (PIMinusP1[axis] / u + k1 - k2) / (k3 - k2);

    Result[X] = N1[X] + u * (N2[X] - N1[X] + v * (N3[X] - N2[X]));
    Result[Y] = N1[Y] + u * (N2[Y] - N1[Y] + v * (N3[Y] - N2[Y]));
    Result[Z] = N1[Z] + u * (N2[Z] - N1[Z] + v * (N3[Z] - N2[Z]));
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Translate_Mesh
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static void Translate_Mesh(OBJECT *Object, VECTOR, TRANSFORM *Trans)
{
  Transform_Mesh(Object, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Rotate_Mesh
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static void Rotate_Mesh(OBJECT *Object, VECTOR, TRANSFORM *Trans)
{
  Transform_Mesh(Object, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Scale_Mesh
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static void Scale_Mesh(OBJECT *Object, VECTOR, TRANSFORM *Trans)
{
  Transform_Mesh(Object, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Transfrom_Mesh
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static void Transform_Mesh(OBJECT *Object, TRANSFORM *Trans)
{
  int i;

  if (((MESH *)Object)->Trans == NULL)
  {
    ((MESH *)Object)->Trans = Create_Transform();
  }

  Recompute_BBox(&Object->BBox, Trans);

  Compose_Transforms(((MESH *)Object)->Trans, Trans);

  /* NK 1998 added if */
  if (!Test_Flag(Object, UV_FLAG))
    for (i=0; i<((MESH *)Object)->Number_Of_Textures; i++)
	{
      Transform_Textures(((MESH *)Object)->Textures[i], Trans);
	}
}



/*****************************************************************************
*
* FUNCTION
*
*   Invert_Mesh
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static void Invert_Mesh(OBJECT *Object)
{
  Invert_Flag(Object, INVERTED_FLAG);
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_Mesh
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
*   Feb 1995 : Creation.
*
******************************************************************************/

MESH *Create_Mesh()
{
  MESH *New;

  New = (MESH *)POV_MALLOC(sizeof(MESH), "mesh");

  INIT_OBJECT_FIELDS(New,MESH_OBJECT,&Mesh_Methods)

  Set_Flag(New, HIERARCHY_FLAG);

  New->Trans = NULL;

  New->Data = NULL;

  New->has_inside_vector=false;

  New->Number_Of_Textures=0;	/* [LSK] these were uninitialized */
  New->Textures=NULL;

  return(New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Mesh
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
*   Copy a mesh.
*
*   NOTE: The components are not copied, only the number of references is
*         counted, so that Destroy_Mesh() knows if they can be destroyed.
*
*   -
*
* CHANGES
*
*   Feb 1995 : Creation.
*
******************************************************************************/

static MESH *Copy_Mesh(OBJECT *Object)
{
  MESH *New;
  int i;

  New = Create_Mesh();

  /* Copy mesh. */

  *New = *((MESH *)Object);
  
  New->Trans = Copy_Transform(New->Trans);
  
  New->Data->References++;

  /* NK 1999 copy textures */
  if (((MESH *)Object)->Textures != NULL)
  {
    New->Textures = (TEXTURE **)POV_MALLOC(((MESH *)Object)->Number_Of_Textures*sizeof(TEXTURE *), "triangle mesh data");
    for (i = 0; i < ((MESH *)Object)->Number_Of_Textures; i++)
    {
      New->Textures[i] = Copy_Textures(((MESH *)Object)->Textures[i]);
    }
  }

  return(New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Mesh
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static void Destroy_Mesh(OBJECT *Object)
{
  int i;
  MESH *Mesh = (MESH *)Object;

  Destroy_Transform(Mesh->Trans);

  /* NK 1999 move texture outside of data block */
  if (Mesh->Textures != NULL)
  {
    for (i = 0; i < Mesh->Number_Of_Textures; i++)
    {
      Destroy_Textures(Mesh->Textures[i]);
    }

    POV_FREE(Mesh->Textures);
  }

  if (--(Mesh->Data->References) == 0)
  {
    Destroy_BBox_Tree(Mesh->Data->Tree);

    if (Mesh->Data->Normals != NULL)
    {
      POV_FREE(Mesh->Data->Normals);
    }

    /* NK 1998 */
    if (Mesh->Data->UVCoords != NULL)
    {
      POV_FREE(Mesh->Data->UVCoords);
    }
    /* NK ---- */

    if (Mesh->Data->Vertices != NULL)
    {
      POV_FREE(Mesh->Data->Vertices);
    }

    if (Mesh->Data->Triangles != NULL)
    {
      POV_FREE(Mesh->Data->Triangles);
    }

    POV_FREE(Mesh->Data);
  }

  POV_FREE(Object);
}



/*****************************************************************************
*
* FUNCTION
*
*   Compute_Mesh_BBox
*
* INPUT
*
*   Mesh - Mesh
*   
* OUTPUT
*
*   Mesh
*   
* RETURNS
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Calculate the bounding box of a triangle.
*
* CHANGES
*
*   Feb 1995 : Creation.
*
******************************************************************************/

void Compute_Mesh_BBox(MESH *Mesh)
{
  int i;
  VECTOR P1, P2, P3;
  VECTOR mins, maxs;

  Make_Vector(mins, BOUND_HUGE, BOUND_HUGE, BOUND_HUGE);
  Make_Vector(maxs, -BOUND_HUGE, -BOUND_HUGE, -BOUND_HUGE);

  for (i = 0; i < Mesh->Data->Number_Of_Triangles; i++)
  {
    get_triangle_vertices(Mesh, &Mesh->Data->Triangles[i], P1, P2, P3);

    mins[X] = min(mins[X], min3(P1[X], P2[X], P3[X]));
    mins[Y] = min(mins[Y], min3(P1[Y], P2[Y], P3[Y]));
    mins[Z] = min(mins[Z], min3(P1[Z], P2[Z], P3[Z]));

    maxs[X] = max(maxs[X], max3(P1[X], P2[X], P3[X]));
    maxs[Y] = max(maxs[Y], max3(P1[Y], P2[Y], P3[Y]));
    maxs[Z] = max(maxs[Z], max3(P1[Z], P2[Z], P3[Z]));
  }

  Make_BBox_from_min_max(Mesh->BBox, mins, maxs);
}



/*****************************************************************************
*
* FUNCTION
*
*   Compute_Mesh
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
*   Feb 1995 : Creation.
*
******************************************************************************/

int Compute_Mesh_Triangle(MESH_TRIANGLE *Triangle, int Smooth, VECTOR P1, VECTOR  P2, VECTOR  P3, VECTOR  S_Normal)
{
  int temp, swap;
  DBL x, y, z;
  VECTOR V1, V2, T1;
  DBL Length;

  VSub(V1, P2, P1);
  VSub(V2, P3, P1);

  VCross(S_Normal, V2, V1);

  VLength(Length, S_Normal);

  /* Set up a flag so we can ignore degenerate triangles */

  if (Length == 0.0)
  {
    return(false);
  }

  /* Normalize the normal vector. */

  VInverseScaleEq(S_Normal, Length);

  VDot(Triangle->Distance, S_Normal, P1);

  Triangle->Distance *= -1.0;

  /* Find triangle's dominant axis. */

  x = fabs(S_Normal[X]);
  y = fabs(S_Normal[Y]);
  z = fabs(S_Normal[Z]);

  Triangle->Dominant_Axis = max3_coordinate(x, y, z);

  swap = false;

  switch (Triangle->Dominant_Axis)
  {
    case X:

      if ((P2[Y] - P3[Y])*(P2[Z] - P1[Z]) < (P2[Z] - P3[Z])*(P2[Y] - P1[Y]))
      {
        swap = true;
      }

      break;

    case Y:

      if ((P2[X] - P3[X])*(P2[Z] - P1[Z]) < (P2[Z] - P3[Z])*(P2[X] - P1[X]))
      {
        swap = true;
      }

      break;

    case Z:

      if ((P2[X] - P3[X])*(P2[Y] - P1[Y]) < (P2[Y] - P3[Y])*(P2[X] - P1[X]))
      {
        swap = true;
      }

      break;
  }

  if (swap)
  {
    temp = Triangle->P2;
    Triangle->P2 = Triangle->P1;
    Triangle->P1 = temp;

    /* NK 1998 */
    temp = Triangle->UV2;
    Triangle->UV2 = Triangle->UV1;
    Triangle->UV1 = temp;

    if (Triangle->ThreeTex)
    {
      temp = Triangle->Texture2;
      Triangle->Texture2 = Triangle->Texture;
      Triangle->Texture = temp;
    }

    Assign_Vector(T1, P1);
    Assign_Vector(P1, P2);
    Assign_Vector(P2, T1);

    if (Smooth)
    {
      temp = Triangle->N2;
      Triangle->N2 = Triangle->N1;
      Triangle->N1 = temp;
    }
  }

  if (Smooth)
  {
  //  compute_smooth_triangle(Triangle, P1, P2, P3);
  Triangle->Smooth = true;
  }

    compute_smooth_triangle(Triangle, P1, P2, P3);

  return(true);
}



/*****************************************************************************
*
* FUNCTION
*
*   compute_smooth_triangle
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static void compute_smooth_triangle(MESH_TRIANGLE *Triangle, VECTOR P1, VECTOR  P2, VECTOR  P3)
{
  VECTOR P3MinusP2, VTemp1, VTemp2;
  DBL x, y, z, uDenominator, Proj;

  VSub(P3MinusP2, P3, P2);

  x = fabs(P3MinusP2[X]);
  y = fabs(P3MinusP2[Y]);
  z = fabs(P3MinusP2[Z]);

  Triangle->vAxis = max3_coordinate(x, y, z);

  VSub(VTemp1, P2, P3);

  VNormalize(VTemp1, VTemp1);

  VSub(VTemp2, P1, P3);

  VDot(Proj, VTemp2, VTemp1);

  VScaleEq(VTemp1, Proj);

  VSub(Triangle->Perp, VTemp1, VTemp2);

  VNormalize(Triangle->Perp, Triangle->Perp);

  VDot(uDenominator, VTemp2, Triangle->Perp);

  VInverseScaleEq(Triangle->Perp, -uDenominator);
}



/*****************************************************************************
*
* FUNCTION
*
*   intersect_mesh_triangle
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static int intersect_mesh_triangle(RAY *Ray, MESH *Mesh, MESH_TRIANGLE *Triangle, DBL *Depth)
{
  DBL NormalDotOrigin, NormalDotDirection;
  DBL s, t;
  VECTOR P1, P2, P3, S_Normal;

  Assign_Vector(S_Normal, Mesh->Data->Normals[Triangle->Normal_Ind]);

  VDot(NormalDotDirection, S_Normal, Ray->Direction);

  if (fabs(NormalDotDirection) < EPSILON)
  {
    return(false);
  }

  VDot(NormalDotOrigin, S_Normal, Ray->Initial);

  *Depth = -(Triangle->Distance + NormalDotOrigin) / NormalDotDirection;

  if ((*Depth < DEPTH_TOLERANCE) || (*Depth > Max_Distance))
  {
    return(false);
  }

  get_triangle_vertices(Mesh, Triangle, P1, P2, P3);

  switch (Triangle->Dominant_Axis)
  {
    case X:

      s = Ray->Initial[Y] + *Depth * Ray->Direction[Y];
      t = Ray->Initial[Z] + *Depth * Ray->Direction[Z];

      if ((P2[Y] - s) * (P2[Z] - P1[Z]) < (P2[Z] - t) * (P2[Y] - P1[Y]))
      {
        return(false);
      }

      if ((P3[Y] - s) * (P3[Z] - P2[Z]) < (P3[Z] - t) * (P3[Y] - P2[Y]))
      {
        return(false);
      }

      if ((P1[Y] - s) * (P1[Z] - P3[Z]) < (P1[Z] - t) * (P1[Y] - P3[Y]))
      {
        return(false);
      }

      return(true);

    case Y:

      s = Ray->Initial[X] + *Depth * Ray->Direction[X];
      t = Ray->Initial[Z] + *Depth * Ray->Direction[Z];

      if ((P2[X] - s) * (P2[Z] - P1[Z]) < (P2[Z] - t) * (P2[X] - P1[X]))
      {
        return(false);
      }

      if ((P3[X] - s) * (P3[Z] - P2[Z]) < (P3[Z] - t) * (P3[X] - P2[X]))
      {
        return(false);
      }

      if ((P1[X] - s) * (P1[Z] - P3[Z]) < (P1[Z] - t) * (P1[X] - P3[X]))
      {
        return(false);
      }

      return(true);

    case Z:

      s = Ray->Initial[X] + *Depth * Ray->Direction[X];
      t = Ray->Initial[Y] + *Depth * Ray->Direction[Y];

      if ((P2[X] - s) * (P2[Y] - P1[Y]) < (P2[Y] - t) * (P2[X] - P1[X]))
      {
        return(false);
      }

      if ((P3[X] - s) * (P3[Y] - P2[Y]) < (P3[Y] - t) * (P3[X] - P2[X]))
      {
        return(false);
      }

      if ((P1[X] - s) * (P1[Y] - P3[Y]) < (P1[Y] - t) * (P1[X] - P3[X]))
      {
        return(false);
      }

      return(true);
  }

  return(false);
}

/*
 *  MeshUV - By Xander Enzmann
 *
 *  Currently unused
 *
 */

const DBL BARY_VAL1 = -0.00001;
const DBL BARY_VAL2 =  1.00001;

static void MeshUV(VECTOR P, MESH_TRIANGLE *Triangle, MESH *Mesh, UV_VECT Result)
{
	DBL a, b, r;
	VECTOR Q, B[3], IB[3], P1, P2, P3;
	UV_VECT UV1, UV2, UV3;
	
	get_triangle_vertices(Mesh, Triangle, P1, P2, P3);
	VSub(B[0], P2, P1);
		VSub(B[1], P3, P1);
		Assign_Vector(B[2], Mesh->Data->Normals[Triangle->Normal_Ind]);
	
	if (!MInvers3(B, IB)) {
		// Failed to invert - that means this is a degenerate triangle
		Result[U] = P[X];
		Result[V] = P[Y];
		return;
	}
	
	VSub(Q, P, P1);
	VDot(a, Q, IB[0]);
	VDot(b, Q, IB[1]);
	
	if (a < BARY_VAL1 || b < BARY_VAL1 || a + b > BARY_VAL2)
	{
		// The use of BARY_VAL1 is an attempt to compensate for the
		//  lack of precision in the floating point numbers used in
		//  the matrices B and IB.  Since floats only have around
		//  7 digits of precision, we make sure that we allow any
		//  slop in a and b that is less than that. */
		Result[U] = P[X];
		Result[V] = P[Y];
		return;
	}
	
	r = 1.0f - a - b;
	
	get_triangle_uvcoords(Mesh, Triangle, UV1, UV2, UV3);
	VScale(Q, UV1, r);
	VAddScaledEq(Q, a, UV2);
	VAddScaledEq(Q, b, UV3);
	
	Result[U] = Q[U];
	Result[V] = Q[V];
}


/*****************************************************************************
*
* FUNCTION
*
*   test_hit
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
*   Test if a hit is valid and push if on the intersection depth.
*
* CHANGES
*
*   Feb 1995 : Creation.
*
******************************************************************************/

static int test_hit(MESH_TRIANGLE *Triangle, MESH *Mesh, RAY *OrigRay, RAY * /*Ray*/, DBL Depth, DBL len, ISTACK *Depth_Stack)
{
  VECTOR IPoint;
  OBJECT *Object = (OBJECT *)Mesh;
  DBL world_dist = Depth / len;
	
  VEvaluateRay(IPoint, OrigRay->Initial, world_dist, OrigRay->Direction);
	
  if (Point_In_Clip(IPoint, Object->Clip))
  {
    /*
    don't bother calling MeshUV because we reclaculate it later (if needed) anyway
    UV_VECT uv;
    VECTOR P; // Object coordinates of intersection
    VEvaluateRay(P, Ray->Initial, Depth, Ray->Direction);
    
    MeshUV(P, Triangle, Mesh, uv);  

    push_entry_pointer_uv(world_dist, IPoint, uv, Object, Triangle, Depth_Stack);
    */
    
		push_entry_pointer(world_dist, IPoint, Object, Triangle, Depth_Stack);
    return(true);
  }

  return(false);
}



/*****************************************************************************
*
* FUNCTION
*
*   Init_Mesh_Triangle
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
*   Feb 1995 : Creation.
*
******************************************************************************/

void Init_Mesh_Triangle(MESH_TRIANGLE *Triangle)
{
  Triangle->Smooth = false;
  Triangle->ThreeTex = false;
  Triangle->Dominant_Axis = 0;
  Triangle->vAxis         = 0;

  Triangle->P1 =
  Triangle->P2 =
  Triangle->P3 = -1;

  Triangle->Normal_Ind = -1;
  Triangle->Texture2 =
  Triangle->Texture3 = -1;

  Triangle->Texture = -1;

  Triangle->N1 =
  Triangle->N2 =
  Triangle->N3 = -1;

  Triangle->UV1 =
  Triangle->UV2 =
  Triangle->UV3 = -1;

  Make_Vector(Triangle->Perp, 0.0, 0.0, 0.0);

  Triangle->Distance = 0.0;
}



/*****************************************************************************
*
* FUNCTION
*
*   get_triangle_bbox
*
* INPUT
*
*   Triangle - Pointer to triangle
*   
* OUTPUT
*
*   BBox     - Bounding box
*   
* RETURNS
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Calculate the bounding box of a triangle.
*
* CHANGES
*
*   Sep 1994 : Creation.
*
******************************************************************************/

static void get_triangle_bbox(MESH *Mesh, MESH_TRIANGLE *Triangle, BBOX *BBox)
{
  VECTOR P1, P2, P3;
  VECTOR Min, Max;

  get_triangle_vertices(Mesh, Triangle, P1, P2, P3);

  Min[X] = min3(P1[X], P2[X], P3[X]);
  Min[Y] = min3(P1[Y], P2[Y], P3[Y]);
  Min[Z] = min3(P1[Z], P2[Z], P3[Z]);

  Max[X] = max3(P1[X], P2[X], P3[X]);
  Max[Y] = max3(P1[Y], P2[Y], P3[Y]);
  Max[Z] = max3(P1[Z], P2[Z], P3[Z]);

  Make_BBox_from_min_max(*BBox, Min, Max);
}



/*****************************************************************************
*
* FUNCTION
*
*   Build_Mesh_BBox_Tree
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
*   Create the bounding box hierarchy.
*
* CHANGES
*
*   Feb 1995 : Creation. (Derived from the bounding slab creation code)
*
******************************************************************************/

void Build_Mesh_BBox_Tree(MESH *Mesh)
{
  int i, nElem, maxelements;
  BBOX_TREE **Triangles;

  if (!Test_Flag(Mesh, HIERARCHY_FLAG))
  {
    return;
  }

  nElem = (int)Mesh->Data->Number_Of_Triangles;

  maxelements = 2 * nElem;

  /* Now allocate an array to hold references to these elements. */

  Triangles = (BBOX_TREE **)POV_MALLOC(maxelements*sizeof(BBOX_TREE *), "mesh bbox tree");

  /* Init list with mesh elements. */

  for (i = 0; i < nElem; i++)
  {
    Triangles[i] = (BBOX_TREE *)POV_MALLOC(sizeof(BBOX_TREE), "mesh bbox tree");

    Triangles[i]->Infinite = false;
    Triangles[i]->Entries  = 0;
    Triangles[i]->Node     = (BBOX_TREE **)&Mesh->Data->Triangles[i];

    get_triangle_bbox(Mesh, &Mesh->Data->Triangles[i], &Triangles[i]->BBox);
  }

  Build_BBox_Tree(&Mesh->Data->Tree, nElem, Triangles, 0, NULL);

  /* Get rid of the Triangles array. */

  POV_FREE(Triangles);
}



/*****************************************************************************
*
* FUNCTION
*
*   intersect_bbox_tree
*
* INPUT
*
*   Mesh     - Mesh object
*   Ray      - Current ray
*   Orig_Ray - Original, untransformed ray
*   len      - Length of the transformed ray direction
*   
* OUTPUT
*
*   Depth_Stack - Stack of intersections
*   
* RETURNS
*
*   int - true if an intersection was found
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Intersect a ray with the bounding box tree of a mesh.
*
* CHANGES
*
*   Feb 1995 : Creation.
*
******************************************************************************/

static int intersect_bbox_tree(MESH *Mesh, RAY *Ray, RAY  *Orig_Ray, DBL len, ISTACK *Depth_Stack)
{
  int i, found;
  DBL Best, Depth;
  RAYINFO rayinfo;
  BBOX_TREE *Node, *Root;
   short OldStyle= Mesh->has_inside_vector;

  /* Create the direction vectors for this ray. */

  Create_Rayinfo(Ray, &rayinfo);

  /* Start with an empty priority queue. */

  found = 0;

  Mesh_Queue->QSize = 0;

  Best = BOUND_HUGE;

#ifdef BBOX_EXTRA_STATS
  Increase_Counter(stats[totalQueueResets]);
#endif

  /* Check top node. */

  Root = Mesh->Data->Tree;

  /* Set the root object infinite to avoid a test. */

  Check_And_Enqueue(Mesh_Queue, Root, &Root->BBox, &rayinfo);

  /* Check elements in the priority queue. */

  while (Mesh_Queue->QSize > 0)
  {
    Priority_Queue_Delete(Mesh_Queue, &Depth, &Node);

    /*
     * If current intersection is larger than the best intersection found
     * so far our task is finished, because all other bounding boxes in
     * the priority queue are further away.
     */

    /* NK 1999 - had to comment this out for use with CSG
    if (Depth > Best)
    {
      break;
    }
    */
   	 if ( !OldStyle && Depth > Best)
      	break;

    /* Check current node. */

    if (Node->Entries)
    {
      /* This is a node containing leaves to be checked. */

      for (i = 0; i < Node->Entries; i++)
      {
        Check_And_Enqueue(Mesh_Queue, Node->Node[i], &Node->Node[i]->BBox, &rayinfo);
      }
    }
    else
    {
      /* This is a leaf so test the contained triangle. */

      if (intersect_mesh_triangle(Ray, Mesh, (MESH_TRIANGLE *)Node->Node, &Depth))
      {
        if (test_hit((MESH_TRIANGLE *)Node->Node, Mesh, Orig_Ray, Ray, Depth, len, Depth_Stack))
        {
          found = true;

          Best = Depth;
        }
      }
    }
  }

  return(found);
}



/*****************************************************************************
*
* FUNCTION
*
*   mesh_hash
*
* INPUT
*
*   aPoint - Normal/Vertex to store
*   
* OUTPUT
*
*   Hash_Table - Normal/Vertex hash table
*   Number     - Number of normals/vertices
*   Max        - Max. number of normals/vertices
*   Elements   - List of normals/vertices
*   
* RETURNS
*
*   int - Index of normal/vertex into the normals/vertices list
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Try to locate a triangle normal/vertex in the normal/vertex list.
*   If the vertex is not found its stored in the normal/vertex list.
*
* CHANGES
*
*   Feb 1995 : Creation. (With help from Steve Anger's RAW2POV code)
*
******************************************************************************/

static int mesh_hash(HASH_TABLE **Hash_Table, int *Number, int  *Max, SNGL_VECT **Elements, VECTOR aPoint)
{
  int hash;
  SNGL_VECT D, P;
  HASH_TABLE *p;

  Assign_Vector(P, aPoint);

  /* Get hash value. */

  hash = (unsigned)((int)(326.0*P[X])^(int)(694.7*P[Y])^(int)(1423.6*P[Z])) % HASH_SIZE;

  /* Try to find normal/vertex. */

  for (p = Hash_Table[hash]; p != NULL; p = p->Next)
  {
    VSub(D, p->P, P);

    if ((fabs(D[X]) < EPSILON) && (fabs(D[Y]) < EPSILON) && (fabs(D[Z]) < EPSILON))
    {
      break;
    }
  }

  if ((p != NULL) && (p->Index >= 0))
  {
    return(p->Index);
  }

  /* Add new normal/vertex to the list and hash table. */

  if ((*Number) >= (*Max))
  {
    if ((*Max) >= INT_MAX/2)
    {
      Error("Too many normals/vertices in mesh.");
    }

    (*Max) *= 2;

    (*Elements) = (SNGL_VECT *)POV_REALLOC((*Elements), (*Max)*sizeof(SNGL_VECT), "mesh data");
  }

  Assign_Vector((*Elements)[*Number], P);

  p = (HASH_TABLE *)POV_MALLOC(sizeof(HASH_TABLE), "mesh data");

  Assign_Vector(p->P, P);

  p->Index = *Number;

  p->Next = Hash_Table[hash];

  Hash_Table[hash] = p;

  return((*Number)++);
}



/*****************************************************************************
*
* FUNCTION
*
*   Mesh_Hash_Vertex
*
* INPUT
*
*   Vertex - Vertex to store
*   
* OUTPUT
*
*   Number_Of_Vertices - Number of vertices
*   Max_Vertices       - Max. number of vertices
*   Vertices           - List of vertices
*   
* RETURNS
*
*   int - Index of vertex into the vertices list
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Try to locate a triangle vertex in the vertex list.
*   If the vertex is not found its stored in the vertex list.
*
* CHANGES
*
*   Feb 1995 : Creation. (With help from Steve Anger's RAW2POV code)
*
******************************************************************************/

int Mesh_Hash_Vertex(int *Number_Of_Vertices, int  *Max_Vertices, SNGL_VECT **Vertices, VECTOR Vertex)
{
  return(mesh_hash(Vertex_Hash_Table, Number_Of_Vertices, Max_Vertices, Vertices, Vertex));
}



/*****************************************************************************
*
* FUNCTION
*
*   Mesh_Hash_Normal
*
* INPUT
*
*   Normal - Normal to store
*   
* OUTPUT
*
*   Number_Of_Normals - Number of normals
*   Max_Normals       - Max. number of normals
*   Normals           - List of normals
*   
* RETURNS
*
*   int - Index of normal into the normals list
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Try to locate a triangle normal in the normal list.
*   If the normal is not found its stored in the normal list.
*
* CHANGES
*
*   Feb 1995 : Creation. (With help from Steve Anger's RAW2POV code)
*
******************************************************************************/

int Mesh_Hash_Normal(int *Number_Of_Normals, int  *Max_Normals, SNGL_VECT **Normals, VECTOR S_Normal)
{
  return(mesh_hash(Normal_Hash_Table, Number_Of_Normals, Max_Normals, Normals, S_Normal));
}



/*****************************************************************************
*
* FUNCTION
*
*   Mesh_Hash_Texture
*
* INPUT
*
*   Texture - Texture to store
*   
* OUTPUT
*
*   Number_Of_Textures - Number of textures
*   Max_Textures       - Max. number of textures
*   Textures           - List of textures
*   
* RETURNS
*
*   int - Index of texture into the texture list
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Try to locate a texture in the texture list.
*   If the texture is not found its stored in the texture list.
*
* CHANGES
*
*   Feb 1995 : Creation.
*
******************************************************************************/

int Mesh_Hash_Texture(int *Number_Of_Textures, int  *Max_Textures, TEXTURE ***Textures, TEXTURE  *Texture)
{
  int i;

  if (Texture == NULL)
  {
    return(-1);
  }

  /* Just do a linear search. */

  for (i = 0; i < *Number_Of_Textures; i++)
  {
    if ((*Textures)[i] == Texture)
    {
      break;
    }
  }

  if (i == *Number_Of_Textures)
  {
    if ((*Number_Of_Textures) >= (*Max_Textures))
    {
      if ((*Max_Textures) >= INT_MAX/2)
      {
        Error("Too many textures in mesh.");
      }

      (*Max_Textures) *= 2;

      (*Textures) = (TEXTURE **)POV_REALLOC((*Textures), (*Max_Textures)*sizeof(TEXTURE *), "mesh data");
    }

    (*Textures)[(*Number_Of_Textures)++] = Copy_Texture_Pointer(Texture);
  }

  return(i);
}

/*****************************************************************************
*
* FUNCTION
*
*   Mesh_Hash_UV
*
* INPUT
*
*   aPoint - UV vector to store
*
* OUTPUT
*
*   Hash_Table - UV vector hash table
*   Number     - Number of UV vectors
*   Max        - Max. number of UV vectors
*   Elements   - List of UV vectors
*
* RETURNS
*
*   int - Index of UV vector into the UV vector list
*
* AUTHOR
*
*   Dieter Bayer / Nathan Kopp
*
* DESCRIPTION
*
*   adapted from mesh_hash
*
* CHANGES
*
*
******************************************************************************/

int Mesh_Hash_UV(int *Number, int *Max, UV_VECT **Elements, UV_VECT aPoint)
{
  int hash;
  UV_VECT D, P;
  UV_HASH_TABLE *p;

  Assign_UV_Vect(P, aPoint);

  /* Get hash value. */

  hash = (unsigned)((int)(326.0*P[U])^(int)(694.7*P[V])) % HASH_SIZE;

  /* Try to find normal/vertex. */

  for (p = UV_Hash_Table[hash]; p != NULL; p = p->Next)
  {
    /* VSub(D, p->P, P); */
    D[U] = p->P[U] - P[U];
    D[V] = p->P[V] - P[V];

    if ((fabs(D[U]) < EPSILON) && (fabs(D[V]) < EPSILON))
    {
      break;
    }
  }

  if ((p != NULL) && (p->Index >= 0))
  {
    return(p->Index);
  }

  /* Add new normal/vertex to the list and hash table. */

  if ((*Number) >= (*Max))
  {
    if ((*Max) >= INT_MAX/2)
    {
      Error("Too many normals/vertices in mesh.");
    }

    (*Max) *= 2;

    (*Elements) = (UV_VECT *)POV_REALLOC((*Elements), (*Max)*sizeof(UV_VECT), "mesh data");
  }

  Assign_UV_Vect((*Elements)[*Number], P);

  p = (UV_HASH_TABLE *)POV_MALLOC(sizeof(UV_HASH_TABLE), "mesh data");

  Assign_UV_Vect(p->P, P);

  p->Index = *Number;

  p->Next = UV_Hash_Table[hash];

  UV_Hash_Table[hash] = p;

  return((*Number)++);
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_Mesh_Hash_Tables
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
*   Feb 1995 : Creation.
*
******************************************************************************/

void Create_Mesh_Hash_Tables()
{
  int i;

  Vertex_Hash_Table = (HASH_TABLE **)POV_MALLOC(HASH_SIZE*sizeof(HASH_TABLE *), "mesh hash table");

  for (i = 0; i < HASH_SIZE; i++)
  {
    Vertex_Hash_Table[i] = NULL;
  }

  Normal_Hash_Table = (HASH_TABLE **)POV_MALLOC(HASH_SIZE*sizeof(HASH_TABLE *), "mesh hash table");

  for (i = 0; i < HASH_SIZE; i++)
  {
    Normal_Hash_Table[i] = NULL;
  }

  /* NK 1998 */
  UV_Hash_Table = (UV_HASH_TABLE **)POV_MALLOC(HASH_SIZE*sizeof(UV_HASH_TABLE *), "mesh hash table");

  for (i = 0; i < HASH_SIZE; i++)
  {
    UV_Hash_Table[i] = NULL;
  }
  /* NK ---- */
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Mesh_Hash_Tables
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
*   Feb 1995 : Creation.
*
******************************************************************************/

void Destroy_Mesh_Hash_Tables()
{
  int i;
  HASH_TABLE *Temp;
  /* NK 1998 */
  UV_HASH_TABLE *UVTemp;
  /* NK ---- */

  for (i = 0; i < HASH_SIZE; i++)
  {
    while (Vertex_Hash_Table[i] != NULL)
    {
      Temp = Vertex_Hash_Table[i];
      
      Vertex_Hash_Table[i] = Temp->Next;
      
      POV_FREE(Temp);
    }
  }

  POV_FREE(Vertex_Hash_Table);

  for (i = 0; i < HASH_SIZE; i++)
  {
    while (Normal_Hash_Table[i] != NULL)
    {
      Temp = Normal_Hash_Table[i];
      
      Normal_Hash_Table[i] = Temp->Next;
      
      POV_FREE(Temp);
    }
  }

  POV_FREE(Normal_Hash_Table);

  /* NK 1998 */
  for (i = 0; i < HASH_SIZE; i++)
  {
    while (UV_Hash_Table[i] != NULL)
    {
      UVTemp = UV_Hash_Table[i];

      UV_Hash_Table[i] = UVTemp->Next;

      POV_FREE(UVTemp);
    }
  }

  POV_FREE(UV_Hash_Table);
  /* NK ---- */
}



/*****************************************************************************
*
* FUNCTION
*
*   get_triangle_vertices
*
* INPUT
*
*   Mesh     - Mesh object
*   Triangle - Triangle
*   
* OUTPUT
*   
* RETURNS
*
*   P1, P2, P3 - Vertices of the triangle
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static void get_triangle_vertices(MESH *Mesh, MESH_TRIANGLE *Triangle, VECTOR P1, VECTOR  P2, VECTOR  P3)
{
  Assign_Vector(P1, Mesh->Data->Vertices[Triangle->P1]);
  Assign_Vector(P2, Mesh->Data->Vertices[Triangle->P2]);
  Assign_Vector(P3, Mesh->Data->Vertices[Triangle->P3]);
}



/*****************************************************************************
*
* FUNCTION
*
*   get_triangle_normals
*
* INPUT
*
*   Mesh     - Mesh object
*   Triangle - Triangle
*   
* OUTPUT
*   
* RETURNS
*
*   N1, N2, N3 - Normals of the triangle
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
*   Feb 1995 : Creation.
*
******************************************************************************/

static void get_triangle_normals(MESH *Mesh, MESH_TRIANGLE *Triangle, VECTOR N1, VECTOR  N2, VECTOR  N3)
{
  Assign_Vector(N1, Mesh->Data->Normals[Triangle->N1]);
  Assign_Vector(N2, Mesh->Data->Normals[Triangle->N2]);
  Assign_Vector(N3, Mesh->Data->Normals[Triangle->N3]);
}


/*****************************************************************************
*
* FUNCTION
*
*   get_triangle_uvcoords
*
* INPUT
*
*   Mesh     - Mesh object
*   Triangle - Triangle
*
* OUTPUT
*
* RETURNS
*
*   UV1, UV2, UV3 - UV coordinates of the triangle's corners
*
* AUTHOR
*
*   Nathan Kopp
*
* DESCRIPTION
*
*   adapted from get_triangle_normals
*
* CHANGES
*
******************************************************************************/

static void get_triangle_uvcoords(MESH *Mesh, MESH_TRIANGLE *Triangle, UV_VECT UV1, UV_VECT UV2, UV_VECT UV3)
{
  Assign_UV_Vect(UV1, Mesh->Data->UVCoords[Triangle->UV1]);
  Assign_UV_Vect(UV2, Mesh->Data->UVCoords[Triangle->UV2]);
  Assign_UV_Vect(UV3, Mesh->Data->UVCoords[Triangle->UV3]);
}


/*****************************************************************************
*
* FUNCTION
*
*   Mesh_Degenerate
*
* INPUT
*
*   P1, P2, P3 - Triangle's vertices
*   
* OUTPUT
*   
* RETURNS
*
*   int - true if degenerate
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Test if a triangle is degenerate.
*
* CHANGES
*
*   Feb 1995 : Creation.
*
******************************************************************************/

int Mesh_Degenerate(VECTOR P1, VECTOR  P2, VECTOR  P3)
{
  VECTOR V1, V2, Temp;
  DBL Length;

  VSub(V1, P1, P2);
  VSub(V2, P3, P2);

  VCross(Temp, V1, V2);

  VLength(Length, Temp);

  return(Length == 0.0);
}


/*****************************************************************************
*
* FUNCTION
*
*   Initialize_Mesh_Code
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
*   Initialize mesh specific variables.
*
* CHANGES
*
*   Jul 1995 : Creation.
*
******************************************************************************/

void Initialize_Mesh_Code()
{
  Mesh_Queue = Create_Priority_Queue(INITIAL_NUMBER_OF_ENTRIES);
}



/*****************************************************************************
*
* FUNCTION
*
*   Deinitialize_Mesh_Code
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
*   Deinitialize mesh specific variables.
*
* CHANGES
*
*   Jul 1995 : Creation.
*
******************************************************************************/

void Deinitialize_Mesh_Code()
{
  if (Mesh_Queue != NULL)
  {
    Destroy_Priority_Queue(Mesh_Queue);
  }

  Mesh_Queue = NULL;
}



/*****************************************************************************
*
* FUNCTION
*
*   Test_Mesh_Opacity
*
* INPUT
*
*   Mesh - Pointer to mesh structure
*
* OUTPUT
*
*   Mesh
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Set the opacity flag of the mesh according to the opacity
*   of the mesh's texture(s).
*
* CHANGES
*
*   Apr 1996 : Creation.
*
******************************************************************************/

void Test_Mesh_Opacity(MESH *Mesh)
{
  int i;

  /* Initialize opacity flag to the opacity of the object's texture. */

  if ((Mesh->Texture == NULL) || (Test_Opacity(Mesh->Texture)))
  {
    Set_Flag(Mesh, OPAQUE_FLAG);
  }

  if (Test_Flag(Mesh, MULTITEXTURE_FLAG))
  {
    for (i = 0; i < Mesh->Number_Of_Textures; i++)
    {
      if (Mesh->Textures[i] != NULL)
      {
        /* If component's texture isn't opaque the mesh is neither. */

        if (!Test_Opacity(Mesh->Textures[i]))
        {
          Clear_Flag(Mesh, OPAQUE_FLAG);
        }
      }
    }
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Mesh_UVCoord
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Nathan Kopp
*
* DESCRIPTION
*
*   computes the UV coordinates of the intersection for a mesh
*
******************************************************************************/

static void Mesh_UVCoord(VECTOR Result, OBJECT *Object, INTERSECTION *Inter)
{
  DBL w1, w2, w3, t1, t2;
  VECTOR vA, vB;
  VECTOR Side1, Side2;
  MESH_TRIANGLE *Triangle;
  MESH *Mesh = (MESH *)Object;
  VECTOR P;

  if (Mesh->Trans != NULL)
    MInvTransPoint(P, Inter->IPoint, Mesh->Trans);
  else
    Assign_Vector(P, Inter->IPoint);

  Triangle = (MESH_TRIANGLE *)Inter->Pointer;

  /* ---------------- this is for P1 ---------------- */
  /* Side1 is opposite side, Side2 is an adjacent side (vector pointing away) */
  VSub(Side1, Mesh->Data->Vertices[Triangle->P3], Mesh->Data->Vertices[Triangle->P2]);
  VSub(Side2, Mesh->Data->Vertices[Triangle->P3], Mesh->Data->Vertices[Triangle->P1]);

  /* find A */
  /* A is a vector from this vertex to the intersection point */
  VSub(vA, P, Mesh->Data->Vertices[Triangle->P1]);

  /* find B */
  /* B is a vector from this intersection to the opposite side (Side1) */
  /*    which is the exact length to get to the side                   */
  /* to do this we split Side2 into two components, but only keep the  */
  /*    one that's perp to Side2                                       */
  VDot(t1, Side2, Side1);
  VDot(t2, Side1, Side1);
  VScale(vB, Side1, t1/t2);
  VSubEq(vB, Side2);

  /* finding the weight is the scale part of a projection of A onto B */
  VDot(t1, vA, vB);
  VDot(t2, vB, vB);
  /* w1 = 1-fabs(t1/t2); */
  w1 = 1+t1/t2;

  /* ---------------- this is for P2 ---------------- */
  VSub(Side1, Mesh->Data->Vertices[Triangle->P3], Mesh->Data->Vertices[Triangle->P1]);
  VSub(Side2, Mesh->Data->Vertices[Triangle->P3], Mesh->Data->Vertices[Triangle->P2]);

  /* find A */
  VSub(vA, P, Mesh->Data->Vertices[Triangle->P2]);

  /* find B */
  VDot(t1, Side2, Side1);
  VDot(t2, Side1, Side1);
  VScale(vB, Side1, t1/t2);
  VSubEq(vB, Side2);

  VDot(t1, vA, vB);
  VDot(t2, vB, vB);
  /* w2 = 1-fabs(t1/t2); */
  w2 = 1+t1/t2;

  /* ---------------- this is for P3 ---------------- */
  VSub(Side1, Mesh->Data->Vertices[Triangle->P2], Mesh->Data->Vertices[Triangle->P1]);
  VSub(Side2, Mesh->Data->Vertices[Triangle->P2], Mesh->Data->Vertices[Triangle->P3]);

  /* find A */
  VSub(vA, P, Mesh->Data->Vertices[Triangle->P3]);

  /* find B */
  VDot(t1, Side2, Side1);
  VDot(t2, Side1, Side1);
  VScale(vB, Side1, t1/t2);
  VSubEq(vB, Side2);

  VDot(t1, vA, vB);
  VDot(t2, vB, vB);
  /* w3 = 1-fabs(t1/t2); */
  w3 = 1+t1/t2;

  Result[U] =  w1 * Mesh->Data->UVCoords[Triangle->UV1][U] +
               w2 * Mesh->Data->UVCoords[Triangle->UV2][U] +
               w3 * Mesh->Data->UVCoords[Triangle->UV3][U];
  Result[V] =  w1 * Mesh->Data->UVCoords[Triangle->UV1][V] +
               w2 * Mesh->Data->UVCoords[Triangle->UV2][V] +
               w3 * Mesh->Data->UVCoords[Triangle->UV3][V];
}


/*****************************************************************************
*
* FUNCTION
*
*   inside_bbox_tree
*
* INPUT
*
*   Mesh     - Mesh object
*   Ray      - Current ray
*   
* OUTPUT
*
* RETURNS
*
*   int - true if inside the object
*   
* AUTHOR
*
*   Nathan Kopp & Dieter Bayer
*   
* DESCRIPTION
*
*   Check if a point is within the bounding box tree of a mesh.
*
* CHANGES
*
*   Oct 1998 : Creation.
*
******************************************************************************/

static int inside_bbox_tree(MESH *Mesh, RAY *Ray)
{
  int i, found;
  DBL Best, Depth;
  RAYINFO rayinfo;
  BBOX_TREE *Node, *Root;

  /* Create the direction vectors for this ray. */
  Create_Rayinfo(Ray, &rayinfo);

  /* Start with an empty priority queue. */
  found = 0;

  Mesh_Queue->QSize = 0;
  Best = BOUND_HUGE;

#ifdef BBOX_EXTRA_STATS
  Increase_Counter(stats[totalQueueResets]);
#endif

  /* Check top node. */
  Root = Mesh->Data->Tree;

  /* Set the root object infinite to avoid a test. */
  Check_And_Enqueue(Mesh_Queue, Root, &Root->BBox, &rayinfo);

  /* Check elements in the priority queue. */
  while (Mesh_Queue->QSize > 0)
  {
    Priority_Queue_Delete(Mesh_Queue, &Depth, &Node);

    /* Check current node. */
    if (Node->Entries)
    {
      /* This is a node containing leaves to be checked. */
      for (i = 0; i < Node->Entries; i++)
        Check_And_Enqueue(Mesh_Queue, Node->Node[i], &Node->Node[i]->BBox, &rayinfo);
    }
    else
    {
      /* This is a leaf so test the contained triangle. */

      if (intersect_mesh_triangle(Ray, Mesh, (MESH_TRIANGLE *)Node->Node, &Depth))
      {
			/* actually, this should push onto a local depth stack and
			   make sure that we don't have the same intersection point from
			   two (or three) different triangles!!!!! */
          found++;
      }
    }
  }

	/* odd number = inside, even number = outside */
	return(found & 1);
}


/* AP and NK */

/*
  Determine the weights for interpolation of the three vertex textures of 
  mesh-triangle tri at position IPoint. If mesh-triangle is not a 
  smooth_texture_triange, ignore interpolation and return false
*/

int Mesh_Interpolate(VECTOR Weights, VECTOR IPoint, MESH *m, MESH_TRIANGLE *tri)
{
  VECTOR P1,P2,P3;
  DBL wsum;
  VECTOR EPoint;

  if (m->Trans != NULL)
  {
    MInvTransPoint(EPoint, IPoint, m->Trans);
  }
  else
  {
    Assign_Vector(EPoint, IPoint);
  }

  if(tri->ThreeTex) 
  {
    /*if(tri->C1==-1 ||
       tri->C2==-1 ||
       tri->C3==-1)
      return false;*/

    P1[0]=m->Data->Vertices[tri->P1][0];
    P1[1]=m->Data->Vertices[tri->P1][1];
    P1[2]=m->Data->Vertices[tri->P1][2];
    P2[0]=m->Data->Vertices[tri->P2][0];
    P2[1]=m->Data->Vertices[tri->P2][1];
    P2[2]=m->Data->Vertices[tri->P2][2];
    P3[0]=m->Data->Vertices[tri->P3][0];
    P3[1]=m->Data->Vertices[tri->P3][1];
    P3[2]=m->Data->Vertices[tri->P3][2];

    Weights[0]=1.0-Calculate_Smooth_T(EPoint, P1, P2, P3);
    Weights[1]=1.0-Calculate_Smooth_T(EPoint, P2, P3, P1);
    Weights[2]=1.0-Calculate_Smooth_T(EPoint, P3, P1, P2);

    wsum = Weights[0]+Weights[1]+Weights[2];
    VScaleEq(Weights, 1.0/wsum);

    return true;
  } 
  else 
  {
    return false;
  }
}

END_POV_NAMESPACE
