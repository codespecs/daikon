/****************************************************************************
 *                  blob.h
 *
 * This module contains all defines, typedefs, and prototypes for BLOB.CPP.
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
 * $File: //depot/povray/3.6-release/source/blob.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef BLOB_H
#define BLOB_H

#include "bsphere.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define BLOB_OBJECT (STURM_OK_OBJECT+HIERARCHY_OK_OBJECT)

/* Do not use the first bit!!! (Used for enter/exit in intersection test) */

#define BLOB_SPHERE               2
#define BLOB_CYLINDER             4
#define BLOB_ELLIPSOID            8
#define BLOB_BASE_HEMISPHERE     16
#define BLOB_APEX_HEMISPHERE     32
#define BLOB_BASE_HEMIELLIPSOID  64
#define BLOB_APEX_HEMIELLIPSOID 128


/* Define max. number of blob components. */

#define MAX_BLOB_COMPONENTS 1000000

/* Generate additional blob statistics. */

#define BLOB_EXTRA_STATS 1



/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Blob_Struct BLOB;
typedef struct Blob_Element_Struct BLOB_ELEMENT;
typedef struct Blob_Data_Struct BLOB_DATA;
typedef struct Blob_List_Struct BLOB_LIST;
typedef struct Blob_Interval_Struct BLOB_INTERVAL;

struct Blob_Element_Struct
{
  short Type;       /* Type of component: sphere, hemisphere, cylinder */
  int index;
  VECTOR O;         /* Element's origin                                */
  DBL len;          /* Cylinder's length                               */
  DBL rad2;         /* Sphere's/Cylinder's radius^2                    */
  DBL c[3];         /* Component's coeffs                              */
  DBL f[5];         /* Component's final coeffs                        */
  TEXTURE *Texture; /* Component's texture                             */
  TRANSFORM *Trans; /* Component's transformation                      */
};

struct Blob_Data_Struct
{
  int References;           /* Number of references     */
  int Number_Of_Components; /* Number of components     */
  DBL Threshold;            /* Blob threshold           */
  BLOB_ELEMENT *Entry;      /* Array of blob components */
  BLOB_INTERVAL* Intervals; /* Intervals used during intersection testing */
  BSPHERE_TREE *Tree;       /* Bounding hierarchy       */
};

struct Blob_Struct
{
  OBJECT_FIELDS
  BLOB_DATA *Data;    /* Pointer to blob data  */
  TEXTURE **Element_Texture;
  BSPHERE_TREE **Queue;
  unsigned int Max_Queue_Size;
};

struct Blob_List_Struct
{
  BLOB_ELEMENT elem;  /* Current element          */
  BLOB_LIST *next;    /* Pointer to next element  */
};

struct Blob_Interval_Struct
{
  int type;
  DBL bound;
  BLOB_ELEMENT *Element;
};



/*****************************************************************************
* Global variables
******************************************************************************/

extern METHODS Blob_Methods;



/*****************************************************************************
* Global functions
******************************************************************************/

BLOB *Create_Blob (void);
void Make_Blob (BLOB *blob, DBL threshold, BLOB_LIST *bloblist, int npoints);
BLOB_LIST *Create_Blob_List_Element (void);
void Create_Blob_Element_Texture_List (BLOB *Blob, BLOB_LIST *BlobList, int npoints);
void Determine_Blob_Textures (BLOB *Blob, VECTOR P, int *Count, TEXTURE **Textures, DBL *Weights);
void Test_Blob_Opacity (BLOB *Blob);

void Translate_Blob_Element (BLOB_ELEMENT *Element, VECTOR Vector);
void Rotate_Blob_Element (BLOB_ELEMENT *Element, VECTOR Vector);
void Scale_Blob_Element (BLOB_ELEMENT *Element, VECTOR Vector);
void Invert_Blob_Element (BLOB_ELEMENT *Element);
void Transform_Blob_Element (BLOB_ELEMENT *Element, TRANSFORM *Trans);

END_POV_NAMESPACE

#endif
