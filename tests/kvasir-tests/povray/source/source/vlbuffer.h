/****************************************************************************
 *                  vlbuffer.h
 *
 * This module contains all defines, typedefs, and prototypes for VLBUFFER.CPP.
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
 * $File: //depot/povray/3.6-release/source/vlbuffer.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef VLBUFFER_H
#define VLBUFFER_H

#include "frame.h"
#include "bbox.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

/* flag to mark a node as pruned */

#define PRUNE_CHECK 128
#define PRUNE_TEMPORARY 128



/* Define minimum and maximum values for buffer coordinates. */

#define MIN_BUFFER_ENTRY -32000
#define MAX_BUFFER_ENTRY  32000



/* Define maximum number of clippoints. */

#define MAX_CLIP_POINTS 20



/* Define all six coordinate axes. */

#define XaxisP 0
#define XaxisM 1
#define YaxisP 2
#define YaxisM 3
#define ZaxisP 4
#define ZaxisM 5



/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Project_Tree_Leaf_Struct PROJECT_TREE_LEAF;
typedef struct Project_Queue_Struct PROJECT_QUEUE;

struct Project_Tree_Leaf_Struct
{
  unsigned short is_leaf;
  BBOX_TREE *Node;
  PROJECT Project;
};

struct Project_Queue_Struct
{
  unsigned QSize;
  unsigned Max_QSize;
  PROJECT_TREE_NODE **Queue;
};




/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

void Clip_Polygon (VECTOR *Points, int *PointCnt, const VECTOR VX1, const VECTOR VX2, const VECTOR VY1, const VECTOR VY2, const DBL DX1, const DBL DX2, const DBL DY1, const DBL DY2);

void Initialize_VLBuffer_Code (void);
void Reinitialize_VLBuffer_Code (void);
void Deinitialize_VLBuffer_Code (void);

void Destroy_Project_Tree (PROJECT_TREE_NODE *Node);

END_POV_NAMESPACE

#endif
