/****************************************************************************
 *                  octree.h
 *
 * Oct-tree routine prototypes.  Use by Radiosity calculation routies.
 *
 * Implemented by and (c) 1994 Jim McElhiney, mcelhiney@acm.org or cserve 71201,1326
 * All standard POV distribution rights granted.  All other rights reserved.
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
 * $File: //depot/povray/3.6-release/source/octree.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef OCTREE_H
#define OCTREE_H

BEGIN_POV_NAMESPACE

USING_POV_BASE_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define OT_BIAS 10000000.

#define MAX3(a,b,c) ( ((a)>(b)) ? max((a),(c)) : max((b),(c)) )


/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct ot_block_struct OT_BLOCK;
typedef struct ot_id_struct OT_ID;
typedef struct ot_node_struct OT_NODE;

/* Each node in the oct-tree has a (possibly null) linked list of these
   data blocks off it.  */
struct ot_block_struct
{
  OT_BLOCK *next;
  VECTOR Point, S_Normal;
  float  drdx, dgdx, dbdx,  drdy, dgdy, dbdy,  drdz, dgdz, dbdz;
  RGB    Illuminance;
  float  Harmonic_Mean_Distance, Nearest_Distance;
  VECTOR To_Nearest_Surface;
  short  Bounce_Depth;
};

/* This is the information necessary to name an oct-tree node. */
struct ot_id_struct
{
  int x, y, z;
  int Size;
};

/* These are the structures that make up the oct-tree itself, known as nodes */
struct ot_node_struct
{
  OT_ID    Id;
  OT_BLOCK *Values;
  OT_NODE  *Kids[8];
};


/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

void ot_ins (OT_NODE **root, OT_BLOCK *new_block, OT_ID *new_id);
void ot_list_insert (OT_BLOCK **list_ptr, OT_BLOCK *item);
void ot_newroot (OT_NODE **root_ptr);
bool ot_dist_traverse (OT_NODE *subtree, VECTOR point, int bounce_depth,  \
               int (*func)(OT_BLOCK *block, void *handle1), void *handle2);
int ot_point_in_node (VECTOR point, OT_ID *node);
void ot_index_sphere (VECTOR point, DBL radius, OT_ID *id);
void ot_index_box (VECTOR min_point, VECTOR max_point, OT_ID *id);
void ot_parent (OT_ID *dad, OT_ID *kid);
bool ot_save_tree (OT_NODE *rootptr, OStream *fd);
bool ot_write_block (OT_BLOCK *bl, void * handle);
bool ot_free_tree (OT_NODE **ppRoot);
bool ot_read_file (IStream * fd);


/* a trunc function which always returns the floor integer */
int Trunc (DBL value);

END_POV_NAMESPACE

#endif
