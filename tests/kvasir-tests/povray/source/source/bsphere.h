/****************************************************************************
 *                  bsphere.h
 *
 * This module contains all defines, typedefs, and prototypes for BSPHERE.CPP.
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
 * $File: //depot/povray/3.6-release/source/bsphere.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef BSPHERE_H
#define BSPHERE_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/



/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct BSphere_Tree_Struct BSPHERE_TREE;

struct BSphere_Tree_Struct
{
  short Entries;       /* Number of components (node if 0)    */
  VECTOR C;            /* Center of bounding sphere           */
  DBL r2;              /* Radius^2 of bounding sphere         */
  BSPHERE_TREE **Node; /* if node: children; if leaf: element */
};



/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

void Build_Bounding_Sphere_Hierarchy (BSPHERE_TREE **Root, int nElem, BSPHERE_TREE ***Elements);
void Destroy_Bounding_Sphere_Hierarchy (BSPHERE_TREE *Node);

END_POV_NAMESPACE

#endif
