/****************************************************************************
 *                  radiosit.h
 *
 * Include file for radiosit.cpp, Radiosity calculation routies.
 *
 * Implemented by and (c) 1994 Jim McElhiney, mcelhiney@acm.org or 71201,1326
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
 * $File: //depot/povray/3.6-release/source/radiosit.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef RADIOSIT_H
#define RADIOSIT_H

BEGIN_POV_NAMESPACE

#define RADIOSITY_CACHE_EXTENSION ".rca"

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

/* #define RADDEBUG 1 */

#define MAX_NEAREST_COUNT 20




/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct wt_avg_struct WT_AVG;

/* quickie structure used to gather weighted average during tree traversal */
struct wt_avg_struct
{
  COLOUR Weights_Times_Illuminances; /* Aggregates during traversal */
  DBL    Weights;   /* Aggregates during traversal */
  int    Weights_Count;    /* Count of points used, aggregates during trav */
  int    Good_Count;    /* Count of points used, aggregates during trav */
  VECTOR P, N;     /* Point and Normal:  input to traverse */
  DBL    Current_Error_Bound;   /* see Radiosity_Error_Bound */

  COLOUR Weight_Times_Illuminance[MAX_NEAREST_COUNT];
  DBL    Weight[MAX_NEAREST_COUNT];
  DBL    Distance[MAX_NEAREST_COUNT];
  int    Close_Count;
};

typedef struct byte_xyz BYTE_XYZ;

struct byte_xyz {
  unsigned char x, y, z;
};


/*****************************************************************************
* Global variables
******************************************************************************/

extern long ra_reuse_count;
extern long ra_gather_count;
extern OT_NODE *ot_root;
extern OStream *ot_fd;
extern COLOUR Radiosity_Gather_Total;
extern long Radiosity_Gather_Total_Count;
extern int Radiosity_Trace_Level;
extern const BYTE_XYZ rad_samples[];

/*****************************************************************************
* External variables
******************************************************************************/

extern int firstRadiosityPass;

/*****************************************************************************
* Global functions
******************************************************************************/

/* NK rad 22 Nov 1999 - added L_Normal */
int Compute_Ambient (VECTOR IPoint, VECTOR Raw_Normal, VECTOR LayNormal, COLOUR Ambient_Colour, DBL Weight);
bool Initialize_Radiosity_Code (void);
bool Deinitialize_Radiosity_Code (void);

END_POV_NAMESPACE

#endif
