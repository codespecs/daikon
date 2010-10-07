/****************************************************************************
 *               photons.h
 *
 * Author: Nathan Kopp
 *
 * This module contains all defines, typedefs, and prototypes for photons.cpp.
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
 * $File: //depot/povray/3.6-release/source/photons.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef PHOTONS_H
#define PHOTONS_H

#include "point.h"
#include "colutils.h"

BEGIN_POV_NAMESPACE

#define MEDIA_INTERACTION 1

/* this is for photon block allocation and array mapping functions */
#define PHOTON_BLOCK_POWER 14
/* PHOTON_BLOCK_SIZE must be equal to 2 raised to the power PHOTON_BLOCK_POWER */
#define PHOTON_BLOCK_SIZE (16384)
#define PHOTON_BLOCK_MASK (PHOTON_BLOCK_SIZE-1)
#define INITIAL_BASE_ARRAY_SIZE 100

/* ------------------------------------------------------ */
/* photon */
/* ------------------------------------------------------ */
typedef struct photon_struct PHOTON;

struct photon_struct {
  SNGL_VECT Loc;          /* location */
  SMALL_COLOUR Colour;    /* color & intensity (flux) */
  unsigned char info;     /* info byte for kd-tree */
  signed char theta, phi; /* incoming direction */
};

/* ------------------------------------------------------ */
/* photon memory allocation stuff */
/* ------------------------------------------------------ */
typedef PHOTON *PHOTON_BLOCK;

/* ------------------------------------------------------ */
/* These are masks for the info byte - they were intended for
   a threaded kd-tree (which turned out to be a failure.
   They will be removed when I can make sure that they're not
   needed anymore */
#define PH_MASK_XYZ 7  /* 0000 0011 */
#define PH_MASK_RT  8  /* 0000 0100 */
#define PH_MASK_LT  16 /* 0000 1000 */

/* initialize the photon */
#define INIT_PHOTON(ph, d)     ph->info=d;

/*
  Photon array mapping function

  This converts a one-dimensional index into a two-dimensional address
  in the photon array.

  Photon memory looks like this:

    # -> **********
    # -> **********  <- blocks of photons
    # -> **********
    # -> /
    # -> /
    :
    ^
    |
    Base array.

  The base array (which is dynamically resized as needed) contians pointers
  to blocks of photons.  Blocks of photons (of fixed size of a power of two)
  are allocated as needed.

  This mapping function converts a one-dimensional index and into a two-
  dimensional address consisting of a block index and an offset within
  that block.
  
  Note that, while this data structure allows easy allocation of photons,
  it does not easily permit deallocation of photons.  Once photons are placed
  into the photon map, they are not removed.
*/
/* if this is changed, you must also change swapPhotons() and
   allocatePhoton, both in photons.c */
#define PHOTON_AMF(map, idx)   map[(idx)>>PHOTON_BLOCK_POWER][(idx) & PHOTON_BLOCK_MASK]

/* ------------------------------------------------------ */
typedef struct photon_map_struct PHOTON_MAP;

struct photon_map_struct {
  /* these 3 are render-thread safe - NOT pre-process thread safe */
  PHOTON_BLOCK *head;   /* the photon map - array of blocks of photons */
  int numBlocks;        /* number of blocks in base array */
  int numPhotons;       /* total number of photons used */

  DBL minGatherRad;       /* minimum gather radius */
  DBL minGatherRadMult;   /* minimum gather radius multiplier (for speed adjustments) */
  DBL gatherRadStep;      /* step size for gather expansion */
  int gatherNumSteps;     /* maximum times to perform 'gather' */
};

/* ------------------------------------------------------ */
typedef struct photon_options_struct PHOTON_OPTIONS;

struct photon_options_struct {
  /* options */
  /* these scene options are thread safe */
  int photonsEnabled;     /* are photons enabled? */

  DBL surfaceSeparation;  /* surface photon separation */
  DBL globalSeparation;   /* global photon separation */

  DBL expandTolerance; /* see paper for explanation */
  int minExpandCount;     /* see paper for explanation */

  int Max_Trace_Level;    /* trace level for light-ray tracing step */
  DBL ADC_Bailout;        /* adc bailout for light-ray tracing step */

  DBL jitter;             /* jitter amount */
  DBL autoStopPercent;    /* percent at which to start using autostop feature */

  int minGatherCount;     /* minimum number of photons to gather */
  int maxGatherCount;     /* maximum number to gather (size of priority queue) */

  char* fileName;         /* file name to load or save caustic photon map */
  int saveFile;           /* do we save our photon map? (if not, we should load) */
  int loadFile;           /* do we load instead of create? */
                          /* load and save are mutually exculsive */

  /* dynamic variables */
  /* not thread safe */
  /* these 3 are render-thread safe - NOT pre-process thread safe */
  PHOTON_MAP photonMap; /* the photon map - array of blocks of photons */

  /* not thread safe */ /* these 4 are not thread safe */
  int hitObject;           /* did we hit the target object? (for autostop) */
  DBL photonSpread;        /* photon spread (in radians) */
  DBL photonDepth;         /* total distance from light to intersection */
  OBJECT *photonObject;    /* object that we're shooting photons at.. NULL if global */

  /* speed optimization data - sin/cos stored in two arrays
      these are only created if photon mapping is used
  */
  /* these are thread safe - used many times but not modified after initialization */
  DBL *cosTheta;
  DBL *sinTheta;

  /* global priority queue arrays used to conserve stack space */
  /* not thread safe */ /* these 2 are not thread safe */
  PHOTON **photonGatherList;  /* photons */
  DBL *photonDistances;       /* priorities */

  /* the following variables are put here to avoid parameter passing to
     conserve stack space */
  /* not thread safe */ /* these 5 are not thread safe */
  int passThruThis;           /* is this a pass-through object? */
  int passThruPrev;           /* was the previous object pass-through? */
  unsigned int lightFlags;   /* photon flags for the current light source */
  unsigned int objectFlags;  /* photon flags for the current object */
  LIGHT_SOURCE *Light;        /* the current light */
  bool Light_Is_Global;       /* is the current light global? (not part of a light_group? */

#ifdef GLOBAL_PHOTONS
  /* ---------- global photon map ----------*/
  PHOTON_MAP globalPhotonMap; /* the photon map - array of blocks of photons */
  int globalPhotonsToShoot;      /* number of global photons to shoot */
  DBL globalGatherRad;           /* minimum gather radius */
#endif

  /* media photons */
  /* mediaPhotonMap is render-thread safe - NOT pre-process thread safe */
  PHOTON_MAP mediaPhotonMap; /* the photon map - array of blocks of photons */

  DBL mediaSpacingFactor;
  int maxMediaSteps;

  int surfaceCount;
  int globalCount;

  int photonReflectionBlur;
};

/* ------------------------------------------------------ */
/* global functions */
/* for documentation of these functions, see photons.c */
/* ------------------------------------------------------ */
void CheckPassThru(OBJECT *o, int flag);
void BuildPhotonMaps(void);
void InitBacktraceEverything(void);
void FreeBacktraceEverything(void);
void addSurfacePhoton(VECTOR Point, VECTOR Origin, COLOUR LightCol, VECTOR RawNorm);
void addMediaPhoton(VECTOR Point, VECTOR Origin, COLOUR LightCol, DBL depthDiff);
int gatherPhotons(VECTOR pt, DBL Size, DBL *r, VECTOR norm, int flatten, PHOTON_MAP *map);

void ChooseRay(RAY *NewRay, VECTOR Normal, RAY *Ray, VECTOR Raw_Normal, int WhichRay);

int GetPhotonStat(POVMSType a);


extern int backtraceFlag;
extern PHOTON_OPTIONS photonOptions;

END_POV_NAMESPACE

#endif
