/****************************************************************************
 *               photons.cpp
 *
 * Author: Nathan Kopp
 *
 * This module implements Photon Mapping.
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
 * $File: //depot/povray/3.6-release/source/photons.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/


#include "frame.h"
#include "userio.h"
#include "povray.h"
#include "vector.h"
#include "texture.h"  /* for FRAND() */
#include "matrices.h"
#include "objects.h"
#include "csg.h"
#include "octree.h"
#include "radiosit.h"
#include "photons.h"
#include "povms.h"
#include "povmsend.h"
#include "ray.h"
#include "pov_util.h"

#include <algorithm>

BEGIN_POV_NAMESPACE

/* ------------------------------------------------------ */
/* global variables */
/* ------------------------------------------------------ */
int backtraceFlag; // GLOBAL VARIABLE

PHOTON_OPTIONS photonOptions; // GLOBAL VARIABLE

int InitBacktraceWasCalled; // GLOBAL VARIABLE

// statistics helpers
int gPhotonStat_i = 0; // GLOBAL VARIABLE
int gPhotonStat_x_samples = 0; // GLOBAL VARIABLE
int gPhotonStat_y_samples = 0; // GLOBAL VARIABLE
int gPhotonStat_end = 0; // GLOBAL VARIABLE

/* ------------------------------------------------------ */
/* external variables */
/* ------------------------------------------------------ */
extern int Trace_Level; // GLOBAL VARIABLE
extern int disp_elem; // GLOBAL VARIABLE
extern int disp_nelems; // GLOBAL VARIABLE

/* ------------------------------------------------------ */
/* static functions */
/* ------------------------------------------------------ */
static PHOTON* AllocatePhoton(PHOTON_MAP *map);
static void FreePhotonMemory();
static void InitPhotonMemory();
static void sortAndSubdivide(int start, int end, int sorted);
static void buildTree(PHOTON_MAP *map);
static void ShootPhotonsAtObject(OBJECT *Object, LIGHT_SOURCE *Light, int count);
static void SearchThroughObjects(OBJECT *Object, LIGHT_SOURCE *Light, bool count);
static int savePhotonMap(void);
static int loadPhotonMap(void);
static void swapPhotons(int a, int b);
static void PQInsert(PHOTON *photon, DBL d);
static void PQDelMax(void);
static void gatherPhotonsRec(int start, int end);
static void setGatherOptions(PHOTON_MAP *map, int mediaMap);

/* ------------------------------------------------------ */
/* static variables */
/* ------------------------------------------------------ */
/* 
  These static variables are used to conserve stack space during
  extensive recursion when gathering photons.  All of these static
  variables end in "_s".
*/
static PHOTON **map_s;  /* photon map */ // GLOBAL VARIABLE
static DBL size_sq_s;   /* search radius squared */ // GLOBAL VARIABLE
static DBL Size_s;      /* search radius (static) */ // GLOBAL VARIABLE
static DBL sqrt_dmax_s, dmax_s;      /* dynamic search radius... current maximum */ // GLOBAL VARIABLE
static int TargetNum_s; /* how many to gather */ // GLOBAL VARIABLE
static DBL *pt_s;       /* point around which we are gathering */ // GLOBAL VARIABLE
static int numfound_s;  /* number of photons found */ // GLOBAL VARIABLE
static DBL *norm_s;     /* surface normal */ // GLOBAL VARIABLE
static DBL flattenFactor; /* amount to flatten the spher to make it */ // GLOBAL VARIABLE
                          /* an ellipsoid when gathering photons */
                          /* zero = no flatten, one = regular */
static DBL photonCountEstimate; // GLOBAL VARIABLE

/*****************************************************************************

 FUNCTION

   CheckPassThru()

   Checks to make sure that pass-through, high-density, and refraction
   are not simultaneously selected.  If all three are turned on, we need
   to choose an appropriate one to turn off.

  Preconditions:
    'o' is an initialized object
    'flag' is PH_PASSTHRU_FLAG, PH_TARGET_FLAG, or PH_RFR_ON_FLAG
         (this is which flag was set most recently)

  Postconditions:
    One of these flags in 'o' is turned off, since they cannot all be turned on.

******************************************************************************/

void CheckPassThru(OBJECT *o, int flag)
{
  if( Test_Flag(o, PH_PASSTHRU_FLAG) &&
      Test_Flag(o, PH_TARGET_FLAG) &&
      !Test_Flag(o, PH_RFR_OFF_FLAG) )
  {
    switch (flag)
    {
      case PH_PASSTHRU_FLAG:
        Warning(0, "Cannot use pass_through with refraction & target.\nTurning off refraction.");
        Set_Flag(o, PH_RFR_OFF_FLAG);
        Clear_Flag(o, PH_RFR_ON_FLAG);
        break;

      case PH_TARGET_FLAG:
        if(Test_Flag(o, PH_RFR_ON_FLAG))
        {
          Warning(0, "Cannot use pass_through with refraction & target.\nTurning off pass_through.");
          Clear_Flag(o,PH_PASSTHRU_FLAG);
        }
        else
        {
          Warning(0, "Cannot use pass_through with refraction & target.\nTurning off refraction.");
          Set_Flag(o, PH_RFR_OFF_FLAG);
          Clear_Flag(o, PH_RFR_ON_FLAG);
        }
        break;

      case PH_RFR_ON_FLAG:
        Warning(0, "Cannot use pass_through with refraction & target.\nTurning off pass_through.");
        Clear_Flag(o, PH_PASSTHRU_FLAG);
        break;
    }
  }
}

/*****************************************************************************

 FUNCTION

   InitBacktraceEverything()

   Allocates memory.
   Initializes all photon mapping stuff.
   Does not create the photon map.

   Preconditions: InitBacktraceEverything() not yet called
                    or
                  both InitBacktraceEverything() and FreeBacktraceEverything() called

   Postconditions:
      If photonOptions.photonsEnabled is true, then
        memory for photon mapping is allocated.
      else
        nothing is done

******************************************************************************/

void InitBacktraceEverything()
{
  int i;
  double theta;

  if (photonOptions.photonsEnabled)
  {
    InitBacktraceWasCalled = true;

    photonOptions.photonMap.head = NULL;
    photonOptions.photonMap.numPhotons  = 0;
    photonOptions.photonMap.numBlocks  = 0;
#ifdef GLOBAL_PHOTONS
    photonOptions.globalPhotonMap.head = NULL;
    photonOptions.globalPhotonMap.numPhotons  = 0;
    photonOptions.globalPhotonMap.numBlocks  = 0;
#endif
    photonOptions.mediaPhotonMap.head = NULL;
    photonOptions.mediaPhotonMap.numPhotons  = 0;
    photonOptions.mediaPhotonMap.numBlocks  = 0;

    photonOptions.photonGatherList = (PHOTON**)POV_MALLOC(sizeof(PHOTON *)*photonOptions.maxGatherCount, "Photon Map Info");
    photonOptions.photonDistances = (DBL *)POV_MALLOC(sizeof(DBL)*photonOptions.maxGatherCount, "Photon Map Info");

    InitPhotonMemory();

    /* create the sin/cos arrays for speed */
    /* range is -127..+127  =>  0..254 */
    photonOptions.sinTheta = (DBL *)POV_MALLOC(sizeof(DBL)*255, "Photon Map Info");
    photonOptions.cosTheta = (DBL *)POV_MALLOC(sizeof(DBL)*255, "Photon Map Info");
    for(i=0; i<255; i++)
    {
      theta = (double)(i-127)*M_PI/127.0;
      photonOptions.sinTheta[i] = sin(theta);
      photonOptions.cosTheta[i] = cos(theta);
    }
  }
}

/* savePhotonMap()

  Saves the caustic photon map to a file.

  Preconditions:
    InitBacktraceEverything was called
    the photon map has been built and balanced
    photonOptions.fileName contains the filename to save

  Postconditions:
    Returns 1 if success, 0 if failure.
    If success, the photon map has been written to the file.
*/
static int savePhotonMap()
{
  PHOTON *ph;
  FILE *f;
  int i, err;

  f = fopen(photonOptions.fileName, "wb");
  if (!f) return 0;

  /* caustic photons */
  fwrite(&photonOptions.photonMap.numPhotons, sizeof(photonOptions.photonMap.numPhotons),1,f);
  if (photonOptions.photonMap.numPhotons>0 && photonOptions.photonMap.head)
  {
    for(i=0; i<photonOptions.photonMap.numPhotons; i++)
    {
      ph = &(PHOTON_AMF(photonOptions.photonMap.head, i));
      err = fwrite(ph, sizeof(PHOTON), 1, f);

      if (err<=0)
      {
        /* fwrite returned an error! */
        fclose(f);
        return 0;
      }
    }
  }

#ifdef GLOBAL_PHOTONS
  /* global photons */
  fwrite(&photonOptions.globalPhotonMap.numPhotons, sizeof(photonOptions.globalPhotonMap.numPhotons),1,f);
  if (photonOptions.globalPhotonMap.numPhotons>0 && photonOptions.globalPhotonMap.head)
  {
    for(i=0; i<photonOptions.globalPhotonMap.numPhotons; i++)
    {
      ph = &(PHOTON_AMF(photonOptions.globalPhotonMap.head, i));
      err = fwrite(ph, sizeof(PHOTON), 1, f);

      if (err<=0)
      {
        /* fwrite returned an error! */
        fclose(f);
        return 0;
      }
    }
  }
#endif

  /* media photons */
  fwrite(&photonOptions.mediaPhotonMap.numPhotons, sizeof(photonOptions.mediaPhotonMap.numPhotons),1,f);
  if (photonOptions.mediaPhotonMap.numPhotons>0 && photonOptions.mediaPhotonMap.head)
  {
    for(i=0; i<photonOptions.mediaPhotonMap.numPhotons; i++)
    {
      ph = &(PHOTON_AMF(photonOptions.mediaPhotonMap.head, i));
      err = fwrite(ph, sizeof(PHOTON), 1, f);

      if (err<=0)
      {
        /* fwrite returned an error! */
        fclose(f);
        return 0;
      }
    }
  }

  fclose(f);
  return true;
}

/* loadPhotonMap()

  Loads the caustic photon map from a file.

  Preconditions:
    InitBacktraceEverything was called
    the photon map is empty
    photonOptions.fileName contains the filename to load

  Postconditions:
    Returns 1 if success, 0 if failure.
    If success, the photon map has been loaded from the file.
    If failure then the render should stop with an error
*/
static int loadPhotonMap()
{
  int i;
  int err;
  PHOTON *ph;
  FILE *f;
  int numph;

  if (!photonOptions.photonsEnabled) return 0;

  f = fopen(photonOptions.fileName, "rb");
  if (!f) return 0;

  fread(&numph, sizeof(numph),1,f);

  for(i=0; i<numph; i++)
  {
    ph = AllocatePhoton(&photonOptions.photonMap);
    err = fread(ph, sizeof(PHOTON), 1, f);

    if (err<=0)
    {
      /* fread returned an error! */
      fclose(f);
      return 0;
    }
  }

  if (!feof(f)) /* for backwards file format compatibility */
  {

#ifdef GLOBAL_PHOTONS
    /* global photons */
    fread(&numph, sizeof(numph),1,f);
    for(i=0; i<numph; i++)
    {
      ph = AllocatePhoton(&photonOptions.globalPhotonMap);
      err = fread(ph, sizeof(PHOTON), 1, f);

      if (err<=0)
      {
        /* fread returned an error! */
        fclose(f);
        return 0;
      }
    }
#endif

    /* media photons */
    fread(&numph, sizeof(numph),1,f);
    for(i=0; i<numph; i++)
    {
      ph = AllocatePhoton(&photonOptions.mediaPhotonMap);
      err = fread(ph, sizeof(PHOTON), 1, f);

      if (err<=0)
      {
        /* fread returned an error! */
        fclose(f);
        return 0;
      }
    }

  }

  fclose(f);
  return true;
}

/*****************************************************************************

 FUNCTION

  FreeBacktraceEverything()

  Preconditions:
    if photonOptions.photonsEnabled is true, then InitBacktraceEverything()
      must have been called
    
  PostConditions:
    if photonOptions.photonsEnabled is true, then
      photon memory is freed
      sets photonOptions.photonsEnabled to false
    else
      does nothing

******************************************************************************/

void FreeBacktraceEverything()
{
  if (!InitBacktraceWasCalled) return;

  if (photonOptions.photonsEnabled)
  {
    /* free everything that we allocated */

    if(photonOptions.photonGatherList)
      POV_FREE(photonOptions.photonGatherList);
    photonOptions.photonGatherList = NULL;

    if(photonOptions.photonDistances)
      POV_FREE(photonOptions.photonDistances);
    photonOptions.photonDistances = NULL;

    if (photonOptions.sinTheta)
      POV_FREE(photonOptions.sinTheta);
    photonOptions.sinTheta = NULL;

    if (photonOptions.cosTheta)
      POV_FREE(photonOptions.cosTheta);
    photonOptions.cosTheta = NULL;

    FreePhotonMemory();
    photonOptions.photonsEnabled = false;
  }
}

/*****************************************************************************

 FUNCTION

  AllocatePhoton(PHOTON_MAP *map)
    allocates a photon

    Photons are allocated in blocks.  map->head is a
    dynamically-created array of these blocks.  The blocks themselves
    are allocated as they are needed.

  Preconditions:
    InitBacktraceEverything was called

  Postconditions:
    Marks another photon as allocated (and allocates another block of
    photons if necessary).
    Returns a pointer to the new photon.
    This will be the next available photon in array.

******************************************************************************/

PHOTON* AllocatePhoton(PHOTON_MAP *map)
{
  int i,j,k;

  /* array mapping funciton */
  /* !!!!!!!!!!! warning
     This code does the same function as the macro PHOTON_AMF
     It is done here separatly instead of using the macro for
     speed reasons (to avoid duplicate operations).  If the
     macro is changed, this MUST ALSO BE CHANGED!
  */
  i=(map->numPhotons & PHOTON_BLOCK_MASK);
  j=(map->numPhotons >> (PHOTON_BLOCK_POWER));

  /* new photon */
  map->numPhotons++;

  if(j == map->numBlocks)
  {
    /* the base array is too small, we need to reallocate it */
    PHOTON **newMap;
    newMap = (PHOTON **)POV_MALLOC(sizeof(PHOTON *)*map->numBlocks*2, "photons");
    map->numBlocks*=2;

    /* copy entries */
    for(k=0; k<j; k++)
      newMap[k] = map->head[k];

    /* set new entries to zero */
    for(k=j; k<map->numBlocks; k++)
      newMap[k] = NULL;

    /* free old map and put the new map in place */
    POV_FREE(map->head);
    map->head = newMap;
  }

  if(map->head[j] == NULL)
    /* allocate a new block of photons */
    map->head[j] = (PHOTON *)POV_MALLOC(sizeof(PHOTON)*PHOTON_BLOCK_SIZE, "photons");

  return &(map->head[j][i]);
}

/*****************************************************************************

 FUNCTION

   InitPhotonMemory()

  Initializes photon memory.
  Must only be called by InitBacktraceEverything().

******************************************************************************/

static void InitPhotonMemory()
{
  int k;

  /* allocate the base array */
  photonOptions.photonMap.numPhotons = 0;
  photonOptions.photonMap.numBlocks = INITIAL_BASE_ARRAY_SIZE;
  photonOptions.photonMap.head = (PHOTON_BLOCK *)POV_MALLOC(sizeof(PHOTON_BLOCK *)*INITIAL_BASE_ARRAY_SIZE, "photons");

  /* zero the array */
  for(k=0; k<photonOptions.photonMap.numBlocks; k++)
    photonOptions.photonMap.head[k] = NULL;

#ifdef GLOBAL_PHOTONS
  /* ------------ global photons ----------------*/
  /* allocate the base array */
  photonOptions.globalPhotonMap.numPhotons = 0;
  photonOptions.globalPhotonMap.numBlocks = INITIAL_BASE_ARRAY_SIZE;
  photonOptions.globalPhotonMap.head = (PHOTON_BLOCK *)POV_MALLOC(sizeof(PHOTON_BLOCK *)*INITIAL_BASE_ARRAY_SIZE, "photons");

  /* zero the array */
  for(k=0; k<photonOptions.globalPhotonMap.numBlocks; k++)
    photonOptions.globalPhotonMap.head[k] = NULL;
#endif

  /* ------------ media photons ----------------*/
  /* allocate the base array */
  photonOptions.mediaPhotonMap.numPhotons = 0;
  photonOptions.mediaPhotonMap.numBlocks = INITIAL_BASE_ARRAY_SIZE;
  photonOptions.mediaPhotonMap.head = (PHOTON_BLOCK *)POV_MALLOC(sizeof(PHOTON_BLOCK *)*INITIAL_BASE_ARRAY_SIZE, "photons");

  /* zero the array */
  for(k=0; k<photonOptions.mediaPhotonMap.numBlocks; k++)
    photonOptions.mediaPhotonMap.head[k] = NULL;
}

/*****************************************************************************

 FUNCTION

  FreePhotonMemory()

  Frees all allocated blocks and the base array.
  Must be called only by FreeBacktraceEverything()

******************************************************************************/

static void FreePhotonMemory()
{
  int j;

  /* if already freed then stop now */
  if (photonOptions.photonMap.head==NULL)
    return;

  /* file name to load or save caustic photon map */
  if ( photonOptions.fileName )
  {
  		POV_FREE(photonOptions.fileName);
  		photonOptions.fileName=NULL;
  }

  /* free all non-NULL arrays */
  for(j=0; j<photonOptions.photonMap.numBlocks; j++)
  {
    if(photonOptions.photonMap.head[j] != NULL)
    {
      POV_FREE(photonOptions.photonMap.head[j]);
    }
  }

  /* free the base array */
  POV_FREE(photonOptions.photonMap.head);
  photonOptions.photonMap.head = NULL;

#ifdef GLOBAL_PHOTONS
  /* ---------------- global photons -------------- */
  /* if already freed then stop now */
  if (photonOptions.globalPhotonMap.head==NULL)
    return;

  /* free all non-NULL arrays */
  for(j=0; j<photonOptions.globalPhotonMap.numBlocks; j++)
  {
    if(photonOptions.globalPhotonMap.head[j] != NULL)
    {
      POV_FREE(photonOptions.globalPhotonMap.head[j]);
    }
  }

  /* free the base array */
  POV_FREE(photonOptions.globalPhotonMap.head);
  photonOptions.globalPhotonMap.head = NULL;
#endif

  /* ---------------- media photons -------------- */
  /* if already freed then stop now */
  if (photonOptions.mediaPhotonMap.head==NULL)
    return;

  /* free all non-NULL arrays */
  for(j=0; j<photonOptions.mediaPhotonMap.numBlocks; j++)
  {
    if(photonOptions.mediaPhotonMap.head[j] != NULL)
    {
      POV_FREE(photonOptions.mediaPhotonMap.head[j]);
    }
  }

  /* free the base array */
  POV_FREE(photonOptions.mediaPhotonMap.head);
  photonOptions.mediaPhotonMap.head = NULL;

}


/*****************************************************************************
*
* FUNCTION
*
*   cubic_spline  (copied from point.c for use with light attenuation )
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   Cubic spline that has tangents of slope 0 at x == low and at x == high.
*   For a given value "pos" between low and high the spline value is returned.
*
* CHANGES
*
*   -
*
******************************************************************************/

static DBL cubic_spline(DBL low, DBL  high, DBL  pos)
{
  /* Check to see if the position is within the proper boundaries. */
  if (pos < low)
    return(0.0);
  else
  {
    if (pos >= high)
      return(1.0);
  }

  /* Normalize to the interval [0...1]. */
  pos = (pos - low) / (high - low);

  /* See where it is on the cubic curve. */
  return(3 - 2 * pos) * pos * pos;
}

/*****************************************************************************

 FUNCTION

  SearchThroughObjects()

  Searches through 'object' and all siblings  and children of 'object' to
  locate objects with PH_TARGET_FLAG set.  This flag means that the object
  receives photons.

  Preconditions:
    Photon mapping initialized (InitBacktraceEverything() called)
    'Object' is a object (with or without siblings)
    'Light' is a light source in the scene

  Postconditions:
    Photons may have been shot at the object, its siblings, or its children.

******************************************************************************/

static void SearchThroughObjects(OBJECT *Object, LIGHT_SOURCE *Light, bool count)
{
	OBJECT *Sib;

	/* check this object and all siblings */
	for(Sib = Object; Sib != NULL; Sib = Sib -> Sibling)
	{
		if(Test_Flag(Sib, PH_TARGET_FLAG) &&
		   !(Sib->Type & LIGHT_SOURCE_OBJECT))
		{
			/* do not shoot photons if global lights are turned off for object */
			if(!Test_Flag(Sib, NO_GLOBAL_LIGHTS_FLAG))
				ShootPhotonsAtObject(Sib, Light, count);

			Do_Cooperate(1);

			Check_User_Abort(false);
		}
		/* if it has children, check them too */
		else if((Sib->Type & IS_COMPOUND_OBJECT))
		{
			SearchThroughObjects(((CSG *)Sib)->Children, Light, count);
		}
	}
}

/*****************************************************************************

 FUNCTION

  ShootPhotonsAtObject()

  Shoots photons from 'Light' to 'Object'

  Preconditions:
    Photon mapping initialized (InitBacktraceEverything() called)
    'Object' is a object with PH_TARGET_FLAG set
    'Light' is a light source in the scene

    Possible future expansion:
      Object==NULL means create a global photon map.

  Postconditions:
    Photons may have been shot at the object, depending on a variety
    of flags

******************************************************************************/

static void ShootPhotonsAtObject(OBJECT *Object, LIGHT_SOURCE *Light, int count)
{
  RAY Ray;                       /* ray that we shoot */
  COLOUR Colour, PhotonColour;   /* light color and photon color */
  int i;                         /* counter */
  DBL theta, phi;                /* rotation angles */
  DBL dtheta, dphi;              /* deltas for theta and phi */
  DBL jittheta, jitphi;          /* jittered versions of theta and phi */
  DBL mintheta,maxtheta,minphi,maxphi;
                                 /* these are minimum and maximum for theta and
                                     phi for the spiral shooting */
  DBL dist;                      /* distance from light to bounding sphere */
  DBL rad;                       /* radius of bounding sphere */
  DBL st,ct;                     /* cos(theta) & sin(theta) for rotation */
  DBL Attenuation;               /* light attenuation for spotlight */
  DBL costheta_spot;
  VECTOR up, left, ctr, toctr, v; /* vectors to determine direction of shot */
  TRANSFORM Trans;               /* transformation for rotation */
  int mergedFlags=0;             /* merged flags to see if we should shoot photons */
  int notComputed=true;          /* have the ray containers been computed for this point yet?*/
  int hitAtLeastOnce = false;    /* have we hit the object at least once - for autostop stuff */

  /* get the light source colour */
  Assign_Colour(Colour, Light->Colour);

  /* set global variable stuff */
  photonOptions.Light = Light;
  photonOptions.photonObject = Object;

  /* first, check on various flags... make sure all is a go for this object */
  if(Object)
  {
    mergedFlags = Light->Flags | photonOptions.photonObject->Flags;
    photonOptions.lightFlags = Light->Flags;
  }
  else
  {
    mergedFlags = photonOptions.lightFlags = PH_RFR_ON_FLAG | PH_RFL_ON_FLAG; /*Light->Flags;*/
  }

  if (!(((mergedFlags & PH_RFR_ON_FLAG) && !(mergedFlags & PH_RFR_OFF_FLAG)) ||
      ((mergedFlags & PH_RFL_ON_FLAG) && !(mergedFlags & PH_RFL_OFF_FLAG))))
    /* it is a no-go for this object... bail out now */
    return;


  if(Object)
  {
    /* find bounding sphere based on bounding box */
    ctr[X] = photonOptions.photonObject->BBox.Lower_Left[X] + photonOptions.photonObject->BBox.Lengths[X] / 2.0;
    ctr[Y] = photonOptions.photonObject->BBox.Lower_Left[Y] + photonOptions.photonObject->BBox.Lengths[Y] / 2.0;
    ctr[Z] = photonOptions.photonObject->BBox.Lower_Left[Z] + photonOptions.photonObject->BBox.Lengths[Z] / 2.0;
    VSub(v, ctr,photonOptions.photonObject->BBox.Lower_Left);
    VLength(rad, v);

    /* find direction from object to bounding sphere */
    VSub(toctr, ctr, Light->Center);
    VLength(dist, toctr);

    VNormalizeEq(toctr);
    if ( fabs(fabs(toctr[Z])- 1.) < .1 ) {
      /* too close to vertical for comfort, so use cross product with horizon */
      up[X] = 0.; up[Y] = 1.; up[Z] = 0.;
    }
    else
    {
      up[X] = 0.; up[Y] = 0.; up[Z] = 1.;
    }

    /* find "left", which is vector perpendicular to toctr */
    if(Light->Parallel)
    {
      /* for parallel lights, left is actually perpendicular to the direction of the
         light source */
      VCross(left, Light->Direction, up);  VNormalizeEq(left);
    }
    else
    {
      VCross(left, toctr, up);  VNormalizeEq(left);
    }
    

    /*
   light   dist         ctr
    * ------------------ +
         ---___          |
               ---___    | rad
                     ---_|

    */

    /* calculate the spacial separation (spread) */
    photonOptions.photonSpread = photonOptions.photonObject->Ph_Density*photonOptions.surfaceSeparation;

    /* if rays aren't parallel, divide by dist so we get separation at a distance of 1 unit */
    if (!Light->Parallel)
    {
      photonOptions.photonSpread /= dist;
    }

    if (count)
    {
      /* try to guess the number of photons */
      DBL x=rad / (photonOptions.photonObject->Ph_Density*photonOptions.surfaceSeparation);
      x=x*x*M_PI;

#if(1)
      if ( ((mergedFlags & PH_RFR_ON_FLAG) && !(mergedFlags & PH_RFR_OFF_FLAG)) &&
           ((mergedFlags & PH_RFL_ON_FLAG) && !(mergedFlags & PH_RFL_OFF_FLAG)) )
      {
        x *= 1.5;  /* assume 2 times as many photons with both reflection & refraction */
      }

      if ( !Test_Flag(photonOptions.photonObject, PH_IGNORE_PHOTONS_FLAG) )
      {
        if ( ((mergedFlags & PH_RFR_ON_FLAG) && !(mergedFlags & PH_RFR_OFF_FLAG)) )
        {
          if ( ((mergedFlags & PH_RFL_ON_FLAG) && !(mergedFlags & PH_RFL_OFF_FLAG)) )
            x *= 3;  /* assume 3 times as many photons if ignore_photons not used */
          else
            x *= 2;  /* assume less for only refraction */
        }
      }

      x *= 0.5;  /* assume 1/2 of photons hit target object */
#endif

      photonCountEstimate += x;
      return;
    }
  }
  else
  {
#ifdef GLOBAL_PHOTONS
    /* set up for global photon map */
    mintheta = 0;
    maxtheta = M_PI;
    /* determine photonSpread from a number? */
    photonOptions.photonSpread = Light->Ph_Density*photonOptions.globalSeparation;
    Make_Vector(up, 1,0,0);
    Make_Vector(left, 0,1,0);
    Make_Vector(toctr, 0,0,1);
    dist = 1.0;

    if (count)
    {
      /* try to guess the number of photons */
      photonCountEstimate += M_PI*4.0/(photonOptions.photonSpread*photonOptions.photonSpread);
      return;
    }
#else
    Error("Internal Error - global photons have been disabled.");  /* we should never get here if gobal photons are not enabled */
#endif
  }

  /* adjust spread if we are using an area light */
  if(Light->Area_Light && Light->Photon_Area_Light)
  {
    photonOptions.photonSpread *= sqrt((DBL)(Light->Area_Size1*Light->Area_Size2));
  }

  /* set the photon density - calculate angular density from spacial */
  if(Light->Parallel)
  {
    /* OK, here we fake things a bit for parallel lights.  Theta is not really theta.
       It really represents the radius... but why re-code an entire loop.  For POV 4.0
       this should be re-written as an abstract class with polymorphism. */
    dtheta = photonOptions.photonSpread;
  }
  else
  {

    /* calculate delta theta */
    dtheta = atan(photonOptions.photonSpread);
  }

  /* if photonSpread <= 0.0, we must return or we'll get stuck in an infinite loop! */
  if (photonOptions.photonSpread <= 0.0)
    return;

  mintheta = 0;
  if (Light->Parallel)
  {
    maxtheta = rad;
  }
  else if (dist>=rad)
  {
    maxtheta = atan(rad/dist);
  }
  else
  {
    maxtheta = M_PI;
    if (fabs(dist)<EPSILON)
    {
      Make_Vector(up, 1,0,0);
      Make_Vector(left, 0,1,0);
      Make_Vector(toctr, 0,0,1);
    }
    dist = rad;
  }

    
  /* ---------------------------------------------
         main ray-shooting loop 
     --------------------------------------------- */
  i = 0;
  notComputed = true;
  for(theta=mintheta; theta<maxtheta; theta+=dtheta)
  {
    photonOptions.hitObject = false;
    
    if (theta<EPSILON)
    {
      dphi=2*M_PI;
    }
    else
    {
      /* remember that for area lights, "theta" really means "radius" */
      if (Light->Parallel)
      {
        dphi = dtheta / theta;
      }
      else
      {
        dphi=dtheta/sin(theta);
      }
    }

    minphi = -M_PI + dphi*FRAND()*0.5;
    maxphi = M_PI - dphi/2 + (minphi+M_PI);
    for(phi=minphi; phi<maxphi; phi+=dphi)
    {
      int x_samples,y_samples;
      int area_x, area_y;
      /* ------------------- shoot one photon ------------------ */

      /* jitter theta & phi */
      jitphi = phi + (dphi)*(FRAND() - 0.5)*1.0*photonOptions.jitter;
      jittheta = theta + (dtheta)*(FRAND() - 0.5)*1.0*photonOptions.jitter;

      /* actually, shoot multiple samples for area light */
      if(Light->Area_Light && Light->Photon_Area_Light && !Light->Parallel)
      {
        x_samples = Light->Area_Size1;
        y_samples = Light->Area_Size2;
      }
      else
      {
        x_samples = 1;
        y_samples = 1;
      }

      for(area_x=0; area_x<x_samples; area_x++)
      for(area_y=0; area_y<y_samples; area_y++)
      {

        Assign_Vector(Ray.Initial,Light->Center);

        if (Light->Area_Light && !Light->Parallel)
        {
          /* ------------- area light ----------- */
          /* we need to make new up, left, and toctr vectors so we can
             do proper rotations of theta and phi about toctr.  The
             ray's initial point and ending points are both jittered to
             produce the area-light effect. */
          DBL Jitter_u, Jitter_v, ScaleFactor;
          VECTOR NewAxis1, NewAxis2;

          /* we must recompute the media containers (new start point) */
          notComputed = true;

          /*
          Jitter_u = (int)(FRAND()*Light->Area_Size1);
          Jitter_v = (int)(FRAND()*Light->Area_Size2);
          */
          Jitter_u = area_x; /*+(0.5*FRAND() - 0.25);*/
          Jitter_v = area_y; /*+(0.5*FRAND() - 0.25);*/

          if (Light->Area_Size1 > 1 && x_samples>1)
          {
            ScaleFactor = Jitter_u/(DBL)(Light->Area_Size1 - 1) - 0.5;
            VScale (NewAxis1, Light->Axis1, ScaleFactor);
          }
          else
          {
            Make_Vector(NewAxis1, 0.0, 0.0, 0.0);
          }

          if (Light->Area_Size2 > 1 && y_samples>1)
          {
            ScaleFactor = Jitter_v/(DBL)(Light->Area_Size2 - 1) - 0.5;
            VScale (NewAxis2, Light->Axis2, ScaleFactor);
          }
          else
          {
            Make_Vector(NewAxis2, 0.0, 0.0, 0.0);
          }

          /* need a new toctr & left */
          VAddEq(Ray.Initial, NewAxis1);
          VAddEq(Ray.Initial, NewAxis2);

          VSub(toctr, ctr, Ray.Initial);
          VLength(dist, toctr);

          VNormalizeEq(toctr);
          if ( fabs(fabs(toctr[Z])- 1.) < .1 ) {
            /* too close to vertical for comfort, so use cross product with horizon */
            up[X] = 0.; up[Y] = 1.; up[Z] = 0.;
          }
          else
          {
            up[X] = 0.; up[Y] = 0.; up[Z] = 1.;
          }
          VCross(left, toctr, up);  VNormalizeEq(left);

          if (fabs(dist)<EPSILON)
          {
            Make_Vector(up, 1,0,0);
            Make_Vector(left, 0,1,0);
            Make_Vector(toctr, 0,0,1);
          }
        }

        DBL dist_of_initial_from_center;

        if (Light->Parallel)
        {
          DBL a;
          VECTOR v;
          /* assign the direction */
          Assign_Vector(Ray.Direction,Light->Direction);
          
          /* project ctr onto plane defined by Direction & light location */

          VDot(a,Ray.Direction, toctr);
          VScale(v,Ray.Direction, -a*dist); /* MAYBE NEEDS TO BE NEGATIVE! */

          VAdd(Ray.Initial, ctr, v);

          /* move point along "left" distance theta (remember theta means rad) */
          VScale(v,left,jittheta);

          /* rotate pt around Ray.Direction by phi */
          /* use POV funcitons... slower but easy */
          Compute_Axis_Rotation_Transform(&Trans,Light->Direction,jitphi);
          MTransPoint(v, v, &Trans);

          VAddEq(Ray.Initial, v);

          // compute the length of "v" if we're going to use it
          if (Light->Light_Type == CYLINDER_SOURCE)
          {
            VECTOR initial_from_center;
            VSub(initial_from_center, Ray.Initial, Light->Center);
            VLength(dist_of_initial_from_center, initial_from_center);
          }
        }
        else
        {
          /* rotate toctr by theta around up */
          st = sin(jittheta);
          ct = cos(jittheta);
          /* use fast rotation */
          v[X] = -st*left[X] + ct*toctr[X];
          v[Y] = -st*left[Y] + ct*toctr[Y];
          v[Z] = -st*left[Z] + ct*toctr[Z];

          /* then rotate by phi around toctr */
          /* use POV funcitons... slower but easy */
          Compute_Axis_Rotation_Transform(&Trans,toctr,jitphi);
          MTransPoint(Ray.Direction, v, &Trans);
        }

        /* ------ attenuation for spot/cylinder (copied from point.c) ---- */ 
        Attenuation = 1.0;

        /* ---------- spot light --------- */
        if (Light->Light_Type == SPOT_SOURCE)
        {
          VDot(costheta_spot, Ray.Direction, Light->Direction);

          if (costheta_spot > 0.0)
          {
            Attenuation = pow(costheta_spot, Light->Coeff);

            if (Light->Radius > 0.0)
              Attenuation *= cubic_spline(Light->Falloff, Light->Radius, costheta_spot);

          }
          else
            Attenuation = 0.0;
        }
        /* ---------- cylinder light ----------- */
        else if (Light->Light_Type == CYLINDER_SOURCE)
        {
          DBL k, len;

          VDot(k, Ray.Direction, Light->Direction);

          if (k > 0.0)
          {
            len = dist_of_initial_from_center;

            if (len < Light->Falloff)
            {
              DBL dist = 1.0 - len / Light->Falloff;
              Attenuation = pow(dist, Light->Coeff);

              if (Light->Radius > 0.0 && len > Light->Radius)
                Attenuation *= cubic_spline(0.0, 1.0 - Light->Radius / Light->Falloff, dist);

            }
            else
              Attenuation = 0.0;
          }
          else
            Attenuation = 0.0;
        }

        /* set up defaults for reflection, refraction */
        photonOptions.passThruPrev = true;
        photonOptions.passThruThis = false;

        photonOptions.photonDepth = 0.0;
        Trace_Level = 1;
        Total_Depth = 0.0;
        Increase_Counter(stats[Number_Of_Photons_Shot]);

        /* attenuate for area light extra samples */
        Attenuation/=(x_samples*y_samples);

        /* compute photon color from light source & attenuation */

        PhotonColour[0] = Colour[0]*Attenuation;
        PhotonColour[1] = Colour[1]*Attenuation;
        PhotonColour[2] = Colour[2]*Attenuation;
        PhotonColour[3] = 0.0;
        PhotonColour[4] = 0.0;

        if (Attenuation<0.00001) continue;

        /* handle the projected_through object if it exists */
        if (Light->Projected_Through_Object != NULL)
        {
          /* try to intersect ray with projected-through object */
          INTERSECTION Intersect;

          Intersect.Object = NULL;
          if ( Intersection( &Intersect, Light->Projected_Through_Object, &Ray ) ) 
          {
            /* we must recompute the media containers (new start point) */
            notComputed = true;

            /* we did hit it, so find the 'real' starting point of the ray */
            /* find the farthest intersection */
            VAddScaledEq(Ray.Initial,Intersect.Depth+EPSILON, Ray.Direction);
            photonOptions.photonDepth += Intersect.Depth+EPSILON;
            while(Intersection( &Intersect, Light->Projected_Through_Object, &Ray ) )
            {
              VAddScaledEq(Ray.Initial, Intersect.Depth+EPSILON, Ray.Direction);
              photonOptions.photonDepth += Intersect.Depth+EPSILON;
            }
          }
          else
          {
            /* we didn't hit it, so stop now */
            continue;
          }
 
        }

        /* As mike said, "fire photon torpedo!" */
        Initialize_Ray_Containers(&Ray);
        initialize_ray_container_state(&Ray, notComputed);
        notComputed = false;
        disp_elem = 0;   /* for dispersion */
        disp_nelems = 0; /* for dispersion */

        Trace(&Ray, PhotonColour, 1.0);

        /* display here */
        i++;
        if ((i%100) == 0)
        {
            gPhotonStat_i = i;
            gPhotonStat_x_samples = x_samples;
            gPhotonStat_y_samples = y_samples;
            Send_ProgressUpdate(PROGRESS_BUILDING_PHOTON_MAPS);
            Check_User_Abort(false);
        }

      } /* end of multiple samples */
    }

    /* if we didn't hit anything and we're past the autostop angle, then
       we should stop 
       
       as per suggestion from Smellenberg, changed autostop to a percentage
       of the object's bounding sphere. */

    /* suggested by Pabs, we only use autostop if we have it it once */
    if (photonOptions.hitObject) hitAtLeastOnce=true;

    if (hitAtLeastOnce && !photonOptions.hitObject && photonOptions.photonObject)
      if (theta>photonOptions.autoStopPercent*maxtheta) break;
  } /* end of rays loop */
}

/*****************************************************************************

  FUNCTION

  BuildPhotonMaps()

  This is the primary function for building photon maps.

  Preconditions:
    Photon memory is allocated (InitBacktraceEverything has been called).
    The entire scene has been parsed.
    The ray-tracer has been completely initialized.

  Postconditions:
    The photon map is built based on options specified in the scene file.

******************************************************************************/

void BuildPhotonMaps(void)
{
  LIGHT_SOURCE *Light;  /* light source to use */
  LIGHT_GROUP_LIGHT *Light_Group_Light;
  int old_mtl; /* saved max_trace_level */
  DBL old_adc; /* saved adc_bailout */

  /* if not enabled, then return */
  if (!photonOptions.photonsEnabled)
    return;

  /* should we load the photon map instead of building? */
  if (photonOptions.fileName && photonOptions.loadFile)
  {
    /* status bar for user */
    Send_Progress("Loading Photon Maps", PROGRESS_LOADING_PHOTON_MAPS);
    if (!loadPhotonMap())
    {
      Error("Could not load photon map (%s)",photonOptions.fileName);
    }

    Do_Cooperate(0);

    /* don't build */
    /* but do set photon options automatically */

    /* ----------- surface photons ------------- */
    if (photonOptions.photonMap.numPhotons>0)
    {
      setGatherOptions(&photonOptions.photonMap, false);
    }

#ifdef GLOBAL_PHOTONS
    /* ----------- global photons ------------- */
    if (photonOptions.globalPhotonMap.numPhotons>0)
    {
      setGatherOptions(&photonOptions.globalPhotonMap, false);
    }
#endif

    /* ----------- media photons ------------- */
    if (photonOptions.mediaPhotonMap.numPhotons>0)
    {
      setGatherOptions(&photonOptions.mediaPhotonMap, true);
    }

    return;
  }

  /* set flag so POV knows we're in backtrace step */
  backtraceFlag = 1;

  /* status bar for user */
  Send_Progress("Building Photon Maps", PROGRESS_BUILDING_PHOTON_MAPS);

  /* save adc_bailout and max_trace_level */
  old_adc = ADC_Bailout;
  old_mtl = Max_Trace_Level;

  /* use the photon-specific ones if they were specified */
  if (photonOptions.Max_Trace_Level>=0)
    Max_Trace_Level = photonOptions.Max_Trace_Level;

  if (photonOptions.ADC_Bailout>=0)
    ADC_Bailout = photonOptions.ADC_Bailout;

  /*  COUNT THE PHOTONS  */
  if(photonOptions.surfaceCount>0)
  {
    DBL factor;
    photonCountEstimate = 0.0;

    // global lights
    photonOptions.Light_Is_Global = true;
    for (Light = Frame.Light_Sources;
         Light != NULL;
         Light = Light->Next_Light_Source)
    if (Light->Light_Type != FILL_LIGHT_SOURCE)
    {
      SearchThroughObjects(Frame.Objects, Light, true);
    }
    
    // light_group lights
    photonOptions.Light_Is_Global = false;
    for (Light_Group_Light = Frame.Light_Group_Lights;
         Light_Group_Light != NULL;
         Light_Group_Light = Light_Group_Light->Next)
    {
      Light = Light_Group_Light->Light;
      if (Light->Light_Type != FILL_LIGHT_SOURCE)
      {
        SearchThroughObjects(Frame.Objects, Light, true);
      }
    }

    factor = (DBL)photonCountEstimate/photonOptions.surfaceCount;
    factor = sqrt(factor);
    photonOptions.surfaceSeparation *= factor;
  }

  /*  COUNT THE GLOBAL PHOTONS  */
  if(photonOptions.globalCount>0)
  {
    DBL factor;
    photonCountEstimate = 0.0;

    photonOptions.Light_Is_Global = true;
    for (Light = Frame.Light_Sources;
         Light != NULL;
         Light = Light->Next_Light_Source)
    if (Light->Light_Type != FILL_LIGHT_SOURCE)
    {
      ShootPhotonsAtObject(NULL, Light, true);
    }

    // light_group lights
    photonOptions.Light_Is_Global = false;
    for (Light_Group_Light = Frame.Light_Group_Lights;
         Light_Group_Light != NULL;
         Light_Group_Light = Light_Group_Light->Next)
    {
      Light = Light_Group_Light->Light;
      if (Light->Light_Type != FILL_LIGHT_SOURCE)
      {
        ShootPhotonsAtObject(NULL, Light, true);
      }
    }

    factor = (DBL)photonCountEstimate/photonOptions.globalCount;
    factor = sqrt(factor);
    photonOptions.globalSeparation *= factor;

    Do_Cooperate(1);
  }

  // there is a world out there that wants some attention [trf]
  Do_Cooperate(0);

  /*  loop through global light sources  */
  photonOptions.Light_Is_Global = true;
  for (Light = Frame.Light_Sources;
       Light != NULL;
       Light = Light->Next_Light_Source)
  if (Light->Light_Type != FILL_LIGHT_SOURCE)
  {
    if (Light->Light_Type == CYLINDER_SOURCE && !Light->Parallel)
    {
      Warning(0,"Cylinder lights should be parallel when used with photons.");
    }

    /* do global lighting here if it is ever implemented */
    if (Test_Flag(Light, PH_TARGET_FLAG) && (photonOptions.globalCount>0))
      ShootPhotonsAtObject(NULL, Light, false);

    /* do object-specific lighting */
    SearchThroughObjects(Frame.Objects, Light, false);
  }

  // loop through light_group light sources
  photonOptions.Light_Is_Global = false;
  for (Light_Group_Light = Frame.Light_Group_Lights;
       Light_Group_Light != NULL;
       Light_Group_Light = Light_Group_Light->Next)
  {
    Light = Light_Group_Light->Light;

    if (Light->Light_Type == CYLINDER_SOURCE && !Light->Parallel)
    {
      Warning(0,"Cylinder lights should be parallel when used with photons.");
    }

    /* do global lighting here if it is ever implemented */
    if (Test_Flag(Light, PH_TARGET_FLAG) && (photonOptions.globalCount>0))
      ShootPhotonsAtObject(NULL, Light, false);

    /* do object-specific lighting */
    SearchThroughObjects(Frame.Objects, Light, false);
  }

  /* clear this flag */
  backtraceFlag = 0;

  /* restore saved variables */
  ADC_Bailout = old_adc;
  Max_Trace_Level = old_mtl;

  /* now actually build the kd-tree by sorting the array of photons */
  if (photonOptions.photonMap.numPhotons>0)
  {
    buildTree(&photonOptions.photonMap);
    setGatherOptions(&photonOptions.photonMap, false);
  }

#ifdef GLOBAL_PHOTONS
  /* ----------- global photons ------------- */
  if (photonOptions.globalPhotonMap.numPhotons>0)
  {
    buildTree(&photonOptions.globalPhotonMap);
    setGatherOptions(&photonOptions.globalPhotonMap, false);
  }
#endif

  /* ----------- media photons ------------- */
  if (photonOptions.mediaPhotonMap.numPhotons>0)
  {
    buildTree(&photonOptions.mediaPhotonMap);
    setGatherOptions(&photonOptions.mediaPhotonMap, true);
  }

  if (photonOptions.photonMap.numPhotons+
#ifdef GLOBAL_PHOTONS
      photonOptions.globalPhotonMap.numPhotons+
#endif
      photonOptions.mediaPhotonMap.numPhotons > 0)
  {
    /* should we load the photon map now that it is built? */
    if (photonOptions.fileName && !photonOptions.loadFile)
    {
      /* status bar for user */
      Send_Progress("Saving Photon Maps", PROGRESS_SAVING_PHOTON_MAPS);
      if (!savePhotonMap())
      {
        Warning(0,"Could not save photon map.");
      }
    }
  }
  else
  {
    if (photonOptions.fileName && !photonOptions.loadFile)
    {
      Warning(0,"Could not save photon map - no photons!");
    }
  }

  // good idea to make sure all warnings and errors arrive frontend now [trf]
  Do_Cooperate(0);
}

/*****************************************************************************

  FUNCTION

  addSurfacePhoton()

  Adds a photon to the array of photons.

  Preconditions:
    InitBacktraceEverything() was called
    'Point' is the intersection point to store the photon
    'Origin' is the origin of the light ray
    'LightCol' is the color of the light propogated through the scene
    'RawNorm' is the raw normal of the surface intersected

  Postconditions:
    Another photon is allocated (by AllocatePhoton())
    The information passed in (as well as photonOptions.photonDepth)
      is stored in the photon data structure.

******************************************************************************/

void addSurfacePhoton(VECTOR Point, VECTOR Origin, COLOUR LightCol, VECTOR /*RawNorm*/)
{
  PHOTON *Photon;
  COLOUR LightCol2;
  DBL Attenuation;
  VECTOR d;
  DBL d_len, phi, theta;
  PHOTON_MAP *map;

  /* first, compensate for POV's weird light attenuation */
  if ((photonOptions.Light->Fade_Power > 0.0) && (fabs(photonOptions.Light->Fade_Distance) > EPSILON))
  {
    Attenuation = 2.0 / (1.0 + pow(photonOptions.photonDepth / photonOptions.Light->Fade_Distance, photonOptions.Light->Fade_Power));
  }
  else
    Attenuation = 1;

  VScale(LightCol2, LightCol, Attenuation);

  if(!photonOptions.Light->Parallel)
  {
    VScaleEq(LightCol2, photonOptions.photonDepth*photonOptions.photonDepth);
  }

  VScaleEq(LightCol2, photonOptions.photonSpread*photonOptions.photonSpread);

  /* if too dark, maybe we should stop here */

#ifdef GLOBAL_PHOTONS
  if(photonOptions.photonObject==NULL)
  {
    map = &photonOptions.globalPhotonMap;
    Increase_Counter(stats[Number_Of_Global_Photons_Stored]);
  }
  else
#endif
  {
    map = &photonOptions.photonMap;
    Increase_Counter(stats[Number_Of_Photons_Stored]);
  }


  /* allocate the photon */
  Photon = AllocatePhoton(map);

  /* convert photon from three floats to 4 bytes */
  colour2photonRgbe(Photon->Colour, LightCol2);

  /* store the location */
  Assign_Vector(Photon->Loc, Point);

  /* now determine rotation angles */
  VSub(d,Origin, Point);
  VNormalizeEq(d);
  d_len = sqrt(d[X]*d[X]+d[Z]*d[Z]);

  phi = acos(d[X]/d_len);
  if (d[Z]<0) phi = -phi;

  theta = acos(d_len);
  if (d[Y]<0) theta = -theta;

  /* cram these rotation angles into two signed bytes */
  Photon->theta=(signed char)(theta*127.0/M_PI);
  Photon->phi=(signed char)(phi*127.0/M_PI);

}

/*****************************************************************************

  FUNCTION

  addMediaPhoton()

  Adds a photon to the array of photons.

  Preconditions:
    InitBacktraceEverything() was called
    'Point' is the intersection point to store the photon
    'Origin' is the origin of the light ray
    'LightCol' is the color of the light propogated through the scene

  Postconditions:
    Another photon is allocated (by AllocatePhoton())
    The information passed in (as well as photonOptions.photonDepth)
      is stored in the photon data structure.

******************************************************************************/

void addMediaPhoton(VECTOR Point, VECTOR Origin, COLOUR LightCol, DBL depthDiff)
{
  PHOTON *Photon;
  COLOUR LightCol2;
  DBL Attenuation;
  VECTOR d;
  DBL d_len, phi, theta;

  /* first, compensate for POV's weird light attenuation */
  if ((photonOptions.Light->Fade_Power > 0.0) && (fabs(photonOptions.Light->Fade_Distance) > EPSILON))
  {
    Attenuation = 2.0 / (1.0 + pow((photonOptions.photonDepth+depthDiff) / photonOptions.Light->Fade_Distance, photonOptions.Light->Fade_Power));
  }
  else
    Attenuation = 1;

#if 0
  VScale(LightCol2, LightCol, photonOptions.photonSpread*photonOptions.photonSpread);
  if(!photonOptions.Light->Parallel)
  {
    VScaleEq(LightCol2, (photonOptions.photonDepth+depthDiff)*(photonOptions.photonDepth+depthDiff)*Attenuation);
  }
#else
  VScale(LightCol2, LightCol, Attenuation);
  if(!photonOptions.Light->Parallel)
  {
    VScaleEq(LightCol2, (photonOptions.photonDepth+depthDiff) *
                        (photonOptions.photonDepth+depthDiff));
  }
  VScaleEq(LightCol2, photonOptions.photonSpread*photonOptions.photonSpread);
#endif

  /* if too dark, maybe we should stop here */

  /* allocate the photon */
  if(photonOptions.photonObject==NULL) return;

  Increase_Counter(stats[Number_Of_Media_Photons_Stored]);

  Photon = AllocatePhoton(&photonOptions.mediaPhotonMap);

  /* convert photon from three floats to 4 bytes */
  colour2photonRgbe(Photon->Colour, LightCol2);

  /* store the location */
  Assign_Vector(Photon->Loc, Point);

  /* now determine rotation angles */
  VSub(d,Origin, Point);
  VNormalizeEq(d);
  d_len = sqrt(d[X]*d[X]+d[Z]*d[Z]);

  phi = acos(d[X]/d_len);
  if (d[Z]<0) phi = -phi;

  theta = acos(d_len);
  if (d[Y]<0) theta = -theta;

  /* cram these rotation angles into two signed bytes */
  Photon->theta=(signed char)(theta*127.0/M_PI);
  Photon->phi=(signed char)(phi*127.0/M_PI);

}

/* ====================================================================== */
/* ====================================================================== */
/*                              KD - TREE                                 */
/* ====================================================================== */
/* ====================================================================== */

/*****************************************************************************

  FUNCTION

  swapPhotons

  swaps two photons

  Precondition:
    photon memory initialized
    static map_s points to the photon map
    'a' and 'b' are indexes within the range of photons in map_s
      (NO ERROR CHECKING IS DONE)

  Postconditions:
    the photons indexed by 'a' and 'b' are swapped

*****************************************************************************/

static void swapPhotons(int a, int b)
{
  int ai,aj,bi,bj;
  PHOTON tmp;

  /* !!!!!!!!!!! warning
     This code does the same function as the macro PHOTON_AMF
     It is done here separatly instead of using the macro for
     speed reasons (to avoid duplicate operations).  If the
     macro is changed, this MUST ALSO BE CHANGED!
  */
  ai = a & PHOTON_BLOCK_MASK;
  aj = a >> PHOTON_BLOCK_POWER;
  bi = b & PHOTON_BLOCK_MASK;
  bj = b >> PHOTON_BLOCK_POWER;

  tmp = map_s[aj][ai];
  map_s[aj][ai] = map_s[bj][bi];
  map_s[bj][bi] = tmp;
}

/*****************************************************************************

  FUNCTION

  insertSort
  (modified from Data Structures textbook)

  Preconditions:
    photon memory initialized
    static map_s points to the photon map
    'start' is the index of the first photon
    'end' is the index of the last photon
    'd' is the dimension to sort on (X, Y, or Z)

  Postconditions:
    photons from 'start' to 'end' in map_s are sorted in
    ascending order on dimension d
******************************************************************************/
static void insertSort(int start, int end, int d)
{
  int j,k;
  PHOTON tmp;

  for(k=end-1; k>=start; k--)
  {
    j=k+1;
    tmp = PHOTON_AMF(map_s, k);
    while ( (tmp.Loc[d] > PHOTON_AMF(map_s,j).Loc[d]) )
    {
      PHOTON_AMF(map_s,j-1) = PHOTON_AMF(map_s,j);
      j++;
      if (j>end) break;
    }
    PHOTON_AMF(map_s,j-1) = tmp;
  }
}

/*****************************************************************************

  FUNCTION

  quickSortRec
  (modified from Data Structures textbook)

  Recursive part of the quicksort routine
  This does not sort all the way.  once this is done, insertSort
  should be called to finish the sorting process!

  Preconditions:
    photon memory initialized
    static map_s points to the photon map
    'left' is the index of the first photon
    'right' is the index of the last photon
    'd' is the dimension to sort on (X, Y, or Z)

  Postconditions:
    photons from 'left' to 'right' in map_s are MOSTLY sorted in
    ascending order on dimension d
******************************************************************************/
static void quickSortRec(int left, int right, int d)
{
  int j,k;
  if(left<right)
  {
    swapPhotons(((left+right)>>1), left+1);
    if(PHOTON_AMF(map_s,left+1).Loc[d] > PHOTON_AMF(map_s,right).Loc[d])
      swapPhotons(left+1,right);
    if(PHOTON_AMF(map_s,left).Loc[d] > PHOTON_AMF(map_s,right).Loc[d])
      swapPhotons(left,right);
    if(PHOTON_AMF(map_s,left+1).Loc[d] > PHOTON_AMF(map_s,left).Loc[d])
      swapPhotons(left+1,left);

    j=left+1; k=right;
    while(j<=k)
    {
      for(j++; ((j<=right)&&(PHOTON_AMF(map_s,j).Loc[d]<PHOTON_AMF(map_s,left).Loc[d])); j++);
      for(k--; ((k>=left)&&(PHOTON_AMF(map_s,k).Loc[d]>PHOTON_AMF(map_s,left).Loc[d])); k--);

      if(j<k)
        swapPhotons(j,k);
    }

    swapPhotons(left,k);
    if(k-left > 10)
    {
      quickSortRec(left,k-1,d);
    }
    if(right-k > 10)
    {
      quickSortRec(k+1,right,d);
    }
    /* leave the rest for insertSort */
  }
}

/*****************************************************************************

  FUNCTION

  halfSortRec
  (modified quicksort algorithm)

  Recursive part of the quicksort routine, but it only does half
  the quicksort.  It only recurses one branch - the branch that contains
  the midpoint (median).

  Preconditions:
    photon memory initialized
    static map_s points to the photon map
    'left' is the index of the first photon
    'right' is the index of the last photon
    'd' is the dimension to sort on (X, Y, or Z)
    'mid' is the index where the median will end up

  Postconditions:
    the photon at the midpoint (mid) is the median of the photons
    when sorted on dimention d.
******************************************************************************/
static void halfSortRec(int left, int right, int d, int mid)
{
  int j,k;
  if(left<right)
  {
    swapPhotons(((left+right)>>1), left+1);
    if(PHOTON_AMF(map_s,left+1).Loc[d] > PHOTON_AMF(map_s,right).Loc[d])
      swapPhotons(left+1,right);
    if(PHOTON_AMF(map_s,left).Loc[d] > PHOTON_AMF(map_s,right).Loc[d])
      swapPhotons(left,right);
    if(PHOTON_AMF(map_s,left+1).Loc[d] > PHOTON_AMF(map_s,left).Loc[d])
      swapPhotons(left+1,left);

    j=left+1; k=right;
    while(j<=k)
    {
      for(j++; ((j<=right)&&(PHOTON_AMF(map_s,j).Loc[d]<PHOTON_AMF(map_s,left).Loc[d])); j++);
      for(k--; ((k>=left)&&(PHOTON_AMF(map_s,k).Loc[d]>PHOTON_AMF(map_s,left).Loc[d])); k--);

      if(j<k)
        swapPhotons(j,k);
    }

    /* put the pivot into its position */
    swapPhotons(left,k);

    /* only go down the side that contains the midpoint */
    /* don't do anything if the midpoint=k (the pivot, which is
       now in the correct position */
    if(k-left > 0 && (mid>=left) && (mid<k))
    {
      halfSortRec(left,k-1,d,mid);
    }
    else if(right-k > 0 && (mid>k) && (mid<=right))
    {
      halfSortRec(k+1,right,d,mid);
    }
  }
}

/*****************************************************************************

  FUNCTION

  sortAndSubdivide
  
  Finds the dimension with the greates range, sorts the photons on that
  dimension.  Then it recurses on the left and right halves (keeping
  the median photon as a pivot).  This produces a balanced kd-tree.

  Preconditions:
    photon memory initialized
    static map_s points to the photon map
    'start' is the index of the first photon
    'end' is the index of the last photon
    'sorted' is the dimension that was last sorted (so we don't sort again)

  Postconditions:
    photons from 'start' to 'end' in map_s are in a valid kd-tree format
******************************************************************************/
static void sortAndSubdivide(int start, int end, int /*sorted*/)
{
  int i,j;             /* counters */
  SNGL_VECT min,max;   /* min/max vectors for finding range */
  int DimToUse;        /* which dimesion has the greatest range */
  int mid;             /* index of median (middle) */
  int len;             /* length of the array we're sorting */

  if (end==start) 
  {
    PHOTON_AMF(map_s, start).info = 0;
    return;
  }

  if(end<start) return;

  // this seems to be a good place for this call [trf]
  Do_Cooperate(1);

  /*
   * loop and find greatest range
   */
  Make_Vector(min, 1/EPSILON, 1/EPSILON, 1/EPSILON);
  Make_Vector(max, -1/EPSILON, -1/EPSILON, -1/EPSILON);

  for(i=start; i<=end; i++)
  {
    for(j=X; j<=Z; j++)
    {
      PHOTON *ph = &(PHOTON_AMF(map_s,i));

      if (ph->Loc[j] < min[j])
        min[j]=ph->Loc[j];
      if (ph->Loc[j] > max[j])
        max[j]=ph->Loc[j];
    }
  }

  /* choose which dimension to use */
  DimToUse = X;
  if((max[Y]-min[Y])>(max[DimToUse]-min[DimToUse]))
    DimToUse=Y;
  if((max[Z]-min[Z])>(max[DimToUse]-min[DimToUse]))
    DimToUse=Z;

  /* find midpoint */
  mid = (end+start)>>1;

  /* use half of a quicksort to find the median */
  len = end-start;
  if (len>=2)
  {
    /* only display status every so often */
    if(len > 1000)
    {
        gPhotonStat_end = end;
        Send_ProgressUpdate(PROGRESS_SORTING_PHOTONS);
    }

    halfSortRec(start, end, DimToUse, mid);
    //quickSortRec(start, end, DimToUse);
  }

  /* set DimToUse for the midpoint */
  PHOTON_AMF(map_s, mid).info = DimToUse;

  /* now recurse to continue building the kd-tree */
  sortAndSubdivide(start, mid - 1, DimToUse);
  sortAndSubdivide(mid + 1, end, DimToUse);
}

/*****************************************************************************

  FUNCTION

  buildTree
  
  Builds the kd-tree by calling sortAndSubdivide().  Sets the static
  variable map_s.

  Preconditions:
    photon memory initialized
    'map' is a pointer to a photon map containing an array of unsorted
         photons

  Postconditions:
    photons are in a valid kd-tree format
******************************************************************************/
static void buildTree(PHOTON_MAP *map)
{
  Send_Progress("Sorting photons", PROGRESS_SORTING_PHOTONS);
  map_s = map->head;
  sortAndSubdivide(0,map->numPhotons-1,X+Y+Z/*this is not X, Y, or Z*/);
}

/*****************************************************************************

  FUNCTION

  setGatherOptions
  
  determines gather options

  Preconditions:
    photon memory initialized
    'map' points to an already-built (and sorted) photon map
    'mediaMap' is true if 'map' contians media photons, and false if
         'map' contains surface photons

  Postconditions:
    gather gather options are set for this map
******************************************************************************/
static void setGatherOptions(PHOTON_MAP *map, int mediaMap)
{
  DBL r;
  DBL density;
  VECTOR Point;
  int numToSample;
  int n,i,j;
  DBL mind,maxd,avgd;
  DBL sum,sum2;
  DBL saveDensity;
  //int greaterThan;
  int lessThan;

  /* if user did not set minimum gather radius, 
    then we should calculate it */
  if (map->minGatherRad <= 0.0)
  {
    mind=10000000.0;
    maxd=avgd=sum=sum2=0.0;

    /* use 5% of photons, min 100, max 10000 */
    numToSample = map->numPhotons/20;
    if (numToSample>1000) numToSample = 1000;
    if (numToSample<100) numToSample = 100;

    for(i=0; i<numToSample; i++)
    {
      j = rand() % map->numPhotons;

      Assign_Vector(Point,(PHOTON_AMF(map->head, j)).Loc);

      n=gatherPhotons(Point, 10000000.0, &r, NULL, false, map);

      if(mediaMap)
        density = 3.0 * n / (4.0*M_PI*r*r*r); /* should that be 4/3? */
      else
        density = n / (M_PI*r*r);


      if (density>maxd) maxd=density;
      if (density<mind) mind=density;
      sum+=density;
      sum2+=density*density;
    }
    avgd = sum/numToSample;

    /* try using some standard deviation stuff instead of this */
    saveDensity = avgd;
/*
    greaterThan = 0;
    for(i=0; i<numToSample; i++)
    {
      j = rand() % map->numPhotons;

      Assign_Vector(Point,(PHOTON_AMF(map->head, j)).Loc);

      n=gatherPhotons(Point, 10000000.0, &r, NULL, false, map);

      if(mediaMap)
        density = 3.0 * n / (4.0*M_PI*r*r*r); // should that be 4/3?
      else
        density = n / (M_PI*r*r);

      if (density>saveDensity)
        greaterThan++;
    }

    density = saveDensity * (DBL)greaterThan / numToSample;
*/
    density = saveDensity;

    if(mediaMap)
    {
      map->minGatherRad = pow(3.0 * photonOptions.maxGatherCount / (density*M_PI*4.0), 0.3333);
    }
    else
      map->minGatherRad = sqrt(photonOptions.maxGatherCount / (density*M_PI));

    lessThan = 0;
    for(i=0; i<numToSample; i++)
    {
      j = rand() % map->numPhotons;

      Assign_Vector(Point,(PHOTON_AMF(map->head, j)).Loc);

      n=gatherPhotons(Point, map->minGatherRad, &r, NULL, false, map);

      if(mediaMap)
        density = 3.0 * n / (4.0*M_PI*r*r*r); // should that be 4/3?
      else
        density = n / (M_PI*r*r);

      // count as "lessThan" if the density is below 70% of the average,
      // and if it is at least above 5% of the average.
      if (density<(saveDensity*0.7) && density>(saveDensity*0.05))
        lessThan++;
    }

    // the 30.0 is a bit of a fudge-factor.
    map->minGatherRad*=(1.0+20.0*((DBL)lessThan/(numToSample)));

  }

  // Now apply the user-defined multiplier, so that the user can tell
  // POV to take shortcuts which will improve speed at the expensive of
  // quality.  Alternatively, the user could tell POV to use a bigger
  // radius to improve quality.
  map->minGatherRad *= map->minGatherRadMult;

  if(mediaMap)
  {
    /* double the radius if it is a media map */
    map->minGatherRad *= 2;
  }

  /* always do this! - use 6 times the area */
  map->gatherRadStep = map->minGatherRad*2;

  /* somehow we could automatically determine the number of steps */
}


/**************************************************************

  =========== PRIORITY QUEUES ===============
  Each priority stores its data in the static variables below (such as
  numfound_s) and in the global variables 

  Preconditions:

  static DBL size_sq_s; - maximum radius given squared
  static DBL Size_s;  - radius
  static DBL dmax_s;    - square of radius used so far
  static int TargetNum_s; - target number
  static DBL *pt_s;       - center point
  static numfound_s;        - number of photons in priority queue

  these must be allocated:
    photonOptions.photonGatherList - array of photons in priority queue
    photonOptions.photonDistances  - corresponding priorities(distances)

  *Each priority queue has the following functions:

  function PQInsert(PHOTON *photon, DBL d)

    Inserts 'photon' into the priority queue with priority (distance
    from target point) 'd'.

  void PQDelMax()
  
    Removes the photon with the greates distance (highest priority)
    from the queue.

********************************************************************/

/* try different priority queues */

#define ORDERED   0
#define UNORDERED 1
#define HEAP      2

#define PRI_QUE HEAP

/* -------------- ordered list implementation ----------------- */
#if (PRI_QUE == ORDERED)
static void PQInsert(PHOTON *photon, DBL d)
{
  int i,j;

  Increase_Counter(stats[Priority_Queue_Add]);
  /* save this in order, remove maximum, save new dmax_s */

  /* store in array and shift - assumption is that we don't have
     to shift often */
  for (i=0; photonOptions.photonDistances[i]<d && i<(numfound_s); i++);
  for (j=numfound_s; j>i; j--)
  {
    photonOptions.photonGatherList[j] = photonOptions.photonGatherList[j-1];
    photonOptions.photonDistances[j] = photonOptions.photonDistances[j-1];
  }

  numfound_s++;
  photonOptions.photonGatherList[i] = photon;
  photonOptions.photonDistances[i] = d;
  if (numfound_s==TargetNum_s)
    dmax_s=photonOptions.photonDistances[numfound_s-1];

}

static void PQDelMax()
{
  Increase_Counter(stats[Priority_Queue_Remove]);
  numfound_s--;
}
#endif

/* -------------- unordered list implementation ----------------- */
#if (PRI_QUE == UNORDERED)
static void PQInsert(PHOTON *photon, DBL d)
{
  Increase_Counter(stats[Priority_Queue_Add]);

  photonOptions.photonGatherList[numfound_s] = photon;
  photonOptions.photonDistances[numfound_s] = d;

  if (d>dmax_s)
    dmax_s=d;

  numfound_s++;
}

static void PQDelMax()
{
  int i,max;

  Increase_Counter(stats[Priority_Queue_Remove]);

  max=0;
  /* find max */
  for(i=1; i<numfound_s; i++)
    if (photonOptions.photonDistances[i]>photonOptions.photonDistances[max]) max = i;

  /* remove it, shifting the photons */
  for(i=max+1; i<numfound_s; i++)
  {
    photonOptions.photonGatherList[i-1] = photonOptions.photonGatherList[i];
    photonOptions.photonDistances[i-1] = photonOptions.photonDistances[i];
  }

  numfound_s--;

  /* find a new dmax_s */
  dmax_s=photonOptions.photonDistances[0];
  for(i=1; i<numfound_s; i++)
    if (photonOptions.photonDistances[i]>dmax_s) dmax_s = photonOptions.photonDistances[i];
}
#endif

/* -------------- heap implementation ----------------- */
/* this is from Sejwick (spelling?) */
#if (PRI_QUE == HEAP)

static void PQInsert(PHOTON *photon, DBL d)
{
  Increase_Counter(stats[Priority_Queue_Add]);
  DBL* Distances = photonOptions.photonDistances;
  PHOTON** Photons = photonOptions.photonGatherList;

  unsigned int k = ++numfound_s;

  while (k > 1)
  {
    unsigned int half_k = k / 2;
    DBL d_half_k_m1 = Distances[half_k - 1];

    if (d < d_half_k_m1)
      break;

    Distances [k - 1] = d_half_k_m1;
    Photons[k - 1] = Photons[half_k - 1];

    k = half_k;
  }

  Distances [k - 1] = d;
  Photons[k - 1] = photon;
}

static void FullPQInsert(PHOTON *photon, DBL d)
{
  Increase_Counter(stats[Priority_Queue_Remove]);
  DBL* Distances = photonOptions.photonDistances;
  PHOTON** Photons = photonOptions.photonGatherList;

  int k = 1, k2 = 2;
  for (; k2 < numfound_s; k = k2, k2 += k)
  {
    DBL d_k2_m1 = Distances[k2 - 1],
        d_k2 = Distances[k2];

    if (d_k2 > d_k2_m1)
    {
      d_k2_m1 = d_k2;
      ++k2;
    }

    if (!(d_k2_m1 > d))
      break;
    
    Distances [k - 1] = d_k2_m1;
    Photons[k - 1] = Photons[k2 - 1];
  }

  if (k2 == numfound_s) {
    DBL d_k2_m1 = Distances[k2 - 1];
    if (d_k2_m1 > d) {
      Distances [k - 1] = d_k2_m1;
      Photons[k - 1] = Photons[k2 - 1];
      k = k2;
    }
  }

  Distances [k - 1] = d;
  Photons[k - 1] = photon;

  /* find a new dmax_s */
  dmax_s = Distances[0];
}

#endif

/*****************************************************************************

  FUNCTION

  gatherPhotonsRec()

  Recursive part of gatherPhotons
  Searches the kd-tree with range start..end (midpoint is pivot)
  
  Preconditions:
    same preconditions as priority queue functions
    static variable map_s points to the map to use
    'start' is the first photon in this range
    'end' is the last photon in this range

    the range 'start..end' must have been used in building photon map!!!

  Postconditions:
    photons within the range of start..end are added to the priority
    queue (photons may be delted from the queue to make room for photons
    of lower priority)
 
******************************************************************************/

static void gatherPhotonsRec(int start, int end)
{
  DBL delta;
  int DimToUse;
  DBL d,dx,dy,dz;
  int mid;
  PHOTON *photon;
  VECTOR ptToPhoton;
  DBL discFix;   /* use disc(ellipsoid) for gathering instead of sphere */

  /* find midpoint */
  mid = (end+start)>>1;
  photon = &(PHOTON_AMF(map_s, mid));

  DimToUse = photon->info & PH_MASK_XYZ;

  /*
   * check this photon
   */

  /* find distance from pt */
  ptToPhoton[X] = - pt_s[X] + photon->Loc[X];
  ptToPhoton[Y] = - pt_s[Y] + photon->Loc[Y];
  ptToPhoton[Z] = - pt_s[Z] + photon->Loc[Z];
  /* all distances are squared */
  dx = ptToPhoton[X]*ptToPhoton[X];
  dy = ptToPhoton[Y]*ptToPhoton[Y];
  dz = ptToPhoton[Z]*ptToPhoton[Z];

  if (!(  ((dx>dmax_s) && ((DimToUse)==X)) ||
          ((dy>dmax_s) && ((DimToUse)==Y)) ||
          ((dz>dmax_s) && ((DimToUse)==Z)) ))
  {
    /* it fits manhatten distance - maybe we can use this photon */

    /* find euclidian distance (squared) */
    d = dx + dy + dz;

    /* now fix this distance so that we gather using an ellipsoid
       alligned with the surface normal instead of a sphere.  This
       minimizes false bleeding of photons at sharp corners

       dmax_s is square of radius of major axis
       dmax_s/16 is  "   "   "     " minor  "    (1/6 of major axis)
     */
    /*
    VDot(discFix,norm_s,ptToPhoton);
    discFix*=discFix*(dmax_s/1000.0-dmax_s);
    */
    
    if (flattenFactor!=0.0)
    {
      VDot(discFix,norm_s,ptToPhoton);
      discFix = fabs(discFix);
      d += flattenFactor*(discFix)*d*16;
    }
    /* this will add zero if on the plane, and will double distance from
    point to photon if it is ptToPhoton is perpendicular to the surface */

    if(d < dmax_s)
    {
      if (numfound_s+1>TargetNum_s)
      { 
        FullPQInsert(photon, d); 
        sqrt_dmax_s = sqrt(dmax_s);
      }
      else
        PQInsert(photon, d);
    }
  }

  /* now go left & right if appropriate - if going left or right goes out
      the current range, then don't go that way. */
  /*
  delta=pt_s[DimToUse]-photon->Loc[DimToUse];
  if(delta<0)
  {
    if (end>=mid+1) gatherPhotonsRec(start, mid - 1);
    if (delta*delta < dmax_s )
      if (mid-1>=start) gatherPhotonsRec(mid + 1, end);
  }
  else
  {
    if (mid-1>=start) gatherPhotonsRec(mid+1,end);
    if (delta*delta < dmax_s )
      if (end>=mid+1) gatherPhotonsRec(start, mid - 1);
  }
  */
  delta=pt_s[DimToUse]-photon->Loc[DimToUse];
  if(delta<0)
  {
    /* on left - go left first */
    if (pt_s[DimToUse]-sqrt_dmax_s < photon->Loc[DimToUse])
    {
      if (mid-1>=start)
        gatherPhotonsRec(start, mid - 1);
    }
    if (pt_s[DimToUse]+sqrt_dmax_s > photon->Loc[DimToUse])
    {
      if(end>=mid+1)
        gatherPhotonsRec(mid + 1, end);
    }
  }
  else
  {
    /* on right - go right first */
    if (pt_s[DimToUse]+sqrt_dmax_s > photon->Loc[DimToUse])
    {
      if(end>=mid+1)
        gatherPhotonsRec(mid + 1, end);
    }
    if (pt_s[DimToUse]-sqrt_dmax_s < photon->Loc[DimToUse])
    {
      if (mid-1>=start)
        gatherPhotonsRec(start, mid - 1);
    }
  }
}

/*****************************************************************************

  FUNCTION

  gatherPhotons()

  gathers photons from the global photon map

  Preconditons:

    photonOptions.photonGatherList and photonOptions.photonDistances
      are allocated and are each maxGatherCount in length

    'Size' - maximum search radius
    'r' points to a double

    BuildPhotonMaps() has been called for this scene.

  Postconditions:

    *r is radius actually used for gathereing (maximum value is 'Size')
    photonOptions.photonGatherList and photonOptions.photonDistances
      contain the gathered photons
    returns number of photons gathered

******************************************************************************/

int gatherPhotons(VECTOR pt, DBL Size, DBL *r, VECTOR norm, int flatten, PHOTON_MAP *map)
{
  if (map->numPhotons<=0) return 0; /* no crashes, please... */

  /* set the static variables */
  numfound_s=0;
  size_sq_s = Size*Size;
  dmax_s = size_sq_s;
  sqrt_dmax_s = Size;
  norm_s = norm;

  if(flatten)
  {
    flattenFactor = 1.0;
  }
  else
  {
    flattenFactor = 0.0;
  }

  Size_s = Size;
  TargetNum_s = photonOptions.maxGatherCount;
  pt_s = pt;

  map_s = map->head;

  /* now search the kd-tree recursively */
  gatherPhotonsRec(0, map->numPhotons-1);

  /* set the radius variable */
  *r = sqrt_dmax_s;

  /* return the number of photons found */
  return(numfound_s);
}


/******************************************************************
stuff grabbed from radiosit.h & radiosit.c
******************************************************************/

extern const BYTE_XYZ rad_samples[];

static void VUnpack(VECTOR dest_vec, const BYTE_XYZ * pack_vec)
{
  dest_vec[X] = ((double)pack_vec->x * (1./ 255.))*2.-1.;
  dest_vec[Y] = ((double)pack_vec->y * (1./ 255.))*2.-1.;
  dest_vec[Z] = ((double)pack_vec->z * (1./ 255.));

  VNormalizeEq(dest_vec);   /* already good to about 1%, but we can do better */
}

/******************************************************************
******************************************************************/
void ChooseRay(RAY *NewRay, VECTOR Normal, RAY * /*Ray*/, VECTOR Raw_Normal, int WhichRay)
{
  VECTOR random_vec, up, n2, n3;
  int i;
  DBL /*n,*/ NRay_Direction;

#define REFLECT_FOR_RADIANCE 0
#if (REFLECT_FOR_RADIANCE)
  /* Get direction of reflected ray. */
  n = -2.0 * (Ray->Direction[X] * Normal[X] + Ray->Direction[Y] * Normal[Y] + Ray->Direction[Z] * Normal[Z]);

  VLinComb2(NewRay->Direction, n, Normal, 1.0, Ray->Direction);

  VDot(NRay_Direction, NewRay->Direction, Raw_Normal);
  if (NRay_Direction < 0.0)
  {
    /* subtract 2*(projection of NRay.Direction onto Raw_Normal)
       from NRay.Direction */
    DBL Proj;
    Proj = NRay_Direction * -2;
    VAddScaledEq(NewRay->Direction, Proj, Raw_Normal);
  }
  return;
#else
  Assign_Vector(NewRay->Direction, Normal);
#endif

  if ( fabs(fabs(NewRay->Direction[Z])- 1.) < .1 ) {
    /* too close to vertical for comfort, so use cross product with horizon */
    up[X] = 0.; up[Y] = 1.; up[Z] = 0.;
  }
  else
  {
    up[X] = 0.; up[Y] = 0.; up[Z] = 1.;
  }

  VCross(n2, NewRay->Direction, up);  VNormalizeEq(n2);
  VCross(n3, NewRay->Direction, n2);  VNormalizeEq(n3);

  /*i = (int)(FRAND()*1600);*/
  i = WhichRay;
  WhichRay = (WhichRay + 1) % 1600;

  VUnpack(random_vec, &rad_samples[i]);

  if ( fabs(NewRay->Direction[Z] - 1.) < .001 )         /* pretty well straight Z, folks */
  {
    /* we are within 1/20 degree of pointing in the Z axis. */
    /* use all vectors as is--they're precomputed this way */
    Assign_Vector(NewRay->Direction, random_vec);
  }
  else
  {
    NewRay->Direction[X] = n2[X]*random_vec[X] + n3[X]*random_vec[Y] + NewRay->Direction[X]*random_vec[Z];
    NewRay->Direction[Y] = n2[Y]*random_vec[X] + n3[Y]*random_vec[Y] + NewRay->Direction[Y]*random_vec[Z];
    NewRay->Direction[Z] = n2[Z]*random_vec[X] + n3[Z]*random_vec[Y] + NewRay->Direction[Z]*random_vec[Z];
  }

  /* if our new ray goes through, flip it back across raw_normal */

  VDot(NRay_Direction, NewRay->Direction, Raw_Normal);
  if (NRay_Direction < 0.0)
  {
    /* subtract 2*(projection of NRay.Direction onto Raw_Normal)
       from NRay.Direction */
    DBL Proj;
    Proj = NRay_Direction * -2;
    VAddScaledEq(NewRay->Direction, Proj, Raw_Normal);
  }

  VNormalizeEq(NewRay->Direction);
}

int GetPhotonStat(POVMSType a)
{
	switch(a)
	{
		case kPOVAttrib_ObjectPhotonCount:
			return gPhotonStat_i;
		case kPOVAttrib_TotalPhotonCount:
			return photonOptions.photonMap.numPhotons;
		case kPOVAttrib_MediaPhotonCount:
			return photonOptions.mediaPhotonMap.numPhotons;
		case kPOVAttrib_PhotonXSamples:
			return gPhotonStat_x_samples;
		case kPOVAttrib_PhotonYSamples:
			return gPhotonStat_y_samples;
		case kPOVAttrib_CurrentPhotonCount:
			return gPhotonStat_end;
	}

	return 0;
}

END_POV_NAMESPACE
