/****************************************************************************
 *                  lighting.cpp
 *
 * This module calculates lighting properties like ambient, diffuse, specular,
 * reflection, refraction, etc.
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
 * $File: //depot/povray/3.6-release/source/lighting.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "vector.h"
#include "blob.h"
#include "bbox.h"
#include "colour.h"
#include "image.h"
#include "interior.h"
#include "lbuffer.h"
#include "lighting.h"
#include "media.h"
#include "mesh.h"
#include "normal.h"
#include "objects.h"
#include "octree.h"
#include "pattern.h"  /* [CEY 10/94] */
#include "pigment.h"
#include "povray.h"
#include "radiosit.h"
#include "ray.h"
#include "render.h"
#include "texture.h"
#include "lightgrp.h"
#include "photons.h"

#include <algorithm>

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

/*
 * "Small_Tolerance" is just too tight for higher order polynomial equations.
 * this value should probably be a variable of some sort, but for now just
 * use a reasonably small value.  If people render real small objects real
 * close to each other then there may be some shading problems.  Otherwise
 * having SHADOW_TOLERANCE as large as this won't affect images.
 */

#define SHADOW_TOLERANCE 1.0e-3

/* Number of inital entries in the texture and weight list. */

const int NUMBER_OF_ENTRIES = 16;

const int MAX_NESTED_TEXTURES = 100;

const int MAX_LAYERS = 100;

const int DEFAULT_LIGHT_BUF_DEPTH = 20;
const int DEFAULT_MEDIA_BUF_DEPTH =  5;

/*****************************************************************************
* Local typedefs
******************************************************************************/

typedef struct ComTexData_Struct ComTexData;

struct ComTexData_Struct
{
  ComTexData *previous;
  ComTexData *next;
  DBL    ListWeight[MAX_LAYERS];
  VECTOR ListNormal[MAX_LAYERS];
  COLOUR ListReflec[MAX_LAYERS];
  SNGL   ListReflEx[MAX_LAYERS];
};


/*****************************************************************************
* Local variables
******************************************************************************/

TEXTURE **warpNormalTextureList = NULL; // GLOBAL VARIABLE

int warpNormalTextureListMaxSize = 0; // GLOBAL VARIABLE

// Photon mapping
COLOUR GFilCol; /* not thread safe */ // GLOBAL VARIABLE

LIGHT_TESTED *Light_List; // GLOBAL VARIABLE
TEXTURE **Texture_List; // GLOBAL VARIABLE
DBL *Weight_List; // GLOBAL VARIABLE

int Number_Of_Textures_And_Weights; // GLOBAL VARIABLE


/* static variables for multi-layer speedups */
/* this is a hack and should be done in a cleaner way in the future */
/* NOT THREAD SAFE */
int photonsAlreadyGathered; // GLOBAL VARIABLE
DBL previousRad; // GLOBAL VARIABLE

/* ------- cache sizes for statistics gathering ------- */
long MediaMallocPoolSize=0; // GLOBAL VARIABLE
/* ------- Pools for DetermineApparantColour ------- */
long MaxLightPoolDepth = 0; // GLOBAL VARIABLE
long LightingPoolIndex = -1; // GLOBAL VARIABLE

DBL **WeightListPool = NULL;          /* pool weight list - each is fixed size */ // GLOBAL VARIABLE
TEXTURE ***TextureListPool = NULL;    /* pool texture list - each is fixed size */ // GLOBAL VARIABLE
LIGHT_TESTED **LightListPool = NULL;  /* pool for light_list - each is fixed size */ // GLOBAL VARIABLE

/* pool for media list in shadow calculations */
long ShadowMediaListIndex=-1; // GLOBAL VARIABLE
IMEDIA ***ShadowMediaListPool = NULL; // GLOBAL VARIABLE
long *ShadowMediaListPoolSize = NULL; // GLOBAL VARIABLE

/* pool for media list in lighting calculations */
long LightingMediaListIndex=-1; // GLOBAL VARIABLE
IMEDIA ***LightingMediaListPool = NULL; // GLOBAL VARIABLE
long *LightingMediaListPoolSize = NULL; // GLOBAL VARIABLE

/* pool for compute_lighted_texture and compute_backtrace_texture */
ComTexData *ComputeTextureUsedPool = NULL; // GLOBAL VARIABLE
ComTexData *ComputeTextureFreePool = NULL; // GLOBAL VARIABLE
int ComputeTexturePoolSize = 0; // GLOBAL VARIABLE

/*****************************************************************************
* Global variables
******************************************************************************/

int warpNormalTextures; // GLOBAL VARIABLE
/* Global vars to remember which colour component is being traced */

int disp_elem;        /* 0=normal, 1..nelems=we're tracing elements */ // GLOBAL VARIABLE
int disp_nelems; // GLOBAL VARIABLE

/* ------- cache sizes for statistics gathering ------- */
long TotalMallocPoolSize=0; // GLOBAL VARIABLE

/* ------- Pools for media calculations ------- */
long MaxMediaPoolDepth = 0; // GLOBAL VARIABLE

/* caches for all & lit intervals */
long MediaPoolIndex=-1; // GLOBAL VARIABLE
LIGHT_LIST **MediaLightListPool = NULL;     /* cache for light_list - each is fixed size */ // GLOBAL VARIABLE
LIT_INTERVAL **MediaLitIntervalPool = NULL; /* cache for lit_interval - each is fixed size */ // GLOBAL VARIABLE
MEDIA_INTERVAL **MediaIntervalPool = NULL;  /* cache for media_interval */ // GLOBAL VARIABLE
long *MediaIntervalPoolSize = NULL;         /* interval array sizes vary */ // GLOBAL VARIABLE


/* s0 and s1 - always the same size - just malloc once and keep the arrays */
DBL *s0 = NULL; // GLOBAL VARIABLE
DBL *s1 = NULL; // GLOBAL VARIABLE

#ifdef AccumulatePoolStatistics
long MaxAppColourRecCntr=-1; // GLOBAL VARIABLE
long MaxSimMediatRecCntr=-1; // GLOBAL VARIABLE
long MaxShadowTextRecCntr=-1; // GLOBAL VARIABLE
long MaxLightedTexture=-1; // GLOBAL VARIABLE
#endif

/*****************************************************************************
* Static functions
******************************************************************************/
/* ------- cache init / reinit / deinit -------- */
static void InitMallocPools(void);
static void DeInitMallocPools(void);
static void ReInitMallocPools(void);

static void do_light_area_light(LIGHT_SOURCE *Light_Source, DBL *Light_Source_Depth, RAY *Light_Source_Ray, RAY  *Eye_Ray, VECTOR IPoint);

static void block_area_light (LIGHT_SOURCE *Light_Source,
  DBL *Light_Source_Depth, RAY *Light_Source_Ray, RAY *Eye_Ray,
  VECTOR IPoint, COLOUR Light_Colour, int u1, int v1, int u2, int v2, int Level);

static void block_point_light (LIGHT_SOURCE *Light_Source,
  DBL *Light_Source_Depth, RAY *Light_Source_Ray, COLOUR Light_Colour);

static void block_point_light_LBuffer (LIGHT_SOURCE *Light_Source,
  DBL *Light_Source_Depth, RAY *Light_Source_Ray, COLOUR Light_Colour);

static void do_light (LIGHT_SOURCE *Light_Source,
  DBL *Light_Source_Depth, RAY *Light_Source_Ray, RAY *Eye_Ray, VECTOR IPoint,
  COLOUR Light_Colour);

static int do_blocking (INTERSECTION *Local_Intersection,
  RAY *Light_Source_Ray, COLOUR Light_Colour, ISTACK *Local_Stack);

static void do_irid (FINISH *Finish, RAY *Light_Source_Ray,
  VECTOR Layer_Normal, VECTOR IPoint, COLOUR Colour);

static void Diffuse (FINISH *Finish, VECTOR IPoint, RAY *Eye, VECTOR Layer_Normal,
  COLOUR Layer_Pigment_Colour, COLOUR Colour,DBL Attenuation, OBJECT *Object);

static void Diffuse_One_Light (LIGHT_SOURCE *Light_Source, int Light_Index,
  VECTOR REye, FINISH *Finish, VECTOR IPoint, RAY *Eye, VECTOR  Layer_Normal,
  COLOUR Layer_Pigment_Colour, COLOUR Colour, DBL Attenuation, OBJECT *Object);

static void Reflect (VECTOR, RAY*, VECTOR, VECTOR, COLOUR, DBL, DBL, int);

static int Refract (INTERIOR*, VECTOR, RAY*, VECTOR, VECTOR, COLOUR, DBL);

static void filter_shadow_ray (INTERSECTION *Ray_Intersection,
  RAY *Light_Source_Ray, COLOUR Colour);

static int create_texture_list (INTERSECTION *Ray_Intersection, DBL realNormDir);

static void do_texture_map (COLOUR Result_Colour,
  TEXTURE *Texture, VECTOR IPoint, VECTOR Raw_Normal, RAY *Ray, DBL Weight,
  INTERSECTION *Ray_Intersection, int Shadow_Flag);

static void average_textures (COLOUR Result_Colour,
  TEXTURE *Texture, VECTOR IPoint, VECTOR Raw_Normal, RAY *Ray, DBL Weight,
  INTERSECTION *Ray_Intersection, int Shadow_Flag);

static void compute_lighted_texture (COLOUR Result_Colour,
  TEXTURE *Texture, VECTOR IPoint, VECTOR Raw_Normal, RAY *Ray, DBL Weight,
  INTERSECTION *Ray_Intersection);

static void compute_shadow_texture (COLOUR Filter_Colour,
  TEXTURE *Texture, VECTOR IPoint, VECTOR Raw_Normal, RAY *Ray,
  INTERSECTION *Ray_Intersection);

static void block_light_source (LIGHT_SOURCE *Light,
  DBL Depth, RAY *Light_Source_Ray, RAY *Eye_Ray, VECTOR P, COLOUR Colour);

static void do_light_ray_atmosphere (RAY *Light_Source_Ray,
  INTERSECTION *Ray_Intersection, COLOUR Colour, int Valid_Object);

static void determine_reflectivity (DBL *weight,
  RGB reflectivity, COLOUR Reflection_Max, COLOUR Reflection_Min,
  int Reflection_Type, DBL Reflection_Falloff, DBL cos_angle, RAY *Ray, INTERIOR *Interior);

static void compute_backtrace_texture(COLOUR LightCol, 
  TEXTURE *Texture, VECTOR IPoint, VECTOR  Raw_Normal, RAY *Ray, DBL Weight,
  INTERSECTION *Intersection);

static void backtrace_average_textures (COLOUR Result_Colour,
  TEXTURE *Texture, VECTOR IPoint, VECTOR  Raw_Normal, RAY *Ray, DBL Weight,
  INTERSECTION *Ray_Intersection, int Shadow_Flag);

#ifdef GLOBAL_PHOTONS
static void GlobalPhotonDiffuse (FINISH *Finish, VECTOR IPoint, RAY *Eye, VECTOR  Layer_Normal, COLOUR Layer_Pigment_Colour, COLOUR Colour, DBL Attenuation, OBJECT *Object);
#endif

static void Dispersion_Element_Hue(COLOUR Hue, int elem, int nelems);

static DBL Element_IOR(DBL ior, DBL disp,  int e, int nelems);

void InitComTexDataPool();
void FreeComTexDataPool();
ComTexData *NewComTexData();
void FreeComTexData(ComTexData *ctd);


/*****************************************************************************
*
* FUNCTION
*
*   Initialize_Lighting_Code
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
*   Allocate lists needed during lighting calculations.
*
* CHANGES
*
*   Sep 1994 : Creation.
*
*   Okt 1994 : Added initialization of Light_List and test if there are
*              any light sources in the scene. [DB]
*
******************************************************************************/

void Initialize_Lighting_Code()
{
  Light_List = NULL;
  Texture_List = NULL;
  Weight_List  = NULL;

  Number_Of_Textures_And_Weights = NUMBER_OF_ENTRIES;

  InitMallocPools();
  InitComTexDataPool();

  LightingPoolIndex++;
  Light_List=LightListPool[LightingPoolIndex];
  Texture_List = TextureListPool[LightingPoolIndex];
  Weight_List = WeightListPool[LightingPoolIndex];

  warpNormalTextureListMaxSize = MAX_NESTED_TEXTURES;
  warpNormalTextureList = (TEXTURE **)POV_MALLOC(warpNormalTextureListMaxSize * sizeof(TEXTURE *), "Warp normal texture list");

  /* This has nothing to do with light lists but I needed to put it
     somewhere that would get called before each rendering.
   */
  disp_elem = 0;
}



/*****************************************************************************
*
* FUNCTION
*
*   Reinitialize_Lighting_Code
*
* INPUT
*
*   Number_Of_Entries - New number of entries in the texture/weight lists
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
*   Resize variable lists needed during lighting calculation.
*
* CHANGES
*
*   Jul 1995 : Creation.
*
*   Mar 1996 : We have to pass pointers to the lists to resize because during
*              resizing the pointers to the lists change and thus the calling
*              functions does not longer know where the lists are if the
*              pointers to the lists where passed to it using arguments. [DB]
*
******************************************************************************/

void Reinitialize_Lighting_Code(int Number_Of_Entries, TEXTURE ***Textures, DBL **Weights)
{
  if (Number_Of_Entries > Number_Of_Textures_And_Weights)
  {
    if (Number_Of_Entries >= INT_MAX / 2)
    {
      Error("Too many entries in texture and weight lists.");
    }

    Number_Of_Textures_And_Weights = Number_Of_Entries;

    ReInitMallocPools();
    *Textures = Texture_List = TextureListPool[LightingPoolIndex];
    *Weights = Weight_List = WeightListPool[LightingPoolIndex];
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Deinitialize_Lighting_Code
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
*   Destroy all lists needed during lighting calculation.
*
* CHANGES
*
*   Sep 1994 : Creation.
*
*   Jul 1995 : Added code to free local texture and weight lists. [DB]
*
******************************************************************************/

void Deinitialize_Lighting_Code()
{
  DeInitMallocPools();
  FreeComTexDataPool();

  if(warpNormalTextureList != NULL)
    POV_FREE(warpNormalTextureList);
  warpNormalTextureList = NULL;
  warpNormalTextureListMaxSize = 0;

  /*MH If a render is interrupted, this is set greater than one 
  and dispersion would stop working if not given a value of 0*/

  if (disp_elem != 0) 
  {
   disp_elem = 0;
  }

  Light_List   = NULL;
  Texture_List = NULL;
  Weight_List  = NULL;
}



/*****************************************************************************
*
* FUNCTION
*
*   Determine_Apparent_Colour
*
* INPUT
*
*   Ray_Intersection - info on where ray hit & object it hit
*   Ray              - the ray from which object is seen
*   Weight           - Automatic depth control value
*
* OUTPUT
*
*   Colour           - resulting color is added to given color. The RGB
*                      components are significant. The transmittance channel
*                      is used as an alpha channel.
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   Given an intersection point, a ray, add that point's visible color
*   to the given colour and return it.  This routine just does preliminary
*   initializations and calls to set up the multi-texture blob list if any.
*   Then it calls do_texture_map which in turn calls compute_lighted_texture
*   to do the actual lighting calculations.  These functions were seperated
*   from this function because do_texture_map may need to call itself
*   recursively.
*
* CHANGES
*
*   Sep 1994 : Code for multi-textured blobs added. [DB]
*
*   Nov 1994 : Moved calls to Fog and Rainbow into tracing functions. [DB]
*
*   Jan 1995 : Moved much of code to do_texture_map and
*              compute_lighted_texture [CEY]
*
*   Jul 1995 : Added code to support alpha channel. [DB]
*
*   Mar 1996 : Fixed severe bug (weight and texture lists were not saved) [DB]
*
******************************************************************************/

void Determine_Apparent_Colour(INTERSECTION *Ray_Intersection, COLOUR Colour, RAY *Ray, DBL Weight)
{
  int i, Texture_Count;
  DBL *save_Weights = NULL;
  DBL Normal_Direction;
  COLOUR C1;
  VECTOR Raw_Normal;
  VECTOR IPoint;
  UV_VECT UV_Coords;
  TEXTURE **save_Textures = NULL;
  TEXTURE *Texture;
  LIGHT_TESTED *savelights = NULL;


  Total_Depth += Ray_Intersection->Depth;

  Assign_Vector(IPoint,Ray_Intersection->IPoint);

  /*
   * Save existing light list if any. If we are not top level in recursion
   * depth, this information may be reused by upper level of trace.
   */

  LightingPoolIndex++; 
  if ( LightingPoolIndex >= MaxLightPoolDepth) 
  {
    ResizeLightMallocPools(MaxLightPoolDepth*2);
  }

#ifdef AccumulatePoolStatistics
  MaxAppColourRecCntr=max(MaxAppColourRecCntr,LightingPoolIndex);
#endif

  Light_List=LightListPool[LightingPoolIndex];

  /* Init light list. */

  for (i = 0; i < Frame.Number_Of_Light_Sources; i++)
  {
    Light_List[i].Tested = false;
  }

  /* Get the normal to the surface */

  Normal(Raw_Normal, Ray_Intersection->Object, Ray_Intersection);
  /* I added this to flip the normal if the object is inverted (for CSG).
     However, I subsequently commented it out for speed reasons - it doesn't
     make a difference (no pun intended). The preexisting flip code below
     produces a similar (though more extensive) result. [NK]

     Actually, we should keep this code to guarantee that Normal_Direction
     is set properly. [NK]
  */
  if (Test_Flag(Ray_Intersection->Object,INVERTED_FLAG))
  {
    Raw_Normal[X] = -Raw_Normal[X];
    Raw_Normal[Y] = -Raw_Normal[Y];
    Raw_Normal[Z] = -Raw_Normal[Z];
  }
  

  /* If the surface normal points away, flip its direction. */
  VDot(Normal_Direction, Raw_Normal, Ray->Direction);
  if (Normal_Direction > 0.0)
  {
    VScaleEq(Raw_Normal, -1.0);
  }

  Assign_Vector(Ray_Intersection->INormal, Raw_Normal);
  /* and save to intersection -hdf- */
  Assign_Vector(Ray_Intersection->PNormal, Raw_Normal);

  if (Test_Flag(Ray_Intersection->Object, UV_FLAG))
  {
    /* get the UV vect of the intersection */
    UVCoord(UV_Coords, Ray_Intersection->Object, Ray_Intersection);
    /* save the normal and UV coords into Intersection */
    Assign_UV_Vect(Ray_Intersection->Iuv, UV_Coords);
  }

  /* now switch to UV mapping if we need to */
  if (Test_Flag(Ray_Intersection->Object, UV_FLAG))
  {
    IPoint[X] = UV_Coords[U];
    IPoint[Y] = UV_Coords[V];
    IPoint[Z] = 0;
  }

  /*
   * Save texture and weight lists.
   */

  /* this has already been resized when LightingPoolIndex was incremented */
  Weight_List=WeightListPool[LightingPoolIndex];
  Texture_List=TextureListPool[LightingPoolIndex];

  /* Get texture list and weights. */
  Texture_Count = create_texture_list (Ray_Intersection, Normal_Direction);

  /*
   * Now, we perform the lighting calculations by stepping through
   * the list of textures and summing the weighted color.
   */

  /* NK phmap - set static variable to -1 so we will gather */
  /* non-negative will mean reuse already-gathered photons in the rest
     of the layers */
  photonsAlreadyGathered = -1;

  for (i = 0; i < Texture_Count; i++)
  {
    /* If contribution of this texture is neglectable skip ahead. */

    if (Weight_List[i] < ADC_Bailout)
    {
      continue;
    }

    Texture = Texture_List[i];

    /* NK phmap */
    if(backtraceFlag)
    {
      C1[0] = Colour[0]*Weight_List[i];
      C1[1] = Colour[1]*Weight_List[i];
      C1[2] = Colour[2]*Weight_List[i];
      do_texture_map(C1, Texture, IPoint, Raw_Normal, Ray, Weight, Ray_Intersection, false);
    }
    else
    {
      do_texture_map(C1, Texture, IPoint, Raw_Normal, Ray, Weight, Ray_Intersection, false);

      Colour[pRED]   += Weight_List[i] * C1[pRED];
      Colour[pGREEN] += Weight_List[i] * C1[pGREEN];
      Colour[pBLUE]  += Weight_List[i] * C1[pBLUE];
      Colour[pTRANSM]  += Weight_List[i] * C1[pTRANSM];
      /* Use transmittance value for alpha channel support. [DB] */
/*
      Colour[pTRANSM] *= C1[pTRANSM];
 */
    }
  }

  /* Restore the light list to its original form */

  LightingPoolIndex--;

  Light_List=LightListPool[LightingPoolIndex];

  /* Restore the weight and texture list. */

  Weight_List=WeightListPool[LightingPoolIndex];
  Texture_List=TextureListPool[LightingPoolIndex];

  /* NK depth */
  Total_Depth -= Ray_Intersection->Depth;
}



/*****************************************************************************
*
* FUNCTION
*
*   Test_Shadow
*
* INPUT
*
*   Light            - Light source
*   P                - Point to test
*
* OUTPUT
*
*   Depth            - Distance to light source
*   Light_Source_Ray - Light ray pointing towards the light source
*   Eye_Ray          - Current viewing ray
*   Colour           - Light color reaching point P
*
* RETURNS
*
*   int - true if point lies in shadow
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Test if a given point is in shadow in respect to the given light source.
*
*   The viewing ray is used to initialize the ray containers of the
*   light source ray.
*
* CHANGES
*
*   Nov 1994 : Creation.
*
******************************************************************************/

int Test_Shadow(LIGHT_SOURCE *Light, DBL *Depth, RAY *Light_Source_Ray, RAY  *Eye_Ray, VECTOR P, COLOUR Colour)
{
  do_light(Light, Depth, Light_Source_Ray, Eye_Ray, P, Colour);

  /*
   * There's no need to test for shadows if no light
   * is coming from the light source.
   *
   * Test for PURE zero, because we only want to skip this if we're out
   * of the range of a spot light or cylinder light.  Very dim lights
   * should not be ignored.
   */

  if ((fabs(Colour[X]) < EPSILON) && 
      (fabs(Colour[Y]) < EPSILON) && 
      (fabs(Colour[Z]) < EPSILON))
  {
    return(true);
  }
  else
  {
    /* Test for shadows. */

    if ((opts.Quality_Flags & Q_SHADOW) && ((Light->Projected_Through_Object != NULL) || (Light->Light_Type != FILL_LIGHT_SOURCE)))
    {
      block_light_source(Light, *Depth, Light_Source_Ray, Eye_Ray, P, Colour);

      if ((fabs(Colour[X]) < EPSILON) && 
          (fabs(Colour[Y]) < EPSILON) && 
          (fabs(Colour[Z]) < EPSILON))
      {
        return(true);
      }
    }
  }

  return(false);
}



/*****************************************************************************
*
* FUNCTION
*
*   block_point_light_LBuffer
*
* INPUT
*
*   Light_Source       - Light source to test
*
* OUTPUT
*
*   Light_Source_Depth - (Remaining) distance to the light source
*   Light_Source_Ray   - (Remaining) ray to the light source
*   Colour             - Color reaching initial point from light source
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Determine how much light from the given light source arrives at the
*   given point (starting point of the light source ray). The light
*   is blocked by solid objects and/or attenuated by translucent objects.
*
*   Note that both the distance to the light source and the light source
*   ray are modified. Thus after a call to this function one knows
*   how much distance remains to the light source and where the last
*   intersection point with a translucent object was (starting point
*   of light source ray after the call).
*
*   This function uses the light buffer to speed up shadow calculation.
*
* CHANGES
*
*   Jul 1994 : Creation.
*
******************************************************************************/

static void block_point_light_LBuffer(LIGHT_SOURCE *Light_Source, DBL *Light_Source_Depth, RAY *Light_Source_Ray, COLOUR Light_Colour)
{
  int Quit_Looking, Not_Found_Shadow, Cache_Me;
  int u, v, axis;
  DBL ax, ay, az;
  VECTOR V1;
  OBJECT *Blocking_Object;
  ISTACK *Local_Stack;
  INTERSECTION *Local_Intersection;
  INTERSECTION Bounded_Intersection, Temp_Intersection;

  /* Projected through main tests */

  DBL Projected_Depth=0.0;

  if (Light_Source->Projected_Through_Object != NULL)
  {
      if (Intersection(&Temp_Intersection,
        Light_Source->Projected_Through_Object,Light_Source_Ray))
      {
          if ( (Temp_Intersection.Depth - *Light_Source_Depth) < 0.0 )
          {
              Projected_Depth = 
                  *Light_Source_Depth - fabs(Temp_Intersection.Depth) + Small_Tolerance;
          }
          else 
          {
             Light_Colour[0] =
             Light_Colour[1] =
             Light_Colour[2] = 0.0;
             return;
          }
      }
      else 
      {
         Light_Colour[0] =
         Light_Colour[1] =
         Light_Colour[2] = 0.0;
         return;      
      }
      if(Light_Source->Light_Type == FILL_LIGHT_SOURCE) return;
  }

  Local_Stack = open_istack();

  Quit_Looking = false;

  /* First test the cached object (don't cache semi-transparent objects). */

  if (Light_Source->Shadow_Cached_Object != NULL)
  {
    Increase_Counter(stats[Shadow_Ray_Tests]);

    if (Ray_In_Bound(Light_Source_Ray, Light_Source->Shadow_Cached_Object->Bound))
    {
      if (All_Intersections(Light_Source->Shadow_Cached_Object, Light_Source_Ray, Local_Stack))
      {
        while ((Local_Intersection=pop_entry(Local_Stack)) != NULL)
        {
          if ((!Test_Flag(Local_Intersection->Object, NO_SHADOW_FLAG)) &&
              (Local_Intersection->Depth < *Light_Source_Depth-SHADOW_TOLERANCE) &&
              ((*Light_Source_Depth - Local_Intersection->Depth) > Projected_Depth) &&
              (Local_Intersection->Depth > SHADOW_TOLERANCE))
          {
            if (do_blocking(Local_Intersection, Light_Source_Ray, Light_Colour, Local_Stack))
            {
              Quit_Looking = true;

              Increase_Counter(stats[Shadow_Cache_Hits]);

              break;
            }
          }
        }
      }
    }

    /* Exit if the cached object was hit. */

    if (Quit_Looking)
    {
      close_istack(Local_Stack);

      return;
    }
  }

  /*
   * Determine the side and the coordinates at which the ray
   * pierces the cube enclosing the light source.
   */

  V1[X] = -Light_Source_Ray->Direction[X];
  V1[Y] = -Light_Source_Ray->Direction[Y];
  V1[Z] = -Light_Source_Ray->Direction[Z];

  ax = fabs(V1[X]);
  ay = fabs(V1[Y]);
  az = fabs(V1[Z]);

  if ((ax>ay) && (ax>az))
  {
    if (V1[X]>0.0)
    {
      axis = XaxisP;
    }
    else
    {
      axis = XaxisM;
    }

    u = (int)(MAX_BUFFER_ENTRY * V1[Y]/ax);
    v = (int)(MAX_BUFFER_ENTRY * V1[Z]/ax);
  }
  else
  {
    if (ay>az)
    {
      if (V1[Y]>0.0)
      {
        axis = YaxisP;
      }
      else
      {
        axis = YaxisM;
      }

      u = (int)(MAX_BUFFER_ENTRY * V1[X]/ay);
      v = (int)(MAX_BUFFER_ENTRY * V1[Z]/ay);
    }
    else
    {
      if (V1[Z]>0.0)
      {
        axis = ZaxisP;
      }
      else
      {
        axis = ZaxisM;
      }

      u = (int)(MAX_BUFFER_ENTRY * V1[X]/az);
      v = (int)(MAX_BUFFER_ENTRY * V1[Y]/az);
    }
  }

  /* If there are no objects in the direction of the ray we can exit. */

  if (Light_Source->Light_Buffer[axis] == NULL)
  {
    close_istack(Local_Stack);

    return;
  }

  /* Look for shadows. */

  Not_Found_Shadow = true;

  Cache_Me = false;

  while (!Quit_Looking)
  {
    Increase_Counter(stats[Shadow_Ray_Tests]);

    Bounded_Intersection.Object = NULL;
    Bounded_Intersection.Depth = *Light_Source_Depth;

    if (Intersect_Light_Tree(Light_Source_Ray, Light_Source->Light_Buffer[axis], u, v, &Bounded_Intersection, &Blocking_Object, Light_Source))
    {
      if (Bounded_Intersection.Depth > (*Light_Source_Depth - Projected_Depth) )
      {
        /* Intersection was beyond the light. */

        break;
      }

      if (Blocking_Object != Light_Source->Shadow_Cached_Object)
      {
        Increase_Counter(stats[Shadow_Rays_Succeeded]);

        filter_shadow_ray(&Bounded_Intersection, Light_Source_Ray, Light_Colour);

        if ((fabs(Light_Colour[pRED])   < EPSILON) &&
            (fabs(Light_Colour[pGREEN]) < EPSILON) &&
            (fabs(Light_Colour[pBLUE])  < EPSILON) &&
            (Test_Flag(Blocking_Object, OPAQUE_FLAG)))
        {
          Cache_Me = Not_Found_Shadow;

          break; /* from while */
        }
      }

      /* Move the ray to the point of intersection, plus some */

      *Light_Source_Depth -= Bounded_Intersection.Depth;

      Assign_Vector(Light_Source_Ray->Initial, Bounded_Intersection.IPoint);

      Not_Found_Shadow = false;
    }
    else
    {
      /* No intersections in the direction of the ray. */

      break;
    }
  }

  if (Cache_Me)
  {
    Light_Source->Shadow_Cached_Object = Blocking_Object;
  }

  close_istack(Local_Stack);
}



/*****************************************************************************
*
* FUNCTION
*
*   block_point_light
*
* INPUT
*
*   Light_Source       - Light source to test
*   Eye_Ray            - Current viewing ray
*
* OUTPUT
*
*   Light_Source_Depth - (Remaining) distance to the light source
*   Light_Source_Ray   - (Remaining) ray to the light source
*   Colour             - Color reaching initial point from light source
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   See block_point_light_LBuffer for a description.
*
*   This function uses the hierarchical bounding box volume to
*   speed up shadow testing.
*
* CHANGES
*
*   -
*
******************************************************************************/

static void block_point_light (LIGHT_SOURCE *Light_Source, DBL *Light_Source_Depth, RAY *Light_Source_Ray, COLOUR Light_Colour)
{
  OBJECT *Blocking_Object;
  int Quit_Looking, Not_Found_Shadow, Cache_Me, Maybe_Found;
  INTERSECTION *Local_Intersection;
  INTERSECTION Bounded_Intersection, Temp_Intersection;
  ISTACK *Local_Stack;

  /* Projected through main tests */
  
  DBL Projected_Depth=0.0;

  if (Light_Source->Projected_Through_Object != NULL)
  {
    if (Intersection(&Temp_Intersection,
        Light_Source->Projected_Through_Object,Light_Source_Ray))
    {
       if ( (Temp_Intersection.Depth - *Light_Source_Depth) < 0.0 )
       {
          Projected_Depth = *Light_Source_Depth - fabs(Temp_Intersection.Depth) + Small_Tolerance;
       }
       else 
       {
          Light_Colour[0] =
          Light_Colour[1] =
          Light_Colour[2] = 0.0;
          return;
       }
    }
    else 
    {
       Light_Colour[0] =
       Light_Colour[1] =
       Light_Colour[2] = 0.0;
       return;      
    }
    if(Light_Source->Light_Type == FILL_LIGHT_SOURCE) return;
  }

  Local_Stack = open_istack ();

  Quit_Looking = false;

  /* First test the cached object (don't cache semi-transparent objects). */

  if (Light_Source->Shadow_Cached_Object != NULL)
  {
    Increase_Counter(stats[Shadow_Ray_Tests]);

    if (Ray_In_Bound(Light_Source_Ray, Light_Source->Shadow_Cached_Object->Bound))
    {
      if (All_Intersections(Light_Source->Shadow_Cached_Object, Light_Source_Ray, Local_Stack))
      {
        while ((Local_Intersection = pop_entry(Local_Stack)) != NULL)
        {
          if ((!Test_Flag(Local_Intersection->Object, NO_SHADOW_FLAG)) &&
              (Local_Intersection->Depth < *Light_Source_Depth-SHADOW_TOLERANCE) &&
              ((*Light_Source_Depth - Local_Intersection->Depth) > Projected_Depth) &&
              (Local_Intersection->Depth > SHADOW_TOLERANCE))
          {
            if (do_blocking(Local_Intersection, Light_Source_Ray, Light_Colour, Local_Stack))
            {
              Quit_Looking = true;

              Increase_Counter(stats[Shadow_Cache_Hits]);

              break;
            }
          }
        }
      }
    }

    if (Quit_Looking)
    {
      close_istack (Local_Stack);

      return;
    }
  }

  /* Look for shadows. */

  Not_Found_Shadow = true;

  Cache_Me = false;

  if (!opts.Use_Slabs)
  {
    while (!Quit_Looking)
    {
      /* Use brute force method to get shadows. */

      Maybe_Found = false;

      Bounded_Intersection.Object = NULL;
      Bounded_Intersection.Depth = *Light_Source_Depth - Projected_Depth;

      for (Blocking_Object = Frame.Objects; Blocking_Object != NULL; Blocking_Object = Blocking_Object->Sibling)
      {
        if (Blocking_Object != Light_Source->Shadow_Cached_Object)
        {
          if (!Test_Flag(Blocking_Object, NO_SHADOW_FLAG))
          {
            Increase_Counter(stats[Shadow_Ray_Tests]);

            if (Intersection(&Temp_Intersection, Blocking_Object, Light_Source_Ray))
            {
              if (Temp_Intersection.Depth < Bounded_Intersection.Depth)
              {
                Maybe_Found = true;

                Bounded_Intersection = Temp_Intersection;
              }
            }
          }
        }
      }

      if (Maybe_Found)
      {
        Increase_Counter(stats[Shadow_Rays_Succeeded]);

        filter_shadow_ray(&Bounded_Intersection, Light_Source_Ray, Light_Colour);

        if ((fabs(Light_Colour[pRED])   < EPSILON) &&
            (fabs(Light_Colour[pGREEN]) < EPSILON) &&
            (fabs(Light_Colour[pBLUE])  < EPSILON) &&
            (Test_Flag(Bounded_Intersection.Object, OPAQUE_FLAG)))
        {
          Cache_Me = Not_Found_Shadow;

          break;
        }

        /* Move the ray to the point of intersection, plus some */

        *Light_Source_Depth -= Bounded_Intersection.Depth;

        Assign_Vector(Light_Source_Ray->Initial, Bounded_Intersection.IPoint);

        Not_Found_Shadow = false;
      }
      else
      {
        /* No intersections in the direction of the ray. */

        break;
      }
    }
  }
  else
  {
    /* Use bounding slabs to look for shadows. */

    In_Shadow_Ray = true;

    while (!Quit_Looking)
    {
      Increase_Counter(stats[Shadow_Ray_Tests]);

      Bounded_Intersection.Depth = *Light_Source_Depth;

      if (Intersect_BBox_Tree(Root_Object, Light_Source_Ray, &Bounded_Intersection, &Blocking_Object,true))
      {
        if (Bounded_Intersection.Depth > (*Light_Source_Depth - Projected_Depth))
        {
          /* Intersection was beyond the light. */

          break;
        }

        if (!Test_Flag(Bounded_Intersection.Object, NO_SHADOW_FLAG))
        {
          if (Blocking_Object != Light_Source->Shadow_Cached_Object)
          {
            Increase_Counter(stats[Shadow_Rays_Succeeded]);

            filter_shadow_ray(&Bounded_Intersection, Light_Source_Ray, Light_Colour);

            if ((fabs(Light_Colour[pRED])   < EPSILON) &&
                (fabs(Light_Colour[pGREEN]) < EPSILON) &&
                (fabs(Light_Colour[pBLUE])  < EPSILON) &&
                (Test_Flag(Blocking_Object, OPAQUE_FLAG)))
            {
              Cache_Me = Not_Found_Shadow;

              break; /* from while */
            }
          }
        }

        /* Move the ray to the point of intersection, plus some */

        *Light_Source_Depth -= Bounded_Intersection.Depth;

        Assign_Vector(Light_Source_Ray->Initial, Bounded_Intersection.IPoint);

        Not_Found_Shadow = false;
      }
      else
      {
        /* No intersections in the direction of the ray */

        break;
      }
    }

    In_Shadow_Ray = false;
  }

  if (Cache_Me)
  {
    Light_Source->Shadow_Cached_Object = Blocking_Object;
  }

  close_istack (Local_Stack);
}



/*****************************************************************************
*
* FUNCTION
*
*   block_area_light
*
* INPUT
*
*   Light_Source       - Light source to test
*   IPoint             -
*   u1, v1, u2, v2     -
*   Level              -
*
* OUTPUT
*
*   Light_Source_Depth - (Remaining) distance to the light source
*   Light_Source_Ray   - (Remaining) ray to the light source
*   Light_Colour       - Color reaching initial point from light source
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   Get shadow for given area light source by recursively sampling
*   on the light source area.
*
*   The viewing ray is used to initialize the ray containers of the
*   light source ray.
*
* CHANGES
*
*   Aug 1997 - Add code to orient the area light towards the intersection
*              point. [ENB]
*   Aug 1997 - Add code to create circular area lights. [ENB]
*
******************************************************************************/

static void do_light_area_light(LIGHT_SOURCE* Light_Source, DBL* Light_Source_Depth, RAY* Light_Source_Ray, RAY* Eye_Ray, VECTOR IPoint)
{
  DBL  a;
  VECTOR v1;
  /*
   * Get the light ray starting at the intersection point and pointing
   * towards the light source.
   */

  Assign_Vector(Light_Source_Ray->Initial, IPoint);

  /* NK 1998 parallel beams for cylinder source - added 'if' */
  if (Light_Source->Light_Type == CYLINDER_SOURCE)
  {
    DBL distToPointsAt;
    VECTOR toLightCtr;

    /* use new code to get ray direction - use center - points_at for direction */
    VSub(Light_Source_Ray->Direction, Light_Source->Center, Light_Source->Points_At);

    /* get vector pointing to center of light */
    VSub(toLightCtr, Light_Source->Center, IPoint);

    /* project light_ctr-intersect_point onto light_ctr-point_at*/
    VLength(distToPointsAt, Light_Source_Ray->Direction);
    VDot(*Light_Source_Depth, toLightCtr, Light_Source_Ray->Direction);

    /* lenght of shadow ray is the length of the projection */
    *Light_Source_Depth /= distToPointsAt;

    VNormalizeEq(Light_Source_Ray->Direction);
  }
  else
  {
    /* NK 1998 parallel beams for cylinder source - the stuff in this 'else'
      block used to be all that there was... the first half of the if
      statement (before the 'else') is new
    */
    VSub(Light_Source_Ray->Direction, Light_Source->Center, IPoint);
    VLength(*Light_Source_Depth, Light_Source_Ray->Direction);
    VInverseScaleEq(Light_Source_Ray->Direction, *Light_Source_Depth);
  }

  /* Attenuate light source color. */

/*  Attenuation = Attenuate_Light(Light_Source, Light_Source_Ray, *Light_Source_Depth);*/

  /* Recalculate for Parallel light sources */
  if (Light_Source->Parallel)
  {
    if (Light_Source->Area_Light)
    {
      VSub(v1, Light_Source->Center, Light_Source->Points_At);
      VNormalizeEq(v1);
      VDot(a, v1, Light_Source_Ray->Direction);
      *Light_Source_Depth *= a;
      Assign_Vector(Light_Source_Ray->Direction, v1);
    }
    else
    {
      VDot(a, Light_Source->Direction, Light_Source_Ray->Direction);
      *Light_Source_Depth *= (-a);
      Assign_Vector(Light_Source_Ray->Direction, Light_Source->Direction);
      VScaleEq(Light_Source_Ray->Direction, -1.0);
    }
  }
  Light_Source_Ray->Index = - 1;
  {
    register int i;
    if ((Light_Source_Ray->Index = Eye_Ray->Index) >= MAX_CONTAINING_OBJECTS)
      Error("ERROR - Containing Index too high.");
    for (i = 0 ; i <= Eye_Ray->Index; i++)
      Light_Source_Ray->Interiors[i] = Eye_Ray->Interiors[i];
  }
}

/*YS april 2000
   In the function block_area_light() the function do_light() is called a few times. 
   Do_light() uses a parameter 'COLOUR Light_Colour', that is the last one. 
   Now, block_area_light() is sending dummy_colour simply because block_area_light() 
   doesn't need that one.
  All very well but do_light() is initialising that parameter.
  That's not the end of the story: do_light() calls Attenuate_light() and the result of that 
  function is only used to mess with light_colour again. And we do not need that one! Just think 
  of the code we throw away now. 
  To make it even better, the stripped down version of do_light can be merged directly in 
  block_area_light because the parameters are all variables themselves. Not much waste 
  of registers (at assembly level) but eliminating lots of subroutine calls.  */

static void block_area_light (LIGHT_SOURCE *Light_Source, DBL *Light_Source_Depth,
  RAY *Light_Source_Ray, RAY  *Eye_Ray, VECTOR IPoint, COLOUR Light_Colour, int u1, int  v1, int  u2, int  v2, int  Level)
{
  COLOUR Sample_Colour[4];
  VECTOR Center_Save, NewAxis1, NewAxis2, Axis1_Save, Axis2_Save, Temp;
  int i, j, u, v, New_u1, New_v1, New_u2, New_v2;
  DBL Axis1_Length;
  bool First_Call=false;

  DBL Jitter_u, Jitter_v, ScaleFactor;

  /* First call, initialize */

  if ((u1 == 0) && (v1 == 0) && (u2 == 0) && (v2 == 0))
  {
    First_Call = true;

    /* Flag uncalculated points with a negative value for Red */

    for (i = 0; i < Light_Source->Area_Size1; i++)
    {
      for (j = 0; j < Light_Source->Area_Size2; j++)
      {
        Light_Source->Light_Grid[i][j][pRED] = -1.0;
      }
    }

    u1 = 0;
    v1 = 0;
    u2 = Light_Source->Area_Size1 - 1;
    v2 = Light_Source->Area_Size2 - 1;

    if ( Light_Source->Orient == true)
    {
      /* Save the axis vectors since we may be changing them */
      Assign_Vector(Axis1_Save,Light_Source->Axis1);
      Assign_Vector(Axis2_Save,Light_Source->Axis2);

      /* Orient the area light to face the intersection point [ENB 9/97] */

      /* Do Light source to get the correct Light_Source_Ray */
      do_light_area_light(Light_Source, Light_Source_Depth, Light_Source_Ray, Eye_Ray, IPoint);
      VScaleEq(Light_Source_Ray->Direction,-1);

      /* Save the lengths of the axises */
      VLength(Axis1_Length, Light_Source->Axis1);

      /* 
      Make axis 1 be perpendicular with the light-ray 
      */
      if ( fabs(fabs(Light_Source_Ray->Direction[Z])- 1.) < .01 ) {
        /* too close to vertical for comfort, so use cross product with horizon */
        Temp[X] = 0.; Temp[Y] = 1.; Temp[Z] = 0.;
      }
      else
      {
        Temp[X] = 0.; Temp[Y] = 0.; Temp[Z] = 1.;
      }

      VCross(Light_Source->Axis1,Temp,Light_Source_Ray->Direction);
      VNormalizeEq(Light_Source->Axis1);

      /* 
      Make axis 2 be perpendicular with the light-ray and
      with Axis1.  A simple cross-product will do the trick.
      */
      VCross(Light_Source->Axis2, Light_Source->Axis1, Light_Source_Ray->Direction);
      VNormalizeEq(Light_Source->Axis2);

      /* make it square */
      VScaleEq(Light_Source->Axis1,Axis1_Length);
      VScaleEq(Light_Source->Axis2,Axis1_Length);

      VScaleEq(Light_Source_Ray->Direction,-1);
    }
  }

  /* Save the light source center since we'll be fiddling with it */

  Assign_Vector(Center_Save,Light_Source->Center);

  /* Sample the four corners of the region */

  for (i = 0; i < 4; i++)
  {
    switch (i)
    {
      case 0: u = u1; v = v1; break;
      case 1: u = u2; v = v1; break;
      case 2: u = u1; v = v2; break;
      case 3: u = u2; v = v2; break;
      default: u = v = 0;  /* Should never happen! */
    }

    if (Light_Source->Light_Grid[u][v][pRED] >= 0.0)
    {
      /* We've already calculated this point, reuse it */

      Assign_Colour(Sample_Colour[i],Light_Source->Light_Grid[u][v]);
    }
    else
    {
      Jitter_u = (DBL)u;
      Jitter_v = (DBL)v;

      if (Light_Source->Jitter)
      {
        Jitter_u += FRAND() - 0.5;
        Jitter_v += FRAND() - 0.5;
      }

      /* Create circular are lights [ENB 9/97]
         First, make Jitter_u and Jitter_v be numbers from -1 to 1
         Second, set ScaleFactor to the abs max (Jitter_u,Jitter_v) (for shells)
         Third, divide ScaleFactor by the length of <Jitter_u,Jitter_v>
         Fourth, scale Jitter_u & Jitter_v by ScaleFactor
         Finally scale Axis1 by Jitter_u & Axis2 by Jitter_v
      */
      if (Light_Source->Circular == true)
      {
        Jitter_u = Jitter_u / (Light_Source->Area_Size1 - 1) - 0.5 + 0.001;
        Jitter_v = Jitter_v / (Light_Source->Area_Size2 - 1) - 0.5 + 0.001;
        ScaleFactor = ((fabs(Jitter_u) > fabs(Jitter_v)) ? fabs(Jitter_u) : fabs(Jitter_v));
        ScaleFactor /= sqrt(Jitter_u * Jitter_u + Jitter_v * Jitter_v);
        Jitter_u *= ScaleFactor;
        Jitter_v *= ScaleFactor;
        VScale (NewAxis1, Light_Source->Axis1, Jitter_u);
        VScale (NewAxis2, Light_Source->Axis2, Jitter_v);
      }
      else
      {
        if (Light_Source->Area_Size1 > 1)
        {
          ScaleFactor = Jitter_u/(DBL)(Light_Source->Area_Size1 - 1) - 0.5;

          VScale (NewAxis1, Light_Source->Axis1, ScaleFactor);
        }
        else
        {
          Make_Vector(NewAxis1, 0.0, 0.0, 0.0);
        }

        if (Light_Source->Area_Size2 > 1)
        {
          ScaleFactor = Jitter_v/(DBL)(Light_Source->Area_Size2 - 1) - 0.5;

          VScale (NewAxis2, Light_Source->Axis2, ScaleFactor);
        }
        else
        {
          Make_Vector(NewAxis2, 0.0, 0.0, 0.0);
        }
      }

      /* Find the center of the light to test */

      Assign_Vector(Light_Source->Center, Center_Save);

      VAddEq(Light_Source->Center, NewAxis1);
      VAddEq(Light_Source->Center, NewAxis2);

      /* Recalculate the light source ray but not the colour */
    {
      DBL  a;
      VECTOR v1;
       /* Get the light ray starting at the intersection point and pointing
       * towards the light source.   */
      Assign_Vector(Light_Source_Ray->Initial, IPoint);
      /* NK 1998 parallel beams for cylinder source - added 'if' */
      if (Light_Source->Light_Type == CYLINDER_SOURCE)
      {
        DBL distToPointsAt;
        VECTOR toLightCtr;
        /* use new code to get ray direction - use center - points_at for direction */
        VSub(Light_Source_Ray->Direction, Light_Source->Center, Light_Source->Points_At);
        /* get vector pointing to center of light */
        VSub(toLightCtr, Light_Source->Center, IPoint);
        /* project light_ctr-intersect_point onto light_ctr-point_at*/
        VLength(distToPointsAt, Light_Source_Ray->Direction);
        VDot(*Light_Source_Depth, toLightCtr, Light_Source_Ray->Direction);
        /* lenght of shadow ray is the length of the projection */
        *Light_Source_Depth /= distToPointsAt;
        VNormalizeEq(Light_Source_Ray->Direction);
      }
      else
      {
        /* NK 1998 parallel beams for cylinder source - the stuff in this 'else'
          block used to be all that there was... the first half of the if
          statement (before the 'else') is new  */
        VSub(Light_Source_Ray->Direction, Light_Source->Center, IPoint);
        VLength(*Light_Source_Depth, Light_Source_Ray->Direction);
        VInverseScaleEq(Light_Source_Ray->Direction, *Light_Source_Depth);
      }
      /* Attenuate light source color. */
      /*  Attenuation = Attenuate_Light(Light_Source, Light_Source_Ray, *Light_Source_Depth);*/
      /* Recalculate for Parallel light sources */
      if (Light_Source->Parallel)
      {
        if (Light_Source->Area_Light)
        {
          VSub(v1, Light_Source->Center, Light_Source->Points_At);
          VNormalizeEq(v1);
          VDot(a, v1, Light_Source_Ray->Direction);
          *Light_Source_Depth *= a;
          Assign_Vector(Light_Source_Ray->Direction, v1);
        }
        else
        {
          VDot(a, Light_Source->Direction, Light_Source_Ray->Direction);
          *Light_Source_Depth *= (-a);
          Assign_Vector(Light_Source_Ray->Direction, Light_Source->Direction);
          VScaleEq(Light_Source_Ray->Direction, -1.0);
        }
      }
      Light_Source_Ray->Index = - 1;
      {
        register int i;
        if ((Light_Source_Ray->Index = Eye_Ray->Index) >= MAX_CONTAINING_OBJECTS)
          Error("ERROR - Containing Index too high.");
        for (i = 0 ; i <= Eye_Ray->Index; i++)
          Light_Source_Ray->Interiors[i] = Eye_Ray->Interiors[i];
      }
    }

      Assign_Colour(Sample_Colour[i], Light_Colour);

      block_point_light(Light_Source, Light_Source_Depth, Light_Source_Ray, Sample_Colour[i]);

      Assign_Colour(Light_Source->Light_Grid[u][v], Sample_Colour[i]);
    }
  }

  Assign_Vector(Light_Source->Center,Center_Save);

  if ((u2 - u1 > 1) || (v2 - v1 > 1))
  {
    if ((Level < Light_Source->Adaptive_Level) ||
        (Colour_Distance(Sample_Colour[0], Sample_Colour[1]) > 0.1) ||
        (Colour_Distance(Sample_Colour[1], Sample_Colour[3]) > 0.1) ||
        (Colour_Distance(Sample_Colour[3], Sample_Colour[2]) > 0.1) ||
        (Colour_Distance(Sample_Colour[2], Sample_Colour[0]) > 0.1))
    {
      for (i = 0; i < 4; i++)
      {
        switch (i)
        {
          case 0:

            New_u1 = u1;
            New_v1 = v1;
            New_u2 = (int)floor ((u1 + u2)/2.0);
            New_v2 = (int)floor ((v1 + v2)/2.0);

            break;

          case 1:

            New_u1 = (int)ceil  ((u1 + u2)/2.0);
            New_v1 = v1;
            New_u2 = u2;
            New_v2 = (int)floor ((v1 + v2)/2.0);

            break;

          case 2:

            New_u1 = u1;
            New_v1 = (int)ceil  ((v1 + v2)/2.0);
            New_u2 = (int)floor ((u1 + u2)/2.0);
            New_v2 = v2;

            break;

          case 3:

            New_u1 = (int)ceil ((u1 + u2)/2.0);
            New_v1 = (int)ceil ((v1 + v2)/2.0);
            New_u2 = u2;
            New_v2 = v2;

            break;

          default:  /* Should never happen! */

            New_u1 = New_u2 = New_v1 = New_v2 = 0;
        }

        /* Recalculate the light source ray but not the colour */

        {
          DBL  a;
          VECTOR v1;
           /* Get the light ray starting at the intersection point and pointing
           * towards the light source.   */
          Assign_Vector(Light_Source_Ray->Initial, IPoint);
          /* NK 1998 parallel beams for cylinder source - added 'if' */
          if (Light_Source->Light_Type == CYLINDER_SOURCE)
          {
            DBL distToPointsAt;
            VECTOR toLightCtr;
            /* use new code to get ray direction - use center - points_at for direction */
            VSub(Light_Source_Ray->Direction, Light_Source->Center, Light_Source->Points_At);
            /* get vector pointing to center of light */
            VSub(toLightCtr, Light_Source->Center, IPoint);
            /* project light_ctr-intersect_point onto light_ctr-point_at*/
            VLength(distToPointsAt, Light_Source_Ray->Direction);
            VDot(*Light_Source_Depth, toLightCtr, Light_Source_Ray->Direction);
            /* lenght of shadow ray is the length of the projection */
            *Light_Source_Depth /= distToPointsAt;
            VNormalizeEq(Light_Source_Ray->Direction);
          }
          else
          {
            /* NK 1998 parallel beams for cylinder source - the stuff in this 'else'
              block used to be all that there was... the first half of the if
              statement (before the 'else') is new  */
            VSub(Light_Source_Ray->Direction, Light_Source->Center, IPoint);
            VLength(*Light_Source_Depth, Light_Source_Ray->Direction);
            VInverseScaleEq(Light_Source_Ray->Direction, *Light_Source_Depth);
          }
          /* Attenuate light source color. */
        /*  Attenuation = Attenuate_Light(Light_Source, Light_Source_Ray, *Light_Source_Depth);*/
          /* Recalculate for Parallel light sources */
          if (Light_Source->Parallel)
          {
            if (Light_Source->Area_Light)
            {
              VSub(v1, Light_Source->Center, Light_Source->Points_At);
              VNormalizeEq(v1);
              VDot(a, v1, Light_Source_Ray->Direction);
              *Light_Source_Depth *= a;
              Assign_Vector(Light_Source_Ray->Direction, v1);
            }
            else
            {
              VDot(a, Light_Source->Direction, Light_Source_Ray->Direction);
              *Light_Source_Depth *= (-a);
              Assign_Vector(Light_Source_Ray->Direction, Light_Source->Direction);
              VScaleEq(Light_Source_Ray->Direction, -1.0);
            }
          }
          Light_Source_Ray->Index = - 1;
          {
            register int i;
            if ((Light_Source_Ray->Index = Eye_Ray->Index) >= MAX_CONTAINING_OBJECTS)
              Error("ERROR - Containing Index too high.");
            for (i = 0 ; i <= Eye_Ray->Index; i++)
              Light_Source_Ray->Interiors[i] = Eye_Ray->Interiors[i];
          }
        }

        Assign_Colour(Sample_Colour[i],Light_Colour);

        block_area_light (Light_Source, Light_Source_Depth, Light_Source_Ray, Eye_Ray,
          IPoint, Sample_Colour[i], New_u1, New_v1, New_u2, New_v2, Level+1);
      }
    }
  }

  /* Add up the light contributions */
  Light_Colour[pRED] = Sample_Colour[0][pRED] * 0.25;
  Light_Colour[pGREEN] = Sample_Colour[0][pGREEN] * 0.25;
  Light_Colour[pBLUE] = Sample_Colour[0][pBLUE] * 0.25;
  Light_Colour[pFILTER] += Sample_Colour[0][pFILTER] * 0.25;
  Light_Colour[pTRANSM] += Sample_Colour[0][pTRANSM] * 0.25;
  for (i = 1; i < 4; i++)
  {
    Light_Colour[pRED] += Sample_Colour[i][pRED] * 0.25;
    Light_Colour[pGREEN] += Sample_Colour[i][pGREEN] * 0.25;
    Light_Colour[pBLUE] += Sample_Colour[i][pBLUE] * 0.25;
    Light_Colour[pFILTER] += Sample_Colour[i][pFILTER] * 0.25;
    Light_Colour[pTRANSM] += Sample_Colour[i][pTRANSM] * 0.25;
  }

  if (First_Call == true) 
  {
    /* Reset the axises to what they were before */
    if ( Light_Source->Orient == true)
    {
      Assign_Vector(Light_Source->Axis1,Axis1_Save);
      Assign_Vector(Light_Source->Axis2,Axis2_Save);
    }
   }
}


/*****************************************************************************
*
* FUNCTION
*
*   do_light
*
* INPUT
*
*   Light_Source       - Light source
*   Light_Source_Depth - Distance from surface to light source
*   Light_Source_Ray   - Ray from surface to light source
*   Eye_Ray            - Current viewing ray
*   IPoint             - Intersection point in surface
*   Colour             - Light's colour
*
* OUTPUT
*
*   Light_Source_Depth, Light_Source_Ray, Colour
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   The viewing ray is used to initialize the ray containers of the
*   light source ray.
*
* CHANGES
*
*   -
*
******************************************************************************/

static void do_light(LIGHT_SOURCE *Light_Source, DBL *Light_Source_Depth, RAY *Light_Source_Ray, RAY  *Eye_Ray, VECTOR IPoint, COLOUR Light_Colour)
{
  DBL Attenuation,a;
  VECTOR v1;

  /* Get the light source colour. */

  Assign_Colour(Light_Colour, Light_Source->Colour);

  /*
   * Get the light ray starting at the intersection point and pointing
   * towards the light source.
   */

  Assign_Vector(Light_Source_Ray->Initial, IPoint);

  /* NK 1998 parallel beams for cylinder source - added if */
  if (Light_Source->Light_Type == CYLINDER_SOURCE)
  {
    DBL distToPointsAt;
    VECTOR toLightCtr;

    /* use old code to approximate Light_Source_Depth */
    VSub(Light_Source_Ray->Direction,Light_Source->Center, IPoint);
    VLength(*Light_Source_Depth, Light_Source_Ray->Direction);

    /* use new code to get ray direction - use center - points_at for direction */
    VSub(Light_Source_Ray->Direction,Light_Source->Center, Light_Source->Points_At);

    /* get vector pointing to center of light */
    VSub(toLightCtr,Light_Source->Center, IPoint);

    /* project light_ctr-intersect_point onto light_ctr-point_at*/
    VLength(distToPointsAt, Light_Source_Ray->Direction);
    VDot(*Light_Source_Depth,toLightCtr, Light_Source_Ray->Direction);

    /* lenght of shadow ray is the length of the projection */
    *Light_Source_Depth /= distToPointsAt;

    VNormalizeEq(Light_Source_Ray->Direction);
  }
  else
  {
    /* NK 1998 parallel beams for cylinder source - the stuff in this 'else'
         block used to be all that there was... the first half of the if
         statement (before the 'else') is new
    */
    VSub(Light_Source_Ray->Direction,Light_Source->Center, IPoint);

    VLength(*Light_Source_Depth, Light_Source_Ray->Direction);

    VInverseScaleEq(Light_Source_Ray->Direction, *Light_Source_Depth);
  }

  /* Attenuate light source color. */

  Attenuation = Attenuate_Light(Light_Source, Light_Source_Ray, *Light_Source_Depth);

  /* Recalculate for Parallel light sources */
  if (Light_Source->Parallel) 
  {
    if (Light_Source->Area_Light) 
    {
      VSub(v1,Light_Source->Center,Light_Source->Points_At);
      VNormalizeEq( v1 );
      VDot(a,v1,Light_Source_Ray->Direction);
      *Light_Source_Depth *= a;
      Assign_Vector(Light_Source_Ray->Direction,v1);
    } 
    else 
    {
      VDot(a,Light_Source->Direction,Light_Source_Ray->Direction);
      *Light_Source_Depth *= (-a);
      Assign_Vector(Light_Source_Ray->Direction,Light_Source->Direction);
      VScaleEq(Light_Source_Ray->Direction,-1.0);
    }
  }

  /* Now scale the color by the attenuation */

  VScaleEq(Light_Colour, Attenuation);

  /* Init ray containers. */

  Initialize_Ray_Containers(Light_Source_Ray);

  Copy_Ray_Containers(Light_Source_Ray, Eye_Ray);
}



/*****************************************************************************
*
* FUNCTION
*
*   do_diffuse
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
*   Calculate the diffuse color component I_d given by:
*
*     I_d = a * d * I * C * (N . L) ^ b
*
*   where d : surface's diffuse reflection coefficient
*         b : surface's brilliance
*         C : surface's color
*         N : surface's normal vector
*         L : light vector (pointing at the light)
*         I : intensity of the incoming light
*         a : attenuation factor
*
* CHANGES
*
*   -
*
******************************************************************************/

void do_diffuse(FINISH *Finish, RAY *Light_Source_Ray, VECTOR Layer_Normal, COLOUR Colour, COLOUR  Light_Colour, COLOUR  Layer_Pigment_Colour, DBL Attenuation)
{
  DBL Cos_Angle_Of_Incidence, Intensity;

  VDot(Cos_Angle_Of_Incidence, Layer_Normal, Light_Source_Ray->Direction);

  /* Brilliance is likely to be 1.0 (default value) */

  if (Finish->Brilliance != 1.0)
  {
    Intensity = pow(fabs(Cos_Angle_Of_Incidence), (DBL) Finish->Brilliance);
  }
  else
  {
    Intensity = fabs(Cos_Angle_Of_Incidence);
  }

  Intensity *= Finish->Diffuse * Attenuation;

  if (Finish->Crand > 0.0)
  {
    Intensity -= FRAND() * Finish->Crand;
  }

  Colour[pRED]   += Intensity * Layer_Pigment_Colour[pRED]   * Light_Colour[pRED];
  Colour[pGREEN] += Intensity * Layer_Pigment_Colour[pGREEN] * Light_Colour[pGREEN];
  Colour[pBLUE]  += Intensity * Layer_Pigment_Colour[pBLUE]  * Light_Colour[pBLUE];
}



/*****************************************************************************
*
* FUNCTION
*
*   do_irid
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Dan Farmer
*
* DESCRIPTION
*
*   IRIDESCENCE:
*   -----------
*   Programmed by Dan Farmer.
*
*   Based on Chapter 10.2.4 of Three-Dimensional Computer Graphics
*   by Alan Watt.
*
*   Modulates the diffuse coefficients as a function of wavelength, the angle
*   between the light direction vector, and the surface normal.  It models
*   thin-film interference, as in a soap bubble or oilslick.
*
*   Wavelength at which cancellation offurs is a function of the refractive
*   index of the film, its thickness, and the angle of incidence of the
*   incoming light.  In this implementation, IOR is kept constant, while the
*   thickness of the film is specified, as well as being modulated with a
*   turbulence function.
*
* CHANGES
*
*   -
*
******************************************************************************/

static void do_irid(FINISH *Finish, RAY *Light_Source_Ray, VECTOR Layer_Normal, VECTOR  IPoint, COLOUR Colour)
{
  DBL rwl, gwl, bwl;
  DBL Cos_Angle_Of_Incidence, interference;
  DBL film_thickness;
  DBL noise, intensity;
  TURB Turb;

  film_thickness = Finish->Irid_Film_Thickness;

  if (Finish->Irid_Turb != 0)
  {
    /* Uses hardcoded octaves, lambda, omega */
    Turb.Omega=0.5;
    Turb.Lambda=2.0;
    Turb.Octaves=5;

    noise = Turbulence(IPoint, &Turb, NULL) * Finish->Irid_Turb;

    film_thickness *= noise;
  }

  /*
   * Approximate dominant wavelengths of primary hues.
   * Source: 3D Computer Graphics by John Vince (Addison Wesely)
   * These are initialized in parse.c (Parse_Frame)
   * and are user-adjustable with the irid_wavelength keyword.
   * Red = 700 nm  Grn = 520 nm Blu = 480 nm
   * Divided by 100 gives: rwl = 0.70;  gwl = 0.52;  bwl = 0.48;
   *
   * However... I originally "guessed" at the values and came up with
   * the following, which I'm using as the defaults, since it seems
   * to work better:  rwl = 0.25;  gwl = 0.18;  bwl = 0.14;
   */

  /* Could avoid these assignments if we want to */

  rwl = Frame.Irid_Wavelengths[pRED];
  gwl = Frame.Irid_Wavelengths[pGREEN];
  bwl = Frame.Irid_Wavelengths[pBLUE];

  /* NOTE: Shouldn't we compute Cos_Angle_Of_Incidence just once? */

  VDot(Cos_Angle_Of_Incidence, Layer_Normal, Light_Source_Ray->Direction);

  /* Calculate phase offset. */

  interference = 4.0 * M_PI * film_thickness * Cos_Angle_Of_Incidence;

  intensity = Cos_Angle_Of_Incidence * Finish->Irid;

  /* Modify color by phase offset for each wavelength. */

  Colour[pRED]  += Finish->Irid * (intensity * (1.0 - 0.5 * cos(interference/rwl)));
  Colour[pGREEN]+= Finish->Irid * (intensity * (1.0 - 0.5 * cos(interference/gwl)));
  Colour[pBLUE] += Finish->Irid * (intensity * (1.0 - 0.5 * cos(interference/bwl)));
}



/*****************************************************************************
*
* FUNCTION
*
*   do_phong
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
*   Calculate the phong reflected color component I_p given by:
*
*     I_p = p * C * (R . L) ^ s
*
*   where p : surface's phong reflection coefficient
*         s : surface's phong size
*         C : surface's color/light color depending on the metallic flag
*         R : reflection vector
*         L : light vector (pointing at the light)
*
*   The reflection vector is calculated from the surface normal and
*   the viewing vector (looking at the surface point):
*
*     R = -2 * (V . N) * N + V, with R . R = 1
*
* CHANGES
*
*   Sep 1994 : Added improved color calculation for metallic surfaces. [DB]
*
******************************************************************************/

void do_phong(FINISH *Finish, RAY *Light_Source_Ray, VECTOR  Eye, VECTOR Layer_Normal, COLOUR Colour, COLOUR  Light_Colour, COLOUR  Layer_Pigment_Colour)
{
  DBL Cos_Angle_Of_Incidence, Intensity;
  VECTOR Reflect_Direction;
  DBL NdotL, x, F;
  COLOUR Cs;

  VDot(Cos_Angle_Of_Incidence, Eye, Layer_Normal);

  Cos_Angle_Of_Incidence *= -2.0;

  VLinComb2(Reflect_Direction, 1.0, Eye, Cos_Angle_Of_Incidence, Layer_Normal);

  VDot(Cos_Angle_Of_Incidence, Reflect_Direction, Light_Source_Ray->Direction);

  if (Cos_Angle_Of_Incidence > 0.0)
  {
    if ((Finish->Phong_Size < 60) || (Cos_Angle_Of_Incidence > .0008)) /* rgs */
    Intensity = Finish->Phong * pow(Cos_Angle_Of_Incidence, (DBL) Finish->Phong_Size);
    else
      Intensity = 0.0; /* ad */

    if (Finish->Metallic > 0.0)
    {
      /*
       * Calculate the reflected color by interpolating between
       * the light source color and the surface color according
       * to the (empirical) Fresnel reflectivity function. [DB 9/94]
       */

      VDot(NdotL, Layer_Normal, Light_Source_Ray->Direction);

      x = fabs(acos(NdotL)) / M_PI_2;

      F = 0.014567225 / Sqr(x - 1.12) - 0.011612903;

      F=min(1.0,max(0.0,F));
      Cs[pRED]   = Light_Colour[pRED]   * (1.0 + Finish->Metallic * (1.0 - F) * (Layer_Pigment_Colour[pRED]   - 1.0));
      Cs[pGREEN] = Light_Colour[pGREEN] * (1.0 + Finish->Metallic * (1.0 - F) * (Layer_Pigment_Colour[pGREEN] - 1.0));
      Cs[pBLUE]  = Light_Colour[pBLUE]  * (1.0 + Finish->Metallic * (1.0 - F) * (Layer_Pigment_Colour[pBLUE]  - 1.0));

      CRGBAddScaledEq(Colour, Intensity, Cs);
    }
    else
    {
      Colour[pRED]   += Intensity * Light_Colour[pRED];
      Colour[pGREEN] += Intensity * Light_Colour[pGREEN];
      Colour[pBLUE]  += Intensity * Light_Colour[pBLUE];
    }
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   do_specular
*
* INPUT
*
* OUTPUT
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
*   Calculate the specular reflected color component I_s given by:
*
*     I_s = s * C * (H . N) ^ (1 / r)
*
*   where s : surface's specular reflection coefficient
*         r : surface's roughness
*         C : surface's color/light color depending on the metallic flag
*         N : surface's normal
*         H : bisection vector between V and L
*
*   The bisecting vector H is calculated by
*
*     H = (L - V) / sqrt((L - V).(L - V))
*
* CHANGES
*
*   Sep 1994 : Added improved color calculation for metallic surfaces. [DB]
*
******************************************************************************/

void do_specular(FINISH *Finish, RAY *Light_Source_Ray, VECTOR  REye, VECTOR Layer_Normal, COLOUR Colour, COLOUR  Light_Colour, COLOUR  Layer_Pigment_Colour)
{
  DBL Cos_Angle_Of_Incidence, Intensity, Halfway_Length;
  VECTOR Halfway;
  DBL NdotL, x, F;
  COLOUR Cs;

  VHalf(Halfway, REye, Light_Source_Ray->Direction);

  VLength(Halfway_Length, Halfway);

  if (Halfway_Length > 0.0)
  {
    VDot(Cos_Angle_Of_Incidence, Halfway, Layer_Normal);

    Cos_Angle_Of_Incidence /= Halfway_Length;

    if (Cos_Angle_Of_Incidence > 0.0)
    {
      Intensity = Finish->Specular * pow(Cos_Angle_Of_Incidence, (DBL) Finish->Roughness);

      if (Finish->Metallic > 0.0)
      {
        /*
         * Calculate the reflected color by interpolating between
         * the light source color and the surface color according
         * to the (empirical) Fresnel reflectivity function. [DB 9/94]
         */

        VDot(NdotL, Layer_Normal, Light_Source_Ray->Direction);

        x = fabs(acos(NdotL)) / M_PI_2;

        F = 0.014567225 / Sqr(x - 1.12) - 0.011612903;

        F=min(1.0,max(0.0,F));
        Cs[pRED]   = Light_Colour[pRED]   * (1.0 + Finish->Metallic * (1.0 - F) * (Layer_Pigment_Colour[pRED]   - 1.0));
        Cs[pGREEN] = Light_Colour[pGREEN] * (1.0 + Finish->Metallic * (1.0 - F) * (Layer_Pigment_Colour[pGREEN] - 1.0));
        Cs[pBLUE]  = Light_Colour[pBLUE]  * (1.0 + Finish->Metallic * (1.0 - F) * (Layer_Pigment_Colour[pBLUE]  - 1.0));

        CRGBAddScaledEq(Colour, Intensity, Cs);
      }
      else
      {
        Colour[pRED]   += Intensity * Light_Colour[pRED];
        Colour[pGREEN] += Intensity * Light_Colour[pGREEN];
        Colour[pBLUE]  += Intensity * Light_Colour[pBLUE];
      }
    }
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Diffuse
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
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static void Diffuse (FINISH *Finish, VECTOR IPoint, RAY *Eye, VECTOR  Layer_Normal, COLOUR Layer_Pigment_Colour, COLOUR Colour, DBL Attenuation, OBJECT *Object)
{
	LIGHT_SOURCE *Light_Source;
	VECTOR REye;
	int i;

	if ((Finish->Diffuse == 0.0) && (Finish->Specular == 0.0) && (Finish->Phong == 0.0))
	{
		return;
	}

	if (Finish->Specular != 0.0)
	{
		REye[X] = -Eye->Direction[X];
		REye[Y] = -Eye->Direction[Y];
		REye[Z] = -Eye->Direction[Z];
	}

	// global light sources, if not turned off for this object
	if((Object->Flags & NO_GLOBAL_LIGHTS_FLAG) != NO_GLOBAL_LIGHTS_FLAG)
	{
		for (i = 0, Light_Source = Frame.Light_Sources;
		   Light_Source != NULL;
		   Light_Source = Light_Source->Next_Light_Source, i++)
		{
			Diffuse_One_Light(Light_Source, i, REye, Finish, IPoint, Eye, Layer_Normal, Layer_Pigment_Colour, Colour, Attenuation, Object);
		}
	}
	// local light sources from a light group, if any
	if(Object->LLights)
	{
		for (Light_Source = Object->LLights;
		   Light_Source != NULL;
		   Light_Source = Light_Source->Next_Light_Source)
		{
			Diffuse_One_Light(Light_Source, -1, REye, Finish, IPoint, Eye, Layer_Normal, Layer_Pigment_Colour, Colour, Attenuation, Object);
		}
	}
}


/*****************************************************************************
*
* FUNCTION
*
*   Diffuse_One_Light
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
*   -
*
* CHANGES
*
*   Jun 2000 : Split from Diffuse to implement light groups [trf]
*
******************************************************************************/

static void Diffuse_One_Light(LIGHT_SOURCE *Light_Source, int Light_Index, VECTOR REye, FINISH *Finish, VECTOR IPoint, RAY *Eye, VECTOR  Layer_Normal, COLOUR Layer_Pigment_Colour, COLOUR Colour, DBL Attenuation, OBJECT *Object)
{
    DBL Light_Source_Depth, Cos_Shadow_Angle;
    RAY Light_Source_Ray;
    COLOUR Light_Colour;

    /* Get a colour and a ray. */

    do_light(Light_Source, &Light_Source_Depth, &Light_Source_Ray, Eye, IPoint, Light_Colour);

    /* Don't calculate spotlights when outside of the light's cone. */

    if ((fabs(Light_Colour[pRED])   < EPSILON) &&
        (fabs(Light_Colour[pGREEN]) < EPSILON) &&
        (fabs(Light_Colour[pBLUE])  < EPSILON))
    {
      return;
    }

    /* See if light on far side of surface from camera. */

	/* NK 1998 double_illuminate - changed to Test_Flag */
    if (!(Test_Flag(Object, DOUBLE_ILLUMINATE_FLAG)))
    {
      VDot(Cos_Shadow_Angle, Layer_Normal, Light_Source_Ray.Direction);

      if (Cos_Shadow_Angle < EPSILON)
      {
        return;
      }
    }

    /*
     * If light source was not blocked by any intervening object, then
     * calculate it's contribution to the object's overall illumination.
     */

    if ((opts.Quality_Flags & Q_SHADOW) && ((Light_Source->Projected_Through_Object != NULL) || (Light_Source->Light_Type != FILL_LIGHT_SOURCE)))
    {
      /* If this surface point has already been tested use previous result. */

	  if(Light_Index >= 0)
	  {
	     if (Light_List[Light_Index].Tested)
	     {
	       Assign_Colour(Light_Colour, Light_List[Light_Index].Colour);
         }
         else if((Light_Source->Type & LIGHT_GROUP_LIGHT_OBJECT) != LIGHT_GROUP_LIGHT_OBJECT) // don't cache light group lights [trf]
         {
           block_light_source(Light_Source, Light_Source_Depth, &Light_Source_Ray, Eye, IPoint, Light_Colour);

           /* Store light colour. */

	       Light_List[Light_Index].Tested = true;

	       Assign_Colour(Light_List[Light_Index].Colour, Light_Colour);
	     }
      }
      else
      {
      	// added to support light groups [trf]
        block_light_source(Light_Source, Light_Source_Depth, &Light_Source_Ray, Eye, IPoint, Light_Colour);
      }
    }

    if ((fabs(Light_Colour[pRED])   > EPSILON) ||
        (fabs(Light_Colour[pGREEN]) > EPSILON) ||
        (fabs(Light_Colour[pBLUE])  > EPSILON))
    {
      if (Finish->Diffuse > 0.0)
      {
        do_diffuse(Finish,&Light_Source_Ray,Layer_Normal,Colour,Light_Colour,Layer_Pigment_Colour, Attenuation);
      }

      /* NK rad 
        don't compute highlights for radiosity gather rays, since this causes
        problems with colors being far too bright
        */
      if ( (Light_Source->Light_Type != FILL_LIGHT_SOURCE) &&
           (Radiosity_Trace_Level<=1) )
      {
        if (Finish->Phong > 0.0)
        {
          do_phong(Finish,&Light_Source_Ray,Eye->Direction,Layer_Normal,Colour,Light_Colour, Layer_Pigment_Colour);
        }

        if (Finish->Specular > 0.0)
        {
          do_specular(Finish,&Light_Source_Ray,REye,Layer_Normal,Colour,Light_Colour, Layer_Pigment_Colour);
        }
      }

      if (Finish->Irid > 0.0)
      {
        do_irid(Finish,&Light_Source_Ray,Layer_Normal,IPoint,Colour);
      }

    }
  }


/*****************************************************************************
*
* FUNCTION
*
*   PhotonDiffuse (based on Diffuse)
*
*   Preconditions:
*     same as Diffuse() with this addition:
*
*     If photonOptions.photonsEnabled is true now, then
*     InitBacktraceEverything must have been called with
*     photonOptions.photonsEnabled true.
*
*
* AUTHOR
*
*   Nathan Kopp (this is based on Diffuse)
*
* DESCRIPTION
*
*   Computes diffuse, phong, specular, etc. based on the incoming photons
*   stored in the various photon maps.
*
* CHANGES
*
*   -
*
******************************************************************************/

static void PhotonDiffuse (FINISH *Finish, VECTOR IPoint, RAY *Eye, VECTOR  Layer_Normal, VECTOR Raw_Normal, COLOUR Layer_Pigment_Colour, COLOUR Colour, DBL Attenuation, OBJECT *Object)
{
  DBL Cos_Shadow_Angle;
  RAY Light_Source_Ray;
  VECTOR REye;
  COLOUR Light_Colour, TempCol, Colour2;
  DBL Size, r, tempr;
  int n, tempn;
  int j;
  int step;
  DBL thisDensity=0;
  DBL prevDensity=0.0000000000000001; /* avoid div-by-zero error */
  int expanded = false;
  DBL att;  /* attenuation for lambertian compensation & filters */

  if (!photonOptions.photonsEnabled || photonOptions.photonMap.numPhotons<1)
  {
    Make_ColourA(Colour,0.0,0.0,0.0,0.0,0.0);
    return;
  }

  if ((Finish->Diffuse == 0.0) && (Finish->Specular == 0.0) && (Finish->Phong == 0.0))
  {
    Make_ColourA(Colour,0.0,0.0,0.0,0.0,0.0);
    return;
  }

  /* statistics */
  Increase_Counter(stats[Gather_Performed_Count]);

  if (Finish->Specular != 0.0)
  {
    REye[X] = -Eye->Direction[X];
    REye[Y] = -Eye->Direction[Y];
    REye[Z] = -Eye->Direction[Z];
  }

  Make_Colour(Colour,0,0,0);

  Size = photonOptions.photonMap.minGatherRad;

  Make_Colour(Colour2,0,0,0);

  n=-1;
  step=0;
  while(n<photonOptions.minGatherCount && step<photonOptions.photonMap.gatherNumSteps)
  {
    Make_Colour(TempCol,0,0,0);
    tempr = 0;

    /* gather the photons */
    if (photonsAlreadyGathered<=0)
    {
      tempn=gatherPhotons(IPoint, Size, &tempr,Layer_Normal,true,&photonOptions.photonMap);
    }
    else
    {
      tempn = photonsAlreadyGathered;
      tempr = previousRad;
    }

    /* now go through these photons and add up their contribution */
    for(j=0; j<tempn; j++)
    {
      /*DBL theta,phi;*/
      int theta,phi;

      /* convert small color to normal color */
      photonRgbe2colour(Light_Colour, photonOptions.photonGatherList[j]->Colour);

      /* Make_Colour(Light_Colour,1,0,0); use this for testing */

      /* convert theta/phi to vector direction 
         Use a pre-computed array of sin/cos to avoid many calls to the
         sin() and cos() functions.  These arrays were initialized in
         InitBacktraceEverything.
      */
      theta = photonOptions.photonGatherList[j]->theta+127;
      phi = photonOptions.photonGatherList[j]->phi+127;
      
      Light_Source_Ray.Direction[Y] = photonOptions.sinTheta[theta];
      Light_Source_Ray.Direction[X] = photonOptions.cosTheta[theta];

      Light_Source_Ray.Direction[Z] = Light_Source_Ray.Direction[X]*photonOptions.sinTheta[phi];
      Light_Source_Ray.Direction[X] = Light_Source_Ray.Direction[X]*photonOptions.cosTheta[phi];

      VSub(Light_Source_Ray.Initial, photonOptions.photonGatherList[j]->Loc, Light_Source_Ray.Direction);

      /* this compensates for real lambertian (diffuse) lighting (see paper) */
      /* use raw normal, not layer normal */
      /* VDot(att, Layer_Normal, Light_Source_Ray.Direction); */
      VDot(att, Raw_Normal, Light_Source_Ray.Direction);
      if (att>1) att=1.0;
      if (att<.1) att = 0.1; /* limit to 10x - otherwise we get bright dots */
      att = 1.0 / fabs(att);

      /* do gaussian filter */
      /*att *= 0.918*(1.0-(1.0-exp((-1.953) * photonOptions.photonDistances[j])) / (1.0-exp(-1.953)) );*/
      /* do cone filter */
      /*att *= 1.0-(sqrt(photonOptions.photonDistances[j])/(4.0 * tempr)) / (1.0-2.0/(3.0*4.0));*/

      VScaleEq(Light_Colour,att);

      /* See if light on far side of surface from camera. */
      if (!(Test_Flag(Object, DOUBLE_ILLUMINATE_FLAG)))
      {
        VDot(Cos_Shadow_Angle, Layer_Normal, Light_Source_Ray.Direction);
        if (Cos_Shadow_Angle < EPSILON)
          continue;
      }

      /* now add diffuse, phong, specular, irid contribution */
      if (Finish->Diffuse > 0.0)
      {
        do_diffuse(Finish,&Light_Source_Ray,Layer_Normal,TempCol,Light_Colour,Layer_Pigment_Colour, Attenuation);
      }

      /* NK rad 
        don't compute highlights for radiosity gather rays, since this causes
        problems with colors being far too bright
        */
      if(Radiosity_Trace_Level<=1)
      /* NK ---- */
      {
        if (Finish->Phong > 0.0)
        {
          do_phong(Finish,&Light_Source_Ray,Eye->Direction,Layer_Normal,TempCol,Light_Colour, Layer_Pigment_Colour);
        }
        if (Finish->Specular > 0.0)
        {
          do_specular(Finish,&Light_Source_Ray,REye,Layer_Normal,TempCol,Light_Colour, Layer_Pigment_Colour);
        }
      }

      if (Finish->Irid > 0.0)
      {
        do_irid(Finish,&Light_Source_Ray,Layer_Normal,IPoint,TempCol);
      }

    }

    /* density of this search */
    thisDensity = tempn / (tempr*tempr);

    /*
      this next line handles the adaptive search
      if
        the density change ((thisDensity-prevDensity)/prevDensity) is small enough
          or
        this is the first time through (step==0)
          or
        the number gathered is less than photonOptions.minExpandCount and greater than zero

      then
        use the color from this new gathering step and discard any previous
        color

      This adaptive search is explained my paper "Simulating Reflective and Refractive
      Caustics in POV-Ray Using a Photon Map" - May, 1999
    */
    if(((thisDensity-prevDensity)/prevDensity < photonOptions.expandTolerance) 
       || (step==0) 
       || (tempn<photonOptions.minExpandCount && tempn>0))
    {
      /* it passes the tests, so use the new color */
      
      if (step>0)
        expanded = true;

      prevDensity = thisDensity;
      if (prevDensity==0)
        prevDensity = 0.0000000000000001;  /* avoid div-by-zero error */

      Assign_Colour(Colour2, TempCol);

      r = tempr;
      n = tempn;
    }

    if(photonsAlreadyGathered)
    {
      step = photonOptions.photonMap.gatherNumSteps; /* so we don't gather again */
    }
    else
    {
      Size+=photonOptions.photonMap.gatherRadStep;
      step++;
    }

  }
  
  /* stats */
  if (expanded)
    Increase_Counter(stats[Gather_Expanded_Count]);

  /* finish the photons equation */
  VScaleEq(Colour2, (DBL)(1.0)/(M_PI*r*r));

  /* add photon contribution to total lighting */
  VAddEq(Colour, Colour2);

  /* save results for subsequent layers */
  /* even if an expanded search was thrown away, the closest photons n will
  still be the closest n photons */
  previousRad = r;
  photonsAlreadyGathered = n;
}


/*****************************************************************************
*
* FUNCTION
*
*   Reflect
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
*   Trace a ray along the direction of the reflected light and
*   return light internsity coming from that direction.
*
* CHANGES
*
*   JUN 1997 : Changed to return color coming along the reflected ray. [DB]
*   This function heavily edited by MBP to implement reflection-blur. 
******************************************************************************/

static void Reflect(VECTOR IPoint, RAY *Ray, VECTOR Normal, VECTOR Raw_Normal, COLOUR Colour, DBL Weight)
{
  RAY New_Ray;
  COLOUR Temp_Colour2;
  DBL n, n2;

  /* Object-Ray Options [ENB 9/97] */
  In_Reflection_Ray = true;

  Trace_Level++;

  /* NK phmap - added checking for backtraceFlag */
  if(!backtraceFlag)
  {
    Make_ColourA (Colour, 0.0, 0.0, 0.0, 0.0, 0.0);
  }

#define Jitter_Normal Normal
#define Jitter_Raw_Normal Raw_Normal

    /* The rest of this is essentally what was originally here, with small changes. */
    VDot(n,Ray->Direction, Jitter_Normal);
    n *= -2.0;
    VAddScaled(New_Ray.Direction, Ray->Direction, n, Jitter_Normal);

    /* Nathan Kopp & CEY 1998 - Reflection bugfix
      if the new ray is going the opposet direction as raw normal, we
      need to fix it.
      */

    VDot(n, New_Ray.Direction, Jitter_Raw_Normal);

    if (n < 0.0)
    {
      /* It needs fixing.  Which kind? */

      VDot(n2,New_Ray.Direction,Jitter_Normal);

      if (n2 < 0.0)
      {
        /* reflected inside rear virtual surface. Reflect Ray using Raw_Normal */
        VDot(n,Ray->Direction,Jitter_Raw_Normal);
        n *= -2.0;
        VAddScaled(New_Ray.Direction, Ray->Direction, n,Jitter_Raw_Normal);
      }
      else
      {
        /* Double reflect NRay using Raw_Normal */
        /*VDot(n,New_Ray.Direction,Jitter_Raw_Normal); - kept the old n around */
        n *= -2.0;
        VAddScaledEq(New_Ray.Direction, n, Jitter_Raw_Normal);
      }
    }
    VNormalizeEq(New_Ray.Direction);
    /* NK & CEY ---- */

    Assign_Vector(New_Ray.Initial, IPoint);

    Copy_Ray_Containers(&New_Ray, Ray);

    Increase_Counter(stats[Reflected_Rays_Traced]);

    /* Trace reflected ray. */
    /* NK phmap - added checking for backtraceFlag */
    if(!backtraceFlag)
    {
    Trace (&New_Ray, Temp_Colour2, Weight);
    VAddEq(Colour, Temp_Colour2);
  }
  else
  {
    Trace(&New_Ray, Colour, Weight);
  }
  Trace_Level--;
  In_Reflection_Ray = false;
}


/*****************************************************************************
*
* FUNCTION
*
*   Refract
*
* INPUT
*
*   Interior - interior of the current object containing the ior to use
*   IPoint   - current intersection point (here the new ray starts)
*   Ray      - current incoming ray that will be refracted, transmitted
*              or reflected (due to total internal reflection)
*   Normal   - surface normal at the current intersection point
*   Colour   - current color emitted back along the ray
*   Weight   - current weight used by the adaptive tree depth control
*
* OUTPUT
*
*   Colour   - current color including the light due to refraction,
*              transmission or total internal reflection
*
* RETURNS
*
*   int - true, if total internal reflection occured
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   Trace a transmitted ray (either refracted or reflected due to total
*   internal reflection) and return the light intesity coming from the
*   direction of the transmitted ray.
*
* CHANGES
*
*   Aug 1995 : Modified to correctly handle the contained texture
*              list in the transmit only case. [DB]
*
*   Jun 1997 : Rewritten to use interior structure. [DB]
*
*   Aug 1997 : Set "In_Reflection_Ray" to true for Object-Ray Options. [ENB]
*   Spring 1998 : Rewritten to incorporate Daren Wilson's dispersion AND photon
*                 mapping.
*
******************************************************************************/
/* NK phmap ------------
  the Refract function here is highly modified and probably quite
  messy right now.  The primary reason is the inclusion of Daren
  Wilson's dispersion code.
  
  This needs MUCH better comments for the whole thing.
  Mr. Wilson & Mr. Parker: if you want to help clean this up and add
  comments, you're very much welcome to.  ;-)

  [
   Comments by "Mr. Parker": pursuant to the above invitation, I have 
   done exactly that.  None of the original code remains.  :-)
   15 June 2001
  ]
*/

static void Dispersion_Element_Hue(COLOUR Hue, int elem, int nelems)
{
  /* 
    Gives color to a dispersion element.

    Requirements:
      * Sum of all hues must add to white (or white*constand)
      (white tiles seen through glass should still be white)
      
    * Each hue must be maximally saturated, bright
      (The code shown here cheats a little)
      [RLP: maximally saturated, anyway.  maximally bright is a
       mistake.  And the new code no longer cheats.]
      
    * colors must range from red at elem=1 to violet at elem=nelems

    The equations herein were derived by back-of-the-envelope
    curve fitting to some RGB color-matching function tables 
    I found on the web somewhere.  I could have just interpolated
    those tables, but I think this gives results that are as good 
    and scale well.  The various magic numbers below were 
    determined empirically to match four important criteria:

    1) The peak for a given element must be at the same place as
       on the color-matching table.
    2) The width of a given element's curve must be about the same.
    3) The width of the clipped portion of a given element must
       be about the same.
    4) The integral of each element's curve must be approximately
       the same as the integral of each of the other elements.
  */
  
  SNGL hc, r, g, b;

  /*
    When I derived the functions, I went with the assumption that
    0 is near-UV, and 1 is near-IR.  When I looked at the code, I
    realized that it wanted exactly the reverse.  Thus the "1.0-"
  */
  hc = .964-.934*((SNGL)(elem-1)/(SNGL)(nelems-1));

  /*
    The blue component.  The peak is at hc=.28, and there is no
    clipped part.  .98 is a scaling factor to make the integrals
    come out even.  4 determines the width of the nonzero part 
    of the curve; the larger the narrower.  The 1 helps determine
    the width of the clipped portion (but blue has no clipped 
    portion.)  Four constraints, four magic numbers.  Who'da thunk
    it?
  */
  b = .98*(1-Sqr(4*(hc-.28)));
  if ( b<0 ) b=0;
  Hue[pBLUE] = b;
  
  /*
    This is substantially the same code as the blue code above,
    with different magic numbers.
  */
  g = .97*(1.1-Sqr(4.5*(hc-.57)));
  if ( g<0 ) g=0;
  Hue[pGREEN] = g;

  /* 
    This is also substantially the same code as the blue, with 
    one exception: the red component has a second, smaller peak
    at the violet end of the spectrum.  That is represented by
    the second set of magic numbers below.  Also, red is the 
    component to which the others are standardized (because it
    had the smallest integral to begin with) so there is no 
    .9x fudge-factor.
  */
  r = 1.15-Sqr(5*(hc-.75));
  if ( r<0 ) {
    r = .12-Sqr(4*(hc-.12));
  }
  if ( r<0 ) {
    r=0;
  }
  Hue[pRED]=r;
}

static DBL Element_IOR(DBL ior, DBL disp,  int e, int nelems)
{
  return ior * pow(disp, (e-1)*1.0/(nelems-1)-0.5);
}


static int Refract_Guts(COLOUR Colour,  
    DBL ior,
    VECTOR IPoint,
    VECTOR Normal, VECTOR Local_Normal,
    DBL n,
    RAY *Ray, RAY *NRay,
    DBL Weight,
    VECTOR Raw_Normal
    )
{
  /* Adds result of refracted ray to Colour            */
  /* All other args are input args - don't change them */
  /* Pass by value the RAYs and maybe make local copy of normals? */
  
  DBL t;
  
    /* Compute refrated ray direction using Heckbert's method. */
    t = 1.0 + Sqr(ior) * (Sqr(n) - 1.0);

    if (t < 0.0)
    {
      /* Total internal reflection occures. */
      Increase_Counter(stats[Internal_Reflected_Rays_Traced]);

      Reflect(IPoint, /*N?*/ Ray, /*L?*/ Normal, Raw_Normal, Colour, Weight);
      return 1;
    }

    t = ior * n - sqrt(t);

    VLinComb2(NRay->Direction, ior, /*N?*/Ray->Direction, t, Local_Normal);

    /* Trace a refracted ray. */
    Increase_Counter(stats[Refracted_Rays_Traced]);
    
    Trace_Level++;
    Trace(NRay, Colour, Weight);
    Trace_Level--;
  
  return 0;
}


static int BacktraceRefract(INTERIOR *Interior, VECTOR IPoint, RAY *Ray, VECTOR Normal, VECTOR Raw_Normal, COLOUR Colour, DBL Weight)
{
  int nr;
  DBL n, ior, disp, ior_mult;
  VECTOR Local_Normal;
  RAY NRay;
  COLOUR Hue, Elem;

  /* Set up new ray. */
  Copy_Ray_Containers(&NRay, Ray);
  Assign_Vector(NRay.Initial, IPoint);

  /* get the dispersion elements from the light's blend map */
/*
  if ((disp_elem == 0) && (photonOptions.Light->blend_map))
      disp_nelems = photonOptions.Light->blend_map->Number_Of_Entries;
*/  
  /* Get ratio of iors depending on the interiors the ray is traversing. */
  if ( disp_elem == 0 ) {
    disp_nelems = Interior->Disp_NElems;
  } 
   
  if (Ray->Index == -1)
  {
    /* The ray is entering from the atmosphere. */
    Ray_Enter(&NRay, Interior);
    ior = Frame.Atmosphere_IOR / Interior->IOR;
    disp = Frame.Atmosphere_Dispersion / Interior->Dispersion;
  }
  else
  {
    /* The ray is currently inside an object. */
    if ((nr = Interior_In_Ray_Container(&NRay, Interior)) >= 0)
    {
      /* The ray is leaving the current object. */
      Ray_Exit(&NRay, nr);

      if (NRay.Index == -1)
      {
        /* The ray is leaving into the atmosphere. */
        ior = Interior->IOR / Frame.Atmosphere_IOR;
        disp = Interior->Dispersion / Frame.Atmosphere_Dispersion;
      }
      else if (nr==NRay.Index+1)
      {
        /* The ray is leaving into another object. */

        /* NOTE: we only count it as "leaving" if it hasn't already left.  If
           we've already entered object B and now we're leaving object A, then
           that really means that we already left object A (back when we entered
           object B), so if that is the case, this drops down to the "else" clause
           (because nr will be less than NRay.Index+1)*/

        ior = Interior->IOR / NRay.Interiors[NRay.Index]->IOR;
        disp = Interior->Dispersion / NRay.Interiors[NRay.Index]->Dispersion;
      }
      else
      {
        ior = 1.0;
        disp = 1.0;
      }

    }
    else
    {
      /* The ray is entering a new object. */
      ior = NRay.Interiors[NRay.Index]->IOR / Interior->IOR;
      disp = NRay.Interiors[NRay.Index]->Dispersion / Interior->Dispersion;

      Ray_Enter(&NRay, Interior);

    }
  }

  /* Do the two mediums traversed have the sampe indices of refraction? */
  /* DSW: they must also match in dispersion ratios                     */
  if (fabs(ior - 1.0) < EPSILON
   && fabs(disp - 1.0) < EPSILON)
  {
    COLOUR lc;

    /* Only transmit the ray. */
    Assign_Vector(NRay.Direction, Ray->Direction);

    /* Trace a transmitted ray. */
    Increase_Counter(stats[Transmitted_Rays_Traced]);
    
    lc[0] = Colour[0]*(GFilCol[0] * GFilCol[pFILTER] + GFilCol[pTRANSM]);
    lc[1] = Colour[1]*(GFilCol[1] * GFilCol[pFILTER] + GFilCol[pTRANSM]);
    lc[2] = Colour[2]*(GFilCol[2] * GFilCol[pFILTER] + GFilCol[pTRANSM]);

    Trace_Level++;
    Trace(&NRay, lc, Weight);
    Trace_Level--;
  }
  else    /* different media */
  {
    /* Refract the ray. */
    VDot(n, Ray->Direction, Normal);

    if (n <= 0.0)
    {
      Assign_Vector(Local_Normal, Normal);
      n = -n;
    }
    else
    {
      Local_Normal[X] = -Normal[X];
      Local_Normal[Y] = -Normal[Y];
      Local_Normal[Z] = -Normal[Z];
    }

    /* DSW: If this is the first time this ray is encountering          */
    /* a dispersive medium, we need to loop over the color components.  */
    /* If the ray has already been dispersed, use trace a monochromatic */
    /* ray, with no further looping.                                    */
 
    if (disp_elem>0 || disp_nelems <= 1 || fabs(disp-1.0)<EPSILON || firstRadiosityPass)
    {
      DBL ior2;
      /* We're already tracing an element */
      COLOUR lc;
      lc[0] = Colour[0]*(GFilCol[0] * GFilCol[pFILTER] + GFilCol[pTRANSM]);
      lc[1] = Colour[1]*(GFilCol[1] * GFilCol[pFILTER] + GFilCol[pTRANSM]);
      lc[2] = Colour[2]*(GFilCol[2] * GFilCol[pFILTER] + GFilCol[pTRANSM]);
      if (disp_nelems>1 && disp_elem > 0)
        ior2 = Element_IOR( ior, disp, disp_elem, disp_nelems );
      else
        ior2 = ior;
      return Refract_Guts(lc, ior2, IPoint, Normal, Local_Normal, n, Ray, &NRay, Weight, Raw_Normal);
    }

    else
    {
      /* An undispersed ray needs to be broken into elements */
      
      /* Trace each element */
      COLOUR lc;
      DBL ior2;

      lc[0] = Colour[0]/disp_nelems *3.0*(GFilCol[0] * GFilCol[pFILTER] + GFilCol[pTRANSM]);
      lc[1] = Colour[1]/disp_nelems *3.0*(GFilCol[1] * GFilCol[pFILTER] + GFilCol[pTRANSM]);
      lc[2] = Colour[2]/disp_nelems *3.0*(GFilCol[2] * GFilCol[pFILTER] + GFilCol[pTRANSM]);

      ior2 = ior /sqrt(disp);
      ior_mult = pow(disp, 1.0/(disp_nelems-1));
      for (disp_elem = 1; disp_elem <= disp_nelems; disp_elem++)
      {
        Dispersion_Element_Hue(Hue, disp_elem, disp_nelems);
        Elem[pRED]   = lc[pRED]   * Hue[pRED];
        Elem[pGREEN] = lc[pGREEN] * Hue[pGREEN];
        Elem[pBLUE]  = lc[pBLUE]  * Hue[pBLUE];
        
          if ( Elem[pRED] > EPSILON ||
            Elem[pGREEN] > EPSILON ||
            Elem[pBLUE] > EPSILON ) {
            Refract_Guts(Elem, ior2, IPoint, Normal, Local_Normal, n, Ray, &NRay, Weight, Raw_Normal);
          }

          ior2 *= ior_mult;
        }
      disp_elem = 0;   /* reset this for next pixel's tracing */
      disp_nelems = 0;   /* reset this for next pixel's tracing */
    }
  
  
  }    /* end of different media */
  
  return 0;
}

static int Refract(INTERIOR *Interior, VECTOR IPoint, RAY *Ray, VECTOR Normal, VECTOR Raw_Normal, COLOUR Colour, DBL Weight)
{
  int nr;
  DBL n, ior, disp, ior_mult;
  VECTOR Local_Normal;
  RAY NRay;
  COLOUR Hue, Sum, Elem;

  /* Set up new ray. */
  Copy_Ray_Containers(&NRay, Ray);
  Assign_Vector(NRay.Initial, IPoint);

  if (disp_elem == 0)
  disp_nelems = Interior->Disp_NElems;
  
  disp = 1.0;

  /* Get ratio of iors depending on the interiors the ray is traversing. */
  if (Ray->Index == -1)
  {
    /* The ray is entering from the atmosphere. */
    Ray_Enter(&NRay, Interior);
    ior = Frame.Atmosphere_IOR / Interior->IOR;
    if (disp_nelems>1)
      disp = Frame.Atmosphere_Dispersion / Interior->Dispersion;
  }
  else
  {
    /* The ray is currently inside an object. */
    if ((nr = Interior_In_Ray_Container(&NRay, Interior)) >= 0)
    {
      /* The ray is leaving the current object. */
      Ray_Exit(&NRay, nr);

      if (NRay.Index == -1)
      {
        /* The ray is leaving into the atmosphere. */
        ior = Interior->IOR / Frame.Atmosphere_IOR;
        if (disp_nelems>1)
          disp = Interior->Dispersion / Frame.Atmosphere_Dispersion;
      }
      else if (nr==NRay.Index+1)
      {
        /* The ray is leaving into another object. */
        
        /* NOTE: we only count it as "leaving" if it hasn't already left.  If
           we've already entered object B and now we're leaving object A, then
           that really means that we already left object A (back when we entered
           object B), so if that is the case, this drops down to the "else" clause
           (because nr will be less than NRay.Index+1)*/

        ior = Interior->IOR / NRay.Interiors[NRay.Index]->IOR;

        /* Use the largest disp_nelems of the two interiors */
        if (NRay.Interiors[NRay.Index]->Disp_NElems > disp_nelems && disp_elem == 0)
          disp_nelems = NRay.Interiors[NRay.Index]->Disp_NElems;

        if (disp_nelems>1)
          disp = Interior->Dispersion / NRay.Interiors[NRay.Index]->Dispersion;
      }
      else
      {
        ior = 1.0;
      }
    }
    else
    {
      /* The ray is entering a new object. */
      ior = NRay.Interiors[NRay.Index]->IOR / Interior->IOR;
      
      /* Use the largest disp_nelems of the two interiors */
      if (NRay.Interiors[NRay.Index]->Disp_NElems > disp_nelems && disp_elem == 0)
        disp_nelems = NRay.Interiors[NRay.Index]->Disp_NElems;

      if (disp_nelems>1)
        disp = NRay.Interiors[NRay.Index]->Dispersion / Interior->Dispersion;

      Ray_Enter(&NRay, Interior);

    }
  }

  /* Do the two mediums traversed have the sampe indices of refraction? */
  /* DSW: they must also match in dispersion ratios                     */
  if (fabs(ior - 1.0) < EPSILON
   && fabs(disp - 1.0) < EPSILON)
  {
    /* Only transmit the ray. */
    Assign_Vector(NRay.Direction, Ray->Direction);
  
    /* Trace a transmitted ray. */
    Increase_Counter(stats[Transmitted_Rays_Traced]);
      
    Trace_Level++;
    Trace(&NRay, Colour, Weight);
    Trace_Level--;
  }
  else    /* different media */
  {
    /* Refract the ray. */
    VDot(n, Ray->Direction, Normal);
  
    if (n <= 0.0)
    {
      Assign_Vector(Local_Normal, Normal);
      n = -n;
    }
    else
    {
      Local_Normal[X] = -Normal[X];
      Local_Normal[Y] = -Normal[Y];
      Local_Normal[Z] = -Normal[Z];
    }
  
    /* DSW: If this is the first time this ray is encountering          */
    /* a dispersive medium, we need to loop over the color components.  */
    /* If the ray has already been dispersed, use trace a monochromatic */
    /* ray, with no further looping.                                    */
 
    if (disp_elem>0 || disp_nelems <= 1 || fabs(disp-1.0)<EPSILON || firstRadiosityPass)
    {
      /* We're already tracing an element */
      if (disp_nelems>1 && disp_elem > 0)
        ior = Element_IOR(ior, disp, disp_elem, disp_nelems);
        
      return Refract_Guts(Colour, ior, IPoint, Normal, Local_Normal, n, Ray, &NRay, Weight, Raw_Normal);
    }

    else
    {
      /* An undispersed ray needs to be broken into elements */
      
      /*if (disp_nelems==0) disp_nelems = DEFAULT_DISP_NELEMS;*/
      /* or use an adaptive formula? */
      
      /* Trace each element */
      
        ior = ior /sqrt(disp);
        ior_mult = pow(disp, 1.0/(disp_nelems-1));
    
      Sum[pRED]=0.0f;
      Sum[pGREEN]=0.0f;
      Sum[pBLUE]=0.0f;
      
      
        for (disp_elem = 1;  disp_elem <= disp_nelems;  disp_elem++)
        {
          Refract_Guts(Elem, ior, IPoint, Normal, Local_Normal, n, Ray, &NRay, Weight, Raw_Normal);
          
          Dispersion_Element_Hue(Hue, disp_elem, disp_nelems);

          /* speed it up by building a lookup table at povray init */
          /* or no: need to recalc if disp_nelems is adaptive,     */
          /* then we'd need a bunch of tables or calc fresh        */
          
          Sum[pRED]   += Elem[pRED]   * Hue[pRED];
          Sum[pGREEN] += Elem[pGREEN] * Hue[pGREEN];
          Sum[pBLUE]  += Elem[pBLUE]  * Hue[pBLUE];
          
          ior *= ior_mult;
        }
          
        /* compute final color, with fudge factor */
        Colour[pRED]   = Sum[pRED]   /disp_nelems *3.0;
        Colour[pGREEN] = Sum[pGREEN] /disp_nelems *3.0;
        Colour[pBLUE]  = Sum[pBLUE]  /disp_nelems *3.0;
        
        disp_elem = 0;   /* reset this for next pixel's tracing */
      disp_nelems = 0;   /* reset this for next pixel's tracing */
    }
  
  
  }    /* end of different media */

  return(0);
}


/*****************************************************************************
*
* FUNCTION
*
*   create_texture_list
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Chris Young based on Dieter Bayer code
*
* DESCRIPTION
*
*   Get the list of textures used by current object and the list of
*   appropriate weights for each texture. Only multi-colored objects
*   will have more than one texture.
*
* CHANGES
*
*   Feb 1995 : Added code for triangle mesh texturing. [DB]
*
*   Jul 1995 : Modified code to use pre-allocated lists. [DB]
*
******************************************************************************/

static int create_texture_list(INTERSECTION *Ray_Intersection, DBL realNormDir)
{
  int Texture_Count;
  BLOB *Blob;
  MESH_TRIANGLE *Triangle;
  bool useInteriorTexture = false;
  bool skipMultitextured = false;

  if((realNormDir > 0)&&(Ray_Intersection->Object->Interior_Texture != NULL))
      useInteriorTexture = true;


  /*
    Deal with cutaway textures 

    Ray_Intersection->Csg is the root-level object in this CSG tree
	  if the intersection object is the child of a multi-tex (cutaway_texture) CSG object 
  */
  CSG* Csg;
  Csg = (CSG*)Ray_Intersection->Csg;
  if(Csg != NULL &&
     Test_Flag(Ray_Intersection->Object, MULTITEXTURE_FLAG) &&
     ( Csg->Methods == &CSG_Intersection_Methods ||
       Csg->Methods == &CSG_Merge_Methods ||
       Csg->Methods == &CSG_Union_Methods ) )
	{
    if(Ray_Intersection->Object->Texture == NULL)
    {
	    Determine_CSG_Textures(Csg, Ray_Intersection->IPoint, &Texture_Count, Texture_List, Weight_List);
      return Texture_Count;   /* return now - skip the rest */
    }
    else
    {
      skipMultitextured = true;
    }
	}

  /* Test, if object is multi-textured. */
  if (Test_Flag(Ray_Intersection->Object, MULTITEXTURE_FLAG) && !skipMultitextured)
  {
    /* Handle blobs. */

    if (Ray_Intersection->Object->Methods == &Blob_Methods)
    {
      Blob = (BLOB *)Ray_Intersection->Object;

      /* Get list of weighted textures. */

      Determine_Blob_Textures(Blob, Ray_Intersection->IPoint, &Texture_Count, Texture_List, Weight_List);
    }

    /* Handle meshes. */

    if (Ray_Intersection->Object->Methods == &Mesh_Methods)
    {
      /* Set texture to triangle's or object's texture. */

      Triangle = (MESH_TRIANGLE *)Ray_Intersection->Pointer;

      if (Triangle->ThreeTex)
      {
        VECTOR Weights;
        Mesh_Interpolate(Weights, Ray_Intersection->IPoint,
            (MESH *)Ray_Intersection->Object, Triangle);

        if (Triangle->Texture >= 0)
          Texture_List[0] = ((MESH *)Ray_Intersection->Object)->Textures[Triangle->Texture];
        else
          Texture_List[0] = Ray_Intersection->Object->Texture;

        if (Triangle->Texture2 >= 0)
          Texture_List[1] = ((MESH *)Ray_Intersection->Object)->Textures[Triangle->Texture2];
        else
          Texture_List[1] = Ray_Intersection->Object->Texture;

        if (Triangle->Texture3 >= 0)
          Texture_List[2] = ((MESH *)Ray_Intersection->Object)->Textures[Triangle->Texture3];
        else
          Texture_List[2] = Ray_Intersection->Object->Texture;

          Weight_List[0] = Weights[0];
          Weight_List[1] = Weights[1];
          Weight_List[2] = Weights[2];

          Texture_Count = 3;
        }
        else
        {
          if (Triangle->Texture >= 0)
          {
            /* NK 1999 moved textures from Mesh_Data_Struct to Mesh_Struct */
            Texture_List[0] = ((MESH *)Ray_Intersection->Object)->Textures[Triangle->Texture];
            /* NK ---- */
        }
        else
        {
          Texture_List[0] = Ray_Intersection->Object->Texture;
        }

        Weight_List[0] = 1.0;

        Texture_Count = 1;
      }
    }
  }
  else if (Ray_Intersection->Object->Texture == NULL)
  {
	  Texture_Count = 0;
  }
  else
  {
    /* Set texture to object's texture. */
    if(useInteriorTexture == true)/*Chris Huff: Interior Texture patch*/
    {
      Texture_List[0] = Ray_Intersection->Object->Interior_Texture;
    }
    else
    {
      Texture_List[0] = Ray_Intersection->Object->Texture;
    }

    Weight_List[0]  = 1.0;

    Texture_Count = 1;
  }

  return(Texture_Count);
}



/*****************************************************************************
*
* FUNCTION
*
*   do_texture_map
*
* INPUT
*
*   Texture          - possibly texture_mapped texture to be evaluated
*   IPoint           - point to be evaluated
*   Raw_Normal       - non-purturbed surface normal
*   Ray              - view ray needed for reflection and highlighs
*                      light source ray needed for caustics
*   Weight           - ADC control value
*   Ray_Intersection - only Ray_Int..->Object->Type actually
*                      needed.  Will clean-up later.
*   Shadow_Flag      - tells if computation should use
*                      compute_lighted_texture or compute_shadow_texture
*
* OUTPUT
*
*   Result_Colour    - If Shadow_Flag true then the illuminated
*                      color (RGB only) of IPoint is returned.
*                      If false, the amount by which a shadow ray is
*                      filtered and attenuated is returned.
*                      Includes RGB and T.
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   This routine recursively calls itself until it gets a
*   non-texture_mapped texture that is potentially layered.
*   It then calls compute_lighted_texture or compute_shadow_texture
*   to compute the color which is returned in the argument Result_Colour.
*
* CHANGES
*
******************************************************************************/

static void do_texture_map(COLOUR Result_Colour, TEXTURE *Texture, VECTOR IPoint, VECTOR  Raw_Normal,
  RAY *Ray, DBL Weight, INTERSECTION *Ray_Intersection, int Shadow_Flag)
{
  BLEND_MAP *Blend_Map = Texture->Blend_Map;
  BLEND_MAP_ENTRY *Prev, *Cur;
  DBL value1, value2;
  COLOUR C2;
  VECTOR TPoint;
  UV_VECT UV_Coords;
  bool decWarpNormalTextures = false;
  /* 
     ipoint - interseciton point (and evaluation point)
     epoint - evaluation point
     tpoint - turbulated/transformed point
  */

  if(opts.Language_Version > 310)
  {
    switch(Texture->Type)
    {
      case NO_PATTERN:
      case PLAIN_PATTERN:
        break;
      case AVERAGE_PATTERN:
      case UV_MAP_PATTERN:
      case BITMAP_PATTERN:
      default:
        if(warpNormalTextures>=warpNormalTextureListMaxSize)
        {
          warpNormalTextureListMaxSize *= 2;
          warpNormalTextureList = (TEXTURE **)POV_REALLOC(warpNormalTextureList, warpNormalTextureListMaxSize * sizeof(TEXTURE *), "Warp normal texture list");
        }
        warpNormalTextureList[warpNormalTextures++] = Texture;
        decWarpNormalTextures = true;
        break;
    }
  }

  if(Texture->Type <= LAST_SPECIAL_PATTERN)
  {
    switch(Texture->Type)
    {
      case NO_PATTERN:
        Make_ColourA(Result_Colour, 1.0, 1.0, 1.0, 1.0, 1.0);
        break;
      case AVERAGE_PATTERN:
        Warp_EPoint(TPoint, IPoint, (TPATTERN *)Texture);
        if(backtraceFlag)
          backtrace_average_textures(Result_Colour, Texture, TPoint, Raw_Normal, Ray, Weight, Ray_Intersection, Shadow_Flag);
        else
          average_textures(Result_Colour, Texture, TPoint, Raw_Normal, Ray, Weight, Ray_Intersection, Shadow_Flag);
        break;
      case UV_MAP_PATTERN:
        // Don't bother warping, simply get the UV vect of the intersection
        UVCoord(UV_Coords, Ray_Intersection->Object, Ray_Intersection);
        TPoint[X] = UV_Coords[U];
        TPoint[Y] = UV_Coords[V];
        TPoint[Z] = 0;
        Cur = &(Texture->Blend_Map->Blend_Map_Entries[0]);
        do_texture_map(Result_Colour, Cur->Vals.Texture, TPoint, Raw_Normal, Ray, Weight, Ray_Intersection, Shadow_Flag);
        break;
      case BITMAP_PATTERN:
        Warp_EPoint (TPoint, IPoint, (TPATTERN *)Texture);
        Texture = material_map(TPoint, Texture);
        do_texture_map(Result_Colour, Texture, TPoint, Raw_Normal, Ray, Weight, Ray_Intersection, Shadow_Flag);
        break;
      case PLAIN_PATTERN:
        if(backtraceFlag)
          compute_backtrace_texture(Result_Colour, Texture, IPoint, Raw_Normal, Ray, Weight, Ray_Intersection);
        else if(Shadow_Flag)
          compute_shadow_texture(Result_Colour, Texture, IPoint, Raw_Normal, Ray, Ray_Intersection);
        else
          compute_lighted_texture(Result_Colour, Texture, IPoint, Raw_Normal, Ray, Weight, Ray_Intersection);
        break;
      default:
        Error("Bad texture type in do_texture_map()");
    }
  }
  else
  {
    /* NK 19 Nov 1999 added Warp_EPoint */
    Warp_EPoint (TPoint, IPoint, (TPATTERN *)Texture);
    value1 = Evaluate_TPat ((TPATTERN *)Texture,TPoint,Ray_Intersection);

    Search_Blend_Map (value1, Blend_Map, &Prev, &Cur);

    Warp_EPoint (TPoint, IPoint, (TPATTERN *)Texture);

    /* NK phmap */
    if(backtraceFlag)
    {
      if(Prev == Cur)
        do_texture_map(Result_Colour, Cur->Vals.Texture, TPoint, Raw_Normal, Ray, Weight, Ray_Intersection, Shadow_Flag);
      else
      {
        value1 = (value1 - Prev->value) / (Cur->value - Prev->value);
        value2 = 1.0 - value1;
        VScale(C2, Result_Colour, value1);
        do_texture_map(C2, Cur->Vals.Texture, TPoint, Raw_Normal, Ray, Weight, Ray_Intersection, Shadow_Flag);
        VScale(C2, Result_Colour, value2);
        do_texture_map(C2, Prev->Vals.Texture, TPoint, Raw_Normal, Ray, Weight, Ray_Intersection, Shadow_Flag);
      }
    }
    else
    {
      do_texture_map(Result_Colour, Cur->Vals.Texture, TPoint, Raw_Normal, Ray, Weight, Ray_Intersection, Shadow_Flag);

      if(Prev != Cur)
      {
        do_texture_map(C2, Prev->Vals.Texture, TPoint, Raw_Normal, Ray, Weight, Ray_Intersection, Shadow_Flag);
        value1 = (value1 - Prev->value) / (Cur->value - Prev->value);
        value2 = 1.0 - value1;
        CLinComb2(Result_Colour,value1,Result_Colour,value2,C2);
      }
    }
  }

  if(decWarpNormalTextures == true)
    warpNormalTextures--;
}




/*****************************************************************************
*
* FUNCTION
*
*   compute_lighted_texture
*
* INPUT
*
*   Texture          - a linked list of texture layers
*   IPoint           - point to be evaluated
*   Raw_Normal       - non-purturbed surface normal
*   Ray              - needed for reflection and highlighs
*   Weight           - ADC control value
*   Intersection - current intersection (need object type and depth)
*
* OUTPUT
*
*   ResCol    - illuminated color of IPoint
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   This routine loops through all layers of a texture and computes
*   the appearent color of the point with illumination, shadows,
*   reflection, refraction... everything.  This piece of code was broken out
*   of Determine_Appearent_Colour because texture_map needs to call it twice.
*
* CHANGES
*
*   Jul 1995 : Added code to support alpha channel. [DB]
*
*   Jul 1995 : Moved code for save list allocation. [DB]
*
*   Aug 1995 : Added code for distance based attenuation in translucent
*              objects and halos. [DB]
*
*   Oct 1996 : Replaced halo code by participating media code. [DB]
*
******************************************************************************/

static void compute_lighted_texture(COLOUR ResCol, TEXTURE *Texture, VECTOR IPoint, VECTOR  Raw_Normal, RAY *Ray, DBL Weight, INTERSECTION *Intersect)
{
  int i, radiosity_done, radiosity_needed;
  int layer_number;
  int inside_hollow_object;
  int one_colour_found, colour_found;
  DBL w1;/*, w2;*/
  DBL /*Normal_Direction, */New_Weight;/*, TempWeight;*/
  DBL Att, Trans, Max_Radiosity_Contribution;
  VECTOR LayNormal, TopNormal;
  COLOUR AttCol, LayCol, RflCol, RfrCol, FilCol;
  COLOUR TmpCol, AmbCol, Tmp;
  INTERIOR *Interior;
  IMEDIA **TmpMedia, **MediaList;
  DBL Cos_Angle_Incidence;
  TEXTURE *Layer;
  int    TIR_occured;

  ComTexData *ctd = NewComTexData();

  DBL    *ListWeight = ctd->ListWeight;
  VECTOR *ListNormal = ctd->ListNormal;
  COLOUR *ListReflec = ctd->ListReflec;
  SNGL   *ListReflEx = ctd->ListReflEx;

  /*
   * ResCol builds up the apparent visible color of the point.
   * Only RGB components are significant.  You can't "see" transparency --
   * you see the color of whatever is behind the transparent surface.
   * This color includes the visible appearence of what is behind the
   * transparency so only RGB is needed.
   */

  Make_ColourA(ResCol, 0.0, 0.0, 0.0, 0.0, 0.0);

  /*
   * FilCol serves two purposes.  It accumulates the filter properties
   * of a multi-layer texture so that if a ray makes it all the way through
   * all layers, the color of object behind is filtered by this object.
   * It also is used to attenuate how much of an underlayer you
   * can see in a layered texture.  Note that when computing the reflective
   * properties of a layered texture, the upper layers don't filter the
   * light from the lower layers -- the layer colors add together (even
   * before we added additive transparency via the "transmit" 5th
   * color channel).  However when computing the transmitted rays, all layers
   * filter the light from any objects behind this object. [CY 1/95]
   */

  /* NK layers - switched transmit component to zero */
  Make_ColourA(FilCol, 1.0, 1.0, 1.0, 1.0, 0.0);

  Trans = 1.0;

  /* Add in radiosity (stochastic interreflection-based ambient light) if desired */

  radiosity_done = false;

  /* Note that there is no gathering of filter or transparency */

  Make_ColourA(AmbCol, 0., 0., 0., 0., 0.);

  /*
   * This block just sets up radiosity for the code inside the loop, which
   * is first-time-through.
   */
  if ((opts.Radiosity_Enabled) && (Radiosity_Trace_Level <= opts.Radiosity_Recursion_Limit))
  {
    radiosity_needed = true;
  }
  else
  {
    radiosity_needed = false;
  }

  /*
   * Loop through the layers and compute the ambient, diffuse,
   * phong and specular for these textures.
   */

  one_colour_found = false;

  for (layer_number = 0, Layer = Texture;
      (Layer != NULL) && (Trans > ADC_Bailout);
      layer_number++, Layer = (TEXTURE *)Layer->Next)
  {
    /* Get perturbed surface normal. */

    Assign_Vector(LayNormal, Raw_Normal);

    if ((opts.Quality_Flags & Q_NORMAL) && (Layer->Tnormal != NULL))
    {
      if(opts.Language_Version>310)
      {
        for(i=0; i<warpNormalTextures; i++)
          Warp_Normal(LayNormal,LayNormal, (TPATTERN *)warpNormalTextureList[i], Test_Flag(warpNormalTextureList[i],DONT_SCALE_BUMPS_FLAG));

        Perturb_Normal(LayNormal, Layer->Tnormal, IPoint, Intersect);
      
        if((Test_Flag(Layer->Tnormal,DONT_SCALE_BUMPS_FLAG)))
          VNormalizeEq(LayNormal);

        for(i=warpNormalTextures-1; i>=0; i--)
          UnWarp_Normal(LayNormal,LayNormal, (TPATTERN *)warpNormalTextureList[i], Test_Flag(warpNormalTextureList[i],DONT_SCALE_BUMPS_FLAG));
      }
      else
        Perturb_Normal(LayNormal, Layer->Tnormal, IPoint, Intersect);
    }

    /* Store top layer normal.*/

    if (!layer_number)
    {
      Assign_Vector(TopNormal, LayNormal);
    }

    /* Get surface colour. */

    New_Weight = Weight * Trans;

    colour_found = Compute_Pigment (LayCol, Layer->Pigment, IPoint, Intersect);

    /*
     * If a valid color was returned set one_colour_found to true.
     * An invalid color is returned if a surface point is outside
     * an image map used just once.
     */

    if (colour_found)
    {
      one_colour_found = true;
    }

    /*
     * This section of code used to be the routine Compute_Reflected_Colour.
     * I copied it in here to rearrange some of it more easily and to
     * see if we could eliminate passing a zillion parameters for no
     * good reason. [CY 1/95]
     */

    if (opts.Quality_Flags & Q_FULL_AMBIENT)
    {
      /* Only use top layer and kill transparency if low quality. */

      Assign_Colour(ResCol, LayCol);

      ResCol[pFILTER] =
      ResCol[pTRANSM] = 0.0;
    }
    else
    {
      /* --------------------------------------------- */
      /* Store vital information for later reflection. */
      if (layer_number == MAX_LAYERS)
      {
        Error("Too many texture layers.");
      }

      ListReflEx[layer_number] = Layer->Finish->Reflect_Exp;
      ListWeight[layer_number] = New_Weight;
      Assign_Vector(ListNormal[layer_number], LayNormal);

      /* angle-dependent reflectivity */
      VDot(Cos_Angle_Incidence, Ray->Direction, LayNormal);
      Cos_Angle_Incidence *= -1.0;

      if (Intersect->Object->Interior || 
        (Layer->Finish->Reflection_Type != 1)) 
      {
        determine_reflectivity (&ListWeight[layer_number], ListReflec[layer_number],
                  Layer->Finish->Reflection_Max, Layer->Finish->Reflection_Min, 
                  Layer->Finish->Reflection_Type, Layer->Finish->Reflection_Falloff, 
                  Cos_Angle_Incidence, Ray, Intersect->Object->Interior);
      }
      else
      {
        Error("Reflection_Type 1 used with no interior.");
      }

      /* for metallic reflection, apply the surface color using the fresnel equation
         (use the same equaltion as "metallic" in phong and specular 
      */
      if (Layer->Finish->Reflect_Metallic != 0.0)
      {
        DBL R_M=Layer->Finish->Reflect_Metallic;

        DBL x = fabs(acos(Cos_Angle_Incidence)) / M_PI_2;
        DBL F = 0.014567225 / Sqr(x - 1.12) - 0.011612903;
        F=min(1.0,max(0.0,F));

        ListReflec[layer_number][0]*=
          (1.0 + R_M * (1.0 - F) * (LayCol[0] - 1.0));
        ListReflec[layer_number][1]*=
          (1.0 + R_M * (1.0 - F) * (LayCol[1] - 1.0));
        ListReflec[layer_number][2]*=
          (1.0 + R_M * (1.0 - F) * (LayCol[2] - 1.0));
        /*
        This would do a direct application of the color, instead of using the fresnel
        equation.
        ListReflec[layer_number][0]*=
          (1.0 + R_M*LayCol[0] - R_M);
        ListReflec[layer_number][1]*=
          (1.0 + R_M*LayCol[1] - R_M);
        ListReflec[layer_number][2]*=
          (1.0 + R_M*LayCol[2] - R_M);
          */
      }
      /* NK ---- */

      /* NK - I think we SHOULD do something like this: (to apply the layer's color) */
      /*
      ListReflec[layer_number][0]*=FilCol[0];
      ListReflec[layer_number][1]*=FilCol[1];
      ListReflec[layer_number][2]*=FilCol[2];
      */

      Att = (1.0 - (LayCol[pFILTER]*max3(LayCol[0],LayCol[1],LayCol[2]) + LayCol[pTRANSM]));

      /* --------------------------------------------- */
      /* now compute the BRDF contribution ----------- */

      Make_Colour (TmpCol, 0.0, 0.0, 0.0);

      /* if radiosity calculation needed, but not yet done, do it now */
      if (radiosity_needed && !radiosity_done)
      {
        /* calculate max possible contribution of radiosity, to see if calculating it is worthwhile */
        Tmp[0] = FilCol[0]*Att * LayCol[0] * Layer->Finish->Diffuse;
        Tmp[1] = FilCol[1]*Att * LayCol[1] * Layer->Finish->Diffuse;
        Tmp[2] = FilCol[2]*Att * LayCol[2] * Layer->Finish->Diffuse;

        Max_Radiosity_Contribution = GREY_SCALE3(Tmp[0],Tmp[1],Tmp[2]);

        if (Max_Radiosity_Contribution > ADC_Bailout * 3.0)
        {
          /* NK rad 22 Nov 1999 - added LayNormal */
          if (opts.Radiosity_Use_Normal)
            (void)Compute_Ambient(Intersect->IPoint, Raw_Normal, LayNormal, AmbCol, Weight * Max_Radiosity_Contribution);
          else
            (void)Compute_Ambient(Intersect->IPoint, Raw_Normal, Raw_Normal, AmbCol, Weight * Max_Radiosity_Contribution);

          radiosity_done = true;
        }
      }

      /* Add ambient contribution. */
      if(radiosity_needed)
      {
        TmpCol[0] += FilCol[0]*Att * LayCol[0] * AmbCol[0] * Layer->Finish->Diffuse;
        TmpCol[1] += FilCol[1]*Att * LayCol[1] * AmbCol[1] * Layer->Finish->Diffuse;
        TmpCol[2] += FilCol[2]*Att * LayCol[2] * AmbCol[2] * Layer->Finish->Diffuse;
      }
      TmpCol[0] += FilCol[0]*Att * LayCol[0] * Layer->Finish->Ambient[0] * Frame.Ambient_Light[0];
      TmpCol[1] += FilCol[1]*Att * LayCol[1] * Layer->Finish->Ambient[1] * Frame.Ambient_Light[1];
      TmpCol[2] += FilCol[2]*Att * LayCol[2] * Layer->Finish->Ambient[2] * Frame.Ambient_Light[2];

      /* 
      make sure that radiosity/ambient doesn't get multiplied by FilCol[] twice,
      so we add it to ResCol NOW and then go on to do the diffuse stuff
      */
      VAddEq(ResCol, TmpCol);
      Make_Colour(TmpCol,0.0,0.0,0.0);

      /* NK phmap */
      /* this is needed so blocking light sources know if I am ignoring
         photons (or other valuable information */
      photonOptions.objectFlags = Intersect->Object->Flags;

      /* Add diffuse, phong, specular, and iridescence contribution. */
      Diffuse(Layer->Finish, Intersect->IPoint, Ray, LayNormal, LayCol, TmpCol, Att, Intersect->Object);
      
      /* apply the previous layers' filter color [NK] */
      TmpCol[0]*=FilCol[0];
      TmpCol[1]*=FilCol[1];
      TmpCol[2]*=FilCol[2];
      
      /* now add the temp color to the resulting color */
      VAddEq(ResCol, TmpCol);

      /* NK phmap */
      /* now do the same for the photons in the area */
      if(!Test_Flag(Intersect->Object,PH_IGNORE_PHOTONS_FLAG))
      {
        PhotonDiffuse(Layer->Finish, Intersect->IPoint, Ray, LayNormal, Raw_Normal, LayCol, TmpCol, Att, Intersect->Object);
        /* NK layers */
        TmpCol[0]*=FilCol[0];
        TmpCol[1]*=FilCol[1];
        TmpCol[2]*=FilCol[2];
        /* NK ---- */
        VAddEq(ResCol, TmpCol);
      }
      /* NK ---- */

#ifdef GLOBAL_PHOTONS
      /*GlobalPhotonDiffuse(Layer->Finish, Intersect->IPoint, Ray, LayNormal, LayCol, TmpCol, Att, Intersect->Object);*/
      /*TmpCol[0]*=FilCol[0];
      TmpCol[1]*=FilCol[1];
      TmpCol[2]*=FilCol[2];
      */
      /*VAddEq(ResCol, TmpCol);*/
#endif

    }

    /* Get new filter color. */

    if (colour_found)
    {
      FilCol[0] *= (LayCol[0]*LayCol[3]+LayCol[4]);
      FilCol[1] *= (LayCol[1]*LayCol[3]+LayCol[4]);
      FilCol[2] *= (LayCol[2]*LayCol[3]+LayCol[4]);
      /* note FilCol[3] stays at 1.0, [4] stays at 0.0 */
      
      if(Layer->Finish->Conserve_Energy)
      {
        /* adjust filcol based on reflection */
        /* this would work so much better with r,g,b,rt,gt,bt */
        FilCol[0]*=min(1.0,1.0-ListReflec[layer_number][0]);
        FilCol[1]*=min(1.0,1.0-ListReflec[layer_number][1]);
        FilCol[2]*=min(1.0,1.0-ListReflec[layer_number][2]);
      }
    }

    /* Get new remaining translucency. */

    /* NK layers - changed this */
    Trans = min(1.0, fabs(FilCol[pFILTER]*GREY_SCALE(FilCol)) + fabs(FilCol[pTRANSM]));
    /* NK ---- */
  }

  /*
   * Calculate transmitted component.
   *
   * If the surface is translucent a transmitted ray is traced
   * and its contribution is added to the total ResCol after
   * filtering it by FilCol.
   */

  TIR_occured = false;

  if (((Interior = Intersect->Object->Interior) != NULL) && (Trans > ADC_Bailout) && (opts.Quality_Flags & Q_REFRACT))
  {
    w1 = fabs(FilCol[pFILTER]) * max3(fabs(FilCol[0]), fabs(FilCol[1]), fabs(FilCol[2]));
    New_Weight = Weight * w1;

    /* Trace refracted ray. */

    TIR_occured = Refract(Interior, Intersect->IPoint, Ray, TopNormal, Raw_Normal, RfrCol, New_Weight);

    // Since we've done a refraction, we may have gathered photons and
    // overwritten the ones we had before.  So, we want to make a note of
    // it so that we don't try any tricky reuse stuff for the next layer
    photonsAlreadyGathered = -1;

    /* Get distance based attenuation. */

    AttCol[0] = AttCol[1] = AttCol[2] = Interior->Old_Refract;

    if ((Interior != NULL) && Interior_In_Ray_Container(Ray, Interior) >= 0)
    {
      if (fabs(Interior->Fade_Distance) > EPSILON)
      {
        /* NK attenuate */
        if (Interior->Fade_Power>=1000)
        {
          AttCol[0] *= exp(-(1.0-Interior->Fade_Colour[0])*Intersect->Depth/Interior->Fade_Distance);
          AttCol[1] *= exp(-(1.0-Interior->Fade_Colour[1])*Intersect->Depth/Interior->Fade_Distance);
          AttCol[2] *= exp(-(1.0-Interior->Fade_Colour[2])*Intersect->Depth/Interior->Fade_Distance);
        }
        else
        {
          Att = 1.0 + pow(Intersect->Depth / Interior->Fade_Distance, (DBL) Interior->Fade_Power);
          AttCol[0] *= Interior->Fade_Colour[0] + (1.0 - Interior->Fade_Colour[0]) / Att;
          AttCol[1] *= Interior->Fade_Colour[1] + (1.0 - Interior->Fade_Colour[1]) / Att;
          AttCol[2] *= Interior->Fade_Colour[2] + (1.0 - Interior->Fade_Colour[2]) / Att;
        }
      }
    }

    /* If total internal reflection occured the transmitted light is not filtered. */

    if (TIR_occured)
    {
      ResCol[0] += AttCol[0] * RfrCol[0];
      ResCol[1] += AttCol[1] * RfrCol[1];
      ResCol[2] += AttCol[2] * RfrCol[2];
      /* NOTE: pTRANSM (alpha channel) stays zero */
    }
    else
    {
      if (one_colour_found)
      {
        ResCol[0] += AttCol[0] * RfrCol[0] * (FilCol[0] * FilCol[pFILTER] + FilCol[pTRANSM]);
        ResCol[1] += AttCol[1] * RfrCol[1] * (FilCol[1] * FilCol[pFILTER] + FilCol[pTRANSM]);
        ResCol[2] += AttCol[2] * RfrCol[2] * (FilCol[2] * FilCol[pFILTER] + FilCol[pTRANSM]);
        /* We need to know the transmittance value for the alpha channel. [DB] */
        ResCol[pTRANSM] = GREY_SCALE(AttCol) * RfrCol[pTRANSM] * Trans;
      }
      else
      {
        ResCol[0] += AttCol[0] * RfrCol[0];
        ResCol[1] += AttCol[1] * RfrCol[1];
        ResCol[2] += AttCol[2] * RfrCol[2];
        /* We need to know the transmittance value for the alpha channel. [DB] */
        ResCol[pTRANSM] = GREY_SCALE(AttCol) * RfrCol[pTRANSM];
      }
    }
  }

  /*
   * Calculate reflected component.
   *
   * If total internal reflection occured all reflections using
   * TopNormal are skipped.
   */

  if (opts.Quality_Flags & Q_REFLECT)
  {
    for (i = 0; i < layer_number; i++)
    {
      if ((!TIR_occured) ||
          (fabs(TopNormal[0]-ListNormal[i][0]) > EPSILON) ||
          (fabs(TopNormal[1]-ListNormal[i][1]) > EPSILON) ||
          (fabs(TopNormal[2]-ListNormal[i][2]) > EPSILON))
      {
        if ((ListReflec[i][0] != 0.0) ||
            (ListReflec[i][1] != 0.0) ||
            (ListReflec[i][2] != 0.0))
        {
          Reflect(Intersect->IPoint, Ray,
            ListNormal[i], Raw_Normal, RflCol, ListWeight[i]);
          // Since we've done a refleciton, we may have gathered photons and
          // overwritten the ones we had before.  So, we want to make a note of
          // it so that we don't try any tricky reuse stuff for the next layer
          photonsAlreadyGathered = -1;

          if (ListReflEx[i] != 1.0)
          {
            ResCol[0] += ListReflec[i][0] * pow(RflCol[0],ListReflEx[i]);
            ResCol[1] += ListReflec[i][1] * pow(RflCol[1],ListReflEx[i]);
            ResCol[2] += ListReflec[i][2] * pow(RflCol[2],ListReflEx[i]);
          }
          else
          {
            ResCol[0] += ListReflec[i][0] * RflCol[0];
            ResCol[1] += ListReflec[i][1] * RflCol[1];
            ResCol[2] += ListReflec[i][2] * RflCol[2];
          }
        }
      }
    }
  }

  /*
   * Calculate participating media effects.
   */

  if ((opts.Quality_Flags & Q_VOLUME) && (Ray->Index > -1))
  {
    inside_hollow_object = true;

    /* Test for any solid object. */

    for (i = 0; i <= Ray->Index; i++)
    {
      if (!Ray->Interiors[i]->hollow)
      {
        inside_hollow_object = false;

        break;
      }
    }

    /* Calculate effects of all media we're currently in. */

    if (inside_hollow_object)
    {
      LightingMediaListIndex++; 
		#ifdef AccumulatePoolStatistics
      MaxLightedTexture=max(MaxLightedTexture,LightingMediaListIndex);
		#endif
      if ( LightingMediaListIndex >= MaxMediaPoolDepth)
      {
        ResizeMediaMallocPools(MaxMediaPoolDepth*2);
      }

      if ( Ray->Index+2 >= LightingMediaListPoolSize[LightingMediaListIndex])
      {
        POV_FREE(LightingMediaListPool[LightingMediaListIndex]);
        LightingMediaListPool[LightingMediaListIndex] = (IMEDIA **)POV_MALLOC((Ray->Index+2)*sizeof(IMEDIA *), "temp media list");
        LightingMediaListPoolSize[LightingMediaListIndex] = Ray->Index+2;
      }
      
      MediaList=LightingMediaListPool[LightingMediaListIndex];

      TmpMedia = MediaList;

      for (i = 0; i <= Ray->Index; i++)
      {
        if (Ray->Interiors[i]->hollow)
        {
          if (Ray->Interiors[i]->IMedia != NULL)
          {
            *TmpMedia = Ray->Interiors[i]->IMedia;

            TmpMedia++;
          }
        }
      }

      *TmpMedia = NULL;

      if ( *MediaList != NULL)        
        Simulate_Media(MediaList, Ray, Intersect, ResCol, false);
      // Since we've done some media, we may have gathered photons and
      // overwritten the ones we had before.  So, we want to make a note of
      // it so that we don't try any tricky reuse stuff for the next layer
      photonsAlreadyGathered = -1;

      LightingMediaListIndex--;
    }
  }

  FreeComTexData(ctd);
}



/*****************************************************************************
*
* FUNCTION
*
*   compute_shadow_texture
*
* INPUT
*
*   Texture          - layered texture through which shadow ray passes
*   IPoint           - point through which shadow ray passes
*   Raw_Normal       - non-purturbed surface normal
*   Ray              - light source ray
*   Ray_Intersection - current intersection (need intersection depth)
*
* OUTPUT
*
*   Filter_Colour - returned filter for shadow ray
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
* CHANGES
*
*   Dec 1994 : Separated from filter_shadow_ray to do texture_map [CEY]
*
*   May 1995 : Added caustic code by Steve Anger. [DB]
*
*   Aug 1995 : Caustic code moved here from filter_shadow_ray. [CEY]
*
*   Oct 1996 : Replaced halo code by participating media code. [DB]
*
******************************************************************************/

static void compute_shadow_texture (COLOUR Filter_Colour, TEXTURE *Texture, VECTOR IPoint, VECTOR Raw_Normal, RAY *Ray, INTERSECTION *Ray_Intersection)
{
  int i, inside_hollow_object, colour_found, one_colour_found;
  DBL Caustics, dot, k;
  VECTOR Layer_Normal;
  COLOUR Refraction, Layer_Pigment_Colour;
  IMEDIA **Media_List, **Tmp;
  TEXTURE *Layer;
  INTERIOR *Interior = Ray_Intersection->Object->Interior;

  Make_ColourA(Filter_Colour, 1.0, 1.0, 1.0, 1.0, 0.0);

  one_colour_found = false;

  for (Layer = Texture; (Layer != NULL) &&
       (fabs(Filter_Colour[pFILTER]) + fabs(Filter_Colour[pTRANSM]) > ADC_Bailout);
       Layer = (TEXTURE *)Layer->Next)
  {
    colour_found = Compute_Pigment (Layer_Pigment_Colour, Layer->Pigment, IPoint, Ray_Intersection);

    if (colour_found)
    {
      one_colour_found = true;

      Filter_Colour[pRED]    *= Layer_Pigment_Colour[pRED]*Layer_Pigment_Colour[pFILTER]+Layer_Pigment_Colour[pTRANSM];
      Filter_Colour[pGREEN]  *= Layer_Pigment_Colour[pGREEN]*Layer_Pigment_Colour[pFILTER]+Layer_Pigment_Colour[pTRANSM];
      Filter_Colour[pBLUE]   *= Layer_Pigment_Colour[pBLUE]*Layer_Pigment_Colour[pFILTER]+Layer_Pigment_Colour[pTRANSM];
    }

    /* Get normal for faked caustics. (Will rewrite later to cache) */

    if ((Interior != NULL) && ((Caustics = Interior->Caustics) != 0.0))
    {
      Assign_Vector(Layer_Normal, Raw_Normal);

      if ((opts.Quality_Flags & Q_NORMAL) && (Layer->Tnormal != NULL))
      {
        if(opts.Language_Version>310)
        {
          for(i=0; i<warpNormalTextures; i++)
            Warp_Normal(Layer_Normal,Layer_Normal, (TPATTERN *)warpNormalTextureList[i], Test_Flag(warpNormalTextureList[i],DONT_SCALE_BUMPS_FLAG));

          Perturb_Normal(Layer_Normal, Layer->Tnormal, IPoint, Ray_Intersection);

          if((Test_Flag(Layer->Tnormal,DONT_SCALE_BUMPS_FLAG)))
            VNormalizeEq(Layer_Normal);

          for(i=warpNormalTextures-1; i>=0; i--)
            UnWarp_Normal(Layer_Normal,Layer_Normal, (TPATTERN *)warpNormalTextureList[i], Test_Flag(warpNormalTextureList[i],DONT_SCALE_BUMPS_FLAG));
        }
        else
          Perturb_Normal(Layer_Normal, Layer->Tnormal, IPoint, Ray_Intersection);
      }

      /* Get new filter/transmit values. */

      VDot (dot, Layer_Normal, Ray->Direction);

      k = (1.0 + pow(fabs(dot), Caustics));

      Filter_Colour[pRED] *= k;
      Filter_Colour[pGREEN] *= k;
      Filter_Colour[pBLUE] *= k;
    }
  }

  /* Get distance based attenuation. */

  if (Interior != NULL)
  {
    Make_Colour(Refraction, 1.0, 1.0, 1.0);

    if (Interior_In_Ray_Container(Ray, Interior) >= 0)
    {
      if ((Interior->Fade_Power > 0.0) && (fabs(Interior->Fade_Distance) > EPSILON))
      {
        /* NK - attenuation */
        if (Interior->Fade_Power>=1000)
        {
          Refraction[0] *= exp(-(1.0-Interior->Fade_Colour[0])*Ray_Intersection->Depth/Interior->Fade_Distance);
          Refraction[1] *= exp(-(1.0-Interior->Fade_Colour[1])*Ray_Intersection->Depth/Interior->Fade_Distance);
          Refraction[2] *= exp(-(1.0-Interior->Fade_Colour[2])*Ray_Intersection->Depth/Interior->Fade_Distance);
        }
        else
        {
          k = 1.0 + pow(Ray_Intersection->Depth / Interior->Fade_Distance, (DBL) Interior->Fade_Power);
          Refraction[0] *= Interior->Fade_Colour[0] + (1 - Interior->Fade_Colour[0]) / k;
          Refraction[1] *= Interior->Fade_Colour[1] + (1 - Interior->Fade_Colour[1]) / k;
          Refraction[2] *= Interior->Fade_Colour[2] + (1 - Interior->Fade_Colour[2]) / k;
        }
      }
    }
  }
  else
  {
    Make_Colour(Refraction, 1.0, 1.0, 1.0);
  }

  /* Get distance based attenuation. */

  Filter_Colour[pRED]    *= Refraction[0];
  Filter_Colour[pGREEN]  *= Refraction[1];
  Filter_Colour[pBLUE]   *= Refraction[2];

  /* Calculate participating media effects. */

  if ((opts.Quality_Flags & Q_VOLUME) && (Ray->Index > -1))
  {
    inside_hollow_object = true;

    /* Test for any solid object. */

    for (i = 0; i <= Ray->Index; i++)
    {
      if (!Ray->Interiors[i]->hollow)
      {
        inside_hollow_object = false;

        break;
      }
    }

    /* Calculate effects of all participating media we're currently in. */

    if (inside_hollow_object)
    {
      ShadowMediaListIndex++;  

		#ifdef AccumulatePoolStatistics
      MaxShadowTextRecCntr=max(MaxShadowTextRecCntr,ShadowMediaListIndex);
      #endif
      if ( ShadowMediaListIndex >= MaxMediaPoolDepth)
      {
        ResizeMediaMallocPools(MaxMediaPoolDepth*2);
      }

      if ( Ray->Index+2 >= ShadowMediaListPoolSize[ShadowMediaListIndex])
      {
        POV_FREE(ShadowMediaListPool[ShadowMediaListIndex]);
        ShadowMediaListPool[ShadowMediaListIndex] = (IMEDIA **)POV_MALLOC((Ray->Index+2)*sizeof(IMEDIA *), "temp media list");
        ShadowMediaListPoolSize[ShadowMediaListIndex]=Ray->Index+2;
      }

      Media_List=ShadowMediaListPool[ShadowMediaListIndex];

      Tmp = Media_List;

      for (i = 0; i <= Ray->Index; i++)
      {
        if (Ray->Interiors[i]->hollow)
        {
          if (Ray->Interiors[i]->IMedia != NULL)
          {
            *Tmp = Ray->Interiors[i]->IMedia;

            Tmp++;
          }
        }
      }

      *Tmp = NULL;
      if ( *Media_List != NULL)
      Simulate_Media(Media_List, Ray, Ray_Intersection, Filter_Colour, true);

      ShadowMediaListIndex--;
    }
  }
}

/*****************************************************************************
*
* FUNCTION
*
*   BacktraceDiffuse - currently unused
*
*    This was intended for using photon mapping as an estimation of radiance
*    to replace or supplement the radiosity feature, but it hasn't been
*    implemented yet.
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   -
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static void BacktraceDiffuse (FINISH *Finish, VECTOR IPoint, RAY *Out, VECTOR  Layer_Normal, COLOUR Layer_Pigment_Colour, COLOUR Colour, DBL Attenuation, OBJECT *, RAY *Light_Source_Ray, COLOUR Light_Colour)
{
  VECTOR REye;

  Make_ColourA(Colour, 0,0,0,0,0);

  if ((Finish->Diffuse == 0.0) && (Finish->Specular == 0.0) && (Finish->Phong == 0.0))
  {
    return;
  }

  if (Finish->Specular != 0.0)
  {
    REye[X] = -Out->Direction[X];
    REye[Y] = -Out->Direction[Y];
    REye[Z] = -Out->Direction[Z];
  }

  if ((fabs(Light_Colour[pRED])   > EPSILON) ||
      (fabs(Light_Colour[pGREEN]) > EPSILON) ||
      (fabs(Light_Colour[pBLUE])  > EPSILON))
  {
    if (Finish->Diffuse > 0.0)
    {
      do_diffuse(Finish,Light_Source_Ray,Layer_Normal,Colour,Light_Colour,Layer_Pigment_Colour, Attenuation);
    }

    if (Finish->Phong > 0.0)
    {
      do_phong(Finish,Light_Source_Ray,Out->Direction,Layer_Normal,Colour,Light_Colour, Layer_Pigment_Colour);
    }

    if (Finish->Specular > 0.0)
    {
      do_specular(Finish,Light_Source_Ray,REye,Layer_Normal,Colour,Light_Colour, Layer_Pigment_Colour);
    }

    if (Finish->Irid > 0.0)
    {
      do_irid(Finish,Light_Source_Ray,Layer_Normal,IPoint,Colour);
    }

  }
}


/*****************************************************************************
*
* FUNCTION
*
*   compute_backtrace_texture
*
* INPUT
*
*   Texture          - a linked list of texture layers
*   IPoint           - point to be evaluated
*   Raw_Normal       - non-purturbed surface normal
*   Ray              - needed for reflection and highlighs
*   Weight           - ADC control value
*   Intersection - current intersection (need object type and depth)
*   ResCol    - color of light beam
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Nathan Kopp - adpated from compute_lighted_texture by POV-Ray Team
*
* DESCRIPTION
*
*   MUST CHANGE THIS DESCRIPTION
*   This routine loops through all layers of a texture and computes
*   the appearent color of the point with illumination, shadows,
*   reflection, refraction... everything.  This piece of code was broken out
*   of Determine_Appearent_Colour because texture_map needs to call it twice.
*
* CHANGES
*
*
******************************************************************************/

static void compute_backtrace_texture(COLOUR LightCol, TEXTURE *Texture, VECTOR IPoint, VECTOR  Raw_Normal, RAY *Ray, DBL Weight, INTERSECTION *Intersection)
{
  int i;
  int layer_number;
  int one_colour_found, colour_found;
  DBL w1, w2;
  DBL New_Weight, TempWeight;
  DBL Att, Trans;
  VECTOR LayNormal, TopNormal;
  COLOUR LayCol, FilCol;
  COLOUR AttCol, TmpCol, ResCol, CurLightCol;
  INTERIOR *Interior;
  /* IMEDIA **TmpMedia, **MediaList; */
  TEXTURE *Layer;
  RAY NewRay;
  int doReflection, doDiffuse, doRefraction;
  DBL reflectionWeight, refractionWeight, diffuseWeight, dieWeight, totalWeight;
  DBL choice;
  int inside_hollow_object;
  IMEDIA ** MediaList, **TmpMedia;
  DBL Cos_Angle_Incidence;
  int TIR_occured;

  ComTexData *ctd = NewComTexData();

  DBL    *ListWeight = ctd->ListWeight;
  VECTOR *ListNormal = ctd->ListNormal;
  COLOUR *ListReflec = ctd->ListReflec;
  SNGL   *ListReflEx = ctd->ListReflEx;

  /*
   * LightCol is the color of the light beam.
   */

  /* result color for doing diffuse */
  Make_ColourA(ResCol, 0.0, 0.0, 0.0, 0.0, 0.0);

  /* NK 1998 - switched transmit component to zero */
  Make_ColourA(FilCol, 1.0, 1.0, 1.0, 1.0, 0.0);

  Trans = 1.0;

  /* initialize the new ray... we will probably end up using it */
  Assign_Vector(NewRay.Initial, Intersection->IPoint);
  Copy_Ray_Containers(&NewRay, Ray);


  /*
   *  what I've got here for media might work, but maybe not... needs
   *   testing.  Plus, we really want to optionally store photons in
   *   the media... but how do we do that???
   */

  /*
   * Calculate participating media effects.
   */
  if ((opts.Quality_Flags & Q_VOLUME) && (Ray->Index > -1))
  {
/*    DBL dist;*/
/*    VECTOR TempPoint;*/

    inside_hollow_object = true;

    /* Test for any solid object. */

    for (i = 0; i <= Ray->Index; i++)
    {
      if (!Ray->Interiors[i]->hollow)
      {
        inside_hollow_object = false;

        break;
      }
    }

    /* Calculate effects of all media we're currently in. */

    if (inside_hollow_object)
    {
      MediaList = (IMEDIA **)POV_MALLOC((Ray->Index+2)*sizeof(IMEDIA *), "temp media list");

      TmpMedia = MediaList;

      for (i = 0; i <= Ray->Index; i++)
      {
        if (Ray->Interiors[i]->hollow)
        {
          if (Ray->Interiors[i]->IMedia != NULL)
          {
            *TmpMedia = Ray->Interiors[i]->IMedia;

            TmpMedia++;
          }
        }
      }

      *TmpMedia = NULL;

/*
      if ((Trace_Level > 1) && 
          !photonOptions.passThruPrev && photonOptions.maxMediaSteps>0 &&
          !Test_Flag(Intersection->Object,PH_IGNORE_PHOTONS_FLAG) &&
          Check_Light_Group(Intersection->Object,photonOptions.Light))
*/
      if ((Trace_Level > 1) && 
          !photonOptions.passThruPrev && photonOptions.maxMediaSteps>0)
      {
        Backtrace_Simulate_Media(MediaList, Ray, Intersection, LightCol);
      }
      else
      {
        Simulate_Media(MediaList, Ray, Intersection, LightCol, true);  /* light ray */
      }

      POV_FREE(MediaList);
    }
  }

  /* atmosphere attenuation */
  /* - this is already done in Trace?!???! (Do_Finite_Atmosphere)
  if ((photonOptions.Light->Media_Interaction) &&
      (photonOptions.Light->Media_Attenuation))
  {
    do_light_ray_atmosphere(Ray, Intersection, LightCol, true);
  }
  */

  /* Get distance based attenuation. */
  Interior = Intersection->Object->Interior;
  AttCol[0] = AttCol[1] = AttCol[2] = Interior->Old_Refract;

  if (Interior != NULL)
  {
    if (Interior_In_Ray_Container(Ray, Interior) >= 0)
    {
      if ((Interior->Fade_Power > 0.0) && (fabs(Interior->Fade_Distance) > EPSILON))
      {
        /* NK attenuate */
        if (Interior->Fade_Power>=1000)
        {
          AttCol[0] *= exp(-(1.0-Interior->Fade_Colour[0])*Intersection->Depth/Interior->Fade_Distance);
          AttCol[1] *= exp(-(1.0-Interior->Fade_Colour[1])*Intersection->Depth/Interior->Fade_Distance);
          AttCol[2] *= exp(-(1.0-Interior->Fade_Colour[2])*Intersection->Depth/Interior->Fade_Distance);
        }
        else
        {
          Att = 1.0 + pow(Intersection->Depth / Interior->Fade_Distance, (DBL) Interior->Fade_Power);
          AttCol[0] *= Interior->Fade_Colour[0] + (1.0 - Interior->Fade_Colour[0]) / Att;
          AttCol[1] *= Interior->Fade_Colour[1] + (1.0 - Interior->Fade_Colour[1]) / Att;
          AttCol[2] *= Interior->Fade_Colour[2] + (1.0 - Interior->Fade_Colour[2]) / Att;
        }
      }
    }
  }
  LightCol[0] *= AttCol[0];
  LightCol[1] *= AttCol[1];
  LightCol[2] *= AttCol[2];

  /* set size here */
  photonOptions.photonDepth += Intersection->Depth;


  /* First, we should save this photon! */
  if ((Trace_Level > 1) && !photonOptions.passThruPrev && 
       !Test_Flag(Intersection->Object,PH_IGNORE_PHOTONS_FLAG) &&
       Check_Photon_Light_Group(Intersection->Object))
  {
    addSurfacePhoton(Intersection->IPoint, Ray->Initial, LightCol, Raw_Normal);
  }

  if (photonOptions.passThruThis)
  {
    RAY NRay;

    Copy_Ray_Containers(&NRay, Ray);
    Assign_Vector(NRay.Initial, IPoint);
    Assign_Vector(NRay.Direction, Ray->Direction);

    Trace_Level++;
    Trace(&NRay, LightCol, Weight);
    Trace_Level--;
  }

  /*
   * Loop through the layers and compute the ambient, diffuse,
   * phong and specular for these textures.
   */
  one_colour_found = false;
  for (layer_number = 0, Layer = Texture;
      (Layer != NULL) && (Trans > ADC_Bailout);
      layer_number++, Layer = (TEXTURE *)Layer->Next)
  {
    /* Get perturbed surface normal. */
    Assign_Vector(LayNormal, Raw_Normal);

    if ((opts.Quality_Flags & Q_NORMAL) && (Layer->Tnormal != NULL))
    {
      if(opts.Language_Version>310)
      {
        for(i=0; i<warpNormalTextures; i++)
          Warp_Normal(LayNormal,LayNormal, (TPATTERN *)warpNormalTextureList[i], Test_Flag(warpNormalTextureList[i],DONT_SCALE_BUMPS_FLAG));

        Perturb_Normal(LayNormal, Layer->Tnormal, IPoint, Intersection);

        if((Test_Flag(Layer->Tnormal,DONT_SCALE_BUMPS_FLAG)))
          VNormalizeEq(LayNormal);

        for(i=warpNormalTextures-1; i>=0; i--)
          UnWarp_Normal(LayNormal,LayNormal, (TPATTERN *)warpNormalTextureList[i], Test_Flag(warpNormalTextureList[i],DONT_SCALE_BUMPS_FLAG));
      }
      else
        Perturb_Normal(LayNormal, Layer->Tnormal, IPoint, Intersection);
    }

    /* Store top layer normal.*/
    if (!layer_number)
    {
      Assign_Vector(TopNormal, LayNormal);
    }

    /* Get surface colour. */
    New_Weight = Weight * Trans;
    colour_found = Compute_Pigment (LayCol, Layer->Pigment, IPoint, Intersection);
    
    Att = Trans * (1.0 - min(1.0, (DBL)(LayCol[pFILTER] + LayCol[pTRANSM])));

    LayCol[0]*=FilCol[0];
    LayCol[1]*=FilCol[1];
    LayCol[2]*=FilCol[2];
    CRGBAddScaledEq(ResCol, Att, LayCol);

    /*
     * If a valid color was returned set one_colour_found to true.
     * An invalid color is returned if a surface point is outside
     * an image map used just once.
     */
    if (colour_found)
    {
      one_colour_found = true;
    }

    /* Store vital information for later reflection. */
    if (layer_number == MAX_LAYERS)
    {
      Error("Too many texture layers.");
    }

    ListReflEx[layer_number] = Layer->Finish->Reflect_Exp;
    ListWeight[layer_number] = New_Weight;
    Assign_Vector(ListNormal[layer_number], LayNormal);

    /* NK 1999 moved here */
    /* Added by MBP for angle-dependent reflectivity */
    VDot(Cos_Angle_Incidence, Ray->Direction, LayNormal);
    Cos_Angle_Incidence *= -1.0;

    if (Intersection->Object->Interior || 
      (Layer->Finish->Reflection_Type != 1)) 
    {
      determine_reflectivity (&ListWeight[layer_number], ListReflec[layer_number],
                Layer->Finish->Reflection_Max, Layer->Finish->Reflection_Min, 
                Layer->Finish->Reflection_Type, Layer->Finish->Reflection_Falloff, 
                Cos_Angle_Incidence, Ray, Intersection->Object->Interior);
    }
    else
    {
      Error("Reflection_Type 1 used with no interior.");
    }

    /* Added by MBP for metallic reflection */
    if (Layer->Finish->Reflect_Metallic != 0.0)
    {
      DBL R_M=Layer->Finish->Reflect_Metallic;

      DBL x = fabs(acos(Cos_Angle_Incidence)) / M_PI_2;
      DBL F = 0.014567225 / Sqr(x - 1.12) - 0.011612903;
      F=min(1.0,max(0.0,F));

      ListReflec[layer_number][0]*=
        (1.0 + R_M * (1.0 - F) * (LayCol[0] - 1.0));
      ListReflec[layer_number][1]*=
        (1.0 + R_M * (1.0 - F) * (LayCol[1] - 1.0));
      ListReflec[layer_number][2]*=
        (1.0 + R_M * (1.0 - F) * (LayCol[2] - 1.0));
      /*
      ListReflec[layer_number][0]*=
        (1.0 + R_M*LayCol[0] - R_M);
      ListReflec[layer_number][1]*=
        (1.0 + R_M*LayCol[1] - R_M);
      ListReflec[layer_number][2]*=
        (1.0 + R_M*LayCol[2] - R_M);
      */
    }
    /* NK ---- */

    /* NK - SOULD do something like this:*/
    /*
    ListReflec[layer_number][0]*=FilCol[0];
    ListReflec[layer_number][1]*=FilCol[1];
    ListReflec[layer_number][2]*=FilCol[2];
    */

    /* Get new filter color. */
    if (colour_found)
    {
      FilCol[0] *= (LayCol[0]*LayCol[3]+LayCol[4]);
      FilCol[1] *= (LayCol[1]*LayCol[3]+LayCol[4]);
      FilCol[2] *= (LayCol[2]*LayCol[3]+LayCol[4]);
      /* note FilCol[3] stays at 1.0 , [4] stays at 0.0 */
      if(Layer->Finish->Conserve_Energy)
      {
        /* adjust filcol based on reflection */
        /* this would work so much better with r,g,b,rt,gt,bt */
        FilCol[0]*=min(1.0,1.0-ListReflec[layer_number][0]);
        FilCol[1]*=min(1.0,1.0-ListReflec[layer_number][1]);
        FilCol[2]*=min(1.0,1.0-ListReflec[layer_number][2]);
      }
    }

    /* Get new remaining translucency. */
    Trans = min(1.0, fabs(FilCol[pFILTER]*GREY_SCALE(FilCol)));
  }

  /*******************
    now that we have color info, we can determine what we want to do next
  ********************/

  if (photonOptions.photonObject)
  {
    /* if photon is for caustic map, then do
         reflection/refraction always
         diffuse never
    */
    doReflection = 1;
    doRefraction = 1;
    doDiffuse = 0;
  }
  else
  {
    /* if photon is for global map, then decide which we want to do
    */
    diffuseWeight = max(0.0, fabs(GREY_SCALE(ResCol)));
    /* use top-layer finish only */
    if(Texture->Finish)
      diffuseWeight*=Texture->Finish->Diffuse;
    refractionWeight = Trans;
    /* reflection only for top layer!!!!!! */
    reflectionWeight = max(0.0, fabs(GREY_SCALE(ListReflec[0])));
    dieWeight = max(0.0,(1.0-diffuseWeight));

    /* normalize weights: make sum be 1.0 */
    totalWeight = reflectionWeight + refractionWeight + diffuseWeight + dieWeight;
    if (reflectionWeight + refractionWeight + diffuseWeight>ADC_Bailout)
    {
      diffuseWeight /= totalWeight;
      refractionWeight /= totalWeight;
      reflectionWeight /= totalWeight;
      dieWeight /= totalWeight;

      /* now, determine which we want to use */
      choice = FRAND();
      if (choice<diffuseWeight)
      {
        /* do diffuse */
        VScaleEq(ResCol,1.0/diffuseWeight);
        /*if (diffuseWeight<.999)
        {
          VScaleEq(ResCol,20.0);
        }
        else
        {
          VScaleEq(ResCol,2.0);
        }*/
        doReflection = 0;
        doRefraction = 0;
        doDiffuse = 1;
        photonOptions.passThruPrev = false;
      }
      else if (choice<diffuseWeight+refractionWeight)
      {
        /* do refraction */
        VScaleEq(FilCol,1.0/refractionWeight);
        doReflection = 0;
        doRefraction = 1;
        doDiffuse = 0;
        photonOptions.passThruPrev = true;
      }
      else if (choice<diffuseWeight+refractionWeight+reflectionWeight)
      {
        /* do reflection */
        VScaleEq(ListReflec[0],1.0/reflectionWeight);
        doReflection = 1;
        doRefraction = 0;
        doDiffuse = 0;
        photonOptions.passThruPrev = true;
      }
      /* else die */
    }
    else
    {
      doReflection = 0;
      doRefraction = 0;
      doDiffuse = 0;
    }

  }

  if (doDiffuse)
  {
    ChooseRay(&NewRay, LayNormal, Ray, Raw_Normal, rand()%400);

    CurLightCol[0] = LightCol[0]*ResCol[0];
    CurLightCol[1] = LightCol[1]*ResCol[1];
    CurLightCol[2] = LightCol[2]*ResCol[2];

    /* don't trace if < EPSILON */

    /* now trace it */
    Trace_Level++;
    Trace(&NewRay, CurLightCol, 1.0);
    Trace_Level--;
  }

  /*
   * Calculate transmitted component.
   *
   * If the surface is translucent a transmitted ray is traced
   * and its illunination is filtered by FilCol.
   */
  if (doRefraction)
  {
    TIR_occured = false;
    if ( (Test_Flag(Intersection->Object,PH_RFR_ON_FLAG) &&
          !(photonOptions.lightFlags & PH_RFR_OFF_FLAG)) ||
         (!Test_Flag(Intersection->Object,PH_RFR_OFF_FLAG) &&
          (photonOptions.lightFlags & PH_RFR_ON_FLAG)) ||
          photonOptions.passThruThis)
    if (((Interior = Intersection->Object->Interior) != NULL) && (Trans > ADC_Bailout) && (opts.Quality_Flags & Q_REFRACT))
    {
      w1 = fabs(FilCol[pFILTER]) * max3(FilCol[0], FilCol[1], FilCol[2]);
      w2 = fabs(FilCol[pTRANSM]);

      New_Weight = Weight * max(w1, w2);

      /* Trace refracted ray. */
      Assign_Colour(GFilCol, FilCol);
      TIR_occured = BacktraceRefract(Interior, Intersection->IPoint, Ray, TopNormal, Raw_Normal, LightCol, New_Weight);
    }
  }

  /*
   * Calculate reflected component.
   *
   * If total internal reflection occured all reflections using
   * TopNormal are skipped.
   */
  if(doReflection)
  {
    if ( ((Test_Flag(Intersection->Object, PH_RFL_ON_FLAG) &&
          !(photonOptions.lightFlags & PH_RFL_OFF_FLAG)) ||
         (!Test_Flag(Intersection->Object,PH_RFL_OFF_FLAG) &&
          (photonOptions.lightFlags & PH_RFL_ON_FLAG))) &&
            !photonOptions.passThruThis)
    if (opts.Quality_Flags & Q_REFLECT)
    {
      for (i = 0; i < layer_number; i++)
      {
        if ((!TIR_occured) ||
            (fabs(TopNormal[0]-ListNormal[i][0]) > EPSILON) ||
            (fabs(TopNormal[1]-ListNormal[i][1]) > EPSILON) ||
            (fabs(TopNormal[2]-ListNormal[i][2]) > EPSILON))
        {
          if ((ListReflec[i][0] != 0.0) ||
              (ListReflec[i][1] != 0.0) ||
              (ListReflec[i][2] != 0.0))
          {
            /* Added by MBP for metallic reflection */
            TmpCol[0]=LightCol[0];
            TmpCol[1]=LightCol[1];
            TmpCol[2]=LightCol[2];

            if (ListReflEx[i] != 1.0)
            {
              TmpCol[0] = ListReflec[i][0] * pow(TmpCol[0],ListReflEx[i]);
              TmpCol[1] = ListReflec[i][1] * pow(TmpCol[1],ListReflEx[i]);
              TmpCol[2] = ListReflec[i][2] * pow(TmpCol[2],ListReflEx[i]);
            }
            else
            {
              TmpCol[0] = ListReflec[i][0] * TmpCol[0];
              TmpCol[1] = ListReflec[i][1] * TmpCol[1];
              TmpCol[2] = ListReflec[i][2] * TmpCol[2];
            }

            TempWeight = ListWeight[i] * max3(ListReflec[i][0], ListReflec[i][1], ListReflec[i][2]);

            if (photonOptions.photonReflectionBlur)
            {
              Reflect(Intersection->IPoint, Ray,
                LayNormal, Raw_Normal, TmpCol, TempWeight);
            }
            else
            {
              Reflect(Intersection->IPoint, Ray,
                LayNormal, Raw_Normal, TmpCol, TempWeight);
            }

          }
        }

        /* if global photons, the stop after first layer */
        if (photonOptions.photonObject==NULL)
          i=layer_number;
      }
    }
  }

  /* now reset the depth! */
  photonOptions.photonDepth -= Intersection->Depth;

  FreeComTexData(ctd);
}


/*****************************************************************************
*
* FUNCTION
*
*   filter_shadow_ray
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
*   -
*
* CHANGES
*
*   Aug 1994 : Code for early exit due to opaque object added. [DB]
*
*   Sep 1994 : Code for multi-textured blobs added. [DB]
*
*   May 1995 : Added caustic code by Steve Anger. [DB]
*
*   Aug 1995 : Added code to attenuate light source color
*              due to atmospheric effects. [DB]
*
******************************************************************************/

static void filter_shadow_ray(INTERSECTION *Ray_Intersection, RAY *Light_Source_Ray, COLOUR Colour)
{
  int i, Texture_Count;
  VECTOR IPoint;
  VECTOR Raw_Normal;
  COLOUR FC1, Temp_Colour;
  TEXTURE *Texture = NULL;  /* To remove uninitialized use warning [AED] */
  int save_tw_size;
  DBL *save_Weights = NULL;
  TEXTURE **save_Textures = NULL;
  /* NK 1998 */
  UV_VECT UV_Coords;
  /* NK ---- */
  DBL Normal_Direction;

  /* NK phmap */
  if ((photonOptions.photonsEnabled) && 
      Test_Flag(Ray_Intersection->Object,PH_TARGET_FLAG) && 
      !(photonOptions.objectFlags & PH_IGNORE_PHOTONS_FLAG))
  {
    if( (Test_Flag(Ray_Intersection->Object,PH_RFR_ON_FLAG) &&
         !(photonOptions.lightFlags & PH_RFR_OFF_FLAG)) ||
         (!Test_Flag(Ray_Intersection->Object,PH_RFR_OFF_FLAG) &&
         (photonOptions.lightFlags & PH_RFR_ON_FLAG)) )
    {
      Make_Colour(Colour, 0.0, 0.0, 0.0);
      return;
    }
  }

  Assign_Vector(IPoint, Ray_Intersection->IPoint);

  if (!(opts.Quality_Flags & Q_SHADOW))
  {
    return;
  }

  /* If the object is opaque there's no need to go any further. [DB 8/94] */

  if (Test_Flag(Ray_Intersection->Object, OPAQUE_FLAG))
  {
    Make_Colour(Colour, 0.0, 0.0, 0.0);
    return;
  }

  /* Get the normal to the surface */

  Normal(Raw_Normal, Ray_Intersection->Object, Ray_Intersection);
  /* I added this to flip the normal if the object is inverted (for CSG).
     However, I subsequently commented it out for speed reasons - it doesn't
     make a difference (no pun intended). The preexisting flip code below
     produces a similar (though more extensive) result. [NK]

     Actually, we should keep this code to guarantee that Normal_Direction
     is set properly. [NK]
  */
  if (Test_Flag(Ray_Intersection->Object,INVERTED_FLAG))
  {
    Raw_Normal[X] = -Raw_Normal[X];
    Raw_Normal[Y] = -Raw_Normal[Y];
    Raw_Normal[Z] = -Raw_Normal[Z];
  }

  /* If the surface normal points away, flip its direction. */
  VDot(Normal_Direction, Raw_Normal, Light_Source_Ray->Direction);
  if (Normal_Direction > 0.0)
  {
    VScaleEq(Raw_Normal, -1.0);
  }

  Assign_Vector(Ray_Intersection->INormal, Raw_Normal);
  /* and save to intersection -hdf- */
  Assign_Vector(Ray_Intersection->PNormal, Raw_Normal);

  if (Test_Flag(Ray_Intersection->Object, UV_FLAG))
  {
    /* get the UV vect of the intersection */
    UVCoord(UV_Coords, Ray_Intersection->Object, Ray_Intersection);
    /* save the normal and UV coords into Intersection */
    Assign_UV_Vect(Ray_Intersection->Iuv, UV_Coords);
  }

  /* now switch to UV mapping if we need to */
  if (Test_Flag(Ray_Intersection->Object, UV_FLAG))
  {
    IPoint[X] = UV_Coords[U];
    IPoint[Y] = UV_Coords[V];
    IPoint[Z] = 0;
  }
  /* NK ---- */

  /*
   * Save texture and weight lists.
   */

  save_tw_size = Number_Of_Textures_And_Weights;
  LightingPoolIndex++; 
  if ( LightingPoolIndex >= MaxLightPoolDepth) 
  {
    ResizeLightMallocPools(MaxLightPoolDepth*2);
  }

	#ifdef AccumulatePoolStatistics
	  MaxAppColourRecCntr=max(MaxAppColourRecCntr,LightingPoolIndex);
	#endif
  Weight_List=WeightListPool[LightingPoolIndex];
  Light_List=LightListPool[LightingPoolIndex];
  Texture_List=TextureListPool[LightingPoolIndex];
  /* Get texture list and weights. */

  Texture_Count = create_texture_list(Ray_Intersection, Normal_Direction);

  Make_ColourA(Temp_Colour, 0.0, 0.0, 0.0, 0.0, 0.0);

  for (i = 0; i < Texture_Count; i++)
  {
    /* If contribution of this texture is neglectable skip ahead. */

    if (Weight_List[i] < ADC_Bailout)
    {
      continue;
    }

    Texture = Texture_List[i];

    do_texture_map(FC1, Texture, IPoint, Raw_Normal, Light_Source_Ray, 0.0, Ray_Intersection, true);

    Temp_Colour[pRED]     += Weight_List[i] * FC1[pRED];
    Temp_Colour[pGREEN]   += Weight_List[i] * FC1[pGREEN];
    Temp_Colour[pBLUE]    += Weight_List[i] * FC1[pBLUE];
    Temp_Colour[pFILTER]  += Weight_List[i] * FC1[pFILTER];
    Temp_Colour[pTRANSM]  += Weight_List[i] * FC1[pTRANSM];
  }

  /* Restore the weight and texture list. */

  LightingPoolIndex--;
  Weight_List=WeightListPool[LightingPoolIndex];
  Light_List=LightListPool[LightingPoolIndex];
  Texture_List=TextureListPool[LightingPoolIndex];
  if (fabs(Temp_Colour[pFILTER]) + fabs(Temp_Colour[pTRANSM]) < ADC_Bailout)
  {
    Make_Colour(Colour, 0.0, 0.0, 0.0);
  }
  else
  {
    Colour[pRED]   *= Temp_Colour[pFILTER] * Temp_Colour[pRED]  + Temp_Colour[pTRANSM];
    Colour[pGREEN] *= Temp_Colour[pFILTER] * Temp_Colour[pGREEN]+ Temp_Colour[pTRANSM];
    Colour[pBLUE]  *= Temp_Colour[pFILTER] * Temp_Colour[pBLUE] + Temp_Colour[pTRANSM];
  }

  /* Get atmospheric attenuation. */

  do_light_ray_atmosphere(Light_Source_Ray, Ray_Intersection, Colour, true);
}



/*****************************************************************************
*
* FUNCTION
*
*   do_blocking
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
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static int do_blocking(INTERSECTION *Local_Intersection, RAY *Light_Source_Ray, COLOUR Light_Colour, ISTACK *Local_Stack)
{
  Increase_Counter(stats[Shadow_Rays_Succeeded]);

  filter_shadow_ray(Local_Intersection, Light_Source_Ray, Light_Colour);

  if ((fabs(Light_Colour[pRED])   < EPSILON) &&
      (fabs(Light_Colour[pGREEN]) < EPSILON) &&
      (fabs(Light_Colour[pBLUE])  < EPSILON))
  {
    while ((Local_Intersection = pop_entry(Local_Stack)) != NULL)
    {
    }

    return(true);
  }

  return(false);
}



/*****************************************************************************
*
* FUNCTION
*
*   block_light_source
*
* INPUT
*
*   Light            - Light source
*   Depth            - Distance to light source
*   Light_Source_Ray - Light ray
*   Eye_Ray          - Ray from eye to current intersection point
*   P                - Surface point to shade
*
* OUTPUT
*
*   Colour           - Light color reaching point P
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Determine how much light from the given light source reaches
*   the given point. This includes attenuation due to blocking
*   and translucent objects and atmospheric effects.
*
* CHANGES
*
*   Jan 1995 : Creation (Extracted from common code).
*
*   Aug 1995 : Added code to support atmospheric effects. [DB]
*
******************************************************************************/

static void block_light_source(LIGHT_SOURCE *Light, DBL Depth, RAY *Light_Source_Ray, RAY  *Eye_Ray, VECTOR P, COLOUR Colour)
{
  DBL New_Depth;
  INTERSECTION Isection;
  RAY New_Ray;

  /* NK phmap */
  /* save the light's flags so we know if we used real caustics */
  photonOptions.lightFlags = Light->Flags;
  /* NK ---- */

  /* Store current depth and ray because they will be modified. */

  New_Depth = Depth;

  New_Ray = *Light_Source_Ray;
  New_Ray.Optimisiation_Flags = OPTIMISE_SHADOW_TEST;

  /* Get shadows from current light source. */

  if ((Light->Area_Light) && (opts.Quality_Flags & Q_AREA_LIGHT))
  {
    block_area_light(Light, &New_Depth, &New_Ray, Eye_Ray, P, Colour, 0, 0, 0, 0, 0);
  }
  else
  {
    /* NK parallel - can't use light buffer with parallel lights; [trf] or light groups  */
    if ((opts.Options & USE_LIGHT_BUFFER) && (Light->Light_Type!=CYLINDER_SOURCE) &&
        !(Light->Parallel) && ((Light->Type & LIGHT_GROUP_LIGHT_OBJECT) != LIGHT_GROUP_LIGHT_OBJECT))
    {
      block_point_light_LBuffer(Light, &New_Depth, &New_Ray, Colour);
    }
    else
    {
      block_point_light(Light, &New_Depth, &New_Ray, Colour);
    }
  }

  /*
   * If there's some distance left for the ray to reach the light source
   * we have to apply atmospheric stuff to this part of the ray.
   */

  if ((New_Depth > SHADOW_TOLERANCE) &&
      (Light->Media_Interaction) &&
      (Light->Media_Attenuation))
  {
    Isection.Depth = New_Depth;
    Isection.Object = NULL;

    do_light_ray_atmosphere(&New_Ray, &Isection, Colour, false);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   average_textures
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
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static void average_textures (COLOUR Result_Colour, TEXTURE *Texture, VECTOR IPoint, VECTOR  Raw_Normal,
  RAY *Ray, DBL Weight, INTERSECTION *Ray_Intersection, int Shadow_Flag)
{
   int i;
   COLOUR LC;
   BLEND_MAP *Map = Texture->Blend_Map;
   SNGL Value;
   SNGL Total = 0.0;

   Make_Colour (Result_Colour, 0.0, 0.0, 0.0);

   for (i = 0; i < Map->Number_Of_Entries; i++)
   {
     Value = Map->Blend_Map_Entries[i].value;

     do_texture_map (LC,Map->Blend_Map_Entries[i].Vals.Texture, IPoint,Raw_Normal,Ray,Weight,Ray_Intersection,Shadow_Flag);

     Result_Colour[pRED]   += LC[pRED]   *Value;
     Result_Colour[pGREEN] += LC[pGREEN] *Value;
     Result_Colour[pBLUE]  += LC[pBLUE]  *Value;
     Result_Colour[pFILTER]+= LC[pFILTER]*Value;
     Result_Colour[pTRANSM]+= LC[pTRANSM]*Value;

     Total += Value;
   }

   Result_Colour[pRED]   /= Total;
   Result_Colour[pGREEN] /= Total;
   Result_Colour[pBLUE]  /= Total;
   Result_Colour[pFILTER]/= Total;
   Result_Colour[pTRANSM]/= Total;
}


/*****************************************************************************
*
* FUNCTION
*
*   backtrace_average_textures
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
*   based on average_textures by the POV-Ray Team
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static void backtrace_average_textures (COLOUR Result_Colour, TEXTURE *Texture, VECTOR IPoint, VECTOR  Raw_Normal,
  RAY *Ray, DBL Weight, INTERSECTION *Ray_Intersection, int Shadow_Flag)
{
   int i;
   COLOUR LC;
   BLEND_MAP *Map = Texture->Blend_Map;
   SNGL Value;
   SNGL Total = 0.0;

   for (i = 0; i < Map->Number_Of_Entries; i++)
   {
     Value = Map->Blend_Map_Entries[i].value;
     Total += Value;
   }

   for (i = 0; i < Map->Number_Of_Entries; i++)
   {
     Value = Map->Blend_Map_Entries[i].value / Total;
     VScale(LC, Result_Colour, Value);

     do_texture_map (LC,Map->Blend_Map_Entries[i].Vals.Texture, IPoint,Raw_Normal,Ray,Weight,Ray_Intersection,Shadow_Flag);
   }
}


/*****************************************************************************
*
* FUNCTION
*
*   do_light_ray_atmosphere
*
* INPUT
*
*   Light_Source_Ray - Current ray towards light source
*   Ray_Intersection - Current intersection with a blocking object
*   Texture          - Current PNFH texture
*   Valid_Object     - Flag: 1=a valid object is in the intersection struct
*
* OUTPUT
*
*   Colour           - Attenuated light source color
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Determine the influence of atmospheric effects on a light source ray.
*
* CHANGES
*
*   Aug 1995 : Creation.
*
******************************************************************************/

static void do_light_ray_atmosphere(RAY *Light_Source_Ray, INTERSECTION *Ray_Intersection, COLOUR Colour, int Valid_Object)
{
  int interior_nr;
  int i, all_hollow;

  /* Why are we here? */

  if ((Colour[pRED] < EPSILON) && (Colour[pGREEN] < EPSILON) && (Colour[pBLUE] < EPSILON))
  {
    return;
  }

  all_hollow = true;

  for (i = 0; i <= Light_Source_Ray->Index; i++)
  {
    if (!Light_Source_Ray->Interiors[i]->hollow)
    {
      all_hollow = false;

      break;
    }
  }

  /* Apply atmospheric effects inside and/or outside any object. */

  if ((opts.Quality_Flags & Q_VOLUME) && (all_hollow || (Valid_Object && Ray_Intersection->Object->Interior != NULL)))
  {
    Do_Finite_Atmosphere(Light_Source_Ray, Ray_Intersection, Colour, true);
  }

  /* Handle contained textures. */

  if (Valid_Object)
  {
    if (Light_Source_Ray->Index == -1)
    {
      /* The ray is entering from the atmosphere */

      Ray_Enter(Light_Source_Ray, Ray_Intersection->Object->Interior);
    }
    else
    {
      /* The ray is currently inside an object */

      if ((interior_nr = Interior_In_Ray_Container(Light_Source_Ray, Ray_Intersection->Object->Interior)) >= 0)
      {
        /* The ray is leaving the current object */

        Ray_Exit(Light_Source_Ray, interior_nr);
      }
      else
      {
        /* The ray is entering a new object */

        Ray_Enter(Light_Source_Ray, Ray_Intersection->Object->Interior);
      }
    }
  }
}

/*****************************************************************************
*
* FUNCTION
*
*   determine_reflectivity
*
* INPUT
*
* OUTPUT
*
*   weight           - ADC weight of reflected ray
*   reflection       - Reflectivity value to be passed to Reflect()
*
* RETURNS
*
* AUTHOR
*
*   Michael Paul  <mbp@locke.ccil.org>
*
* DESCRIPTION
*
*   Determine the reflecivity of a surface at a point, taking into account the
*   surface's reflectivity and the viewing angle
*
* CHANGES
*
*   Sep 1998 : Creation
*
******************************************************************************/

void determine_reflectivity(DBL *weight, RGB reflectivity,
                            COLOUR Reflection_Max, COLOUR Reflection_Min,
                            int Reflection_Type, DBL Reflection_Falloff, 
                            DBL cos_angle, RAY *Ray, INTERIOR *Interior)
{
  DBL Temp_Weight_Min, Temp_Weight_Max;
  DBL Reflection_Frac;
  DBL g, F;
  //int i;

  DBL ior, disp;
  //DBL ior2;

  if (Reflection_Type==1)
  {
    /* Get ratio of iors depending on the interiors the ray is traversing. */
    if (Ray->Index == -1)
    {
      /* The ray is entering from the atmosphere. */
      ior = Frame.Atmosphere_IOR / Interior->IOR;
      disp = Frame.Atmosphere_Dispersion / Interior->Dispersion;
    }
    else
    {
      /* The ray is currently inside an object. */
      if ((Interior_In_Ray_Container(Ray, Interior)) >= 0)
      {
        if (Ray->Index == 0)
        {
          /* The ray is leaving into the atmosphere. */
          ior = Interior->IOR / Frame.Atmosphere_IOR;
          disp = Interior->Dispersion / Frame.Atmosphere_Dispersion;
        }
        else
        {
          /* The ray is leaving into another object. */
          ior = Interior->IOR / Ray->Interiors[Ray->Index]->IOR;
          disp = Interior->Dispersion / Ray->Interiors[Ray->Index]->Dispersion;
        }
      }
      else
      {
        /* The ray is entering a new object. */
        ior = Ray->Interiors[Ray->Index]->IOR / Interior->IOR;
        disp = Ray->Interiors[Ray->Index]->Dispersion / Interior->Dispersion;
      }
    }

    ior = 1.0/ior;
    disp = 1.0/disp;
  }


  switch (Reflection_Type)
  {
    case 0:  /* Standard reflection */
      Temp_Weight_Max = max3(Reflection_Max[pRED], Reflection_Max[pGREEN], Reflection_Max[pBLUE]);
      Temp_Weight_Min = max3(Reflection_Min[pRED], Reflection_Min[pGREEN], Reflection_Min[pBLUE]);
      *weight = *weight * max(Temp_Weight_Max, Temp_Weight_Min);

      if(fabs(Reflection_Falloff-1.0)>EPSILON)
      {
        Reflection_Frac = pow(1.0 - cos_angle, Reflection_Falloff);
      }
      else
      {
        Reflection_Frac = 1.0 - cos_angle;
      }
      if(fabs(Reflection_Frac)<EPSILON)
      {
        Assign_RGB(reflectivity, Reflection_Min);
      }
      else if (fabs(Reflection_Frac-1.0)<EPSILON)
      {
        Assign_RGB(reflectivity, Reflection_Max);
      }
      else
      {
        CRGBLinComb2(reflectivity, Reflection_Frac, Reflection_Max, (1 - Reflection_Frac), Reflection_Min);
      }
      break;

    case 1:  /* Fresnel */
      //if(fabs(disp - 1.0) < EPSILON)
      //{
        // Christoph's tweak to work around possible negative argument in sqrt
        {
          DBL sqx = Sqr(ior) + Sqr(cos_angle) - 1.0;
          if (sqx > 0.0)
          {
            g = sqrt(sqx);
            F = 0.5 * (Sqr(g - cos_angle) / Sqr(g + cos_angle));
            F = F * (1.0 + Sqr(cos_angle * (g + cos_angle) - 1.0) / Sqr(cos_angle * (g - cos_angle) + 1.0));

            F=min(1.0,max(0.0,F));
            CRGBLinComb2(reflectivity,F,Reflection_Max,(1.0-F),Reflection_Min);
          }
          else
            Assign_RGB(reflectivity, Reflection_Max);
        }
/*
        g = sqrt(Sqr(ior) + Sqr(cos_angle) - 1.0);
        F = 0.5 * (Sqr(g - cos_angle) / Sqr(g + cos_angle));
        F = F * (1.0 + Sqr(cos_angle * (g + cos_angle) - 1.0) / Sqr(cos_angle * (g - cos_angle) + 1.0));

        F=min(1.0,max(0.0,F));
        CRGBLinComb2(reflectivity,F,Reflection_Max,(1.0-F),Reflection_Min);
*/
      /*}
      // This code is experimental, so it remains disabled.
      // I need to verify the theoretical validity of this, and also
      // add performance optimizations.  [NK]
      else
      {
        int count = Interior->Disp_NElems;
        int i;
        COLOUR Hue, Elem;
        RGB refl;
        DBL ior2, lc, ior_mult;

        lc = 3.0/count;

        reflectivity[0] = reflectivity[1] = reflectivity[2] = 0;

        ior2 = ior /sqrt(disp);
        ior_mult = pow(disp, 1.0/(count-1));
        for (i = 1; i <= count; i++)
        {
          Dispersion_Element_Hue(Hue, i, count);
          Elem[pRED]   = lc * Hue[pRED];
          Elem[pGREEN] = lc * Hue[pGREEN];
          Elem[pBLUE]  = lc * Hue[pBLUE];
        
          if ( Elem[pRED] > EPSILON ||
               Elem[pGREEN] > EPSILON ||
               Elem[pBLUE] > EPSILON ) 
          {
            ior2 = ior /sqrt(disp) * pow(disp,(DBL)i/2.5);
            g = sqrt(Sqr(ior2) + Sqr(cos_angle) - 1.0);
            F = 0.5 * (Sqr(g - cos_angle) / Sqr(g + cos_angle));
            F = F * (1.0 + Sqr(cos_angle * (g + cos_angle) - 1.0) / Sqr(cos_angle * (g - cos_angle) + 1.0));

            F=min(1.0,max(0.0,F));

            VLinComb2(refl,F,Reflection_Max,(1.0-F),Reflection_Min);
            
            reflectivity[0] += refl[0]*Elem[0];
            reflectivity[1] += refl[1]*Elem[1];
            reflectivity[2] += refl[2]*Elem[2];
          }

          ior2 *= ior_mult;
        }
      }*/

      *weight = *weight * max3(reflectivity[0],reflectivity[1],reflectivity[2]);
      break;

    default:
      Error("Illegal reflection_type.");
  }
}


#ifdef GLOBAL_PHOTONS
/*****************************************************************************
*
* FUNCTION
*
*   GlobalPhotonDiffuse (based on Diffuse)
*
*   Preconditions:
*     same as Diffuse() with this addition:
*
*     If photonOptions.photonsEnabled is true now, then
*     InitBacktraceEverything must have been called with
*     photonOptions.photonsEnabled true.
*
*
* AUTHOR
*
*   Nathan Kopp (this is based on Diffuse)
*
* DESCRIPTION
*
*   Computes diffuse, phong, specular, etc. based on the incoming photons
*   stored in the various photon maps.
*
* CHANGES
*
*   -
*
******************************************************************************/

static void GlobalPhotonDiffuse (FINISH *Finish, VECTOR IPoint, RAY *Eye, VECTOR  Layer_Normal, COLOUR Layer_Pigment_Colour, COLOUR Colour, DBL Attenuation, OBJECT *Object)
{
  DBL Cos_Shadow_Angle;
  RAY Light_Source_Ray;
  VECTOR REye;
  COLOUR Light_Colour, TempCol, Colour2;
  DBL Size, r, tempr;
  int n, tempn;
  int j;
  /*int step;*/
  DBL thisDensity=0;
  DBL prevDensity=0.0000000000000001; /* avoid div-by-zero error */
  int expanded = false;
  DBL att;  /* attenuation for lambertian compensation & filters */

  if (!photonOptions.globalPhotonMap.numPhotons)
  {
    Make_ColourA(Colour,0.0,0.0,0.0,0.0,0.0);
    return;
  }

  if ((Finish->Diffuse == 0.0) && (Finish->Specular == 0.0) && (Finish->Phong == 0.0))
  {
    Make_ColourA(Colour,0.0,0.0,0.0,0.0,0.0);
    return;
  }

  /* statistics */
  Increase_Counter(stats[Gather_Performed_Count]);

  if (Finish->Specular != 0.0)
  {
    REye[X] = -Eye->Direction[X];
    REye[Y] = -Eye->Direction[Y];
    REye[Z] = -Eye->Direction[Z];
  }

  Make_Colour(Colour,0,0,0);
  Make_Colour(Colour2,0,0,0);
    Make_Colour(TempCol,0,0,0);

  /*Size = photonOptions.globalGatherRad;*/
  Size = 2.0;

    tempr = 0;

    /* gather the photons */
    tempn=gatherPhotons(IPoint, Size, &tempr,Layer_Normal,true,&photonOptions.globalPhotonMap);

    /* now go through these photons and add up their contribution */
    for(j=0; j<tempn; j++)
    {
      /*DBL theta,phi;*/
      int theta,phi;

      /* convert small color to normal color */
      photonRgbe2colour(Light_Colour, photonOptions.photonGatherList[j]->Colour);

      /* convert theta/phi to vector direction 
         Use a pre-computed array of sin/cos to avoid many calls to the
         sin() and cos() functions.  These arrays were initialized in
         InitBacktraceEverything.
      */
      theta = photonOptions.photonGatherList[j]->theta+127;
      phi = photonOptions.photonGatherList[j]->phi+127;
      
      Light_Source_Ray.Direction[Y] = photonOptions.sinTheta[theta];
      Light_Source_Ray.Direction[X] = photonOptions.cosTheta[theta];

      Light_Source_Ray.Direction[Z] = Light_Source_Ray.Direction[X]*photonOptions.sinTheta[phi];
      Light_Source_Ray.Direction[X] = Light_Source_Ray.Direction[X]*photonOptions.cosTheta[phi];

      VSub(Light_Source_Ray.Initial, photonOptions.photonGatherList[j]->Loc, Light_Source_Ray.Direction);

      /* this compensates for real lambertian (diffuse) lighting (see paper) */
      VDot(att, Layer_Normal, Light_Source_Ray.Direction);
      if (att>1) att=1.0;
      if (att<.1) att = 0.1; /* limit to 10x - otherwise we get bright dots */
      att = 1.0 / fabs(att);

      /* do gaussian filter */
      /*att *= 0.918*(1.0-(1.0-exp((-1.953) * photonOptions.photonDistances[j])) / (1.0-exp(-1.953)) );*/
      /* do cone filter */
      /*att *= 1.0-(sqrt(photonOptions.photonDistances[j])/(4.0 * tempr)) / (1.0-2.0/(3.0*4.0));*/

      VScaleEq(Light_Colour,att);

      /* See if light on far side of surface from camera. */
      if (!(Test_Flag(Object, DOUBLE_ILLUMINATE_FLAG)))
      {
        VDot(Cos_Shadow_Angle, Layer_Normal, Light_Source_Ray.Direction);
        if (Cos_Shadow_Angle < EPSILON)
          continue;
      }

      /* now add diffuse, phong, specular, irid contribution */
      if (Finish->Diffuse > 0.0)
      {
        do_diffuse(Finish,&Light_Source_Ray,Layer_Normal,TempCol,Light_Colour,Layer_Pigment_Colour, Attenuation);
      }
      if (Finish->Phong > 0.0)
      {
        do_phong(Finish,&Light_Source_Ray,Eye->Direction,Layer_Normal,TempCol,Light_Colour, Layer_Pigment_Colour);
      }
      if (Finish->Specular > 0.0)
      {
        do_specular(Finish,&Light_Source_Ray,REye,Layer_Normal,TempCol,Light_Colour, Layer_Pigment_Colour);
      }
      if (Finish->Irid > 0.0)
      {
        do_irid(Finish,&Light_Source_Ray,Layer_Normal,IPoint,TempCol);
      }

    }

    /* density of this search */
    thisDensity = tempn / (tempr*tempr);

      r = tempr;
      n = tempn;
      Assign_Colour(Colour2, TempCol);

  /* finish the photons equation */
  VScaleEq(Colour2, (DBL)(1.0)/(M_PI*r*r));

  /* add photon contribution to total lighting */
  VAddEq(Colour, Colour2);
}
/* NK ---- */
#endif

static void InitMallocPools(void)
{ 
  int i;
  long DBLSize = Number_Of_Textures_And_Weights * sizeof(DBL);
  long TEXTURESize = Number_Of_Textures_And_Weights * sizeof(TEXTURE *);
  int NumberOfLightSources = Frame.Number_Of_Light_Sources;

  MaxMediaPoolDepth = DEFAULT_MEDIA_BUF_DEPTH;
  MaxLightPoolDepth = DEFAULT_LIGHT_BUF_DEPTH;

  MediaMallocPoolSize=0;
  TotalMallocPoolSize=0;

	#ifdef AccumulatePoolStatistics
  MaxShadowTextRecCntr=-1;
  MaxLightedTexture=-1;
  MaxAppColourRecCntr=-1;
  MaxSimMediatRecCntr=-1;
	#endif

  ShadowMediaListIndex=-1;
  LightingMediaListIndex=-1;
  MediaPoolIndex=-1;
  LightingPoolIndex=-1;

  if ( NumberOfLightSources==0 )
    NumberOfLightSources = 2;

  s0 = (DBL *)POV_MALLOC(NumberOfLightSources*sizeof(DBL), "temp data");
  s1 = (DBL *)POV_MALLOC(NumberOfLightSources*sizeof(DBL), "temp data");

  ShadowMediaListPoolSize = (long *)POV_MALLOC(MaxMediaPoolDepth*sizeof(long *), "temp media list");
  LightingMediaListPoolSize = (long *)POV_MALLOC(MaxMediaPoolDepth*sizeof(long *), "temp media list");
  MediaIntervalPoolSize = (long *)POV_MALLOC(MaxMediaPoolDepth*sizeof(long *), "temp media list");
  ShadowMediaListPool = (IMEDIA ***)POV_MALLOC(MaxMediaPoolDepth*sizeof(IMEDIA **), "temp media list");
  LightingMediaListPool = (IMEDIA ***)POV_MALLOC(MaxMediaPoolDepth*sizeof(IMEDIA **), "temp media list");
  MediaLightListPool = (LIGHT_LIST **)POV_MALLOC(MaxMediaPoolDepth*sizeof(LIGHT_LIST *), "light list");
  MediaLitIntervalPool = (LIT_INTERVAL **)POV_MALLOC(MaxMediaPoolDepth*sizeof(LIT_INTERVAL *), "lit interval");
  MediaIntervalPool = (MEDIA_INTERVAL **)POV_MALLOC(MaxMediaPoolDepth*sizeof(MEDIA_INTERVAL *), "media intervals");

  for ( i=0; i<MaxMediaPoolDepth; i++) 
  {
    ShadowMediaListPool[i] = (IMEDIA **)POV_MALLOC(10*sizeof(IMEDIA *), "temp media list");
    ShadowMediaListPoolSize[i]=10;
    LightingMediaListPool[i] = (IMEDIA **)POV_MALLOC(10*sizeof(IMEDIA *), "temp media list");
    LightingMediaListPoolSize[i]=10;
    MediaLightListPool[i] = (LIGHT_LIST *)POV_MALLOC(NumberOfLightSources*sizeof(LIGHT_LIST), "light list");
    MediaLitIntervalPool[i] = (LIT_INTERVAL *)POV_MALLOC((2*NumberOfLightSources+1)*sizeof(LIT_INTERVAL), "lit interval");
    MediaIntervalPool[i]= (MEDIA_INTERVAL *)POV_MALLOC(10*sizeof(MEDIA_INTERVAL), "media intervals");
    MediaIntervalPoolSize[i]=10;
    
    MediaMallocPoolSize+=10*sizeof(IMEDIA *)+10*sizeof(IMEDIA *)+NumberOfLightSources*sizeof(LIGHT_LIST)+
      (2*NumberOfLightSources+1)*sizeof(LIT_INTERVAL)+10*sizeof(MEDIA_INTERVAL);
  }

  TotalMallocPoolSize += MediaMallocPoolSize;

  WeightListPool = (DBL **)POV_MALLOC(MaxLightPoolDepth*sizeof(DBL *), "Weight list stack");
  TextureListPool = (TEXTURE ***)POV_MALLOC(MaxLightPoolDepth*sizeof(TEXTURE **), "Weight list stack");
  LightListPool = (LIGHT_TESTED **)POV_MALLOC(MaxLightPoolDepth*sizeof(LIGHT_TESTED *), "Weight list stack");

  for ( i=0; i<MaxLightPoolDepth; i++) 
  {
    WeightListPool[i]= (DBL *)POV_MALLOC(DBLSize, "Weight list stack");
    TextureListPool[i]= (TEXTURE **)POV_MALLOC(TEXTURESize, "Weight list stack");
    LightListPool[i] = (LIGHT_TESTED *)POV_MALLOC(max(1,Frame.Number_Of_Light_Sources)*sizeof(LIGHT_TESTED), "Weight list stack");
    TotalMallocPoolSize += DBLSize+ TEXTURESize+ DBLSize+ TEXTURESize+
            max(1,Frame.Number_Of_Light_Sources)*sizeof(LIGHT_TESTED);
  }
}


static void DeInitMallocPools(void)
{ 
  int i;

  if ( s0 != NULL)  POV_FREE(s0); s0=NULL;
  if ( s1 != NULL) POV_FREE(s1); s1=NULL;

  if (ShadowMediaListPool)
  {
    for (i=0; i<MaxMediaPoolDepth; i++) 
    {
      if ( ShadowMediaListPool[i] != NULL) POV_FREE(ShadowMediaListPool[i]);
      ShadowMediaListPool[i]=NULL;

      if ( LightingMediaListPool[i] != NULL) POV_FREE(LightingMediaListPool[i]);
      LightingMediaListPool[i]=NULL;

      if ( MediaLightListPool[i] != NULL) POV_FREE(MediaLightListPool[i]);
      MediaLightListPool[i]=NULL;

      if ( MediaLitIntervalPool[i] != NULL) POV_FREE(MediaLitIntervalPool[i]);
      MediaLitIntervalPool[i]=NULL;

      if ( MediaIntervalPool[i] != NULL) POV_FREE(MediaIntervalPool[i]);
      MediaIntervalPool[i]=NULL;
    }
  }

  if (ShadowMediaListPool != NULL) POV_FREE(ShadowMediaListPool);
  ShadowMediaListPool=NULL;
  if (ShadowMediaListPoolSize != NULL) POV_FREE(ShadowMediaListPoolSize);
  ShadowMediaListPoolSize=NULL;

  if ( LightingMediaListPool != NULL) POV_FREE(LightingMediaListPool);
  LightingMediaListPool=NULL;
  if ( LightingMediaListPoolSize != NULL) POV_FREE(LightingMediaListPoolSize);
  LightingMediaListPoolSize=NULL;

  if ( MediaLightListPool != NULL) POV_FREE(MediaLightListPool);
  MediaLightListPool=NULL;

  if ( MediaLitIntervalPool != NULL) POV_FREE(MediaLitIntervalPool);
  MediaLitIntervalPool=NULL;

  if ( MediaIntervalPool != NULL) POV_FREE(MediaIntervalPool);
  MediaIntervalPool=NULL;
  if ( MediaIntervalPoolSize != NULL) POV_FREE(MediaIntervalPoolSize);
  MediaIntervalPoolSize=NULL;


  if (LightListPool)
  {
    for (i=0; i<MaxLightPoolDepth; i++) 
    {
      if ( LightListPool[i] != NULL) POV_FREE(LightListPool[i]);   
      LightListPool[i]=NULL;

      if ( WeightListPool[i] != NULL) POV_FREE(WeightListPool[i]); 
      WeightListPool[i]=NULL;

      if ( TextureListPool[i] != NULL) POV_FREE(TextureListPool[i]);  
      TextureListPool[i]=NULL;
    }
  }

  if ( LightListPool != NULL) POV_FREE(LightListPool);   
  LightListPool=NULL;

  if ( WeightListPool != NULL) POV_FREE(WeightListPool); 
  WeightListPool=NULL;

  if ( TextureListPool != NULL) POV_FREE(TextureListPool);  
  TextureListPool=NULL;
}
    
static void ReInitMallocPools(void)
{ 
  int i;
  long DBLSize=Number_Of_Textures_And_Weights * sizeof(DBL);
  long TEXTURESize=Number_Of_Textures_And_Weights * sizeof(TEXTURE *);
  
  TotalMallocPoolSize=MediaMallocPoolSize;

  for ( i=0; i<MaxLightPoolDepth; i++)
  {
    WeightListPool[i]= (DBL *)POV_REALLOC(WeightListPool[i],DBLSize, "Weight list stack");
    TextureListPool[i]= (TEXTURE **)POV_REALLOC(TextureListPool[i],TEXTURESize, "Weight list stack");
    LightListPool[i] = (LIGHT_TESTED *)POV_REALLOC(LightListPool[i],max(1,Frame.Number_Of_Light_Sources)*sizeof(LIGHT_TESTED), "Weight list stack");
    TotalMallocPoolSize+=DBLSize+ TEXTURESize+  DBLSize+TEXTURESize+
      max(1,Frame.Number_Of_Light_Sources)*sizeof(LIGHT_TESTED);
  }
}


void ResizeMediaMallocPools(long newSize)
{ 
  int i;
  int NumberOfLightSources = Frame.Number_Of_Light_Sources;
  if ( NumberOfLightSources==0 ) NumberOfLightSources = 2;
  /* subtract this... we'll add it back later */
  TotalMallocPoolSize -= MediaMallocPoolSize;
  ShadowMediaListPoolSize = (long *)POV_REALLOC(ShadowMediaListPoolSize, newSize*sizeof(long *), "temp media list");
  LightingMediaListPoolSize = (long *)POV_REALLOC(LightingMediaListPoolSize, newSize*sizeof(long *), "temp media list");
  MediaIntervalPoolSize = (long *)POV_REALLOC(MediaIntervalPoolSize, newSize*sizeof(long *), "temp media list");
  ShadowMediaListPool = (IMEDIA ***)POV_REALLOC(ShadowMediaListPool, newSize*sizeof(IMEDIA **), "temp media list");
  LightingMediaListPool = (IMEDIA ***)POV_REALLOC(LightingMediaListPool, newSize*sizeof(IMEDIA **), "temp media list");
  MediaLightListPool = (LIGHT_LIST **)POV_REALLOC(MediaLightListPool, newSize*sizeof(LIGHT_LIST *), "light list");
  MediaLitIntervalPool = (LIT_INTERVAL **)POV_REALLOC(MediaLitIntervalPool, newSize*sizeof(LIT_INTERVAL *), "lit interval");
  MediaIntervalPool = (MEDIA_INTERVAL **)POV_REALLOC(MediaIntervalPool, newSize*sizeof(MEDIA_INTERVAL *), "media intervals");


  for ( i=MaxMediaPoolDepth; i<newSize; i++) 
  {
    ShadowMediaListPool[i] = (IMEDIA **)POV_MALLOC(10*sizeof(IMEDIA *), "temp media list");
    ShadowMediaListPoolSize[i]=10;
    LightingMediaListPool[i] = (IMEDIA **)POV_MALLOC(10*sizeof(IMEDIA *), "temp media list");
    LightingMediaListPoolSize[i]=10;
    MediaLightListPool[i] = (LIGHT_LIST *)POV_MALLOC(NumberOfLightSources*sizeof(LIGHT_LIST), "light list");
    MediaLitIntervalPool[i] = (LIT_INTERVAL *)POV_MALLOC((2*NumberOfLightSources+1)*sizeof(LIT_INTERVAL), "lit interval");
    MediaIntervalPool[i]= (MEDIA_INTERVAL *)POV_MALLOC(10*sizeof(MEDIA_INTERVAL), "media intervals");
    MediaIntervalPoolSize[i]=10;
    
    MediaMallocPoolSize+=10*sizeof(IMEDIA *)+10*sizeof(IMEDIA *)+NumberOfLightSources*sizeof(LIGHT_LIST)+
      (2*NumberOfLightSources+1)*sizeof(LIT_INTERVAL)+10*sizeof(MEDIA_INTERVAL);
  }
  MaxMediaPoolDepth=newSize;

  /* add media stuff back on now */
  TotalMallocPoolSize += MediaMallocPoolSize;
}

void ResizeLightMallocPools(long newSize)
{
  int i;
  long DBLSize = Number_Of_Textures_And_Weights * sizeof(DBL);
  long TEXTURESize = Number_Of_Textures_And_Weights * sizeof(TEXTURE *);
  int NumberOfLightSources = Frame.Number_Of_Light_Sources;
  if ( NumberOfLightSources==0 ) NumberOfLightSources = 2;

  WeightListPool = (DBL **)POV_REALLOC(WeightListPool, newSize*sizeof(DBL *), "Weight list stack");
  TextureListPool = (TEXTURE ***)POV_REALLOC(TextureListPool, newSize*sizeof(TEXTURE **), "Weight list stack");
  LightListPool = (LIGHT_TESTED **)POV_REALLOC(LightListPool, newSize*sizeof(LIGHT_TESTED *), "Weight list stack");

  for ( i=MaxLightPoolDepth; i<newSize; i++) 
  {
    WeightListPool[i]= (DBL *)POV_MALLOC(DBLSize, "Weight list stack");
    TextureListPool[i]= (TEXTURE **)POV_MALLOC(TEXTURESize, "Weight list stack");
    LightListPool[i] = (LIGHT_TESTED *)POV_MALLOC(max(1,Frame.Number_Of_Light_Sources)*sizeof(LIGHT_TESTED), "Weight list stack");
    TotalMallocPoolSize += DBLSize+ TEXTURESize+ DBLSize+ TEXTURESize+
            max(1,Frame.Number_Of_Light_Sources)*sizeof(LIGHT_TESTED);
  }
	MaxLightPoolDepth=newSize;

}


void InitComTexDataPool()
{
	ComputeTextureUsedPool = NULL;
	ComputeTextureFreePool = NULL;
	ComputeTexturePoolSize = 0;
}


void FreeComTexDataPool()
{
	ComTexData *ctd = NULL;

	while(ComputeTextureUsedPool != NULL)
	{
		ctd = ComputeTextureUsedPool;
		ComputeTextureUsedPool = ComputeTextureUsedPool->next;
		POV_FREE(ctd);
	}

	while(ComputeTextureFreePool != NULL)
	{
		ctd = ComputeTextureFreePool;
		ComputeTextureFreePool = ComputeTextureFreePool->next;
		POV_FREE(ctd);
	}

	// Debug_Info("ComTexData pool size %d\n", ComputeTexturePoolSize);
}


ComTexData *NewComTexData()
{
	ComTexData *ctd = ComputeTextureFreePool;

	if(ctd != NULL) // if anything is in the free pool
	{
		// remove from free list
		if(ctd->previous != NULL)
			ctd->previous->next = ctd->next;
		if(ctd->next != NULL)
			ctd->next->previous = ctd->previous;

		if(ctd == ComputeTextureFreePool)
			ComputeTextureFreePool = ctd->next;
	}
	else // if not allocate new space
	{
		ctd = (ComTexData *)POV_MALLOC(sizeof(ComTexData), "compute texture data");
		ComputeTexturePoolSize++;
	}

	// insert into used list
	ctd->previous = NULL;
	ctd->next = ComputeTextureUsedPool;

	if(ComputeTextureUsedPool != NULL)
		ComputeTextureUsedPool->previous = ctd;
	ComputeTextureUsedPool = ctd;

	return ctd;
}


void FreeComTexData(ComTexData *ctd)
{
	if(ctd == NULL)
		return;

	// remove from used list
	if(ctd->previous != NULL)
		ctd->previous->next = ctd->next;
	if(ctd->next != NULL)
		ctd->next->previous = ctd->previous;

	if(ctd == ComputeTextureUsedPool)
		ComputeTextureUsedPool = ctd->next;

	// insert into free list
	ctd->previous = NULL;
	ctd->next = ComputeTextureFreePool;

	if(ComputeTextureFreePool != NULL)
		ComputeTextureFreePool->previous = ctd;
	ComputeTextureFreePool = ctd;
}

END_POV_NAMESPACE
