/****************************************************************************
 *                  media.cpp
 *
 * This module contains all functions for participating media.
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
 * $File: //depot/povray/3.6-release/source/media.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "vector.h"
#include "chi2.h"
#include "colour.h"
#include "povray.h"
#include "texture.h"
#include "pigment.h"
#include "objects.h"
#include "lighting.h"
#include "matrices.h"
#include "media.h"
#include "pattern.h"
#include "povray.h"
#include "point.h"
#include "texture.h"
#include "ray.h"
#include "lightgrp.h"
#include "photons.h"

#include <algorithm>

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

/*****************************************************************************
* Local typedefs
******************************************************************************/
extern long MaxMediaPoolDepth; // GLOBAL VARIABLE
extern LIT_INTERVAL **MediaLitIntervalPool; // GLOBAL VARIABLE
extern LIGHT_LIST **MediaLightListPool; // GLOBAL VARIABLE
extern MEDIA_INTERVAL **MediaIntervalPool; // GLOBAL VARIABLE
extern long *MediaIntervalPoolSize; // GLOBAL VARIABLE
extern DBL *s0, *s1; // GLOBAL VARIABLE
extern long MediaPoolIndex; // GLOBAL VARIABLE
#ifdef AccumulatePoolStatistics
extern long MaxSimMediatRecCntr; // GLOBAL VARIABLE
#endif
/*****************************************************************************
* Local variables
******************************************************************************/
static int sampCount_s; // GLOBAL VARIABLE


/*****************************************************************************
* Static functions
******************************************************************************/

static void sample_media_rec(LIGHT_LIST *light_list, int, RAY *Ray, IMEDIA **Media_List, MEDIA_INTERVAL *Interval, int light_ray,
                             DBL d1, DBL d3,
                             COLOUR Result, COLOUR C1, COLOUR C3,
                             COLOUR ODResult, COLOUR od1, COLOUR od3,
                             int depth, DBL Jitter, DBL aa_threshold,
                             LIGHT_LIST *local_light_list, int local_light_list_cnt);

static void sample_media (LIGHT_LIST *, int, RAY *, IMEDIA **, MEDIA_INTERVAL *, int, DBL, COLOUR, COLOUR, int, LIGHT_LIST *, int);
static void scattering_attenuation(IMEDIA **, COLOUR, COLOUR, COLOUR, RAY *, RAY *);

static void get_light_list (LIGHT_LIST *, LIGHT_LIST*&, int&, RAY *, INTERSECTION *);
static void update_light_list_entry(LIGHT_LIST *, RAY *, INTERSECTION *);
static void get_lit_interval (int *, LIT_INTERVAL *, int, LIGHT_LIST *, INTERSECTION *, LIGHT_LIST *, int);
static int set_up_sampling_intervals (MEDIA_INTERVAL *, int, LIT_INTERVAL *, IMEDIA *);

static int intersect_spotlight (RAY *Ray, LIGHT_SOURCE *Light, DBL *d1, DBL *d2);
static int intersect_cylinderlight (RAY *Ray, LIGHT_SOURCE *Light, DBL *d1, DBL *d2);

static int CDECL compdoubles (const void *in_a, const void *in_b);

static void evaluate_density_pattern (IMEDIA *, VECTOR , COLOUR);


void Backtrace_Simulate_Media(IMEDIA **Media_List, RAY *Ray, INTERSECTION *Inter, COLOUR Colour)
{
  int i, j, intervals, use_extinction, use_scattering, ignore_photons;
  int lit_interval_entries;
  COLOUR Od;
  LIT_INTERVAL *Lit_Interval;
  IMEDIA *IMedia, **Tmp, *Local;
  MEDIA_INTERVAL *Media_Interval, *curr;
  int minSamples;
  /* NK samples */
  DBL d0;
  COLOUR C0;
  COLOUR od0;
  COLOUR PhotonColour;
  VECTOR TempPoint;
  DBL mediaSpacingFactor;

  /* Why are we here? */

	int thesize;	

  MediaPoolIndex++;
	#ifdef AccumulatePoolStatistics
	  MaxSimMediatRecCntr=max(MaxSimMediatRecCntr,MediaPoolIndex);
	#endif
  if ((Media_List == NULL) || (Media_List[0] == NULL))  
  {
    MediaPoolIndex--;
    return;  
  }

  /* Find media with the largest number of intervals. */

  intervals = 0;

  use_extinction = false;
  use_scattering = false;
  ignore_photons = true;

  IMedia = Media_List[0];

  for (Tmp = Media_List; (*Tmp) != NULL; Tmp++)
  {
    for (Local = *Tmp; Local != NULL; Local = Local->Next_Media)
    {
      if (Local->Intervals > intervals)
      {
        intervals = Local->Intervals;

        IMedia = Local;
      }

      use_extinction |= Local->use_extinction;
      use_scattering |= Local->use_scattering;
      ignore_photons &= Local->ignore_photons;
    }
  }

  /* If this is a light ray and no extinction is used we can return. */

  if (!use_extinction)
  {
    MediaPoolIndex--;
    return;
  }

  /*
   * Prepare the Monte Carlo integration along the ray from P0 to P1.
   */

  /* light_ray always true */
  if ( MediaPoolIndex >= MaxMediaPoolDepth )
  {
    ResizeMediaMallocPools(MaxMediaPoolDepth*2);
  }

  Lit_Interval=MediaLitIntervalPool[MediaPoolIndex];

  lit_interval_entries = 1;

  Lit_Interval[0].lit = false;

  Lit_Interval[0].s0 = 0.0;
  Lit_Interval[0].s1 =
  Lit_Interval[0].ds = Inter->Depth;

  /* Set up sampling intervals. */

  /* NK samples - make sure we will always have enough intervals 
     getting half-way through a really long render and then having it stop with a
     "too few intervals" error message is just not acceptable.
     And for adaptive subdivision, the fewer intervals, the better
  
     By the way, I don't know everything about this media code, so this may not
     work perfectly, but I haven't had any problems yet, and it doesn't look like
     it should cause any in the future. */
  if (lit_interval_entries>IMedia->Intervals)
    thesize=lit_interval_entries*sizeof(MEDIA_INTERVAL);
  else		
    thesize=IMedia->Intervals*sizeof(MEDIA_INTERVAL);

  if ( MediaPoolIndex >= MaxMediaPoolDepth)	
  {
    ResizeMediaMallocPools(MaxMediaPoolDepth*2);
  }

  if ( thesize <= MediaIntervalPoolSize[MediaPoolIndex])
    Media_Interval=MediaIntervalPool[MediaPoolIndex];
  else		
  {
    POV_FREE(MediaIntervalPool[MediaPoolIndex]);
    Media_Interval=MediaIntervalPool[MediaPoolIndex] = (MEDIA_INTERVAL *)POV_MALLOC(thesize, "media intervals");
    MediaIntervalPoolSize[MediaPoolIndex]=thesize;
  }
  intervals = set_up_sampling_intervals(Media_Interval, lit_interval_entries, Lit_Interval, IMedia);

  minSamples = IMedia->Min_Samples;
  if (IMedia->Sample_Method == 3)
    minSamples*=2;

  /* -------- should only use one interval! ----------- */

  /* Sample all intervals. */
  Od[0] = Colour[0];
  Od[1] = Colour[1];
  Od[2] = Colour[2];

  for (i = 0; i < intervals; i++)
  {
    if(!photonOptions.Light->Parallel)
    {
      minSamples=(int)
        (Media_Interval[i].ds /
        (photonOptions.photonSpread *
        photonOptions.photonDepth *
        photonOptions.mediaSpacingFactor));
    }
    else
    {
      minSamples=(int)
        (Media_Interval[i].ds /
        (photonOptions.photonSpread *
        photonOptions.mediaSpacingFactor));
    }

    if (minSamples<=photonOptions.maxMediaSteps)
    {
      /* all's well */
      mediaSpacingFactor = photonOptions.mediaSpacingFactor;
    }
    else
    {
      /* too many steps - use fewer steps and a bigger spacing factor */
      minSamples = photonOptions.maxMediaSteps;
      if(!photonOptions.Light->Parallel)
      {
        mediaSpacingFactor = 
          (Media_Interval[i].ds /
          (photonOptions.photonSpread *
          photonOptions.photonDepth *
          minSamples));
      }
      else
      {
        mediaSpacingFactor = 
          (Media_Interval[i].ds /
          (photonOptions.photonSpread *
          minSamples));
      }
    }
    /* Sample current interval. */

    Increase_Counter(stats[Media_Intervals]);

    /* set static sample count */
    sampCount_s = minSamples;

    for (j = 0; j < minSamples; j++)
    {
      d0 = (j + 0.5 + FRAND()*photonOptions.jitter - 0.5*photonOptions.jitter) / minSamples;
      sample_media(NULL, 0, Ray, Media_List, &Media_Interval[i], true, d0, C0, od0, 2 /* use method 2 */, NULL, 0);

      if (use_scattering && !ignore_photons)
      {
        if(!photonOptions.Light->Parallel)
        {
          VScale(PhotonColour,Od, mediaSpacingFactor * 
                                  photonOptions.photonSpread *
                                  (photonOptions.photonDepth+d0*Media_Interval[i].ds+Media_Interval[i].s0));
        }
        else
        {
          VScale(PhotonColour,Od, mediaSpacingFactor * 
                                  photonOptions.photonSpread);
        }

        Od[0] = Colour[0]*exp(-Media_Interval[i].od[0]/(minSamples*2));
        Od[1] = Colour[1]*exp(-Media_Interval[i].od[1]/(minSamples*2));
        Od[2] = Colour[2]*exp(-Media_Interval[i].od[2]/(minSamples*2));

        VEvaluateRay(TempPoint, Ray->Initial, d0*Media_Interval[i].ds+Media_Interval[i].s0, Ray->Direction);
        
        addMediaPhoton(TempPoint, Ray->Initial, PhotonColour, d0*Media_Interval[i].ds+Media_Interval[i].s0);
      }
    }
  }

  /* Sum the influences of all intervals. */
  Make_Colour(Od, 0.0, 0.0, 0.0);

  curr = &Media_Interval[0];

  for (i = 0; i < intervals; i++)
  {
    /* Add optical depth of current interval. */

    Od[0] += curr->od[0] / (DBL)curr->samples;
    Od[1] += curr->od[1] / (DBL)curr->samples;
    Od[2] += curr->od[2] / (DBL)curr->samples;

    curr++;
  }

  /* Add contribution estimated for the participating media. */
  Colour[0] = Colour[0] * exp(-Od[0]);
  Colour[1] = Colour[1] * exp(-Od[1]);
  Colour[2] = Colour[2] * exp(-Od[2]);

	MediaPoolIndex--;
}
/*****************************************************************************
*
* FUNCTION
*
*   Simulate_Media
*
* INPUT
*
*   Ray       - Current ray, start point P0
*   Inter     - Current intersection, end point P1
*   Colour    - Color emitted at P1 towards P0
*   light_ray - true if we are looking at a light source ray
*
* OUTPUT
*
*   Colour    - Color arriving at the end point
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Simulate participating media using volume sampling.
*
*   The effects of participating media on the light emitted at P1
*   towards P0 are determined using Monte Carlo integration.
*
*   The effects include: emission, absoprtion and scattering.
*
*   Currently one global medium with constant coefficients is implemented.
*
*   Ideas for the atmospheric scattering were taken from:
*
*     - M. Inakage, "An Illumination Model for Atmospheric Environments", ..
*
*     - Nishita, T., Miyawaki, Y. and Nakamae, E., "A Shading Model for
*       Atmospheric Scattering Considering Luminous Intensity Distribution
*       of Light Sources", Computer Graphics, 21, 4, 1987, 303-310
*
* CHANGES
*
*   Nov 1994 : Creation.
*
*   Jan 1995 : Added support of cylindrical light sources. [DB]
*
*   Jun 1995 : Added code for alpha channel support. [DB]
*
******************************************************************************/

void Simulate_Media(IMEDIA **Media_List, RAY *Ray, INTERSECTION *Inter, COLOUR Colour, int light_ray)
{
  int i, j, intervals, use_extinction;
  int lit_interval_entries;
  DBL n;
  COLOUR Od, Te, Va;
  LIGHT_LIST *light_list = NULL;
  LIGHT_LIST *local_light_list = NULL;
  LIT_INTERVAL *Lit_Interval;
  IMEDIA **Tmp;
  IMEDIA *IMedia, *Local;
  MEDIA_INTERVAL *Media_Interval, *curr;
  /* NK samples */
  DBL d0, dd;
  COLOUR C0, C1, Result;
  COLOUR ODResult;
  COLOUR od0,od1;
  /* NK fast light_ray media calculation for constant media */
  int all_constant_and_light_ray;  /* is all the media constant? */
  int minSamples;		/* JB */
  int local_light_list_cnt = 0;
  int global_light_list_cnt = Frame.Number_Of_Light_Sources;

  /* Why are we here? */

	int thesize;	

  MediaPoolIndex++;
	#ifdef AccumulatePoolStatistics
		 MaxSimMediatRecCntr=max(MaxSimMediatRecCntr,MediaPoolIndex);
	#endif

  if ((Media_List == NULL) || (Media_List[0] == NULL))  
  {
    MediaPoolIndex--;
    return;  
  }
  /* Find media with the largest number of intervals. */

  intervals = 0;

  use_extinction = false;

  /* NK fast light_ray media calculation for constant media */
  all_constant_and_light_ray = light_ray;

  IMedia = Media_List[0];

  for (Tmp = Media_List; (*Tmp) != NULL; Tmp++)
  {
    for (Local = *Tmp; Local != NULL; Local = Local->Next_Media)
    {
      if (Local->Intervals > intervals)
      {
        intervals = Local->Intervals;

        IMedia = Local;
      }

      use_extinction |= Local->use_extinction;

      /* NK fast light_ray media calculation for constant media */
      if(Local->Density)
      {
        all_constant_and_light_ray &= (Local->Density->Type == PLAIN_PATTERN);
      }
    }
  }

  /* If this is a light ray and no extinction is used we can return. */

  if ((light_ray) && (!use_extinction))
  {
    MediaPoolIndex--;
    return;
  }

  /* NK fast light_ray media calculation for constant media */
  if (all_constant_and_light_ray)
  {
    intervals = 1;
  }

  /*
   * Prepare the Monte Carlo integration along the ray from P0 to P1.
   */

  if(Inter->Object != NULL)
  {
    if((Inter->Object->Flags & NO_GLOBAL_LIGHTS_FLAG) == NO_GLOBAL_LIGHTS_FLAG)
      global_light_list_cnt = 0;
  }

  if (light_ray || (global_light_list_cnt==0))
  {
    if ( MediaPoolIndex >= MaxMediaPoolDepth )
    {
      ResizeMediaMallocPools(MaxMediaPoolDepth*2);
    }

    Lit_Interval=MediaLitIntervalPool[MediaPoolIndex];

    lit_interval_entries = 1;

    Lit_Interval[0].lit = false;

    Lit_Interval[0].s0 = 0.0;
    Lit_Interval[0].s1 =
    Lit_Interval[0].ds = Inter->Depth;
  }

  if(!light_ray)
  {
    /* Get light list. */

    if ( MediaPoolIndex >= MaxMediaPoolDepth)	
    {
      ResizeMediaMallocPools(MaxMediaPoolDepth*2);
    }
    light_list = MediaLightListPool[MediaPoolIndex];

    get_light_list(light_list, local_light_list, local_light_list_cnt, Ray, Inter);

    /* Get lit intervals. */
    /* already resized */
    Lit_Interval=MediaLitIntervalPool[MediaPoolIndex];

    get_lit_interval(&lit_interval_entries, Lit_Interval, global_light_list_cnt, light_list, Inter, local_light_list, local_light_list_cnt);
  }

  /* Set up sampling intervals. */

  /* NK samples - make sure we will always have enough intervals 
     getting half-way through a really long render and then having it stop with a
     "too few intervals" error message is just not acceptable.
     And for adaptive subdivision, the fewer intervals, the better
  
     By the way, I don't know everything about this media code, so this may not
     work perfectly, but I haven't had any problems yet, and it doesn't look like
     it should cause any in the future. */
  if (lit_interval_entries>IMedia->Intervals)
    thesize=lit_interval_entries*sizeof(MEDIA_INTERVAL);
  else		
    thesize=IMedia->Intervals*sizeof(MEDIA_INTERVAL);

  if ( MediaPoolIndex >= MaxMediaPoolDepth)	
  {
    ResizeMediaMallocPools(MaxMediaPoolDepth*2);
  }

  if ( thesize <= MediaIntervalPoolSize[MediaPoolIndex])
    Media_Interval=MediaIntervalPool[MediaPoolIndex];
  else		
  {
    POV_FREE(MediaIntervalPool[MediaPoolIndex]);
    Media_Interval=MediaIntervalPool[MediaPoolIndex] = (MEDIA_INTERVAL *)POV_MALLOC(thesize, "media intervals");
    MediaIntervalPoolSize[MediaPoolIndex]=thesize;
  }
  intervals = set_up_sampling_intervals(Media_Interval, lit_interval_entries, Lit_Interval, IMedia);

  minSamples = IMedia->Min_Samples;

  /* Sample all intervals. */

  if ((IMedia->Sample_Method == 3) && !all_constant_and_light_ray)
  {
    /* adaptive sampling */
    int sampleCount; /* internal count for samples to take */
    IMEDIA *AA_Search_Media=*Media_List;
    DBL aa_threshold=1000;

    /* find smallest AA_Threshold */
    while (AA_Search_Media) {
    if (AA_Search_Media->AA_Threshold < aa_threshold)
      aa_threshold=AA_Search_Media->AA_Threshold;
      AA_Search_Media=AA_Search_Media->Next_Media;
    }

    for (i = 0; i < intervals; i++)
    {
      /* Sample current interval. */

      Increase_Counter(stats[Media_Intervals]);

      sampleCount = (int)((minSamples) / 2.0) + 1;

      if (sampleCount < 2)
        sampleCount = 2;

      /* set static sample count */
      sampCount_s = sampleCount - 1;

      if (sampleCount < 2)
      {
        /* always do at least three samples - one on each end and one in the middle */

        /* don't re-sample this if we've already done it in the previous interval */
        sample_media(light_list, global_light_list_cnt, Ray, Media_List, &Media_Interval[i], light_ray, 0.0+IMedia->Jitter*(FRAND()-0.5), C0, od0, 3, local_light_list, local_light_list_cnt);
        sample_media(light_list, global_light_list_cnt, Ray, Media_List, &Media_Interval[i], light_ray, 1.0+IMedia->Jitter*(FRAND()-0.5), C1, od1, 3, local_light_list, local_light_list_cnt);
        sample_media_rec(light_list, global_light_list_cnt, Ray, Media_List, &Media_Interval[i], light_ray,
                         0.0, 1.0, Media_Interval[i].te, C0, C1,
                         Media_Interval[i].od, od0, od1,
                         IMedia->AA_Level-1, IMedia->Jitter,aa_threshold,
                         local_light_list, local_light_list_cnt);
        Media_Interval[i].samples = 1;

        /* move c1 to c0 to go on to next sample/interval */
        C0[0]=C1[0];
        C0[1]=C1[1];
        C0[2]=C1[2];
        /* move od1 to od0 to go on to the next sample/interval */
        od0[0]=od1[0];
        od0[1]=od1[1];
        od0[2]=od1[2];
      }
      else
      {
        dd = 1.0 / (sampleCount+1);
        d0 = 0.0;

          sample_media(light_list, global_light_list_cnt, Ray, Media_List, &Media_Interval[i], light_ray,
                     d0+dd*IMedia->Jitter*(FRAND()-0.5), C0, od0, 3, local_light_list, local_light_list_cnt);
        /*else
        {
          VScaleEq(C0,Media_Interval[i].ds/Media_Interval[i-1].ds);
          VScaleEq(od0,Media_Interval[i].ds/Media_Interval[i-1].ds);
        }*/

        /* clear out od & te */
        Media_Interval[i].te[0] =
        Media_Interval[i].te[1] =
        Media_Interval[i].te[2] = 0.;

        Media_Interval[i].od[0] =
        Media_Interval[i].od[1] =
        Media_Interval[i].od[2] = 0.;

        for (j = 1, d0+=dd; j <= sampleCount; j++, d0+=dd)
        {
          sample_media(light_list, global_light_list_cnt, Ray, Media_List, &Media_Interval[i], light_ray,
                       d0+dd*IMedia->Jitter*(FRAND()-0.5), C1, od1, 3, local_light_list, local_light_list_cnt);
          sample_media_rec(light_list, global_light_list_cnt, Ray, Media_List, &Media_Interval[i], light_ray,
                           d0-dd, d0, Result, C0, C1,
                           ODResult, od0, od1,
                           IMedia->AA_Level-1, IMedia->Jitter,aa_threshold,
                           local_light_list, local_light_list_cnt);

          /* keep a sum of the results */
          /* do some attenuation, too, since we are doing samples in order */
          Media_Interval[i].te[0]+=Result[0] * exp(-Media_Interval[i].od[0]/sampleCount);  /* should this be sampleCount+1? */
          Media_Interval[i].te[1]+=Result[1] * exp(-Media_Interval[i].od[1]/sampleCount);
          Media_Interval[i].te[2]+=Result[2] * exp(-Media_Interval[i].od[2]/sampleCount);
          /* move c1 to c0 to go on to next sample/interval */
          C0[0]=C1[0];
          C0[1]=C1[1];
          C0[2]=C1[2];

          /* now do the same for optical depth */
          Media_Interval[i].od[0]+=ODResult[0];
          Media_Interval[i].od[1]+=ODResult[1];
          Media_Interval[i].od[2]+=ODResult[2];
          /* move od1 to od0 to go on to the next sample/interval */
          od0[0]=od1[0];
          od0[1]=od1[1];
          od0[2]=od1[2];
        }
        Media_Interval[i].samples = sampleCount;
      }
    }
  }
  else
  {

  for (i = 0; i < intervals; i++)
  {
    /* Sample current interval. */

    Increase_Counter(stats[Media_Intervals]);

    /* set static sample count */
    sampCount_s = minSamples;

    for (j = 0; j < minSamples; j++)
    {
      if(IMedia->Sample_Method==2)
      {
        d0 = (j+0.5) / minSamples +
	        (FRAND() * IMedia->Jitter / minSamples);
        sample_media(light_list, global_light_list_cnt, Ray, Media_List, &Media_Interval[i], light_ray, d0, C0, od0, 2, local_light_list, local_light_list_cnt);
      }
      else
      {
        /* we may get here with media method 3 */
        d0 = FRAND();
        sample_media(light_list, global_light_list_cnt, Ray, Media_List, &Media_Interval[i], light_ray, d0, C0, od0, 1, local_light_list, local_light_list_cnt);
      }

      if (all_constant_and_light_ray)
        j = minSamples;
    }
  }

  /* Cast additional samples if necessary. */

  if ((!light_ray) && (IMedia->Max_Samples > minSamples))
  {
    curr = &Media_Interval[0];

    for (i = 0; i < intervals; i++)
    {
      if (curr->samples < IMedia->Max_Samples)
      {
        /* Get variance of samples. */

        n = (DBL)curr->samples;

        Va[0] = (curr->te2[0] / n - Sqr(curr->te[0] / n)) / n;
        Va[1] = (curr->te2[1] / n - Sqr(curr->te[1] / n)) / n;
        Va[2] = (curr->te2[2] / n - Sqr(curr->te[2] / n)) / n;

        /* Take additional samples until variance is small enough. */

        while ((Va[0] >= IMedia->Sample_Threshold[curr->samples-1]) ||
               (Va[1] >= IMedia->Sample_Threshold[curr->samples-1]) ||
               (Va[2] >= IMedia->Sample_Threshold[curr->samples-1]))
        {
          /* Sample current interval again. */

          sample_media(light_list, global_light_list_cnt, Ray, Media_List, curr, light_ray, FRAND(), C0, od0, 1 /* treat all like samp meth 1 */, local_light_list, local_light_list_cnt);

          /* Have we reached maximum number of samples. */

          if (curr->samples > IMedia->Max_Samples)
          {
            break;
          }

          /* Get variance of samples. */

          n = (DBL)curr->samples;

          Va[0] = (curr->te2[0] / n - Sqr(curr->te[0] / n)) / n;
          Va[1] = (curr->te2[1] / n - Sqr(curr->te[1] / n)) / n;
          Va[2] = (curr->te2[2] / n - Sqr(curr->te[2] / n)) / n;
        }
      }

      curr++;
    }
  }

  } /* end of if for adaptive sampling else */

  /* Sum the influences of all intervals. */

  Make_Colour(Od, 0.0, 0.0, 0.0);
  Make_Colour(Te, 0.0, 0.0, 0.0);

  curr = &Media_Interval[0];

  for (i = 0; i < intervals; i++)
  {
    /* Add total emission. */

    Te[0] += curr->te[0] / (DBL)curr->samples * exp(-Od[0]);
    Te[1] += curr->te[1] / (DBL)curr->samples * exp(-Od[1]);
    Te[2] += curr->te[2] / (DBL)curr->samples * exp(-Od[2]);

    /* Add optical depth of current interval. */

    Od[0] += curr->od[0] / (DBL)curr->samples;
    Od[1] += curr->od[1] / (DBL)curr->samples;
    Od[2] += curr->od[2] / (DBL)curr->samples;

    curr++;
  }

  /* Add contribution estimated for the participating media. */

  Od[0] = exp(-Od[0]);
  Od[1] = exp(-Od[1]);
  Od[2] = exp(-Od[2]);

  Colour[0] = Colour[0] * Od[0] + Te[0];
  Colour[1] = Colour[1] * Od[1] + Te[1];
  Colour[2] = Colour[2] * Od[2] + Te[2];

  Colour[pTRANSM] *= GREY_SCALE(Od);

  MediaPoolIndex--;

  if(local_light_list != NULL)
    POV_FREE(local_light_list);
}

static void sample_media_rec(LIGHT_LIST *light_list, int global_light_list_cnt, RAY *Ray, IMEDIA **Media_List, 
                             MEDIA_INTERVAL *Interval, int light_ray, 
                             DBL d1, DBL d3, 
                             COLOUR Result, COLOUR C1, COLOUR C3, 
                             COLOUR ODResult, COLOUR od1, COLOUR od3, 
                             int depth, DBL Jitter, DBL aa_threshold,
                             LIGHT_LIST *local_light_list, int local_light_list_cnt)
{
  COLOUR C2, Result2;
  COLOUR od2, ODResult2;
  DBL d2, jdist;

  /* d2 is between d1 and d3 (all in range of 0..1 */
  d2 = 0.5 * (d1 + d3);
  jdist = d2 + Jitter * (d3 - d1) * (FRAND() - 0.5);

  sample_media(light_list, global_light_list_cnt, Ray, Media_List, Interval, light_ray, 
               jdist, C2, od2, 3/* sample method*/, local_light_list, local_light_list_cnt);

  /* if we're at max depth, then let's just use this last sample and average
     it with the two end points */
  if (depth<=0)
  {
    /* average colors */
    Result[0]=(C1[0]+C3[0]+C2[0])/3.;
    Result[1]=(C1[1]+C3[1]+C2[1])/3.;
    Result[2]=(C1[2]+C3[2]+C2[2])/3.;

    /* average the optical depth */
    ODResult[0]=(od1[0]+od3[0]+od2[0])/3.;
    ODResult[1]=(od1[1]+od3[1]+od2[1])/3.;
    ODResult[2]=(od1[2]+od3[2]+od2[2])/3.;

    /* bail out - we're done now */
    return;
  }

  /* check if we should sample between points 1 and 2 */
  if ( Colour_Distance(C1, C2) > aa_threshold )
  {
    /* recurse again */
    sample_media_rec(light_list, global_light_list_cnt, Ray, Media_List, Interval, light_ray, 
                     d1, d2,
                     Result2, C1, C2,
                     ODResult2, od1, od2,
                     depth-1, Jitter, aa_threshold,
                     local_light_list, local_light_list_cnt);

    /* average colors */
    Result[0]=Result2[0]*0.5;
    Result[1]=Result2[1]*0.5;
    Result[2]=Result2[2]*0.5;

    /* average the optical depth */
    ODResult[0]=ODResult2[0]*0.5;
    ODResult[1]=ODResult2[1]*0.5;
    ODResult[2]=ODResult2[2]*0.5;
  }
  else
  {
    /* no new points needed - just average what we've got.
       use c1 weight = 1/3, c2 weight = 1/6 (we use c2, the middle point, again)
    */
    /* average colors */
    Result[0]=C1[0]/3.0 + C2[0]/6.0;
    Result[1]=C1[1]/3.0 + C2[1]/6.0;
    Result[2]=C1[2]/3.0 + C2[2]/6.0;

    /* average the optical depth */
    ODResult[0]=od1[0]/3.0 + od2[0]/6.0;
    ODResult[1]=od1[1]/3.0 + od2[1]/6.0;
    ODResult[2]=od1[2]/3.0 + od2[2]/6.0;
  }

  /* check if we should sample between points 2 and 3 */
  if ( Colour_Distance(C2, C3) > aa_threshold )
  {
    /* recurse again */
    sample_media_rec(light_list, global_light_list_cnt, Ray, Media_List, Interval, light_ray,
                     d2, d3,
                     Result2, C2, C3,
                     ODResult2, od2, od3,
                     depth-1, Jitter, aa_threshold,
                     local_light_list, local_light_list_cnt);

    /* average colors */
    Result[0]+=Result2[0]*0.5;
    Result[1]+=Result2[1]*0.5;
    Result[2]+=Result2[2]*0.5;

    /* average the optical depth */
    ODResult[0]+=ODResult2[0]*0.5;
    ODResult[1]+=ODResult2[1]*0.5;
    ODResult[2]+=ODResult2[2]*0.5;
  }
  else
  {
    /* no new points needed - just average what we've got.
       use c1 weight = 1/3, c2 weight = 1/6 (we already used c2 once)
    */
    /* average colors */
    Result[0]+=C3[0]/3.0 + C2[0]/6.0;
    Result[1]+=C3[1]/3.0 + C2[1]/6.0;
    Result[2]+=C3[2]/3.0 + C2[2]/6.0;

    /* average the optical depth */
    ODResult[0]+=od3[0]/3.0 + od2[0]/6.0;
    ODResult[1]+=od3[1]/3.0 + od2[1]/6.0;
    ODResult[2]+=od3[2]/3.0 + od2[2]/6.0;
  }
  


}


/*****************************************************************************
*
* FUNCTION
*
*   sample_media
*
* INPUT
*
*   dist  - distance of current sample
*   Ray   - pointer to ray
*   IMedia - pointer to media to use
*
* OUTPUT
*
*   Col          - color of current sample
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Calculate the color of the current media sample.
*
* CHANGES
*
*   Nov 1994 : Creation.
*
******************************************************************************/

static void sample_media(LIGHT_LIST *light_list, int global_light_list_cnt, RAY *Ray, IMEDIA **Media_List, MEDIA_INTERVAL *Interval, int light_ray, DBL d0,
                         COLOUR SampCol, COLOUR SampOptDepth, int sample_method, LIGHT_LIST *local_light_list, int local_light_list_cnt)
{
  int i, n, use_scattering;
  /* NK samples - moved d0 to parameter list */
  DBL /*d0,*/ d1, len;
  VECTOR P, H;
  COLOUR C0, Light_Colour, Te;
  COLOUR Em, Ex, Sc;
  RAY Light_Ray;
  IMEDIA **Tmp, *Local;
  /* NK phmap */
  int ignore_photons;

  Increase_Counter(stats[Media_Samples]);

  /* Set up sampling location. */

  /*
  d0 = Interval->ds * FRAND();
  */

  d0 *= Interval->ds;

  d1 = Interval->s0 + d0;

  VEvaluateRay(H, Ray->Initial, d1, Ray->Direction);

  /* Get coefficients in current sample location. */

  Make_Colour(Em, 0.0, 0.0, 0.0);
  Make_Colour(Ex, 0.0, 0.0, 0.0);
  Make_Colour(Sc, 0.0, 0.0, 0.0);

  use_scattering = false;

  for (Tmp = Media_List; (*Tmp) != NULL; Tmp++)
  {
    for (Local = *Tmp; Local != NULL; Local = Local->Next_Media)
    {
      Assign_Vector(P, H);

      evaluate_density_pattern(Local, P, C0);

      Ex[0] += C0[0] * Local->Extinction[0];
      Ex[1] += C0[1] * Local->Extinction[1];
      Ex[2] += C0[2] * Local->Extinction[2];

      if (!light_ray)
      {
        Em[0] += C0[0] * Local->Emission[0];
        Em[1] += C0[1] * Local->Emission[1];
        Em[2] += C0[2] * Local->Emission[2];

        Sc[0] += C0[0] * Local->Scattering[0];
        Sc[1] += C0[1] * Local->Scattering[1];
        Sc[2] += C0[2] * Local->Scattering[2];
      }

      use_scattering |= Local->use_scattering;
    }
  }

  /* Get estimate for the total optical depth of the current interval. */

  SampOptDepth[0] = Ex[0] * Interval->ds;
  SampOptDepth[1] = Ex[1] * Interval->ds;
  SampOptDepth[2] = Ex[2] * Interval->ds;

  if(sample_method!=3)
  {
    Interval->od[0] += SampOptDepth[0];
    Interval->od[1] += SampOptDepth[1];
    Interval->od[2] += SampOptDepth[2];
  }


  /* Get estimate for the total emission of the current interval. */

  Te[0] = Em[0];
  Te[1] = Em[1];
  Te[2] = Em[2];

  if ((!light_ray) && (use_scattering) && (Interval->lit))
  {
    /* set up flags for photons */
    if(!backtraceFlag)
    {
    photonOptions.objectFlags = PH_RFR_ON_FLAG | PH_RFL_ON_FLAG;

    ignore_photons = true;
    for (n = 0, Tmp = Media_List; (*Tmp) != NULL; n++, Tmp++)
      for (Local = *Tmp; Local != NULL; Local = Local->Next_Media)
        if (!Local->ignore_photons)
        {
          ignore_photons = false;
          break;
        }
    if (ignore_photons)
      photonOptions.objectFlags |= PH_IGNORE_PHOTONS_FLAG;

    photonOptions.photonObject = NULL;
    }

    /* Process all light sources. */

    for (i = 0; i < global_light_list_cnt; i++)
    {
      /* Use light only if active and within it's boundaries. */
      if (((light_list[i].active) && (d1 >= light_list[i].s0) && (d1 <= light_list[i].s1)))
      {
        photonOptions.lightFlags = light_list[i].Light->Flags;
        if (!(Test_Shadow(light_list[i].Light, &len, &Light_Ray, Ray, P, Light_Colour)))
        {
           scattering_attenuation(Media_List, Te, Sc, Light_Colour, Ray, &Light_Ray);
        }
      }
    }

    for (i = 0; i < local_light_list_cnt; i++)
    {
      /* Use light only if active and within it's boundaries. */
      if (((local_light_list[i].active) && (d1 >= local_light_list[i].s0) && (d1 <= local_light_list[i].s1)))
      {
        photonOptions.lightFlags = local_light_list[i].Light->Flags;
        if (!(Test_Shadow(local_light_list[i].Light, &len, &Light_Ray, Ray, P, Light_Colour)))
        {
           scattering_attenuation(Media_List, Te, Sc, Light_Colour, Ray, &Light_Ray);
        }
      }
    }
  }

#if MEDIA_INTERACTION
  /* use all intervals for photons */
  if ((!light_ray) && (use_scattering))
  {
    if (photonOptions.mediaPhotonMap.numPhotons>0)
    {
      /* Process photons. */
      int n,tempn,j,step=0;
      int minGatherCount;
      DBL Size,r=0,tempr=0.0;
      DBL thisDensity=0;
      DBL prevDensity=0.0000000000000001; /* avoid div-by-zero error */
      int expanded=false;
      COLOUR Colour2, TempCol;

      Size = photonOptions.mediaPhotonMap.minGatherRad;
      /* size /= 2; */
      Make_Colour(Colour2,0,0,0);
      n=-1;
      step=0;
      minGatherCount=photonOptions.minGatherCount;
      while(n<minGatherCount && step<photonOptions.mediaPhotonMap.gatherNumSteps)
      {
        Make_Colour(TempCol,0,0,0);
        tempr = 0;

        /* gather the photons */
        tempn=gatherPhotons(H, Size, &tempr,NULL,false,&photonOptions.mediaPhotonMap);

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
      
          Light_Ray.Direction[Y] = photonOptions.sinTheta[theta];
          Light_Ray.Direction[X] = photonOptions.cosTheta[theta];

          Light_Ray.Direction[Z] = Light_Ray.Direction[X]*photonOptions.sinTheta[phi];
          Light_Ray.Direction[X] = Light_Ray.Direction[X]*photonOptions.cosTheta[phi];

          VSub(Light_Ray.Initial, photonOptions.photonGatherList[j]->Loc, Light_Ray.Direction);

          scattering_attenuation(Media_List, TempCol, Sc, Light_Colour, Ray, &Light_Ray);
        }
        /* density of this search */
        thisDensity = tempn / (tempr*tempr*tempr*4.0/3.0);

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

        Size+=photonOptions.mediaPhotonMap.gatherRadStep;
        step++;
        /* force one step only for now */
        step = photonOptions.mediaPhotonMap.gatherNumSteps;
      }
      /* finish the photons equation */
      VScaleEq(Colour2, (DBL)(3.0)/(M_PI*r*r*r*4.0));
      
      Te[0] += Colour2[0];
      Te[1] += Colour2[1];
      Te[2] += Colour2[2];
    }
#endif
  }

  if (sample_method==3)
  {
    // We're doing the samples in order, so we can attenuate correctly
    // instead of assuming a constant absorption/extinction.
    // Therefore, we do the attenuation later (back up in Simulate_Media).
    Te[0] *= Interval->ds;
    Te[1] *= Interval->ds;
    Te[2] *= Interval->ds;
  }
  else
  {
    // NOTE: this assumes constant absorption+extinction over the length of the interval
    Te[0] *= Interval->ds * exp(-Ex[0] * d0);
    Te[1] *= Interval->ds * exp(-Ex[1] * d0);
    Te[2] *= Interval->ds * exp(-Ex[2] * d0);
  }
  

  SampCol[0] = Te[0];
  SampCol[1] = Te[1];
  SampCol[2] = Te[2];

  if(sample_method!=3)
  {
    /* Add emission. */

    Interval->te[0] += Te[0];
    Interval->te[1] += Te[1];
    Interval->te[2] += Te[2];

    Interval->te2[0] += Sqr(Te[0]);
    Interval->te2[1] += Sqr(Te[1]);
    Interval->te2[2] += Sqr(Te[2]);
  }

  Interval->samples++;
}

/*****************************************************************************
*
* FUNCTION
*
*   scattering_attenuation
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
*   Get attenuation due to scattering.
*
* CHANGES
*
*   Dec 1996 : Creation.
*
******************************************************************************/

static void scattering_attenuation(IMEDIA **Media_List, COLOUR OutputColor, COLOUR Sc, COLOUR Light_Colour, RAY *Ray, RAY *Light_Ray)
{
  IMEDIA **Tmp = NULL, *Local = NULL;
  DBL k = 0.0, g = 0.0, g2 = 0.0, alpha = 0.0;
  int n;

  for (n = 0, Tmp = Media_List; (*Tmp) != NULL; n++, Tmp++)
  {
    for (Local = *Tmp; Local != NULL; Local = Local->Next_Media)
    {
      switch (Local->Type)
      {
        case RAYLEIGH_SCATTERING:

          VDot(alpha, Light_Ray->Direction, Ray->Direction);

          k += 0.799372013 * (1.0 + Sqr(alpha));

          break;

        case MIE_HAZY_SCATTERING:

          VDot(alpha, Light_Ray->Direction, Ray->Direction);

          k += 0.576655375 * (1.0 + 9.0 * pow(0.5 * (1.0 + alpha), 8.0));

          break;

        case MIE_MURKY_SCATTERING:

          VDot(alpha, Light_Ray->Direction, Ray->Direction);

          k += 0.495714547 * (1.0 + 50.0 * pow(0.5 * (1.0 + alpha), 32.0));

          break;

        case HENYEY_GREENSTEIN_SCATTERING:

          VDot(alpha, Light_Ray->Direction, Ray->Direction);

          g = Local->Eccentricity;

          g2 = Sqr(g);

          k += (1.0 - g2) / pow(1.0 + g2 - 2.0 * g * alpha, 1.5);

          break;

        case ISOTROPIC_SCATTERING:
        default:

          k += 1.0;

        break;
      }
    }
  }

  k /= (DBL)n;

  OutputColor[0] += k * Sc[0] * Light_Colour[0];
  OutputColor[1] += k * Sc[1] * Light_Colour[1];
  OutputColor[2] += k * Sc[2] * Light_Colour[2];
}    

/*****************************************************************************
*
* FUNCTION
*
*   evaluate_density_pattern
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
* CHANGES
*
*   Dec 1996 : Creation.
*
******************************************************************************/

static void evaluate_density_pattern(IMEDIA *IMedia, VECTOR P, COLOUR C)
{
  COLOUR Local_Color;
  PIGMENT *Temp=IMedia->Density;
  
  Make_Colour (C, 1.0, 1.0, 1.0);

  while (Temp != NULL)
  {
    Make_Colour (Local_Color, 0.0, 0.0, 0.0);
    
    Compute_Pigment (Local_Color, Temp, P, NULL);

    C[0] *= Local_Color[0];
    C[1] *= Local_Color[1];
    C[2] *= Local_Color[2];

    Temp=(PIGMENT *)Temp->Next;
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   get_light_list
*
* INPUT
*
*   Ray        - pointer to ray
*   IMedia     - pointer to media to use
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
* CHANGES
*
*   Nov 1994 : Created
*   Mar 2002 : Updated to support light groups [trf]
*
******************************************************************************/

static void get_light_list (LIGHT_LIST *light_list, LIGHT_LIST *&local_light_list, int& Local_Light_Count, RAY *Ray, INTERSECTION *Inter)
{
  LIGHT_SOURCE *Light = NULL;
  int i;

  for(i = 0, Light = Frame.Light_Sources; Light != NULL; Light = Light->Next_Light_Source, i++)
  {
    light_list[i].Light = Light;
    update_light_list_entry(&light_list[i], Ray, Inter);
  }

  // list of local light source media intervals (if any)
  if(Inter->Object != NULL)
  {
    if(Inter->Object->LLights != NULL)
    {
      // count number of local lights
      for(Local_Light_Count = 0, Light = Inter->Object->LLights;
          Light != NULL;
          Light = Light->Next_Light_Source)
      {
        Local_Light_Count++;
      }

      local_light_list = (LIGHT_LIST *)POV_MALLOC(sizeof(LIGHT_LIST) * Local_Light_Count, "local media light list");

      for(i = 0, Light = Inter->Object->LLights; Light != NULL; Light = Light->Next_Light_Source, i++)
      {
        local_light_list[i].Light = Light;
        update_light_list_entry(&local_light_list[i], Ray, Inter);
      }
    }
  }
}

void update_light_list_entry(LIGHT_LIST *light, RAY *Ray, INTERSECTION *Inter)
{
    DBL t1 = 0.0, t2 = 0.0;
    bool insert = false;

    /* Init interval. */
    light->active = false;
    light->s0     = 0.0;
    light->s1     = Max_Distance;

    if (!light->Light->Media_Interaction)
      return;

    switch (light->Light->Light_Type)
    {
      case CYLINDER_SOURCE:
        if (intersect_cylinderlight(Ray, light->Light, &t1, &t2))
            insert = ((t1 < Inter->Depth) && (t2 > Small_Tolerance));
        break;
      case POINT_SOURCE:
        t1 = 0.0;
        t2 = Inter->Depth;
        insert = true;
        break;
      case SPOT_SOURCE:
        if (intersect_spotlight(Ray, light->Light, &t1, &t2))
            insert = ((t1 < Inter->Depth) && (t2 > Small_Tolerance));
        break;
    }

    /* Insert distances into sampling interval list. */
//    if (insert)
    {
      /* Insert light source intersections into light list. */

      t1 = max(t1, 0.0);
      t2 = min(t2, Inter->Depth);

      light->active = true;
      light->s0 = t1;
      light->s1 = t2;
    }
}

/*****************************************************************************
*
* FUNCTION
*
*   get_lit_interval
*
* INPUT
*
*   light_list - array containing light source information
*   dist       - distance of current sample
*   Ray        - pointer to ray
*   IMedia     - pointer to media to use
*
* OUTPUT
*
*   Col        - color of current sample
*
* RETURNS
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
* CHANGES
*
*   Nov 1994 : Creation.
*
******************************************************************************/

static void get_lit_interval(int *number, LIT_INTERVAL *Lit_Interval, int entries, LIGHT_LIST *light_list,
                             INTERSECTION *Inter, LIGHT_LIST *local_light_list, int local_light_list_cnt)
{
  int a, i, n;
  LIT_INTERVAL *curr, *prev;

  if(entries + local_light_list_cnt > Frame.Number_Of_Light_Sources)
  {
    s0 = (DBL *)POV_REALLOC(s0, (entries + local_light_list_cnt)*sizeof(DBL), "temp data");
    s1 = (DBL *)POV_REALLOC(s1, (entries + local_light_list_cnt)*sizeof(DBL), "temp data");
  }

  for (i = a = 0; i < entries; i++)
  {
    if (light_list[i].active)
    {
      s0[a] = light_list[i].s0;
      s1[a] = light_list[i].s1;

      a++;
    }
  }

  for (i = 0; i < local_light_list_cnt; i++)
  {
    if (local_light_list[i].active)
    {
      s0[a] = local_light_list[i].s0;
      s1[a] = local_light_list[i].s1;

      a++;
    }
  }

  n = 0;

  curr = &Lit_Interval[0];

  if (a)
  {
    QSORT((void *)(&s0[0]), (unsigned long)a, sizeof(DBL), compdoubles);
    QSORT((void *)(&s1[0]), (unsigned long)a, sizeof(DBL), compdoubles);

    if (s0[0] > 0.0)
    {
      curr->lit = false;

      curr->s0 = 0.0;
      curr->s1 = s0[0];

      curr++;

      n++;
    }

    curr->lit = true;

    curr->s0 = s0[0];
    curr->s1 = s1[0];

    prev = curr;

    curr++;

    n++;

    for (i = 1; i < a; i++)
    {
      if (s0[i] > prev->s1)
      {
        curr->lit = false;

        curr->s0 = prev->s1;
        curr->s1 = s0[i];

        prev++;
        curr++;

        n++;

        curr->lit = true;

        curr->s0 = s0[i];
        curr->s1 = s1[i];

        prev++;
        curr++;

        n++;
      }
      else
      {
        if (s1[i] > prev->s1)
        {
          prev->s1 = s1[i];
        }
      }
    }

    if (prev->s1 < Inter->Depth)
    {
      curr->lit = false;

      curr->s0 = prev->s1;
      curr->s1 = Inter->Depth;

      curr++;

      n++;
    }
  }
  else
  {
    curr->lit = false;

    curr->s0 = 0.0;
    curr->s1 = Inter->Depth;

    curr++;

    n++;
  }

  curr = &Lit_Interval[0];

  for (i = 0; i < n; i++)
  {
    curr->ds = curr->s1 - curr->s0;

    curr++;
  }

  *number = n;
}



/*****************************************************************************
*
* FUNCTION
*
*   set_up_sampling_intervals
*
* INPUT
*
*   interval - array containing media intervals
*   number   - number of lit intervals
*   list     - array of lit intervals
*   media    - media to use
*
* OUTPUT
*
*   interval - array containing media intervals
*
* RETURNS
*
*   int      - number of media intervals created
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Distribute samples along an interval according to the maximum
*   number of samples and the ratio of samples in lit and unlit
*   areas as given by the participating media.
*
* CHANGES
*
*   Nov 1994 : Creation.
*
******************************************************************************/

static int set_up_sampling_intervals(MEDIA_INTERVAL *interval, int number, LIT_INTERVAL *list, IMEDIA *media)
{
  int i, j, n, r, remaining, intervals;
  DBL delta, sum, weight;
  MEDIA_INTERVAL *curr;
  LIT_INTERVAL *entry;

  /* Set up sampling intervals. */

  /* NK samples - we will always have enough intervals 
     we always use the larger of the two numbers */
  if (media->Intervals < number)
    intervals = number;
  else
    intervals = media->Intervals;

  /* Use one interval if no lit intervals and constant media. */

  if ((number == 0) && (media->is_constant))
  {
    intervals = 1;

    delta = list[0].ds;

    curr = interval;

    curr->lit = true;

    curr->samples = 0;

    curr->s0 = list[0].s0;
    curr->s1 = list[0].s0 + delta;
    curr->ds = delta;

    Make_Colour(curr->od,  0.0, 0.0, 0.0);
    Make_Colour(curr->te,  0.0, 0.0, 0.0);
    Make_Colour(curr->te2, 0.0, 0.0, 0.0);

    return(intervals);
  }

  /* Choose intervals. */

  if (number == 1)
  {
    /* Use uniform intervals. */

    delta = list[0].ds / (DBL)intervals;

    curr = interval;

    for (i = 0; i < intervals; i++)
    {
      curr->lit = true;

      curr->samples = 0;

      curr->s0 = list[0].s0 + delta * (DBL)i;
      curr->s1 = list[0].s0 + delta * (DBL)(i + 1);
      curr->ds = delta;

      Make_Colour(curr->od,  0.0, 0.0, 0.0);
      Make_Colour(curr->te,  0.0, 0.0, 0.0);
      Make_Colour(curr->te2, 0.0, 0.0, 0.0);

      curr++;
    }
  }
  else
  {
    /* Choose intervals according to the specified ratio. */

    if (number > intervals)
    {
      Error("Too few sampling intervals.");
    }

    sum = 0.0;

    entry = list;

    for (i = 0; i < number; i++)
    {
/*
      sum += ((entry->lit) ? (0.9) : (0.1)) * entry->ds;
*/
      sum += ((entry->lit) ? (media->Ratio) : (1.0 - media->Ratio));

      entry++;
    }

    remaining = intervals;

    curr = interval;

    entry = list;

    for (i = 0; i < number; i++)
    {
/*
      weight = ((entry->lit) ? (0.9) : (0.1)) * entry->ds;
*/
      weight = ((entry->lit) ? (media->Ratio) : (1.0 - media->Ratio));

      n = (int)(weight / sum * (DBL)intervals) + 1;

      r = remaining - number + i + 1;

      if (n > r)
      {
        n = r;
      }

      delta = entry->ds / (DBL)n;

      for (j = 0; j < n; j++)
      {
        curr->lit = entry->lit;

        curr->samples = 0;

        curr->s0 = entry->s0 + delta * (DBL)j;
        curr->s1 = entry->s0 + delta * (DBL)(j + 1);
        curr->ds = delta;

        Make_Colour(curr->od,  0.0, 0.0, 0.0);
        Make_Colour(curr->te,  0.0, 0.0, 0.0);
        Make_Colour(curr->te2, 0.0, 0.0, 0.0);

        curr++;
      }

      remaining -= n;

      entry++;
    }
  }

  return(intervals);
}



/*****************************************************************************
*
* FUNCTION
*
*   intersect_spotlight
*
* INPUT
*
*   Ray    - current ray
*   Light  - current light source
*
* OUTPUT
*
*   d1, d2 - intersection depths
*
* RETURNS
*
*   int - true, if hit
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Intersect a ray with the light cone of a spotlight.
*
* CHANGES
*
*   Nov 1994 : Creation.
*
******************************************************************************/

static int intersect_spotlight(RAY *Ray, LIGHT_SOURCE *Light, DBL *d1, DBL  *d2)
{
  int viewpoint_is_in_cone;
  DBL a, b, c, d, m, l, l1, l2, t, t1, t2, k1, k2, k3, k4;
  VECTOR V1;

  /* Get cone's slope. Note that cos(falloff) is stored in Falloff! */

  m = 1 / (Light->Falloff * Light->Falloff);

  VSub(V1, Ray->Initial, Light->Center);

  VDot(k1, Ray->Direction, Light->Direction);

  VDot(k2, V1, Light->Direction);

  VLength(l, V1);

  if (l > EPSILON)
  {
    viewpoint_is_in_cone = (k2 / l >= Light->Falloff);
  }
  else
  {
    viewpoint_is_in_cone = false;
  }

  if ((k1 <= 0.0) && (k2 < 0.0))
  {
    return (false);
  }

  VDot(k3, V1, Ray->Direction);

  VDot(k4, V1, V1);

  a = 1.0 - Sqr(k1) * m;

  b = k3 - k1 * k2 * m;

  c = k4 - Sqr(k2) * m;

  if (a != 0.0)
  {
    d = Sqr(b) - a * c;

    if (d > EPSILON)
    {
      d = sqrt(d);

      t1 = (-b + d) / a;
      t2 = (-b - d) / a;

      if (t1 > t2)
      {
        t = t1; t1 = t2; t2 = t;
      }

      l1 = k2 + t1 * k1;
      l2 = k2 + t2 * k1;

      if ((l1 <= 0.0) && (l2 <= 0.0))
      {
        return (false);
      }

      if ((l1 <= 0.0) || (l2 <= 0.0))
      {
        if (l1 <= 0.0)
        {
          if (viewpoint_is_in_cone)
          {
            t1 = 0.0;
            t2 = (t2 > 0.0) ? (t2) : (Max_Distance);
          }
          else
          {
            t1 = t2;
            t2 = Max_Distance;
          }
        }
        else
        {
          if (viewpoint_is_in_cone)
          {
            t2 = t1;
            t1 = 0.0;
          }
          else
          {
            t2 = Max_Distance;
          }
        }
      }

      *d1 = t1;
      *d2 = t2;

      return (true);
    }
    else
    {
      if (d > -EPSILON)
      {
        if (viewpoint_is_in_cone)
        {
          *d1 = 0.0;
          *d2 = -b / a;
        }
        else
        {
          *d1 = -b / a;
          *d2 = Max_Distance;
        }

        return(true);
      }
    }
  }
  else
  {
    if (viewpoint_is_in_cone)
    {
      *d1 = 0.0;
      *d2 = -c/b;

      return(true);
    }
  }

  return (false);
}



/*****************************************************************************
*
* FUNCTION
*
*   intersect_cylinderlight
*
* INPUT
*
*   Ray    - current ray
*   Light  - current light source
*
* OUTPUT
*
*   d1, d2 - intersection depths
*
* RETURNS
*
*   int - true, if hit
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Intersect a ray with the light cylinder of a cylinderlight.
*
* CHANGES
*
*   Jan 1995 : Creation.
*
******************************************************************************/

static int intersect_cylinderlight(RAY *Ray, LIGHT_SOURCE *Light, DBL *d1, DBL  *d2)
{
  DBL a, b, c, d, l1, l2, t, t1, t2, k1, k2, k3, k4;
  VECTOR V1;

  VSub(V1, Ray->Initial, Light->Center);

  VDot(k1, Ray->Direction, Light->Direction);

  VDot(k2, V1, Light->Direction);

  if ((k1 <= 0.0) && (k2 < 0.0))
  {
    return (false);
  }

  a = 1.0 - Sqr(k1);

  if (a != 0.0)
  {
    VDot(k3, V1, Ray->Direction);

    VDot(k4, V1, V1);

    b = k3 - k1 * k2;

    c = k4 - Sqr(k2) - Sqr(Light->Falloff);

    d = Sqr(b) - a * c;

    if (d > EPSILON)
    {
      d = sqrt(d);

      t1 = (-b + d) / a;
      t2 = (-b - d) / a;

      if (t1 > t2)
      {
        t = t1; t1 = t2; t2 = t;
      }

      l1 = k2 + t1 * k1;
      l2 = k2 + t2 * k1;

      if ((l1 <= 0.0) && (l2 <= 0.0))
      {
        return (false);
      }

      if ((l1 <= 0.0) || (l2 <= 0.0))
      {
        if (l1 <= 0.0)
        {
          t1 = 0.0;
        }
        else
        {
          t2 = (Max_Distance - k2) / k1;
        }
      }

      *d1 = t1;
      *d2 = t2;

      return (true);
    }
  }

  return (false);
}



/*****************************************************************************
*
* FUNCTION
*
*   Post_Media
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
*   Initialize media.
*
* CHANGES
*
*   Dec 1996 : Creation.
*
******************************************************************************/

void Post_Media(IMEDIA *IMedia)
{
  int i;
  DBL t;

  if (IMedia == NULL)
  {
    return;
  }
  
  /* Get extinction coefficient. */

  CLinComb2(IMedia->Extinction, 1.0, IMedia->Absorption, IMedia->sc_ext, IMedia->Scattering);

  /* Determine used effects. */

  IMedia->use_absorption = (IMedia->Absorption[0] != 0.0) ||
                          (IMedia->Absorption[1] != 0.0) ||
                          (IMedia->Absorption[2] != 0.0);

  IMedia->use_emission = (IMedia->Emission[0] != 0.0) ||
                        (IMedia->Emission[1] != 0.0) ||
                        (IMedia->Emission[2] != 0.0);

  IMedia->use_scattering = (IMedia->Scattering[0] != 0.0) ||
                          (IMedia->Scattering[1] != 0.0) ||
                          (IMedia->Scattering[2] != 0.0);

  IMedia->use_extinction = IMedia->use_absorption || IMedia->use_scattering;

  IMedia->is_constant = (IMedia->Density == NULL);

  /* Init sample threshold array. */

  if (IMedia->Sample_Threshold != NULL)
  {
    POV_FREE(IMedia->Sample_Threshold);
  }

  /* Create list of thresholds for confidence test. */

  IMedia->Sample_Threshold = (DBL *)POV_MALLOC(IMedia->Max_Samples*sizeof(DBL), "sample threshold list");

  if (IMedia->Max_Samples > 1)
  {
    t = chdtri((DBL)(IMedia->Max_Samples-1), IMedia->Confidence);

    if (t > 0.0)
    {
      t = IMedia->Variance / t;
    }
    else
    {
      t = IMedia->Variance * EPSILON;
    }

    for (i = 0; i < IMedia->Max_Samples; i++)
    {
      IMedia->Sample_Threshold[i] = t * chdtri((DBL)(i+1), IMedia->Confidence);

/*
      fprintf(stderr, "threshold for n = %3d: %f\n", i+1, IMedia->Sample_Threshold[i]);
*/
    }
  }
  else
  {
    IMedia->Sample_Threshold[0] = 0.0;
  }

  if (IMedia->Density != NULL)
  {
    Post_Pigment(IMedia->Density);
  }

  Post_Media(IMedia->Next_Media);  
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_Media
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
*   IMEDIA * - created media
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Create a media.
*
* CHANGES
*
*   Dec 1994 : Creation.
*
******************************************************************************/

IMEDIA *Create_Media()
{
  IMEDIA *New;

  New = (IMEDIA *)POV_MALLOC(sizeof(IMEDIA), "media");

  New->Type = ISOTROPIC_SCATTERING;

  New->Intervals      = 10;
  New->Min_Samples    = 1;
  New->Max_Samples    = 1;
  New->Eccentricity   = 0.0;

  Make_Colour(New->Absorption, 0.0, 0.0, 0.0);
  Make_Colour(New->Emission,   0.0, 0.0, 0.0);
  Make_Colour(New->Extinction, 0.0, 0.0, 0.0);
  Make_Colour(New->Scattering, 0.0, 0.0, 0.0);

  New->use_absorption = false;
  New->use_emission   = false;
  New->use_extinction = false;
  New->use_scattering = false;

  New->is_constant = false;

  New->sc_ext     = 1.0;
  New->Ratio      = 0.9;
  New->Confidence = 0.9;
  New->Variance   = 1.0 / 128.0;

  New->Sample_Threshold = NULL;

  New->Density = NULL;

  New->Next_Media = NULL;

  New->Sample_Method = 1;
  New->AA_Threshold = 0.1;
  New->AA_Level = 3;
  New->Jitter = 0.0;

  New->ignore_photons = false;

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Media
*
* INPUT
*
*   Old - media to copy
*
* OUTPUT
*
* RETURNS
*
*   IMEDIA * - new media
*
* AUTHOR
*
*   Dieter Bayer
*
* DESCRIPTION
*
*   Copy an media.
*
* CHANGES
*
*   Dec 1994 : Creation.
*
******************************************************************************/

IMEDIA *Copy_Media(IMEDIA *Old)
{
  int i;
  IMEDIA *New, *First, *Previous, *Local_Media;

  Previous = First = NULL;

  if (Old != NULL)
  {
    for (Local_Media = Old; Local_Media != NULL; Local_Media = Local_Media->Next_Media)
    {
      New = Create_Media();

      *New = *Local_Media;

      if (Local_Media->Sample_Threshold != NULL)
      {
        if (New->Intervals > 0)
        {
          New->Sample_Threshold = (DBL *)POV_MALLOC(New->Intervals*sizeof(DBL), "sample threshold list");

          for (i = 0; i < New->Intervals; i++)
          {
            New->Sample_Threshold[i] =  Local_Media->Sample_Threshold[i];
          }
        }
      }

      New->Density = Copy_Pigment(Local_Media->Density);

      if (First == NULL)
      {
        First = New;
      }

      if (Previous != NULL)
      {
        Previous->Next_Media = New;
      }

      Previous = New;
    }
  }

  return(First);
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Media
*
* INPUT
*
*   IMedia - media to destroy
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
*   Destroy an media.
*
* CHANGES
*
*   Dec 1994 : Creation.
*
******************************************************************************/

void Destroy_Media(IMEDIA *IMedia)
{
  IMEDIA *Local_Media, *Temp;

  if (IMedia != NULL)
  {
    Local_Media = IMedia;

    while (Local_Media != NULL)
    {
      if (Local_Media->Sample_Threshold != NULL)
      {
        POV_FREE(Local_Media->Sample_Threshold);
      }

      /* Note Destroy_Pigment also handles Density->Next */
      Destroy_Pigment(Local_Media->Density);

      Temp = Local_Media->Next_Media;

      POV_FREE(Local_Media);

      Local_Media = Temp;
    }
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   compdoubles
*
* INPUT
*
*   in_a, in_b - Elements to compare
*
* OUTPUT
*
* RETURNS
*
*   int - result of comparison
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
*   Dec 1996 : Creation.
*
******************************************************************************/

static int CDECL compdoubles(const void *in_a, const void *in_b)
{
  DBL *a, *b;

  a = (DBL *)in_a;
  b = (DBL *)in_b;

  if (*a < *b)
  {
    return (-1);
  }
  else
  {
    if (*a == *b)
    {
      return (0);
    }
    else
    {
      return (1);
    }
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Transform_Media
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
*   Transform media.
*
* CHANGES
*
*   Dec 1996 : Creation.
*
******************************************************************************/

void Transform_Media(IMEDIA *IMedia, TRANSFORM *Trans)
{
  IMEDIA *Temp;

  if (IMedia != NULL)
  {
    for (Temp = IMedia; Temp != NULL; Temp = Temp->Next_Media)
    {
      Transform_Density(Temp->Density, Trans);
    }
  }
}

void Transform_Density(PIGMENT *Density, TRANSFORM *Trans)
{
  TPATTERN *Temp = (TPATTERN *)Density;
  
  while (Temp != NULL)
  {
    Transform_Tpattern(Temp, Trans);
    Temp = Temp->Next;
  }
}

END_POV_NAMESPACE
