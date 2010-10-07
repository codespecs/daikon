/****************************************************************************
 *               normal.cpp
 *
 * This module implements solid texturing functions that perturb the surface
 * normal to create a bumpy effect. 
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
 * $File: //depot/povray/3.6-release/source/normal.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

/*
 * Some texture ideas garnered from SIGGRAPH '85 Volume 19 Number 3,
 * "An Image Synthesizer" By Ken Perlin.
 *
 * Further Ideas Garnered from "The RenderMan Companion" (Addison Wesley)
 */

#include "frame.h"
#include "vector.h"
#include "texture.h"
#include "image.h"
#include "matrices.h"
#include "normal.h"
#include "objects.h"
#include "povray.h"
#include "txttest.h"
#include "pattern.h"
#include "pigment.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/



/*****************************************************************************
* Local typedefs
******************************************************************************/



/*****************************************************************************
* Local constants
******************************************************************************/

static const
VECTOR Pyramid_Vect [4]= {{ 0.942809041,-0.333333333, 0.0},
                          {-0.471404521,-0.333333333, 0.816496581},
                          {-0.471404521,-0.333333333,-0.816496581},
                          { 0.0        , 1.0        , 0.0}};


/*****************************************************************************
* Static functions
******************************************************************************/

static void ripples (VECTOR EPoint, TNORMAL *Tnormal, VECTOR Vector);
static void waves (VECTOR EPoint, TNORMAL *Tnormal, VECTOR Vector);
static void bumps (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal);
static void dents (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal);
static void wrinkles (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal);
static void quilted (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal);
static DBL Hermite_Cubic (DBL T1,UV_VECT UV1,UV_VECT UV2);
static DBL Do_Slope_Map (DBL value, BLEND_MAP *Blend_Map);
static void Do_Average_Normals (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal, INTERSECTION *Inter);
static void facets (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal);


/*****************************************************************************
*
* FUNCTION
*
*   ripples
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
* CHANGES
*
******************************************************************************/

static void ripples (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal)
{
  register unsigned int i;
  register DBL length, scalar, index;
  VECTOR point;

  for (i = 0 ; i < Number_Of_Waves ; i++)
  {
    VSub (point, EPoint, Wave_Sources[i]);
    VLength (length, point);

    if (length == 0.0)
      length = 1.0;

    index = length * Tnormal->Frequency + Tnormal->Phase;

    scalar = cycloidal(index) * Tnormal ->Amount;

    VAddScaledEq(normal, scalar / (length * (DBL)Number_Of_Waves), point);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   waves
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
* CHANGES
*
******************************************************************************/

static void waves (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal)
{
  register unsigned int i;
  register DBL length, scalar, index, sinValue ;
  VECTOR point;

  for (i = 0 ; i < Number_Of_Waves ; i++)
  {
    VSub (point, EPoint, Wave_Sources[i]);

    VLength (length, point);

    if (length == 0.0)
    {
      length = 1.0;
    }

    index = length * Tnormal->Frequency * frequency[i] + Tnormal->Phase;

    sinValue = cycloidal(index);

    scalar = sinValue * Tnormal->Amount / frequency[i];

    VAddScaledEq(normal, scalar / (length * (DBL)Number_Of_Waves), point);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   bumps
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
* CHANGES
*
******************************************************************************/

static void bumps (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal)
{
  VECTOR bump_turb;

  /* Get normal displacement value. */

  DNoise (bump_turb, EPoint);

  /* Displace "normal". */

  VAddScaledEq(normal, Tnormal->Amount, bump_turb);
}



/*****************************************************************************
*
* FUNCTION
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
*   Dents is similar to bumps, but uses noise() to control the amount of
*   dnoise() perturbation of the object normal...
*
* CHANGES
*
******************************************************************************/

static void dents (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal)
{
  DBL noise;
  VECTOR stucco_turb;

  noise = Noise (EPoint, (TPATTERN*)Tnormal);

  noise = noise * noise * noise * Tnormal->Amount;

  /* Get normal displacement value. */

  DNoise(stucco_turb, EPoint);

  /* Displace "normal". */

  VAddScaledEq(normal, noise, stucco_turb);
}




/*****************************************************************************
*
* FUNCTION
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
*   Wrinkles - This is my implementation of the dented() routine, using
*   a surface iterative fractal derived from DTurbulence.
*
*   This is a 3-D version (thanks to DNoise()...) of the usual version
*   using the singular Noise()...
*
*   Seems to look a lot like wrinkles, however... (hmmm)
*
*   Idea garnered from the April 89 Byte Graphics Supplement on RenderMan,
*   refined from "The RenderMan Companion, by Steve Upstill of Pixar,
*   (C) 1990 Addison-Wesley.
*
* CHANGES
*
******************************************************************************/

static void wrinkles (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal)
{
  register int i;
  register DBL scale = 1.0;
  VECTOR result, value, value2;

  Make_Vector(result, 0.0, 0.0, 0.0);

  for (i = 0; i < 10; scale *= 2.0, i++)
  {
    VScale(value2,EPoint,scale);
    DNoise(value, value2);

    result[X] += fabs(value[X] / scale);
    result[Y] += fabs(value[Y] / scale);
    result[Z] += fabs(value[Z] / scale);
  }

  /* Displace "normal". */

  VAddScaledEq(normal, Tnormal->Amount, result);
}


/*****************************************************************************
*
* FUNCTION
*
*   quilted
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Dan Farmer '94
*   
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static void quilted (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal)
{
  VECTOR value;
  DBL t;

  value[X] = EPoint[X]-FLOOR(EPoint[X])-0.5;
  value[Y] = EPoint[Y]-FLOOR(EPoint[Y])-0.5;
  value[Z] = EPoint[Z]-FLOOR(EPoint[Z])-0.5;

  t = sqrt(value[X]*value[X]+value[Y]*value[Y]+value[Z]*value[Z]);

  t = quilt_cubic(t, Tnormal->Vals.Quilted.Control0, Tnormal->Vals.Quilted.Control1);

  value[X] *= t;
  value[Y] *= t;
  value[Z] *= t;

  VAddScaledEq (normal, Tnormal->Amount,value);
}

/*****************************************************************************
*
* FUNCTION
*
*   facets
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Ronald Parker '98
*   
* DESCRIPTION
* 
*   This pattern is based on the "Crackle" pattern and creates a faceted
*   look on a curved surface.
*
* CHANGES
*
******************************************************************************/

static void facets (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal)
{
  int    i;
  int    thisseed;
  DBL    sum, minsum;
  VECTOR sv, tv, dv, t1, add, newnormal, pert;
  DBL    scale;  
  int    UseSquare;
  int    UseUnity;
  DBL    Metric;

  static int cvc;
  static int lastseed = 0x80000000;
  static VECTOR cv[81];

  Metric = Tnormal->Vals.Facets.Metric[X];

  UseSquare = (Metric == 2 );
  UseUnity  = (Metric == 1 );

  VNormalize( normal, normal );

  if ( Tnormal->Vals.Facets.UseCoords ) 
  {
      Assign_Vector(tv,EPoint);
  }
  else 
  {
      Assign_Vector(tv,normal);
  }

  if ( Tnormal->Vals.Facets.Size < 1e-6 ) 
  {
      scale = 1e6;
  }
  else 
  {
      scale = 1. / Tnormal->Vals.Facets.Size;
  }

  VScaleEq( tv, scale );

  /*
   * Check to see if the input point is in the same unit cube as the last
   * call to this function, to use cache of cubelets for speed.
   */

  thisseed = PickInCube(tv, t1);

  if (thisseed != lastseed)
  {
    /*
     * No, not same unit cube.  Calculate the random points for this new
     * cube and its 80 neighbours which differ in any axis by 1 or 2.
     * Why distance of 2?  If there is 1 point in each cube, located
     * randomly, it is possible for the closest random point to be in the
     * cube 2 over, or the one two over and one up.  It is NOT possible
     * for it to be two over and two up.  Picture a 3x3x3 cube with 9 more
     * cubes glued onto each face.
     */

    /* Now store a points for this cube and each of the 80 neighbour cubes. */

    cvc = 0;

    for (add[X] = -2.0; add[X] < 2.5; add[X] +=1.0)
    {
      for (add[Y] = -2.0; add[Y] < 2.5; add[Y] += 1.0)
      {
        for (add[Z] = -2.0; add[Z] < 2.5; add[Z] += 1.0)
        {
          /* For each cubelet in a 5x5 cube. */

          if ((fabs(add[X])>1.5)+(fabs(add[Y])>1.5)+(fabs(add[Z])>1.5) <= 1.0)
          {
            /* Yes, it's within a 3d knight move away. */

            VAdd(sv, tv, add);

            PickInCube(sv, t1);

            cv[cvc][X] = t1[X];
            cv[cvc][Y] = t1[Y];
            cv[cvc][Z] = t1[Z];
            cvc++;
          }
        }
      }
    }

    lastseed = thisseed;
  }

  /*
   * Find the point with the shortest distance from the input point.
   */

  VSub(dv, cv[0], tv);  
  if ( UseSquare ) 
  {
      minsum  = VSumSqr(dv);
  }
  else 
  {
     if ( UseUnity ) 
     {
        minsum = dv[X]+dv[Y]+dv[Z];
     }
     else 
     {
        minsum = pow(fabs(dv[X]), Metric)+
                 pow(fabs(dv[Y]), Metric)+
                 pow(fabs(dv[Z]), Metric);
     }
  }

  Assign_Vector( newnormal, cv[0] );

  /* Loop for the 81 cubelets to find closest. */

  for (i = 1; i < cvc; i++)
  {
    VSub(dv, cv[i], tv);

    if ( UseSquare ) 
    {
       sum  = VSumSqr(dv);
    }
    else 
    {
      if ( UseUnity ) 
      {
          sum = dv[X]+dv[Y]+dv[Z];
      }
      else 
      {
          sum = pow(fabs(dv[X]), Metric)+
                pow(fabs(dv[Y]), Metric)+
                pow(fabs(dv[Z]), Metric);
      }
    }

    if (sum < minsum)
    {
      minsum = sum;
      Assign_Vector( newnormal, cv[i] );
    }
  }

  if ( Tnormal->Vals.Facets.UseCoords ) 
  {
     DNoise( pert, newnormal );
     VDot( sum, pert, normal );
     VScale( newnormal, normal, sum );
     VSubEq( pert, newnormal );
     VAddScaledEq( normal, Tnormal->Vals.Facets.UseCoords, pert );
  }
  else 
  {
      Assign_Vector( normal, newnormal );
  }

  if(opts.Language_Version<=310)
    VNormalize( normal, normal );
}

/*****************************************************************************
*
* FUNCTION
*
*   Create_Tnormal
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
*   pointer to the created Tnormal
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION   : Allocate memory for new Tnormal and initialize it to
*                 system default values.
*
* CHANGES
*
******************************************************************************/


TNORMAL *Create_Tnormal ()
{
  TNORMAL *New;

  New = (TNORMAL *)POV_MALLOC(sizeof(TNORMAL), "normal");

  Init_TPat_Fields((TPATTERN *)New);

  New->Amount = 0.5;

  /* NK delta */
  New->Delta = (float)0.02; /* this is a good starting point for delta */

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Tnormal
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
* CHANGES
*
******************************************************************************/

TNORMAL *Copy_Tnormal (TNORMAL *Old)
{
  TNORMAL *New;

  if (Old != NULL)
  {
    New = Create_Tnormal();

    Copy_TPat_Fields ((TPATTERN *)New, (TPATTERN *)Old);

    New->Amount = Old->Amount;
    New->Delta = Old->Delta;
  }
  else
  {
    New = NULL;
  }

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Tnormal
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
* CHANGES
*
******************************************************************************/

void Destroy_Tnormal(TNORMAL *Tnormal)
{
  if (Tnormal != NULL)
  {
    Destroy_TPat_Fields ((TPATTERN *)Tnormal);

    POV_FREE(Tnormal);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Post_Tnormal
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
* CHANGES
*
******************************************************************************/

void Post_Tnormal (TNORMAL *Tnormal)
{
  int i;
  BLEND_MAP *Map;

  if (Tnormal != NULL)
  {
    if (Tnormal->Flags & POST_DONE)
    {
      return;
    }

    if (Tnormal->Type == NO_PATTERN)
    {
      Error("No normal type given.");
    }

    Tnormal->Flags |= POST_DONE;

    if ((Map = Tnormal->Blend_Map) != NULL)
    {
      for (i = 0; i < Map->Number_Of_Entries; i++)
      {
        switch (Map->Type)
        {
          case PIGMENT_TYPE:

            Post_Pigment(Map->Blend_Map_Entries[i].Vals.Pigment);

            break;

          case NORMAL_TYPE:
            Map->Blend_Map_Entries[i].Vals.Tnormal->Flags |= 
              (Tnormal->Flags & DONT_SCALE_BUMPS_FLAG);

            Post_Tnormal(Map->Blend_Map_Entries[i].Vals.Tnormal);

            break;

          case TEXTURE_TYPE:

            Post_Textures(Map->Blend_Map_Entries[i].Vals.Texture);

            break;

          case SLOPE_TYPE:
          case COLOUR_TYPE:
          case PATTERN_TYPE:

            break;

          default:

            Error("Unknown pattern type in Post_Tnormal.");
        }
      }
    }
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Perturb_Normal
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
* CHANGES
*    Added intersectin parameter for UV mapping - NK 1998
*
******************************************************************************/

void Perturb_Normal(VECTOR Layer_Normal, TNORMAL *Tnormal, VECTOR  EPoint,INTERSECTION *Intersection)
{
  VECTOR TPoint,P1;
  DBL value1,value2,Amount;
  int i;
  BLEND_MAP *Blend_Map;
  BLEND_MAP_ENTRY *Prev, *Cur;
  
  if (Tnormal==NULL)
  {
    return;
  }

  /* If normal_map present, use it and return */

  if ((Blend_Map=Tnormal->Blend_Map) != NULL)
  {
    if ((Blend_Map->Type == NORMAL_TYPE) && (Tnormal->Type == UV_MAP_PATTERN))
    {
      UV_VECT UV_Coords;

      Cur = &(Tnormal->Blend_Map->Blend_Map_Entries[0]);

      /* Don't bother warping, simply get the UV vect of the intersection */
      UVCoord(UV_Coords, Intersection->Object, Intersection);
      TPoint[X] = UV_Coords[U];
      TPoint[Y] = UV_Coords[V];
      TPoint[Z] = 0;

      Perturb_Normal(Layer_Normal,Cur->Vals.Tnormal,TPoint,Intersection);
      VNormalizeEq(Layer_Normal);
      Assign_Vector(Intersection->PNormal, Layer_Normal); /* -hdf- June 98 */

      return;
    }
    else if ((Blend_Map->Type == NORMAL_TYPE) && (Tnormal->Type != AVERAGE_PATTERN))
    {
      /* NK 19 Nov 1999 added Warp_EPoint */
      Warp_EPoint (TPoint, EPoint, (TPATTERN *)Tnormal);
      value1 = Evaluate_TPat((TPATTERN *)Tnormal,TPoint,Intersection);

      Search_Blend_Map (value1,Blend_Map,&Prev,&Cur);
      
      if(opts.Language_Version>310)
        Warp_Normal(Layer_Normal,Layer_Normal, (TPATTERN *)Tnormal, Test_Flag(Tnormal,DONT_SCALE_BUMPS_FLAG));
      Assign_Vector(P1,Layer_Normal);

      Warp_EPoint (TPoint, EPoint, (TPATTERN *)Tnormal);

      Perturb_Normal(Layer_Normal,Cur->Vals.Tnormal,TPoint,Intersection);

      if (Prev != Cur)
      {
        Perturb_Normal(P1,Prev->Vals.Tnormal,TPoint,Intersection);
  
        value2 = (value1-Prev->value)/(Cur->value-Prev->value);
        value1 = 1.0-value2;

        VLinComb2(Layer_Normal,value1,P1,value2,Layer_Normal);
      }

      if(opts.Language_Version>310)
        UnWarp_Normal(Layer_Normal,Layer_Normal,(TPATTERN *)Tnormal, Test_Flag(Tnormal,DONT_SCALE_BUMPS_FLAG));

      VNormalizeEq(Layer_Normal);

      Assign_Vector(Intersection->PNormal, Layer_Normal); /* -hdf- June 98 */

      return;
    }
  }
  
  /* No normal_map. */

  if (Tnormal->Type <= LAST_NORM_ONLY_PATTERN)
  {
    if(opts.Language_Version>310)
    {
      Warp_Normal(Layer_Normal,Layer_Normal, (TPATTERN *)Tnormal, 
        Test_Flag(Tnormal,DONT_SCALE_BUMPS_FLAG));
    }
    Warp_EPoint (TPoint, EPoint, (TPATTERN *)Tnormal);

    switch (Tnormal->Type)
      {
       case BITMAP_PATTERN: bump_map (TPoint, Tnormal, Layer_Normal); break;
       case BUMPS_PATTERN:  bumps (TPoint, Tnormal, Layer_Normal);    break;
       case DENTS_PATTERN:  dents (TPoint, Tnormal, Layer_Normal);    break;
       case RIPPLES_PATTERN:ripples (TPoint, Tnormal, Layer_Normal);  break;
       case WAVES_PATTERN:  waves (TPoint, Tnormal, Layer_Normal);    break;
       case WRINKLES_PATTERN:wrinkles (TPoint, Tnormal, Layer_Normal);break;
       case QUILTED_PATTERN:quilted (TPoint, Tnormal, Layer_Normal);  break;
       case FACETS_PATTERN: facets( TPoint, Tnormal, Layer_Normal);   break;
       case AVERAGE_PATTERN: Do_Average_Normals (TPoint, Tnormal, Layer_Normal, Intersection);  break;
       default:
         Error("Normal pattern not yet implemented.");
      }

    if(opts.Language_Version>310)
    {
      UnWarp_Normal(Layer_Normal,Layer_Normal, (TPATTERN *)Tnormal,
        Test_Flag(Tnormal,DONT_SCALE_BUMPS_FLAG));
    }
  }
  else
  {
    if(opts.Language_Version>310)
    {
      Warp_Normal(Layer_Normal,Layer_Normal, (TPATTERN *)Tnormal,
        Test_Flag(Tnormal,DONT_SCALE_BUMPS_FLAG));
    }

    Amount=Tnormal->Amount * -5.0; /*fudge factor*/
    Amount*=0.02/Tnormal->Delta; /* NK delta */

    /* warp the center point first - this is the last warp */
    if(opts.Language_Version>310)
      Warp_EPoint(TPoint,EPoint,(TPATTERN *)Tnormal);
    else
      Assign_Vector(TPoint,EPoint);
   
    for(i=0; i<=3; i++)
    {
      VAddScaled(P1,TPoint,Tnormal->Delta,Pyramid_Vect[i]); /* NK delta */
      if(opts.Language_Version<=310)
        Warp_EPoint(P1,P1,(TPATTERN *)Tnormal);
      value1 = Do_Slope_Map(Evaluate_TPat((TPATTERN *)Tnormal,P1,Intersection),Blend_Map);
      VAddScaledEq(Layer_Normal,value1*Amount,Pyramid_Vect[i]);
    }

    if(opts.Language_Version>310)
    {
      UnWarp_Normal(Layer_Normal,Layer_Normal,(TPATTERN *)Tnormal,
        Test_Flag(Tnormal,DONT_SCALE_BUMPS_FLAG));
    }

  }

  if(opts.Language_Version<=310)
    VNormalizeEq(Layer_Normal);

  if ( Intersection )
	Assign_Vector(Intersection->PNormal, Layer_Normal); /* -hdf- June 98 */
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

static DBL Do_Slope_Map (DBL value,BLEND_MAP *Blend_Map)
{
  DBL Result;
  BLEND_MAP_ENTRY *Prev, *Cur;

  if (Blend_Map == NULL)
  {
    return(value);
  }

  Search_Blend_Map (value,Blend_Map,&Prev,&Cur);

  if (Prev == Cur)
  {
     return(Cur->Vals.Point_Slope[0]);
  }

  Result = (value-Prev->value)/(Cur->value-Prev->value);

  return(Hermite_Cubic(Result,Prev->Vals.Point_Slope,Cur->Vals.Point_Slope));
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

#define S1 UV1[1]
#define S2 UV2[1]
#define P1 UV1[0]
#define P2 UV2[0]

static DBL Hermite_Cubic(DBL T1,UV_VECT UV1,UV_VECT UV2)
{
  DBL TT=T1*T1;
  DBL TTT=TT*T1;
  DBL rv;        /* simplified equation for poor Symantec */

  rv  = TTT*(S1+S2+2.0*(P1-P2));
  rv += -TT*(2.0*S1+S2+3.0*(P1-P2));
  rv += T1*S1 +P1;

  return (rv);
}



/*****************************************************************************
*
* FUNCTION
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*    Added intersectin parameter for UV mapping - NK 1998
*
******************************************************************************/

static void Do_Average_Normals (VECTOR EPoint,TNORMAL *Tnormal,VECTOR normal, INTERSECTION *Inter)
{
   int i;
   BLEND_MAP *Map = Tnormal->Blend_Map;
   SNGL Value;
   SNGL Total = 0.0;
   VECTOR V1,V2;
   
   Make_Vector (V1, 0.0, 0.0, 0.0);

   for (i = 0; i < Map->Number_Of_Entries; i++)
   {
     Value = Map->Blend_Map_Entries[i].value;
     
     Assign_Vector(V2,normal);

     Perturb_Normal(V2,Map->Blend_Map_Entries[i].Vals.Tnormal,EPoint, Inter);
     
     VAddScaledEq(V1,Value,V2);

     Total += Value;
   }

   VInverseScale(normal,V1,Total);
}

END_POV_NAMESPACE
