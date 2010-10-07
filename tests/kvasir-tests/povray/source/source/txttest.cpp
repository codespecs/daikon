/****************************************************************************
 *               txttest.cpp
 *
 * This module implements "fill-in-the-blank" pre-programmed texture 
 * functions for easy modification and testing. Create new textures here.
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
 * $File: //depot/povray/3.6-release/source/txttest.cpp $
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
#include "povray.h"    /* [DB 9/94] */
#include "txttest.h"   /* [DB 9/94] */
#include "pattern.h"   /* [CY 10/94] */

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/



/*****************************************************************************
* Local typedefs
******************************************************************************/



/*****************************************************************************
* Local variables
******************************************************************************/



/*****************************************************************************
* Static functions
******************************************************************************/



/*
 * Test new textures in the routines that follow.
 */

/*****************************************************************************
*
* FUNCTION
*
*   pattern1
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
*   The pattern routines take an x,y,z point on an object and a pointer to
*   the object's texture description and return the color at that point
*   Similar routines are granite, agate, marble. See txtcolor.c for examples.
*
* CHANGES
*
******************************************************************************/

DBL pattern1 (VECTOR EPoint, TPATTERN *TPat)
{
  DBL value;
  /* YOUR NAME HERE */

  TPat=TPat;

  value = Noise(EPoint, TPat);

  return(value);

}



/*****************************************************************************
*
* FUNCTION
*
*   pattern2
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
*   The pattern routines take an x,y,z point on an object and a pointer to
*   the object's texture description and return the color at that point
*   Similar routines are granite, agate, marble. See txtcolor.c for examples.
*
* CHANGES
*
******************************************************************************/

DBL pattern2 (VECTOR EPoint, TPATTERN *TPat)
{
  DBL value;
  /* YOUR NAME HERE */
  TPat=TPat;

  value = Noise(EPoint, TPat);

  return(value);

}




/*****************************************************************************
*
* FUNCTION
*
*   pattern3
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
*   The pattern routines take an x,y,z point on an object and a pointer to
*   the object's texture description and return the color at that point
*   Similar routines are granite, agate, marble. See txtcolor.c for examples.
*
* CHANGES
*
******************************************************************************/

DBL pattern3 (VECTOR EPoint, TPATTERN *TPat)
{
  DBL value;
  /* YOUR NAME HERE */
  TPat=TPat;

  value = Noise(EPoint, TPat);

  return(value);

}



/*****************************************************************************
*
* FUNCTION
*
*   bumpy1
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
*   The bumpy routines take a point on an object,  a pointer to the
*   object's texture description and the surface normal at that point and
*   return a peturb surface normal to create the illusion that the surface
*   has been displaced.
*
*   Similar routines are ripples, dents, bumps. See txtbump.c for examples.
*
* CHANGES
*
******************************************************************************/

void bumpy1 (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal)
{
  /* YOUR NAME HERE */
  EPoint=EPoint;

  Tnormal = Tnormal;

  Assign_Vector(normal, normal);
}



/*****************************************************************************
*
* FUNCTION
*
*   bumpy2
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
*   The bumpy routines take a point on an object,  a pointer to the
*   object's texture description and the surface normal at that point and
*   return a peturb surface normal to create the illusion that the surface
*   has been displaced.
*
*   Similar routines are ripples, dents, bumps. See txtbump.c for examples.
*
* CHANGES
*
******************************************************************************/

void bumpy2 (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal)
{
  /* YOUR NAME HERE */
  EPoint=EPoint;

  Tnormal = Tnormal;

  Assign_Vector(normal, normal);
}



/*****************************************************************************
*
* FUNCTION
*
*   bumpy3
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
*   The bumpy routines take a point on an object,  a pointer to the
*   object's texture description and the surface normal at that point and
*   return a peturb surface normal to create the illusion that the surface
*   has been displaced.
*
*   Similar routines are ripples, dents, bumps. See txtbump.c for examples.
*
* CHANGES
*
******************************************************************************/

void bumpy3 (VECTOR EPoint, TNORMAL *Tnormal, VECTOR normal)
{
  /* YOUR NAME HERE */
  EPoint=EPoint;

  Tnormal = Tnormal;

  Assign_Vector(normal, normal);
}

END_POV_NAMESPACE
