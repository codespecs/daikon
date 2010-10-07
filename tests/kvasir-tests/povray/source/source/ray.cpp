/****************************************************************************
 *               ray.cpp
 *
 * This module implements the code pertaining to rays.
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
 * $File: //depot/povray/3.6-release/source/ray.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "vector.h"
#include "povray.h"
#include "interior.h"
#include "ray.h"
#include "texture.h"

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



/*****************************************************************************
*
* FUNCTION
*
*   Initialize_Ray_Containers
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

void Initialize_Ray_Containers(RAY *Ray, unsigned int optimisiation_flags)
{
  Ray->Index = - 1;
  Ray->Optimisiation_Flags = optimisiation_flags;
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Ray_Containers
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

void Copy_Ray_Containers(RAY *Dest_Ray, RAY  *Source_Ray)
{
  register int i;

  if ((Dest_Ray->Index = Source_Ray->Index) >= MAX_CONTAINING_OBJECTS)
  {
    Error("Containing index too high.");
  }

  for (i = 0 ; i <= Source_Ray->Index; i++)
  {
    Dest_Ray->Interiors[i] = Source_Ray->Interiors[i];
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Ray_Enter
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
*   Oct 1995 : Fixed bug with IOR assignment (only valid for plain textures) [DB]
*
******************************************************************************/

void Ray_Enter(RAY *Ray, INTERIOR *interior)
{
  int index;

  if ((index = ++(Ray->Index)) >= MAX_CONTAINING_OBJECTS)
  {
    Error("Too many nested refracting objects.");
  }

  Ray->Interiors[index] = interior;
}



/*****************************************************************************
*
* FUNCTION
*
*   Ray_Exit
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
*   Remove given entry from given ray's container.
*
* CHANGES
*
*   -
*
******************************************************************************/

void Ray_Exit(RAY *Ray, int nr)
{
  int i;

  for (i = nr; i < Ray->Index; i++)
  {
    Ray->Interiors[i] = Ray->Interiors[i+1];
  }

  if (--(Ray->Index) < - 1)
  {
    Error("Too many exits from refractions.");
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Interior_In_Ray_Container
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
*   Test if a given interior is in the container of a given ray.
*
* CHANGES
*
*   Mar 1996 : Creation.
*
******************************************************************************/

int Interior_In_Ray_Container(RAY *ray, INTERIOR *interior)
{
  int i, found = -1;

  if (ray->Index > -1)
  {
    for (i = 0; i <= ray->Index; i++)
    {
      if (interior == ray->Interiors[i])
      {
        found = i;

        break;
      }
    }
  }

  return(found);
}

END_POV_NAMESPACE
