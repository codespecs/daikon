/****************************************************************************
 *                  LIGHTGRP.CPP
 *
 * Implements light group utility functions.
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
 * $File: //depot/povray/3.6-release/source/lightgrp.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "lightgrp.h"
#include "povray.h"
#include "point.h"
#include "objects.h"
#include "pov_util.h"
#include "csg.h"
#include "photons.h"

BEGIN_POV_NAMESPACE

void Promote_Local_Lights_Recursive(COMPOUND_OBJECT *Object, LIGHT_SOURCE *Lights);


/*****************************************************************************
*
* FUNCTION
*
*   Promote_Local_Lights
*
* INPUT
*
*   Object - CSG union object
*
* OUTPUT
*
*   Modified CSG union object with local lights added to each object
*
* RETURNS
*   
* AUTHOR
*
*   Thorsten Froehlich [trf]
*   
* DESCRIPTION
*
*   Collects all light sources in CSG union object (only those who are direct
*   children of the object, not childrens children light sources - this was
*   taken care of before) and adds them to local light list of every object
*   in the CSG union.
*   NOTE: Only pointers are changed, the original light source is still the
*   one in the CSG union, and once this light source has been deallocated,
*   the pointers will be invalid.  Note that because of this we do not need
*   to (and we may not!) free the LLight lights of each object!
*
* CHANGES
*
*   Jun 2000 : Creation.
*
******************************************************************************/

void Promote_Local_Lights(CSG *Object)
{
	LIGHT_SOURCE *lights = NULL;
	OBJECT *curObject = NULL;
	int light_counter = 0;
	int object_counter = 0;

	if(Object == NULL)
		return;

	// find all light sources in the light group and connect them to form a list
	for(curObject = Object->Children, light_counter = 0, object_counter = 0;
	    curObject != NULL;
	    curObject = curObject->Sibling)
	{
		if((curObject->Type & LIGHT_GROUP_LIGHT_OBJECT) == LIGHT_GROUP_LIGHT_OBJECT)
		{
			// linked list of light sources
			if(lights == NULL)
			{
				lights = (LIGHT_SOURCE *)curObject;
				lights->Next_Light_Source = NULL;
			}
			else
			{
				LIGHT_SOURCE *l = (LIGHT_SOURCE *)curObject;

				l->Next_Light_Source = lights;
				lights = l;
			}

			light_counter++;
		}
		else
			object_counter++;
	}

	// if no lights have been found in the light group we don't need to continue, but the
	// user should know there are no lights (also it will continue to work as a union)
	if(light_counter <= 0)
	{
		Warning(0, "No light source(s) found in light group.");
		return;
	}
	// if no objects have been found nothing will happen at all (the light group is only wasting memory)
	if(object_counter <= 0)
	{
		Warning(0, "No object(s) found in light group.");
		return;
	}

	// allow easy promotion of lights (if this is part of another light group)
	Object->LLights = lights;

	// promote light recursively to all other objects in the CSG union
	Promote_Local_Lights_Recursive((COMPOUND_OBJECT *)Object, lights);
}

/*****************************************************************************
*
* FUNCTION
*
*   Promote_Local_Lights_Recursive
*
* INPUT
*
*   Object - compound object
*   Lights - local lights to add to children objects
*
* OUTPUT
*
*   Modified compound object with local lights added to each object
*
* RETURNS
*   
* AUTHOR
*
*   Thorsten Froehlich [trf]
*   
* DESCRIPTION
*
*   Adds input list of light sources to local light list of every object in
*   the compound object, recursively if there are other compound objects.
*   NOTE: Only pointers are changed and because of this we do not need to
*   (and we may not!) free the LLight lights of each object!
*
* CHANGES
*
*   Jun 2000 : Creation.
*
******************************************************************************/

void Promote_Local_Lights_Recursive(COMPOUND_OBJECT *Object, LIGHT_SOURCE *Lights)
{
	OBJECT *curObject = NULL;

	for(curObject = Object->Children;
	    curObject != NULL;
	    curObject = curObject->Sibling)
	{
		if(curObject->LLights != NULL)
		{
			LIGHT_SOURCE *l = curObject->LLights;

			// the code below is using (l != Lights) as safety
			// check - it should never be true if my logic works [trf]

			// find end of current object's light list
			while((l->Next_Light_Source != NULL) && (l != Lights))
				l = l->Next_Light_Source;

			// add inherited light list
			if(l != Lights)
				l->Next_Light_Source = Lights;
		}
		else if((curObject->Type & IS_COMPOUND_OBJECT) == IS_COMPOUND_OBJECT)
		{
			// allow easy promotion of lights (if this is part of another light group)
			curObject->LLights = Lights;

			Promote_Local_Lights_Recursive((COMPOUND_OBJECT *)curObject, Lights);
		}
		else
		{
			curObject->LLights = Lights;
		}
	}
}



/*****************************************************************************
*
* FUNCTION
*
*   Check_Photon_Light_Group
*
* INPUT
*
*   Object - any object
*
* OUTPUT
*
* RETURNS
*
*   True if this object is lit by the photon light (according to the light_group rules)
*
*
* AUTHOR
*
*   Nathan Kopp [NK]
*   
* DESCRIPTION
*
*   If the photon light is a global light (not in a light group) as determined by
*   the photonOptions object, then we just check to see if the object interacts
*   with global lights.
*
*   Otherwise...
*
*   Checks to see if Light is one of Object's local lights (part of the light
*   group).
*   
* CHANGES
*
*   Apr 2002 : Creation.
*
******************************************************************************/

bool Check_Photon_Light_Group(OBJECT* Object)
{
  if(photonOptions.Light_Is_Global)
  {
    if ((Object->Flags & NO_GLOBAL_LIGHTS_FLAG) == NO_GLOBAL_LIGHTS_FLAG)
      return false;
    else
      return true;
  }
  else
  {
    LIGHT_SOURCE *Test_Light;
    for(Test_Light = Object->LLights;
        Test_Light != NULL;
        Test_Light = Test_Light->Next_Light_Source)
    {
      if (Test_Light == photonOptions.Light) return true;
    }
    return false;
  }
}

END_POV_NAMESPACE
