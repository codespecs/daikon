/****************************************************************************
 *                  LIGHTGRP.H
 *
 * This module contains all defines, typedefs, and prototypes for LIGHTGRP.CPP.
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
 * $File: //depot/povray/3.6-release/source/lightgrp.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef LIGHTGRP_H
#define LIGHTGRP_H

#include "csg.h"

BEGIN_POV_NAMESPACE

// This struct is for creating a list of light sources that is INDEPENDENT
// from the Next_Light_Source pointer that is part of the light source
// struct.
typedef struct light_group_light_struct LIGHT_GROUP_LIGHT;

struct light_group_light_struct {
  LIGHT_SOURCE* Light;
  LIGHT_GROUP_LIGHT* Next;
};


void Promote_Local_Lights(CSG *Object);
bool Check_Photon_Light_Group(OBJECT* Object);

END_POV_NAMESPACE

#endif
