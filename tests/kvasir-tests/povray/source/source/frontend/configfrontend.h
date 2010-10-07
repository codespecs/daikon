/****************************************************************************
 *               configfrontend.h
 *
 * This module contains all defines, typedefs, and prototypes for the frontend layer.
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
 * $File: //depot/povray/3.6-release/source/frontend/configfrontend.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef CONFIGFRONTEND_H
#define CONFIGFRONTEND_H

#include <cctype>

#include "config.h"

#ifndef POV_FRONTEND_NAMESPACE
	#define POV_FRONTEND_NAMESPACE pov_frontend
	#define BEGIN_POV_FRONTEND_NAMESPACE namespace pov_frontend { using namespace std;
	#define END_POV_FRONTEND_NAMESPACE }
	#define USING_POV_FRONTEND_NAMESPACE using namespace pov_frontend;
#endif

#ifndef IStream
	#define IStream IStream
#endif

#ifndef OStream
	#define OStream OStream
#endif

#ifndef POV_LONG
	#define POV_LONG long long
#endif

#ifndef FILE_NAME_LENGTH
	#define FILE_NAME_LENGTH 150
#endif

#ifndef FILENAME_SEPARATOR
	#define FILENAME_SEPARATOR '/'
#endif

#ifndef DRIVE_SEPARATOR
	#define DRIVE_SEPARATOR ':'
#endif

#endif
