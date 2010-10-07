/****************************************************************************
 *               configbase.h
 *
 * This module contains all defines, typedefs, and prototypes for the base layer.
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
 * $File: //depot/povray/3.6-release/source/base/configbase.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef CONFIGBASE_H
#define CONFIGBASE_H

#include <cctype>
#include <cstring>
#include <cstdlib>

#include "config.h"

#ifndef POV_BASE_NAMESPACE
	#define POV_BASE_NAMESPACE pov_base
	#define BEGIN_POV_BASE_NAMESPACE namespace pov_base { using namespace std;
	#define END_POV_BASE_NAMESPACE }
	#define USING_POV_BASE_NAMESPACE using namespace pov_base;
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

#ifndef POV_GET_FULL_PATH
	#define POV_GET_FULL_PATH(p,b) if (b) strcpy(b,p);
#endif

#ifndef EXIST_FILE
	#define EXIST_FILE(name) POV_BASE_NAMESPACE::File_Exist(name)
#endif

#ifndef EXIST_FONT_FILE
	#define EXIST_FONT_FILE(name) (0)
#endif

#ifndef DEFAULT_ITEXTSTREAM_BUFFER_SIZE
	#define DEFAULT_ITEXTSTREAM_BUFFER_SIZE 512
#endif

#ifndef POV_ALLOW_FILE_READ
	#define POV_ALLOW_FILE_READ(f,t) (1)
#endif

#ifndef POV_ALLOW_FILE_WRITE
	#define POV_ALLOW_FILE_WRITE(f,t) (1)
#endif

#endif
