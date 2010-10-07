/****************************************************************************
 *               stringutilities.cpp
 *
 * This module implements string utility functions.
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
 * $File: //depot/povray/3.6-release/source/base/stringutilities.cpp $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#include <cctype>
#include <cstdarg>
#include <cstring>
#include <cstdlib>
#include <cstdio>

#include "configbase.h"

#include "stringutilities.h"

BEGIN_POV_BASE_NAMESPACE

/*****************************************************************************
*
* FUNCTION
*
*   pov_stricmp
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
*   Since the stricmp function isn't available on all systems, we've
*   provided a simplified version of it here.
*
* CHANGES
*
*   -
*
******************************************************************************/

int pov_stricmp(const char *s1, const char *s2)
{
	char c1, c2;

	while((*s1 != 0) && (*s2 != 0))
	{
		c1 = *s1++;
		c2 = *s2++;

		c1 = (char)toupper(c1);
		c2 = (char)toupper(c2);

		if(c1 < c2)
			return -1;
		if(c1 > c2)
			return 1;
	}

	if(*s1 == 0)
	{
		if(*s2 == 0)
			return 0;
		else
			return -1;
	}
	else
		return 1;
}

/*****************************************************************************
*
* FUNCTION
*
*   pov_tsprintf
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
*   Since the stricmp function isn't available on all systems, we've
*   provided a simplified version of it here.
*
* CHANGES
*
*   -
*
******************************************************************************/

const char *pov_tsprintf(const char *format,...)
{
	va_list marker;

	static char pov_tsprintf_buffer[1024];

	va_start(marker, format);
	vsnprintf(pov_tsprintf_buffer, 1023, format, marker);
	va_end(marker);

	return pov_tsprintf_buffer;
}

END_POV_BASE_NAMESPACE
