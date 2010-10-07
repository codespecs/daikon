/****************************************************************************
 *               mathutil.cpp
 *
 * This module implements the utility functions for scalar math.
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
 * $File: //depot/povray/3.6-release/source/mathutil.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <ctype.h>
#include <time.h>
#include <algorithm>
#include <math.h>

#include "frame.h"
#include "mathutil.h"

BEGIN_POV_NAMESPACE

#ifdef NEED_INVHYP
DBL asinh(DBL x)
{
	return (x < 0 ? -1 : (x > 0 ? 1 : 0)) * log(fabs(x) + sqrt(1 + x * x));
}

DBL acosh(DBL x)
{
	if(x < 1.0)
		return 0;
	return log(x + sqrt(x * x - 1));
}

DBL atanh(DBL x)
{
	if(fabs(x) >= 1)
		return 0;
	return 0.5 * log((1 + x) / (1 - x));
}
#endif

END_POV_NAMESPACE
