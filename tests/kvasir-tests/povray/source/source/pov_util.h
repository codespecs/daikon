/****************************************************************************
 *                  pov_util.h
 *
 * This module contains all defines, typedefs, and prototypes for POV_UTIL.CPP.
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
 * $File: //depot/povray/3.6-release/source/pov_util.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef POV_UTIL_H
#define POV_UTIL_H

#include "povms.h"
#include "stringutilities.h"
#include "fileinputoutput.h"

BEGIN_POV_NAMESPACE

USING_POV_BASE_NAMESPACE

unsigned closest_power_of_2(unsigned theNumber);
void POV_Std_Split_Time(DBL time_dif,unsigned int *hrs,unsigned int *mins,DBL *secs);
int POVMSUtil_SetFormatString(POVMSObjectPtr object, POVMSType key, const char *format, ...); // Note: Strings may not contain \0 characters codes!

IStream *New_Checked_IStream(char *filename, unsigned int stype);
OStream *New_Checked_OStream(char *filename, unsigned int stype, bool append = false);

char *Locate_Filename(char *filename, unsigned int stype, bool err_flag = false);
IStream *Locate_File(char *filename, unsigned int stype, char *buffer, bool err_flag = false);

END_POV_NAMESPACE

#endif
