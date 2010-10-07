/****************************************************************************
 *               pov_err.h
 *
 * This module contains all error numbers.
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
 * $File: //depot/povray/3.6-release/source/base/pov_err.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef POV_ERR_H
#define POV_ERR_H

#ifdef POVMS_DISCONNECTED
	#include "cnfpovms.h"
#else
	#include "configbase.h"
#endif

BEGIN_POV_BASE_NAMESPACE

// fatal errors
enum
{
	kNoError              = 0,
	kNoErr                = kNoError,
	kParamErr             = -1,
	kMemFullErr           = -2,
	kOutOfMemoryErr       = kMemFullErr,
	kInvalidDataSizeErr   = -3,
	kCannotHandleDataErr  = -4,
	kNullPointerErr       = -5,
	kChecksumErr          = -6,
	kParseErr             = -7,
	kCannotOpenFileErr    = -8,
	kInvalidDestAddrErr   = -9,
	kCannotConnectErr     = -10,
	kDisconnectedErr      = -11,
	kHostDisconnectedErr  = -12,
	kNetworkDataErr       = -13,
	kNetworkConnectionErr = -14,
	kObjectAccessErr      = -15,
	kVersionErr           = -16,
	kFileDataErr          = -17,
	kAuthorisationErr     = -18,
	kDataTypeErr          = -19,
	kTimeoutErr           = -20,
	kInvalidContextErr    = -21
};

// non fatal errors
enum
{
	kFalseErr             = 1,
	kOutOfSyncErr         = 2,
	kNotNowErr            = kOutOfSyncErr,
	kQueueFullErr         = 3
};

END_POV_BASE_NAMESPACE

#endif
