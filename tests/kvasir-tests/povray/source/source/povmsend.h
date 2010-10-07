/****************************************************************************
 *                  povmsend.h
 *
 * This module contains all defines, typedefs, and prototypes for povmsend.cpp
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
 * $File: //depot/povray/3.6-release/source/povmsend.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef POVMSSEND_H
#define POVMSSEND_H

#include "povmsgid.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/


/*****************************************************************************
* Global typedefs
******************************************************************************/


/*****************************************************************************
* Global variables
******************************************************************************/

enum
{
	PROGRESS_CREATING_BOUNDING_SLABS = kPOVList_Prog_CreatingBoundingSlabs,
	PROGRESS_CREATING_VISTA_BUFFER = kPOVList_Prog_CreatingVistaBuffer,
	PROGRESS_CREATE_LIGHT_BUFFERS = kPOVList_Prog_CreatingLightBuffers,
	PROGRESS_BUILDING_PHOTON_MAPS = kPOVList_Prog_BuildingPhotonMaps,
	PROGRESS_LOADING_PHOTON_MAPS = kPOVList_Prog_LoadingPhotonMaps,
	PROGRESS_SAVING_PHOTON_MAPS = kPOVList_Prog_SavingPhotonMaps,
	PROGRESS_SORTING_PHOTONS = kPOVList_Prog_SortingPhotons,
	PROGRESS_RECLAIMING_MEMORY = kPOVList_Prog_ReclaimingMemory,
	PROGRESS_WRITE_INI_FILE = kPOVList_Prog_WritingINIFile,
	PROGRESS_WRITE_HISTOGRAM_FILE = kPOVList_Prog_WritingHistogramFile,
	PROGRESS_PERFORMING_SHELLOUT_COMMAND = kPOVList_Prog_PerformingShelloutCommand,
	PROGRESS_RESUMING_INTERRUPTED_TRACE = kPOVList_Prog_ResumingInterruptedTrace,
	PROGRESS_PROCESSING_FRAME = kPOVList_Prog_ProcessingFrame,
	PROGRESS_PARSING = kPOVList_Prog_Parsing,
	PROGRESS_DISPLAYING = kPOVList_Prog_Displaying,
	PROGRESS_RENDERING = kPOVList_Prog_Rendering,
	PROGRESS_DONE_TRACING = kPOVList_Prog_DoneTracing,
	PROGRESS_ABORTING_RENDER = kPOVList_Prog_AbortingRender,
	PROGRESS_USER_ABORT = kPOVList_Prog_UserAbort
};


/*****************************************************************************
* Global functions
******************************************************************************/

int BuildRenderOptions(POVMSObjectPtr msg);

void Send_InitInfo();
int Send_Progress(const char *statusString, int progressState);
int Send_ProgressUpdate(int progressState, int timeDiff = 1);
int Send_FrameStatistics();
int Send_ParseStatistics();
int Send_RenderStatistics(bool total = false);
int Send_RenderOptions();
int Send_RenderStarted(bool continuetrace);
int Send_RenderDone(bool withtime);

END_POV_NAMESPACE

#endif /* POVMSSEND_H */
