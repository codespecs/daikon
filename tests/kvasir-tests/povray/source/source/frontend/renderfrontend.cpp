/****************************************************************************
 *               renderfrontend.cpp
 *
 * This module contains the basic C++ interface for the RenderFrontend class.
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
 * $File: //depot/povray/3.6-release/source/frontend/renderfrontend.cpp $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#include "configfrontend.h"

#include "renderfrontend.h"
#include "povmsgid.h"
#include "pov_err.h"

BEGIN_POV_FRONTEND_NAMESPACE

RenderFrontend::RenderFrontend(POVMSContext ctx, POVMSAddress baddr) : MessageOutput(ctx)
{
	int err = kNoErr;

	context = ctx;
	backendaddress = baddr;
	state = kUnknown;

	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_RenderStarted, this, &RenderFrontend::RenderStarted);
	InstallBack(kPOVMsgClass_RenderOutput, kPOVMsgIdent_RenderDone, this, &RenderFrontend::RenderDone);

	state = kReady;
}

RenderFrontend::~RenderFrontend()
{
	state = kUnknown;
}

void RenderFrontend::StartRender(POVMS_Object& renderoptions)
{
	if(state != kReady)
		throw int(kNotNowErr);

	POVMS_Message optionsmsg(kPOVObjectClass_ROptions, kPOVMsgClass_RenderControl, kPOVMsgIdent_RenderOptions);
	POVMS_Message startmsg(kPOVMSType_WildCard, kPOVMsgClass_RenderControl, kPOVMsgIdent_RenderAll);

	optionsmsg.Merge(renderoptions);
	optionsmsg.SetDestinationAddress(backendaddress);
	POVMS_SendMessage(context, optionsmsg, NULL, kPOVMSSendMode_NoReply);

	startmsg.SetDestinationAddress(backendaddress);
	POVMS_SendMessage(context, startmsg, NULL, kPOVMSSendMode_NoReply);

	state = kStartRequested;
}

void RenderFrontend::StopRender()
{
	if(state <= kReady)
		throw int(kNotNowErr);

	POVMS_Message stopmsg(kPOVMSType_WildCard, kPOVMsgClass_RenderControl, kPOVMsgIdent_RenderStop);

	stopmsg.SetDestinationAddress(backendaddress);
	POVMS_SendMessage(context, stopmsg, NULL, kPOVMSSendMode_NoReply);

	state = kStopRequested;
}

void RenderFrontend::RenderStarted(POVMS_Message&, POVMS_Message&, int)
{
	state = kRendering;
}

void RenderFrontend::RenderDone(POVMS_Message&, POVMS_Message&, int)
{
	state = kReady;
}

END_POV_FRONTEND_NAMESPACE
