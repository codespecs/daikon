/****************************************************************************
 *               renderfrontend.h
 *
 * This module contains all defines, typedefs, and prototypes for the
 * C++ interface of renderfrontend.cpp.
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
 * $File: //depot/povray/3.6-release/source/frontend/renderfrontend.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef RENDERFRONTEND_H
#define RENDERFRONTEND_H

#include "configbase.h"
#include "messageoutput.h"

BEGIN_POV_FRONTEND_NAMESPACE

class RenderFrontend : public MessageOutput
{
	public:
		enum EngineState
		{
			kUnknown = 0,
			kReady,
			kStartRequested,
			kRendering,
			kStopRequested
		};

		RenderFrontend(POVMSContext, POVMSAddress);
		~RenderFrontend();

		void StartRender(POVMS_Object&);
		void StopRender();

		EngineState GetState() { return state; };
	protected:
		void RenderStarted(POVMS_Message&, POVMS_Message&, int);
		void RenderDone(POVMS_Message&, POVMS_Message&, int);
	private:
		POVMSAddress backendaddress;
		POVMSContext context;
		EngineState state;
};

END_POV_FRONTEND_NAMESPACE

#endif
