/****************************************************************************
 *               defaultrenderfrontend.h
 *
 * This module contains all defines, typedefs, and prototypes for the
 * C++ interface of defaultrenderfrontend.cpp.
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
 * $File: //depot/povray/3.6-release/source/frontend/defaultrenderfrontend.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef DEFAULTMESSAGEOUTPUT_H
#define DEFAULTMESSAGEOUTPUT_H

#include "configfrontend.h"

#include "renderfrontend.h"
#include "messageoutput.h"
#include "fileinputoutput.h"

BEGIN_POV_FRONTEND_NAMESPACE

USING_POV_BASE_NAMESPACE

class DefaultRenderFrontend : public RenderFrontend
{
	public:
		DefaultRenderFrontend(POVMSContext, POVMSAddress);
		~DefaultRenderFrontend();

		void BannerPrintf(const char *, ...);
		void BannerFlush();

		void PrintHelpScreens();
		void PrintUsage(int n);
	protected:
		class DefaultStreamBuffer : public TextStreamBuffer
		{
			public:
				DefaultStreamBuffer(OStream *h, bool l, bool i);
				~DefaultStreamBuffer();

				virtual void lineoutput(const char *str, unsigned int chars);
				virtual void directoutput(const char *str, unsigned int chars);
			private:
				OStream *handle;
				bool linebuffermode;
				bool inhibitmode;
		};

		virtual void OpenStreams(bool);
		virtual void CloseStreams();
};

END_POV_FRONTEND_NAMESPACE

#endif
