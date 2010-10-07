/****************************************************************************
 *               messageoutput.h
 *
 * This module contains all defines, typedefs, and prototypes for the
 * C++ interface version of messageoutput.cpp.
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
 * $File: //depot/povray/3.6-release/source/frontend/messageoutput.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef STANDARDMESSAGEOUTPUT_H
#define STANDARDMESSAGEOUTPUT_H

#include "configfrontend.h"

#include "povmscpp.h"
#include "textstreambuffer.h"

BEGIN_POV_FRONTEND_NAMESPACE

USING_POV_BASE_NAMESPACE

class MessageOutput : public POVMS_MessageReceiver
{
	public:
		MessageOutput(POVMSContext);
		~MessageOutput();
	protected:
		enum
		{
			BANNER_STREAM = 0,
			STATUS_STREAM,
			DEBUG_STREAM,
			FATAL_STREAM,
			RENDER_STREAM,
			STATISTIC_STREAM,
			WARNING_STREAM,
			ALL_STREAM,
			MAX_STREAMS
		};

		TextStreamBuffer *streams[MAX_STREAMS];
		char *streamnames[MAX_STREAMS];
		bool consoleoutput[MAX_STREAMS];

		virtual void OpenStreams(bool) = 0;
		virtual void CloseStreams() = 0;

		void Printfile(int, const char *, unsigned long, int);
		void Printf(int, const char *, ...);
		void Flush(int);
	protected:
		void InitInfo(POVMSObjectPtr, POVMSObjectPtr, int);
		void RenderOptions(POVMSObjectPtr, POVMSObjectPtr, int);
		void RenderStarted(POVMSObjectPtr, POVMSObjectPtr, int);
		void FrameStatistics(POVMSObjectPtr, POVMSObjectPtr, int);
		void ParseStatistics(POVMSObjectPtr, POVMSObjectPtr, int);
		void RenderStatistics(POVMSObjectPtr, POVMSObjectPtr, int);
		void RenderDone(POVMSObjectPtr, POVMSObjectPtr, int);
		void Progress(POVMSObjectPtr, POVMSObjectPtr, int);
		void Warning(POVMSObjectPtr, POVMSObjectPtr, int);
		void Error(POVMSObjectPtr, POVMSObjectPtr, int);
		void FatalError(POVMSObjectPtr, POVMSObjectPtr, int);
		void DebugInfo(POVMSObjectPtr, POVMSObjectPtr, int);

		void FileMessage(int, POVMSObjectPtr);

		const char *GetOptionSwitchString(POVMSObjectPtr, POVMSType, bool defaultstate= false);
	private:
		const unsigned int output_string_buffer_size;
		char *output_string_buffer;
		char status_string_buffer[80];
};

END_POV_FRONTEND_NAMESPACE

#endif
