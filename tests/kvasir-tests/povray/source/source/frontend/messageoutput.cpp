/****************************************************************************
 *               messageoutput.h
 *
 * This module contains the basic C++ interface for message output.
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
 * $File: //depot/povray/3.6-release/source/frontend/messageoutput.cpp $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#include <algorithm>

#include "configfrontend.h"

#include "messageoutput.h"
#include "povmsgid.h"
#include "pov_err.h"

BEGIN_POV_FRONTEND_NAMESPACE

const int Num_Echo_Lines = 5; // FIXME

MessageOutput::MessageOutput(POVMSContext context) : POVMS_MessageReceiver(context), output_string_buffer_size(1024 * 8)
{
	output_string_buffer = new char[output_string_buffer_size];

	for(int i = 0; i < MAX_STREAMS; i++)
	{
		streams[i] = NULL;
		streamnames[i] = NULL;
		consoleoutput[i] = false;
	}

	InstallFront(kPOVMsgClass_Miscellaneous, kPOVMsgIdent_InitInfo, this,  &MessageOutput::InitInfo);
	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_RenderOptions, this, &MessageOutput::RenderOptions);
	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_RenderStarted, this, &MessageOutput::RenderStarted);
	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_FrameStatistics, this, &MessageOutput::FrameStatistics);
	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_ParseStatistics, this, &MessageOutput::ParseStatistics);
	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_RenderStatistics, this, &MessageOutput::RenderStatistics);
	InstallBack(kPOVMsgClass_RenderOutput, kPOVMsgIdent_RenderDone, this, &MessageOutput::RenderDone);
	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_Progress, this, &MessageOutput::Progress);
	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_Warning, this, &MessageOutput::Warning);
	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_Error, this, &MessageOutput::Error);
	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_FatalError, this, &MessageOutput::FatalError);
	InstallFront(kPOVMsgClass_RenderOutput, kPOVMsgIdent_Debug, this, &MessageOutput::DebugInfo);
}

MessageOutput::~MessageOutput()
{
	delete[] output_string_buffer;
}

void MessageOutput::Printfile(int stream, const char *filename, unsigned long offset, int lines)
{
	if(streams[stream] != NULL)
		streams[stream]->printfile(filename, offset, lines);
	if(streams[ALL_STREAM] != NULL)
		streams[ALL_STREAM]->printfile(filename, offset, lines);
}

void MessageOutput::Printf(int stream, const char *format, ...)
{
	va_list marker;
	char localvsbuffer[1024];

	va_start(marker, format);
	vsnprintf(localvsbuffer, 1023, format, marker);
	va_end(marker);

	if(streams[stream] != NULL)
		streams[stream]->print(localvsbuffer);
	if(streams[ALL_STREAM] != NULL)
		streams[ALL_STREAM]->print(localvsbuffer);
}

void MessageOutput::Flush(int stream)
{
	if(streams[stream] != NULL)
		streams[stream]->flush();
	if(streams[ALL_STREAM] != NULL)
		streams[ALL_STREAM]->flush();
}

void MessageOutput::InitInfo(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	const int NUMBER_OF_AUTHORS_ACROSS = 4;
	POVMSAttributeList attrlist;
	POVMSAttribute item;
	char charbuf[1024];
	int h, i, j;
	int cnt;
	int l;

	Flush(STATUS_STREAM);
	Flush(DEBUG_STREAM);

	l = 1024;
	charbuf[0] = 0;
	if(POVMSUtil_GetString(msg, kPOVAttrib_CoreVersion, charbuf, &l) == kNoErr)
		Printf(BANNER_STREAM, "%s\n", charbuf);

	l = 1024;
	charbuf[0] = 0;
	if(POVMSUtil_GetString(msg, kPOVAttrib_EnglishText, charbuf, &l) == kNoErr)
		Printf(BANNER_STREAM, "%s\n", charbuf);

	Printf(BANNER_STREAM, "\n");

	Printf(BANNER_STREAM, "Primary POV-Ray 3.5/3.6 Developers: (Alphabetically)\n");

	if(POVMSObject_Get(msg, &attrlist, kPOVAttrib_PrimaryDevs) == kNoErr)
	{
		cnt = 0;

		if(POVMSAttrList_Count(&attrlist, &cnt) == kNoErr)
		{
			for(i = 0, h = 1; h <= cnt; i++)
			{
				for(j = 0; (j < NUMBER_OF_AUTHORS_ACROSS) && (h <= cnt); j++, h++)
				{
					if(POVMSAttrList_GetNth(&attrlist, h, &item) == kNoErr)
					{
						l = 1023;
						charbuf[0] = 0;
						if(POVMSAttr_Get(&item, kPOVMSType_CString, charbuf, &l) == kNoErr)
							Printf(BANNER_STREAM, "  %-18s", charbuf);

						(void)POVMSAttr_Delete(&item);
					}
				}
				Printf(BANNER_STREAM, "\n");
			}
		}

		(void)POVMSAttrList_Delete(&attrlist);
	}

    Printf(BANNER_STREAM, "\n");
    Printf(BANNER_STREAM, "Contributing Authors: (Alphabetically)\n");

	if(POVMSObject_Get(msg, &attrlist, kPOVAttrib_ContributingDevs) == kNoErr)
	{
		cnt = 0;

		if(POVMSAttrList_Count(&attrlist, &cnt) == kNoErr)
		{
			for(i = 0, h = 1; h <= cnt; i++)
			{
				for(j = 0; (j < NUMBER_OF_AUTHORS_ACROSS) && (h <= cnt); j++, h++)
				{
					if(POVMSAttrList_GetNth(&attrlist, h, &item) == kNoErr)
					{
						l = 1023;
						charbuf[0] = 0;
						if(POVMSAttr_Get(&item, kPOVMSType_CString, charbuf, &l) == kNoErr)
							Printf(BANNER_STREAM, "  %-18s", charbuf);

						(void)POVMSAttr_Delete(&item);
					}
				}
				Printf(BANNER_STREAM, "\n");
			}
		}

		(void)POVMSAttrList_Delete(&attrlist);
	}

    Printf(BANNER_STREAM, "\n");
    Printf(BANNER_STREAM, "Other contributors are listed in the documentation.\n");
	Printf(BANNER_STREAM, "\n");

	if(POVMSObject_Get(msg, &attrlist, kPOVAttrib_ImageLibVersions) == kNoErr)
	{
		cnt = 0;

		if(POVMSAttrList_Count(&attrlist, &cnt) == kNoErr)
		{
			if(cnt > 0)
			{
				Printf(BANNER_STREAM, "Support libraries used by POV-Ray:\n");

				for(i = 1; i <= cnt; i++)
				{
					if(POVMSAttrList_GetNth(&attrlist, i, &item) == kNoErr)
					{
						l = 1023;
						charbuf[0] = 0;
						if(POVMSAttr_Get(&item, kPOVMSType_CString, charbuf, &l) == kNoErr)
							Printf(BANNER_STREAM, "  %s\n", charbuf);

						(void)POVMSAttr_Delete(&item);
					}
				}
			}
		}

		(void)POVMSAttrList_Delete(&attrlist);
	}
}

void MessageOutput::RenderOptions(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	POVMSAttribute attr;
	POVMSInt i, i2;
	POVMSFloat f, f2, f3, f4;
	POVMSBool b;
	char charbuf[1024];
	char *t;
	int outputQuality;
	int outputCompression;
	int l;
	char outputFormat;

	Flush(STATUS_STREAM);
	Flush(DEBUG_STREAM);

	Printf(RENDER_STREAM, "Parsing Options\n");

	f = 0.0;
	l = 1024;
	charbuf[0] = 0;
	(void)POVMSUtil_GetString(msg, kPOVAttrib_InputFile, charbuf, &l);
	(void)POVMSUtil_GetFloat(msg, kPOVAttrib_Version, &f);
	Printf(RENDER_STREAM, "  Input file: %s (compatible to version %1.2f)\n", charbuf, (double)f);
	Printf(RENDER_STREAM, "  Remove bounds.......%s\n  Split unions........%s\n",
	              GetOptionSwitchString(msg, kPOVAttrib_RemoveBounds), GetOptionSwitchString(msg, kPOVAttrib_SplitUnions));

	Printf(RENDER_STREAM, "  Library paths:\n");
	if(POVMSObject_Get(msg, &attr, kPOVAttrib_LibraryPath) == kNoErr)
	{
		int cnt = 0;

		if(POVMSAttrList_Count(&attr, &cnt) == kNoErr)
		{
			POVMSAttribute item;
			long ii;

			for(ii = 1; ii <= cnt; ii++)
			{
				if(POVMSAttrList_GetNth(&attr, ii, &item) == kNoErr)
				{
					l = 1023;
					charbuf[0] = 0;
					(void)POVMSAttr_Get(&item, kPOVMSType_CString, charbuf, &l);
					Printf(RENDER_STREAM, "    %s\n", charbuf);

					(void)POVMSAttr_Delete(&item);
				}
			}
		}

		(void)POVMSAttr_Delete(&attr);
	}

	Printf(RENDER_STREAM, "Output Options\n");

	(void)POVMSUtil_GetInt(msg, kPOVAttrib_Width, &i);
	(void)POVMSUtil_GetInt(msg, kPOVAttrib_Height, &i2);
	(void)POVMSUtil_GetFloat(msg, kPOVAttrib_StartRow, &f);
	(void)POVMSUtil_GetFloat(msg, kPOVAttrib_EndRow, &f2);
	(void)POVMSUtil_GetFloat(msg, kPOVAttrib_StartColumn, &f3);
	(void)POVMSUtil_GetFloat(msg, kPOVAttrib_EndColumn, &f4);
	Printf(RENDER_STREAM, "  Image resolution %d by %d (rows %d to %d, columns %d to %d).\n",
	              (int)i, (int)i2, (int)(f + 1), (int)f2, (int)(f3 + 1), (int)f4);

	if(POVMSUtil_GetInt(msg, kPOVAttrib_OutputFileType, &i) == kNoErr)
		outputFormat = (char)tolower(i);
	if(POVMSUtil_GetInt(msg, kPOVAttrib_BitsPerColor, &i) == kNoErr)
		outputQuality = i;
	if(POVMSUtil_GetInt(msg, kPOVAttrib_Compression, &i) == kNoErr)
	{
		if(toupper(outputFormat) == 'J')
		{
			outputQuality = i;
			outputQuality = max(0, outputQuality);
			outputQuality = min(100, outputQuality);
		}
	}

	b = false;
	(void)POVMSUtil_GetBool(msg, kPOVAttrib_OutputToFile, &b);
	if(b == true)
	{
		char charbuf2[1024];
		char *al = "";

		l = 1023;
		charbuf2[0] = 0;
		(void)POVMSUtil_GetString(msg, kPOVAttrib_OutputPath, charbuf2, &l);
		l = 1023;
		charbuf[0] = 0;
		(void)POVMSUtil_GetString(msg, kPOVAttrib_OutputFile, charbuf, &l);
		b = false;
		(void)POVMSUtil_GetBool(msg, kPOVAttrib_OutputAlpha, &b);

		if(toupper(outputFormat) == 'J')
		{
			outputCompression = outputQuality;
			outputQuality = 8;
		}

		if(b == true)
		{
			outputQuality *= 4;
			al = " with alpha";
		}
		else
			outputQuality *= 3;

		switch(toupper(outputFormat))
		{
			case 'C': t = "RLE Targa";       break;
			case 'N': t = "PNG";             break;
			case 'J': t = "JPEG";            break;
			case 'P': t = "PPM";             break;
			case 'S': t = "(system format)"; break;
			case 'T': t = "Targa";           break;
			default:  t = "(none)";          break;
		}

		if(toupper(outputFormat) == 'J')
			Printf(RENDER_STREAM, "  Output file: %s%s, %d bpp, quality %d%s%s %s\n", charbuf2, charbuf, outputQuality, outputCompression, "%", al, t);
		else
			Printf(RENDER_STREAM, "  Output file: %s%s, %d bpp%s %s\n", charbuf2, charbuf, outputQuality, al, t);
	}

	b = false;
	(void)POVMSUtil_GetBool(msg, kPOVAttrib_Display, &b);
	if(b == true)
	{
		f = 0.0;
		(void)POVMSUtil_GetFloat(msg, kPOVAttrib_DisplayGamma, &f);
		Printf(RENDER_STREAM, "  Graphic display......On  (gamma: %g)\n", (float)f);
	}
	else
		Printf(RENDER_STREAM, "  Graphic display......Off\n");

	i = 0;
	(void)POVMSUtil_GetInt(msg, kPOVAttrib_PreviewStartSize, &i);
	if(i > 1)
	{
		i2 = 0;
		(void)POVMSUtil_GetInt(msg, kPOVAttrib_PreviewEndSize, &i2);
		Printf(RENDER_STREAM, "  Mosaic preview.......On  (pixel sizes %d to %d)\n", (int)i, (int)i2);
	}
	else
		Printf(RENDER_STREAM, "  Mosaic preview.......Off\n");

	b = false;
	(void)POVMSUtil_GetBool(msg, kPOVAttrib_CreateHistogram, &b);
	if(b == true)
	{
		if(POVMSUtil_GetInt(msg, kPOVAttrib_HistogramFileType, &i) == kNoErr)
		{
			switch(i)
			{
		         case 'C':
		         case 'c':
					t = "CSV";
					break;
		         case 'T':
		         case 't':
					t = "TGA";
					break;
		         case 'N':
		         case 'n':
					t = "PNG";
					break;
		         case 'P' :
		         case 'p' :
					t = "PPM";
					break;
		         case 'S':
		         case 's':
					t = "(system format)";
					break;
				default:    
					t = "(none)";
					break;
			}

			i = 0;
			i2 = 0;
			l = 1023;
			charbuf[0] = 0;
			(void)POVMSUtil_GetInt(msg, kPOVAttrib_HistogramGridSizeX, &i);
			(void)POVMSUtil_GetInt(msg, kPOVAttrib_HistogramGridSizeY, &i2);
			(void)POVMSUtil_GetString(msg, kPOVAttrib_HistogramFile, charbuf, &l);
			Printf(RENDER_STREAM, "  CPU usage histogram..On  (name: %s type: %s, grid: %dx%d)\n",
			              charbuf, t, (int)i, (int)i2);
		}
	}
	else
		Printf(RENDER_STREAM, "  CPU usage histogram..Off\n");

	Printf(RENDER_STREAM, "  Continued trace.....%s\n", GetOptionSwitchString(msg, kPOVAttrib_ContinueTrace));

	Printf(RENDER_STREAM, "Tracing Options\n");

	i = 0;
	(void)POVMSUtil_GetInt(msg, kPOVAttrib_Quality, &i);
	Printf(RENDER_STREAM, "  Quality: %2d\n", (int)i);

	b = false;
	(void)POVMSUtil_GetBool(msg, kPOVAttrib_Bounding, &b);
	if(b == true)
	{
		i = 0;
		(void)POVMSUtil_GetInt(msg, kPOVAttrib_BoundingThreshold, &i);
		Printf(RENDER_STREAM, "  Bounding boxes.......On   Bounding threshold: %d\n", (int)i);
	}
	else
		Printf(RENDER_STREAM, "  Bounding boxes.......Off\n");

	Printf(RENDER_STREAM, "  Light Buffer........%s\n", GetOptionSwitchString(msg, kPOVAttrib_LightBuffer));
	Printf(RENDER_STREAM, "  Vista Buffer........%-3s", GetOptionSwitchString(msg, kPOVAttrib_VistaBuffer));
	b = false;
	(void)POVMSUtil_GetBool(msg, kPOVAttrib_VistaBuffer, &b);
	if(b == true)
		Printf(RENDER_STREAM, "  Draw Vista Buffer...%s", GetOptionSwitchString(msg, kPOVAttrib_DrawVistas));
	Printf(RENDER_STREAM, "\n");

	b = false;
	(void)POVMSUtil_GetBool(msg, kPOVAttrib_Antialias, &b);
	if(b == true)
	{
		i = 0;
		i2 = 0;
		f = 0.0;
		f2 = 0.0;
		(void)POVMSUtil_GetInt(msg, kPOVAttrib_SamplingMethod, &i);
		(void)POVMSUtil_GetInt(msg, kPOVAttrib_AntialiasDepth, &i2);
		(void)POVMSUtil_GetFloat(msg, kPOVAttrib_AntialiasThreshold, &f);
		(void)POVMSUtil_GetFloat(msg, kPOVAttrib_JitterAmount, &f2);
		Printf(RENDER_STREAM, "  Antialiasing.........On  (Method %d, Threshold %.3f, Depth %d, Jitter %.2f)\n",
		              (int)i, (float)f, (int)i2, (float)f2);
	}
	else
		Printf(RENDER_STREAM, "  Antialiasing.........Off\n");

	i = 1;
	i2 = 1;
	f = 0.0;
	(void)POVMSUtil_GetInt(msg, kPOVAttrib_InitialFrame, &i);
	(void)POVMSUtil_GetInt(msg, kPOVAttrib_FinalFrame, &i2);
	(void)POVMSUtil_GetFloat(msg, kPOVAttrib_Clock, &f);
	if((i != 1) || (i2 != 1) || (i != i2) || (f != 0.0))
	{
		Printf(RENDER_STREAM, "Animation Options\n");
		Printf(RENDER_STREAM, "  Initial Frame: %8d  Final Frame: %8d\n", (int)i, (int)i2);
		f = 0.0;
		f2 = 0.0;
		(void)POVMSUtil_GetFloat(msg, kPOVAttrib_InitialClock, &f);
		(void)POVMSUtil_GetFloat(msg, kPOVAttrib_FinalClock, &f2);
		Printf(RENDER_STREAM, "  Initial Clock: %8.3f  Final Clock: %8.3f\n", (float)f, (float)f2);
		Printf(RENDER_STREAM, "  Cyclic Animation....%s  Field render........%s  Odd lines/frames....%s",
		              GetOptionSwitchString(msg, kPOVAttrib_CyclicAnimation),
		              GetOptionSwitchString(msg, kPOVAttrib_FieldRender),
		              GetOptionSwitchString(msg, kPOVAttrib_OddField));
	}
	else
		Printf(RENDER_STREAM, "  Clock value: %8.3f  (Animation off)", (float)f);
	Printf(RENDER_STREAM, "\n");
}

void MessageOutput::RenderStarted(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	POVMSType streamTypeUtilData[MAX_STREAMS] =
	{
		kPOVMSType_WildCard,
		kPOVMSType_WildCard,
		kPOVAttrib_DebugFile,
		kPOVAttrib_FatalFile,
		kPOVAttrib_RenderFile,
		kPOVAttrib_StatisticsFile,
		kPOVAttrib_WarningFile,
		kPOVAttrib_AllFile
	};
	POVMSBool b;

	b = true;
	if(POVMSObject_Exist(msg, kPOVAttrib_AllConsole) == kNoErr)
		(void)POVMSUtil_GetBool(msg, kPOVAttrib_AllConsole, &b);
	consoleoutput[BANNER_STREAM] =
	consoleoutput[STATUS_STREAM] =
	consoleoutput[DEBUG_STREAM] =
	consoleoutput[FATAL_STREAM] =
	consoleoutput[RENDER_STREAM] =
	consoleoutput[STATISTIC_STREAM] =
	consoleoutput[WARNING_STREAM] =
	consoleoutput[ALL_STREAM] = b;

	b = true;
	if(POVMSObject_Exist(msg, kPOVAttrib_DebugConsole) == kNoErr)
		(void)POVMSUtil_GetBool(msg, kPOVAttrib_DebugConsole, &b);
	consoleoutput[DEBUG_STREAM] = b;

	b = true;
	if(POVMSObject_Exist(msg, kPOVAttrib_FatalConsole) == kNoErr)
		(void)POVMSUtil_GetBool(msg, kPOVAttrib_FatalConsole, &b);
	consoleoutput[FATAL_STREAM] = b;

	b = true;
	if(POVMSObject_Exist(msg, kPOVAttrib_RenderConsole) == kNoErr)
		(void)POVMSUtil_GetBool(msg, kPOVAttrib_RenderConsole, &b);
	consoleoutput[RENDER_STREAM] = b;

	b = true;
	if(POVMSObject_Exist(msg, kPOVAttrib_StatisticsConsole) == kNoErr)
		(void)POVMSUtil_GetBool(msg, kPOVAttrib_StatisticsConsole, &b);
	consoleoutput[STATISTIC_STREAM] = b;

	b = true;
	if(POVMSObject_Exist(msg, kPOVAttrib_WarningConsole) == kNoErr)
		(void)POVMSUtil_GetBool(msg, kPOVAttrib_WarningConsole, &b);
	consoleoutput[WARNING_STREAM] = b;

	for(int i = 0; i < MAX_STREAMS; i++)
	{
		if(POVMSObject_Exist(msg, streamTypeUtilData[i]) == kNoErr)
		{
			int l = 0;
			if(POVMSUtil_GetStringLength(msg, streamTypeUtilData[i], &l) == kNoErr)
			{
				streamnames[i] = new char[l];
				streamnames[i][0] = 0;
				(void)POVMSUtil_GetString(msg, streamTypeUtilData[i], streamnames[i], &l);
			}
			else
				streamnames[i] = NULL;
		}
		else
			streamnames[i] = NULL;
	}

	b = false;
	if(POVMSUtil_GetBool(msg, kPOVAttrib_ContinueTrace, &b) != kNoErr)
		b = false;
	OpenStreams(b);

	Flush(STATUS_STREAM);
	Flush(DEBUG_STREAM);

	Printf(RENDER_STREAM, "Redirecting Options\n");

	Printf(RENDER_STREAM, "  All Streams to console.........%s", GetOptionSwitchString(msg, kPOVAttrib_AllConsole, true));
	if(streamnames[ALL_STREAM] != NULL)
		Printf(RENDER_STREAM, "  and file %s\n", streamnames[ALL_STREAM]);
	else
		Printf(RENDER_STREAM, "\n");

	Printf(RENDER_STREAM, "  Debug Stream to console........%s", GetOptionSwitchString(msg, kPOVAttrib_DebugConsole, true));
	if(streamnames[DEBUG_STREAM] != NULL)
		Printf(RENDER_STREAM, "  and file %s\n", streamnames[DEBUG_STREAM]);
	else
		Printf(RENDER_STREAM, "\n");

	Printf(RENDER_STREAM, "  Fatal Stream to console........%s", GetOptionSwitchString(msg, kPOVAttrib_FatalConsole, true));
	if(streamnames[FATAL_STREAM] != NULL)
		Printf(RENDER_STREAM, "  and file %s\n", streamnames[FATAL_STREAM]);
	else
		Printf(RENDER_STREAM, "\n");

	Printf(RENDER_STREAM, "  Render Stream to console.......%s", GetOptionSwitchString(msg, kPOVAttrib_RenderConsole, true));
	if(streamnames[RENDER_STREAM] != NULL)
		Printf(RENDER_STREAM, "  and file %s\n", streamnames[RENDER_STREAM]);
	else
		Printf(RENDER_STREAM, "\n");

	Printf(RENDER_STREAM, "  Statistics Stream to console...%s", GetOptionSwitchString(msg, kPOVAttrib_StatisticsConsole, true));
	if(streamnames[STATISTIC_STREAM] != NULL)
		Printf(RENDER_STREAM, "  and file %s\n", streamnames[STATISTIC_STREAM]);
	else
		Printf(RENDER_STREAM, "\n");

	Printf(RENDER_STREAM, "  Warning Stream to console......%s", GetOptionSwitchString(msg, kPOVAttrib_WarningConsole, true));
	if(streamnames[WARNING_STREAM] != NULL)
		Printf(RENDER_STREAM, "  and file %s\n", streamnames[WARNING_STREAM]);
	else
		Printf(RENDER_STREAM, "\n");
}

void MessageOutput::FrameStatistics(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	POVMSObject object;
	int ret = 0;
	int i = 0;

	Flush(STATUS_STREAM);
	Flush(DEBUG_STREAM);

	ret = POVMSObject_Get(msg, &object, kPOVAttrib_FrameTime);
	if(ret == kNoErr)
		Printf(STATISTIC_STREAM, "Frame Processing Times\n");

	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(&object, kPOVAttrib_ParseTime, &i);
	if(ret == kNoErr)
		Printf(STATISTIC_STREAM, "  Parse Time:  %3d hours %2d minutes %2d seconds (%d seconds)\n", (int)(i / 3600), (int)((i / 60) % 60), (int)(i % 60), (int)i);

	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(&object, kPOVAttrib_PhotonTime, &i);
	if(ret == kNoErr)
		Printf(STATISTIC_STREAM, "  Photon Time: %3d hours %2d minutes %2d seconds (%d seconds)\n", (int)(i / 3600), (int)((i / 60) % 60), (int)(i % 60), (int)i);

	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(&object, kPOVAttrib_TraceTime, &i);
	if(ret == kNoErr)
		Printf(STATISTIC_STREAM, "  Render Time: %3d hours %2d minutes %2d seconds (%d seconds)\n", (int)(i / 3600), (int)((i / 60) % 60), (int)(i % 60), (int)i);

	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(&object, kPOVAttrib_TotalTime, &i);
	if(ret == kNoErr)
		Printf(STATISTIC_STREAM, "  Total Time:  %3d hours %2d minutes %2d seconds (%d seconds)\n", (int)(i / 3600), (int)((i / 60) % 60), (int)(i % 60), (int)i);

	(void)POVMSObject_Delete(&object);

	if(ret != kNoErr)
		throw ret;
}

void MessageOutput::ParseStatistics(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	POVMSLong ll = 0;
	int ret = kNoErr;
	int l = 0;
	int s = 0;
	int i = 0;

	Flush(STATUS_STREAM);
	Flush(DEBUG_STREAM);

	ret = POVMSUtil_GetInt(msg, kPOVAttrib_FiniteObjects, &s);
	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(msg, kPOVAttrib_InfiniteObjects, &i);
	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(msg, kPOVAttrib_LightSources, &l);
	if(ret == kNoErr)
	{
		Printf(STATISTIC_STREAM, "Scene Statistics\n");
		Printf(STATISTIC_STREAM, "  Finite objects:   %10d\n", s);
		Printf(STATISTIC_STREAM, "  Infinite objects: %10d\n", i);
		Printf(STATISTIC_STREAM, "  Light sources:    %10d\n", l);
		Printf(STATISTIC_STREAM, "  Total:            %10d\n", s + i + l);
	}

	if(ret != kNoErr)
		throw ret;
}

void MessageOutput::RenderStatistics(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	POVMSAttribute attr;
	POVMSLong l, l2;
	POVMSFloat f, f2;
	long Pixels_In_Image;
	int i, i2;

	Flush(STATUS_STREAM);
	Flush(DEBUG_STREAM);

	(void)POVMSUtil_GetInt(msg, kPOVAttrib_Width, &i);
	(void)POVMSUtil_GetInt(msg, kPOVAttrib_Height, &i2);
	Pixels_In_Image = (long)i * (long)i2;

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_Pixels, &l);
	if(Pixels_In_Image > POVMSLongToCDouble(l))
		Printf(STATISTIC_STREAM, "Render Statistics (Partial Image Rendered)\n");
	else
		Printf(STATISTIC_STREAM, "Render Statistics\n");

	Printf(STATISTIC_STREAM, "Image Resolution %d x %d\n", i, i2);

	Printf(STATISTIC_STREAM, "----------------------------------------------------------------------------\n");

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_Pixels, &l);
	(void)POVMSUtil_GetLong(msg, kPOVAttrib_PixelSamples, &l2);
	if(POVMSLongToCDouble(l) > 0.5)
		Printf(STATISTIC_STREAM, "Pixels:  %15.0f   Samples: %15.0f   Smpls/Pxl: %.2f\n",
		              POVMSLongToCDouble(l), POVMSLongToCDouble(l2), POVMSLongToCDouble(l2) / POVMSLongToCDouble(l));
	else
		Printf(STATISTIC_STREAM, "Pixels:  %15.0f   Samples: %15.0f   Smpls/Pxl: -\n",
		              POVMSLongToCDouble(l), POVMSLongToCDouble(l2));

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_Rays, &l);
	(void)POVMSUtil_GetLong(msg, kPOVAttrib_RaysSaved, &l2);
	(void)POVMSUtil_GetInt(msg, kPOVAttrib_TraceLevel, &i);
	(void)POVMSUtil_GetInt(msg, kPOVAttrib_MaxTraceLevel, &i2);
	Printf(STATISTIC_STREAM, "Rays:    %15.0f   Saved:   %15.0f   Max Level: %d/%d\n",
	              POVMSLongToCDouble(l), POVMSLongToCDouble(l2), i, i2);

	Printf(STATISTIC_STREAM, "----------------------------------------------------------------------------\n");
	Printf(STATISTIC_STREAM, "Ray->Shape Intersection          Tests       Succeeded  Percentage\n");
	Printf(STATISTIC_STREAM, "----------------------------------------------------------------------------\n");

	if(POVMSObject_Get(msg, &attr, kPOVAttrib_ObjectIStats) == kNoErr)
	{
		int cnt = 0;

		if(POVMSAttrList_Count(&attr, &cnt) == kNoErr)
		{
			POVMSObject obj;
			int ii, len;
			char str[40];

			for(ii = 1; ii <= cnt; ii++)
			{
				if(POVMSAttrList_GetNth(&attr, ii, &obj) == kNoErr)
				{
					len = 40;
					str[0] = 0;
					(void)POVMSUtil_GetString(&obj, kPOVAttrib_ObjectName, str, &len);
					(void)POVMSUtil_GetLong(&obj, kPOVAttrib_ISectsTests, &l);
					(void)POVMSUtil_GetLong(&obj, kPOVAttrib_ISectsSucceeded, &l2);

					if(POVMSLongToCDouble(l) > 0.5)
					{
						Printf(STATISTIC_STREAM, "%-22s  %14.0f  %14.0f  %8.2f\n", str,
						              POVMSLongToCDouble(l), POVMSLongToCDouble(l2),
						              100.0 * POVMSLongToCDouble(l2) / POVMSLongToCDouble(l));
					}

					(void)POVMSAttr_Delete(&obj);
				}
			}
		}

		(void)POVMSAttr_Delete(&attr);
	}

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_IsoFindRoot, &l);
	(void)POVMSUtil_GetLong(msg, kPOVAttrib_FunctionVMCalls, &l2);
	if((POVMSLongToCDouble(l) > 0.5) || (POVMSLongToCDouble(l2) > 0.5))
	{
		Printf(STATISTIC_STREAM, "----------------------------------------------------------------------------\n");
    	if(POVMSLongToCDouble(l) > 0.5)
    		Printf(STATISTIC_STREAM, "Isosurface roots:   %15.0f\n", POVMSLongToCDouble(l));
    	if(POVMSLongToCDouble(l2) > 0.5)
    		Printf(STATISTIC_STREAM, "Function VM calls:  %15.0f\n", POVMSLongToCDouble(l2));
	}

	Printf(STATISTIC_STREAM, "----------------------------------------------------------------------------\n");

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_PolynomTest, &l);
	if(POVMSLongToCDouble(l) > 0.5)
	{
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_RootsEliminated, &l2);
		Printf(STATISTIC_STREAM, "Roots tested:       %15.0f   eliminated:      %15.0f\n",
		              POVMSLongToCDouble(l), POVMSLongToCDouble(l2));
	}

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_CallsToNoise, &l);
	(void)POVMSUtil_GetLong(msg, kPOVAttrib_CallsToDNoise, &l2);
	Printf(STATISTIC_STREAM, "Calls to Noise:     %15.0f   Calls to DNoise: %15.0f\n",
	              POVMSLongToCDouble(l), POVMSLongToCDouble(l2));

	Printf(STATISTIC_STREAM, "----------------------------------------------------------------------------\n");

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_MediaIntervals, &l);
	if(POVMSLongToCDouble(l) > 0.5)
	{
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_MediaSamples, &l2);
		Printf(STATISTIC_STREAM, "Media Intervals:    %15.0f   Media Samples:   %15.0f (%4.2f)\n",
		              POVMSLongToCDouble(l), POVMSLongToCDouble(l2), POVMSLongToCDouble(l2) / POVMSLongToCDouble(l));
	}

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_ShadowTest, &l);
	if(POVMSLongToCDouble(l) > 0.5)
	{
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_ShadowTestSuc, &l2);

		Printf(STATISTIC_STREAM, "Shadow Ray Tests:   %15.0f   Succeeded:       %15.0f\n",
		              POVMSLongToCDouble(l), POVMSLongToCDouble(l2));
	}

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_ReflectedRays, &l);
	if(POVMSLongToCDouble(l) > 0.5)
	{
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_InnerReflectedRays, &l2);
		if(POVMSLongToCDouble(l2) > 0)
			Printf(STATISTIC_STREAM, "Reflected Rays:     %15.0f   Total Internal:  %15.0f\n",
			              POVMSLongToCDouble(l), POVMSLongToCDouble(l2));
	    else
	    	Printf(STATISTIC_STREAM, "Reflected Rays:     %15.0f\n", POVMSLongToCDouble(l));
	}

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_RefractedRays, &l);
	if(POVMSLongToCDouble(l) > 0.5)
		Printf(STATISTIC_STREAM, "Refracted Rays:     %15.0f\n", POVMSLongToCDouble(l));

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_TransmittedRays, &l);
	if(POVMSLongToCDouble(l) > 0.5)
		Printf(STATISTIC_STREAM, "Transmitted Rays:   %15.0f\n", POVMSLongToCDouble(l));

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_IStackOverflow, &l);
	if(POVMSLongToCDouble(l) > 0.5)
		Printf(STATISTIC_STREAM, "I-Stack overflows:  %15.0f\n", POVMSLongToCDouble(l));

	(void)POVMSUtil_GetInt(msg, kPOVAttrib_RadGatherCount, &i);
	(void)POVMSUtil_GetInt(msg, kPOVAttrib_RadReuseCount, &i2);
	if((i > 0) || (i > 0))
	{
		Printf(STATISTIC_STREAM, "----------------------------------------------------------------------------\n");
		Printf(STATISTIC_STREAM, "Radiosity samples calculated:  %15d (%.2f %%)\n", i, 100.0 * double(i) / double(i + i2));
		Printf(STATISTIC_STREAM, "Radiosity samples reused:      %15d\n", i2);
	}

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_PhotonsShot, &l);
	if(POVMSLongToCDouble(l) > 0.5)
	{
		Printf(STATISTIC_STREAM, "----------------------------------------------------------------------------\n");
		Printf(STATISTIC_STREAM, "Number of photons shot: %15.0f\n", POVMSLongToCDouble(l));
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_PhotonsStored, &l);
		if(POVMSLongToCDouble(l) > 0.5)
			Printf(STATISTIC_STREAM, "Surface photons stored: %15.0f\n", POVMSLongToCDouble(l));
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_MediaPhotonsStored, &l);
		if(POVMSLongToCDouble(l) > 0.5)
			Printf(STATISTIC_STREAM, "Media photons stored:   %15.0f\n", POVMSLongToCDouble(l));
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_GlobalPhotonsStored, &l);
		if(POVMSLongToCDouble(l) > 0.5)
			Printf(STATISTIC_STREAM, "Global photons stored:  %15.0f\n", POVMSLongToCDouble(l));
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_PhotonsPriQInsert, &l);
		if(POVMSLongToCDouble(l) > 0.5)
			Printf(STATISTIC_STREAM, "Priority queue insert:  %15.0f\n", POVMSLongToCDouble(l));
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_PhotonsPriQRemove, &l);
		if(POVMSLongToCDouble(l) > 0.5)
			Printf(STATISTIC_STREAM, "Priority queue remove:  %15.0f\n", POVMSLongToCDouble(l));
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_GatherPerformedCnt, &l);
		if(POVMSLongToCDouble(l) > 0.5)
			Printf(STATISTIC_STREAM, "Gather function called: %15.0f\n", POVMSLongToCDouble(l));
		(void)POVMSUtil_GetLong(msg, kPOVAttrib_GatherExpandedCnt, &l);
		if(POVMSLongToCDouble(l) > 0.5)
			Printf(STATISTIC_STREAM, "Gather radius expanded: %15.0f\n", POVMSLongToCDouble(l));
	}

	Printf(STATISTIC_STREAM, "----------------------------------------------------------------------------\n");

	(void)POVMSUtil_GetLong(msg, kPOVAttrib_MinAlloc, &l);
	Printf(STATISTIC_STREAM, "Smallest Alloc:     %15.0f bytes\n", POVMSLongToCDouble(l));
	(void)POVMSUtil_GetLong(msg, kPOVAttrib_MaxAlloc, &l);
	Printf(STATISTIC_STREAM, "Largest  Alloc:     %15.0f bytes\n", POVMSLongToCDouble(l));

	l = 0;
	l2 = 0;
	(void)POVMSUtil_GetLong(msg, kPOVAttrib_CallsToAlloc, &l);
	(void)POVMSUtil_GetLong(msg, kPOVAttrib_CallsToFree, &l2);
	if(POVMSLongToCDouble(l) > 0.5)
		Printf(STATISTIC_STREAM, "Total Alloc calls:  %15.0f         Free calls:%15.0f\n", POVMSLongToCDouble(l), POVMSLongToCDouble(l2));

	l = 0;
	(void)POVMSUtil_GetLong(msg, kPOVAttrib_PeakMemoryUsage, &l);
	if(POVMSLongToCDouble(l) > 0.5)
		Printf(STATISTIC_STREAM, "Peak memory used:   %15.0f bytes\n", POVMSLongToCDouble(l));
}

void MessageOutput::RenderDone(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	POVMSObject object;
	int ret = 0;
	int i = 0;

	Flush(STATUS_STREAM);
	Flush(DEBUG_STREAM);

	ret = POVMSObject_Get(msg, &object, kPOVAttrib_AnimationTime);
	if(ret == kNoErr)
		Printf(STATISTIC_STREAM, "Total Scene Processing Times\n");

	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(&object, kPOVAttrib_ParseTime, &i);
	if(ret == kNoErr)
		Printf(STATISTIC_STREAM, "  Parse Time:  %3d hours %2d minutes %2d seconds (%d seconds)\n", (int)(i / 3600), (int)((i / 60) % 60), (int)(i % 60), (int)i);

	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(&object, kPOVAttrib_PhotonTime, &i);
	if(ret == kNoErr)
		Printf(STATISTIC_STREAM, "  Photon Time: %3d hours %2d minutes %2d seconds (%d seconds)\n", (int)(i / 3600), (int)((i / 60) % 60), (int)(i % 60), (int)i);

	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(&object, kPOVAttrib_TraceTime, &i);
	if(ret == kNoErr)
		Printf(STATISTIC_STREAM, "  Render Time: %3d hours %2d minutes %2d seconds (%d seconds)\n", (int)(i / 3600), (int)((i / 60) % 60), (int)(i % 60), (int)i);

	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(&object, kPOVAttrib_TotalTime, &i);
	if(ret == kNoErr)
		Printf(STATISTIC_STREAM, "  Total Time:  %3d hours %2d minutes %2d seconds (%d seconds)\n", (int)(i / 3600), (int)((i / 60) % 60), (int)(i % 60), (int)i);

	(void)POVMSObject_Delete(&object);

	CloseStreams();

	if(ret != kNoErr)
		throw ret;
}

void MessageOutput::Progress(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	POVMSLong ll = 0;
	POVMSBool b = false;
	int ret = kNoErr;
	int l = 0;
	int s = 0;

	Flush(DEBUG_STREAM);

	ret = POVMSUtil_GetBool(msg, kPOVAttrib_ProgressStatus, &b);
	if(ret == kNoErr)
		ret = POVMSUtil_GetInt(msg, kPOVAttrib_TotalTime, &s);
	if(ret == kNoErr)
	{
		l = 80;

		if(b == false)
		{
			ret = POVMSUtil_GetString(msg, kPOVAttrib_EnglishText, status_string_buffer, &l);
			if(ret == kNoErr)
				Printf(STATUS_STREAM, "\n%3d:%02d:%02d %s", (int)(s / 3600), (int)((s / 60) % 60), (int)(s % 60), status_string_buffer);
		}
		else // if(opts.Options & VERBOSE) // Should this be part of verbose reporting only or not? I really don't know which way would be better... [trf]
		{
			(void)POVMSUtil_GetString(msg, kPOVAttrib_EnglishText, status_string_buffer, &l);
			Printf(STATUS_STREAM, "\r%3d:%02d:%02d %s", (int)(s / 3600), (int)((s / 60) % 60), (int)(s % 60), status_string_buffer);
		}

// FIXME		if(opts.Options & VERBOSE)
		{
			// animation frame progress
			if(POVMSUtil_GetInt(msg, kPOVAttrib_FrameCount, &l) == kNoErr)
			{
				if(POVMSUtil_GetInt(msg, kPOVAttrib_AbsoluteCurFrame, &s) == kNoErr)
					Printf(STATUS_STREAM, " %d of %d", s, l);
			}
			// parsing progress
			else if((POVMSUtil_GetLong(msg, kPOVAttrib_CurrentToken, &ll) == kNoErr) && (ll > 0))
			{
				Printf(STATUS_STREAM, " %ldK tokens", long(((POV_LONG)(ll))/1000));
			}
			// rendering progress
			else if(POVMSUtil_GetInt(msg, kPOVAttrib_CurrentLine, &l) == kNoErr)
			{
				if(POVMSUtil_GetInt(msg, kPOVAttrib_LineCount, &s) == kNoErr)
					Printf(STATUS_STREAM, " line %d of %d", l, s);
				if(POVMSUtil_GetInt(msg, kPOVAttrib_MosaicPreviewSize, &l) == kNoErr)
					Printf(STATUS_STREAM, " at %dx%d", l, l);
				if(POVMSUtil_GetInt(msg, kPOVAttrib_SuperSampleCount, &l) == kNoErr)
					Printf(STATUS_STREAM, ", %d supersamples", l);
				if(POVMSUtil_GetInt(msg, kPOVAttrib_RadGatherCount, &l) == kNoErr)
					Printf(STATUS_STREAM, ", %d rad. samples", l);
			}
			// photon progress
			else if(POVMSUtil_GetInt(msg, kPOVAttrib_TotalPhotonCount, &l) == kNoErr)
			{
				// sorting
				if(POVMSUtil_GetInt(msg, kPOVAttrib_CurrentPhotonCount, &s) == kNoErr)
					Printf(STATUS_STREAM, " %d of %d", s, l);
				// shooting
				else
				{
					Printf(STATUS_STREAM, " Photons %d", l);
					l = 0;
					(void)POVMSUtil_GetInt(msg, kPOVAttrib_PhotonXSamples, &l);
					s = 0;
					(void)POVMSUtil_GetInt(msg, kPOVAttrib_PhotonYSamples, &s);
					Printf(STATUS_STREAM, " (sampling %dx%d)", l, s);
				}
			}
		}
	}

	if(ret != kNoErr)
		throw ret;
}

void MessageOutput::Warning(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	Flush(STATUS_STREAM);
	Flush(DEBUG_STREAM);

	FileMessage(WARNING_STREAM, msg);
}

void MessageOutput::Error(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	Flush(STATUS_STREAM);
	Flush(DEBUG_STREAM);

	FileMessage(WARNING_STREAM, msg);
}

void MessageOutput::FatalError(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	int ret = kNoErr;
	int l = 0;
	int s = 0;

	Flush(STATUS_STREAM);
	Flush(DEBUG_STREAM);

	if(ret == kNoErr)
		FileMessage(FATAL_STREAM, msg);

	if(ret != kNoErr)
		throw ret;
}

void MessageOutput::DebugInfo(POVMSObjectPtr msg, POVMSObjectPtr, int)
{
	int ret = kNoErr;
	int l = 0;

	Flush(STATUS_STREAM);

	l = output_string_buffer_size;
	output_string_buffer[0] = 0;
	ret = POVMSUtil_GetString(msg, kPOVAttrib_EnglishText, output_string_buffer, &l);
	if(ret == kNoErr)
		Printf(DEBUG_STREAM, "%s\n", output_string_buffer);

	if(ret != kNoErr)
		throw ret;
}

void MessageOutput::FileMessage(int stream, POVMSObjectPtr msg)
{
	POVMSLong ll = 0;
	int ret = kNoErr;
	int l = 0;

	l = output_string_buffer_size;
	output_string_buffer[0] = 0;
	if(POVMSUtil_GetString(msg, kPOVAttrib_FileName, output_string_buffer, &l) == kNoErr)
	{
		if((POVMSUtil_GetInt(msg, kPOVAttrib_Line, &l) == kNoErr) && ((stream == WARNING_STREAM) || (stream == FATAL_STREAM)))
		{
			if((strlen(output_string_buffer) > 0) && (l > 0))
				Printf(stream, "File: %s  Line: %d\n", output_string_buffer, l);
		}
		if(((POVMSUtil_GetLong(msg, kPOVAttrib_FilePosition, &ll) == kNoErr) && (Num_Echo_Lines > 0)) && (stream == FATAL_STREAM))
		{
			Printf(stream, "File Context (%d lines):\n", Num_Echo_Lines);
			Printfile(stream, output_string_buffer, ll, -Num_Echo_Lines);
			Printf(stream, "\n");
		}
	}

	l = output_string_buffer_size;
	output_string_buffer[0] = 0;
	ret = POVMSUtil_GetString(msg, kPOVAttrib_EnglishText, output_string_buffer, &l);
	if(ret == kNoErr)
		Printf(stream, "%s\n", output_string_buffer);

	if(ret != kNoErr)
		throw ret;
}

const char *MessageOutput::GetOptionSwitchString(POVMSObjectPtr msg, POVMSType key, bool defaultstate)
{
	POVMSBool b = false;

	if(POVMSUtil_GetBool(msg, key, &b) != kNoErr)
		b = defaultstate;

	if(b == true)
		return ".On ";

	return ".Off";
}

END_POV_FRONTEND_NAMESPACE
