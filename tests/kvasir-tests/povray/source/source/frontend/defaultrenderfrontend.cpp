/****************************************************************************
 *               defaultrenderfrontend.cpp
 *
 * This module contains the default C++ interface for render frontend.
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
 * $File: //depot/povray/3.6-release/source/frontend/defaultrenderfrontend.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <algorithm>

#include "configfrontend.h"

#include "defaultrenderfrontend.h"
#include "povmsgid.h"
#include "povray.h"

// Number of help pages (numbered 0 to MAX_HELP_PAGE)
#define MAX_HELP_PAGE 7


BEGIN_POV_FRONTEND_NAMESPACE

DefaultRenderFrontend::DefaultRenderFrontend(POVMSContext ctx, POVMSAddress baddr) : RenderFrontend(ctx, baddr)
{
	OpenStreams(false);
}

DefaultRenderFrontend::~DefaultRenderFrontend()
{
	CloseStreams();
}

void DefaultRenderFrontend::BannerPrintf(const char *format, ...)
{
	va_list marker;
	char localvsbuffer[1024];

	va_start(marker, format);
	vsnprintf(localvsbuffer, 1023, format, marker);
	va_end(marker);

	Printf(BANNER_STREAM, "%s", localvsbuffer);
}

void DefaultRenderFrontend::BannerFlush()
{
	Flush(BANNER_STREAM);
}

void DefaultRenderFrontend::PrintHelpScreens()
{
#ifdef GET_KEY_EXISTS
	char c;
	int n, x, ok;

	Print_Usage(-1);

	for (n = 0; ; )
	{
		BannerPrintf("\n");
		BannerPrintf("[ Press 0 for general help, 1 to %d for help screen. Press 'q' to quit. ]", MAX_HELP_PAGE);

		do
		{
			ok = false;

			GET_KEY(x);

			c = (char)x;

			if((c >= '0') && (c <= '0' + MAX_HELP_PAGE))
			{
				ok = true;

				n = (int)c - (int)'0';
			}
			else if((c == 'q') || (c == 'Q'))
				ok = true;
		}
		while(!ok);

		BannerPrintf("\n");

		if((c == 'q') || (c == 'Q'))
			break;

		Print_Usage(n);
	}

#else
	int n;

	for (n = -1; n <= MAX_HELP_PAGE; n++)
		PrintUsage(n);
#endif
}

void DefaultRenderFrontend::PrintUsage(int n)
{
	switch(n)
	{
		// Help screen. 
		case 0:
			BannerPrintf("\n");
			BannerPrintf("Usage: POVRAY [+/-]Option1 [+/-]Option2 ... (-h or -? for help)\n");
			BannerPrintf("\n");
			BannerPrintf("  Example: POVRAY scene.ini +Iscene.pov +Oscene.tga +FT +W320 +H200\n");
			BannerPrintf("  Example: POVRAY +Iscene.pov +Oscene.tga +FT +W160 +H200 +V -D +X\n");
			BannerPrintf("\n");
			BannerPrintf("The n or n.n (0.n) notation following a command-line option listed\n");
			BannerPrintf("below denotes an integer or a floating-point number, respectively.\n");
			BannerPrintf("Brackets mean that this number is optional.\n");
			BannerPrintf("\n");
			BannerPrintf("The help screen is divided into several parts. To access one part\n");
			BannerPrintf("just enter the number of the screen after the -? option or the\n");
			BannerPrintf("-help option.\n");
			BannerPrintf("\n");
			BannerPrintf("E.g. use -?5 or -help5 to see the help screen about the tracing\n");
			BannerPrintf("options.\n");
			BannerPrintf("\n");
			BannerPrintf("  Number  Part\n");
			BannerPrintf("    1     Parsing Options\n");
			BannerPrintf("    2     Output Options\n");
			BannerPrintf("    3     Output Options - display related\n");
			BannerPrintf("    4     Output Options - file related\n");
			BannerPrintf("    5     Tracing Options\n");
			BannerPrintf("    6     Animation Options\n");
			BannerPrintf("    7     Redirecting Options\n");
			break;
		// Parsing options. 
		case 1:
			BannerPrintf("\n");
			BannerPrintf("Parsing options\n");
			BannerPrintf("\n");
			BannerPrintf("  I<name> = input file name\n");
			BannerPrintf("  HI<name>= header include file name\n");
			BannerPrintf("  L<name> = library path prefix\n");
			BannerPrintf("  MVn.n   = set compability to version n.n\n");
			BannerPrintf("  SU      = split bounded unions if children are finite\n");
			BannerPrintf("  UR      = remove unnecessary bounding objects\n");
			break;
		// Output options. 
		case 2:
			BannerPrintf("\n");
			BannerPrintf("Output options\n");
			BannerPrintf("\n");
			BannerPrintf("  Hn      = image height of n pixels\n");
			BannerPrintf("  Wn      = image width of n pixels\n");
			BannerPrintf("\n");
			BannerPrintf("  SRn|0.n = start at row n | start row at n percent of image\n");
			BannerPrintf("  ERn|0.n = end   at row n | end   row at n percent of image\n");
			BannerPrintf("  SCn|0.n = start at col n | start col at n percent of image\n");
			BannerPrintf("  ECn|0.n = end   at col n | end   col at n percent of image\n");
			BannerPrintf("\n");
			BannerPrintf("  C       = continue aborted trace\n");
			BannerPrintf("  P       = pause before exit\n");
			BannerPrintf("  V       = verbose messages on\n");
			BannerPrintf("  WLn     = set warning level to n\n");
			BannerPrintf("  X[n]    = enable early exit by key hit (every n pixels)\n");
			break;
		case 3:
			BannerPrintf("\n");
			BannerPrintf("Output options - display related\n");
			BannerPrintf("\n");
			BannerPrintf("  D[xy]   = display rendering (in format x, using palette option y)\n");
			BannerPrintf("  SPn     = display mosaic preview, start grid size = 2, 4, 8, 16, ...\n");
			BannerPrintf("  EPn     = display mosaic preview, end grid size   = 2, 4, 8, 16, ...\n");
			BannerPrintf("  UD      = draw vista rectangles\n");
			break;
		// Output options - file related. 
		case 4:
			BannerPrintf("\n");
			BannerPrintf("Output options - file related\n");
			BannerPrintf("\n");
			BannerPrintf("  B[n]    = Use buffer (of n KB) for output file\n");
			BannerPrintf("  F[x]    = write output file (in format x)\n");
			BannerPrintf("            FC    - Compressed Targa with 24 or 32 bpp\n");
			BannerPrintf("            FN[n] - PNG (n bits/color, n = 5 to 16, default is 8)\n");
			//      BannerPrintf("            FJn - JPEG (n compression quality, n = 0 to 100, default is 100)\n");
			BannerPrintf("            FP    - PPM\n");
			BannerPrintf("            FS    - System specific\n");
			BannerPrintf("            FT    - Uncompressed Targa with 24 or 32 bpp\n");
			BannerPrintf("  O<name> = output file name\n");
			#if PRECISION_TIMER_AVAILABLE
				BannerPrintf("\n");
				BannerPrintf("  HTx     = write CPU utilization histogram in format x\n");
				BannerPrintf("            HTC - Comma separated values (CSV - spreadsheet)\n");
				BannerPrintf("            HTN - PNG grayscale\n");
				BannerPrintf("            HTP - PPM heightfield\n");
				BannerPrintf("            HTS - System specific\n");
				BannerPrintf("            HTT - Uncompressed TGA heightfield\n");
				BannerPrintf("            HTX - No histogram output\n");
				BannerPrintf("  HN<name>= histogram filename\n");
				BannerPrintf("  HSx.y   = histogram grid number of x, y divisions\n");
			#endif
			break;
		// Tracing options. 
		case 5:
			BannerPrintf("\n");
			BannerPrintf("Tracing options\n");
			BannerPrintf("\n");
			BannerPrintf("  MB[n]   = use bounding slabs (if more than n objects)\n");
			BannerPrintf("  Qn      = image quality (0 = rough, 9 = full)\n");
			//BannerPrintf("  Qn      = image quality (0 = rough, 9 = full, R = radiosity)\n");
			//BannerPrintf("  QR      = enable radiosity calculations for ambient light\n");
			BannerPrintf("\n");
			BannerPrintf("  A[0.n]  = perform antialiasing (if color change is above n percent)\n");
			BannerPrintf("  AMn     = use non-adaptive (n=1) or adaptive (n=2) supersampling\n");
			BannerPrintf("  J[n.n]  = set antialiasing-jitter (and amount)\n");
			BannerPrintf("  Rn      = set antialiasing-depth (use n X n rays/pixel)\n");
			BannerPrintf("\n");
			BannerPrintf("  UA      = use alpha channel\n");
			BannerPrintf("  UL      = use light buffer\n");
			BannerPrintf("  UV      = use vista buffer\n");
			break;
		// Animation options. 
		case 6:
			BannerPrintf("\n");
			BannerPrintf("Animation options\n");
			BannerPrintf("\n");
			BannerPrintf("  Kn.n    = set frame clock to n.n\n");
			BannerPrintf("  KFIn    = set initial frame number to n\n");
			BannerPrintf("  KFFn    = set final frame number to n\n");
			BannerPrintf("  KIn.n   = set initial clock value to n.n\n");
			BannerPrintf("  KFn.n   = set final clock value to n.n\n");
			BannerPrintf("  SFn|0.n = start subset at frame n | start at n percent in sequence\n");
			BannerPrintf("  EFn|0.n = end subset at frame n | end at n percent in sequence\n");
			BannerPrintf("  KC      = calculate clock value for cyclic animation\n");
			BannerPrintf("\n");
			BannerPrintf("  UF      = use field rendering\n");
			BannerPrintf("  UO      = use odd lines in odd frames\n");
			break;
		// Redirecting options. 
		case 7:
			BannerPrintf("\n");
			BannerPrintf("Redirecting options\n");
			BannerPrintf("\n");
			BannerPrintf("  GI<name>= write all .INI parameters to file name\n");
			BannerPrintf("  Gx<name>= write stream x to console (and/or optional file name)\n");
			BannerPrintf("            GA - All streams (except status)\n");
			BannerPrintf("            GD - Debug stream\n");
			BannerPrintf("            GF - Fatal stream\n");
			BannerPrintf("            GR - Render stream\n");
			BannerPrintf("            GS - Statistics stream\n");
			BannerPrintf("            GW - Warning stream\n");
			break;
		// Usage ... 
		default:
			// FIXME
			//	PRINT_CREDITS
			//	PRINT_OTHER_CREDITS
			//	Print_Authors();
			break;
	}

	BannerFlush();

	#if defined(WAIT_FOR_KEYPRESS_EXISTS) && !defined(GET_KEY_EXISTS)
		BannerPrintf("\n");
		BannerPrintf("[ Paused for keypress... ]");
		BannerFlush();

		WAIT_FOR_KEYPRESS;

		BannerPrintf("\n");
	#endif
}

void DefaultRenderFrontend::OpenStreams(bool append)
{
	for(int i = 0; i < MAX_STREAMS; i++)
	{
		if(streams[i] != NULL)
			delete streams[i];
		streams[i] = NULL;

		if(append)
		{
			OStream *os = NULL;

			if(streamnames[i] != NULL)
			{
				os = New_OStream(streamnames[i], POV_File_Text_Stream, true);
				if(os == NULL)
					POV_NAMESPACE::Warning(0, "Could not append stream to file %s.", streamnames[i]);
			}

			streams[i] = new DefaultStreamBuffer(os, (i == ALL_STREAM), (i == ALL_STREAM) || !consoleoutput);
		}
		else
		{
			OStream *os = NULL;

			if(streamnames[i] != NULL)
			{
				os = New_OStream(streamnames[i], POV_File_Text_Stream, false);
				if(os == NULL)
					POV_NAMESPACE::Warning(0, "Could not write stream to file %s.", streamnames[i]);
			}

			streams[i] = new DefaultStreamBuffer(os, (i == ALL_STREAM), (i == ALL_STREAM) || !consoleoutput);
		}
	}
}

void DefaultRenderFrontend::CloseStreams()
{
	for(int i = 0; i < MAX_STREAMS; i++)
	{
		if(streams[i] != NULL)
			delete streams[i];
		streams[i] = NULL;

		if(streamnames[i] != NULL)
			delete[] streamnames[i];
		streamnames[i] = NULL;
	}
}

DefaultRenderFrontend::DefaultStreamBuffer::DefaultStreamBuffer(OStream *h, bool l, bool i)
{
	handle = h;
	linebuffermode = l;
	inhibitmode = i;
}

DefaultRenderFrontend::DefaultStreamBuffer::~DefaultStreamBuffer()
{
	if(handle != NULL)
		delete handle;
}

void DefaultRenderFrontend::DefaultStreamBuffer::lineoutput(const char *str, unsigned int chars)
{
	char buffer[124];

	buffer[0] = 0;
	strncat(buffer, str, min((unsigned int)120, chars));

	if((linebuffermode == true) && (inhibitmode == false))
	{
		fputs(buffer, stderr);
		fputs("\n", stderr);
	}
}

void DefaultRenderFrontend::DefaultStreamBuffer::directoutput(const char *str, unsigned int chars)
{
	char buffer[124];

	buffer[0] = 0;
	strncat(buffer, str, min((unsigned int)120, chars));

	if((linebuffermode == false) && (inhibitmode == false))
		fputs(buffer, stderr);

	if(handle != NULL)
	{
		*handle << buffer;
		handle->flush();
	}
}

END_POV_FRONTEND_NAMESPACE
