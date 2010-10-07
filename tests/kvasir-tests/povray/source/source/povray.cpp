/****************************************************************************
 *               povray.cpp
 *
 * This module contains the entry routine for the raytracer and the code to
 * parse the parameters on the command line.
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
 * $File: //depot/povray/3.6-release/source/povray.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <ctype.h>
#include <time.h>
#include <algorithm>

#include "frame.h"
#include "bezier.h"
#include "blob.h"
#include "bbox.h"
#include "cones.h"
#include "csg.h"
#include "discs.h"
#include "express.h"
#include "fnpovfpu.h"
#include "fractal.h"
#include "hfield.h"
#include "lathe.h"
#include "lighting.h"
#include "lightgrp.h"
#include "mesh.h"
#include "photons.h"
#include "polysolv.h"
#include "objects.h"
#include "octree.h"
#include "parse.h"
#include "parstxtr.h"
#include "pigment.h"
#include "point.h"
#include "poly.h"
#include "polygon.h"
#include "povray.h"
#include "optout.h"
#include "quadrics.h"
#include "prism.h"
#include "radiosit.h"
#include "render.h"
#include "sor.h"
#include "spheres.h"
#include "super.h"
#include "targa.h"
#include "texture.h"
#include "tokenize.h"
#include "torus.h"
#include "triangle.h"
#include "truetype.h"
#include "userio.h"
#include "userdisp.h"
#include "lbuffer.h"
#include "vbuffer.h"
#include "povmsend.h"
#include "povmsrec.h"
#include "isosurf.h"
#include "sphsweep.h"
#include "pov_util.h"
#include "renderio.h"
#include "statspov.h"
#include "pov_err.h"
#include "optout.h"
#include "povms.h"
#include "rendctrl.h"
#include "platformbase.h"

#if(USE_LOCAL_POVMS_OUTPUT == 1)
	#include "defaultrenderfrontend.h"
	#include "defaultplatformbase.h"
	#include "processrenderoptions.h"

	USING_POV_FRONTEND_NAMESPACE
	USING_POV_BASE_NAMESPACE
#endif

USING_POV_NAMESPACE

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

#ifndef POVMS_ALLOW_BINARY_MODE
	#ifdef ALTMAIN
		#define POVMS_ALLOW_BINARY_MODE 0
	#else
		#define POVMS_ALLOW_BINARY_MODE 1
	#endif
#endif


/*****************************************************************************
* Local typedefs
******************************************************************************/


/*****************************************************************************
* Global variables
******************************************************************************/

END_POV_NAMESPACE

bool Binary_POVMS_Stream_Mode = false; // GLOBAL VARIABLE

#ifdef POVRAY_COOPERATE_GLOBAL
POVRAY_COOPERATE_GLOBAL
#endif

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local variables
******************************************************************************/

// povray_init
int pre_init_flag = 0; // GLOBAL VARIABLE

// Used for povray_cooperate tricks
int Cooperate_Render_Flag = 0; // GLOBAL VARIABLE

END_POV_NAMESPACE

// Used for POVMS message receiving
POVMSContext POVMS_Render_Context = NULL; // GLOBAL VARIABLE

// Used for POVMS message sending
#if(USE_LOCAL_POVMS_OUTPUT == 1)
POVMSContext POVMS_Output_Context = NULL; // GLOBAL VARIABLE
#endif

// Platform specific function interface self reference pointer
PlatformBase *POV_BASE_NAMESPACE::PlatformBase::self = NULL; // GLOBAL VARIABLE

/*****************************************************************************
*
* FUNCTION
*
*   main
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

#ifndef ALTMAIN
int main(int argc, char **argv)
{
	DefaultPlatformBase platformbase;
	POVMSAddress addr = POVMSInvalidAddress;
	int err = kNoErr;
	int ret = 0;
	int i = 0;

	// Init
	povray_init();

	if(err == kNoErr)
		err = POVMS_GetContextAddress(POVMS_Render_Context, &addr);
	if(err != kNoErr)
         (void)POVMS_ASSERT_OUTPUT("Accessing POVMS render context failed.", "povray.cpp", 0);
	if(err == kNoErr)
		err = POVMS_OpenContext(&POVMS_Output_Context);
	if(err != kNoErr)
         (void)POVMS_ASSERT_OUTPUT("Creating POVMS output context failed.", "povray.cpp", 0);
	else
	{
		DefaultRenderFrontend frontend(POVMS_Output_Context, addr);

		argc = GETCOMMANDLINE(argc, argv);

		#if(POVMS_ALLOW_BINARY_MODE == 1)
			// Binary control mode via POVMS on stdin and stdout
			if((argc > 1) && (pov_stricmp(argv[1], "-povms") == 0))
				Binary_POVMS_Stream_Mode = true;
		#endif

		// Print help screens
		if(argc == 1)
		{
			frontend.PrintHelpScreens();
			return 0;
		}
		else if(argc == 2)
		{
			if((pov_stricmp(argv[1], "-h") == 0) ||
			   (pov_stricmp(argv[1], "-?") == 0) ||
			   (pov_stricmp(argv[1], "--help") == 0) ||
			   (pov_stricmp(argv[1], "-help") == 0))
			{
				frontend.PrintHelpScreens();
				return 0;
			}
			else if(argv[1][0] == '-')
			{
				if(argv[1][1] == '?')
				{
					frontend.PrintUsage(argv[1][2] - '0');
					return 0;
				}
				else if(strlen(argv[1]) == 6)
				{
					if(((argv[1][1] == 'h') || (argv[1][1] == 'H')) &&
					   ((argv[1][2] == 'e') || (argv[1][2] == 'E')) &&
					   ((argv[1][3] == 'l') || (argv[1][3] == 'L')) &&
					   ((argv[1][4] == 'p') || (argv[1][4] == 'P')))
					{
						frontend.PrintUsage(argv[1][5] - '0');
						return 0;
					}
				}
			}
		}

		// Render
		#if(POVMS_ALLOW_BINARY_MODE == 1)
			// Binary control mode via POVMS on stdin and stdout
			if((argc > 1) && (pov_stricmp(argv[1], "-povms") == 0))
				Binary_POVMS_Stream_Mode = true;

			if(Binary_POVMS_Stream_Mode == true)
			{
				while(Cooperate_Render_Flag >= 0)
					povray_cooperate();
				return 0;
			}
		#endif

		try
		{
			ProcessRenderOptions renderoptions;
			POVMSObject obj;
			int l = 0;

			err = POVMSObject_New(&obj, kPOVObjectClass_ROptions);
			if(err != kNoErr)
				throw err;

			for(i = 1 ;i < argc; i++)
			{
				if(pov_stricmp(argv[i], "-povms") != 0)
				{
					err = renderoptions.ParseString(argv[i], &obj, true);
					if(err != kNoErr)
						throw err;
				}
			}

			if(POVMSUtil_GetStringLength(&obj, kPOVAttrib_CreateIni, &l) == kNoErr)
			{
				char *outputini = new char[l];
				if(POVMSUtil_GetString(&obj, kPOVAttrib_CreateIni, outputini, &l) == kNoErr)
					renderoptions.WriteFile(outputini, &obj);
			}

			POVMS_Object optionsobj(obj);
			frontend.StartRender(optionsobj);

			while(frontend.GetState() != RenderFrontend::kReady)
				povray_cooperate();
		}
		catch(int err)
		{
			fprintf(stderr, "Failed to render file due to error(s)!\n");
			return err;
		}
		catch(const char *str)
		{
			fprintf(stderr, "%s\n Failed to render file!\n", str);
			return -1;
		}
		catch(...)
		{
			fprintf(stderr, "Failed to render file due to error(s)!\n");
			return -1;
		}

		// NOTE: It is important that 'frontend' be destroyed in this block scope because
		// 'POVMS_CloseContext' will destroy the render context too early otherwise!
	}

	// Finish
	povray_terminate();

	(void)POVMS_CloseContext(POVMS_Output_Context);

	return ret;
}
#endif


/*****************************************************************************
*
* FUNCTION
*
*   povray_init
*
* INPUT -- none
*
* OUTPUT
*
* RETURNS
*
* AUTHOR -- CEY
*
* DESCRIPTION
*
*   This routine does essential initialization that is required before any
*   POV_MALLOC-like routines may be called and before any text streams and
*   the POVMS may be used.
*   
*   If you are not using any built-in main and need access to any part of
*   the generic code before povray_render is called, you MUST call this routine
*   first!  Also note that it is safe to call it twice. If you don't call it,
*   povray_render will. It won't hurt if you both do it.
*   
* CHANGES
*   Nov 1995 : Created by CEY
*
******************************************************************************/

void povray_init()
{
   Stage = STAGE_PREINIT;

   if (pre_init_flag == 0)
   {
      int err;

      Cooperate_Render_Flag = 0;

      err = POVMS_OpenContext(&POVMS_Render_Context);
      if(err == 0)
         err = POVMS_InstallReceiver(POVMS_Render_Context, Receive_RenderOptions, kPOVMsgClass_RenderControl, kPOVMsgIdent_RenderOptions, NULL);
      if(err == 0)
         err = POVMS_InstallReceiver(POVMS_Render_Context, Receive_RenderAll, kPOVMsgClass_RenderControl, kPOVMsgIdent_RenderAll, NULL);
      if(err == 0)
         err = POVMS_InstallReceiver(POVMS_Render_Context, Receive_RenderArea, kPOVMsgClass_RenderControl, kPOVMsgIdent_RenderArea, NULL);
      if(err == 0)
         err = POVMS_InstallReceiver(POVMS_Render_Context, Receive_RenderStop, kPOVMsgClass_RenderControl, kPOVMsgIdent_RenderStop, NULL);

      if(err != 0)
         (void)POVMS_ASSERT_OUTPUT("Installing POVMS receive handler functions failed.", "povray.cpp", 0);
   }

   /* Initialize memory. */
   POV_MEM_INIT();

   pre_init_tokenizer();

   pre_init_flag = 1234;
}


/*****************************************************************************
*
* FUNCTION
*
*   povray_terminate
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void povray_terminate()
{
   close_all();
   POV_MEM_RELEASE_ALL();

   (void)POVMS_CloseContext(POVMS_Render_Context);
   pre_init_flag = 0;

   FINISH_POVRAY;
}


/*****************************************************************************
*
* FUNCTION
*
*   povray_exit
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void povray_exit(int i)
{
	if(Stop_Flag)
	{
		Send_Progress("Aborting render!", PROGRESS_ABORTING_RENDER);

		if(POV_SHELLOUT(USER_ABORT_SHL) != FATAL_RET)
			Send_Progress("User abort", PROGRESS_USER_ABORT);
		else // Do *not* call "Error" here because it would in turn call povray_exit! [trf]
			PossibleError("Fatal error in User_Abort_Command.");
	}

	if(Stage == STAGE_PARSING) 
	{
		Terminate_Tokenizer();
		Destroy_Textures(Default_Texture); 
		Destroy_Camera(Default_Camera); 
	}

	Do_Cooperate(0);

	pre_init_flag = 1;

	Cooperate_Render_Flag = 3;

	EXIT_POVRAY(i); /* Must call exit(i) or somehow stop */
}


/*****************************************************************************
*
* FUNCTION
*
*   povray_cooperate
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void povray_cooperate()
{
   bool withtime = false;

   POVRAY_BEGIN_COOPERATE

   switch(Cooperate_Render_Flag)
   {
      case 0:
         Cooperate_Render_Flag = 1;

         // Call init
         povray_init();

         // Startup povray
         Stage = STAGE_STARTUP;
         STARTUP_POVRAY

         // Print banner and credit info
         Stage = STAGE_BANNER;
         Send_InitInfo();

         // Initialize variables
         init_vars();
         break;
      case 1:
         // Take option setting message, only render message receive
         // handler switches to next state
         (void)POVMS_ProcessMessages(POVMS_Render_Context, true);
         break;
      case 2:
         // RenderDone message will include time statistics
         withtime = true;

         // Send start notification
         Send_RenderStarted(opts.Options & CONTINUE_TRACE);

         // Strip path and extension off input name to create scene name
         fix_up_scene_name();
         // Make sure clock is okay, validate animation parameters
         fix_up_animation_values();
         // Fix-up rendering window values if necessary
         fix_up_rendering_window();

         // Set output file handle for options screen
         init_output_file_handle();
         // Print options used.
         Send_RenderOptions();

         // Enter the frame loop
         FrameLoop();

         // Drop through (intentional)
      case 3:
         // Clean up
         close_all();

         // Free all memory
         POV_MEM_RELEASE_ALL();

         // Send end notification
         Send_RenderDone(withtime);

         withtime = false;
         Cooperate_Render_Flag = 1;

         #if(POVMS_ALLOW_BINARY_MODE == 1)
            if(Binary_POVMS_Stream_Mode == false)
            {
               Cooperate_Render_Flag = -1;
               break;
            }
         #endif

         init_vars();
         break;
   }

   POVRAY_END_COOPERATE

   Do_Cooperate(2);
}


/*****************************************************************************
*
* FUNCTION
*
*   povray_getoutputcontext
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

#if(USE_LOCAL_POVMS_OUTPUT == 1)
POVMSAddress povray_getoutputcontext()
{
	POVMSAddress addr = POVMSInvalidAddress;

	if(POVMS_GetContextAddress(POVMS_Output_Context, &addr) != kNoErr)
		return POVMSInvalidAddress;

	return addr;
}
#endif


BEGIN_POV_NAMESPACE

/*****************************************************************************
*
* FUNCTION
*
*   Do_Cooperate
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Do_Cooperate(int level)
{
   switch(level)
   {
      case 2:
      default:
         COOPERATE_2
      case 0:
         while(POVMS_ProcessMessages(POVMS_Render_Context, false) == kFalseErr)
         {
         }
#if(USE_LOCAL_POVMS_OUTPUT == 1)
         while(POVMS_ProcessMessages(POVMS_Output_Context, false) == kFalseErr)
         {
         }
#endif
         COOPERATE_0
         break;
      case 1:
         COOPERATE_1
         break;
   };
}

END_POV_NAMESPACE
