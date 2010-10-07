/****************************************************************************
 *               rendctrl.cpp
 *
 * This module contains the control for the raytracer.
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
 * $File: //depot/povray/3.6-release/source/rendctrl.cpp $
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

BEGIN_POV_NAMESPACE

USING_POV_BASE_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

/* Flags for the variable store. */

const int STORE   = 1;
const int RESTORE = 2;

/*****************************************************************************
* Local typedefs
******************************************************************************/


/*****************************************************************************
* Global variables
******************************************************************************/

// Photon map stuff
extern int backtraceFlag; // GLOBAL VARIABLE

extern PHOTON_OPTIONS photonOptions; // GLOBAL VARIABLE

extern int disp_elem; // GLOBAL VARIABLE
extern int disp_nelems; // GLOBAL VARIABLE

extern int warpNormalTextures; // GLOBAL VARIABLE
extern int InitBacktraceWasCalled; // GLOBAL VARIABLE

// The frame and frame related stuff
FRAME Frame; // GLOBAL VARIABLE
DBL Clock_Delta; // GLOBAL VARIABLE

// Statistics stuff
OStream *stat_file; // GLOBAL VARIABLE
COUNTER stats[MaxStat]; // GLOBAL VARIABLE
COUNTER totalstats[MaxStat]; // GLOBAL VARIABLE

// Option stuff
Opts opts; // GLOBAL VARIABLE
char *Option_String_Ptr; // GLOBAL VARIABLE

// File and parsing stuff
int Number_Of_Files; // GLOBAL VARIABLE
Image_File_Class *Output_File; // GLOBAL VARIABLE
int Num_Echo_Lines;      // May make user setable later - CEY // GLOBAL VARIABLE

// Timing stuff
time_t tstart, tstop; // GLOBAL VARIABLE
DBL tparse, tphoton, trender; // GLOBAL VARIABLE
DBL tparse_frame, tphoton_frame, trender_frame; // GLOBAL VARIABLE
DBL tparse_total, tphoton_total, trender_total; // GLOBAL VARIABLE

// Options and display stuff
char Color_Bits; // GLOBAL VARIABLE

// Options and display stuff
int Display_Started; // GLOBAL VARIABLE
int Abort_Test_Every; // GLOBAL VARIABLE
int Experimental_Flag; // GLOBAL VARIABLE

// Current stage of the program
int Stage; // GLOBAL VARIABLE

// Stop flag (for abort etc)
volatile int Stop_Flag; // GLOBAL VARIABLE


/*****************************************************************************
* Local variables
******************************************************************************/

char Actual_Output_Name[FILE_NAME_LENGTH]; // GLOBAL VARIABLE

// Flag if close_all() was already called
int closed_flag; // GLOBAL VARIABLE

int STORE_First_Line; // GLOBAL VARIABLE


/*****************************************************************************
* functions
******************************************************************************/



/*****************************************************************************
*
* FUNCTION
*
*   FrameLoop
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

void FrameLoop()
{
   int Diff_Frame;
   DBL Diff_Clock;
   SHELLRET Pre_Scene_Result, Frame_Result;

   Diff_Clock = opts.FrameSeq.FinalClock - opts.FrameSeq.InitialClock;

   if(opts.Options & CYCLIC_ANIMATION)
      Diff_Frame = opts.FrameSeq.FinalFrame - opts.FrameSeq.InitialFrame + 1;
   else
      Diff_Frame = opts.FrameSeq.FinalFrame - opts.FrameSeq.InitialFrame;

   Clock_Delta = ((Diff_Frame == 0) ? 0 : Diff_Clock/Diff_Frame);

   // Execute the first shell-out command
   Pre_Scene_Result = (POV_SHELLOUT_CAST)POV_SHELLOUT(PRE_SCENE_SHL);

   // Loop over each frame

   if(Pre_Scene_Result != ALL_SKIP_RET)
   {
      if(Pre_Scene_Result != SKIP_ONCE_RET)
      {
         for(opts.FrameSeq.FrameNumber = opts.FrameSeq.InitialFrame,
             opts.FrameSeq.Clock_Value = opts.FrameSeq.InitialClock;

             opts.FrameSeq.FrameNumber <= opts.FrameSeq.FinalFrame;

             // ISO/IEC 14882:1998(E) section 5.18 Comma operator [expr.comma] (page 90) says
             // that comma expressions are evaluated left-to-right, and according to section
             // 6.5.3 The for statement [stmt.for] (page 97) the following is an expression.
             // I just hope all compilers really know about the standard... [trf]
             opts.FrameSeq.FrameNumber++,
             opts.FrameSeq.Clock_Value = opts.FrameSeq.InitialClock +
               (Clock_Delta * (DBL)(opts.FrameSeq.FrameNumber - opts.FrameSeq.InitialFrame)))
         {
            if(opts.FrameSeq.FrameType == FT_MULTIPLE_FRAME)
            {
               START_TIME
               Send_Progress("Processing Frame", PROGRESS_PROCESSING_FRAME);
            }

            setup_output_file_name();

            // Execute a shell-out command before tracing

            Frame_Result = (POV_SHELLOUT_CAST)POV_SHELLOUT(PRE_FRAME_SHL);

            if(Frame_Result == ALL_SKIP_RET)
               break;

            if(Frame_Result != SKIP_ONCE_RET)
            {
               FrameRender();

               // Execute a shell-out command after tracing

               Frame_Result = (POV_SHELLOUT_CAST)POV_SHELLOUT(POST_FRAME_SHL);
           
               if((Frame_Result == SKIP_ONCE_RET) || (Frame_Result == ALL_SKIP_RET))
                  break;
            }

            if(opts.FrameSeq.FrameType == FT_MULTIPLE_FRAME)
               Send_FrameStatistics();

            Do_Cooperate(1);
         }

         // Print total stats ...

         if(opts.FrameSeq.FrameType == FT_MULTIPLE_FRAME)
         {
            opts.FrameSeq.FrameNumber--;

            Send_RenderStatistics(true);

            opts.FrameSeq.FrameNumber++;
         }
      }

      // Execute the final shell-out command
      POV_SHELLOUT(POST_SCENE_SHL);
   }
}

/*****************************************************************************
*
* FUNCTION
*
*   FrameRender
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
*   Do all that is necessary for rendering a single frame, including parsing
*
* CHANGES
*
*   Feb 1996: Make sure we are displaying when doing a mosaic preview [AED]
*
******************************************************************************/

void FrameRender()
{
   // Store start time for parse.
   START_TIME

   Current_Token_Count = 0;
   tparse_frame = tphoton_frame = trender_frame = 0.0;

   // Parse the scene file.
   Send_Progress("Parsing", PROGRESS_PARSING);

   opts.Do_Stats = false;

   // Set up noise-tables
   Initialize_Noise();

   // Set up function VM
   POVFPU_Init();

   // Init module specific stuff.
   Initialize_Mesh_Code();

   Parse();

   opts.Do_Stats = true;

   if (opts.Radiosity_Enabled)
      Experimental_Flag |= EF_RADIOS;

   if (Experimental_Flag)
   {
      char str[512] = "" ;

      if (Experimental_Flag & EF_SPLINE)
        strcat (str, str [0] ? ", spline" : "spline") ;
      if (Experimental_Flag & EF_RADIOS)
        strcat (str, str [0] ? ", radiosity" : "radiosity") ;
      if (Experimental_Flag & EF_SLOPEM)
        strcat (str, str [0] ? ", slope pattern" : "slope pattern") ;
      if (Experimental_Flag & EF_ISOFN) 
        strcat (str, str [0] ? ", function '.hf'" : "function '.hf'") ;
      if (Experimental_Flag & EF_TIFF) 
        strcat (str, str [0] ? ", TIFF image support" : "TIFF image support") ;

      Warning(0, "This rendering uses the following experimental feature(s): %s.\n"
                 "The design and implementation of these features is likely to change in future versions\n"
                 "of POV-Ray. Full backward compatibility with the current implementation is NOT guaranteed.",
                 str);
   }

   Experimental_Flag = 0;

   // Switch off standard anti-aliasing.

   if((Frame.Camera->Aperture != 0.0) && (Frame.Camera->Blur_Samples > 0))
   {
      opts.Options &= ~ANTIALIAS;

      Warning(0, "Focal blur is used. Standard antialiasing is switched off.");
   }

   // Create the bounding box hierarchy.

   Stage = STAGE_SLAB_BUILDING;

   if(opts.Use_Slabs)
      Send_Progress("Creating bounding slabs", PROGRESS_CREATING_BOUNDING_SLABS);

   // Init module specific stuff.
   Initialize_Atmosphere_Code();
   Initialize_BBox_Code();
   Initialize_Lighting_Code();
   Initialize_VLBuffer_Code();
   Initialize_Radiosity_Code();

   // Always call this to print number of objects.
   Build_Bounding_Slabs(&Root_Object);

   // Create the vista buffer.
   Build_Vista_Buffer();

   // Create the light buffers.
   Build_Light_Buffers();

   // Save variable values.
   variable_store(STORE);

   // Get the parsing time.
   STOP_TIME
   tparse = TIME_ELAPSED
   Send_ProgressUpdate(PROGRESS_PARSING, 0);

   // Output parsing statistics.
   Send_ParseStatistics();

   if (photonOptions.photonsEnabled)
   {
     /* Store start time for photons. */
     START_TIME

     /* now backwards-trace the scene and build the photon maps */
     InitBacktraceEverything();
     BuildPhotonMaps();

     /* Get the photon-shooting time. */
     STOP_TIME
     tphoton = TIME_ELAPSED

     /* Get total parsing time. */
     tphoton_total += tphoton;
     tphoton_frame = tphoton;
     tphoton = 0;
   }

   /* Store start time for the rest of parsing. */
   START_TIME
   Stage = STAGE_INIT;

   // Open output file and if we are continuing an interrupted trace,
   // read in the previous file settings and any data there.  This has to
   // be done before any image-size related allocations, since the settings
   // in a resumed file take precedence over that specified by the user. [AED]
   open_output_file();

   // Start the display.
   if(opts.Options & DISPLAY)
   {
      Send_Progress("Displaying", PROGRESS_DISPLAYING);

      Display_Started = POV_DISPLAY_INIT(opts.Preview_RefCon, Frame.Screen_Width, Frame.Screen_Height);

      // Display vista tree.
      Draw_Vista_Buffer();
   }
   else
   {
      Display_Started = false;
   }

   // Get things ready for ray tracing (misc init, mem alloc)
   Initialize_Renderer();

   // This had to be taken out of open_output_file() because we don't have
   // the final image size until the output file has been opened, so we can't
   // initialize the display until we know this, which in turn means we can't
   // read the rendered part before the display is initialized. [AED]
   if((opts.Options & DISKWRITE) && (opts.Options & CONTINUE_TRACE))
   {
      Read_Rendered_Part(Actual_Output_Name);

      if (opts.Last_Line > Frame.Screen_Height)
         opts.Last_Line = Frame.Screen_Height;

      if (opts.Last_Column > Frame.Screen_Width)
         opts.Last_Column = Frame.Screen_Width;
   }

   // Get the rest of the parsing time.
   STOP_TIME
   tparse += TIME_ELAPSED

   // Store start time for trace.
   START_TIME

   // Get total parsing time.
   tparse_total += tparse;
   tparse_frame = tparse;
   tparse = 0;

   // Start tracing.
   Stage = STAGE_RENDERING;

   POV_PRE_RENDER

   Send_Progress("Rendering", PROGRESS_RENDERING);

   // Macro for setting up any special FP options
   CONFIG_MATH

   // Ok, go for it - trace the picture.

   // If radiosity preview has been done, we are continuing a trace, so it
   // is important NOT to do the preview, even if the user requests it, as it
   // will cause discontinuities in radiosity shading by (probably) calculating
   // a few more radiosity values.

   // Note that radiosity REQUIRES a mosaic preview prior to main scan
   if ( opts.Radiosity_Enabled && !opts.Radiosity_Preview_Done)
      Start_Tracing_Radiosity_Preview(opts.PreviewGridSize_Start, opts.PreviewGridSize_End);

   else if((opts.Options & PREVIEW) && (opts.Options & DISPLAY))
      Start_Tracing_Mosaic_Preview(opts.PreviewGridSize_Start, opts.PreviewGridSize_End);

   switch(opts.Tracing_Method)
   {
      case 2:
         Start_Adaptive_Tracing();
         break;
      case 1:
      default:
         Start_Non_Adaptive_Tracing();
   }

   // Record time so well spent before file close so it can be in comments
   STOP_TIME
   trender = TIME_ELAPSED

   // shutdown (freeing memory) does not get included in the time!

   // Get total render time.
   trender_total += trender;
   trender_frame = trender;
   trender = 0;

   // Close out our file
   if(Output_File != NULL)
   {
      delete Output_File;
      Output_File = NULL;
   }

   // For all those who never rtfm [trf]
   if((Highest_Trace_Level >= Max_Trace_Level) && (Had_Max_Trace_Level == false))
      PossibleError("Maximum trace level reached! If your scene contains black spots\nread more about the max_trace_level setting in the documentation!");

   Stage = STAGE_SHUTDOWN;

   POV_PRE_SHUTDOWN

   // DESTROY lots of stuff
   /* NK phmap */
   FreeBacktraceEverything();
   Deinitialize_Atmosphere_Code();
   Deinitialize_BBox_Code();
   Deinitialize_Lighting_Code();
   Deinitialize_Mesh_Code();
   Deinitialize_VLBuffer_Code();
   Deinitialize_Radiosity_Code();
   Destroy_Light_Buffers();
   Destroy_Vista_Buffer();
   Destroy_Bounding_Slabs();
   Destroy_Frame();
   Terminate_Renderer();
   FreeFontInfo();
   Free_Iteration_Stack();
   Free_Noise_Tables();

   POVFPU_Terminate();

   POV_POST_SHUTDOWN

   if((opts.Options & DISPLAY) && Display_Started)
   {
      POV_DISPLAY_FINISHED(opts.Preview_RefCon);

      POV_DISPLAY_CLOSE(opts.Preview_RefCon);

      Display_Started = false;
   }

   if(opts.histogram_on)
      write_histogram(opts.Histogram_File_Name);

   Send_Progress("Done Tracing", PROGRESS_DONE_TRACING);

   // Print stats ...
   Send_RenderStatistics();

   if(opts.FrameSeq.FrameType == FT_MULTIPLE_FRAME)
   {
      // Add them up
      sum_statistics(totalstats, stats);

      // ... and then clear them for the next frame
      init_statistics(stats);
   }

   // Restore variable values.
   variable_store(RESTORE);
}

/*****************************************************************************
*
* FUNCTION
*
*   fix_up_rendering_window
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
*   Fix wrong window and mosaic preview values.
*
* CHANGES
*
*   -
*
******************************************************************************/

void fix_up_rendering_window()
{
  int temp;
  
  if (opts.First_Column_Percent > 0.0)
    opts.First_Column = (int) (Frame.Screen_Width * opts.First_Column_Percent);

  if (opts.First_Line_Percent > 0.0)
    opts.First_Line = (int) (Frame.Screen_Height * opts.First_Line_Percent);

  /* The decrements are a fudge factor that used to be in OPTIN.C
   * but it messed up Write_INI_File so its moved here.
   */

  if (opts.First_Column <= 0)
    opts.First_Column = 0;
  else
    opts.First_Column--;

  if (opts.First_Line <= 0)
    opts.First_Line = 0;
  else
    opts.First_Line--;
  
  if ((opts.Last_Column == -1) && (opts.Last_Column_Percent <= 1.0))
    opts.Last_Column = (int) (Frame.Screen_Width * opts.Last_Column_Percent);

  if ((opts.Last_Line == -1) && (opts.Last_Line_Percent <= 1.0))
    opts.Last_Line = (int) (Frame.Screen_Height * opts.Last_Line_Percent);

  if (opts.Last_Line == -1)
    opts.Last_Line = Frame.Screen_Height;

  if (opts.Last_Column == -1)
    opts.Last_Column = Frame.Screen_Width;

  if (opts.Last_Column < 0 || opts.Last_Column > Frame.Screen_Width)
    opts.Last_Column = Frame.Screen_Width;

  if (opts.Last_Line > Frame.Screen_Height)
    opts.Last_Line = Frame.Screen_Height;

  /* Fix up Mosaic Preview values */
  opts.PreviewGridSize_Start=max(1,opts.PreviewGridSize_Start);
  opts.PreviewGridSize_End=max(1,opts.PreviewGridSize_End);

  if ((temp=closest_power_of_2((unsigned)opts.PreviewGridSize_Start))!=opts.PreviewGridSize_Start)
  {
     Warning(0,"Preview_Start_Size must be a power of 2. Changing to %d.",temp);
     opts.PreviewGridSize_Start=temp;
  }

  if ((temp=closest_power_of_2((unsigned)opts.PreviewGridSize_End))!=opts.PreviewGridSize_End)
  {
     Warning(0,"Preview_End_Size must be a power of 2. Changing to %d.",temp);
     opts.PreviewGridSize_End=temp;
  }

  /* End must be less than or equal to start */
  if (opts.PreviewGridSize_End > opts.PreviewGridSize_Start)
    opts.PreviewGridSize_End = opts.PreviewGridSize_Start;
    
  if (opts.PreviewGridSize_Start > 1)
  {
     opts.PreviewGridSize_End=max(opts.PreviewGridSize_End,2);
     opts.Options |= PREVIEW;
  }
  else
  {
     opts.Options &= ~PREVIEW;
  }

  /* Set histogram size here so it is available for Print_Options, and
   * make sure that it has an integer number of pixels/bucket. */
  if (opts.histogram_on)
  {
    if (opts.histogram_x == 0 || opts.histogram_x > Frame.Screen_Width)
      opts.histogram_x = Frame.Screen_Width;
    else if (opts.histogram_x < Frame.Screen_Width)
      opts.histogram_x = Frame.Screen_Width / ((Frame.Screen_Width +
                         opts.histogram_x - 1) / opts.histogram_x);

    if (opts.histogram_y == 0 || opts.histogram_y > Frame.Screen_Height)
      opts.histogram_y = Frame.Screen_Height;
    else if (opts.histogram_y < Frame.Screen_Height)
      opts.histogram_y = Frame.Screen_Height / ((Frame.Screen_Height +
                         opts.histogram_y - 1) /opts.histogram_y);
  }
}

/*****************************************************************************
*
* FUNCTION
*
*   fix_up_animation_values
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
*   Validate animation parameters, compute subset values
*
* CHANGES
*
*   -
*
******************************************************************************/
void fix_up_animation_values()
{
  DBL ClockDiff;
  int FrameDiff;
  int FrameIncr;
  DBL ClockPerFrameIncr;
  int NumFrames;

  /*
   * Added because that is no animation. [trf]
   */
  if ((opts.FrameSeq.InitialFrame == opts.FrameSeq.FinalFrame) &&
      ((opts.FrameSeq.FinalFrame == 0) || (opts.FrameSeq.FinalFrame == 1)))
  {
     opts.FrameSeq.InitialFrame = -1;
     opts.FrameSeq.FinalFrame = -1;
  }

  if (opts.FrameSeq.FinalFrame != -1)
  {
    opts.FrameSeq.FrameType = FT_MULTIPLE_FRAME;

    if (opts.FrameSeq.Clock_Value != 0.0)
    {
       Warning(0,"Attempted to set single clock value in multi frame\n"
                 "animation. Clock value overridden.");
    }
  }
  else
  {
    if (opts.FrameSeq.Clock_Value != 0.0)
    {
       opts.FrameSeq.FrameType = FT_SINGLE_FRAME;
    }
  }

  if (opts.FrameSeq.FrameType == FT_SINGLE_FRAME)
  {
    /*
     * These are dummy values that will work for single_frame,
     * even in an animation loop.
     */

    opts.FrameSeq.InitialFrame = 0;
    opts.FrameSeq.FinalFrame   = 0;
    opts.FrameSeq.InitialClock = opts.FrameSeq.Clock_Value;
    opts.FrameSeq.FinalClock   = 0.0;
  }
  else
  {
    /* FrameType==FT_MULTIPLE_FRAME */

    if(opts.FrameSeq.InitialFrame == -1)
    {
      opts.FrameSeq.InitialFrame = 1;
    }

    if (opts.FrameSeq.FinalFrame < opts.FrameSeq.InitialFrame)
    {
      Error("Final frame %d is less than Start Frame %d.",
            opts.FrameSeq.FinalFrame, opts.FrameSeq.InitialFrame);
    }

    ClockDiff = opts.FrameSeq.FinalClock-opts.FrameSeq.InitialClock;

    if (opts.Options & CYCLIC_ANIMATION)
    {
      FrameDiff = opts.FrameSeq.FinalFrame-opts.FrameSeq.InitialFrame+1;
    }
    else
    {
      FrameDiff = opts.FrameSeq.FinalFrame-opts.FrameSeq.InitialFrame;
    }

    ClockPerFrameIncr = (FrameDiff == 0) ? 0 : (ClockDiff/FrameDiff);

    /* Calculate width, which is an integer log10 */

    NumFrames = opts.FrameSeq.FinalFrame;

    opts.FrameSeq.FrameNumWidth = 1;

    while (NumFrames >= 10)
    {
      opts.FrameSeq.FrameNumWidth++;

      NumFrames = NumFrames / 10;
    }

    if (opts.FrameSeq.FrameNumWidth > POV_NAME_MAX-1)
    {
      Error("Cannot render %d frames requiring %d chars with %d width filename.",
          opts.FrameSeq.FinalFrame - opts.FrameSeq.InitialFrame + 1,
          opts.FrameSeq.FrameNumWidth, POV_NAME_MAX);
    }

    /* STARTING FRAME SUBSET */

    if (opts.FrameSeq.SubsetStartPercent != DBL_VALUE_UNSET)
    {
      FrameIncr = FrameDiff * opts.FrameSeq.SubsetStartPercent + 0.5; /* w/rounding */

      opts.FrameSeq.SubsetStartFrame = opts.FrameSeq.InitialFrame + FrameIncr;
    }

    if (opts.FrameSeq.SubsetStartFrame != INT_VALUE_UNSET)
    {
      NumFrames = opts.FrameSeq.SubsetStartFrame - opts.FrameSeq.InitialFrame;

      opts.FrameSeq.InitialFrame = opts.FrameSeq.SubsetStartFrame;
      opts.FrameSeq.InitialClock = opts.FrameSeq.InitialClock + NumFrames * ClockPerFrameIncr;
    }

    /* ENDING FRAME SUBSET */

    if (opts.FrameSeq.SubsetEndPercent != DBL_VALUE_UNSET)
    {
      /*
       * By this time, we have possibly lost InitialFrame, so we calculate
       * it via FinalFrame-FrameDiff
       */

      FrameIncr = FrameDiff * opts.FrameSeq.SubsetEndPercent + 0.5; /* w/rounding */

      opts.FrameSeq.SubsetEndFrame = (opts.FrameSeq.FinalFrame - FrameDiff) + FrameIncr;
    }

    if (opts.FrameSeq.SubsetEndFrame != INT_VALUE_UNSET)
    {
      NumFrames = opts.FrameSeq.FinalFrame - opts.FrameSeq.SubsetEndFrame;

      opts.FrameSeq.FinalFrame = opts.FrameSeq.SubsetEndFrame;
      opts.FrameSeq.FinalClock = opts.FrameSeq.FinalClock - NumFrames * ClockPerFrameIncr;
    }

    /*
     * Now that we have everything calculated, we check FinalFrame
     * and InitialFrame one more time, in case the subsets messed them up
     */

    if (opts.FrameSeq.FinalFrame < opts.FrameSeq.InitialFrame)
    {
      Error("Final frame %d is less than Start Frame %d due to bad subset specification.",
            opts.FrameSeq.FinalFrame, opts.FrameSeq.InitialFrame);
    }
  }

  /* Needed for pre-render shellout fixup */

  opts.FrameSeq.FrameNumber = opts.FrameSeq.InitialFrame;
  opts.FrameSeq.Clock_Value = opts.FrameSeq.InitialClock;
}

/*****************************************************************************
*
* FUNCTION
*
*   fix_up_scene_name
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
*   Strip path and extention of input file to create scene name
*
* CHANGES
*
******************************************************************************/

void fix_up_scene_name()
{
  int i, l;
  char temp[FILE_NAME_LENGTH];
  
  if ((l=strlen(opts.Input_File_Name)-1)<1)
  {
     strcpy(opts.Scene_Name,opts.Input_File_Name);
     return;
  }

  strcpy(temp,opts.Input_File_Name);
  for (i=l;i>0;i--)
  {
     if (temp[i]==FILENAME_SEPARATOR)
     {
        break;
     }
     if (temp[i]=='.')
     {
        temp[i]=0;
        break;
     }
  }

  i=strlen(temp)-1;
  
  while ((i>0) && (temp[i]!=FILENAME_SEPARATOR))
    i--;
  if (temp[i]==FILENAME_SEPARATOR)
    i++;
  strcpy(opts.Scene_Name,&(temp[i]));

  if (opts.Language_Version > OFFICIAL_VERSION_NUMBER)
  {
     Error("Your scene file requires POV-Ray version %g or later!\n", (DBL)(opts.Language_Version / 100.0));
  }
}

/*****************************************************************************
*
* FUNCTION
*
*   init_vars
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
*   Initialize all global variables.
*
* CHANGES
*
*   -
*
******************************************************************************/

void init_vars()
{
  Stage=STAGE_INIT;
  opts.Abort_Test_Counter = Abort_Test_Every ;
  Abort_Test_Every = 1;
  opts.AntialiasDepth = 3;
  opts.Antialias_Threshold = 0.3;
  opts.BBox_Threshold = 25;
  Color_Bits = 8;
  opts.DisplayFormat = '0';
  Display_Started = false;
  opts.First_Column = 0;
  opts.First_Column_Percent = 0.0;
  opts.First_Line = 0;
  opts.First_Line_Percent = 0.0;
  Frame.Screen_Height = 100;
  Frame.Screen_Width  = 100;
  Root_Object = NULL;
  free_istack = NULL;
  opts.JitterScale = 1.0;
  opts.Language_Version = OFFICIAL_VERSION_NUMBER;
  opts.Last_Column = -1;
  opts.Last_Column_Percent = 1.0;
  opts.Last_Line = -1;
  opts.Last_Line_Percent = 1.0;
  opts.PreviewGridSize_Start = 1;
  opts.PreviewGridSize_End   = 1;
  opts.Library_Paths[0] = NULL;
  opts.Library_Path_Index = 0;
  Max_Intersections = 64; /*128*/
  Number_Of_Files = 0;
  Number_of_istacks = 0;

  opts.Options = USE_VISTA_BUFFER + USE_LIGHT_BUFFER + JITTER +
                 DISKWRITE + REMOVE_BOUNDS;
  opts.OutputFormat = DEFAULT_OUTPUT_FORMAT;
  opts.OutputQuality = 8;
  Output_File = NULL;
  opts.Output_Numbered_Name[0]='\0';
  opts.Output_File_Name[0]='\0';
  opts.Output_Path[0]='\0';
  opts.Output_File_Type=0;
  opts.PaletteOption = '3';
  opts.Quality = 9;
  opts.Quality_Flags = QUALITY_9;
  opts.DisplayGamma = DEFAULT_DISPLAY_GAMMA;

  opts.Header_File_Name[0] = '\0';

  /* 
   * If DisplayGamma == 2.2, then GammaFactor == .45, which is what we want.
   */
  opts.GammaFactor = DEFAULT_ASSUMED_GAMMA/opts.DisplayGamma;

  opts.FrameSeq.FrameType = FT_SINGLE_FRAME;
  opts.FrameSeq.Clock_Value = 0.0;
  opts.FrameSeq.InitialFrame = 1;
  opts.FrameSeq.InitialClock = 0.0;
  opts.FrameSeq.FinalFrame = INT_VALUE_UNSET;
  opts.FrameSeq.FrameNumWidth = 0;
  opts.FrameSeq.FinalClock = 1.0;
  opts.FrameSeq.SubsetStartFrame = INT_VALUE_UNSET;
  opts.FrameSeq.SubsetStartPercent = DBL_VALUE_UNSET;
  opts.FrameSeq.SubsetEndFrame = INT_VALUE_UNSET;
  opts.FrameSeq.SubsetEndPercent = DBL_VALUE_UNSET;
  opts.FrameSeq.Field_Render_Flag = false;
  opts.FrameSeq.Odd_Field_Flag = false;

  /* NK rad - these default settings are low quality
  for relatively high quality, use

  opts.Radiosity_Nearest_Count = 8;
  opts.Radiosity_Count = 100;
  opts.Radiosity_Recursion_Limit = 5;

  Only these variables should need adjustment
  */

  opts.Radiosity_Brightness = 1.0;
  opts.Radiosity_Count = 35;
  opts.Radiosity_Dist_Max = 0.0;   /* NK rad - dist_max is always computed on the fly now - FYI */
  opts.Radiosity_Error_Bound = 1.8;
  opts.Radiosity_Gray = 0.0;       /* degree to which gathered light is grayed */
  opts.Radiosity_Low_Error_Factor = 0.5;
  opts.Radiosity_Min_Reuse = 0.015;
  opts.Radiosity_Nearest_Count = 5;
  opts.Radiosity_Recursion_Limit = 3;
  opts.Radiosity_Quality = 6;     /* Q-flag value for light gathering */
  opts.Radiosity_File_ReadOnContinue = 1;
  opts.Radiosity_File_SaveWhileRendering = 1;
  opts.Radiosity_File_AlwaysReadAtStart = 0;
  opts.Radiosity_File_KeepOnAbort = 1;
  opts.Radiosity_File_KeepAlways = 0;
  opts.Maximum_Sample_Brightness = -1.0;  /* default max brightness allows any */
  opts.Radiosity_ADC_Bailout = 0.01;     /* use a fairly high default adc_bailout for rad */
  opts.Radiosity_Use_Normal = false;
  opts.Radiosity_Use_Media = false;
  opts.radPretraceStart = 0.08;
  opts.radPretraceEnd = 0.04;
  opts.Radiosity_Load_File_Name = NULL;
  opts.Radiosity_Save_File_Name = NULL;
  opts.Radiosity_Add_On_Final_Trace = true;
  opts.Radiosity_Enabled = false;

  Current_Line_Number = 0;

  init_statistics(stats);
  init_statistics(totalstats);

  strcpy (opts.Input_File_Name, "OBJECT.POV");
  opts.Scene_Name[0]='\0';
  opts.Ini_Output_File_Name[0]='\0';
  opts.Use_Slabs=true;
  Num_Echo_Lines = POV_NUM_ECHO_LINES;   /* May make user setable later - CEY*/

  closed_flag = false;
  Stop_Flag = false;

  trender = trender_frame = trender_total = 0.0;
  tparse  = tparse_frame  = tparse_total  = 0.0;
  tphoton = tphoton_frame = tphoton_total = 0.0;

  histogram_grid = NULL;
  opts.histogram_on = false;
  opts.histogram_type = NONE;
  opts.histogram_file_type=0;
  opts.Histogram_File_Name[0] = '\0';
  Histogram_File = NULL;
  /*
   * Note that late initialization of the histogram_x and histogram_y
   * variables is done in fix_up_rendering_window, if they aren't specified
   * on the command line.  This is because they are based on the image
   * dimensions, and we can't be certain that we have this info at the
   * time we parse the histogram options in optin.c. [AED]
   */
  opts.histogram_x = opts.histogram_y = 0;
  max_histogram_value = 0;

  opts.Tracing_Method = 1;
  Experimental_Flag = 0;
  Make_Pigment_Entries();

  opts.Preview_RefCon = 0;

  opts.Warning_Level = 10; // all warnings

  opts.String_Encoding = 0; // ASCII

  (void)POVMSAttrList_New(&opts.Declared_Variables); // we have to be careful... [trf]

  /* NK phmap */
  backtraceFlag=0;
  photonOptions.photonsEnabled = 0;
  InitBacktraceWasCalled=false;

  photonOptions.photonMap.head = NULL;
  photonOptions.photonMap.numPhotons  = 0;
  photonOptions.photonMap.numBlocks  = 0;

  photonOptions.photonMap.gatherNumSteps = 2;
  photonOptions.photonMap.minGatherRad = -1.0;
  photonOptions.photonMap.minGatherRadMult = 1.0;
#ifdef GLOBAL_PHOTONS
  photonOptions.globalPhotonMap.gatherNumSteps = 1;
  photonOptions.globalPhotonMap.minGatherRad = -1.0;
  photonOptions.globalPhotonMap.minGatherRadMult = 1.0;
#endif
  photonOptions.mediaPhotonMap.gatherNumSteps = 1;
  photonOptions.mediaPhotonMap.minGatherRad = -1.0;
  photonOptions.mediaPhotonMap.minGatherRadMult = 1.0;

  photonOptions.minGatherCount = 20;
  photonOptions.maxGatherCount = 100;

  photonOptions.ADC_Bailout = -1;  /* use the normal adc bailout */
  photonOptions.Max_Trace_Level = -1; /* use the normal max_trace_level */

  photonOptions.jitter = 0.4;
  photonOptions.autoStopPercent = 0.5;

  photonOptions.expandTolerance = 0.2;
  photonOptions.minExpandCount = 35;

  photonOptions.fileName = NULL;
  photonOptions.loadFile = false;

  disp_elem = 0; /* for dispersion */
  disp_nelems = 0;   /* reset this for next pixel's tracing */

  photonOptions.photonGatherList = NULL;
  photonOptions.photonDistances = NULL;

#ifdef GLOBAL_PHOTONS
  /* global photon map */
  photonOptions.globalGatherRad = 10.0;
  photonOptions.globalPhotonsToShoot = 0;
#endif

  photonOptions.surfaceSeparation = 1.0;
  photonOptions.globalSeparation = 1.0;

  photonOptions.photonMap.head = NULL;
  photonOptions.photonMap.numPhotons = 0;
  photonOptions.photonMap.numBlocks = 0;
#ifdef GLOBAL_PHOTONS
  photonOptions.globalPhotonMap.head = NULL;
  photonOptions.globalPhotonMap.numPhotons = 0;
  photonOptions.globalPhotonMap.numBlocks = 0;
#endif
  photonOptions.mediaPhotonMap.head = NULL;
  photonOptions.mediaPhotonMap.numPhotons = 0;
  photonOptions.mediaPhotonMap.numBlocks = 0;

  photonOptions.maxMediaSteps = 0;  /* disable media photons by default */
  photonOptions.mediaSpacingFactor = 1.0;

  photonOptions.photonReflectionBlur = false; /* off by default */

  photonOptions.surfaceCount = 0;
  photonOptions.globalCount = 0;

  Highest_Trace_Level = 0 ;

  /* NK 1999 - bugfix */
  Trace_Level = 0;
  // [trf] Total_Depth = 0.0;
  Radiosity_Trace_Level = 1;

  warpNormalTextures = 0;

  opts.Noise_Generator = 2; /* default is the range-corrected noise, since the perlin noise (gen 3) seems buggy */

  ADC_Bailout = 1.0/255.0;

  SuperSampleCount = 0;
  RadiosityCount = 0;
  MosaicPreviewSize = 0;
}


/*****************************************************************************
*
* FUNCTION
*
*   variable_store
*
* INPUT
*
*   flag - flag telling wether to store or restore variables.
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Dieter Bayer
*   
* DESCRIPTION
*
*   Store or restore variables whose value has to be the same for all
*   frames of an animation and who are changed during every frame.
*
* CHANGES
*
*   May 1995 : Creation.
*
******************************************************************************/

void variable_store(int Flag)
{
  switch (Flag)
  {
    case STORE:

      STORE_First_Line = opts.First_Line;

      break;

    case RESTORE:

      opts.First_Line = STORE_First_Line;

      break;

    default:

      Error("Unknown flag in variable_store().");
  }
}

/*****************************************************************************
*
* FUNCTION
*
*   destroy_libraries
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
*   Free library path memory.
*
* CHANGES
*
*   -
*
******************************************************************************/

void destroy_libraries()
{
  int i;

  for (i = 0; i < opts.Library_Path_Index; i++)
  {
    POV_FREE(opts.Library_Paths[i]);
    
    opts.Library_Paths[i] = NULL;
  }

  opts.Library_Path_Index = 0;
}



/*****************************************************************************
*
* FUNCTION
*
*   close_all
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
*   Close all the stuff that has been opened and free all allocated memory.
*
* CHANGES
*
*   -
*
******************************************************************************/

void close_all()
{
  /* Only close things once */

  if (closed_flag)
  {
    return;
  }

  FlushDebugMessageBuffer();

  FreeBacktraceEverything();

  // Close out our file
  if(Output_File != NULL)
  {
    delete Output_File;
    Output_File = NULL;
  }

  destroy_libraries();
  Terminate_Renderer();
  Destroy_Bounding_Slabs();
  Destroy_Vista_Buffer();
  Destroy_Light_Buffers();
  Destroy_Random_Generators();
  Deinitialize_Radiosity_Code();
  Free_Iteration_Stack();
  Free_Noise_Tables();
  destroy_histogram();
  Deinitialize_Atmosphere_Code();
  Deinitialize_BBox_Code();
  Deinitialize_Lighting_Code();
  Deinitialize_Mesh_Code();
  Deinitialize_VLBuffer_Code();
  Destroy_Frame();
  Destroy_IStacks();
  FreeFontInfo();

  POVFPU_Terminate();

  if ((opts.Options & DISPLAY) && Display_Started)
  {
    POV_DISPLAY_CLOSE(opts.Preview_RefCon);
  }

  (void)POVMSAttrList_Delete(&opts.Declared_Variables);

  FlushDebugMessageBuffer();

  init_shellouts();
  closed_flag = true;
}

END_POV_NAMESPACE
