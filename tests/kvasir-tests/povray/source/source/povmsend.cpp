/****************************************************************************
 *               povmsend.cpp
 *
 * This module contains POVMS received message handling routines.
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
 * $File: //depot/povray/3.6-release/source/povmsend.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <stdarg.h>
#include <float.h>
#include <algorithm>

#include "frame.h"
#include "vector.h"
#include "parse.h"
#include "povray.h"
#include "tokenize.h"
#include "userio.h"
#include "userdisp.h"
#include "povms.h"
#include "povmsend.h"
#include "octree.h"
#include "radiosit.h"
#include "optout.h"
#include "pov_err.h"
#include "pov_util.h"

#ifndef DONT_SHOW_IMAGE_LIB_VERSIONS
// these are needed for copyright notices and version numbers
#include "zlib.h"
#include "png.h"
#include "jversion.h"

// Including tiffio.h causes the Windows compile to break. As all we need is the
// version function, we just declare it here.
//#define __STDC__
//#include "tiffio.h"

extern "C" const char* TIFFGetVersion(void);
#endif

/*****************************************************************************
* Global variables
******************************************************************************/

extern POVMSContext POVMS_Render_Context; // GLOBAL VARIABLE

BEGIN_POV_NAMESPACE

extern POVMSObject *gStartedStreamMessage; // GLOBAL VARIABLE

extern const char *Extract_Version(const char *str);
extern int GetPhotonStat(POVMSType a);

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/


/*****************************************************************************
* Local typedefs
******************************************************************************/


/*****************************************************************************
* Local variables
******************************************************************************/

DBL Previous_t = 0; // GLOBAL VARIABLE
int Previous_tp = 0; // GLOBAL VARIABLE
int Previous_th = 0; // GLOBAL VARIABLE
int Previous_tr = 0; // GLOBAL VARIABLE


/*****************************************************************************
* Local functions
******************************************************************************/

int BuildCommand(POVMSObjectPtr msg, POVMSType key, SHELLDATA *data);
int BuildRenderTime(POVMSObjectPtr msg, POVMSType key, int parse, int photon, int render, int total);
int BuildParseStatistics(POVMSObjectPtr msg);
int BuildRenderStatistics(POVMSObjectPtr msg, COUNTER *pstats);
int BuildProgress(POVMSObjectPtr msg, int progress);
int AddStatistic(POVMSObjectPtr msg, POVMSType key, COUNTER *counter);
int AddOIStatistic(POVMSAttributeListPtr list, int index, COUNTER *pstats);


/*****************************************************************************
*
* FUNCTION
*
*   BuildCommand
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int BuildCommand(POVMSObjectPtr msg, POVMSType key, SHELLDATA *data)
{
   POVMSObject obj;
   int err;

   err = POVMSObject_New(&obj, kPOVObjectClass_Command);
   if(err == 0)
      err = POVMSUtil_SetString(&obj, kPOVAttrib_CommandString, data->Command);
   if(err == 0)
   {
      int i;

      switch(data->Ret)
      {
         case IGNORE_RET:
            i = 'I';
            break;
         case QUIT_RET:
            i = 'Q';
            break;
         case USER_RET:
            i = 'U';
            break;
         case FATAL_RET:
            i = 'F';
            break;
         case SKIP_ONCE_RET:
            i = 'S';
            break;
         case ALL_SKIP_RET:
            i = 'A';
            break;
      }

      if(data->Inverse == true)
         i = -i;

      err = POVMSUtil_SetInt(&obj, kPOVAttrib_ReturnAction, i);
   }
   if(err == 0)
      err = POVMSObject_Set(msg, &obj, key);

   return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   BuildRenderTime
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int BuildRenderTime(POVMSObjectPtr msg, POVMSType key, int parse, int photon, int render, int total)
{
   POVMSObject obj;
   int err;

   err = POVMSObject_New(&obj, kPOVObjectClass_RTime);
   if(err == 0)
      err = POVMSUtil_SetInt(&obj, kPOVAttrib_ParseTime, parse);
   if(err == 0)
      err = POVMSUtil_SetInt(&obj, kPOVAttrib_PhotonTime, photon);
   if(err == 0)
      err = POVMSUtil_SetInt(&obj, kPOVAttrib_TraceTime, render);
   if(err == 0)
      err = POVMSUtil_SetInt(&obj, kPOVAttrib_TotalTime, total);
   if(err == 0)
      err = POVMSObject_Set(msg, &obj, key);

   return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   BuildRenderOptions
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int BuildRenderOptions(POVMSObjectPtr msg)
{
   POVMSAttribute attr;
   int err = kNoErr;

   if(msg == NULL)
      return kParamErr;

   if(err == kNoErr)
      err = POVMSAttr_New(&attr);
   if(err == kNoErr)
   {
      err = POVMSAttr_Set(&attr, kPOVMSType_WildCard, (void *)(&opts.Preview_RefCon), sizeof(unsigned long));
      if(err == kNoErr)
         err = POVMSObject_Set(msg, &attr, kPOVAttrib_PreviewRefCon);
   }
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_Height, Frame.Screen_Height);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_Width, Frame.Screen_Width);
   if(err == kNoErr)
   {
      if (opts.First_Column == -1)
         err = POVMSUtil_SetFloat(msg, kPOVAttrib_StartColumn, opts.First_Column_Percent);
      else
         err = POVMSUtil_SetFloat(msg, kPOVAttrib_StartColumn, opts.First_Column);
   }
   if(err == kNoErr)
   {
      if (opts.Last_Column == -1)
         err = POVMSUtil_SetFloat(msg, kPOVAttrib_EndColumn, opts.Last_Column_Percent);
      else
         err = POVMSUtil_SetFloat(msg, kPOVAttrib_EndColumn, opts.Last_Column);
   }
   if(err == kNoErr)
   {
      if (opts.First_Line == -1)
         err = POVMSUtil_SetFloat(msg, kPOVAttrib_StartRow, opts.First_Line_Percent);
      else
         err = POVMSUtil_SetFloat(msg, kPOVAttrib_StartRow, opts.First_Line);
   }
   if(err == kNoErr)
   {
      if (opts.Last_Line == -1)
         err = POVMSUtil_SetFloat(msg, kPOVAttrib_EndRow, opts.Last_Line_Percent);
      else
         err = POVMSUtil_SetFloat(msg, kPOVAttrib_EndRow, opts.Last_Line);
   }
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_TestAbort, (opts.Options & EXITENABLE) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_TestAbortCount, opts.Abort_Test_Counter);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_ContinueTrace, (opts.Options & CONTINUE_TRACE) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetString(msg, kPOVAttrib_CreateIni, opts.Ini_Output_File_Name);
   if(err == kNoErr)
      err = POVMSUtil_SetFloat(msg, kPOVAttrib_Clock, opts.FrameSeq.Clock_Value);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_InitialFrame, max(opts.FrameSeq.InitialFrame, 1));
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_FinalFrame, max(opts.FrameSeq.FinalFrame, 1));
   if(err == kNoErr)
      err = POVMSUtil_SetFloat(msg, kPOVAttrib_InitialClock, opts.FrameSeq.InitialClock);
   if(err == kNoErr)
   {
      if(opts.FrameSeq.FinalFrame <= 1)
         err = POVMSUtil_SetFloat(msg, kPOVAttrib_FinalClock, 1.0);
      else
         err = POVMSUtil_SetFloat(msg, kPOVAttrib_FinalClock, opts.FrameSeq.FinalClock);
   }
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_SubsetStartFrame, max(opts.FrameSeq.SubsetStartFrame, 1));
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_SubsetEndFrame, max(opts.FrameSeq.SubsetEndFrame, 1));
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_CyclicAnimation, (opts.Options & CYCLIC_ANIMATION) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_FieldRender, opts.FrameSeq.Field_Render_Flag);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_OddField, opts.FrameSeq.Odd_Field_Flag);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_PauseWhenDone, (opts.Options & PROMPTEXIT) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_Verbose, (opts.Options & VERBOSE) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_DrawVistas, (opts.Options & USE_VISTA_DRAW) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_Display, (opts.Options & DISPLAY) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_VideoMode, opts.DisplayFormat);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_Palette, opts.PaletteOption);
   if(err == kNoErr)
      err = POVMSUtil_SetFloat(msg, kPOVAttrib_DisplayGamma, opts.DisplayGamma);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_PreviewStartSize, opts.PreviewGridSize_Start);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_PreviewEndSize, opts.PreviewGridSize_End);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_OutputToFile, (opts.Options & DISKWRITE) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_OutputFileType, opts.OutputFormat);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_Compression, opts.OutputQuality);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_OutputAlpha, (opts.Options & OUTPUT_ALPHA) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_BitsPerColor, opts.OutputQuality);
   if(err == kNoErr)
      err = POVMSUtil_SetString(msg, kPOVAttrib_OutputFile, opts.Output_File_Name);
   if(err == kNoErr)
      err = POVMSUtil_SetString(msg, kPOVAttrib_OutputPath, opts.Output_Path);
#if PRECISION_TIMER_AVAILABLE
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_CreateHistogram, opts.histogram_on != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_HistogramFileType, opts.histogram_type);
   if(err == kNoErr)
      err = POVMSUtil_SetString(msg, kPOVAttrib_HistogramFile, opts.Histogram_File_Name);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_HistogramGridSizeX, opts.histogram_x);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_HistogramGridSizeY, opts.histogram_y);
#endif /* PRECISION_TIMER_AVAILABLE */
   if(err == kNoErr)
      err = BuildCommand(msg, kPOVAttrib_PreSceneCommand, &opts.Shellouts[PRE_SCENE_SHL]);
   if(err == kNoErr)
      err = BuildCommand(msg, kPOVAttrib_PreFrameCommand, &opts.Shellouts[PRE_FRAME_SHL]);
   if(err == kNoErr)
      err = BuildCommand(msg, kPOVAttrib_PostSceneCommand, &opts.Shellouts[POST_SCENE_SHL]);
   if(err == kNoErr)
      err = BuildCommand(msg, kPOVAttrib_PostFrameCommand, &opts.Shellouts[POST_FRAME_SHL]);
   if(err == kNoErr)
      err = BuildCommand(msg, kPOVAttrib_UserAbortCommand, &opts.Shellouts[USER_ABORT_SHL]);
   if(err == kNoErr)
      err = BuildCommand(msg, kPOVAttrib_FatalErrorCommand, &opts.Shellouts[FATAL_SHL]);
   if(err == kNoErr)
      err = POVMSUtil_SetString(msg, kPOVAttrib_InputFile, opts.Input_File_Name);
   if(err == kNoErr)
   {
      POVMSAttributeList list;

      err = POVMSAttrList_New(&list);
      if(err == kNoErr)
      {
         int ii;

         for(ii = 0; ii < opts.Library_Path_Index; ii++)
         {
            err = POVMSAttr_New(&attr);
            if(err == kNoErr)
            {
               err = POVMSAttr_Set(&attr, kPOVMSType_CString, opts.Library_Paths[ii], strlen(opts.Library_Paths[ii]) + 1);
               if(err == kNoErr)
                  err = POVMSAttrList_Append(&list, &attr);
               else
                  err = POVMSAttr_Delete(&attr);
            }
         }
         if(err == kNoErr)
            err = POVMSObject_Set(msg, &list, kPOVAttrib_LibraryPath);
      }
   }
   if(err == kNoErr)
   {
      POVMSFloat f = opts.Language_Version / 100.0;
      err = POVMSUtil_SetFloat(msg, kPOVAttrib_Version, f);
   }
/* FIXME
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_DebugConsole, Stream_Info[DEBUG_STREAM]->console != NULL);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_FatalConsole, Stream_Info[FATAL_STREAM]->console != NULL);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_RenderConsole, Stream_Info[RENDER_STREAM]->console != NULL);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_StatisticsConsole, Stream_Info[STATISTIC_STREAM]->console != NULL);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_WarningConsole, Stream_Info[WARNING_STREAM]->console != NULL);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_AllConsole, Stream_Info[ALL_STREAM]->console != NULL);
   if((err == kNoErr) && (Stream_Info[DEBUG_STREAM]->name != NULL))
      err = POVMSUtil_SetString(msg, kPOVAttrib_DebugFile, Stream_Info[DEBUG_STREAM]->name);
   if((err == kNoErr) && (Stream_Info[FATAL_STREAM]->name != NULL))
      err = POVMSUtil_SetString(msg, kPOVAttrib_FatalFile, Stream_Info[FATAL_STREAM]->name);
   if((err == kNoErr) && (Stream_Info[RENDER_STREAM]->name != NULL))
      err = POVMSUtil_SetString(msg, kPOVAttrib_RenderFile, Stream_Info[RENDER_STREAM]->name);
   if((err == kNoErr) && (Stream_Info[STATISTIC_STREAM]->name != NULL))
      err = POVMSUtil_SetString(msg, kPOVAttrib_StatisticsFile, Stream_Info[STATISTIC_STREAM]->name);
   if((err == kNoErr) && (Stream_Info[WARNING_STREAM]->name != NULL))
      err = POVMSUtil_SetString(msg, kPOVAttrib_WarningFile, Stream_Info[WARNING_STREAM]->name);
   if((err == kNoErr) && (Stream_Info[ALL_STREAM]->name != NULL))
      err = POVMSUtil_SetString(msg, kPOVAttrib_AllFile, Stream_Info[ALL_STREAM]->name);
*/   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_Quality, opts.Quality);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_Bounding, opts.Use_Slabs);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_BoundingThreshold, opts.BBox_Threshold);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_LightBuffer, (opts.Options & USE_LIGHT_BUFFER) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_VistaBuffer, (opts.Options & USE_VISTA_BUFFER) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_RemoveBounds, (opts.Options & REMOVE_BOUNDS) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_SplitUnions, (opts.Options & SPLIT_UNION) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_Antialias, (opts.Options & ANTIALIAS) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_SamplingMethod, opts.Tracing_Method);
   if(err == kNoErr)
      err = POVMSUtil_SetFloat(msg, kPOVAttrib_AntialiasThreshold, opts.Antialias_Threshold);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_AntialiasDepth, opts.AntialiasDepth);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(msg, kPOVAttrib_Jitter, (opts.Options & JITTER) != 0);
   if(err == kNoErr)
      err = POVMSUtil_SetFloat(msg, kPOVAttrib_JitterAmount, opts.JitterScale);
   if(err == kNoErr)
      err = POVMSUtil_SetString(msg, kPOVAttrib_IncludeHeader, opts.Header_File_Name);

   return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   BuildRenderStatus
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int BuildParseStatistics(POVMSObjectPtr msg)
{
   int err = kNoErr;

   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_FiniteObjects, numberOfFiniteObjects);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_InfiniteObjects, numberOfInfiniteObjects);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_LightSources, numberOfLightSources);

   return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   BuildRenderStatistics
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int BuildRenderStatistics(POVMSObjectPtr msg, COUNTER *pstats)
{
   POVMSAttributeList list;
   int err = kNoErr;

   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_Height, Frame.Screen_Height);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_Width, Frame.Screen_Width);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_Pixels, &pstats[Number_Of_Pixels]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_PixelSamples, &pstats[Number_Of_Samples]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_Rays, &pstats[Number_Of_Rays]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_RaysSaved, &pstats[ADC_Saves]);
   if(err == kNoErr)
      err = POVMSAttrList_New(&list);
   if(err == kNoErr)
   {
      int i;

      for(i = 0; intersection_stats[i].infotext != NULL; i++)
      {
         err = AddOIStatistic(&list, i, pstats);
         if(err != kNoErr)
            break;
      }
   }
   if(err == kNoErr)
      err = POVMSObject_Set(msg, &list, kPOVAttrib_ObjectIStats);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_ShadowTest, &pstats[Shadow_Ray_Tests]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_ShadowTestSuc, &pstats[Shadow_Rays_Succeeded]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_IsoFindRoot, &pstats[Ray_IsoSurface_Find_Root]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_FunctionVMCalls, &pstats[Ray_Function_VM_Calls]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_FunctionVMInstrEst, &pstats[Ray_Function_VM_Instruction_Est]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_PolynomTest, &pstats[Polynomials_Tested]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_RootsEliminated, &pstats[Roots_Eliminated]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_CallsToNoise, &pstats[Calls_To_Noise]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_CallsToDNoise, &pstats[Calls_To_DNoise]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_MediaSamples, &pstats[Media_Samples]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_MediaIntervals, &pstats[Media_Intervals]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_ReflectedRays, &pstats[Reflected_Rays_Traced]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_InnerReflectedRays, &pstats[Internal_Reflected_Rays_Traced]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_RefractedRays, &pstats[Refracted_Rays_Traced]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_TransmittedRays, &pstats[Transmitted_Rays_Traced]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_IStackOverflow, &pstats[Istack_overflows]);
#if defined(MEM_STATS)
   Long_To_Counter(mem_stats_smallest_alloc(), pstats[MemStat_Smallest_Alloc]);
   Long_To_Counter(mem_stats_largest_alloc(), pstats[MemStat_Largest_Alloc]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_MinAlloc, &pstats[MemStat_Smallest_Alloc]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_MaxAlloc, &pstats[MemStat_Largest_Alloc]);
#if (MEM_STATS>=2)
   Long_To_Counter(mem_stats_total_allocs(), pstats[MemStat_Total_Allocs]);
   Long_To_Counter(mem_stats_total_frees(), pstats[MemStat_Total_Frees]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_CallsToAlloc, &pstats[MemStat_Total_Allocs]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_CallsToFree, &pstats[MemStat_Total_Frees]);
#endif
   Long_To_Counter(mem_stats_largest_mem_usage(), pstats[MemStat_Largest_Mem_Usage]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_PeakMemoryUsage, &pstats[MemStat_Largest_Mem_Usage]);
#endif

   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_TraceLevel, Highest_Trace_Level);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_MaxTraceLevel, Max_Trace_Level);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_RadGatherCount, ra_gather_count);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(msg, kPOVAttrib_RadReuseCount, ra_reuse_count);

   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_PhotonsShot, &pstats[Number_Of_Photons_Shot]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_PhotonsStored, &pstats[Number_Of_Photons_Stored]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_GlobalPhotonsStored, &pstats[Number_Of_Global_Photons_Stored]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_MediaPhotonsStored, &pstats[Number_Of_Media_Photons_Stored]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_PhotonsPriQInsert, &pstats[Priority_Queue_Add]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_PhotonsPriQRemove, &pstats[Priority_Queue_Remove]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_GatherPerformedCnt, &pstats[Gather_Performed_Count]);
   if(err == kNoErr)
      err = AddStatistic(msg, kPOVAttrib_GatherExpandedCnt, &pstats[Gather_Expanded_Count]);

   return 0;
}


/*****************************************************************************
*
* FUNCTION
*
*   BuildProgress
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int BuildProgress(POVMSObjectPtr msg, int progress)
{
	int ret = kNoErr;
	DBL time_dif;

	STOP_TIME
	time_dif = TIME_ELAPSED

	ret = POVMSUtil_SetInt(msg, kPOVAttrib_TotalTime, int(time_dif));

	switch(progress)
	{
		case kPOVList_Prog_CreatingBoundingSlabs:
			break;
		case kPOVList_Prog_CreatingVistaBuffer:
			break;
		case kPOVList_Prog_CreatingLightBuffers:
			break;
		case kPOVList_Prog_BuildingPhotonMaps:
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_TotalPhotonCount, GetPhotonStat(kPOVAttrib_TotalPhotonCount));
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_ObjectPhotonCount, GetPhotonStat(kPOVAttrib_ObjectPhotonCount));
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_MediaPhotonCount, GetPhotonStat(kPOVAttrib_MediaPhotonCount));
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_PhotonXSamples, GetPhotonStat(kPOVAttrib_PhotonXSamples));
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_PhotonYSamples, GetPhotonStat(kPOVAttrib_PhotonYSamples));
			break;
		case kPOVList_Prog_LoadingPhotonMaps:
			break;
		case kPOVList_Prog_SavingPhotonMaps:
			break;
		case kPOVList_Prog_SortingPhotons:
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_CurrentPhotonCount, GetPhotonStat(kPOVAttrib_CurrentPhotonCount));
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_TotalPhotonCount, GetPhotonStat(kPOVAttrib_TotalPhotonCount));
			break;
		case kPOVList_Prog_ReclaimingMemory:
			break;
		case kPOVList_Prog_WritingINIFile:
			break;
		case kPOVList_Prog_WritingHistogramFile:
			break;
		case kPOVList_Prog_PerformingShelloutCommand:
			break;
		case kPOVList_Prog_ResumingInterruptedTrace:
			break;
		case kPOVList_Prog_ProcessingFrame:
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_CurrentFrame, opts.FrameSeq.FrameNumber - opts.FrameSeq.InitialFrame + 1);
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_FrameCount, opts.FrameSeq.FinalFrame - opts.FrameSeq.InitialFrame + 1);
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_AbsoluteCurFrame, opts.FrameSeq.FrameNumber);
			(void)POVMSUtil_SetFloat(msg, kPOVAttrib_FirstClock, opts.FrameSeq.InitialClock);
			(void)POVMSUtil_SetFloat(msg, kPOVAttrib_CurrentClock, opts.FrameSeq.Clock_Value);
			(void)POVMSUtil_SetFloat(msg, kPOVAttrib_LastClock, opts.FrameSeq.FinalClock);
			break;
		case kPOVList_Prog_Parsing:
			(void)POVMSUtil_SetLong(msg, kPOVAttrib_CurrentToken, Current_Token_Count);
			break;
		case kPOVList_Prog_Displaying:
			break;
		case kPOVList_Prog_Rendering:
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_CurrentLine, Current_Line_Number - opts.First_Line + 1);
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_AbsoluteCurrentLine, Current_Line_Number);
			(void)POVMSUtil_SetInt(msg, kPOVAttrib_LineCount, opts.Last_Line - opts.First_Line);
			if(MosaicPreviewSize > 1)
				(void)POVMSUtil_SetInt(msg, kPOVAttrib_MosaicPreviewSize, MosaicPreviewSize);
			else
			{
				if(opts.Options & ANTIALIAS)
					(void)POVMSUtil_SetInt(msg, kPOVAttrib_SuperSampleCount, SuperSampleCount);
				if(opts.Radiosity_Enabled)
					(void)POVMSUtil_SetInt(msg, kPOVAttrib_RadGatherCount, ra_gather_count - RadiosityCount);
			}
			break;
		case kPOVList_Prog_DoneTracing:
			break;
		case kPOVList_Prog_AbortingRender:
			break;
		case kPOVList_Prog_UserAbort:
			break;
	}

	return ret;
}


/*****************************************************************************
*
* FUNCTION
*
*   AddStatistic
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int AddStatistic(POVMSObjectPtr msg, POVMSType key, COUNTER *counter)
{
   return POVMSUtil_SetLong(msg, key, *counter);
}


/*****************************************************************************
*
* FUNCTION
*
*   AddOIStatistic
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int AddOIStatistic(POVMSAttributeListPtr list, int index, COUNTER *pstats)
{
   POVMSObject obj;
   int err;

   err = POVMSObject_New(&obj, kPOVObjectClass_OIStat);
   if(err == kNoErr)
      err = POVMSUtil_SetString(&obj, kPOVAttrib_ObjectName, intersection_stats[index].infotext);
   if(err == kNoErr)
      err = POVMSUtil_SetInt(&obj, kPOVAttrib_ObjectID, intersection_stats[index].povms_id);
   if(err == kNoErr)
      err = POVMSUtil_SetLong(&obj, kPOVAttrib_ISectsTests, pstats[intersection_stats[index].stat_test_id]);
   if(err == kNoErr)
      err = POVMSUtil_SetLong(&obj, kPOVAttrib_ISectsSucceeded, pstats[intersection_stats[index].stat_suc_id]);
   if(err == kNoErr)
      err = POVMSAttrList_Append(list, &obj);

   return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   Send_InitInfo
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

void Send_InitInfo()
{
   POVMSAttributeList attrlist;
   POVMSAttribute attr;
   POVMSObject msg;
   int err = kNoErr;

   if(err == kNoErr)
      err = POVMSObject_New(&msg, kPOVMSType_WildCard);

   if(err == kNoErr)
      err = POVMSUtil_SetString(&msg, kPOVAttrib_PlatformName, POVRAY_PLATFORM_NAME);
   if(err == kNoErr)
      err = POVMSUtil_SetFormatString(&msg, kPOVAttrib_CoreVersion,
                                      "Persistence of Vision(tm) Ray Tracer Version %s%s", POV_RAY_VERSION, COMPILER_VER);
   if(err == kNoErr)
      err = POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText,
                                DISTRIBUTION_MESSAGE_1 "\n" DISTRIBUTION_MESSAGE_2 "\n" DISTRIBUTION_MESSAGE_3
                                "\nPOV-Ray is based on DKBTrace 2.12 by David K. Buck & Aaron A. Collins\n" POV_RAY_COPYRIGHT);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(&msg, kPOVAttrib_Official, POV_RAY_IS_OFFICIAL);

   if(err == kNoErr)
      err = POVMSAttrList_New(&attrlist);
   if(err == kNoErr)
   {
      for(int i = 0; Primary_Developers[i] != NULL; i++)
      {
         err = POVMSAttr_New(&attr);
         if(err == kNoErr)
         {
            err = POVMSAttr_Set(&attr, kPOVMSType_CString, Primary_Developers[i], strlen(Primary_Developers[i]) + 1);
            if(err == kNoErr)
               err = POVMSAttrList_Append(&attrlist, &attr);
            else
               err = POVMSAttr_Delete(&attr);
         }
      }
   }
   if(err == kNoErr)
      err = POVMSObject_Set(&msg, &attrlist, kPOVAttrib_PrimaryDevs);

   if(err == kNoErr)
      err = POVMSAttrList_New(&attrlist);
   if(err == kNoErr)
   {
      for(int i = 0; Contributing_Authors[i] != NULL; i++)
      {
         err = POVMSAttr_New(&attr);
         if(err == kNoErr)
         {
            err = POVMSAttr_Set(&attr, kPOVMSType_CString, Contributing_Authors[i], strlen(Contributing_Authors[i]) + 1);
            if(err == kNoErr)
               err = POVMSAttrList_Append(&attrlist, &attr);
            else
               err = POVMSAttr_Delete(&attr);
         }
      }
   }
   if(err == kNoErr)
      err = POVMSObject_Set(&msg, &attrlist, kPOVAttrib_ContributingDevs);

   if(err == kNoErr)
      err = POVMSAttrList_New(&attrlist);
#ifndef DONT_SHOW_IMAGE_LIB_VERSIONS
   // ZLib library version and copyright notice
   if(err == kNoErr)
   {
      err = POVMSAttr_New(&attr);
      if(err == kNoErr)
      {
         const char *tempstr = pov_tsprintf("ZLib %s, Copyright 1995-1998 Jean-loup Gailly and Mark Adler", Extract_Version(zlibVersion()));
         err = POVMSAttr_Set(&attr, kPOVMSType_CString, (void *)tempstr, strlen(tempstr) + 1);
         if(err == kNoErr)
            err = POVMSAttrList_Append(&attrlist, &attr);
         else
            err = POVMSAttr_Delete(&attr);
      }
   }
   // LibPNG library version and copyright notice
   if(err == kNoErr)
   {
      err = POVMSAttr_New(&attr);
      if(err == kNoErr)
      {
         const char *tempstr = pov_tsprintf("LibPNG %s, Copyright 1998-2002 Glenn Randers-Pehrson", Extract_Version(png_get_libpng_ver(NULL)));
         err = POVMSAttr_Set(&attr, kPOVMSType_CString, (void *)tempstr, strlen(tempstr) + 1);
         if(err == kNoErr)
            err = POVMSAttrList_Append(&attrlist, &attr);
         else
            err = POVMSAttr_Delete(&attr);
      }
   }
   // LibJPEG library version and copyright notice
   if(err == kNoErr)
   {
      err = POVMSAttr_New(&attr);
      if(err == kNoErr)
      {
         const char *tempstr = pov_tsprintf("LibJPEG %s, Copyright 1998 Thomas G. Lane", Extract_Version(JVERSION));
         err = POVMSAttr_Set(&attr, kPOVMSType_CString, (void *)tempstr, strlen(tempstr) + 1);
         if(err == kNoErr)
            err = POVMSAttrList_Append(&attrlist, &attr);
         else
            err = POVMSAttr_Delete(&attr);
      }
   }
   // LibTIFF library version and copyright notice
   if(err == kNoErr)
   {
      err = POVMSAttr_New(&attr);
      if(err == kNoErr)
      {
         const char *tempstr = pov_tsprintf("LibTIFF %s, Copyright 1988-1997 Sam Leffler, 1991-1997 SGI", Extract_Version(TIFFGetVersion()));
         err = POVMSAttr_Set(&attr, kPOVMSType_CString, (void *)tempstr, strlen(tempstr) + 1);
         if(err == kNoErr)
            err = POVMSAttrList_Append(&attrlist, &attr);
         else
            err = POVMSAttr_Delete(&attr);
      }
   }
#endif
   if(err == kNoErr)
      err = POVMSObject_Set(&msg, &attrlist, kPOVAttrib_ImageLibVersions);

   if(err == kNoErr)
      err = POVMSMsg_SetupMessage(&msg, kPOVMsgClass_Miscellaneous, kPOVMsgIdent_InitInfo);
   if(err == kNoErr)
      err = POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);

   if(err == kNoErr)
      err = POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);

   if(err != 0)
      (void)POVMS_ASSERT_OUTPUT("Sending InitInfo failed!", "povmsend.cpp", 0);
}

/*****************************************************************************
*
* FUNCTION
*
*   Send_Progress
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int Send_Progress(const char *statusString, int progressState)
{
	POVMSObject msg;
	int err = kNoErr;

	Previous_t = 0;

	if(err == kNoErr)
		err = POVMSObject_New(&msg, kPOVObjectClass_Progress);

	if(err == kNoErr)
		err = POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText, statusString);
	if(err == kNoErr)
		err = POVMSUtil_SetBool(&msg, kPOVAttrib_ProgressStatus, false);

	if(err == kNoErr)
		err = BuildProgress(&msg, progressState);

	if(err == kNoErr)
		err = POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_Progress);
	if(err == kNoErr)
		err = POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);

	if(err == kNoErr)
		err = POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);

	return err;
}

/*****************************************************************************
*
* FUNCTION
*
*   Send_ProgressUpdate
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int Send_ProgressUpdate(int progressState, int t)
{
	POVMSObject msg;
	int err = kNoErr;
	DBL time_dif;

	STOP_TIME
	time_dif = TIME_ELAPSED

	if((fabs(time_dif - Previous_t) >= t) || (t == 0))
	{
		Previous_t = time_dif;

		if(err == kNoErr)
			err = POVMSObject_New(&msg, kPOVObjectClass_Progress);

		if(err == kNoErr)
			err = POVMSUtil_SetBool(&msg, kPOVAttrib_ProgressStatus, true);

		if(err == kNoErr)
			err = BuildProgress(&msg, progressState);

		if(err == kNoErr)
			err = POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_Progress);
		if(err == kNoErr)
			err = POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);

		if(err == kNoErr)
			err = POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);
	}

	return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   Send_FrameStatistics
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int Send_FrameStatistics()
{
	POVMSObject msg;
	int err = kNoErr;

	if(err == kNoErr)
		err = POVMSObject_New(&msg, kPOVObjectClass_FStats);

    if(err == kNoErr)
        err = BuildRenderTime(&msg, kPOVAttrib_FrameTime, tparse_frame, tphoton_frame, trender_frame, tparse_frame+tphoton_frame+trender_frame);
    if(err == kNoErr)
        err = BuildRenderTime(&msg, kPOVAttrib_AnimationTime, tparse_total, tphoton_total, trender_total, tparse_total+tphoton_total+trender_total);

	if(err == kNoErr)
		err = POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_FrameStatistics);
	if(err == kNoErr)
		err = POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);

	if(err == kNoErr)
		err = POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);

	return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   Send_ParseStatistics
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int Send_ParseStatistics()
{
	POVMSObject msg;
	int err = kNoErr;

	if(err == kNoErr)
		err = POVMSObject_New(&msg, kPOVObjectClass_PStats);

	if(err == kNoErr)
		err = BuildParseStatistics(&msg);

	if(err == kNoErr)
		err = POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_ParseStatistics);
	if(err == kNoErr)
		err = POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);

	if(err == kNoErr)
		err = POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);

	return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   Send_RenderStatistics
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int Send_RenderStatistics(bool total)
{
   POVMSObject msg;
   int err = kNoErr;

   if(err == kNoErr)
      err = POVMSObject_New(&msg, kPOVObjectClass_RStats);
   if(err == kNoErr)
   {
      if(total == true)
         err = BuildRenderStatistics(&msg, totalstats);
      else
         err = BuildRenderStatistics(&msg, stats);
   }
   if(err == kNoErr)
      err = POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_RenderStatistics);
   if(err == kNoErr)
      err = POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
   if(err == kNoErr)
      err = POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);

   return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   Send_RenderOptions
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int Send_RenderOptions()
{
   POVMSObject msg;
   int err = kNoErr;

   if(err == kNoErr)
      err = POVMSObject_New(&msg, kPOVObjectClass_ROptions);
   if(err == kNoErr)
      err = BuildRenderOptions(&msg);
   if(err == kNoErr)
      err = POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_RenderOptions);
   if(err == kNoErr)
      err = POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
   if(err == kNoErr)
      err = POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);

   return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   Send_RenderDone
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int Send_RenderStarted(bool continuetrace)
{
   POVMSObject msg;
   int err = kNoErr;

   err = POVMSObject_New(&msg, kPOVMSType_WildCard);
   if(err == kNoErr)
      err = POVMSObject_Copy(gStartedStreamMessage, &msg);
   if(err == kNoErr)
      err = POVMSUtil_SetBool(&msg, kPOVAttrib_ContinueTrace, continuetrace);
   if(err == kNoErr)
      err = POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_RenderStarted);
   if(err == kNoErr)
      err = POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
   if(err == kNoErr)
      err = POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);

   return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   Send_RenderDone
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
*   Thorsten Froehlich
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

int Send_RenderDone(bool withtime)
{
   POVMSObject msg;
   int err = kNoErr;
   int tp, th, tr;

   if (trender == 0.0)
   {
     STOP_TIME

     trender = TIME_ELAPSED
   }

   tp = tparse_total;
   if(tp <= 0)
     tp = tparse;
   th = tphoton_total;
   if(th <= 0)
     th = tphoton;
   tr = trender_total;
   if(tr <= 0)
     tr = trender;

   err = POVMSObject_New(&msg, kPOVMSType_WildCard);
   if(err == kNoErr)
      BuildRenderTime(&msg, kPOVAttrib_AnimationTime, tp, th, tr, tp + th + tr);
   if(err == kNoErr)
      err = POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_RenderDone);
   if(err == kNoErr)
      err = POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
   if(err == kNoErr)
      err = POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);

   return err;
}

END_POV_NAMESPACE
