/****************************************************************************
 *               povmsrec.cpp
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
 * $File: //depot/povray/3.6-release/source/povmsrec.cpp $
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
#include "bbox.h"
#include "lighting.h"
#include "pov_mem.h"        /*POV_FREE*/
#include "octree.h"
#include "povray.h"
#include "optout.h"
#include "parse.h"
#include "radiosit.h"
#include "render.h"
#include "tokenize.h"
#include "vlbuffer.h"
#include "userio.h"
#include "userdisp.h"
#include "povmsrec.h"
#include "povmsend.h"
#include "pov_err.h"

BEGIN_POV_NAMESPACE

USING_POV_BASE_NAMESPACE

extern volatile int Stop_Flag; // GLOBAL VARIABLE


/*****************************************************************************
* Local preprocessor defines
******************************************************************************/


/*****************************************************************************
* Local typedefs
******************************************************************************/


/*****************************************************************************
* Local variables
******************************************************************************/

const int gStreamTypeUtilDataCount = 6;
const POVMSType gStreamTypeUtilData[gStreamTypeUtilDataCount] =
{
	kPOVAttrib_DebugFile,
	kPOVAttrib_FatalFile,
	kPOVAttrib_RenderFile,
	kPOVAttrib_StatisticsFile,
	kPOVAttrib_WarningFile,
	kPOVAttrib_AllFile
};

const char *gStreamDefaultFile[gStreamTypeUtilDataCount] =
{
	"debug.out",
	"fatal.out",
	"render.out",
	"stats.out",
	"warning.out",
	"alltext.out"
};

POVMSObject gStartedStreamMessageData; // GLOBAL VARIABLE
POVMSObject *gStartedStreamMessage = NULL; // GLOBAL VARIABLE

const int Quality_Values[12]=
{
  QUALITY_0, QUALITY_1, QUALITY_2, QUALITY_3, QUALITY_4,
  QUALITY_5, QUALITY_6, QUALITY_7, QUALITY_8, QUALITY_9
};


/*****************************************************************************
* Local functions
******************************************************************************/

bool Matches(const char *v1, const char *v2);
bool IsTrue(const char *value);
bool IsFalse(const char *value);

bool Matches(const char *v1, const char *v2)
{
   int i = 0;
   int ans = 1;

   while((ans) && (v1[i] != 0) && (v2[i] != 0))
   {
      ans = ans && (int)(v1[i] == tolower(v2[i]));
      i++;
   }

   return (ans != 0);
}

bool IsTrue(const char *value)
{
   return (Matches("on",value)  || Matches("true",value) || 
           Matches("yes",value) || Matches("1",value));
}

bool IsFalse(const char *value)
{
   return (Matches("off",value)  || Matches("false",value) || 
           Matches("no",value)   || Matches("0",value));
}


/*****************************************************************************
*
* FUNCTION
*
*   Receive_RenderOptions
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

int SetCommandOption(POVMSObjectPtr msg, POVMSType key, SHELLDATA *data)
{
   POVMSObject obj;
   int len = POV_MAX_CMD_LENGTH;
   int err;

   err = POVMSObject_Get(msg, &obj, key);
   if(err == 0)
   {
      err = POVMSUtil_GetString(&obj, kPOVAttrib_CommandString, data->Command, &len);
      if(err == 0)
      {
         int ret = 0;

         err = POVMSUtil_GetInt(&obj, kPOVAttrib_ReturnAction, &ret);
         if(err == 0)
         {
       		data->Inverse = (ret < 0);
         	if(ret < 0)
         		ret = -ret;
            switch(ret)
            {
               case 'I':
               case 'i':
                  data->Ret = IGNORE_RET;
                  break;
               case 'Q':
               case 'q':
                  data->Ret = QUIT_RET;
                  break;
               case 'U':
               case 'u':
                  data->Ret = USER_RET;
                  break;
               case 'F':
               case 'f':
                  data->Ret = FATAL_RET;
                  break;
               case 'S':
               case 's':
                  data->Ret = SKIP_ONCE_RET;
                  break;
               case 'A':
               case 'a':
                  data->Ret = ALL_SKIP_RET;
                  break;
            }
         }
      }
      (void)POVMSObject_Delete(&obj);
   }

   return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   Receive_RenderOptions
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
*   There are a lot of conditions here, some optimising would be good. [trf]
*
* CHANGES
*
*   -
*
******************************************************************************/

int Receive_RenderOptions(POVMSObjectPtr msg, POVMSObjectPtr result, int, void *)
{
   POVMSAttribute attr;
   POVMSInt i;
   POVMSFloat f;
   POVMSBool b;
   int l = 0;

   if(Cooperate_Render_Flag != 1)
      return -1;

   if(gStartedStreamMessage == NULL)
   {
      if(POVMSObject_New(&gStartedStreamMessageData, kPOVMSType_WildCard) == kNoErr)
         gStartedStreamMessage = &gStartedStreamMessageData;
   }

   l = sizeof(unsigned long);
   if(POVMSObject_Get(msg, &attr, kPOVAttrib_PreviewRefCon) == kNoErr)
   {
      (void)POVMSAttr_Get(&attr, kPOVMSType_WildCard, (void *)(&opts.Preview_RefCon), &l);
      (void)POVMSAttr_Delete(&attr);
   }
   if(POVMSUtil_GetInt(msg, kPOVAttrib_WarningLevel, &i) == 0)
      opts.Warning_Level = i;
   if(POVMSUtil_GetInt(msg, kPOVAttrib_Height, &i) == 0)
      Frame.Screen_Height = i;
   if(POVMSUtil_GetInt(msg, kPOVAttrib_Width, &i) == 0)
      Frame.Screen_Width = i;
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_StartColumn, &f) == 0)
   {
      if(f >= 0.0 && f < 1.0)
      {
         opts.First_Column = -1;
         opts.First_Column_Percent = f;
      }
      else
         opts.First_Column = (int)f;
   }
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_EndColumn, &f) == 0)
   {
      if((f >= 0.0 && f < 1.0) || ((f >= 0.0 && f <= 1.0) && (opts.First_Column < 1)))
      {
         opts.Last_Column = -1;
         opts.Last_Column_Percent = f;
      }
      else
         opts.Last_Column = (int)f;
   }
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_StartRow, &f) == 0)
   {
      if(f >= 0.0 && f < 1.0)
      {
         opts.First_Line = -1;
         opts.First_Line_Percent = f;
      }
      else
         opts.First_Line = (int)f;
   }
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_EndRow, &f) == 0)
   {
      if((f >= 0.0 && f < 1.0) || ((f >= 0.0 && f <= 1.0) && (opts.First_Line < 1)))
      {
         opts.Last_Line = -1;
         opts.Last_Line_Percent = f;
      }
      else
         opts.Last_Line = (int)f;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_TestAbort, &b) == 0)
   {
      if(b == true)
         opts.Options |= EXITENABLE;
      else
         opts.Options &= ~EXITENABLE;
   }
   if(POVMSUtil_GetInt(msg, kPOVAttrib_TestAbortCount, &i) == 0)
      opts.Abort_Test_Counter = i;
   if(POVMSUtil_GetBool(msg, kPOVAttrib_ContinueTrace, &b) == 0)
   {
      if(b == true)
         opts.Options |= CONTINUE_TRACE;
      else
         opts.Options &= ~CONTINUE_TRACE;
   }
   l = FILE_NAME_LENGTH;
   (void)POVMSUtil_GetString(msg, kPOVAttrib_CreateIni, opts.Ini_Output_File_Name, &l);
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_Clock, &f) == 0)
      opts.FrameSeq.Clock_Value = f;
   if(POVMSUtil_GetInt(msg, kPOVAttrib_InitialFrame, &i) == 0)
      opts.FrameSeq.InitialFrame = i;
   if(POVMSUtil_GetInt(msg, kPOVAttrib_FinalFrame, &i) == 0)
      opts.FrameSeq.FinalFrame = i;
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_InitialClock, &f) == 0)
      opts.FrameSeq.InitialClock = f;
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_FinalClock, &f) == 0)
      opts.FrameSeq.FinalClock = f;
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_SubsetStartFrame, &f) == 0)
   {
      if(f > 0.0 && f < 1.0)
         opts.FrameSeq.SubsetStartPercent = f;
      else
         opts.FrameSeq.SubsetStartFrame = (int)f;
   }
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_SubsetEndFrame, &f) == 0)
   {
      if(f > 0.0 && f < 1.0)
         opts.FrameSeq.SubsetEndPercent = f;
      else
         opts.FrameSeq.SubsetEndFrame = (int)f;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_CyclicAnimation, &b) == 0)
   {
      if(b == true)
         opts.Options |= CYCLIC_ANIMATION;
      else
         opts.Options &= ~CYCLIC_ANIMATION;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_FieldRender, &b) == 0)
      opts.FrameSeq.Field_Render_Flag = b;
   if(POVMSUtil_GetBool(msg, kPOVAttrib_OddField, &b) == 0)
      opts.FrameSeq.Odd_Field_Flag = b;
   if(POVMSUtil_GetBool(msg, kPOVAttrib_PauseWhenDone, &b) == 0)
   {
      if(b == true)
         opts.Options |= PROMPTEXIT;
      else
         opts.Options &= ~PROMPTEXIT;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_Verbose, &b) == 0)
   {
      if(b == true)
         opts.Options |= VERBOSE;
      else
         opts.Options &= ~VERBOSE;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_DrawVistas, &b) == 0)
   {
      if(b == true)
         opts.Options |= USE_VISTA_DRAW;
      else
         opts.Options &= ~USE_VISTA_DRAW;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_Display, &b) == 0)
   {
      if(b == true)
         opts.Options |= DISPLAY;
      else
         opts.Options &= ~DISPLAY;
   }
   if(POVMSUtil_GetInt(msg, kPOVAttrib_VideoMode, &i) == 0)
      opts.DisplayFormat = (char)toupper(i);
   if(POVMSUtil_GetInt(msg, kPOVAttrib_Palette, &i) == 0)
      opts.PaletteOption = (char)toupper(i);
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_DisplayGamma, &f) == 0)
   {
      if(f > 0.0)
         opts.DisplayGamma = f;
   }
   if(POVMSUtil_GetInt(msg, kPOVAttrib_PreviewStartSize, &i) == 0)
      opts.PreviewGridSize_Start = i;
   if(POVMSUtil_GetInt(msg, kPOVAttrib_PreviewEndSize, &i) == 0)
      opts.PreviewGridSize_End = i;
   if(POVMSUtil_GetBool(msg, kPOVAttrib_OutputToFile, &b) == 0)
   {
      if(b == true)
         opts.Options |= DISKWRITE;
      else
         opts.Options &= ~DISKWRITE;
   }
   if(POVMSUtil_GetInt(msg, kPOVAttrib_OutputFileType, &i) == 0)
      opts.OutputFormat = (char)tolower(i);
   if(POVMSUtil_GetInt(msg, kPOVAttrib_Compression, &i) == 0)
   {
      if(opts.OutputFormat == 'j')
      {
         opts.OutputQuality = i;
         opts.OutputQuality = max(0, opts.OutputQuality);
         opts.OutputQuality = min(100, opts.OutputQuality);
      }
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_OutputAlpha, &b) == 0)
   {
      if(b == true)
         opts.Options |= OUTPUT_ALPHA;
      else
         opts.Options &= ~OUTPUT_ALPHA;
   }
   if(POVMSUtil_GetInt(msg, kPOVAttrib_BitsPerColor, &i) == 0)
   {
      if(opts.OutputFormat != 'j')
      {
         opts.OutputQuality = i;
         opts.OutputQuality = max(5, opts.OutputQuality);
         opts.OutputQuality = min(16, opts.OutputQuality);
      }
   }
   l = FILE_NAME_LENGTH;
   if(POVMSUtil_GetString(msg, kPOVAttrib_OutputFile, opts.Output_File_Name, &l) == 0)
   {
      if(!strcmp(opts.Output_File_Name, "-") || !strcmp(opts.Output_File_Name, "stdout"))
      {
         strcpy(opts.Output_File_Name, "stdout");
         opts.Options |= TO_STDOUT;
      }
   }
   l = FILE_NAME_LENGTH;
   opts.Ini_Output_File_Name[0] = '\0';
   (void)POVMSUtil_GetString(msg, kPOVAttrib_CreateIni, opts.Ini_Output_File_Name, &l);

#if PRECISION_TIMER_AVAILABLE
   if(POVMSUtil_GetBool(msg, kPOVAttrib_CreateHistogram, &b) == 0)
      opts.histogram_on = b;
   if(POVMSUtil_GetInt(msg, kPOVAttrib_HistogramFileType, &i) == 0)
   {
      char *def_ext = NULL;

      switch(i)
      {
         case 'C':
         case 'c':
            opts.histogram_type = CSV;
    	    opts.histogram_file_type = NO_FILE; // CSV has special handling in histogram output, so this is correct [trf]
            def_ext = ".csv";
            break;
         case 'S':
         case 's':
            opts.histogram_type = SYS ;
    	    opts.histogram_file_type = SYS_FILE;
            def_ext = SYS_DEF_EXT;
            break ;
         case 'P' :
         case 'p' :
            opts.histogram_type = PPM;
    	    opts.histogram_file_type = PPM_FILE;
            def_ext = ".ppm";
            break;
         case 'T':
         case 't':
            opts.histogram_type = TARGA;
    	    opts.histogram_file_type = TGA_FILE;
            def_ext = ".tga";
            break;
         case 'N':
         case 'n':
            opts.histogram_type = PNG;
    	    opts.histogram_file_type = PNG_FILE;
            def_ext = ".png";
            break;
         default:
            opts.histogram_type = TARGA;
    	    opts.histogram_file_type = TGA_FILE;
            Warning(0, "Unknown histogram output type '%c'.", (char)i);
            break ;
      }

      // Process the histogram file name now, if it hasn't
      // yet been specified, and in case it isn't set later.
      if (opts.histogram_on && (opts.Histogram_File_Name[0] == '\0') && (def_ext != NULL))
         sprintf(opts.Histogram_File_Name, "histgram%s", def_ext);
   }
   l = FILE_NAME_LENGTH;
   if(POVMSUtil_GetString(msg, kPOVAttrib_HistogramFile, opts.Histogram_File_Name, &l) == 0)
   {
      if(opts.histogram_on && opts.Histogram_File_Name[0] == '\0')
      {
         char *def_ext = NULL;

         switch(opts.histogram_type)
         {
				case CSV:	 def_ext = ".csv"; break;
				case TARGA:  def_ext = ".tga"; break;
				case PNG:	 def_ext = ".png"; break;
				case PPM:	 def_ext = ".ppm"; break;
				case SYS:	 def_ext = SYS_DEF_EXT; break;
				case NONE:	 def_ext = ""; break;	/* To quiet warnings */
         }
         sprintf(opts.Histogram_File_Name, "histgram%s", def_ext);
      }
   }
   if(POVMSUtil_GetInt(msg, kPOVAttrib_HistogramGridSizeX, &i) == 0)
      opts.histogram_x = i;
   if(POVMSUtil_GetInt(msg, kPOVAttrib_HistogramGridSizeY, &i) == 0)
      opts.histogram_y = i;

#endif /* PRECISION_TIMER_AVAILABLE */
   (void)SetCommandOption(msg, kPOVAttrib_PreSceneCommand, &opts.Shellouts[PRE_SCENE_SHL]);
   (void)SetCommandOption(msg, kPOVAttrib_PreFrameCommand, &opts.Shellouts[PRE_FRAME_SHL]);
   (void)SetCommandOption(msg, kPOVAttrib_PostSceneCommand, &opts.Shellouts[POST_SCENE_SHL]);
   (void)SetCommandOption(msg, kPOVAttrib_PostFrameCommand, &opts.Shellouts[POST_FRAME_SHL]);
   (void)SetCommandOption(msg, kPOVAttrib_UserAbortCommand, &opts.Shellouts[USER_ABORT_SHL]);
   (void)SetCommandOption(msg, kPOVAttrib_FatalErrorCommand, &opts.Shellouts[FATAL_SHL]);
   l = FILE_NAME_LENGTH;
   if(POVMSUtil_GetString(msg, kPOVAttrib_InputFile, opts.Input_File_Name, &l) == 0)
   {
      if(!strcmp(opts.Input_File_Name, "-") || !strcmp(opts.Input_File_Name, "stdin"))
      {
         strcpy (opts.Input_File_Name, "stdin");
         opts.Options |= FROM_STDIN;
      }
   }
   if(POVMSObject_Get(msg, &attr, kPOVAttrib_LibraryPath) == 0)
   {
      int cnt = 0;

      if(POVMSAttrList_Count(&attr, &cnt) == 0)
      {
         POVMSAttribute item;
         int ii,iii;
         bool rem = false;

         for(ii = 1; ii <= cnt; ii++)
         {
            if(POVMSAttrList_GetNth(&attr, ii, &item) == 0)
            {
               l = 0;
               if(POVMSAttr_Size(&item, &l) == 0)
               {
                  if(l > 0)
                  {
                     if(opts.Library_Path_Index >= MAX_LIBRARIES)
                        Error ("Too many library directories specified.");
                     opts.Library_Paths[opts.Library_Path_Index] = (char *)POV_MALLOC(l, "library paths");
                     if(POVMSAttr_Get(&item, kPOVMSType_CString, opts.Library_Paths[opts.Library_Path_Index], &l) == 0)
                        rem = false;
                     else
                        rem = true;

                     // remove path again if the same one already exists
                     for(iii = 0; iii < opts.Library_Path_Index - 1; iii++)
                     {
                        if(strcmp(opts.Library_Paths[iii], opts.Library_Paths[opts.Library_Path_Index]) == 0)
                        {
                           rem = true;
                           break;
                        }
                     }

                     if(rem == true)
                     {
                        POV_FREE(opts.Library_Paths[opts.Library_Path_Index]);
                        opts.Library_Paths[opts.Library_Path_Index] = NULL;
                     }
                     else
                        opts.Library_Path_Index++;
                  }
               }
               (void)POVMSAttr_Delete(&item);
            }
         }
      }
      (void)POVMSAttr_Delete(&attr);
   }
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_Version, &f) == 0)
      opts.Language_Version = (int)(f * 100 + 0.5);

   (void)POVMSObject_Delete(gStartedStreamMessage);
   (void)POVMSObject_New(gStartedStreamMessage, kPOVMSType_WildCard);
   if(POVMSUtil_GetBool(msg, kPOVAttrib_AllConsole, &b) == 0)
   {
      (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_AllConsole, b);
      (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_DebugConsole, b);
      (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_FatalConsole, b);
      (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_RenderConsole, b);
      (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_StatisticsConsole, b);
      (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_WarningConsole, b);
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_DebugConsole, &b) == 0)
      (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_DebugConsole, b);
   if(POVMSUtil_GetBool(msg, kPOVAttrib_FatalConsole, &b) == 0)
      (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_FatalConsole, b);
   if(POVMSUtil_GetBool(msg, kPOVAttrib_RenderConsole, &b) == 0)
      (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_RenderConsole, b);
   if(POVMSUtil_GetBool(msg, kPOVAttrib_StatisticsConsole, &b) == 0)
      (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_StatisticsConsole, b);
   if(POVMSUtil_GetBool(msg, kPOVAttrib_WarningConsole, &b) == 0)
     (void)POVMSUtil_SetBool(gStartedStreamMessage, kPOVAttrib_WarningConsole, b);
   for(i = 0; i < gStreamTypeUtilDataCount; i++)
   {
      l = 0;
      if(POVMSUtil_GetStringLength(msg, gStreamTypeUtilData[i], &l) == kNoErr)
      {
          char *str = (char *)POV_MALLOC(l, "stream name");

          if(POVMSUtil_GetString(msg, gStreamTypeUtilData[i], str, &l) == kNoErr)
          {
             if(l > 1)
             {
                if(IsTrue(str) == true)
                   (void)POVMSUtil_SetString(gStartedStreamMessage, gStreamTypeUtilData[i], gStreamDefaultFile[i]);
                else if(IsFalse(str) == false)
                   (void)POVMSUtil_SetString(gStartedStreamMessage, gStreamTypeUtilData[i], str);
             }
          }

          POV_FREE(str);
      }
   }

   if(POVMSUtil_GetInt(msg, kPOVAttrib_Quality, &i) == 0)
   {
      opts.Quality = i;
      /* Emit a warning about the "radiosity" quality levels for
       * now.   We can get rid of this some time in the future.
       */
      if ((opts.Quality == 10) || (opts.Quality == 11))
      {
         Warning(0, "Quality settings 10 and 11 are no longer valid.");
         opts.Quality = 9;
      }
      else if ((opts.Quality < 0) || (opts.Quality > 9))
          Error("Illegal Quality setting.");
      opts.Quality_Flags = Quality_Values[opts.Quality];
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_Bounding, &b) == 0)
      opts.Use_Slabs = b;
   if(POVMSUtil_GetInt(msg, kPOVAttrib_BoundingThreshold, &i) == 0)
   {
      if(opts.BBox_Threshold < 1)
         Warning(0, "Too small bounding threshold adjusted to its minimum of one.");
      opts.BBox_Threshold = max(1, i);
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_LightBuffer, &b) == 0)
   {
      if(b == true)
         opts.Options |= USE_LIGHT_BUFFER;
      else
         opts.Options &= ~USE_LIGHT_BUFFER;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_VistaBuffer, &b) == 0)
   {
      if(b == true)
         opts.Options |= USE_VISTA_BUFFER;
      else
         opts.Options &= ~USE_VISTA_BUFFER;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_Radiosity, &b) == 0)
   {
      Warning(0, "Radiosity commandline/INI switch is not needed in POV-Ray 3.5.\n"
                 "Add a radiosity{}-block to your scene to turn on radiosity.");
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_RemoveBounds, &b) == 0)
   {
      if(b == true)
         opts.Options |= REMOVE_BOUNDS;
      else
         opts.Options &= ~REMOVE_BOUNDS;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_SplitUnions, &b) == 0)
   {
      if(b == true)
         opts.Options |= SPLIT_UNION;
      else
         opts.Options &= ~SPLIT_UNION;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_Antialias, &b) == 0)
   {
      if(b == true)
         opts.Options |= ANTIALIAS;
      else
         opts.Options &= ~ANTIALIAS;
   }
   if(POVMSUtil_GetInt(msg, kPOVAttrib_SamplingMethod, &i) == 0)
      opts.Tracing_Method = i;
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_AntialiasThreshold, &f) == 0)
      opts.Antialias_Threshold = f;
   if(POVMSUtil_GetInt(msg, kPOVAttrib_AntialiasDepth, &i) == 0)
   {
      opts.AntialiasDepth = i;
      if(opts.AntialiasDepth < 1)
         opts.AntialiasDepth = 1;
      if(opts.AntialiasDepth > 9)
         opts.AntialiasDepth = 9;
   }
   if(POVMSUtil_GetBool(msg, kPOVAttrib_Jitter, &b) == 0)
   {
      if(b == true)
         opts.Options |= JITTER;
      else
         opts.Options &= ~JITTER;
   }
   if(POVMSUtil_GetFloat(msg, kPOVAttrib_JitterAmount, &f) == 0)
   {
      opts.JitterScale = f;
      if(opts.JitterScale <= 0.0)
         opts.Options &= ~JITTER;
   }
   if(POVMSObject_Exist(msg, kPOVAttrib_IncludeHeader) == 0)
   {
      l = FILE_NAME_LENGTH;
      opts.Header_File_Name[0] = '\0';
      (void)POVMSUtil_GetString(msg, kPOVAttrib_IncludeHeader, opts.Header_File_Name, &l);
   }
   (void)POVMSObject_Get(msg, &opts.Declared_Variables, kPOVAttrib_Declare);

   if(result != NULL)
      (void)BuildRenderOptions(result);

   return 0;
}


/*****************************************************************************
*
* FUNCTION
*
*   Receive_RenderAll
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

int Receive_RenderAll(POVMSObjectPtr, POVMSObjectPtr, int, void *)
{
   if(Cooperate_Render_Flag != 1)
      return -1;

   Cooperate_Render_Flag = 2;

   return 0;
}


/*****************************************************************************
*
* FUNCTION
*
*   Receive_RenderArea
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

int Receive_RenderArea(POVMSObjectPtr msg, POVMSObjectPtr, int, void *)
{
   POVMSInt l,r,t,b;
   int err;

   if(Cooperate_Render_Flag != 1)
      return -1;

   err = POVMSUtil_GetInt(msg, kPOVAttrib_Left, &l);
   if(err == 0)
      err = POVMSUtil_GetInt(msg, kPOVAttrib_Right, &r);
   if(err == 0)
      err = POVMSUtil_GetInt(msg, kPOVAttrib_Top, &t);
   if(err == 0)
      err = POVMSUtil_GetInt(msg, kPOVAttrib_Bottom, &b);
   if(err == 0)
   {
      opts.First_Column = l;
      opts.Last_Column = r;
      opts.First_Line = t;
      opts.Last_Line = b;

      Cooperate_Render_Flag = 2;
   }

   return err;
}


/*****************************************************************************
*
* FUNCTION
*
*   Receive_RenderStop
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

int Receive_RenderStop(POVMSObjectPtr, POVMSObjectPtr, int, void *)
{
   if((Cooperate_Render_Flag != 1) && (Cooperate_Render_Flag != 2))
      return -1;

   if(Cooperate_Render_Flag == 1)
      Cooperate_Render_Flag = 3;
   else
      Stop_Flag = true;

   return 0;
}

END_POV_NAMESPACE
