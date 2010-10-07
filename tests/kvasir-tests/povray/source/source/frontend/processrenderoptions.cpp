/****************************************************************************
 *               processrenderoptions.cpp
 *
 * This module contains the C++ interface for render option processing.
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
 * $File: //depot/povray/3.6-release/source/frontend/processrenderoptions.cpp $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#include "configfrontend.h"

#include "processrenderoptions.h"
#include "fileinputoutput.h"
#include "stringutilities.h"
#include "textstream.h"
#include "povms.h"
#include "povmsgid.h"
#include "pov_err.h"

BEGIN_POV_FRONTEND_NAMESPACE

USING_POV_BASE_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

#define kUseSpecialHandler	 kPOVMSType_WildCard
#define kNoParameter		 kPOVMSType_Null

/*****************************************************************************
* Local variables
******************************************************************************/

/* Supported output file types */
const char *Output_File_Types = "sStTcCpPnNdDrR";

/*
   Keyword table for the INI-file parser.
   The parser converts the INI-file options into a POVMS object using
   the specifications provided in this table. The first element is the
   INI-file keyword, the second element is the POVMS object attribute
   key, the third is the attribute type.
*/
struct ProcessOptions::INI_Parser_Table RenderOptions_INI_Table[] =
{
	{ "All_Console",		 kPOVAttrib_AllConsole,			kPOVMSType_Bool },
	{ "All_File",			 kPOVAttrib_AllFile,			kPOVObjectClass_File },
	{ "Antialias_Depth",	 kPOVAttrib_AntialiasDepth,		kPOVMSType_Int },
	{ "Antialias",			 kPOVAttrib_Antialias,			kPOVMSType_Bool },
	{ "Antialias_Threshold", kPOVAttrib_AntialiasThreshold,	kPOVMSType_Float },

	{ "Bits_Per_Color",		 kPOVAttrib_BitsPerColor,		kPOVMSType_Int },
	{ "Bits_Per_Colour",	 kPOVAttrib_BitsPerColor,		kPOVMSType_Int },
	{ "Bounding",			 kPOVAttrib_Bounding,			kPOVMSType_Bool },
	{ "Bounding_Threshold",	 kPOVAttrib_BoundingThreshold,	kPOVMSType_Int },
	{ "Buffer_Output",		 '****',						kPOVMSType_Bool },
	{ "Buffer_Size",		 '****',						kPOVMSType_Int },

	{ "Clock",				 kPOVAttrib_Clock,				kPOVMSType_Float },
//	{ "Compression",		 kPOVAttrib_Compression,		kPOVMSType_Int },
	{ "Continue_Trace",		 kPOVAttrib_ContinueTrace,		kPOVMSType_Bool },
	{ "Create_Histogram",	 kPOVAttrib_CreateHistogram,	kPOVMSType_Bool },
	{ "Create_Ini",			 kPOVAttrib_CreateIni,			kPOVObjectClass_File },
	{ "Cyclic_Animation",	 kPOVAttrib_CyclicAnimation,	kPOVMSType_Bool },

	{ "Debug_Console",		 kPOVAttrib_DebugConsole,		kPOVMSType_Bool },
	{ "Debug_File",			 kPOVAttrib_DebugFile,			kPOVObjectClass_File },
	{ "Declare",			 kPOVAttrib_Declare,			kUseSpecialHandler },
	{ "Display",			 kPOVAttrib_Display,			kPOVMSType_Bool },
	{ "Display_Gamma",		 kPOVAttrib_DisplayGamma,		kPOVMSType_Float },
	{ "Draw_Vistas",		 kPOVAttrib_DrawVistas,			kPOVMSType_Bool },

	{ "End_Column",			 kPOVAttrib_Right,				kPOVMSType_Float },
	{ "End_Row",			 kPOVAttrib_Bottom,				kPOVMSType_Float },

	{ "Fatal_Console",		 kPOVAttrib_FatalConsole,		kPOVMSType_Bool },
	{ "Fatal_Error_Command", kPOVAttrib_FatalErrorCommand,	kUseSpecialHandler },
	{ "Fatal_Error_Return",	 kPOVAttrib_FatalErrorCommand,	kUseSpecialHandler },
	{ "Fatal_File",			 kPOVAttrib_FatalFile,			kPOVObjectClass_File },
	{ "Field_Render",		 kPOVAttrib_FieldRender,		kPOVMSType_Bool },
	{ "Final_Clock",		 kPOVAttrib_FinalClock,			kPOVMSType_Float },
	{ "Final_Frame",		 kPOVAttrib_FinalFrame,			kPOVMSType_Int },

	{ "Height",				 kPOVAttrib_Height,				kPOVMSType_Int },
	{ "Histogram_Name",		 kPOVAttrib_HistogramFile,		kPOVObjectClass_File },
	{ "Histogram_Grid_Size", kPOVAttrib_HistogramGridSizeX,	kUseSpecialHandler },
	{ "Histogram_Type",		 kPOVAttrib_HistogramFileType,	kUseSpecialHandler },

	{ "Initial_Clock",		 kPOVAttrib_InitialClock,		kPOVMSType_Float },
	{ "Initial_Frame",		 kPOVAttrib_InitialFrame,		kPOVMSType_Int },
	{ "Input_File_Name",	 kPOVAttrib_InputFile,			kPOVObjectClass_File },
	{ "Include_Header",		 kPOVAttrib_IncludeHeader,		kPOVObjectClass_File },
	{ "Include_Ini",		 kPOVAttrib_IncludeIni,			kUseSpecialHandler },

	{ "Jitter_Amount",		 kPOVAttrib_JitterAmount,		kPOVMSType_Float },
	{ "Jitter",				 kPOVAttrib_Jitter,				kPOVMSType_Bool },

	{ "Library_Path",		 kPOVAttrib_LibraryPath,		kUseSpecialHandler },
	{ "Light_Buffer",		 kPOVAttrib_LightBuffer,		kPOVMSType_Bool },

	{ "Odd_Field",			 kPOVAttrib_OddField,			kPOVMSType_Bool },
	{ "Output_Alpha",		 kPOVAttrib_OutputAlpha,		kPOVMSType_Bool },
	{ "Output_File_Name",	 kPOVAttrib_OutputFile,			kPOVObjectClass_File },
	{ "Output_File_Type",	 kPOVAttrib_OutputFileType,		kUseSpecialHandler },
	{ "Output_To_File",		 kPOVAttrib_OutputToFile,		kPOVMSType_Bool },

	{ "Palette",			 kPOVAttrib_Palette,			kUseSpecialHandler },
	{ "Pause_When_Done",	 kPOVAttrib_PauseWhenDone,		kPOVMSType_Bool },
	{ "Post_Frame_Command",	 kPOVAttrib_PostFrameCommand,	kUseSpecialHandler },
	{ "Post_Frame_Return",	 kPOVAttrib_PostFrameCommand,	kUseSpecialHandler },
	{ "Post_Scene_Command",	 kPOVAttrib_PostSceneCommand,	kUseSpecialHandler },
	{ "Post_Scene_Return",	 kPOVAttrib_PostSceneCommand,	kUseSpecialHandler },
	{ "Preview_End_Size",	 kPOVAttrib_PreviewEndSize,		kPOVMSType_Int },
	{ "Preview_Start_Size",	 kPOVAttrib_PreviewStartSize,	kPOVMSType_Int },
	{ "Pre_Frame_Command",	 kPOVAttrib_PreFrameCommand,	kUseSpecialHandler },
	{ "Pre_Frame_Return",	 kPOVAttrib_PreFrameCommand,	kUseSpecialHandler },
	{ "Pre_Scene_command",	 kPOVAttrib_PreSceneCommand,	kUseSpecialHandler },
	{ "Pre_Scene_Return",	 kPOVAttrib_PreSceneCommand,	kUseSpecialHandler },

	{ "Quality",			 kPOVAttrib_Quality,			kPOVMSType_Int },

	{ "Radiosity",			 kPOVAttrib_Radiosity,			kPOVMSType_Bool },
	{ "Remove_Bounds",		 kPOVAttrib_RemoveBounds,		kPOVMSType_Bool },
	{ "Render_Console",		 kPOVAttrib_RenderConsole,		kPOVMSType_Bool },
	{ "Render_File",		 kPOVAttrib_RenderFile,			kPOVObjectClass_File },

	{ "Sampling_Method",	 kPOVAttrib_SamplingMethod,		kPOVMSType_Int },
	{ "Split_Unions",		 kPOVAttrib_SplitUnions,		kPOVMSType_Bool },
	{ "Start_Column",		 kPOVAttrib_Left,				kPOVMSType_Float },
	{ "Start_Row",			 kPOVAttrib_Top,				kPOVMSType_Float },
	{ "Statistic_Console",	 kPOVAttrib_StatisticsConsole,	kPOVMSType_Bool },
	{ "Statistic_File",		 kPOVAttrib_StatisticsFile,		kPOVObjectClass_File },
	{ "Subset_End_Frame",	 kPOVAttrib_SubsetEndFrame,		kPOVMSType_Int },
	{ "Subset_Start_Frame",	 kPOVAttrib_SubsetStartFrame,	kPOVMSType_Int },

	{ "Test_Abort_Count",	 kPOVAttrib_TestAbortCount,		kPOVMSType_Int },
	{ "Test_Abort",			 kPOVAttrib_TestAbort,			kPOVMSType_Bool },

	{ "User_Abort_Command",	 kPOVAttrib_UserAbortCommand,	kUseSpecialHandler },
	{ "User_Abort_Return",	 kPOVAttrib_UserAbortCommand,	kUseSpecialHandler },

	{ "Verbose",			 kPOVAttrib_Verbose,			kPOVMSType_Bool },
	{ "Version",			 kPOVAttrib_Version,			kPOVMSType_Float },
	{ "Video_Mode",			 kPOVAttrib_VideoMode,			kUseSpecialHandler },
	{ "Vista_Buffer",		 kPOVAttrib_VistaBuffer,		kPOVMSType_Bool },

	{ "Warning_Console",	 kPOVAttrib_WarningConsole,		kPOVMSType_Bool },
	{ "Warning_File",		 kPOVAttrib_WarningFile,		kPOVObjectClass_File },
	{ "Warning_Level",		 kPOVAttrib_WarningLevel,		kPOVMSType_Int },
	{ "Width",				 kPOVAttrib_Width,				kPOVMSType_Int },

	{ NULL, 0, 0 }
};

/*
   Keyword table for the command line parser.
   The parser converts the command line options into a POVMS object using
   the specifications provided in this table. The first element is the
   command keyword, the second element is the POVMS object attribute key
   of the parameter, the third is the attribute type and the last specifies
   is the +/- switch is used as boolean parameter if an attribute key is
   provided.
*/
struct ProcessOptions::Cmd_Parser_Table RenderOptions_Cmd_Table[] =
{
	{ "A0",	 kPOVAttrib_AntialiasThreshold,	kPOVMSType_Float,		kPOVAttrib_Antialias },
	{ "AM",	 kPOVAttrib_SamplingMethod,		kPOVMSType_Int,			kNoParameter },
	{ "A",	 kNoParameter,					kNoParameter,			kPOVAttrib_Antialias },

	{ "B",	 '****',						kPOVMSType_Int,			'****' },
	{ "B",	 kNoParameter,					kNoParameter,			'****' },

	{ "C",	 kNoParameter,					kNoParameter,			kPOVAttrib_ContinueTrace },

	{ "D",	 kPOVAttrib_Display,			kUseSpecialHandler,		kPOVAttrib_Display },
	{ "D",	 kNoParameter,					kNoParameter,			kPOVAttrib_Display },

	{ "EC",	 kPOVAttrib_Right,				kPOVMSType_Float,		kNoParameter },
	{ "EF0", kPOVAttrib_SubsetEndFrame,		kPOVMSType_Float,		kNoParameter },
	{ "EF",	 kPOVAttrib_SubsetEndFrame,		kPOVMSType_Int,			kNoParameter },
	{ "EP",	 kPOVAttrib_PreviewEndSize,		kPOVMSType_Int,			kNoParameter },
	{ "ER",	 kPOVAttrib_Bottom,				kPOVMSType_Float,		kNoParameter },

	{ "F",	 kPOVAttrib_OutputFileType,		kUseSpecialHandler,		kPOVAttrib_OutputToFile },
	{ "F",	 kNoParameter,					kNoParameter,			kPOVAttrib_OutputToFile },

	{ "GA",	 kPOVAttrib_AllFile,			kPOVObjectClass_File,	kPOVAttrib_AllConsole },
	{ "GA",	 kNoParameter,					kNoParameter,			kPOVAttrib_AllConsole },
	{ "GD",	 kPOVAttrib_DebugFile,			kPOVObjectClass_File,	kPOVAttrib_DebugConsole },
	{ "GD",	 kNoParameter,					kNoParameter,			kPOVAttrib_DebugConsole },
	{ "GF",	 kPOVAttrib_FatalFile,			kPOVObjectClass_File,	kPOVAttrib_FatalConsole },
	{ "GF",	 kNoParameter,					kNoParameter,			kPOVAttrib_FatalConsole },
	{ "GI",	 kPOVAttrib_CreateIni,			kPOVObjectClass_File,	kNoParameter },
	{ "GR",	 kPOVAttrib_RenderFile,			kPOVObjectClass_File,	kPOVAttrib_RenderConsole },
	{ "GR",	 kNoParameter,					kNoParameter,			kPOVAttrib_RenderConsole },
	{ "GS",	 kPOVAttrib_StatisticsFile,		kPOVObjectClass_File,	kPOVAttrib_StatisticsConsole },
	{ "GS",	 kNoParameter,					kNoParameter,			kPOVAttrib_StatisticsConsole },
	{ "GW",	 kPOVAttrib_WarningFile,		kPOVObjectClass_File,	kPOVAttrib_WarningConsole },
	{ "GW",	 kNoParameter,					kNoParameter,			kPOVAttrib_WarningConsole },

	{ "HI",	 kPOVAttrib_IncludeHeader,		kPOVObjectClass_File,	kNoParameter },
	{ "HN",	 kPOVAttrib_HistogramFile,		kPOVObjectClass_File,	kNoParameter },
	{ "HS",	 kPOVAttrib_HistogramGridSizeX,	kUseSpecialHandler,		kNoParameter },
	{ "HT",	 kPOVAttrib_HistogramFileType,	kUseSpecialHandler,		kPOVAttrib_CreateHistogram },
	{ "H",	 kPOVAttrib_Height,				kPOVMSType_Int,			kNoParameter },

	{ "I",	 kPOVAttrib_InputFile,			kPOVObjectClass_File,	kNoParameter },

	{ "J",	 kPOVAttrib_JitterAmount,		kPOVMSType_Float,		kPOVAttrib_Jitter },
	{ "J",	 kNoParameter,					kNoParameter,			kPOVAttrib_Jitter },

	{ "KC",	 kNoParameter,					kNoParameter,			kPOVAttrib_CyclicAnimation },
	{ "KI",	 kPOVAttrib_InitialClock,		kPOVMSType_Float,		kNoParameter },
	{ "KFF", kPOVAttrib_FinalFrame,			kPOVMSType_Int,			kNoParameter },
	{ "KFI", kPOVAttrib_InitialFrame,		kPOVMSType_Int,			kNoParameter },
	{ "KF",	 kPOVAttrib_FinalClock,			kPOVMSType_Float,		kNoParameter },
	{ "K",	 kPOVAttrib_Clock,				kPOVMSType_Float,		kNoParameter },

	{ "L",	 kPOVAttrib_LibraryPath,		kUseSpecialHandler,		kNoParameter },

	{ "MB",	 kPOVAttrib_BoundingThreshold,	kPOVMSType_Int,			kPOVAttrib_Bounding },
	{ "MB",	 kNoParameter,					kNoParameter,			kPOVAttrib_Bounding },
	{ "MV",	 kPOVAttrib_Version,			kPOVMSType_Float,		kNoParameter },

	{ "O",	 kPOVAttrib_OutputFile,			kPOVObjectClass_File,	kNoParameter },

	{ "P",	 kNoParameter,					kNoParameter,			kPOVAttrib_PauseWhenDone },

	{ "QR",	 kNoParameter,					kNoParameter,			kPOVAttrib_Radiosity },
	{ "Q",	 kPOVAttrib_Quality,			kPOVMSType_Int,			kNoParameter },

	{ "R",	 kPOVAttrib_AntialiasDepth,		kPOVMSType_Int,			kNoParameter },

	{ "SC",	 kPOVAttrib_Left,				kPOVMSType_Float,		kNoParameter },
	{ "SF0", kPOVAttrib_SubsetStartFrame,	kPOVMSType_Float,		kNoParameter },
	{ "SF",	 kPOVAttrib_SubsetStartFrame,	kPOVMSType_Int,			kNoParameter },
	{ "SP",	 kPOVAttrib_PreviewStartSize,	kPOVMSType_Int,			kNoParameter },
	{ "SR",	 kPOVAttrib_Top,				kPOVMSType_Float,		kNoParameter },
	{ "SU",	 kNoParameter,					kNoParameter,			kPOVAttrib_SplitUnions },

	{ "UA",	 kNoParameter,					kNoParameter,			kPOVAttrib_OutputAlpha },
	{ "UD",	 kNoParameter,					kNoParameter,			kPOVAttrib_DrawVistas },
	{ "UF",	 kNoParameter,					kNoParameter,			kPOVAttrib_FieldRender },
	{ "UL",	 kNoParameter,					kNoParameter,			kPOVAttrib_LightBuffer },
	{ "UO",	 kNoParameter,					kNoParameter,			kPOVAttrib_OddField },
	{ "UR",	 kNoParameter,					kNoParameter,			kPOVAttrib_RemoveBounds },
	{ "UV",	 kNoParameter,					kNoParameter,			kPOVAttrib_VistaBuffer },

	{ "V",	 kNoParameter,					kNoParameter,			kPOVAttrib_Verbose },

	{ "WL",	 kPOVAttrib_WarningLevel,		kPOVMSType_Int,			kNoParameter },
	{ "W",	 kPOVAttrib_Width,				kPOVMSType_Int,			kNoParameter },

	{ "X",	 kPOVAttrib_TestAbortCount,		kUseSpecialHandler,		kPOVAttrib_TestAbort },
	{ "X",	 kNoParameter,					kNoParameter,			kPOVAttrib_TestAbort },

	{ NULL, 0, 0, 0 }
};

ProcessRenderOptions::ProcessRenderOptions() : ProcessOptions(RenderOptions_INI_Table, RenderOptions_Cmd_Table)
{
}

ProcessRenderOptions::~ProcessRenderOptions()
{
}

int ProcessRenderOptions::ReadSpecialOptionHandler(INI_Parser_Table *option, char *param, POVMSObjectPtr obj)
{
	POVMSAttributeList list;
	double floatval = 0.0;
	int intval = 0;
	int intval2 = 0;
	int err = kNoErr;

	switch(option->key)
	{
		case kPOVAttrib_HistogramGridSizeX:
			if(sscanf(param, "%d.%d", &intval, &intval2) == 2)
			{
				err = POVMSUtil_SetInt(obj, kPOVAttrib_HistogramGridSizeX, intval);
				if(err == kNoErr)
					err = POVMSUtil_SetInt(obj, kPOVAttrib_HistogramGridSizeY, intval2);
			}
			else
			{
				ParseError("Invalid histogram grid size '%s'.", param);
				err = kParseErr;
			}
			break;
		case kPOVAttrib_Palette:
		case kPOVAttrib_VideoMode:
			while(isspace(*param))
				param++;
			err = POVMSUtil_SetInt(obj, option->key, tolower(*param));
			break;
		case kPOVAttrib_HistogramFileType:
		case kPOVAttrib_OutputFileType:
			while(isspace(*param))
				param++;
			if(strchr(Output_File_Types, *param) == NULL)
				ParseError("Unrecognized output file format %c.", *param);
			err = POVMSUtil_SetInt(obj, option->key, tolower(*param));
			break;
		case kPOVAttrib_IncludeIni:
		case kPOVAttrib_LibraryPath:
			POVMSAttribute attr;

			if(err == kNoErr)
			{
				// parse INI file (recursive)
				if(option->key == kPOVAttrib_IncludeIni)
					err = ParseFile(param, obj);

				// create list if it isn't there
				if(err == kNoErr)
				{
					if(POVMSObject_Exist(obj, option->key) == kFalseErr)
						err = POVMSAttrList_New(&list);
					else if(POVMSObject_Exist(obj, option->key) != kNoErr)
						err = kObjectAccessErr;
					else
						err = POVMSObject_Get(obj, &list, option->key);
				}
			}
			else
			{
				ParseError("File name or path parameter expected for option '%s', found '%s'.", option->keyword, param);
				err = kParseErr;
			}

			// add path or file to list
			if(err == kNoErr)
				err = POVMSAttr_New(&attr);
			if(err == kNoErr)
			{
				err = POVMSAttr_Set(&attr, kPOVMSType_CString, (void *)param, strlen(param) + 1);
				if(err == kNoErr)
					err = POVMSAttrList_Append(&list, &attr);
				else
					err = POVMSAttr_Delete(&attr);
			}
			if(err == kNoErr)
				err = POVMSObject_Set(obj, &list, option->key);
			break;
		case kPOVAttrib_Declare:
			POVMSObject decobj;

			// create list if it isn't there
			if(POVMSObject_Exist(obj, option->key) == kFalseErr)
				err = POVMSAttrList_New(&list);
			else if(POVMSObject_Exist(obj, option->key) != kNoErr)
				err = kObjectAccessErr;
			else
				err = POVMSObject_Get(obj, &list, option->key);

			// add value to list
			if(err == kNoErr)
				err = POVMSObject_New(&decobj, kPOVMSType_WildCard);
			if(err == kNoErr)
			{
				char *ptr = NULL;

				err = POVMSUtil_SetString(&decobj, kPOVAttrib_Identifier, strtok(param, "="));
				if(err == kNoErr)
				{
					ptr = strtok(NULL, "");
					if(ptr == NULL)
						err = kParseErr;
				}
				if(err == kNoErr)
				{
					if(strchr(ptr, '"') != NULL)
					{
						ptr = strchr(ptr, '"') + 1;
						strtok(ptr, "\"");
						err = POVMSUtil_SetString(&decobj, kPOVAttrib_Value, ptr);
					}
					else
						err = POVMSUtil_SetFloat(&decobj, kPOVAttrib_Value, atof(ptr));
				}
				if(err == kNoErr)
					err = POVMSAttrList_Append(&list, &decobj);
				else
					err = POVMSObject_Delete(&decobj);
			}
			if(err == kNoErr)
				err = POVMSObject_Set(obj, &list, option->key);
			break;
		case kPOVAttrib_FatalErrorCommand:
		case kPOVAttrib_PostFrameCommand:
		case kPOVAttrib_PostSceneCommand:
		case kPOVAttrib_PreFrameCommand:
		case kPOVAttrib_PreSceneCommand:
		case kPOVAttrib_UserAbortCommand:
			POVMSObject cmdobj;

			if(POVMSObject_Exist(obj, option->key) == kNoErr)
				err = POVMSObject_Get(obj, &cmdobj, option->key);
			else
				err = POVMSObject_New(&cmdobj, kPOVMSType_WildCard);
			if(toupper(*(option->keyword + strlen(option->keyword) - 1)) == 'D')
			{
				if(err == kNoErr)
					err = POVMSUtil_SetString(&cmdobj, kPOVAttrib_CommandString, param);
			}
			else
			{
				if(err == kNoErr)
				{
					int i = 0;

					if((*param == '-') || (*param == '!'))
						i = tolower(*(param + 1));
					else
						i = tolower(*param);
					err = POVMSUtil_SetInt(&cmdobj, kPOVAttrib_ReturnAction, i);
				}
			}
			if(err == kNoErr)
				err = POVMSObject_Set(obj, &cmdobj, option->key);
			break;
	}

	return err;
}

int ProcessRenderOptions::ReadSpecialSwitchHandler(Cmd_Parser_Table *option, char *param, POVMSObjectPtr obj, bool)
{
	int intval = 0;
	int intval2 = 0;
	int err = 0;
	char chr = 0;

	switch(option->key)
	{
		case kPOVAttrib_Display:
			if(param[0] != '\0')
			{
				err = POVMSUtil_SetInt(obj, kPOVAttrib_VideoMode, (int)toupper(param[0]));
				if((param[1] != '\0') && (err == 0))
					err = POVMSUtil_SetInt(obj, kPOVAttrib_Palette, (int)toupper(param[1]));
			}
			break;
		case kPOVAttrib_HistogramGridSizeX:
			if(sscanf(param, "%d.%d", &intval, &intval2) == 2)
			{
				err = POVMSUtil_SetInt(obj, kPOVAttrib_HistogramGridSizeX, intval);
				if(err == kNoErr)
					err = POVMSUtil_SetInt(obj, kPOVAttrib_HistogramGridSizeY, intval2);
			}
			else
			{
				ParseError("Invalid histogram grid size '%s'.", param);
				err = kParseErr;
			}
			break;
		case kPOVAttrib_OutputFileType:
			if(strchr(Output_File_Types, *param) == NULL)
				ParseError("Unrecognized output file format %c.", *param);
			err = POVMSUtil_SetInt(obj, option->key, tolower(*param));
			param++;
			if((err == kNoErr) && (*param > ' '))
			{
				if(isdigit(*param) != 0)
				{
					if(sscanf(param, "%d", &intval) == 1)
						err = POVMSUtil_SetInt(obj, kPOVAttrib_BitsPerColor, intval);
					else
					{
						ParseError("Invalid bits per color '%s'.", param);
						err = kParseErr;
					}
				}
				else
				{
					ParseError("Missing bits per color, '%s' found instead.", param);
					err = kParseErr;
				}
			}
			break;
		case kPOVAttrib_HistogramFileType:
			if(strchr(Output_File_Types, *param) == NULL)
			{
				ParseError("Unrecognized output file format %c.", *param);
				err = kParseErr;
			}
			else
			{
				chr = tolower(*param);
				err = POVMSUtil_SetInt(obj, option->key, chr);
			}
			break;
		case kPOVAttrib_LibraryPath:
			POVMSAttributeList list;
			POVMSAttribute attr;

			if(err == kNoErr)
			{
				// create list if it isn't there
				if(POVMSObject_Exist(obj, option->key) == kFalseErr)
					err = POVMSAttrList_New(&list);
				else if(POVMSObject_Exist(obj, option->key) != kNoErr)
					err = kObjectAccessErr;
				else
					err = POVMSObject_Get(obj, &list, option->key);
			}
			else
			{
				ParseError("File name or path parameter expected for switch '%s', found '%s'.", option->command, param);
				err = kParseErr;
			}

			// add path or file to list
			if(err == kNoErr)
				err = POVMSAttr_New(&attr);
			if(err == kNoErr)
			{
				err = POVMSAttr_Set(&attr, kPOVMSType_CString, (void *)param, strlen(param) + 1);
				if(err == kNoErr)
					err = POVMSAttrList_Append(&list, &attr);
				else
					err = POVMSAttr_Delete(&attr);
			}
			if(err == kNoErr)
				err = POVMSObject_Set(obj, &list, option->key);
			break;
		case kPOVAttrib_TestAbortCount:
			if((*param) == 0)
				break;
			if(sscanf(param, "%d", &intval) == 1)
				err = POVMSUtil_SetInt(obj, option->key, intval);
			else
			{
				ParseError("No or integer parameter expected for switch '%s', found '%s'.", option->command, param);
				err = kParseErr;
			}
			break;
	}

	return err;
}

int ProcessRenderOptions::WriteSpecialOptionHandler(INI_Parser_Table *option, POVMSObjectPtr obj, OTextStream *file)
{
	POVMSAttributeList list;
	POVMSFloat floatval;
	POVMSInt intval,intval2;
	int err = 0;
	int l;
	int i,imax;
	POVMSAttribute item;
	char *bufptr;
	char chr;

	switch(option->key)
	{
		case kPOVAttrib_HistogramGridSizeX:
			if(POVMSUtil_GetInt(obj, kPOVAttrib_HistogramGridSizeX, &intval) == 0)
			{
				if(POVMSUtil_GetInt(obj, kPOVAttrib_HistogramGridSizeY, &intval2) == 0)
					file->printf("%s=%d.%d\n", option->keyword, intval, intval2);
			}
			break;
		case kPOVAttrib_Palette:
		case kPOVAttrib_VideoMode:
		case kPOVAttrib_OutputFileType:
		case kPOVAttrib_HistogramFileType:
			if(POVMSUtil_GetInt(obj, option->key, &intval) == 0)
			{
				chr = intval;
				if(chr > 32)
					file->printf("%s=%c\n", option->keyword, chr);
			}
			break;
		case kPOVAttrib_IncludeIni:
			break;
		case kPOVAttrib_Declare:
			POVMSObject decobj;

			err = POVMSObject_Get(obj, &list, option->key);
			if(err != 0)
				break;

			l = 0;
			err = POVMSAttrList_Count(&list, &l);
			if(err != 0)
				break;
			if(l == 0)
				break;

			imax = l;
			for(i = 1; i <= imax; i++)
			{
				err = POVMSAttrList_GetNth(&list, i, &decobj);
				if(err == 0)
					err = POVMSObject_Get(&decobj, &item, kPOVAttrib_Identifier);
				if(err == 0)
				{
					l = 0;
					err = POVMSAttr_Size(&item, &l);
					if(l > 0)
					{
						bufptr = new char[l];
						bufptr[0] = 0;
						if((POVMSUtil_GetFloat(&decobj, kPOVAttrib_Value, &floatval) == 0) &&
						   (POVMSAttr_Get(&item, kPOVMSType_CString, bufptr, &l) == 0))
							file->printf("%s=%s=%g\n", option->keyword, bufptr, (float)floatval);
						delete[] bufptr;
					}
					(void)POVMSAttr_Delete(&item);
				}
			}
			break;
		case kPOVAttrib_LibraryPath:
			err = POVMSObject_Get(obj, &list, option->key);
			if(err != 0)
				break;

			l = 0;
			err = POVMSAttrList_Count(&list, &l);
			if(err != 0)
				break;
			if(l == 0)
				break;

			imax = l;
			for(i = 1; i <= imax; i++)
			{
				err = POVMSAttrList_GetNth(&list, i, &item);
				if(err == 0)
				{
					l = 0;
					err = POVMSAttr_Size(&item, &l);
					if(l > 0)
					{
						bufptr = new char[l];
						bufptr[0] = 0;
						if(POVMSAttr_Get(&item, kPOVMSType_CString, bufptr, &l) == 0)
							file->printf("%s=\"%s\"\n", option->keyword, bufptr);
						delete[] bufptr;
					}
					(void)POVMSAttr_Delete(&item);
				}
			}
			break;
		case kPOVAttrib_FatalErrorCommand:
		case kPOVAttrib_PostFrameCommand:
		case kPOVAttrib_PostSceneCommand:
		case kPOVAttrib_PreFrameCommand:
		case kPOVAttrib_PreSceneCommand:
		case kPOVAttrib_UserAbortCommand:
			POVMSObject cmdobj;

			err = POVMSObject_Get(obj, &cmdobj, option->key);
			if(err != 0)
				break;

			err = POVMSObject_Get(&cmdobj, &item, kPOVAttrib_CommandString);
			if(err == 0)
			{
				if(toupper(*(option->keyword + strlen(option->keyword) - 1)) == 'D')
				{
					l = 0;
					err = POVMSAttr_Size(&item, &l);
					if(l > 0)
					{
						bufptr = new char[l];
						bufptr[0] = 0;
						if(POVMSAttr_Get(&item, kPOVMSType_CString, bufptr, &l) == 0)
							file->printf("%s=%s\n", option->keyword, bufptr);
						delete[] bufptr;
					}
				}
				else
				{
					if(POVMSUtil_GetInt(&cmdobj, kPOVAttrib_ReturnAction, &intval) == 0)
					{
						if(intval < 0)
						{
							chr = -intval;
							file->printf("%s=!%c\n", option->keyword, chr);
						}
						else
						{
							chr = intval;
							file->printf("%s=%c\n", option->keyword, chr);
						}
					}
				}
			}
			if(err == 0)
				err = POVMSObject_Delete(&cmdobj);
			break;
	}

	return err;
}

bool ProcessRenderOptions::WriteOptionFilter(INI_Parser_Table *table)
{
	// So that we don't get both Bits_Per_Color and Bits_Per_Colour in the INI file.
	return (strcmp(table->keyword, "Bits_Per_Colour") != 0);
}

int ProcessRenderOptions::ProcessUnknownString(char *str, POVMSObjectPtr obj)
{
	POVMSAttributeList list;
	POVMSAttribute attr;
	int state = 0; // INI file
	int err = kNoErr;

	if(str == NULL)
	{
		ParseError("Expected filename, nothing was found.");
		return kParseErr;
	}

	// add filename or path

	// see if it is a POV file
	if(state == 0)
	{
		char *ptr = strrchr(str, '.');
		if(ptr != NULL)
		{
			if(pov_stricmp(ptr, ".pov") == 0)
				state = 1; // POV file
		}
	}

	// see if it is a path
	if(state == 0)
	{
		if(strlen(str) > 0)
		{
			if(str[strlen(str) - 1] == FILENAME_SEPARATOR)
				state = 2; // library path
		}
	}

	switch(state)
	{
		// INI file
		case 0:
			// parse INI file (recursive)
			err = ParseFile(str, obj);
			if(err == kNoErr)
			{
				// create list if it isn't there
				if(POVMSObject_Exist(obj, kPOVAttrib_IncludeIni) == kFalseErr)
					err = POVMSAttrList_New(&list);
				else if(POVMSObject_Exist(obj, kPOVAttrib_IncludeIni) != kNoErr)
					err = kObjectAccessErr;
				else
					err = POVMSObject_Get(obj, &list, kPOVAttrib_IncludeIni);
			}

			// add INI file to list
			if(err == kNoErr)
				err = POVMSAttr_New(&attr);
			if(err == kNoErr)
			{
				err = POVMSAttr_Set(&attr, kPOVMSType_CString, str, strlen(str) + 1);
				if(err == kNoErr)
					err = POVMSAttrList_Append(&list, &attr);
				else
					err = POVMSAttr_Delete(&attr);
			}
			if(err == kNoErr)
				err = POVMSObject_Set(obj, &list, kPOVAttrib_IncludeIni);
			break;
		// POV file
		case 1:
			// set POV file
			err = POVMSUtil_SetString(obj, kPOVAttrib_InputFile, str);
			break;
		// library path
		case 2:
			// create list if it isn't there
			if(POVMSObject_Exist(obj, kPOVAttrib_LibraryPath) == kFalseErr)
				err = POVMSAttrList_New(&list);
			else if(POVMSObject_Exist(obj, kPOVAttrib_LibraryPath) != kNoErr)
				err = kObjectAccessErr;
			else
				err = POVMSObject_Get(obj, &list, kPOVAttrib_LibraryPath);

			// add librarypath to list
			if(err == kNoErr)
				err = POVMSAttr_New(&attr);
			if(err == kNoErr)
			{
				err = POVMSAttr_Set(&attr, kPOVMSType_CString, str, strlen(str) + 1);
				if(err == kNoErr)
					err = POVMSAttrList_Append(&list, &attr);
				else
					err = POVMSAttr_Delete(&attr);
			}
			if(err == kNoErr)
				err = POVMSObject_Set(obj, &list, kPOVAttrib_LibraryPath);
			break;
	}

	return err;
}

ITextStream *ProcessRenderOptions::OpenFileForRead(const char *filename, POVMSObjectPtr obj)
{
	char buffer[FILE_NAME_LENGTH];

	return OpenINIFileStream(filename, POV_BASE_NAMESPACE::POV_File_Text_INI, buffer, obj);
}

OTextStream *ProcessRenderOptions::OpenFileForWrite(const char *filename, POVMSObjectPtr)
{
	return new OTextStream(filename, POV_BASE_NAMESPACE::POV_File_Text_INI);
}

ITextStream *ProcessRenderOptions::OpenINIFileStream(const char *filename, unsigned int stype, char *buffer, POVMSObjectPtr obj)
{
	int i,ii,l[4];
	char pathname[FILE_NAME_LENGTH];
	char file[FILE_NAME_LENGTH];
	char file_x[4][FILE_NAME_LENGTH];
	int cnt = 0;
	int ll;
	POVMSAttribute attr, item;

	if(Has_Extension(filename))
	{
		for(i = 0; i < 4; i++)
			l[i]=0;
	}
	else
	{
		for(i = 0; i < 4; i++)
		{
			if((l[i] = strlen(gPOV_File_Extensions[stype].ext[i])) > 0)
			{
				strcpy(file_x[i], filename);
				strcat(file_x[i], gPOV_File_Extensions[stype].ext[i]);
			}
		}
	}

	// Check the current directory first
	for(i = 0; i < 4; i++)
	{
		if(l[i])
		{
			if(EXIST_FILE(file_x[i]) == true)
			{
				strcpy(buffer,file_x[i]);
				return new ITextStream(file_x[i], stype);
			}
		}
	}
	if(EXIST_FILE(filename) == true)
	{
		strcpy(buffer,filename);
		return new ITextStream(filename, stype);
	}

	if(POVMSObject_Get(obj, &attr, kPOVAttrib_LibraryPath) != 0)
		return NULL;

	if(POVMSAttrList_Count(&attr, &cnt) != 0)
	{
		(void)POVMSAttrList_Delete(&attr);
		return NULL;
	}

	for (i = 1; i <= cnt; i++)
	{
		(void)POVMSAttr_New(&item);
		if(POVMSAttrList_GetNth(&attr, i, &item) != 0)
			continue;
		ll = 0;
		if(POVMSAttr_Size(&item, &ll) != 0)
		{
			(void)POVMSAttr_Delete(&item);
			continue;
		}
		if(ll <= 0)
		{
			(void)POVMSAttr_Delete(&item);
			continue;
		}
		if(POVMSAttr_Get(&item, kPOVMSType_CString, file, &ll) != 0)
		{
			(void)POVMSAttr_Delete(&item);
			continue;
		}
		(void)POVMSAttr_Delete(&item);

		file[strlen(file)+1] = '\0';
		if(file[strlen(file) - 1] != DRIVE_SEPARATOR)
			file[strlen(file)] = FILENAME_SEPARATOR;

		for(ii = 0; ii < 4; ii++)
		{
			if(l[ii])
			{
				strcpy(pathname, file);
				strcat(pathname, file_x[ii]);
				if(EXIST_FILE(pathname) == true)
				{
					strcpy(buffer,pathname);
					(void)POVMSAttrList_Delete(&attr);
					return new ITextStream(pathname, stype);
				}
			}
		}
		strcpy(pathname, file);
		strcat(pathname, filename);
		if(EXIST_FILE(pathname) == true)
		{
			strcpy(buffer,pathname);
			(void)POVMSAttrList_Delete(&attr);
			return new ITextStream(pathname, stype);
		}
	}

	(void)POVMSAttrList_Delete(&attr);

	if(l[0])
		ParseError("Could not find file '%s%s'", filename, gPOV_File_Extensions[stype].ext[0]);
	else
		ParseError("Could not find file '%s'", filename);

	return NULL;
}

END_POV_FRONTEND_NAMESPACE
