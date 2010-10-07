/****************************************************************************
 *               userio.cpp
 *
 * This module contains I/O routines.
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
 * $File: //depot/povray/3.6-release/source/userio.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <stdarg.h>
#include <ctype.h>
#include <algorithm>
#include "frame.h"
#include "vector.h"
#include "parse.h"
#include "povray.h"
#include "tokenize.h"
#include "userdisp.h"
#include "povms.h"
#include "pov_err.h"
#include "pov_util.h"
#include "userio.h"
#include "povmsend.h"
#include "pov_util.h"
#include "textstreambuffer.h"

BEGIN_POV_NAMESPACE

USING_POV_BASE_NAMESPACE

/*****************************************************************************
* Classes
******************************************************************************/

class DebugTextStreamBuffer : public TextStreamBuffer
{
	public:
		DebugTextStreamBuffer();
		~DebugTextStreamBuffer();
	protected:
		virtual void lineoutput(const char *str, unsigned int chars);
		virtual void directoutput(const char *str, unsigned int chars);
};


/*****************************************************************************
* Global variables
******************************************************************************/

END_POV_NAMESPACE

extern POVMSContext POVMS_Render_Context; // GLOBAL VARIABLE

BEGIN_POV_NAMESPACE


/*****************************************************************************
* Local variables
******************************************************************************/

const STAGENAME Stage_Names[STAGECOUNT] =
{
	{ STAGE_PREINIT, "Early Init" },
	{ STAGE_STARTUP, "Startup" },
	{ STAGE_BANNER, "Banner" },
	{ STAGE_INIT, "Init" },
	{ STAGE_FILE_INIT, "File Init" },
	{ STAGE_PARSING, "Parse" },
	{ STAGE_CONTINUING, "Continue Trace" },
	{ STAGE_RENDERING, "Rendering" },
	{ STAGE_SHUTDOWN, "Shutdown" },
	{ STAGE_CLEANUP_PARSE, "Cleanup Parse" },
	{ STAGE_SLAB_BUILDING, "Slab Building" },
	{ STAGE_TOKEN_INIT, "Scene File Parser Initialization" },
	{ STAGE_INCLUDE_ERR, "Parse" },
	{ STAGE_FOUND_INSTEAD, "Parse" }
};

DebugTextStreamBuffer Debug_Message_Buffer; // GLOBAL VARIABLE


/*****************************************************************************
* Local prototypes
******************************************************************************/

void CleanupString(char *str);


/****************************************************************************/
/* DebugTextStreamBuffer class. */
DebugTextStreamBuffer::DebugTextStreamBuffer() : TextStreamBuffer (1024*8, 160)
{
	// do nothing
}

DebugTextStreamBuffer::~DebugTextStreamBuffer()
{
	// do nothing
}

void DebugTextStreamBuffer::lineoutput(const char *str, unsigned int chars)
{
	POVMSObject msg;
	char buffer[256];

	buffer[0] = 0;
	strncat(buffer, str, min((unsigned int)252, chars));

	(void)POVMSObject_New(&msg, kPOVObjectClass_FileLoc);
	(void)POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText, buffer);
	(void)POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_Debug);
	(void)POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
	(void)POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);
}

void DebugTextStreamBuffer::directoutput(const char *, unsigned int)
{
	// do nothing
}

void FlushDebugMessageBuffer()
{
	Debug_Message_Buffer.flush();
}

/****************************************************************************/
/* Use this routine to display debug information. */
int Debug_Info(const char *format,...)
{
	va_list marker;
	char localvsbuffer[1024];

	va_start(marker, format);
	vsnprintf(localvsbuffer, 1023, format, marker);
	va_end(marker);

	Debug_Message_Buffer.printf("%s", localvsbuffer);

	Do_Cooperate(0);

	return 0;
}

/*
 * Use this routine to display non-fatal warning message if
 * opts.Language_Version is greater than level parameter.
 */
int Warning(unsigned int level, const char *format,...)
{
	va_list marker;
	POVMSObject msg;
	char localvsbuffer[1024];

	sprintf(localvsbuffer, "%s Warning: ", Stage_Names[Stage].stage_name);

	va_start(marker, format);
	vsnprintf(localvsbuffer + strlen(localvsbuffer), 1023 - strlen(localvsbuffer), format, marker);
	va_end(marker);

	CleanupString(localvsbuffer);

	if((opts.Warning_Level < 5) || ((opts.Warning_Level < 10) && (level == 0)))
		return 0;

	if(level >= opts.Language_Version)
		return 0;

	if((Stage == STAGE_PARSING) || (Stage == STAGE_INCLUDE_ERR) || (Stage == STAGE_FOUND_INSTEAD))
	{
		(void)POVMSObject_New(&msg, kPOVObjectClass_FileLoc);

		Where_Warning(&msg);

		(void)POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText, localvsbuffer);
		(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Warning, 0);
		(void)POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_Warning);
		(void)POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
		(void)POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);
	}
	else
	{
		(void)POVMSObject_New(&msg, kPOVObjectClass_FileLoc);
		(void)POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText, localvsbuffer);
		(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Warning, 0);
		(void)POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_Warning);
		(void)POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
		(void)POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);
	}

	Do_Cooperate(0);

	return 0;
}

/****************************************************************************/
/* Prints a non-fatal error message and adds file and line number */
int WarningAt(unsigned int level, const char *filename, long line, unsigned long offset, const char *format, ...)
{
	va_list marker;
	POVMSObject msg;
	char localvsbuffer[1024];

	sprintf(localvsbuffer, "%s Warning: ", Stage_Names[Stage].stage_name);

	va_start(marker, format);
	vsnprintf(localvsbuffer + strlen(localvsbuffer), 1023 - strlen(localvsbuffer), format, marker);
	va_end(marker);

	CleanupString(localvsbuffer);

	if((opts.Warning_Level < 5) || ((opts.Warning_Level < 10) && (level == 0)))
		return 0;

	if(level >= opts.Language_Version)
		return 0;

	(void)POVMSObject_New(&msg, kPOVObjectClass_FileLoc);
	(void)POVMSUtil_SetString(&msg, kPOVAttrib_FileName, filename);
	(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Line, line);
	(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Column, 0);
	(void)POVMSUtil_SetLong(&msg, kPOVAttrib_FilePosition, offset);

	(void)POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText, localvsbuffer);
	(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Warning, 0);
	(void)POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_FatalError);
	(void)POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
	(void)POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);

	Do_Cooperate(0);

	return 0;
}

/****************************************************************************/
/* Prints a non-fatal error message and adds file and line number if parsing. */
int PossibleError(const char *format,...)
{
	va_list marker;
	POVMSObject msg;
	char localvsbuffer[1024];

	sprintf(localvsbuffer, "Possible %s Error: ", Stage_Names[Stage].stage_name);

	va_start(marker, format);
	vsnprintf(localvsbuffer + strlen(localvsbuffer), 1023 - strlen(localvsbuffer), format, marker);
	va_end(marker);

	CleanupString(localvsbuffer);

	if(opts.Warning_Level == 0)
		return 0;

	if((Stage == STAGE_PARSING) || (Stage == STAGE_INCLUDE_ERR) || (Stage == STAGE_FOUND_INSTEAD))
	{
		(void)POVMSObject_New(&msg, kPOVObjectClass_FileLoc);

		Where_Warning(&msg);

		(void)POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText, localvsbuffer);
		(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Error, 0);
		(void)POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_Error);
		(void)POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
		(void)POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);
	}
	else
	{
		(void)POVMSObject_New(&msg, kPOVObjectClass_FileLoc);
		(void)POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText, localvsbuffer);
		(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Error, 0);
		(void)POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_Error);
		(void)POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
		(void)POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);
	}

	Do_Cooperate(0);

	return 0;
}

/****************************************************************************/
/* Prints a fatal error message and adds file and line number if parsing, then terminates. */
int Error(const char *format,...)
{
	va_list marker;
	POVMSObject msg;
	char localvsbuffer[1024];

	sprintf(localvsbuffer, "%s Error: ", Stage_Names[Stage].stage_name);

	va_start(marker, format);
	vsnprintf(localvsbuffer + strlen(localvsbuffer), 1023 - strlen(localvsbuffer), format, marker);
	va_end(marker);

	CleanupString(localvsbuffer);

	if((Stage == STAGE_PARSING) || (Stage == STAGE_INCLUDE_ERR) || (Stage == STAGE_FOUND_INSTEAD))
	{
		(void)POVMSObject_New(&msg, kPOVObjectClass_FileLoc);

		Where_Error(&msg);

		(void)POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText, localvsbuffer);
		(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Error, 0);
		(void)POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_FatalError);
		(void)POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
		(void)POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);
	}
	else
	{
		(void)POVMSObject_New(&msg, kPOVObjectClass_FileLoc);
		(void)POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText, localvsbuffer);
		(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Error, 0);
		(void)POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_FatalError);
		(void)POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
		(void)POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);
	}

	/* This could be just an "if" but we may add special messages later */
	if(Stage == STAGE_INCLUDE_ERR)
	{
		Warning(0, "Check that the file is in a directory specifed with a +L switch\n"
		           "or 'Library_Path=' .INI item. Standard include files are in the\n"
		           "include directory or folder. Please read your documentation carefully.");
	}

	Terminate_Tokenizer(); /* Closes scene file */

	POV_SHELLOUT(FATAL_SHL);

	povray_exit(1);

	return 0;
}

/****************************************************************************/
/* Prints a fatal error message and adds file and line number, then terminates. */
int ErrorAt(const char *filename, long line, unsigned long offset, const char *format, ...)
{
	va_list marker;
	POVMSObject msg;
	char localvsbuffer[1024];

	sprintf(localvsbuffer, "%s Error: ", Stage_Names[STAGE_PARSING].stage_name);

	va_start(marker, format);
	vsnprintf(localvsbuffer + strlen(localvsbuffer), 1023 - strlen(localvsbuffer), format, marker);
	va_end(marker);

	CleanupString(localvsbuffer);

	(void)POVMSObject_New(&msg, kPOVObjectClass_FileLoc);
	(void)POVMSUtil_SetString(&msg, kPOVAttrib_FileName, filename);
	(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Line, line);
	(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Column, 0);
	(void)POVMSUtil_SetLong(&msg, kPOVAttrib_FilePosition, offset);

	(void)POVMSUtil_SetString(&msg, kPOVAttrib_EnglishText, localvsbuffer);
	(void)POVMSUtil_SetInt(&msg, kPOVAttrib_Error, 0);
	(void)POVMSMsg_SetupMessage(&msg, kPOVMsgClass_RenderOutput, kPOVMsgIdent_FatalError);
	(void)POVMSMsg_SetDestinationAddress(&msg, FRONTEND_ADDRESS);
	(void)POVMS_Send(POVMS_Render_Context, &msg, NULL, kPOVMSSendMode_NoReply);

	Terminate_Tokenizer(); /* Closes scene file */

	POV_SHELLOUT(FATAL_SHL);

	povray_exit(1);

	return 0;
}

/****************************************************************************/
/* Use this routine to replace newlines with spaces in a string. */
void CleanupString(char *str)
{
	while(*str != 0)
	{
		if(*str == '\n')
			*str = ' ';
		str++;
	}
}


/*****************************************************************************
*
* FUNCTION
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void init_shellouts()
{
  int i;

  for (i=0; i < MAX_SHL; i++)
  {
    opts.Shellouts[i].Ret=IGNORE_RET;
    opts.Shellouts[i].Inverse=false;
    opts.Shellouts[i].Command[0]='\0';
  }
}


/*****************************************************************************
*
* FUNCTION
*
*   pov_shellout
*
* INPUT
*
*   template_command - the template command string to execute
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
*   Execute the command line described by the string being passed in
*
* CHANGES
*
*   -
*
******************************************************************************/

SHELLRET pov_shellout (SHELLTYPE Type)
{
  char real_command[POV_MAX_CMD_LENGTH];
  int i, j, l = 0;
  int length;
  SHELLRET Return_Code;
  char *s = NULL;
  char *template_command;


  if ( opts.Shellouts == NULL ) return(IGNORE_RET);

  template_command=opts.Shellouts[Type].Command;

  if ((length = strlen(template_command)) == 0)
  {
    return(IGNORE_RET);
  }

  switch(Type)
  {
    case PRE_SCENE_SHL:  s="pre-scene";   break;
    case PRE_FRAME_SHL:  s="pre-frame";   break;
    case POST_FRAME_SHL: s="post-frame";  break;
    case POST_SCENE_SHL: s="post-scene";  break;
    case USER_ABORT_SHL: s="user about";  break;
    case FATAL_SHL:      s="fatal error"; break;
    case MAX_SHL: /* To remove warnings*/ break;
  }

  Send_Progress(pov_tsprintf("Performing %s shell-out command", s), PROGRESS_PERFORMING_SHELLOUT_COMMAND);

  /* First, find the real command */

  for (i = 0, j = 0; i < length; )
  {
    if (template_command[i] == '%')
    {
      switch (toupper(template_command[i+1]))
      {
         case 'O':

          strncpy(&real_command[j], opts.Output_Numbered_Name, 
               (unsigned)(l=strlen(opts.Output_Numbered_Name)));

          break;

         case 'P':

          strncpy(&real_command[j], opts.Output_Path,(unsigned)(l=strlen(opts.Output_Path)));

          break;

         case 'S':

          strncpy(&real_command[j], opts.Scene_Name, (unsigned)(l=strlen(opts.Scene_Name)));

          break;

         case 'N':

          sprintf(&real_command[j],"%d",opts.FrameSeq.FrameNumber);
          l = strlen(&real_command[j]);

          break;

         case 'K':

          sprintf(&real_command[j],"%f",opts.FrameSeq.Clock_Value);
          l = strlen(&real_command[j]);

          break;

         case 'H':

          sprintf(&real_command[j],"%d",Frame.Screen_Height);
          l = strlen(&real_command[j]);

          break;

         case 'W':

          sprintf(&real_command[j],"%d",Frame.Screen_Width);
          l = strlen(&real_command[j]);

          break;

         case '%':

          real_command[j]='%';

          l=1;

          break;
       }

       j+=l;

       i+=2; /* we used 2 characters of template_command */
    }
    else
    {
      real_command[j++]=template_command[i++];
    }
  }

  real_command[j]='\0';

  Return_Code=(POV_SHELLOUT_CAST)POV_SYSTEM(real_command);

  if (opts.Shellouts[Type].Inverse)
  {
    Return_Code=(POV_SHELLOUT_CAST)(!((int)Return_Code));
  }

  if (Return_Code)
  {
    if (Type < USER_ABORT_SHL)
    {
      switch(opts.Shellouts[Type].Ret)
      {
        case FATAL_RET:

          Error("Fatal error returned from shellout command.");

          break;

        case USER_RET:

          Check_User_Abort(true); /* the true forces user abort */

          break;

        case QUIT_RET:

          povray_exit(0);

          break;

        case IGNORE_RET:
        case SKIP_ONCE_RET:
        case ALL_SKIP_RET: /* Added to remove warnings */
          break;
      }
    }

    return(opts.Shellouts[Type].Ret);
  }

  return(IGNORE_RET);
}

END_POV_NAMESPACE
