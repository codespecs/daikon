/****************************************************************************
 *               renderio.cpp
 *
 * This module implements the render image input and output.
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
 * $File: //depot/povray/3.6-release/source/renderio.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <time.h>
#include "frame.h"
#include "vector.h"
#include "bbox.h"
#include "chi2.h"
#include "colour.h"
#include "interior.h"
#include "lighting.h"
#include "normal.h"
#include "objects.h"
#include "octree.h"
#include "optout.h"
#include "povray.h"
#include "radiosit.h"
#include "ray.h"
#include "render.h"
#include "targa.h"
#include "texture.h"
#include "vbuffer.h"
#include "userio.h"
#include "userdisp.h"
#include "parse.h"
#include "tokenize.h"
#include "povmsend.h"
#include "renderio.h"
#include "colutils.h"
#include "pov_util.h"
#include "pov_util.h"
#include "pgm.h"
#include "ppm.h"
#include "targa.h"
#include "gif.h"
#include "png_pov.h"
#include "jpeg_pov.h"
#include "tiff_pov.h"

#include <algorithm>

#ifdef SYS_IMAGE_HEADER
#include SYS_IMAGE_HEADER
#endif

BEGIN_POV_NAMESPACE

static void Compensate_For_Alpha_AA(COLOUR out, COLOUR in);

/*****************************************************************************
*
* FUNCTION
*
*   Read_Rendered_Part
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
*   Sep 1994 : Call extract_colors to get pixel's color. [DB]
*
******************************************************************************/

void Read_Rendered_Part(char *New_Fname)
{
  int rc, x;
  int lineno = 0 ;
  unsigned char Red, Green, Blue, Alpha;
  DBL grey;

  maxclr = (DBL)(1 << Color_Bits) - 1.0;

  while ((rc = Output_File->Read_Line(Previous_Line)) == 1)
  {
    if (opts.Options & DISPLAY)
    {
      for (x = 0; x < Frame.Screen_Width ; x++)
      {
        extract_colors_nocorrect(Previous_Line[x], &Red, &Green, &Blue, &Alpha, &grey);
        if (Display_Started && x >= opts.First_Column && x < opts.Last_Column)
        {
          POV_DISPLAY_PLOT(opts.Preview_RefCon, x, Output_File->Line() - 1, Red, Green, Blue, Alpha);
          Red_Row_255[x] = Red;
          Green_Row_255[x] = Green;
          Blue_Row_255[x] = Blue;
          Alpha_Row_255[x] = Alpha;
        }
      }
      if (Display_Started)
      {
        POV_DISPLAY_PLOT_ROW(opts.Preview_RefCon, Frame.Screen_Width, Output_File->Line() - 1, 0, Frame.Screen_Width, Red_Row_255, Green_Row_255, Blue_Row_255, Alpha_Row_255);
      }
      Do_Cooperate(0);
    }
    if (++lineno == Frame.Screen_Height)
    {
      rc = 0 ;
      break ;
    }
  }

  opts.First_Line = Output_File->Line();
  if(opts.First_Line < 1)
  {
    opts.First_Line = 1;
  }

  if(Output_File != NULL)
  {
    delete Output_File;
    Output_File = NULL;
  }

  if (rc == 0)
  {
    Output_File = Open_Image(opts.Output_File_Type, New_Fname, Frame.Screen_Width, Frame.Screen_Height, APPEND_MODE, opts.First_Line);
    if (Output_File == NULL)
    {
      Error("Error opening output file.");
    }

    return;
  }

  Error("Error reading aborted data file.");
}



/*****************************************************************************
*
* FUNCTION
*
*   set_output_file_handle
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
*   Set the output file handle according to the file type used.
*
* CHANGES
*
* Oct 95 - Removed test where the output file handle was only set if
*          output_to_file was true. The output file handle structure
*          contains a pointer to read line, which is used by the continue
*          trace option. If you tried a continue trace with file output
*          manually turned OFF, then a GPF would occur due to a call to a
*          NULL function pointer.
*
******************************************************************************/

void init_output_file_handle()
{
  char separator_string[2] = {FILENAME_SEPARATOR, 0} ;
  char *def_ext = NULL;
  char temp[FILE_NAME_LENGTH];

  opts.Output_File_Type = NO_FILE;

    switch (opts.OutputFormat)
    {
      case '\0':
      case 's' :
      case 'S' :
        opts.Output_File_Type = SYS_FILE;
        def_ext=SYS_DEF_EXT;
        break;
      case 't' :
      case 'T' :
      case 'c' :
      case 'C' :
        opts.Output_File_Type = TGA_FILE;
        def_ext=".tga";
        break;
      case 'j' :
      case 'J' :
        opts.Output_File_Type = JPEG_FILE;
        def_ext=".jpg";
        break;
      case 'p' :
      case 'P' :
        opts.Output_File_Type = PPM_FILE;
        def_ext=".ppm";
        break;
      case 'n' :
      case 'N' :
        opts.Output_File_Type = PNG_FILE;
        def_ext=".png";
        break;
      case 'd' :
      case 'D' :
        Error ("Dump format no longer supported.");
        break;
      case 'r' :
      case 'R' :
        Error ("Raw format no longer supported.");
        break;
      default  :
        Error ("Unrecognized output file format %c.", opts.OutputFormat);
    }

    strcpy(temp,opts.Output_File_Name);
    
    POV_SPLIT_PATH(temp,opts.Output_Path,opts.Output_File_Name);

    if (opts.Output_File_Name[0] == '\0')
    {
      // [NC] fix for possible buffer overrun.
      // If we are here, def_ext != NULL so strlen and sprintf won't crash
      long n = FILE_NAME_LENGTH - strlen(def_ext) - 1;
      if (n < 0)
        n = 0;
      sprintf(opts.Output_File_Name, "%.*s%s", n, opts.Scene_Name, def_ext);
    }
    else if (!(opts.Options & TO_STDOUT))
    {
       if (!Has_Extension(opts.Output_File_Name))
       {
         strcat(opts.Output_File_Name, def_ext);
       }
    }

    strcpy(opts.Output_Numbered_Name,opts.Output_File_Name);

    if (opts.Output_Path [0] != 0)
    {
      if (getcwd (temp, sizeof (temp) - 1) == NULL)
        Error ("Failed to get current working directory while determining output path.") ;

      if (chdir(opts.Output_Path) == -1)
        Error ("Failed to chdir to '%s' while determining output path.", opts.Output_Path) ;

      if (getcwd (opts.Output_Path, sizeof (opts.Output_Path) - 1) == NULL)
        Error ("Failed to get current working directory while determining output path.") ;

      /* on some systems (MacOS) getcwd adds the path separator on the end */
      /* so only add it if it isn't already there...  [esp]                */
      if (opts.Output_Path[strlen(opts.Output_Path)-1] != FILENAME_SEPARATOR)
        strcat (opts.Output_Path, separator_string) ;

      if (chdir(temp) == -1)
        Error ("Failed to return to '%s' after determining output path.", temp) ;
    }
    else
    {
      if (getcwd (opts.Output_Path, sizeof (opts.Output_Path) - 1) == NULL)
        Error ("Failed to get current working directory while determining output path.") ;
      if (opts.Output_Path[strlen(opts.Output_Path)-1] != FILENAME_SEPARATOR)
        strcat (opts.Output_Path, separator_string) ;
    }
}


void destroy_output_file_handle()
{
	if(Output_File != NULL)
	{
		delete Output_File;
		Output_File = NULL;
	}
}


/*****************************************************************************
*
* FUNCTION
*
*   setup_output_file_name
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
*   Determine the file name for this frame.  For an animation, the frame
*   number is inserted into the file name.
*
* CHANGES
*
*   Jan-97  [esp]  Added conditional after getcwd, because Metrowerks getcwd
*                  function appends a path separator on output.
*
******************************************************************************/

void setup_output_file_name()
{
  char number_string[10];
  char *plast_period;
  int available_characters;
  int ilast_period;
  int fname_chars;

  /* This will create the real name for the file */
  if(opts.FrameSeq.FrameType!=FT_MULTIPLE_FRAME ||
     opts.Options & TO_STDOUT)
  {
    strcpy(opts.Output_Numbered_Name,opts.Output_File_Name);
  }
  else
  {
    /*
     * This is the maximum number of characters that can be used of the
     * original filename.  This will ensure that enough space is available
     * for the frame number in the filename
     */

    available_characters = POV_NAME_MAX-opts.FrameSeq.FrameNumWidth;

    plast_period = strrchr(opts.Output_File_Name, '.');

    if (plast_period == NULL)
    {
      Error("Illegal file name %s -- no extension.", opts.Output_File_Name);
    }

    ilast_period = plast_period - opts.Output_File_Name;

    fname_chars = ilast_period;

    if (fname_chars > available_characters)
    {
      /* Only give the warning once */

      if (opts.FrameSeq.FrameNumber == opts.FrameSeq.InitialFrame)
      {
        Warning(0, "Need to cut the output filename by %d characters.",
                ilast_period - available_characters);
      }

      fname_chars = available_characters;
    }

    /* Perform actual generation of filename */

    strncpy(opts.Output_Numbered_Name, opts.Output_File_Name, (unsigned)fname_chars);

    /* strncpy doesn't terminate if strlen(opts.Output_File_Name)<fname_chars */

    opts.Output_Numbered_Name[fname_chars]='\0';

    sprintf(number_string, "%0*d", opts.FrameSeq.FrameNumWidth, opts.FrameSeq.FrameNumber);

    strcat(opts.Output_Numbered_Name, number_string);

    strcat(opts.Output_Numbered_Name, &opts.Output_File_Name[ilast_period]);
  }

  strncpy (Actual_Output_Name,opts.Output_Path, sizeof (Actual_Output_Name));
  strncat (Actual_Output_Name,opts.Output_Numbered_Name, sizeof (Actual_Output_Name));
/*
Debug_Info("P='%s',O='%s',A='%s',N='%s'\n",opts.Output_Path,
  opts.Output_Numbered_Name, Actual_Output_Name,opts.Output_Numbered_Name);
*/
}


/*****************************************************************************
*
* FUNCTION
*
*   open_output_file
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
*   Open file and read in previous image if continued trace is on.
*
*   GOTCHA : This saves a POINTER to the file name, so the file
*            name must exist over the entire life/use of the file
*
* CHANGES
*
*   -
*
******************************************************************************/

void open_output_file()
{
  if (opts.Options & DISKWRITE)
  {
    Stage = STAGE_FILE_INIT;

    if (opts.Options & CONTINUE_TRACE)
    {
      Stage = STAGE_CONTINUING;

      Output_File = Open_Image(opts.Output_File_Type, Actual_Output_Name, Frame.Screen_Width, Frame.Screen_Height, READ_MODE);
      if (Output_File == NULL)
      {
        Warning(0,"Cannot open continue trace output file. "
                  "Opening new output file %s.", Actual_Output_Name);

        /* Turn off continue trace */

        opts.Options &= ~CONTINUE_TRACE;

        Output_File = Open_Image(opts.Output_File_Type, Actual_Output_Name, Frame.Screen_Width, Frame.Screen_Height, WRITE_MODE);
        if (Output_File == NULL)
        {
          Error ("Cannot open output file.");
        }
      }
    }
    else
    {
      Output_File = Open_Image(opts.Output_File_Type, Actual_Output_Name, Frame.Screen_Width, Frame.Screen_Height, WRITE_MODE);
      if (Output_File == NULL)
      {
        Error ("Cannot open output file.");
      }
    }
  }
}


/*****************************************************************************
*
* FUNCTION
*
*   Open_Image
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

Image_File_Class *Open_Image(int file_type, char *filename, int w, int h, int m, int l)
{
	Image_File_Class *i = NULL;

	if((file_type & PPM_FILE) == PPM_FILE)
	{
		i = new PPM_Image(filename, w, h, m, l);
		if(i != NULL)
		{
			if(i->Valid() == false)
			{
				delete i;
				i = NULL;
			}
		}
	}
	else if((file_type & TGA_FILE) == TGA_FILE)
	{
		i = new Targa_Image(filename, w, h, m, l);
		if(i != NULL)
		{
			if(i->Valid() == false)
			{
				delete i;
				i = NULL;
			}
		}
	}
	else if((file_type & PNG_FILE) == PNG_FILE)
	{
		i = new PNG_Image(filename, w, h, m, l);
		if(i != NULL)
		{
			if(i->Valid() == false)
			{
				delete i;
				i = NULL;
			}
		}
	}
	else if((file_type & JPEG_FILE) == JPEG_FILE)
	{
		i = new JPEG_Image(filename, w, h, m, l);
		if(i != NULL)
		{
			if(i->Valid() == false)
			{
				delete i;
				i = NULL;
			}
		}
	}
	else if((file_type & SYS_FILE) == SYS_FILE)
	{
		i = new SYS_IMAGE_CLASS(filename, w, h, m, l);
		if(i != NULL)
		{
			if(i->Valid() == false)
			{
				delete i;
				i = NULL;
			}
		}
	}

	return i;
}


/*****************************************************************************
*
* FUNCTION
*
*   output_prev_image_line_and_advance
*
*    Outputs Previous_Line, then swaps Previous_Line and Next_Line so that
*    we can advance and work on the next line.
*
* INPUT
*
*   current_y  - current line number
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

void output_prev_image_line_and_advance(int current_y)
{
  COLOUR *Temp_Colour_Ptr;
  char *Temp_Char_Ptr;

  if (current_y > opts.First_Line)
  {
    output_single_image_line_with_alpha_correction(Previous_Line, current_y-1);
  }

  Temp_Colour_Ptr = Previous_Line;
  Previous_Line   = Current_Line;
  Current_Line    = Temp_Colour_Ptr;

  Temp_Char_Ptr                   = Previous_Line_Antialiased_Flags;
  Previous_Line_Antialiased_Flags = Current_Line_Antialiased_Flags;
  Current_Line_Antialiased_Flags  = Temp_Char_Ptr;
}

/*****************************************************************************
*
* FUNCTION
*
*   output_single_image_line
*
* INPUT
*
*   Line  - Line to output  
*   y     - line number of the line being output
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

void output_single_image_line_with_alpha_correction(COLOUR *Line, int y)
{
  unsigned char Red, Green, Blue, Alpha;
  DBL grey;
  int i;

  if (opts.Options & OUTPUT_ALPHA)
  {
    // copy-and-convert from Line to Temp_Line
    for (i = 0; i <= Frame.Screen_Width ; i++)
    {
      Compensate_For_Alpha_AA(Temp_Line[i],Line[i]);

      extract_colors(Temp_Line[i], &Red, &Green, &Blue, &Alpha, &grey);
      Red_Row_255[i] = Red;
      Green_Row_255[i] = Green;
      Blue_Row_255[i] = Blue;
      Alpha_Row_255[i] = Alpha;
    }

    // now write Temp_Line instead of Line
    Line = Temp_Line;
  }
  else
  {
    for (i = 0; i <= Frame.Screen_Width ; i++)
    {
      extract_colors(Line[i], &Red, &Green, &Blue, &Alpha, &grey);
      Red_Row_255[i] = Red;
      Green_Row_255[i] = Green;
      Blue_Row_255[i] = Blue;
      Alpha_Row_255[i] = Alpha;
    }
  }

  if (opts.Options & DISKWRITE)
  {
    prepare_output_line(Line);
    Output_File->Write_Line(Line);
  }

  if (Display_Started)
  {
    POV_DISPLAY_PLOT_ROW(opts.Preview_RefCon, Frame.Screen_Width, y, opts.First_Column, opts.Last_Column, Red_Row_255, Green_Row_255, Blue_Row_255, Alpha_Row_255);
  }

  POV_WRITE_LINE (Line, y)
}

/*****************************************************************************
*
* FUNCTION
*
*   plot_pixel
*
* INPUT
*
*   x, y   - pixel location
*   Colour - pixel color
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
*   Display a pixel on the screen.
*
* CHANGES
*
*   Aug 1995 : Creation. Extracted from common code.
*
******************************************************************************/

void plot_pixel(int x, int  y, COLOUR Colour)
{
  unsigned char Red, Green, Blue, Alpha;
  DBL grey;
  COLOUR c;

  Compensate_For_Alpha_AA(c,Colour);

  if ((opts.Options & DISPLAY) && (y != opts.First_Line - 1) && (Display_Started))
  {
    extract_colors(c, &Red, &Green, &Blue, &Alpha, &grey);

    POV_DISPLAY_PLOT(opts.Preview_RefCon, x, y, Red, Green, Blue, Alpha);
  }
}


static void Compensate_For_Alpha_AA(COLOUR out, COLOUR in)
{
  Assign_Colour(out,in);

  if (opts.Options & OUTPUT_ALPHA)
  {
    /* when using the alpha channel, we change the weight */
    SNGL smax, weight;
    weight = min(max(1.0-in[pTRANSM], 0.0), 1.0);
    smax = max3(in[pRED], in[pGREEN], in[pBLUE]);
    if (smax > weight)
    {
      weight = smax;
      out[pTRANSM] = 1.0 - weight;
    }

    if (weight != 0.0)
    {
      out[pRED] /= weight;
      out[pGREEN] /= weight;
      out[pBLUE] /= weight;
    }
  }
}

END_POV_NAMESPACE
