/****************************************************************************
 *               histogra.cpp
 *
 * This module implements the methods CPU usage histogram output.
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
 * $File: //depot/povray/3.6-release/source/histogra.cpp $
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
#include "pov_util.h"
#include "povmsend.h"
#include "renderio.h"
#include "histogra.h"

BEGIN_POV_NAMESPACE

USING_POV_BASE_NAMESPACE

/*****************************************************************************
*
* FUNCTION
*
*   initialise_histogram
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Chris Cason
*
* DESCRIPTION
*
* Initialise histogram collation if available, otherwise force histogram off.
*
* CHANGES
*
******************************************************************************/

void initialise_histogram()
{
#if PRECISION_TIMER_AVAILABLE
  if (opts.histogram_on)
  {
    PRECISION_TIMER_INIT
    histogram_grid = (unsigned long *) POV_CALLOC (opts.histogram_x * opts.histogram_y, sizeof(unsigned long), "histogram grid");
  }
#else
  opts.histogram_on = false ;
#endif
}

/*****************************************************************************
*
* FUNCTION
*
*   destroy_histogram
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   Andreas Dilger
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void destroy_histogram()
{
#if PRECISION_TIMER_AVAILABLE
  if (Histogram_File != NULL)
  {
    delete Histogram_File;
    Histogram_File = NULL;
  }

  if (histogram_grid != NULL)
  {
    POV_FREE (histogram_grid) ;
    histogram_grid = NULL;
  }
#endif
}

/*****************************************************************************
*
* FUNCTION
*
*   accumulate_histogram
*
* INPUT
*
*   int x, y - pixel position on screen, on - starting pixel
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Chris Cason
*   
* DESCRIPTION
*
* Accumulate statistics on the histogram grid given on the command line.
* The total amount of time that the program spends in tracing pixels in the
* particular grid section that the pixel lies in is eventually written to an
* output file.
*
* CHANGES
*
* Oct 1995:  Modified to return if PRECISION_TIMER_AVAILABLE == 0 [CJC]
*
******************************************************************************/

void accumulate_histogram(int x, int  y, int  on)
{
#if PRECISION_TIMER_AVAILABLE
  int loc_x, loc_y;
  unsigned long  *p;

  if (y < 0 || y > Frame.Screen_Height)
  {
    return;
  }

  if (x < 0 || x > Frame.Screen_Width)
  {
    return;
  }

  if (!on)
  {
    PRECISION_TIMER_STOP

    /* determine which grid section the pixel lies in */

    loc_x = x * opts.histogram_x / Frame.Screen_Width;
    loc_y = y * opts.histogram_y / Frame.Screen_Height;

    p = histogram_grid + (long) loc_y * opts.histogram_x + loc_x;

    *p += PRECISION_TIMER_COUNT;

    if (*p > max_histogram_value)
    {
      max_histogram_value = *p;
    }
  }
  else
  {
    PRECISION_TIMER_START
  }
#else
	// silence compiler warnings
	x = y = on = 0;
#endif
}

/*****************************************************************************
*
* FUNCTION
*
*   write_histogram
*
* INPUT
*
*   filename - file to output the data to.
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Chris Cason
*   
* DESCRIPTION
*
* Processes and then writes the accumulated histogram statistics to a CSV
* (comma separated value) file. Programs such as Microsoft Excel can read CSV.
* Also accomodates image file output, such as Targa, for height field usage.
*
* CHANGES
*
* Sep 1995:  Added support for grayscale PNG histogram output [AED]
* Oct 1995:  Fixed TGA, PPM output for 0 - 1 range and use HF_GRAY_16 [AED]
* Oct 1995:  Modified to return if PRECISION_TIMER_AVAILABLE == 0 [CJC]
*
******************************************************************************/

void write_histogram(char *filename)
{
#if PRECISION_TIMER_AVAILABLE
  int x, y, x_step, y_step;
  unsigned long OptionsSave;
  int OutputQualitySave;
  unsigned long *p;
  OStream *f;
  COLOUR *line, *l;

  Send_Progress("Writing histogram file", PROGRESS_WRITE_HISTOGRAM_FILE);

  switch (opts.histogram_type)
  {
    case CSV :

      if((f = New_Checked_OStream(filename, POV_File_Text_CSV, false)) == NULL)
      {
        Warning (0, "Cannot open file %s for histogram output.", filename);

        return;
      }

      for(y = 0, p = histogram_grid; y < opts.histogram_y ; y++)
      {
        for(x = 0; x < opts.histogram_x ; x++)
        {
          f->printf("%s%010lu", x ? ", " : "", *p++);
        }

        f->printf("\n");
      }

      delete f;

      break;

    case PPM :
    case SYS :
    case TARGA :
    case PNG:

      OptionsSave = opts.Options;
      OutputQualitySave = opts.OutputQuality;

      opts.Options = HF_GRAY_16;
      opts.OutputQuality = 12;

      opts.histogram_file_type |= HIST_FTYPE;

      Histogram_File = Open_Image(opts.histogram_file_type, filename, Frame.Screen_Width, Frame.Screen_Height, WRITE_MODE);
      if (Histogram_File == NULL)
      {
        Warning (0, "Cannot open file %s for histogram output.", filename);
        return;
      }

      line = (COLOUR *)POV_CALLOC(Frame.Screen_Width, sizeof(COLOUR), "histogram buffer");

      y_step = Frame.Screen_Height / opts.histogram_y;
      x_step = Frame.Screen_Width / opts.histogram_x;

      for (y = 0; y < Frame.Screen_Height ; y++)
      {
        p = histogram_grid + (long) (y / y_step) * opts.histogram_x;

        for (x = 0, l = line; x < Frame.Screen_Width ; x++, l++)
        {
          (*l) [pRED] =
          (*l) [pGREEN] =
          (*l) [pBLUE] = (COLC)*(p + x / x_step) / max_histogram_value;
        }
        Histogram_File->Write_Line(line);
      }

      POV_FREE (line);

      delete Histogram_File;
      Histogram_File = NULL;

      opts.Options = OptionsSave;
      opts.OutputQuality = OutputQualitySave;

      break;

    case NONE:  /* Just to quiet warnings */
    default:
      break;
  }
#else
	// silence compiler warnings
	filename = NULL;
#endif
}

END_POV_NAMESPACE
