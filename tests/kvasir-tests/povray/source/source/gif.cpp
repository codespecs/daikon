/****************************************************************************
 *                  gif.cpp
 *
 * Gif-format file reader.
 *
 * NOTE:  Portions of this module were written by Steve Bennett and are used
 *        here with his permission.
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
 * $File: //depot/povray/3.6-release/source/gif.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

/*
 * The following routines were borrowed freely from FRACTINT, and represent
 * a generalized GIF file decoder.  This once seemed the best, most universal
 * format for reading in Bitmapped images, until Unisys began enforcing
 * its patent on the LZ compression that GIF uses.  POV-Ray, as freeware, is
 * exempt from GIF licensing fees.  GIF is a Copyright of Compuserve, Inc.
 *
 * Swiped and converted to entirely "C" coded routines by AAC for the most
 * in future portability!
 */

#include "frame.h"
#include "gif.h"
#include "gifdecod.h"
#include "povray.h"
#include "pov_util.h"
#include "fileinputoutput.h"

BEGIN_POV_NAMESPACE

USING_POV_BASE_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/



/*****************************************************************************
* Local typedefs
******************************************************************************/



/*****************************************************************************
* Local variables
******************************************************************************/

static IMAGE *Current_Image; // GLOBAL VARIABLE
static int Bitmap_Line; // GLOBAL VARIABLE
static IStream *Bit_File; // GLOBAL VARIABLE
unsigned char *decoderline  /*  [2049] */ ;  /* write-line routines use this */ // GLOBAL VARIABLE

static IMAGE_COLOUR *gif_colour_map; // GLOBAL VARIABLE
static int colourmap_size; // GLOBAL VARIABLE



/*****************************************************************************
* Static functions
******************************************************************************/



/*****************************************************************************
*
* FUNCTION
*
*   out_line
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

int out_line (unsigned char *pixels, int linelen)
{
  register int x;
  register unsigned char *line;

  if (Bitmap_Line == Current_Image->iheight)
  {
    Warning (0, "Extra data at end of GIF image.");
    return (0) ;
  }

  line = Current_Image->data.map_lines[Bitmap_Line++];

  for (x = 0; x < linelen; x++)
  {
    if ((int)(*pixels) > Current_Image->Colour_Map_Size)
    {
      Error ("Error - GIF image map color out of range.");
    }

    line[x] = *pixels;

    pixels++;
  }

  return (0);
}




/*****************************************************************************
*
* FUNCTION
*
*   gif_get_byte
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
*   Get byte from file, return the next byte or an error.
*
* CHANGES
*
*   -
*
******************************************************************************/

int gif_get_byte()
{
  register int byte;

  if ((byte = Bit_File->Read_Byte ()) != EOF)
  {
    return (byte);
  }
  else
  {
    Error ("Error reading data from GIF image.");
  }

  /* Keep the compiler happy. */

  return(0);
}



/*****************************************************************************
*
* FUNCTION
*
*   Read_Gif_Image
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
*   Main GIF file decoder.
*
* CHANGES
*
*   -
*
******************************************************************************/

void Read_Gif_Image(IMAGE *Image, char *filename)
{
  register int i, j, status;
  unsigned finished, planes;
  unsigned char buffer[16];

  status = 0;

  Current_Image = Image;

  if ((Bit_File = Locate_File(filename, POV_File_Image_GIF,NULL,true)) == NULL)
  {
    Error ("Error opening GIF image.");
  }

  /* Get the screen description. */

  for (i = 0; i < 13; i++)
  {
    buffer[i] = (unsigned char)gif_get_byte();
  }

  /* Use updated GIF specs. */

  if (strncmp((char *) buffer,"GIF",3) == 0)  /* Allow only GIF87 and GIF89 */
  {
    if ((buffer[3] != '8') || ((buffer[4] != '7') && (buffer[4]) != '9') ||
        (buffer[5] < 'A') || (buffer[5]) > 'z')
    {
      Error ("Unsupported GIF version %c%c%c.",
             buffer[3], buffer[4], buffer[5]);
    }
  }
  else
  {
    Error ("File is not in GIF format.");
  }

  planes = ((unsigned)buffer[10] & 0x0F) + 1;

  colourmap_size = (int)(1 << planes);

  gif_colour_map = (IMAGE_COLOUR *)POV_CALLOC(colourmap_size, sizeof(IMAGE_COLOUR), "GIF color map");

  /* Color map (better be!) */

  if ((buffer[10] & 0x80) == 0)
  {
    Error ("Error in GIF color map.");
  }

  for (i = 0; i < colourmap_size ; i++)
  {
    gif_colour_map[i].Red    = (unsigned char)gif_get_byte();
    gif_colour_map[i].Green  = (unsigned char)gif_get_byte();
    gif_colour_map[i].Blue   = (unsigned char)gif_get_byte();
    gif_colour_map[i].Filter = 0;
    gif_colour_map[i].Transmit = 0;
  }

  /* Now display one or more GIF objects. */

  finished = false;

  while (!finished)
  {
    switch (gif_get_byte())
    {
      /* End of the GIF dataset. */

      case ';':

        finished = true;
        status = 0;

        break;

      /* GIF Extension Block. */

      case '!':

        /* Read (and ignore) the ID. */

        gif_get_byte();

        /* Get data len. */

        while ((i = gif_get_byte()) > 0)
        {
          for (j = 0; j < i; j++)
          {
            /* Flush data. */

            gif_get_byte();
          }
        }

        break;

      /* Start of image object. Get description. */

      case ',':

        for (i = 0; i < 9; i++)
        {
          /* EOF test (?).*/

          if ((j = gif_get_byte()) < 0)
          {
            status = -1;

            break;
          }

          buffer[i] = (unsigned char) j;
        }

        /* Check "interlaced" bit. */

        if (j & 0x40)
        {
          Error ("Interlacing in GIF image unsupported.");
        }

        if (status < 0)
        {
          finished = true;

          break;
        }

        Image->iwidth  = buffer[4] | (buffer[5] << 8);
        Image->iheight = buffer[6] | (buffer[7] << 8);

        Image->width = (DBL) Image->iwidth;
        Image->height = (DBL) Image->iheight;

        Bitmap_Line = 0;

        Image->Colour_Map_Size = colourmap_size;
        Image->Colour_Map = gif_colour_map;

        Image->data.map_lines = (unsigned char **)POV_MALLOC(Image->iheight * sizeof(unsigned char *), "GIF image");

        for (i = 0 ; i < Image->iheight ; i++)
        {
          Image->data.map_lines[i] = (unsigned char *)POV_CALLOC(Image->iwidth, sizeof(unsigned char), "GIF image line");
        }

        /* Setup the color palette for the image. */

        decoderline = (unsigned char *)POV_CALLOC(Image->iwidth, sizeof(unsigned char), "GIF decoder line");

        /* Put bytes in Buf. */

        status = decoder(Image->iwidth);

        POV_FREE (decoderline);

        decoderline = NULL;

        finished = true;

        break;

      default:

        status = -1;

        finished = true;

        break;
    }
  }

  if (Bit_File != NULL)
  {
     delete Bit_File;
  }
}

END_POV_NAMESPACE
