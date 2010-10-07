/****************************************************************************
 *                  iff.cpp
 *
 * This file implements a simple IFF format file reader.
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
 * $File: //depot/povray/3.6-release/source/iff.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "iff.h"
#include "povray.h"
#include "pov_util.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

#define FORM 0x464f524dL
#define ILBM 0x494c424dL
#define BMHD 0x424d4844L
#define CAMG 0x43414d47L
#define CMAP 0x434d4150L
#define BODY 0x424f4459L
#define CMPNONE 0

#define HAM 0x800



/*****************************************************************************
* Local typedefs
******************************************************************************/



/*****************************************************************************
* Local variables
******************************************************************************/

static IMAGE_COLOUR *iff_colour_map; // GLOBAL VARIABLE
static int colourmap_size; // GLOBAL VARIABLE
static CHUNK_HEADER Chunk_Header; // GLOBAL VARIABLE



/*****************************************************************************
* Static functions
******************************************************************************/

static void iff_error(void);
static int read_byte(IStream *f);
static int read_word(IStream *f);
static long read_long(IStream *f);
static void Read_Chunk_Header(IStream *f, CHUNK_HEADER *dest);



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

static void iff_error()
{
  Error("Cannot read IFF image.");
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

static int read_byte(IStream *f)
{
  int c;

  if ((c = f->Read_Byte()) == EOF)
  {
    iff_error();
  }

  return (c);
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

static int read_word(IStream *f)
{
  int result;

  result = read_byte(f) * 256;

  result += read_byte(f);

  return (result);
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

static long read_long(IStream *f)
{
  int i;
  long result;

  result = 0;

  for (i = 0; i < 4; i++)
  {
    result = result * 256 + read_byte(f);
  }

  return (result);
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

static void Read_Chunk_Header(IStream *f, CHUNK_HEADER *dest)
{
  dest->name = read_long(f);
  dest->size = read_long(f);
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

void Read_Iff_Image(IMAGE *Image, char *filename)
{
  unsigned char **row_bytes;
  int c, i, j, k, nBytes, nPlanes = 0, compression = 0;
  int mask, byte_index, count, viewmodes;
  int Previous_Red, Previous_Green, Previous_Blue;
  unsigned long creg;
  IStream *f;
  IMAGE8_LINE *line;

  if ((f = Locate_File(filename, POV_File_Image_IFF,NULL,true)) == NULL)
  {
    Error("Cannot open IFF image.");
  }

  Previous_Red = Previous_Green = Previous_Blue = 0;

  viewmodes = 0;

  iff_colour_map = NULL;

  while (1)
  {
    Read_Chunk_Header(f, &Chunk_Header);

    switch (IFF_SWITCH_CAST Chunk_Header.name)
    {
      case FORM:

        if (read_long(f) != ILBM)
        {
          iff_error();
        }

        break;

      case BMHD:

        Image->iwidth = read_word(f);
        Image->iheight = read_word(f);

        Image->width = (DBL)Image->iwidth;
        Image->height = (DBL)Image->iheight;

        read_word(f); /* x position ignored */
        read_word(f); /* y position ignored */

        nPlanes = read_byte(f);

        colourmap_size = 1 << nPlanes;

        read_byte(f); /* masking ignored */

        compression = read_byte(f); /* masking ignored */

        read_byte(f); /* pad */
        read_word(f); /* Transparent colour ignored */
        read_word(f); /* Aspect ratio ignored */
        read_word(f); /* page width ignored */
        read_word(f); /* page height ignored */

        break;

      case CAMG:

        viewmodes = (int)read_long(f);  /* Viewmodes */

        if (viewmodes & HAM)
        {
          colourmap_size = 16;
        }

        break;

      case CMAP:

        colourmap_size = (int)Chunk_Header.size / 3;

        iff_colour_map = (IMAGE_COLOUR *)POV_MALLOC(sizeof(IMAGE_COLOUR) * colourmap_size, "IFF color map");

        for (i = 0; i < colourmap_size; i++)
        {
          iff_colour_map[i].Red = read_byte(f);
          iff_colour_map[i].Green = read_byte(f);
          iff_colour_map[i].Blue = read_byte(f);
          iff_colour_map[i].Filter = 0;
          iff_colour_map[i].Transmit = 0;
        }

        Previous_Red = iff_colour_map[0].Red;
        Previous_Green = iff_colour_map[0].Green;
        Previous_Blue = iff_colour_map[0].Blue;

        for (i = colourmap_size * 3; (long)i < Chunk_Header.size; i++)
        {
          read_byte(f);
        }

        break;

      case BODY:

        if ((iff_colour_map == NULL) || (viewmodes & HAM))
        {
          Image->Colour_Map_Size = 0;
          Image->Colour_Map = NULL;
        }
        else
        {
          Image->Colour_Map_Size = colourmap_size;
          Image->Colour_Map = iff_colour_map;
        }

        row_bytes = (unsigned char **)POV_MALLOC((4 * nPlanes), "IFF decoder line");

        for (i = 0; i < nPlanes; i++)
        {
          row_bytes[i] = (unsigned char *)POV_MALLOC((((Image->iwidth + 15) / 16) * 2), "IFF decoder line");
        }

        if (Image->Colour_Map == NULL)
        {
          Image->data.rgb8_lines = (IMAGE8_LINE *)POV_MALLOC(Image->iheight * sizeof(IMAGE8_LINE), "IFF image");
        }
        else
        {
          Image->data.map_lines = (unsigned char **)POV_MALLOC(Image->iheight * sizeof(unsigned char *), "IFF image");
        }

        for (i = 0; i < Image->iheight; i++)
        {
          if (Image->Colour_Map == NULL)
          {
            Image->data.rgb8_lines[i].red = (unsigned char *)POV_MALLOC(Image->iwidth, "IFF image line");
            Image->data.rgb8_lines[i].green = (unsigned char *)POV_MALLOC(Image->iwidth, "IFF image line");
            Image->data.rgb8_lines[i].blue = (unsigned char *)POV_MALLOC(Image->iwidth, "IFF image line");
          }
          else
          {
            Image->data.map_lines[i] = (unsigned char *)POV_MALLOC(Image->iwidth * sizeof(unsigned char), "IFF image line");
          }

          for (j = 0; j < nPlanes; j++)
          {
            if (compression == CMPNONE)
            {
              for (k = 0; k < (((Image->iwidth + 15) / 16) * 2); k++)
              {
                row_bytes[j][k] = (unsigned char)read_byte(f);
              }

              if ((k & 1) != 0)
              {
                read_byte(f);
              }
            }
            else
            {
              nBytes = 0;

              while (nBytes != ((Image->iwidth + 15) / 16) * 2)
              {
                c = read_byte(f);

                if ((c >= 0) && (c <= 127))
                {
                  for (k = 0; k <= c; k++)
                  {
                    row_bytes[j][nBytes++] = (unsigned char)read_byte(f);
                  }
                }
                else
                {
                  if ((c >= 129) && (c <= 255))
                  {
                    count = 257 - c;

                    c = read_byte(f);

                    for (k = 0; k < count; k++)
                    {
                      row_bytes[j][nBytes++] = (unsigned char)c;
                    }
                  }
                }
              }
            }
          }

          mask = 0x80;

          byte_index = 0;

          for (j = 0; j < Image->iwidth; j++)
          {
            creg = 0;

            for (k = nPlanes - 1; k >= 0; k--)
            {
              if (row_bytes[k][byte_index] & mask)
              {
                creg = creg * 2 + 1;
              }
              else
              {
                creg *= 2;
              }
            }

            if (viewmodes & HAM)
            {
              line = &Image->data.rgb8_lines[i];

              switch ((int)(creg >> 4))
              {
                case 0:

                  Previous_Red = line->red[j] = (unsigned char)iff_colour_map[creg].Red;
                  Previous_Green = line->green[j] = (unsigned char)iff_colour_map[creg].Green;
                  Previous_Blue = line->blue[j] = (unsigned char)iff_colour_map[creg].Blue;

                  break;

                case 1:

                  line->red[j] = (unsigned char)Previous_Red;
                  line->green[j] = (unsigned char)Previous_Green;
                  line->blue[j] = (unsigned char)(((creg & 0xf) << 4) + (creg & 0xf));

                  Previous_Blue = (int)line->blue[j];

                  break;

                case 2:

                  line->red[j] = (unsigned char)(((creg & 0xf) << 4) + (creg & 0xf));

                  Previous_Red = (int)line->red[j];

                  line->green[j] = (unsigned char)Previous_Green;
                  line->blue[j] = (unsigned char)Previous_Blue;

                  break;

                case 3:

                  line->red[j] = (unsigned char)Previous_Red;
                  line->green[j] = (unsigned char)(((creg & 0xf) << 4) + (creg & 0xf));

                  Previous_Green = (int)line->green[j];

                  line->blue[j] = (unsigned char)Previous_Blue;

                  break;
              }
            }
            else
            {
              if (nPlanes == 24)
              {
                line = &Image->data.rgb8_lines[i];

                line->red[j] = (unsigned char)((creg >> 16) & 0xFF);
                line->green[j] = (unsigned char)((creg >> 8) & 0xFF);
                line->blue[j] = (unsigned char)(creg & 0xFF);
              }
              else
              {
                if (creg > (unsigned long)Image->Colour_Map_Size)
                {
                  Error("IFF color out of range in image.");
                }

                Image->data.map_lines[i][j] = (char)creg;
              }
            }

            mask >>= 1;

            if (mask == 0)
            {
              mask = 0x80;

              byte_index++;
            }
          }
        }

        if (row_bytes != NULL)
        {
          for (i = 0; i < nPlanes; i++)
          {
            POV_FREE(row_bytes[i]);
          }

          POV_FREE(row_bytes);
        }

        if (f != NULL)
        {
           delete f;
        }

        return;

      default:

        for (i = 0; (long)i < Chunk_Header.size; i++)
        {
          if (f->Read_Byte() == EOF)
          {
            iff_error();
          }
        }

        break;
    }
  }
}

END_POV_NAMESPACE
