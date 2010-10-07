/****************************************************************************
 *               targa.cpp
 *
 * This module contains the code to read and write the Targa output file
 * format.
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
 * $File: //depot/povray/3.6-release/source/targa.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

/****************************************************************************
*
*  Explanation:
*
*    -
*
*  ---
*
*  May 1994 : Support for 24-bit RLE Targa output files added: John Baily
*             and David Payne.
*
*  Jul 1994 : Resume trace support and minor algorithm fix (one more still
*             needed, see comments in Write_Targa_Line); resume will force
*             Targa format to match the original trace format -- the T or C
*             format flag is adjusted as necessary: Charles Marslett,
*
*  Jun 1995 : Added support for 32-bit Targa input and output files.
*             The alpha channel has a value of 0 for 100% transparency
*             and a value of 255 for 0% transparency. [DB]
*
*****************************************************************************/

#include "frame.h"
#include "povray.h"
#include "targa.h"
#include "optout.h"
#include "pov_util.h"
#include "povmsend.h"
#include "colour.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/


/*****************************************************************************
* Local typedefs
******************************************************************************/

typedef struct pix
{
  DBL blue, green, red, transm;
} pix;



/*****************************************************************************
* Local variables
******************************************************************************/



/*****************************************************************************
* Static functions
******************************************************************************/

static void convert_targa_color (IMAGE_COLOUR *, unsigned, unsigned char *);


/*****************************************************************************
*
* FUNCTION
*
*   Open_Targa_File
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
*   Jun 1995 : Added code for 32 bit Targa files. [DB]
*
*   Sept 1995: Modified header output for Targa files. [AED]
*
******************************************************************************/

Targa_Image::Targa_Image(char *name, int w, int h, int m, int l)
{
  unsigned char tgaheader[18];

  mode = m;
  filename = name;
  in_file = NULL;
  out_file = NULL;
  width = w ;
  height = h ;
  line_number = l;

  switch (mode)
  {
    case READ_MODE:

      /* We can't resume from stdout. */
      if (opts.Options & TO_STDOUT || (in_file = New_Checked_IStream(name, POV_File_Image_Targa)) == NULL)
        return;

      /* Read targa header information. */

      if (!in_file->read((char *)tgaheader, 18))
        return;

      /* Decipher the header information */

      switch (tgaheader[2])
      {
        case 2  : opts.OutputFormat = 'T'; break;
        case 10 : opts.OutputFormat = 'C'; break;
        default : return;
      }

      switch (tgaheader[16])
      {
        case 24 : break;
        case 32 : opts.Options |= OUTPUT_ALPHA; break;
        default : return;
      }

      /* First_Column set to x offset.  Bytes 8, 9 */
      opts.First_Column = tgaheader[8] + (tgaheader[9] << 8);

      /* First line set to y offset.  Bytes 10, 11 */
      opts.First_Line = line_number = tgaheader[10] + (tgaheader[11]<<8);

      width  = tgaheader[12] + (tgaheader[13] << 8);
      height = tgaheader[14] + (tgaheader[15] << 8);

      if (w != width || h != height)
        Error ("Targa file dimensions do not match render resolution.");

      Send_Progress("Resuming interrupted trace", PROGRESS_RESUMING_INTERRUPTED_TRACE);

      break;

    case WRITE_MODE:

      if (opts.Options & TO_STDOUT)
      {
        out_file = New_OStream("stdout", POV_File_Image_Targa, false);
      }
      else if ((out_file = New_Checked_OStream(name, POV_File_Image_Targa, false)) == NULL)
      {
        return;
      }

      /* Output TGA file header info */
      out_file->Write_Byte(0);  /* Byte 0 - Length of Image ID field */

      out_file->Write_Byte(0);  /* Byte 1 - Color map type (0 is no color map) */

      switch(opts.OutputFormat)  /* Byte 2 - TGA file type */
      {
        case 't': /* Uncompressed True-Color Image */
        case 'T': 
        case 's': 
        case 'S': out_file->Write_Byte(2); break;

        case 'c': /* Run-length compressed True-Color Image */
        case 'C': out_file->Write_Byte(10); break;
      }

      out_file->Write_Byte(0);  /* Byte 3 - Index of first color map entry LSB */
      out_file->Write_Byte(0);  /* Byte 4 - Index of first color map entry MSB */

      out_file->Write_Byte(0);  /* Byte 5 - Color map length LSB */
      out_file->Write_Byte(0);  /* Byte 6 - Color map legth MSB */

      out_file->Write_Byte(0);  /* Byte 7 - Color map size */

      /* x origin set to "First_Column"  Bytes 8, 9 */

      out_file->Write_Byte(opts.First_Column % 256);
      out_file->Write_Byte(opts.First_Column / 256);

      /* y origin set to "First_Line"    Bytes 10, 11 */

      out_file->Write_Byte(opts.First_Line % 256);
      out_file->Write_Byte(opts.First_Line / 256);

      /* write width and height  Bytes 12 - 15 */

      out_file->Write_Byte(w % 256);
      out_file->Write_Byte(w / 256);
      out_file->Write_Byte(h % 256);
      out_file->Write_Byte(h / 256);

      /* We write 24 or 32 bits/pixel (16 million colors and alpha channel)
       * and also store the orientation and Alpha channel depth.  Bytes 16, 17.
       */
      if (opts.Options & OUTPUT_ALPHA)
      {
        out_file->Write_Byte(32);    /* 32 bits/pixel (BGRA) */
        out_file->Write_Byte(0x28);  /* Data starts at top left, 8 bits Alpha */
      }
      else
      {
        out_file->Write_Byte(24);    /* 24 bits/pixel (BGR) */
        out_file->Write_Byte(0x20);  /* Data starts at top left, 0 bits Alpha */
      }

      /* TGA file Image ID field data would go here (up to 255 chars) */

      width = w;
      height = h;

      break;

    case APPEND_MODE:

      if (opts.Options & TO_STDOUT)
      {
        out_file = New_OStream("stdout", POV_File_Image_Targa, true);
      }
      else if ((out_file = New_Checked_OStream(name, POV_File_Image_Targa, true)) == NULL)
      {
        return;
      }

      break;
  }

  valid = true;
}



/*****************************************************************************
*
* FUNCTION
*
*   Close_Targa_File
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

Targa_Image::~Targa_Image()
{
  /* Close the input file (if open) */
  if(in_file != NULL)
    delete in_file;

  /* Close the output file (if open) */
  if(out_file != NULL)
  {
    out_file->flush();
    delete out_file;
  }

  in_file = NULL;
  out_file = NULL;
}



/*****************************************************************************
*
* FUNCTION
*
*   Write_Targa_Pixel
*
* INPUT
*
*   handle     - Current file handel
*   r, g, b, f - Color values to write
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   Dan Farmer
*   
* DESCRIPTION   :
*
*   Moves redundant code to a single function.  Adding 16 bit grayscale
*   conditional code to each occurance of writing a pixel was getting a bit
*   too wordy.
*
* CHANGES
*
*   Jun 1995 : Added code for 32 bit Targa files. [DB]
*
*   Sept 1995: Modified handling of Alpha channel to use only opts.Options
*   Sept 1995: Modified handling of grayscale to use only opts.Options
*
******************************************************************************/

void Targa_Image::Write_Pixel(DBL b, DBL g, DBL r, DBL f)
{
  unsigned int gray;

  if (opts.Options & HF_GRAY_16)
  {
    /* Ouput heightfield in POV red/green format */
    gray = (int)floor((GREY_SCALE3(r,g,b)) * 65535.0);

    out_file->Write_Byte(0);
    out_file->Write_Byte(gray & 0xFF);
    if (!out_file->Write_Byte((gray >> 8) & 0xFF))
      Error("Cannot write TGA output data to %s.",filename);
  }
  else
  {
    /* Normal 24/32 bit pixel coloring */

    out_file->Write_Byte((int) floor(b * 255.0));
    out_file->Write_Byte((int) floor(g * 255.0));
    if (!out_file->Write_Byte((int) floor(r * 255.0)))
      Error("Cannot write TGA output data to %s.",filename);

    if (opts.Options & OUTPUT_ALPHA)
    {
      if (!out_file->Write_Byte((int) floor((1.0 - f) * 255.0)))
        Error("Cannot write TGA output data to %s.",filename);
    }
  }
}


/*****************************************************************************
*
* FUNCTION
*
*   Write_Targa_Line
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
*   Jun 1995 : Added code for 32 bit Targa files. [DB]
*
******************************************************************************/

void Targa_Image::Write_Line(COLOUR *line_data)
{
	if(valid == false)
		Error("Cannot access output image file.");

  register int x;
  int ptype, cnt, llen, startx;
  bool writenow;
  pix curpix, nexpix;

  switch (opts.OutputFormat)
  {
    case 't':
    case 'T':
    case 's':
    case 'S':

      for (x = 0; x < width; x++)
      {
        Write_Pixel(line_data[x][pBLUE], line_data[x][pGREEN], line_data[x][pRED], line_data[x][pTRANSM]);
      }

      break;

    case 'c':
    case 'C':

      llen = width;

      startx = 0;

      cnt = 1;

      curpix.blue   = line_data[startx][pBLUE];
      curpix.green  = line_data[startx][pGREEN];
      curpix.red    = line_data[startx][pRED];
      curpix.transm = line_data[startx][pTRANSM];

      ptype = 0;

      writenow = false;

      for (;;)
      {
        nexpix.blue   = line_data[startx+cnt][pBLUE];
        nexpix.green  = line_data[startx+cnt][pGREEN];
        nexpix.red    = line_data[startx+cnt][pRED];
        nexpix.transm = line_data[startx+cnt][pTRANSM];

        if ((nexpix.red == curpix.red) && (nexpix.blue == curpix.blue) &&
            (nexpix.green == curpix.green) && (nexpix.transm == curpix.transm))
        {
          if (ptype == 0)
          {
            cnt++;

            if ((cnt >= 128) || ((startx + cnt) >= llen))
            {
              writenow = true;
            }
          }
          else
          {
            cnt--;

            writenow = true;
          }
        }
        else
        {
          if ((ptype == 1) || (cnt <= 1))
          {
            ptype = 1;

            curpix = nexpix;

            cnt++;

            if ((cnt >= 128) || ((startx + cnt) >= llen))
            {
              writenow = true;
            }
          }
          else
          {
            writenow = true;
          }
        }

        if (writenow)
        {
          /* This test SHOULD be unnecessary!  However, it isn't!  [CWM] */

          if (startx + cnt > llen)
          {
            cnt = llen - startx;
          }

          if (ptype == 0)
          {
            out_file->Write_Byte((int) ((cnt - 1) | 0x80));

            Write_Pixel(curpix.blue, curpix.green, curpix.red, curpix.transm);

            curpix = nexpix;
          }
          else
          {
            out_file->Write_Byte((int) cnt - 1);

            for (x = 0; x < cnt; x++)
            {
               Write_Pixel(line_data[startx+x][pBLUE], line_data[startx+x][pGREEN], line_data[startx+x][pRED], line_data[startx+x][pTRANSM]);
            }
          }
          startx = startx + cnt;

          writenow = false;

          ptype = 0;

          cnt = 1;

          if (startx >= llen)
          {
             break; /* Exit while */
          }
        }
      }

      break; /* End of case */
  }

  out_file->flush();
}



/*****************************************************************************
*
* FUNCTION
*
*   Read_Targa_Line
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
*   Jun 1995 : Added code for 32 bit Targa files. [DB]
*
******************************************************************************/

int Targa_Image::Read_Line(COLOUR *line_data)
{
	if(valid == false)
		Error("Cannot access output image file.");

  int x, data, cnt;
  DBL rdata, gdata, bdata, fdata;

  switch (opts.OutputFormat)
  {
    case 't':
    case 'T':
    case 's':
    case 'S':

      for (x = 0; x < width; x++)
      {
        /* Read the BLUE data byte.  If EOF is reached on the first
         * character read, then this line hasn't been rendered yet.
         * Return 0.  If an EOF occurs somewhere within the line, this
         * is an error - return -1.
         */

        if ((data = in_file->Read_Byte ()) == EOF)
        {
          if (x == 0)
          {
            return 0;
          }
          else
          {
            return -1;
          }
        }

        line_data[x][pBLUE] = (DBL) data / 255.0;

        /* Read the GREEN data byte. */

        if ((data = in_file->Read_Byte ()) == EOF)
        {
          return -1;
        }

        line_data[x][pGREEN] = (DBL) data / 255.0;

        /* Read the RED data byte. */

        if ((data = in_file->Read_Byte ()) == EOF)
        {
          return -1;
        }

        line_data[x][pRED] = (DBL) data / 255.0;

        /* Read alpha channel. */

        if (opts.Options & OUTPUT_ALPHA)
        {
          if ((data = in_file->Read_Byte ()) == EOF)
          {
            return -1;
          }

          line_data[x][pTRANSM] = 1.0 - (DBL)data / 255.0;
        }
        else
        {
          line_data[x][pTRANSM] = 0.0;
        }
      }

      break;

    case 'c':
    case 'C':

      cnt = 0;

      do
      {
        if ((data = in_file->Read_Byte ()) == EOF)
        {
          if (cnt == 0)
            return 0;
          else
            return -1;
        }

        if (data & 0x80)
        {
          x = data & 0x7F;

          if ((data = in_file->Read_Byte ()) == EOF)
            return -1;

          bdata = (DBL) data / 255.0;

          if ((data = in_file->Read_Byte ()) == EOF)
            return -1;

          gdata = (DBL) data / 255.0;

          if ((data = in_file->Read_Byte ()) == EOF)
            return -1;

          rdata = (DBL) data / 255.0;

          /* Read alpha channel if any. */

          if (opts.Options & OUTPUT_ALPHA)
          {
            if ((data = in_file->Read_Byte ()) == EOF)
              return -1;

            fdata = 1.0 - (DBL)data / 255.0;
          }
          else
          {
            fdata = 0.0;
          }

          do
          {
            line_data[cnt][pBLUE]   = bdata;
            line_data[cnt][pGREEN]  = gdata;
            line_data[cnt][pRED]    = rdata;
            line_data[cnt][pTRANSM] = fdata;

            cnt++;
          }
          while (--x != -1);
        }
        else
        {
          x = data & 0x7F;

          do
          {
            if ((data = in_file->Read_Byte ()) == EOF)
              return -1;
            bdata = (DBL) data / 255.0;

            if ((data = in_file->Read_Byte ()) == EOF)
              return -1;
            gdata = (DBL) data / 255.0;

            if ((data = in_file->Read_Byte ()) == EOF)
              return -1;
            rdata = (DBL) data / 255.0;

            /* Read alpha channel if any. */

            if (opts.Options & OUTPUT_ALPHA)
            {
              if ((data = in_file->Read_Byte ()) == EOF)
                return -1;

              fdata = 1.0 - (DBL)data / 255.0;
            }
            else
            {
              fdata = 0.0;
            }

            line_data[cnt][pBLUE]   = bdata;
            line_data[cnt][pGREEN]  = gdata;
            line_data[cnt][pRED]    = rdata;
            line_data[cnt][pTRANSM] = fdata;

            cnt++;
          }
          while (--x != -1);
        }
      }
      while (cnt < width);

      if (cnt != width)
      {
        return -1;
      }

      break;
  }

  line_number++;

  return 1;
}



/*****************************************************************************
*
* FUNCTION
*
*   convert_targa_color
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

static void convert_targa_color(IMAGE_COLOUR *tcolor, unsigned pixelsize, unsigned char *bytes)
{
  switch (pixelsize)
  {
    case 1:

      tcolor->Red    = bytes[0];
      tcolor->Green  = bytes[0];
      tcolor->Blue   = bytes[0];
      tcolor->Filter = 0;
      tcolor->Transmit = 0;

      break;

    case 2:

      tcolor->Red    = ((bytes[1] & 0x7c) << 1);
      tcolor->Green  = (((bytes[1] & 0x03) << 3) | ((bytes[0] & 0xe0) >> 5)) << 3;
      tcolor->Blue   = (bytes[0] & 0x1f) << 3;
      tcolor->Filter = 0;
      tcolor->Transmit = 255 - (bytes[1] & 0x80 ? 255 : 0);

      break;

    case 3:

      tcolor->Red    = bytes[2];
      tcolor->Green  = bytes[1];
      tcolor->Blue   = bytes[0];
      tcolor->Filter = 0;
      tcolor->Transmit = 0;

      break;

    case 4:

      tcolor->Red    = bytes[2];
      tcolor->Green  = bytes[1];
      tcolor->Blue   = bytes[0];
      tcolor->Filter = 0;
      tcolor->Transmit = 255 - bytes[3];

      break;

    default:

      Error("Bad pixelsize in TGA color.");
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Read_Targa_Image
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
*   Reads a Targa image into an RGB image buffer.  Handles 8, 16, 24, 32 bit
*   formats.  Raw or color mapped. Simple raster and RLE compressed pixel
*   encoding. Right side up or upside down orientations.
*
* CHANGES
*
*   Jun 1995 : Added code for 32 bit Targa files. [DB]
*
******************************************************************************/

void Read_Targa_Image(IMAGE *Image, char *name)
{
  int h;
  int temp;
  unsigned i, j, k;
  unsigned char cflag = 0;
  unsigned char *map_line = NULL;
  unsigned char bytes[4];
  unsigned char tgaheader[18];
  unsigned char idbuf[256];
  unsigned ftype, idlen, cmlen, cmsiz, psize, orien;
  unsigned width, height;
  IStream *filep;
  IMAGE8_LINE *line_data = NULL;
  IMAGE_COLOUR *cmap;
  IMAGE_COLOUR pixel;

  /* Start by trying to open the file */

  if ((filep = Locate_File(name,POV_File_Image_Targa,NULL,true)) == NULL)
    Error ("Cannot open TGA image.");

  if (!filep->read((char *)tgaheader, 18))
    Error ("Cannot reading header of TGA image.");

  /* Decipher the header information */

  idlen  = tgaheader[ 0];
  ftype  = tgaheader[ 2];
  cmlen  = tgaheader[ 5] + (tgaheader[ 6] << 8);
  cmsiz  = tgaheader[ 7] / 8;
  width  = tgaheader[12] + (tgaheader[13] << 8);
  height = tgaheader[14] + (tgaheader[15] << 8);
  psize  = tgaheader[16] / 8;
  orien  = tgaheader[17] & 0x20; /* Right side up ? */

  Image->iwidth  = width;
  Image->iheight = height;
  Image->width   = (DBL)width;
  Image->height  = (DBL)height;
  Image->Colour_Map_Size = cmlen;
  Image->Colour_Map = NULL;

  /* Determine if this is a supported Targa type */

  if (ftype == 9 || ftype == 10 || ftype == 11)
  {
    cflag = 1;
  }
  else
  {
    if (ftype == 1 || ftype == 2 || ftype == 3)
    {
      cflag = 0;
    }
    else
    {
      Error("Unsupported file type %d in TGA image.", ftype);
    }
  }

  /* Skip over the picture ID information */

  if(idlen > 0)
  {
    if (!filep->read((char *)idbuf, idlen))
    Error ("Cannot read header from TGA image.");
  }

  /* Read in the the color map (if any) */

  if (cmlen > 0)
  {
    if (psize != 1)
    {
      Error("Unsupported color map bit depth (%d bpp) in TGA image.",
            psize * 8);
    }

    cmap = (IMAGE_COLOUR *)POV_MALLOC(cmlen * sizeof(IMAGE_COLOUR), "TGA color map");

    for (i = 0; i < cmlen; i++)
    {
      for (j = 0; j < cmsiz; j++)
      {
        if ((temp = filep->Read_Byte ()) == EOF)
        {
          Error("Cannot read color map from TGA image.");
        }
        else
        {
          bytes[j] = (unsigned char)temp;
        }
      }

      convert_targa_color(&cmap[i], cmsiz, bytes);
    }

    Image->Colour_Map = cmap;
  }
  else
  {
    Image->Colour_Map = NULL;
  }

  /* Allocate the buffer for the image */

  if (cmlen > 0)
  {
    Image->data.map_lines = (unsigned char **)POV_MALLOC(height * sizeof(unsigned char *), "TGA image");
  }
  else
  {
    Image->data.rgb8_lines = (IMAGE8_LINE *)POV_MALLOC(height * sizeof(IMAGE8_LINE), "TGA image");
  }

  for (i = 0; i < height; i++)
  {
    k = width * sizeof(unsigned char);

    if (cmlen > 0)
    {
      map_line = (unsigned char *)POV_MALLOC(k, "TGA image line");

      Image->data.map_lines[i] = map_line;
    }
    else
    {
      line_data = &Image->data.rgb8_lines[i];

      line_data->red    = (unsigned char *)POV_MALLOC(k, "TGA image line");
      line_data->green  = (unsigned char *)POV_MALLOC(k, "TGA image line");
      line_data->blue   = (unsigned char *)POV_MALLOC(k, "TGA image line");

      if (psize > 3)
      {
        line_data->transm = (unsigned char *)POV_MALLOC(k, "TGA image line");
      }
      else
      {
        line_data->transm = (unsigned char *)NULL;
      }
    }
  }

  /* Read the image into the buffer */

  if (cflag)
  {
    /* RLE compressed images */

    if (cmlen > 0)
    {
      if (orien)
      {
        map_line = Image->data.map_lines[0];
      }
      else
      {
        map_line = Image->data.map_lines[height-1];
      }
    }
    else
    {
      if (orien)
      {
        line_data = &Image->data.rgb8_lines[0];
      }
      else
      {
        line_data = &Image->data.rgb8_lines[height-1];
      }
    }

    i = 0; /* row counter */
    j = 0; /* column counter */

    while (i < height)
    {
      /* Grab a header */

      if ((h = filep->Read_Byte ()) == EOF)
      {
        Error("Cannot read data from TGA image.");
      }

      if (h & 0x80)
      {
        /* Repeat buffer */

        h &= 0x7F;

        for (k = 0; k < psize; k++)
        {
          if ((temp = filep->Read_Byte ()) == EOF)
          {
            Error("Cannot read data from TGA image.");
          }
          else
          {
            bytes[k] = (unsigned char)temp;
          }
        }

        if (cmlen == 0)
        {
          convert_targa_color(&pixel, psize, bytes);
        }

        for (; h >= 0; h--)
        {
          if (cmlen > 0)
          {
            map_line[j] = bytes[0];
          }
          else
          {
            line_data->red[j]    = (unsigned char)pixel.Red;
            line_data->green[j]  = (unsigned char)pixel.Green;
            line_data->blue[j]   = (unsigned char)pixel.Blue;
            if (psize > 3)
            {
              line_data->transm[j] = (unsigned char)pixel.Transmit;
            }
          }

          if (++j == width)
          {
            i++;

            if (cmlen > 0)
            {
              if (orien)
              {
                map_line = Image->data.map_lines[i];
              }
              else
              {
                map_line = Image->data.map_lines[height-i-1];
              }
            }
            else
            {
              line_data += (orien ? 1 : -1);
            }

            j = 0;
          }
        }
      }
      else
      {
        /* Copy buffer */

        for (; h >= 0; h--)
        {
          for (k = 0; k < psize ; k++)
          {
            if ((temp = filep->Read_Byte ()) == EOF)
            {
              Error("Cannot reading data from TGA image.");
            }
            else
            {
              bytes[k] = (unsigned char)temp;
            }
          }

          if (cmlen > 0)
          {
            map_line[j] = bytes[0];
          }
          else
          {
            convert_targa_color(&pixel, psize, bytes);

            line_data->red[j]    = (unsigned char)pixel.Red;
            line_data->green[j]  = (unsigned char)pixel.Green;
            line_data->blue[j]   = (unsigned char)pixel.Blue;
            if (psize > 3)
            {
              line_data->transm[j] = (unsigned char)pixel.Transmit;
            }
          }

          if (++j == width)
          {
            i++;

            if (cmlen > 0)
            {
              if (orien)
              {
                map_line = Image->data.map_lines[i];
              }
              else
              {
                map_line = Image->data.map_lines[height-i-1];
              }
            }
            else
            {
              line_data += (orien ? 1 : -1);
            }

            j = 0;
          }
        }
      }
    }
  }
  else
  {
    /* Simple raster image file, read in all of the pixels */

    if (cmlen == 0)
    {
      if (orien)
      {
        line_data = &Image->data.rgb8_lines[0];
      }
      else
      {
        line_data = &Image->data.rgb8_lines[height-1];
      }
    }

    for (i = 0; i < height; i++)
    {
      if (cmlen > 0)
      {
        if (orien)
        {
          map_line = Image->data.map_lines[i];
        }
        else
        {
          map_line = Image->data.map_lines[height-i-1];
        }
      }

      for (j = 0; j < width; j++)
      {
        for (k = 0; k < psize; k++)
        {
          if ((temp = filep->Read_Byte ()) == EOF)
          {
            Error("Cannot read data from TGA image.");
          }
          else
          {
            bytes[k] = (unsigned char)temp;
          }
        }

        if (cmlen > 0)
        {
          map_line[j] = bytes[0];
        }
        else
        {
          convert_targa_color(&pixel, psize, bytes);

          line_data->red[j]    = (unsigned char)pixel.Red;
          line_data->green[j]  = (unsigned char)pixel.Green;
          line_data->blue[j]   = (unsigned char)pixel.Blue;
          if (psize > 3)
          {
            line_data->transm[j] = (unsigned char)pixel.Transmit;
          }
        }
      }

      if (cmlen == 0)
      {
        line_data += (orien ? 1 : -1);
      }
    }
  }

  /* Any data following the image is ignored. */

  /* Close the image file */

  delete filep;
}

END_POV_NAMESPACE
