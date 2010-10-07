/****************************************************************************
 *               ppm.cpp
 *
 * This module contains the code to read and write the PPM file format.
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
 * $File: //depot/povray/3.6-release/source/ppm.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

/****************************************************************************
*
*  PPM format according to NetPBM specs (http://netpbm.sourceforge.net/doc/):
*
*  This module implements read support for PPM image maps and
*  write support for PPM output.
*
*  For reading both ASCII and binary files are supported ('P3' and 'P6').
*
*  For writing we use binary files. OutputQuality > 8 leads to 16 bit files.
*  Special handling of HF_GRAY_16 -> 16 bit PGM files ('P5')
*  All formats supported for writing can now also be used in continued trace.
*
*****************************************************************************/

#include "frame.h"
#include "povray.h"
#include "optout.h"
#include "pgm.h"
#include "ppm.h"
#include "pov_util.h"
#include "povmsend.h"
#include "colour.h"

#include <ctype.h>

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

/*****************************************************************************
* Local typedefs
******************************************************************************/

/*****************************************************************************
* Local variables
******************************************************************************/

/*****************************************************************************
* Static functions
******************************************************************************/

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

PPM_Image::PPM_Image(char *name, int w, int h, int m, int l)
{
  int  file_type;
  unsigned char header[2];
  char line[1024];
  char *ptr;
  int depth;

  mode = m;
  filename = name;
  in_file = NULL;
  out_file = NULL;
  width = w ;
  height = h ;
  line_number = l;

  // HF_GRAY_16 leads to 16 bit grayscale (PGM) output 
  if (opts.Options & HF_GRAY_16) file_type = POV_File_Image_PGM;
  else file_type = POV_File_Image_PPM;

  switch (mode)
  {
    case READ_MODE:

      // We can't resume from stdout. 
      if (opts.Options & TO_STDOUT || (in_file = New_Checked_IStream(name, file_type)) == NULL)
      {
        return;
      }

      // --- Read Header --- 
      if (!in_file->read((char *)header, 2)) return;

      if(header[0] != 'P') return;

      if((header[1] != '5') && (header[1] != '6')) return;

      do
      {
        in_file->getline (line, 1024);
        line[1023] = '\0';
        if ((ptr = strchr(line, '#')) != NULL) *ptr = '\0';  // remove comment 
      }
      while (line[0]=='\0');  // read until line without comment from beginning 

      // --- First: two numbers: with and height --- 
      if (sscanf(line,"%d %d",&width, &height) != 2) return;

      do
      {
        in_file->getline (line, 1024);
        line[1023] = '\0';
        if ((ptr = strchr(line, '#')) != NULL) *ptr = '\0';  // remove comment 
      }
      while (line[0]=='\0');  // read until line without comment from beginning 

      // --- Second: one number: color depth --- 
      if (sscanf(line,"%d",&depth) != 1) return;

      if ((depth > 65535) || (depth < 1)) return;

      if (w != width || h != height)
        Error ("PPM file dimensions do not match render resolution.");

      Send_Progress("Resuming interrupted trace", PROGRESS_RESUMING_INTERRUPTED_TRACE);

      break;

    case WRITE_MODE:

      if (opts.Options & TO_STDOUT)
      {
        out_file = New_OStream("stdout", file_type, false);
      }
      else if ((out_file = New_Checked_OStream(name, file_type, false)) == NULL)
      {
        return;
      }

      // HF_GRAY_16 leads to 16 bit grayscale (PGM) output 
      if (opts.Options & HF_GRAY_16)
      {
        out_file->printf("P5\n%d %d\n65535\n", w, h);
      }
      else
      {
        out_file->printf("P6\n%d %d\n%d\n", w, h, (1 << opts.OutputQuality) - 1);
      }

      width = w;
      height = h;

      break;

    case APPEND_MODE:

      if (opts.Options & TO_STDOUT)
      {
        out_file = New_OStream("stdout", file_type, true);
      }
      else if ((out_file = New_Checked_OStream(name, file_type, true)) == NULL)
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
*    Identical to targa version
*
******************************************************************************/

PPM_Image::~PPM_Image()
{
  // Close the input file (if open) 
  if(in_file != NULL)
    delete in_file;

  // Close the output file (if open) 
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
*  PPM_Image::Write_Line
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*    Christoph Hormann
*
* DESCRIPTION
*
*    Writes an image line to PPM image file
*
* CHANGES
*
*    August 2003 - New implementation based on targa/png code
*
******************************************************************************/

void PPM_Image::Write_Line(COLOUR *line_data)
{
  if(valid == false)
    Error("Cannot access output image file.");

  register int x;
  unsigned int rval, gval, bval, gray;
  unsigned int mask;
  COLC fac;

  for (x = 0; x < width; x++)
  {
    // HF_GRAY_16 leads to 16 bit grayscale (PGM) output 
    if (opts.Options & HF_GRAY_16)
    {
      gray = (int)floor((GREY_SCALE3(line_data[x][pRED],line_data[x][pGREEN],line_data[x][pBLUE])) * 65535.0);

      out_file->Write_Byte((gray >> 8) & 0xFF);
      if (!out_file->Write_Byte(gray & 0xFF))
        Error("Cannot write PPM output data to %s.",filename);
    }
    else
    // otherwise 3*OutputQuality bit pixel coloring written to 8 or 16 bit file
    {
      mask = (1 << opts.OutputQuality) - 1 ;
      fac = (COLC) (mask) ;

      rval = (unsigned int)floor(line_data[x][pRED] * fac) & mask;
      gval = (unsigned int)floor(line_data[x][pGREEN] * fac) & mask;
      bval = (unsigned int)floor(line_data[x][pBLUE] * fac) & mask;

      if (opts.OutputQuality>8)  // 16 bit per value 
      {
        out_file->Write_Byte(rval >> 8) ;
        out_file->Write_Byte(rval & 0xFF) ;
        out_file->Write_Byte(gval >> 8) ;
        out_file->Write_Byte(gval & 0xFF) ;
        out_file->Write_Byte(bval >> 8) ;
        if (!out_file->Write_Byte(bval & 0xFF))
        {
          Error("Error writing PPM output data to %s.",filename);
        }
      }
      else                       // 8 bit per value 
      {
        out_file->Write_Byte(rval & 0xFF) ;
        out_file->Write_Byte(gval & 0xFF) ;
        if (!out_file->Write_Byte(bval & 0xFF))
        {
          Error("Cannot write PPM output data to %s.",filename);
        }
      }
    }
  }

  line_number++;

  out_file->flush();
}

/*****************************************************************************
*
* FUNCTION
*
*  PPM_Image::Read_Line
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*    Christoph Hormann
*
* DESCRIPTION
*
*    Reads a PPM image line for continued trace
*
* CHANGES
*
*    August 2003 - New implementation based on targa/png code
*
******************************************************************************/

int PPM_Image::Read_Line(COLOUR *line_data)
{
  if(valid == false)
    Error("Cannot access output image file.");

  int data, x;
  int data_hi, data_lo;


  if (in_file->eof()) return 0;

  if (opts.Options & HF_GRAY_16)
  {
    for (x = 0; x < width; x++)
    {
      if ((data_hi = in_file->Read_Byte ()) == EOF)
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
      if ((data_lo = in_file->Read_Byte ()) == EOF) return -1;

      line_data[x][pRED]   = ((COLC)(256*data_hi + data_lo))/65535.0;
      line_data[x][pGREEN] = line_data[x][pRED];
      line_data[x][pBLUE]  = line_data[x][pRED];
    }
  }
  /* depending on the output Quality settings we expect
   * an 8 or 16 bit file.  So using continued trace requires
   * correct OutputQuality settings
   */
  else if (opts.OutputQuality>8)
  {
    COLC fac = (COLC)((1 << opts.OutputQuality) - 1);

    for (x = 0; x < width; x++)
    {
      /* Read the first data byte.  If EOF is reached on the first
       * character read, then this line hasn't been rendered yet.
       * Return 0.  If an EOF occurs somewhere within the line, this
       * is an error - return -1.
       */
      if ((data_hi = in_file->Read_Byte ()) == EOF)
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
      if ((data_lo = in_file->Read_Byte ()) == EOF) return -1;

      line_data[x][pRED] = ((COLC)(256*data_hi + data_lo))/fac;

      if ((data_hi = in_file->Read_Byte ()) == EOF) return -1;
      if ((data_lo = in_file->Read_Byte ()) == EOF) return -1;

      line_data[x][pGREEN] = ((COLC)(256*data_hi + data_lo))/fac;

      if ((data_hi = in_file->Read_Byte ()) == EOF) return -1;
      if ((data_lo = in_file->Read_Byte ()) == EOF) return -1;

      line_data[x][pBLUE] = ((COLC)(256*data_hi + data_lo))/fac;
    }
  }
  else
  {
    for (x = 0; x < width; x++)
    {
      /* Read the first data byte.  If EOF is reached on the first
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

      line_data[x][pRED]  = (DBL) data / 255.0;

      // Read the GREEN data byte. 

      if ((data = in_file->Read_Byte ()) == EOF)
      {
        return -1;
      }

      line_data[x][pGREEN] = (DBL) data / 255.0;

      // Read the BLUE data byte. 

      if ((data = in_file->Read_Byte ()) == EOF)
      {
        return -1;
      }

      line_data[x][pBLUE]  = (DBL) data / 255.0;
    }
  }

  line_number++;

  return 1;
}

/*****************************************************************************
*
* FUNCTION
*
*  Read_PPM_Image
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*    Christoph Hormann
*
* DESCRIPTION
*
*    Reads an PPM image file
*
* CHANGES
*
*    August 2003 - New implementation based on targa/png reading code
*
******************************************************************************/

void Read_PPM_Image(IMAGE *Image, char *name)
{
  IStream *filep;
  unsigned char header[2];
  char line[1024];
  char *ptr;
  int nbr;

  int width, height;
  unsigned int depth;

  IMAGE8_LINE *line_data;
  IMAGE16_LINE *line_16_data;
  int data_hi, data_lo;
  int x, i;

  // --- Start by trying to open the file --- 
  if((filep = Locate_File(name,POV_File_Image_PPM,NULL,true)) == NULL)
    Error ("Cannot open PPM image %s.", name);

  // --- Read Header --- 
  if (!filep->read((char *)header, 2))
    Error ("Cannot read header of PPM image %s.", name);

  if(header[0] != 'P') Error ("File is not in PPM format.");

  if((header[1] != '3') && (header[1] != '6'))
    Error ("File is not in PPM format (type %d).", header[1]);

  do
  {
    filep->getline (line, 1024);
    line[1023] = '\0';
    if ((ptr = strchr(line, '#')) != NULL) *ptr = '\0';  // remove comment 
  }
  while (line[0]=='\0');  // read until line without comment from beginning 

  // --- First: two numbers: with and height --- 
  if (sscanf(line,"%d %d",&width, &height) != 2)
    Error ("Cannot read width and height from PPM image.");

  if (width <= 0 || height <= 0)
    Error ("Invalid width or height read from PPM image.");

  do
  {
    filep->getline (line, 1024) ;
    line[1023] = '\0';
    if ((ptr = strchr(line, '#')) != NULL) *ptr = '\0';  // remove comment 
  }
  while (line[0]=='\0');  // read until line without comment from beginning 

  // --- Second: one number: color depth --- 
  if (sscanf(line,"%d",&depth) != 1)
    Error ("Cannot read color depth from PPM image.");

  if ((depth > 65535) || (depth < 1))
    Error ("Unsupported number of colors (%d) in PPM image.", depth);

  Image->iwidth = width;
  Image->iheight = height;
  Image->width = (DBL)width;
  Image->height = (DBL)height;
  Image->Colour_Map = NULL;
  Image->Colour_Map_Size = 0;

  if (depth < 256)
  {
    Image->data.rgb8_lines = (IMAGE8_LINE *)POV_MALLOC(height * sizeof(IMAGE8_LINE), "PPM image");

    for (i = 0; i < height; i++)
    {
      line_data = &Image->data.rgb8_lines[i];

      line_data->red    = (unsigned char *)POV_MALLOC(width, "PPM image line");
      line_data->green  = (unsigned char *)POV_MALLOC(width, "PPM image line");
      line_data->blue   = (unsigned char *)POV_MALLOC(width, "PPM image line");
      line_data->transm = (unsigned char *)NULL;

      if (header[1] == '3') // --- ASCII PPM file (type 3) --- 
      {
        for (x = 0; x < width; x++)
        {
          nbr = Read_ASCII_File_Number(filep);
          if (nbr >= 0) line_data->red[x] = (nbr*255)/depth;
          else Error ("Cannot read image data from PPM image.");
          nbr = Read_ASCII_File_Number(filep);
          if (nbr >= 0) line_data->green[x] = (nbr*255)/depth;
          else Error ("Cannot read image data from PPM image.");
          nbr = Read_ASCII_File_Number(filep);
          if (nbr >= 0) line_data->blue[x] = (nbr*255)/depth;
          else Error ("Cannot read image data from PPM image.");
        }
      }
      else                  // --- binary PPM file (type 6) --- 
      {
        for (x = 0; x < width; x++)
        {
          if ((nbr = filep->Read_Byte ()) == EOF)
            Error("Cannot read data from PPM image.");

          line_data->red[x] = (nbr*255)/depth;

          if ((nbr = filep->Read_Byte ()) == EOF)
            Error("Cannot read data from PPM image.");

          line_data->green[x] = (nbr*255)/depth;

          if ((nbr = filep->Read_Byte ()) == EOF)
            Error("Cannot read data from PPM image.");

          line_data->blue[x] = (nbr*255)/depth;
        }
      }
    }
  }
  else // --- 16 bit PPM (binary or ASCII) --- 
  {
    Image->Image_Type |= IS16BITIMAGE;

    Image->data.rgb16_lines = (IMAGE16_LINE *)POV_MALLOC(height * sizeof(IMAGE16_LINE), "PPM image");

    for (i = 0; i < height; i++)
    {
      line_16_data = &Image->data.rgb16_lines[i];

      line_16_data->red    = (unsigned short *)POV_MALLOC(width * sizeof(unsigned short), "PPM image line");
      line_16_data->green  = (unsigned short *)POV_MALLOC(width * sizeof(unsigned short), "PPM image line");
      line_16_data->blue   = (unsigned short *)POV_MALLOC(width * sizeof(unsigned short), "PPM image line");
      line_16_data->transm = (unsigned short *)NULL;

      if (header[1] == '3') // --- ASCII PPM file (type 3) --- 
      {
        for (x = 0; x < width; x++)
        {
          nbr = Read_ASCII_File_Number(filep);
          if (nbr >= 0) line_16_data->red[x] = (nbr*65535)/depth;
          else Error ("Cannot read image data from PPM image.");

          nbr = Read_ASCII_File_Number(filep);
          if (nbr >= 0) line_16_data->green[x] = (nbr*65535)/depth;
          else Error ("Cannot read image data from PPM image.");

          nbr = Read_ASCII_File_Number(filep);
          if (nbr >= 0) line_16_data->blue[x] = (nbr*65535)/depth;
          else Error ("Cannot read image data from PPM image.");
        }
      }
      else                  // --- binary PPM file (type 6) --- 
      {
        for (x = 0; x < width; x++)
        {
          if ((data_hi = filep->Read_Byte ()) == EOF)
            Error ("Cannot read data from PPM image.");
          if ((data_lo = filep->Read_Byte ()) == EOF)
            Error ("Cannot read data from PPM image.");
          line_16_data->red[x] = (256*data_hi + data_lo)*65535/depth;

          if ((data_hi = filep->Read_Byte ()) == EOF)
            Error ("Cannot read data from PPM image.");
          if ((data_lo = filep->Read_Byte ()) == EOF)
            Error ("Cannot read data from PPM image.");
          line_16_data->green[x] = (256*data_hi + data_lo)*65535/depth;

          if ((data_hi = filep->Read_Byte ()) == EOF)
            Error ("Cannot read data from PPM image.");
          if ((data_lo = filep->Read_Byte ()) == EOF)
            Error ("Cannot read data from PPM image.");
          line_16_data->blue[x] = (256*data_hi + data_lo)*65535/depth;
        }
      }
    }
  }

  // Close the image file 

  delete filep;
}

END_POV_NAMESPACE
