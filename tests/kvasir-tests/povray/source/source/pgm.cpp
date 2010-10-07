/****************************************************************************
 *               pgm.cpp
 *
 * This module contains the code to read the PGM file format.
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
 * $File: //depot/povray/3.6-release/source/pgm.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

/****************************************************************************
*
*  PGM format according to NetPBM specs (http://netpbm.sourceforge.net/doc/):
*
*  This module implements read support for PGM (grayscale) image maps.
*
*  Both ASCII and binary files are supported ('P2' and 'P5')
*  in 8 and 16 bit color depth.
*
*****************************************************************************/

#include "frame.h"
#include "povray.h"
#include "pgm.h"
#include "ppm.h"
#include "pov_util.h"

#include <ctype.h>

BEGIN_POV_NAMESPACE

/*****************************************************************************
*
* FUNCTION
*
*  Read_ASCII_File_Number
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
*    Reads an integer number from an ASCII file skipping whitespaces
*
* CHANGES
*
*    August 2003 - Creation
*
******************************************************************************/

int Read_ASCII_File_Number(IStream *filep)
{
  int value;
  int pos = 0;
  char buffer[50];

  do
  {
    value = filep->Read_Byte();
  }
  while ( isspace(value) );

  if ( isdigit(value) ) buffer[pos] = (char)value;
  else return -1;

  while ( !isspace(value = filep->Read_Byte()) && (pos<48))
  {
    if ( isdigit(value) )
    {
      pos++;
      buffer[pos] = (char)value;
    }
    else return -1;
  }

  buffer[pos+1] = '\0';

  value = atoi(buffer);

  return value;
}

/*****************************************************************************
*
* FUNCTION
*
*  Read_PGM_Image
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
*    Reads an PGM image file
*
* CHANGES
*
*    August 2003 - New implementation based on targa/png reading code
*
******************************************************************************/

void Read_PGM_Image(IMAGE *Image, char *name)
{
  IStream *filep;
  unsigned char header[2];
  char line[1024];
  char *ptr;
  int nbr;

  int width, height;
  unsigned int depth;

  IMAGE_COLOUR *cmap;
  unsigned char *map_line;
  int data_hi, data_lo;
  int index, x, i;

  // --- Start by trying to open the file --- 
  if((filep = Locate_File(name,POV_File_Image_PGM,NULL,true)) == NULL)
    Error ("Cannot open PGM image %s.", name);

  // --- Read Header --- 
  if (!filep->read((char *)header, 2))
    Error ("Cannot read header of PGM image %s.", name);

  if(header[0] != 'P') Error ("File is not in PGM format.");

  if((header[1] != '2') && (header[1] != '5'))
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
    Error ("Cannot read width and height from PGM image.");

  if (width <= 0 || height <= 0)
    Error ("Invalid width or height read from PGM image.");

  do
  {
    filep->getline (line, 1024) ;
    line[1023] = '\0';
    if ((ptr = strchr(line, '#')) != NULL) *ptr = '\0';  // remove comment 
  }
  while (line[0]=='\0');  // read until line without comment from beginning 

  // --- Second: one number: color depth --- 
  if (sscanf(line,"%d",&depth) != 1)
    Error ("Cannot read color depth from PGM image.");

  if ((depth > 65535) || (depth < 1))
    Error ("Unsupported number of colors (%d) in PGM image.", depth);

  Image->iwidth = width;
  Image->iheight = height;
  Image->width = (DBL)width;
  Image->height = (DBL)height;

  if (depth < 256)
  {
    cmap = (IMAGE_COLOUR *)POV_MALLOC(depth*sizeof(IMAGE_COLOUR), "PGM image color map");

    for(index = 0; index < depth; index++)
    {
      cmap[index].Red = (index*255)/depth;
      cmap[index].Green = cmap[index].Red;
      cmap[index].Blue = cmap[index].Red;
      cmap[index].Filter = 0;
      cmap[index].Transmit = 0;
    }

    Image->Colour_Map = cmap;
    Image->Colour_Map_Size = depth;

    Image->data.map_lines = (unsigned char **)POV_MALLOC(height * sizeof(unsigned char *), "PGM image");

    for (i = 0; i < height; i++)
    {
      map_line = (unsigned char *)POV_MALLOC(width * sizeof(unsigned char), "PGM image line");

      if (header[1] == '2') // --- ASCII PGM file (type 2) --- 
      {
        for (x = 0; x < width; x++)
        {
          nbr = Read_ASCII_File_Number(filep);
          if (nbr >= 0) map_line[x] = nbr;
          else Error ("Cannot read image data from PGM image.");
        }
      }
      else                  // --- binary PGM file (type 5) --- 
      {
        for (x = 0; x < width; x++)
        {
          if ((nbr = filep->Read_Byte ()) == EOF)
            Error("Cannot read data from PGM image.");

          map_line[x] = nbr;
        }
      }

      Image->data.map_lines[i] = map_line;
    }
  }
  else // --- 16 bit PGM (binary or ASCII) --- 
  {
    Image->Colour_Map = NULL;
    Image->Colour_Map_Size = 0;
    Image->Image_Type |= IS16BITIMAGE;
    Image->Image_Type |= IS16GRAYIMAGE;

    Image->data.gray16_lines = (unsigned short **)POV_MALLOC(height * sizeof(unsigned short *), "PGM image");

    for (i = 0; i < height; i++)
    {
      Image->data.gray16_lines[i] = (unsigned short *)POV_MALLOC(width * sizeof(unsigned short), "PGM image line");

      if (header[1] == '2') // --- ASCII PGM file (type 2) --- 
      {
        for (x = 0; x < width; x++)
        {
          nbr = Read_ASCII_File_Number(filep);
          if (nbr >= 0) Image->data.gray16_lines[i][x] = nbr;
          else Error ("Cannot read image data from PGM image.");
        }
      }
      else                  // --- binary PGM file (type 5) --- 
      {
        for (x = 0; x < width; x++)
        {
          if ((data_hi = filep->Read_Byte ()) == EOF)
            Error ("Cannot read data from PGM image.");
          if ((data_lo = filep->Read_Byte ()) == EOF)
            Error ("Cannot read data from PGM image.");
          Image->data.gray16_lines[i][x] = (unsigned short)(256*data_hi + data_lo);
        }
      }

    }
  }

  // Close the image file 

  delete filep;
}

END_POV_NAMESPACE
