/*****************************************************************************
 *       jpeg_pov.cpp
 *
 * This module contains the code to read and write the JPEG output file.
 * (This file has been created using the PNG_POV.C file as template and
 *  and the example.c provided with ligjpeg.)
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
 * $File: //depot/povray/3.6-release/source/jpeg_pov.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

/*****************************************************************************
*
*  This code requires the use of jpeg-6b, The Independent JPEG Group's JPEG
*  software. The complete legal section below is taken from README in the
*  jpeg-6b directory, note that not all issues listed there may apply to this
*  distribution. For complete details read the README in the jpeg-6b directory.
*
******************************************************************************

LEGAL ISSUES
============

In plain English:

1. We don't promise that this software works.  (But if you find any bugs,
   please let us know!)
2. You can use this software for whatever you want.  You don't have to pay us.
3. You may not pretend that you wrote this software.  If you use it in a
   program, you must acknowledge somewhere in your documentation that
   you've used the IJG code.

In legalese:

The authors make NO WARRANTY or representation, either express or implied,
with respect to this software, its quality, accuracy, merchantability, or
fitness for a particular purpose.  This software is provided "AS IS", and you,
its user, assume the entire risk as to its quality and accuracy.

This software is copyright (C) 1991-1998, Thomas G. Lane.
All Rights Reserved except as specified below.

Permission is hereby granted to use, copy, modify, and distribute this
software (or portions thereof) for any purpose, without fee, subject to these
conditions:
(1) If any part of the source code for this software is distributed, then this
README file must be included, with this copyright and no-warranty notice
unaltered; and any additions, deletions, or changes to the original files
must be clearly indicated in accompanying documentation.
(2) If only executable code is distributed, then the accompanying
documentation must state that "this software is based in part on the work of
the Independent JPEG Group".
(3) Permission for use of this software is granted only if the user accepts
full responsibility for any undesirable consequences; the authors accept
NO LIABILITY for damages of any kind.

These conditions apply to any software derived from or based on the IJG code,
not just to the unmodified library.  If you use our work, you ought to
acknowledge us.

Permission is NOT granted for the use of any IJG author's name or company name
in advertising or publicity relating to this software or products derived from
it.  This software may be referred to only as "the Independent JPEG Group's
software".

We specifically permit and encourage the use of this software as the basis of
commercial products, provided that all warranty or liability claims are
assumed by the product vendor.


ansi2knr.c is included in this distribution by permission of L. Peter Deutsch,
sole proprietor of its copyright holder, Aladdin Enterprises of Menlo Park, CA.
ansi2knr.c is NOT covered by the above copyright and conditions, but instead
by the usual distribution terms of the Free Software Foundation; principally,
that you must include source code if you redistribute it.  (See the file
ansi2knr.c for full details.)  However, since ansi2knr.c is not needed as part
of any program generated from the IJG code, this does not limit you more than
the foregoing paragraphs do.

The Unix configuration script "configure" was produced with GNU Autoconf.
It is copyright by the Free Software Foundation but is freely distributable.
The same holds for its supporting scripts (config.guess, config.sub,
ltconfig, ltmain.sh).  Another support script, install-sh, is copyright
by M.I.T. but is also freely distributable.

It appears that the arithmetic coding option of the JPEG spec is covered by
patents owned by IBM, AT&T, and Mitsubishi.  Hence arithmetic coding cannot
legally be used without obtaining one or more licenses.  For this reason,
support for arithmetic coding has been removed from the free JPEG software.
(Since arithmetic coding provides only a marginal gain over the unpatented
Huffman mode, it is unlikely that very many implementations will support it.)
So far as we are aware, there are no patent restrictions on the remaining
code.

The IJG distribution formerly included code to read and write GIF files.
To avoid entanglement with the Unisys LZW patent, GIF reading support has
been removed altogether, and the GIF writer has been simplified to produce
"uncompressed GIFs".  This technique does not use the LZW algorithm; the
resulting GIF files are larger than usual, but are readable by all standard
GIF decoders.

We are required to state that
    "The Graphics Interchange Format(c) is the Copyright property of
    CompuServe Incorporated.  GIF(sm) is a Service Mark property of
    CompuServe Incorporated."

*****************************************************************************/

#include "frame.h"
#include <setjmp.h>
#include "povray.h"
#include "optout.h"
#include "jpeg_pov.h"
#include "pov_util.h"

#include "fileinputoutput.h"

#ifdef __cplusplus
 extern "C" {
#endif

#include "jpeglib.h"

#ifdef __cplusplus
}
#endif

USING_POV_NAMESPACE
USING_POV_BASE_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

const int POV_JPEG_BUFFER_SIZE = 1024;


/*****************************************************************************
* Local typedefs
******************************************************************************/

// write buffer for JPEG images
typedef struct
{
	struct jpeg_error_mgr jerr;
	jpeg_source_mgr jsrc;
	jpeg_destination_mgr jdest;
	jmp_buf setjmp_buffer;	// for return to caller 
	char buffer[POV_JPEG_BUFFER_SIZE];
	JSAMPROW row_pointer[1];
	int row_stride;
	struct jpeg_compress_struct cinfo;
	OStream *file;
} POV_JPEG_Write_Buffer;

typedef struct
{
	struct jpeg_error_mgr jerr;
	jpeg_source_mgr jsrc;
	jpeg_destination_mgr jdest;
	jmp_buf setjmp_buffer;	// for return to caller 
	char buffer[POV_JPEG_BUFFER_SIZE];
	JSAMPROW row_pointer[1];
	int row_stride;
	struct jpeg_decompress_struct cinfo;
	IStream *file;
} POV_JPEG_Read_Buffer;


/*****************************************************************************
* Local variables
******************************************************************************/


/*****************************************************************************
* Static functions
******************************************************************************/

// These are an internal functions and data structures for jpeglib 
#ifdef __cplusplus
 extern "C" {
#endif


METHODDEF(void) my_error_exit(j_common_ptr cinfo);
METHODDEF(void) my_output_message(j_common_ptr cinfo);
METHODDEF(void) my_init_dest(j_compress_ptr cinfo);
METHODDEF(void) my_init_source(j_decompress_ptr cinfo);
METHODDEF(boolean) my_fill_input_buffer(j_decompress_ptr cinfo);
METHODDEF(void) my_skip_input_data(j_decompress_ptr cinfo, long num_bytes);
METHODDEF(void) my_term_source(j_decompress_ptr cinfo);
METHODDEF(boolean) my_empty_output_buffer(j_compress_ptr cinfo);
METHODDEF(void) my_term_destination(j_compress_ptr cinfo);


METHODDEF(void) my_error_exit (j_common_ptr cinfo)
{
  POV_JPEG_Write_Buffer * myerr = (POV_JPEG_Write_Buffer *) cinfo->client_data ;

  // Always display the message. 
  // We could postpone this until after returning, if we chose. 
  (*cinfo->err->output_message)(cinfo);

  // Return control to the setjmp point 
  longjmp(myerr->setjmp_buffer, 1);
}

METHODDEF(void) my_output_message(j_common_ptr cinfo)
{
  char buffer[JMSG_LENGTH_MAX];

  // Create the message 
  (*cinfo->err->format_message) (cinfo, buffer);

  // Send it to stdout 
  PossibleError("jpeglib: %s",buffer);
}

METHODDEF(void) my_init_dest(j_compress_ptr cinfo)
{
	POV_JPEG_Write_Buffer * bufptr = (POV_JPEG_Write_Buffer *)(cinfo->client_data);

	bufptr->jdest.next_output_byte = (unsigned char *)(&(bufptr->buffer[0]));
	bufptr->jdest.free_in_buffer = POV_JPEG_BUFFER_SIZE;
}

METHODDEF(void) my_init_source(j_decompress_ptr cinfo)
{
	POV_JPEG_Read_Buffer * bufptr = (POV_JPEG_Read_Buffer *)(cinfo->client_data);

	bufptr->jsrc.next_input_byte = (unsigned char *)(&(bufptr->buffer[0]));
	bufptr->jsrc.bytes_in_buffer = 0;
}

METHODDEF(boolean) my_fill_input_buffer(j_decompress_ptr cinfo)
{
	POV_JPEG_Read_Buffer * bufptr = (POV_JPEG_Read_Buffer *)(cinfo->client_data);
	int i;

	for(i = 0; i < POV_JPEG_BUFFER_SIZE; i++)
	{
		if (!bufptr->file->read((char *)(&(bufptr->buffer[i])), 1))
			break;
	}
	bufptr->jsrc.bytes_in_buffer = i;
	bufptr->jsrc.next_input_byte = (unsigned char *)(&(bufptr->buffer[0]));

	return true;
}

METHODDEF(void) my_skip_input_data(j_decompress_ptr cinfo, long num_bytes)
{
	POV_JPEG_Read_Buffer * bufptr = (POV_JPEG_Read_Buffer *)(cinfo->client_data);

	if ( bufptr->jsrc.bytes_in_buffer < num_bytes )
	{
		num_bytes -= bufptr->jsrc.bytes_in_buffer;
		bufptr->jsrc.bytes_in_buffer = 0; 
		bufptr->file->seekg(num_bytes, POV_SEEK_CUR);
	}  
	else
	{
		bufptr->jsrc.bytes_in_buffer -= num_bytes;
		bufptr->jsrc.next_input_byte += num_bytes;
	}

}

METHODDEF(void) my_term_source(j_decompress_ptr /*cinfo*/)
{
}

METHODDEF(boolean) my_empty_output_buffer(j_compress_ptr cinfo)
{
	POV_JPEG_Write_Buffer * bufptr = (POV_JPEG_Write_Buffer *)(cinfo->client_data);

	bufptr->file->write((char *)(bufptr->buffer), POV_JPEG_BUFFER_SIZE);

	bufptr->jdest.next_output_byte = (unsigned char *)(&(bufptr->buffer[0]));
	bufptr->jdest.free_in_buffer = POV_JPEG_BUFFER_SIZE;

	return true;
}

METHODDEF(void) my_term_destination(j_compress_ptr cinfo)
{
	POV_JPEG_Write_Buffer * bufptr = (POV_JPEG_Write_Buffer *)(cinfo->client_data);

	if(POV_JPEG_BUFFER_SIZE - bufptr->jdest.free_in_buffer > 0)
		bufptr->file->write((char *)(bufptr->buffer), POV_JPEG_BUFFER_SIZE - bufptr->jdest.free_in_buffer);
}


#ifdef __cplusplus
}
#endif

BEGIN_POV_NAMESPACE

/*****************************************************************************
*
* FUNCTION      : Open_JPEG_File
*
* ARGUMENTS     : FILE_HANDLE *handle; char *name; int *width; int *height;
*                 int buffer_size; int mode;
*
* MODIFIED ARGS : handle, width, height
*
* RETURN VALUE  : 1 or 0 for success or failure
*
* AUTHOR        : Thorsten Froehlich
*
* DESCRIPTION
*
*   Open a JPEG file and allocate the needed JPEG structure buffers
*
* CHANGES
*
*   -
*
******************************************************************************/

JPEG_Image::JPEG_Image(char *name, int w, int h, int m, int l)
{
	POV_JPEG_Write_Buffer *bufptr;

	mode = m;
	filename = name;
	in_file = NULL;
	out_file = NULL;
	buffer = NULL;

	switch(mode)
	{
		case READ_MODE:
			Error("Cannot do partial reading of JPEG images.");
			return;
		case APPEND_MODE:
			Error("Cannot continue from partial rendered JPEG images.");
			return;
		case WRITE_MODE:
			bufptr = (POV_JPEG_Write_Buffer *)POV_MALLOC(sizeof(POV_JPEG_Write_Buffer), "JPEG write buffer");
			buffer = (char *)bufptr;

			// We set up the normal JPEG routines, then override io methods. 
			bufptr->cinfo.err = jpeg_std_error(&bufptr->jerr);
			bufptr->jerr.error_exit = my_error_exit;
			bufptr->jerr.output_message = my_output_message;
			bufptr->cinfo.client_data = (void *)bufptr;

			// Now we can initialize the JPEG compression object. 
			jpeg_create_compress(&bufptr->cinfo);

			if((out_file = New_Checked_OStream(name, POV_File_Image_JPEG, false)) == NULL)
			{
				delete out_file;
				return;
			}

			bufptr->file = out_file;
			bufptr->jdest.init_destination = my_init_dest;
			bufptr->jdest.empty_output_buffer = my_empty_output_buffer;
			bufptr->jdest.term_destination = my_term_destination;
			bufptr->cinfo.dest = &bufptr->jdest;
			bufptr->cinfo.image_width = w;
			bufptr->cinfo.image_height = h;
			bufptr->cinfo.input_components = 3;
			bufptr->cinfo.in_color_space = JCS_RGB;

			jpeg_set_defaults(&bufptr->cinfo);

			// NOTE: Default color space is JCS_YCbCr!!! [trf]
			jpeg_set_colorspace(&bufptr->cinfo, JCS_RGB);

			jpeg_set_quality(&bufptr->cinfo, opts.OutputQuality, true); // quality (range 0 to 100!)

			// begin compression
			jpeg_start_compress(&bufptr->cinfo, true);

			bufptr->row_stride = w * 3;

			bufptr->row_pointer[0] = (JSAMPROW)POV_MALLOC(bufptr->row_stride * sizeof(JSAMPLE), "JPEG line buffer");

			valid = true;
	}
}



/*****************************************************************************
*
* FUNCTION      : Close_JPEG_File
*
* ARGUMENTS     : FILE_HANDLE *handle
*
* MODIFIED ARGS : handle
*
* RETURN VALUE  : none
*
* AUTHOR        : Thorsten Froehlich
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

JPEG_Image::~JPEG_Image()
{
	POV_JPEG_Write_Buffer *bufptr;

	bufptr = (POV_JPEG_Write_Buffer *)(buffer);

	// finish compression
	jpeg_finish_compress(&bufptr->cinfo);

	if(in_file != NULL)
	{
		delete in_file;
		in_file = NULL;
	}

	if(out_file != NULL)
	{
		delete out_file;
		out_file = NULL;
	}

	// release JPEG compression object
	jpeg_destroy_compress(&bufptr->cinfo);

	POV_FREE(bufptr->row_pointer[0]);
	POV_FREE(buffer);
}



/*****************************************************************************
*
* FUNCTION      : Write_JPEG_Line
*
* ARGUMENTS     : handle, line_data, line_number
*
* MODIFIED ARGS : none
*
* RETURN VALUE  : none
*
* AUTHOR        : Thorsten Froehlich
*
* DESCRIPTION
*
*   Write a line of data to the PNG file
*
* CHANGES
*
*   -
*
******************************************************************************/

void JPEG_Image::Write_Line(COLOUR *line_data)
{
	if(valid == false)
		Error("Cannot access output image file.");

	POV_JPEG_Write_Buffer *bufptr = (POV_JPEG_Write_Buffer *)(buffer);
	unsigned char *colptr;
	int col;

	colptr = (unsigned char *)(bufptr->row_pointer[0]);
	for(col = 0; col < bufptr->cinfo.image_width; col++)
	{
		 (*colptr++) = (unsigned char)(line_data[col][pRED] * 255.0);
		 (*colptr++) = (unsigned char)(line_data[col][pGREEN] * 255.0);
		 (*colptr++) = (unsigned char)(line_data[col][pBLUE] * 255.0);
	}

	(void)jpeg_write_scanlines(&bufptr->cinfo, bufptr->row_pointer, 1);
}



/*****************************************************************************
*
* FUNCTION      : Read_JPEG_Line
*
* ARGUMENTS     : FILE_HANDLE *handle; COLOUR *line_data; int *line_number;
*
* MODIFIED ARGS : none
*
* RETURN VALUE  : 1 if no error exit
*
* AUTHOR        : Thorsten Froehlich
*
* DESCRIPTION
*
*   Read a line of JPEG data
*
* CHANGES
*
*   -
*
******************************************************************************/

int JPEG_Image::Read_Line(COLOUR *)
{
	if(valid == false)
		Error("Cannot access output image file.");

	Error("Cannot do partial reading of JPEG images.");

	return 0;
}



/*****************************************************************************
*
* FUNCTION      : Read_JPEG_Image
*
* ARGUMENTS     : IMAGE *Image; char *name;
*
* MODIFIED ARGS : Image
*
* RETURN VALUE  : none
*
* AUTHOR        : Thorsten Froehlich
*
* DESCRIPTION
*
*   Reads a JPEG image into an RGB image buffer
*
* CHANGES
*
*   -
*
******************************************************************************/

void Read_JPEG_Image(IMAGE *Image, char *name)
{
	unsigned int width, height;
	int row, col;
	IStream *file;
	IMAGE8_LINE *line_data;
	POV_JPEG_Read_Buffer bufptr; 

	// Start by trying to open the file 

	if ((file = Locate_File(name, POV_File_Image_JPEG,NULL,true)) == NULL)
	{
		Error("Cannot open JPEG file.");
	}

	// We set up the normal JPEG error routines, then override error_exit and output_message. 
	
	bufptr.cinfo.err = jpeg_std_error(&bufptr.jerr);
	bufptr.jerr.error_exit = my_error_exit;
	bufptr.jerr.output_message = my_output_message;
	bufptr.cinfo.client_data = (void *)&bufptr;

	if (setjmp(bufptr.setjmp_buffer))
	{
		jpeg_destroy_decompress(&bufptr.cinfo);
		delete file;
		Error("Cannot read JPEG image.");
		return;
	}

	// Now we can initialize the JPEG decompression object. 
	jpeg_create_decompress(&bufptr.cinfo);

	bufptr.file = file;
	bufptr.jsrc.init_source = my_init_source;
	bufptr.jsrc.fill_input_buffer = my_fill_input_buffer;
	bufptr.jsrc.skip_input_data = my_skip_input_data;
	bufptr.jsrc.resync_to_restart = jpeg_resync_to_restart;
	bufptr.jsrc.term_source = my_term_source;
	bufptr.cinfo.src = &bufptr.jsrc;

	// jpeg_set_defaults(&bufptr.cinfo);

	/* We can ignore the return value from jpeg_read_header since
	 *   (a) suspension is not possible with the stdio data source, and
	 *   (b) we passed true to reject a tables-only JPEG file as an error.
	 * See libjpeg.doc for more info.
	 */
	(void)jpeg_read_header(&bufptr.cinfo, true);

	// check for unsupported formats
	if((bufptr.cinfo.output_components != 1) && (bufptr.cinfo.output_components != 3) &&
	   (bufptr.cinfo.out_color_space != JCS_GRAYSCALE) && (bufptr.cinfo.out_color_space != JCS_RGB))
	{
		jpeg_destroy_decompress(&bufptr.cinfo);
		delete file;
		Error("Unsupported color format in JPEG image.");
		return;
	}

	// from png_pov.c
	bufptr.cinfo.output_gamma = opts.GammaFactor*opts.DisplayGamma;

	// begin decompression
	(void)jpeg_start_decompress(&bufptr.cinfo);

	height = bufptr.cinfo.output_height;
	width = bufptr.cinfo.output_width;

	// JSAMPLEs per row in output buffer 
	bufptr.row_stride = bufptr.cinfo.output_width * bufptr.cinfo.output_components;
	// Make a one-row-high sample array 
	bufptr.row_pointer[0] = (JSAMPROW)POV_MALLOC(bufptr.row_stride * sizeof(JSAMPLE), "JPEG line buffer");

	Image->iwidth = width;
	Image->iheight = height;
	Image->width = (DBL)width;
	Image->height = (DBL)height;

	Image->Colour_Map = NULL;

	Image->data.rgb8_lines = (IMAGE8_LINE *)POV_MALLOC(height * sizeof(IMAGE8_LINE), "JPEG image");

	// read image row by row
	for (row = 0; row < height; row++)
	{
		// read scanline
		(void)jpeg_read_scanlines(&bufptr.cinfo, &bufptr.row_pointer[0], 1);

		line_data = &Image->data.rgb8_lines[row];

		line_data->red = (unsigned char *)POV_MALLOC(width, "JPEG image line");
		line_data->green = (unsigned char *)POV_MALLOC(width, "JPEG image line");
		line_data->blue = (unsigned char *)POV_MALLOC(width, "JPEG image line");
		line_data->transm = NULL;

		if(bufptr.cinfo.output_components == 1) // 8-bit grayscale image
		{
			for (col = 0; col < width; col++)
			{
				line_data->red[col] =
				line_data->green[col] =
				line_data->blue[col] = bufptr.row_pointer[0][col];
			}
		}
		else if(bufptr.cinfo.output_components == 3) // 24-bit rgb image
		{
			for (col = 0; col < width; col++)
			{
				line_data->red[col] = bufptr.row_pointer[0][col * 3];
				line_data->green[col] = bufptr.row_pointer[0][(col * 3) + 1];
				line_data->blue[col] = bufptr.row_pointer[0][(col * 3) + 2];
			}
		}
	}

	// finish decompression
	(void)jpeg_finish_decompress(&bufptr.cinfo);

	// release JPEG decompression object
	jpeg_destroy_decompress(&bufptr.cinfo);

	POV_FREE(bufptr.row_pointer[0]);

	delete file;

	return;
}

END_POV_NAMESPACE
