/****************************************************************************
 *                  renderio.h
 *
 * This module contains all defines, typedefs, and prototypes for renderio.cpp.
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
 * $File: //depot/povray/3.6-release/source/renderio.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef RENDERIO_H
#define RENDERIO_H

BEGIN_POV_NAMESPACE

void Read_Rendered_Part(char *New_Fname);
void init_output_file_handle();
void destroy_output_file_handle();
void setup_output_file_name();
void open_output_file();
void output_prev_image_line_and_advance(int y);
void output_single_image_line_with_alpha_correction(COLOUR *Line, int y);
void plot_pixel(int x, int  y, COLOUR Colour);
Image_File_Class *Open_Image(int file_type, char *filename, int w, int h, int m, int l = 0);

END_POV_NAMESPACE

#endif
