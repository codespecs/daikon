/****************************************************************************
 *                  render.h
 *
 * This module contains all defines, typedefs, and prototypes for RENDER.CPP.
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
 * $File: //depot/povray/3.6-release/source/render.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef RENDER_H
#define RENDER_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/




/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef enum
{
  CSV,
  SYS,
  PPM,
  TARGA,
  PNG,
  NONE
} Histogram_Types;

/*****************************************************************************
* Global variables
******************************************************************************/

extern COLOUR *Previous_Line, *Current_Line, *Temp_Line;
extern char *Previous_Line_Antialiased_Flags, *Current_Line_Antialiased_Flags;

extern unsigned char *Red_Row_255, *Green_Row_255, *Blue_Row_255, *Alpha_Row_255;

extern long SuperSampleCount, RadiosityCount, MosaicPreviewSize;

extern DBL maxclr;

extern int Current_Line_Number;

extern int Trace_Level, Max_Trace_Level, Highest_Trace_Level;
extern bool Had_Max_Trace_Level;
extern DBL Total_Depth;

/* Object-Ray Options [ENB 9/97] */
extern bool In_Reflection_Ray;
extern bool In_Shadow_Ray;

extern DBL ADC_Bailout;

extern unsigned long *histogram_grid ;
extern unsigned long max_histogram_value ;
extern Image_File_Class *Histogram_File ;

/*****************************************************************************
* Global functions
******************************************************************************/

void Read_Rendered_Part (char *New_Fname);
void Start_Tracing_Radiosity_Preview (int StartPixelSize, int EndPixelSize);
void Start_Tracing_Mosaic_Preview (int StartPixelSize, int EndPixelSize);
void Start_Adaptive_Tracing (void);
void Start_Non_Adaptive_Tracing (void);
void Initialize_Renderer (void);
void Terminate_Renderer (void);
DBL  Trace (RAY *Ray, COLOUR Colour, DBL Weight);
void Check_User_Abort (int Do_Stats);
void write_histogram (char *filename);
void destroy_histogram (void);
void initialize_ray_container_state(RAY *Ray, int Compute);

END_POV_NAMESPACE

#endif
