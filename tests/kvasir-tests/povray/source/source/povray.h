/****************************************************************************
 *                  povray.h
 *
 * This module contains all defines, typedefs, and prototypes for POVRAY.CPP.
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
 * $File: //depot/povray/3.6-release/source/povray.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef POVRAY_H
#define POVRAY_H

#include <time.h>

#include "frame.h"
#include "atmosph.h"
#include "camera.h"
#include "media.h"
#include "point.h"
#include "render.h"
#include "userio.h"
#include "povms.h"
#include "lightgrp.h"

BEGIN_POV_NAMESPACE

USING_POV_BASE_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define MAX_LIBRARIES 25

#define STAGE_PREINIT         0  /* set in POVRAY.C */
#define STAGE_STARTUP         1  /* set in POVRAY.C */
#define STAGE_BANNER          2  /* set in POVRAY.C */
#define STAGE_INIT            3  /* set in POVRAY.C */
#define STAGE_FILE_INIT       4  /* set in POVRAY.C */
#define STAGE_PARSING         5  /* set in PARSE.C */
#define STAGE_CONTINUING      6  /* set in POVRAY.C */
#define STAGE_RENDERING       7  /* set in POVRAY.C */
#define STAGE_SHUTDOWN        8  /* set in POVRAY.C */
#define STAGE_CLEANUP_PARSE   9  /* set in PARSE.C */
#define STAGE_SLAB_BUILDING  10  /* set in POVRAY.C */
#define STAGE_TOKEN_INIT     11  /* set in TOKENIZE.C */
#define STAGE_INCLUDE_ERR    12  /* set in TOKENIZE.C */
#define STAGE_FOUND_INSTEAD  13  /* set in TOKENIZE.C */
#define STAGECOUNT           14  /* number of stages */

#define DISPLAY           0x000001L
#define VERBOSE           0x000002L
#define DISKWRITE         0x000004L
#define PROMPTEXIT        0x000008L
#define ANTIALIAS         0x000010L
#define RGBSEPARATE       0x000020L
#define EXITENABLE        0x000040L
#define CONTINUE_TRACE    0x000080L
#define JITTER            0x000100L
#define PREVIEW           0x000200L
#define SPLIT_UNION       0x000400L
#define USE_VISTA_BUFFER  0x000800L
#define USE_LIGHT_BUFFER  0x001000L
#define USE_VISTA_DRAW    0x002000L
#define REMOVE_BOUNDS     0x004000L
#define CYCLIC_ANIMATION  0x008000L
#define OUTPUT_ALPHA      0x010000L
#define HF_GRAY_16        0x020000L
#define GAMMA_CORRECT     0x040000L
#define FROM_STDIN        0x080000L
#define TO_STDOUT         0x100000L

#define Q_FULL_AMBIENT 0x000001L
#define Q_QUICKC       0x000002L
#define Q_SHADOW       0x000004L
#define Q_AREA_LIGHT   0x000008L
#define Q_REFRACT      0x000010L
#define Q_REFLECT      0x000020L
#define Q_NORMAL       0x000040L
#define Q_VOLUME       0x000080L

#define EF_RADIOS  1
#define EF_SLOPEM  2
#define EF_ISOFN   4
#define EF_SPLINE  8
#define EF_TIFF    16

#define QUALITY_0  Q_QUICKC+Q_FULL_AMBIENT
#define QUALITY_1  QUALITY_0
#define QUALITY_2  QUALITY_1-Q_FULL_AMBIENT
#define QUALITY_3  QUALITY_2
#define QUALITY_4  QUALITY_3+Q_SHADOW
#define QUALITY_5  QUALITY_4+Q_AREA_LIGHT
#define QUALITY_6  QUALITY_5-Q_QUICKC+Q_REFRACT
#define QUALITY_7  QUALITY_6
#define QUALITY_8  QUALITY_7+Q_REFLECT+Q_NORMAL
#define QUALITY_9  QUALITY_8+Q_VOLUME


/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Frame_Struct FRAME;

struct Frame_Struct
{
  CAMERA *Camera;
  int Screen_Height, Screen_Width; /* OPTIONS */
  int Number_Of_Light_Sources;
  LIGHT_SOURCE *Light_Sources;
  OBJECT *Objects;
  DBL Atmosphere_IOR, Atmosphere_Dispersion, Antialias_Threshold;
  COLOUR Background_Colour;
  COLOUR Ambient_Light;
  COLOUR Irid_Wavelengths;
  IMEDIA *Atmosphere;
  FOG *Fog;
  RAINBOW *Rainbow;
  SKYSPHERE *Skysphere;
  LIGHT_GROUP_LIGHT *Light_Group_Lights;  // Maintain a separate, independent list of all lights
                                          // that are part of light groups.
};

typedef enum STATS
{
  /* Computations are performed on these three */
  Number_Of_Pixels = 0,
  Number_Of_Pixels_Supersampled,
  Number_Of_Samples,
  Number_Of_Rays,
  Calls_To_DNoise,
  Calls_To_Noise,
  ADC_Saves,

  /* intersecion stack */
  Istack_overflows,

  /* objects */
  Ray_RBezier_Tests,
  Ray_RBezier_Tests_Succeeded,
  Ray_Bicubic_Tests,
  Ray_Bicubic_Tests_Succeeded,
  Ray_Blob_Tests,
  Ray_Blob_Tests_Succeeded,
  Blob_Element_Tests,
  Blob_Element_Tests_Succeeded,
  Blob_Bound_Tests,
  Blob_Bound_Tests_Succeeded,
  Ray_Box_Tests,
  Ray_Box_Tests_Succeeded,
  Ray_Cone_Tests,
  Ray_Cone_Tests_Succeeded,
  Ray_CSG_Intersection_Tests,
  Ray_CSG_Intersection_Tests_Succeeded,
  Ray_CSG_Merge_Tests,
  Ray_CSG_Merge_Tests_Succeeded,
  Ray_CSG_Union_Tests,
  Ray_CSG_Union_Tests_Succeeded,
  Ray_Disc_Tests,
  Ray_Disc_Tests_Succeeded,
  Ray_Fractal_Tests,
  Ray_Fractal_Tests_Succeeded,
  Ray_HField_Tests,
  Ray_HField_Tests_Succeeded,
  Ray_HField_Box_Tests,
  Ray_HField_Box_Tests_Succeeded,
  Ray_HField_Triangle_Tests,
  Ray_HField_Triangle_Tests_Succeeded,
  Ray_HField_Block_Tests,
  Ray_HField_Block_Tests_Succeeded,
  Ray_HField_Cell_Tests,
  Ray_HField_Cell_Tests_Succeeded,
  Ray_IsoSurface_Tests,
  Ray_IsoSurface_Tests_Succeeded,
  Ray_IsoSurface_Bound_Tests,
  Ray_IsoSurface_Bound_Tests_Succeeded,
  Ray_IsoSurface_Cache,
  Ray_IsoSurface_Cache_Succeeded,
  Ray_Lathe_Tests,
  Ray_Lathe_Tests_Succeeded,
  Lathe_Bound_Tests,
  Lathe_Bound_Tests_Succeeded,
  Ray_Mesh_Tests,
  Ray_Mesh_Tests_Succeeded,
  Ray_Plane_Tests,
  Ray_Plane_Tests_Succeeded,
  Ray_Polygon_Tests,
  Ray_Polygon_Tests_Succeeded,
  Ray_Prism_Tests,
  Ray_Prism_Tests_Succeeded,
  Prism_Bound_Tests,
  Prism_Bound_Tests_Succeeded,
  Ray_Parametric_Tests,
  Ray_Parametric_Tests_Succeeded,
  Ray_Par_Bound_Tests,
  Ray_Par_Bound_Tests_Succeeded,
  Ray_Quadric_Tests,
  Ray_Quadric_Tests_Succeeded,
  Ray_Poly_Tests,
  Ray_Poly_Tests_Succeeded,
  Ray_Sphere_Tests,
  Ray_Sphere_Tests_Succeeded,
  Ray_Sphere_Sweep_Tests,
  Ray_Sphere_Sweep_Tests_Succeeded,
  Ray_Superellipsoid_Tests,
  Ray_Superellipsoid_Tests_Succeeded,
  Ray_Sor_Tests,
  Ray_Sor_Tests_Succeeded,
  Sor_Bound_Tests,
  Sor_Bound_Tests_Succeeded,
  Ray_Torus_Tests,
  Ray_Torus_Tests_Succeeded,
  Torus_Bound_Tests,
  Torus_Bound_Tests_Succeeded,
  Ray_Triangle_Tests,
  Ray_Triangle_Tests_Succeeded,
  Ray_TTF_Tests,
  Ray_TTF_Tests_Succeeded,

  /* bounding etc */
  Bounding_Region_Tests,
  Bounding_Region_Tests_Succeeded,
  Clipping_Region_Tests,
  Clipping_Region_Tests_Succeeded,

  /* isosurface and functions */
  Ray_IsoSurface_Find_Root,
  Ray_Function_VM_Calls,
  Ray_Function_VM_Instruction_Est,

  /* Vista and light buffer */
  VBuffer_Tests,
  VBuffer_Tests_Succeeded,
  LBuffer_Tests,
  LBuffer_Tests_Succeeded,

  /* Media */
  Media_Samples,
  Media_Intervals,

  /* Ray */
  Reflected_Rays_Traced,
  Refracted_Rays_Traced,
  Transmitted_Rays_Traced,
  Internal_Reflected_Rays_Traced,
  Shadow_Cache_Hits,
  Shadow_Rays_Succeeded,
  Shadow_Ray_Tests,

  nChecked,
  nEnqueued,
  totalQueues,
  totalQueueResets,
  totalQueueResizes,
  Polynomials_Tested,
  Roots_Eliminated,

#if defined(MEM_STATS)
  MemStat_Smallest_Alloc,
  MemStat_Largest_Alloc,
  MemStat_Largest_Mem_Usage,
#if (MEM_STATS>=2)
  MemStat_Total_Allocs,
  MemStat_Total_Frees,
#endif
#endif

  /* NK phmap */
  Number_Of_Photons_Shot,
  Number_Of_Photons_Stored,
  Number_Of_Global_Photons_Stored,
  Number_Of_Media_Photons_Stored,
  Priority_Queue_Add,
  Priority_Queue_Remove,
  Gather_Performed_Count,
  Gather_Expanded_Count,

  /* Must be the last */
  MaxStat

} Stats;

typedef struct intersection_stats_info
{
  int povms_id;
  Stats stat_test_id;
  Stats stat_suc_id;
  char *infotext;
} INTERSECTION_STATS_INFO;

typedef struct OPTIONS_STRUCT
{
  unsigned int Options;
  char DisplayFormat;
  char PaletteOption;

  char OutputFormat;
  int OutputQuality;
  int Output_File_Type;
  char Input_File_Name[FILE_NAME_LENGTH];
  char Output_File_Name[FILE_NAME_LENGTH];
  char Output_Path[FILE_NAME_LENGTH];
  char Output_Numbered_Name[FILE_NAME_LENGTH];
  char Scene_Name[FILE_NAME_LENGTH];
  COLC DisplayGamma;
  COLC GammaFactor;

  unsigned int Quality_Flags;

  int AntialiasDepth;
  DBL Antialias_Threshold;

  DBL JitterScale;

  int Abort_Test_Counter;

  char *Library_Paths[MAX_LIBRARIES];
  int Library_Path_Index;

  int First_Column, Last_Column;
  DBL First_Column_Percent, Last_Column_Percent;

  int First_Line, Last_Line;
  DBL First_Line_Percent, Last_Line_Percent;

  /* Parse */
  int Language_Version;

  bool Use_Slabs;
  long BBox_Threshold;

  int Quality;

  int PreviewGridSize_Start;   /* Mosaic Preview - Initial pixel grid size */
  int PreviewGridSize_End;     /* Mosaic Preview - Ending pixel grid size */

  FRAMESEQ FrameSeq;

  /* Should STREAM PATHS go somewhere here? */

  DBL Radiosity_Brightness;
  long Radiosity_Count;
  DBL Radiosity_Dist_Max;
  DBL Radiosity_Error_Bound;
  DBL Radiosity_Gray;  /* degree to which gathered light is grayed */
  DBL Radiosity_Low_Error_Factor;
  DBL Radiosity_Min_Reuse;
  long Radiosity_Nearest_Count;
  int Radiosity_Recursion_Limit;
  long Radiosity_Quality;  /* Q-flag value for light gathering */
  int Radiosity_File_ReadOnContinue;
  int Radiosity_File_SaveWhileRendering;
  int Radiosity_File_AlwaysReadAtStart;
  int Radiosity_File_KeepOnAbort;
  int Radiosity_File_KeepAlways;
  int Radiosity_Preview_Done;  /* used in cache file processing */

  /* NK rad */
  DBL Real_Radiosity_Error_Bound;
  DBL Maximum_Sample_Brightness;
  DBL Radiosity_ADC_Bailout;
  int Radiosity_Use_Normal;
  int Radiosity_Use_Media;
  char *Radiosity_Load_File_Name;
  char *Radiosity_Save_File_Name;
  int Radiosity_Add_On_Final_Trace;
  bool Radiosity_Enabled;
  DBL radPretraceStart;
  DBL radPretraceEnd;

  int histogram_x, histogram_y;
  bool histogram_on;
  Histogram_Types histogram_type;
  int histogram_file_type;
  char Histogram_File_Name[FILE_NAME_LENGTH];

  SHELLDATA Shellouts[MAX_SHL];

  char Ini_Output_File_Name[FILE_NAME_LENGTH];

  char Header_File_Name[FILE_NAME_LENGTH];

  int Tracing_Method;
  int Do_Stats;

  unsigned long Preview_RefCon;

  int Warning_Level;

  int String_Encoding;

  int Noise_Generator;

  POVMSAttributeList Declared_Variables;
} Opts;


/*****************************************************************************
* Global variables
******************************************************************************/

extern char Actual_Output_Name[FILE_NAME_LENGTH];

extern int Cooperate_Render_Flag;

extern FRAME Frame;

extern COUNTER stats[MaxStat];
extern COUNTER totalstats[MaxStat];

extern time_t tstart, tstop;
extern DBL tparse, tphoton, trender;
extern DBL tparse_frame, tphoton_frame, trender_frame;
extern DBL tparse_total, tphoton_total, trender_total;

extern char Color_Bits;

extern int Number_Of_Files;
extern Opts opts;

extern OStream *stat_file;
extern Image_File_Class *Output_File;

extern int Abort_Test_Every;
extern int Display_Started;
extern int Stage;
extern volatile int Stop_Flag;
extern int Experimental_Flag;

extern int Num_Echo_Lines;
extern char *Option_String_Ptr;      
extern DBL Clock_Delta;

END_POV_NAMESPACE

#if(USE_LOCAL_POVMS_OUTPUT == 1)
extern POVMSContext POVMS_Output_Context;
#endif


/*****************************************************************************
* Global functions
******************************************************************************/

void povray_init();
void povray_terminate();
void povray_exit(int i);

void povray_cooperate();

#if(USE_LOCAL_POVMS_OUTPUT == 1)
POVMSAddress povray_getoutputcontext();
#endif

BEGIN_POV_NAMESPACE

void Do_Cooperate(int level);

END_POV_NAMESPACE

#endif
