/****************************************************************************
 *                  frame.h
 *
 * This header file is included by all C modules in POV-Ray. It defines all
 * globally-accessible types and constants.
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
 * $File: //depot/povray/3.6-release/source/frame.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef FRAME_H
#define FRAME_H

/* Generic header for all modules */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "config.h"

#include "configbase.h"
#include "fileinputoutput.h"

#ifndef POV_NAMESPACE
	#define POV_NAMESPACE pov
#endif

#ifndef BEGIN_POV_NAMESPACE
	#define BEGIN_POV_NAMESPACE namespace POV_NAMESPACE { using namespace std;
#endif

#ifndef END_POV_NAMESPACE
	#define END_POV_NAMESPACE }
#endif

#ifndef USING_POV_NAMESPACE
	#define USING_POV_NAMESPACE using namespace POV_NAMESPACE;
#endif

#include "pov_mem.h"

BEGIN_POV_NAMESPACE

/*
 * Platform name default.
 */
#ifndef POVRAY_PLATFORM_NAME
 #define POVRAY_PLATFORM_NAME "Unknown Platform"
#endif

/*
 * You have to define the macros POVRAY_BEGIN_COOPERATE and POVRAY_END_COOPERATE!
 */

#ifndef ALTMAIN
 #define USE_LOCAL_POVMS_OUTPUT 1
#endif

#ifndef POVRAY_BEGIN_COOPERATE
 *** ERROR ***
#endif
#ifndef POVRAY_END_COOPERATE
 *** ERROR ***
#endif

#if(USE_LOCAL_POVMS_OUTPUT == 1)
 #define FRONTEND_ADDRESS povray_getoutputcontext()
#endif

#ifndef FRONTEND_ADDRESS
 #define FRONTEND_ADDRESS 0
#endif

/*
 * Functions that POV calls once per run to do various initializations,
 * in the order that they are normally called.
 */
#ifndef STARTUP_POVRAY  /* First function called for each run */
#define STARTUP_POVRAY
#endif

#ifndef PRINT_CREDITS   /* Prints POV-Ray version information banner */
#define PRINT_CREDITS Print_Credits();
#endif

#ifndef PRINT_OTHER_CREDITS /* Prints credits for custom POV versions */
#define PRINT_OTHER_CREDITS
#endif

/*
 * To allow GUI platforms like the Mac to access a command line and provide
 * a command line only interface (for debugging) a different call to an
 * internal function of the standard library is required. This macro takes
 * both argc and argv and is expected to return argc.
 */
#ifndef GETCOMMANDLINE
#define GETCOMMANDLINE(ac,av) ac
#endif

/*
 * Functions that POV calls in povray_terminate (FINISH_POVRAY) or
 * povray_exit (EXIT_POVRAY).
 */

#ifndef FINISH_POVRAY   /* The last call that POV makes to terminate */
#define FINISH_POVRAY
#endif

#ifndef EXIT_POVRAY     /* The last call that POV makes to exit after rendering */
#define EXIT_POVRAY(n)  povray_terminate(); exit(n);
#endif


/*
 * Functions that POV calls once per frame to do varios (de)initializations,
 * in the order they are normally called.
 */
#ifndef POV_PRE_RENDER    /* Called just prior to the start of rendering */
#define POV_PRE_RENDER
#endif

#ifndef CONFIG_MATH       /* Macro for setting up any special FP options */
#define CONFIG_MATH
#endif

#ifndef POV_PRE_PIXEL     /* Called before each pixel is rendered */
#define POV_PRE_PIXEL(x,y,c)
#endif

#ifndef POV_POST_PIXEL    /* Called after each pixel is rendered */
#define POV_POST_PIXEL(x,y,c)
#endif

#ifndef POV_PRE_SHUTDOWN  /* Called before memory and objects are freed */
#define POV_PRE_SHUTDOWN
#endif

#ifndef POV_POST_SHUTDOWN /* Called after memory and objects are freed */
#define POV_POST_SHUTDOWN
#endif

/* Specify number of source file lines printed before error line, their maximum length and
 * the error marker text that is appended to mark the error
 */
#ifndef POV_NUM_ECHO_LINES
#define POV_NUM_ECHO_LINES 5
#endif

#ifndef POV_ERROR_MARKER_TEXT
#define POV_ERROR_MARKER_TEXT " <----ERROR\n"
#endif

#ifndef POV_WHERE_ERROR
#define POV_WHERE_ERROR(fn,ln,cl,ts)
#endif

/* Upper bound for max_trace_level specified by the user */
#ifndef MAX_TRACE_LEVEL_LIMIT
#define MAX_TRACE_LEVEL_LIMIT 256
#endif

/* Various numerical constants that are used in the calculations */
#ifndef EPSILON     /* A small value used to see if a value is nearly zero */
#define EPSILON 1.0e-10
#endif

#ifndef HUGE_VAL    /* A very large value, can be considered infinity */
#define HUGE_VAL 1.0e+17
#endif

/*
 * If the width of a bounding box in one dimension is greater than
 * the critical length, the bounding box should be set to infinite.
 */

#ifndef CRITICAL_LENGTH
#define CRITICAL_LENGTH 1.0e6
#endif

#ifndef BOUND_HUGE  /* Maximum lengths of a bounding box. */
#define BOUND_HUGE 2.0e10
#endif

/*
 * These values determine the minumum and maximum distances
 * that qualify as ray-object intersections.
 */

#define Small_Tolerance 0.001
#define Max_Distance 1.0e7


#ifndef DBL_FORMAT_STRING
#define DBL_FORMAT_STRING "%lf"
#endif

#ifndef DBL
#define DBL double
#endif

#ifndef SNGL
#define SNGL float
#endif

#ifndef COLC
#define COLC float
#endif

#ifndef UCS2
#define UCS2 unsigned short
#endif

#ifndef UCS4
#define UCS4 unsigned int
#endif

#ifndef POV_LONG
#define POV_LONG long long
#define POV_ULONG unsigned long long
#endif

#ifndef POV_ULONG
#define POV_ULONG unsigned POV_LONG
#endif

#ifndef M_PI
#define M_PI   3.1415926535897932384626
#endif

#ifndef M_PI_2
#define M_PI_2 1.57079632679489661923
#endif

#ifndef TWO_M_PI
#define TWO_M_PI 6.283185307179586476925286766560
#endif

#ifndef M_PI_180
#define M_PI_180 0.01745329251994329576
#endif

#ifndef M_PI_360
#define M_PI_360 0.00872664625997164788
#endif

/* Some implementations of scanf return 0 on failure rather than EOF */
#ifndef SCANF_EOF
#define SCANF_EOF EOF
#endif

/* Adjust to match floating-point parameter(s) of functions in math.h/cmath */
#ifndef SYS_MATH_PARAM
#define SYS_MATH_PARAM double
#endif

/* Adjust to match floating-point return value of functions in math.h/cmath */
#ifndef SYS_MATH_RETURN
#define SYS_MATH_RETURN double
#endif

/* Function that executes functions, the parameter is the function index */
#ifndef POVFPU_Run
#define POVFPU_Run(fn) POVFPU_RunDefault(fn)
#endif

/* Adjust to add system specific handling of functions like just-in-time compilation */
#if (SYS_FUNCTIONS == 0)

// Note that if SYS_FUNCTIONS is 1, it will enable the field dblstack
// in FPUContext_Struct and corresponding calculations in POVFPU_SetLocal
// as well as POVFPU_NewContext.
#define SYS_FUNCTIONS 0

// Called after a function has been added, parameter is the function index
#define SYS_ADD_FUNCTION(fe)
// Called before a function is deleted, parameter is a pointer to the FunctionEntry_Struct
#define SYS_DELETE_FUNCTION(fe)
// Called inside POVFPU_Init after everything else has been inited
#define SYS_INIT_FUNCTIONS()
// Called inside POVFPU_Terminate before anything else is deleted
#define SYS_TERM_FUNCTIONS()
// Called inside POVFPU_Reset before anything else is reset
#define SYS_RESET_FUNCTIONS()

// Adjust to add system specific fields to FunctionEntry_Struct
#define SYS_FUNCTION_ENTRY

#endif // SYS_FUNCTIONS

#ifndef CDECL
#define CDECL
#endif

#ifndef ALIGN16
#define ALIGN16
#endif

#ifndef INLINE_NOISE
#define INLINE_NOISE
#endif

#ifndef USE_FASTER_NOISE
#define USE_FASTER_NOISE 0
#endif

#ifndef NEW_LINE_STRING
#define NEW_LINE_STRING "\n"
#endif

/* If compiler version is undefined, then make it 'u' for unknown */
#ifndef COMPILER_VER
#define COMPILER_VER ".u"
#endif

#ifndef QSORT
#define QSORT(a,b,c,d) qsort((a),(b),(c),(d))
#endif

/* Get minimum/maximum of three values. */

#define max3(x,y,z) (max((x), max((y), (z))))
#define min3(x,y,z) (min((x), min((y), (z))))


/*
 * POV_NAME_MAX is for file systems that have a separation of the filename
 * into name.ext.  The POV_NAME_MAX is the name part.  FILE_NAME_LENGTH
 * is the sum of name + extension.
 */
#ifndef POV_NAME_MAX
#define POV_NAME_MAX 8
#endif

#ifndef FILE_NAME_LENGTH
#define FILE_NAME_LENGTH 150
#endif

#ifndef FILENAME_SEPARATOR
#define FILENAME_SEPARATOR '/'
#endif

#ifndef DRIVE_SEPARATOR
#define DRIVE_SEPARATOR ':'
#endif

/*
 * Splits a given string into the path and file components using the
 * FILENAME_SEPARATOR and DRIVE_SEPARATOR
 */
#ifndef POV_SPLIT_PATH
#define POV_SPLIT_PATH(s,p,f) POV_BASE_NAMESPACE::Split_Path((s),(p),(f))
#endif

/* The output file format used if the user doesn't specify one */
#ifndef DEFAULT_OUTPUT_FORMAT
#define DEFAULT_OUTPUT_FORMAT   't'
#endif

/* System specific image format like BMP for Windows or PICT for Mac */
#ifndef READ_SYS_IMAGE
#define READ_SYS_IMAGE(i,f) Read_Targa_Image(i,f)
#endif

#ifndef SYS_IMAGE_CLASS
#define SYS_IMAGE_CLASS Targa_Image
#endif

#ifndef SYS_DEF_EXT
#define SYS_DEF_EXT ".tga"
#endif


/*
 * The TIME macros are used when displaying the rendering time for the user.
 * These are called in such a manner that STOP_TIME can be called multiple
 * times for a givn START_TIME in order to get intermediate TIME_ELAPSED
 * values.  TIME_ELAPSED is often defined as (tstop - tstart).
 */
#ifndef START_TIME
#define START_TIME time(&tstart);     
#endif

#ifndef STOP_TIME
#define STOP_TIME  time(&tstop);
#endif

#ifndef TIME_ELAPSED
#define TIME_ELAPSED difftime (tstop, tstart);
#endif

#ifndef SPLIT_TIME
#define SPLIT_TIME(d,h,m,s) POV_Std_Split_Time ((d),(h),(m),(s))
#endif


/*
 * The PRECISION_TIMER macros are used in generating histogram images on
 * systems that have very accurate timers (usually in the microsecond range).
 */
#ifndef PRECISION_TIMER_AVAILABLE
#define PRECISION_TIMER_AVAILABLE 0
#endif

#ifndef PRECISION_TIMER_INIT  /* Called once to initialize the timer */
#define PRECISION_TIMER_INIT
#endif

#ifndef PRECISION_TIMER_START
#define PRECISION_TIMER_START ;
#endif

#ifndef PRECISION_TIMER_STOP
#define PRECISION_TIMER_STOP
#endif

#ifndef PRECISION_TIMER_COUNT  /* The difference between START and STOP times */
#define PRECISION_TIMER_COUNT 0
#endif

/*
 * Font related macros [trf]
 */

#ifndef POV_CONVERT_TEXT_TO_UCS2
#define POV_CONVERT_TEXT_TO_UCS2(ts, tsl, as) (NULL)
#endif

/*
 * The COOPERATE macros are used on co-operative multi-tasking systems to
 * return control to the GUI or OS.  COOPERATE is the old form, and one
 * or both of COOPERATE_0 and COOPERATE_1 should be defined instead.
 */
#ifdef COOPERATE
#define COOPERATE_0     COOPERATE
#define COOPERATE_1     COOPERATE
#define COOPERATE_2     COOPERATE
#endif

#ifndef COOPERATE_0    /* Called less frequently */
#define COOPERATE_0
#endif

#ifndef COOPERATE_1    /* Called more frequently */
#define COOPERATE_1
#endif

#ifndef COOPERATE_2    /* Called only when using povray_cooperate */
#define COOPERATE_2
#endif


/* How to get input from the user */
#ifndef TEST_ABORT
#define TEST_ABORT
#endif

#ifndef WAIT_FOR_KEYPRESS
#define WAIT_FOR_KEYPRESS
#else
#define WAIT_FOR_KEYPRESS_EXISTS
#endif

#ifndef GET_KEY /* Gets a keystroke from the user without waiting */
#define GET_KEY
#else
#define GET_KEY_EXISTS
#endif

/*
 * Functions that handle the graphical display preview.  These functions
 * will be customeized for all versions of POV that want to do any sort
 * of rendering preview.  The default functions will create a 80x25 text
 * "rendering" using crude ASCII graphics.
 */
#ifndef POV_DISPLAY_INIT     /* Initializes display for each frame rendered */
#define POV_DISPLAY_INIT(ref,w,h) POV_Std_Display_Init((w),(h));
#endif

#ifndef POV_DISPLAY_FINISHED  /* Waits for user input after rendering done */
#define POV_DISPLAY_FINISHED(ref) POV_Std_Display_Finished();
#endif

#ifndef POV_DISPLAY_CLOSE     /* Closes the display window after each frame */
#define POV_DISPLAY_CLOSE(ref) POV_Std_Display_Close();
#endif

#ifndef POV_DISPLAY_PLOT      /* Plots a single pixel */
#define POV_DISPLAY_PLOT(ref,x,y,r,g,b,a) POV_Std_Display_Plot((x),(y),(r),(g),(b),(a));
#endif

#ifndef POV_DISPLAY_PLOT_ROW  /* Plots a row of pixels */
#define POV_DISPLAY_PLOT_ROW(ref,w,y,s,e,r,g,b,a) // do nothing because POV_DISPLAY_PLOT will be used to send the same data by default
#endif

#ifndef POV_DISPLAY_PLOT_RECT /* Plots a filled rectangle */
#define POV_DISPLAY_PLOT_RECT(ref,x1,y1,x2,y2,r,g,b,a) POV_Std_Display_Plot_Rect((x1),(y1),(x2),(y2),(r),(g),(b),(a));
#endif

#ifndef POV_DISPLAY_PLOT_BOX  /* Plots a hollow box */
#define POV_DISPLAY_PLOT_BOX(ref,x1,y1,x2,y2,r,g,b,a) POV_Std_Display_Plot_Box((x1),(y1),(x2),(y2),(r),(g),(b),(a));
#endif

#ifndef POV_GET_FULL_PATH     /* returns full pathspec */
#define POV_GET_FULL_PATH(f,p,b) if (b) strcpy(b,p);
#endif

#ifndef POV_WRITE_LINE        /* write the current line to something */
#define POV_WRITE_LINE(line,y)
#endif

#ifndef POV_ASSIGN_PIXEL      /* assign the colour of a pixel */
#define POV_ASSIGN_PIXEL(x,y,colour)
#endif

#ifndef POV_ASSIGN_PIXEL_UNCLIPPED      /* assign the unclipped colour of a pixel */
#define POV_ASSIGN_PIXEL_UNCLIPPED(x,y,colour)
#endif

/* The next two are palette modes, for normal and grayscale display */
#ifndef NORMAL
#define NORMAL '0'
#endif

#ifndef GREY
#define GREY   'G'
#endif

/*
 * The DEFAULT_DISPLAY_GAMMA is used when there isn't one specified by the
 * user in the POVRAY.INI.  For those systems that are very savvy, this
 * could be a function which returns the current display gamma.  The
 * DEFAULT_ASSUMED_GAMMA should be left alone.
 */
#ifndef DEFAULT_DISPLAY_GAMMA
#define DEFAULT_DISPLAY_GAMMA 2.2
#endif

#ifndef DEFAULT_ASSUMED_GAMMA
#define DEFAULT_ASSUMED_GAMMA 1.0
#endif


/*****************************************************************************
 *
 * MEMIO.C Memory macros
 *
 *****************************************************************************/

/*
 * These functions define macros which do checking for memory allocation,
 * and can also do other things.  Check mem.c before you change them, since
 * they aren't simply replacements for malloc, calloc, realloc, and free.
 */
#ifndef POV_MALLOC
#define POV_MALLOC(size,msg)        pov_malloc ((size), __FILE__, __LINE__, (msg))
#endif

#ifndef POV_CALLOC
#define POV_CALLOC(nitems,size,msg) pov_calloc ((nitems), (size), __FILE__, __LINE__, (msg))
#endif

#ifndef POV_REALLOC
#define POV_REALLOC(ptr,size,msg)   pov_realloc ((ptr), (size), __FILE__, __LINE__, (msg))
#endif

#ifndef POV_FREE
#define POV_FREE(ptr)               { pov_free ((void *)(ptr), __FILE__, __LINE__); (ptr) = NULL; }
#endif

#ifndef POV_MEM_INIT
#define POV_MEM_INIT()              mem_init()
#endif

#ifndef POV_MEM_RELEASE_ALL
#define POV_MEM_RELEASE_ALL()       mem_release_all()
#endif

#ifndef POV_STRDUP
#define POV_STRDUP(str)             pov_strdup(str)
#endif

/* For those systems that don't have memmove, this can also be pov_memmove */
#ifndef POV_MEMMOVE
#define POV_MEMMOVE(dst,src,len)    pov_memmove((dst),(src),(len))
#endif

#ifndef POV_MEMCPY
#define POV_MEMCPY(dst,src,len)     memcpy((dst),(src),(len))
#endif

/*
 * Functions which invoke external programs to do work for POV, generally
 * at the request of the user.
 */
#ifndef POV_SHELLOUT
#define POV_SHELLOUT(string) pov_shellout(string)
#endif

#ifndef POV_MAX_CMD_LENGTH
#define POV_MAX_CMD_LENGTH 250
#endif

#ifndef POV_SYSTEM
#define POV_SYSTEM(string) system(string)
#endif

/* Functions to delete and rename a file */
#ifndef DELETE_FILE_ERR
#define DELETE_FILE_ERR -1
#endif

#ifndef DELETE_FILE
#define DELETE_FILE(name) unlink(name)
#endif

#ifndef RENAME_FILE_ERR
#define RENAME_FILE_ERR -1
#endif

#ifndef RENAME_FILE
#define RENAME_FILE(orig,new) rename(orig,new)
#endif

#ifndef MAX_BUFSIZE  /* The maximum size of the output file buffer */
#define MAX_BUFSIZE INT_MAX
#endif

/*****************************************************************************
 *
 * Typedefs that need to be known here.
 *
 *****************************************************************************/

typedef struct Object_Struct OBJECT;
typedef struct Compound_Object_Struct COMPOUND_OBJECT;
typedef struct Ray_Struct RAY;
typedef struct istack_struct ISTACK;
typedef struct istk_entry INTERSECTION;

/*****************************************************************************
 *
 * Scalar, color and vector stuff.
 *
 *****************************************************************************/

typedef DBL UV_VECT[2];
typedef DBL VECTOR[3];
typedef DBL VECTOR_4D[4];
typedef DBL MATRIX[4][4];
typedef DBL EXPRESS[5];
typedef COLC COLOUR[5];
typedef COLC RGB[3];
typedef SNGL SNGL_VECT[3];

/* Vector array elements. */
enum
{
	U = 0,
	V = 1
};

enum
{
	X = 0,
	Y = 1,
	Z = 2,
	T = 3
};

/* Color array elements. */
enum
{
	pRED    = 0,
	pGREEN  = 1,
	pBLUE   = 2,
	pFILTER = 3,
	pTRANSM = 4
};

/* Macros to manipulate scalars, vectors, and colors. */

inline void Assign_Vector(VECTOR d, VECTOR s)
{
	d[X] = s[X];
	d[Y] = s[Y];
	d[Z] = s[Z];
}

inline void Assign_Vector(VECTOR d, SNGL_VECT s)
{
	d[X] = s[X];
	d[Y] = s[Y];
	d[Z] = s[Z];
}

inline void Assign_Vector(SNGL_VECT d, VECTOR s)
{
	d[X] = s[X];
	d[Y] = s[Y];
	d[Z] = s[Z];
}

inline void Assign_Vector(SNGL_VECT d, SNGL_VECT s)
{
	d[X] = s[X];
	d[Y] = s[Y];
	d[Z] = s[Z];
}

inline void Assign_UV_Vect(UV_VECT d, UV_VECT s)
{
	d[X] = s[X];
	d[Y] = s[Y];
}

inline void Assign_Vector_4D(VECTOR_4D d, VECTOR_4D s)
{
	d[X] = s[X];
	d[Y] = s[Y];
	d[Z] = s[Z];
	d[T] = s[T];
}

inline void Assign_Colour(COLOUR d, COLOUR s)
{
	d[pRED] = s[pRED];
	d[pGREEN] = s[pGREEN];
	d[pBLUE] = s[pBLUE];
	d[pFILTER] = s[pFILTER];
	d[pTRANSM] = s[pTRANSM];
}

inline void Assign_RGB(RGB d, RGB s)
{
	d[pRED] = s[pRED];
	d[pGREEN] = s[pGREEN];
	d[pBLUE] = s[pBLUE];
}

inline void Assign_Colour_Express(COLOUR d, EXPRESS s)
{
	d[pRED] = s[pRED];
	d[pGREEN] = s[pGREEN];
	d[pBLUE] = s[pBLUE];
	d[pFILTER] = s[pFILTER];
	d[pTRANSM] = s[pTRANSM];
}

inline void Assign_Express(EXPRESS d, EXPRESS s)
{
	d[pRED] = s[pRED];
	d[pGREEN] = s[pGREEN];
	d[pBLUE] = s[pBLUE];
	d[pFILTER] = s[pFILTER];
	d[pTRANSM] = s[pTRANSM];
}

inline void Make_Colour(COLOUR c, COLC r, COLC g, COLC b)
{
	c[pRED] = r;
	c[pGREEN] = g;
	c[pBLUE] = b;
	c[pFILTER] = 0.0;
	c[pTRANSM] = 0.0;
}

inline void Make_ColourA(COLOUR c, COLC r, COLC g, COLC b, COLC a, COLC t)
{
	c[pRED] = r;
	c[pGREEN] = g;
	c[pBLUE] = b;
	c[pFILTER] = a;
	c[pTRANSM] = t;
}

inline void Make_Vector(VECTOR v, DBL a, DBL b, DBL c)
{
	v[X] = a;
	v[Y] = b;
	v[Z] = c;
}

inline void Make_Vector(SNGL_VECT v, SNGL a, SNGL b, SNGL c)
{
	v[X] = a;
	v[Y] = b;
	v[Z] = c;
}

inline void Make_UV_Vector(UV_VECT v, DBL a, DBL b)
{
	v[U] = a;
	v[V] = b;
}

inline void Make_RGB(RGB c, COLC r, COLC g, COLC b)
{
	c[pRED] = r;
	c[pGREEN] = g;
	c[pBLUE] = b;
}

inline void Destroy_Float(DBL *x)
{
	if(x != NULL)
		POV_FREE(x);
}

inline void Destroy_Vector(VECTOR *x)
{
	if(x != NULL)
		POV_FREE(x);
}

inline void Destroy_UV_Vect(UV_VECT *x)
{
	if(x != NULL)
		POV_FREE(x);
}

inline void Destroy_Vector_4D(VECTOR_4D *x)
{
	if(x != NULL)
		POV_FREE(x);
}

inline void Destroy_Colour(COLOUR *x)
{
	if(x != NULL)
		POV_FREE(x);
}


/*****************************************************************************
 *
 * Bounding box stuff (see also BOUND.H).
 *
 *****************************************************************************/

typedef SNGL BBOX_VAL;

typedef BBOX_VAL BBOX_VECT[3];

typedef struct Bounding_Box_Struct BBOX;

struct Bounding_Box_Struct
{
  BBOX_VECT Lower_Left, Lengths;
};

inline void Assign_BBox_Vect(BBOX_VECT& d, BBOX_VECT s)
{
	d[X] = s[X];
	d[Y] = s[Y];
	d[Z] = s[Z];
}

inline void Assign_BBox_Vect(BBOX_VECT& d, VECTOR s)
{
	d[X] = s[X];
	d[Y] = s[Y];
	d[Z] = s[Z];
}

inline void Assign_BBox_Vect(VECTOR& d, BBOX_VECT s)
{
	d[X] = s[X];
	d[Y] = s[Y];
	d[Z] = s[Z];
}

inline void Make_BBox(BBOX& BBox, BBOX_VAL llx, BBOX_VAL lly, BBOX_VAL llz, BBOX_VAL lex, BBOX_VAL ley, BBOX_VAL lez)
{
	BBox.Lower_Left[X] = (BBOX_VAL)(llx);
	BBox.Lower_Left[Y] = (BBOX_VAL)(lly);
	BBox.Lower_Left[Z] = (BBOX_VAL)(llz);
	BBox.Lengths[X] = (BBOX_VAL)(lex);
	BBox.Lengths[Y] = (BBOX_VAL)(ley);
	BBox.Lengths[Z] = (BBOX_VAL)(lez);
}

inline void Make_BBox_from_min_max(BBOX& BBox, BBOX_VECT mins, BBOX_VECT maxs)
{
	BBox.Lower_Left[X] = (BBOX_VAL)(mins[X]);
	BBox.Lower_Left[Y] = (BBOX_VAL)(mins[Y]);
	BBox.Lower_Left[Z] = (BBOX_VAL)(mins[Z]);
	BBox.Lengths[X] = (BBOX_VAL)(maxs[X]-mins[X]);
	BBox.Lengths[Y] = (BBOX_VAL)(maxs[Y]-mins[Y]);
	BBox.Lengths[Z] = (BBOX_VAL)(maxs[Z]-mins[Z]);
}

inline void Make_BBox_from_min_max(BBOX& BBox, VECTOR mins, VECTOR maxs)
{
	BBox.Lower_Left[X] = (BBOX_VAL)(mins[X]);
	BBox.Lower_Left[Y] = (BBOX_VAL)(mins[Y]);
	BBox.Lower_Left[Z] = (BBOX_VAL)(mins[Z]);
	BBox.Lengths[X] = (BBOX_VAL)(maxs[X]-mins[X]);
	BBox.Lengths[Y] = (BBOX_VAL)(maxs[Y]-mins[Y]);
	BBox.Lengths[Z] = (BBOX_VAL)(maxs[Z]-mins[Z]);
}

inline void Make_min_max_from_BBox(BBOX_VECT& mins, BBOX_VECT& maxs, BBOX BBox)
{
  mins[X] = BBox.Lower_Left[X];
  mins[Y] = BBox.Lower_Left[Y];
  mins[Z] = BBox.Lower_Left[Z];
  maxs[X] = mins[X] + BBox.Lengths[X];
  maxs[Y] = mins[Y] + BBox.Lengths[Y];
  maxs[Z] = mins[Z] + BBox.Lengths[Z];
}

inline void Make_min_max_from_BBox(VECTOR& mins, VECTOR& maxs, BBOX BBox)
{
  mins[X] = BBox.Lower_Left[X];
  mins[Y] = BBox.Lower_Left[Y];
  mins[Z] = BBox.Lower_Left[Z];
  maxs[X] = mins[X] + BBox.Lengths[X];
  maxs[Y] = mins[Y] + BBox.Lengths[Y];
  maxs[Z] = mins[Z] + BBox.Lengths[Z];
}


/*****************************************************************************
 *
 * Hi-resolution counter.
 *
 *****************************************************************************/

/* 64bit counter. */

typedef POV_LONG COUNTER;

inline DBL DBL_Counter(COUNTER x)
{
	return (DBL)(x);
}

inline void Long_To_Counter(long i, COUNTER& x)
{
	x = i;
}

inline void Init_Counter(COUNTER& x)
{
	x = 0;
}

inline void Increase_Counter(COUNTER& x)
{
	x++;
}

inline void Decrease_Counter(COUNTER& x)
{
	x--;
}

inline void Add_Counter(COUNTER& x, COUNTER a, COUNTER b)
{
	x = a + b;
}


/*****************************************************************************
 *
 * Transformation stuff.
 *
 *****************************************************************************/

typedef struct Transform_Struct TRANSFORM;

struct Transform_Struct
{
  MATRIX matrix;
  MATRIX inverse;
};



/*****************************************************************************
 *
 * Color map stuff.
 *
 *****************************************************************************/

const int MAX_BLEND_MAP_ENTRIES = 256;

typedef struct Blend_Map_Entry BLEND_MAP_ENTRY;
typedef struct Blend_Map_Struct BLEND_MAP;
typedef struct Pattern_Struct TPATTERN;
typedef struct Texture_Struct TEXTURE;
typedef struct Pigment_Struct PIGMENT;
typedef struct Tnormal_Struct TNORMAL;
typedef struct Finish_Struct FINISH;
typedef struct Turb_Struct TURB;
typedef struct Warps_Struct WARP;
typedef struct Light_Source_Struct LIGHT_SOURCE;
typedef struct Spline_Entry SPLINE_ENTRY;
typedef struct Spline_Struct SPLINE;

struct Blend_Map_Entry
{
  SNGL value;
  unsigned char Same;
  union
  {
   COLOUR Colour;
   PIGMENT *Pigment;
   TNORMAL *Tnormal;
   TEXTURE *Texture;
   UV_VECT Point_Slope;
  } Vals;
};

struct Blend_Map_Struct
{
  int Users;
  short Number_Of_Entries;
  char Transparency_Flag, Type;
  BLEND_MAP_ENTRY *Blend_Map_Entries;
};

inline void Make_Blend_Map_Entry(BLEND_MAP_ENTRY& entry, SNGL v, unsigned char s, COLC r, COLC g, COLC b, COLC a, COLC t)
{
	entry.value = v;
	entry.Same = s;
	Make_ColourA(entry.Vals.Colour, r, g, b, a, t);
}


/*****************************************************************************
 *
 * Media stuff.
 *
 *****************************************************************************/

typedef struct Media_Struct IMEDIA;

struct Media_Struct
{
  int Type;
  int Intervals;
  int Min_Samples;
  int Max_Samples;
  int Sample_Method; /* MH */
  DBL Jitter; /* MH */
  int is_constant;
  DBL Eccentricity,sc_ext;
  int use_absorption;
  int use_emission;
  int use_extinction;
  int use_scattering;
  COLOUR Absorption;
  COLOUR Emission;
  COLOUR Extinction;
  COLOUR Scattering;

  DBL Ratio;
  DBL Confidence;
  DBL Variance;
  DBL *Sample_Threshold;

  DBL AA_Threshold;
  int AA_Level;
  int ignore_photons;

  PIGMENT *Density;

  IMEDIA *Next_Media;
};



/*****************************************************************************
 *
 * Interior stuff.
 *
 *****************************************************************************/

typedef struct Interior_Struct INTERIOR;

struct Interior_Struct
{
  int References;
  int  hollow, Disp_NElems;
  SNGL IOR, Dispersion;
  SNGL Caustics, Old_Refract;
  SNGL Fade_Distance, Fade_Power;
  COLOUR Fade_Colour;
  IMEDIA *IMedia;
};

struct Spline_Entry
{
  DBL par;      /* Parameter */
  DBL vec[5];   /* Value at the parameter */
  DBL coeff[5]; /* Interpolating coefficients at the parameter */
};

struct Spline_Struct
{
  int Number_Of_Entries, Type;
  int Max_Entries;
  SPLINE_ENTRY *SplineEntries;
  int Coeffs_Computed;
  int Terms;
  bool Cache_Valid;
  int Cache_Type;
  DBL Cache_Point;
  EXPRESS Cache_Data;
};

typedef struct Spline_Entry SPLINE_ENTRY;

/*****************************************************************************
 *
 * IFF file stuff.
 *
 *****************************************************************************/

#ifndef IFF_SWITCH_CAST
#define IFF_SWITCH_CAST (int)
#endif

typedef struct Image_Colour_Struct IMAGE_COLOUR;

typedef struct Image8_Line_Struct IMAGE8_LINE;

typedef struct Image16_Line_Struct IMAGE16_LINE;

struct Image_Colour_Struct
{
  unsigned short Red, Green, Blue, Filter, Transmit;
};

struct Image8_Line_Struct
{
  unsigned char *red, *green, *blue, *transm;
};

struct Image16_Line_Struct
{
  unsigned short *red, *green, *blue, *transm;
};



/*****************************************************************************
 *
 * Image stuff.
 *
 *****************************************************************************/

/* Legal image attributes. */

#define NO_FILE         0x00000000
#define GIF_FILE        0x00000001
#define POT_FILE        0x00000002
#define SYS_FILE        0x00000004
#define IFF_FILE        0x00000008
#define TGA_FILE        0x00000010
#define GRAD_FILE       0x00000020
#define PGM_FILE        0x00000040
#define PPM_FILE        0x00000080
#define PNG_FILE        0x00000100
#define JPEG_FILE       0x00000200
#define TIFF_FILE       0x00000400

#define IMAGE_FILE_MASK 0x000007FF

#define IMAGE_FTYPE     0x00000800
#define HF_FTYPE        0x00001000
#define HIST_FTYPE      0x00002000
#define GRAY_FTYPE      0x00004000
#define NORMAL_FTYPE    0x00008000
#define MATERIAL_FTYPE  0x00010000

#define IS16BITIMAGE    0x00020000
#define IS16GRAYIMAGE   0x00040000

/* Image types. */

#define IMAGE_FILE    IMAGE_FTYPE+GIF_FILE+SYS_FILE+IFF_FILE+GRAD_FILE+TGA_FILE+PGM_FILE+PPM_FILE+PNG_FILE+JPEG_FILE+TIFF_FILE
#define NORMAL_FILE   NORMAL_FTYPE+GIF_FILE+SYS_FILE+IFF_FILE+GRAD_FILE+TGA_FILE+PGM_FILE+PPM_FILE+PNG_FILE+JPEG_FILE+TIFF_FILE
#define MATERIAL_FILE MATERIAL_FTYPE+GIF_FILE+SYS_FILE+IFF_FILE+GRAD_FILE+TGA_FILE+PGM_FILE+PPM_FILE+PNG_FILE+JPEG_FILE+TIFF_FILE
#define HF_FILE       HF_FTYPE+GIF_FILE+SYS_FILE+POT_FILE+TGA_FILE+PGM_FILE+PPM_FILE+PNG_FILE+JPEG_FILE+TIFF_FILE

typedef struct Image_Struct IMAGE;

struct Image_Struct
{
  int References; /* Keeps track of number of pointers to this structure */
  int Map_Type;
  int File_Type;
  int Image_Type; /* What this image is being used for */
  int Interpolation_Type;
  int iwidth, iheight;
  short Colour_Map_Size;
  char Once_Flag;
  char Use_Colour_Flag;
  VECTOR Gradient;
  SNGL width, height;
  UV_VECT Offset;
  DBL AllFilter, AllTransmit; 
  IMAGE_COLOUR *Colour_Map;
  void *Object;
  union
  {
    IMAGE8_LINE *rgb8_lines;
    IMAGE16_LINE *rgb16_lines;
    unsigned short **gray16_lines;
    unsigned char **map_lines;
  } data;
};

#define PIGMENT_TYPE  0
#define NORMAL_TYPE   1
#define PATTERN_TYPE  2
#define TEXTURE_TYPE  4
#define COLOUR_TYPE   5
#define SLOPE_TYPE    6
#define DENSITY_TYPE  7

#define DEFAULT_FRACTAL_EXTERIOR_TYPE 1
#define DEFAULT_FRACTAL_INTERIOR_TYPE 0
#define DEFAULT_FRACTAL_EXTERIOR_FACTOR 1
#define DEFAULT_FRACTAL_INTERIOR_FACTOR 1


/*****************************************************************************
 *
 * Pigment, Tnormal, Finish, Texture & Warps stuff.
 *
 *****************************************************************************/

typedef struct Density_file_Struct DENSITY_FILE;
typedef struct Density_file_Data_Struct DENSITY_FILE_DATA;

struct Density_file_Struct
{
  int Interpolation;
  DENSITY_FILE_DATA *Data;
};

struct Density_file_Data_Struct
{
  int References;
  char *Name;
  int Sx, Sy, Sz;
  int Type;
  union
  {
    unsigned char *Density8;
    unsigned short *Density16;
    unsigned int *Density32;
  };
};


#define TPATTERN_FIELDS       \
 unsigned short Type, Wave_Type, Flags; \
 int References;             \
 SNGL Frequency, Phase;      \
 SNGL Exponent;              \
 WARP *Warps;                \
 TPATTERN *Next;             \
 BLEND_MAP *Blend_Map;       \
 union {                     \
   DENSITY_FILE *Density_File; \
   IMAGE *Image;              \
   VECTOR Gradient;           \
   SNGL Agate_Turb_Scale;     \
   short Num_of_Waves;        \
   short Iterations;          \
   short Arms;                \
   struct { SNGL Mortar; VECTOR Size; } Brick; \
   struct { SNGL Control0, Control1; } Quilted;   \
   struct { DBL Size, UseCoords; VECTOR Metric; } Facets; \
   struct { VECTOR Form; VECTOR Metric; DBL Offset; DBL Dim; \
            short IsSolid; VECTOR *cv; int lastseed; \
            VECTOR lastcenter; }  Crackle;  \
   struct { VECTOR Slope_Vector, Altit_Vector; \
            short Slope_Base, Altit_Base; DBL Slope_Len, \
            Altit_Len; UV_VECT Slope_Mod, Altit_Mod; } Slope; \
   struct { UV_VECT Coord; short Iterations, interior_type, \
            exterior_type; DBL efactor, ifactor; \
            int Exponent; } Fractal; \
   struct { void *Fn; void *Data; } Function;\
   PIGMENT *Pigment; \
   OBJECT *Object;\
 } Vals;

struct Pattern_Struct
{
  TPATTERN_FIELDS
};

struct Pigment_Struct
{
  TPATTERN_FIELDS
  COLOUR Colour; 
};

struct Tnormal_Struct
{
  TPATTERN_FIELDS
  SNGL Amount;
  SNGL Delta; /* NK delta */
};

#define TEXTURE_FIELDS \
  TPATTERN_FIELDS      \
  TEXTURE *Next_Material;

struct Texture_Struct
{
  TEXTURE_FIELDS
  PIGMENT *Pigment;
  TNORMAL *Tnormal;
  FINISH *Finish;
  TEXTURE *Materials;
  int Num_Of_Mats;

};

struct Finish_Struct
{
  SNGL Diffuse, Brilliance;
  SNGL Specular, Roughness;
  SNGL Phong, Phong_Size;
  SNGL Irid, Irid_Film_Thickness, Irid_Turb;
  SNGL Temp_Caustics, Temp_IOR, Temp_Dispersion, Temp_Refract, Reflect_Exp;
  SNGL Crand, Metallic;
  RGB Ambient, Reflection_Max, Reflection_Min;  /* Changed by MBP 8/27/98 */
  SNGL Reflection_Falloff;  /* Added by MBP 8/27/98 */
  int Reflection_Type;  /* Added by MBP 9/5/98 */
  SNGL Reflect_Metallic; /* MBP */
  int Conserve_Energy;  /* added by NK Dec 19 1999 */
};

#define WARP_FIELDS unsigned short Warp_Type; WARP *Prev_Warp; WARP *Next_Warp;

struct Warps_Struct
{
  WARP_FIELDS
};

struct Turb_Struct
{
  WARP_FIELDS
  VECTOR Turbulence;
  int Octaves;
  SNGL Lambda, Omega;
};

#define Destroy_Finish(x) if ((x)!=NULL) POV_FREE(x)

typedef struct Material_Struct MATERIAL;

struct Material_Struct
{
   TEXTURE *Texture;
   INTERIOR *Interior;
   TEXTURE * Interior_Texture;
};




/*****************************************************************************
 *
 * Object stuff (see also OBJECTS.H and primitive include files).
 *
 *****************************************************************************/

#define All_Intersections(x,y,z) ((*((x)->Methods->All_Intersections_Method)) (x,y,z))
#define Inside(x,y) ((*((y)->Methods->Inside_Method)) (x,y))
#define Normal(x,y,z) ((*((y)->Methods->Normal_Method)) (x,y,z))
#define UVCoord(x,y,z) ((*((y)->Methods->UVCoord_Method)) (x,y,z))
#define Copy(x) ((*((x)->Methods->Copy_Method)) (x))
#define Translate(x,y,z) ((*((x)->Methods->Translate_Method)) (x,y,z))
#define Scale(x,y,z) ((*((x)->Methods->Scale_Method)) (x,y,z))
#define Rotate(x,y,z) ((*((x)->Methods->Rotate_Method)) (x,y,z))
#define Transform(x,y) ((*((x)->Methods->Transform_Method)) (x,y))
#define Invert(x) ((*((x)->Methods->Invert_Method)) (x))
#define Destroy(x) ((*((x)->Methods->Destroy_Method)) (x))

typedef struct Method_Struct METHODS;

typedef int (*ALL_INTERSECTIONS_METHOD)(OBJECT *, RAY *, ISTACK *);
typedef int (*INSIDE_METHOD)(VECTOR , OBJECT *);
typedef void (*NORMAL_METHOD)(VECTOR, OBJECT *, INTERSECTION *);
typedef void (*UVCOORD_METHOD)(UV_VECT, OBJECT *, INTERSECTION *);
typedef void *(*COPY_METHOD)(OBJECT *);
typedef void (*TRANSLATE_METHOD)(OBJECT *, VECTOR, TRANSFORM *);
typedef void (*ROTATE_METHOD)(OBJECT *, VECTOR, TRANSFORM *);
typedef void (*SCALE_METHOD)(OBJECT *, VECTOR, TRANSFORM *);
typedef void (*TRANSFORM_METHOD)(OBJECT *, TRANSFORM *);
typedef void (*INVERT_METHOD)(OBJECT *);
typedef void (*DESTROY_METHOD)(OBJECT *);

/* These fields are common to all objects. */

#define OBJECT_FIELDS        \
  METHODS *Methods;          \
  int Type;                  \
  OBJECT *Sibling;           \
  TEXTURE *Texture;          \
  TEXTURE *Interior_Texture; \
  INTERIOR *Interior;        \
  OBJECT *Bound;             \
  OBJECT *Clip;              \
  LIGHT_SOURCE *LLights;     \
  BBOX BBox;                 \
  TRANSFORM *Trans;          \
  TRANSFORM *UV_Trans;       \
  SNGL Ph_Density;           \
  unsigned int Flags;

/* These fields are common to all compound objects */

#define COMPOUND_FIELDS \
  OBJECT_FIELDS         \
  OBJECT *Children;

#define INIT_OBJECT_FIELDS(o,t,m) \
  o->Type     = t;                \
  o->Methods  = m;                \
  o->Sibling  = NULL;             \
  o->Texture  = NULL;             \
  o->Interior_Texture = NULL;     \
  o->Interior = NULL;             \
  o->Bound    = NULL;             \
  o->Clip     = NULL;             \
  o->LLights  = NULL;             \
  o->Trans    = NULL;             \
  o->UV_Trans = NULL;             \
  o->Ph_Density = 0;              \
  o->Flags    = 0;                \
  Make_BBox(o->BBox, -BOUND_HUGE/2.0, -BOUND_HUGE/2.0, -BOUND_HUGE/2.0, \
    BOUND_HUGE, BOUND_HUGE, BOUND_HUGE);

#define DUMP_OBJECT_FIELDS(o) \
  Debug_Info("\tNULL, // Methods\n"); \
  Debug_Info("\t%d, // Type\n", (int)o->Type); \
  Debug_Info("\tNULL, // Sibling\n"); \
  Debug_Info("\tNULL, // Texture\n"); \
  Debug_Info("\tNULL, // Interior_Texture\n"); \
  Debug_Info("\tNULL, // Interior\n"); \
  Debug_Info("\tNULL, // Bound\n"); \
  Debug_Info("\tNULL, // Clip\n"); \
  Debug_Info("\tNULL, // LLights\n"); \
  Debug_Info("\t{ // BBox\n"); \
  Debug_Info("\t\t{ %f, %f, %f }, // Lower_Left\n", \
             (DBL)o->BBox.Lower_Left[X], \
             (DBL)o->BBox.Lower_Left[Y], \
             (DBL)o->BBox.Lower_Left[Z]); \
  Debug_Info("\t\t{ %f, %f, %f } // Lengths\n", \
             (DBL)o->BBox.Lengths[X], \
             (DBL)o->BBox.Lengths[Y], \
             (DBL)o->BBox.Lengths[Z]); \
  Debug_Info("\t},\n"); \
  Debug_Info("\tNULL, // Trans\n"); \
  Debug_Info("\tNULL, // UV_Trans\n"); \
  Debug_Info("\t%f, // Ph_Density\n", (DBL)o->Ph_Density); \
  Debug_Info("\t0x%x, // Flags\n", (unsigned int)o->Flags);

#ifndef DUMP_OBJECT_DATA
#define DUMP_OBJECT_DATA 0
#endif


struct Method_Struct
{
  ALL_INTERSECTIONS_METHOD All_Intersections_Method;
  INSIDE_METHOD Inside_Method;
  NORMAL_METHOD Normal_Method;
  UVCOORD_METHOD UVCoord_Method;
  COPY_METHOD Copy_Method;
  TRANSLATE_METHOD Translate_Method;
  ROTATE_METHOD Rotate_Method;
  SCALE_METHOD Scale_Method;
  TRANSFORM_METHOD Transform_Method;
  INVERT_METHOD Invert_Method;
  DESTROY_METHOD Destroy_Method;
};

/* This is an abstract structure that is never actually used.
   All other objects are descendents of this primative type */

struct Object_Struct
{
  OBJECT_FIELDS
};

struct Compound_Object_Struct
{
  COMPOUND_FIELDS
};


typedef struct BBox_Tree_Struct BBOX_TREE;

struct BBox_Tree_Struct
{
  short Infinite;   /* Flag if node is infinite            */
  short Entries;    /* Number of sub-nodes in this node    */
  BBOX BBox;        /* Bounding box of this node           */
  BBOX_TREE **Node; /* If node: children; if leaf: element */
};

typedef struct Project_Struct PROJECT;
typedef struct Project_Tree_Node_Struct PROJECT_TREE_NODE;

struct Project_Struct
{
  int x1, y1, x2, y2;
};

/*
 * The following structure represent the bounding box hierarchy in 2d space.
 * Because is_leaf, Object and Project are the first elements in both
 * structures they can be accessed without knowing at which structure
 * a pointer is pointing.
 */

struct Project_Tree_Node_Struct
{
  unsigned short is_leaf;
  BBOX_TREE *Node;
  PROJECT Project;
  unsigned short Entries;
  PROJECT_TREE_NODE **Entry;
};

struct Light_Source_Struct
{
  COMPOUND_FIELDS
  COLOUR Colour;
  VECTOR Direction, Center, Points_At, Axis1, Axis2;
  DBL Coeff, Radius, Falloff;
  DBL Fade_Distance, Fade_Power;
  LIGHT_SOURCE *Next_Light_Source;
  unsigned char Light_Type, Area_Light, Jitter;
  bool Orient;
  bool Circular;
  unsigned char Track, Parallel;
  unsigned char Photon_Area_Light; /* these bytes could be compressed to a single flag byte */
  int Area_Size1, Area_Size2;
  int Adaptive_Level;
  int Media_Attenuation;
  int Media_Interaction;
  COLOUR **Light_Grid;
  OBJECT *Shadow_Cached_Object,*Projected_Through_Object;
  BLEND_MAP *blend_map;/* NK for dispersion */
  PROJECT_TREE_NODE *Light_Buffer[6]; /* Light buffers for the six general directions in space. [DB 9/94] */
};


/*****************************************************************************
 *
 * Intersection stack stuff.
 *
 *****************************************************************************/

struct istk_entry
{
   DBL Depth;
   VECTOR IPoint;
   VECTOR INormal;
   VECTOR PNormal; /* perturbed normal vector; -hdf- June 98 */
   UV_VECT Iuv;
   OBJECT *Object;
/*
 *  [DB 8/94]
 *
 * Pass additional values from the intersection function to other functions
 * (normal calculation).
 */
   int i1, i2;
   DBL d1, d2;
   DBL u,v;
   DBL d3,d4;
   DBL w1,w2,w3;
 /* Arbitrary pointer that can be passed. */
   void *Pointer;
   /* pass root-level parent CSG object for cutaway textures */
   void *Csg;
};

struct istack_struct
{
   struct istack_struct *next;
   struct istk_entry *istack;
   unsigned int max_entries;
   unsigned int top_entry;
};

inline istk_entry& itop(istack_struct *i)
{
	return (i->istack[i->top_entry]);
}

void incstack(ISTACK *istk);

/* Macros to push intersection onto stack. */

inline void push_entry(DBL d, VECTOR v, OBJECT *o, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	Assign_Vector(itop(i).IPoint,v);
	Assign_UV_Vect(itop(i).Iuv, v);
	itop(i).Csg = NULL;
	incstack(i);
}

inline void push_normal_entry(DBL d, VECTOR v, VECTOR n, OBJECT *o, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	Assign_Vector(itop(i).IPoint,v);
	Assign_UV_Vect(itop(i).Iuv, v);
	Assign_Vector(itop(i).INormal,n);
	itop(i).Csg = NULL;
	incstack(i);
}

inline void push_uv_entry(DBL d, VECTOR v, UV_VECT uv, OBJECT *o, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	Assign_Vector(itop(i).IPoint,v);
	Assign_UV_Vect(itop(i).Iuv,uv);
	itop(i).Csg = NULL;
	incstack(i);
}

inline void push_normal_uv_entry(DBL d, VECTOR v, VECTOR n, UV_VECT uv, OBJECT *o, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	Assign_Vector(itop(i).IPoint,v);
	Assign_Vector(itop(i).INormal,n);
	Assign_UV_Vect(itop(i).Iuv,uv);
	itop(i).Csg = NULL;
	incstack(i);
}


/* Use these macros to push additional parameters onto the stack. [DB 8/94] */

inline void push_entry_pointer(DBL d, VECTOR v, OBJECT *o, void *a, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	itop(i).Pointer = (void *)(a);
	Assign_Vector(itop(i).IPoint,v);
	Assign_UV_Vect(itop(i).Iuv, v);
	itop(i).Csg = NULL;
	incstack(i);
}

inline void push_entry_pointer_uv(DBL d, VECTOR v, UV_VECT uv, OBJECT *o, void *a, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	itop(i).Pointer = (void *)(a);
	Assign_Vector(itop(i).IPoint,v);
	Assign_UV_Vect(itop(i).Iuv,uv);
	itop(i).Csg = NULL;
	incstack(i);
}

inline void push_entry_i1(DBL d, VECTOR v, OBJECT *o, int a, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	itop(i).i1 = a;
	Assign_Vector(itop(i).IPoint,v);
	itop(i).Csg = NULL;
	incstack(i);
}

inline void push_entry_d1(DBL d, VECTOR v, OBJECT *o, DBL a, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	itop(i).d1 = a;
	Assign_Vector(itop(i).IPoint,v);
	itop(i).Csg = NULL;
	incstack(i);
}

inline void push_entry_i1_i2(DBL d, VECTOR v, OBJECT *o, int a, int b, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	itop(i).i1 = a;
	itop(i).i2 = b;
	Assign_Vector(itop(i).IPoint,v);
	itop(i).Csg = NULL;
	incstack(i);
}

inline void push_entry_i1_d1(DBL d, VECTOR v, OBJECT *o, int a, DBL b, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	itop(i).i1 = a;
	itop(i).d1 = b;
	Assign_Vector(itop(i).IPoint,v);
	itop(i).Csg = NULL;
	incstack(i);
}

inline void push_entry_i1_i2_d1(DBL d, VECTOR v, OBJECT *o, int a, int b, DBL c, istack_struct *i)
{
	itop(i).Depth  = d;
	itop(i).Object = o;
	itop(i).i1 = a;
	itop(i).i2 = b;
	itop(i).d1 = c;
	Assign_Vector(itop(i).IPoint,v);
	itop(i).Csg = NULL;
	incstack(i);
}

inline void push_copy(istack_struct *i, istk_entry *e)
{
	itop(i)= *e;
	incstack(i);
}

inline istk_entry *pop_entry(istack_struct *i)
{
	return (i->top_entry > 0)?&(i->istack[--i->top_entry]):NULL;
}



/*****************************************************************************
 *
 * Ray stuff (see also RAY.H).
 *
 *****************************************************************************/

#define MAX_CONTAINING_OBJECTS 100

#define OPTIMISE_NONE          0
#define OPTIMISE_SHADOW_TEST   1

struct Ray_Struct
{
  VECTOR Initial;
  VECTOR Direction;
  int Index;
  unsigned int Optimisiation_Flags;
  INTERIOR *Interiors[MAX_CONTAINING_OBJECTS];
};


/*****************************************************************************
 *
 * Frame tracking information
 *
 *****************************************************************************/

typedef enum
{
  FT_SINGLE_FRAME,
  FT_MULTIPLE_FRAME
} FRAMETYPE;

#define INT_VALUE_UNSET (-1)
#define DBL_VALUE_UNSET (-1.0)

typedef struct
{
  FRAMETYPE FrameType;
  DBL Clock_Value;      /* May change between frames of an animation */
  int FrameNumber;      /* May change between frames of an animation */

  int InitialFrame;
  DBL InitialClock;

  int FinalFrame;
  int FrameNumWidth;
  DBL FinalClock;

  int SubsetStartFrame;
  DBL SubsetStartPercent;
  int SubsetEndFrame;
  DBL SubsetEndPercent;
  
  bool Field_Render_Flag;
  bool Odd_Field_Flag;
} FRAMESEQ;


/*****************************************************************************
 *
 * Miscellaneous stuff.
 *
 *****************************************************************************/

typedef struct Chunk_Header_Struct CHUNK_HEADER;
typedef struct complex_block complex;
typedef struct file_handle_struct FILE_HANDLE;
typedef struct Reserved_Word_Struct RESERVED_WORD;
typedef int TOKEN;

struct Reserved_Word_Struct
{
  TOKEN Token_Number;
  char *Token_Name;
};

typedef struct Sym_Table_Entry SYM_ENTRY;

struct Sym_Table_Entry 
{
  SYM_ENTRY *next;
  char *Token_Name;
  void *Data;
  TOKEN Token_Number;
};

struct Chunk_Header_Struct
{
  int name;
  int size;
};

struct complex_block
{
  DBL r, c;
};

enum
{
	READ_MODE = 0,
	WRITE_MODE = 1,
	APPEND_MODE = 2
};

// This is a terrible class design, but it fits well with the even worse 3.1 code... [trf]
class Image_File_Class
{
	public:
		Image_File_Class() { valid = false; };
		virtual ~Image_File_Class() { };

		virtual void Write_Line(COLOUR *line_data) = 0;
		virtual int Read_Line(COLOUR *line_data) = 0;

		virtual int Line() = 0;
		virtual int Width() = 0;
		virtual int Height() = 0;

		bool Valid() { return valid; };
	protected:
		bool valid;
};

#ifndef POV_ALLOW_FILE_READ
#define POV_ALLOW_FILE_READ(f,t) (1)
#endif

#ifndef POV_ALLOW_FILE_WRITE
#define POV_ALLOW_FILE_WRITE(f,t) (1)
#endif

END_POV_NAMESPACE

#ifdef USE_SYSPROTO
#include "sysproto.h"
#endif

#endif
