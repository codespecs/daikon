/****************************************************************************
 *                  userio.h
 *
 * This module contains all defines, typedefs, and prototypes for userio.cpp
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
 * $File: //depot/povray/3.6-release/source/userio.h $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/


#ifndef USERIO_H
#define USERIO_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#ifdef __cplusplus
#undef POV_SHELLOUT_CAST
#define POV_SHELLOUT_CAST SHELLRET
#else
#define POV_SHELLOUT_CAST int
#endif

#undef putc

/*****************************************************************************
* Global typedefs
******************************************************************************/

struct StageName_Struct
{
  int stage_id;
  char *stage_name;
};

typedef struct StageName_Struct STAGENAME;

typedef enum shelltype
{
   PRE_SCENE_SHL = 0,
   PRE_FRAME_SHL,
   POST_FRAME_SHL,
   POST_SCENE_SHL,
   USER_ABORT_SHL,
   FATAL_SHL,
   MAX_SHL /* Must be last */
} SHELLTYPE;

typedef enum shellret
{
  IGNORE_RET = 0,
  QUIT_RET,
  USER_RET,
  FATAL_RET,
  SKIP_ONCE_RET,
  ALL_SKIP_RET
} SHELLRET;

typedef struct shelldata
{
   SHELLRET Ret;
   int Inverse;
   char Command[POV_MAX_CMD_LENGTH];
} SHELLDATA;

/*****************************************************************************
* Global variables
******************************************************************************/


/*****************************************************************************
* Global functions
******************************************************************************/

int CDECL Debug_Info(const char *format, ...);
int CDECL Warning(unsigned int level, const char *format,...);
int CDECL WarningAt(unsigned int level, const char *filename, long line, unsigned long offset, const char *format, ...);
int CDECL Error(const char *format,...);
int CDECL PossibleError(const char *format,...);
int CDECL ErrorAt(const char *filename, long line, unsigned long offset, const char *format, ...);

void FlushDebugMessageBuffer();

void init_shellouts (void);
SHELLRET pov_shellout (SHELLTYPE Type);

END_POV_NAMESPACE

#endif /* USERIO_H */
