/****************************************************************************
 *                  pov_mem.h
 *
 * This module contains all defines, typedefs, and prototypes for pov_mem.cpp
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
 * $File: //depot/povray/3.6-release/source/pov_mem.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef POV_MEM_H
#define POV_MEM_H

BEGIN_POV_NAMESPACE


/*****************************************************************************
* Global preprocessor defines
******************************************************************************/



/*****************************************************************************
* Global typedefs
******************************************************************************/



/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

void mem_init (void);
void mem_mark (void);
void mem_release (void);
void mem_release_all (void);
void *pov_malloc (size_t size, const char *file, int line, const char *msg);
void *pov_calloc (size_t nitems, size_t size, const char *file, int line, const char *msg);
void *pov_realloc (void *ptr, size_t size, const char *file, int line, const char *msg);
void pov_free (void *ptr, const char *file, int line);
char *pov_strdup (const char *s);
void *pov_memmove (void *dest, void *src, size_t length);

#if defined(MEM_STATS)
/* These are level 1 routines */
size_t mem_stats_current_mem_usage (void);
size_t mem_stats_largest_mem_usage (void);
size_t mem_stats_smallest_alloc (void);
size_t mem_stats_largest_alloc (void);
/* These are level 2 routines */
#if (MEM_STATS>=2)
const char* mem_stats_smallest_file (void);
int mem_stats_smallest_line (void);
const char* mem_stats_largest_file (void);
int mem_stats_largest_line (void);
long int mem_stats_total_allocs (void);
long int mem_stats_total_frees (void);
#endif
#endif

END_POV_NAMESPACE

#endif /* MEM_H */
