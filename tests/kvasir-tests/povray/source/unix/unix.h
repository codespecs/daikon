/****************************************************************************
 *               unix.h
 *
 * This header file contains all variables and function prototypes required
 * to use the unix specific functions.
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
 * $File: //depot/povray/3.6-release/unix/unix.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef POV_UNIX_UNIX_H
#define POV_UNIX_UNIX_H


/****************************************************************************
 * Function Prototypes
 ****************************************************************************/

// [NC] String functions; create new string.
char *UNIX_strdup            (const char *str);
char *UNIX_strndup           (const char *str, size_t n);
char *UNIX_stradd            (const char *s1, const char *s2);
char *UNIX_getcwd            (void);
char *UNIX_canonicalize_path (const char *path);
char *UNIX_dirname           (const char *path);

// [NC]
void UNIX_free_globals(void);


#if PRECISION_TIMER_AVAILABLE
void UNIX_timer_start (void);
void UNIX_timer_stop  (void);
int  UNIX_timer_count (void);
#endif  /* PRECISION_TIMER_AVAILABLE */

void UNIX_abort_start   (void);
void UNIX_abort_handler (int signum);

int  UNIX_allow_file_write (const char *Filename, unsigned int FileType);
int  UNIX_allow_file_read  (const char *Filename, unsigned int FileType);
int  UNIX_system           (const char *string);

void UNIX_startup_povray   (void);


/*
 * Generic functions to supplement those defined in userdisp.cpp
 */
void POV_Std_Finish_Povray (void);
int  POV_Std_Test_Abort    (void);


/****************************************************************************
 * Global variables
 ****************************************************************************/

/*
 * Function pointers for the POV_* , XWIN_* and SGVA_* functions.
 */
extern void (*UNIX_finish_povray) (void);

extern int  (*UNIX_display_init) (int w, int h);
extern void (*UNIX_display_plot) (int x, int y,
                                  unsigned int Red, unsigned int Green,
                                  unsigned int Blue, unsigned int Alpha);
extern void (*UNIX_display_plot_rect) (int x1, int y1, int x2, int y2,
                                       unsigned int Red, unsigned int Green,
                                       unsigned int Blue, unsigned int Alpha);
extern void (*UNIX_display_plot_box) (int x1, int y1, int x2, int y2,
                                      unsigned int Red, unsigned int Green,
                                      unsigned int Blue, unsigned int Alpha);
extern void (*UNIX_display_finished) (void);
extern void (*UNIX_display_close) (void);
extern int  (*UNIX_test_abort) (void);


#endif  /* POV_UNIX_UNIX_H */
