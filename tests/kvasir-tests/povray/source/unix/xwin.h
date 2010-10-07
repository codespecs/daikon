/****************************************************************************
 *               xwin.h
 *
 * This header file contains all variables and function prototypes required
 * to use the X-Windows specific functions.
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
 * $File: //depot/povray/3.6-release/unix/xwin.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef X_DISPLAY_MISSING

#ifndef POV_UNIX_XWIN_H
#define POV_UNIX_XWIN_H

void XWIN_init_povray (int *argc, char **argv[]);
void XWIN_finish_povray (void);
int  XWIN_display_init (int w, int h);
void XWIN_display_plot (int x, int y,
                        unsigned int Red, unsigned int Green,
                        unsigned int Blue, unsigned int Alpha);
void XWIN_display_plot_rect (int x1, int y1, int x2, int y2,
                             unsigned int Red, unsigned int Green,
                             unsigned int Blue, unsigned int Alpha);
void XWIN_display_plot_box (int x1, int y1, int x2, int y2,
                            unsigned int Red, unsigned int Green,
                            unsigned int Blue, unsigned int Alpha);
void XWIN_display_finished (void);
void XWIN_display_close (void);
int  XWIN_test_abort (void);

#endif  /* POV_UNIX_XWIN_H */

#endif  /* X_DISPLAY_MISSING */
