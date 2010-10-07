/****************************************************************************
 *               svga.cpp
 *
 * This module implements SVGA specific routines.
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
 * $File: //depot/povray/3.6-release/unix/svga.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/


#ifdef HAVE_CONFIG_H
# include "conf.h"
#else
# error "!!!!! conf.h is required !!!!!"
#endif


#if defined(HAVE_LIBVGA) && defined(HAVE_LIBVGAGL)


#include <ctype.h>
#include <vga.h>
#include <vgagl.h>

#include "frame.h"
#include "pov_util.h"   /* For pov_stricmp */
#include "povray.h"     /* For Stop_Flag */
#include "defaultrenderfrontend.h"
#include "svga.h"
#include "userio.h"     /* for POV_Std functions */
#include "userdisp.h"


USING_POV_NAMESPACE
USING_POV_BASE_NAMESPACE
USING_POV_FRONTEND_NAMESPACE

/*****************************************************************************
 * Local preprocessor defines
 *****************************************************************************/

#ifndef theRED
# define theRED   0
#endif

#ifndef theGREEN
# define theGREEN 1
#endif

#ifndef theBLUE
# define theBLUE  2
#endif

#define SVGA_DEBUG 1


/*****************************************************************************
* Local typedefs
******************************************************************************/


/*****************************************************************************
* Local variables
******************************************************************************/

static int *theErrors[3];  /* error diffusion storage arrays */
static int scale, xoff, yoff;
static int couldwrite = false;
static GraphicsContext physicalScreen, internalScreen;


/*****************************************************************************
* Static functions
******************************************************************************/



/*****************************************************************************
*
* FUNCTION  SVGA_init_povray
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR   jepler@inetnebr.com <Jeff Epler>
*
* DESCRIPTION
*
*   Starts SVGA povray, and grabs access to the actual video memory.  This
*   is why we need to make povray suid root.
*
* CHANGES
*
******************************************************************************/

void SVGA_init_povray (void)
{
  short i;

  for (i = 0; i < 3; i++)
    theErrors[i] = NULL;

  seteuid(0);
  vga_init();
  seteuid(getuid());
  vga_runinbackground(1);
}


/*****************************************************************************
*
* FUNCTION  SVGA_display_init
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
*   Initializes the SVGA display using the best graphics mode possible.
*
* CHANGES
*
*   Fixed video mode selection, added grayscale palette.  July 22, 1996 [AED]
*
******************************************************************************/

#define MODE(a, b, c) G ## a ## x ## b ## x ## c
#define TRY_MODE(x, y)\
  if (w <= x && h <= y) {\
    int trymode;\
    if (opts.PaletteOption == 'T') trymode = MODE(x, y, 16M);\
    else if (opts.PaletteOption == 'H') trymode = MODE(x, y, 32K);\
    else trymode = MODE(x, y, 256);\
    if (vga_hasmode(trymode))\
    {\
      mode = trymode;\
      break;\
    }\
  }

int SVGA_display_init(int width, int height)
{
  int i, w, h;
  int mode = TEXT;

  /* Should we do something with vga_getdefaultmode() here first? */

TRY_MODES:
  scale = 1;
  w = width;
  h = height;

  while (mode == TEXT && scale <= 8) {
#if (SVGA_DEBUG > 0)
    Debug_Info(
      "Trying palette mode %c at scale %d\n",
      opts.PaletteOption, scale
    );
#endif
    TRY_MODE(320, 200);
    TRY_MODE(640, 480);
    TRY_MODE(800, 600);
    TRY_MODE(1024, 768);
    TRY_MODE(1280, 1024);
    scale++;
    w = width / scale;
    h = height / scale;
  }

  if (mode == TEXT) {
    if (opts.PaletteOption == 'T') {
      Debug_Info(
        "\nUnable to find true-color SVGA modes.  Trying high-color modes."
      );
      opts.PaletteOption = 'H';
      goto TRY_MODES;
    }
    else if (opts.PaletteOption == 'H') {
      Debug_Info(
        "\nUnable to find high-color SVGA modes.  Trying paletted modes."
      );
      opts.PaletteOption = '0';
      goto TRY_MODES;
    }
    else {
      Debug_Info("\nUnable to find any useful SVGA modes.  No display.");
      opts.Options &= ~DISPLAY;
      return (false);
    }
  }

#if (SVGA_DEBUG > 0)
  Debug_Info(
    "Using palette mode %c at %dx%d (scale = %d)\n",
    opts.PaletteOption, w, h, scale
  );
#endif

  if (scale > 1)
    Debug_Info("Scaling by %d:1 on-screen\n", scale);

  vga_setmode(mode);
  gl_setcontextvga(mode);
  if (opts.PaletteOption == GREY)
  {
    for (i = 0; i < 256; i++)
    {
      int gray;
      gray = (i + 2) / 4;

      gl_setpalettecolor(i, gray, gray, gray);
    }
  }
  else
  {
    gl_setrgbpalette();
  }
  gl_getcontext(&physicalScreen);
  gl_clearscreen(0);

  xoff = (WIDTH - w)/2;
  yoff = (HEIGHT - h)/2;

  SVGA_display_plot_box(0, 0, width, height, 255, 255, 255, 0);

  gl_setcontextvgavirtual(mode);
  gl_getcontext(&internalScreen);
  gl_clearscreen(0);
  SVGA_display_plot_box(0, 0, width, height, 255, 255, 255, 0);

  /* Allocate space for dithering */
  for (i = 0; i < 3; i++)
  {
    theErrors[i] = (int *)POV_CALLOC(width, sizeof(int), "dither array");
  }
  return (true);
}


/*****************************************************************************
*
* FUNCTION  SVGA_display_close
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

void SVGA_display_close (void)
{
  int i;

  vga_setmode(TEXT);

  for (i = 0; i < 3; i++)
  {
    if (theErrors[i] != NULL)
    {
      POV_FREE(theErrors[i]);
      theErrors[i] = NULL;
    }
  }
}


/*****************************************************************************
*
* FUNCTION  SVGA_display_plot
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
* CHANGES
*
*   Mar 7, 1996:  Added dithering code from X Windows [AED]
*   July 22, 1996: Added grayscale display code [AED]
*
******************************************************************************/

void SVGA_display_plot(int x, int y,
                              unsigned int Red, unsigned int Green,
                              unsigned int Blue, unsigned int Alpha)
{
  int want[3], got[3];
  int i;
  int sx = x / scale + xoff,
      sy = y / scale + yoff;

#if (SVGA_DEBUG > 1)
  Debug_Info(
    "Orig = <%03d, %03d, %03d>  Error = <%03d, %03d, %03d>\n",
    Red, Green, Blue,
    theErrors[theRED][x], theErrors[theGREEN][x], theErrors[theBLUE][x]
  );
#endif

  want[theRED]   = (int)Red   + theErrors[theRED][x];
  want[theGREEN] = (int)Green + theErrors[theGREEN][x];
  want[theBLUE]  = (int)Blue  + theErrors[theBLUE][x];

  /* clamp colors to the valid range */
  for (i = 0; i < 3; i++)
  {
    want[i] = (want[i] < 0) ? 0 : ((want[i] > 255) ? 255 : want[i]);
  }

  /* Output the pixel, then find what we actually got.  This could be
   * more efficient, but then we'd have to have special cases for all
   * of the color depths, like in the X Windows code.
   */
  if (opts.PaletteOption == GREY)
  {
    gl_setpixel(sx,sy,want[theRED]);
    /* I should be able to use getpixelrgb to do this, but it doesn't work */
    got[theRED] = got[theGREEN] = got[theBLUE] = (want[theRED] / 4) * 4;
  }
  else
  {
    gl_setpixelrgb(sx, sy, want[theRED], want[theGREEN], want[theBLUE]);
    gl_getpixelrgb(sx, sy, &got[theRED], &got[theGREEN], &got[theBLUE]);
  }

#if (SVGA_DEBUG > 1)
  Debug_Info(
    "  Want <%03d, %03d, %03d>  got <%03d, %03d, %03d>\n",
    want[theRED], want[theGREEN], want[theBLUE],
    got[theRED], got[theGREEN], got[theBLUE]
  );
#endif

  vga_lockvc();    /* Will this solve all our problems? */
  if ((i = vga_oktowrite()))
  {
    if (!couldwrite)
      gl_copyscreen(&physicalScreen);

    gl_setcontext(&physicalScreen);
    if (opts.PaletteOption == GREY)
    {
      gl_setpixel(sx,sy,want[theRED]);
    }
    else
    {
      gl_setpixelrgb(sx, sy, want[theRED], want[theGREEN], want[theBLUE]);
    }
    gl_setcontext(&internalScreen);
  }
  vga_unlockvc();
  couldwrite = i;

  /*
   * Compute errors and accumulate in error diffusion array.  As well,
   * since we are keeping all the errors in a single array, things are
   * a little different than usual.  The error array from x = 0 to the
   * current x value are for the next scanline, and the errors to the
   * right are for the current scanline.  This means we have to keep
   * the error for the lower right pixel in a separate location until
   * we use the error from the pixel to the right.
   */

  for (i = 0; i < 3; i++)
  {
    static int drError[3] = {0, 0, 0};
    int error;

    /* find the error between the desired color and actually used color */

    error = want[i] - got[i];

    if (x > 0)
    {
      theErrors[i][x - 1] += (3 * error + 8) / 16;        /* down & left */
    }

    theErrors[i][x] = (5 * error + 8) / 16 + drError[i];  /* down */

    if (sx < Frame.Screen_Width - 1)
    {
      drError[i] = (error + 8)/ 16;                       /* down & right */
      theErrors[i][x + 1] += (7 * error + 8) / 16;        /* right */
    }
    else
    {
      drError[i] = 0;         /* next scanline start with no error */
    }
  }
}


/*****************************************************************************
*
* FUNCTION  SVGA_display_plot_rect
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR   Andreas Dilger
*
* DESCRIPTION
*
*   Draw a block of pixels in the given color.  We call SVGA_display_plot()
*   repeatedly so that we can take advantage of dithering over the whole
*   block.
*
* CHANGES
*
*   Fixed display loop to be horizontal for dithering.  Jul, 1996. [AED]
*   Modified order of parameters for 3.1.  March, 1999 [mtg]
*
******************************************************************************/

void SVGA_display_plot_rect(int x1, int y1, int x2, int y2,
                            unsigned int Red, unsigned int Green,
                            unsigned int Blue, unsigned int Alpha)
{
  int i;
  int xx, yy;
  int height = y2 - y1 + 1;

  for (yy = y1; yy <= y2; yy += scale)
    for (xx = x1; xx <= x2; xx += scale)
      SVGA_display_plot(xx, yy, Red, Green, Blue, Alpha);

  if (x2 < Frame.Screen_Width - 1)
  {
    for (i = 0; i < 3; i++)
    {
      theErrors[i][x2 + 1] /= height;
    }
  }

  if (x1 > 0)
  {
    for (i = 0; i < 3; i++)
    {
      theErrors[i][x1 - 1] /= height;
    }
  }
}


/*****************************************************************************
*
* FUNCTION  SVGA_display_plot_box
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    adilger@enel.ucalgary.ca (Andreas Dilger)
*
* DESCRIPTION
*
*   Display the outline of a box in the given color
*
* CHANGES
*
*   Mar 1996: Creation
*
******************************************************************************/

void SVGA_display_plot_box(int x1, int y1, int x2, int y2,
                           unsigned int Red, unsigned int Green,
                           unsigned int Blue, unsigned int Alpha)
{
  int i, c;
  int sx, sy, ex, ey;

  sx = x1 / scale + xoff;
  ex = (x2 - 1)/ scale + xoff;
  sy = y1/ scale + yoff;
  ey = (y2 - 1)/ scale + yoff;

  if (opts.PaletteOption == GREY)
  {
    c = 255;
  }
  else
  {
    c = gl_rgbcolor(Red, Green, Blue);
  }

  gl_line(sx, sy, ex, sy, c);
  gl_line(sx, ey, ex, ey, c);
  gl_line(sx, sy, sx, ey, c);
  gl_line(ex, sy, ex, ey, c);

  vga_lockvc();   /* Will this solve all our problems? */
  if ((i = vga_oktowrite()))
  {
    if (!couldwrite)
      gl_copyscreen(&physicalScreen);

    gl_setcontext(&physicalScreen);
    gl_line(sx, sy, ex, sy, c);
    gl_line(sx, ey, ex, ey, c);
    gl_line(sx, sy, sx, ey, c);
    gl_line(ex, sy, ex, ey, c);
    gl_setcontext(&internalScreen);
  }
  vga_unlockvc();
  couldwrite = i;
}


/*****************************************************************************
*
* FUNCTION  SVGA_display_finished
*
* INPUT     status
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Mark Gordon <mtgordon@povray.org>
*
* DESCRIPTION
*
*   Just a dummy function to satisfy the function pointer.
*
* CHANGES
*
*   Brand-new, June 2000. [mtg]
*
******************************************************************************/

void SVGA_display_finished(void)
{
  return;
}


/*****************************************************************************
*
* FUNCTION  SVGA_finish_povray
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Mark Gordon <mtgordon@povray.org>
*
* DESCRIPTION
*
*   Just a dummy function to satisfy the function pointer.
*
* CHANGES
*
*   Brand-new, June 2000. [mtg]
*
******************************************************************************/

void SVGA_finish_povray(void)
{
  return;
}


/*****************************************************************************
*
* FUNCTION  SVGA_test_abort
*
* INPUT
*
* OUTPUT
*
* RETURNS   a number to add to Stop_Flag
*
* AUTHOR    Mark Gordon <mtgordon@povray.org>
*
* DESCRIPTION
*
*   If user wants to quit, this returns TRUE, which gets added to Stop_Flag.
*
* CHANGES
*
*   Brand-new, June 2000. [mtg]
*
******************************************************************************/

int SVGA_test_abort(void)
{
  return ((opts.Options&EXITENABLE && toupper(vga_getkey())=='Q') ? true : false);
}

#endif  /* HAVE_LIBVGA && HAVE_LIBVGAGL */
