/****************************************************************************
 *               unix.cpp
 *
 * This module implements UNIX specific routines.
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
 * $File: //depot/povray/3.6-release/unix/unix.cpp $
 * $Revision: 1.3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: rudd $
 * $Log: unix.cpp,v $
 * Revision 1.3  2009-12-16 15:32:37  rudd
 * Modified version of the povray test. I took the relatively crude solution of removing all it's ini-file searching/parsing, which I think was the primary cause of environmental differences. I'll see how the nightlies go tonight
 *
 * Revision 1.2  2009-10-22 16:18:18  rudd
 * changed to povray test: Removed a random number call. Modified the ini file to only reference relative to the current directory. Modified references in source to the home directory. Hopefully this'll get it passing on the nightlies
 *
 * Revision 1.1  2009-10-08 19:46:01  rudd
 * Adding povray to nightly test. I tried to strip out the useless parts by it still clocks in at about 15 MB.
 *
 *****************************************************************************/

/*
 * Various modifications from Mike Fleetwood, February 1999
 * In a nutshell, they allow for enhanced (and fixed) default INI support.
 * They include two new functions, UNIX_Process_Env and unix_process_povray_ini
 * and associated modifications as noted below
 */

/*
 * Multiple binaries differing only in their display glued together by
 * Mark Gordon, June 2000.
 * [NC] Mark also added the primary code for handling I/O restrictions.
 */
 
/*
 * Rewrite for 3.6, August 2003 - May 2004.
 * Basically, the whole I/O restriction code has been revisited to avoid
 * use of static buffers (and thus possible buffer overruns) and to allow
 * resolving all symbolic links.  Also changed the syntax/format of the
 * povray.conf file.
 *
 * Nicolas Calimet <pov4grasp@free.fr> [NC]
 * - Code and layout cleanup: remove deprecated stuff and improve consistency.
 *   The idea is that all UNIX_* functions are global, while all the others
 *   unix_* functions are local.
 * - moved X Window code to xwin.cpp
 * - moved SVGAlib code to svga.cpp
 * - rewrote most functions that deal with configuration/ini files.
 * - added several functions such a string manipulation.
 *
 * Christoph Hormann <chris_hormann@gmx.de> [C.H.]
 * - changing some of the text output functions.
 * - added benchmark mode.
 */


#include <ctype.h>      // for isspace()
#include <signal.h>
#include <sys/time.h>   // for gettimeofday() and time()

#ifdef UNIX_DEBUG
# include <assert.h>
#endif

#ifdef HAVE_CONFIG_H
# include "conf.h"
#else
# error "!!!!! conf.h is required !!!!!"
#endif

// source/frontend
#include "defaultrenderfrontend.h"
#include "processrenderoptions.h"
#include "defaultplatformbase.h"

// source/base
#include "stringutilities.h"  // for pov_stricmp(), pov_tsprintf() 

// source
#include "benchmark.h"
#include "povray.h"
#include "povmsgid.h"   // for kPOVObjectClass_ROptions
#include "povmsrec.h"   // for Receive_RenderOptions()
#include "userdisp.h"   // for POV_Std functions

// unix
#include "unix.h"

#ifndef X_DISPLAY_MISSING
# include "xwin.h"
#endif

#if defined(HAVE_LIBVGA) && defined(HAVE_LIBVGAGL)
# include "svga.h"
#endif


USING_POV_NAMESPACE
USING_POV_BASE_NAMESPACE
USING_POV_FRONTEND_NAMESPACE

#define X_DISPLAY_MISSING 1

/*****************************************************************************
 * Local preprocessor defines
 *****************************************************************************/

#ifndef TRUE
# define TRUE 1
#endif

#ifndef FALSE
# define FALSE 0
#endif

/*
 * [NC]
 * Default values for the location of the povray library and configuration.
 * These constants don't have to be in config.h .
 */
#ifndef POVLIBDIR
# define POVLIBDIR  "/usr/local/share/" PACKAGE "-" VERSION_BASE
#endif

#ifndef POVCONFDIR
# define POVCONFDIR  "/usr/local/etc/" PACKAGE "/" VERSION_BASE
#endif

#ifndef POVCONFDIR_BACKWARD
# define POVCONFDIR_BACKWARD  "/usr/local/etc"
#endif


/*****************************************************************************
* Local typedefs
******************************************************************************/

/*
 * [NC] structures to handle configuration-related (povray.conf) code.
 */
typedef struct Path     Path;
typedef struct PathList PathList;
typedef struct Config   Config;
typedef enum {IO_UNSET, IO_NONE, IO_READONLY, IO_RESTRICTED, IO_UNKNOWN} FileIO;
typedef enum {SHL_UNSET, SHL_ALLOWED, SHL_FORBIDDEN, SHL_UNKNOWN} ShellOut;

struct Path
{
  char *str;
  bool  descend, writable;
  Path *next;
};

struct PathList
{
  Path *first;
  Path *last;
};

struct Config
{
  char     *home;
  char     *sysconf;                // system conf filename
  char     *userconf;               // user conf filename
  char     *conf;                   // selected conf file
  char     *sysini,  *sysini_old;   // system ini filename
  char     *userini, *userini_old;  // user ini filename
  FileIO    file_io;
  ShellOut  shellout;
  PathList *permitted_paths;
};


/*****************************************************************************
* Local variables
******************************************************************************/

/*
 * These are used when determining which (if any) display functions are
 * to be used.
 */
static bool is_using_xwin;
static bool is_using_svga;

/*
 * Store all configuration-related data.
 */
static Config *config;
static bool no_error_call;  // workaround for early calls of Error()

/*
 * Variables used by the histogram timers
 */
#if PRECISION_TIMER_AVAILABLE
static struct timeval hstart, hstop;
#endif

/*
 * Function pointers for the POV_* , XWIN_* and SGVA_* functions.
 */
void (*UNIX_finish_povray) (void);

int  (*UNIX_display_init) (int w, int h);
void (*UNIX_display_plot) (int x, int y,
                           unsigned int Red, unsigned int Green,
                           unsigned int Blue, unsigned int Alpha);
void (*UNIX_display_plot_rect) (int x1, int y1, int x2, int y2,
                                unsigned int Red, unsigned int Green,
                                unsigned int Blue, unsigned int Alpha);
void (*UNIX_display_plot_box) (int x1, int y1, int x2, int y2,
                               unsigned int Red, unsigned int Green,
                               unsigned int Blue, unsigned int Alpha);
void (*UNIX_display_finished) (void);
void (*UNIX_display_close) (void);
int  (*UNIX_test_abort) (void);


/*****************************************************************************
* Global variables
******************************************************************************/

/*
 * For frontend handling.
 */
DefaultRenderFrontend *globalDefaultRenderFrontendPointer = NULL;


/*
 * For POVMS handling.
 */
jmp_buf globalSetJmpEnv;

extern POVMSContext POVMS_Output_Context;
extern POVMSContext POVMS_Render_Context;
extern bool         Binary_POVMS_Stream_Mode;


/*****************************************************************************
* Static functions
******************************************************************************/

// [NC]
static void unix_clear_paths (PathList *list);
#ifdef HAVE_READLINK
static char *unix_readlink (const char *path);
#endif
static char *unix_try_temp_file (const char *Filename);

/*
 * Whether we are using the X Window System or SVGAlib.
 */
static bool unix_xwin_mode (int argc, char *argv[]);
static bool unix_svga_mode (void);


/*****************************************************************************
*
* FUNCTION  unix_create_globals
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*    Allocate all global variables that need their own memory.
*
* CHANGES
*
*    New for 3.6, September 2003 [NC]
*
*****************************************************************************/

static void unix_create_globals(void)
{
  // allocate memory
  config = (Config *) POV_CALLOC(1, sizeof(Config), "Configuration");
  config->permitted_paths =
    (PathList *) POV_CALLOC(1, sizeof(PathList), "Paths");

  // home directory
  config->home = "./";

  // Default values for I/O restrictions: everything is allowed.
  // Any restrictions must come from system or user configuration.
  config->file_io  = IO_UNSET;
  config->shellout = SHL_UNSET;

  // system configuration file
  config->conf    = NULL;
  config->sysconf = UNIX_strdup(POVCONFDIR "/povray.conf");

  // user configuration file
  config->userconf = config->home
    ? UNIX_stradd(config->home, "/povray.conf")
    : NULL
    ;

  // system ini file
  config->sysini     = UNIX_strdup(POVCONFDIR "/povray.ini");
  config->sysini_old = UNIX_strdup(POVCONFDIR_BACKWARD "/povray.ini");

  // user ini file
  config->userini = config->home
    ? UNIX_stradd(config->home, "/povray.ini")
    : NULL
    ;
  config->userini_old = config->home
    ? UNIX_stradd(config->home, "/.povrayrc")
    : NULL
    ;

#ifdef UNIX_DEBUG
  fprintf(stderr,
   "PATHS\n"
   "  HOME        = %s\n"
   "  SYSCONF     = %s\n"
   "  USERCONF    = %s\n"
   "  SYSINI      = %s\n"
   "  SYSINI_OLD  = %s\n"
   "  USERINI     = %s\n"
   "  USERINI_OLD = %s\n",
    config->home,
    config->sysconf,
    config->userconf,
    config->sysini, config->sysini_old,
    config->userini, config->userini_old
  );
#endif
}


/*****************************************************************************
*
* FUNCTION  UNIX_free_globals
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*    Free all global variables that have been allocated.
*
* CHANGES
*
*    New for 3.6, September 2003 [NC]
*
*****************************************************************************/

void UNIX_free_globals(void)
{
  if(config)
  {
    if(config->sysconf)
      POV_FREE(config->sysconf);

    if(config->userconf)
      POV_FREE(config->userconf);

    if(config->sysini)
      POV_FREE(config->sysini);
    if(config->sysini_old)
      POV_FREE(config->sysini_old);

    if(config->userini)
      POV_FREE(config->userini);
    if(config->userini_old)
      POV_FREE(config->userini_old);

    if(config->permitted_paths)
    {
      unix_clear_paths(config->permitted_paths);
      POV_FREE(config->permitted_paths);
    }

    POV_FREE(config);
  }
}


/*****************************************************************************
*
* FUNCTION  UNIX_strdup
*
* INPUT     string of nul-terminated caracters
*
* OUTPUT
*
* RETURNS   a new string with the same content
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*    Duplicate the content of a string and create a new one.
*
* CHANGES
*
*    New for 3.6, September 2003 [NC]
*
*****************************************************************************/

char *UNIX_strdup(const char *str)
{
  char   *s;
  size_t  len;

  if(! str)
    return (char *) POV_CALLOC(1, 1, "UNIX_strdup");

  len = strlen(str) + 1;
  s = (char *) POV_CALLOC(len, 1, "UNIX_strdup");

  return (char *) memcpy(s, str, len);
}


/*****************************************************************************
*
* FUNCTION  UNIX_strndup
*
* INPUT     string of nul-terminated caracters
*
* OUTPUT
*
* RETURNS   a new string with up to n caracters
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   Duplicate the first n caracters of a string and create a new one.
*
* CHANGES
*
*   New for 3.6, September 2003 [NC]
*
*****************************************************************************/

char *UNIX_strndup(const char *str, size_t n)
{
  char   *s;
  size_t  len;

  if(! str  ||  ! n)
    return (char *) POV_CALLOC(1, 1, "UNIX_strndup");

  len = strlen(str);
  if(len > n)
    len = n;
  s = (char *) POV_MALLOC(len + 1, "UNIX_strndup");
  memcpy(s, str, len);
  s[len] = '\0';

  return s;
}


/*****************************************************************************
*
* FUNCTION  UNIX_stradd
*
* INPUT     two strings of nul-terminated caracters
*
* OUTPUT
*
* RETURNS   a new concatenated string
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   Add the content of (concatenate) two strings and create a new one.
*
* CHANGES
*
*   New for 3.6, September 2003 [NC]
*
*****************************************************************************/

char *UNIX_stradd(const char *s1, const char *s2)
{
  char   *s;
  size_t  s1len, s2len;

  if(! s2)
    return UNIX_strdup(s1);

  s1len = (s1) ? strlen(s1) : 0;
  s2len = strlen(s2);
  s = (char *) POV_MALLOC(s1len + s2len + 1, "UNIX_stradd");

  memcpy(s, s1, s1len);
  memcpy(s + s1len, s2, s2len);
  s[s1len + s2len] = '\0';

  return s;
}


/*****************************************************************************
*
* FUNCTION  UNIX_getcwd
*
* INPUT     string containing a path or filename
*
* OUTPUT
*
* RETURNS   a new string
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   Get the current working directory (including final slash)
*   and create a new string.
*
* CHANGES
*
*   New for 3.6, September 2003 [NC]
*
*****************************************************************************/

char *UNIX_getcwd(void)
{
  char *s, *tmp, *errormsg;

#ifdef HAVE_GETCWD
  size_t len;

  len = 256;  // default buffer size
  tmp = (char *) POV_CALLOC(len, 1, "UNIX_getcwd");

  while(getcwd(tmp, len) == NULL)  // buffer is too small
  {
    POV_FREE(tmp);
    len *= 2;  // double buffer size and try again
    tmp = (char *) POV_CALLOC(len, 1, "UNIX_getcwd");
  }
#else
  tmp = getenv("PWD");  // must not be NULL; checked by configure
  if(! tmp)             // run-time checks are safer anyway
  {
    errormsg =
      "Cannot determine the current working directory.\n"
      "Check that the PWD environment variable does exist and is valid.\n";
    if(no_error_call)
    {
      fprintf(stderr, "%s: %s\n", PACKAGE, errormsg);
      exit(EXIT_FAILURE);
    }
    else
      Error("%s", errormsg);
  }
#endif

  s = UNIX_stradd(tmp, "/");  // add final slash

#ifdef HAVE_GETCWD
  POV_FREE(tmp);
#endif

  return s;
}


/*****************************************************************************
*
* FUNCTION  unix_basename
*
* INPUT     path
*
* OUTPUT
*
* RETURNS   final filename component of the path
*           creates a new string [NC]
*
* AUTHOR    Christopher James Huff
*
* DESCRIPTION
*
*   Implementation of basename, roughly patterned after SUSv2
*
* CHANGES
*
*   Rewrite for 3.6, September 2003 [NC]
*   - input path is now constant, returns a new string.
*   - the original implementation was incomplete and had side-effects.
*
******************************************************************************/

static char *unix_basename(const char *path)
{
  int first = 0;  // index of the first caracter to keep
  int last = 0;   // index of the last caracter to keep

  if(! path)
    return UNIX_strdup("");

  first = strlen(path);
  if(first < 2)  // less than two caracters
    return UNIX_strdup(path);

  if(--first  &&  path[first] == '/')  // skip last slash
    first--;

  last = first;  // store current position
  while(first > 0  &&  path[first] != '/')  // scan the string backwards
    first--;

  if(first)
    return UNIX_strndup(path+first+1, last-first);
  else
    return UNIX_strndup(path, last+1);
}


/*****************************************************************************
*
* FUNCTION  unix_dirname
*
* INPUT     path
*
* OUTPUT
*
* RETURNS   directory component of the path
*
* AUTHOR    Christopher James Huff
*
* DESCRIPTION
*
*   Implementation of dirname, roughly patterned after SUSv2
*
* CHANGES
*
*   Rewrite for 3.6, September 2003 [NC]
*   - input path is now constant, returns a new string.
*   - the original implementation was incomplete and had side-effects.
*
******************************************************************************/

static char *unix_dirname(const char *path)
{
  int last;

  if(! path)
    return UNIX_strdup("");

  last = strlen(path);
  if(last < 2)  // less than 2 caracters
    return UNIX_strdup(path);

  if(--last  &&  path[last] == '/')  // skip last slash
    last--;

  while(last > 0  &&  path[last] != '/')  // scan the string backwards
    last--;

  return UNIX_strndup(path, last);  // last slash is skipped
}


/*****************************************************************************
*
* FUNCTION  unix_readlink
*
* INPUT     string containing a path or filename
*
* OUTPUT
*
* RETURNS   a new string
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   Get the path for a symbolic link and create a new string.
*   A symlink is translated to its target only if the symlink is the last
*   component of the path (i.e. the basename).
*   Note: don't use the unsecure unix realpath() function.
*
* CHANGES
*
*   New for 3.6, October 2003 [NC]
*
*****************************************************************************/

#ifdef HAVE_READLINK

static char *unix_readlink(const char *path)
{
  char   *s, *tmp1, *tmp2, *tmp3;
  size_t  len;
  int     status;

  len = 256;  // default buffer size
  tmp1 = (char *) POV_CALLOC(len, 1, "unix_readlink");  // init with '\0'

  while(true)
  {
    status = readlink(path, tmp1, len-1);  // without terminating '\0'
    if(status < 0)  // an error occured, return empty string
    {
      tmp1[0] = '\0';  // for safety
      return tmp1;
    }
    else if(status == len-1)  // the buffer is probably too small
    {
      POV_FREE(tmp1);
      len *= 2;  // double buffer size and try again
      tmp1 = (char *) POV_CALLOC(len, 1, "unix_readlink");
    }
    else  // all right, let's go further
      break;
  }

  // do we have an absolute path ?
  if(tmp1[0] != '/')  // no; concatenate symlink to the path dirname
  {
    tmp2 = unix_dirname(path);
    tmp3 = UNIX_stradd(tmp2, "/");
    s = UNIX_stradd(tmp3, tmp1);
    POV_FREE(tmp1);
    POV_FREE(tmp2);
    POV_FREE(tmp3);
  }
  else  // yes; just resize buffer
  {
    s = UNIX_strdup(tmp1);
    POV_FREE(tmp1);
  }

  return s;
}

#endif  /* HAVE_READLINK */


/*****************************************************************************
*
* FUNCTION  UNIX_canonicalize_path
*
* INPUT     string containing a path or filename
*
* OUTPUT
*
* RETURNS   a new string [NC]
*
* AUTHOR    Mark Gordon <mtgordon@povray.org> July 2002
*
* DESCRIPTION
*
*   Replaces instances of . and .. in the input string.
*   Note: this is a replacement for the unsecure unix realpath() funtion [NC]
*
* CHANGES
*
*   New for 3.5
*
*   Rewrite for 3.6, September 2003 and May 2004 [NC]
*     Use dynamic strings only, in order to avoid static buffers, side effects,
*     and recursivity.  In particular, it should fix many potential buffer
*     overrun vulnerabilities (the original code was not using the strncpy and
*     strncat functions properly).  Without any side effects, this new code
*     should be really portable (provided the path seperator is '/').
*     Also added full support for symbolic links.
*
******************************************************************************/

char *UNIX_canonicalize_path(const char *path)
{
  char *s, *tmp1, *tmp2, *tmp3;
  char *match_begin, *prev_dir;
  int   i;
  typedef struct { const char *match, *replace; } subst;
  const subst strings[] = {  // beware: order does matter
    { "%INSTALLDIR%", POVLIBDIR },
    { "%HOME%", config->home },
    { "//", "/" },
    { "/./", "/" },
    { NULL, NULL }  // sentinel
  };

  // nothing to canonicalize; return an empty string
  if(! path)
    return (char *) POV_CALLOC(1, 1, "UNIX_canonicalize_path");

#ifdef UNIX_DEBUG
  fprintf(stderr, "CANONICALIZE '%s'\n", path);
#endif

  // create a copy first
  s = UNIX_strdup(path);

  // substitute all occurences of 'match' by 'replace'
  i = 0;
  while(strings[i].match)
  {
    while((match_begin = strstr(s, strings[i].match)) != NULL)
    {
      tmp1 = UNIX_strndup(s, match_begin - s);
      tmp2 = UNIX_stradd(tmp1, strings[i].replace);
      tmp3 = UNIX_stradd(tmp2, match_begin + strlen(strings[i].match));
      POV_FREE(tmp1);
      POV_FREE(tmp2);
      POV_FREE(s);
      s = tmp3;
#ifdef UNIX_DEBUG
      fprintf(stderr, "  %s\n", s);
#endif
    }
    ++i;
  }

  // substitute the current working dir to the first "./"
  // or add the cwd to the first directory or "../"
  if(! strncmp(s, "./", 2))
  {
    tmp1 = UNIX_getcwd();
    tmp2 = UNIX_stradd(tmp1, s + strlen("./"));
    POV_FREE(tmp1);
    POV_FREE(s);
    s = tmp2;
#ifdef UNIX_DEBUG
    fprintf(stderr, "  %s\n", s);
#endif
  }
  else if(s[0] != '/'  ||  ! strncmp(s, "../", 3))
  {
    tmp1 = UNIX_getcwd();
    tmp2 = UNIX_stradd(tmp1, s);
    POV_FREE(tmp1);
    POV_FREE(s);
    s = tmp2;
#ifdef UNIX_DEBUG
    fprintf(stderr, "  %s\n", s);
#endif
  }

  // iteratively translate all symlinks in the path (dirname and basename)
#ifdef HAVE_READLINK
  i = 0;
  if(s[i] == '/')
    i++;
  do
  {
    while(s[i] != '\0'  &&  s[i] != '/')
      i++;
    tmp1 = UNIX_strndup(s, i);
    tmp2 = UNIX_strdup(s + i);
    tmp3 = unix_readlink(tmp1);
    if(strlen(tmp3))  // found symlink
    {
#ifdef UNIX_DEBUG
      fprintf(stderr, "  %s -> %s\n  %s%s\n", tmp1, tmp3, tmp3, tmp2);
#endif
      POV_FREE(s);
      s = UNIX_stradd(tmp3, tmp2);
      i = 0;  // start again from beginning
      if(s[i] == '/')
        i++;
    }
    else
    {
#ifdef UNIX_DEBUG
      fprintf(stderr, "  %s\n", tmp1);
#endif
      i++;
    }
    POV_FREE(tmp1);
    POV_FREE(tmp2);
    POV_FREE(tmp3);
  } while(s[i]);
#endif  // HAVE_READLINK

  // substitute all occurences of "dir/.." by ""
  while((match_begin = strstr(s, "/..")) != NULL)
  {
    tmp1 = UNIX_strndup(s, match_begin - s);
    prev_dir = strrchr(tmp1, '/');
    tmp2 = UNIX_strndup(s, (prev_dir) ? prev_dir - tmp1 : 0);
    tmp3 = UNIX_stradd(tmp2, match_begin + strlen("/.."));
    POV_FREE(tmp1);
    POV_FREE(tmp2);
    POV_FREE(s);
    s = tmp3;
#ifdef UNIX_DEBUG
   fprintf(stderr, "  %s\n", s);
#endif
  }

  // remove the last "/." if any
  if((match_begin = strstr(s, "/.")) != NULL  &&  *(match_begin + 2) == '\0')
  {
    tmp1 = UNIX_strndup(s, match_begin - s);
    POV_FREE(s);
    s = tmp1;
#ifdef UNIX_DEBUG
    fprintf(stderr, "  %s\n", s);
#endif
  }

  return s;
}


/*****************************************************************************
*
* FUNCTION  unix_append_path
*
* INPUT     list, path (string), flag for writable, flag for descend
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   Append a new entry to a list of paths.
*
* CHANGES
*
*   New for 3.6, February 2004 [NC]
*
******************************************************************************/

static void unix_append_path(PathList *list,
                             const char *path,
                             bool writable,
                             bool descend)
{
  Path *node;

#ifdef UNIX_DEBUG
  assert(list);
#endif

  node = (Path *) POV_MALLOC(sizeof(Path), "Path");
  node->str = (char *)path;
  node->writable = writable;
  node->descend = descend;
  node->next = NULL;

  if(! list->first)
    list->first = list->last = node;
  else
  {
    list->last->next = node;
    list->last = node;
  }
}


/*****************************************************************************
*
* FUNCTION  unix_clear_paths
*
* INPUT     list of paths
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   Remove all entries from a list of paths.
*
* CHANGES
*
*   New for 3.6, February 2004 [NC]
*
******************************************************************************/

static void unix_clear_paths(PathList *list)
{
  Path *path, *next;

#ifdef UNIX_DEBUG
  assert(list);
#endif

  path = list->first;
  if(path) do
  {
    if(path->str)
      POV_FREE(path->str);
    next = path->next;
    POV_FREE(path);
    path = next;
  } while(path);

  list->first = list->last = NULL;
}


/*****************************************************************************
*
* FUNCTION  unix_getline
*
* INPUT     file descriptor
*
* OUTPUT
*
* RETURNS   a new string
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   Read a line from a file and create a string.
*
* CHANGES
*
*   New for 3.6, February 2004 [NC]
*
******************************************************************************/

static char *unix_getline (FILE *f)
{
  char   *tmp;
  int     c;
  size_t  len, i;

  // inits
  i = 0;
  len = 256;  // default buffer size
  tmp = (char *) POV_CALLOC(len, 1, "unix_getline");

  // read stream
  do
  {
    c = fgetc(f);
    if(i > len-1)
    {
      len *= 2;  // double buffer size
      tmp = (char *) POV_REALLOC(tmp, len, "unix_getline");
    }
    if(c == EOF  ||  c == '\n')
    {
      tmp[i++] = '\0';  // does not include final '\n'
      return (char *) POV_REALLOC(tmp, i, "unix_getline");  // resize string
    }
    else
      tmp[i++] = c;
  } while(true);
}


/*****************************************************************************
*
* FUNCTION  unix_pre_process_line
*
* INPUT     input string
*
* OUTPUT
*
* RETURNS   a new string
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   Create a new string from processed input.
*
* CHANGES
*
*   New for 3.6, March 2004 [NC]
*
******************************************************************************/

static char *unix_pre_process_line(const char *input) 
{
  char *begin, *end;

  begin = (char *) input;

  while(*begin  &&  isspace((int) *begin))  // skip leading spaces
    ++begin;

  end = begin;
  while(*end  &&  *end != ';')  // find comment mark
    ++end;
  if(end-begin  &&  *end == ';')
    --end;

  while(end-begin > 0  &&  isspace((int) *end))  // ignore trailing spaces
    --end;
  if(end-begin)
    ++end;

  return UNIX_strndup(begin, end-begin);
}


/*****************************************************************************
*
* FUNCTION  unix_add_permitted_path
*
* INPUT     list of paths, input string, conf filename, line number
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   Add an entry in the list of permitted paths.
*
* CHANGES
*
*   New for 3.6, March 2004 [NC]
*
******************************************************************************/

static void unix_add_permitted_path(PathList      *list,
                                    const char    *input,
                                    const char    *conf_name,
                                    unsigned long  line_number)
{
  char *p, *directory, *begin, *tmp, quote;
  bool  descend, writable;

  // inits
  p = (char *) input;
  quote = 0;
  descend = writable = false;

  // read or read+write entry
  if(! strncmp(p, "read", strlen("read")))
  {
    p += strlen("read");
    writable = false;

    // we have a read+write path
    if(! strncmp(p, "+write", strlen("+write")))
      p += strlen("+write"), writable = true;

    // sub-directory search
    descend = false;
    if(*p == '*')
      ++p, descend = true;

    p = strchr(p, '=');  // find equal sign
    if(p)  // equal sign found
    {
      ++p;  // skip equal sign
      while(*p  &&  isspace((int) *p))
        ++p;  // skip leading spaces
      quote = 0;
      if(*p == '"'  ||  *p == '\'')
        quote = *p++;  // store and skip quote
      begin = p;
      while(*p)  // find next space caracter or closing quote
      {
        if(*p == quote  ||  (! quote  &&  isspace((int) *p)))
          break;
        ++p;
      }
      if(quote  &&  *p != quote)  // no closing quote
        fprintf(stderr,
          "%s: %s: %lu: ignored entry: missing closing %c quote.\n",
          PACKAGE, conf_name, line_number, quote
        );
      else if(p-begin)  // store given directory
      {
        directory = UNIX_strndup(begin, p-begin);
        tmp = UNIX_stradd(directory, "/");
        POV_FREE(directory);
        directory = UNIX_canonicalize_path(tmp);
        POV_FREE(tmp);
        unix_append_path(list, directory, writable, descend);
      }
      else  // nothing found after the equal sign
        fprintf(stderr,
          "%s: %s: %lu: ignored entry: missing directory.\n",
          PACKAGE, conf_name, line_number
        );
    }
    else  // equal sign not found
      fprintf(stderr,
        "%s: %s: %lu: ignored entry: missing equal sign.\n",
        PACKAGE, conf_name, line_number
      );
  }  // read or read+write entry

  // unknown entry
  else if(*p)
    fprintf(stderr,
      "%s: %s: %lu: unknown '%s' setting.\n",
      PACKAGE, conf_name, line_number, p
    );
}


/*****************************************************************************
*
* FUNCTION  unix_parse_conf_file
*
* INPUT     file descriptor and filename, flag for user mode
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Mark Gordon <mtgordon@povray.org> July 2002
*
* DESCRIPTION
*
*   Parse a povray.conf file.
*
* CHANGES
*
*   New as of July 2002
*
*   Rewrite for 3.6, September 2003 [NC]
*   - removed all internal static buffers.
*   - New and better "free format" for the povray.conf files.
*
******************************************************************************/

void unix_parse_conf_file(FILE       *conf_file,
                          const char *conf_name,
                          bool        user_mode)
{
  char           *line, *line2;
  unsigned long   line_number;
  bool            user_file_io_rejected;
  FileIO          file_io;
  bool            file_io_is_set;
  ShellOut        shellout;
  bool            shellout_is_set;
  PathList       *paths;
  short           i;

  typedef enum { NONE, FILE_IO, SHELLOUT, PERMITTED_PATHS, UNKNOWN } SectionVal;
  SectionVal section;
  typedef struct { const char *label; const SectionVal value; } Section;
  const Section sections[] =
  {
    { ""                   , NONE            },  // init
    { "[File I/O Security]", FILE_IO         },
    { "[Shellout Security]", SHELLOUT        },
    { "[Permitted Paths]"  , PERMITTED_PATHS },
    { NULL                 , UNKNOWN         }   // sentinel
  };

  typedef struct { const char *label; const FileIO value; } IOSettings;
  const IOSettings io_settings[] =
  {
    { ""          , IO_UNSET      },
    { "none"      , IO_NONE       },
    { "read-only" , IO_READONLY   },
    { "restricted", IO_RESTRICTED },
    { NULL        , IO_UNKNOWN    }
  };

  typedef struct { const char *label; const ShellOut value; } SHLSettings;
  const SHLSettings shl_settings[] =
  {
    { ""         , SHL_UNSET     },
    { "allowed"  , SHL_ALLOWED   },
    { "forbidden", SHL_FORBIDDEN },
    { NULL       , SHL_UNKNOWN   }
  };


  // inits
  line = line2 = NULL;
  line_number = 0;
  user_file_io_rejected = false;
  file_io_is_set = shellout_is_set = false;
  section = NONE;
  file_io = IO_UNSET;
  shellout = SHL_UNSET;

#ifdef UNIX_DEBUG
  fprintf(stderr, "PARSE CONF '%s'\n", conf_name);
#endif
 
  // Since the file format allows to read permitted paths before
  // setting [File I/O Security], the paths must be stored in a local
  // list which will be used only when the user setting for file I/O
  // is accepted.
  if(user_mode)  // create local list
    paths = (PathList *) POV_CALLOC(1, sizeof(PathList), "Paths");
  else  // use the config list
    paths = config->permitted_paths;

  // read in file
  while(! feof(conf_file))
  {
    // get and preprocess line
    line  = unix_getline(conf_file);
    line2 = unix_pre_process_line(line);
    POV_FREE(line);
    ++line_number;

    // skip empty line
    if(line2[0] == '\0')
    {
      POV_FREE(line2);
      continue;
    }

    // check section
    else if(line2[0] == '[')  // new section
    {
      // search section
      for(i = 0; sections[i].label; ++i)
        if(! strcmp(line2, sections[i].label))
          break;
      section = sections[i].value;

      // unknown section
      if(section == UNKNOWN)
        fprintf(stderr,
          "%s: %s: %lu: unknown '%s' section.\n",
          PACKAGE, conf_name, line_number, line2
        );
    }  // check section

    // file I/O security
    else if(section == FILE_IO)
    {
      // search setting
      for(i = 0; io_settings[i].label; ++i)
        if(! strcmp(line2, io_settings[i].label))
          break;
      file_io = io_settings[i].value;

      // multiple settings were found
      if(file_io_is_set)
        fprintf(stderr,
          "%s: %s: %lu: multiple settings for %s.\n",
          PACKAGE, conf_name, line_number, sections[section]
        );
      if(file_io != IO_UNSET)
        file_io_is_set = true;

      // unknown setting
      if(file_io == IO_UNKNOWN)
      {
        fprintf(stderr,
          "%s: %s: %lu: unknown '%s' setting for %s.  ",
          PACKAGE, conf_name, line_number, line2, sections[section]
        );
        if(user_mode)
        {
          fprintf(stderr,
            "Using system setting '%s'.\n",
            io_settings[config->file_io]
          );
          file_io = config->file_io;
          user_file_io_rejected = true;  // won't account for the user paths
        }
        else
        {
          fprintf(stderr, "I/O restrictions are disabled.\n");
          file_io = IO_NONE;
        }
      }

      // user setting not allowed
      if(user_mode  &&  file_io < config->file_io)
      {
        fprintf(stderr,
          "%s: %s: %lu: "
          "the user setting '%s' for %s is less restrictive than "
          "the system setting '%s' in '%s'.  Using system setting.\n",
          PACKAGE, conf_name, line_number,
          io_settings[        file_io].label, sections[section].label,
          io_settings[config->file_io].label, config->conf
        );
        file_io = config->file_io;
        user_file_io_rejected = true;  // won't account for the user paths
      }

      config->file_io = file_io;
    }  // file I/O security

    // shellout security
    else if(section == SHELLOUT)
    {
      // search setting
      for(i = 0; shl_settings[i].label; ++i)
        if(! strcmp(line2, shl_settings[i].label))
          break;
      shellout = shl_settings[i].value;

      // multiple settings were found
      if(shellout_is_set)
        fprintf(stderr,
          "%s: %s: %lu: multiple settings for %s.\n",
          PACKAGE, conf_name, line_number, sections[section]
        );
      if(shellout != SHL_UNSET)
        shellout_is_set = true;

      // unknown setting
      if(shellout == SHL_UNKNOWN)
      {
        fprintf(stderr,
          "%s: %s: %lu: unknown '%s' setting for %s.  ",
          PACKAGE, conf_name, line_number, line2, sections[section]
        );
        if(user_mode)
        {
          fprintf(stderr,
            "Using system setting '%s'.\n",
            shl_settings[config->shellout].label
          );
          shellout = config->shellout;
        }
        else
        {
          fprintf(stderr, "Shellout security is disabled.\n");
          shellout = SHL_ALLOWED;
        }
      }

      // user setting not allowed
      if(user_mode
      && config->shellout == SHL_FORBIDDEN
      && config->shellout != shellout)
      {
        fprintf(stderr,
          "%s: %s: %lu: "
          "the user setting '%s' for %s is less restrictive than "
          "the system '%s' setting in '%s'.  Using system setting.\n",
          PACKAGE, conf_name, line_number,
          shl_settings[        shellout].label, sections[section].label,
          shl_settings[config->shellout].label, config->conf
        );
        shellout = config->shellout;
      }

      config->shellout = shellout;
    }  // shellout security

    // permitted paths
    else if(section == PERMITTED_PATHS)
      unix_add_permitted_path(paths, line2, conf_name, line_number);

    // cleanup
    POV_FREE(line2);
  }  // read in file

#ifdef UNIX_DEBUG
  fprintf(stderr,
    "I/O RESTRICTIONS\n"
    "  file_io  = %d\tconfig->file_io  = %d\n"
    "  shellout = %d\tconfig->shellout = %d\n",
    file_io, config->file_io,
    shellout, config->shellout
  );
#endif

  // assign user settings and paths
  if(user_mode)
  {
    if(user_file_io_rejected)
    {
      unix_clear_paths(paths);  // free all user paths assigned here
      POV_FREE(paths);
      fprintf(stderr,
        "%s: %s: user permitted paths are ignored.  Using system paths.\n",
        PACKAGE, conf_name
      );
    }
    else
    {
      unix_clear_paths(config->permitted_paths);  // clear existing paths
      POV_FREE(config->permitted_paths);
      config->permitted_paths = paths;            // assign new paths
    }
  }
}


/*****************************************************************************
*
* FUNCTION  unix_process_povray_conf
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
*   Process povray.conf files in the following search order:
*     1) $PREFIX/etc/povray/3.6/povray.conf  ($PREFIX defaults to /usr/local)
*     2) $HOME/.povray/3.6/povray.conf
*   There is no backward compatibility for conf files as the format changed.
*
* CHANGES
*
*   New for 3.5, July 2002.
*
*   Rewrite for 3.6, September 2003 [NC]
*
******************************************************************************/

static void unix_process_povray_conf(void)
{
  FILE *f;

  // // read system configuration file
  // if(config->sysconf)
  // {
  //   f = fopen(config->sysconf, "r");
  //   if(f)
  //   {
  //     unix_parse_conf_file(f, config->sysconf, false);
  //     fclose(f);
  //     config->conf = config->sysconf;
  //   }
  //   else
  //   {
  //     fprintf(stderr,
  //       "%s: cannot open the system configuration file ",
  //       PACKAGE
  //     );
  //     perror(config->sysconf);
  //   }
  // }

  // // read user configuration file
  // if(config->userconf)
  // {
  //   f = fopen(config->userconf, "r");
  //   if(f)
  //   {
  //     unix_parse_conf_file(f, config->userconf, true);
  //     fclose(f);
  //     config->conf = config->userconf;
  //   }
  //   else
  //   {
  //     fprintf(stderr,
  //       "%s: cannot open the user configuration file ",
  //       PACKAGE
  //     );
  //     perror(config->userconf);
  //   }
  // }

  // no file was read, disable I/O restrictions
  if(! config->conf)
    fprintf(stderr, "%s: I/O restrictions are disabled.\n", PACKAGE);

  // if no paths specified, at least include POVLIBDIR and POVCONFDIR
  // else if(! config->permitted_paths->first)
  // {
  //   unix_append_path(config->permitted_paths,
  //     UNIX_stradd(POVLIBDIR, "/"), false, true   // read*
  //   );
  //   unix_append_path(config->permitted_paths,
  //     UNIX_stradd(POVCONFDIR,"/"), false, false  // read
  //   );
  // }

#ifdef UNIX_DEBUG
  {
    Path *path;

    fprintf(stderr, "PERMITTED PATHS\n");
    path = config->permitted_paths->first;
    if(path) do
    {
      fprintf(stderr,
        "  %s%s = \"%s\"\n",
        path->writable ? "WRITE" : "READ", path->descend ? "*" : "", path->str
      );
      path = path->next;
    } while(path);
  }
#endif
}


/*****************************************************************************
*
* FUNCTION  unix_parse_ini_file
*
* INPUT     filename
*
* OUTPUT
*
* RETURNS   error code
*
* AUTHOR    Mark Gordon <mtgordon@povray.org>
*
* DESCRIPTION
*
*   Parse INI file.
*
* CHANGES
*
*   New for 3.5 - copied from Windows version.
*
*   Rewrite for 3.6, November 2003 [trf]
*
*****************************************************************************/

static int unix_parse_ini_file(const char            *filename,
                               ProcessRenderOptions&  renderopts,
                               POVMSObjectPtr         renderoptsobject)
{
  int err = 0;
  return err;
}


/*****************************************************************************
*
* FUNCTION  unix_process_povray_ini
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   Process INI files in the following search order:
*     1) $POVINI
*     2) ./povray.ini
*     3) $HOME/.povray/3.6/povray.ini
*     4) $PREFIX/etc/povray/3.6/povray.ini  ($PREFIX defaults to /usr/local)
*     5) $HOME/.povrayrc                    (backward compatibility with 3.5)
*     6) $PREFIX/etc/povray.ini             (backward compatibility with 3.5)
*
* CHANGES
*
*   Created by Mike Fleetwood  Jan 1999
*
*   Rewrite for 3.6, September 2003 [NC]
*
******************************************************************************/

static void unix_process_povray_ini(ProcessRenderOptions& renderopts,
                                    POVMSObjectPtr        renderoptsobject)
{
  return;
}


/*****************************************************************************
*
* FUNCTION  unix_subdir
*
* INPUT     Filename (canonicalized !), system or user paths
*
* OUTPUT
*
* RETURNS   a boolean, telling whether the file is in any of the
*           directories in the list (or their subdirectories).
*
* AUTHOR    Mark Gordon <mtgordon@povray.org> July 2002
*
* DESCRIPTION
*
*   Checks to see whether the file requested is in a directory
*   specified in either POVCONFDIR/povray.conf or $HOME/.povray.conf
*   or in a subdirectory of any of the dirs in the list.
*
* CHANGES
*
*   Security bug fixed October 2002
*
*   Rewrite for 3.6, September 2003 [NC]
*   - removed all internal static buffers.
*   - subdirectories are not systematically allowed.
*   - added write parameter.
*
******************************************************************************/

static bool unix_subdir (const char *Filename, PathList *list, bool write)
{
  int   i;
  char *dirname, *pathname;
  Path *path;

#ifdef UNIX_DEBUG
  assert(list);
#endif

  // NOTE: Filename must be already canonicalized

  // no filename nor paths
  if(! Filename  ||  ! list->first)
    return false;

  // scan the list of paths
  path = list->first;
  do
  {
    if(path->descend)  // allows sub-directories
    {
      if(! write  ||  path->writable)
      {
#ifdef UNIX_DEBUG
        fprintf(stderr,
          "  FILE '%s' <-> %s* '%s'\n",
          Filename, path->writable ? "WRITE" : "READ", path->str
        );
#endif
        if(! strncmp(Filename, path->str, strlen(path->str)))  // match found
        {
#ifdef UNIX_DEBUG
          fprintf(stderr, "  OK\n");
#endif
          return true;
        }
      }
    }
    else  // check for exact match with path->str (without last slash)
    {
      dirname = unix_dirname(Filename);
      pathname = UNIX_strndup(path->str, strlen(path->str) - 1);
      if(! write  ||  path->writable)
      {
#ifdef UNIX_DEBUG
        fprintf(stderr,
          "  DIRNAME '%s' <-> %s '%s'\n",
          dirname, path->writable ? "WRITE" : "READ", pathname
        );
#endif
        if(! strcmp(dirname, pathname))
        {
          POV_FREE(dirname);
          POV_FREE(pathname);
#ifdef UNIX_DEBUG
          fprintf(stderr, "  OK\n");
#endif
          return true;
        }
        POV_FREE(dirname);
        POV_FREE(pathname);
      }
    }
    path = path->next;
  } while(path);

#ifdef UNIX_DEBUG
  fprintf(stderr, "  BAD\n");
#endif

  return false;
}


/*****************************************************************************
*
* FUNCTION  UNIX_allow_file_read
*
* INPUT     Filename, FileType
*
* OUTPUT
*
* RETURNS   TRUE if file reading is allowed, else FALSE
*
* AUTHOR    Mark Gordon <mtgordon@povray.org> July 2002
*
* DESCRIPTION
*
*    Performs tests to determine whether file reading is allowed.
*
* CHANGES
*
*    Brand new, March 2002 [mtg]
*
*    Rewrite for 3.6, September 2003 [NC]
*    - removed internal static buffer.
*    - added comments and code cleaning.
*    - added support for the write_paths (which are also readable).
*    - removed support for current working directory (changed .conf format).
*
******************************************************************************/

int UNIX_allow_file_read (const char *Filename, const unsigned int FileType)
{
  char       *filename;
  const char *errormsg;

  // read is always possible in these cases
  if(IO_RESTRICTIONS_DISABLED  ||  config->file_io < IO_RESTRICTED)
    return (TRUE);
  if(! Filename)
    return (FALSE);

#ifdef UNIX_DEBUG
  fprintf(stderr, "READ '%s'\n", Filename);
#endif

  // canonicalize Filename and create a new string
  filename = UNIX_canonicalize_path(Filename);

  // check against read-only paths or read/write paths
  if(unix_subdir(filename, config->permitted_paths, false))
  {
    POV_FREE(filename);
    return (TRUE);
  }

  // bad luck
  else
  {
    errormsg = pov_tsprintf(
      "Reading from '%s' is not permitted.  Check the configuration in '%s'.",
      filename, config->conf
    );
    POV_FREE(filename);
    UNIX_free_globals();
    if(no_error_call)
    {
      fprintf(stderr, "%s: %s\n", PACKAGE, errormsg);
      exit(EXIT_FAILURE);
    }
    else
      Error("%s", errormsg);
  }

  return (FALSE);
}


/*****************************************************************************
*
* FUNCTION  UNIX_allow_file_write
*
* INPUT     Filename, FileType
*
* OUTPUT
*
* RETURNS   TRUE if file writing is allowed, else FALSE
*
* AUTHOR    Mark Gordon <mtgordon@povray.org> July 2002
*
* DESCRIPTION
*
*    Performs tests to determine whether file writing is allowed.
*
* CHANGES
*
*    Brand new, March 2002 [mtg]
*
*    Rewrite for 3.6, September 2003 [NC]
*    - removed internal static buffer.
*    - added comments and code cleaning.
*    - removed support for current working directory (changed .conf format).
*
******************************************************************************/

int UNIX_allow_file_write (const char *Filename, const unsigned int FileType)
{
  char       *filename;
  const char *errormsg;

  // binary built without I/O restrictions, everything is allowed
  if(IO_RESTRICTIONS_DISABLED)
    return (TRUE);
  if(! Filename)
    return (FALSE);

#ifdef UNIX_DEBUG
  fprintf(stderr, "WRITE '%s'\n", Filename);
#endif

  // canonicalize Filename and create a new string
  filename = UNIX_canonicalize_path(Filename);

  // try to write to the config->sysconf or config->userconf files
  // so terminate with Error()
  if((config->sysconf   &&  ! strcmp(config->sysconf, filename))
  || (config->userconf  &&  ! strcmp(config->userconf, filename)))
  {
    errormsg = pov_tsprintf("Writing to '%s' is not permitted.\n", filename);
    POV_FREE(filename);
    UNIX_free_globals();
    if(no_error_call)
    {
      fprintf(stderr, "%s: %s\n", PACKAGE, errormsg);
      exit(EXIT_FAILURE);
    }
    else
      Error("%s", errormsg);
    return (FALSE);  // not used, here just for clarity
  }

  // can write everywhere
  else if(config->file_io < IO_READONLY)
  {
    POV_FREE(filename);
    return (TRUE);
  }

  // file is in a permitted directory
  else if(unix_subdir(filename, config->permitted_paths, true))
  {
    POV_FREE(filename);
    return (TRUE);
  }

  // bad luck
  else
  {
    errormsg = pov_tsprintf(
      "Writing to '%s' is not permitted.  Check the configuration in '%s'.\n",
      filename, config->conf
    );
    POV_FREE(filename);
    UNIX_free_globals();
    if(no_error_call)
    {
      fprintf(stderr, "%s: %s\n", PACKAGE, errormsg);
      exit(EXIT_FAILURE);
    }
    else
      Error("%s", errormsg);
  }

  return (FALSE);
}


/*****************************************************************************
*
* FUNCTION  unix_try_temp_file
*
* INPUT     Filename
*
* OUTPUT
*
* RETURNS   A new string containing the full path and filename
*           or an empty string otherwise
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*    Tries to locate a directory from the permitted list of paths to
*    write a temporary file.
*
* CHANGES
*
*    New for 3.6, May 2004 [NC]
*
******************************************************************************/

static char *unix_try_temp_file (const char *Filename)
{
  char *filename, *basename, *tmp;
  Path *path = NULL;

  if(! Filename)
    return UNIX_strdup("");

#ifdef UNIX_DEBUG
  fprintf(stderr, "TRY '%s'\n", Filename);
#endif

  // canonicalize Filename and create a new string
  filename = UNIX_canonicalize_path(Filename);

  // binary built without I/O restrictions, everything is allowed
  if(IO_RESTRICTIONS_DISABLED)
    return filename;

  // get the basename of the original (uncanonicalized) Filename
  basename = unix_basename(Filename);

  if(config->permitted_paths)
    path = config->permitted_paths->first;

  do
  {
    // try to write to the config->sysconf or config->userconf files
    if((config->sysconf   &&  ! strcmp(config->sysconf, filename))
    || (config->userconf  &&  ! strcmp(config->userconf, filename)))
    {
#ifdef UNIX_DEBUG
      fprintf(stderr, "  BAD\n");
#endif
      ;  // do nothing
    }

    // can write everywhere, or file is in a permitted directory
    else if(config->file_io < IO_READONLY
         || unix_subdir(filename, config->permitted_paths, true))
    {
      POV_FREE(basename);
      return filename;
    }

    // search next writable path
    if(path) do
    {
      if(path->writable)
        break;
      path = path->next;
    } while(path);

    // no path, return empty string
    if(! path)
    {
      POV_FREE(basename);
      POV_FREE(filename);
#ifdef UNIX_DEBUG
      fprintf(stderr, "  BAD\n");
#endif
      return UNIX_strdup("");
    }

    // create new filename
    POV_FREE(filename);
    tmp = UNIX_stradd(path->str, basename);
#ifdef UNIX_DEBUG
    fprintf(stderr, "TRY '%s'\n", tmp);
#endif
    filename = UNIX_canonicalize_path(tmp);
    POV_FREE(tmp);
    path = path->next;
  } while(true);

  POV_FREE(basename);

  return filename;
}


/*****************************************************************************
*
* FUNCTION  UNIX_system
*
* INPUT     wrapper for system(3)
*
* OUTPUT
*
* RETURNS   Return value of child process, or failure if it's not allowed.
*
* AUTHOR    Mark Gordon <mtgordon@povray.org> July 2002
*
* DESCRIPTION
*
*    Passes the string to system(3) if and only if system calls are allowed.
*
* CHANGES
*
*    Brand new, July 2002 [mtg]
*
*    Updated for 3.6, September 2003 [NC]
*    - code simplified.
*    - changed the warning level to 350 (i.e. version > 3.5).
*
******************************************************************************/

int UNIX_system (const char *string)
{
  if(IO_RESTRICTIONS_DISABLED  ||  config->shellout < SHL_FORBIDDEN)
    return (system(string));
  else
    Warning(350, "Shellout not allowed under your configuration.");

  return (-1);
}


/*****************************************************************************
*
* FUNCTION  unix_xwin_mode
*
* INPUT     argc, argv - the command-line arguments
*
* OUTPUT
*
* RETURNS   TRUE or FALSE (whether to use X preview functions)
*
* AUTHOR    mtgordon@povray.org <Mark Gordon>
*
* DESCRIPTION
*
*    Determines whether preview should use functions from the
*    X Window System.  It assumes you want X preview if you're
*    currently in X (as determined by checking for the presence
*    of the $DISPLAY environment variable) or if you're passing
*    -display on the command line.  Unless I'm mistaken, one of these
*    has to be true in order for you to use X-based preview.
*
*    If you don't have X installed (as determined by the configure script),
*    this is just going to return false.  Since the official binary
*    will depend on X to run, you'll need to recompile if you don't have X.
*
* CHANGES
*
*    Brand-new, June 2000. [mtg]
*
******************************************************************************/

static bool unix_xwin_mode (int argc, char *argv[])
{
#ifndef X_DISPLAY_MISSING
  int index;

  /* if $DISPLAY is non-null, return true */
  if(getenv("DISPLAY") != NULL)
    return (true);

  /* if passing -display, return true */
  for(index = 1; index < argc; index++)
    if(!strncmp(argv[index], "-display\0", 9))
      return (true);

#endif /* X_DISPLAY_MISSING */

  return (false);
}


/*****************************************************************************
*
* FUNCTION  unix_svga_mode
*
* INPUT
*
* OUTPUT
*
* RETURNS   TRUE or FALSE (whether to use SVGALib display functions)
*
* AUTHOR    mtgordon@povray.org <Mark Gordon>
*
* DESCRIPTION
*
*   Determines whether preview should use SVGAlib functions.
*   It assumes you want SVGA preview if you are at a TTY and either
*   you're root or POV-Ray is running suid root.  I've tried my best
*   to reduce the risk of buffer-overflow exploits for anyone who wants
*   to run POV-Ray suid root, but I still feel like I'm playing with fire.
*   Run suid root at your own risk.
*
*   If you don't have SVGAlib installed (as determined by the configure
*   script), this is just going to return false.  Since the official binary
*   will depend on SVGAlib to run, you'll need to recompile if you don't
*   SVGAlib.
*
* CHANGES
*
*   Brand-new as of June 2000.  Let me know if you find any problems.
*
*   Updated for 3.6, October 2003
*   - added test for ttyname()  [C.H.]
*   - moved test for is_using_xwin out of here  [NC]
*
******************************************************************************/

static bool unix_svga_mode (void)
{
#if defined(HAVE_LIBVGA) && defined(HAVE_LIBVGAGL)
  /* C.H. ttyname() might return NULL - this needs to be caught */
  if(ttyname(0) == NULL)
    return (false);

  /* if this isn't actually a TTY, no need for SVGA */
  if(!strstr(ttyname(0), "tty"))
    return (false);

  /* if POV-Ray is setuid-root, proceed */
  if(geteuid() == 0)
    return (true);

  /* or if the user is actually root, proceed */
  if(getuid() == 0)
    return (true);

  /*
   * if display is turned off, revert to non-SVGA, otherwise non-root users
   * won't be able to run it from the console.
   */
  if(!(opts.Options & DISPLAY))
  {
    UNIX_finish_povray     = &POV_Std_Finish_Povray;
    UNIX_display_init      = &POV_Std_Display_Init;
    UNIX_display_plot      = &POV_Std_Display_Plot;
    UNIX_display_plot_rect = &POV_Std_Display_Plot_Rect;
    UNIX_display_plot_box  = &POV_Std_Display_Plot_Box;
    UNIX_display_finished  = &POV_Std_Display_Finished;
    UNIX_display_close     = &POV_Std_Display_Close;
    UNIX_test_abort        = &POV_Std_Test_Abort;
    return (false);
  }
#endif  /* HAVE_LIBVGA && HAVE_LIBVGAGL */

  return (false);
}


/*****************************************************************************
*
* FUNCTION  unix_get_command_line
*
* INPUT     pointers to program arguments
*
* OUTPUT
*
* RETURNS   the new value of argc after arguments modifications
*
* AUTHOR    Nicolas Calimet
*
* DESCRIPTION
*
*   This is the function used to define the GETCOMMANDLINE macro (config.h).
*   The purpose is to process the command line before it is parsed by
*   the main POV-Ray code.  This pre-processing is required under X-Windows.
*   See the XWIN_init_povray() function (in xwin.cpp).  Note that since
*   we define our own main() function, the GETCOMMANDLINE macro is actually
*   unused.
*
* CHANGES
*
*   New for 3.6, September 2003 [NC]
*   - taken from UNIX_init_povray()
*
*****************************************************************************/

static int unix_get_command_line (int *argc, char **argv[])
{
  is_using_xwin = unix_xwin_mode(*argc, *argv);
  if(is_using_xwin)
#ifndef X_DISPLAY_MISSING
    XWIN_init_povray(argc, argv);  // this changes the command-line args
#else
    ;  // do nothing
#endif
  else
  {
    is_using_svga = unix_svga_mode();
    if(is_using_svga)
#if defined(HAVE_LIBVGA) && defined(HAVE_LIBVGAGL)
      SVGA_init_povray();
#else
      ;  // do nothing
#endif
  }

  return *argc;
}


/*****************************************************************************
*
* FUNCTION  UNIX_startup_povray
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
*   Performs X, SVGA, or ASCII initialization (using function pointers).
*
* CHANGES
*
*   Brand new, June 2000 [mtg]
*
*   Updated for 3.6, September 2003 [NC]
*   - renamed from UNIX_init_povray().
*   - code cleanup and a few commented changes.
*   - removed most of the static buffers.
*   - moved the I/O restrictions calls elsewhere.
*
******************************************************************************/

void UNIX_startup_povray (void)
{
  /*
   * Assume these and override as needed.
   */
  UNIX_finish_povray     = &POV_Std_Finish_Povray;
  UNIX_display_init      = &POV_Std_Display_Init;
  UNIX_display_plot      = &POV_Std_Display_Plot;
  UNIX_display_plot_rect = &POV_Std_Display_Plot_Rect;
  UNIX_display_plot_box  = &POV_Std_Display_Plot_Box;
  UNIX_display_finished  = &POV_Std_Display_Finished;
  UNIX_display_close     = &POV_Std_Display_Close;
  UNIX_test_abort        = &POV_Std_Test_Abort;

  /*
   * If we have X available to us, find out if we want to use it.
   * We'll use it if we're currently in an X session or if we're
   * passing -display on the command line.  If we decide that here,
   * set appropriate function counters to reduce later calculation.
   * [NC] unix_xwin_mode() and XWIN_init_povray() moved out of here. 
   */
  if(is_using_xwin)
  {
#ifndef X_DISPLAY_MISSING  /* [NC] moved inside the if condition */
    UNIX_finish_povray     = &XWIN_finish_povray;
    UNIX_display_init      = &XWIN_display_init;
    UNIX_display_plot      = &XWIN_display_plot;
    UNIX_display_plot_rect = &XWIN_display_plot_rect;
    UNIX_display_plot_box  = &XWIN_display_plot_box;
    UNIX_display_finished  = &XWIN_display_finished;
    UNIX_display_close     = &XWIN_display_close;
    UNIX_test_abort        = &XWIN_test_abort;
#endif  /* X_DISPLAY_MISSING */
  }

  /*
   * If we have SVGA available to us, find out if we want to use it.
   * We must be at the console, and we must either be root or running
   * suid root.
   * [NC] unix_svga_mode() moved out of here.
   */
  else if(is_using_svga)  // [NC] added else
  {
#if defined(HAVE_LIBVGA) && defined(HAVE_LIBVGAGL)  // [NC] moved in here
    UNIX_finish_povray     = &SVGA_finish_povray;
    UNIX_display_init      = &SVGA_display_init;
    UNIX_display_plot      = &SVGA_display_plot;
    UNIX_display_plot_rect = &SVGA_display_plot_rect;
    UNIX_display_plot_box  = &SVGA_display_plot_box;
    UNIX_display_finished  = &SVGA_display_finished;
    UNIX_display_close     = &SVGA_display_close;
    UNIX_test_abort        = &SVGA_test_abort;
#endif  /* HAVE_LIBVGA && HAVE_LIBVGAGL */
  }

  /* 
   * Insert other display types here (e.g. frame buffer)
   * if you feel like it.
   */
  else
  {
  }

  return;
}


/*****************************************************************************
*
* FUNCTION  POV_Std_Finish_Povray
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

void POV_Std_Finish_Povray(void)
{
  return;
}


/*****************************************************************************
*
* FUNCTION  POV_Std_Test_Abort
*
* INPUT
*
* OUTPUT
*
* RETURNS   0
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

int POV_Std_Test_Abort(void)
{
  /*
   * This is the render abort function.
   * It is not needed for the command-line interface,
   * since the abort is handled via interrupts.
   */
  return (0);
}


/*****************************************************************************
*
* FUNCTION  matherr
*
* INPUT     pointer to exception
*
* OUTPUT
*
* RETURNS   1
*
* AUTHOR
*
* DESCRIPTION
*
*   The error handler for floating point exceptions.  Not totally critical,
*   but can avoid aborting a trace that gets a floating-point error during
*   the render.  It tries to return a reasonable value for the errors.
*   However, this slows down the rendering noticably.
*
*   Note: this is a SystemVism, so don't be surprised if this doesn't work
*   on BSDish systems.  It shouldn't be critical, and it should still compile.
*
* CHANGES
*
******************************************************************************/

#ifdef UNDERFLOW
#ifdef exception

int matherr(struct exception *x)
{
  switch(x->type)
  {
    case DOMAIN:
    case OVERFLOW:
      x->retval = 1.0e17;
      break;
    case SING:
    case UNDERFLOW:
      x->retval = 0.0;
      break;
  }

  return (1);
}

#endif /* exception  */
#endif /* UNDERFLOW  */


#if PRECISION_TIMER_AVAILABLE

/*****************************************************************************
*
* FUNCTION  UNIX_timer_start
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Andreas Dilger   Oct 1995
*
* DESCRIPTION
*
*   Starts the histogram timer.
*
* CHANGES
*
******************************************************************************/

void UNIX_timer_start(void)
{
  gettimeofday(&hstart, (struct timezone *)NULL);
}


/*****************************************************************************
*
* FUNCTION  UNIX_timer_stop
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Andreas Dilger   Oct 1995
*
* DESCRIPTION
*
*   Stops the histogram timer.
*
* CHANGES
*
******************************************************************************/

void UNIX_timer_stop(void)
{
  gettimeofday(&hstop, (struct timezone *)NULL);
}


/*****************************************************************************
*
* FUNCTION  UNIX_timer_count
*
* INPUT
*
* OUTPUT
*
* RETURNS   the elapsed real time between start and stop in 10^-5s increments
*
* AUTHOR    Andreas Dilger   Oct 1995
*
* DESCRIPTION
*
*   The elapsed wall-clock time between the last two calls to
*   accumulate_histogram is measured in tens of microseconds.
*   Using microseconds for the histogram timer would only allow
*   1.2 hours per histogram bucket (ie 2^32 x 10^-6s in an unsigned
*   32 bit long), which may not be enough in some rare cases :-)
*   Note that since this uses wall-clock time rather than CPU
*   time, the histogram will have noise in it because of the
*   task switching going on.
*
* CHANGES
*
******************************************************************************/

int UNIX_timer_count(void)
{
  return ((hstop.tv_sec - hstart.tv_sec) * 100000 +
          (hstop.tv_usec - hstart.tv_usec + 5) / 10);
}


#endif  /* PRECISION_TIMER_AVAILABLE */


/*****************************************************************************
*
* FUNCTION  UNIX_abort_start
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Andreas Dilger   Oct 1995
*
* DESCRIPTION
*
*   Initializes the signal handlers.
*
* CHANGES
*
*   Apr 1996: Add SIGFPE to be ignored, bacause of floating point exceptions.
*             This isn't a great solution, but I don't know of another way to
*             prevent core dumps on Linux systems that use older versions of
*             libm. [AED]
*
******************************************************************************/

void UNIX_abort_start(void)
{
#ifdef SIGTERM
  signal(SIGTERM, UNIX_abort_handler);
#endif /* SIGTERM */
#ifdef SIGINT
  signal(SIGINT, UNIX_abort_handler);
#endif /* SIGINT */
#ifdef SIGPIPE
  signal(SIGPIPE, UNIX_abort_handler);
#endif /* SIGPIPE */
#if !defined(OVERFLOW) && defined(SIGFPE) /* avoid floating point exceptions */
  signal(SIGFPE, SIG_IGN);
#endif /* !defined(OVERFLOW) && defined (SIGPFE) */
}


/*****************************************************************************
*
* FUNCTION  UNIX_abort_handler
*
* INPUT     signum - signal number, or 0 if not called by a signal
*
* OUTPUT
*
* RETURNS
*
* AUTHOR    Andreas Dilger   Oct 1995
*
* DESCRIPTION
*
*   This is the signal handler.  If it is called by a signal (signal > 0),
*   then it will call set the global "Stop_Flag", and the rendering will
*   be aborted when the current pixel is complete.  We re-initialize the
*   signal handler for those older systems that reset the signal handlers
*   when they are called.  If we are really quitting (signal = 0) or this
*   routine has been called many times, we set the signal handlers back to
*   their default action, so that further signals are handled normally.
*   This is so we don't get caught in a loop if we are handling a signal
*   caused by the shutdown process (ie file output to a pipe that's closed).
*
* CHANGES
*
*   Sept 1, 1996   Don't write to stderr if there have been many interrupts,
*                  so we don't write to a PIPE that's closed and cause more
*                  interrupts. [AED]
*
******************************************************************************/

void UNIX_abort_handler(int signum)
{
  UNIX_abort_start();

  if(signum > 0 && ++Stop_Flag < 5)
  {
    switch(signum)
    {
#ifdef SIGINT
      case SIGINT:
        PossibleError("Got %d SIGINT.", Stop_Flag);
        break;
#endif /* SIGINT  */
#ifdef SIGPIPE
      case SIGPIPE:
        PossibleError("Got %d SIGPIPE.", Stop_Flag);
        break;
#endif /* SIGPIPE  */
#ifdef SIGTERM
      case SIGTERM:
        PossibleError("Got %d SIGTERM.", Stop_Flag);
        break;
#endif /* SIGTERM  */
      default:
        PossibleError("Got %d unknown signal (%d).", Stop_Flag, signum);
        break;
    }
  }

  if(signum == 0 || Stop_Flag > 10)
  {
#ifdef SIGTERM
    signal(SIGTERM, SIG_DFL);
#endif /* SIGTERM  */
#ifdef SIGINT
    signal(SIGINT, SIG_DFL);
#endif /* SIGINT  */
#ifdef SIGPIPE
    signal(SIGPIPE, SIG_DFL);
#endif /* SIGPIPE  */
  }
}


/*****************************************************************************
*
* FUNCTION  main
*
* INPUT     program arguments
*
* OUTPUT
*
* RETURNS   program result
*
* AUTHOR    Thorsten Froehlich [trf]
*
* DESCRIPTION
*
*   Main function based on November 2003 povray.cpp main function.
*
* CHANGES
*
*   Created Nov. 13, 2003 [trf]
*
*****************************************************************************/

int main(int argc, char **argv)
{
  USING_POV_NAMESPACE
  USING_POV_BASE_NAMESPACE
  USING_POV_FRONTEND_NAMESPACE

  DefaultPlatformBase platformbase;
  POVMSAddress addr = POVMSInvalidAddress;
  int err = kNoErr;
  int ret = 0;
  int i = 0;
  bool Benchmark_Mode=false;
  char *command_line;
  char *demo_ini_name;
  char *demo_file_name;
  int len;
  char s[3];

  // Init
  no_error_call = true;
  povray_init();

  // unix-specific inits
  is_using_xwin = is_using_svga = false;
  unix_create_globals();
 
  if(err == kNoErr)  // get render context address
    err = POVMS_GetContextAddress(POVMS_Render_Context, &addr);
  if(err != kNoErr)
    (void)POVMS_ASSERT_OUTPUT(
      "Accessing POVMS render context failed.",
      "unix.cpp",
      0
    );

  if(err == kNoErr)  // create local frontend context
    err = POVMS_OpenContext(&POVMS_Output_Context);
  if(err != kNoErr)
    (void)POVMS_ASSERT_OUTPUT(
      "Creating POVMS output context failed.",
      "unix.cpp",
      0
    );
  else
  {
    DefaultRenderFrontend frontend(POVMS_Output_Context, addr);

    globalDefaultRenderFrontendPointer = &frontend;

    unix_process_povray_conf();
    unix_get_command_line(&argc, &argv);

    // Binary control mode via POVMS on stdin and stdout
    if((argc > 1) && (pov_stricmp(argv[1], "-povms") == 0))
      Binary_POVMS_Stream_Mode = true;

    // benchmark mode
    if(argc > 1) 
    if((pov_stricmp(argv[1], "-benchmark") == 0)
    || (pov_stricmp(argv[1], "--benchmark") == 0))
    {
      i = Get_Benchmark_Version() ;
      fprintf(stderr, "\
%s: entering the standard POV-Ray 3.6 benchmark version %x.%02x.\n\
Running the benchmark will take some time, e.g. about 45 minutes on\n\
a 2 GHz Intel Pentium 4 (tm) processor.  There will be no display or\n\
file output.  To get an accurate benchmark result you might consider\n\
running POV-Ray with 'nice'.\n\n\
Press <Enter> to continue or CTRL+C to abort.\n\
",
        PACKAGE, i / 256, i % 256
      );
      // don't change this to getchar() - otherwise <Enter> will also 
      // influence commands after povray finishes. [C.H.]
      fgets(s, 2, stdin); 

      demo_ini_name = unix_try_temp_file("pov36_benchmark.ini");
      demo_file_name = unix_try_temp_file("pov36_benchmark.pov");
      if(Write_Benchmark_File(demo_file_name, demo_ini_name))
      {
        fprintf(stderr, "%s: creating '%s'.\n", PACKAGE, demo_file_name);
        fprintf(stderr, "%s: creating '%s'.\n", PACKAGE, demo_ini_name);
        len = strlen(demo_ini_name) + strlen(demo_file_name) + 100 ;
        command_line = (char *) POV_MALLOC(len, "benchmark render");
        sprintf(command_line,
          "Include_Ini='%s' Input_File_Name='%s'",
          demo_ini_name, demo_file_name
        );
        fprintf(stderr,
          "Running standard POV-Ray benchmark version %x.%02x\n",
          i / 256, i % 256
        );
      }
      else
      {
        fprintf(stderr,
          "%s: failed to write temporary files for benchmark\n",
          PACKAGE
        );
        exit(EXIT_FAILURE);
      }

      Benchmark_Mode=true;
    }

    // Print help screens
    if(argc == 1)
    {
      povray_cooperate();
      frontend.PrintHelpScreens();
        return 0;
    }
    else if(argc == 2)
    {
      if((pov_stricmp(argv[1], "-h") == 0)
      || (pov_stricmp(argv[1], "-?") == 0)
      || (pov_stricmp(argv[1], "--help") == 0)
      || (pov_stricmp(argv[1], "-help") == 0))
      {
        povray_cooperate();
        frontend.PrintHelpScreens();
          return 0;
      }
      else if(argv[1][0] == '-')
      {
        if(argv[1][1] == '?')
        {
          frontend.PrintUsage(argv[1][2] - '0');
            return 0;
        }
        else if(strlen(argv[1]) == 6)
        {
          if(((argv[1][1] == 'h') || (argv[1][1] == 'H'))
          && ((argv[1][2] == 'e') || (argv[1][2] == 'E'))
          && ((argv[1][3] == 'l') || (argv[1][3] == 'L'))
          && ((argv[1][4] == 'p') || (argv[1][4] == 'P')))
          {
            frontend.PrintUsage(argv[1][5] - '0');
            return 0;
          }
        }
      }
    }  // argc == 2

    // Binary control mode via POVMS on stdin and stdout
    if((argc > 1) && (pov_stricmp(argv[1], "-povms") == 0))
      Binary_POVMS_Stream_Mode = true;

    if(Binary_POVMS_Stream_Mode == true)
    {
      while(Cooperate_Render_Flag >= 0)
        povray_cooperate();
      return 0;
    }

    try
    {
      ProcessRenderOptions renderoptions;
      POVMSObject obj;
      int l = 0;

      // create obj, a regular POVMS object that holds render options
      err = POVMSObject_New(&obj, kPOVObjectClass_ROptions);
      if(err != kNoErr)
        throw err;

      // get render options from some INI file
      unix_process_povray_ini(renderoptions, &obj);

      // benchmark mode: just parse the prepared command line
      if(Benchmark_Mode)
      {
        err = renderoptions.ParseString(command_line, &obj, false);
        if(err != kNoErr)
          throw err;
      }
      else
        // get the command-line options
        for(i = 1; i < argc; i++)
          if(pov_stricmp(argv[i], "-povms") != 0)
          {
            err = renderoptions.ParseString(argv[i], &obj, true);
            if(err != kNoErr)
              throw err;
          }

      no_error_call = false;

      // give obj to optionsobj, a POVMS C++ wrapper needed for frontend */
      POVMS_Object optionsobj(obj);
      frontend.StartRender(optionsobj);  // start rendering

      // wait until the render is complete
      while(frontend.GetState() != RenderFrontend::kReady)
        povray_cooperate();
    }

    // catch exceptions
    catch(int err)
    {
      fprintf(stderr, "Failed to render file due to error(s)!\n");
      return err;
    }
    catch(const char *str)
    {
      fprintf(stderr, "%s\n Failed to render file!\n", str);
      return -1;
    }
    catch(...)
    {
      fprintf(stderr, "Failed to render file due to error(s)!\n");
      return -1;
    }

    // benchmark mode - cleanup
    if(Benchmark_Mode)
    {
      fprintf(stderr, "%s: removing '%s'.\n", PACKAGE, demo_file_name);
      fprintf(stderr, "%s: removing '%s'.\n", PACKAGE, demo_ini_name);
      DELETE_FILE(demo_file_name) ;
      DELETE_FILE(demo_ini_name) ;
      POV_FREE(demo_file_name);
      POV_FREE(demo_ini_name);
      POV_FREE(command_line);
    }

    // NOTE: It is important that 'frontend' be destroyed in this block scope
    // because 'POVMS_CloseContext' will destroy the render context too early
    // otherwise!
  }  // err == kNoErr

  // Finish
  povray_terminate();

  (void)POVMS_CloseContext(POVMS_Output_Context);

  return ret;
}
