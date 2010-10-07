/****************************************************************************
 *               pov_util.cpp
 *
 * This module implements misc utility functions.
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
 * $File: //depot/povray/3.6-release/source/pov_util.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <ctype.h>
#include <stdarg.h>

#include "frame.h"
#include "pov_util.h"
#include "povray.h"

BEGIN_POV_NAMESPACE

/*
// not used right now
typedef struct
{
  bool read_local;
  bool read_global;
  bool write_local;
  bool write_global;
} POV_File_Restrictions;

POV_File_Restrictions gPOV_File_Restrictions[POV_File_Unknown_Count] =
{
  { false, false, false, false }, // POV_File_Unknown
  { true,  true,  false, false }, // POV_File_Image_Targa
  { true,  true,  false, false }, // POV_File_Image_PNG
  { true,  true,  false, false }, // POV_File_Image_PPM
  { true,  true,  false, false }, // POV_File_Image_PGM
  { true,  true,  false, false }, // POV_File_Image_GIF
  { true,  true,  false, false }, // POV_File_Image_IFF
  { true,  true,  false, false }, // POV_File_Image_JPEG
  { true,  true,  false, false }, // POV_File_Image_TIFF
  { true,  true,  false, false }, // POV_File_Image_System
  { true,  false, false, false }, // POV_File_Text_POV
  { true,  false, false, false }, // POV_File_Text_INC
  { true,  false, false, false }, // POV_File_Text_INI
  { true,  true,  false, false }, // POV_File_Text_CSV
  { true,  false, false, false }, // POV_File_Text_Stream
  { true,  true,  false, false }, // POV_File_Text_User
  { true,  true,  true,  false }, // POV_File_Data_DF3
  { true,  true,  true,  true  }, // POV_File_Data_RCA
  { true,  true,  true,  true  }, // POV_File_Data_LOG
  { true,  false, true,  false }  // POV_File_Font_TTF
};
*/


/*****************************************************************************
*
* FUNCTION
*
*   POV_Std_Split_Time
*
* INPUT
*
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   Split time into hours, minutes and seconds.
*
* CHANGES
*
*   Changed to use plain integer math. Will no longer operate correctly for
*   time differences longer than 6.8 years. [trf]
*
******************************************************************************/

void POV_Std_Split_Time(DBL time_dif, unsigned int *hrs, unsigned int *mins, DBL *secs)
{
  int t = (int)(time_dif * 10.0);

  *hrs = t / 36000;

  *mins = (t / 600) % 60;

  *secs = ((DBL)(t % 600)) / 10.0;
}



/*****************************************************************************
*
* FUNCTION
*
*   closest_power_of_2
*
* INPUT
*
*   theNumber - the value to determine closest power of 2 for.
*
* OUTPUT
*
* RETURNS
*
*   The closest power of two is returned, or zero if the
*   argument is less than or equal to zero.
*
* AUTHOR
*
*   Eduard Schwan
*
* DESCRIPTION
*
*   Decription: Find the highest positive power of 2 that is
*   less than or equal to the number passed.
*
*   Input  Output
*   -----  ------
*     0      0
*     1      1
*     2      2
*     3      2
*     8      8
*     9      8
*
* CHANGES
*
*   Aug 1994 : Created by Eduard.
*
******************************************************************************/

unsigned closest_power_of_2(unsigned theNumber)
{
  int PowerOf2Counter;

  /* do not handle zero or negative numbers for now */

  if (theNumber <= 0)
  {
    return(0);
  }

  /* count the number in question down as we count up a power of 2 */

  PowerOf2Counter = 1;

  while (theNumber > 1)
  {
    /* move our power of 2 counter bit up... */

    PowerOf2Counter <<= 1;

    /* and reduce our test number by a factor of 2 two */

    theNumber >>= 1;
  }

  return(PowerOf2Counter);
}



/*****************************************************************************
*
* FUNCTION
*   POVMSUtil_SetFormatString
*   
* DESCRIPTION
*   Stores a string with format information in the given attribute.
*
* CHANGES
*   -
*
******************************************************************************/

int POVMSUtil_SetFormatString(POVMSObjectPtr object, POVMSType key, const char *format, ...) // Note: Strings may not contain \0 characters codes!
{
	va_list marker;
	char buffer[1024];

	va_start(marker, format);
	vsprintf(buffer, format, marker);
	va_end(marker);

	return POVMSUtil_SetString(object, key, buffer);
}


/*****************************************************************************
*
* FUNCTION
*
*   New_Checked_IStream
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

IStream *New_Checked_IStream(char *filename, unsigned int stype)
{
	if(POV_ALLOW_FILE_READ(filename, stype) == true)
		return New_IStream(filename, stype);
	return NULL;
}


/*****************************************************************************
*
* FUNCTION
*
*   New_Checked_OStream
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
* CHANGES
*
******************************************************************************/

OStream *New_Checked_OStream(char *filename, unsigned int stype, bool append)
{
	if(POV_ALLOW_FILE_WRITE(filename, stype) == true)
		return New_OStream(filename, stype, append);
	return NULL;
}


/*****************************************************************************
*
* FUNCTION
*
*   Locate_File
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   Find a file in the search path.
*
* CHANGES
*
*   Apr 1996: Don't add trailing FILENAME_SEPARATOR if we are immediately
*             following DRIVE_SEPARATOR because of Amiga probs.  [AED]
*
******************************************************************************/

IStream *Locate_File(char *filename, unsigned int stype, char *buffer, bool err_flag)
{
  IStream *result;
  char *qualified_name = Locate_Filename(filename, stype, err_flag);

  if (qualified_name != NULL)
  {
    POV_GET_FULL_PATH(qualified_name, buffer);
    result = New_Checked_IStream(qualified_name, stype);
    delete[] qualified_name;
  }
  else
  {
    /* Any error was already reported in Locate_Filename(...) */
    result = NULL;
  }

  return result;
}

/*****************************************************************************
*
* FUNCTION
*
*   Locate_Filename
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*  Fully expanded filename, including drive, path, ...
*   
* AUTHOR
*
*   Alexander R. Enzmann
*   
* DESCRIPTION
*
*   Find a file in the search path.
*
* CHANGES
*
*
******************************************************************************/

char *Locate_Filename(char *filename, unsigned int stype, bool err_flag)
{
  int i,ii,l[4];
  char pathname[FILE_NAME_LENGTH];
  char file[FILE_NAME_LENGTH];
  char file_x[4][FILE_NAME_LENGTH];
  char *result = NULL;

  if (Has_Extension(filename))
  {
    for(i = 0; i < 4; i++)
      l[i]=0;
  }
  else
  {
    for(i = 0; i < 4; i++)
    {
      if ((l[i] = strlen(gPOV_File_Extensions[stype].ext[i])) > 0)
      {
        strcpy(file_x[i], filename);
        strcat(file_x[i], gPOV_File_Extensions[stype].ext[i]);
      }
    }
  }

  /* Check the current directory first. */
  for(i = 0; i < 4; i++)
  {
    /* Try appending the variations of the file extension */
    if(l[i])
    {
      if (EXIST_FILE(file_x[i]) == true)
      {
        result = new char[strlen(file_x[i]) + 1];
        POV_GET_FULL_PATH(file_x[i], result);
        return result;
      }
    }
  }
  /* Try the filename without any modifications */
  if (EXIST_FILE(filename) == true)
  {
    result = new char[strlen(filename) + 1];
    POV_GET_FULL_PATH(filename, result);
    return result;
  }
  
  /* Walk through the library paths, trying with and without file extensions */
  for (i = 0; i < opts.Library_Path_Index; i++)
  {
    strcpy(file, opts.Library_Paths[i]);
    file[strlen(file)+1] = '\0';
    if (file[strlen(file) - 1] != DRIVE_SEPARATOR)
      file[strlen(file)] = FILENAME_SEPARATOR;
    
    for(ii = 0; ii < 4; ii++)
    {
      if(l[ii])
      {
        strcpy(pathname, file);
        strcat(pathname, file_x[ii]);
        if (EXIST_FILE(pathname) == true)
        {
          result = new char[strlen(pathname) + 1];
          POV_GET_FULL_PATH(pathname, result);
          return result;
        }
      }
    }
    
    strcpy(pathname, file);
    strcat(pathname, filename);
    if (EXIST_FILE(pathname) == true)
    {
      result = new char[strlen(pathname) + 1];
      POV_GET_FULL_PATH(pathname, result);
      return result;
    }
  }

  // Allow system specific access of font files:
  // Obviously this requires POV_NEW_ISTREAM
  // to be platform specific as well! [trf]
  if(stype == POV_File_Font_TTF)
  {
    if(EXIST_FONT_FILE(filename))
    {
       result = new char[strlen(filename) + 1];
       strcpy(filename, result);
       return result;
    }
  }

  if (err_flag)
  {
    if (l[0])
      PossibleError("Could not find file '%s%s'",filename,gPOV_File_Extensions[stype].ext[0]);
    else
      PossibleError("Could not find file '%s'",filename);
  }
  
  return NULL;
}

END_POV_NAMESPACE
