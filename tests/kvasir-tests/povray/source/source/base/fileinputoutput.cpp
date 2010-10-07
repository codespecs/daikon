/****************************************************************************
 *               fileinputoutput.cpp
 *
 * This module implements the classes handling file input and output.
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
 * $File: //depot/povray/3.6-release/source/base/fileinputoutput.cpp $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#include <cstdlib>
#include <cstdarg>

#include "configbase.h"

#include "fileinputoutput.h"
#include "stringutilities.h"
#include "platformbase.h"
#include "pointer.h"

BEGIN_POV_BASE_NAMESPACE

#ifndef POV_IS1
	#define POV_IS1 ""
#endif

#ifndef POV_IS2
	#define POV_IS2 ""
#endif

#ifndef POV_IS3
	#define POV_IS3 ""
#endif

#ifndef POV_IS4
	#define POV_IS4 ""
#endif

POV_File_Extensions gPOV_File_Extensions[POV_File_Unknown_Count] =
{
  {{ "",      "",      "",      ""      }}, // POV_File_Unknown
  {{ ".tga",  ".TGA",  "",      ""      }}, // POV_File_Image_Targa
  {{ ".png",  ".PNG",  "",      ""      }}, // POV_File_Image_PNG
  {{ ".ppm",  ".PPM",  "",      ""      }}, // POV_File_Image_PPM
  {{ ".pgm",  ".PGM",  "",      ""      }}, // POV_File_Image_PGM
  {{ ".gif",  ".GIF",  "",      ""      }}, // POV_File_Image_GIF
  {{ ".iff",  ".IFF",  "",      ""      }}, // POV_File_Image_IFF
  {{ ".jpg",  ".JPG",  ".jpeg", ".JPEG" }}, // POV_File_Image_JPEG
  {{ ".tif",  ".TIF",  ".tiff", ".TIFF" }}, // POV_File_Image_TIFF
  {{ POV_IS1, POV_IS2, POV_IS3, POV_IS4 }}, // POV_File_Image_System
  {{ ".pov",  ".POV",  "",      ""      }}, // POV_File_Text_POV
  {{ ".inc",  ".INC",  "",      ""      }}, // POV_File_Text_INC
  {{ ".ini",  ".INI",  "",      ""      }}, // POV_File_Text_INI
  {{ ".csv",  ".CSV",  "",      ""      }}, // POV_File_Text_CSV
  {{ ".txt",  ".TXT",  "",      ""      }}, // POV_File_Text_Stream
  {{ "",      "",      "",      ""      }}, // POV_File_Text_User
  {{ ".df3",  ".DF3",  "",      ""      }}, // POV_File_Data_DF3
  {{ ".rca",  ".RCA",  "",      ""      }}, // POV_File_Data_RCA
  {{ ".log",  ".LOG",  "",      ""      }}, // POV_File_Data_LOG
  {{ ".ttf",  ".TTF",  "",      ""      }}  // POV_File_Font_TTF
};

IOBase::IOBase(unsigned int dir, unsigned int type)
{
  filetype = type;
  direction = dir;
  fail = true;
  f = NULL;
}

IOBase::~IOBase()
{
  close();
}

bool IOBase::open(const char *Name, unsigned int Flags /* = 0 */)
{
  char mode[8];

  close();

  if((Flags & append) == 0)
  {
    switch(direction)
    {
      case input:
           strcpy(mode, "r");
           break;
      case output:
           strcpy(mode, "w");
           break;
      case io:
           strcpy(mode, "w+");
           break;
      default:
        return false;
    }
  }
  else
  {
    // we cannot use append mode here, since "a" mode is totally incompatible with any
    // output file format that requires in-place updates(i.e. writing to any location
    // other than the end of the file). BMP files are in this category. In theory, "r+"
    // can do anything "a" can do(with appropriate use of seek()) so append mode should
    // not be needed.
    strcpy(mode, "r+");
  }

  strcat(mode, "b");

  f = NULL;
  if(pov_stricmp(Name, "stdin") == 0)
  {
    if(direction != input ||(Flags & append) != 0)
      return false;
    f = stdin;
  }
  else if(pov_stricmp(Name, "stdout") == 0)
  {
    if(direction != output ||(Flags & append) != 0)
      return false;
    f = stdout;
  }
  else
  {
    if((f = fopen(Name, mode)) == NULL)
    {
      if((Flags & append) == 0)
        return false;

      // to maintain traditional POV +c(continue) mode compatibility, if
      // the open for append of an existing file fails, we allow a new file
      // to be created.
      mode [0] = 'w';
      if((f = fopen(Name, mode)) == NULL)
        return false;
    }
  }
  fail = false;

  if((Flags & append) != 0)
  {
    if(!seekg(0, seek_end))
    {
      close();
      return false;
    }
  }
  
  return true;
}

bool IOBase::close(void)
{
  if(f != NULL)
  {
    fclose(f);
    f = NULL;
  }
  fail = true;
  return true;
}

IOBase& IOBase::flush(void)
{
  if(f != NULL)
    fflush(f);
  return *this;
}

IOBase& IOBase::read(void *buffer, unsigned long count)
{
  if(!fail && count > 0)
    fail = fread(buffer, count, 1, f) != 1;
  return *this;
}

IOBase& IOBase::write(void *buffer, unsigned long count)
{
  if(!fail && count > 0)
    fail = fwrite(buffer, count, 1, f) != 1;
  return *this;
}

// Strictly speaking, this should -not- be called seekg, since 'seekg'(an iostreams
// term) applies only to an input stream, and therefore the use of this name here
// implies that only the input stream will be affected on streams opened for I/O
//(which is not the case with fseek, since fseek moves the pointer for output too).
// However, the macintosh code seems to need it to be called seekg, so it is ...
IOBase& IOBase::seekg(unsigned long pos, unsigned int whence /* = seek_set */)
{
  if(!fail)
    fail = fseek(f, pos, whence) != 0;
  return *this;
}

IStream::IStream(const unsigned int stype) : IOBase(input, stype)
{
}

IStream::~IStream()
{
}

int IStream::Read_Short(void)
{
  short result;
  read((char *) &result, sizeof(short));
  return result;
}

int IStream::Read_Int(void)
{
  int result;
  read((char *) &result, sizeof(int));
  return result;
}

long IStream::Read_Long(void)
{
  long result;
  read((char *) &result, sizeof(long));
  return result;
}

IStream& IStream::UnRead_Byte(int c)
{
  if(!fail)
    fail = ungetc(c, f) != c;
  return *this;
}

IStream& IStream::getline(char *s, unsigned long buflen)
{
  int chr = 0;

  if(feof(f) != 0)
    fail = true;

  if(!fail && buflen > 0)
  {
    while(buflen > 1)
    {
      chr = fgetc(f);
      if(chr == EOF)
        break;
      else if(chr == 10)
      {
        chr = fgetc(f);
        if(chr != 13)
          ungetc(chr, f);
        break;
      }
      else if(chr == 13)
      {
        chr = fgetc(f);
        if(chr != 10)
          ungetc(chr, f);
        break;
      }
      *s = chr;
      s++;
      buflen--;
    }
    *s = 0;
  }

  return *this;
}

OStream::OStream(const unsigned int stype) : IOBase(output, stype)
{
}

OStream::~OStream()
{
}

void OStream::printf(const char *format, ...)
{
  va_list marker;
  char buffer[1024];

  va_start(marker, format);
  vsnprintf(buffer, 1023, format, marker);
  va_end(marker);

  *this << buffer;
}

IStream *New_IStream(const char *sname, const unsigned int stype)
{
	Pointer<IStream> istreamptr(POV_PLATFORM_BASE.CreateIStream(stype));

	if(istreamptr == NULL)
		return NULL;

	if(istreamptr->open(sname) == 0)
		return NULL;

	return istreamptr.release();
}

OStream *New_OStream(const char *sname, const unsigned int stype, const bool sappend)
{
	Pointer<OStream> ostreamptr(POV_PLATFORM_BASE.CreateOStream(stype));
	unsigned int Flags = IOBase::none;

	if(ostreamptr == NULL)
		return NULL;

	if(sappend)
		Flags |= IOBase::append;

	if(ostreamptr->open(sname, Flags) == 0)
		return NULL;

	return ostreamptr.release();
}

/*****************************************************************************
*
* FUNCTION
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

int Has_Extension(const char *name)
{
   if (name!=NULL)
   {
     const char *p=strrchr(name, '.');

     if (p!=NULL)
     {
        if ((strlen(name)-(p-name))<=4)
        {
           return (true);
        }
     }
   }
   return (false);
}

void Split_Path(char *s, char *p, char *f)
{
  char *l;

  strcpy(p,s);

  if ((l=strrchr(p,FILENAME_SEPARATOR))==NULL)
  {
     if ((l=strrchr(p,DRIVE_SEPARATOR))==NULL)
     {
        strcpy(f,s);
        p[0]='\0';
        return;
     }
  }
  
  l++;
  strcpy(f,l);
  *l='\0';

}

bool File_Exist(const char *name)
{
  FILE *file = fopen(name, "r");

  if(file != NULL)
    fclose(file);
  else
    return false;

  return true;
}

END_POV_BASE_NAMESPACE
