/****************************************************************************
 *               textstream.cpp
 *
 * This module implements the classes handling text file input and output.
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
 * $File: //depot/povray/3.6-release/source/base/textstream.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <cstdlib>
#include <cstdarg>
#include <algorithm>

#include "configbase.h"

#include "textstream.h"

BEGIN_POV_BASE_NAMESPACE

ITextStream::ITextStream(const char *sname, unsigned int stype)
{
  if(sname == NULL)
    throw int(kParamErr);

  stream = New_IStream(sname, stype);
  if(stream == NULL)
    throw int(kCannotOpenFileErr);

  filename = new char[strlen(sname) + 1];
  strcpy(filename, sname);
  lineno = 1;
  bufferoffset = 0;
  maxbufferoffset = 0;
  filelength = 0;
  ungetbuffer = EOF;
  curpos = 0 ;

  stream->seekg(0, IOBase::seek_end);
  filelength = stream->tellg();
  stream->seekg(0, IOBase::seek_set);

  RefillBuffer();
}

ITextStream::ITextStream(const char *sname, IStream *sstream)
{
  if(sname == NULL)
    throw int(kParamErr);
  if(sstream == NULL)
    throw int(kParamErr);

  stream = sstream;
  filename = new char[strlen(sname) + 1];
  strcpy(filename, sname);
  lineno = 1;
  bufferoffset = 0;
  maxbufferoffset = 0;
  filelength = 0;
  ungetbuffer = EOF;
  curpos = 0 ;

  stream->seekg(0, IOBase::seek_end);
  filelength = stream->tellg();
  stream->seekg(0, IOBase::seek_set);

  RefillBuffer();
}

ITextStream::~ITextStream()
{
  delete[] filename;
  filename = NULL;
  delete stream;
  stream = NULL;
}

int ITextStream::getchar()
{
  int chr = 0;

  if(ungetbuffer != EOF)
  {
    chr = ungetbuffer;
    ungetbuffer = EOF;
  }
  else
  {
    if(bufferoffset >= maxbufferoffset)
      chr = EOF;
    else
    {
      chr = buffer[bufferoffset];
      bufferoffset++;
    }
  }

  if(((chr == 10) || (chr == 13)) && (bufferoffset >= maxbufferoffset))
    RefillBuffer();

  if(chr == 10)
  {
    if(buffer[bufferoffset] == 13)
      bufferoffset++;
    chr = '\n';
    lineno++;
  }
  else if(chr == 13)
  {
    if(buffer[bufferoffset] == 10)
      bufferoffset++;
    chr = '\n';
    lineno++;
  }

  if(bufferoffset >= maxbufferoffset)
    RefillBuffer();

  return chr;
}

void ITextStream::ungetchar(int chr)
{
  ungetbuffer = chr;
  if(chr == '\n')
    lineno--;
}

bool ITextStream::eof()
{
  if(ungetbuffer != EOF)
    return false;
  if(bufferoffset >= maxbufferoffset)
    return true;
  return stream->eof();
}

bool ITextStream::seekg(ITextStream::FilePos fp)
{
  bool result = true;

  if((fp.offset < curpos) && ((curpos - fp.offset) < maxbufferoffset))
  {
    bufferoffset = maxbufferoffset - (curpos - fp.offset);
    lineno = fp.lineno;
    ungetbuffer = EOF;
  }
  else
  {
    result = (stream->seekg(fp.offset) != 0);

    if(result == true)
    {
      lineno = fp.lineno;

      bufferoffset = 0;
      maxbufferoffset = 0;
      ungetbuffer = EOF;
      curpos = fp.offset ;

      RefillBuffer();
    }
    else
      curpos = stream->tellg() ;
  }

  return result;
}

ITextStream::FilePos ITextStream::tellg()
{
  FilePos fp;

  fp.lineno = lineno;
  fp.offset = curpos - (maxbufferoffset - bufferoffset);

  if(ungetbuffer != EOF)
    fp.offset--;

  return fp;
}

void ITextStream::RefillBuffer()
{
  if(bufferoffset < maxbufferoffset)
  {
    curpos -= (maxbufferoffset - bufferoffset);
    stream->seekg(curpos, IOBase::seek_set);
  }

  maxbufferoffset = min((unsigned long)ITEXTSTREAM_BUFFER_SIZE, filelength - curpos);
  bufferoffset = 0;

  stream->read(buffer, maxbufferoffset);
  if (*stream)
    curpos += maxbufferoffset ;
  else
    curpos = stream->tellg() ;
}

OTextStream::OTextStream(const char *sname, unsigned int stype, bool append)
{
  if(sname == NULL)
    throw int(kParamErr);

  stream = New_OStream(sname, stype, append);
  if(stream == NULL)
    throw int(kCannotOpenFileErr);

  filename = new char[strlen(sname) + 1];
  strcpy(filename, sname);
}

OTextStream::OTextStream(const char *sname, OStream *sstream)
{
  if(sname == NULL)
    throw int(kParamErr);
  if(sstream == NULL)
    throw int(kParamErr);

  stream = sstream;
  filename = new char[strlen(sname) + 1];
  strcpy(filename, sname);
}

OTextStream::~OTextStream()
{
  delete[] filename;
  filename = NULL;
  delete stream;
  stream = NULL;
}

void OTextStream::putchar(int chr)
{
#ifdef TEXTSTREAM_CRLF
  if (chr == '\n')
    stream->Write_Byte('\r');
#endif

  stream->Write_Byte((unsigned char)chr);
}

void OTextStream::printf(const char *format, ...)
{
  va_list marker;
  char buffer[1024];

  va_start(marker, format);
  vsnprintf(buffer, 1023, format, marker);
  va_end(marker);

#ifdef TEXTSTREAM_CRLF
  char *s1 = buffer ;
  char *s2 ;

  while ((s2 = strchr (s1, '\n')) != NULL)
  {
    *s2++ = '\0' ;
    stream->printf("%s\r\n", s1); 
    s1 = s2 ;
  }
  if (*s1)
    stream->printf("%s", s1); 
#else
  stream->printf("%s", buffer); 
#endif
}

END_POV_BASE_NAMESPACE
