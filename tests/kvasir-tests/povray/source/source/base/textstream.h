/****************************************************************************
 *                  textstream.h
 *
 * This module contains all defines, typedefs, and prototypes for the
 * C++ interface of textstream.cpp.
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
 * $File: //depot/povray/3.6-release/source/base/textstream.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef TEXTSTREAM_H
#define TEXTSTREAM_H

#include <cstdio>

// must nuke these since everyone's favourite monopoly's cstdio still defines
// them for some reason (why not just use inlines like everyone else?)
#undef  getc
#undef  putc
#undef  getchar
#undef  putchar

#include "configbase.h"

#include "fileinputoutput.h"
#include "pov_err.h"

BEGIN_POV_BASE_NAMESPACE

const int ITEXTSTREAM_BUFFER_SIZE = DEFAULT_ITEXTSTREAM_BUFFER_SIZE;

class ITextStream
{
  public:
    struct FilePos
    {
      unsigned long offset;
      int lineno;
    };

    ITextStream(const char *, unsigned int);
    ITextStream(const char *, IStream *);
    virtual ~ITextStream();

    int getchar();
    void ungetchar(int);

    bool eof();
    bool seekg(FilePos);
    FilePos tellg();

    int line() { return lineno; };

    const char *name() { return filename; };
  private:
    IStream *stream;
    char buffer[ITEXTSTREAM_BUFFER_SIZE];
    unsigned long bufferoffset;
    unsigned long maxbufferoffset;
    unsigned long filelength;
    unsigned long curpos ;
    char *filename;
    int lineno;
    int ungetbuffer;

    void RefillBuffer();
};

class OTextStream
{
  public:
    OTextStream(const char *, unsigned int, bool append = false);
    OTextStream(const char *, OStream *);
    virtual ~OTextStream();

    void putchar(int);
    void printf(const char *, ...);

    const char *name() { return filename; };
  private:
    OStream *stream;
    char *filename;
};

END_POV_BASE_NAMESPACE

#endif
