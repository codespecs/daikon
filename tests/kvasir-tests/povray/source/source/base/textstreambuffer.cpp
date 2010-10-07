/****************************************************************************
 *               textstreambuffer.cpp
 *
 * This module contains the basic C++ text stream buffer.
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
 * $File: //depot/povray/3.6-release/source/base/textstreambuffer.cpp $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#include <algorithm>

#include "configbase.h"

#include "textstreambuffer.h"

#include "povms.h"
#include "pov_err.h"

BEGIN_POV_BASE_NAMESPACE

TextStreamBuffer::TextStreamBuffer(size_t buffersize, unsigned int wrapwidth)
{
	boffset = 0;
	bsize = buffersize;
	wrap = wrapwidth;
	curline = 0;
	if(POVMSUtil_TempAlloc((void **)&buffer, bsize) != kNoErr)
		throw int(kOutOfMemoryErr);
}

TextStreamBuffer::~TextStreamBuffer()
{
	boffset = 0;
	bsize = 0;
	wrap = 0;
	curline = 0;
	if(buffer != NULL)
		(void)POVMSUtil_TempFree((void *)buffer);
	buffer = NULL;
}

void TextStreamBuffer::printf(const char *format, ...)
{
	va_list marker;

	va_start(marker, format);
	vsnprintf(&buffer[boffset], bsize - boffset - 1, format, marker);
	va_end(marker);

	// direct output
	directflush(&buffer[boffset], strlen(&buffer[boffset]));

	boffset = strlen(buffer);

	// line buffered output
	lineflush();
}

void TextStreamBuffer::print(const char *str)
{
	printf("%s", str);
}

void TextStreamBuffer::puts(const char *str)
{
	printf("%s\n", str);
}

void TextStreamBuffer::putc(int chr)
{
	printf("%c", chr);
}

void TextStreamBuffer::printfile(const char *filename, unsigned long offset, int lines)
{
	FILE *file = fopen(filename, "r");

	if(file != NULL)
	{
		fseek(file, offset, SEEK_SET);
		printfile(file, lines);
		fclose(file);
	}
}

void TextStreamBuffer::printfile(FILE *file, int lines)
{
	if(file != NULL)
	{
		bool stopposset = (lines < 0); // only if walking backwards stop at current position
		long stoppos = long(ftell(file));
		int chr = 0;

		if(lines < 0)
		{
			int lineoffset = lines;

			// NOTE: This will walk back one line too far! However, it only walks
			// back to the end of that line. Thus, the next step will walk forward
			// again to the beginning of the right line, which is the desired
			// position. Do not change this behavior without testing! [trf]
			for(long pos = long(ftell(file)) - 1; (lineoffset < 1) && (pos >= 0); pos--)
			{
				// WARNING: Expensive way to walk backward through a file, but will only
				// be used when problems are encountered anyway, and then it most likely
				// does not matter if the output of the message takes a tiny bit longer!
				fseek(file, pos, SEEK_SET);

				chr = fgetc(file);

				if((chr == 10) || (chr == 13))
				{
					chr = fgetc(file);
					if(!((chr == 10) || (chr == 13)))
					 	ungetc(chr, file);
					lineoffset++;
				}
				else if(chr == EOF)
					break;
			}

			// beginning of file was previously reached
			if(lineoffset < 1)
				fseek(file, 0, SEEK_SET);

			while(lineoffset > 0)
			{
				chr = fgetc(file);

				if((chr == 10) || (chr == 13))
				{
					chr = fgetc(file);
					if(!((chr == 10) || (chr == 13)))
					 	ungetc(chr, file);
					lineoffset--;
				}
				else if(chr == EOF)
					break;
			}

			// make number of lines to output positive for next step
			lines = -lines;
		}

		while(lines > 0)
		{
			chr = fgetc(file);

			if((stopposset == true) && (stoppos == (long(ftell(file)) - 1))) // only if walking backwards stop at initial position
				break;

			// count newlines in file and replace newlines with system specific newline charcater
			if((chr == 10) || (chr == 13))
			{
				chr = fgetc(file);
				if(!((chr == 10) || (chr == 13)))
				 	ungetc(chr, file);
				 else
				 {
					if((stopposset == true) && (stoppos == (long(ftell(file)) - 1))) // only if walking backwards stop at initial position
						break;
				}
				printf("\n");
				lines--;
			}
			else if(chr == EOF)
				break;
			else
				printf("%c", chr);
		}
	}
}

void TextStreamBuffer::flush()
{
	if(curline > 0)
		directoutput("\n", 1);
	curline = 0;

	lineflush();
	if(boffset > 0)
		lineoutput(buffer, boffset);
	boffset = 0;
}

void TextStreamBuffer::lineoutput(const char *str, unsigned int chars)
{
	// by default output to stdout
	fwrite(str, sizeof(char), chars, stdout);
	fprintf(stdout, "\n");
	fflush(stdout);
}

void TextStreamBuffer::directoutput(const char *, unsigned int)
{
	// does nothing by default
}

void TextStreamBuffer::rawoutput(const char *, unsigned int)
{
	// does nothing by default
}

void TextStreamBuffer::lineflush()
{
	unsigned int lasti = 0;
	unsigned int ii = 0;
	unsigned int i = 0;

	// output all complete lines in the buffer
	while(i < boffset)
	{
		if((buffer[i] == '\n') || (buffer[i] == '\r'))
		{
			lineoutput(&buffer[lasti], i - lasti);
			lasti = i + 1;
		}
		else if(i - lasti >= wrap)
		{
			// track back to last space up to 1/4 in the line to wrap
			for(ii = 0; ii < min((wrap / 4), i); ii++)
			{
				if(isspace(buffer[i - ii]))
					break;
			}

			// if no space was found in the last 1/4 of the line to wrap, split it at the end anyway
			if(ii == min((wrap / 4), i))
				ii = 0;
			i -= ii;

			lineoutput(&buffer[lasti], i - lasti);
			lasti = i;
			continue;
		}
		i++;
	}

	// remove all completely output lines
	memmove(buffer, &buffer[lasti], bsize - lasti);
	boffset = boffset - lasti;
}

void TextStreamBuffer::directflush(const char *str, unsigned int chars)
{
	unsigned int ii = 0;
	unsigned int i = 0;

	rawoutput(str, chars);

	for(i = 0; i < chars; i++)
	{
		if((str[i] == '\n') || (str[i] == '\r'))
		{
			i++;
			directoutput(str, i);
			str += i;
			chars -= i;
			i = 0;
			curline = 0;
		}
		else if(curline + i >= wrap)
		{
			// track back to last space up to 1/4 in the line to wrap
			for(ii = 0; ii < min((wrap / 4), i); ii++)
			{
				if(isspace(str[i - ii]))
					break;
			}

			// if no space was found in the last 1/4 of the line to wrap, split it at the end anyway
			if(ii == min((wrap / 4), i))
				ii = 0;
			i -= ii;

			directoutput(str, i);
			directoutput("\n", 1);

			str += i;
			chars -= i;
			i = 0;
			curline = 0;
		}
	}

	if(chars > 0)
	{
		directoutput(str, chars);
		curline += chars;
	}
}

END_POV_BASE_NAMESPACE
