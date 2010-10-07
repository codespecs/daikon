/****************************************************************************
 *                  fileinputoutput.h
 *
 * This module contains all defines, typedefs, and prototypes for fileinputoutput.cpp.
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
 * $File: //depot/povray/3.6-release/source/base/fileinputoutput.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef FILEINPUTOUTPUT_H
#define FILEINPUTOUTPUT_H

#include "configbase.h"

BEGIN_POV_BASE_NAMESPACE

#ifndef POV_SEEK_SET
	#define POV_SEEK_SET IOBase::seek_set
#endif

#ifndef POV_SEEK_CUR
	#define POV_SEEK_CUR IOBase::seek_cur
#endif

#ifndef POV_SEEK_END
	#define POV_SEEK_END IOBase::seek_end
#endif

enum
{
	POV_File_Unknown = 0,
	POV_File_Image_Targa = 1,
	POV_File_Image_PNG = 2,
	POV_File_Image_PPM = 3,
	POV_File_Image_PGM = 4,
	POV_File_Image_GIF = 5,
	POV_File_Image_IFF = 6,
	POV_File_Image_JPEG = 7,
	POV_File_Image_TIFF = 8,
	POV_File_Image_System = 9,
	POV_File_Text_POV = 10,
	POV_File_Text_INC = 11,
	POV_File_Text_Macro = POV_File_Text_INC, 
	POV_File_Text_INI = 12,
	POV_File_Text_CSV = 13,
	POV_File_Text_Stream = 14,
	POV_File_Text_User = 15,
	POV_File_Data_DF3 = 16,
	POV_File_Data_RCA = 17,
	POV_File_Data_LOG = 18,
	POV_File_Font_TTF = 19,
	POV_File_Unknown_Count = 20
};

typedef struct
{
  char *ext[4];
} POV_File_Extensions;

extern POV_File_Extensions gPOV_File_Extensions[];

class IOBase
{
	public:
		IOBase(unsigned int dir, unsigned int type);
		virtual ~IOBase();

		enum {none = 0, append = 1 };
		enum {input, output, io};
		enum {seek_set = SEEK_SET, seek_cur = SEEK_CUR, seek_end = SEEK_END};

		virtual bool open(const char *Name, unsigned int Flags = 0);
		virtual bool close(void);
		IOBase& read(void *buffer, unsigned long count);
		IOBase& write(void *buffer, unsigned long count);
		IOBase& seekg(unsigned long pos, unsigned int whence = seek_set);

		inline unsigned int gettype(void) { return(filetype); }
		inline unsigned int getdirection(void) { return(direction); }
		inline bool eof(void) { return(fail ? true : feof(f) != 0); }
		inline unsigned long tellg(void) { return(f == NULL ? -1 : ftell(f)); }
		inline IOBase& clearstate(void) { if(f != NULL) fail = false; return *this; }
		inline const char *Name(void) { return(filename); }

		inline operator void *() const { return(fail ? 0 :(void *) this); }
		inline bool operator!() const { return(fail); }
	protected:
		bool fail;
		FILE *f;
		IOBase& flush(void);
		unsigned int filetype;
		unsigned int direction;
		char *filename;
};

class IStream : public IOBase
{
	public:
		IStream(const unsigned int Type);
		virtual ~IStream();

		inline int Read_Byte(void) { return(fail ? EOF : fgetc(f)); }
		int Read_Short(void);
		int Read_Int(void);
		long Read_Long(void);
		inline IStream& Read_Byte(char& c) { c =(char) Read_Byte(); return *this; }
		inline IStream& Read_Byte(unsigned char& c) { c =(unsigned char) Read_Byte(); return *this; }
		inline IStream& Read_Short(short& n) { n =(short) Read_Short(); return *this; }
		inline IStream& Read_Short(unsigned short& n) { n =(unsigned short) Read_Short(); return *this; }
		inline IStream& Read_Int(int& n) { n = Read_Int(); return *this; }
		inline IStream& Read_Int(unsigned int& n) { n = Read_Int(); return *this; }
		inline IStream& Read_Long(long& n) { n = Read_Long(); return *this; }
		inline IStream& Read_Long(unsigned long& n) { n = Read_Long(); return *this; }

		inline IStream& operator>>(long& n) { read(&n, sizeof(n)); return *this; }
		inline IStream& operator>>(int& n) { read(&n, sizeof(n)); return *this; }
		inline IStream& operator>>(short& n) { read(&n, sizeof(n)); return *this; }
		inline IStream& operator>>(char& n) { read(&n, sizeof(n)); return *this; }
		inline IStream& operator>>(unsigned long& n) { read(&n, sizeof(n)); return *this; }
		inline IStream& operator>>(unsigned int& n) { read(&n, sizeof(n)); return *this; }
		inline IStream& operator>>(unsigned short& n) { read(&n, sizeof(n)); return *this; }
		inline IStream& operator>>(unsigned char& n) { read(&n, sizeof(n)); return *this; }
		IStream& UnRead_Byte(int c);
		IStream& getline(char *s, unsigned long buflen);
		IStream& ignore(unsigned long count) { seekg(count, seek_cur); return *this; }
};

class OStream : public IOBase
{
	public:
		OStream(const unsigned int Type);
		virtual ~OStream();

		void printf(const char *format, ...);

		inline OStream& Write_Byte(unsigned char data) { if(!fail) fail = fputc(data, f) != data; return *this; }
		inline OStream& Write_Short(unsigned short data) { write(&data, sizeof(data)); return *this; }
		inline OStream& Write_Int(unsigned int data) { write(&data, sizeof(data)); return *this; }
		inline OStream& Write_Long(unsigned long data) { write(&data, sizeof(data)); return *this; }
		inline OStream& flush(void) { IOBase::flush(); return *this; }

		inline OStream& operator<<(const char *s) { write((void *)s, strlen(s)); return *this; }
		inline OStream& operator<<(unsigned char *s) { return operator<<((char *) s); }
		inline OStream& operator<<(char c) { return(Write_Byte(c)); }
		inline OStream& operator<<(unsigned char c) { return operator <<((char) c); }
		inline OStream& operator<<(short n) { return(Write_Short(n)); }
		inline OStream& operator<<(unsigned short n) { return operator <<((short) n); }
		inline OStream& operator<<(int n) { return(Write_Int(n)); }
		inline OStream& operator<<(unsigned int n) { return operator <<((int) n); }
		inline OStream& operator<<(long n) { return(Write_Long(n)); }
		inline OStream& operator<<(unsigned long n) { return operator <<((long) n); }
};

IStream *New_IStream(const char *, const unsigned int);
OStream *New_OStream(const char *, const unsigned int, const bool);

int Has_Extension(const char *name);
void Split_Path(char *s, char *p, char *f);
bool File_Exist(const char *name);

END_POV_BASE_NAMESPACE

#endif
