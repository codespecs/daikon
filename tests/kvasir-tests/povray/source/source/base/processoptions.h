/****************************************************************************
 *               processoptions.h
 *
 * This module contains all defines, typedefs, and prototypes for the
 * C++ interface of processoptions.cpp.
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
 * $File: //depot/povray/3.6-release/source/base/processoptions.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef PROCESSOPTIONS_H
#define PROCESSOPTIONS_H

#include "configbase.h"

#include "povms.h"
#include "textstream.h"

BEGIN_POV_BASE_NAMESPACE

class ProcessOptions
{
	public:
		struct INI_Parser_Table
		{
			const char *keyword;
			POVMSType key;
			POVMSType type;
		};

		struct Cmd_Parser_Table
		{
			const char *command;
			POVMSType key;
			POVMSType type;
			POVMSType is_switch;
		};

		ProcessOptions(INI_Parser_Table *, Cmd_Parser_Table *);
		~ProcessOptions();

		int ParseFile(const char *, POVMSObjectPtr);
		int ParseString(const char *, POVMSObjectPtr, bool singleswitch = false);

		int WriteFile(const char *, POVMSObjectPtr);
		int WriteFile(OTextStream *, POVMSObjectPtr);
	protected:
		virtual int ReadSpecialOptionHandler(INI_Parser_Table *, char *, POVMSObjectPtr);
		virtual int ReadSpecialSwitchHandler(Cmd_Parser_Table *, char *, POVMSObjectPtr, bool);
		virtual int WriteSpecialOptionHandler(INI_Parser_Table *, POVMSObjectPtr, OTextStream *);
		virtual bool WriteOptionFilter(INI_Parser_Table *);
		virtual bool ProcessUnknownSwitch(char *, char *, POVMSObjectPtr);
		virtual int ProcessUnknownString(char *, POVMSObjectPtr);

		virtual ITextStream *OpenFileForRead(const char *, POVMSObjectPtr) = 0;
		virtual OTextStream *OpenFileForWrite(const char *, POVMSObjectPtr) = 0;

		virtual void ParseError(const char *, ...);
		virtual void ParseErrorAt(ITextStream *, const char *, ...);
		virtual void WriteError(const char *, ...);
	private:
		INI_Parser_Table *parse_ini_table;
		Cmd_Parser_Table *parse_cmd_table;

		int Output_INI_Option(INI_Parser_Table *, POVMSObjectPtr, OTextStream *);

		int Parse_INI_Specification(const char *, char *&, char *&);
		int Parse_INI_Skip_Space(ITextStream *, bool);
		int Parse_INI_Skip_Line(ITextStream *);
		int Parse_INI_Option(ITextStream *, POVMSObjectPtr);
		int Parse_INI_Switch(ITextStream *, int, POVMSObjectPtr);
		char *Parse_INI_String(ITextStream *, int endchr = -1, bool smartmode = false);
		bool Parse_INI_String_Smartmode(ITextStream *);

		int Parse_CL(char *, POVMSObjectPtr, bool);
		void Parse_CL_Skip_Space(const char *&);
		int Parse_CL_Switch(const char *&, int , POVMSObjectPtr, bool);
		int Parse_CL_Option(const char *&, POVMSObjectPtr, bool);
		char *Parse_CL_String(const char *&, int endchr = -1);

		int Process_INI_Option(INI_Parser_Table *, char *, POVMSObjectPtr);
		int Process_Switch(Cmd_Parser_Table *, char *, POVMSObjectPtr, bool);

		bool Matches(const char *, const char *);
		bool IsTrue(const char *);
		bool IsFalse(const char *);
};

END_POV_BASE_NAMESPACE

#endif
