/****************************************************************************
 *               processoptions.cpp
 *
 * This module contains the C++ interface for option processing.
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
 * $File: //depot/povray/3.6-release/source/base/processoptions.cpp $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#include <cstdarg>

#include "configbase.h"

#include "processoptions.h"
#include "stringutilities.h"
#include "povmsgid.h"
#include "pov_err.h"

BEGIN_POV_BASE_NAMESPACE

ProcessOptions::ProcessOptions(INI_Parser_Table *pit, Cmd_Parser_Table *pct)
{
	parse_ini_table = pit;
	parse_cmd_table = pct;
}

ProcessOptions::~ProcessOptions()
{
}

int ProcessOptions::ParseFile(const char *filespec, POVMSObjectPtr obj)
{
	ITextStream *file = NULL;
	const char *currentsection = NULL;
	char *sectionname = NULL;
	char *filename = NULL;
	int err = kNoErr;
	POVMSObjectPtr section = NULL;
	int currentline = 1;

	// split the INI files specification into filename and optional section name
	err = Parse_INI_Specification(filespec, filename, sectionname);
	if(err == kNoErr)
	{
		file = OpenFileForRead(filename, obj);
		if(file == NULL)
		{
			// all errors here are non-fatal, the calling code has to decide if an error is fatal
			ParseError("Cannot open INI file '%s'.", filename);
			err= kCannotOpenFileErr;
		}
	}

	if(err == kNoErr)
	{
		int token = 0;

		// apply options prior to any named section
		section = obj;

		// read the whole file to the end
		while((file->eof() == false) && (err == kNoErr))
		{
			currentline += Parse_INI_Skip_Space(file, true);

			token = file->getchar();

			// INI file section name
			if(token == '[')
			{
				// free old section name, if any
				if(currentsection != NULL)
					delete[] currentsection;

				// read until the section name end marker
				currentsection = Parse_INI_String(file, ']');

				// if the user specified a section name, compare the two and enable reading
				if((sectionname != NULL) && (currentsection != NULL))
				{
					if(pov_stricmp(currentsection, sectionname) == 0)
						section = obj; // named section matches specified section name, apply options
					else
						section = NULL; // named section does not match specified section name, ignore options
				}
				// if there was no user specified section name, ignore all named sections
				else
					section = NULL; // no section name was specified, ignore options in named section
			}
			// skip lines that do not belong to the desired sections
			else if(section == NULL)
			{
				currentline += Parse_INI_Skip_Line(file);
			}
			// fully process lines in desired sections
			else
			{
				switch(token)
				{
					// end of file
					case EOF:
						break;
					// quoted string
					case '\"':
					case '\'':
						err = kFalseErr;
						break;
					// POV-Ray-style INI file with command-line switch
					case '+':
					case '-':
					// POV-Ray-style INI file with system specific command-line switch on some systems (i.e. Windos)
					#if(FILENAME_SEPARATOR != '/')
					case '/':
					#endif
						err = Parse_INI_Switch(file, token, section);
						break;
					// INI file comment
					case ';':
					case '#':
						currentline += Parse_INI_Skip_Line(file);
						break;
					// INI file option
					default:
						if(isalnum(token) || (token == '_'))
						{
							file->ungetchar(token);
							ITextStream::FilePos backtrackpos = file->tellg();
							err = Parse_INI_Option(file, section);
							// only one option is allowed per line
							if(err == kNoErr)
								currentline += Parse_INI_Skip_Line(file);
							else if(err == kParseErr)
							{
								file->seekg(backtrackpos);
								err = kFalseErr;
							}
						}
						else
							err = kFalseErr;
						break;
				}

				// if nothing else was appropriate, assume it is some other kind of string requiring special attention
				if(err == kFalseErr)
				{
					char *plainstring = NULL;

					if((token == '\"') || (token == '\''))
						plainstring = Parse_INI_String(file, token, true);
					// if there were no quotes, just read up to the next space or newline
					else
						plainstring = Parse_INI_String(file, -1, true);

					err = ProcessUnknownString(plainstring, obj);

					if(plainstring != NULL)
						delete[] plainstring;
				}
			}
		}

		// all errors here are non-fatal, the calling code has to decide if an error is fatal
		if(err != kNoErr)
		{
			if(currentsection != NULL)
			{
				ParseErrorAt(file,
				             "Cannot continue to process INI file '%s' due to a parse error in line %d section '%s'.\n"
				             "This is not a valid INI file. Check the file for syntax errors, correct them, and try again!\n"
				             "Valid options in INI files are explained in detail in the reference part of the documentation.",
				             filename, currentline, currentsection);
			}
			else
			{
				ParseErrorAt(file,
				             "Cannot continue to process INI file '%s' due to a parse error in line %d.\n"
				             "This is not a valid INI file. Check the file for syntax errors, correct them, and try again!\n"
				             "Valid options in INI files are explained in detail in the reference part of the documentation.",
				             filename, currentline);
			}
		}

		if(currentsection != NULL)
			delete[] currentsection;
	}

	if(filename != NULL)
		delete[] filename;

	if(sectionname != NULL)
		delete[] sectionname;

	if(file != NULL)
		delete file;

	return err;
}

int ProcessOptions::ParseString(const char *commandline, POVMSObjectPtr obj, bool singleswitch)
{
	int err = kNoErr;

	// read the whole command-line to the end
	while((*commandline != 0) && (err == kNoErr))
	{
		if(singleswitch == false) // see if quotes had been stripped outside POV-Ray
			Parse_CL_Skip_Space(commandline);

		switch(*commandline)
		{
			// end of command-line
			case 0:
				break;
			// quoted string
			case '\"':
			case '\'':
				err = kFalseErr;
				break;
			// switch
			case '+':
			case '-':
			// system specific switch on some systems (i.e. Windos)
			#if(FILENAME_SEPARATOR != '/')
			case '/':
			#endif
				commandline++;
				err = Parse_CL_Switch(commandline, *(commandline - 1), obj, singleswitch);
				break;
			// INI file style option
			default:
				if(isalnum(*commandline) || (*commandline == '_'))
				{
					const char *cltemp = commandline;
					err = Parse_CL_Option(commandline, obj, singleswitch);
					if(err == kParseErr)
					{
						commandline = cltemp;
						err = kFalseErr;
					}
				}
				else
					err = kFalseErr;
				break;
		}

		// if nothing else was appropriate, assume it is some other kind of string requiring special attention
		if(err == kFalseErr)
		{
			int chr = *commandline;
			char *plainstring = NULL;

			if((chr == '\"') || (chr == '\''))
			{
				commandline++;

				plainstring = Parse_CL_String(commandline, chr);
			}
			// if there were no quotes, just read up to the next space or newline
			else if(singleswitch == false) // see if quotes had been stripped outside POV-Ray
				plainstring = Parse_CL_String(commandline);
			else
				plainstring = Parse_CL_String(commandline, 0);

			err = ProcessUnknownString(plainstring, obj);

			if(plainstring != NULL)
				delete[] plainstring;
		}
	}

	// all errors here are non-fatal, the calling code has to decide if an error is fatal
	if(err != kNoErr)
	{
		if(*commandline != 0)
		{
			ParseError("Cannot process command-line at '%s' due to a parse error.\n"
			           "This is not a valid command-line. Check the command-line for syntax errors, correct them, and try again!\n"
			           "Valid command-line switches are explained in detail in the reference part of the documentation.\n"
			           "To get a short list of command-line switches, use either the '-h', '-?', '-help' or '--help' switch.",
			           commandline);
		}
		else
		{
			ParseError("Cannot process command-line due to a parse error.\n"
			           "This is not a valid command-line. Check the command-line for syntax errors, correct them, and try again!\n"
			           "Valid command-line switches are explained in detail in the reference part of the documentation.\n"
			           "To get a short list of command-line switches, use either the '-h', '-?', '-help' or '--help' switch.");
		}
	}

	return err;
}

int ProcessOptions::WriteFile(OTextStream *ini_file, POVMSObjectPtr obj)
{
	struct INI_Parser_Table *table = parse_ini_table;

	// find the keyword
	while(table->keyword != NULL)
	{
		if(WriteOptionFilter(table) == true)
			Output_INI_Option(table, obj, ini_file);
		table++;
	}

	return kNoErr;
}

int ProcessOptions::WriteFile(const char *filename, POVMSObjectPtr obj)
{
	struct INI_Parser_Table *table = parse_ini_table;
	OTextStream *ini_file;
	int err = kNoErr;

	if(!POV_ALLOW_FILE_WRITE(filename, POV_File_Text_INI))
		return kCannotOpenFileErr;

	ini_file = OpenFileForWrite(filename, obj);
	if(ini_file == NULL)
		return kCannotOpenFileErr;
	err = WriteFile (ini_file, obj);
	delete ini_file;

	return err;
}

int ProcessOptions::ReadSpecialOptionHandler(INI_Parser_Table *, char *, POVMSObjectPtr)
{
	// do nothing by default
	return kNoErr;
}

int ProcessOptions::ReadSpecialSwitchHandler(Cmd_Parser_Table *, char *, POVMSObjectPtr, bool)
{
	// do nothing by default
	return kNoErr;
}

int ProcessOptions::WriteSpecialOptionHandler(INI_Parser_Table *, POVMSObjectPtr, OTextStream *)
{
	// do nothing by default
	return kNoErr;
}

bool ProcessOptions::WriteOptionFilter(INI_Parser_Table *)
{
	// filter nothing out by default
	return true;
}

bool ProcessOptions::ProcessUnknownSwitch(char *, char *, POVMSObjectPtr)
{
	// accept no unknown switches by default
	return false;
}

int ProcessOptions::ProcessUnknownString(char *, POVMSObjectPtr)
{
	// do nothing by default
	return kNoErr;
}

void ProcessOptions::ParseError(const char *format, ...)
{
	va_list marker;
	char error_buffer[1024];

	va_start(marker, format);
	vsnprintf(error_buffer, 1023, format, marker);
	va_end(marker);

	fprintf(stderr, "%s\n", error_buffer);
}

void ProcessOptions::ParseErrorAt(ITextStream *file, const char *format, ...)
{
	va_list marker;
	char error_buffer[1024];

	va_start(marker, format);
	vsnprintf(error_buffer, 1023, format, marker);
	va_end(marker);

	fprintf(stderr, "%s\nFile '%s' at line '%d'", error_buffer, file->name(), file->line());
}

void ProcessOptions::WriteError(const char *format, ...)
{
	va_list marker;
	char error_buffer[1024];

	va_start(marker, format);
	vsnprintf(error_buffer, 1023, format, marker);
	va_end(marker);

	fprintf(stderr, "%s\n", error_buffer);
}

int ProcessOptions::Output_INI_Option(INI_Parser_Table *option, POVMSObjectPtr obj, OTextStream *file)
{
	POVMSFloat floatval;
	POVMSBool b;
	POVMSInt intval;
	int err = 0;
	int l;
	POVMSAttribute item;
	char *bufptr;

	switch(option->type)
	{
		case kPOVMSType_Int:
			if(POVMSUtil_GetInt(obj, option->key, &intval) == 0)
				file->printf("%s=%d\n", option->keyword, (int)intval);
			break;
		case kPOVMSType_Float:
			if(POVMSUtil_GetFloat(obj, option->key, &floatval) == 0)
				file->printf("%s=%g\n", option->keyword, (float)floatval);
			break;
		case kPOVMSType_Bool:
			if(POVMSUtil_GetBool(obj, option->key, &b) == 0)
			{
				if(b == true)
					file->printf("%s=On\n", option->keyword);
				else
					file->printf("%s=Off\n", option->keyword);
			}
			break;
		case kPOVObjectClass_File:
			err = POVMSObject_Get(obj, &item, option->key);
			if(err != 0)
				break;
			// get the file name and path string
			l = 0;
			err = POVMSAttr_Size(&item, &l);
			if(l > 0)
			{
				bufptr = new char[l];
				bufptr[0] = 0;
				if(POVMSAttr_Get(&item, kPOVMSType_CString, bufptr, &l) == 0)
					file->printf("%s=\"%s\"\n", option->keyword, bufptr);
				delete[] bufptr;
			} 
			(void)POVMSAttr_Delete(&item);
			break;
		case kPOVMSType_WildCard:
			WriteSpecialOptionHandler(option, obj, file);
			break;
		default:
			WriteError("Ignoring unknown INI option.");
			break;
	}

	return err;
}

int ProcessOptions::Parse_INI_Specification(const char *filespec, char *&filename, char *&sectionname)
{
	const char *sectionpos = strchr(filespec, '[');

	// if there is no section string, this is the whole filename
	if(sectionpos == NULL)
	{
		filename = new char[strlen(filespec) + 1];
		strcpy(filename, filespec);
	}
	// if there is a section string, the filename ends at the section beginning
	else
	{
		const char *sectionend = strchr(filespec, ']');

		// if there was no section end, this file specification is invalid
		if(sectionend == NULL)
			return kParamErr;
		// if there valid section specification, use it
		else
		{
			// section string starts at sectionpos + 1 and has the length sectionend - sectionpos - 1 + terminating zero
			sectionname = new char[sectionend - sectionpos];
			strncpy(sectionname, sectionpos + 1, sectionend - sectionpos - 1);
			sectionname[sectionend - sectionpos - 1] = 0;

			// filename string ends at sectionpos and the the length sectionpos - filespec + terminating zero
			filename = new char[sectionpos - filespec + 1];
			strncpy(filename, filespec, sectionpos - filespec);
			filename[sectionpos - filespec] = 0;
		}
	}

	return kNoErr;
}

int ProcessOptions::Parse_INI_Skip_Space(ITextStream *file, bool countnewlines)
{
	int linecount = 0;

	// read until the end of file or until a non-space is found
	while(file->eof() == false)
	{
		int chr = file->getchar();

		// count newlinegets
		if((chr == 10) && (countnewlines == true))
		{
			int c2 = file->getchar();
			if(c2 != 13)
				file->ungetchar(c2);
			linecount++;
		}
		else if((chr == 13) && (countnewlines == true))
		{
			int c2 = file->getchar();
			if(c2 != 10)
				file->ungetchar(c2);
			linecount++;
		}
		// apart from a newline, spaces and tabs are considered "space"
		else if((chr != ' ') && (chr != '\t'))
		{
			file->ungetchar(chr);
			break;
		}
	}

	return linecount;
}

int ProcessOptions::Parse_INI_Skip_Line(ITextStream *file)
{
	int linecount = 0;

	// read until the end of file or until a newline is found
	while(file->eof() == false)
	{
		int chr = file->getchar();

		// count newlines
		if(chr == 10)
		{
			int c2 = file->getchar();
			if(c2 != 13)
				file->ungetchar(c2);
			linecount++;
			break;
		}
		else if(chr == 13)
		{
			int c2 = file->getchar();
			if(c2 != 10)
				file->ungetchar(c2);
			linecount++;
			break;
		}
	}

	return linecount;
}

int ProcessOptions::Parse_INI_Option(ITextStream *file, POVMSObjectPtr obj)
{
	struct INI_Parser_Table *table = parse_ini_table;
	char *value = NULL;
	char *key = NULL;
	char chr = 0;
	int err = kNoErr;

	// read the key string
	key = Parse_INI_String(file);
	if(key == NULL)
	{
		ParseErrorAt(file, "Expected key in INI file, no key was found.");
		return kParseErr;
	}

	// find the keyword
	while(table->keyword != NULL)
	{
		if(pov_stricmp(table->keyword, key) == 0)
			break;
		table++;
	}

	// return if no valid keyword has been found
	if(table->keyword == NULL)
	{
		ParseErrorAt(file, "Unknown key '%s' in INI file.", key);
		delete[] key;
		return kParseErr;
	}
	else
	{
		delete[] key;
		key = NULL;
	}

	// skip any spaces
	(void)Parse_INI_Skip_Space(file, false);

	// expect the equal sign
	if(file->getchar() != '=')
		return kParseErr;

	// skip any spaces
	(void)Parse_INI_Skip_Space(file, false);

	// if the string is quoted, parse it matching quotes
	chr = file->getchar();
	if((chr == '\"') || (chr == '\''))
		value = Parse_INI_String(file, chr);
	// if there were no quotes, just read up to the next space or newline
	else
	{
		file->ungetchar(chr);
		value = Parse_INI_String(file, -2, true);
	}

	if(value == NULL)
	{
		ParseErrorAt(file, "Expected value in INI file, no value was found.");
		return kParseErr;
	}

	err = Process_INI_Option(table, value, obj);
	delete[] value;
	value = NULL;

	// skip any spaces
	(void)Parse_INI_Skip_Space(file, false);

	// if there is a comma, parse more values and append them
	chr = file->getchar();
	if(chr == ',')
	{
		// read more value strings
		while((file->eof() == false) && (err == kNoErr))
		{
			// skip any spaces
			(void)Parse_INI_Skip_Space(file, false);

			// if the string is quoted, parse it matching quotes
			chr = file->getchar();
			if((chr == '\"') || (chr == '\''))
				value = Parse_INI_String(file, chr);
			// if there were no quotes, just read up to the next space or newline
			else
			{
				file->ungetchar(chr);
				value = Parse_INI_String(file, -2);
			}

			if(value == NULL)
			{
				ParseErrorAt(file, "Expected value in INI file, no value was found.");
				return kParseErr;
			}

			err = Process_INI_Option(table, value, obj);
			delete[] value;
			value = NULL;

			// skip any spaces
			(void)Parse_INI_Skip_Space(file, false);

			// if there is no other comma, stop parsing values
			chr = file->getchar();
			if(chr != ',')
			{
				file->ungetchar(chr);
				break;
			}
		}
	}
	else
		file->ungetchar(chr);

	return err;
}

int ProcessOptions::Parse_INI_Switch(ITextStream *file, int token, POVMSObjectPtr obj)
{
	struct Cmd_Parser_Table *table = parse_cmd_table;
	char *value = NULL;
	char *key = NULL;
	int err = kNoErr;
	int chr = 0;

	// read the switch string
	key = Parse_INI_String(file);
	if(key == NULL)
	{
		ParseErrorAt(file, "Expected command-line switch in INI file, no command-line switch was found.");
		err = kParseErr;
	}
	else
	{
		// if there is a quoted string directory following the switch, parse it matching quotes
		chr = file->getchar();
		if((chr == '\"') || (chr == '\''))
		{
			char *value = Parse_INI_String(file, chr);
			if(value == NULL)
				ParseErrorAt(file, "Expected command-line switch in INI file to be followed by quoted parameter.");
		}
		else
			file->ungetchar(chr);

		// find the command-line switch
		while(table->command != NULL)
		{
			char *srcptr = key;
			const char *dstptr = table->command;

			// compared ignoring case until the end of either string has been reached
			while((toupper(*srcptr) == toupper(*dstptr)) && (*srcptr != 0) && (*dstptr != 0))
			{
				srcptr++;
				dstptr++;
			}
			// if the end of the switch string in the table had been reached, see if there are parameters
			// to consider and if there are, expect the source string to be followed by those parameters
			if((*dstptr) == 0)
			{
				// if there was a quoted value string and the switch string is longer, this is an unknown switch
				if((value != NULL) && (*srcptr != 0))
				{
					table = NULL;
					break;
				}
				// if there was a quoted value string and the switch matches, use the value string as parameter
				else if((value != NULL) && (*srcptr == 0))
					srcptr = value;

				// only if a paremeter is expected allow it, and vice versa
				if(((*srcptr > ' ') && (table->type != kPOVMSType_Null)) || ((*srcptr <= ' ') && (table->type == kPOVMSType_Null)))
				{
					err = Process_Switch(table, srcptr, obj, (token != '-'));
					break;
				}
			}
			table++;
		}

		// if there was no sucessful match so far, see if it is a system specific switch
		if(table == NULL)
		{
			if(ProcessUnknownSwitch(key, value, obj) == false)
			{
				if(value != NULL)
					ParseErrorAt(file, "Unknown switch '%s' with value '%s' in INI file.", key, value);
				else
					ParseErrorAt(file, "Unknown switch '%s' in INI file.", key);
				err = kParseErr;
			}
			else
				err = kNoErr;
		}
	}

	if(key != NULL)
		delete[] key;
	if(value != NULL)
		delete[] value;

	return err;
}

char *ProcessOptions::Parse_INI_String(ITextStream *file, int endchr, bool smartmode)
{
	char *str = new char[65536];
	char *pos = str;

	while((pos - str) < 65535)
	{
		int chr = file->getchar();
		// terminate the string if the end of file has been reached
		if(chr == EOF)
			break;
		// parse to the next space or special token
		else if((endchr == -1) || (endchr == -2))
		{
			if((smartmode == true) && ((chr == ' ') || (chr == '\t')))
			{
				// In "smart mode" the function called below tries to detect if the
				// user is trying to use an unquoted path as value of an INI option.
				// The detection logic is detailed in the function below!
				file->ungetchar(chr);
				if(Parse_INI_String_Smartmode(file) == false)
					break;
				else
				{
					chr = file->getchar();
					endchr = -3; // switch to special mode
				}
			}
			else if(isspace(chr) || (chr == ',') || (chr == ';') || (chr == '#') || (chr == '\"') || (chr == '\'') ||
			        ((endchr == -1) && ((chr == '[') || (chr == ']') || (chr == '='))))
			{
				file->ungetchar(chr);
				break;
			}
		}
		// this should only be switched on by "smart mode" and parses to either  the end of the line or the comment string
		else if(endchr == -3)
		{
			if((chr == ';') || (chr == '#') || (chr == 10) || (chr == 13))
			{
				file->ungetchar(chr);
				break;
			}
		}
		// parse to the next character specified by the caller in endchr
		else if(chr == endchr)
			break;

		*pos = chr;
		pos++;
	}

	*pos = 0;

	return str;
}

bool ProcessOptions::Parse_INI_String_Smartmode(ITextStream *file)
{
	ITextStream::FilePos backtrackpos = file->tellg();
	bool result = false; // false - end string here, true - continue parsing string
	struct INI_Parser_Table *table = parse_ini_table;
	char *key = NULL;

	(void)Parse_INI_Skip_Space(file, false);

	switch(file->getchar())
	{
		// end of file
		case EOF:
			break; // return false, parsing more of the string simply is not possible
		// INI file comment
		case ';':
		case '#':
			break; // return false, this is a comment which terminates the string
		// INI value list separator 
		case ',':
			break; // return false, this is a value list of unquoted strings
		// POV-Ray-style INI file with command-line switch
		case '+':
		case '-':
		// POV-Ray-style INI file with system specific command-line switch on some systems (i.e. Windos)
		#if(FILENAME_SEPARATOR != '/')
		case '/':
		#endif
			if(isalpha(file->getchar()))
				break; // return false, this is most likely a command-line
			else
				file->seekg(backtrackpos); // most likely an unquoted string, so allow parsing it as a whole
		// INI file option
		default:
			// read the key string
			key = Parse_INI_String(file);
			if(key != NULL)
			{
				// find the keyword
				while(table->keyword != NULL)
				{
					if(pov_stricmp(table->keyword, key) == 0)
						break;
					table++;
				}

				// if no valid keyword has been found
				if(table->keyword == NULL)
				{
					result = true; // return true, this is most likely an unquoted path
					ParseErrorAt(file,
					             "Most likely detected an unquoted string with spaces in INI file. Assuming string ends at the of the line.\n"
					             "Make sure all strings with spaces are properly quoted in the INI file.\n"
					             "Use either \" or \' to quote strings. For details, please check the user manual!");
				}

				delete[] key;
				key = NULL;
			}
			break; // return false, unless the code above did not find a valid keyword
	}

	file->seekg(backtrackpos);

	return result;
}

void ProcessOptions::Parse_CL_Skip_Space(const char *&commandline)
{
	// read until the end of the string or until a non-space is found
	while(*commandline != 0)
	{
		// spaces and tabs are considered "space"
		if((*commandline != ' ') && (*commandline != '\t'))
			break;
		commandline++;
	}
}

int ProcessOptions::Parse_CL_Switch(const char *&commandline, int token, POVMSObjectPtr obj, bool singleswitch)
{
	struct Cmd_Parser_Table *table = parse_cmd_table;
	char *value = NULL;
	char *key = NULL;
	int err = kNoErr;
	int chr = 0;

	// read the switch string
	if(singleswitch == false) // see if quotes had been stripped outside POV-Ray
		key = Parse_CL_String(commandline);
	else
		key = Parse_CL_String(commandline, 0);
	if(key == NULL)
	{
		ParseError("Expected command-line switch on command-line, no command-line switch was found.");
		err = kParseErr;
	}
	else
	{
		// if there is a quoted string directly following the switch, parse its matching quotes
		chr = *commandline;
		commandline++;
		if((chr == '\"') || (chr == '\''))
		{
			value = Parse_CL_String(commandline, chr);
			if(value == NULL)
				ParseError("Expected command-line switch on command-line to be followed by quoted parameter.");
		}
		else
			commandline--;

		// find the command-line switch
		while(table->command != NULL)
		{
			char *srcptr = key;
			const char *dstptr = table->command;

			// compared ignoring case until the end of either string has been reached
			while((toupper(*srcptr) == toupper(*dstptr)) && (*srcptr != 0) && (*dstptr != 0))
			{
				srcptr++;
				dstptr++;
			}
			// if the end of the switch string in the table had been reached, see if there are parameters
			// to consider and if there are, expect the source string to be followed by those parameters
			if((*dstptr) == 0)
			{
				// if there was a quoted value string and the switch string is longer, this is an unknown switch
				if((value != NULL) && (*srcptr != 0))
				{
					table = NULL;
					break;
				}
				// if there was a quoted value string and the switch matches, use the value string as parameter
				else if((value != NULL) && (*srcptr == 0))
					srcptr = value;

				// only if a paremeter is expected allow it, and vice versa
				if(((*srcptr > ' ') && (table->type != kPOVMSType_Null)) || ((*srcptr <= ' ') && (table->type == kPOVMSType_Null)))
				{
					err = Process_Switch(table, srcptr, obj, (token != '-'));
					break;
				}
			}
			table++;
		}

		// if there was no successful match so far, see if it is a system specific switch
		if(table == NULL)
		{
			if(ProcessUnknownSwitch(key, value, obj) == false)
			{
				if(value != NULL)
					ParseError("Unknown switch '%s' with value '%s' on command-line.", key, value);
				else
					ParseError("Unknown switch '%s' on command-line.", key);
				err = kParseErr;
			}
			else
				err = kNoErr;
		}
	}

	if(key != NULL)
		delete[] key;
	if(value != NULL)
		delete[] value;

	return err;
}

int ProcessOptions::Parse_CL_Option(const char *&commandline, POVMSObjectPtr obj, bool singleswitch)
{
	struct INI_Parser_Table *table = parse_ini_table;
	char *value = NULL;
	char *key = NULL;
	char chr = 0;
	int err = kNoErr;

	// read the key string
	key = Parse_CL_String(commandline);
	if(key == NULL)
	{
		ParseError("Expected INI file key on command-line, no key was found.");
		return kParseErr;
	}

	// find the keyword
	while(table->keyword != NULL)
	{
		if(pov_stricmp(table->keyword, key) == 0)
			break;
		table++;
	}

	// return false if no valid keyword has been found
	if(table->keyword == NULL)
	{
		delete[] key;
		return kParseErr;
	}
	else
	{
		delete[] key;
		key = NULL;
	}

	// expect the equal sign
	if(*commandline != '=')
		return kParseErr;
	commandline++;

	// if the string is quoted, parse it matching quotes
	chr = *commandline;
	if((chr == '\"') || (chr == '\''))
	{
		commandline++;

		value = Parse_CL_String(commandline, chr);
	}
	// if there were no quotes, just read up to the next space or newline
	else if(singleswitch == false) // see if quotes had been stripped outside POV-Ray
		value = Parse_CL_String(commandline, -2);
	else
		value = Parse_CL_String(commandline, 0);

	if(value == NULL)
	{
		ParseError("Expected value on command-line, no value was found.");
		return kParseErr;
	}

	err = Process_INI_Option(table, value, obj);
	delete[] value;
	value = NULL;

	return err;
}

char *ProcessOptions::Parse_CL_String(const char *&commandline, int endchr)
{
	int maxlen = strlen(commandline) + 1;
	char *str = new char[maxlen];
	char *pos = str;

	while(*commandline != 0)
	{
		int chr = *commandline;

		if(endchr <= -1)
		{
			if(isspace(chr) || (chr == ';') || (chr == '#') || (chr == '\"') || (chr == '\''))
				break;
			else if((endchr == -1) && ((chr == '[') || (chr == ']') || (chr == '=')))
				break;
		}

		commandline++;

		if(chr == endchr)
			break;

		*pos = chr;
		pos++;
	}

	*pos = 0;

	return str;
}

int ProcessOptions::Process_INI_Option(INI_Parser_Table *option, char *param, POVMSObjectPtr obj)
{
	double floatval = 0.0;
	int intval = 0;
	int intval2 = 0;
	int err = kNoErr;

	switch(option->type)
	{
		case kPOVMSType_Int:
			if(sscanf(param, "%d", &intval) == 1)
				err = POVMSUtil_SetInt(obj, option->key, intval);
			else
			{
				ParseError("Integer parameter expected for option '%s', found '%s'.", option->keyword, param);
				err = kParseErr;
			}
			break;
		case kPOVMSType_Float:
			if(sscanf(param, "%lf", &floatval) == 1)
				err = POVMSUtil_SetFloat(obj, option->key, floatval);
			else
			{
				ParseError("Floating-point parameter expected for option '%s', found '%s'.", option->keyword, param);
				err = kParseErr;
			}
			break;
		case kPOVMSType_Bool:
			err = POVMSUtil_SetBool(obj, option->key, IsTrue(param));
			break;
		case kPOVObjectClass_File:
			// make the file object
			if(err == kNoErr)
				err = POVMSUtil_SetString(obj, option->key, param);
			else
			{
				ParseError("File name or path parameter expected for option '%s', found '%s'.", option->keyword, param);
				err = kParseErr;
			}
			break;
		case kPOVMSType_WildCard:
			err = ReadSpecialOptionHandler(option, param, obj);
			break;
		default:
			err = kParseErr;
			break;
	}

	return err;
}

int ProcessOptions::Process_Switch(Cmd_Parser_Table *option, char *param, POVMSObjectPtr obj, bool is_on)
{
	double floatval = 0.0;
	int intval = 0;
	int intval2 = 0;
	int err = 0;
	char chr = 0;

	if(option->is_switch != kPOVMSType_Null)
	{
		err = POVMSUtil_SetBool(obj, option->is_switch, is_on);
		if(err != kNoErr)
			return err;
	}

	switch(option->type)
	{
		case kPOVMSType_Int:
			if(sscanf(param, "%d", &intval) == 1)
				err = POVMSUtil_SetInt(obj, option->key, intval);
			else
			{
				ParseError("Integer parameter expected for switch '%s', found '%s'.", option->command, param);
				err = kParseErr;
			}
			break;
		case kPOVMSType_Float:
			if(sscanf(param, "%lf", &floatval) == 1)
				err = POVMSUtil_SetFloat(obj, option->key, floatval);
			else
			{
				ParseError("Floating-point parameter expected for switch '%s', found '%s'.", option->command, param);
				err = kParseErr;
			}
			break;
		case kPOVMSType_Bool:
			err = POVMSUtil_SetBool(obj, option->key, IsTrue(param));
			break;
		case kPOVObjectClass_File:
			// make the file object
			if(err == kNoErr)
				err = POVMSUtil_SetString(obj, option->key, param);
			else
			{
				ParseError("File name or path parameter expected for switch '%s', found '%s'.", option->command, param);
				err = kParseErr;
			}
			break;
		case kPOVMSType_WildCard:
			err = ReadSpecialSwitchHandler(option, param, obj, is_on);
			break;
		case kPOVMSType_Null:
			break;
		default:
			err = kParseErr;
			break;
	}

	return err;
}

bool ProcessOptions::Matches(const char *v1, const char *v2)
{
   int i = 0;
   int ans = 1;

   while((ans) && (v1[i] != 0) && (v2[i] != 0))
   {
      ans = ans && (int)(v1[i] == tolower(v2[i]));
      i++;
   }

   return (ans != 0);
}

bool ProcessOptions::IsTrue(const char *value)
{
   return (Matches("on",value)  || Matches("true",value) || 
           Matches("yes",value) || Matches("1",value));
}

bool ProcessOptions::IsFalse(const char *value)
{
   return (Matches("off",value)  || Matches("false",value) || 
           Matches("no",value)   || Matches("0",value));
}

END_POV_BASE_NAMESPACE
