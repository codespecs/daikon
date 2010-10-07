/****************************************************************************
 *               parsestr.cpp
 *
 * This module implements parsing and conversion of string expressions.
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
 * $File: //depot/povray/3.6-release/source/parsestr.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <stdlib.h>
#include <ctype.h>
#include "frame.h"
#include "userio.h"
#include "parse.h"
#include "parsestr.h"
#include "express.h"
#include "pov_mem.h"
#include "tokenize.h"
#include "povray.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local variables
******************************************************************************/

const unsigned char gUTF8SequenceArray[256] =
{
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5
};

const unsigned int gUTF8Offsets[6] =
{
	0x00000000UL,
	0x00003080UL,
	0x000E2080UL,
	0x03C82080UL,
	0xFA082080UL,
	0x82082080UL
};


/*****************************************************************************
* Local functions
******************************************************************************/

UCS2 *Parse_Str(bool pathname);
UCS2 *Parse_VStr(bool pathname);
UCS2 *Parse_Concat(bool pathname);
UCS2 *Parse_Chr(bool pathname);
UCS2 *Parse_Substr(bool pathname);
UCS2 *Parse_Strupr(bool pathname);
UCS2 *Parse_Strlwr(bool pathname);

UCS4 *Convert_UTF8_To_UCS4(unsigned char *text_array, int text_array_size, int *char_array_size);
UCS2 *Convert_UTF8_To_UCS2(unsigned char *text_array, int text_array_size, int *char_array_size);



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

char *Parse_C_String(bool pathname)
{
	UCS2 *str = Parse_String(pathname);
	char *New = UCS2_To_String(str, pathname);

	POV_FREE(str);

	return New;
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

UCS2 *Parse_String(bool pathname)
{
	UCS2 *New = NULL;
	int len = 0;

	EXPECT
		CASE(STRING_LITERAL_TOKEN)
			New = String_To_UCS2(Token.Token_String, pathname);
			EXIT
		END_CASE

		CASE(STR_TOKEN)
			New = Parse_Str(pathname);
			EXIT
		END_CASE

		CASE(VSTR_TOKEN)
			New = Parse_VStr(pathname);
			EXIT
		END_CASE

		CASE(CONCAT_TOKEN)
			New = Parse_Concat(pathname);
			EXIT
		END_CASE

		CASE(CHR_TOKEN)
			New = Parse_Chr(pathname);
			EXIT
		END_CASE

		CASE(SUBSTR_TOKEN)
			New = Parse_Substr(pathname);
			EXIT
		END_CASE

		CASE(STRUPR_TOKEN)
			New = Parse_Strupr(pathname);
			EXIT
		END_CASE

		CASE(STRLWR_TOKEN)
			New = Parse_Strlwr(pathname);
			EXIT
		END_CASE

		CASE(STRING_ID_TOKEN)
			len = UCS2_strlen((UCS2 *)(Token.Data)) + 1;
			New = (UCS2 *)POV_MALLOC(len * sizeof(UCS2), "UCS2 String");
			POV_MEMMOVE((void *)New, (void *)(Token.Data), len * sizeof(UCS2));
			EXIT
		END_CASE

		OTHERWISE
			Expectation_Error("string expression");
		END_CASE
	END_EXPECT

	return New;
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

UCS2 *Parse_Str(bool pathname)
{
	char *p;
	char temp3[128];
	char temp4[256];
	DBL val;
	int l, d;

	GET(LEFT_PAREN_TOKEN);
	val = Parse_Float();
	Parse_Comma();
	l = (int)Parse_Float();
	Parse_Comma();
	d = (int)Parse_Float();
	GET(RIGHT_PAREN_TOKEN);

	p = temp3;
	*(p++) = '%';
	if (l > 0)
	{
		sprintf(p, "%d", l);
		while (*p != '\0')
		   p++;
	}
	else
	{
		if (l)
		{
			sprintf(p, "0%d", abs(l));
		    while (*p != '\0')
		       p++;
		}
	}

	if (d >= 0)
	{
		*(p++) = '.';
		sprintf(p, "%d", d);
		while (*(++p))
			;
	}
	*(p++) = 'f';
	*p = '\0';

	sprintf(temp4, temp3, val);

	return String_To_UCS2(temp4, pathname);
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

UCS2 *Parse_VStr(bool pathname)
{
	char *p;
	char temp3[128];
	char temp4[768];
	int l, d, vl;
	EXPRESS Express;
	int Terms;
	int Dim = 5;
	UCS2 *str;
	UCS2 *str2;
	UCS2 *New;

	GET(LEFT_PAREN_TOKEN);

	vl = (int)Parse_Float();
	Parse_Comma();

	if(vl < 2)
		vl = 2;
	else if(vl > 5)
		vl = 5;
	Dim = vl;

	Terms = Parse_Unknown_Vector(Express);

	Parse_Comma();
	str = Parse_String(pathname);
	Parse_Comma();
	l = (int)Parse_Float();
	Parse_Comma();
	d = (int)Parse_Float();

	GET(RIGHT_PAREN_TOKEN);

	p = temp3;
	*(p++) = '%';
	if (l > 0)
	{
		sprintf(p, "%d", l);
		while (*p != '\0')
		   p++;
	}
	else
	{
		if (l)
		{
			sprintf(p, "0%d", abs(l));
		    while (*p != '\0')
		       p++;
		}
	}

	if (d >= 0)
	{
		*(p++) = '.';
		sprintf(p, "%d", d);
		while (*(++p))
			;
	}
	*(p++) = 'f';
	*p = '\0';

	sprintf(temp4, temp3, Express[X]);
	New = String_To_UCS2(temp4, pathname);       // add first component

	for(Terms = 1; Terms < Dim; Terms++)
	{
		New = UCS2_strcat(New, str);   // add separator
		sprintf(temp4, temp3, Express[Terms]);
		str2 = String_To_UCS2(temp4, pathname);
		New = UCS2_strcat(New, str2);  // add component
		POV_FREE(str2);
	}

	POV_FREE(str);

	return New;
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

UCS2 *Parse_Concat(bool pathname)
{
	UCS2 *str;
	UCS2 *New;

	GET(LEFT_PAREN_TOKEN);

	New = Parse_String();

	EXPECT
		CASE(RIGHT_PAREN_TOKEN)
			EXIT
		END_CASE

		OTHERWISE 
			UNGET
			Parse_Comma();
			str = Parse_String(pathname);
			New = UCS2_strcat(New, str);
			POV_FREE(str);
		END_CASE
	END_EXPECT

	return New;
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

UCS2 *Parse_Chr(bool /*pathname*/)
{
	UCS2 *New;
	int d;

	New = (UCS2 *)POV_MALLOC(sizeof(UCS2) * 2, "temporary string");

	d = (int)Parse_Float_Param();
	if((d < 0) || (d > 65535))
		Error("Value %d cannot be used in chr(...).", d);

	New[0] = d;
	New[1] = 0;

	return New;
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

UCS2 *Parse_Substr(bool pathname)
{
	UCS2 *str;
	UCS2 *New;
	int l, d;

	GET(LEFT_PAREN_TOKEN);

	str = Parse_String(pathname);
	Parse_Comma();
	l = (int)Parse_Float();
	Parse_Comma();
	d = (int)Parse_Float();

	GET(RIGHT_PAREN_TOKEN);

	if(((l + d - 1) > UCS2_strlen(str)) || (l < 0) || (d < 0))
		Error("Illegal parameters in substr.");

	New = (UCS2 *)POV_MALLOC(sizeof(UCS2) * (d + 1), "temporary string");
	UCS2_strncpy(New, &(str[l - 1]), d);
	New[d] = 0;

	POV_FREE(str);

	return New;
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

UCS2 *Parse_Strupr(bool pathname)
{
	UCS2 *New;

	GET(LEFT_PAREN_TOKEN);

	New = Parse_String(pathname);
	UCS2_strupr(New);

	GET(RIGHT_PAREN_TOKEN);

	return New;
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

UCS2 *Parse_Strlwr(bool pathname)
{
	UCS2 *New;

	GET(LEFT_PAREN_TOKEN);

	New = Parse_String(pathname);
	UCS2_strlwr(New);

	GET(RIGHT_PAREN_TOKEN);

	return New;
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

UCS2 *String_To_UCS2(char *str, bool pathname)
{
	UCS2 *char_string = NULL;
	UCS2 *char_array = NULL;
	int char_array_size = 0;
	int utf8arraysize = 0;
	unsigned char *utf8array = NULL;
	int index_in = 0;
	int index_out = 0;
	char buffer[8];
	char *dummy_ptr = NULL;
	int i = 0;

	if(strlen(str) == 0)
	{
		char_string = (UCS2 *)POV_MALLOC(sizeof(UCS2), "UCS2 String");
		char_string[0] = 0;

		return char_string;
	}

	switch(opts.String_Encoding)
	{
		case 0: // ASCII
			char_array_size = strlen(str);
			char_array = (UCS2 *)POV_MALLOC(char_array_size * sizeof(UCS2), "Character Array");
			for(i = 0; i < char_array_size; i++)
			{
				if(opts.Language_Version < 350)
					char_array[i] = (unsigned char)(str[i]);
				else
				{
					char_array[i] = str[i] & 0x007F;
					if(char_array[i] != str[i])
					{
						char_array[i] = ' ';
						PossibleError("Non-ASCII character has been replaced by space character.");
					}
				}
			}
			break;
		case 1: // UTF8
			char_array = Convert_UTF8_To_UCS2((unsigned char *)str, strlen(str), &char_array_size);
			break;
		case 2: // System Specific
			char_array = POV_CONVERT_TEXT_TO_UCS2((unsigned char *)str, strlen(str), &char_array_size);
			if(char_array == NULL)
				Error("Cannot convert system specific text format to Unicode.");
			break;
		default:
			Error("Unsupported text encoding format.");
			break;
	}

	if(char_array == NULL)
		Error("Cannot convert text to UCS2 format.");

	char_string = (UCS2 *)POV_MALLOC((char_array_size + 1) * sizeof(UCS2), "UCS2 String");
	for(index_in = 0, index_out = 0; index_in < char_array_size; index_in++, index_out++)
	{
		if((char_array[index_in] == '\\') && (pathname == false))
		{
			index_in++;

			switch(char_array[index_in])
			{
				case 'a':
					char_string[index_out] = 0x07;
					break;
				case 'b':
					char_string[index_out] = 0x08;
					break;
				case 'f':
					char_string[index_out] = 0x0c;
					break;
				case 'n':
					char_string[index_out] = 0x0a;
					break;
				case 'r':
					char_string[index_out] = 0x0d;
					break;
				case 't':
					char_string[index_out] = 0x09;
					break;
				case 'v':
					char_string[index_out] = 0x0b;
					break;
				case '\0':
					char_string[index_out] = 0x5c;
					break;
				case '\'':
					char_string[index_out] = 0x27;
					break;
				case '\\':
					char_string[index_out] = '\\';
					break;
				case 'u':
					if(index_in + 4 >= char_array_size)
						Error("Unexpected end of escape sequence in text string.");

					buffer[0] = char_array[++index_in];
					buffer[1] = char_array[++index_in];
					buffer[2] = char_array[++index_in];
					buffer[3] = char_array[++index_in];
					buffer[4] = 0;

					char_string[index_out] = (UCS2)strtoul(buffer, &dummy_ptr, 16);
					break;
				default:
					char_string[index_out] = char_array[index_in];
					if ( char_array )
						POV_FREE(char_array);
					char_array = NULL;
					Error( "Illegal escape sequence in string." );
					break;
			}
		}
		else
			char_string[index_out] = char_array[index_in];
	}

	char_string[index_out] = 0;
	index_out++;

	char_string = (UCS2 *)POV_REALLOC(char_string, index_out * sizeof(UCS2), "UCS2 String");

	if(char_array != NULL)
		POV_FREE(char_array);

	return char_string;
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

char *UCS2_To_String(UCS2 *str, bool)
{
	char *str_out;
	char *strp;

	str_out = (char *)POV_MALLOC(UCS2_strlen(str)+1, "C String");

	for(strp = str_out; *str != 0; str++, strp++)
	{
		if((*str > 127) && (opts.Language_Version >= 350))
			*strp = ' ';
		else
			*strp = (char)(*str);
	}

	*strp = 0;

	return str_out;
}


/*****************************************************************************
*
* FUNCTION
*
*   Convert_UTF8_To_UCS4
*
* INPUT
*
*   Array of bytes, length of this sequence
*
* OUTPUT
*
*   Size of the array of UCS4s returned
*
* RETURNS
*
*   Array of UCS4s (allocated with POV_MALLOC)
*
* AUTHOR
*
* DESCRIPTION
*
*   Converts UTF8 to UCS4 characters.
*
* CHANGES
*
*   -
*
******************************************************************************/

UCS4 *Convert_UTF8_To_UCS4(unsigned char *text_array, int text_array_size, int *char_array_size)
{
	UCS4 *char_array = NULL;
	UCS4 chr;
	int i, j, k, seqlen;

	char_array = (UCS4 *)POV_MALLOC(text_array_size * sizeof(UCS4), "Character Array");
	if((char_array == NULL) || (text_array == NULL) || (text_array_size == 0) || (char_array_size == NULL))
		return NULL;

	for(i = 0, k = 0; i < text_array_size; k++, i++)
	{
		seqlen = gUTF8SequenceArray[text_array[i]];
		chr = 0;
		for(j = seqlen; j > 0; j--)
		{
			chr += text_array[i];
			chr <<= 6;
			i++;
		}
		chr += text_array[i];

		char_array[k] = chr - gUTF8Offsets[seqlen];
	}

	char_array = (UCS4 *)POV_REALLOC(char_array, k * sizeof(UCS4), "Character Array");
	*char_array_size = k;

	return char_array;
}

/*****************************************************************************
*
* FUNCTION
*
*   Convert_UTF8_To_UCS2
*
* INPUT
*
*   Array of bytes, length of this sequence
*
* OUTPUT
*
*   Size of the array of UCS2s returned
*
* RETURNS
*
*   Array of UCS2s (allocated with POV_MALLOC)
*
* AUTHOR
*
* DESCRIPTION
*
*   Converts UTF8 to UCS2 characters, however all surrogates are dropped.
*
* CHANGES
*
*   -
*
******************************************************************************/

UCS2 *Convert_UTF8_To_UCS2(unsigned char *text_array, int text_array_size, int *char_array_size)
{
	UCS2 *char_array = NULL;
	UCS4 chr;
	int i, j, k, seqlen;

	char_array = (UCS2 *)POV_MALLOC(text_array_size * sizeof(UCS2), "Character Array");
	if((char_array == NULL) || (text_array == NULL) || (text_array_size == 0) || (char_array_size == NULL))
		return NULL;

	for(i = 0, k = 0; i < text_array_size; k++, i++)
	{
		seqlen = gUTF8SequenceArray[text_array[i]];
		chr = 0;
		for(j = seqlen; j > 0; j--)
		{
			chr += text_array[i];
			chr <<= 6;
			i++;
		}

		chr += text_array[i];
		chr -= gUTF8Offsets[seqlen];

		if(chr <= 0x0000FFFFUL)
			char_array[k] = chr;
		else
			char_array[k] = 0x0000FFFDUL;
	}

	char_array = (UCS2 *)POV_REALLOC(char_array, k * sizeof(UCS2), "Character Array");
	*char_array_size = k;

	return char_array;
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

UCS2 *UCS2_strcat(UCS2 *s1, UCS2 *s2)
{
	int l1, l2;

	l1 = UCS2_strlen(s1);
	l2 = UCS2_strlen(s2);

	s1 = (UCS2 *)POV_REALLOC(s1, sizeof(UCS2) * (l1 + l2 + 1), "UCS2 String");

	UCS2_strcpy(&s1[l1], s2);

	return s1;
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

int UCS2_strlen(UCS2 *str)
{
	register int i;

	for(i = 0; *str != 0; str++, i++) { }

	return i;
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

int UCS2_strcmp(UCS2 *s1, UCS2 *s2)
{
	UCS2 t1, t2;

	while((t1 = *s1++) == (t2 = *s2++))
	{
		if(t1 == 0)
			return 0;
	}

	return (t1 - t2);
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

void UCS2_strcpy(UCS2 *s1, UCS2 *s2)
{
	for(; *s2 != 0; s1++, s2++)
		*s1 = *s2;

	*s1 = 0;
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

void UCS2_strncpy(UCS2 *s1, UCS2 *s2, int n)
{
	for(; (*s2 != 0) && (n > 0); s1++, s2++, n--)
		*s1 = *s2;

	*s1 = 0;
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

void UCS2_strupr(UCS2 *str)
{
	bool err = false;

	while(true)
	{
		if (((int) *str < 0) || (*str > 127))
			err = true;
		else if(*str == 0)
			break;

		*str = toupper(*str);
		str++;
	}

	if(err == true)
		Warning(0, "Non-ASCII charcater in string, strupr may not work as expected.");
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

void UCS2_strlwr(UCS2 *str)
{
	bool err = false;

	while(true)
	{
		if (((int) *str < 0) || (*str > 127))
			err = true;
		else if(*str == 0)
			break;

		*str = tolower(*str);
		str++;
	}

	if(err == true)
		Warning(0, "Non-ASCII charcater in string, strlwr may not work as expected.");
}

END_POV_NAMESPACE
