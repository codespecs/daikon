/****************************************************************************
 *                  tokenize.h
 *
 * This module contains all defines, typedefs, and prototypes for TOKENIZE.CPP.
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
 * $File: //depot/povray/3.6-release/source/tokenize.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef TOKENIZE_H
#define TOKENIZE_H

#include "frame.h"
#include "povms.h"
#include "textstream.h"

BEGIN_POV_NAMESPACE

USING_POV_BASE_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define MAX_NUMBER_OF_TABLES 100



/*****************************************************************************
* Global typedefs
******************************************************************************/


struct Token_Struct
{
  TOKEN Token_Id;
  TOKEN Function_Id;
  POV_BASE_NAMESPACE::ITextStream::FilePos Token_File_Pos;
  int Token_Col_No,Table_Index;
  char *Token_String;
  DBL Token_Float;
  int Unget_Token, End_Of_File;
  POV_BASE_NAMESPACE::ITextStream *FileHandle;
  void *Data;
  int *NumberPtr;
  void **DataPtr;
  bool is_array_elem;
};

#define MAX_PARAMETER_LIST 56

typedef struct Pov_Macro_Struct POV_MACRO;

struct Pov_Macro_Struct
{
  char *Macro_Name;
  char *Macro_Filename;
  POV_BASE_NAMESPACE::ITextStream::FilePos Macro_File_Pos;
  long Macro_End;
  int Num_Of_Pars;
  char *Par_Name[MAX_PARAMETER_LIST];
};

typedef struct Pov_Array_Struct POV_ARRAY;

struct Pov_Array_Struct
{
   int Dims, Type, Total;
   int Sizes[5];
   int Mags[5];
   void **DataPtrs;
};

typedef struct Pov_Param_Struct POV_PARAM;

struct Pov_Param_Struct
{
   int *NumberPtr;
   void **DataPtr;
   int Table_Index;
};

typedef struct Data_File_Struct DATA_FILE;

struct Data_File_Struct
{
  POV_BASE_NAMESPACE::ITextStream *In_File;
  POV_BASE_NAMESPACE::OTextStream *Out_File;
  bool R_Flag;
};


/*****************************************************************************
* Global variables
******************************************************************************/

extern struct Token_Struct Token;

extern const struct Reserved_Word_Struct Reserved_Words [LAST_TOKEN];
extern int Table_Index;
extern int token_count; // WARNING: This variable has very little to do with counting tokens! [trf]
extern POV_LONG Current_Token_Count; // This variable really counts tokens! [trf]


/*****************************************************************************
* Global functions
******************************************************************************/

void Get_Token (void);
void Unget_Token (void);
void Parse_String_Literal(void);
void Where_Error (POVMSObjectPtr msg);
void Where_Warning (POVMSObjectPtr msg);
void Parse_Directive (int After_Hash);
void Open_Include (void);
void IncludeHeader(char *temp);
void pre_init_tokenizer (void);
void Initialize_Tokenizer (void);
void Terminate_Tokenizer (void);
SYM_ENTRY *Add_Symbol (int Index,char *Name,TOKEN Number);
void Destroy_Macro (POV_MACRO *PMac);
POV_ARRAY *Parse_Array_Declare (void);
SYM_ENTRY *Create_Entry (int Index,char *Name,TOKEN Number);
SYM_ENTRY *Destroy_Entry (int Index,SYM_ENTRY *Entry);
int Parse_Ifdef_Param (void);

END_POV_NAMESPACE

#endif
