/****************************************************************************
 *                  fncode.h
 *
 * This module contains all defines, typedefs, and prototypes for fncode.cpp.
 *
 * This module is inspired by code by D. Skarda, T. Bily and R. Suzuki.
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
 * $File: //depot/povray/3.6-release/source/fncode.h $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/


#ifndef FNCODE_H
#define FNCODE_H

#include "frame.h"
#include "userio.h"
#include "function.h"
#include "fnsyntax.h"
#include "parse.h"
#include "tokenize.h"
#include "textstream.h"

BEGIN_POV_NAMESPACE

#ifndef DEBUG_FLOATFUNCTION
 #define DEBUG_FLOATFUNCTION 0
#endif

#define FN_INLINE_FLAG 1
#define FN_LOCAL_FLAG  2

#define MAX_K ((unsigned int)0x000fffff)
#define MAX_FN MAX_K

#define MAKE_INSTRUCTION(op, k) ((((k) << 12) & 0xfffff000) | ((op) & 0x00000fff))

#define GET_OP(w) ((w) & 0x00000fff)
#define GET_K(w) (((w) >> 12) & 0x000fffff)


typedef void *(*FNCODE_PRIVATE_COPY_METHOD)(void *);
typedef void (*FNCODE_PRIVATE_DESTROY_METHOD)(void *);

typedef unsigned int Instruction;

typedef struct
{
	Instruction *program;
	unsigned int program_size;
	unsigned char return_size;
	unsigned char parameter_cnt;
	unsigned char localvar_cnt;
	unsigned int localvar_pos[MAX_PARAMETER_LIST];
	char *localvar[MAX_PARAMETER_LIST];
	char *parameter[MAX_PARAMETER_LIST];
	char *name;
	char *filename;
	POV_BASE_NAMESPACE::ITextStream::FilePos filepos;
	unsigned int flags;
	FNCODE_PRIVATE_COPY_METHOD private_copy_method;
	FNCODE_PRIVATE_DESTROY_METHOD private_destroy_method;
	void *private_data;
} FunctionCode;

void FNCode_Copy(FunctionCode *, FunctionCode *);
void FNCode_Delete(FunctionCode *);

class FNCode
{
	public:
		FNCode(FunctionCode *, bool, char *);

		void Parameter();
		void Compile(ExprNode *);
		void SetFlag(unsigned int, char *);
	private:
		FunctionCode *function;

		unsigned int max_program_size;
		unsigned int max_stack_size;
		unsigned int stack_pointer;
		unsigned int parameter_stack_pointer;
		int level;

		#if (DEBUG_FLOATFUNCTION == 1)

		char *asm_input;
		char *asm_output;
		char *asm_error;

		#endif

		FNCode() { Error("Cannot use FNCode default constructor!"); };
		FNCode(FNCode&) { Error("Cannot use FNCode copy constructor!"); };

		void compile_recursive(ExprNode *expr);
		void compile_member(char *name);
		void compile_call(ExprNode *expr, FUNCTION fn, int token, char *name);
		void compile_select(ExprNode *expr);
		void compile_seq_op(ExprNode *expr, unsigned int op, DBL neutral);
		void compile_float_function_call(ExprNode *expr, FUNCTION fn, char *name);
		void compile_vector_function_call(ExprNode *expr, FUNCTION fn, char *name);
		void compile_inline(FunctionCode *function);
		void compile_variable(char *name);
		void compile_parameters();
		unsigned int compile_push_result();
		void compile_pop_result(unsigned int local_k);
		unsigned int compile_instruction(unsigned int, unsigned int, unsigned int, unsigned int);
		unsigned int compile_instruction(unsigned int, unsigned int, unsigned int, unsigned int, unsigned int);

		#if (DEBUG_FLOATFUNCTION == 1)

		int assemble(char *filename);
		int disassemble(char *filename);
		void disassemble_instruction(FILE *f, Instruction& i);

		unsigned int parse_instruction(FILE *f);
		unsigned int parse_reg(FILE *f);
		DBL parse_float(FILE *f);
		unsigned int parse_integer(FILE *f);
		unsigned int parse_move(FILE *f, unsigned int& r, unsigned int& k);
		void parse_comma(FILE *f);
		bool parse_comment(FILE *f);
		void skip_space(FILE *f);
		void skip_newline(FILE *f);

		#endif
};

END_POV_NAMESPACE

#endif
