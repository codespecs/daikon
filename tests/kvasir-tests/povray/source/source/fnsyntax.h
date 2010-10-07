/****************************************************************************
 *                  fnsyntax.h
 *
 * This module contains all defines, typedefs, and prototypes for fnsyntax.cpp.
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
 * $File: //depot/povray/3.6-release/source/fnsyntax.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef FNSYNTAX_H
#define FNSYNTAX_H

BEGIN_POV_NAMESPACE

// Caution: The compiler depends on the order of these constants!
enum
{
	OP_NONE = 0,    // 0

	OP_CMP_EQ,      // 1
	OP_CMP_NE,      // 2
	OP_CMP_LT,      // 3
	OP_CMP_LE,      // 4
	OP_CMP_GT,      // 5
	OP_CMP_GE,      // 6
	OP_ADD,         // 7
	OP_SUB,         // 8
	OP_OR,          // 9
	OP_MUL,         // 10
	OP_DIV,         // 11
	OP_AND,         // 12
	OP_POW,         // 13
	OP_NEG,         // 14
	OP_NOT,         // 15

	OP_LEFTMOST,    // 16
	OP_FIRST,       // 17

	OP_CONSTANT,    // 18
	OP_VARIABLE,    // 19
	OP_DOT,         // 20
	OP_MEMBER,      // 21
	OP_CALL,        // 22
	OP_TRAP         // 23
};

typedef struct ExprNodeStruct
{
	struct ExprNodeStruct *parent;
	struct ExprNodeStruct *child;
	struct ExprNodeStruct *prev;
	struct ExprNodeStruct *next;
	int stage;
	int op;
	union
	{
		char *variable;
		struct
		{
			char *name;
			TOKEN token;
			FUNCTION fn;
		} call;
		unsigned int trap;
		DBL number;
	};
} ExprNode;

ExprNode *FNSyntax_ParseExpression();
ExprNode *FNSyntax_GetTrapExpression(unsigned int);
void FNSyntax_DeleteExpression(ExprNode *);

END_POV_NAMESPACE

#endif
