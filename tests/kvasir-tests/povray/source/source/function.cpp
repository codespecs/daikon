/****************************************************************************
 *                  function.cpp
 *
 * This module implements the the function type used by iso surfaces and
 * the function pattern.
 *
 * This module is based on code by D. Skarda, T. Bily and R. Suzuki.
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
 * $File: //depot/povray/3.6-release/source/function.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <limits.h>

#include "frame.h"
#include "isosurf.h"
#include "parse.h"
#include "povray.h"
#include "tokenize.h"
#include "vector.h"
#include "pov_util.h"
#include "function.h"
#include "fnpovfpu.h"
#include "pigment.h"
#include "parstxtr.h"
#include "matrices.h"
#include "express.h"
#include "splines.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/


/*****************************************************************************
* Local typedefs
******************************************************************************/


/*****************************************************************************
* Global variables
******************************************************************************/


/*****************************************************************************
* Local variables
******************************************************************************/


/*****************************************************************************
* Static functions
******************************************************************************/



/*****************************************************************************
 *
 * FUNCTION
 *
 *   Destroy_Function
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
 *   -
 *
 * CHANGES
 *   
 *   -
 *
 ******************************************************************************/

void Destroy_Function(FUNCTION_PTR Function)
{
	if(Function != NULL)
	{
		POVFPU_RemoveFunction(*Function);
		POV_FREE(Function);
	}
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Copy_Function
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
 *   -
 *
 * CHANGES
 *   
 *   -
 *
 ******************************************************************************/

FUNCTION_PTR Copy_Function(FUNCTION_PTR Function)
{
	FUNCTION_PTR ptr = (FUNCTION_PTR)POV_MALLOC(sizeof(FUNCTION), "Function ID");

	(void)POVFPU_GetFunctionAndReference(*Function); // increase the reference count
	*ptr = *Function;

	return ptr;
}


/*****************************************************************************
*
* FUNCTION
*
*   Parse_Function
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
*   FUNCTION - parsed and compiled function reference number
*
* AUTHOR
*
*   Thorsten Froehlich
*   
* DESCRIPTION
*
*   Parse and compile a function and add it to the global function table.
*
* CHANGES
*
*   -
*
******************************************************************************/

FUNCTION_PTR Parse_Function(void)
{
	FUNCTION_PTR ptr = (FUNCTION_PTR)POV_MALLOC(sizeof(FUNCTION), "Function ID");
	ExprNode *expression = NULL;
	FunctionCode function;

	Parse_Begin();

	FNCode f(&function, false, NULL);

	expression = FNSyntax_ParseExpression();
	f.Compile(expression);
	FNSyntax_DeleteExpression(expression);

	Parse_End();

	*ptr = POVFPU_AddFunction(&function);

	return ptr;
}


/*****************************************************************************
*
* FUNCTION
*
*   Parse_FunctionContent
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
*   FUNCTION - parsed and compiled function reference number
*
* AUTHOR
*
*   Thorsten Froehlich
*   
* DESCRIPTION
*
*   Parse and compile a function and add it to the global function table.
*
* CHANGES
*
*   -
*
******************************************************************************/

FUNCTION_PTR Parse_FunctionContent(void)
{
	FUNCTION_PTR ptr = (FUNCTION_PTR)POV_MALLOC(sizeof(FUNCTION), "Function ID");
	ExprNode *expression = NULL;
	FunctionCode function;

	FNCode f(&function, false, NULL);

	expression = FNSyntax_ParseExpression();
	f.Compile(expression);
	FNSyntax_DeleteExpression(expression);

	*ptr = POVFPU_AddFunction(&function);

	return ptr;
}


/*****************************************************************************
*
* FUNCTION
*
*   Parse_DeclareFunction
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*
*   FUNCTION - parsed and compiled function reference number
*
* AUTHOR
*
*   Thorsten Froehlich
*   
* DESCRIPTION
*
*   Parse and compile a function and add it to the global function table.
*   Additionally, this function takes an optional list of parameter names.
*
* CHANGES
*
*   -
*
******************************************************************************/

FUNCTION_PTR Parse_DeclareFunction(int *token_id, char *fn_name, bool is_local)
{
	FUNCTION_PTR ptr = (FUNCTION_PTR)POV_MALLOC(sizeof(FUNCTION), "Function ID");
	ExprNode *expression = NULL;
	FunctionCode function;

	// default type is float function
	*token_id = FUNCT_ID_TOKEN;

	FNCode f(&function, is_local, fn_name);
	f.Parameter();

	Parse_Begin();

	Get_Token();
	if(Token.Token_Id == INTERNAL_TOKEN)
	{
		GET(LEFT_PAREN_TOKEN);

		Get_Token();
		if(Token.Function_Id != FLOAT_TOKEN)
			Expectation_Error("internal function identifier");
		expression = FNSyntax_GetTrapExpression((unsigned int)(Token.Token_Float));

		function.flags = FN_INLINE_FLAG;

		GET(RIGHT_PAREN_TOKEN);
	}
	else if(Token.Token_Id == TRANSFORM_TOKEN)
	{
		if(function.parameter_cnt != 0)
			Error("Function parameters for transform functions are not allowed.");

		expression = FNSyntax_GetTrapExpression(1); // 1 refers to POVFPU_TrapSTable[1] = f_transform [trf]

		function.private_copy_method = (FNCODE_PRIVATE_COPY_METHOD)Copy_Transform;
		function.private_destroy_method = (FNCODE_PRIVATE_DESTROY_METHOD)Destroy_Transform;

		function.private_data = (void *)Parse_Transform_Block();

		function.return_size = 3; // returns a 3d vector!!!

		// function type is vector function
		*token_id = VECTFUNCT_ID_TOKEN;
	}
	else if(Token.Token_Id == SPLINE_TOKEN)
	{
		if(function.parameter_cnt != 0)
			Error("Function parameters for spline functions are not allowed.");

		Experimental_Flag |= EF_SPLINE;

		expression = FNSyntax_GetTrapExpression(2); // 2 refers to POVFPU_TrapSTable[2] = f_spline [trf]

		function.private_copy_method = (FNCODE_PRIVATE_COPY_METHOD)Copy_Spline;
		function.private_destroy_method = (FNCODE_PRIVATE_DESTROY_METHOD)Destroy_Spline;

		Parse_Begin();
		function.private_data = (void *)Parse_Spline();
		Parse_End();

		function.return_size = ((SPLINE *)(function.private_data))->Terms; // returns a 2d, 3d, 4d or 5d vector!!!

		// function type is vector function
		*token_id = VECTFUNCT_ID_TOKEN;
	}
	else if(Token.Token_Id == PIGMENT_TOKEN)
	{
		if(function.parameter_cnt != 0)
			Error("Function parameters for pigment functions are not allowed.");

		expression = FNSyntax_GetTrapExpression(0); // 0 refers to POVFPU_TrapSTable[0] = f_pigment [trf]

		function.private_copy_method = (FNCODE_PRIVATE_COPY_METHOD)Copy_Pigment;
		function.private_destroy_method = (FNCODE_PRIVATE_DESTROY_METHOD)Destroy_Pigment;

		Parse_Begin();
		function.private_data = (void *)Create_Pigment();
		Parse_Pigment((PIGMENT **)(&function.private_data));
		Parse_End();
		Post_Pigment((PIGMENT *)(function.private_data));

		function.return_size = 5; // returns a color!!!

		// function type is vector function
		*token_id = VECTFUNCT_ID_TOKEN;
	}
	else if(Token.Token_Id == PATTERN_TOKEN)
	{
		if(function.parameter_cnt != 0)
			Error("Function parameters for pattern functions are not allowed.");

		expression = FNSyntax_GetTrapExpression(77); // 77 refers to POVFPU_TrapTable[77] = f_pattern [trf]

		function.private_copy_method = (FNCODE_PRIVATE_COPY_METHOD)Copy_Pigment;
		function.private_destroy_method = (FNCODE_PRIVATE_DESTROY_METHOD)Destroy_Pigment;

		Parse_Begin();
		function.private_data = (void *)Create_Pigment(); // Yes, this is a pigment! [trf]
		Parse_PatternFunction((TPATTERN *)(function.private_data));
		Parse_End();
		Post_Pigment((PIGMENT *)(function.private_data));
	}
	else if(Token.Token_Id == STRING_LITERAL_TOKEN)
	{
		f.SetFlag(2, Token.Token_String);
		Get_Token();
		if(Token.Token_Id == COMMA_TOKEN)
		{
			Get_Token();
			if(Token.Token_Id != STRING_LITERAL_TOKEN)
				Expectation_Error("valid function expression");
			f.SetFlag(1, Token.Token_String);
		}
		else
		{
			Unget_Token();
			expression = FNSyntax_ParseExpression();
		}
	}
	else
	{
		Unget_Token();
		expression = FNSyntax_ParseExpression();
	}

	f.Compile(expression);
	FNSyntax_DeleteExpression(expression);

	Parse_End();

	*ptr = POVFPU_AddFunction(&function);

	return ptr;
}

END_POV_NAMESPACE
