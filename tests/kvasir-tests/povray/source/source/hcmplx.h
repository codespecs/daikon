/****************************************************************************
 *                  hcmplx.h
 *
 * This module contains all defines, typedefs, and prototypes for HCMPLX.CPP.
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
 * $File: //depot/povray/3.6-release/source/hcmplx.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef HCMPLX_H
#define HCMPLX_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

/*****************************************************************************
* Global typedefs
******************************************************************************/

/*****************************************************************************
* Global variables
******************************************************************************/

/*****************************************************************************
* Global functions
******************************************************************************/

int F_Bound_HCompl (RAY *, FRACTAL *, DBL *, DBL *);
void Normal_Calc_HCompl (VECTOR, int, FRACTAL *);
int Iteration_HCompl (VECTOR, FRACTAL *);
int D_Iteration_HCompl (VECTOR, FRACTAL *, DBL *);

int F_Bound_HCompl_z3 (RAY *, FRACTAL *, DBL *, DBL *);
void Normal_Calc_HCompl_z3 (VECTOR, int, FRACTAL *);
int Iteration_HCompl_z3 (VECTOR, FRACTAL *);
int D_Iteration_HCompl_z3 (VECTOR, FRACTAL *, DBL *);

int F_Bound_HCompl_Reciprocal (RAY *, FRACTAL *, DBL *, DBL *);
void Normal_Calc_HCompl_Reciprocal (VECTOR, int, FRACTAL *);
int Iteration_HCompl_Reciprocal (VECTOR, FRACTAL *);
int D_Iteration_HCompl_Reciprocal (VECTOR, FRACTAL *, DBL *);

int F_Bound_HCompl_Func (RAY *, FRACTAL *, DBL *, DBL *);
void Normal_Calc_HCompl_Func (VECTOR, int, FRACTAL *);
int Iteration_HCompl_Func (VECTOR, FRACTAL *);
int D_Iteration_HCompl_Func (VECTOR, FRACTAL *, DBL *);

void Complex_Exp (CMPLX *target, CMPLX *source);
void Complex_Ln (CMPLX *target, CMPLX *source);
void Complex_Sin (CMPLX *target, CMPLX *source);
void Complex_ASin (CMPLX *target, CMPLX *source);
void Complex_Sinh (CMPLX *target, CMPLX *source);
void Complex_ASinh (CMPLX *target, CMPLX *source);
void Complex_Cos (CMPLX *target, CMPLX *source);
void Complex_ACos (CMPLX *target, CMPLX *source);
void Complex_Cosh (CMPLX *target, CMPLX *source);
void Complex_ACosh (CMPLX *target, CMPLX *source);
void Complex_Tan (CMPLX *target, CMPLX *source);
void Complex_ATan (CMPLX *target, CMPLX *source);
void Complex_Tanh (CMPLX *target, CMPLX *source);
void Complex_ATanh (CMPLX *target, CMPLX *source);
void Complex_Sqrt (CMPLX *target, CMPLX *source);
void Complex_Pwr (CMPLX *target, CMPLX *source);

void Complex_Mult (CMPLX *target, CMPLX *source1, CMPLX *source2);
void Complex_Div (CMPLX *target, CMPLX *source1, CMPLX *source2);
void Complex_Power (CMPLX *target, CMPLX *source1, CMPLX *source2);

END_POV_NAMESPACE

#endif
