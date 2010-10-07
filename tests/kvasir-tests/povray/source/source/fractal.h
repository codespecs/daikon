/****************************************************************************
 *                  fractal.h
 *
 * This module contains all defines, typedefs, and prototypes for FRACTAL.CPP.
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
 * $File: //depot/povray/3.6-release/source/fractal.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef FRACTAL_H
#define FRACTAL_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define QUATERNION_TYPE    0
#define HYPERCOMPLEX_TYPE  1

/* Hcmplx function stypes must come first */
#define EXP_STYPE          0
#define LN_STYPE           1
#define SIN_STYPE          2
#define ASIN_STYPE         3
#define COS_STYPE          4
#define ACOS_STYPE         5
#define TAN_STYPE          6
#define ATAN_STYPE         7
#define SINH_STYPE         8
#define ASINH_STYPE        9
#define COSH_STYPE        10
#define ACOSH_STYPE       11
#define TANH_STYPE        12
#define ATANH_STYPE       13
#define PWR_STYPE         14

/* end function stypes */
#define SQR_STYPE         15
#define CUBE_STYPE        16
#define RECIPROCAL_STYPE  17

#define Iteration(V,F) ( (*((F)->Iteration_Method))(V,F) )
#define Normal_Calc(F,V) ( (*((F)->Normal_Calc_Method))(V,(F)->n,F) )
#define F_Bound(R,F,dm,dM) ( (*((F)->F_Bound_Method))(R,F,dm,dM) )
#define D_Iteration(V,F,D) ( (*((F)->D_Iteration_Method))(V,F,D) )
#define Complex_Function(t,s,F) ( (*((F)->Complex_Function_Method))(t,s) )

/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Fractal_Struct FRACTAL;
typedef struct cmplx { DBL x,y; } CMPLX;
typedef void (*NORMAL_CALC_METHOD) (VECTOR, int, FRACTAL *);
typedef int (*ITERATION_METHOD) (VECTOR, FRACTAL *);
typedef int (*D_ITERATION_METHOD) (VECTOR, FRACTAL *, DBL *);
typedef int (*F_BOUND_METHOD) (RAY *, FRACTAL *, DBL *, DBL *);
typedef void (*COMPLEX_FUNCTION_METHOD) (CMPLX *, CMPLX *);

struct Fractal_Struct
{
  OBJECT_FIELDS
  VECTOR Center;
  DBL Julia_Parm[4];
  DBL Slice[4];                 /* vector perpendicular to slice plane */
  DBL SliceDist;                /* distance from slice plane to origin */
  DBL Exit_Value;
  int n;                        /* number of iterations */
  DBL Precision;                /* Precision value */
  int Inverted;

  int Algebra;                  /* Quaternion or Hypercomplex */
  int Sub_Type;
  CMPLX exponent;               /* exponent of power function */

  NORMAL_CALC_METHOD Normal_Calc_Method;
  ITERATION_METHOD Iteration_Method;
  D_ITERATION_METHOD D_Iteration_Method;
  F_BOUND_METHOD F_Bound_Method;
  COMPLEX_FUNCTION_METHOD Complex_Function_Method;

  DBL Radius_Squared;           /* For F_Bound(), if needed */
};

/*****************************************************************************
* Global variables
******************************************************************************/

extern DBL *Sx, *Sy, *Sz, *Sw;
extern DBL Precision;
extern VECTOR Direction;

/*****************************************************************************
* Global functions
******************************************************************************/

FRACTAL *Create_Fractal (void);
void SetUp_Fractal (FRACTAL * Fractal);
void Allocate_Iteration_Stack (int n);
void Free_Iteration_Stack (void);

END_POV_NAMESPACE

#endif
