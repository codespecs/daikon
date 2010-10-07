/****************************************************************************
 *                  matrices.h
 *
 * This module contains all defines, typedefs, and prototypes for MATRICES.CPP.
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
 * $File: //depot/povray/3.6-release/source/matrices.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef MATRICES_H
#define MATRICES_H

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

void MZero (MATRIX result);
void MIdentity (MATRIX result);
void MTimesA (MATRIX result, MATRIX matrix2);
void MTimesB (MATRIX matrix1, MATRIX result);
void MTimesC (MATRIX result, MATRIX matrix1, MATRIX matrix2);
void MAdd (MATRIX result, MATRIX matrix1, MATRIX matrix2);
void MSub (MATRIX result, MATRIX matrix1, MATRIX matrix2);
void MScale (MATRIX result, MATRIX matrix1, DBL amount);
void MTranspose (MATRIX result);
void MTranspose (MATRIX result, MATRIX matrix1);
void MTransPoint (VECTOR result, VECTOR vector, TRANSFORM *trans);
void MInvTransPoint (VECTOR result, VECTOR vector, TRANSFORM *trans);
void MTransDirection (VECTOR result, VECTOR vector, TRANSFORM *trans);
void MInvTransDirection (VECTOR result, VECTOR vector, TRANSFORM *trans);
void MTransNormal (VECTOR result, VECTOR vector, TRANSFORM *trans);
void MInvTransNormal (VECTOR result, VECTOR vector, TRANSFORM *trans);
void Compute_Matrix_Transform (TRANSFORM *result, MATRIX matrix);
void Compute_Scaling_Transform (TRANSFORM *result, VECTOR vector);
void Compute_Inversion_Transform (TRANSFORM *result);
void Compute_Translation_Transform (TRANSFORM *transform, VECTOR vector);
void Compute_Rotation_Transform (TRANSFORM *transform, VECTOR vector);
void Compute_Look_At_Transform (TRANSFORM *transform, VECTOR Look_At, VECTOR Up, VECTOR Right);
void Compose_Transforms (TRANSFORM *Original_Transform, TRANSFORM *New_Transform);
void Compute_Axis_Rotation_Transform (TRANSFORM *transform, VECTOR AxisVect, DBL angle);
void Compute_Coordinate_Transform (TRANSFORM *trans, VECTOR origin, VECTOR up, DBL r, DBL len);
TRANSFORM *Create_Transform (void);
TRANSFORM *Copy_Transform (TRANSFORM *Old);
void Destroy_Transform (TRANSFORM *Trans);
UV_VECT *Create_UV_Vect (void);
VECTOR *Create_Vector (void);
VECTOR_4D *Create_Vector_4D (void);
DBL *Create_Float (void);
void MInvers (MATRIX r, MATRIX m);
int MInvers3(VECTOR inM[3], VECTOR outM[3]);
void MTransUVPoint(DBL p[2], DBL m[3][3], DBL t[2]);
void MSquareQuad(UV_VECT st[4], DBL sq[3][3]);

END_POV_NAMESPACE

#endif
