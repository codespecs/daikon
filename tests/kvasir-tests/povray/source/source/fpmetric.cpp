/****************************************************************************
 *                  fpmetric.cpp
 *
 * This module implements the parametric shapetype.
 *
 * This module was written by D.Skarda&T.Bily and modified by R.Suzuki.
 * Ported to POV-Ray 3.5 by Thorsten Froehlich.
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
 * $File: //depot/povray/3.6-release/source/fpmetric.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <limits.h>
#include <algorithm>

#include "frame.h"
#include "povray.h"
#include "objects.h"
#include "boxes.h"
#include "spheres.h"
#include "vector.h"
#include "matrices.h"
#include "bbox.h"
#include "isosurf.h"
#include "fpmetric.h"
#include "parse.h"
#include "function.h"
#include "fnpovfpu.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
 * Global variables
******************************************************************************/


/*****************************************************************************
 * Local preprocessor defines
 ******************************************************************************/

const int Max_intNumber = 10000000;
#define close(x, y) (fabs(x-y) < EPSILON ? 1 : 0)

const int INDEX_U = 0;
const int INDEX_V = 1;

/* Side hit. */
const int SIDE_X_0 = 1;
const int SIDE_X_1 = 2;
const int SIDE_Y_0 = 3;
const int SIDE_Y_1 = 4;
const int SIDE_Z_0 = 5;
const int SIDE_Z_1 = 6;


/*****************************************************************************
 * Static functions
 ******************************************************************************/

static int All_Parametric_Intersections(OBJECT* Object, RAY* Ray, ISTACK* Depth_Stack);
static int Inside_Parametric(VECTOR point, OBJECT* Object);
static void Parametric_Normal(VECTOR Result, OBJECT* Object, INTERSECTION* Inter);
static void Translate_Parametric(OBJECT* Object, VECTOR Vector, TRANSFORM* Trans);
static void Rotate_Parametric(OBJECT* Object, VECTOR Vector, TRANSFORM* Trans);
static void Scale_Parametric(OBJECT* Object, VECTOR Vector, TRANSFORM* Trans);
static void Transform_Parametric(OBJECT* Object, TRANSFORM* Trans);
static void Invert_Parametric(OBJECT* Object);
static void Parametric_UVCoord (UV_VECT Result, OBJECT *Object, INTERSECTION *Inter);

inline DBL Evaluate_Function_UV(FUNCTION funct, UV_VECT fnvec);
inline void Evaluate_Function_Interval_UV(FUNCTION funct, DBL threshold, UV_VECT fnvec_low, UV_VECT fnvec_hi, DBL max_gradient, DBL& low, DBL& hi);
void Interval(DBL dx, DBL a, DBL b, DBL max_gradient, DBL *Min, DBL *Max);


/*****************************************************************************
 * Local variables
 ******************************************************************************/

METHODS Parametric_Methods =
{ 
	All_Parametric_Intersections,
	Inside_Parametric, Parametric_Normal,	Parametric_UVCoord,
	Copy_Parametric,
	Translate_Parametric, Rotate_Parametric,
	Scale_Parametric, Transform_Parametric, Invert_Parametric,
	Destroy_Parametric
};

static PRECOMP_PAR_DATA* PrecParData; // GLOBAL VARIABLE
static PARAMETRIC* PrecompParFunc; // GLOBAL VARIABLE
static int PrecompLastDepth; // GLOBAL VARIABLE

static DBL Intervals_Low[2][32], Intervals_Hi[2][32]; // GLOBAL VARIABLE
static int SectorNum[32]; // GLOBAL VARIABLE


/*****************************************************************************
 *
 * FUNCTION
 *
 *   All_Parametric_Intersections
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

int All_Parametric_Intersections(OBJECT* Object, RAY* Ray, ISTACK* Depth_Stack)
{
	PARAMETRIC * Par = (PARAMETRIC *)Object;
	PRECOMP_PAR_DATA * PData = ((PARAMETRIC *)Object)->PData;
	VECTOR P, D, IPoint;
	UV_VECT low_vect, hi_vect;
	RAY New_Ray;
	DBL XRayMin, XRayMax, YRayMin, YRayMax, ZRayMin, ZRayMax, TPotRes, TLen;
	DBL Depth1, Depth2, temp, Len, UResult, VResult, TResult = HUGE_VAL;
	DBL low, hi, len;
	int MaxPrecompX, MaxPrecompY, MaxPrecompZ;
	int split, i = 0, Side1, Side2;
	int parX, parY;
	int i_flg;

	Increase_Counter(stats[Ray_Par_Bound_Tests]);

	if(Par->container_shape)
	{
		if(Par->Trans != NULL)
		{
			MInvTransPoint(New_Ray.Initial, Ray->Initial, Par->Trans);
			MInvTransDirection(New_Ray.Direction, Ray->Direction, Par->Trans);
			VLength(len, New_Ray.Direction);
			VInverseScaleEq(New_Ray.Direction, len);
			i_flg = Intersect_Sphere(&New_Ray, Par->container.sphere.center, 
			                         (Par->container.sphere.radius) * (Par->container.sphere.radius),
			                         &Depth1, &Depth2);
			Depth1 = Depth1 / len;
			Depth2 = Depth2 / len;
		}
		else
		{
			i_flg = Intersect_Sphere(Ray, Par->container.sphere.center, 
			                         (Par->container.sphere.radius) * (Par->container.sphere.radius), &Depth1, &Depth2);
		}
		Decrease_Counter(stats[Ray_Sphere_Tests]);
		if(i_flg)
			Decrease_Counter(stats[Ray_Sphere_Tests_Succeeded]);
	}
	else
	{
		i_flg = Intersect_Box(Ray, Par->Trans, Par->container.box.corner1, Par->container.box.corner2,
		                      &Depth1, &Depth2, &Side1, &Side2);
	}

	if(!i_flg)
		return false;

	Increase_Counter(stats[Ray_Par_Bound_Tests_Succeeded]);
	Increase_Counter(stats[Ray_Parametric_Tests]);

	if (Par->Trans != NULL)
	{
		MInvTransPoint(P, Ray->Initial, Par->Trans);
		MInvTransDirection(D, Ray->Direction, Par->Trans);
	}
	else
	{
		P[X] = Ray->Initial[X];
		P[Y] = Ray->Initial[Y];
		P[Z] = Ray->Initial[Z];
		D[X] = Ray->Direction[X];
		D[Y] = Ray->Direction[Y];
		D[Z] = Ray->Direction[Z];
	}

	if (Depth1 == Depth2)
		Depth1 = 0;

	if ((Depth1 += 4 * Par->accuracy) > Depth2)
		return false;

	Intervals_Low[INDEX_U][0] = Par->umin;
	Intervals_Hi[INDEX_U][0] = Par->umax;

	Intervals_Low[INDEX_V][0] = Par->vmin;
	Intervals_Hi[INDEX_V][0] = Par->vmax;
	/* Fri 09-27-1996 0. */
	SectorNum[0] = 1;

	MaxPrecompX = MaxPrecompY = MaxPrecompZ = 0;
	if (PData != NULL)
	{
		if (((PData->flags) & OK_X) != 0)
			MaxPrecompX = 1 << (PData->depth);
		if (((PData->flags) & OK_Y) != 0)
			MaxPrecompY = 1 << (PData->depth);
		if (((PData->flags) & OK_Z) != 0)
			MaxPrecompZ = 1 << (PData->depth);
	}
	/* 0 */
	while (i >= 0)
	{
		low_vect[U] = Intervals_Low[INDEX_U][i];
		hi_vect[U] = Intervals_Hi[INDEX_U][i];
		Len = hi_vect[U] - low_vect[U];
		split = INDEX_U;

		low_vect[V] = Intervals_Low[INDEX_V][i];
		hi_vect[V] = Intervals_Hi[INDEX_V][i];
		temp = hi_vect[V] - low_vect[V];
		if (temp > Len)
		{
			Len = temp;
			split = INDEX_V;
		}
		parX = parY = 0;
		TLen = 0;

		/* X */
		if (SectorNum[i] < MaxPrecompX)
		{
			low = PData->Low[0][SectorNum[i]];
			hi = PData->Hi[0][SectorNum[i]];
		}
		else
			Evaluate_Function_Interval_UV(*(Par->Function[0]), Par->accuracy, low_vect, hi_vect, Par->max_gradient, low, hi);
		/* fabs(D[X] *(T2-T1)) is not OK with new method */

		if (close(D[0], 0))
		{
			parX = 1;
			if ((hi < P[0]) || (low > P[0]))
			{
				i--;
				continue;
			}
		}
		else
		{
			XRayMin = (hi - P[0]) / D[0];
			XRayMax = (low - P[0]) / D[0];
			if (XRayMin > XRayMax)
			{
				temp = XRayMin;
				XRayMin = XRayMax;
				XRayMax = temp;
			}

			if ((XRayMin > Depth2) || (XRayMax < Depth1))
			{
				i--;
				continue;
			}

			if ((TPotRes = XRayMin) > TResult)
			{
				i--;
				continue;
			}

			TLen = XRayMax - XRayMin;
		}

		/* Y */
		if (SectorNum[i] < MaxPrecompY)
		{
			low = PData->Low[1][SectorNum[i]];
			hi = PData->Hi[1][SectorNum[i]];
		}
		else
			Evaluate_Function_Interval_UV(*(Par->Function[1]), Par->accuracy, low_vect, hi_vect, Par->max_gradient, low, hi);

		if (close(D[1], 0))
		{
			parY = 1;
			if ((hi < P[1]) || (low > P[1]))
			{
				i--;
				continue;
			}
		}
		else
		{
			YRayMin = (hi - P[1]) / D[1];
			YRayMax = (low - P[1]) / D[1];
			if (YRayMin > YRayMax)
			{
				temp = YRayMin;
				YRayMin = YRayMax;
				YRayMax = temp;
			}
			if ((YRayMin > Depth2) || (YRayMax < Depth1))
			{
				i--;
				continue;
			}
			if ((TPotRes = YRayMin) > TResult)
			{
				i--;
				continue;
			}
			if (parX == 0)
			{
				if ((YRayMin > XRayMax) || (YRayMax < XRayMin))
				{
					i--;
					continue;
				}
			}
			if ((temp = YRayMax - YRayMin) > TLen)
				TLen = temp;
		}

		/* Z */
		if ((SectorNum[i] < MaxPrecompZ) && (0 < SectorNum[i]))
		{
			low = PData->Low[2][SectorNum[i]];
			hi = PData->Hi[2][SectorNum[i]];
		}
		else
			Evaluate_Function_Interval_UV(*(Par->Function[2]), Par->accuracy, low_vect, hi_vect, Par->max_gradient, low, hi);

		if (close(D[2], 0))
		{
			if ((hi < P[2]) || (low > P[2]))
			{
				i--;
				continue;
			}
		}
		else
		{
			ZRayMin = (hi - P[2]) / D[2];
			ZRayMax = (low - P[2]) / D[2];
			if (ZRayMin > ZRayMax)
			{
				temp = ZRayMin;
				ZRayMin = ZRayMax;
				ZRayMax = temp;
			}
			if ((ZRayMin > Depth2) || (ZRayMax < Depth1))
			{
				i--;
				continue;
			}
			if ((TPotRes = ZRayMin) > TResult)
			{
				i--;
				continue;
			}
			if (parX == 0)
			{
				if ((ZRayMin > XRayMax) || (ZRayMax < XRayMin))
				{
					i--;
					continue;
				}
			}
			if (parY == 0)
			{
				if ((ZRayMin > YRayMax) || (ZRayMax < YRayMin))
				{
					i--;
					continue;
				}
			}
			if ((temp = ZRayMax - ZRayMin) > TLen)
				TLen = temp;
		}

		if (Len > TLen)
			Len = TLen;
		if (Len < Par->accuracy)
		{
			if ((TResult > TPotRes) && (TPotRes > Depth1))
			{
				TResult = TPotRes;
				Par->last_u = UResult = low_vect[U];
				Par->last_v = VResult = low_vect[V];
			}
			i--;
		}
		else
		{
			/* 1 copy */
			if ((SectorNum[i] *= 2) >= Max_intNumber)
				SectorNum[i] = Max_intNumber;
			SectorNum[i + 1] = SectorNum[i];
			SectorNum[i]++;
			i++;
			Intervals_Low[INDEX_U][i] = low_vect[U];
			Intervals_Hi[INDEX_U][i] = hi_vect[U];

			Intervals_Low[INDEX_V][i] = low_vect[V];
			Intervals_Hi[INDEX_V][i] = hi_vect[V];

			/* 2 split */
			temp = (Intervals_Hi[split][i] + Intervals_Low[split][i]) / 2.0;
			Intervals_Hi[split][i] = temp;
			Intervals_Low[split][i - 1] = temp;
		}
	}

	if (TResult < Depth2)
	{
		Increase_Counter(stats[Ray_Parametric_Tests_Succeeded]);
		VScale(IPoint, Ray->Direction, TResult);
		VAddEq(IPoint, Ray->Initial);

		if (Point_In_Clip(IPoint, Par->Clip))
		{
			/*
			  compute_param_normal( Par, UResult, VResult , &N); 
			  push_normal_entry( TResult ,IPoint, N, (OBJECT *) Object, Depth_Stack);
			*/
//			UV_VECT uv;
//			Make_UV_Vector(uv, UResult, VResult);
//			push_uv_entry(TResult, IPoint, uv, (OBJECT *)Object, Depth_Stack);
			push_entry(TResult, IPoint, (OBJECT *)Object, Depth_Stack);

			return true;
		}
	}

	return false;
}
/* 0 */


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Inside_Parametric
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

int Inside_Parametric(VECTOR, OBJECT*)
{
	return false;
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Parametric_Normal
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

void Parametric_Normal(VECTOR Result, OBJECT* Object, INTERSECTION* Inter)
{
	VECTOR RU, RV;
	PARAMETRIC * Par = (PARAMETRIC *)Object;
	VECTOR * IPoint = &(Inter->IPoint);
	UV_VECT uv_vect;

	uv_vect[U] = Par->last_u;
	uv_vect[V] = Par->last_v;
	RU[X] = RV[X] = -Evaluate_Function_UV(*(Par->Function[X]), uv_vect);
	RU[Y] = RV[Y] = -Evaluate_Function_UV(*(Par->Function[Y]), uv_vect);
	RU[Z] = RV[Z] = -Evaluate_Function_UV(*(Par->Function[Z]), uv_vect);

	uv_vect[U] += Par->accuracy;
	RU[X] += Evaluate_Function_UV(*(Par->Function[X]), uv_vect);
	RU[Y] += Evaluate_Function_UV(*(Par->Function[Y]), uv_vect);
	RU[Z] += Evaluate_Function_UV(*(Par->Function[Z]), uv_vect);

	uv_vect[U] = Par->last_u;
	uv_vect[V] += Par->accuracy;
	RV[X] += Evaluate_Function_UV(*(Par->Function[X]), uv_vect);
	RV[Y] += Evaluate_Function_UV(*(Par->Function[Y]), uv_vect);
	RV[Z] += Evaluate_Function_UV(*(Par->Function[Z]), uv_vect);

	VCross(Result, RU, RV);
	if (Par->Trans != NULL)
		MTransNormal(Result, Result, Par->Trans);
	VNormalize(Result, Result);
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Compute_Parametric_BBox
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

void Compute_Parametric_BBox(PARAMETRIC* Param)
{
	if(Param->container_shape != 0)
	{
		Make_BBox(Param->BBox,
		          Param->container.sphere.center[X] - Param->container.sphere.radius,
		          Param->container.sphere.center[Y] - Param->container.sphere.radius,
		          Param->container.sphere.center[Z] - Param->container.sphere.radius,
		          Param->container.sphere.radius * 2,
		          Param->container.sphere.radius * 2,
		          Param->container.sphere.radius * 2);
	}
	else
	{
		// [ABX 20.01.2004] Low_Left introduced to hide BCC 5.5 bug
		BBOX_VECT& Low_Left = Param->BBox.Lower_Left;

		Assign_BBox_Vect(Low_Left, Param->container.box.corner1);
		VSub(Param->BBox.Lengths, Param->container.box.corner2, Param->container.box.corner1);
	}

	if(Param->Trans != NULL)
	{
		Recompute_BBox(&Param->BBox, Param->Trans);
	}
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Translate_Parametric
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

void Translate_Parametric(OBJECT* Object, VECTOR, TRANSFORM* Trans)
{
	Transform_Parametric(Object, Trans);
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Rotate_Parametric
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

void Rotate_Parametric(OBJECT* Object, VECTOR, TRANSFORM* Trans)
{
	Transform_Parametric(Object, Trans);
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Scale_Parametric
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

void Scale_Parametric(OBJECT* Object, VECTOR, TRANSFORM* Trans)
{
	Transform_Parametric(Object, Trans);
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Transform_Parametric
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

void Transform_Parametric(OBJECT* Object, TRANSFORM* Trans)
{
	PARAMETRIC * Param = (PARAMETRIC *)Object;
	if (Param->Trans == NULL)
		Param->Trans = Create_Transform();
	Compose_Transforms(Param->Trans, Trans);
	Compute_Parametric_BBox(Param);
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Invert_Parametric
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

void Invert_Parametric(OBJECT*)
{
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Copy_Parametric
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

void* Copy_Parametric(OBJECT* Object)
{
	PARAMETRIC *New, *Old;

	Old = (PARAMETRIC *)Object;

	New = Create_Parametric();
	*New = *((PARAMETRIC *)Object);

	New->Function[0] = Copy_Function(Old->Function[0]);
	New->Function[1] = Copy_Function(Old->Function[1]);
	New->Function[2] = Copy_Function(Old->Function[2]);
	New->Trans = Copy_Transform(Old->Trans);
	New->PData = Copy_PrecompParVal(Old->PData);

	return (New);
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Destroy_Parametric
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

void Destroy_Parametric(OBJECT* Object)
{
	Destroy_Transform(((PARAMETRIC *)Object)->Trans);
	Destroy_Function(((PARAMETRIC *)Object)->Function[0]);
	Destroy_Function(((PARAMETRIC *)Object)->Function[1]);
	Destroy_Function(((PARAMETRIC *)Object)->Function[2]);
	
	Destroy_PrecompParVal(((PARAMETRIC *)Object)->PData);
	POV_FREE(Object);
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Create_Parametric
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

PARAMETRIC* Create_Parametric()
{
	PARAMETRIC *New;

	New = (PARAMETRIC *)POV_MALLOC(sizeof(PARAMETRIC), "parametric");

	INIT_OBJECT_FIELDS(New, PARAMETRIC_OBJECT, &Parametric_Methods);

	Make_Vector(New->container.box.corner1, -1.0, -1.0, -1.0);
	Make_Vector(New->container.box.corner2, 1.0, 1.0, 1.0);

	Make_BBox(New->BBox, -1.0, -1.0, -1.0, 2.0, 2.0, 2.0);

	New->Trans = Create_Transform();

	New->Function[0] = NULL;
	New->Function[1] = NULL;
	New->Function[2] = NULL;
	New->accuracy = 0.001;
	New->max_gradient = 1;
	New->Inverted = false;
	New->PData = NULL;
	New->container_shape = 0;

	return New;
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Parametric_UVCoord
 *
 * INPUT
 *
 *   As for all UVCoord methods object and interstcion structure
 *
 * OUTPUT
 *
 *   As for all UVCoord methods 2D vector with UV coordinates
 *
 * RETURNS
 *
 * AUTHOR
 *
 *   Wlodzimierz ABX Skiba
 *
 * DESCRIPTION
 *
 *   I'm not sure if last_u and last_v fields are safe place for storing
 *   uv coordinates but they are used for normal calculations. I hope
 *   tests will confirm it.
 *
 * CHANGES
 *
 *   -
 *
******************************************************************************/

static void Parametric_UVCoord(UV_VECT Result, OBJECT *Object, INTERSECTION * /*Inter*/)
{
	PARAMETRIC *Par = (PARAMETRIC *)Object;
	Result[U] = Par->last_u;
	Result[V] = Par->last_v;
}

   
/*****************************************************************************
 *
 * FUNCTION
 *
 *   Precomp_Par_Int
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

static void Precomp_Par_Int(int depth, DBL umin, DBL vmin, DBL umax, DBL vmax)
{
	int j;

	if(depth == PrecompLastDepth / 2)
		Do_Cooperate(1);

	if(depth >= PrecompLastDepth)
	{
		for(j = 0; j < 3; j++)
		{
			if(PrecParData->flags & (1 << j))
			{
				UV_VECT low,hi;

				low[U] = umin;
				hi[U] = umax;
				low[V] = vmin;
				hi[V] = vmax;

				Evaluate_Function_Interval_UV(*(PrecompParFunc->Function[j]), PrecompParFunc->accuracy, low, hi,
				                              PrecompParFunc->max_gradient,
				                              PrecParData->Low[j][depth],
				                              PrecParData->Hi[j][depth]);
			}
		}
	}
	else										/* split */
	{
		if(umax - umin < vmax - vmin)
		{
			Precomp_Par_Int(2 * depth, umin, vmin, umax, (vmin + vmax) / 2.0);
			Precomp_Par_Int(2 * depth + 1, umin, (vmin + vmax) / 2.0, umax, vmax);
		}
		else
		{
			Precomp_Par_Int(2 * depth, umin, vmin, (umin + umax) / 2.0, vmax);
			Precomp_Par_Int(2 * depth + 1, (umin + umax) / 2.0, vmin, umax, vmax);
		}
		for(j = 0; j < 3; j++)
		{
			if(PrecParData->flags & (1 << j))
			{
				if(PrecParData->Hi[j][2 * depth] > PrecParData->Hi[j][2 * depth + 1])
					PrecParData->Hi[j][depth] = PrecParData->Hi[j][2 * depth];
				else
					PrecParData->Hi[j][depth] = PrecParData->Hi[j][2 * depth + 1];
				if(PrecParData->Low[j][2 * depth] < PrecParData->Low[j][2 * depth + 1])
					PrecParData->Low[j][depth] = PrecParData->Low[j][2 * depth];
				else
					PrecParData->Low[j][depth] = PrecParData->Low[j][2 * depth + 1];
			}
		}
	}
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Precompute_Parametric_Values
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

PRECOMP_PAR_DATA* Precompute_Parametric_Values(PARAMETRIC* Par, char flags, int depth)
{
	PRECOMP_PAR_DATA * PData;
	DBL * Last;
	char* es = "precompute";
	int nmb;

	if ((depth < 1) || (depth > 20))
		Error("Precompute: invalid depth");
	nmb = 1 << depth;

	PData = (PRECOMP_PAR_DATA *)POV_MALLOC(sizeof(PRECOMP_PAR_DATA), es);
	if (PData == NULL)
		MAError("precompute", sizeof(PRECOMP_PAR_DATA));
	PData->flags = flags;
	PData->depth = depth;
	PData->use = 1;

	if (flags & OK_X)
	{
		PData->Low[0] = (DBL *)POV_MALLOC(sizeof(DBL) * nmb, es);
		Last = PData->Hi[0] = (DBL *)POV_MALLOC(sizeof(DBL) * nmb, es);
	}
	if (flags & OK_Y)
	{
		PData->Low[1] = (DBL *)POV_MALLOC(sizeof(DBL) * nmb, es);
		Last = PData->Hi[1] = (DBL *)POV_MALLOC(sizeof(DBL) * nmb, es);
	}
	if (flags & OK_Z)
	{
		PData->Low[2] = (DBL *)POV_MALLOC(sizeof(DBL) * nmb, es);
		Last = PData->Hi[2] = (DBL *)POV_MALLOC(sizeof(DBL) * nmb, es);
	}
	if (Last == NULL)
		MAError("precompute", sizeof(DBL) * nmb);

	PrecompLastDepth = 1 << (depth - 1);
	PrecParData = PData;
	PrecompParFunc = Par;

	Precomp_Par_Int(1, Par->umin, Par->vmin, Par->umax, Par->vmax);

	return (PData);
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Copy_PrecompParVal
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

PRECOMP_PAR_DATA* Copy_PrecompParVal(PRECOMP_PAR_DATA* PPV)
{
	if (PPV == NULL)
		return NULL;

	(PPV->use)++;

	return (PPV);
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Destroy_PrecompParVal
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

void Destroy_PrecompParVal(PRECOMP_PAR_DATA* PPV)
{
	if (PPV == NULL)
		return;

	PPV->use--;
	if (PPV->use == 0)
	{
		if (PPV->flags & OK_X)
		{
			POV_FREE(PPV->Low[0]);
			POV_FREE(PPV->Hi[0]);
		}
		if (PPV->flags & OK_Y)
		{
			POV_FREE(PPV->Low[1]);
			POV_FREE(PPV->Hi[1]);
		}
		if (PPV->flags & OK_Z)
		{
			POV_FREE(PPV->Low[2]);
			POV_FREE(PPV->Hi[2]);
		}
		POV_FREE(PPV);
	}
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Evaluate_Function
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

DBL Evaluate_Function_UV(FUNCTION funct, UV_VECT fnvec)
{
	POVFPU_SetLocal(U, fnvec[U]);
	POVFPU_SetLocal(V, fnvec[V]);

   	return POVFPU_Run(funct);
}

/*****************************************************************************
 *
 * FUNCTION
 *
 *   Interval
 *
 * INPUT
 *
 * OUTPUT
 *
 * RETURNS
 *
 * AUTHOR
 *   
 *   Ron Parker
 *
 * DESCRIPTION
 *
 *   Assume that the function attains its maximum gradient over the whole
 *   range and determine its minimum and maximum (really lower and upper
 *   bounds, not strict maxima and minima) accordingly.
 *
 * CHANGES
 *   
 *   -
 *
 ******************************************************************************/

void Interval(DBL dx, DBL a, DBL b, DBL max_gradient, DBL *Min, DBL *Max)
{
	DBL dy = fabs(a-b);
	DBL ofs = max_gradient*(dx-dy/max_gradient)/2;
        if ( ofs < 0 ) {
          ofs=0;
        }
	*Max = max(a,b)+ofs;
	*Min = min(a,b)-ofs;
}


/*****************************************************************************
 *
 * FUNCTION
 *
 *   Evaluate_Function_Interval
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

void Evaluate_Function_Interval_UV(FUNCTION funct, DBL threshold, UV_VECT fnvec_low, UV_VECT fnvec_hi, DBL max_gradient, DBL& low, DBL& hi)
{

  DBL f_0_0, f_0_1, f_1_0, f_1_1;
  DBL f_0_min, f_0_max;
  DBL f_1_min, f_1_max;
  DBL junk;

  /* Calculate the values at each corner */
  POVFPU_SetLocal(U, fnvec_low[U]);
	POVFPU_SetLocal(V, fnvec_low[V]);

 	f_0_0 = POVFPU_Run(funct) - threshold;

  POVFPU_SetLocal(U, fnvec_low[U]);
	POVFPU_SetLocal(V, fnvec_hi[V]);

 	f_0_1 = POVFPU_Run(funct) - threshold;

  POVFPU_SetLocal(U, fnvec_hi[U]);
	POVFPU_SetLocal(V, fnvec_low[V]);

 	f_1_0 = POVFPU_Run(funct) - threshold;

	POVFPU_SetLocal(U, fnvec_hi[U]);
	POVFPU_SetLocal(V, fnvec_hi[V]);

  f_1_1 = POVFPU_Run(funct) - threshold;

  /* Determine a min and a max along the left edge of the patch */
  Interval( fnvec_hi[V]-fnvec_low[V], f_0_0, f_0_1, max_gradient, &f_0_min, &f_0_max);

  /* Determine a min and a max along the right edge of the patch */
  Interval( fnvec_hi[V]-fnvec_low[V], f_1_0, f_1_1, max_gradient, &f_1_min, &f_1_max);

  /* Assume that the upper bounds of both edges are attained at the same
     u coordinate and determine what an upper bound along that line would
     be if it existed.  That's the worst-case maximum value we can reach. */
  Interval( fnvec_hi[U]-fnvec_low[U], f_0_max, f_1_max, max_gradient, &junk, &hi);

  /* same as above to get a lower bound from the two edge lower bounds */
  Interval( fnvec_hi[U]-fnvec_low[U], f_0_min, f_1_min, max_gradient, &low, &junk);

  /*
  char str[200];
  static int its=0;
  its++;
  if (its>20) Error("Boom!");
  sprintf( str, "%lf     %lf %lf %lf %lf   %lf %lf\n%lf %lf %lf %lf    %lf %lf    %lf %lf    %lf %lf\n",
    max_gradient,
     fnvec_low[U], fnvec_low[V],
     fnvec_hi[U], fnvec_hi[V],
     fnvec_hi[U]-fnvec_low[U],
     fnvec_hi[V]-fnvec_low[V],

     f_0_0,f_0_1,f_1_0,f_1_1,
     f_0_min,f_0_max,
     f_1_min,f_1_max,
     low,hi);
  Warning( 0, str );
  */
  /* f_min and f_max now contain the maximum interval. */
}

END_POV_NAMESPACE
