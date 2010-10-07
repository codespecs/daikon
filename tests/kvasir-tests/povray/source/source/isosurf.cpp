/****************************************************************************
 *                  isosurf.cpp
 *
 * This module implements the iso surface shapetype.
 *
 * This module was written by R.Suzuki.
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
 * $File: //depot/povray/3.6-release/source/isosurf.cpp $
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
#include "vector.h"
#include "boxes.h"
#include "bbox.h"
#include "spheres.h"
#include "matrices.h"
#include "objects.h"
#include "isosurf.h"
#include "function.h"
#include "fnpovfpu.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

#define close(x, y) (fabs(x-y) < EPSILON ? 1 : 0)

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

static int All_IsoSurface_Intersections(OBJECT* Object, RAY* Ray, ISTACK* Depth_Stack);
static int Inside_IsoSurface(VECTOR point, OBJECT* Object);
static void IsoSurface_Normal(VECTOR Result, OBJECT* Object, INTERSECTION* Inter);
static void Translate_IsoSurface(OBJECT* Object, VECTOR Vector, TRANSFORM* Trans);
static void Rotate_IsoSurface(OBJECT* Object, VECTOR Vector, TRANSFORM* Trans);
static void Scale_IsoSurface(OBJECT* Object, VECTOR Vector, TRANSFORM* Trans);
static void Transform_IsoSurface(OBJECT* Object, TRANSFORM* Trans);
static void Invert_IsoSurface(OBJECT* Object);

int IsoSurface_Function_Find_Root(ISOSURFACE* , VECTOR, VECTOR, DBL* , DBL* , bool in_shadow_test);
int IsoSurface_Function_Find_Root_R(ISOSURFACE* , ISO_Pair* , ISO_Pair* , DBL, DBL, DBL, bool in_shadow_test);

inline DBL Vector_IsoSurface_Function(ISOSURFACE* ISOSRF, VECTOR VPos);
inline DBL Float_IsoSurface_Function(ISOSURFACE* ISOSRF, DBL* t);
inline DBL Evaluate_Function(FUNCTION funct, VECTOR fnvec);


/*****************************************************************************
* Local variables
******************************************************************************/

METHODS IsoSurface_Methods =
{
	All_IsoSurface_Intersections,
	Inside_IsoSurface, IsoSurface_Normal, Default_UVCoord,
	(COPY_METHOD)Copy_IsoSurface,
	Translate_IsoSurface, Rotate_IsoSurface,
	Scale_IsoSurface, Transform_IsoSurface, Invert_IsoSurface,
	Destroy_IsoSurface
};


/*****************************************************************************
*
* FUNCTION
*
*   All_IsoSurface_Intersections
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

static int All_IsoSurface_Intersections(OBJECT* Object, RAY* Ray, ISTACK* Depth_Stack)
{
	ISOSURFACE * Isosrf = (ISOSURFACE *)Object;
	int Side1 = 0, Side2 = 0, itrace = 0, i_flg = 0;
	DBL Depth1 = 0.0, Depth2 = 0.0, len = 0.0;
	RAY New_Ray;
	VECTOR IPoint;
	VECTOR P, D;
	DBL tmax = 0.0, tmin = 0.0, tmp = 0.0;
	int i = 0 ; /* count of intervals in stack - 1      */
	int IFound = false;
	int begin = 0, end = 0;
	bool in_shadow_test = false;
	VECTOR VTmp;

	Increase_Counter(stats[Ray_IsoSurface_Bound_Tests]);

	in_shadow_test = ((Ray->Optimisiation_Flags & OPTIMISE_SHADOW_TEST) == OPTIMISE_SHADOW_TEST);

	if(Isosrf->container_shape)
	{
		if(Isosrf->Trans != NULL)
		{
			MInvTransPoint(New_Ray.Initial, Ray->Initial, Isosrf->Trans);
			MInvTransDirection(New_Ray.Direction, Ray->Direction, Isosrf->Trans);
			VLength(len, New_Ray.Direction);
			VInverseScaleEq(New_Ray.Direction, len);
			i_flg = Intersect_Sphere(&New_Ray, Isosrf->container.sphere.center, 
			                         (Isosrf->container.sphere.radius) * (Isosrf->container.sphere.radius),
			                         &Depth1, &Depth2);
			Depth1 = Depth1 / len;
			Depth2 = Depth2 / len;
		}
		else
		{
			i_flg = Intersect_Sphere(Ray, Isosrf->container.sphere.center, 
			                         (Isosrf->container.sphere.radius) * (Isosrf->container.sphere.radius), &Depth1, &Depth2);
		}
		Decrease_Counter(stats[Ray_Sphere_Tests]);
		if(i_flg)
			Decrease_Counter(stats[Ray_Sphere_Tests_Succeeded]);
	}
	else
	{
		i_flg = Intersect_Box(Ray, Isosrf->Trans, Isosrf->container.box.corner1, Isosrf->container.box.corner2,
		                      &Depth1, &Depth2, &Side1, &Side2);
	}

	if(Depth1 < 0.0)
		Depth1 = 0.0;

	if(i_flg)									/* IsoSurface_Bound_Tests */
	{
		Increase_Counter(stats[Ray_IsoSurface_Bound_Tests_Succeeded]);
		if(Isosrf->Trans != NULL)
		{
			MInvTransPoint(P, Ray->Initial, Isosrf->Trans);
			MInvTransDirection(D, Ray->Direction, Isosrf->Trans);
		}
		else
		{
			Assign_Vector(P, Ray->Initial);
			Assign_Vector(D, Ray->Direction);
		}
		Isosrf->Inv3 = 1;

		if(Isosrf->closed != false)
		{
			VEvaluateRay(VTmp, P, Depth1, D);
			tmp = Vector_IsoSurface_Function(Isosrf, VTmp);
			if(Depth1 > Isosrf->accuracy)
			{
				if(tmp < 0.0)					/* The ray hits the bounding shape */
				{
					VEvaluateRay(IPoint, Ray->Initial, Depth1, Ray->Direction);
					if(Point_In_Clip(IPoint, Object->Clip))
					{
						push_entry_i1(Depth1, IPoint, Object, Side1, Depth_Stack);
						IFound = true;
						itrace++;
						Isosrf->Inv3 *= -1;
					}
				}
			}
			else
			{
				if(tmp < (Isosrf->max_gradient * Isosrf->accuracy * 4.0))
				{
					Depth1 = Isosrf->accuracy * 5.0;
					VEvaluateRay(VTmp, P, Depth1, D);
					if(Vector_IsoSurface_Function(Isosrf, VTmp) < 0)
						Isosrf->Inv3 = -1;
					/* Change the sign of the function (IPoint is in the bounding shpae.)*/
				}
				VEvaluateRay(VTmp, P, Depth2, D);
				if(Vector_IsoSurface_Function(Isosrf, VTmp) < 0.0)
				{
					VEvaluateRay(IPoint, Ray->Initial, Depth2, Ray->Direction);
					if(Point_In_Clip(IPoint, Object->Clip))
					{
						push_entry_i1(Depth2, IPoint, Object, Side2, Depth_Stack);
						IFound = true;
					}
				}
			}
		}

		/*  METHOD 2   by R. Suzuki */
		tmax = Depth2 = min(Depth2, BOUND_HUGE);
		tmin = Depth1 = min(Depth2, Depth1);
		if((tmax - tmin) < Isosrf->accuracy)
			return (false);
		Increase_Counter(stats[Ray_IsoSurface_Tests]);
		if((Depth1 < Isosrf->accuracy) && (Isosrf->Inv3 == 1))
		{
			/* IPoint is on the isosurface */
			VEvaluateRay(VTmp, P, tmin, D);
			if(fabs(Vector_IsoSurface_Function(Isosrf, VTmp)) < (Isosrf->max_gradient * Isosrf->accuracy * 4.0))
			{
				tmin = Isosrf->accuracy * 5.0;
				VEvaluateRay(VTmp, P, tmin, D);
				if(Vector_IsoSurface_Function(Isosrf, VTmp) < 0)
					Isosrf->Inv3 = -1;
				/* change the sign and go into the isosurface */
			}
		}

		for (; itrace < Isosrf->max_trace; itrace++)
		{
			if(IsoSurface_Function_Find_Root(Isosrf, P, D, &tmin, &tmax, in_shadow_test) == false)
				break;
			else
			{
				VEvaluateRay(IPoint, Ray->Initial, tmin, Ray->Direction);
				if(Point_In_Clip(IPoint, Object->Clip))
				{
					push_entry_i1(tmin, IPoint, Object, 0 /*Side1*/, Depth_Stack);
					IFound = true;
				}
			}
			tmin += Isosrf->accuracy * 5.0;
			if((tmax - tmin) < Isosrf->accuracy)
				break;
			Isosrf->Inv3 *= -1;
		}

		if(IFound)
			Increase_Counter(stats[Ray_IsoSurface_Tests_Succeeded]);
	}

	return (IFound);
}


/*****************************************************************************
*
* FUNCTION
*
*   Inside_IsoSurface
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

static int Inside_IsoSurface(VECTOR IPoint, OBJECT* Object)
{
	VECTOR Origin_To_Center;
	VECTOR New_Point;
	ISOSURFACE * Isosrf = (ISOSURFACE *)Object;
	DBL OCSquared;


	/* Transform the point into box space. */

	if(Isosrf->Trans != NULL)
	{
		MInvTransPoint(New_Point, IPoint, Isosrf->Trans);
	}
	else
	{
		Assign_Vector(New_Point, IPoint);
	}

	if(Isosrf->container_shape != 0)
	{
		/* Use ellipse method. */
		VSub(Origin_To_Center, Isosrf->container.sphere.center, New_Point);
		VDot(OCSquared, Origin_To_Center, Origin_To_Center);
		if(OCSquared > Sqr(Isosrf->container.sphere.radius))
			return (Test_Flag(Isosrf, INVERTED_FLAG));

	}
	else
	{
		/* Test to see if we are outside the box. */
		if((New_Point[X] < Isosrf->container.box.corner1[X]) || (New_Point[X] > Isosrf->container.box.corner2[X]))
			return (Test_Flag(Isosrf, INVERTED_FLAG));
		if((New_Point[Y] < Isosrf->container.box.corner1[Y]) || (New_Point[Y] > Isosrf->container.box.corner2[Y]))
			return (Test_Flag(Isosrf, INVERTED_FLAG));
		if((New_Point[Z] < Isosrf->container.box.corner1[Z]) || (New_Point[Z] > Isosrf->container.box.corner2[Z]))
			return (Test_Flag(Isosrf, INVERTED_FLAG));
	}

	if(Vector_IsoSurface_Function(Isosrf, New_Point) > 0)
		return (Test_Flag(Isosrf, INVERTED_FLAG));

	/* Inside the box. */
	return (!Test_Flag(Isosrf, INVERTED_FLAG));
}



/*****************************************************************************
*
* FUNCTION
*
*   IsoSurface_Normal
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

static void IsoSurface_Normal(VECTOR Result, OBJECT* Object, INTERSECTION* Inter)
{
	VECTOR New_Point, TPoint;
	ISOSURFACE *Isosrf = (ISOSURFACE *)Object;
	FUNCTION Function = *(((ISOSURFACE *)Object)->Function);
	DBL funct;

	switch (Inter->i1)
	{
		case SIDE_X_0:
			Make_Vector(Result, -1.0, 0.0, 0.0);
			break;
		case SIDE_X_1:
			Make_Vector(Result, 1.0, 0.0, 0.0);
			break;
		case SIDE_Y_0:
			Make_Vector(Result, 0.0, -1.0, 0.0);
			break;
		case SIDE_Y_1:
			Make_Vector(Result, 0.0, 1.0, 0.0);
			break;
		case SIDE_Z_0:
			Make_Vector(Result, 0.0, 0.0, -1.0);
			break;
		case SIDE_Z_1:
			Make_Vector(Result, 0.0, 0.0, 1.0);
			break;

		default:

			/* Transform the point into the isosurface space */
			if(((ISOSURFACE *)Object)->Trans != NULL)
				MInvTransPoint(New_Point, Inter->IPoint, Isosrf->Trans);
			else
				Assign_Vector(New_Point, Inter->IPoint);

			if(Isosrf->container_shape)
			{
				VSub(Result, New_Point, Isosrf->container.sphere.center);
				VLength(funct, Result);
				if(fabs(funct - Isosrf->container.sphere.radius) < EPSILON)
				{
					VInverseScaleEq(Result, Isosrf->container.sphere.radius);
					break;
				}
			}

			Assign_Vector(TPoint, New_Point);
			funct = Evaluate_Function(Function, TPoint);
			Assign_Vector(TPoint, New_Point);
			TPoint[X] += Isosrf->accuracy;
			Result[X] = Evaluate_Function(Function, TPoint) - funct;
			Assign_Vector(TPoint, New_Point);
			TPoint[Y] += Isosrf->accuracy;
			Result[Y] = Evaluate_Function(Function, TPoint) - funct;
			Assign_Vector(TPoint, New_Point);
			TPoint[Z] += Isosrf->accuracy;
			Result[Z] = Evaluate_Function(Function, TPoint) - funct;

			if((Result[X] == 0) && (Result[Y] == 0) && (Result[Z] == 0))
				Result[X] = 1.0;
			VNormalize(Result, Result);
	}


	/* Transform the point into the boxes space. */

	if(((ISOSURFACE *)Object)->Trans != NULL)
	{
		MTransNormal(Result, Result, ((ISOSURFACE *)Object)->Trans);

		VNormalize(Result, Result);
	}
}



/*****************************************************************************
*
* FUNCTION
*
*   Translate_IsoSurface
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

static void Translate_IsoSurface(OBJECT* Object, VECTOR, TRANSFORM* Trans)
{
	Transform_IsoSurface(Object, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Rotate_IsoSurface
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

static void Rotate_IsoSurface(OBJECT* Object, VECTOR, TRANSFORM* Trans)
{
	Transform_IsoSurface(Object, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Scale_IsoSurface
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

static void Scale_IsoSurface(OBJECT* Object, VECTOR, TRANSFORM* Trans)
{
	Transform_IsoSurface(Object, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Invert_IsoSurface
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

static void Invert_IsoSurface(OBJECT* Object)
{
	Invert_Flag(Object, INVERTED_FLAG);
}



/*****************************************************************************
*
* FUNCTION
*
*   Transform_IsoSurface
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

static void Transform_IsoSurface(OBJECT* Object, TRANSFORM* Trans)
{
	ISOSURFACE * Isosrf = (ISOSURFACE *)Object;

	if(Isosrf->Trans == NULL)
	{
		Isosrf->Trans = Create_Transform();
	}

	Compose_Transforms(Isosrf->Trans, Trans);

	Compute_IsoSurface_BBox(Isosrf);
}



/*****************************************************************************
*
* FUNCTION
*
*   Create_IsoSurface
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

ISOSURFACE* Create_IsoSurface()
{
	ISOSURFACE * New;

	New = (ISOSURFACE *)POV_MALLOC(sizeof(ISOSURFACE), "isosurface");

	INIT_OBJECT_FIELDS(New, ISOSURFACE_OBJECT, &IsoSurface_Methods)

	Make_Vector(New->container.box.corner1, -1.0, -1.0, -1.0);
	Make_Vector(New->container.box.corner2, 1.0, 1.0, 1.0);

	Make_BBox(New->BBox, -1.0, -1.0, -1.0, 2.0, 2.0, 2.0);

	New->Trans = Create_Transform();

	New->Function = NULL;
	New->accuracy = 0.001;
	New->max_trace = 1;

	New->cache = false;
	New->eval_param[0] = 0.0; // 1.1; // not necessary
	New->eval_param[1] = 0.0; // 1.4; // not necessary
	New->eval_param[2] = 0.0; // 0.99; // not necessary
	New->eval = false;
	New->closed = true;
	New->Inv3 = 1;
	New->container_shape = 0;

	New->max_gradient = 1.1;
	New->gradient = 0.0;
	New->threshold = 0.0;

	New->mginfo = (ISO_Max_Gradient *)POV_MALLOC(sizeof(ISO_Max_Gradient), "isosurface max_gradient info");
	New->mginfo->refcnt = 1;
	New->mginfo->max_gradient = 0.0;
	New->mginfo->gradient = 0.0; // not really necessary yet [trf]
	New->mginfo->eval_max = 0.0;
	New->mginfo->eval_cnt = 0.0;
	New->mginfo->eval_gradient_sum = 0.0;

	return New;
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_IsoSurface
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

void* Copy_IsoSurface(OBJECT* Object)
{
	ISOSURFACE *New;

	New = (ISOSURFACE *)POV_MALLOC(sizeof(ISOSURFACE), "isosurface");
	*New = *((ISOSURFACE *)Object);

	New->Function = Copy_Function(((ISOSURFACE *)Object)->Function);
	New->Trans = Copy_Transform(((ISOSURFACE *)Object)->Trans);

	New->mginfo->refcnt++;

	return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_IsoSurface
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

void Destroy_IsoSurface(OBJECT* Object)
{
	ISOSURFACE *Isosrf = (ISOSURFACE *)Object;
	ISO_Max_Gradient *mginfo = Isosrf->mginfo;

	mginfo->refcnt--;
	mginfo->gradient = max(Isosrf->gradient, mginfo->gradient);
	mginfo->max_gradient = max(Isosrf->max_gradient, mginfo->max_gradient);

	if((Stage == STAGE_SHUTDOWN) && (mginfo->refcnt == 0))
	{
		FunctionCode *fn = POVFPU_GetFunction(*(Isosrf->Function));

		if(fn != NULL)
		{
			if(Isosrf->eval == false)
			{
				// Only show the warning if necessary!
				// BTW, not being too picky here is a feature and not a bug ;-)  [trf]
				if((mginfo->gradient > EPSILON) && (mginfo->max_gradient > EPSILON))
				{
					DBL diff = mginfo->max_gradient - mginfo->gradient;
					DBL prop = fabs(mginfo->max_gradient / mginfo->gradient);

					if(((prop <= 0.9) && (diff <= -0.5)) ||
					   (((prop <= 0.95) || (diff <= -0.1)) && (mginfo->max_gradient < 10.0)))
					{
						WarningAt(0, fn->filename, fn->filepos.lineno, fn->filepos.offset,
						          "The maximum gradient found was %0.3f, but max_gradient of the\n"
						          "isosurface was set to %0.3f. The isosurface may contain holes!\n"
						          "Adjust max_gradient to get a proper rendering of the isosurface.",
						          (float)(mginfo->gradient),
						          (float)(mginfo->max_gradient));
					}
					else if((diff >= 10.0) || ((prop >= 1.1) && (diff >= 0.5)))
					{
						WarningAt(0, fn->filename, fn->filepos.lineno, fn->filepos.offset,
						          "The maximum gradient found was %0.3f, but max_gradient of\n"
						          "the isosurface was set to %0.3f. Adjust max_gradient to\n"
						          "get a faster rendering of the isosurface.",
						          (float)(mginfo->gradient),
						          (float)(mginfo->max_gradient));
					}
				}
			}
			else
			{
				DBL diff = (mginfo->eval_max / max(mginfo->eval_max - mginfo->eval_var, EPSILON));

				if((Isosrf->eval_param[0] > mginfo->eval_max) ||
				   (Isosrf->eval_param[1] > diff))
				{
					mginfo->eval_cnt = max(mginfo->eval_cnt, 1.0); // make sure it won't be zero

					WarningAt(0, fn->filename, fn->filepos.lineno, fn->filepos.offset,
					          "Evaluate found a maximum gradient of %0.3f and an average\n"
					          "gradient of %0.3f. The maximum gradient variation was %0.3f.\n",
					          (float)(mginfo->eval_max),
					          (float)(mginfo->eval_gradient_sum / mginfo->eval_cnt),
					          (float)(mginfo->eval_var));

					if(opts.Options & VERBOSE)
					{
						diff = max(diff, 1.0); // prevent contradicting output

						Debug_Info("It is recommended to adjust the parameters of 'evaluate' to:\n"
					               "First parameter less than %0.3f\n"
					               "Second parameter less than %0.3f and greater than 1.0\n"
					               "Third parameter greater than %0.3f and less than 1.0\n",
						          (float)(mginfo->eval_max),
						          (float)(diff),
						          (float)(1.0 / diff));
					}
				}
			}
		}
	}

	if(mginfo->refcnt == 0)
		POV_FREE(mginfo);

	Destroy_Function(Isosrf->Function);
	Destroy_Transform(Isosrf->Trans);
	POV_FREE(Object);
}



/*****************************************************************************
*
* FUNCTION
*
*   Compute_IsoSurface_BBox
*
* INPUT
*
*   ISOSURFACE - IsoSurface
*
* OUTPUT
*
*   ISOSURFACE
*
* RETURNS
*
* AUTHOR
*
* DESCRIPTION
*
*   Calculate the bounding box of an Isosurface.
*
* CHANGES
*
******************************************************************************/

void Compute_IsoSurface_BBox(ISOSURFACE* IsoSurface)
{
	if(IsoSurface->container_shape != 0)
	{
		Make_BBox(IsoSurface->BBox,
		          IsoSurface->container.sphere.center[X] - IsoSurface->container.sphere.radius,
		          IsoSurface->container.sphere.center[Y] - IsoSurface->container.sphere.radius,
		          IsoSurface->container.sphere.center[Z] - IsoSurface->container.sphere.radius,
		          IsoSurface->container.sphere.radius * 2,
		          IsoSurface->container.sphere.radius * 2,
		          IsoSurface->container.sphere.radius * 2);
	}
	else
	{
		// [ABX 20.01.2004] Low_Left introduced to hide BCC 5.5 bug
		BBOX_VECT& Low_Left = IsoSurface->BBox.Lower_Left;

		Assign_BBox_Vect(Low_Left, IsoSurface->container.box.corner1);
		VSub(IsoSurface->BBox.Lengths, IsoSurface->container.box.corner2, IsoSurface->container.box.corner1);
	}

	if(IsoSurface->Trans != NULL)
	{
		Recompute_BBox(&IsoSurface->BBox, IsoSurface->Trans);
	}
}


/*****************************************************************************
*
* FUNCTION
*
*   IsoSurface_Function_Find_Root
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

int IsoSurface_Function_Find_Root(ISOSURFACE* ISOSRF, VECTOR P, VECTOR D, DBL* Depth1, DBL* Depth2, bool in_shadow_test)
{
	DBL dt, t21, l_b, l_e, oldmg;
	ISO_Pair EP1, EP2;
	VECTOR VTmp;

	Increase_Counter(stats[Ray_IsoSurface_Find_Root]);

	VLength(ISOSRF->Vlength, D);

	if(ISOSRF->cache)
	{
		Increase_Counter(stats[Ray_IsoSurface_Cache]);
		VEvaluateRay(VTmp, P, *Depth1, D);
		VSubEq(VTmp, ISOSRF->P);
		VLength(l_b, VTmp);
		VEvaluateRay(VTmp, P, *Depth2, D);
		VSubEq(VTmp, ISOSRF->D);
		VLength(l_e, VTmp);
		if((ISOSRF->fmax - ISOSRF->max_gradient * max(l_b, l_e)) > 0.0)
		{
			Increase_Counter(stats[Ray_IsoSurface_Cache_Succeeded]);
			return false;
		}
	}

	Assign_Vector(ISOSRF->P, P);
	Assign_Vector(ISOSRF->D, D);

	ISOSRF->cache = false;
	EP1.t = *Depth1;
	EP1.f = Float_IsoSurface_Function(ISOSRF, Depth1);
	ISOSRF->fmax = EP1.f;
	if((ISOSRF->closed == false) && (EP1.f < 0.0))
	{
		ISOSRF->Inv3 *= -1;
		EP1.f *= -1;
	}

	EP2.t = *Depth2;
	EP2.f = Float_IsoSurface_Function(ISOSRF, Depth2);
	ISOSRF->fmax = min(EP2.f, ISOSRF->fmax);

	oldmg = ISOSRF->max_gradient;
	t21 = (*Depth2 - *Depth1);
	if((ISOSRF->eval == true) && (ISOSRF->max_gradient > ISOSRF->eval_param[0]))
		ISOSRF->max_gradient *= ISOSRF->eval_param[2];
	dt = ISOSRF->max_gradient * ISOSRF->Vlength * t21;
	if(IsoSurface_Function_Find_Root_R(ISOSRF, &EP1, &EP2, dt, t21, 1.0 / (ISOSRF->Vlength * t21), in_shadow_test))
	{
		if(ISOSRF->eval == true)
		{
			DBL curvar = fabs(ISOSRF->max_gradient - oldmg);

			if(curvar > ISOSRF->mginfo->eval_var)
				ISOSRF->mginfo->eval_var = curvar;

			ISOSRF->mginfo->eval_cnt++;
			ISOSRF->mginfo->eval_gradient_sum += ISOSRF->max_gradient;

			if(ISOSRF->max_gradient > ISOSRF->mginfo->eval_max)
				ISOSRF->mginfo->eval_max = ISOSRF->max_gradient;
		}

		*Depth1 = ISOSRF->tl;

		return true;
	}
	else if(!in_shadow_test)
	{
		ISOSRF->cache = true;
		VEvaluateRay(ISOSRF->P, P, EP1.t, D);
		VEvaluateRay(ISOSRF->D, P, EP2.t, D);

		return false;
	}

	return false;
}

/*****************************************************************************
*
* FUNCTION
*
*   IsoSurface_Function_Find_Root_R
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

int IsoSurface_Function_Find_Root_R(ISOSURFACE* ISOSRF, ISO_Pair* EP1, ISO_Pair* EP2, DBL dt, DBL t21, DBL len, bool in_shadow_test)
{
	ISO_Pair EPa;
	DBL temp;

	temp = fabs((EP2->f - EP1->f) * len);
	if(ISOSRF->gradient < temp)
		ISOSRF->gradient = temp;

	if((ISOSRF->eval == true) && (ISOSRF->max_gradient < temp * ISOSRF->eval_param[1]))
	{
		ISOSRF->max_gradient = temp * ISOSRF->eval_param[1] * ISOSRF->eval_param[1];
		dt = ISOSRF->max_gradient * ISOSRF->Vlength * t21;
	}

	if(t21 < ISOSRF->accuracy)
	{
		if(EP2->f < 0)
		{
			ISOSRF->tl = EP2->t;
			return true;
		}
		else
			return false;
	}

	if((EP1->f + EP2->f - dt) < 0)
	{
		t21 *= 0.5;
		dt *= 0.5;
		EPa.t = EP1->t + t21;
		EPa.f = Float_IsoSurface_Function(ISOSRF, &EPa.t);

		ISOSRF->fmax = min(EPa.f, ISOSRF->fmax);
		if(!IsoSurface_Function_Find_Root_R(ISOSRF, EP1, &EPa, dt, t21, len * 2.0, in_shadow_test))
			return (IsoSurface_Function_Find_Root_R(ISOSRF, &EPa, EP2, dt, t21, len * 2.0, in_shadow_test));
		else
			return true;
	}
	else
		return false;
}


/*****************************************************************************
*
* FUNCTION
*
*   Vector_IsoSurface_Function
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

inline DBL Vector_IsoSurface_Function(ISOSURFACE* ISOSRF, VECTOR VPos)
{
	return Evaluate_Function(*(ISOSRF->Function), VPos) - ISOSRF->threshold;
}


/*****************************************************************************
*
* FUNCTION
*
*   Float_IsoSurface_Function
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   R. Suzuki
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

inline DBL Float_IsoSurface_Function(ISOSURFACE* ISOSRF, DBL* t)
{
	VECTOR VTmp;

	VEvaluateRay(VTmp, ISOSRF->P, *t, ISOSRF->D);

	return ((DBL)(ISOSRF->Inv3) * (Evaluate_Function(*(ISOSRF->Function), VTmp) - ISOSRF->threshold));
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

DBL Evaluate_Function(FUNCTION funct, VECTOR fnvec)
{
	POVFPU_SetLocal(X, fnvec[X]);
	POVFPU_SetLocal(Y, fnvec[Y]);
	POVFPU_SetLocal(Z, fnvec[Z]);

   	return POVFPU_Run(funct);
}

END_POV_NAMESPACE

