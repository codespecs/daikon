/****************************************************************************
 *                  sphsweep.h
 *
 * Contributed by Jochen Lippert
 *
 * This module contains the global defines, typedefs, and prototypes
 * for sphsweep.cpp.
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
 * $File: //depot/povray/3.6-release/source/sphsweep.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef SPHERE_SWEEP_H
#define SPHERE_SWEEP_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define SPHERE_SWEEP_OBJECT 	(BASIC_OBJECT)

/* Sphere sweep interpolated by a piecewise linear function */
#define LINEAR_SPHERE_SWEEP				0

/* Sphere sweep interpolated by a cubic Catmull-Rom-Spline function */
#define CATMULL_ROM_SPLINE_SPHERE_SWEEP	1

/* Sphere sweep approximated by a cubic B-Spline function */
#define B_SPLINE_SPHERE_SWEEP			2

/* Maximum number of coefficients of the polynomials describing one segment */
#define SPH_SWP_MAX_COEFS				4



/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct Sphere_Sweep_Struct SPHERE_SWEEP;
typedef struct Sphere_Sweep_Sphere_Struct SPHSWEEP_SPH;
typedef struct Sphere_Sweep_Segment_Struct SPHSWEEP_SEG;

/* The complete object */
struct Sphere_Sweep_Struct
{
	OBJECT_FIELDS
	int			Interpolation;
	int			Num_Modeling_Spheres;		/* Number of modeling spheres    */
	SPHSWEEP_SPH	*Modeling_Sphere;	/* Spheres describing the shape  */
	int			Num_Spheres;				/* Number of single spheres      */
	SPHSWEEP_SPH	*Sphere;			/* Spheres that close segments   */
	int			Num_Segments;				/* Number of tubular segments    */
	SPHSWEEP_SEG	*Segment;		/* Tubular segments              */
	DBL			Depth_Tolerance;			/* Preferred depth tolerance     */
};

/* Single sphere, used to connect two adjacent segments */
struct Sphere_Sweep_Sphere_Struct
{
	VECTOR		Center;
	DBL			Radius;
};

/* One segment of the sphere sweep */
struct Sphere_Sweep_Segment_Struct
{
	SPHSWEEP_SPH	Closing_Sphere[2];		/* Spheres closing the segment   */
	VECTOR	Center_Deriv[2];	/* Derivatives of center funcs for 0 and 1   */
	DBL		Radius_Deriv[2];	/* Derivatives of radius funcs for 0 and 1   */
	int		Num_Coefs;						/* Number of coefficients        */
	VECTOR	Center_Coef[SPH_SWP_MAX_COEFS];	/* Coefs of center polynomial    */
	DBL		Radius_Coef[SPH_SWP_MAX_COEFS];	/* Coefs of radius polynomial    */
};



/*****************************************************************************
* Global variables
******************************************************************************/




/*****************************************************************************
* Global functions
******************************************************************************/

SPHERE_SWEEP *Create_Sphere_Sweep (void);
void Compute_Sphere_Sweep_BBox (SPHERE_SWEEP *Sphere_Sweep);
int Intersect_Sphere_Sweep (RAY *Ray, VECTOR Center, DBL Radius2, DBL *Depth1, DBL *Depth2);
void *Copy_Sphere_Sweep (OBJECT *Object);
void Transform_Sphere_Sweep (OBJECT *Object, TRANSFORM *Trans);
void Destroy_Sphere_Sweep (OBJECT *Object);

void Compute_Sphere_Sweep (SPHERE_SWEEP *Sphere_Sweep);

END_POV_NAMESPACE

#endif
