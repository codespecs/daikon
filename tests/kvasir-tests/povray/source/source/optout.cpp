/****************************************************************************
 *                  optout.cpp
 *
 * This module contains functions for credit, usage and options.
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
 * $File: //depot/povray/3.6-release/source/optout.cpp $
 * $Revision: #3 $
 * $Change: 3032 $
 * $DateTime: 2004/08/02 18:43:41 $
 * $Author: chrisc $
 * $Log$
 *****************************************************************************/

#include <ctype.h>
#include <time.h>
#include <stdarg.h>
#include <algorithm>
#include "frame.h"
#include "vector.h"
#include "atmosph.h"
#include "bezier.h"
#include "blob.h"
#include "bbox.h"
#include "cones.h"
#include "csg.h"
#include "discs.h"
#include "fractal.h"
#include "hfield.h"
#include "lathe.h"
#include "lighting.h"
#include "mesh.h"
#include "polysolv.h"
#include "objects.h"
#include "parse.h"
#include "point.h"
#include "poly.h"
#include "polygon.h"
#include "octree.h"
#include "quadrics.h"
#include "pgm.h"
#include "ppm.h"
#include "prism.h"
#include "radiosit.h"
#include "render.h"
#include "sor.h"
#include "spheres.h"
#include "super.h"
#include "targa.h"
#include "texture.h"
#include "torus.h"
#include "triangle.h"
#include "truetype.h"
#include "userio.h"
#include "userdisp.h"
#include "lbuffer.h"
#include "vbuffer.h"
#include "povray.h"
#include "optout.h"
#include "povmsgid.h"
#include "isosurf.h"

#ifndef DONT_SHOW_IMAGE_LIB_VERSIONS
// these are needed for copyright notices and version numbers
#include "zlib.h"
#include "png.h"
#include "jversion.h"

// Including tiffio.h causes the Windows compile to break. As all we need is the
// version function, we just declare it here.
//#define __STDC__
//#include "tiffio.h"

extern "C" const char* TIFFGetVersion(void);
#endif

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/


/*****************************************************************************
* Local typedefs
******************************************************************************/


/*****************************************************************************
* Local variables
******************************************************************************/


/*****************************************************************************
* Local preprocessor defines
******************************************************************************/


/*****************************************************************************
* Global variables
******************************************************************************/

const char *Primary_Developers[] =
{
	"Chris Cason",
	"Thorsten Froehlich",
	"Nathan Kopp",
	"Ron Parker",
	NULL
};

const char *Contributing_Authors[] =
{
	"Steve Anger",
	"Eric Barish",
	"Dieter Bayer",
	"Steve A. Bennett",
	"David K. Buck",
	"Nicolas Calimet",
	"Aaron A. Collins",
	"Chris Dailey",
	"Steve Demlow",
	"Andreas Dilger",
	"Alexander Enzmann",
	"Dan Farmer",
	"Mark Gordon",
	"Christoph Hormann",
	"Mike Hough",
	"Chris Huff",
	"Kari Kivisalo",
	"Lutz Kretzschmar",
	"Jochen Lippert",
	"Pascal Massimino",
	"Jim McElhiney",
	"Douglas Muir",
	"Juha Nieminen",
	"Bill Pulver",
    "Eduard Schwan",
	"Wlodzimierz Skiba",
	"Robert Skinner",
	"Yvo Smellenbergh",
	"Zsolt Szalavari",
	"Scott Taylor",
	"Massimo Valentini",
	"Timothy Wegner",
	"Drew Wells",
	"Chris Young",
	NULL   // NULL flags the end of the list 
};

const INTERSECTION_STATS_INFO intersection_stats[kPOVList_Stat_Last] =
{
	{ kPOVList_Stat_RBezierTest,		Ray_RBezier_Tests, Ray_RBezier_Tests_Succeeded,
	  "Bezier Patch" },
	{ kPOVList_Stat_BicubicTest,		Ray_Bicubic_Tests, Ray_Bicubic_Tests_Succeeded,
	  "Bicubic Patch" },
	{ kPOVList_Stat_BlobTest,			Ray_Blob_Tests, Ray_Blob_Tests_Succeeded,
	  "Blob" },
	{ kPOVList_Stat_BlobCpTest,			Blob_Element_Tests, Blob_Element_Tests_Succeeded,
	  "Blob Component" },
	{ kPOVList_Stat_BlobBdTest,			Blob_Bound_Tests, Blob_Bound_Tests_Succeeded,
	  "Blob Bound" },
	{ kPOVList_Stat_BoxTest,			Ray_Box_Tests, Ray_Box_Tests_Succeeded,
	  "Box" },
	{ kPOVList_Stat_ConeCylTest,		Ray_Cone_Tests, Ray_Cone_Tests_Succeeded,
	  "Cone/Cylinder" },
	{ kPOVList_Stat_CSGIntersectTest,	Ray_CSG_Intersection_Tests, Ray_CSG_Intersection_Tests_Succeeded,
	  "CSG Intersection" },
	{ kPOVList_Stat_CSGMergeTest,		Ray_CSG_Merge_Tests, Ray_CSG_Merge_Tests_Succeeded,
	  "CSG Merge" },
	{ kPOVList_Stat_CSGUnionTest,		Ray_CSG_Union_Tests, Ray_CSG_Union_Tests_Succeeded,
	  "CSG Union" },
	{ kPOVList_Stat_DiscTest,			Ray_Disc_Tests, Ray_Disc_Tests_Succeeded,
	  "Disc" },
	{ kPOVList_Stat_FractalTest,		Ray_Fractal_Tests, Ray_Fractal_Tests_Succeeded,
	  "Fractal" },
	{ kPOVList_Stat_HFTest,				Ray_HField_Tests, Ray_HField_Tests_Succeeded,
	  "Height Field" },
	{ kPOVList_Stat_HFBoxTest,			Ray_HField_Box_Tests, Ray_HField_Box_Tests_Succeeded,
	  "Height Field Box" },
	{ kPOVList_Stat_HFTriangleTest,		Ray_HField_Triangle_Tests, Ray_HField_Triangle_Tests_Succeeded,
	  "Height Field Triangle" },
	{ kPOVList_Stat_HFBlockTest,		Ray_HField_Block_Tests, Ray_HField_Block_Tests_Succeeded,
	  "Height Field Block" },
	{ kPOVList_Stat_HFCellTest,			Ray_HField_Cell_Tests, Ray_HField_Cell_Tests_Succeeded,
	  "Height Field Cell" },
	{ kPOVList_Stat_IsosurfaceTest,		Ray_IsoSurface_Tests, Ray_IsoSurface_Tests_Succeeded,
	  "Isosurface" },
	{ kPOVList_Stat_IsosurfaceBdTest,	Ray_IsoSurface_Bound_Tests, Ray_IsoSurface_Bound_Tests_Succeeded,
	  "Isosurface Container" },
	{ kPOVList_Stat_IsosurfaceCacheTest,Ray_IsoSurface_Cache, Ray_IsoSurface_Cache_Succeeded,
	  "Isosurface Cache" },
	{ kPOVList_Stat_LatheTest,			Ray_Lathe_Tests, Ray_Lathe_Tests_Succeeded,
	  "Lathe" },
	{ kPOVList_Stat_LatheBdTest,		Lathe_Bound_Tests, Lathe_Bound_Tests_Succeeded,
	  "Lathe Bound" },
	{ kPOVList_Stat_MeshTest,			Ray_Mesh_Tests, Ray_Mesh_Tests_Succeeded,
	  "Mesh" },
	{ kPOVList_Stat_PlaneTest,			Ray_Plane_Tests, Ray_Plane_Tests_Succeeded,
	  "Plane" },
	{ kPOVList_Stat_PolygonTest,		Ray_Polygon_Tests, Ray_Polygon_Tests_Succeeded,
	  "Polygon" },
	{ kPOVList_Stat_PrismTest,			Ray_Prism_Tests, Ray_Prism_Tests_Succeeded,
	  "Prism" },
	{ kPOVList_Stat_PrismBdTest,		Prism_Bound_Tests, Prism_Bound_Tests_Succeeded,
	  "Prism Bound" },
	{ kPOVList_Stat_ParametricTest,		Ray_Parametric_Tests, Ray_Parametric_Tests_Succeeded,
	  "Parametric" },
	{ kPOVList_Stat_ParametricBoxTest,	Ray_Par_Bound_Tests, Ray_Par_Bound_Tests_Succeeded,
	  "Parametric Bound" },
	{ kPOVList_Stat_QuardicTest,		Ray_Quadric_Tests, Ray_Quadric_Tests_Succeeded,
	  "Quadric" },
	{ kPOVList_Stat_QuadPolyTest,		Ray_Poly_Tests, Ray_Poly_Tests_Succeeded,
	  "Quartic/Poly" },
	{ kPOVList_Stat_SphereTest,			Ray_Sphere_Tests, Ray_Sphere_Tests_Succeeded,
	  "Sphere" },
	{ kPOVList_Stat_SphereSweepTest,	Ray_Sphere_Sweep_Tests, Ray_Sphere_Sweep_Tests_Succeeded,
	  "Sphere Sweep" },
	{ kPOVList_Stat_SuperellipsTest,	Ray_Superellipsoid_Tests, Ray_Superellipsoid_Tests_Succeeded,
	  "Superellipsoid" },
	{ kPOVList_Stat_SORTest,			Ray_Sor_Tests, Ray_Sor_Tests_Succeeded,
	  "Surface of Revolution" },
	{ kPOVList_Stat_SORBdTest,			Sor_Bound_Tests, Sor_Bound_Tests_Succeeded,
	  "Surface of Rev. Bound" },
	{ kPOVList_Stat_TorusTest,			Ray_Torus_Tests, Ray_Torus_Tests_Succeeded,
	  "Torus" },
	{ kPOVList_Stat_TorusBdTest,		Torus_Bound_Tests, Torus_Bound_Tests_Succeeded,
	  "Torus Bound" },
	{ kPOVList_Stat_TriangleTest,		Ray_Triangle_Tests, Ray_Triangle_Tests_Succeeded,
	  "Triangle" },
	{ kPOVList_Stat_TTFontTest,			Ray_TTF_Tests, Ray_TTF_Tests_Succeeded,
	  "True Type Font" },
	{ kPOVList_Stat_BoundObjectTest,	Bounding_Region_Tests, Bounding_Region_Tests_Succeeded,
	  "Bounding Object" },
	{ kPOVList_Stat_ClipObjectTest,		Clipping_Region_Tests, Clipping_Region_Tests_Succeeded,
	  "Clipping Object" },
	{ kPOVList_Stat_BoundingBoxTest,	nChecked, nEnqueued,
	  "Bounding Box" },
	{ kPOVList_Stat_LightBufferTest,	LBuffer_Tests, LBuffer_Tests_Succeeded,
	  "Light Buffer" },
	{ kPOVList_Stat_VistaBufferTest,	VBuffer_Tests, VBuffer_Tests_Succeeded,
	  "Vista Buffer" },
	{ kPOVList_Stat_Last, MaxStat, MaxStat, NULL }
};

/*****************************************************************************
* Static functions
******************************************************************************/

const char *Extract_Version(const char *str);
char *GetOptionSwitchString(POVMSObjectPtr msg, POVMSType key);


/*****************************************************************************
*
* FUNCTION
*
*   ExtractVersion
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
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

char LibVersionStringBuffer[20]; // GLOBAL VARIABLE

const char *Extract_Version(const char *str)
{
	int pos = 0;

	for(; *str != 0; str++)
	{
		if(isdigit(*str))
		{
			while(((isalnum(*str)) || (*str == '.')) && (pos < 10))
			{
				LibVersionStringBuffer[pos] = *str;
				str++;
				pos++;
			}
			break;
		}
	}

	LibVersionStringBuffer[pos] = 0;

	return LibVersionStringBuffer;
}

char *GetOptionSwitchString(POVMSObjectPtr msg, POVMSType key)
{
	POVMSBool b = false;

	(void)POVMSUtil_GetBool(msg, key, &b);
	if(b == true)
		return ".On ";

	return ".Off";
}

END_POV_NAMESPACE
