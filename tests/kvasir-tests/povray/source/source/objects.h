/****************************************************************************
 *                  objects.h
 *
 * This module contains all defines, typedefs, and prototypes for OBJECTS.CPP.
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
 * $File: //depot/povray/3.6-release/source/objects.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

/* NOTE: FRAME.H contains other object stuff. */

#ifndef OBJECTS_H
#define OBJECTS_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

/*
 * [DB 7/94]
 *
 * The flag field is used to store all possible flags that are
 * used for objects (up to 32).
 *
 * The flages are manipulated using the following macros:
 *
 *   Set_Flag    (Object, Flag) : set    specified Flag in Object
 *   Clear_Flag  (Object, Flag) : clear  specified Flag in Object
 *   Invert_Flag (Object, Flag) : invert specified Flag in Object
 *   Test_Flag   (Object, Flag) : test   specified Flag in Object
 *
 *   Copy_Flag   (Object1, Object2, Flag) : Set the Flag in Object1 to the
 *                                          value of the Flag in Object2.
 *   Bool_Flag   (Object, Flag, Bool)     : if(Bool) Set flag else Clear flag
 *
 * Object is a pointer to the object.
 * Flag is the number of the flag to test.
 *
 */

#define NO_SHADOW_FLAG            0x0000001L /* Object doesn't cast shadows            */
#define CLOSED_FLAG               0x0000002L /* Object is closed                       */
#define INVERTED_FLAG             0x0000004L /* Object is inverted                     */
#define SMOOTHED_FLAG             0x0000008L /* Object is smoothed                     */
#define CYLINDER_FLAG             0x0000010L /* Object is a cylinder                   */
#define DEGENERATE_FLAG           0x0000020L /* Object is degenerate                   */
#define STURM_FLAG                0x0000040L /* Object should use sturmian root solver */
#define OPAQUE_FLAG               0x0000080L /* Object is opaque                       */
#define MULTITEXTURE_FLAG         0x0000100L /* Object is multi-textured               */
#define INFINITE_FLAG             0x0000200L /* Object is infinite                     */
#define HIERARCHY_FLAG            0x0000400L /* Object can have a bounding hierarchy   */
#define HOLLOW_FLAG               0x0000800L /* Object is hollow (atmosphere inside)   */
#define HOLLOW_SET_FLAG           0x0001000L /* Hollow explicitly set in scene file    */
#define UV_FLAG                   0x0002000L /* Object uses UV mapping                 */
#define DOUBLE_ILLUMINATE_FLAG    0x0004000L /* Illuminate both sides of the surface   */
#define NO_IMAGE_FLAG             0x0008000L /* Object doesn't catch camera rays    [ENB 9/97] */
#define NO_REFLECTION_FLAG        0x0010000L /* Object doesn't cast reflection rays [ENB 9/97] */
#define NO_GLOBAL_LIGHTS_FLAG     0x0020000L /* Object doesn't receive light from global lights */
#define NO_GLOBAL_LIGHTS_SET_FLAG 0x0040000L /* Object doesn't receive light from global lights explicitly set in scene file */
/* Photon-related flags */
#define PH_TARGET_FLAG            0x0080000L /* object receives photons */
#define PH_PASSTHRU_FLAG          0x0100000L /* this is pass through object */
#define PH_RFL_ON_FLAG            0x0200000L /* this object reflects photons */
#define PH_RFL_OFF_FLAG           0x0400000L /* this object does not reflect photons */
#define PH_RFR_ON_FLAG            0x0800000L /* this object refracts photons */
#define PH_RFR_OFF_FLAG           0x1000000L /* this object does not refract photons */
#define PH_IGNORE_PHOTONS_FLAG    0x2000000L /* this object ignores photons */


// #define INVERT_NO_SHADOW_GROUP   0x2000000L // invert Lights on NO shadow


#define Set_Flag(Object, Flag)     \
  { (Object)->Flags |=  (Flag); }

#define Clear_Flag(Object, Flag)   \
  { (Object)->Flags &= ~(Flag); }

#define Invert_Flag(Object, Flag)  \
  { (Object)->Flags ^=  (Flag); }

#define Test_Flag(Object, Flag)    \
  ((Object)->Flags & (Flag))

#define Copy_Flag(Object1, Object2, Flag) \
  { (Object1)->Flags = (((Object1)->Flags) & (~Flag)) | \
                       (((Object2)->Flags) &  (Flag)); }

#define Bool_Flag(Object, Flag, Bool) \
  { if(Bool){ (Object)->Flags |=  (Flag); } else { (Object)->Flags &= ~(Flag); }}



/* Object types. */

#define BASIC_OBJECT                0
#define PATCH_OBJECT                1 /* Has no inside, no inverse */
#define TEXTURED_OBJECT             2 /* Has texture, possibly in children */
#define IS_COMPOUND_OBJECT          4 /* Has children field */
#define STURM_OK_OBJECT             8 /* STRUM legal */
//#define WATER_LEVEL_OK_OBJECT      16 /* WATER_LEVEL legal */
#define LIGHT_SOURCE_OBJECT        32 /* link me in frame.light_sources */
#define BOUNDING_OBJECT            64 /* This is a holder for bounded object */
//#define SMOOTH_OK_OBJECT          128 /* SMOOTH legal */
#define IS_CHILD_OBJECT           256 /* Object is inside a COMPOUND */
/* NK 1998 - DOUBLE_ILLUMINATE is not used anymore - use DOUBLE_ILLUMINATE_FLAG */
#define HIERARCHY_OK_OBJECT       512 /* NO_HIERARCHY legal */
#define LT_SRC_UNION_OBJECT      1024 /* Union of light_source objects only */
#define LIGHT_GROUP_OBJECT       2048 /* light_group union object [trf] */
#define LIGHT_GROUP_LIGHT_OBJECT 4096 /* light in light_group object [trf] */
#define CSG_DIFFERENCE_OBJECT    8192 /* csg difference object */
#define CHILDREN_FLAGS (PATCH_OBJECT+TEXTURED_OBJECT)  /* Reverse inherited flags */



/*****************************************************************************
* Global typedefs
******************************************************************************/

#define TEST_RAY_FLAGS(obj) \
      (  (!backtraceFlag && (!Test_Flag((obj), NO_IMAGE_FLAG) || In_Reflection_Ray == true) && \
          (!Test_Flag((obj), NO_REFLECTION_FLAG) || In_Reflection_Ray == false)) \
       || (backtraceFlag && !Test_Flag((obj), NO_SHADOW_FLAG)))

#define TEST_RAY_FLAGS_SHADOW(obj) \
      (  (!backtraceFlag && (!Test_Flag((obj), NO_IMAGE_FLAG) || In_Reflection_Ray == true) && \
          (!Test_Flag((obj), NO_REFLECTION_FLAG) || In_Reflection_Ray == false)) \
       || (backtraceFlag && !Test_Flag((obj), NO_SHADOW_FLAG)) \
       || (shadow_flag && !Test_Flag((obj), NO_SHADOW_FLAG)))



/*****************************************************************************
* Global variables
******************************************************************************/

extern unsigned int Number_of_istacks;
extern unsigned int Max_Intersections;
extern ISTACK *free_istack;



/*****************************************************************************
* Global functions
******************************************************************************/

void Default_UVCoord (UV_VECT Result, OBJECT *Object, INTERSECTION *Inter);

bool Intersection (INTERSECTION *Ray_Intersection, OBJECT *Object, RAY *Ray);
bool Ray_In_Bound (RAY *Ray, OBJECT *Bounding_Object);
bool Point_In_Clip (VECTOR IPoint, OBJECT *Clip);
OBJECT *Copy_Object (OBJECT *Old);
void Translate_Object (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
void Rotate_Object (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
void Scale_Object (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans);
void Transform_Object (OBJECT *Object, TRANSFORM *Trans);
bool Inside_Object (VECTOR IPoint, OBJECT *Vector);
void Invert_Object (OBJECT *Object);
void Destroy_Object (OBJECT *Object);
ISTACK *open_istack (void);
void close_istack (ISTACK *istk);
void Destroy_IStacks (void);
void Destroy_Single_Object (OBJECT **ObjectPtr);
void Default_UVCoord (UV_VECT Result, OBJECT *Object, INTERSECTION *Inter);

END_POV_NAMESPACE

#endif
