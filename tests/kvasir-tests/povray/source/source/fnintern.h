/****************************************************************************
 *                  fnintern.h
 *
 * This module contains all defines, typedefs, and prototypes for fnintern.cpp.
 *
 * This module is inspired by code by D. Skarda, T. Bily and R. Suzuki.
 * It includes functions based on code first introduced by many other
 * contributors.
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
 * $File: //depot/povray/3.6-release/source/fnintern.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef FNINTERN_H
#define FNINTERN_H

BEGIN_POV_NAMESPACE

typedef struct
{
	DBL (*fn)(DBL *ptr, unsigned int fn);
	unsigned int parameter_cnt;
} Trap;

typedef struct
{
	void (*fn)(DBL *ptr, unsigned int fn, unsigned int sp);
	unsigned int parameter_cnt;
} TrapS;

extern const Trap POVFPU_TrapTable[];
extern const TrapS POVFPU_TrapSTable[];

extern const unsigned int POVFPU_TrapTableSize;
extern const unsigned int POVFPU_TrapSTableSize;

END_POV_NAMESPACE

#endif
