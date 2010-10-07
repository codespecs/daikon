/****************************************************************************
 *                  pointer.h
 *
 * This module contains to Pointer class which is a limited version of std::auto_ptr.
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
 * $File: //depot/povray/3.6-release/source/base/pointer.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef POINTER_H
#define POINTER_H

#include "configbase.h"

BEGIN_POV_BASE_NAMESPACE

template<class X> class Pointer
{
	public:
		explicit Pointer(X *p = NULL)
		{
			ptr = p;
		}

		Pointer(X& a)
		{
			ptr = a.release();
		}

		template<class Y> Pointer(Pointer<Y>& a)
		{
			ptr = a.release();
		}

		~Pointer()
		{
			if(ptr != NULL)
				delete ptr;
			ptr = NULL;
		}

		Pointer& operator=(Pointer& a)
		{
			reset(a.release());
			return *this;
		}

		template<class Y> Pointer& operator=(Pointer<Y>& a)
		{
			reset(a.release());
			return *this;
		}

		X& operator*() const
		{
			return *ptr;
		}

		X *operator->() const
		{
			return ptr;
		}

		X *get() const
		{
			return ptr;
		}

		X *release()
		{
			X *t = ptr;
			ptr = NULL;
			return t;
		}

		void reset(X *p = NULL)
		{
			if(ptr != NULL)
				delete ptr;
			ptr = p;
		}

		bool operator==(void *p)
		{
			return (ptr == p);
		}

		bool operator!=(void *p)
		{
			return (ptr != p);
		}
	private:
		X *ptr;
};

END_POV_BASE_NAMESPACE

#endif
