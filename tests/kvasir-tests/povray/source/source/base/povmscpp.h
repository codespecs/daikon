/****************************************************************************
 *                  povmscpp.h
 *
 * This module contains all defines, typedefs, and prototypes for the
 * C++ interface version of povms.cpp.
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
 * $File: //depot/povray/3.6-release/source/base/povmscpp.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#ifndef POVMSCPP_H
#define POVMSCPP_H

#include "povms.h"
#include "pov_err.h"

/*****************************************************************************
* Global classes
******************************************************************************/

class POVMS_Container;
class POVMS_Attribute;
class POVMS_List;
class POVMS_Object;
class POVMS_Message;
class POVMS_MessageReceiver;

class POVMS_Container
{
		friend void POVMS_SendMessage(POVMSContext, POVMS_Message&, POVMS_Message *, int);
		friend class POVMS_List;
		friend class POVMS_Object;
		friend class POVMS_MessageReceiver;
	public:
		POVMS_Container();
		virtual ~POVMS_Container();

		POVMSType Type();
		long Size();
		bool IsNull();
	protected:
		POVMSData data;

		void DetachData();
};

class POVMS_Attribute : public POVMS_Container
{
	public:
		POVMS_Attribute();
		POVMS_Attribute(const char *str);
		POVMS_Attribute(POVMSInt value);
		POVMS_Attribute(POVMSLong value);
		POVMS_Attribute(POVMSFloat value);
		POVMS_Attribute(bool value);
		POVMS_Attribute(POVMSType value);
		POVMS_Attribute(POVMSAttribute& convert);
		POVMS_Attribute(POVMS_Attribute& source);
		virtual ~POVMS_Attribute();

		POVMS_Attribute& operator=(POVMS_Attribute& source);

		void Get(POVMSType type, void *data, int *maxdatasize);
		void Set(POVMSType type, void *data, int datasize);
};

class POVMS_List : public POVMS_Container
{
	public:
		POVMS_List();
		POVMS_List(POVMSAttributeList& convert);
		POVMS_List(POVMS_List& source);
		virtual ~POVMS_List();

		POVMS_List& operator=(POVMS_List& source);

		void Append(POVMS_Attribute& item);
		void Append(POVMS_List& item);
		void Append(POVMS_Object& item);
		void GetNth(int index, POVMS_Attribute& item);
		void GetNth(int index, POVMS_List& item);
		void GetNth(int index, POVMS_Object& item);
		void SetNth(int index, POVMS_Attribute& item);
		void SetNth(int index, POVMS_List& item);
		void SetNth(int index, POVMS_Object& item);
		void RemoveNth(int index);
		void Clear();
};

class POVMS_Object : public POVMS_Container
{
		friend class POVMS_Message;
		friend class POVMS_MessageReceiver;
	public:
		POVMS_Object(POVMSType objclass = kPOVMSType_WildCard);
		POVMS_Object(POVMSObject& convert);
		POVMS_Object(POVMSObjectPtr convert);
		POVMS_Object(POVMS_Object& source);
		~POVMS_Object();

		POVMS_Object& operator=(POVMS_Object& source);

		void Get(POVMSType key, POVMS_Attribute& attr);
		void Get(POVMSType key, POVMS_List& attr);
		void Get(POVMSType key, POVMS_Object& attr);
		void Set(POVMSType key, POVMS_Attribute& attr);
		void Set(POVMSType key, POVMS_List& attr);
		void Set(POVMSType key, POVMS_Object& attr);
		void Remove(POVMSType key);
		bool Exist(POVMSType key);
		void Merge(POVMS_Object& source);

		const POVMSObject& operator*() const;
		const POVMSObjectPtr operator->() const;
		POVMSObject operator()();

		void SetString(POVMSType key, const char *str); // Note: Strings may not contain \0 characters codes!
		void SetInt(POVMSType key, POVMSInt value);
		void SetLong(POVMSType key, POVMSLong value);
		void SetFloat(POVMSType key, POVMSFloat value);
		void SetBool(POVMSType key, POVMSBool value);
		void SetType(POVMSType key, POVMSType value);
		int GetStringLength(POVMSType key); // Note: Includes trailing \0 character code!
		int GetString(POVMSType key, char *str, int maxlen);
		POVMSInt GetInt(POVMSType key);
		POVMSLong GetLong(POVMSType key);
		POVMSFloat GetFloat(POVMSType key);
		POVMSBool GetBool(POVMSType key);
		POVMSType GetType(POVMSType key);

		template<class T> void Read(T& stream)
		{
			POVMSStream headerdata[16];
			POVMSStream *ptr = NULL;
			int objectsize = 0;

			if(!stream.read((void *)headerdata, 16))
				throw int(POV_BASE_NAMESPACE::kFileDataErr);
			if(ReadHeader(headerdata, objectsize) == false)
				throw int(POV_BASE_NAMESPACE::kVersionErr);
			ptr = new POVMSStream[objectsize];
			if(!stream.read((void *)ptr, objectsize))
			{
				delete[] ptr;
				throw int(POV_BASE_NAMESPACE::kFileDataErr);
			}
			if(ReadObject(ptr, objectsize) == false)
			{
				delete[] ptr;
				throw int(POV_BASE_NAMESPACE::kCannotHandleDataErr);
			}
			delete[] ptr;
		}

		template<class T> void Write(T& stream)
		{
			POVMSStream headerdata[16];
			POVMSStream *ptr = NULL;
			int objectsize = 0;

			if(WriteHeader(headerdata, objectsize) == false)
				throw int(POV_BASE_NAMESPACE::kNullPointerErr);
			if(!stream.write((void *)headerdata, 16))
				throw int(POV_BASE_NAMESPACE::kFileDataErr);
			ptr = new POVMSStream[objectsize];
			if(WriteObject(ptr, objectsize) == false)
			{
				delete[] ptr;
				throw int(POV_BASE_NAMESPACE::kNullPointerErr);
			}
			if(!stream.write((void *)ptr, objectsize))
			{
				delete[] ptr;
				throw int(POV_BASE_NAMESPACE::kFileDataErr);
			}
			delete[] ptr;
		}
	private:
		bool ReadHeader(POVMSStream *stream, int& objectsize);
		bool ReadObject(POVMSStream *stream, int objectsize);
		bool WriteHeader(POVMSStream *stream, int& objectsize);
		bool WriteObject(POVMSStream *stream, int objectsize);
};

class POVMS_Message : public POVMS_Object
{
	public:
		POVMS_Message(POVMSType objclass = kPOVMSType_Null, POVMSType msgclass = kPOVMSType_WildCard, POVMSType msgid = kPOVMSType_WildCard);
		POVMS_Message(POVMSObject& convert);
		POVMS_Message(POVMSObjectPtr convert);
		POVMS_Message(POVMS_Object& convert);
		POVMS_Message(POVMS_Message& source);

		POVMS_Message& operator=(POVMS_Message& source);

		POVMSType GetClass();
		POVMSType GetIdentifier();

		POVMSAddress GetSourceAddress();
		POVMSAddress GetDestinationAddress();
		void SetDestinationAddress(POVMSAddress);
};

class POVMS_MessageReceiver
{
	private:
		class HandlerOO
		{
			public:
				virtual void Call(POVMS_Message&, POVMS_Message&, int) = 0;
		};
		class Handler
		{
			public:
				virtual void Call(POVMSObjectPtr, POVMSObjectPtr, int) = 0;
		};
	protected:
		template<class T> class MemberHandlerOO : public HandlerOO
		{
			public:
				typedef void (T::*MemberHandlerPtr)(POVMS_Message&, POVMS_Message&, int);

				MemberHandlerOO()
				{
					classptr = NULL;
					handlerptr = NULL;
				}

				MemberHandlerOO(T *cptr, MemberHandlerPtr hptr)
				{
					classptr = cptr;
					handlerptr = hptr;
				}

				void Call(POVMS_Message& msg, POVMS_Message& result, int mode)
				{
					if((classptr != NULL) && (handlerptr != NULL))
						(classptr->*handlerptr)(msg, result, mode);
					else
						throw int(POV_BASE_NAMESPACE::kNullPointerErr);
				}
			private:
				MemberHandlerPtr handlerptr;
				T *classptr;
		};

		template<class T> class MemberHandler : public Handler
		{
			public:
				typedef void (T::*MemberHandlerPtr)(POVMSObjectPtr, POVMSObjectPtr, int);

				MemberHandler()
				{
					classptr = NULL;
					handlerptr = NULL;
				}

				MemberHandler(T *cptr, MemberHandlerPtr hptr)
				{
					classptr = cptr;
					handlerptr = hptr;
				}

				void Call(POVMSObjectPtr msg, POVMSObjectPtr result, int mode)
				{
					if((classptr != NULL) && (handlerptr != NULL))
						(classptr->*handlerptr)(msg, result, mode);
					else
						throw int(POV_BASE_NAMESPACE::kNullPointerErr);
				}
			private:
				MemberHandlerPtr handlerptr;
				T *classptr;
		};

		class FunctionHandlerOO : public HandlerOO
		{
			public:
				typedef void (*FunctionHandlerPtr)(POVMS_Message&, POVMS_Message&, int, void *);

				FunctionHandlerOO()
				{
					handlerptr = NULL;
					privatedata = NULL;
				}

				FunctionHandlerOO(FunctionHandlerPtr hptr, void *pptr)
				{
					handlerptr = hptr;
					privatedata = pptr;
				}

				void Call(POVMS_Message& msg, POVMS_Message& result, int mode)
				{
					if(handlerptr != NULL)
						handlerptr(msg, result, mode, privatedata);
					else
						throw int(POV_BASE_NAMESPACE::kNullPointerErr);
				}
			private:
				FunctionHandlerPtr handlerptr;
				void *privatedata;
		};

		class FunctionHandler : public Handler
		{
			public:
				typedef void (*FunctionHandlerPtr)(POVMSObjectPtr, POVMSObjectPtr, int, void *);

				FunctionHandler()
				{
					handlerptr = NULL;
					privatedata = NULL;
				}

				FunctionHandler(FunctionHandlerPtr hptr, void *pptr)
				{
					handlerptr = hptr;
					privatedata = pptr;
				}

				void Call(POVMSObjectPtr msg, POVMSObjectPtr result, int mode)
				{
					if(handlerptr != NULL)
						handlerptr(msg, result, mode, privatedata);
					else
						throw int(POV_BASE_NAMESPACE::kNullPointerErr);
				}
			private:
				FunctionHandlerPtr handlerptr;
				void *privatedata;
		};

		POVMS_MessageReceiver(POVMSContext contextref);
		virtual ~POVMS_MessageReceiver();

		template<class T> void InstallFront(POVMSType hclass, POVMSType hid, T *cptr, typename MemberHandlerOO<T>::MemberHandlerPtr hptr)
		{
			AddNodeFront(hclass, hid, new MemberHandlerOO<T>(cptr, hptr), NULL);
		}

		template<class T> void InstallFront(POVMSType hclass, POVMSType hid, T *cptr, typename MemberHandler<T>::MemberHandlerPtr hptr)
		{
			AddNodeFront(hclass, hid, NULL, new MemberHandler<T>(cptr, hptr));
		}

		void InstallFront(POVMSType hclass, POVMSType hid, FunctionHandlerOO::FunctionHandlerPtr hptr, void *pptr)
		{
			AddNodeFront(hclass, hid, new FunctionHandlerOO(hptr, pptr), NULL);
		}

		void InstallFront(POVMSType hclass, POVMSType hid, FunctionHandler::FunctionHandlerPtr hptr, void *pptr)
		{
			AddNodeFront(hclass, hid, NULL, new FunctionHandler(hptr, pptr));
		}

		template<class T> void InstallBack(POVMSType hclass, POVMSType hid, T *cptr, typename MemberHandlerOO<T>::MemberHandlerPtr hptr)
		{
			AddNodeBack(hclass, hid, new MemberHandlerOO<T>(cptr, hptr), NULL);
		}

		template<class T> void InstallBack(POVMSType hclass, POVMSType hid, T *cptr, typename MemberHandler<T>::MemberHandlerPtr hptr)
		{
			AddNodeBack(hclass, hid, NULL, new MemberHandler<T>(cptr, hptr));
		}

		void InstallBack(POVMSType hclass, POVMSType hid, FunctionHandlerOO::FunctionHandlerPtr hptr, void *pptr)
		{
			AddNodeBack(hclass, hid, new FunctionHandlerOO(hptr, pptr), NULL);
		}

		void InstallBack(POVMSType hclass, POVMSType hid, FunctionHandler::FunctionHandlerPtr hptr, void *pptr)
		{
			AddNodeBack(hclass, hid, NULL, new FunctionHandler(hptr, pptr));
		}

		void Remove(POVMSType hclass, POVMSType hid);
	private:
		struct HandlerNode
		{
			struct HandlerNode *last;
			struct HandlerNode *next;
			POVMSType hclass;
			POVMSType hid;
			HandlerOO *handleroo;
			Handler *handler;
		};

		POVMSContext context;
		HandlerNode *receivers;

		POVMS_MessageReceiver() { throw int(-1); } // default constructor not allowed
		POVMS_MessageReceiver(const POVMS_MessageReceiver&) { throw int(-1); } // no copies allowed
		POVMS_MessageReceiver& operator=(const POVMS_MessageReceiver&) { throw int(-1); } // no copy assignments allowed

		static int ReceiveHandler(POVMSObjectPtr msg, POVMSObjectPtr result, int mode, void *privatedataptr);

		void AddNodeFront(POVMSType hclass, POVMSType hid, HandlerOO *hooptr, Handler *hptr);
		void AddNodeBack(POVMSType hclass, POVMSType hid, HandlerOO *hooptr, Handler *hptr);
		void RemoveNode(HandlerNode *nodeptr);
};

void POVMS_SendMessage(POVMSContext contextref, POVMS_Message& msg, POVMS_Message *result, int mode);

#endif
