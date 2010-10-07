/****************************************************************************
 *               povmscpp.cpp
 *
 * This module contains the C++ interface version of povms.cpp
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
 * $File: //depot/povray/3.6-release/source/base/povmscpp.cpp $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/

#include "configbase.h"

#include "povmscpp.h"


USING_POV_BASE_NAMESPACE

/*****************************************************************************
*
* CLASS
*   POVMS_Container
*   
* DESCRIPTION
*   Base class for all other POVMS classes.
*
* CHANGES
*   -
*
******************************************************************************/

POVMS_Container::POVMS_Container()
{
	data.type = kPOVMSType_Null;
	data.size = 0;
	data.ptr = NULL;
}

POVMS_Container::~POVMS_Container()
{
	// do nothing for now
}

POVMSType POVMS_Container::Type()
{
	return data.type;
}

long POVMS_Container::Size()
{
	return data.size;
}

bool POVMS_Container::IsNull()
{
	return (data.type == kPOVMSType_Null);
}

void POVMS_Container::DetachData()
{
	data.type = kPOVMSType_Null;
	data.size = 0;
	data.ptr = NULL;
}


/*****************************************************************************
*
* CLASS
*   POVMS_Attribute
*   
* DESCRIPTION
*   Class handling POVMSAttribute.
*
* CHANGES
*   -
*
******************************************************************************/

POVMS_Attribute::POVMS_Attribute()
{
	int err;

	err = POVMSAttr_New(&data);
	if(err != kNoErr)
		throw err;
}

POVMS_Attribute::POVMS_Attribute(const char *str)
{
	int err;

	err = POVMSAttr_New(&data);
	if(err != kNoErr)
		throw err;
	err = POVMSAttr_Set(&data, kPOVMSType_CString, (void *)str, strlen(str) + 1);
	if(err != kNoErr)
	{
		(void)POVMSAttr_Delete(&data);
		throw err;
	}
}

POVMS_Attribute::POVMS_Attribute(POVMSInt value)
{
	int err;

	err = POVMSAttr_New(&data);
	if(err != kNoErr)
		throw err;
	err = POVMSAttr_Set(&data, kPOVMSType_Int, (void *)(&value), sizeof(POVMSLong));
	if(err != kNoErr)
	{
		(void)POVMSAttr_Delete(&data);
		throw err;
	}
}

POVMS_Attribute::POVMS_Attribute(POVMSLong value)
{
	int err;

	err = POVMSAttr_New(&data);
	if(err != kNoErr)
		throw err;
	err = POVMSAttr_Set(&data, kPOVMSType_Long, (void *)(&value), sizeof(POVMSLong));
	if(err != kNoErr)
	{
		(void)POVMSAttr_Delete(&data);
		throw err;
	}
}

POVMS_Attribute::POVMS_Attribute(POVMSFloat value)
{
	int err;

	err = POVMSAttr_New(&data);
	if(err != kNoErr)
		throw err;
	err = POVMSAttr_Set(&data, kPOVMSType_Float, (void *)(&value), sizeof(POVMSFloat));
	if(err != kNoErr)
	{
		(void)POVMSAttr_Delete(&data);
		throw err;
	}
}

POVMS_Attribute::POVMS_Attribute(bool b)
{
	POVMSBool value = (POVMSBool)b;
	int err;

	err = POVMSAttr_New(&data);
	if(err != kNoErr)
		throw err;
	err = POVMSAttr_Set(&data, kPOVMSType_Bool, (void *)(&value), sizeof(POVMSBool));
	if(err != kNoErr)
	{
		(void)POVMSAttr_Delete(&data);
		throw err;
	}
}

POVMS_Attribute::POVMS_Attribute(POVMSType value)
{
	int err;

	err = POVMSAttr_New(&data);
	if(err != kNoErr)
		throw err;
	err = POVMSAttr_Set(&data, kPOVMSType_Type, (void *)(&value), sizeof(POVMSType));
	if(err != kNoErr)
	{
		(void)POVMSAttr_Delete(&data);
		throw err;
	}
}

POVMS_Attribute::POVMS_Attribute(POVMSAttribute& convert)
{
	data = convert;
}

POVMS_Attribute::POVMS_Attribute(POVMS_Attribute& source)
{
	int err;

	err = POVMSAttr_Copy(&source.data, &data);
	if(err != kNoErr)
		throw err;
}

POVMS_Attribute::~POVMS_Attribute()
{
	int err;

	err = POVMSAttr_Delete(&data);
	if(err != kNoErr)
		throw err;
}

POVMS_Attribute& POVMS_Attribute::operator=(POVMS_Attribute& source)
{
	int err;

	err = POVMSAttr_Delete(&data);
	if(err == kNoErr)
		err = POVMSAttr_Copy(&source.data, &data);
	if(err != kNoErr)
		throw err;

	return *this;
}

void POVMS_Attribute::Get(POVMSType type, void *data, int *maxdatasize)
{
	int err;

	err = POVMSAttr_Get(&this->data, type, data, maxdatasize);
	if(err != kNoErr)
		throw err;
}

void POVMS_Attribute::Set(POVMSType type, void *data, int datasize)
{
	int err;

	err = POVMSAttr_Set(&this->data, type, data, datasize);
	if(err != kNoErr)
		throw err;
}


/*****************************************************************************
*
* CLASS
*   POVMS_List
*   
* DESCRIPTION
*   Class handling POVMSAttributeList.
*
* CHANGES
*   -
*
******************************************************************************/

POVMS_List::POVMS_List()
{
	int err;

	err = POVMSAttrList_New(&data);
	if(err != kNoErr)
		throw err;
}

POVMS_List::POVMS_List(POVMSAttributeList& convert)
{
	data = convert;
}

POVMS_List::POVMS_List(POVMS_List& source)
{
	int err;

	err = POVMSAttrList_Copy(&source.data, &data);
	if(err != kNoErr)
		throw err;
}

POVMS_List::~POVMS_List()
{
	int err;

	err = POVMSAttrList_Delete(&data);
	if(err != kNoErr)
		throw err;
}

POVMS_List& POVMS_List::operator=(POVMS_List& source)
{
	int err;

	err = POVMSAttrList_Delete(&data);
	if(err == kNoErr)
		err = POVMSAttrList_Copy(&source.data, &data);
	if(err != kNoErr)
		throw err;

	return *this;
}

void POVMS_List::Append(POVMS_Attribute& item)
{
	int err;

	err = POVMSAttrList_Append(&data, &item.data);
	if(err != kNoErr)
		throw err;

	item.DetachData();
}

void POVMS_List::Append(POVMS_List& item)
{
	int err;

	err = POVMSAttrList_Append(&data, &item.data);
	if(err != kNoErr)
		throw err;

	item.DetachData();
}

void POVMS_List::Append(POVMS_Object& item)
{
	int err;

	err = POVMSAttrList_Append(&data, &item.data);
	if(err != kNoErr)
		throw err;

	item.DetachData();
}

void POVMS_List::GetNth(int index, POVMS_Attribute& item)
{
	int err;

	err = POVMSAttr_Delete(&item.data);
	if(err != kNoErr)
		throw err;
	err = POVMSAttrList_GetNth(&data, index, &item.data);
	if(err != kNoErr)
		throw err;
}

void POVMS_List::GetNth(int index, POVMS_List& item)
{
	int err;

	err = POVMSAttrList_Delete(&item.data);
	if(err != kNoErr)
		throw err;
	err = POVMSAttrList_GetNth(&data, index, &item.data);
	if(err != kNoErr)
		throw err;
}

void POVMS_List::GetNth(int index, POVMS_Object& item)
{
	int err;

	err = POVMSObject_Delete(&item.data);
	if(err != kNoErr)
		throw err;
	err = POVMSAttrList_GetNth(&data, index, &item.data);
	if(err != kNoErr)
		throw err;
}

void POVMS_List::SetNth(int index, POVMS_Attribute& item)
{
	int err;

	err = POVMSAttrList_SetNth(&data, index, &item.data);
	if(err != kNoErr)
		throw err;

	item.DetachData();
}

void POVMS_List::SetNth(int index, POVMS_List& item)
{
	int err;

	err = POVMSAttrList_SetNth(&data, index, &item.data);
	if(err != kNoErr)
		throw err;

	item.DetachData();
}

void POVMS_List::SetNth(int index, POVMS_Object& item)
{
	int err;

	err = POVMSAttrList_SetNth(&data, index, &item.data);
	if(err != kNoErr)
		throw err;

	item.DetachData();
}

void POVMS_List::RemoveNth(int index)
{
	int err;

	err = POVMSAttrList_RemoveNth(&data, index);
	if(err != kNoErr)
		throw err;
}

void POVMS_List::Clear()
{
	int err;

	err = POVMSAttrList_Clear(&data);
	if(err != kNoErr)
		throw err;
}


/*****************************************************************************
*
* CLASS
*   POVMS_Object
*   
* DESCRIPTION
*   Class handling POVMSObject.
*
* CHANGES
*   -
*
******************************************************************************/

POVMS_Object::POVMS_Object(POVMSType objclass)
{
	int err;

	err = POVMSObject_New(&data, objclass);
	if(err != kNoErr)
		throw err;
}

POVMS_Object::POVMS_Object(POVMSObject& convert)
{
	data = convert;
}

POVMS_Object::POVMS_Object(POVMSObjectPtr convert)
{
	if(convert != NULL)
		data = *convert;
}

POVMS_Object::POVMS_Object(POVMS_Object& source)
{
	int err;

	err = POVMSObject_Copy(&source.data, &data);
	if(err != kNoErr)
		throw err;
}

POVMS_Object::~POVMS_Object()
{
	int err;

	err = POVMSObject_Delete(&data);
	if(err != kNoErr)
		throw err;
}

POVMS_Object& POVMS_Object::operator=(POVMS_Object& source)
{
	int err;

	err = POVMSObject_Delete(&data);
	if(err == kNoErr)
		err = POVMSObject_Copy(&source.data, &data);
	if(err != kNoErr)
		throw err;

	return *this;
}

void POVMS_Object::Get(POVMSType key, POVMS_Attribute& attr)
{
	int err;

	err = POVMSAttr_Delete(&attr.data);
	if(err != kNoErr)
		throw err;
	err = POVMSObject_Get(&data, &attr.data, key);
	if(err != kNoErr)
		throw err;
}

void POVMS_Object::Get(POVMSType key, POVMS_List& attr)
{
	int err;

	err = POVMSAttrList_Delete(&attr.data);
	if(err != kNoErr)
		throw err;
	err = POVMSObject_Get(&data, &attr.data, key);
	if(err != kNoErr)
		throw err;
}

void POVMS_Object::Get(POVMSType key, POVMS_Object& attr)
{
	int err;

	err = POVMSObject_Delete(&attr.data);
	if(err != kNoErr)
		throw err;
	err = POVMSObject_Get(&data, &attr.data, key);
	if(err != kNoErr)
		throw err;
}

void POVMS_Object::Set(POVMSType key, POVMS_Attribute& attr)
{
	int err;

	err = POVMSObject_Set(&data, &attr.data, key);
	if(err != kNoErr)
		throw err;

	attr.DetachData();
}

void POVMS_Object::Set(POVMSType key, POVMS_List& attr)
{
	int err;

	err = POVMSObject_Set(&data, &attr.data, key);
	if(err != kNoErr)
		throw err;

	attr.DetachData();
}

void POVMS_Object::Set(POVMSType key, POVMS_Object& attr)
{
	int err;

	err = POVMSObject_Set(&data, &attr.data, key);
	if(err != kNoErr)
		throw err;

	attr.DetachData();
}

void POVMS_Object::Remove(POVMSType key)
{
	int err;

	err = POVMSObject_Remove(&data, key);
	if(err != kNoErr)
		throw err;
}

bool POVMS_Object::Exist(POVMSType key)
{
	return (POVMSObject_Exist(&data, key) == kNoErr);
}

void POVMS_Object::Merge(POVMS_Object& source)
{
	int err;

	err = POVMSObject_Merge(&source.data, &data);
	if(err != kNoErr)
		throw err;
}

const POVMSObject& POVMS_Object::operator*() const
{
	return *((const POVMSObjectPtr)(&data));
}

const POVMSObjectPtr POVMS_Object::operator->() const
{
	return (const POVMSObjectPtr)(&data);
}

POVMSObject POVMS_Object::operator()()
{
	POVMS_Object tc(*this);
	POVMSObject t = *((POVMSObjectPtr)(&tc.data));
	tc.DetachData();
	return t;
}

void POVMS_Object::SetString(POVMSType key, const char *str)
{
	int err;

	err = POVMSUtil_SetString(&data, key, str);
	if(err != kNoErr)
		throw err;
}

void POVMS_Object::SetInt(POVMSType key, POVMSInt value)
{
	int err;

	err = POVMSUtil_SetInt(&data, key, value);
	if(err != kNoErr)
		throw err;
}

void POVMS_Object::SetLong(POVMSType key, POVMSLong value)
{
	int err;

	err = POVMSUtil_SetLong(&data, key, value);
	if(err != kNoErr)
		throw err;
}

void POVMS_Object::SetFloat(POVMSType key, POVMSFloat value)
{
	int err;

	err = POVMSUtil_SetFloat(&data, key, value);
	if(err != kNoErr)
		throw err;
}

void POVMS_Object::SetBool(POVMSType key, POVMSBool value)
{
	int err;

	err = POVMSUtil_SetBool(&data, key, value);
	if(err != kNoErr)
		throw err;
}

void POVMS_Object::SetType(POVMSType key, POVMSType value)
{
	int err;

	err = POVMSUtil_SetType(&data, key, value);
	if(err != kNoErr)
		throw err;
}

int POVMS_Object::GetStringLength(POVMSType key)
{
	int len = 0;
	int err;

	err = POVMSUtil_GetStringLength(&data, key, &len);
	if(err != kNoErr)
		throw err;

	return len;
}

int POVMS_Object::GetString(POVMSType key, char *str, int maxlen)
{
	int err;

	err = POVMSUtil_GetString(&data, key, str, &maxlen);
	if(err != kNoErr)
		throw err;

	return maxlen;
}

POVMSInt POVMS_Object::GetInt(POVMSType key)
{
	POVMSInt value;
	int err;

	err = POVMSUtil_GetInt(&data, key, &value);
	if(err != kNoErr)
		throw err;

	return value;
}

POVMSLong POVMS_Object::GetLong(POVMSType key)
{
	POVMSLong value;
	int err;

	err = POVMSUtil_GetLong(&data, key, &value);
	if(err != kNoErr)
		throw err;

	return value;
}

POVMSFloat POVMS_Object::GetFloat(POVMSType key)
{
	POVMSFloat value;
	int err;

	err = POVMSUtil_GetFloat(&data, key, &value);
	if(err != kNoErr)
		throw err;

	return value;
}

POVMSBool POVMS_Object::GetBool(POVMSType key)
{
	POVMSBool value;
	int err;

	err = POVMSUtil_GetBool(&data, key, &value);
	if(err != kNoErr)
		throw err;

	return value;
}

POVMSType POVMS_Object::GetType(POVMSType key)
{
	POVMSType value;
	int err;

	err = POVMSUtil_GetType(&data, key, &value);
	if(err != kNoErr)
		throw err;

	return value;
}

bool POVMS_Object::ReadHeader(POVMSStream *stream, int& objectsize)
{
	char header[8];
	POVMSInt datasize = 0;
	POVMSInt version = 0;
	POVMSInt objsize = 0;
	POVMSInt maxsize = 16;

	objectsize = 0;

	if(stream == NULL)
		return false;

	datasize += POVMSStream_ReadString(header, stream, 8, &maxsize);               // header       8 byte
	if(!((header[0] == 'P') && (header[1] == 'O') && (header[2] == 'V') && (header[3] == 'R') &&
	     (header[4] == 'A') && (header[5] == 'Y') && (header[6] == 'M') && (header[7] == 'S')))
		return false;

	datasize += POVMSStream_ReadInt(&version, stream + datasize, &maxsize);        // version      4 byte
	if(version != 0x0351)
		return false;

	datasize += POVMSStream_ReadInt(&objsize, stream + datasize, &maxsize);        // object size   4 byte
	if(objsize == 0)
		return false;

	if((datasize != 16) || (maxsize != 0))
		return false;

	objectsize = objsize;

	return true;
}

bool POVMS_Object::ReadObject(POVMSStream *stream, int objectsize)
{
	int err;

	if(stream == NULL)
		return false;
	if(objectsize == 0)
		return false;

	err = POVMSObject_Delete(&data);
	if(err != kNoErr)
		return false;

	err = POVMSStream_Read(&data, stream, &objectsize);
	if(err != kNoErr)
		return false;
	if(objectsize != 0)
		return false;

	return true;
}

bool POVMS_Object::WriteHeader(POVMSStream *stream, int& objectsize)
{
	POVMSInt datasize = 0;
	POVMSInt maxsize = 16;

	objectsize = 0;

	if(stream == NULL)
		return false;

	objectsize = POVMSStream_Size(&data);
	if(objectsize == 0)
		return false;

	datasize += POVMSStream_WriteString("POVRAYMS", stream, &maxsize);             // header       8 byte
	datasize += POVMSStream_WriteInt(0x0351, stream + datasize, &maxsize);         // version      4 byte
	datasize += POVMSStream_WriteInt(objectsize, stream + datasize, &maxsize);     // object size  4 byte

	if((datasize != 16) || (maxsize != 0))
		return false;

	return true;
}

bool POVMS_Object::WriteObject(POVMSStream *stream, int objectsize)
{
	POVMSInt datasize = 0;

	if(stream == NULL)
		return false;
	if(objectsize == 0)
		return false;

	datasize += POVMSStream_Write(&data, stream, &objectsize);                       // object       x byte

	if(objectsize != 0)
		return false;

	return true;
}


/*****************************************************************************
*
* CLASS
*   POVMS_Message
*   
* DESCRIPTION
*   Class handling messages contained in POVMSObjects.
*
* CHANGES
*   -
*
******************************************************************************/

POVMS_Message::POVMS_Message(POVMSType objclass, POVMSType msgclass, POVMSType msgid) : POVMS_Object(objclass)
{
	int err;

	err = POVMSMsg_SetupMessage(&data, msgclass, msgid);
	if(err != kNoErr)
		throw err;
}

POVMS_Message::POVMS_Message(POVMSObject& convert) : POVMS_Object(convert)
{
}

POVMS_Message::POVMS_Message(POVMSObjectPtr convert) : POVMS_Object(convert)
{
}

POVMS_Message::POVMS_Message(POVMS_Object& source) : POVMS_Object(source.data)
{
}

POVMS_Message::POVMS_Message(POVMS_Message& source) : POVMS_Object(source.data)
{
}

POVMS_Message& POVMS_Message::operator=(POVMS_Message& source)
{
	int err;

	err = POVMSObject_Delete(&data);
	if(err == kNoErr)
		err = POVMSObject_Copy(&source.data, &data);
	if(err != kNoErr)
		throw err;

	return *this;
}

POVMSType POVMS_Message::GetClass()
{
	POVMSType type;
	int err;

	err = POVMSMsg_GetMessageClass(&data, &type);
	if(err != kNoErr)
		throw err;

	return type;
}

POVMSType POVMS_Message::GetIdentifier()
{
	POVMSType type;
	int err;

	err = POVMSMsg_GetMessageIdentifier(&data, &type);
	if(err != kNoErr)
		throw err;

	return type;
}

POVMSAddress POVMS_Message::GetSourceAddress()
{
	POVMSAddress addr;
	int err;

	err = POVMSMsg_GetSourceAddress(&data, &addr);
	if(err != kNoErr)
		throw err;

	return addr;
}

POVMSAddress POVMS_Message::GetDestinationAddress()
{
	POVMSAddress addr;
	int err;

	err = POVMSMsg_GetDestinationAddress(&data, &addr);
	if(err != kNoErr)
		throw err;

	return addr;
}

void POVMS_Message::SetDestinationAddress(POVMSAddress addr)
{
	int err;

	err = POVMSMsg_SetDestinationAddress(&data, addr);
	if(err != kNoErr)
		throw err;
}


/*****************************************************************************
*
* CLASS
*   POVMS_MessageReceiver
*   
* DESCRIPTION
*   Class receiving messages.
*
* CHANGES
*   -
*
******************************************************************************/

POVMS_MessageReceiver::POVMS_MessageReceiver(POVMSContext contextref)
{
	context = contextref;
	receivers = NULL;
}

POVMS_MessageReceiver::~POVMS_MessageReceiver()
{
	while(receivers != NULL)
		RemoveNode(receivers);

	receivers = NULL;
	context = NULL;
}

void POVMS_MessageReceiver::Remove(POVMSType hclass, POVMSType hid)
{
	HandlerNode *nodeptr = NULL;

	for(nodeptr = receivers; nodeptr != NULL; nodeptr = nodeptr->next)
	{
		if((nodeptr->hclass == hclass) && ((nodeptr->hid == hid) || (hid == kPOVMSType_WildCard)))
		{
			RemoveNode(nodeptr);
			nodeptr = receivers;
		}
	}
}

int POVMS_MessageReceiver::ReceiveHandler(POVMSObjectPtr msg, POVMSObjectPtr result, int mode, void *privatedataptr)
{
	POVMS_MessageReceiver *self = (POVMS_MessageReceiver *)privatedataptr;
	HandlerNode *nodeptr = NULL;
	POVMSType hclass = kPOVMSType_Null;
	POVMSType hid = kPOVMSType_Null;
	int err = kNoErr;

	if(self == NULL)
		err = kParamErr;
	if(err == kNoErr)
		err = POVMSMsg_GetMessageClass(msg, &hclass);
	if(err == kNoErr)
		err = POVMSUtil_GetType(msg, kPOVMSMessageIdentID, &hid);
	if(err == kNoErr)
	{
		for(nodeptr = self->receivers; nodeptr != NULL; nodeptr = nodeptr->next)
		{
			if((nodeptr->hclass == hclass) && ((nodeptr->hid == hid) || (nodeptr->hid == kPOVMSType_WildCard)))
			{
				try
				{
					if(nodeptr->handleroo != NULL)
					{
						POVMS_Message msg_obj(msg);
						POVMS_Message result_obj(result);

						nodeptr->handleroo->Call(msg_obj, result_obj, mode);

						msg_obj.DetachData();
						if(result != NULL)
							*result = result_obj();
						result_obj.DetachData();
					}
					else if(nodeptr->handler != NULL)
						nodeptr->handler->Call(msg, result, mode);
					else
						err = kNullPointerErr;
				}
				catch(int e)
				{
					err = e;
				}
				catch(...)
				{
					err = kParamErr;
				}
			}
		}
	}

	return err;
}

void POVMS_MessageReceiver::AddNodeFront(POVMSType hclass, POVMSType hid, HandlerOO *hooptr, Handler *hptr)
{
	HandlerNode *nodeptr = new HandlerNode;
	int err = kNoErr;

	nodeptr->last = NULL;
	nodeptr->next = NULL;
	nodeptr->hclass = hclass;
	nodeptr->hid = hid;
	nodeptr->handleroo = hooptr;
	nodeptr->handler = hptr;

	err = POVMS_InstallReceiver(context, ReceiveHandler, hclass, hid, (void *)this);
	if(err != kNoErr)
		throw err;

	nodeptr->last = NULL;
	nodeptr->next = receivers;
	if(nodeptr->next != NULL)
		nodeptr->next->last = nodeptr;
	receivers = nodeptr;
}

void POVMS_MessageReceiver::AddNodeBack(POVMSType hclass, POVMSType hid, HandlerOO *hooptr, Handler *hptr)
{
	HandlerNode *nodeptr = new HandlerNode;
	HandlerNode *iptr = NULL;
	int err = kNoErr;

	nodeptr->last = NULL;
	nodeptr->next = NULL;
	nodeptr->hclass = hclass;
	nodeptr->hid = hid;
	nodeptr->handleroo = hooptr;
	nodeptr->handler = hptr;

	err = POVMS_InstallReceiver(context, ReceiveHandler, hclass, hid, (void *)this);
	if(err != kNoErr)
		throw err;

	if(receivers == NULL)
	{
		nodeptr->last = NULL;
		nodeptr->next = NULL;
		if(nodeptr->next != NULL)
			nodeptr->next->last = nodeptr;
		receivers = nodeptr;
	}
	else
	{
		iptr = receivers;
		while(iptr->next != NULL)
			iptr = iptr->next;
		nodeptr->last = iptr;
		nodeptr->next = NULL;
		iptr->next = nodeptr;
	}
}

void POVMS_MessageReceiver::RemoveNode(HandlerNode *nodeptr)
{
	if(nodeptr != NULL)
	{
		(void)POVMS_RemoveReceiver(context, nodeptr->hclass, nodeptr->hid);

		if(nodeptr->last != NULL)
			nodeptr->last->next = nodeptr->next;
		if(nodeptr->next != NULL)
			nodeptr->next->last = nodeptr->last;
		if(receivers == nodeptr)
			receivers = nodeptr->next;

		if(nodeptr->handleroo != NULL)
			delete nodeptr->handleroo;
		if(nodeptr->handler != NULL)
			delete nodeptr->handler;

		delete nodeptr;
	}
}


/*****************************************************************************
*
* FUNCTION
*   POVMS_Send
*   
* DESCRIPTION
*   POVMS_SendMessage same as POVMS_Send in povms.cpp, but takes
*   POVMS_Messages objects as arguments.
*
* CHANGES
*   -
*
******************************************************************************/

void POVMS_SendMessage(POVMSContext contextref, POVMS_Message& msg, POVMS_Message *result, int mode)
{
	int err;

	if(result != NULL)
		err = POVMS_Send(contextref, &msg.data, &result->data, mode);
	else
		err = POVMS_Send(contextref, &msg.data, NULL, mode);

	if(err != kNoErr)
		throw err;

	msg.DetachData();
}
