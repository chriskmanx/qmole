/**
 *
 * $Id: Callback.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>

#include "lookup.h"
#include "Callback.h"
#include "misc.h"
#include "main.h"


CallbackList *
CallbackListNew(void)
{
    CallbackList *this = (CallbackList *)malloc(sizeof(CallbackList));

    this->theList = NULL;

    return this;
}

void 
CallbackListAppend(CallbackList *this, CallbackType *cb)
{
    CallbackListElement **i;

    for (i = &(this->theList); (*i) != NULL; i = &((*i)->Next));

    *i = CallbackListElementNew(cb);
}

void 
CallbackListAppendLists(CallbackList *this, CallbackList *a)
{
    CallbackListElement **i;

    for (i = &(this->theList); (*i) != NULL; i = &((*i)->Next));

    *i = a->theList;
}

CallbackListElement *
CallbackListElementNew(CallbackType *cb)
{
    CallbackListElement *this = (CallbackListElement *)
    malloc(sizeof(CallbackListElement));

    this->theCallback = cb;
    this->Next = NULL;

    return this;
}

Bool 
CallbackListElementIsInherited(CallbackListElement *this)
{
    return (strcmp(this->theCallback->CallbackID, "Inherited") == 0);
}

char *
CallbackListElementGetFunctionName(CallbackListElement *this)
{
    return this->theCallback->FunctionName;
}

CallbackType *
CallbackTypeNew(char *callbackID, char *functionName,
		ParameterList *parameters)
{
    CallbackType *this = (CallbackType *)malloc(sizeof(CallbackType));

    this->CallbackID = callbackID;
    this->FunctionName = functionName;
    this->Parameters = parameters;

    return this;
}

#if 0
static Bool 
CallbackTypeIsInherited(CallbackType *this)
{
    return (0 == strcmp(this->CallbackID, "Inherited"));
}

static char *
CallbackTypeGetFunctionName(CallbackType *this)
{
    return this->FunctionName;
}
#endif

Bool 
Equal_CE_CE(CallbackListElement *a, CallbackListElement *b)
{
    return (strcmp(a->theCallback->FunctionName,
		   b->theCallback->FunctionName) == 0);
}

void 
CallbackListElementDistroy(CallbackListElement *this)
{
    free(this);
}

CallbackListElement *
CallbackListErase(CallbackList *this,
		  CallbackListElement *cb)
{
    CallbackListElement **j, **prev, *Return, *Destroy;
    Bool equal = False;

    for (prev = j = &(this->theList);
	 (*j != NULL) && !(equal = Equal_CE_CE(*j, cb));
	 j = &((*j)->Next))
    {
	prev = j;
    }

    if (equal)
    {
	Destroy = *j;

	*prev = (*j)->Next;

	Return = (*prev);

	CallbackListElementDistroy(Destroy);

	return Return;
    }

    __MrmExit(LOC, "Can't find Callback to destroy\n");

    return NULL;
}

void 
CallbackListEmit(CallbackList *this, int IDNumber)
{
    char *names;
    CallbackListElement *i;

    if (NULL == this)
    {
	return;
    }

    for (i = this->theList; i != NULL; i = i->Next)
    {
	int size = i->theCallback->Parameters->size;

	fputc((unsigned char)MrmRtypeCallback, outFile);

	fwrite(&IDNumber, 1, sizeof(IDNumber), outFile);

	names = __MrmArgNamesString(i->theCallback->CallbackID);

	fwrite(names, 1, strlen(names), outFile);

	fputc('"', outFile);

	fwrite(i->theCallback->FunctionName, 1,
	       strlen(i->theCallback->FunctionName), outFile);

	fputc('"', outFile);

	fputc(size, outFile);

	if (size)
	{
	    ParameterListEmit(i->theCallback->Parameters);
	}
    }
}
