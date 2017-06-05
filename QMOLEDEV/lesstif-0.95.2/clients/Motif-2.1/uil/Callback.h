/**
 *
 * $Id: Callback.h,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
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

#ifndef _CALLBACK_H
#define _CALLBACK_H

#include <Mrm/MrmPublic.h>
#include "ExpressionList.h"

typedef struct _CallbackType
{
    char *CallbackID;
    char *FunctionName;
    ParameterList *Parameters;
}
CallbackType;

typedef struct _CallbackListElement
{
    struct _CallbackListElement *Next;
    CallbackType *theCallback;
}
CallbackListElement;

typedef struct _CallbackList
{
    CallbackListElement *theList;
}
CallbackList;

CallbackList *CallbackListNew(void);
CallbackListElement *CallbackListElementNew(CallbackType *cb);
void CallbackListEmit(CallbackList *this, int IDNumber);

char *CallbackListElementGetFunctionName(CallbackListElement *);
Bool CallbackListElementIsInherited(CallbackListElement *this);

void CallbackListAppend(CallbackList *, CallbackType *);
void CallbackListAppendLists(CallbackList *this, CallbackList *a);
CallbackListElement *CallbackListErase(CallbackList *, CallbackListElement *);
CallbackType *CallbackTypeNew(char *callbackID, char *functionName,
			      ParameterList *parameters);
Bool Equal_CE_CE(CallbackListElement *a, CallbackListElement *b);
void CallbackListElementDistroy(CallbackListElement *this);

#endif /* #ifndef _CALLBACK_H */
