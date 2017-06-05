/**
 *
 * $Id: FakeWidget.h,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
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

#ifndef _FAKEWIDGET_H
#define _FAKEWIDGET_H

#include "Attribute.h"
#include "Callback.h"
#include "misc.h"

typedef struct _FakeWidgetType
{
    char *theName;
    char *theClass;
    char managed;
    struct _FakeWidgetList *children;
    struct _FakeWidgetType *parent;
    AttributeList *Attributes;
    CallbackList *Callbacks;
    struct _FakeWidgetList *Inherits;
    int IDNumber;
}
FakeWidgetType;

typedef struct _FakeWidgetListElement
{
    struct _FakeWidgetListElement *Next;
    FakeWidgetType *theWidget;
}
FakeWidgetListElement;

typedef struct _FakeWidgetList
{
    FakeWidgetListElement *theList;
    int size;
    int WidgetCount;
}
FakeWidgetList;


FakeWidgetList *FakeWidgetListNew(FakeWidgetList *this);
FakeWidgetList *FakeWidgetTypeKids(FakeWidgetType *this);
CallbackList *FakeWidgetTypeCalls(FakeWidgetType *this);
FakeWidgetList *FakeWidgetTypeInheritance(FakeWidgetType *wid);
AttributeList *FakeWidgetTypeAtts(FakeWidgetType *this);

FakeWidgetType *FakeWidgetTypeNew(void);
FakeWidgetType *FakeWidgetTypeNew1(char *Name, char *class);
FakeWidgetType *FakeWidgetTypeNew2(char *Name, char *class, int managed);

FakeWidgetListElement *FakeWidgetListElementNew(FakeWidgetType *wid);

void FakeWidgetTypeEmitName(FakeWidgetType *);
void FakeWidgetTypeSetName(FakeWidgetType *this, char *name, char *newType);
void FakeWidgetTypeUnmanage(FakeWidgetType *this);
void FakeWidgetTypeEmit(FakeWidgetType *);
void FakeWidgetTypeEmitID(FakeWidgetType *);

void FakeWidgetListAppend(FakeWidgetList *this, FakeWidgetType *that);
void FakeWidgetListAppendLists(FakeWidgetList *this, FakeWidgetList *that);
void FakeWidgetListSetParents(FakeWidgetList *this, FakeWidgetType *Parent);
void FakeWidgetListIndex(FakeWidgetList *);
void FakeWidgetListEmit(FakeWidgetList *this);
void FakeWidgetListEmitList(FakeWidgetList *wl);
FakeWidgetListElement *FakeWidgetListErase(FakeWidgetList *, FakeWidgetListElement *);

Bool Equal_FWT_FWT(FakeWidgetType *a, FakeWidgetType *b);

void FakeWidgetListUpdate(FakeWidgetList *, FakeWidgetType **);
void FakeWidgetTypeUpdate(FakeWidgetType *, FakeWidgetType *);
void FakeWidgetListPrint(FakeWidgetList *);

#endif /* #define _FAKEWIDGET_H */
