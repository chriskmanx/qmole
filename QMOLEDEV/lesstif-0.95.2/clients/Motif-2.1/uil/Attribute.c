/**
 *
 * $Id: Attribute.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

#include <Mrm/MrmPublic.h>

#include "Attribute.h"
#include "lookup.h"
#include "main.h"


void
AttributeListEmit(AttributeList *this)
{
    ExpressionElement **j;
    int ArgIndex;

    if (NULL == this)
    {
	return;
    }

    for (j = &this->theExpressionList->theList; *j != NULL; j = &((*j)->Next))
    {
	fputc((unsigned char)MrmRtypeResource, outFile);

	ArgIndex = __MrmLookUpArgIndex(ExpressionElementGetName(*j));

	fwrite(&ArgIndex, sizeof(ArgIndex), 1, outFile);

	ExpressionElementEmit(*j);
    }
}

AttributeList *
AttributeListNew(void)
{
    AttributeList *this = (AttributeList *)malloc(sizeof(AttributeList));

    this->size = 0;

    this->theExpressionList = ExpressionListNew(NULL);

    return this;
}

ExpressionElement *
AttributeListErase(AttributeList *this,
		   ExpressionElement *expr)
{
    ExpressionElement **j, **prev, *Return, *Destroy;
    Bool equal = False;

    for (prev = j = &this->theExpressionList->theList;
	 (*j != NULL) && !(equal = Equal_EE_EE(*j, expr));
	 j = &((*j)->Next))
    {
	prev = j;
    }

    if (equal)
    {
	Destroy = *j;

	*prev = (*j)->Next;

	Return = (*prev);

	ExpressionElementDistroy(Destroy);

	this->size--;

	return Return;
    }

    __MrmExit(LOC, "Couldn't find node to Erase\n");

    return NULL;
}

void
AttributeListAppend(AttributeList *this, ExpressionElement *expr)
{
    ExpressionElement **j;

    if (this == NULL || expr == NULL) return;

    for (j = &this->theExpressionList->theList; *j != NULL; j = &((*j)->Next));

    (*j) = expr;

    this->size++;
}

void
AttributeListAppendLists(AttributeList *this, AttributeList *list)
{
    ExpressionElement **j;

    if (this == NULL || list == NULL) return;

    for (j = &this->theExpressionList->theList; *j != NULL; j = &((*j)->Next));

    *j = list->theExpressionList->theList;

    this->size += list->size;
}
