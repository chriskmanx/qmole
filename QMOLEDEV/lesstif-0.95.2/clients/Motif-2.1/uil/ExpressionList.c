/**
 *
 * $Id: ExpressionList.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
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

#include <string.h>

#include "ExpressionList.h"

#include "main.h"


#if 0
ExpressionElement *
ExpressionElementNew()
{
    ExpressionElement *this =
	(ExpressionElement *)malloc(sizeof(ExpressionElement));

    this->name = NULL;
    this->expr = NULL;
    this->Next = NULL;

    return this;
};
#endif

Bool 
Equal_EE_EE(ExpressionElement *a, ExpressionElement *b)
{
    return (strcmp(a->name, b->name) == 0);
}

ExpressionElement *
ExpressionElementNew1(char *Name, ExpressionType *Expr)
{
    ExpressionElement *this =
	(ExpressionElement *)malloc(sizeof(ExpressionElement));

    this->Next = NULL;
    this->name = Name;
    this->expr = Expr;

    return this;
}

void 
ExpressionElementDistroy(ExpressionElement *this)
{
    free(this);
}

void 
ExpressionElementEmit(ExpressionElement *this)
{
    ExpressionTypeEmit(this->expr);
}

long 
ExpressionElementGetValue(ExpressionElement *this)
{
    return (this->expr->value);
}

Bool 
ExpressionElementIsType(ExpressionElement *this, unsigned char type)
{
    return (this->expr->type == type);
}

char *
ExpressionElementGetName(ExpressionElement *this)
{
    return this->name;
}

ParameterElement *
ParameterElementNew()
{
    ParameterElement *this =
	(ParameterElement *)malloc(sizeof(ParameterElement));

    this->expr = NULL;
    this->Next = NULL;

    return this;
}

ParameterElement *
ParameterElementNew1(ExpressionType *Expr)
{
    ParameterElement *this =
	(ParameterElement *)malloc(sizeof(ParameterElement));

    this->expr = Expr;
    this->Next = NULL;

    return this;
}

void 
ParameterListAppend(ParameterList *this, ParameterElement *param)
{
    ParameterElement **j;

    for (j = &(this->theList); (*j) != NULL; j = &((*j)->Next));

    *j = param;

    this->size++;
}

#if 0
static void 
ParameterElementEmit(ExpressionElement *this)
{
    ExpressionTypeEmit(this->expr);
}
#endif

long 
ParameterElementGetValue(ExpressionElement *this)
{
    return (this->expr->value);
}

#if 0
static Bool 
ParameterElementIsType(ExpressionElement *this, unsigned char type)
{
    return (this->expr->type == type);
}
#endif

ParameterList *
ParameterListNew()
{
    ParameterList *this = (ParameterList *)malloc(sizeof(ParameterList));

    this->size = 0;
    this->theList = NULL;

    return this;
}

void 
ParameterListEmit(ParameterList *this)
{
    ParameterElement *i;

    if (NULL == this)
    {
	return;
    }

    for (i = this->theList; i != NULL; i = i->Next)
    {
	ExpressionTypeEmit(i->expr);
    }

    return;
}

ExpressionList *
ExpressionListNew(ExpressionList *this)
{
    if (NULL == this)
    {
	this = (ExpressionList *)malloc(sizeof(ExpressionList));
    }

    this->theList = NULL;

    return this;
}

void 
ExpressionListAppend(ExpressionList *this, ExpressionElement *elem)
{
    ExpressionElement **j;

    for (j = &(this->theList); (*j) != NULL; j = &((*j)->Next));

    *j = elem;
}

void 
ExpressionListEmit(ExpressionList *this)
{
    ExpressionElement *i;
    char *name;

    if (NULL == this)
    {
	return;
    }

    for (i = this->theList; i != NULL; i = i->Next)
    {
	name = ExpressionElementGetName(i);

	fwrite(name, 1, strlen(name), outFile);

	fputc('"', outFile);

	if (NULL == i->expr)
	{
	    __MrmExit(LOC,
	      "The code to create %s has not been implemented correctly yet\n",
		 i->name);
	}

	ExpressionElementEmit(i);
    }

    return;
}

ExpressionElement *
ExpressionListFind(ExpressionList *this, char *name)
{
    ExpressionElement *i;

    if (NULL == this)
    {
	return NULL;
    }

    for (i = this->theList; i != NULL; i = i->Next)
    {
	if (strcmp(name, i->name) == 0)
	{
	    return i;
	}
    }

    return NULL;
}

ExpressionType *
ExpressionListLookup(ExpressionType **this)
{
    ExpressionElement *t = ExpressionListFind(&LocalSymbolTable,
					      (char *)(*this)->value);
    if (NULL == t)
    {
	t = ExpressionListFind(&GlobalSymbolTable, (char *)(*this)->value);
    }

    if (NULL == t)
    {
	__MrmExit(LOC, "Can't find in symbol tables\n");
    }

    (*this) = t->expr;

    return t->expr;
}
