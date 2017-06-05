/**
 *
 * $Id: ExpressionList.h,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
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
#ifndef _EXPRESSION_LIST_H
#define _EXPRESSION_LIST_H
#include <stdlib.h>
#include "Expression.h"
#include "lookup.h"

typedef struct _ExpressionElement
{
    struct _ExpressionElement *Next;
    char *name;
    ExpressionType *expr;
}
ExpressionElement;

typedef struct _ParameterElement
{
    struct _ParameterElement *Next;
    ExpressionType *expr;
}
ParameterElement;

typedef struct _ExpressionList
{
    ExpressionElement *theList;
}
ExpressionList;

typedef struct _ParameterList
{
    ParameterElement *theList;
    int size;
}
ParameterList;

void ParameterListAppend(ParameterList *this, ParameterElement *parm);
ExpressionList *ExpressionListNew(ExpressionList *);
void ExpressionListAppend(ExpressionList *this, ExpressionElement *elem);
char *ExpressionElementGetName(ExpressionElement *);
ExpressionElement *ExpressionListFind(ExpressionList *, char *);
void ExpressionListEmit(ExpressionList *this);
ExpressionElement *ExpressionElementNew(void);
ExpressionElement *ExpressionElementNew1(char *name, ExpressionType *expr);
Bool ExpressionElementIsType(ExpressionElement *this, unsigned char type);
long ParameterElementGetValue(ExpressionElement *this);
void ExpressionElementEmit(ExpressionElement *this);
void ExpressionElementDistroy(ExpressionElement *this);
ParameterElement *ParameterElementNew(void);
ParameterElement *ParameterElementNew1(ExpressionType *expr);
ParameterList *ParameterListNew(void);
void ParameterListEmit(ParameterList *this);
long ExpressionElementGetValue(ExpressionElement *this);
Bool Equal_EE_EE(ExpressionElement *a, ExpressionElement *b);
ExpressionType *ExpressionListLookup(ExpressionType **this);

#endif
