/**
 *
 * $Id: Expression.h,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
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

#ifndef _EXPRESSION_H
#define _EXPRESSION_H

#include "uil.h"
#include "misc.h"

typedef void (*PFI) (void *);
typedef void *(*GetEvalFunctionType)(void);

typedef struct _ExpressionType
{
    long value;
    unsigned char type;
    PFI Emit;
    GetEvalFunctionType GetEvalValue;
}
ExpressionType;

void ExpressionTypeAdd(ExpressionType *this, ExpressionType *a);
void ExpressionTypeSubtract(ExpressionType *this, ExpressionType *a);
void ExpressionTypeMultiply(ExpressionType *this, ExpressionType *a);
void ExpressionTypeDivide(ExpressionType *this, ExpressionType *a);
void ExpressionTypeDelete(ExpressionType *this);
void ExpressionTypeEmit(ExpressionType *this);
void ExpressionHierarchy(ExpressionType **this, ExpressionType **that);
#endif
