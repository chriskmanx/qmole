/*
 * $Id: Expression.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
 *
 * Copyright (C) 1995-2000 LessTif Development Team
 */

#include <LTconfig.h>

#include <stdio.h>
#include <Mrm/MrmPublic.h>

#include "main.h"
#include "Expression.h"
#include "ExpressionList.h"



void
ExpressionTypeEmit(ExpressionType *etype)
{
    etype->Emit(etype);
}


/* 
 *  Swap if needed to make the type of 'etype1' "hold" more than the type of 
 *  'etype2'.  i.e.  if 'etype1' is integer and 'etype2' is float  -- then swap
 *  This is similar to the concept of automatic casting in C expressions 
 *  such as (int) + (float) will evaluate to (float).
 */
void
ExpressionHierarchy(ExpressionType **etype1, ExpressionType **etype2)
{
    int Order[] =
	{
	    0, 11, 12, 0, 0, 0, 0, 10, 0, 0, 0,
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	};

    if (((*etype1)->type >= MrmRtypeMax) || ((*etype2)->type >= MrmRtypeMax) ||
	((*etype1)->type <= 0) || ((*etype2)->type <= 0))
    {
	__MrmExit(LOC, "Can't Evaluate\n");
    }

    if (Order[(*etype1)->type] > Order[(*etype2)->type])
    {
	ExpressionType *temp;

	temp = *etype1;
	*etype1 = *etype2;
	*etype2 = temp;

	return;
    }
}

void
ExpressionTypeAdd(ExpressionType *etype1, ExpressionType *etype2)
{
    if (MrmRtypeAddrName == etype1->type)
    {
	ExpressionListLookup(&etype1);
    }

    if (MrmRtypeAddrName == etype2->type)
    {
	ExpressionListLookup(&etype2);
    }

    ExpressionHierarchy(&etype1, &etype2);

    etype1->value += etype2->value;
}

void
ExpressionTypeSubtract(ExpressionType *etype1, ExpressionType *etype2)
{
    if (MrmRtypeAddrName == etype1->type)
    {
	ExpressionListLookup(&etype1);
    }

    if (MrmRtypeAddrName == etype2->type)
    {
	ExpressionListLookup(&etype2);
    }

    ExpressionHierarchy(&etype1, &etype2);

    etype1->value -= etype2->value;
}

void
ExpressionTypeMultiply(ExpressionType *etype1, ExpressionType *etype2)
{
    if (MrmRtypeAddrName == etype1->type)
    {
	ExpressionListLookup(&etype1);
    }

    if (MrmRtypeAddrName == etype2->type)
    {
	ExpressionListLookup(&etype2);
    }

    ExpressionHierarchy(&etype1, &etype2);
    etype1->value *= etype2->value;

}

void
ExpressionTypeDivide(ExpressionType *etype1, ExpressionType *etype2)
{
    if (MrmRtypeAddrName == etype1->type)
    {
	ExpressionListLookup(&etype1);
    }

    if (MrmRtypeAddrName == etype2->type)
    {
	ExpressionListLookup(&etype2);
    }

    ExpressionHierarchy(&etype1, &etype2);

    etype1->value /= etype2->value;
}

void
ExpressionTypeDelete(ExpressionType *etype1)
{
    /* free(etype1);  */
}
