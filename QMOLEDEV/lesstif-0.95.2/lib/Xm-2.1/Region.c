/**
 *
 * $Id: Region.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
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
 **/
static const char rcsid[] = "$Id: Region.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>

#include <XmI/DebugUtil.h>

/* some defines to clean things up a bit. */

#define VALUE_IN_RANGE(x,a,b) (x >= a && x <= b)
#define VALUE_OUT_RANGE(x,a,b) (x < a || x > b)

#define ASSIGN_MAX(a,b,v) { if ((a) < (b)) (v) = (b); \
                            else if ((a) > (b)) (v) = (a); \
                            else (v) = (b); }
#define ASSIGN_MIN(a,b,v) { if ((a) > (b)) (v) = (b); \
                            else if ((a) < (b)) (v) = (a); \
                            else (v) = (b); }
#define ASSIGN_IF_GT(a,b) { if ((a) > (b)) a = b; }
#define ASSIGN_IF_LT(a,b) { if ((a) < (b)) a = b; }


#if 0
static int
qsort_compare_function(const void *a,
		       const void *b)
{
    XmRegionBox *box_a = (XmRegionBox *)a;
    XmRegionBox *box_b = (XmRegionBox *)b;

    return box_a->y1 - box_b->y1;
}

/**
 * __XmCompactRectangles
 *
 * compacts rectangles.  this is a fairly complicated algorithm...
 * not sure if there's an easier way to do it.  Also not sure
 * right now if it gives the same results as Motif's...
 *
 **/
static void
__XmCompactRectangles(XmRegionBox **rects,
		      long *size,
		      long *numRects)
{
    /* first we need to sort the rectangles by y1 */

    qsort(*rects, *numRects, sizeof(XmRegionBox),
	  qsort_compare_function);
}
#endif

/**
 * __XmAddRectangle
 *
 * adds a rectangle to a list of rectangles, reallocating if necessary.
 * useful for routines that must deal with a destination region, which
 * may in fact be one of the source regions.
 *
 **/
static void
__XmAddRectangle(XmRegionBox **rects,
		 long *size,
		 long *numRects,
		 short x1,
		 short y1,
		 short x2,
		 short y2)
{
    if (*numRects + 1 >= *size)
    {
	*size *= 2;
	*rects = (XmRegionBox *)XtRealloc((char *)(*rects),
					  sizeof(XmRegionBox) * *size);
    }

    (*rects)[*numRects].x1 = x1;
    (*rects)[*numRects].y1 = y1;
    (*rects)[*numRects].x2 = x2;
    (*rects)[*numRects].y2 = y2;

    *numRects += 1;
}

/**
 * _XmRegionCreate
 * 
 * Allocates a new XmRegion structure, with a size of one.
 *
 **/
XmRegion
_XmRegionCreate(void)
{
    XmRegion newRegion = (XmRegion)XtMalloc(sizeof(XmRegionRec));

    newRegion->size = 1;
    newRegion->numRects = 0;
    newRegion->rects = (XmRegionBox *)XtMalloc(sizeof(XmRegionBox)
					       * newRegion->size);

    _XmRegionComputeExtents(newRegion);

    return newRegion;
}

/**
 * _XmRegionCreateSize
 * 
 * Allocates a new XmRegion structure, with a size specified
 * by the parameter.
 *
 **/
XmRegion
_XmRegionCreateSize(long size)
{
    XmRegion newRegion = (XmRegion)XtMalloc(sizeof(XmRegionRec));

    newRegion->size = size;
    newRegion->numRects = 0;
    newRegion->rects = (XmRegionBox *)XtRealloc((char *)newRegion->rects,
					sizeof(XmRegionBox) * newRegion->size);

    _XmRegionComputeExtents(newRegion);

    return newRegion;
}

/**
 * _XmRegionComputeExtents
 *
 * Loops through the list of boxes within a region, keeping track
 * of the lowest and highest x,y.
 * These become the extents box.
 *
 **/
void
_XmRegionComputeExtents(XmRegion r)
{
    int i;

    if (r->numRects == 0)
    {
	r->extents.x1 =
	    r->extents.y1 =
	    r->extents.x2 =
	    r->extents.y2 = 0;
    }
    else
    {
	r->extents.x1 = r->extents.y1 = 32767;

	r->extents.x2 = r->extents.y2 = 0;

	for (i = 0; i < r->numRects; i++)
	{
	    ASSIGN_IF_GT(r->extents.x1, r->rects[i].x1)
	    ASSIGN_IF_GT(r->extents.y1, r->rects[i].y1)
	    ASSIGN_IF_LT(r->extents.x2, r->rects[i].x2)
	    ASSIGN_IF_LT(r->extents.y2, r->rects[i].y2)
	}
    }
}

/**
 * _XmRegionGetExtents
 *
 * Converts the extents box into an XRectangle
 * (using width/height instead of two points.)
 *
 **/
void
_XmRegionGetExtents(XmRegion r,
		    XRectangle *rect)
{
    rect->x = r->extents.x1;
    rect->y = r->extents.y1;

    rect->width = r->extents.x2 - r->extents.x1;
    rect->height = r->extents.y2 - r->extents.y1;
}

/**
 * _XmRegionUnionRectWithRegion
 *
 * Adds a rectangle to a region's list of rectangles.
 * This (and all functions which add rectangles to
 * a region) should compact the list, but doesn't
 * now.
 *
 **/
void
_XmRegionUnionRectWithRegion(XRectangle *rect,
			     XmRegion source,
			     XmRegion dest)
{
    int i;
    long size = 1;
    long numRects = 0;
    XmRegionBox *boxes = (XmRegionBox *)XtMalloc(sizeof(XmRegionBox)
						 * size);

    for (i = 0; i < source->numRects; i++)
    {
	__XmAddRectangle(&boxes, &size, &numRects,
			 source->rects[i].x1, source->rects[i].y1,
			 source->rects[i].x2, source->rects[i].y2);
    }

    __XmAddRectangle(&boxes, &size, &numRects,
		     rect->x, rect->y,
		     rect->x + rect->width, rect->y + rect->height);

    XtFree((char *)dest->rects);
    dest->rects = boxes;
    dest->size = size;
    dest->numRects = numRects;

    /* should compact the region here.. */

    _XmRegionComputeExtents(dest);
}

/**
 * _XmRegionIntersectRectWithRegion
 *
 * Computes the intersection of the rectangle with
 * all the boxes within the region.
 *
 **/
void
_XmRegionIntersectRectWithRegion(XRectangle *rect,
				 XmRegion source,
				 XmRegion dest)
{
    int i;
    long size = 1;
    long numRects = 0;
    XmRegionBox *boxes = (XmRegionBox *)XtMalloc(sizeof(XmRegionBox)
						 * size);

    for (i = 0; i < source->numRects; i++)
    {
	short new_x1, new_y1, new_x2, new_y2;

	/* special case for rectangles that don't intersect */

	if (rect->x + rect->width < source->rects[i].x1
	    || rect->x > source->rects[i].x2
	    || rect->y + rect->height < source->rects[i].y1
	    || rect->y > source->rects[i].y2)
	{
	    DEBUGOUT(_LtDebug(__FILE__, NULL, "Rectangles don't intersect\n"));
	    continue;
	}

/*	printf("Rectangle %d intersects\n", i); */

	/* if we're here the rectangles intersect */

	ASSIGN_MAX(rect->x, source->rects[i].x1, new_x1);
	ASSIGN_MIN(rect->x + rect->width, source->rects[i].x2, new_x2);
	ASSIGN_MAX(rect->y, source->rects[i].y1, new_y1);
	ASSIGN_MIN(rect->y + rect->height, source->rects[i].y2, new_y2);

	/* now we add the new rectangle. */

	__XmAddRectangle(&boxes, &size, &numRects,
			 new_x1, new_y1, new_x2, new_y2);
    }

    XtFree((char *)dest->rects);
    dest->rects = boxes;
    dest->size = size;
    dest->numRects = numRects;

    /* should compact the region here.. */

    _XmRegionComputeExtents(dest);
}

/**
 * _XmRegionGetNumRectangles
 *
 * Returns the number of rectangles in a region
 *
 **/
long
_XmRegionGetNumRectangles(XmRegion r)
{
    return r->numRects;
}

/**
 * _XmRegionGetRectangles
 *
 * Returns the array of boxes (and the count)
 * from a region. 
 *
 **/
void
_XmRegionGetRectangles(XmRegion r,
		       XRectangle **rects,
		       long *nrects)
{
    int i;

    *rects = (XRectangle *)XtMalloc(sizeof(XRectangle) * r->numRects);
    *nrects = r->numRects;

    for (i = 0; i < *nrects; i++)
    {
	(*rects)[i].x = r->rects[i].x1;
	(*rects)[i].y = r->rects[i].y1;
	(*rects)[i].width = r->rects[i].x2 - r->rects[i].x1;
	(*rects)[i].height = r->rects[i].y2 - r->rects[i].y1;
    }
}

/**
 * _XmRegionSetGCRegion
 *
 * Sets the region field in a GC to
 * the XmRegion.
 *
 **/
void
_XmRegionSetGCRegion(Display *dpy,
		     GC gc,
		     int x_origin,
		     int y_origin,
		     XmRegion r)
{
    XGCValues values;

    XSetRegion(dpy, gc, (Region)r);
    values.clip_x_origin = x_origin;
    values.clip_y_origin = y_origin;
    XChangeGC(dpy, gc, GCClipXOrigin | GCClipYOrigin, &values);
}

/**
 * _XmRegionDestroy
 *
 * Deallocates the region.
 *
 **/
void
_XmRegionDestroy(XmRegion r)
{
    XtFree((char *)r->rects);
    XtFree((char *)r);
}

/**
 * _XmRegionOffset
 *
 * Adds an offset (x,y) to all points within
 * a region.
 *
 **/
void
_XmRegionOffset(XmRegion region,
		int x,
		int y)
{
    int i;

    for (i = 0; i < region->numRects; i++)
    {
	region->rects[i].x1 += x;
	region->rects[i].x2 += x;

	region->rects[i].y1 += y;
	region->rects[i].y2 += y;
    }

    _XmRegionComputeExtents(region);
}

/**
 * _XmRegionIntersect
 *
 * Computes the intersection between two regions.
 *
 **/
void
_XmRegionIntersect(XmRegion reg1,
		   XmRegion reg2,
		   XmRegion newReg)
{
    long size = 1;
    long numRects = 0;
    XmRegionBox *boxes = (XmRegionBox *)XtMalloc(sizeof(XmRegionBox)
						 * size);
    int i, j;

    for (i = 0; i < reg1->numRects; i++)
    {
	for (j = 0; j < reg2->numRects; j++)
	{
	    int new_x1, new_y1, new_x2, new_y2;

	    /* special case for rectangles that don't intersect */

	    if (reg1->rects[i].x2 < reg2->rects[j].x1
		|| reg1->rects[i].x1 > reg2->rects[j].x2
		|| reg1->rects[i].y2 < reg2->rects[j].y1
		|| reg1->rects[i].y1 > reg2->rects[j].y2)
		continue;

	    /* if we're here the rectangles intersect */

	    ASSIGN_MAX(reg1->rects[i].x1, reg2->rects[j].x1, new_x1);
	    ASSIGN_MIN(reg1->rects[i].x2, reg2->rects[j].x2, new_x2);
	    ASSIGN_MAX(reg1->rects[i].y1, reg2->rects[j].y1, new_y1);
	    ASSIGN_MIN(reg1->rects[i].y2, reg2->rects[j].y2, new_y2);

	    /* now we add the new rectangle. */

	    __XmAddRectangle(&boxes, &size, &numRects,
			     new_x1, new_y1, new_x2, new_y2);
	}
    }

    XtFree((char *)newReg->rects);
    newReg->rects = boxes;
    newReg->size = size;
    newReg->numRects = numRects;

    /* compact the region here ... */

    _XmRegionComputeExtents(newReg);
}

/**
 * _XmRegionUnion
 *
 * Adds the lists of rectangles from two regions together.
 *
 **/
void
_XmRegionUnion(XmRegion reg1,
	       XmRegion reg2,
	       XmRegion newReg)
{
    long size = reg1->numRects + reg2->numRects;
    long numRects = 0;
    XmRegionBox *boxes = (XmRegionBox *)XtMalloc(sizeof(XmRegionBox)
						 * size);
    int i;

    for (i = 0; i < reg1->numRects; i++)
    {
	__XmAddRectangle(&boxes, &size, &numRects,
			 reg1->rects[i].x1, reg1->rects[i].y1,
			 reg1->rects[i].x2, reg1->rects[i].y2);
    }

    for (i = 0; i < reg2->numRects; i++)
    {
	__XmAddRectangle(&boxes, &size, &numRects,
			 reg2->rects[i].x1, reg2->rects[i].y1,
			 reg2->rects[i].x2, reg2->rects[i].y2);
    }

    XtFree((char *)newReg->rects);
    newReg->rects = boxes;
    newReg->size = size;
    newReg->numRects = numRects;

    /* compact the region here.. */

    _XmRegionComputeExtents(newReg);
}

/**
 * _XmRegionSubtract
 *
 * Computes the difference between two regions.
 *
 **/
void
_XmRegionSubtract(XmRegion regM,
		  XmRegion regS,
		  XmRegion regD)
{
    int i, j;
    long size = 1;
    long numRects = 0;
    XmRegionBox *boxes = (XmRegionBox *)XtMalloc(sizeof(XmRegionBox)
						 * size);

    /* this one will be a bit tougher, but we can simplify things by
       realizing that when a rectangle is subtracted from another rectangle
       we have the following cases:

       1) The two are equal, and the the result is nothing.
       2) one is contained completely within the other, resulting in a hole,
       which can be represented by four rectangles
       3) only one vertex of S lies inside of M, resulting in two rectangles
       4) only two vertices lie inside of M, resulting in three rectangles
       5) They do not intersect, so the resulting rectangle is the same 
       as before the subtraction.

       This computes regM - regS = regD... Test this, and just change
       the reg* names to what they should be. FIX ME
     */

    for (i = 0; i < regM->numRects; i++)
    {
	for (j = 0; j < regS->numRects; i++)
	{

	    /* (5) */
	    if (regM->rects[i].x1 > regS->rects[j].x2
		|| regM->rects[i].x2 < regS->rects[j].x1
		|| regM->rects[i].y1 > regS->rects[j].y2
		|| regM->rects[i].y2 < regS->rects[j].y1)
	    {
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regM->rects[i].y1,
				 regM->rects[i].x2, regM->rects[i].y2);
	    }

	    /* (1) */
	    if (regM->rects[i].x1 == regS->rects[j].x1
		&& regM->rects[i].x2 == regS->rects[j].x2
		&& regM->rects[i].y1 == regS->rects[j].y1
		&& regM->rects[i].y2 == regS->rects[j].y2)
	    {
		continue;
	    }

	    /* (3) -- part 1: x1,y1 is inside, x2,y2 is outside (so, x2,y1 as
	     * well as x2,y2 is outside) */
	    if (VALUE_IN_RANGE(regS->rects[j].x1,
			       regM->rects[i].x1, regM->rects[i].x2) &&
		VALUE_IN_RANGE(regS->rects[j].y1,
			       regM->rects[i].y1, regM->rects[i].y2) &&
		VALUE_OUT_RANGE(regS->rects[j].x2,
			        regM->rects[i].x1, regM->rects[i].x2) &&
		VALUE_OUT_RANGE(regS->rects[j].y2,
			        regM->rects[i].y1, regM->rects[i].y2))
	    {
		/* two rectangles -- looks like this


		   +------------------+
		   |       =          |
		   |   M   =  rect2   |           
		   |       +----------+
		   |       |          |
		   | rect1 |    S     |
		   |       |          |
		   +-------+----------+

		 */

		/* do rect1 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regM->rects[i].y1,
				 regS->rects[j].x1, regM->rects[i].y2);


		/* do rect2 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regS->rects[j].x1, regM->rects[i].y1,
				 regM->rects[i].x2, regS->rects[j].y1);
	    }
	    /* (3) -- part 2: x1,y1 is outside (so, x1,y2 as well as x2,y1
	     * is outside) x2,y2 is inside */
	    else if (VALUE_OUT_RANGE(regS->rects[j].x1,
				     regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_OUT_RANGE(regS->rects[j].y1,
				     regM->rects[i].y1, regM->rects[i].y2) &&
		     VALUE_IN_RANGE(regS->rects[j].x2,
				    regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_IN_RANGE(regS->rects[j].y2,
				    regM->rects[i].y1, regM->rects[i].y2))
	    {
		/* two rectangles -- looks like this


		   +------------------+
		   |   S   |          |
		   |       |  rect2   |           
		   |-------+          +
		   |       =          |
		   | rect1 =    M     |
		   |       =          |
		   +-------+----------+

		 */

		/* do rect1 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regS->rects[j].y2,
				 regS->rects[j].x2, regM->rects[i].y2);

		/* do rect2 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regS->rects[j].x2, regM->rects[i].y1,
				 regM->rects[i].x2, regM->rects[i].y2);
	    }
	    /* (2): x1, y1 is inside and x2, y2 is inside */
	    else if (VALUE_IN_RANGE(regS->rects[j].x1,
				    regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_IN_RANGE(regS->rects[j].y1,
				    regM->rects[i].y1, regM->rects[i].y2) &&
		     VALUE_IN_RANGE(regS->rects[j].x2,
				    regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_IN_RANGE(regS->rects[j].y2,
				    regM->rects[i].y1, regM->rects[i].y2))
	    {
		/* four rectangles -- looks like this


		   +-------M----------+
		   | 1                |
		   |====+---S--+======|           
		   | 2  |      |  3   |
		   |    |      |      |
		   |====+------+======|
		   |        4         |
		   +------------------+

		 */

		/* do rect1 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regM->rects[i].y1,
				 regM->rects[i].x2, regS->rects[i].y1);

		/* do rect2 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regS->rects[j].y1,
				 regS->rects[j].x1, regS->rects[j].y2);

		/* do rect3 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regS->rects[j].x2, regS->rects[j].y1,
				 regM->rects[j].x2, regS->rects[j].y2);

		/* do rect4 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regS->rects[j].y2,
				 regM->rects[i].x2, regM->rects[i].y2);

	    }
	    /* (4) part 1: x1, y1 is inside, x2 is inside, but not y2 */
	    else if (VALUE_IN_RANGE(regS->rects[j].x1,
				    regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_IN_RANGE(regS->rects[j].y1,
				    regM->rects[i].y1, regM->rects[i].y2) &&
		     VALUE_IN_RANGE(regS->rects[j].x2,
				    regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_OUT_RANGE(regS->rects[j].y2,
				     regM->rects[i].y1, regM->rects[i].y2))
	    {
		/* two rectangles -- looks like this


		   +-------M----------+
		   |    rect1         |
		   |                  |           
		   |====+--------+====|
		   |    |        |    |
		   | 2  |   S    | 3  |
		   |    |        |    |
		   +----+--------+----+

		 */

		/* do rect1 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regM->rects[i].y1,
				 regM->rects[i].x2, regS->rects[j].y1);

		/* do rect2 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regS->rects[j].y1,
				 regS->rects[j].x1, regM->rects[i].y2);

		/* do rect3 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regS->rects[j].x2, regS->rects[j].y1,
				 regM->rects[i].x2, regM->rects[i].y2);
	    }
	    /* (4) part 2: x1, y1 is inside, x2 is outside, but y2 is inside */
	    else if (VALUE_IN_RANGE(regS->rects[j].x1,
				    regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_IN_RANGE(regS->rects[j].y1,
				    regM->rects[i].y1, regM->rects[i].y2) &&
		     VALUE_OUT_RANGE(regS->rects[j].x2,
				     regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_IN_RANGE(regS->rects[j].y2,
				    regM->rects[i].y1, regM->rects[i].y2))
	    {
		/* two rectangles -- looks like this


		   +-------M----------+
		   |    rect1         |
		   |====+---S---------|
		   | 2  |             |
		   |====+-------------|
		   |    rect3         |
		   +------------------+

		 */

		/* do rect1 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regM->rects[i].y1,
				 regM->rects[i].x2, regS->rects[j].y1);

		/* do rect2 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regS->rects[j].y1,
				 regS->rects[j].x1, regS->rects[i].y2);

		/* do rect3 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regS->rects[j].y2,
				 regM->rects[i].x2, regM->rects[i].y2);
	    }
	    /* (4) part 3: x1 is inside, but y1 is outside, x2, y2 is inside */
	    else if (VALUE_IN_RANGE(regS->rects[j].x1,
				    regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_OUT_RANGE(regS->rects[j].y1,
				     regM->rects[i].y1, regM->rects[i].y2) &&
		     VALUE_IN_RANGE(regS->rects[j].x2,
				    regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_IN_RANGE(regS->rects[j].y2,
				    regM->rects[i].x1, regM->rects[i].x2))
	    {
		/* two rectangles -- looks like this

		   +------------------+
		   |    |     |       |
		   | 1  |  S  |   3   |
		   |    |     |       |
		   |====+-----+=======|
		   |         2        |
		   +--------M---------+

		 */

		/* do rect1 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regM->rects[j].y1,
				 regS->rects[j].x1, regS->rects[j].y2);
		/* do rect2 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regS->rects[j].y2,
				 regM->rects[i].x2, regM->rects[i].y2);
		/* do rect3 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regS->rects[j].x2, regM->rects[i].y1,
				 regM->rects[i].x2, regS->rects[j].y2);
	    }
	    /* (4) part 3: x1 is outside, y1 is inside, x2, y2 is inside */
	    else if (VALUE_OUT_RANGE(regS->rects[j].x1,
				     regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_IN_RANGE(regS->rects[j].y1,
				    regM->rects[i].y1, regM->rects[i].y2) &&
		     VALUE_IN_RANGE(regS->rects[j].x2,
				    regM->rects[i].x1, regM->rects[i].x2) &&
		     VALUE_IN_RANGE(regS->rects[j].y2,
				    regM->rects[i].x1, regM->rects[i].x2))
	    {
		/* two rectangles -- looks like this

		   +------M-----------+
		   |  rect1           |
		   |-------------+====|
		   |       S     | 2  |
		   |-------------+====|
		   |  rect3           |
		   +------------------+

		 */

		/* do rect1 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regM->rects[j].y1,
				 regM->rects[i].x2, regS->rects[j].y1);
		/* do rect2 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regS->rects[j].x2, regS->rects[j].y1,
				 regM->rects[i].x2, regS->rects[j].y2);

		/* do rect3 */
		__XmAddRectangle(&boxes, &size, &numRects,
				 regM->rects[i].x1, regS->rects[j].y2,
				 regM->rects[i].x2, regM->rects[i].y2);
	    }
	}
    }

    XtFree((char *)regD->rects);
    regD->rects = boxes;
    regD->size = size;
    regD->numRects = numRects;

    /* compact the region here... */

    _XmRegionComputeExtents(regD);
}

/**
 * _XmRegionIsEmpty
 *
 * Returns a truth value based on whether the
 * region has any rectangles in it or not.
 *
 **/
Boolean
_XmRegionIsEmpty(XmRegion r)
{
    return (r->numRects == 0);
}

/**
 * _XmRegionEqual
 *
 * Returns a boolean value telling whether or
 * not the regions are equal.
 *
 * This might be simpler (and more efficient)
 * if we start coalescing the rectangles in
 * a uniform manner (so the same region will
 * have the same number of rectangles, and they
 * will all be the same.)  For now we take the
 * rather inefficient -- but easy -- way out
 * by subtracting one from the other and seeing if
 * the result is empty. 
 *
 **/
Boolean
_XmRegionEqual(XmRegion r1, XmRegion r2)
{
    Boolean	r;
    XmRegion	sub = _XmRegionCreate();

    _XmRegionSubtract(r1, r2, sub);

    r =  _XmRegionIsEmpty(sub);

    _XmRegionDestroy(sub);
    return r;
}

/**
 * _XmRegionPointInRegion
 *
 * Returns true if the x,y point is contained
 * within any of the rectangles of the region.
 *
 **/
Boolean
_XmRegionPointInRegion(XmRegion pRegion,
		       int x,
		       int y)
{
    int i;

    for (i = 0; i < pRegion->numRects; i++)
    {
	if (VALUE_IN_RANGE(x, pRegion->rects[i].x1, pRegion->rects[i].x2) &&
	    VALUE_IN_RANGE(y, pRegion->rects[i].y1, pRegion->rects[i].y2))
	{
	    return True;
	}
    }

    return False;
}

/**
 * _XmRegionClear
 *
 * Clears out the rects array by setting numRects
 * to zero.  Does not deallocate anything, though.
 * 
 **/
void
_XmRegionClear(XmRegion r)
{
    r->numRects = 0;
    _XmRegionComputeExtents(r);
}

/**
 * _XmRegionShrink
 * 
 * We shrink a region by adding the dx,dy to the
 * x1,y1 points, and subtracting it from the
 * x2,y2 points.
 *
 **/
void
_XmRegionShrink(XmRegion r,
		int dx,
		int dy)
{
    int i;

    for (i = 0; i < r->numRects; i++)
    {
	r->rects[i].x1 += dx;
	r->rects[i].y1 += dy;

	r->rects[i].x2 -= dx;
	r->rects[i].y2 -= dy;
    }

    _XmRegionComputeExtents(r);
}

void 
_XmRegionDrawShadow(Display *display,
		    Drawable d,
		    GC top_gc,
		    GC bottom_gc,
		    XmRegion region,
		    Dimension border_thick,
		    Dimension shadow_thick,
		    unsigned int shadow_type)
{
    /* this is all wrong, but... */
    int i;

    for (i = 0; i < region->numRects; i++)
    {
	_XmDrawShadows(display,
		       d,
		       top_gc,
		       bottom_gc,
		       region->rects[i].x1, region->rects[i].y1,
		       region->rects[i].x2 - region->rects[i].x1,
		       region->rects[i].y2 - region->rects[i].y1,
		       shadow_thick, shadow_type);
    }
}
