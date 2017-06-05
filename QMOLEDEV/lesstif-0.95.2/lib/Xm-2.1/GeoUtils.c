/**
 *
 * $Id: GeoUtils.c,v 1.3 2004/09/28 04:09:19 dannybackx Exp $
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

static const char rcsid[] = "$Id: GeoUtils.c,v 1.3 2004/09/28 04:09:19 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>		/* for qsort */
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/BulletinBP.h>
#include <Xm/SeparatorP.h>
#include <Xm/SeparatoGP.h>
#include <Xm/RowColumnP.h>

#include <XmI/DebugUtil.h>

/*
 * Rick Scott made a *major* contribution figuring out the PRE_ and POST_
 * set semantics.  Thanks, Rick.
 */

#ifdef GEO_DEBUG

static void
dump_box(XmKidGeometry box)
{
    if (!box)
    {
	return;
    }

    while (box->kid)
    {
	printf("    KID %s REQ %08x X %-5d Y %-5d W %-5d H %-5d B %-5d\n",
	       XtName(box->kid), box->box.request_mode, box->box.x, box->box.y,
	       box->box.width, box->box.height, box->box.border_width);

	box++;
    }
}

static void
dump_layout(XmGeoRowLayout rows, XmKidGeometry boxes)
{
    if (!rows)
    {
	return;
    }

    --rows;

    do
    {
	rows++;

	printf("ROW: %08x\n", (unsigned int)rows);

	printf("  end: %d fixup: %08x even_width: %d even_height: %d\n",
	       rows->end, (unsigned int)rows->fix_up,
	       rows->even_width, rows->even_height);

	printf("  min_height: %d stretch_height: %d uniform_border %d\n",
	       rows->min_height, rows->stretch_height, rows->uniform_border);

	printf("  border: %d fill_mode: %d fit_mode: %d sticky_end: %d\n",
	       rows->border, rows->fill_mode, rows->fit_mode, rows->sticky_end);

	printf("  space_above: %d space_end: %d space_between: %d\n",
	       rows->space_above, rows->space_end, rows->space_between);

	printf("  max_box_height: %d boxes_width: %d fill_width %d\n",
	       rows->max_box_height, rows->boxes_width, rows->fill_width);

	printf("  box_count: %d\n", rows->box_count);

	dump_box(boxes);

	boxes += rows->box_count + 1;
    }
    while (!rows->end);
}

static void
dump_matrix(XmGeoMatrix geo)
{
    printf("MATRIX: composite: %08x instigator %08x boxes %08x\n",
	   (unsigned int)geo->composite, (unsigned int)geo->instigator,
	   (unsigned int)geo->boxes);

    printf("  layouts: %08x margin_w: %d margin_h: %d stretch_boxes: %d\n",
	   (unsigned int)geo->layouts, geo->margin_w, geo->margin_h,
	   geo->stretch_boxes);

    printf("  uniform_border: %d border: %d max_major: %d boxes_minor: %d\n",
	   geo->uniform_border, geo->border, geo->max_major, geo->boxes_minor);

    printf("  fill_minor: %d width: %d height: %d set: %08x\n",
	   geo->fill_minor, geo->width, geo->height,
	   (unsigned int)geo->set_except);

    printf("  almost: %08x no_geo: %08x extension %08x destruct: %08x\n",
	   (unsigned int)geo->almost_except, (unsigned int)geo->no_geo_request,
	   (unsigned int)geo->extension, (unsigned int)geo->ext_destructor);

    printf("  arrange: %08x major: %d\n",
	   (unsigned int)geo->arrange_boxes, geo->major_order);

    dump_layout((XmGeoRowLayout)geo->layouts, geo->boxes);
}
#endif

static void
FitBoxesProportional(XmKidGeometry boxes, Dimension box_count,
		     Dimension box_wd, short fill_wd)
{
    int tmp, curx;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		"FitBoxesProportional: box_count: %d box_wd: %d fill_wd: %d\n",
		      box_count, box_wd, fill_wd));

    if (box_wd > box_count)
    {
	curx = 0;

	while (boxes->kid)
	{
	    tmp = boxes->box.width + boxes->box.border_width * 2;
	    tmp *= fill_wd;
	    tmp /= box_wd;

	    if (tmp > boxes->box.width)
	    {
		boxes->box.width = 1;
	    }
	    else
	    {
		boxes->box.width -= tmp;
	    }

	    boxes->box.x += curx;
	    curx -= tmp;

	    boxes++;
	}
    }
    else
    {
	tmp = box_wd -fill_wd;

	if (tmp > box_count && box_count > 0)
	{
	    tmp /= box_count;
	}
	else
	{
	    tmp = 1;
	}

	curx = 0;

	while (boxes->kid)
	{
	    boxes->box.width = tmp;
	    boxes->box.x = curx;
	    curx += tmp;
	    boxes++;
	}
    }
}

static int
sort_by_width(XmConst void *a, XmConst void *b)
{
    XmKidGeometry geoA, geoB;

    geoA = *((XmKidGeometry *)a);
    geoB = *((XmKidGeometry *)b);

    if (geoA->box.width < geoB->box.width)
    {
	return 0;
    }

    return 1;
}

static void
FitBoxesAveraging(XmKidGeometry boxes, Dimension box_count,
		  Dimension box_wd, Dimension fill_wd)
{
    XmKidGeometry *tbox;
    int i, cnt;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		   "FitBoxesAveraging: box_count: %d box_wd: %d fill_wd: %d\n",
		      box_count, box_wd, fill_wd));

    tbox = (XmKidGeometry *)XtCalloc(box_count, sizeof(Widget));

    for (i = 0; i < box_count; i++)
    {
	tbox[i] = boxes;
	box_wd -= boxes->box.border_width * 2;
	boxes++;
    }

    qsort((void *)tbox, box_count, sizeof(XmKidGeometry), sort_by_width);

    for (i = 0, cnt = box_count; i < box_count; i++)
    {
	if (fill_wd < box_wd - (tbox[i]->box.width * cnt))
	{
	    box_wd -= tbox[i]->box.width;
	}
	else
	{
	    break;
	}

	cnt--;
    }

    if (i < box_count)
    {
	if (box_wd < fill_wd)
	{
	    box_wd = 1;
	}
	else
	{
	    box_wd = (box_wd - fill_wd) / (box_count - i);
	}
    }

    for (i = 0; i < box_count; i++)
    {
	tbox[i]->box.width = box_wd;
    }

    XtFree((char *)tbox);
}



static void
_XmGeoCalcFill(Dimension width, Dimension margin, int box_count,
	       Dimension space_end, Dimension space_between,
	       Dimension *start, Dimension *tween)
{
    /*
    Dimension tmp, tmp2;
    */

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmGeoCalcFill: %d %d %d %d %d %d %d\n",
		      width, margin, box_count, space_end, space_between,
		      *start, *tween));

#if 1
	if (box_count - 1 > 0)
	{
	    *start = margin;
	    *tween = (width - (2 * margin)) / (box_count - 1);
	}
	else
	{
		*start = width / 2;
		*tween = 0;
	}
#else
    if (space_end == 0)
    {
	if (margin != 1)
	{
	    if (space_between == 0)
	    {
		space_between = box_count - 1;
	    }
	}
	space_end = 1;
    }

    tmp = space_between;
    space_between = space_end;
    tmp2 = tmp * (box_count - 1);
    tmp2 += space_between * 2;
    space_end = tmp2;

    *start = (width * space_between) / space_end;
    if (*start < margin)
    {
	space_between *= 2;

	if (space_between < space_end)
	{
	    space_end -= space_between;
	}

	margin *= 2;

	if (margin < width)
	{
	    width -= margin;
	    *start = margin / 2;
	}
    }

    *tween = (tmp * width) / space_end;
#endif
}



static Position
_XmGeoLayoutSimple(XmKidGeometry boxes, XmGeoRowLayout layout,
		   Position x, Position y,
		   Dimension end, Dimension start, Dimension space_between)
{
    Dimension bw, bh;
    Position e;			/* Replacement for "end" which is unsigned */

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmGeoLayoutSimple: x,y %d %d\n", x, y));

    x += start;

    while (boxes->kid)
    {
	bh = boxes->box.height + boxes->box.border_width * 2;
	boxes->box.x = x;

	if (bh == layout->max_box_height)
	{
	    boxes->box.y = y;
	}
	else
	{
	    boxes->box.y = y + (layout->max_box_height - bh) / 2;
	}

	x += boxes->box.width + (boxes->box.border_width * 2) + space_between;

	boxes++;
    }

    if (layout->sticky_end)
    {
	boxes--;

	bw = boxes->box.border_width * 2 + boxes->box.width;

	e = end - bw;

	if (e < boxes->box.x)
	{
	    boxes->box.x = e;
	}
    }

    return y + layout->max_box_height;
}

static void
SegmentFill(XmKidGeometry boxes, int box_count, XmGeoRowLayout layout,
	    Position x, Dimension width, Dimension margin,
	    Position curx, Dimension max_x, Dimension end_space,
	    Dimension space_between)
{
    XmKidGeometry end_box = boxes + box_count;
    Widget end_kid;
    int fill_wd;
    int space_width, box_width;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
 		      "SEGMENT FILL: %d %d %d %d %d %d %d %d\n",
 		      box_count, x, width, margin, curx,
 		      max_x, end_space, space_between));
    
    space_width = 2 * end_space + space_between * (box_count - 1);
    box_width = curx - space_width + end_space;
    if (box_width <= 0)
    {
	box_width = 1;
    }

    end_kid = end_box->kid;
    end_box->kid = NULL;
    if (layout->fill_mode == XmGEO_CENTER)
    {
	if (width - (space_width + box_width) > 0)
	{
	    fill_wd = width - box_width;
	}
	else
	{
	    fill_wd = margin * 2;
	}
	
	_XmGeoCalcFill(fill_wd, margin, box_count, end_space,
		       space_between, &end_space, &space_between);
    }
    else if (layout->fill_mode == XmGEO_EXPAND)
    {
	FitBoxesProportional(boxes, box_count, box_width, curx - end_space);
    }
    fill_wd = x + end_space;
    while (boxes->kid != NULL)
    {
	boxes->box.x = fill_wd;
	fill_wd += 
	    boxes->box.width + 2 * boxes->box.border_width + space_between;
	boxes++;
    }
    end_box->kid = end_kid;
}


/*
 *  FUNCTION: _XmGeoLayoutWrap
 *  RETURNS: The position below the last used row
 *  DESCRIPTION:
 *  This function arranges one (locical) row to fit inside width and margin,
 *  and possibly breaking the row into 2 or more rows.
 *  CALLED FROM _XmGeoArrangeList
*/

static Position
_XmGeoLayoutWrap(XmKidGeometry boxes, XmGeoRowLayout layout,
		 Position x, Position y,
		 Dimension start, Dimension space_between, Dimension end,
		 Dimension width, Dimension margin)
{
    Position left_edge, right_edge;
    int boxcount;
    Dimension kid_width, kid_height;
    XmKidGeometry curbox;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGeoLayoutWrap\n"));

    left_edge = x + start;
    boxcount = 0;
    curbox = boxes;
    while(curbox->kid != NULL)
    {
	kid_width = curbox->box.width + 2 * curbox->box.border_width;
	kid_height = curbox->box.height + 2 * curbox->box.border_width;
	right_edge = left_edge + kid_width;
	if (right_edge > end)
	{
	    if (boxcount != 0)
	    {
		/* move the kid one row down and finish this one */
		
		SegmentFill(boxes, boxcount, layout, x, width, margin,
			    left_edge - space_between, end, start,
			    space_between);
		
		boxcount = 0;
		left_edge = x + start;
		y += layout->max_box_height;
		boxes = curbox;
		right_edge = left_edge + kid_width;
	    }
	}
	if (right_edge > end)
	{
	    /* If the kid is alone on the line can't i move it to the 
	     * next row, but i most squeeze it to fit
	     */
	    int missing_space = right_edge + start - end - margin; 
	    
	    if (missing_space < boxes->box.width)
	    {
		if (missing_space > 0)
		{
		    curbox->box.width -= missing_space;
		}
	    }
	    else
	    {
		curbox->box.width = 1;
	    }
	    kid_width = curbox->box.width + 2 * curbox->box.border_width;
	    right_edge = left_edge + kid_width;
	}
	curbox->box.x = left_edge;
	curbox->box.y = y;
	if (layout->max_box_height != kid_height)
	{
	    curbox->box.y += layout->max_box_height - kid_height / 2;
	}
	left_edge = space_between + right_edge;
	boxcount++;
	curbox++;
    }
    SegmentFill(boxes, boxcount, layout, x, width, start,
		left_edge - space_between, end, start,
		space_between);
    if (layout->sticky_end)
    {
	curbox--;
	kid_width = curbox->box.width + 2 * curbox->box.border_width;
	if (curbox->box.x + kid_width < end)
	{
	    curbox->box.x  = end - kid_width;
	}
    }
    return y + layout->max_box_height;
}


/*
 *  FUNCTION: _XmGeoArrangeList
 *  RETURNS: The position below the last used row
 *  DESCRIPTION:
 *  This function arranges one (locical) row to fit inside width and margin,
 *  and possibly breaking the row into 2 or more rows.
 *  CALLED FROM _XmGeoArrangeBoxes
*/
static Position
_XmGeoArrangeList(XmKidGeometry boxes, XmGeoRowLayout layout,
		  Position x, Position y,
		  Dimension width, Dimension margin)
{
    Dimension start, box_wd, end, tween, fill_wd;
    short adjust;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmGeoArrangeList: x: %d y: %d wd: %d\n",
		      x, y, width));

    box_wd = layout->boxes_width;
    fill_wd = layout->boxes_width + layout->fill_width + 2 * margin;
    adjust = fill_wd - width;

    if (layout->space_end > margin)
    {
	start = layout->space_end;
    }
    else
    {
	start = margin;
    }

    end = width + x - margin;

    tween = layout->space_between;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmGeoArrangeList: fill_wd: %d width: %d\n",
		      fill_wd, width));

    if (fill_wd > width && layout->fit_mode == XmGEO_WRAP)
    {
	return _XmGeoLayoutWrap(boxes, layout, x, y,
				start, tween, end,
				width, margin);
    }
    else if (fill_wd > width)
    {
	if (layout->fit_mode == XmGEO_AVERAGING)
	{
	    FitBoxesAveraging(boxes, layout->box_count, box_wd, adjust);
	}
	else
	{
	    FitBoxesProportional(boxes, layout->box_count, box_wd, adjust);
	}
    }
    else if (fill_wd != width)
    {
	if (layout->fill_mode == XmGEO_CENTER)
	{
	    fill_wd = layout->fill_width + margin * 2 + width - fill_wd;

	    _XmGeoCalcFill(fill_wd,
			   margin, layout->box_count,
			   layout->space_end, tween,
			   &start, &tween);
	}
	else
	{
	    FitBoxesProportional(boxes, layout->box_count, box_wd, adjust);
	}
    }

    return _XmGeoLayoutSimple(boxes, layout,
			      x, y, end, start, tween);
}

/*
 * also called from arrange boxes
 */
static Dimension
_XmGeoStretchVertical(XmGeoMatrix geoSpec, Dimension height, Dimension maxh)
{
    register XmGeoRowLayout layout;
    register XmKidGeometry boxPtr;
    int stretch = 0;
    short fill = 0, fill_used;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmGeoStretchVertical\n"));

    layout = &(geoSpec->layouts->row);

    if (maxh - height < 0)
    {

	fill = 0;

	while (!layout->end)
	{

	    if (layout->stretch_height &&
		layout->min_height < layout->max_box_height)
	    {

		fill += layout->max_box_height - layout->min_height;
		stretch++;
	    }

	    layout++;
	}

	if (fill >= (height - maxh))
	{
	    fill = -(height - maxh) / stretch;
	}
	else if (fill)
	{
	    fill /= -stretch;
	}
    }
    else
    {

	fill = 0;

	while (!layout->end)
	{
	    if (layout->stretch_height)
	    {
		stretch++;
	    }
	    layout++;
	}

	fill = (int)maxh - (int)height;
	fill /= stretch;
    }

    layout = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    fill_used = 0;
    while (!layout->end)
    {
	if (layout->stretch_height)
	{

	    while (boxPtr->kid)
	    {
		boxPtr->box.y += fill_used;
		boxPtr->box.height += fill;
		boxPtr++;
	    }

	    boxPtr++;

	    fill_used += fill;
	}
	else
	{
	    while (boxPtr->kid)
	    {
		boxPtr->box.y += fill_used;
		boxPtr++;
	    }

	    boxPtr++;
	}

	layout++;
    }

    return height + fill_used;
}

/*
 * also called from arrange boxes
 */
static Dimension
_XmGeoFillVertical(XmGeoMatrix geoSpec, Dimension height, Dimension maxh)
{
    register XmGeoRowLayout layout;
    register XmKidGeometry boxPtr;
    int stretch = 0;
    short fill = 0, fill_used;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmGeoFillVertical: ht: %d maxh: %d\n",
		      height, maxh));

    layout = &(geoSpec->layouts->row);

    while (!layout->end)
    {
	if (layout->stretch_height)
	{
	    return _XmGeoStretchVertical(geoSpec, height, maxh);
	}

	stretch++;
	layout++;
    }

    fill = (10 * ((int)maxh - (int)height)) / (stretch + 1);

    layout = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    fill_used = 0;
    if (fill > 0)
    {
    int i = 1;

	while (!layout->end)
	{
	    while (boxPtr->kid)
	    {
		boxPtr->box.y += ((i * fill) / 10);
		boxPtr++;
	    }

	    boxPtr++;
	    fill_used += (fill / 10);

	    layout++;
	    i++;
	}
    }

    return height + fill_used;
}

/*
 * called from ArrangeBoxes
 * calls _XmGeoBoxesSameWidth, _XmGeoBoxesSameHeight
 */
void
_XmGeoAdjustBoxes(XmGeoMatrix geoSpec)
{
    Dimension border;
    register XmKidGeometry boxPtr;
    register XmGeoRowLayout layoutPtr;

    DEBUGOUT(_LtDebug(__FILE__, geoSpec->composite, "_XmGeoAdjustBoxes\n"));

    layoutPtr = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    while (!layoutPtr->end)
    {

	border = 0;

	if (layoutPtr->even_width != 0)
	{
	    _XmGeoBoxesSameWidth(boxPtr, layoutPtr->even_width);
	}

	if (layoutPtr->even_height != 0)
	{
	    _XmGeoBoxesSameHeight(boxPtr, layoutPtr->even_height);
	}

	if (geoSpec->uniform_border)
	{
	    border = geoSpec->border;
	}
	else if (layoutPtr->uniform_border)
	{
	    border = layoutPtr->border;
	}

	while (boxPtr->kid != NULL)
	{
	    boxPtr->box.border_width = border;
	    boxPtr++;
	}

	boxPtr++;

	layoutPtr++;
    }
}

/*
 * called from HandleChangeManaged (third)
 * calls AdjustBoxes
 */
void
_XmGeoArrangeBoxes(XmGeoMatrix geoSpec,
		   Position x, Position y,
		   Dimension *pW, Dimension *pH)
{
    XmGeoArrangeProc arrange;
    register XmGeoRowLayout layoutPtr;
    register XmKidGeometry boxPtr;
    Dimension wd;
    Position oy;

    DEBUGOUT(_LtDebug(__FILE__, geoSpec->composite,
		      "_XmGeoArrangeBoxes %dx%d%+d%+d\n", 
		      pW ? *pW : 0,
		      pH ? *pH : 0,
		      x, y));

    oy = y;

    arrange = geoSpec->arrange_boxes;

    /* recursion avoidance */
    if (arrange != NULL && arrange != _XmGeoArrangeBoxes)
    {
	(arrange) (geoSpec, x, y, pW, pH);
    }

    _XmGeoAdjustBoxes(geoSpec);

    _XmGeoGetDimensions(geoSpec);

    layoutPtr = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    if (geoSpec->margin_h < layoutPtr->space_above)
    {
	y += layoutPtr->space_above;
    }
    else
    {
	y += geoSpec->margin_h;
    }

    wd = 2 * geoSpec->margin_w + geoSpec->max_major;
    if (*pW)
    {
	wd = *pW;
    }

    while (!layoutPtr->end)
    {

	y = _XmGeoArrangeList(boxPtr, layoutPtr, x, y, wd, geoSpec->margin_w);

	boxPtr += layoutPtr->box_count + 1;

	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "_XmGeoArrangeBoxes(2): y: %d\n", y));

	layoutPtr++;
	y += layoutPtr->space_above;
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmGeoArrangeBoxes(3): y: %d\n", y));

    if (layoutPtr->space_above < geoSpec->margin_h)
    {
	y += geoSpec->margin_h - layoutPtr->space_above;
    }

    y -= oy;

    /* thumpers FFT dialog when shrinking it */
    /* messagebox/test20 */
    /* messagebox/test3 */

    if (*pH != 0 /*&& *pH > y*/)
    {
	if (geoSpec->stretch_boxes)
	{
	    y = _XmGeoStretchVertical(geoSpec, y, *pH);
	}
	else
	{
	    y = _XmGeoFillVertical(geoSpec, y, *pH);
	}
    }

    geoSpec->width = wd;
    if (*pW < wd)
    {
	*pW = wd;
    }

    geoSpec->height = y;
    if (*pH < y)
    {
	*pH = y;
    }
}

/*
 * called from AdjustBoxes
 */
Dimension
_XmGeoBoxesSameWidth(XmKidGeometry rowPtr,
		     Dimension width)
{
    Dimension wd = width;
    XmKidGeometry boxes = rowPtr;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmGeoBoxesSameWidth: %d", width));

    if (width == 0)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "\n"));
	return 0;
    }
    else if (width >= 1)
    {
	while (boxes->kid != NULL)
	{
	    if (boxes->box.width > wd)
	    {
		wd = boxes->box.width;
	    }
	    boxes++;
	}
	if (width > 1)
	{
	    wd += width;
	}
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, " %i", wd));
    boxes = rowPtr;

    while (boxes->kid != NULL)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, " %s", XtName(boxes->kid)));
	boxes->box.width = wd;
	boxes++;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "\n"));
    return wd;
}

/*
 * also called from AdjustBoxes
 */
Dimension
_XmGeoBoxesSameHeight(XmKidGeometry rowPtr,
		      Dimension height)
{
    Dimension ht = height;
    XmKidGeometry boxes = rowPtr;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmGeoBoxesSameHeight: %d", height));

    if (height == 0)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "\n"));
	return 0;
    }
    else if (height >= 1)
    {
	while (boxes->kid != NULL)
	{
	    if (boxes->box.height > ht)
	    {
		ht = boxes->box.height;
	    }
	    boxes++;
	}
	if (height > 1)
	{
	    ht += height;
	}
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, " %i", ht));
    boxes = rowPtr;
    while (boxes->kid != NULL)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, " %s", XtName(boxes->kid)));
	boxes->box.height = ht;
	boxes++;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "\n"));
    return ht;
}

/*
 * Erase the background at both the old and the new position of a rectangle
 * object. Redrawing events will be triggered (and queued) for the affected
 * areas.
 * This could be useful dealing with that weird double-expose problem I had
 * with gadgets after I put traversal in.  Now I've got to remember *where*
 * I fixed that.  I seem to remember an extra expose in BB... - MLM
 */
void
_XmGeoClearRectObjAreas(RectObj r, XWindowChanges *old)
{
    Widget par;			/* This really better be a composite, or we're dead */
    int full_border;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGeoClearRectObjAreas\n"));

    par = XtParent((Widget)r);

    full_border = old->border_width * 2;

    XClearArea(XtDisplay(par), XtWindow(par),
	       old->x, old->y,
	       old->width + full_border, old->height + full_border,
	       True);

    full_border = r->rectangle.border_width * 2;

    XClearArea(XtDisplay(par), XtWindow(par),
	       r->rectangle.x, r->rectangle.y,
	       r->rectangle.width + full_border,
	       r->rectangle.height + full_border,
	       True);
}

/*
 * called from XmRCGetKidGeo
 */
int
_XmGeoCount_kids(CompositeWidget c)
{
    Cardinal i, cnt;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGeoCount_kids\n"));

    for (i = 0, cnt = 0; i < MGR_NumChildren(c); i++)
    {
	if (XtIsManaged(MGR_Children(c)[i]))
	{
	    cnt++;
	}
    }

    return cnt;
}

/*
 * called from ArrangeBoxes (second)
 */
void
_XmGeoGetDimensions(XmGeoMatrix geoSpec)
{
    register XmKidGeometry boxPtr;
    register XmGeoRowLayout layoutPtr;
    Dimension curw = 0, curh = 0, fillh = 0, tmpw, tmph, roww, rowh;
    int bcount;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGeoGetDimensions\n"));

    layoutPtr = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    if (layoutPtr->space_above > geoSpec->margin_h)
    {
	fillh = layoutPtr->space_above - geoSpec->margin_h;
    }

    geoSpec->stretch_boxes = False;

    while (!layoutPtr->end)
    {

	bcount = 0;
	roww = rowh = 0;
	while (boxPtr->kid)
	{

	    tmph = boxPtr->box.border_width * 2 + boxPtr->box.height;
	    tmpw = boxPtr->box.border_width * 2 + boxPtr->box.width;

	    if (tmph > rowh)
	    {
		rowh = tmph;
	    }

	    roww += tmpw;
	    boxPtr++;
	    bcount++;
	}

	layoutPtr->box_count = bcount;
	layoutPtr->boxes_width = roww;
	layoutPtr->max_box_height = rowh;

	if (layoutPtr->border != 0)
	{
	    if (layoutPtr->fit_mode != XmGEO_WRAP)
	    {
		layoutPtr->stretch_height = 0;
		geoSpec->stretch_boxes = True;
	    }
	}

	if (layoutPtr->space_end > geoSpec->margin_w)
	{
	    tmpw = (layoutPtr->space_end - geoSpec->margin_w) * 2;
	}
	else
	{
	    tmpw = 0;
	}

	tmpw += (bcount - 1) * layoutPtr->space_between;

	layoutPtr->fill_width = tmpw;

	roww += tmpw;

	if (curw < roww)
	{
	    curw = roww;
	}

	layoutPtr++;
	curh += rowh;
	if (!layoutPtr->end)
	{
	    fillh += layoutPtr->space_above;
	}

	boxPtr++;
    }

    if (layoutPtr->space_above > geoSpec->margin_h)
    {
	fillh -= layoutPtr->space_above - geoSpec->margin_h;
    }

    geoSpec->max_major = curw;
    geoSpec->boxes_minor = curh;
    geoSpec->fill_minor = fillh;
}

XmKidGeometry
_XmGetKidGeo(Widget wid,
	     Widget instigator,
	     XtWidgetGeometry *request,
	     int uniform_border,
	     Dimension border,
	     int uniform_width_margins,
	     int uniform_height_margins,
	     Widget help,
	     int geo_type)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGetKidGeo\n"));

    return NULL;
}

/*
 * called from GeoMatrixGet.  Called once for each kid in the Matrix.
 * also called from XmRCGetKidGeo
 */
void
_XmGeoLoadValues(Widget wid,
		 int geoType,
		 Widget instigator,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *geoResult)
{
    XtWidgetGeometry dummy;
    int mode = 0;

    if (request)
    {
	mode = request->request_mode;
    }

    if (wid != instigator)
    {
	request = &dummy;

	if (geoType == XmGET_ACTUAL_SIZE || geoType != XmGET_PREFERRED_SIZE)
	{
	    mode = 0;
	}
	else
	{
	    XtQueryGeometry(wid, NULL, &dummy);
	    mode = dummy.request_mode;
	    DEBUGOUT(_LtDebug2(__FILE__, XtParent(wid), wid,
				    "_XmGeoLoadValues: query_geometry -> [%s]\n",
				    _LtDebugWidgetGeometry2String(&dummy)));
	}
    }

    if (mode & CWX)
    {
	geoResult->x = request->x;
    }
    else
    {
	geoResult->x = XtX(wid);
    }
    if (mode & CWY)
    {
	geoResult->y = request->y;
    }
    else
    {
	geoResult->y = XtY(wid);
    }
    if (mode & CWWidth)
    {
	geoResult->width = request->width;
    }
    else
    {
	geoResult->width = XtWidth(wid);
    }
    if (mode & CWHeight)
    {
	geoResult->height = request->height;
    }
    else
    {
	geoResult->height = XtHeight(wid);
    }
    if (mode & CWBorderWidth)
    {
	geoResult->border_width = request->border_width;
    }
    else
    {
	geoResult->border_width = XtBorderWidth(wid);
    }

    geoResult->request_mode = CWX | CWY | CWWidth | CWHeight | CWBorderWidth;

    DEBUGOUT(_LtDebug(__FILE__, wid,
		      "_XmGeoLoadValues: x: %d y: %d wd: %d ht: %d bdr: %d\n",
		      geoResult->x, geoResult->y,
		      geoResult->width, geoResult->height,
		      geoResult->border_width));
}

/*
 * class method calls this to allocate the matrix record (first).
 * class method is called from BB HandleChangeManaged.
 */
XmGeoMatrix
_XmGeoMatrixAlloc(unsigned int numRows,
		  unsigned int numBoxes,
		  unsigned int extSize)
{
    XmGeoMatrix mat;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "XmGeoMatrixAlloc called with %d %d %d\n",
		      numRows, numBoxes, extSize));

    mat = (XmGeoMatrix)XtCalloc(1, sizeof(XmGeoMatrixRec));

    mat->extension = XtMalloc(extSize);

    mat->layouts = (XmGeoMajorLayout)XtCalloc(numRows + 1,
					      sizeof(XmGeoMajorLayoutRec));

    mat->boxes = (XmKidGeometry)XtCalloc(numRows + numBoxes + 1,
					 sizeof(XmKidGeometryRec));

    return mat;
}

/*
 * called last in HandleChangeManaged (fifth)
 */
void
_XmGeoMatrixFree(XmGeoMatrix geo_spec)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGeoMatrixFree\n"));

    if (geo_spec->ext_destructor) {
	(*geo_spec->ext_destructor) (geo_spec->extension);
    }
    if (geo_spec->extension)
	    XtFree((char *)geo_spec->extension);

    if (geo_spec->layouts) {
	XtFree((char *)geo_spec->layouts);
    }

    if (geo_spec->boxes) {
	XtFree((char *)geo_spec->boxes);
    }

    XtFree((char *)geo_spec);
}

/*
 * also called from HandleChangeManaged, after the matrix is created (second).
 * calls GeoLoadValues
 */
void
_XmGeoMatrixGet(XmGeoMatrix geoSpec,
		int geoType)
{
    register XmGeoRowLayout layoutPtr;
    register XmKidGeometry boxPtr, startBox;

    DEBUGOUT(_LtDebug(__FILE__, geoSpec->composite, "_XmGeoMatrixGet\n"));

    layoutPtr = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;
    startBox = boxPtr;

    while (!layoutPtr->end)
    {
	if (boxPtr->kid == NULL)
	{
	    if (layoutPtr->fix_up)
	    {
		(layoutPtr->fix_up) (geoSpec, XmGET_PREFERRED_SIZE,
				     (XmGeoMajorLayout)layoutPtr, startBox);
	    }
	    boxPtr++;

	    startBox = boxPtr;

	    layoutPtr++;

	    continue;
	}

	_XmGeoLoadValues(boxPtr->kid, XmGET_PREFERRED_SIZE, geoSpec->instigator,
			 &geoSpec->instig_request, &boxPtr->box);

	if (boxPtr->kid == geoSpec->instigator)
	{
#if 1
	    /* rws 22 Apr 1998
	       messagebox/test19
	     */
	    if (layoutPtr->even_width > 1)
	    {
		boxPtr->box.width -= layoutPtr->even_width;
	    }
	    if (layoutPtr->even_height > 1)
	    {
		boxPtr->box.height -= layoutPtr->even_height;
	    }
#endif
	    geoSpec->in_layout = &boxPtr->box;
	}

	boxPtr++;
    }
}

/*
 * called from HandleChangeManaged (fourth)
 */
void
_XmGeoMatrixSet(XmGeoMatrix geoSpec)
{
    register XmKidGeometry boxPtr;
    register XmGeoRowLayout layoutPtr;
    int bcount;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmGeoMatrixSet HERE *********************\n"));

    if (geoSpec->set_except != NULL)
    {
	if ((geoSpec->set_except) (geoSpec))
	{
	    return;
	}
    }

    boxPtr = geoSpec->boxes;
    layoutPtr = &(geoSpec->layouts->row);

    while (!layoutPtr->end)
    {

	if (layoutPtr->fix_up)
	{
	    (layoutPtr->fix_up) (geoSpec, XmGEO_PRE_SET,
				 (XmGeoMajorLayout)layoutPtr, boxPtr);
	}

	bcount = layoutPtr->box_count;

	layoutPtr++;
	boxPtr += bcount + 1;
    }

    boxPtr = geoSpec->boxes;
    layoutPtr = &(geoSpec->layouts->row);

    while (!layoutPtr->end)
    {

	_XmSetKidGeo(boxPtr, geoSpec->instigator);

	bcount = layoutPtr->box_count;
	layoutPtr++;
	boxPtr += bcount + 1;
    }

    boxPtr = geoSpec->boxes;
    layoutPtr = &(geoSpec->layouts->row);

    while (!layoutPtr->end)
    {
	if (layoutPtr->fix_up)
	{
	    (layoutPtr->fix_up) (geoSpec, XmGEO_POST_SET,
				 (XmGeoMajorLayout)layoutPtr, boxPtr);
	}

	bcount = layoutPtr->box_count;

	layoutPtr++;
	boxPtr += bcount + 1;
    }
}

/*
 * Checks the widget's desired geometry against the parent's proposal
 * and returns True only if the width, height, border width as well as
 * the position (x, y) are of concern in the desired geometry and are
 * equal to the proposal.
 */
#define WANTS(r,x)	((r)->request_mode & (x))
Boolean
_XmGeoReplyYes(Widget wid,
	       XtWidgetGeometry *desired,
	       XtWidgetGeometry *response)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGeoReplyYes\n"));

    if (WANTS(desired, CWWidth))
    {
	if (!WANTS(response, CWWidth) || desired->width != response->width)
	{
	    return False;
	}
    }

    if (WANTS(desired, CWHeight))
    {
	if (!WANTS(response, CWHeight) || desired->height != response->height)
	{
	    return False;
	}
    }

    if (WANTS(desired, CWBorderWidth))
    {
	if (!WANTS(response, CWBorderWidth) ||
	    desired->border_width != response->border_width)
	{
	    return False;
	}
    }

    if (WANTS(desired, CWX))
    {
	if (!WANTS(response, CWX) || desired->x != response->x)
	{
	    return False;
	}
    }

    if (WANTS(desired, CWY))
    {
	if (!WANTS(response, CWY) || desired->y != response->y)
	{
	    return False;
	}
    }

    return True;
}
#undef WANTS

/*
 * Called from geoMatrixCreate, apparently to fill in init values for
 * KidGeo
 */
Boolean
_XmGeoSetupKid(XmKidGeometry geo,
	       Widget kidWid)
{
    DEBUGOUT(_LtDebug2(__FILE__, kidWid ? XtParent(kidWid) : NULL, kidWid, "_XmGeoSetupKid\n"));

    if (!kidWid)
    {
	return False;
    }
    if (!XtIsRectObj(kidWid))
    {
	return False;
    }
    if (!XtIsManaged(kidWid))
    {
	return False;
    }

    geo->kid = kidWid;

    return True;
}

Boolean
_XmGeometryEqual(Widget wid,
		 XtWidgetGeometry *geoA,
		 XtWidgetGeometry *geoB)
{
    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmGeometryEqual (%s) (%s)\n",
    	_LtDebugWidgetGeometry2String(geoA),
    	_LtDebugWidgetGeometry2String(geoB)));

    return (memcmp(geoA, geoB, sizeof(XtWidgetGeometry)) == 0);
}

/*
 * resize method
 */
void
_XmHandleSizeUpdate(Widget w,
		    unsigned char policy,
		    XmGeoCreateProc createMatrix)
{
    XmGeoMatrix geo;
    Dimension wd, ht;
    XtWidgetGeometry request;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmHandleSizeUpdate %dx%d\n",XtWidth(w),XtHeight(w)));

    geo = (createMatrix) (w, NULL, NULL);

    _XmGeoMatrixGet(geo, XmGET_PREFERRED_SIZE);

    if (policy == XmRESIZE_NONE)
    {
	wd = XtWidth(w);
	ht = XtHeight(w);
    }
    else
    {
	wd = 0;
	ht = 0;
    }

    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmHandleSizeUpdate %dx%d %dx%d\n",XtWidth(w),XtHeight(w),wd,ht));
    if (policy == XmRESIZE_GROW)
    {
	/* check the return against the original.  If the procedure would
	 * like the BB to shrink, call again */
	if (wd < XtWidth(w) || ht < XtHeight(w))
	{
	    if (wd < XtWidth(w))
	    {
		wd = XtWidth(w);
	    }
	    if (ht < XtHeight(w))
	    {
		ht = XtHeight(w);
	    }
	    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);
	}
    }

    if (policy != XmRESIZE_NONE)
    {
	request.request_mode = (CWWidth | CWHeight);
	request.width = wd;
	request.height = ht;
	request.request_mode |= CWBorderWidth;
	request.border_width = XtBorderWidth(w);

	if (XtGeometryYes == _XmMakeGeometryRequest(w, &request))
	{
	    if (request.width != wd || request.height != ht)
	    {
		_XmGeoArrangeBoxes(geo, 0, 0, &request.width, &request.height);
	    }

	    _XmGeoMatrixSet(geo);
	}
    }

    _XmGeoMatrixFree(geo);
}

/*
 * query geom method
 */
XtGeometryResult
_XmHandleQueryGeometry(Widget wid,
		       XtWidgetGeometry *intended,
		       XtWidgetGeometry *desired,
		       unsigned char policy,
		       XmGeoCreateProc createMatrix)
{
#if 0
    Dimension wd, ht;
    XmGeoMatrix cache;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmHandleQueryGeometry\n"));

    *desired = *intended;

    desired->request_mode &= ~(XtCWQueryOnly | CWWidth | CWHeight);

    if ((desired->request_mode & CWWidth) && desired->width == XtWidth(wid) &&
	(desired->request_mode & CWHeight) && desired->height == XtHeight(wid))
    {
	return XtGeometryNo;
    }

    if (policy == XmRESIZE_NONE)
    {
	desired->width = XtWidth(wid);
	desired->height = XtHeight(wid);
	return XtGeometryNo;
    }
    else if (policy == XmRESIZE_ANY)
    {
	return XtGeometryYes;
    }

    cache = createMatrix(wid, NULL, NULL);

    _XmGeoMatrixGet(cache, XmGET_PREFERRED_SIZE);

    wd = ht = 0;

    _XmGeoArrangeBoxes(cache, 0, 0, &wd, &ht);

    if (wd < XtWidth(wid))
    {
	wd = XtWidth(wid);
    }

    if (ht < XtHeight(wid))
    {
	ht = XtHeight(wid);
    }

    _XmGeoMatrixFree(cache);

    if (((desired->request_mode & CWWidth) &&
	 desired->width < wd) ||
	((desired->request_mode & CWHeight) &&
	 desired->height < ht))
    {

	desired->width = wd;
	desired->height = ht;

	return XtGeometryAlmost;
    }

    return XtGeometryYes;
#else
    Dimension wd, ht;
    XmGeoMatrix cache;

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmHandleQueryGeometry - intended %s\n",
    		_LtDebugWidgetGeometry2String(intended)));

    if (policy == XmRESIZE_NONE)
    {
	desired->width = XtWidth(wid);
	desired->height = XtHeight(wid);
    }
    else
    {
	cache = createMatrix(wid, NULL, NULL);

	_XmGeoMatrixGet(cache, XmGET_PREFERRED_SIZE);

	wd = XtIsRealized(wid) ? 0 : XtWidth(wid);
	ht = XtIsRealized(wid) ? 0 : XtHeight(wid);

	_XmGeoArrangeBoxes(cache, 0, 0, &wd, &ht);
	_XmGeoMatrixFree(cache);

	if (policy == XmRESIZE_GROW)
	{
	    if (wd < XtWidth(wid))
	    {
		wd = XtWidth(wid);
	    }
	    if (ht < XtHeight(wid))
	    {
		ht = XtHeight(wid);
	    }
	}

	desired->request_mode = CWWidth | CWHeight;
	desired->width  = wd == 0 ? 1 : wd;
	desired->height = ht == 0 ? 1 : ht;
    }

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmHandleQueryGeometry - desired %s\n",
    		_LtDebugWidgetGeometry2String(desired)));
    return _XmGMReplyToQueryGeometry(wid, intended, desired);
#endif
}

static XtGeometryResult
QueryNonePolicy(XmGeoMatrix cache, XtWidgetGeometry *geom)
{
    Dimension wd, ht;

    DEBUGOUT(_LtDebug(__FILE__, cache->composite, "QUERY NONE POLICY\n"));

    _XmGeoMatrixGet(cache, XmGET_PREFERRED_SIZE);

    wd = XtWidth(cache->composite);
    ht = XtHeight(cache->composite);

    _XmGeoArrangeBoxes(cache, 0, 0, &wd, &ht);

    geom->request_mode = 0;

    if (wd == XtWidth(cache->composite) && ht == XtHeight(cache->composite))
    {
	return XtGeometryYes;
    }

    return XtGeometryNo;
}

static XtGeometryResult
QueryGrowPolicy(XmGeoMatrix cache, XtWidgetGeometry *geom)
{
    Dimension wd, ht;
    XtWidgetGeometry ret;
    XtGeometryResult res;

    DEBUGOUT(_LtDebug(__FILE__, cache->composite, "QUERY GROW POLICY\n"));

    _XmGeoMatrixGet(cache, XmGET_PREFERRED_SIZE);

    wd = 0;
    ht = 0;

    _XmGeoArrangeBoxes(cache, 0, 0, &wd, &ht);

    geom->request_mode = CWWidth | CWHeight;
    geom->width = wd;
    geom->height = ht;

    if (wd == XtWidth(cache->composite) && ht == XtHeight(cache->composite))
    {
	geom->request_mode = 0;
	return XtGeometryYes;
    }

    if (wd < XtWidth(cache->composite) || ht < XtHeight(cache->composite))
    {
	_XmGeoMatrixGet(cache, XmGET_PREFERRED_SIZE);

	if (wd < XtWidth(cache->composite))
	{
	    wd = XtWidth(cache->composite);
	}
	if (ht < XtHeight(cache->composite))
	{
	    ht = XtHeight(cache->composite);
	}

	_XmGeoArrangeBoxes(cache, 0, 0, &wd, &ht);
    }

    ret = *geom;
    ret.request_mode |= XtCWQueryOnly;

    res = _XmMakeGeometryRequest(cache->composite, &ret);

    if (res == XtGeometryAlmost)
    {
	if (ret.width < XtWidth(cache->composite) ||
	    ret.height < XtHeight(cache->composite))
	{
	    return XtGeometryNo;
	}

	_XmGeoMatrixGet(cache, XmGET_PREFERRED_SIZE);

	wd = ret.width;
	ht = ret.height;

	_XmGeoArrangeBoxes(cache, 0, 0, &wd, &ht);

	*geom = ret;

	if (wd != ret.width || ht != ret.height)
	{
	    return XtGeometryNo;
	}

	return res;
    }

    if ((ret.request_mode & (CWWidth | CWHeight)) != (CWWidth | CWHeight))
    {
	return XtGeometryNo;
    }

    *geom = ret;

    if (wd == ret.width && ht == ret.height)
    {
	return XtGeometryYes;
    }

    _XmGeoMatrixGet(cache, XmGET_PREFERRED_SIZE);

    wd = ret.width;
    ht = ret.height;
    _XmGeoArrangeBoxes(cache, 0, 0, &wd, &ht);

    if (wd != ret.width || ht != ret.height)
    {
	return XtGeometryNo;
    }

    return XtGeometryYes;
}

static XtGeometryResult
QueryAnyPolicy(XmGeoMatrix cache, XtWidgetGeometry *geom)
{
    Dimension wd, ht;
    XtWidgetGeometry ret;
    XtGeometryResult res;

    DEBUGOUT(_LtDebug(__FILE__, cache->composite, "QUERY ANY POLICY\n"));

    _XmGeoMatrixGet(cache, XmGET_PREFERRED_SIZE);

    wd = 0;
    ht = 0;

    _XmGeoArrangeBoxes(cache, 0, 0, &wd, &ht);

    geom->request_mode = CWWidth | CWHeight;
    geom->width = wd;
    geom->height = ht;

    if (wd == XtWidth(cache->composite) && ht == XtHeight(cache->composite))
    {
	geom->request_mode = 0;
	return XtGeometryYes;
    }

    ret = *geom;
    ret.request_mode |= XtCWQueryOnly;

    res = _XmMakeGeometryRequest(cache->composite, &ret);

    if (res == XtGeometryAlmost)
    {
	return res;
    }

    if ((ret.request_mode & (CWWidth | CWHeight)) != (CWWidth | CWHeight))
    {
	return XtGeometryNo;
    }

    *geom = ret;

    if (wd == ret.width && ht == ret.height)
    {
	return XtGeometryYes;
    }

    _XmGeoMatrixGet(cache, XmGET_PREFERRED_SIZE);

    wd = ret.width;
    ht = ret.height;

    _XmGeoArrangeBoxes(cache, 0, 0, &wd, &ht);

    if (wd != ret.width || ht != ret.height)
    {
	return XtGeometryNo;
    }

    return XtGeometryYes;
}

/*
 * parent geometry manager
 */
XtGeometryResult
_XmHandleGeometryManager(Widget w,
			 Widget instigator,
			 XtWidgetGeometry *desired,
			 XtWidgetGeometry *allowed,
			 unsigned char policy,
			 XmGeoMatrix *cachePtr,
			 XmGeoCreateProc createMatrix)
{
    XmGeoMatrix cache = NULL;
    XtGeometryResult res = XtGeometryNo;
    XtWidgetGeometry want;

    DEBUGOUT(_LtDebug2(__FILE__, w, instigator, "_XmHandleGeometryManager (%s)\n",
    	_LtDebugWidgetGeometry2String(desired)));

    memset((void *)&want, 0, sizeof(XtWidgetGeometry));

    if (cachePtr && *cachePtr)
    {
	cache = *cachePtr;

	if (cache->composite == w && cache->instigator == instigator &&
	    _XmGeometryEqual(instigator, cache->in_layout, desired))
	{

	    if (desired->request_mode & XtCWQueryOnly)
	    {
		return XtGeometryYes;
	    }

	    if (cache->parent_request.request_mode)
	    {
		cache->parent_request.request_mode &= ~XtCWQueryOnly;

		_XmMakeGeometryRequest(w, &cache->parent_request);
	    }

	    _XmGeoMatrixSet(cache);
	    _XmGeoMatrixFree(cache);

	    *cachePtr = NULL;

	    return XtGeometryYes;
	}

	*cachePtr = NULL;
	_XmGeoMatrixFree(cache);
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "geo request for %d %d\n",
		      desired->width, desired->height));

    cache = (createMatrix) (w, instigator, desired);
    if (cache->no_geo_request && (cache->no_geo_request) (cache))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "geo request said no\n"));

	_XmGeoMatrixFree(cache);

	*cachePtr = NULL;

	return XtGeometryNo;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "querying policy\n"));
    switch (policy)
    {
    case XmRESIZE_NONE:
	res = QueryNonePolicy(cache, &want);
	break;

    case XmRESIZE_GROW:
	res = QueryGrowPolicy(cache, &want);
	break;

    case XmRESIZE_ANY:
    default:
	res = QueryAnyPolicy(cache, &want);
	break;
    }

    switch (res)
    {
    case XtGeometryYes:
	if (cache->in_layout)
	{
	    if (_XmGeometryEqual(instigator, desired, cache->in_layout))
	    {
		if (desired->request_mode & XtCWQueryOnly)
		{
		    cache->parent_request = want;
		    *allowed = *cache->in_layout;
		    *cachePtr = cache;
		}
		else
		{
		    want.request_mode &= ~XtCWQueryOnly;

		    _XmMakeGeometryRequest(cache->composite, &want);

		    *allowed = *cache->in_layout;

		    _XmGeoMatrixSet(cache);
		    _XmGeoMatrixFree(cache);

		    *cachePtr = NULL;
		}
	    }
	    else
	    {
		res = XtGeometryAlmost;
		*allowed = *cache->in_layout;
		_XmGeoMatrixFree(cache);
		*cachePtr = NULL;
	    }
	}
	else
	{
	    /* rws 1 Jul 1999 (Canada Day)
	       selectionbox/test10 deals with this case. The list gets
	       un-managed and therefore the instigator, which is the SW,
	       never gets into the GeoMatrix.
	     */
	    res = XtGeometryNo;
	    _XmGeoMatrixFree(cache);
	    *cachePtr = NULL;
	}
	break;

    case XtGeometryNo:
	*allowed = *cache->in_layout;
	_XmGeoMatrixFree(cache);
	*cachePtr = NULL;
	break;

    case XtGeometryAlmost:
	cache->parent_request = want;
	*cachePtr = cache;
	*allowed = *cache->in_layout;
	break;

    default:
	_XmError(cache->composite,
		 "Geometry request returned unknow result: %d\n", res);
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmHandleGeometry returns %s\n", _LtDebugGeometryResult2String(res)));

    return res;
}

/*
 * called from the child to the parent??  Yep - MLM
 * Ask our parent for our new desired geometry. In case of an answer of
 * XtGeometryAlmost we ask him a second time with the proposed geometry,
 * so he can accept and set the new geometry. If he refuses in any way then
 * we'll inform the user of our parent's impudent habbits (because he
 * doesn't conform to the geometry negotiation protocol, see O'Reilly,
 * Vol 5, pp. 264 for details about XtMakeGeometryRequest()).
 * This is at least only a convenience function.
 * According to informed sources, this is *ALWAYS* used by Motif; Motif never
 * uses a bare XtMakeGeometryRequest()
 */
XtGeometryResult
_XmMakeGeometryRequest(Widget w,
		       XtWidgetGeometry *desired)
{
    XtWidgetGeometry response;
    XtGeometryResult answer;
    /* Only for warnings : */
    XtWidgetGeometry g1, g2;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmMakeGeometryRequest %s to %s\n",
		      _LtDebugWidgetGeometry2String(desired), XtName(XtParent(w))));
    DEBUGOUT(_LtDebug("GM", w, "_XmMakeGeometryRequest %s to %s\n",
		      _LtDebugWidgetGeometry2String(desired), XtName(XtParent(w))));

    g1 = *desired;
    response.request_mode = 0;	/* Eliminate Uninitialized Memory Read (UMR) from Purify */

    /*
     * This happens in too many places; take it together here.
     *
     * If we ask for 0 width/height, Xt will fail on it.
     * Prevent this from happening by changing this to 1.
     *
     * Is this a hack ? Not really, in some occasions it is possible
     * that we go through temporary stuff like this. Either we prevent
     * this from going to Xt in all individual places, or we put it
     * together here, as we do now.
     */
    if ((desired->request_mode & CWWidth) && desired->width == 0) {
	desired->width = 1;
	if (XtWidth(w) == 0)
		XtWidth(w) = 1;
    }
    if ((desired->request_mode & CWHeight) && desired->height == 0) {
	desired->height = 1;
	if (XtHeight(w) == 0)
		XtHeight(w) = 1;
    }
    /* End Xt crash prevention */

    answer = XtMakeGeometryRequest(w, desired, &response);

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmMakeGeometryRequest : %s [%s]\n",
		      _LtDebugGeometryResult2String(answer),
		      _LtDebugWidgetGeometry2String(&response)));

    if (answer == XtGeometryAlmost)
    {
	/*
	 * Give the parent a second chance... If it won't accept what it has
	 * proposed just a second (or so) before, we'll cry as loud
	 * as we can (hoping no one has set the sink to /dev/null...)
	 */
	/*
	memcpy(desired, &response, sizeof(XtWidgetGeometry));
	*/
	*desired = response;

#if 1
	/* rws 31 Jan 1999
	   grok is causing an X error when you start it up with the name
	   of a database.  This protects against that.
	 */
	desired->width = desired->width == 0 ? 1 : desired->width;
	desired->height = desired->height == 0 ? 1 : desired->height;
#endif

	g2 = *desired;

	answer = XtMakeGeometryRequest(w, desired, &response);

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmMakeGeometryRequest (2nd request) => %s [%s]\n",
			  _LtDebugGeometryResult2String(answer),
			  _LtDebugWidgetGeometry2String(&response)));

	if (answer != XtGeometryYes)
	{
	    _XmWarning(w, "Parent refused resize request. "
		       "Second XtMakeGeometryRequest() failed\n"
		       "\tParent is %s (%s)\n"
		       "\tOriginal request %s, Second request %s\n"
		       "\tParent size %d %d",
		       XtName(XtParent(w)),
		       XtClass(XtParent(w))->core_class.class_name,
		       _LtDebugWidgetGeometry2String(&g1),
		       _LtDebugWidgetGeometry2String(&g2),
		       XtWidth(XtParent(w)), XtHeight(XtParent(w)));
	}
    }

    return answer;
}

/*
 * XmGeoSegmentFixUpProc
 */
void
_XmMenuBarFix(XmGeoMatrix geoSpec,
	      int action,
	      XmGeoMajorLayout layoutPtr,
	      XmKidGeometry rowPtr)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmMenuBarFix\n"));

    if (action == XmGEO_PRE_SET)
    {
	while (rowPtr->kid)
	{

	    if (XmIsMenuBar(rowPtr->kid) && XtIsManaged(rowPtr->kid))
	    {
		rowPtr->box.x -= geoSpec->margin_w;
		rowPtr->box.y -= geoSpec->margin_h;
		rowPtr->box.width += geoSpec->margin_w * 2;
	    }

	    rowPtr++;
	}
    }
    else if (action == XmGET_PREFERRED_SIZE)
    {
	while (rowPtr->kid)
	{

	    if (XmIsMenuBar(rowPtr->kid) && XtIsManaged(rowPtr->kid))
	    {
	    XtWidgetGeometry pref;

	    	XtQueryGeometry(rowPtr->kid, NULL, &pref);
		DEBUGOUT(_LtDebug(__FILE__, rowPtr->kid,
			"_XmMenuBarFix(GET_PREFERRED_SIZE) %s\n",
		        _LtDebugWidgetGeometry2String(&pref)));
		rowPtr->box.x = 0;
		rowPtr->box.y = 0;
		rowPtr->box.width = pref.width;
		rowPtr->box.height = pref.height;
	    }
	    rowPtr++;
	}
    }
}

/*
 * XmGeoSegmentFixUpProc
 */
void
_XmSeparatorFix(XmGeoMatrix geoSpec,
		int action,
		XmGeoMajorLayout layoutPtr,
		XmKidGeometry rowPtr)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmSeparatorFix: action: %d\n", action));

    if (action == XmGEO_PRE_SET)
    {
	while (rowPtr->kid)
	{
	    if (XmIsSeparator(rowPtr->kid) && XtIsManaged(rowPtr->kid))
	    {
		if (SEP_Orientation(rowPtr->kid) == XmHORIZONTAL)
		{
		    rowPtr->box.x -= geoSpec->margin_w;
		    rowPtr->box.width += geoSpec->margin_w * 2;
		}
		else if (SEP_Orientation(rowPtr->kid) == XmVERTICAL)
		{
		    rowPtr->box.y -= geoSpec->margin_h;
		    rowPtr->box.height += geoSpec->margin_h * 2;
		}
	    }
	    else if (XmIsSeparatorGadget(rowPtr->kid) &&
		     XtIsManaged(rowPtr->kid))
	    {
		if (SEPG_Orientation(rowPtr->kid) == XmHORIZONTAL)
		{
		    rowPtr->box.x -= geoSpec->margin_w;
		    rowPtr->box.width += geoSpec->margin_w * 2;
		}
		else if (SEPG_Orientation(rowPtr->kid) == XmVERTICAL)
		{
		    rowPtr->box.y -= geoSpec->margin_h;
		    rowPtr->box.height += geoSpec->margin_h * 2;
		}
	    }

	    rowPtr++;
	}
    }
    else if (action == XmGET_PREFERRED_SIZE)
    {
	while (rowPtr->kid)
	{

	    if (XmIsSeparator(rowPtr->kid) && XtIsManaged(rowPtr->kid))
	    {
		if (SEP_Orientation(rowPtr->kid) == XmHORIZONTAL)
		{
		    rowPtr->box.x = 0;
		    rowPtr->box.width = 0;
		}
		else if (SEP_Orientation(rowPtr->kid) == XmVERTICAL)
		{
		    rowPtr->box.y = 0;
		    rowPtr->box.height = 0;
		}
	    }
	    else if (XmIsSeparatorGadget(rowPtr->kid) &&
		     XtIsManaged(rowPtr->kid))
	    {
		if (SEPG_Orientation(rowPtr->kid) == XmHORIZONTAL)
		{
		    rowPtr->box.x = 0;
		    rowPtr->box.width = 0;
		}
		else if (SEPG_Orientation(rowPtr->kid) == XmVERTICAL)
		{
		    rowPtr->box.y = 0;
		    rowPtr->box.height = 0;
		}
	    }

	    rowPtr++;
	}
    }
}

/* 
 * called from GeoMatrixSet
 * calls _XmConfigureObject
 */
void
_XmSetKidGeo(XmKidGeometry kg,
	     Widget instigator)
{
    while (kg->kid != NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, kg->kid,
			  "_XmSetKidGeo: x,y: %d %d w,h: %d %d b: %d\n",
			  kg->box.x, kg->box.y, kg->box.width, kg->box.height,
			  kg->box.border_width));

	if (kg->kid == instigator)
	{
	    DEBUGOUT(_LtDebug(__FILE__, NULL, "setting instig\n"));
	    XtX(instigator) = kg->box.x;
	    XtY(instigator) = kg->box.y;
	    XtWidth(instigator) = kg->box.width;
	    XtHeight(instigator) = kg->box.height;
	    instigator->core.border_width = kg->box.border_width;
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, NULL, "setting normal\n"));
	    _XmConfigureObject(kg->kid,
			       kg->box.x, kg->box.y,
			       kg->box.width, kg->box.height,
			       kg->box.border_width);
	}
	kg++;
    }
}

/*
 * This function is
 * a shortcut for simple query_geometry() methods which are interested
 * only in their width and height but neither their position nor border
 * width. First, compute your desired width and height in your
 * query_geometry() method and then return the result of the call to
 * _XmGMReplyToQueryGeometry().
 * Danny beat me to it - MLM
 */
#define	Wants(x)	(request->request_mode & x)
XtGeometryResult
_XmGMReplyToQueryGeometry(Widget w,
			  XtWidgetGeometry *request,
			  XtWidgetGeometry *reply)
{
#if 0

    if (_LtDebugInDebug(__FILE__, w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmGMReplyToQueryGeometry(Flags "));
	if (Wants(CWX))
	    DEBUGOUT(_LtDebug0(__FILE__, w, "x "));
	if (Wants(CWY))
	    DEBUGOUT(_LtDebug0(__FILE__, w, "y "));
	if (Wants(CWWidth))
	    DEBUGOUT(_LtDebug0(__FILE__, w, "w "));
	if (Wants(CWHeight))
	    DEBUGOUT(_LtDebug0(__FILE__, w, "h "));
	DEBUGOUT(_LtDebug0(__FILE__, w, ")\n"));
    }

    if (Wants(CWWidth) && request->width == reply->width &&
	Wants(CWHeight) && request->height == reply->height)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmGMReplyToQueryGeometry (%d %d) => Yes\n",
			  request->width, request->height));

	return XtGeometryYes;
    }

    if (Wants(CWWidth) && request->width == reply->width &&
	Wants(CWHeight) && request->height != reply->height)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmGMReplyToQueryGeometry (%d %d) => Almost\n",
			  request->width, request->height));

	return XtGeometryAlmost;
    }

    if (Wants(CWWidth) && request->width != reply->width &&
	Wants(CWHeight) && request->height == reply->height)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmGMReplyToQueryGeometry (%d %d) => Almost\n",
			  request->width, request->height));

	return XtGeometryAlmost;
    }

    if (Wants(CWWidth) && request->width != reply->width &&
	Wants(CWHeight) && request->height != reply->height)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmGMReplyToQueryGeometry (%d %d) => Almost\n",
			  request->width, request->height));

	return XtGeometryAlmost;
    }

    if (Wants(CWWidth) && request->width == reply->width && !Wants(CWHeight))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmGMReplyToQueryGeometry (%d %d) => Yes\n",
			  request->width, request->height));

	return XtGeometryYes;
    }

    if (Wants(CWHeight) && request->height == reply->height && !Wants(CWWidth))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmGMReplyToQueryGeometry (%d %d) => Yes\n",
			  request->width, request->height));

	return XtGeometryYes;
    }

    if (reply->width == XtWidth(w) && reply->height == XtHeight(w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmGMReplyToQueryGeometry (REPLY %d %d "
			  "ACTUAL %d %d) => No\n",
			  reply->width, reply->height,
			  XtWidth(w), XtHeight(w)));

	return XtGeometryNo;
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmGMReplyToQueryGeometry (REQUEST %d %d REPLY %d %d "
		      "ACTUAL %d %d) => Almost\n",
		      request->width, request->height,
		      reply->width, reply->height,
		      XtWidth(w), XtHeight(w)));

    return XtGeometryAlmost;
#else
    XtGeometryResult result;

    if (XmIsLabel(w))
    {
	/* rather not change */
	reply->request_mode = CWWidth | CWHeight;
    }
    else if (XmIsList(w))
    {
	/* rather not change */
	reply->request_mode = CWWidth | CWHeight;
    }
    else if (XmIsText(w))
    {
	/* don't care if I change */
	reply->request_mode = 0;
    }
    else if (XmIsTextField(w))
    {
	/* don't care if I change */
	reply->request_mode = 0;
    }
    else if (XmIsBulletinBoard(w))
    {
	/* rather not change */
	reply->request_mode = CWWidth | CWHeight;
    }
    else if (XmIsForm(w))
    {
	/* don't care if I change */
	reply->request_mode = 0;
    }
    else if (XmIsDrawingArea(w))
    {
	/* don't care if I change */
	reply->request_mode = 0;
    }
    else if (XmIsFrame(w))
    {
	/* rather not change */
	reply->request_mode = CWWidth | CWHeight;
    }
    else if (XmIsScrolledWindow(w))
    {
	/* don't care if I change */
	reply->request_mode = 0;
    }
    else if (XmIsRowColumn(w))
    {
	/* rather not change */
	reply->request_mode = CWWidth | CWHeight;
    }
    else
    {
	/* don't care if I change */
	reply->request_mode = 0;
    }
    if (((request->request_mode & (CWWidth | CWHeight)) == (CWWidth | CWHeight)) && 
		request->width == reply->width && 
		request->height == reply->height)
    {
	result = XtGeometryYes;
    }
    else if ((reply->width == XtWidth(w) && reply->height == XtHeight(w)) &&
             ((reply->request_mode & (CWWidth | CWHeight)) == (CWWidth | CWHeight)))
    {
	result = XtGeometryNo;
    }
    else
    {
	result = XtGeometryAlmost;
    }
    reply->request_mode = CWWidth | CWHeight;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmGMReplyToQueryGeometry(%d)\n\trequest (%s) reply (%s) result %s\n",
		      __FILE__, __LINE__,
		      _LtDebugWidgetGeometry2String(request),
		      _LtDebugWidgetGeometry2String(reply),
		      _LtDebugGeometryResult2String(result)));
    return result;
#endif
}
#undef	Wants

XtGeometryResult
_XmGMHandleQueryGeometry(Widget w,
			 XtWidgetGeometry *proposed, XtWidgetGeometry *answer,
			 Dimension margin_width, Dimension margin_height,
			 unsigned char resize_policy)
{
#if 0
    Dimension wantw, wanth;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGMHandleQueryGeometry\n"));

    *answer = *proposed;

    if (resize_policy == XmRESIZE_NONE && XtIsRealized(w))
    {
	wantw = XtWidth(w);
	wanth = XtHeight(w);
    }
    else
    {
	_XmGMCalcSize(w, margin_width, margin_height, &wantw, &wanth);

	if (XtIsRealized(w))
	{
	    if (resize_policy == XmRESIZE_GROW && XtWidth(w) > wantw)
	    {
		wantw = XtWidth(w);
	    }
	    if (resize_policy == XmRESIZE_GROW && XtHeight(w) > wanth)
	    {
		wanth = XtHeight(w);
	    }
	}
    }

    if (wantw == XtWidth(w) && wanth == XtHeight(w))
    {
	return XtGeometryNo;
    }

    if (wantw == proposed->width && wanth == proposed->height)
    {
	*answer = *proposed;
	return XtGeometryYes;
    }

    answer->request_mode &= (CWWidth | CWHeight | XtCWQueryOnly);
    answer->width = wantw;
    answer->height = wanth;

    return XtGeometryAlmost;
#else
    Dimension wantw, wanth;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGMHandleQueryGeometry\n"));

    if (resize_policy == XmRESIZE_NONE)
    {
	wantw = XtWidth(w);
	wanth = XtHeight(w);
    }
    else
    {
	_XmGMCalcSize(w, margin_width, margin_height, &wantw, &wanth);

	if (resize_policy == XmRESIZE_GROW && XtWidth(w) > wantw)
	{
	    wantw = XtWidth(w);
	}
	if (resize_policy == XmRESIZE_GROW && XtHeight(w) > wanth)
	{
	    wanth = XtHeight(w);
	}
    }
    answer->width = wantw;
    answer->height = wanth;

    return _XmGMReplyToQueryGeometry(w, proposed, answer);
#endif
}

void
_XmGMEnforceMargin(Widget w,
		   Dimension margin_width, Dimension margin_height,
		   Boolean useSetValues)
{
    Position x, y;
    Cardinal i;
    Widget child;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGMEnforceMargin\n"));

    for (i = 0; i < MGR_NumChildren(w); i++)
    {
	if (!XtIsRectObj(w) || !XtIsManaged(MGR_Children(w)[i]))
	{
	    continue;
	}

	child = MGR_Children(w)[i];

	x = XtX(child);
	y = XtY(child);

	if (x < margin_width)
	{
	    x = margin_width;
	}

	if (y < margin_height)
	{
	    y = margin_height;
	}

	if (x != XtX(child) || y != XtY(child))
	{

	    if (useSetValues)
	    {
		XtVaSetValues(child,
			      XmNx, x, XmNy, y,
			      NULL);
	    }
	    else
	    {
		_XmMoveObject(child, x, y);
	    }
	}
    }
}

void
_XmGMCalcSize(Widget w, Dimension margin_w, Dimension margin_h,
	      Dimension *retw, Dimension *reth)
{
    Widget child;
    Cardinal i;
    Dimension curx = 0, cury = 0;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGMCalcSize\n"));

    *retw = *reth = 0;
    for (i = 0; i < MGR_NumChildren(w); i++)
    {

	child = MGR_Children(w)[i];

	if (!XtIsRectObj(child) || !XtIsManaged(child))
	{
	    continue;
	}

	curx = child->core.x + child->core.width
	    + 2 * child->core.border_width;
	cury = child->core.y + child->core.height
	    + 2 * child->core.border_width;

	if (curx > *retw)
	{
	    *retw = curx;
	}
	if (cury > *reth)
	{
	    *reth = cury;
	}
    }

    *retw += margin_w + MGR_ShadowThickness(w);
    *reth += margin_h + MGR_ShadowThickness(w);

    if (!*retw)
    {
	*retw = 10;
    }
    if (!*reth)
    {
	*reth = 10;
    }
}

void
_XmGMDoLayout(Widget w, Dimension margin_w, Dimension margin_h,
	      unsigned char resize_policy, short adjust)
{
    Dimension retw, reth;
    XtWidgetGeometry request;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGMDoLayout\n"));

    _XmGMCalcSize(w, margin_w, margin_h, &retw, &reth);

    if (resize_policy == XmRESIZE_NONE)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "DoResize: XmRESIZE_NONE: %s\n", XtName(w)));
	return;
    }

    request.width = XtWidth(w);
    request.height = XtHeight(w);
    request.border_width = XtBorderWidth(w);

    if (resize_policy == XmRESIZE_ANY)
    {
	request.width = retw;
	request.height = reth;
    }

    if (resize_policy == XmRESIZE_GROW)
    {
	if (retw > XtWidth(w))
	{
	    request.width = retw;
	}
	if (reth > XtHeight(w))
	{
	    request.height = reth;
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmGMDoLayout => %d %d\n",
		      request.width, request.height));

    /*
     * Now try to resize yourself - if parent permits ...
     */
    if (request.width != XtWidth(w) || request.height != XtHeight(w))
    {
	request.request_mode = CWWidth | CWHeight;
	request.request_mode |= CWBorderWidth;
	_XmMakeGeometryRequest(w, &request);
    }
}

Boolean
_XmGMOverlap(Widget w, Widget instigator,
	     Position x, Position y, Dimension width, Dimension height)
{
    Boolean overlap = False;
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmGMOverlap\n"));

    /*
     * this algorithm only works with rectangles
     */
    for (i = 0; i < MGR_NumChildren(w); i++)
    {
	Widget cw = MGR_Children(w)[i];

	if (cw == instigator)
	{
	    continue;
	}

	if (XtY(cw) + XtHeight(cw) <= y)	/* above us */
	{
	    continue;		/* then quit */
	}

	if (XtY(cw) >= y + height)	/* below us */
	{
	    continue;		/* then quit */
	}

	if (XtX(cw) + XtWidth(cw) <= x)		/* to the left */
	{
	    continue;		/* then quit */
	}

	if (XtX(cw) >= x + width)	/* to the right */
	{
	    continue;		/* then quit */
	}

	/*
	 * all your edges aren't above, below, left, or right of us;
	 * you must intersect/overlay/are-contained-by us
	 */
	overlap = True;
	break;
    }

    return overlap;
}

/*
 * this guy is the real generic BB geom manager
 */
#define Wants(x)        (desired->request_mode & x)
XtGeometryResult
_XmGMHandleGeometryManager(Widget w, Widget instigator,
			   XtWidgetGeometry *desired, XtWidgetGeometry *allowed,
			   Dimension margin_width, Dimension margin_height,
			   unsigned char resize_policy, Boolean allow_overlap)
{
    Position		x, y;
    Dimension		width, height;
    Boolean		adjust;
    XtWidgetGeometry	instgeo;

    DEBUGOUT(_LtDebug2(__FILE__, w, instigator,
		       "%s:_XmGMHandleGeometryManager(%d) desired (%s)\n",
		       __FILE__, __LINE__,
		       _LtDebugWidgetGeometry2String(desired)));

    /* Save instigator's geometry */
    instgeo.x = XtX(instigator);
    instgeo.y = XtY(instigator);
    instgeo.width = XtWidth(instigator);
    instgeo.height = XtHeight(instigator);
    instgeo.border_width = XtBorderWidth(instigator);

    if (Wants(CWX))
    {
	x = desired->x;
    }
    else
    {
	x = XtX(instigator);
    }
    if (Wants(CWY))
    {
	y = desired->y;
    }
    else
    {
	y = XtY(instigator);
    }
    if (Wants(CWWidth))
    {
	width = desired->width;
    }
    else
    {
	width = XtWidth(instigator);
    }
    if (Wants(CWHeight))
    {
	height = desired->height;
    }
    else
    {
	height = XtHeight(instigator);
    }

    if (Wants(CWHeight))
	XtHeight(instigator) = desired->height;
    if (Wants(CWWidth))
	XtWidth(instigator) = desired->width;
    if (Wants(CWX))
	XtX(instigator) = desired->x;
    if (Wants(CWY))
	XtY(instigator) = desired->y;

    /*
     * Danny experiment 5/12/1997
     */
    adjust = ! Wants(XtCWQueryOnly);
    _XmGMDoLayout(w, margin_width, margin_height, BB_ResizePolicy(w), adjust);

    /*
     * if we don't fit in the MarginWidth/MarginHeight bounding box, say no
     */
    allowed->request_mode = CWHeight;
    allowed->height = 1;
    if (x < margin_width || x + width + 2 * XtBorderWidth(instigator) > XtWidth(w) - margin_width)
    {
	DEBUGOUT(_LtDebug2(__FILE__, w, instigator,
		"\tx %d margin_width %d width %d XtWidth(w) %d\n",
		x, margin_width, width, XtWidth(w)));
	DEBUGOUT(_LtDebug2(__FILE__, w, instigator,
		"_XmGMHandleGeometryManager => NO at line %d\n", __LINE__));

	/* Reset instigator's geometry */
	XtX(instigator) = instgeo.x;
	XtY(instigator) = instgeo.y;
	XtWidth(instigator) = instgeo.width;
	XtHeight(instigator) = instgeo.height;
	XtBorderWidth(instigator) = instgeo.border_width;

	return XtGeometryNo;
    }

    if (y < margin_height || y + height + 2 * XtBorderWidth(instigator) > XtHeight(w) - margin_height)
    {
	DEBUGOUT(_LtDebug2(__FILE__, w, instigator,
		"_XmGMHandleGeometryManager => NO at line %d\n", __LINE__));

	/* Reset instigator's geometry */
	XtX(instigator) = instgeo.x;
	XtY(instigator) = instgeo.y;
	XtWidth(instigator) = instgeo.width;
	XtHeight(instigator) = instgeo.height;
	XtBorderWidth(instigator) = instgeo.border_width;

	return XtGeometryNo;
    }

    if (XtWidth(w) < width + 2 * (margin_width + XtBorderWidth(instigator)))
    {
	DEBUGOUT(_LtDebug2(__FILE__, w, instigator,
		"_XmGMHandleGeometryManager => NO at line %d\n", __LINE__));

	/* Reset instigator's geometry */
	XtX(instigator) = instgeo.x;
	XtY(instigator) = instgeo.y;
	XtWidth(instigator) = instgeo.width;
	XtHeight(instigator) = instgeo.height;
	XtBorderWidth(instigator) = instgeo.border_width;

	return XtGeometryNo;
    }

    if (XtHeight(w) < height + 2 * (margin_height + XtBorderWidth(instigator)))
    {
	DEBUGOUT(_LtDebug2(__FILE__, w, instigator,
		"_XmGMHandleGeometryManager => NO at line %d\n", __LINE__));

	/* Reset instigator's geometry */
	XtX(instigator) = instgeo.x;
	XtY(instigator) = instgeo.y;
	XtWidth(instigator) = instgeo.width;
	XtHeight(instigator) = instgeo.height;
	XtBorderWidth(instigator) = instgeo.border_width;

	return XtGeometryNo;
    }

    /* check for overlap */
    if (allow_overlap == False && _XmGMOverlap(w, instigator,
					       x, y, width, height))
    {
	DEBUGOUT(_LtDebug2(__FILE__, w, instigator,
		"_XmGMHandleGeometryManager => NO at line %d\n", __LINE__));
	DEBUGOUT(_LtDebug(__FILE__, w, "Overlap => NO\n"));


	/* Reset instigator's geometry */
	XtX(instigator) = instgeo.x;
	XtY(instigator) = instgeo.y;
	XtWidth(instigator) = instgeo.width;
	XtHeight(instigator) = instgeo.height;
	XtBorderWidth(instigator) = instgeo.border_width;

	return XtGeometryNo;
    }

    if (Wants(XtCWQueryOnly))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "QueryOnly => YES\n"));


	/* Reset instigator's geometry */
	XtX(instigator) = instgeo.x;
	XtY(instigator) = instgeo.y;
	XtWidth(instigator) = instgeo.width;
	XtHeight(instigator) = instgeo.height;
	XtBorderWidth(instigator) = instgeo.border_width;

	return XtGeometryYes;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "GeometryManager => "));

    if (Wants(CWWidth))
    {
	XtWidth(instigator) = desired->width;

	DEBUGOUT(_LtDebug0(__FILE__, w, "W %d ", desired->width));
    }
    if (Wants(CWHeight))
    {
	XtHeight(instigator) = desired->height;

	DEBUGOUT(_LtDebug0(__FILE__, w, "H %d ", desired->height));
    }
    if (Wants(CWX))
    {
	XtX(instigator) = desired->x;

	DEBUGOUT(_LtDebug0(__FILE__, w, "X %d ", desired->x));
    }
    if (Wants(CWY))
    {
	XtY(instigator) = desired->y;

	DEBUGOUT(_LtDebug0(__FILE__, w, "Y %d ", desired->y));
    }
    if (Wants(CWBorderWidth))
    {
	XtBorderWidth(instigator) = desired->border_width;

	DEBUGOUT(_LtDebug0(__FILE__, w, "BW %d ", desired->border_width));
    }
    DEBUGOUT(_LtDebug0(__FILE__, w, "=> YES\n"));

    return XtGeometryYes;
#undef  Wants
}
