/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Shadow.c,v 1.3 2005/04/04 07:22:48 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team 
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Shadow.c,v 1.3 2005/04/04 07:22:48 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/ManagerP.h>

#include <XmI/DebugUtil.h>


extern void
_XmClearBorder(Display *display,
	       Window win,
	       Position x, Position y,
	       Dimension width, Dimension height,
	       Dimension shadow_thick)
{
    if (!shadow_thick)
	return;
    if (shadow_thick >= (width + 1) >> 1 || shadow_thick >= (height + 1) >> 1)
    {
	XClearArea(display, win, x, y, width, height, False);
	return;
    }

    XClearArea(display, win,
	       x, y,
	       width - shadow_thick, shadow_thick,
	       False);

    XClearArea(display, win,
	       x + width - shadow_thick, y,
	       shadow_thick, height - shadow_thick,
	       False);

    XClearArea(display, win,
	       x + shadow_thick, y + height - shadow_thick,
	       width - shadow_thick, shadow_thick,
	       False);

    XClearArea(display, win,
	       x, y + shadow_thick,
	       shadow_thick, height - shadow_thick,
	       False);
}


#if XmVERSION > 1
extern void 
_XmDrawPolygonShadow(Display *display,
                     Drawable drawable,
                     GC top_gc,
                     GC bottom_gc,
                     XPoint *points,
                     int point_count,
                     Dimension shadow_thickness,
                     unsigned char shadow_type)
{
  /* FIXME */
  _XmWarning(NULL, "_XmDrawPolygonShadow()/XmeDrawPolygonShadow(): not yet implemented!");
}
#endif


extern void
_XmDrawShadows(Display *display,
	       Drawable d,
	       GC top_gc,
	       GC bottom_gc,
	       Position x, Position y,
	       Dimension width, Dimension height,
	       Dimension shadow_thick,
	       unsigned int shadow_type)
{
    int i, il, ir, it, ib;
    GC tgc;
    XPoint points[4];

    /* Trim the shadow down so it doesn't go past the middle.
     * Etched shadows get split in half, to be done as two shadows.
     */

    if (shadow_type == XmSHADOW_ETCHED_IN ||
	shadow_type == XmSHADOW_ETCHED_OUT)
	shadow_thick >>= 1;
    if (shadow_thick > width >> 1)
	shadow_thick = width >> 1;
    if (shadow_thick > height >> 1)
	shadow_thick = height >> 1;
    if (!shadow_thick)
	return;

    il = x + shadow_thick;
    it = y + shadow_thick;
    ir = x + width - shadow_thick;
    ib = y + height - shadow_thick;

    /* Draw trapezoids for the shadow segments */

    switch (shadow_type)
    {
    case XmSHADOW_IN:
	tgc = top_gc;
	top_gc = bottom_gc;
	bottom_gc = tgc;
	/* FALLTHROUGH */

    default:
    case XmSHADOW_OUT:
	points[0].x = il;
	points[0].y = it;
	points[1].x = points[1].y = points[3].y = -shadow_thick;
	points[2].x = 0;
	points[2].y = height;
	points[3].x = shadow_thick;
	XFillPolygon(display, d, top_gc, points, 4, Convex,
		     CoordModePrevious);

	points[2].x = width;
	points[2].y = 0;
	points[3].x = -shadow_thick;
	points[3].y = shadow_thick;
	XFillPolygon(display, d, top_gc, points, 4, Convex,
		     CoordModePrevious);

	points[0].y = ib;
	points[1].y = shadow_thick;
	points[3].y = -shadow_thick;
	XFillPolygon(display, d, bottom_gc, points, 4, Convex,
		     CoordModePrevious);

	points[0].x = ir;
	points[0].y = it;
	points[1].x = shadow_thick;
	points[1].y = -shadow_thick;
	points[2].x = 0;
	points[2].y = height;
	XFillPolygon(display, d, bottom_gc, points, 4, Convex,
		     CoordModePrevious);
	break;

    case XmSHADOW_ETCHED_IN:
	tgc = top_gc;
	top_gc = bottom_gc;
	bottom_gc = tgc;
	/* FALLTHROUGH */

    case XmSHADOW_ETCHED_OUT:
	/* These are different on the corners - the bottom-right dominates.
	 * And, of course, we draw twice - once for each sub-shadow.
	 */

	for (i = 0; i < 2; i++)
	{
	    points[0].x = il;
	    points[0].y = it;
	    points[1].x = points[1].y = points[3].y = -shadow_thick;
	    points[2].x = 0;
	    points[2].y = height - 1;
	    points[3].x = shadow_thick;
	    XFillPolygon(display, d, top_gc, points, 4, Convex,
			 CoordModePrevious);

	    points[2].x = width - 1;
	    points[2].y = 0;
	    points[3].x = -shadow_thick;
	    points[3].y = shadow_thick;
	    XFillPolygon(display, d, top_gc, points, 4, Convex,
			 CoordModePrevious);

	    points[0].x = il - 1;
	    points[0].y = ib;
	    points[1].y = shadow_thick;
	    points[2].x = width + 1;
	    points[3].y = -shadow_thick;
	    XFillPolygon(display, d, bottom_gc, points, 4, Convex,
			 CoordModePrevious);

	    points[0].x = ir;
	    points[0].y = it - 1;
	    points[1].x = shadow_thick;
	    points[1].y = -shadow_thick;
	    points[2].x = 0;
	    points[2].y = height + 1;
	    XFillPolygon(display, d, bottom_gc, points, 4, Convex,
			 CoordModePrevious);

	    /* Shrink the rectangle to draw again */

	    if (!i)
	    {
		x += shadow_thick;
		y += shadow_thick;
		width -= shadow_thick << 1;
		height -= shadow_thick << 1;
		if (shadow_thick > width >> 1)
		    shadow_thick = width >> 1;
		if (shadow_thick > height >> 1)
		    shadow_thick = height >> 1;
		if (!shadow_thick)
		    return;

		il += shadow_thick;
		it += shadow_thick;
		ir -= shadow_thick;
		ib -= shadow_thick;
		tgc = top_gc;
		top_gc = bottom_gc;
		bottom_gc = tgc;
	    }
	}
    }
}


extern void
_XmDrawShadow(Display *display,
	      Drawable d,
	      GC top_gc,
	      GC bottom_gc,
	      Dimension shadow_thick,
	      Position x, Position y,
	      Dimension width, Dimension height)
{
    _XmDrawShadows(display, d,
		   top_gc, bottom_gc,
		   x, y,
		   width, height,
		   shadow_thick, XmSHADOW_OUT);
}

/* 1.2.5 and above (including 2.X) can draw arrows with shadow_thick of
 * 0, 1, or 2.  1.2.4 and below are limited to 0 and 2.  So adjust to taste:
 */

#define DA_SHADOW_1	True
/* #define DA_SHADOW_1	XmVERSION > 1 */


extern void
_XmDrawArrow(Display *display,
	     Drawable d,
	     GC top_gc,
	     GC bottom_gc,
	     GC fill_gc,
	     Position x, Position y,
	     Dimension width, Dimension height,
	     Dimension shadow_thick,
	     unsigned char direction)
{
    int i, size, odd, dr, txw;
    int num_top, num_bottom, num_fill;
    GC tgc;
    XRectangle *top;
    XRectangle *bottom;
    XRectangle *fill;
    /* The define below is a heuristic.  Adjust as needed to avoid memory
     * fragmentation.  Otherwise change the code to use a static pointer to
     * reallocated memory as needed but would static pointers be thread safe?
     */
#define XM_DRAW_ARROW_INTERNAL_SIZE 60
    XRectangle _internal_rects[XM_DRAW_ARROW_INTERNAL_SIZE];

    /* Determine size and offsets */

    if (width > height)
    {
        size = height;
        x += (width - size) >> 1;
    }
    else
    {
	size = width;
        y += (height - size) >> 1;
    }

    /* Shrink the arrow a pixel around the edges.
     * Don't try to draw arrows that are too small.
     */

    size -= 2;
    if (size <= 0)
	return;
    odd = size & 1;
    x++;
    y++;

    /* Zero-thickness shadows are really just 2-thick drawn in the fill GC */

    if (!shadow_thick)
    {
	if (!(top_gc = bottom_gc = fill_gc))
	    return;
    }

    /* Find the number of rectangles to draw.
     * Down- and right- arrows look a bit different, and so is their count.
     */

    dr = direction & 1;
    num_bottom = ((size + 2) >> 1) - (size < 3);
    num_top = (size + 2 - dr) >> 1;
    if (dr && ((size - 1) & ~2) == 1)
    {
	num_top += 3 - size;
	if (size == 4)
	    num_bottom--;
    }

    /* The fill is bigger for 1-thick shadows */

    if (fill_gc)
    {
#if DA_SHADOW_1
	if (shadow_thick == 1)
	{
	    num_fill = (size - 1) >> 1;
	    if (dr && !odd && num_fill > 1)
		num_fill--;
	}
	else
#endif
	    num_fill = (size - 5) >> 1;
	if (num_fill < 0)
	    num_fill = 0;
    }
    else
	num_fill = 0;

    /* Allocate space if arrow is too big for internal array */

    top = num_top + num_bottom + num_fill > XM_DRAW_ARROW_INTERNAL_SIZE
	? (XRectangle *) XtMalloc(sizeof(XRectangle) *
				  (num_top + num_bottom + num_fill))
	: _internal_rects;
    bottom = top + num_top;
    fill = bottom + num_bottom;

    /* Calculate the rectangles for an up-arrow at 0,0.
     * First, the top shadow.
     */

    if (num_top)
    {
	top[0].x = 0;
	top[0].y = size - 2;
	top[0].width = 1;
	top[0].height = 2 - dr;
	if (num_top > 1)
	{
	    for (i = 1; i < num_top; i++)
	    {
		top[i].x = i;
		top[i].y = top[i-1].y - 2;
		top[i].width = 1;
		top[i].height = 4;
	    }
	    top[1].height = 3;
	    top[num_top-1].height = 1 + odd + (dr | odd);
	}
	top[num_top-1].y += 2 - odd;

	/* Special cases for tiny arrows */

	if ((size & ~2) == 1)
	    top[size>>1].height = 1 + (size >> 1);
	else if (dr && ((size - 1) & ~2) == 1)
	{
	    top[0].y = size >> 1;
	    top[num_top-1].x = 0;
	    top[num_top-1].width = 2;
	    top[num_top-1].height = 1;
	}
    }

    /* Bottom shadow rectangles */

    if (num_bottom)
    {
	bottom[0].y = size - 1;
	bottom[0].height = 1;
	if (num_bottom == 1)
	{
	    bottom[0].x = 1;
	    bottom[0].width = 1;
	}
	else
	{
	    bottom[0].x = 1 - dr;
	    bottom[0].width = 1 + dr;
	    bottom[1].x = 2;
	    bottom[1].y = size - 2;
	    bottom[1].width = size - 2;
	    bottom[1].height = 2;
	    if (num_bottom > 2)
	    {
		bottom[2].x = size - 2;
		bottom[2].y = size - 4;
		bottom[2].width = 1;
		bottom[2].height = (dr << 1) + 2;
		for (i = 3; i < num_bottom; i++)
		{
		    bottom[i].x = bottom[i-1].x - 1;
		    bottom[i].y = bottom[i-1].y - 2;
		    bottom[i].width = 1;
		    bottom[i].height = 4;
		}
		bottom[2].height = 2;
		if (!odd)
		{
		    bottom[num_bottom-1].y += dr + 1;
		    bottom[num_bottom-1].height -= dr + 1;
		}
	    }
	}
    }

    /* Fill (center arrow) rectangles.
     * The offset changes depending on the shadow thickness.
     */

    if (num_fill)
    {
#if DA_SHADOW_1
	if (shadow_thick == 1)
	{
	    fill[0].x = 1;
	    fill[0].y = size - 3;
	    fill[0].width = size - 2;
	}
	else
#endif
	{
	    fill[0].x = 3;
	    fill[0].y = size - 4;
	    fill[0].width = size - 6;
	}
	fill[0].height = 2;
	for (i = 1; i < num_fill; i++)
	{
	    fill[i].x = fill[i-1].x + 1;
	    fill[i].y = fill[i-1].y - 2;
	    fill[i].width = fill[i-1].width - 2;
	    fill[i].height = 2;
	}
#if DA_SHADOW_1
	if (odd && shadow_thick == 1)
	{
	    fill[num_fill-1].y++;
	    fill[num_fill-1].height = 1;
	}
#endif
    }

    /* Offset and/or rotate the arrow rectangles.
     * Switch the shadows on down- and right- arrows.
     */

    if (dr)
    {
	x += size + ((width ^ size) & 1);
	y += size + ((height ^ size) & 1);
	tgc = top_gc;
	top_gc = bottom_gc;
	bottom_gc = tgc;
    }
    i = num_top + num_bottom + num_fill;
    switch (direction)
    {
    case XmARROW_UP:
	while (i--)
	{
	    top[i].x += x;
	    top[i].y += y;
	}
	break;
    case XmARROW_DOWN:
	while (i--)
	{
	    top[i].x = x - top[i].x - top[i].width;
	    top[i].y = y - top[i].y - top[i].height;
	}
	break;
    case XmARROW_LEFT:
	while (i--)
	{
	    txw = top[i].x;
	    top[i].x = top[i].y + x;
	    top[i].y = txw + y;
	    txw = top[i].width;
	    top[i].width = top[i].height;
	    top[i].height = txw;
	}
	break;
    case XmARROW_RIGHT:
	while (i--)
	{
	    txw = top[i].x;
	    top[i].x = x - top[i].y - top[i].height;
	    top[i].y = y - txw - top[i].width;
	    txw = top[i].width;
	    top[i].width = top[i].height;
	    top[i].height = txw;
	}
    }

    /* Fill in the rectangles */

    if (num_top)
	XFillRectangles(display, d, top_gc, top, num_top);
    if (num_bottom)
	XFillRectangles(display, d, bottom_gc, bottom, num_bottom);
    if (num_fill)
	XFillRectangles(display, d, fill_gc, fill, num_fill);

    /* Free allocated memory */

    if (top != _internal_rects)
        XtFree((char *) top);
}


extern void
_XmDrawDiamond(Display *display,
	       Drawable d,
	       GC top_gc,
	       GC bottom_gc,
	       GC select_gc,
	       Position x, Position y,
	       Dimension width, Dimension height,
	       Dimension shadow_thick,
	       Dimension fill)
{
    int r, b, hm, vm;
    XPoint points[4];

    /* The height argument is unused, and is assumed to be equal to width. */
    /* Don't draw even-width diamonds: make them one pixel smaller. */

    width = (width - 1) | 1;
    r = x + width;
    b = y + width;
    hm = x + (width >> 1);
    vm = y + (width >> 1);

    /* Special case: tiny diamonds are just the top color */

    if (width <= 4)
    {
	points[0].x = x;
	points[0].y = points[2].y = vm;
	points[1].x = points[3].x = hm;
	points[1].y = y - 1;
	points[2].x = x + width;
	points[3].y = y + width;
	XFillPolygon(display, d, top_gc, points, 4, Convex, CoordModeOrigin);
	return;

    }

    /* 2.0 can have shadows of 0, 1 (really 2), or 2 (really 3) pixels;
     * 1.2 ignores this and always has a 3-pixel shadow.
     * They also have vastly different ideas about what constitutes fill.
     * To 1.2, it's a boolean, with False (0) meaning the center is a pixel
     * away from the shadows.  To 2.0, it's a dimension of how many pixels
     * between center and shadow, with 0 meaning no pixels away.
     */

#if XmVERSION == 1
#define DD_SHADOW_THICK	3
#define	DD_FILL		(!fill)
#else
#define DD_SHADOW_THICK	shadow_thick
#define	DD_FILL		fill
    shadow_thick = width <= 6 || shadow_thick >= 2 ? 3 : shadow_thick << 1;
#endif

    /* Draw the center, if there's a GC to draw it with.
     * The center is smaller by fill pixels, but only for 3-thick shadows.
     */

    if (select_gc)
    {
	points[0].x = x + DD_SHADOW_THICK;
	points[0].y = points[2].y = vm;
	points[1].x = points[3].x = hm;
	points[1].y = y + (DD_SHADOW_THICK - 1);
	points[2].x = x + width - DD_SHADOW_THICK;
	points[3].y = y + width - DD_SHADOW_THICK;
	if (DD_FILL && DD_SHADOW_THICK == 3)
	{
	    points[0].x += DD_FILL;
	    points[1].y += DD_FILL;
	    points[2].x -= DD_FILL;
	    points[3].y -= DD_FILL;
	}
	if (points[0].x < points[2].x)
	    XFillPolygon(display, d, select_gc, points, 4, Convex,
			 CoordModeOrigin);
    }

#if XmVERSION > 1
    if (!shadow_thick)
	return;
#endif

    /* Drawing the shadows isn't as simple as it seems;
     * you have to take into account X's wierd rounding.
     * Still, this doesn't hold a candle to the elegant ugliness
     * of the old polygon-based arrow code.
     */
    /* This is correct to the pixel, except that the center pixel of a
     * 5-pixel-wide diamond should be the top color, not the bottom.
     * The last pixel is always the hardest.
     */

    points[0].x = x - 1;
    points[0].y = points[3].y = vm + 1;
    points[1].x = points[2].x = hm;
    points[1].y = y;
    points[2].y = y + DD_SHADOW_THICK;
    points[3].x = x + (DD_SHADOW_THICK - 1);
    XFillPolygon(display, d, top_gc, points, 4, Convex, CoordModeOrigin);

    points[0].x = points[3].x = hm;
    points[0].y = y - 1;
    points[1].x = r;
    points[1].y = points[2].y = vm;
    points[2].x = r - DD_SHADOW_THICK;
    points[3].y = y + (DD_SHADOW_THICK - 1);
    XFillPolygon(display, d, top_gc, points, 4, Convex, CoordModeOrigin);

    points[0].x = r;
    points[0].y = points[3].y = vm;
    points[1].x = points[2].x = hm + 1;
    points[1].y = b - 1;
    points[2].y = b - (DD_SHADOW_THICK + 1);
    points[3].x = r - DD_SHADOW_THICK;
    XFillPolygon(display, d, bottom_gc, points, 4, Convex, CoordModeOrigin);

    points[0].x = points[3].x = hm + 1;
    points[0].y = b;
    points[1].x = x + 1;
    points[1].y = vm + 1;
    points[2].x = x + (DD_SHADOW_THICK - 1);
    points[2].y = vm;
    points[3].y = b - DD_SHADOW_THICK;
    XFillPolygon(display, d, bottom_gc, points, 4, Convex, CoordModeOrigin);

}


extern void
_XmDrawSeparator(Display *display,
		 Drawable d,
		 GC top_gc,
		 GC bottom_gc,
		 GC separator_gc,
		 Position x, Position y,
		 Dimension width, Dimension height,
		 Dimension shadow_thick,
		 Dimension margin,
		 unsigned char orientation,
		 unsigned char separator_type)
{
    int i, j, c, length, om;

    if (!shadow_thick && separator_type > XmDOUBLE_DASHED_LINE)
	return;
    shadow_thick >>= 1;
    if (orientation == XmHORIZONTAL)
    {
	c = y + (height >> 1);
	if ((length = width - (margin << 1)) > 0)
	{
	    om = (margin += x) + length;
	    switch (separator_type)
	    {
	    case XmNO_LINE:
		break;

	    case XmSINGLE_LINE:
	    case XmSINGLE_DASHED_LINE:
		/* Treat dashed and undashed lines the same here.
		 * The caller is expected to set the dashes in the GC,
		 * as well as a line width of 1.
		 */

		XDrawLine(display, d, separator_gc, margin, c, om, c);
		break;

	    case XmDOUBLE_LINE:
	    case XmDOUBLE_DASHED_LINE:
		/* Ditto */

		XDrawLine(display, d, separator_gc, margin, c - 1, om, c - 1);
		XDrawLine(display, d, separator_gc, margin, c + 1, om, c + 1);
		break;

	    default:
		separator_type = XmSHADOW_ETCHED_OUT;
		/* FALLTHROUGH */

	    case XmSHADOW_ETCHED_IN:
	    case XmSHADOW_ETCHED_OUT:
		/* Etched lines are shadows drawn around nothing,
		 * unless they're one pixel thick (a single line)
		 * or two (top and bottom lines, one pixel different
		 * from the shadow case - argh).
		 */

		switch (shadow_thick)
		{
		case 0:
		    XFillRectangle(display, d,
				   separator_type == XmSHADOW_ETCHED_IN
				   ? bottom_gc : top_gc, margin, c, length, 1);
		    break;
		case 1:
		    XFillRectangle(display, d, top_gc, margin,
				   c - (separator_type - XmSHADOW_ETCHED_IN),
				   length, 1);
		    XFillRectangle(display, d, bottom_gc, margin, c - 1 +
				   (separator_type - XmSHADOW_ETCHED_IN),
				   length, 1);
		    break;
		default:
		    _XmDrawShadows(display, d, top_gc, bottom_gc, margin,
				   c - shadow_thick, length,
				   shadow_thick << 1, shadow_thick,
				   separator_type + (XmSHADOW_IN -
						     XmSHADOW_ETCHED_IN));
		}
		break;

	    case XmSHADOW_ETCHED_IN_DASH:
	    case XmSHADOW_ETCHED_OUT_DASH:
		/* Dashed etched lines are many such shadows,
		 * with similar implications for "thin" shadows.
		 */

		switch (shadow_thick)
		{
		case 0:
		    if (separator_type == XmSHADOW_ETCHED_IN_DASH)
			top_gc = bottom_gc;
		    for (i = margin; i < om; i += 2)
			XDrawPoint(display, d, top_gc, i, c);
		    break;
		case 1:
		    j = 6;
		    for (i = margin; i < om; i += 12)
		    {
			if (6 > om - i)
			    j = om - i;
			XFillRectangle(display, d, top_gc, i, c -
				       (separator_type -
					XmSHADOW_ETCHED_IN_DASH), j, 1);
			XFillRectangle(display, d, bottom_gc, i, c - 1 +
				       (separator_type -
					XmSHADOW_ETCHED_IN_DASH), j, 1);
		    }
		    break;
		default:
		    j =  shadow_thick ? 6 * shadow_thick : 1;
		    for (i = margin; i < om; i += j << 1)
		    {
			if (j > om - i)
			    j = om - i;
			_XmDrawShadows(display, d, top_gc, bottom_gc, i,
				       c - shadow_thick, j, shadow_thick << 1,
				       shadow_thick, separator_type +
				      (XmSHADOW_IN - XmSHADOW_ETCHED_IN_DASH));
		    }
		}
	    }
	}
    }
    else
    {
	/* Everything the same here but sideways */

	c = x + (width >> 1);
	if ((length = height - (margin << 1)) > 0)
	{
	    om = (margin += y) + length;
	    switch (separator_type)
	    {
	    case XmNO_LINE:
		break;
	    case XmSINGLE_LINE:
	    case XmSINGLE_DASHED_LINE:
		XDrawLine(display, d, separator_gc, c, margin, c, om);
		break;
	    case XmDOUBLE_LINE:
	    case XmDOUBLE_DASHED_LINE:
		XDrawLine(display, d, separator_gc, c - 1, margin, c - 1, om);
		XDrawLine(display, d, separator_gc, c + 1, margin, c + 1, om);
		break;
	    default:
		separator_type = XmSHADOW_ETCHED_OUT;
		/* FALLTHROUGH */
	    case XmSHADOW_ETCHED_IN:
	    case XmSHADOW_ETCHED_OUT:
		switch (shadow_thick)
		{
		case 0:
		    XFillRectangle(display, d,
				   separator_type == XmSHADOW_ETCHED_IN
				   ? bottom_gc : top_gc, c, margin, 1, length);
		    break;
		case 1:
		    XFillRectangle(display, d, top_gc,
				   c - (separator_type - XmSHADOW_ETCHED_IN),
				   margin, 1, length);
		    XFillRectangle(display, d, bottom_gc, c - 1 +
				   (separator_type - XmSHADOW_ETCHED_IN),
				   margin, 1, length);
		    break;
		default:
		    _XmDrawShadows(display, d, top_gc, bottom_gc,
				   c - shadow_thick, margin, shadow_thick << 1,
				   length, shadow_thick, separator_type +
				   (XmSHADOW_IN - XmSHADOW_ETCHED_IN));
		}
		break;
	    case XmSHADOW_ETCHED_IN_DASH:
	    case XmSHADOW_ETCHED_OUT_DASH:
		switch (shadow_thick)
		{
		case 0:
		    if (separator_type == XmSHADOW_ETCHED_IN_DASH)
			top_gc = bottom_gc;
		    for (i = margin; i < om; i += 2)
			XDrawPoint(display, d, top_gc, c, i);
		    break;
		case 1:
		    j = 6;
		    for (i = margin; i < om; i += 12)
		    {
			if (6 > om - i)
			    j = om - i;
			XFillRectangle(display, d, top_gc, c -
				       (separator_type -
					XmSHADOW_ETCHED_IN_DASH), i, 1, j);
			XFillRectangle(display, d, bottom_gc, c - 1 +
				       (separator_type -
					XmSHADOW_ETCHED_IN_DASH), i, 1, j);
		    }
		    break;
		default:
		    j =  6 * shadow_thick;
		    for (i = margin; i < om; i += j << 1)
		    {
			if (j > om - i)
			    j = om - i;
			_XmDrawShadows(display, d, top_gc, bottom_gc,
				       c - shadow_thick, i, shadow_thick << 1,
				       j, shadow_thick, separator_type +
				      (XmSHADOW_IN - XmSHADOW_ETCHED_IN_DASH));
		    }
		}
	    }
	}
    }
}


extern void
_XmDrawSimpleHighlight(Display *display,
		       Drawable d,
		       GC gc,
		       Position x, Position y,
		       Dimension width, Dimension height,
		       Dimension highlight_thick)
{
    XRectangle rects[4];

    DEBUGOUT(_LtDebug(__FILE__, XtWindowToWidget(display, d),
			    "_XmDrawSimpleHighlight(gc %p x %d y %d width %d height %d hlt %d)\n",
			    gc, x, y, width, height, highlight_thick));

    if (highlight_thick >= (width + 1) >> 1 ||
	highlight_thick >= (height + 1) >> 1)
    {
	XFillRectangle(display, d, gc, x, y, width, height);
	return;
    }

    rects[0].x = rects[3].x = x;
    rects[0].y = rects[1].y = y;
    rects[0].width = rects[2].width = width - highlight_thick;
    rects[0].height = rects[1].width = rects[2].height = rects[3].width =
	highlight_thick;
    rects[1].x = x + width - highlight_thick;
    rects[1].height = rects[3].height = height - highlight_thick;
    rects[2].x = x + highlight_thick;
    rects[2].y = y + height - highlight_thick;
    rects[3].y = y + highlight_thick;

    XFillRectangles(display, d, gc, rects, 4);
}


extern void
_XmDrawHighlight(Display *display,
		 Drawable d,
		 GC gc,
		 Position x, Position y,
		 Dimension width, Dimension height,
		 Dimension highlight_thick,
		 int line_style)
{
    Dimension ht2l, ht2h;
    XGCValues gcv;
    XSegment segs[4];
    static char dash_list[] = {4, 4};

    if (!highlight_thick)
	return;

    /* This function messes with the GC,
     * so handle with care with using Xt cached GCs.
     */

    gcv.line_width = highlight_thick;
    gcv.line_style = line_style;
    gcv.join_style = JoinMiter;
    XChangeGC(display, gc, GCLineWidth | GCLineStyle | GCJoinStyle, &gcv);
    if (line_style == LineOnOffDash || line_style == LineDoubleDash)
	XSetDashes(display, gc, 0, dash_list, 2);

    ht2h = highlight_thick - (ht2l = (highlight_thick >> 1));

    segs[0].x1 = x;
    segs[0].y1 = segs[0].y2 = y + ht2l;
    segs[0].x2 = x + width - highlight_thick;

    segs[1].x1 = segs[1].x2 = x + width - ht2h;
    segs[1].y1 = y;
    segs[1].y2 = y + height;

    segs[2].x1 = x;
    segs[2].y1 = segs[2].y2 = y + height - ht2h;
    segs[2].x2 = x + width;

    segs[3].x1 = segs[3].x2 = x + ht2l;
    segs[3].y1 = y;
    segs[3].y2 = y + height - ht2l;

    XDrawSegments(display, d, gc, segs, 4);
}

/*
 * Undocumented but present since the early days.
 * XmDiary is an example of their use.
 *
 * Looks like they just call the methods ...
 */
extern void
_XmHighlightBorder(Widget w)
{
    if (XmIsGadget(w))
    {
	(*(xmGadgetClassRec.gadget_class.border_highlight)) (w);
    }
    else if (XmIsPrimitive(w))
    {
	(*(xmPrimitiveClassRec.primitive_class.border_highlight)) (w);
    }
    else
    {
	_XmWarning(w, "_XmHighlightBorder: called with non-Motif object");
    }
}


extern void
_XmUnhighlightBorder(Widget w)
{
    if (XmIsGadget(w))
    {
	(*(xmGadgetClassRec.gadget_class.border_unhighlight)) (w);
    }
    else if (XmIsPrimitive(w))
    {
	(*(xmPrimitiveClassRec.primitive_class.border_unhighlight)) (w);
    }
    else
    {
	_XmWarning(w, "_XmUnhighlightBorder: called with non-Motif object");
    }
}


/* Backward compatibility for ToggleButton.  They're not used now, and are
 * in fact insufficient for 2.0's detailShadowThickness.
 */

extern void
_XmDrawSquareButton(Widget w,
		    int x, int y, int size,
		    GC top_gc, GC bottom_gc, GC select_gc,
		    Boolean fill)
{
    int delta;

    _XmDrawShadows(XtDisplay(w), XtWindow(w), top_gc, bottom_gc,
		   x, y, size, size, Xm3D_ENHANCE_PIXEL, XmSHADOW_OUT);

    delta = Xm3D_ENHANCE_PIXEL + !fill;
    if (select_gc && size > delta << 1)
	XFillRectangle(XtDisplay(w), XtWindow(w), select_gc,
		       x + delta, y + delta,
		       size - (delta << 1), size - (delta << 1));
}


extern void
_XmDrawDiamondButton(Widget w,
		     int x, int y, int size,
		     GC top_gc, GC bottom_gc, GC select_gc,
		     Boolean fill)
{
    _XmDrawDiamond(XtDisplay(w), XtWindow(w), top_gc, bottom_gc, select_gc,
		   x, y, size, size, Xm3D_ENHANCE_PIXEL,
#if XmVERSION > 1
		   !fill);
#else
		   fill);
#endif
}
