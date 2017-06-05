/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Draw.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright © 1997, 1998, 1999, 2000, 2001, 2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Draw.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <Xm/XmP.h>
#include <Xm/DrawP.h>

#include <XmI/XmI.h>

#include <XmI/DebugUtil.h>


#define INDICATOR_GLYPH_MASK	0xf0

void
XmeClearBorder(Display *display,
	       Window w,
	       Position x, Position y,
	       Dimension width, Dimension height,
	       Dimension shadow_thick)
{
    _XmClearBorder(display, w, x, y, width, height, shadow_thick);
}

void

XmeDrawShadows(Display *display,
	       Drawable d,
	       GC top_gc,
	       GC bottom_gc,
	       Position x, Position y,
	       Dimension width, Dimension height,
	       Dimension shadow_thick,
	       unsigned int shadow_type)
{
    _XmDrawShadows(display, d, top_gc, bottom_gc, x, y, width, height,
		   shadow_thick, shadow_type);
}


void
XmeDrawDiamond(Display *display,
	       Drawable d,
	       GC top_gc,
	       GC bottom_gc,
	       GC select_gc,
	       Position x, Position y,
	       Dimension width, Dimension height,
	       Dimension shadow_thick,
	       Dimension fill)
{
    _XmDrawDiamond(display, d, top_gc, bottom_gc, select_gc,
		   x, y, width, height, shadow_thick, fill);
}


void
XmeDrawSeparator(Display *display,
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
    _XmDrawSeparator(display, d, top_gc, bottom_gc, separator_gc,
		     x, y, width, height, shadow_thick, margin,
		     orientation, separator_type);
}


void
XmeDrawHighlight(Display *display,
		 Drawable d,
		 GC gc,
		 Position x, Position y,
		 Dimension width, Dimension height,
		 Dimension highlight_thick)
{
    _XmDrawSimpleHighlight(display, d, gc, x, y, width, height,
			   highlight_thick);
}


void
XmeDrawArrow(Display *display,
	     Drawable d,
	     GC top_gc,
	     GC bot_gc,
	     GC cent_gc,
	     Position x, Position y,
	     Dimension width, Dimension height,
	     Dimension shadow_thick,
	     unsigned char direction)
{
    _XmDrawArrow(display, d, top_gc, bot_gc, cent_gc, x, y, width, height,
		 shadow_thick, direction);
}


void
XmeDrawCircle(Display *display,
	      Drawable d,
	      GC top_gc,
	      GC bot_gc,
	      GC cent_gc,
	      Position x, Position y,
	      Dimension width, Dimension height,
	      Dimension shadow_thick,
	      Dimension margin)
{
    XGCValues tgcv, bgcv, ngcv;

    if (shadow_thick)
    {
	if (width <= shadow_thick << 1 || height <= shadow_thick << 1)
	{
	    /* If too small for lines, just fill with the shadows */

	    XFillArc(display, d, top_gc,
		     x, y, width, height,
		     45 << 6, 180 << 6);
	    XFillArc(display, d, bot_gc,
		     x, y, width, height,
		     225 << 6, 180 << 6);
	    return;
	}

	/* Make sure top and bottom GC are set for the right line thickness */
	XGetGCValues(display, top_gc, GCLineWidth | GCCapStyle, &tgcv);
	if (tgcv.line_width != shadow_thick || tgcv.cap_style != CapButt)
	{
	    ngcv.line_width = shadow_thick;
	    ngcv.cap_style = CapButt;
	    XChangeGC(display, top_gc, GCLineWidth | GCCapStyle, &ngcv);
	}
	XGetGCValues(display, bot_gc, GCLineWidth | GCCapStyle, &bgcv);
	if (bgcv.line_width != shadow_thick || bgcv.cap_style != CapButt)
	{
	    ngcv.line_width = shadow_thick;
	    ngcv.cap_style = CapButt;
	    XChangeGC(display, bot_gc, GCLineWidth | GCCapStyle, &ngcv);
	}

	/* Draw the shadow arcs */
	XDrawArc(display, d, top_gc,
		 x + (shadow_thick >> 1), y + (shadow_thick >> 1),
		 width - shadow_thick, height - shadow_thick,
		 45 << 6, 180 << 6);
	XDrawArc(display, d, bot_gc,
		 x + (shadow_thick >> 1), y + (shadow_thick >> 1),
		 width - shadow_thick, height - shadow_thick,
		 225 << 6, 180 << 6);

	/* Restore GCs if necessary */
	if (tgcv.line_width != shadow_thick || tgcv.cap_style != CapButt)
	    XChangeGC(display, top_gc, GCLineWidth | GCCapStyle, &tgcv);
	if (bgcv.line_width != shadow_thick || bgcv.cap_style != CapButt)
	    XChangeGC(display, bot_gc, GCLineWidth | GCCapStyle, &bgcv);
    }

    /* Draw the center if requested and if it fits */
    margin += shadow_thick;
    if (cent_gc && width > margin << 1 && height > margin << 1)
    {
	XFillArc(display, d, cent_gc,
		 x + margin, y + margin,
		 width - (margin << 1), height - (margin << 1),
		 0, 360 << 6);
    }
}


void
XmeDrawIndicator(Display *display, 
		 Drawable d, 
		 GC gc, 
		 Position x, Position y, 
		 Dimension width, Dimension height,
		 Dimension margin, 
		 XtEnum type)
{
	XPoint xp[7];

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmeDrawIndicator(type %d)\n", type));

	if (width <= margin << 1 || height <= margin << 1)
		return;
	x += margin;
	y += margin;
	width -= margin << 1;
	height -= margin << 1;

	switch (type & INDICATOR_GLYPH_MASK) {
	case XmINDICATOR_CHECK_GLYPH:
		xp[0].x = x;
		xp[0].y = y + ((int)height * 12) / 25;
		xp[1].x = x + ((int)width * 19) / 100;
		xp[1].y = y + ((int)height * 29) / 100;
		xp[2].x = x + ((int)width * 9) / 20;
		xp[2].y = xp[5].y = y + ((int)height * 11) / 20;
		xp[3].x = x + width;
		xp[3].y = y - 1;
		xp[4].x = x + width;
		xp[4].y = y + height / 10;
		xp[5].x = x + ((int)width * 17) / 25 + 1;
		xp[6].x = x + ((int)width * 13) / 25;
		xp[6].y = y + height;
		XFillPolygon(display, d, gc, xp, 7, Nonconvex, CoordModeOrigin);
		break;

	case XmINDICATOR_CROSS_GLYPH:
		xp[0].x = x;
		xp[0].y = y;
		xp[1].x = xp[3].y = 2;
		xp[1].y = xp[3].x = xp[4].y = 0;
		xp[2].x = width - 2;
		xp[2].y = height - 2;
		xp[4].x = -1;
		xp[5].x = 1 - width;
		xp[5].y = 1 - height;
		XFillPolygon(display, d, gc, xp, 6, Convex, CoordModePrevious);
		xp[0].y = y + height;
		xp[1].x = xp[3].y = xp[4].x = 0;
		xp[1].y = -2;
		xp[2].y = 2 - height;
		xp[3].x = 2;
		xp[4].y = 1;
		xp[5].y = height - 1;
		XFillPolygon(display, d, gc, xp, 6, Convex, CoordModePrevious);

	default:
		/* The docs say don't do anything here */
	    	break;
	}
}


void 
XmeDrawPolygonShadow(Display *display,
                     Drawable drawable,
                     GC top_gc,
                     GC bottom_gc,
                     XPoint *points,
                     int point_count,
                     Dimension shadow_thickness,
                     unsigned char shadow_type)
{

  /* That function in lib/Xm/Shadow.c may be only a stub yet... */
  _XmDrawPolygonShadow(display, drawable,
                       top_gc, bottom_gc,
                       points, point_count,
                       shadow_thickness, shadow_type);
}
