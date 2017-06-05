/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/
/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 ****************************************************************************/
/***********************************************************************
 * The rest of it is all my fault -- MLM
 * mwm - "LessTif Window Manager"
 ***********************************************************************/

#include <LTconfig.h>

#include <string.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include "mwm.h"

#include <X11/extensions/shape.h>

/*
 * button dimensions
 */
static int lbut_styles[2] =
{
    22,
    55
};
static int rbut_style_min[2] =
{
    22,
    22
};
static int rbut_style_max[2] =
{
    55,
    55
};


/*
 * change some window attributes
 */
static void
set_value_attributes(ScreenInfo *scr, MwmWindow *t, unsigned long *valuemask,
		     XSetWindowAttributes *attributes, ComponentInfo *comp)
{

    *valuemask = CWBorderPixel;
    if (scr->mwm_highlight == t)
    {
	attributes->border_pixel = comp->active_background;
	if (comp->active_background_pixmap != XmUNSPECIFIED_PIXMAP)
	{
	    attributes->background_pixmap = comp->active_background_pixmap;
	    *valuemask |= CWBackPixmap;
	}
	else
	{
	    attributes->background_pixel = comp->active_background;
	    *valuemask |= CWBackPixel;
	}
    }
    else
    {
	attributes->border_pixel = comp->background;
	if (comp->background_pixmap != XmUNSPECIFIED_PIXMAP)
	{
	    attributes->background_pixmap = comp->background_pixmap;
	    *valuemask |= CWBackPixmap;
	}
	else
	{
	    attributes->background_pixel = comp->background;
	    *valuemask |= CWBackPixel;
	}
    }
}

static void
draw_corners(MwmWindow *t, int i, GC hor, GC vert)
{
    XSegment seg[2];
    int n = 0;

    switch (i)
    {
    case 0:
	seg[0].x1 = t->boundary_width - 1;
	seg[0].x2 = t->corner_width;
	seg[0].y1 = t->boundary_width - 1;
	seg[0].y2 = t->boundary_width - 1;
	n = 1;
	break;
    case 1:
	seg[0].x1 = 0;
	seg[0].x2 = t->corner_width - t->boundary_width;
	seg[0].y1 = t->boundary_width - 1;
	seg[0].y2 = t->boundary_width - 1;
	n = 1;
	break;
    case 2:
	seg[0].x1 = t->boundary_width - 1;
	seg[0].x2 = t->corner_width - 2;
	seg[0].y1 = t->corner_width - t->boundary_width + t->bw;
	seg[0].y2 = t->corner_width - t->boundary_width + t->bw;
	n = 1;
	break;
    case 3:
	seg[0].x1 = 0;
	seg[0].x2 = t->corner_width - t->boundary_width;
	seg[0].y1 = t->corner_width - t->boundary_width + t->bw;
	seg[0].y2 = t->corner_width - t->boundary_width + t->bw;
	n = 1;
	break;
    }
    XDrawSegments(dpy, t->corners[i], hor, seg, n);
    switch (i)
    {
    case 0:
	seg[0].y1 = t->boundary_width - 1;
	seg[0].y2 = t->corner_width;
	seg[0].x1 = t->boundary_width - 1;
	seg[0].x2 = t->boundary_width - 1;
	n = 1;
	break;
    case 1:
	seg[0].y1 = t->boundary_width - 1;
	seg[0].y2 = t->corner_width - 2;
	seg[0].x1 = t->corner_width - t->boundary_width;
	seg[0].x2 = t->corner_width - t->boundary_width;
	n = 1;
	break;
    case 2:
	seg[0].y1 = 0;
	seg[0].y2 = t->corner_width - t->boundary_width;
	seg[0].x1 = t->boundary_width - 1;
	seg[0].x2 = t->boundary_width - 1;
	n = 1;
	break;
    case 3:
	seg[0].y1 = 0;
	seg[0].y2 = t->corner_width - t->boundary_width + t->bw;
	seg[0].x1 = t->corner_width - t->boundary_width;
	seg[0].x2 = t->corner_width - t->boundary_width;
	n = 1;
	break;
    }
    XDrawSegments(dpy, t->corners[i], vert, seg, 1);
}

/*
 * draws a pattern within a window
 */
static void
draw_pattern(Window win, GC ShadowGC, GC ReliefGC, int h1, int w1, int t1)
{
    XSegment seg[2];
    int i, h, b, u, w, r, l;

    h = t1 * h1 / 200;
    b = (t1 >> 1) + h;
    u = t1 - b - 1;
    w = t1 * w1 / 200;
    r = (t1 >> 1) + w;
    l = t1 - r - 1;

    i = 0;
    seg[i].x1 = l;
    seg[i].y1 = u;
    seg[i].x2 = r;
    seg[i++].y2 = u;

    seg[i].x1 = l;
    seg[i].y1 = u;
    seg[i].x2 = l;
    seg[i++].y2 = b;
    XDrawSegments(dpy, win, ShadowGC, seg, i);

    i = 0;
    seg[i].x1 = l;
    seg[i].y1 = b;
    seg[i].x2 = r;
    seg[i++].y2 = b;

    seg[i].x1 = r;
    seg[i].y1 = u;
    seg[i].x2 = r;
    seg[i++].y2 = b;
    XDrawSegments(dpy, win, ReliefGC, seg, i);
}

/*
 * relieve a rectangle
 */
static void
relieve_rectangle(Window win, int x, int y, int w, int h, GC Hilite, GC Shadow)
{
    XDrawLine(dpy, win, Hilite, x, y, w + x - 1, y);
    XDrawLine(dpy, win, Hilite, x, y, x, h + y - 1);
    XDrawLine(dpy, win, Hilite, x, y + 1, w + x - 2, y + 1);
    XDrawLine(dpy, win, Hilite, x + 1, y + 1, x + 1, h + y - 2);

    XDrawLine(dpy, win, Shadow, x, h + y - 1, w + x - 1, h + y - 1);
    XDrawLine(dpy, win, Shadow, w + x - 1, y, w + x - 1, h + y - 1);
    XDrawLine(dpy, win, Shadow, x + 1, h + y - 2, w + x - 2, h + y - 2);
    XDrawLine(dpy, win, Shadow, w + x - 2, y + 1, w + x - 2, h + y - 2);
}

/*
 * draw border windows
 */
void
DEC_DrawBorder(MwmWindow *t, Window win, int x, int y, int w, int h,
	       GC ReliefGC, GC ShadowGC)
{
    XSegment seg[4];
    int i;

    /* top */
    if (win == t->sides[0])
    {
	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y;

	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = x;
	seg[i++].y2 = h + y - 1;

	XDrawSegments(dpy, win, ReliefGC, seg, i);

	i = 0;
	seg[i].x1 = x + t->boundary_width;
	seg[i].y1 = y + h - 1;
	seg[i].x2 = w + x - 1 - t->boundary_width;
	seg[i++].y2 = y + h - 1;

	seg[i].x1 = x + w - 1;
	seg[i].y1 = y;
	seg[i].x2 = x + w - 1;
	seg[i++].y2 = y + h - 1;

	XDrawSegments(dpy, win, ShadowGC, seg, i);
    }
    /* right */
    else if (win == t->sides[1])
    {
	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y;

	seg[i].x1 = x;
	seg[i].y1 = y + t->boundary_width - 1;
	seg[i].x2 = x;
	seg[i++].y2 = h + y - 1 - t->boundary_width;

	XDrawSegments(dpy, win, ReliefGC, seg, i);

	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y + h - 1;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y + h - 1;

	seg[i].x1 = x + w - 1;
	seg[i].y1 = y;
	seg[i].x2 = x + w - 1;
	seg[i++].y2 = y + h - 1;

	XDrawSegments(dpy, win, ShadowGC, seg, i);
    }
    /* bottom */
    else if (win == t->sides[2])
    {
	i = 0;
	seg[i].x1 = x + t->boundary_width;
	seg[i].y1 = y;
	seg[i].x2 = w + x - t->boundary_width;
	seg[i++].y2 = y;

	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = x;
	seg[i++].y2 = h + y - 1;

	XDrawSegments(dpy, win, ReliefGC, seg, i);

	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y + h - 1;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y + h - 1;

	seg[i].x1 = x + w - 1;
	seg[i].y1 = y;
	seg[i].x2 = x + w - 1;
	seg[i++].y2 = y + h - 1;

	XDrawSegments(dpy, win, ShadowGC, seg, i);
    }
    /* left */
    else
    {
	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y;

	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = x;
	seg[i++].y2 = h + y - 1;

	XDrawSegments(dpy, win, ReliefGC, seg, i);

	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y + h - 1;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y + h - 1;

	seg[i].x1 = x + w - 1;
	seg[i].y1 = y + t->boundary_width - 1;
	seg[i].x2 = x + w - 1;
	seg[i++].y2 = y + h - t->boundary_width;

	XDrawSegments(dpy, win, ShadowGC, seg, i);
    }
}

/*
 * draws the relief pattern around a window
 */
void
DEC_DrawShadows(MwmWindow *t, Window win, int x, int y, int w, int h,
		GC ReliefGC, GC ShadowGC)
{
    XSegment seg[4];
    int i;

    i = 0;
    seg[i].x1 = x;
    seg[i].y1 = y;
    seg[i].x2 = w + x - 1;
    seg[i++].y2 = y;

    seg[i].x1 = x;
    seg[i].y1 = y;
    seg[i].x2 = x;
    seg[i++].y2 = h + y - 1;

    XDrawSegments(dpy, win, ReliefGC, seg, i);

    i = 0;
    seg[i].x1 = x;
    seg[i].y1 = y + h - 1;
    seg[i].x2 = w + x - 1;
    seg[i++].y2 = y + h - 1;

    seg[i].x1 = x + w - 1;
    seg[i].y1 = y;
    seg[i].x2 = x + w - 1;
    seg[i++].y2 = y + h - 1;

    XDrawSegments(dpy, win, ShadowGC, seg, i);
}

/*
 * Interprets the property MOTIF_WM_HINTS, sets decoration and functions
 * accordingly
 */
void
DEC_SelectDecorations(ScreenInfo *scr, MwmWindow *t)
{
    int border_width, resize_width;

    border_width = scr->frame_border_width;
    resize_width = scr->resize_border_width;

    if (t->mwm_hints && (t->mwm_hints->flags & MWM_HINTS_FUNCTIONS))
    {
	t->functions = t->mwm_hints->functions;

	/*
	 * functions affect the decorations! if the user says no iconify
	 * function, then the iconify button doesn't show up.  So do functions
	 * first.
	 */
	if (t->functions & MWM_FUNC_ALL)
	{
	    /* If we get ALL + some other things, that means to use ALL except
	     * the other things... */
	    t->functions &= ~MWM_FUNC_ALL;
	    t->functions = (MWM_FUNC_RESIZE | MWM_FUNC_MOVE | MWM_FUNC_MINIMIZE |
			    MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE)
		& (~(t->functions));
	}

	if (t->flags & TRANSIENT)
	    t->functions &= scr->transient_functions;
	else
	    t->functions &= t->client_functions;

    }
    else if (t->flags & TRANSIENT)
	t->functions = scr->transient_functions;
    else
	t->functions = t->client_functions;


    if (t->mwm_hints && (t->mwm_hints->flags & MWM_HINTS_DECORATIONS))
    {
	t->decorations = t->mwm_hints->decorations;

	/*
	 * next, figure out the decorations
	 */
	if (t->decorations & MWM_DECOR_ALL)
	{
	    /* If we get ALL + some other things, that means to use ALL except
	     * the other things... */
	    t->decorations &= ~MWM_DECOR_ALL;
	    t->decorations = (MWM_DECOR_BORDER | MWM_DECOR_RESIZEH |
			      MWM_DECOR_TITLE | MWM_DECOR_MENU |
			      MWM_DECOR_MINIMIZE | MWM_DECOR_MAXIMIZE)
		& (~t->decorations);
	}

	if (t->flags & TRANSIENT)
	    t->decorations &= scr->transient_decoration;
	else
	    t->decorations &= t->client_decoration;

    }
    else if (t->flags & TRANSIENT)
	t->decorations = scr->transient_decoration;
    else
	t->decorations = t->client_decoration;

    /*
     * Now I have the un-altered decor and functions, but with the ALL
     * attribute cleared and interpreted. I need to modify the decorations
     * that are affected by the functions
     */
    if (!(t->functions & MWM_FUNC_RESIZE))
	t->decorations &= ~MWM_DECOR_RESIZEH;
    /* MWM_FUNC_MOVE has no impact on decorations. */
    if (!(t->functions & MWM_FUNC_MINIMIZE))
	t->decorations &= ~MWM_DECOR_MINIMIZE;
    if (!(t->functions & MWM_FUNC_MAXIMIZE))
	t->decorations &= ~MWM_DECOR_MAXIMIZE;
    /* MWM_FUNC_CLOSE has no impact on decorations. */

    /*
     * This rule is implicit, but its easier to deal with if I take care of 
     * it now
     */
    if (t->decorations & (MWM_DECOR_MENU | MWM_DECOR_MINIMIZE | MWM_DECOR_MAXIMIZE))
	t->decorations |= MWM_DECOR_TITLE;

    if (t->decorations & (MWM_DECOR_TITLE | MWM_DECOR_RESIZEH))
	t->decorations |= MWM_DECOR_BORDER;

    if (t->wShaped)
	t->decorations &= ~(MWM_DECOR_BORDER | MWM_DECOR_RESIZEH);

    /*
     * Assume no decorations, and build up
     */
    t->boundary_width = 0;
    t->corner_width = 0;
    t->title_height = 0;

    if (t->decorations & MWM_DECOR_BORDER)
    {
	/* A narrow border is displayed (5 pixels - 2 relief, 1 top, 2
	 * shadow) */
	t->boundary_width = border_width;
    }

    if (t->decorations & MWM_DECOR_TITLE)
    {
	/* A title barm with no buttons in it - window gets a 1 pixel wide
	 * black border. */
	t->title_height = scr->components[MWM_TITLE_A].f_height + 3;
    }

    if (t->decorations & MWM_DECOR_RESIZEH)
    {
	/* A wide border, with corner tiles is desplayed (10 pixels - 2
	 * relief, 2 shadow) */
	t->boundary_width = resize_width;
	t->corner_width = scr->components[MWM_TITLE_A].f_height + 3 +
	    t->boundary_width;
    }

    t->bw = 0;
    if (t->title_height > 0)
	t->title_height += t->bw;
}

/*
 * Interprets the property MOTIF_WM_HINTS, sets decoration and functions
 * accordingly
 */
void
DEC_ReselectDecorations(ScreenInfo *scr, MwmWindow *t)
{
    int border_width, resize_width, i;
    unsigned long valuemask;	/* mask for create windows */
    XSetWindowAttributes attributes;	/* attributes for create windows */

    border_width = scr->frame_border_width;
    resize_width = scr->resize_border_width;

    if (t->mwm_hints && (t->mwm_hints->flags & MWM_HINTS_FUNCTIONS))
    {
	t->functions = t->mwm_hints->functions;

	/*
	 * functions affect the decorations! if the user says no iconify
	 * function, then the iconify button doesn't show up.  So do functions
	 * first.
	 */
	if (t->functions & MWM_FUNC_ALL)
	{
	    /* If we get ALL + some other things, that means to use ALL except
	     * the other things... */
	    t->functions &= ~MWM_FUNC_ALL;
	    t->functions = (MWM_FUNC_RESIZE | MWM_FUNC_MOVE | MWM_FUNC_MINIMIZE |
			    MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE)
		& (~(t->functions));
	}

	if (t->flags & TRANSIENT)
	    t->functions &= scr->transient_functions;
	else
	    t->functions &= t->client_functions;

    }
    else if (t->flags & TRANSIENT)
	t->functions = scr->transient_functions;
    else
	t->functions = t->client_functions;


    if (t->mwm_hints && (t->mwm_hints->flags & MWM_HINTS_DECORATIONS))
    {
	t->decorations = t->mwm_hints->decorations;

	/*
	 * next, figure out the decorations
	 */
	if (t->decorations & MWM_DECOR_ALL)
	{
	    /* If we get ALL + some other things, that means to use ALL except
	     * the other things... */
	    t->decorations &= ~MWM_DECOR_ALL;
	    t->decorations = (MWM_DECOR_BORDER | MWM_DECOR_RESIZEH |
			      MWM_DECOR_TITLE | MWM_DECOR_MENU |
			      MWM_DECOR_MINIMIZE | MWM_DECOR_MAXIMIZE)
		& (~t->decorations);
	}

	if (t->flags & TRANSIENT)
	    t->decorations &= scr->transient_decoration;
	else
	    t->decorations &= t->client_decoration;

    }
    else if (t->flags & TRANSIENT)
	t->decorations = scr->transient_decoration;
    else
	t->decorations = t->client_decoration;

    /*
     * Now I have the un-altered decor and functions, but with the ALL
     * attribute cleared and interpreted. I need to modify the decorations
     * that are affected by the functions
     */
    if (!(t->functions & MWM_FUNC_RESIZE))
	t->decorations &= ~MWM_DECOR_RESIZEH;
    /* MWM_FUNC_MOVE has no impact on decorations. */
    if (!(t->functions & MWM_FUNC_MINIMIZE))
	t->decorations &= ~MWM_DECOR_MINIMIZE;
    if (!(t->functions & MWM_FUNC_MAXIMIZE))
	t->decorations &= ~MWM_DECOR_MAXIMIZE;
    /* MWM_FUNC_CLOSE has no impact on decorations. */

    /*
     * This rule is implicit, but its easier to deal with if I take care of 
     * it now
     */
    if (t->decorations & (MWM_DECOR_MENU | MWM_DECOR_MINIMIZE | MWM_DECOR_MAXIMIZE))
	t->decorations |= MWM_DECOR_TITLE;

    if (t->decorations & (MWM_DECOR_TITLE | MWM_DECOR_RESIZEH))
	t->decorations |= MWM_DECOR_BORDER;

    if (t->wShaped)
	t->decorations &= ~(MWM_DECOR_BORDER | MWM_DECOR_RESIZEH);

    /*
     * Assume no decorations, and build up
     */
    t->boundary_width = 0;
    t->corner_width = 0;
    t->title_height = 0;

    valuemask = CWBorderPixel | CWCursor | CWEventMask |
	CWSaveUnder | CWBackingStore;
    attributes.backing_store = WhenMapped;
    if (scr->components[MWM_BORDER].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_BORDER].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel = scr->components[MWM_BORDER].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    attributes.event_mask = (ButtonPressMask | ButtonReleaseMask |
			     ExposureMask | EnterWindowMask | LeaveWindowMask);


    if (t->decorations & MWM_DECOR_BORDER)
    {
	/* A narrow border is displayed (5 pixels - 2 relief, 1 top, 2
	 * shadow) */
	t->boundary_width = border_width;

	if (t->sides[0] == None)
	{

	    for (i = 0; i < 4; i++)
	    {
		if (scr->resize_cursors)
		    attributes.cursor = scr->cursors[TOP_CURS + i];
		else
		    attributes.cursor = scr->cursors[DEFAULT_CURS];
		attributes.save_under =
		    scr->components[MWM_BORDER].save_under;
		t->sides[i] = XCreateWindow(dpy, t->frame, 0, 0,
					    t->boundary_width,
					    t->boundary_width,
					    0,
					    CopyFromParent, InputOutput,
					    CopyFromParent,
					    valuemask, &attributes);

		XSaveContext(dpy, t->sides[i], MwmContext, (XPointer)t);
	    }
	}
	for (i = 0; i < 4; i++)
	    XRaiseWindow(dpy, t->sides[i]);
    }
    else if (t->sides[0] != None)
    {
	for (i = 0; i < 4; i++)
	{
	    XDeleteContext(dpy, t->sides[i], MwmContext);
	    XDestroyWindow(dpy, t->sides[i]);
	    t->sides[i] = None;
	}
    }

    if (scr->components[MWM_RESIZE_H].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_RESIZE_H].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_RESIZE_H].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (t->decorations & MWM_DECOR_RESIZEH)
    {
	/* A wide border, with corner tiles is desplayed (10 pixels - 2
	 * relief, 2 shadow) */
	t->boundary_width = resize_width;
	t->corner_width = scr->components[MWM_TITLE_A].f_height + 3 +
	    t->boundary_width;

	if (t->corners[0] == None)
	{

	    attributes.event_mask = (ButtonPressMask | ButtonReleaseMask |
			     ExposureMask | EnterWindowMask | LeaveWindowMask);

	    /*
	     * Just dump the windows any old place and let
	     * DEC_ConfigureDecorations take care of the mess
	     */
	    for (i = 0; i < 4; i++)
	    {
		if (scr->resize_cursors)
		    attributes.cursor = scr->cursors[TOP_LEFT_CURS + i];
		else
		    attributes.cursor = scr->cursors[DEFAULT_CURS];
		attributes.save_under =
		    scr->components[MWM_RESIZE_H].save_under;
		t->corners[i] = XCreateWindow(dpy, t->frame, 0, 0,
					      t->corner_width,
					      t->corner_width,
					      0, CopyFromParent,
					      InputOutput, CopyFromParent,
					      valuemask, &attributes);

		XSaveContext(dpy, t->corners[i], MwmContext, (XPointer)t);
	    }
	}
	for (i = 0; i < 4; i++)
	    XRaiseWindow(dpy, t->corners[i]);
    }
    else if (t->corners[0] != None)
    {
	for (i = 0; i < 4; i++)
	{
	    XDestroyWindow(dpy, t->corners[i]);
	    XDeleteContext(dpy, t->corners[i], MwmContext);
	    t->corners[i] = None;
	}
    }

    if (scr->components[MWM_TITLE_A].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_TITLE_A].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_TITLE_A].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (t->decorations & MWM_DECOR_TITLE)
    {
	/* A title barm with no buttons in it - window gets a 1 pixel wide
	 * black border. */
	t->title_height = scr->components[MWM_TITLE_A].f_height + 3;
	if (t->title_height > 0)
	    t->title_height += t->bw;

	t->title_x = t->title_y = 0;
	t->title_width = t->frame_width - 2 * t->corner_width - 3 + t->bw;
	if (t->title_width < 1)
	    t->title_width = 1;

	if (t->title == None)
	{

	    attributes.cursor = scr->cursors[TITLE_CURS];
	    attributes.save_under = scr->components[MWM_TITLE_A].save_under;
	    t->title = XCreateWindow(dpy, t->frame,
				     t->title_x, t->title_y,
				     t->title_width, t->title_height,
				     0,
				     CopyFromParent, InputOutput,
				     CopyFromParent,
				     valuemask, &attributes);

	    XSaveContext(dpy, t->title, MwmContext, (XPointer)t);
	}
	XRaiseWindow(dpy, t->title);
    }
    else if (t->title != None)
    {
	XDeleteContext(dpy, t->title, MwmContext);
	XDestroyWindow(dpy, t->title);
	t->title = None;
    }

    if (scr->components[MWM_MENU_B].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_MENU_B].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_MENU_B].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (t->decorations & MWM_DECOR_MENU)
    {
	/* title-bar menu button window gets 1 pixel wide black border */
	if (t->menub == None)
	{
	    attributes.save_under = scr->components[MWM_MENU_B].save_under;
	    t->menub = XCreateWindow(dpy, t->frame,
				     t->title_height, 0,
				     t->title_height, t->title_height,
				     0,
				     CopyFromParent, InputOutput,
				     CopyFromParent,
				     valuemask, &attributes);

	    XSaveContext(dpy, t->menub, MwmContext, (XPointer)t);
	}
	XRaiseWindow(dpy, t->menub);
    }
    else if (t->menub != None)
    {
	XDeleteContext(dpy, t->menub, MwmContext);
	XDestroyWindow(dpy, t->menub);
	t->menub = None;
    }

    if (scr->components[MWM_MINIMIZE_B].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_MINIMIZE_B].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_MINIMIZE_B].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (t->decorations & MWM_DECOR_MINIMIZE)
    {
	/* title-bar + iconify button, no menu button. window gets 1 pixel
	 * wide black border */
	if (t->minimizeb == None)
	{
	    attributes.save_under = scr->components[MWM_MINIMIZE_B].save_under;
	    t->minimizeb = XCreateWindow(dpy, t->frame,
					 t->title_width - t->title_height * 2,
					 0,
					 t->title_height, t->title_height,
					 0,
					 CopyFromParent, InputOutput,
					 CopyFromParent,
					 valuemask, &attributes);

	    XSaveContext(dpy, t->minimizeb, MwmContext, (XPointer)t);
	}
	XRaiseWindow(dpy, t->minimizeb);
    }
    else if (t->minimizeb != None)
    {
	XDeleteContext(dpy, t->minimizeb, MwmContext);
	XDestroyWindow(dpy, t->minimizeb);
	t->minimizeb = None;
    }

    if (scr->components[MWM_MAXIMIZE_B].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_MAXIMIZE_B].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_MAXIMIZE_B].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (t->decorations & MWM_DECOR_MAXIMIZE)
    {
	/* title-bar + maximize button, no menu button, no iconify. * window
	 * has 1 pixel wide black border */
	if (t->maximizeb == None)
	{
	    attributes.save_under = scr->components[MWM_MINIMIZE_B].save_under;
	    t->maximizeb = XCreateWindow(dpy, t->frame,
					 t->title_width - t->title_height * 1,
					 0,
					 t->title_height, t->title_height,
					 0,
					 CopyFromParent, InputOutput,
					 CopyFromParent,
					 valuemask, &attributes);

	    XSaveContext(dpy, t->maximizeb, MwmContext, (XPointer)t);
	}
	XRaiseWindow(dpy, t->maximizeb);
    }
    else if (t->maximizeb != None)
    {
	XDeleteContext(dpy, t->maximizeb, MwmContext);
	XDestroyWindow(dpy, t->maximizeb);
	t->maximizeb = None;
    }

    XMapSubwindows(dpy, t->frame);
    XLowerWindow(dpy, t->shield);
    XRaiseWindow(dpy, t->parent);

    t->bw = 0;
}

/*
 * create the decoration windows
 */
void
DEC_CreateDecorations(ScreenInfo *scr, MwmWindow *tmp_win)
{
    unsigned long valuemask;	/* mask for create windows */
    XSetWindowAttributes attributes;	/* attributes for create windows */
    int i;

    /*
     * create windows
     */
    tmp_win->frame_x = tmp_win->attr.x + tmp_win->old_bw - tmp_win->bw;
    tmp_win->frame_y = tmp_win->attr.y + tmp_win->old_bw - tmp_win->bw;

    tmp_win->frame_width = tmp_win->attr.width +
	2 * tmp_win->boundary_width +
	2 * tmp_win->matte_width;
    tmp_win->frame_height = tmp_win->attr.height + tmp_win->title_height +
	2 * tmp_win->matte_width +
	2 * tmp_win->boundary_width;

    valuemask = CWBorderPixel | CWCursor | CWEventMask;
    if (scr->components[MWM_BORDER].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_BORDER].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_BORDER].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }

    attributes.border_pixel = scr->components[MWM_BORDER].bottom_shadow_color;

    attributes.cursor = scr->cursors[DEFAULT_CURS];
    attributes.event_mask = (SubstructureRedirectMask | ButtonPressMask |
			     ButtonReleaseMask | EnterWindowMask |
			     LeaveWindowMask | ExposureMask);
    valuemask |= CWSaveUnder;
    attributes.save_under = True;

    /* What the heck, we'll always reparent everything from now on! */
    tmp_win->frame =
	XCreateWindow(dpy, scr->root_win, tmp_win->frame_x, tmp_win->frame_y,
		      tmp_win->frame_width, tmp_win->frame_height,
		      tmp_win->bw, CopyFromParent, InputOutput,
		      CopyFromParent, valuemask, &attributes);

    attributes.save_under = False;

    /* Thats not all, we'll double-reparent the window ! */
    attributes.cursor = scr->cursors[DEFAULT_CURS];
    attributes.background_pixel = tmp_win->matte_background;
    valuemask &= ~CWBackPixmap;
    valuemask |= CWBackPixel;
    tmp_win->parent =
	XCreateWindow(dpy, tmp_win->frame,
		      tmp_win->boundary_width,
		      tmp_win->boundary_width + tmp_win->title_height,
		      (tmp_win->frame_width - 2 * tmp_win->boundary_width),
		      (tmp_win->frame_height - 2 * tmp_win->boundary_width -
		       tmp_win->title_height), tmp_win->bw, CopyFromParent,
		      InputOutput, CopyFromParent, valuemask, &attributes);

    attributes.cursor = scr->cursors[SYS_MODAL_CURS];
    attributes.override_redirect = True;
    attributes.event_mask = ButtonPressMask | ButtonPressMask;

    tmp_win->shield =
	XCreateWindow(dpy, tmp_win->frame,
		      tmp_win->boundary_width,
		      tmp_win->boundary_width + tmp_win->title_height,
		      (tmp_win->frame_width - 2 * tmp_win->boundary_width),
		      (tmp_win->frame_height - 2 * tmp_win->boundary_width -
		       tmp_win->title_height),
		      tmp_win->bw,
		      0, InputOnly, CopyFromParent,
		      CWEventMask | CWCursor | CWOverrideRedirect, &attributes);

    valuemask |= CWBackingStore;
    attributes.backing_store = WhenMapped;
    attributes.event_mask = (SubstructureRedirectMask | ButtonPressMask |
			     ButtonReleaseMask | EnterWindowMask |
			     LeaveWindowMask | ExposureMask);

    attributes.cursor = scr->cursors[DEFAULT_CURS];
    if (scr->pager_win)
    {
	if (scr->components[MWM_PAGER].background_pixmap
	    != XmUNSPECIFIED_PIXMAP)
	{
	    attributes.background_pixmap =
		scr->components[MWM_PAGER].background_pixmap;
	    valuemask |= CWBackPixmap;
	}
	else
	{
	    attributes.background_pixel = scr->components[MWM_PAGER].background;
	    valuemask |= CWBackPixel;
	}

	/* Create the pager_view window even if we're sticky, in case the
	 * user unsticks the window */
	attributes.event_mask = ExposureMask;
	tmp_win->pager_view = XCreateWindow(dpy, scr->pager_win,
					    -10, -10,
					    2, 2,
					    1,
					    CopyFromParent, InputOutput,
					    CopyFromParent, valuemask,
					    &attributes);
	XMapRaised(dpy, tmp_win->pager_view);
    }

    attributes.event_mask = (ButtonPressMask | ButtonReleaseMask |
			     ExposureMask | EnterWindowMask | LeaveWindowMask);
    tmp_win->title_x = tmp_win->title_y = 0;
    tmp_win->title_width = tmp_win->frame_width - 2 * tmp_win->corner_width
	- 3 + tmp_win->bw;
    if (tmp_win->title_width < 1)
	tmp_win->title_width = 1;

    if (scr->components[MWM_RESIZE_H].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_RESIZE_H].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_RESIZE_H].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (tmp_win->decorations & MWM_DECOR_RESIZEH)
    {
	/* Just dump the windows any old place and let
	 * DEC_ConfigureDecorations take care of the mess */
	attributes.save_under = scr->components[MWM_RESIZE_H].save_under;
	for (i = 0; i < 4; i++)
	{
	    attributes.cursor = scr->cursors[TOP_LEFT_CURS + i];
	    tmp_win->corners[i] =
		XCreateWindow(dpy, tmp_win->frame, 0, 0,
			      tmp_win->corner_width, tmp_win->corner_width,
			      0, CopyFromParent, InputOutput,
			      CopyFromParent, valuemask, &attributes);
	}
    }
    else
    {
	for (i = 0; i < 4; i++)
	    tmp_win->corners[i] = None;
    }

    if (scr->components[MWM_TITLE_A].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_TITLE_A].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_TITLE_A].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (tmp_win->decorations & MWM_DECOR_TITLE)
    {
	tmp_win->title_x = tmp_win->boundary_width + tmp_win->title_height + 1;
	tmp_win->title_y = tmp_win->boundary_width;
	attributes.cursor = scr->cursors[TITLE_CURS];
	attributes.save_under = scr->components[MWM_TITLE_A].save_under;
	tmp_win->title =
	    XCreateWindow(dpy, tmp_win->frame,
			  tmp_win->title_x, tmp_win->title_y,
			  tmp_win->title_width, tmp_win->title_height,
			  0,
			  CopyFromParent, InputOutput, CopyFromParent,
			  valuemask, &attributes);
    }
    else
	tmp_win->title = None;

    if (scr->components[MWM_MENU_B].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_MENU_B].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_MENU_B].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (tmp_win->decorations & MWM_DECOR_MENU)
    {
	attributes.cursor = scr->cursors[SYS_CURS];
	attributes.save_under = scr->components[MWM_MENU_B].save_under;
	tmp_win->menub =
	    XCreateWindow(dpy, tmp_win->frame,
			  tmp_win->title_height, 0,
			  tmp_win->title_height, tmp_win->title_height,
			  0,
			  CopyFromParent, InputOutput, CopyFromParent,
			  valuemask, &attributes);
    }
    else
	tmp_win->menub = None;

    if (scr->components[MWM_MINIMIZE_B].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_MINIMIZE_B].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_MINIMIZE_B].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (tmp_win->decorations & MWM_DECOR_MINIMIZE)
    {
	attributes.save_under = scr->components[MWM_MINIMIZE_B].save_under;
	tmp_win->minimizeb =
	    XCreateWindow(dpy, tmp_win->frame,
			  tmp_win->title_width -
			  tmp_win->title_height * 2, 0,
			  tmp_win->title_height, tmp_win->title_height,
			  0,
			  CopyFromParent, InputOutput, CopyFromParent,
			  valuemask, &attributes);
    }
    else
	tmp_win->minimizeb = None;

    if (scr->components[MWM_MAXIMIZE_B].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_MAXIMIZE_B].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_MAXIMIZE_B].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (tmp_win->decorations & MWM_DECOR_MAXIMIZE)
    {
	attributes.save_under = scr->components[MWM_MAXIMIZE_B].save_under;
	tmp_win->maximizeb =
	    XCreateWindow(dpy, tmp_win->frame,
			  tmp_win->title_width -
			  tmp_win->title_height * 1, 0,
			  tmp_win->title_height, tmp_win->title_height,
			  0,
			  CopyFromParent, InputOutput, CopyFromParent,
			  valuemask, &attributes);
    }
    else
	tmp_win->maximizeb = None;

    if (scr->components[MWM_BORDER].background_pixmap
	!= XmUNSPECIFIED_PIXMAP)
    {
	attributes.background_pixmap =
	    scr->components[MWM_BORDER].background_pixmap;
	valuemask &= ~CWBackPixel;
	valuemask |= CWBackPixmap;
    }
    else
    {
	attributes.background_pixel =
	    scr->components[MWM_BORDER].background;
	valuemask &= ~CWBackPixmap;
	valuemask |= CWBackPixel;
    }
    if (tmp_win->decorations & MWM_DECOR_BORDER)
    {
	attributes.save_under = scr->components[MWM_BORDER].save_under;
	for (i = 0; i < 4; i++)
	{
	    attributes.cursor = scr->cursors[TOP_CURS + i];
	    tmp_win->sides[i] =
		XCreateWindow(dpy, tmp_win->frame,
			      0, 0,
			      tmp_win->boundary_width, tmp_win->boundary_width,
			      0,
			      CopyFromParent, InputOutput, CopyFromParent,
			      valuemask, &attributes);
	}
    }
    else
    {
	for (i = 0; i < 4; i++)
	    tmp_win->sides[i] = None;
    }

    XMapSubwindows(dpy, tmp_win->frame);
    XLowerWindow(dpy, tmp_win->shield);
    XRaiseWindow(dpy, tmp_win->parent);
    XReparentWindow(dpy, tmp_win->w, tmp_win->parent,
		    tmp_win->matte_width, tmp_win->matte_width);

    valuemask = (CWEventMask | CWDontPropagate);
    attributes.event_mask = (StructureNotifyMask | PropertyChangeMask |
			     VisibilityChangeMask | EnterWindowMask |
			     LeaveWindowMask | ColormapChangeMask |
			     FocusChangeMask);

    if (tmp_win->w == scr->pager_win)
    {
	scr->mwm_pager = tmp_win;
	tmp_win->flags |= STICKY;
	attributes.event_mask |= ButtonPressMask | ButtonReleaseMask |
	    ExposureMask | ButtonMotionMask;
	attributes.do_not_propagate_mask = ButtonPressMask;
    }
    else
	attributes.do_not_propagate_mask = ButtonPressMask | ButtonReleaseMask;

    valuemask |= CWBackingStore;
    attributes.backing_store = WhenMapped;

    XChangeWindowAttributes(dpy, tmp_win->w, valuemask, &attributes);
}

/*
 *  Inputs:
 *      tmp_win - the MwmWindow pointer
 *      x       - the x coordinate of the upper-left outer corner of the frame
 *      y       - the y coordinate of the upper-left outer corner of the frame
 *      w       - the width of the frame window w/o border
 *      h       - the height of the frame window w/o border
 *
 *  Special Considerations:
 *      This routine will check to make sure the window is not completely
 *      off the display, if it is, it'll bring some of it back on.
 *
 *      The tmp_win->frame_XXX variables should NOT be updated with the
 *      values of x,y,w,h prior to calling this routine, since the new
 *      values are compared against the old to see whether a synthetic
 *      ConfigureNotify event should be sent.  (It should be sent if the
 *      window was moved but not resized.)
 *
 */
void
DEC_ConfigureDecorations(ScreenInfo *scr, MwmWindow *tmp_win,
			 int x, int y, int w, int h, Boolean sendEvent)
{
    XEvent client_event;
    XWindowChanges frame_wc, xwc;
    unsigned long frame_mask, xwcm;
    int cx, cy, i;
    Bool Resized = False;
    MwmWindow *t;
    int xwidth, ywidth;

    /* if windows is not being maximized, save size in case of maximization */
    if (!(tmp_win->flags & MAXIMIZED))
    {
	tmp_win->orig_x = x;
	tmp_win->orig_y = y;
	tmp_win->orig_wd = w;
	tmp_win->orig_ht = h;
    }

    /* make sure we stay on the screen -- used to be DontMoveOff */
    if (x + scr->virt_x + w < 16)
	x = 16 - scr->virt_x - w;
    if (y + scr->virt_y + h < 16)
	y = 16 - scr->virt_y - h;

    if (x >= scr->d_width + scr->virt_x_max - scr->virt_x - 16)
	x = scr->d_width + scr->virt_x_max - scr->virt_x - 16;
    if (y >= scr->d_height + scr->virt_y_max - scr->virt_y - 16)
	y = scr->d_height + scr->virt_y_max - scr->virt_y - 16;

    /*
     * According to the July 27, 1988 ICCCM draft, we should send a
     * "synthetic" ConfigureNotify event to the client if the window
     * was moved but not resized.
     */
    if ((x != tmp_win->frame_x || y != tmp_win->frame_y) &&
	(w == tmp_win->frame_width && h == tmp_win->frame_height))
	sendEvent = True;

    if ((w != tmp_win->frame_width) || (h != tmp_win->frame_height))
	Resized = True;

    if (Resized)
    {
	if (tmp_win->menub != None && tmp_win->minimizeb != None &&
	    tmp_win->maximizeb != None)
	{
	    tmp_win->title_width = w - 3 * tmp_win->title_height -
		2 * tmp_win->boundary_width + tmp_win->bw;
	}
	else if ((tmp_win->menub != None && tmp_win->minimizeb != None) ||
		 (tmp_win->menub != None && tmp_win->maximizeb != None) ||
		 (tmp_win->menub != None && tmp_win->minimizeb != None))
	{
	    tmp_win->title_width = w - 2 * tmp_win->title_height -
		2 * tmp_win->boundary_width + tmp_win->bw;
	}
	else if ((tmp_win->menub != None) || (tmp_win->minimizeb != None) ||
		 (tmp_win->maximizeb != None))
	{
	    tmp_win->title_width = w - 1 * tmp_win->title_height -
		2 * tmp_win->boundary_width + tmp_win->bw;
	}
	else
	{
	    tmp_win->title_width = w - 2 * tmp_win->boundary_width +
		tmp_win->bw;
	}


	if (tmp_win->title_width < 1)
	    tmp_win->title_width = 1;

	if (tmp_win->decorations & MWM_DECOR_TITLE)
	{
	    xwcm = CWWidth | CWX | CWY;

	    if (tmp_win->menub != None)
		tmp_win->title_x = tmp_win->boundary_width +
		    (1) * tmp_win->title_height;
	    else
		tmp_win->title_x = tmp_win->boundary_width;

	    if (tmp_win->title_x >= w - tmp_win->boundary_width)
		tmp_win->title_x = -10;
	    tmp_win->title_y = tmp_win->boundary_width;

	    xwc.width = tmp_win->title_width;
	    xwc.x = tmp_win->title_x;
	    xwc.y = tmp_win->title_y;
	    XConfigureWindow(dpy, tmp_win->title, xwcm, &xwc);
	}

	if (tmp_win->decorations & MWM_DECOR_MENU)
	{
	    xwcm = CWX | CWY;
	    xwc.x = tmp_win->boundary_width;
	    xwc.y = tmp_win->boundary_width;

	    if (tmp_win->menub != None)
	    {
		if (xwc.x + tmp_win->title_height < w - tmp_win->boundary_width)
		    XConfigureWindow(dpy, tmp_win->menub, xwcm, &xwc);
		else
		{
		    xwc.x = -tmp_win->title_height;
		    XConfigureWindow(dpy, tmp_win->menub, xwcm, &xwc);
		}
		xwc.x += tmp_win->title_height;
	    }
	}

	/* Note that we set X here.  That's because MINIMIZE will use what's
	 * computed by maximize (if it's around) to adjust it's right margin */
	xwc.x = w - tmp_win->boundary_width + tmp_win->bw;
	if (tmp_win->decorations & MWM_DECOR_MAXIMIZE)
	{
	    xwcm = CWX | CWY;
	    xwc.y = tmp_win->boundary_width;

	    if (tmp_win->maximizeb != None)
	    {
		xwc.x -= tmp_win->title_height;
		if (xwc.x > tmp_win->boundary_width)
		    XConfigureWindow(dpy, tmp_win->maximizeb, xwcm, &xwc);
		else
		{
		    xwc.x = -tmp_win->title_height;
		    XConfigureWindow(dpy, tmp_win->maximizeb, xwcm, &xwc);
		}
	    }
	}

	if (tmp_win->decorations & MWM_DECOR_MINIMIZE)
	{
	    xwcm = CWX | CWY;
	    /* note that X doesn't appear here.  See comment in paragraph
	     * above */
	    xwc.y = tmp_win->boundary_width;

	    if (tmp_win->minimizeb != None)
	    {
		xwc.x -= tmp_win->title_height;
		if (xwc.x > tmp_win->boundary_width)
		    XConfigureWindow(dpy, tmp_win->minimizeb, xwcm, &xwc);
		else
		{
		    xwc.x = -tmp_win->title_height;
		    XConfigureWindow(dpy, tmp_win->minimizeb, xwcm, &xwc);
		}
	    }
	}

	if (tmp_win->decorations & MWM_DECOR_BORDER)
	{
	    xwcm = CWWidth | CWHeight | CWX | CWY;
	    xwidth = w - 2 * tmp_win->corner_width + tmp_win->bw;
	    ywidth = h - 2 * tmp_win->corner_width;

	    if (xwidth < 2)
		xwidth = 2;
	    if (ywidth < 2)
		ywidth = 2;

	    for (i = 0; i < 4; i++)
	    {
		if (i == 0)
		{
		    xwc.x = tmp_win->corner_width;
		    xwc.y = 0;
		    xwc.height = tmp_win->boundary_width;
		    xwc.width = xwidth;
		}
		else if (i == 1)
		{
		    xwc.x = w - tmp_win->boundary_width + tmp_win->bw;
		    xwc.y = tmp_win->corner_width;
		    xwc.width = tmp_win->boundary_width;
		    xwc.height = ywidth;

		}
		else if (i == 2)
		{
		    xwc.x = tmp_win->corner_width;
		    xwc.y = h - tmp_win->boundary_width + tmp_win->bw;
		    xwc.height = tmp_win->boundary_width + tmp_win->bw;
		    xwc.width = xwidth;
		}
		else
		{
		    xwc.x = 0;
		    xwc.y = tmp_win->corner_width;
		    xwc.width = tmp_win->boundary_width;
		    xwc.height = ywidth;
		}
		XConfigureWindow(dpy, tmp_win->sides[i], xwcm, &xwc);
	    }
	}

	if (tmp_win->decorations & MWM_DECOR_RESIZEH)
	{
	    xwcm = CWX | CWY;

	    for (i = 0; i < 4; i++)
	    {
		if (i % 2)
		    xwc.x = w - tmp_win->corner_width + tmp_win->bw;
		else
		    xwc.x = 0;

		if (i / 2)
		    xwc.y = h - tmp_win->corner_width;
		else
		    xwc.y = 0;

		XConfigureWindow(dpy, tmp_win->corners[i], xwcm, &xwc);
	    }
	}
    }

    tmp_win->attr.width = w - 2 * tmp_win->boundary_width -
	2 * tmp_win->matte_width;
    tmp_win->attr.height = h - tmp_win->title_height -
	2 * tmp_win->boundary_width -
	2 * tmp_win->matte_width;
    /* may need to omit the -1 for shaped windows, next two lines */
    cx = tmp_win->boundary_width - tmp_win->bw;
    cy = tmp_win->title_height + tmp_win->boundary_width - tmp_win->bw;

    XMoveResizeWindow(dpy, tmp_win->w,
		      tmp_win->matte_width, tmp_win->matte_width,
		      tmp_win->attr.width, tmp_win->attr.height);
    XMoveResizeWindow(dpy, tmp_win->parent, cx, cy,
		      tmp_win->attr.width + 2 * tmp_win->matte_width,
		      tmp_win->attr.height + 2 * tmp_win->matte_width);
    XMoveResizeWindow(dpy, tmp_win->shield, cx, cy,
		      tmp_win->attr.width + 2 * tmp_win->matte_width,
		      tmp_win->attr.height + 2 * tmp_win->matte_width);

    /* 
     * fix up frame and assign size/location values in tmp_win
     */
    frame_wc.x = tmp_win->frame_x = x;
    frame_wc.y = tmp_win->frame_y = y;
    frame_wc.width = tmp_win->frame_width = w;
    frame_wc.height = tmp_win->frame_height = h;
    frame_mask = (CWX | CWY | CWWidth | CWHeight);
    XConfigureWindow(dpy, tmp_win->frame, frame_mask, &frame_wc);


    if ((Resized) && (tmp_win->wShaped))
    {
	DEC_SetShape(tmp_win, w);
    }
    XSync(dpy, 0);
    if (sendEvent)
    {
	client_event.type = ConfigureNotify;
	client_event.xconfigure.display = dpy;
	client_event.xconfigure.event = tmp_win->w;
	client_event.xconfigure.window = tmp_win->w;

	client_event.xconfigure.x = x + tmp_win->boundary_width +
	    tmp_win->matte_width;
	client_event.xconfigure.y = y + tmp_win->title_height +
	    tmp_win->boundary_width +
	    tmp_win->matte_width;
	client_event.xconfigure.width = w - 2 * tmp_win->boundary_width -
	    2 * tmp_win->matte_width;
	client_event.xconfigure.height = h - tmp_win->title_height -
	    2 * tmp_win->boundary_width -
	    2 * tmp_win->matte_width;

	client_event.xconfigure.border_width = tmp_win->bw;
	/* Real ConfigureNotify events say we're above title window, so ... */
	/* what if we don't have a title ????? */
	client_event.xconfigure.above = tmp_win->frame;
	client_event.xconfigure.override_redirect = False;
	XSendEvent(dpy, tmp_win->w, False, StructureNotifyMask, &client_event);
    }
    if (tmp_win == scr->mwm_pager)
    {
	PAGER_UpdateViewPort(scr);
	for (t = scr->mwm_root.next; t != NULL; t = t->next)
	{
	    PAGER_UpdateView(scr, t);
	}
    }
    else
	PAGER_UpdateView(scr, tmp_win);
}



/*
 *  draws the title bar
 */
void
DEC_DrawTitleBar(ScreenInfo *scr, MwmWindow *t, Bool onoroff, Bool NewTitle)
{
    int hor_off, w;
    GC ReliefGC, ShadowGC;
    Pixel Forecolor, BackColor;

    if (!t)
	return;
    if (!(t->decorations & MWM_DECOR_TITLE))
	return;

    if (onoroff)
    {
	Forecolor = scr->components[MWM_TITLE_A].active_foreground;
	BackColor = scr->components[MWM_TITLE_A].active_background;
	ReliefGC = scr->pressed_win == t->title
	    ? scr->components[MWM_TITLE_A].active_bot_GC
	    : scr->components[MWM_TITLE_A].active_top_GC;
	ShadowGC = scr->pressed_win == t->title
	    ? scr->components[MWM_TITLE_A].active_top_GC
	    : scr->components[MWM_TITLE_A].active_bot_GC;
    }
    else
    {
	Forecolor = scr->components[MWM_TITLE_A].foreground;
	BackColor = scr->components[MWM_TITLE_A].background;
	ReliefGC = scr->pressed_win == t->title
	    ? scr->components[MWM_TITLE_A].bot_GC
	    : scr->components[MWM_TITLE_A].top_GC;
	ShadowGC = scr->pressed_win == t->title
	    ? scr->components[MWM_TITLE_A].top_GC
	    : scr->components[MWM_TITLE_A].bot_GC;
    }
    MISC_FlushExpose(t->title);

    if (t->name != (char *)NULL)
    {
	w = XTextWidth(scr->components[MWM_TITLE_A].font,
		       t->name, strlen(t->name));
	if (w > t->title_width - 12)
	    w = t->title_width - 4;
	if (w < 0)
	    w = 0;
    }
    else
	w = 0;


    hor_off = (t->title_width - w) / 2;

    if (NewTitle)
	XClearWindow(dpy, t->title);

    if (t->name != (char *)NULL)
    {
	if (scr->clean_text)
	{
	    XClearArea(dpy, t->title,
		       hor_off - 2, 0, w + 4, t->title_height, False);
	}

	XDrawImageString(dpy, t->title,
			 onoroff
			 ? scr->components[MWM_TITLE_A].active_GC
			 : scr->components[MWM_TITLE_A].normal_GC,
			 hor_off,
			 scr->components[MWM_TITLE_A].f_y + 2,
			 t->name, strlen(t->name));

	DEC_DrawShadows(t, t->title, 0, 0, t->title_width, t->title_height,
			ReliefGC, ShadowGC);
    }

    XFlush(dpy);
}

/*
 * draws the windows decorations
 */
void
DEC_DrawDecorations(ScreenInfo *scr, MwmWindow *t,
		    Bool onoroff, Bool force, Bool Mapped, Window expose_win)
{
    Window w = None;
    int y, x;
    Bool NewColor = False;
    XSetWindowAttributes attributes;
    unsigned long valuemask;
    GC top_GC, bot_GC;

    if (!t)
	return;

    if (onoroff)
    {
	/* don't re-draw just for kicks */
	if ((!force) && (scr->mwm_highlight == t))
	    return;

	if (scr->mwm_highlight != t)
	    NewColor = True;

	/* make sure that the previously highlighted window got unhighlighted */
	if ((scr->mwm_highlight != t) && (scr->mwm_highlight != NULL))
	    DEC_DrawDecorations(scr, scr->mwm_highlight, False, False, True, None);

	/* set the keyboard focus */
	if ((Mapped) && (t->flags & MAPPED) && (scr->mwm_highlight != t))
	    w = t->w;
	else if ((t->flags & ICONIFIED) && scr->mwm_highlight != t)
	    w = t->icon_w;
	scr->mwm_highlight = t;
    }
    else
    {
	/* don't re-draw just for kicks */
	if ((!force) && (scr->mwm_highlight != t))
	    return;

	if (scr->mwm_highlight == t)
	{
	    scr->mwm_highlight = NULL;
	    NewColor = True;
	}
    }

    if ((scr->pager_win) && !(t->flags & STICKY))
    {
	if (NewColor)
	{
	    if (scr->components[MWM_PAGER].background_pixmap
		!= XmUNSPECIFIED_PIXMAP)
		XSetWindowBackgroundPixmap(dpy,
					   t->pager_view,
				 scr->components[MWM_PAGER].background_pixmap);
	    else
		XSetWindowBackground(dpy, t->pager_view,
				     scr->components[MWM_PAGER].background);
	    XClearWindow(dpy, t->pager_view);
	}

	if ((t->icon_image != NULL) &&
	    (scr->components[MWM_PAGER].f_height > 0))
	{
	    XDrawImageString(dpy, t->pager_view,
			     scr->components[MWM_PAGER].normal_GC,
			     2, scr->components[MWM_PAGER].f_y + 2,
			     t->icon_image, strlen(t->icon_image));
	}
    }

    if (t->flags & ICONIFIED)
    {
	ICON_DrawWindow(scr, t);
	return;
    }

    set_value_attributes(scr, t, &valuemask, &attributes, &scr->components[MWM_BORDER]);

    if (t->decorations & (MWM_DECOR_TITLE | MWM_DECOR_BORDER))
    {
	XSetWindowBorder(dpy, t->parent, attributes.background_pixel);
	XSetWindowBorder(dpy, t->frame, attributes.background_pixel);
    }


    if (t->decorations & MWM_DECOR_TITLE)
    {
	set_value_attributes(scr, t, &valuemask, &attributes, &scr->components[MWM_TITLE_A]);
	if (NewColor)
	{
	    XChangeWindowAttributes(dpy, t->title, valuemask, &attributes);
	    XClearWindow(dpy, t->title);
	}
    }
    if (t->decorations & MWM_DECOR_MENU)
    {
	if (t->menub != None)
	{
	    set_value_attributes(scr, t, &valuemask, &attributes, &scr->components[MWM_MENU_B]);
	    if (scr->mwm_highlight == t)
	    {
		top_GC = scr->components[MWM_MENU_B].active_top_GC;
		bot_GC = scr->components[MWM_MENU_B].active_bot_GC;
	    }
	    else
	    {
		top_GC = scr->components[MWM_MENU_B].top_GC;
		bot_GC = scr->components[MWM_MENU_B].bot_GC;
	    }
	    if (NewColor)
	    {
		XChangeWindowAttributes(dpy, t->menub, valuemask, &attributes);
		XClearWindow(dpy, t->menub);
	    }
	    if (MISC_FlushExpose(t->menub) || (expose_win == t->menub) ||
		(expose_win == None))
	    {
		DEC_DrawShadows(t, t->menub, 0, 0, t->title_height,
				t->title_height,
				(scr->pressed_win == t->menub
				 ? bot_GC
				 : top_GC),
				(scr->pressed_win == t->menub
				 ? top_GC
				 : bot_GC));
		draw_pattern(t->menub, top_GC, bot_GC,
			     lbut_styles[0],
			     lbut_styles[1],
			     t->title_height);
	    }
	}
    }

    if (t->decorations & MWM_DECOR_MAXIMIZE)
    {
	if (t->maximizeb != None)
	{
	    set_value_attributes(scr, t, &valuemask, &attributes,
				 &scr->components[MWM_MAXIMIZE_B]);
	    if (NewColor)
	    {
		XChangeWindowAttributes(dpy, t->maximizeb, valuemask, &attributes);
		XClearWindow(dpy, t->maximizeb);
	    }
	    if (scr->mwm_highlight == t)
	    {
		top_GC = scr->components[MWM_MAXIMIZE_B].active_top_GC;
		bot_GC = scr->components[MWM_MAXIMIZE_B].active_bot_GC;
	    }
	    else
	    {
		top_GC = scr->components[MWM_MAXIMIZE_B].top_GC;
		bot_GC = scr->components[MWM_MAXIMIZE_B].bot_GC;
	    }
	    if (MISC_FlushExpose(t->maximizeb) || (expose_win == t->maximizeb) ||
		(expose_win == None))
	    {
		DEC_DrawShadows(t, t->maximizeb, 0, 0, t->title_height,
				t->title_height,
				(scr->pressed_win == t->maximizeb
				 ? bot_GC
				 : top_GC),
				(scr->pressed_win == t->maximizeb
				 ? top_GC
				 : bot_GC));
		draw_pattern(t->maximizeb, top_GC, bot_GC,
			     rbut_style_max[0],
			     rbut_style_max[1],
			     t->title_height);
	    }
	}
    }

    if (t->decorations & MWM_DECOR_MINIMIZE)
    {
	if (t->minimizeb != None)
	{
	    set_value_attributes(scr, t, &valuemask, &attributes, &scr->components[MWM_MINIMIZE_B]);
	    if (NewColor)
	    {
		XChangeWindowAttributes(dpy, t->minimizeb, valuemask, &attributes);
		XClearWindow(dpy, t->minimizeb);
	    }
	    if (scr->mwm_highlight == t)
	    {
		top_GC = scr->components[MWM_MINIMIZE_B].active_top_GC;
		bot_GC = scr->components[MWM_MINIMIZE_B].active_bot_GC;
	    }
	    else
	    {
		top_GC = scr->components[MWM_MINIMIZE_B].top_GC;
		bot_GC = scr->components[MWM_MINIMIZE_B].bot_GC;
	    }
	    if (MISC_FlushExpose(t->minimizeb) || (expose_win == t->minimizeb) ||
		(expose_win == None))
	    {
		DEC_DrawShadows(t, t->minimizeb, 0, 0, t->title_height,
				t->title_height,
				(scr->pressed_win == t->minimizeb
				 ? bot_GC
				 : top_GC),
				(scr->pressed_win == t->minimizeb
				 ? top_GC
				 : bot_GC));
		draw_pattern(t->minimizeb, top_GC, bot_GC,
			     rbut_style_min[0],
			     rbut_style_min[1],
			     t->title_height);
	    }
	}
    }

    if (t->decorations & MWM_DECOR_TITLE)
	DEC_DrawTitleBar(scr, t, onoroff, False);

    if (t->decorations & MWM_DECOR_BORDER)
    {
	int i;

	/* draw relief lines */
	y = t->frame_height - 2 * t->corner_width;
	x = t->frame_width - 2 * t->corner_width + t->bw;

	for (i = 0; i < 4; i++)
	{
	    set_value_attributes(scr, t, &valuemask, &attributes, &scr->components[MWM_BORDER]);
	    if (NewColor)
	    {
		XChangeWindowAttributes(dpy, t->sides[i], valuemask, &attributes);
		XClearWindow(dpy, t->sides[i]);
	    }
	    if (scr->mwm_highlight == t)
	    {
		top_GC = scr->components[MWM_BORDER].active_top_GC;
		bot_GC = scr->components[MWM_BORDER].active_bot_GC;
	    }
	    else
	    {
		top_GC = scr->components[MWM_BORDER].top_GC;
		bot_GC = scr->components[MWM_BORDER].bot_GC;
	    }

	    if ((MISC_FlushExpose(t->sides[i])) || (expose_win == t->sides[i]) ||
		(expose_win == None))
	    {
		GC sgc, rgc;

		sgc = bot_GC;
		rgc = top_GC;
		/* index    side
		 * 0        TOP
		 * 1        RIGHT
		 * 2        BOTTOM
		 * 3        LEFT
		 */

		if (t->decorations & MWM_DECOR_RESIZEH)
		    DEC_DrawShadows(t, t->sides[i], 0, 0,
				    ((i % 2) ? t->boundary_width : x),
				    ((i % 2) ? y : t->boundary_width),
				    rgc, sgc);
		else
		    DEC_DrawBorder(t, t->sides[i], 0, 0,
				   ((i % 2) ? t->boundary_width : x),
				   ((i % 2) ? y : t->boundary_width),
				   rgc, sgc);
	    }
	}
    }


    if (t->decorations & MWM_DECOR_RESIZEH)
    {
	int i;

	/* draw relief lines */
	y = t->frame_height - 2 * t->corner_width;
	x = t->frame_width - 2 * t->corner_width + t->bw;

	for (i = 0; i < 4; i++)
	{
	    set_value_attributes(scr, t, &valuemask, &attributes, &scr->components[MWM_RESIZE_H]);
	    if (NewColor)
	    {
		XChangeWindowAttributes(dpy, t->corners[i], valuemask, &attributes);
		XClearWindow(dpy, t->corners[i]);
	    }
	    if (scr->mwm_highlight == t)
	    {
		top_GC = scr->components[MWM_RESIZE_H].active_top_GC;
		bot_GC = scr->components[MWM_RESIZE_H].active_bot_GC;
	    }
	    else
	    {
		top_GC = scr->components[MWM_RESIZE_H].top_GC;
		bot_GC = scr->components[MWM_RESIZE_H].bot_GC;
	    }
	    if ((MISC_FlushExpose(t->corners[i])) ||
		(expose_win == t->corners[i]) || (expose_win == None))
	    {
		GC rgc, sgc;

		rgc = top_GC;
		sgc = bot_GC;
		DEC_DrawShadows(t, t->corners[i], 0, 0, t->corner_width,
				(i / 2)
				? t->corner_width + t->bw
				: t->corner_width,
				rgc, sgc);
		if (t->boundary_width > 1)
		    draw_corners(t, i, ((i / 2) ? rgc : sgc), ((i % 2) ? rgc : sgc));
		else
		    draw_corners(t, i, ((i / 2) ? sgc : sgc), ((i % 2) ? sgc : sgc));
	    }
	}
    }

    if (t->matte_width > 1)
    {
	if (MISC_FlushExpose(t->parent) || expose_win == t->parent ||
	    expose_win == None)
	{
	    XGCValues gcv;
	    unsigned long gcm;

	    if (t->matte_bottom_shadow_pixmap != None &&
		t->matte_bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
	    {
		gcm = GCTile | GCFillStyle;

		gcv.tile = t->matte_bottom_shadow_pixmap;
		gcv.fill_style = FillTiled;
	    }
	    else
	    {
		gcm = GCForeground | GCBackground;

		gcv.foreground = t->matte_bottom_shadow_color;
		gcv.background = t->matte_background;
	    }
	    gcm |= GCLineWidth | GCLineStyle | GCCapStyle | GCGraphicsExposures;
	    gcv.line_width = 0;
	    gcv.line_style = LineSolid;
	    gcv.cap_style = CapButt;
	    gcv.graphics_exposures = False;
	    XChangeGC(dpy, scr->matte_bs_GC, gcm, &gcv);

	    if (t->matte_top_shadow_pixmap != None &&
		t->matte_top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
	    {
		gcm = GCTile | GCFillStyle;

		gcv.tile = t->matte_top_shadow_pixmap;
		gcv.fill_style = FillTiled;
	    }
	    else
	    {
		gcm = GCForeground | GCBackground;

		gcv.foreground = t->matte_top_shadow_color;
		gcv.background = t->matte_background;
	    }
	    gcm |= GCLineWidth | GCLineStyle | GCCapStyle | GCGraphicsExposures;
	    gcv.line_width = 0;
	    gcv.line_style = LineSolid;
	    gcv.cap_style = CapButt;
	    gcv.graphics_exposures = False;
	    XChangeGC(dpy, scr->matte_ts_GC, gcm, &gcv);

	    if (t->matte_width > 3)
	    {
		DEC_DrawShadows(t, t->parent, 0, 0,
				t->attr.width + 2 * t->matte_width,
				t->attr.height + 2 * t->matte_width,
				scr->matte_ts_GC, scr->matte_bs_GC);
		relieve_rectangle(t->parent,
				  t->matte_width - 2, t->matte_width - 2,
				  t->attr.width + 4, t->attr.height + 4,
				  scr->matte_bs_GC, scr->matte_ts_GC);
	    }
	    else
	    {
		DEC_DrawShadows(t, t->parent,
				t->matte_width - 2, t->matte_width - 2,
				t->attr.width + 3, t->attr.height + 3,
				scr->matte_bs_GC, scr->matte_ts_GC);
	    }
	}
    }

    if (!(t->decorations & MWM_DECOR_BORDER))
    {				/* no decorative border */
	/* for mono - put a black border on 
	 * for color, make it the color of the decoration background */
	if (t->boundary_width < 2)
	{
	    MISC_FlushExpose(t->frame);
	    XSetWindowBorder(dpy, t->frame, scr->components[MWM_BORDER].background);
	    XSetWindowBorder(dpy, t->parent, scr->components[MWM_BORDER].background);
	    if (scr->components[MWM_BORDER].background_pixmap)
		XSetWindowBackgroundPixmap(dpy, t->frame, scr->components[MWM_BORDER].background_pixmap);
	    XClearWindow(dpy, t->frame);
	    if (scr->components[MWM_BORDER].background_pixmap)
		XSetWindowBackgroundPixmap(dpy, t->parent, scr->components[MWM_BORDER].background_pixmap);
	    XClearWindow(dpy, t->parent);
	}
	else
	{
	    GC rgc, sgc;

	    if (scr->mwm_highlight == t)
	    {
		top_GC = scr->components[MWM_BORDER].active_top_GC;
		bot_GC = scr->components[MWM_BORDER].active_bot_GC;
	    }
	    else
	    {
		top_GC = scr->components[MWM_BORDER].top_GC;
		bot_GC = scr->components[MWM_BORDER].bot_GC;
	    }
	    XSetWindowBorder(dpy, t->parent, scr->components[MWM_BORDER].background);
	    XSetWindowBorder(dpy, t->frame, scr->components[MWM_BORDER].background);

	    rgc = top_GC;
	    sgc = bot_GC;
	    if (NewColor)
	    {
		XChangeWindowAttributes(dpy, t->frame, valuemask, &attributes);
		XClearWindow(dpy, t->frame);
	    }
	    if ((MISC_FlushExpose(t->frame)) || (expose_win == t->frame) ||
		(expose_win == None))
	    {
		if (t->boundary_width > 2)
		{
		    DEC_DrawShadows(t, t->frame, t->boundary_width - 1 - t->bw,
				    t->boundary_width - 1 - t->bw,
				    t->frame_width -
				    (t->boundary_width << 1) + 2 + 3 * t->bw,
				    t->frame_height -
				    (t->boundary_width << 1) + 2 + 3 * t->bw,
				    sgc, rgc);
		    DEC_DrawShadows(t, t->frame, 0, 0, t->frame_width + t->bw,
				    t->frame_height + t->bw, rgc, sgc);
		}
		else
		{
		    DEC_DrawShadows(t, t->frame, 0, 0, t->frame_width + t->bw,
				    t->frame_height + t->bw, rgc, rgc);
		}
	    }
	    else
	    {
		XSetWindowBackground(dpy, t->parent, scr->components[MWM_BORDER].background);
	    }
	}
    }
}

/*
 * set up the shaped window borders 
 */
void
DEC_SetShape(MwmWindow *tmp_win, int w)
{
    XRectangle rect;

    XShapeCombineShape(dpy, tmp_win->frame, ShapeBounding,
		       tmp_win->boundary_width,
		       tmp_win->title_height + tmp_win->boundary_width,
		       tmp_win->w,
		       ShapeBounding, ShapeSet);
    if (tmp_win->title)
    {
	/* windows w/ titles */
	rect.x = tmp_win->boundary_width;
	rect.y = tmp_win->title_y;
	rect.width = w - 2 * tmp_win->boundary_width + tmp_win->bw;
	rect.height = tmp_win->title_height;


	XShapeCombineRectangles(dpy, tmp_win->frame, ShapeBounding,
				0, 0, &rect, 1, ShapeUnion, Unsorted);
    }
}
