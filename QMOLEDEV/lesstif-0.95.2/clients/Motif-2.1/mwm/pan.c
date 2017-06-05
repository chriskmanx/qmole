/* $Id: pan.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $ */
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

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include "mwm.h"

#include <Xm/XmosP.h>
#if XmVERSION >= 2
#include <XmI/XmI.h>
#endif

/****************************************************************************
 * The root window is surrounded by four windows which are InputOnly.  This
 * means you can see 'through' them, but they eat the input that happens
 * inside them. An EnterEvent in one of these windows causes paging. The
 * windows have the cursor pointing in the pan direction or are hidden if
 * there is no more panning left in that direction. This is mostly intended
 * to enable panning even over Motif applictions, which does not work yet; it
 * seems that Motif windows eat all mouse events.
 *
 * Hermann Dunkel, HEDU, dunkel@cul-ipn.uni-kiel.de 1/94
 * Anglified the grammar so I could read it -- MLM 9/96
 ***************************************************************************/

/*
 * Creates the windows for edge-scrolling 
 */
void
PAN_Initialize(ScreenInfo *scr)
{
    XSetWindowAttributes attributes;	/* attributes for create */
    unsigned long valuemask;

    attributes.event_mask = (EnterWindowMask | LeaveWindowMask |
			     VisibilityChangeMask);
    valuemask = (CWEventMask | CWCursor);

    attributes.cursor = scr->cursors[TOP_CURS];
    scr->panner_top.win =
	XCreateWindow(dpy, scr->root_win,
		      0, 0,
		      scr->d_width, PAN_FRAME_THICKNESS,
		      0,	/* no border */
		      CopyFromParent, InputOnly,
		      CopyFromParent,
		      valuemask, &attributes);
    attributes.cursor = scr->cursors[LEFT_CURS];
    scr->panner_left.win =
	XCreateWindow(dpy, scr->root_win,
		      0, PAN_FRAME_THICKNESS,
		      PAN_FRAME_THICKNESS,
		      scr->d_height - 2 * PAN_FRAME_THICKNESS,
		      0,	/* no border */
		      CopyFromParent, InputOnly, CopyFromParent,
		      valuemask, &attributes);
    attributes.cursor = scr->cursors[RIGHT_CURS];
    scr->panner_right.win =
	XCreateWindow(dpy, scr->root_win,
		      scr->d_width - PAN_FRAME_THICKNESS, PAN_FRAME_THICKNESS,
		      PAN_FRAME_THICKNESS,
		      scr->d_height - 2 * PAN_FRAME_THICKNESS,
		      0,	/* no border */
		      CopyFromParent, InputOnly, CopyFromParent,
		      valuemask, &attributes);
    attributes.cursor = scr->cursors[BOTTOM_CURS];
    scr->panner_bottom.win =
	XCreateWindow(dpy, scr->root_win,
		      0, scr->d_height - PAN_FRAME_THICKNESS,
		      scr->d_width, PAN_FRAME_THICKNESS,
		      0,	/* no border */
		      CopyFromParent, InputOnly, CopyFromParent,
		      valuemask, &attributes);
    scr->panner_top.isMapped = scr->panner_left.isMapped =
	scr->panner_right.isMapped = scr->panner_bottom.isMapped = False;
}

/*
 * checkPanFrames hides PanFrames if they are on the very border of the
 * VIRTUAL screen and EdgeWrap for that direction is off. 
 * (A special cursor for the EdgeWrap border could be nice) HEDU
 */
void
PAN_CheckBounds(ScreenInfo *scr)
{

    /* Remove Pan frames if paging by edge-scroll is permanently or
     * temporarily disabled */
    if ((scr->edge_scroll_y == 0) || (DoHandlePageing))
    {
	XUnmapWindow(dpy, scr->panner_top.win);
	scr->panner_top.isMapped = False;
	XUnmapWindow(dpy, scr->panner_bottom.win);
	scr->panner_bottom.isMapped = False;
    }
    if ((scr->edge_scroll_x == 0) || (DoHandlePageing))
    {
	XUnmapWindow(dpy, scr->panner_left.win);
	scr->panner_left.isMapped = False;
	XUnmapWindow(dpy, scr->panner_right.win);
	scr->panner_right.isMapped = False;
    }

    if (scr->virt_x == 0 && scr->panner_left.isMapped)
    {
	XUnmapWindow(dpy, scr->panner_left.win);
	scr->panner_left.isMapped = False;
    }
    else if (scr->virt_x > 0 && scr->panner_left.isMapped == False)
    {
	XMapRaised(dpy, scr->panner_left.win);
	scr->panner_left.isMapped = True;
    }
    if (scr->virt_x == scr->virt_x_max && scr->panner_right.isMapped)
    {
	XUnmapWindow(dpy, scr->panner_right.win);
	scr->panner_right.isMapped = False;
    }
    else if (scr->virt_x < scr->virt_x_max && scr->panner_right.isMapped == False)
    {
	XMapRaised(dpy, scr->panner_right.win);
	scr->panner_right.isMapped = True;
    }
    /* TOP, hide only if EdgeWrap is off */
    if (scr->virt_y == 0 && scr->panner_top.isMapped)
    {
	XUnmapWindow(dpy, scr->panner_top.win);
	scr->panner_top.isMapped = False;
    }
    else if (scr->virt_y > 0 && scr->panner_top.isMapped == False)
    {
	XMapRaised(dpy, scr->panner_top.win);
	scr->panner_top.isMapped = True;
    }
    /* BOTTOM, hide only if EdgeWrap is off */
    if (scr->virt_y == scr->virt_y_max && scr->panner_bottom.isMapped)
    {
	XUnmapWindow(dpy, scr->panner_bottom.win);
	scr->panner_bottom.isMapped = False;
    }
    else if (scr->virt_y < scr->virt_y_max && scr->panner_bottom.isMapped == False)
    {
	XMapRaised(dpy, scr->panner_bottom.win);
	scr->panner_bottom.isMapped = True;
    }
}

/*
 * Gotta make sure these things are on top of everything else, or they
 * don't work!
 */
void
PAN_Raise(ScreenInfo *scr)
{
    if (scr->panner_top.isMapped)
	XRaiseWindow(dpy, scr->panner_top.win);
    if (scr->panner_left.isMapped)
	XRaiseWindow(dpy, scr->panner_left.win);
    if (scr->panner_right.isMapped)
	XRaiseWindow(dpy, scr->panner_right.win);
    if (scr->panner_bottom.isMapped)
	XRaiseWindow(dpy, scr->panner_bottom.win);
}

/*
 * check to see if a window is a panner window
 */
Boolean
PAN_IsPannerWindow(ScreenInfo *scr, Window win)
{
    if (win == scr->panner_top.win || win == scr->panner_left.win ||
	win == scr->panner_right.win || win == scr->panner_bottom.win)
	return True;

    return False;
}

/*
 * Check to see if the pointer is on the edge of the screen, and scroll/page
 * if needed 
 */
void
PAN_PanDesktop(ScreenInfo *scr, int HorWarpSize, int VertWarpSize,
	       int *xl, int *yt, int *delta_x, int *delta_y,
	       Boolean Grab, XEvent *event)
{
    extern Bool DoHandlePageing;
    int x, y, total;

    *delta_x = 0;
    *delta_y = 0;

    if (DoHandlePageing)
    {
	if ((scr->ScrollResistance >= 10000) ||
	    ((HorWarpSize == 0) && (VertWarpSize == 0)))
	    return;

	/* need to move the viewport */
	if ((*xl >= SCROLL_REGION) && (*xl < scr->d_width - SCROLL_REGION) &&
	    (*yt >= SCROLL_REGION) && (*yt < scr->d_height - SCROLL_REGION))
	    return;

	total = 0;
	while (total < scr->ScrollResistance)
	{
	    _XmMicroSleep(10000);
	    total += 10;
	    if (XCheckWindowEvent(dpy, scr->panner_top.win,
				  LeaveWindowMask, event))
	    {
		MISC_StashEventTime(event);
		return;
	    }
	    if (XCheckWindowEvent(dpy, scr->panner_bottom.win,
				  LeaveWindowMask, event))
	    {
		MISC_StashEventTime(event);
		return;
	    }
	    if (XCheckWindowEvent(dpy, scr->panner_left.win,
				  LeaveWindowMask, event))
	    {
		MISC_StashEventTime(event);
		return;
	    }
	    if (XCheckWindowEvent(dpy, scr->panner_right.win,
				  LeaveWindowMask, event))
	    {
		MISC_StashEventTime(event);
		return;
	    }
	}

	XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
		      &x, &y, &JunkX, &JunkY, &JunkMask);

	/* Turn off the rubberband if its on */
	WIN_DrawOutline(scr, scr->root_win, 0, 0, 0, 0);

	/* Move the viewport */
	/* and/or move the cursor back to the approximate correct location */
	/* that is, the same place on the virtual desktop that it */
	/* started at */
	if (x < SCROLL_REGION)
	    *delta_x = -HorWarpSize;
	else if (x >= scr->d_width - SCROLL_REGION)
	    *delta_x = HorWarpSize;
	else
	    *delta_x = 0;
	if (y < SCROLL_REGION)
	    *delta_y = -VertWarpSize;
	else if (y >= scr->d_height - SCROLL_REGION)
	    *delta_y = VertWarpSize;
	else
	    *delta_y = 0;

	/* Ouch! lots of bounds checking */
	if (scr->virt_x + *delta_x < 0)
	{
	    *delta_x = -scr->virt_x;
	    *xl = x - *delta_x;
	}
	else if (scr->virt_x + *delta_x > scr->virt_x_max)
	{
	    *delta_x = scr->virt_x_max - scr->virt_x;
	    *xl = x - *delta_x;
	}
	else
	    *xl = x - *delta_x;

	if (scr->virt_y + *delta_y < 0)
	{
	    *delta_y = -scr->virt_y;
	    *yt = y - *delta_y;
	}
	else if (scr->virt_y + *delta_y > scr->virt_y_max)
	{
	    *delta_y = scr->virt_y_max - scr->virt_y;
	    *yt = y - *delta_y;
	}
	else
	    *yt = y - *delta_y;

	if (*xl <= SCROLL_REGION)
	    *xl = SCROLL_REGION + 1;
	if (*yt <= SCROLL_REGION)
	    *yt = SCROLL_REGION + 1;
	if (*xl >= scr->d_width - SCROLL_REGION)
	    *xl = scr->d_width - SCROLL_REGION - 1;
	if (*yt >= scr->d_height - SCROLL_REGION)
	    *yt = scr->d_height - SCROLL_REGION - 1;

	if ((*delta_x != 0) || (*delta_y != 0))
	{
	    if (Grab)
		XGrabServer(dpy);
	    XWarpPointer(dpy, None, scr->root_win, 0, 0, 0, 0, *xl, *yt);
	    PAGER_MoveViewPort(scr, scr->virt_x + *delta_x, scr->virt_y + *delta_y, False);
	    XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
			  xl, yt, &JunkX, &JunkY, &JunkMask);
	    if (Grab)
		XUngrabServer(dpy);
	}
    }
}
