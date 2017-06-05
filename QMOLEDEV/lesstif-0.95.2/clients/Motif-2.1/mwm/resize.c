/* $Id: resize.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $ */
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

#include <stdio.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>

#include "mwm.h"


static int dragx;		/* all these variables are used */
static int dragy;		/* in resize operations */
static int dragWidth;
static int dragHeight;

static int origx;
static int origy;
static int origWidth;
static int origHeight;

static int ymotion, xmotion;
static int last_width, last_height;

/*
 * display the size in the dimensions window
 */
static void
display_size(ScreenInfo *scr, MwmWindow *tmp_win, int width, int height,
	     Boolean Init)
{
    char str[100];
    int dwidth, dheight, offset, ssw;

    if (last_width == width && last_height == height)
	return;

    ssw = XTextWidth(scr->components[MWM_FEEDBACK].font,
		     " +8888 x +8888 ", 15);
    last_width = width;
    last_height = height;

    dheight = height - tmp_win->title_height -
	2 * tmp_win->boundary_width -
	2 * tmp_win->matte_width;
    dwidth = width - 2 * tmp_win->boundary_width -
	2 * tmp_win->matte_width;

    dwidth -= tmp_win->hints.base_width;
    dheight -= tmp_win->hints.base_height;
    dwidth /= tmp_win->hints.width_inc;
    dheight /= tmp_win->hints.height_inc;

    sprintf(str, " %4d x %-4d ", dwidth, dheight);
    offset = (ssw + SIZE_HINDENT * 2 -
	      XTextWidth(scr->components[MWM_FEEDBACK].font,
			 str, strlen(str))) / 2;

    if (Init)
    {
	XClearWindow(dpy, scr->size_win);
	if (scr->d_depth >= 2)
	    DEC_DrawShadows(tmp_win, scr->size_win,
			    0, 0,
			    ssw + SIZE_HINDENT * 2,
			    scr->components[MWM_FEEDBACK].f_height +
			    SIZE_VINDENT * 2,
			    scr->components[MWM_FEEDBACK].top_GC,
			    scr->components[MWM_FEEDBACK].bot_GC);
    }
    else
    {
	XClearArea(dpy, scr->size_win, SIZE_HINDENT, SIZE_VINDENT,
		   ssw, scr->components[MWM_FEEDBACK].f_height,
		   False);
    }

    XDrawString(dpy, scr->size_win, scr->components[MWM_FEEDBACK].normal_GC,
		offset, scr->components[MWM_FEEDBACK].f_y + SIZE_VINDENT,
		str, 13);

}

/*
 * move the rubberband around.  This is called for each motion event when we
 * are resizing
 */
static void
resize_window(ScreenInfo *scr, int x_root, int y_root, MwmWindow *tmp_win)
{
    int action = 0;
    unsigned int width, height;
    int ww, wh;
    int wx, wy;
    int MaxH, MaxW;
    static int last_w = -10000, last_h = -10000;

    if ((y_root <= origy) || ((ymotion == 1) && (y_root < origy + origHeight - 1)))
    {
	dragy = y_root;
	dragHeight = origy + origHeight - y_root;
	action = 1;
	ymotion = 1;
    }
    else if ((y_root >= origy + origHeight - 1) ||
	     ((ymotion == -1) && (y_root > origy)))
    {
	dragy = origy;
	dragHeight = 1 + y_root - dragy;
	action = 1;
	ymotion = -1;
    }

    if ((x_root <= origx) ||
	((xmotion == 1) && (x_root < origx + origWidth - 1)))
    {
	dragx = x_root;
	dragWidth = origx + origWidth - x_root;
	action = 1;
	xmotion = 1;
    }
    if ((x_root >= origx + origWidth - 1) ||
	((xmotion == -1) && (x_root > origx)))
    {
	dragx = origx;
	dragWidth = 1 + x_root - origx;
	action = 1;
	xmotion = -1;
    }

    if (action)
    {
	WIN_ConstrainWindow(scr, tmp_win, &dragWidth, &dragHeight);
	if (xmotion == 1)
	    dragx = origx + origWidth - dragWidth;
	if (ymotion == 1)
	    dragy = origy + origHeight - dragHeight;

	/* update size of the pager_view window */
	if ((scr->mwm_pager != NULL) &&
	    (dragx < scr->mwm_pager->frame_x + scr->mwm_pager->frame_width) &&
	    (dragx + dragWidth > scr->mwm_pager->frame_x) &&
	    (dragy < scr->mwm_pager->frame_y + scr->mwm_pager->frame_height) &&
	    (dragy + dragHeight > scr->mwm_pager->frame_y) &&
	    ((!(scr->flags & OpaqueResize)) ||
	     ((scr->flags & OpaqueResize) && (!(tmp_win->flags & MAPPED)))))
	    WIN_DrawOutline(scr, scr->root_win, 0, 0, 0, 0);
	if (scr->mwm_pager)
	{
	    width = scr->mwm_pager->frame_width -
		2 * scr->mwm_pager->boundary_width -
		2 * scr->mwm_pager->matte_width;
	    height = scr->mwm_pager->frame_height -
		scr->mwm_pager->title_height -
		2 * scr->mwm_pager->boundary_width -
		2 * scr->mwm_pager->matte_width;

	    MaxW = scr->virt_x_max + scr->d_width;
	    MaxH = scr->virt_y_max + scr->d_height;

	    if (!(tmp_win->flags & STICKY))
	    {
		/* show the actual window */
		wx = (dragx + scr->virt_x) * (int)width / MaxW;
		wy = (dragy + scr->virt_y) * (int)height / MaxH;
		ww = dragWidth * (int)width / MaxW;
		wh = dragHeight * (int)height / MaxH;

		if ((last_w - ww >= 2) || (last_w - ww <= -2) ||
		    (last_h - wh >= 2) || (last_h - wh <= -2))
		{
		    if (ww < 2)
			ww = 2;
		    if (wh < 2)
			wh = 2;
		    XMoveResizeWindow(dpy, tmp_win->pager_view, wx, wy, ww, wh);
		    last_h = wh;
		    last_w = ww;
		}
	    }
	}

	if ((!(scr->flags & OpaqueResize)) ||
	    ((scr->flags & OpaqueResize) && (!(tmp_win->flags & MAPPED))))
	{
	    WIN_DrawOutline(scr, scr->root_win, dragx - tmp_win->bw, dragy - tmp_win->bw,
		    dragWidth + 2 * tmp_win->bw, dragHeight + 2 * tmp_win->bw);
	}
	else
	{
	    DEC_ConfigureDecorations(scr, tmp_win, dragx - tmp_win->bw,
			    dragy - tmp_win->bw, dragWidth, dragHeight, False);
	}
    }
    if (Mwm.show_feedback & MWM_FEEDBACK_RESIZE)
	display_size(scr, tmp_win, dragWidth, dragHeight, False);
}

/*
 * Starts a window resize operation
 */
void
RESIZE_EventLoop(ScreenInfo *scr, Window w, MwmWindow *tmp_win,
		 int val1, int val2, int val1_unit, int val2_unit)
{
    Bool finished = False, done = False;
    int x, y, delta_x, delta_y;
    Window ResizeWindow;
    XEvent oevent;

    if ((w == None) || (tmp_win == NULL))
	return;

    /* Already checked this in functions.c, but its here too incase
     * there's a resize on initial placement. */
    if (tmp_win && !(tmp_win->functions & MWM_FUNC_RESIZE))
    {
	XBell(dpy, scr->screen);
	return;
    }
    /* can't resize icons */
    if (tmp_win->flags & ICONIFIED)
	return;

    ResizeWindow = tmp_win->frame;

    if ((val1 != 0) && (val2 != 0))
    {
	dragWidth = val1 * val1_unit / 100;
	dragHeight = val2 * val2_unit / 100;

	WIN_ConstrainWindow(scr, tmp_win, &dragWidth, &dragHeight);
	DEC_ConfigureDecorations(scr, tmp_win, tmp_win->frame_x,
			       tmp_win->frame_y, dragWidth, dragHeight, False);

	ResizeWindow = None;
	PAGER_Clear(scr);
	return;
    }

    COLOR_PushRootColorMap(scr);
    if (menuFromFrameOrWindowOrTitlebar)
    {
	/* warp the pointer to the cursor position from before menu appeared */
	XWarpPointer(dpy, None, scr->root_win, 0, 0, 0, 0, Stashed_X, Stashed_Y);
	XFlush(dpy);
    }

    if (!MISC_Grab(scr, MOVE_CURS))
    {
	XBell(dpy, scr->screen);
	return;
    }

    if ((!(scr->flags & OpaqueResize)) ||
	((scr->flags & OpaqueResize) && (!(tmp_win->flags & MAPPED))))
	XGrabServer(dpy);


    pagerOn = False;
    XGetGeometry(dpy, (Drawable)ResizeWindow, &JunkRoot,
		 &dragx, &dragy, (unsigned int *)&dragWidth,
		 (unsigned int *)&dragHeight, &JunkBW, &JunkDepth);

    dragx += tmp_win->bw;
    dragy += tmp_win->bw;
    origx = dragx;
    origy = dragy;
    origWidth = dragWidth;
    origHeight = dragHeight;
    ymotion = xmotion = 0;

    /* pop up a resize dimensions window */
    if (Mwm.show_feedback & MWM_FEEDBACK_RESIZE)
	XMapRaised(dpy, scr->size_win);

    last_width = 0;
    last_height = 0;

    if (Mwm.show_feedback & MWM_FEEDBACK_RESIZE)
	display_size(scr, tmp_win, origWidth, origHeight, True);

    /* Get the current position to determine which border to resize */
    if ((scr->pressed_win != scr->root_win) && (scr->pressed_win != None))
    {
	if (scr->pressed_win == tmp_win->sides[0])	/* top */
	    ymotion = 1;
	if (scr->pressed_win == tmp_win->sides[1])	/* right */
	    xmotion = -1;
	if (scr->pressed_win == tmp_win->sides[2])	/* bottom */
	    ymotion = -1;
	if (scr->pressed_win == tmp_win->sides[3])	/* left */
	    xmotion = 1;
	if (scr->pressed_win == tmp_win->corners[0])
	{			/* upper-left */
	    ymotion = 1;
	    xmotion = 1;
	}
	if (scr->pressed_win == tmp_win->corners[1])
	{			/* upper-right */
	    xmotion = -1;
	    ymotion = 1;
	}
	if (scr->pressed_win == tmp_win->corners[2])
	{			/* lower right */
	    ymotion = -1;
	    xmotion = 1;
	}
	if (scr->pressed_win == tmp_win->corners[3])
	{			/* lower left */
	    ymotion = -1;
	    xmotion = -1;
	}
    }
    /* draw the rubber-band window */
    if ((!(scr->flags & OpaqueResize)) ||
	((scr->flags & OpaqueResize) && (!(tmp_win->flags & MAPPED))))
	WIN_DrawOutline(scr, scr->root_win, dragx - tmp_win->bw, dragy - tmp_win->bw, dragWidth + 2 * tmp_win->bw,
			dragHeight + 2 * tmp_win->bw);

    /* loop to resize */
    while (!finished)
    {
	XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask | KeyPressMask |
		 ButtonMotionMask | PointerMotionMask | ExposureMask, &oevent);
	MISC_StashEventTime(&oevent);

	if (oevent.type == MotionNotify)
	    /* discard any extra motion events before a release */
	    while (XCheckMaskEvent(dpy, ButtonMotionMask | ButtonReleaseMask |
				   PointerMotionMask, &oevent))
	    {
		MISC_StashEventTime(&oevent);
		if (oevent.type == ButtonRelease)
		    break;
	    }

	done = False;
	/* Handle a limited number of key press events to allow mouseless
	 * operation */
	if (oevent.type == KeyPress)
	    MISC_KeyboardShortcut(scr, &oevent, ButtonRelease);
	switch (oevent.type)
	{
	case ButtonPress:
	    XAllowEvents(dpy, ReplayPointer, CurrentTime);
	case KeyPress:
	    done = True;
	    break;

	case ButtonRelease:
	    finished = True;
	    done = True;
	    break;

	case MotionNotify:
	    x = oevent.xmotion.x_root;
	    y = oevent.xmotion.y_root;
	    /* need to move the viewport */
	    PAN_PanDesktop(scr, scr->edge_scroll_x, scr->edge_scroll_y, &x, &y,
			   &delta_x, &delta_y, False, &oevent);
	    origx -= delta_x;
	    origy -= delta_y;
	    dragx -= delta_x;
	    dragy -= delta_y;

	    resize_window(scr, x, y, tmp_win);
	    done = True;
	default:
	    break;
	}
	if (!done)
	{
	    if ((!(scr->flags & OpaqueResize)) ||
		((scr->flags & OpaqueResize) && (!(tmp_win->flags & MAPPED))))
		WIN_DrawOutline(scr, scr->root_win, 0, 0, 0, 0);

	    EVENT_Dispatch(&oevent);

	    if ((!(scr->flags & OpaqueResize)) ||
		((scr->flags & OpaqueResize) && (!(tmp_win->flags & MAPPED))))
		WIN_DrawOutline(scr, scr->root_win, dragx - tmp_win->bw, dragy - tmp_win->bw,
		    dragWidth + 2 * tmp_win->bw, dragHeight + 2 * tmp_win->bw);

	}
    }

    /* erase the rubber-band */
    if ((!(scr->flags & OpaqueResize)) ||
	((scr->flags & OpaqueResize) && (!(tmp_win->flags & MAPPED))))
	WIN_DrawOutline(scr, scr->root_win, 0, 0, 0, 0);

    /* pop down the size window */
    if (Mwm.show_feedback & MWM_FEEDBACK_RESIZE)
	XUnmapWindow(dpy, scr->size_win);

    pagerOn = True;
    WIN_ConstrainWindow(scr, tmp_win, &dragWidth, &dragHeight);
    DEC_ConfigureDecorations(scr, tmp_win, dragx - tmp_win->bw,
			     dragy - tmp_win->bw, dragWidth, dragHeight, False);

    COLOR_PopRootColorMap(scr);
    ResizeWindow = None;
    XUngrabServer(dpy);
    MISC_Ungrab(scr);
    PAGER_Clear(scr);
}
