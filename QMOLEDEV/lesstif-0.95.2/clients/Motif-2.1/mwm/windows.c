/* $Id: windows.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $ */
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
 * THIS module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 ****************************************************************************/
/****************************************************************************
 * The placement code is by Rob Nation 
 *
 * This code does smart-placement initial window placement stuff
 *
 * Copyright 1994 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved . No guarantees or
 * warrantees of any sort whatsoever are given or implied or anything.
 ****************************************************************************/
/***********************************************************************
 * The rest of it is all my fault -- MLM
 * mwm - "LessTif Window Manager"
 ***********************************************************************/


#include <LTconfig.h>

#include <stdio.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <X11/Intrinsic.h>
#include <X11/extensions/shape.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>

#include "mwm.h"

/*
 * some macros used by the constraint code
 */
#define makemult(a,b) ((b==1) ? (a) : (((int)((a)/(b))) * (b)) )
#define _min(a,b) (((a) < (b)) ? (a) : (b))
#define MaxAspectX(t) ((t)->hints.max_aspect.x)
#define MaxAspectY(t) ((t)->hints.max_aspect.y)
#define MinAspectX(t) ((t)->hints.min_aspect.x)
#define MinAspectY(t) ((t)->hints.min_aspect.y)

static Boolean PPosOverride = False;
static long isIconicState = 0;

char NoName[] = "Untitled";	/* name if no name is specified */

/*
 * check to see if we should really put a mwm frame on the window
 */
static Boolean
mapped_not_override(ScreenInfo *scr, Window w)
{
    XWindowAttributes wa;
    Atom atype;
    int aformat;
    unsigned long nitems, bytes_remain;
    unsigned char *prop;

    isIconicState = DontCareState;

    if (!XGetWindowAttributes(dpy, w, &wa))
	return False;

    if (XGetWindowProperty(dpy, w, XA_WM_STATE, 0L, 3L, False, XA_WM_STATE,
		   &atype, &aformat, &nitems, &bytes_remain, &prop) == Success)
    {
	if (prop != NULL)
	{
	    isIconicState = *(long *)prop;
	    XFree(prop);
	}
    }

    if (w == scr->pager_win)
	return True;

    if ((isIconicState == IconicState || wa.map_state != IsUnmapped) &&
	wa.override_redirect != True)
	return True;

    return False;
}

/*
 * map gravity to (x,y) offset signs for adding to x and y when window is
 * mapped to get proper placement.
 */
static void
get_gravity_offsets(MwmWindow *tmp, int *xp, int *yp)
{
    static struct _gravity_offset
    {
	int x, y;
    }
    gravity_offsets[11] =
    {
	{
	    0, 0
	}
	,			/* ForgetGravity */
	{
	    -1, -1
	}
	,			/* NorthWestGravity */
	{
	    0, -1
	}
	,			/* NorthGravity */
	{
	    1, -1
	}
	,			/* NorthEastGravity */
	{
	    -1, 0
	}
	,			/* WestGravity */
	{
	    0, 0
	}
	,			/* CenterGravity */
	{
	    1, 0
	}
	,			/* EastGravity */
	{
	    -1, 1
	}
	,			/* SouthWestGravity */
	{
	    0, 1
	}
	,			/* SouthGravity */
	{
	    1, 1
	}
	,			/* SouthEastGravity */
	{
	    0, 0
	}
	,			/* StaticGravity */
    };
    register int g = ((tmp->hints.flags & PWinGravity)
		      ? tmp->hints.win_gravity : NorthWestGravity);

    if (g < ForgetGravity || g > StaticGravity)
	*xp = *yp = 0;
    else
    {
	*xp = (int)gravity_offsets[g].x;
	*yp = (int)gravity_offsets[g].y;
    }
}

/*
 * grab needed buttons for the window
 */
static void
grab_buttons(ScreenInfo *scr, MwmWindow *tmp_win)
{
    MouseButton *MouseEntry;

    MouseEntry = scr->buttons;
    while (MouseEntry != (MouseButton *)0)
    {
	if ((MouseEntry->func != (int)0) && (MouseEntry->context & C_WINDOW))
	{
	    if (MouseEntry->button > 0)
	    {
		XGrabButton(dpy, MouseEntry->button, MouseEntry->modifier,
			    tmp_win->w,
			    True, ButtonPressMask | ButtonReleaseMask,
			    GrabModeAsync, GrabModeAsync, None,
			    scr->cursors[DEFAULT_CURS]);
		if (MouseEntry->modifier != AnyModifier)
		{
		    XGrabButton(dpy, MouseEntry->button,
				(MouseEntry->modifier | LockMask),
				tmp_win->w,
				True, ButtonPressMask | ButtonReleaseMask,
				GrabModeAsync, GrabModeAsync, None,
				scr->cursors[DEFAULT_CURS]);
		}
	    }
	    else
	    {
		XGrabButton(dpy, 1, MouseEntry->modifier,
			    tmp_win->w,
			    True, ButtonPressMask | ButtonReleaseMask,
			    GrabModeAsync, GrabModeAsync, None,
			    scr->cursors[DEFAULT_CURS]);
		XGrabButton(dpy, 2, MouseEntry->modifier,
			    tmp_win->w,
			    True, ButtonPressMask | ButtonReleaseMask,
			    GrabModeAsync, GrabModeAsync, None,
			    scr->cursors[DEFAULT_CURS]);
		XGrabButton(dpy, 3, MouseEntry->modifier,
			    tmp_win->w,
			    True, ButtonPressMask | ButtonReleaseMask,
			    GrabModeAsync, GrabModeAsync, None,
			    scr->cursors[DEFAULT_CURS]);
		if (MouseEntry->modifier != AnyModifier)
		{
		    XGrabButton(dpy, 1,
				(MouseEntry->modifier | LockMask),
				tmp_win->w,
				True, ButtonPressMask | ButtonReleaseMask,
				GrabModeAsync, GrabModeAsync, None,
				scr->cursors[DEFAULT_CURS]);
		    XGrabButton(dpy, 2,
				(MouseEntry->modifier | LockMask),
				tmp_win->w,
				True, ButtonPressMask | ButtonReleaseMask,
				GrabModeAsync, GrabModeAsync, None,
				scr->cursors[DEFAULT_CURS]);
		    XGrabButton(dpy, 3,
				(MouseEntry->modifier | LockMask),
				tmp_win->w,
				True, ButtonPressMask | ButtonReleaseMask,
				GrabModeAsync, GrabModeAsync, None,
				scr->cursors[DEFAULT_CURS]);
		}
	    }
	}
	MouseEntry = MouseEntry->next;
    }
}

/*
 * grab needed keys for the window
 */
static void
grab_keys(ScreenInfo *scr, MwmWindow *tmp_win)
{
    FuncKey *tmp;

    for (tmp = scr->keys; tmp != NULL; tmp = tmp->next)
    {
	if (tmp->cont & (C_WINDOW | C_TITLE | C_RALL | C_LALL | C_FRAME))
	{
	    XGrabKey(dpy, tmp->keycode, tmp->mods, tmp_win->frame, True,
		     GrabModeAsync, GrabModeAsync);
	    if (tmp->mods != AnyModifier)
	    {
		XGrabKey(dpy, tmp->keycode, tmp->mods | LockMask,
			 tmp_win->frame, True,
			 GrabModeAsync, GrabModeAsync);
	    }
	}
    }
}

/*
 * set the contexts for the various windows
 */
static void
save_context(MwmWindow *tmp_win)
{
    int i;

    XSaveContext(dpy, tmp_win->w, MwmContext, (XPointer)tmp_win);
    XSaveContext(dpy, tmp_win->frame, MwmContext, (XPointer)tmp_win);
    XSaveContext(dpy, tmp_win->parent, MwmContext, (XPointer)tmp_win);
    if (tmp_win->decorations & MWM_DECOR_TITLE)
    {
	XSaveContext(dpy, tmp_win->title, MwmContext,
		     (XPointer)tmp_win);
	if (tmp_win->menub != None)
	    XSaveContext(dpy, tmp_win->menub, MwmContext,
			 (XPointer)tmp_win);
	if (tmp_win->minimizeb != None)
	    XSaveContext(dpy, tmp_win->minimizeb, MwmContext,
			 (XPointer)tmp_win);
	if (tmp_win->maximizeb != None)
	    XSaveContext(dpy, tmp_win->maximizeb, MwmContext,
			 (XPointer)tmp_win);
    }
    if (tmp_win->decorations & MWM_DECOR_BORDER)
    {
	for (i = 0; i < 4; i++)
	{
	    XSaveContext(dpy, tmp_win->sides[i],
			 MwmContext, (XPointer)tmp_win);
	}
    }
    if (tmp_win->decorations & MWM_DECOR_RESIZEH)
    {
	for (i = 0; i < 4; i++)
	{
	    XSaveContext(dpy, tmp_win->corners[i],
			 MwmContext, (XPointer)tmp_win);
	}
    }
}

/*
 * The function below, in its previous life, tried to allocate a window
 * such that it wouldn't overlap with others. I guess this comes from fvwm.
 * If the result (x and y) are negative, our caller will prompt the user
 * to let him interactively position the window.
 *
 * As far as I know mwm isn't supposed to behave like this so I really
 * disabled that functionality.
 *
 * Danny 19/4/1997
 *
 * Converted this to a resource file setting.  MLM (sometime in August 97).
 */

#define	INC	25
/*
 * try to be smart about how to place the window
 */
static void
smart_placement(ScreenInfo *scr, MwmWindow *t,
		int width, int height, int *x, int *y)
{
    int temp_h, temp_w;
    int test_x = 0, test_y = 0;
    int loc_ok = False, tw, tx, ty, th;
    MwmWindow *test_window;
    static int last_x = 0, last_y = 0, begin_x = INC, begin_y = INC;

    temp_h = height;
    temp_w = width;

    if (!scr->smart_placement)
    {
	if (t == NULL)
	{
	    test_x = test_y = -1;
	}
	else
	{
	    test_x = t->frame_x;
	    test_y = t->frame_y;

	    /* If the user specified a position, grant it */
	    if (t->hints.flags & USPosition)
	    {
		*x = test_x;
		*y = test_y;
		return;
	    }

	    /* Otherwise, we need to make up a position.

	     * Let's start near the top left of the screen, always move a bit
	     * to the lower right with each next window, until a window would
	     * fall of the screen either on the right or on the bottom edge.
	     * When that happens, start near the upper left again.
	     * The upper left starting point should change somewhat too.
	     */
#if 0
	    if (test_x == 0 && test_y == 0)
#endif
	    {
		last_x += INC;
		last_y += INC;

		test_x = last_x;
		test_y = last_y;

		if (t->frame_width + last_x >= scr->d_width
		    || t->frame_height + last_y >= scr->d_height)
		{
		    begin_x += INC;
		    begin_y = INC;

		    test_x = last_x = begin_x;
		    test_y = last_y = begin_y;

		    if (begin_x > scr->d_width / 2)
			begin_x = begin_y = INC;
		}

		/* Is this window simply too big ? */
		if (t->frame_width + last_x >= scr->d_width)
		    test_x = 0;
		if (t->frame_height + last_y >= scr->d_height)
		    test_y = 0;

	    }
	}
    }
    /* smart placement */
    else
    {
	while (((test_y + temp_h) < (scr->d_height)) && (!loc_ok))
	{
	    test_x = 0;
	    while (((test_x + temp_w) < (scr->d_width)) && (!loc_ok))
	    {
		loc_ok = True;
		test_window = scr->mwm_root.next;
		while ((test_window != (MwmWindow *)0) && (loc_ok == True))
		{
		    if (test_window->Desk == scr->current_desk)
		    {
			if (scr->flags & StubbornPlacement)
			{
			    if ((test_window->flags & ICONIFIED) &&
				(!(test_window->flags & ICON_UNMAPPED)) &&
				(test_window->icon_w) &&
				(test_window != t))
			    {
				tw = test_window->icon_p_width;
				th = test_window->icon_p_height +
				    test_window->icon_w_height;
				tx = test_window->icon_x_loc;
				ty = test_window->icon_y_loc;

				if ((tx < (test_x + width)) && ((tx + tw) > test_x) &&
				    (ty < (test_y + height)) && ((ty + th) > test_y))
				{
				    loc_ok = False;
				    test_x = tx + tw;
				}
			    }
			}

			if (!(test_window->flags & ICONIFIED) && (test_window != t))
			{
			    tw = test_window->frame_width + 2 * test_window->bw;
			    th = test_window->frame_height + 2 * test_window->bw;
			    tx = test_window->frame_x;
			    ty = test_window->frame_y;
			    if ((tx <= (test_x + width)) && ((tx + tw) >= test_x) &&
			    (ty <= (test_y + height)) && ((ty + th) >= test_y))
			    {
				loc_ok = False;
				test_x = tx + tw;
			    }
			}
		    }
		    test_window = test_window->next;
		}
		test_x += 1;
	    }
	    test_y += 1;
	}
	if (loc_ok == False)
	{
	    *x = -1;
	    *y = -1;
	    return;
	}
    }
    *x = test_x;
    *y = test_y;
}

/*
 * Handles initial placement and sizing of a new window. Returns False in
 * the event of a lost window.
 */
static Boolean
place_window(ScreenInfo *scr, MwmWindow *tmp_win)
{
    MwmWindow *t;
    int xl = -1, yt, DragWidth, DragHeight;
    int gravx, gravy;		/* gravity signs for positioning */
    int show_feed;
    Boolean usePPos;

    get_gravity_offsets(tmp_win, &gravx, &gravy);


    /* Select a desk to put the window on (in list of priority):
     * 1. Sticky Windows stay on the current desk.
     * 2. Windows specified with StartsOnDesk go where specified
     * 3. Put it on the desk it was on before the restart.
     * 4. Transients go on the same desk as their parents.
     * 5. Window groups stay together (completely untested)
     */
    tmp_win->Desk = scr->current_desk;
    if (tmp_win->flags & STICKY)
	tmp_win->Desk = scr->current_desk;
    else
    {
	Atom atype;
	int aformat;
	unsigned long nitems, bytes_remain;
	unsigned char *prop;

	if ((tmp_win->wmhints) && (tmp_win->wmhints->flags & WindowGroupHint) &&
	    (tmp_win->wmhints->window_group != None) &&
	    (tmp_win->wmhints->window_group != scr->root_win))
	{
	    /* Try to find the group leader or another window
	     * in the group */
	    for (t = scr->mwm_root.next; t != NULL; t = t->next)
	    {
		if ((t->w == tmp_win->wmhints->window_group) ||
		    ((t->wmhints) && (t->wmhints->flags & WindowGroupHint) &&
		 (t->wmhints->window_group == tmp_win->wmhints->window_group)))
		    tmp_win->Desk = t->Desk;
	    }
	}
	if ((tmp_win->flags & TRANSIENT) && (tmp_win->transientfor != None) &&
	    (tmp_win->transientfor != scr->root_win))
	{
	    /* Try to find the parent's desktop */
	    for (t = scr->mwm_root.next; t != NULL; t = t->next)
	    {
		if (t->w == tmp_win->transientfor)
		    tmp_win->Desk = t->Desk;
	    }
	}

	if ((XGetWindowProperty(dpy, tmp_win->w, XA_WM_DESKTOP, 0L, 1L, True,
				XA_WM_DESKTOP, &atype, &aformat, &nitems,
				&bytes_remain, &prop)) == Success)
	{
	    if (prop != NULL)
	    {
		tmp_win->Desk = *(unsigned long *)prop;
		XFree(prop);
	    }
	}
    }
    /* I think it would be good to switch to the selected desk
     * whenever a new window pops up, except during initialization */
    if (!PPosOverride)
	DT_ChangeDesks(scr, 0, tmp_win->Desk);


    /* Desk has been selected, now pick a location for the window */
    /*
     *  If
     *     o  the window is a transient, or
     * 
     *     o  a USPosition was requested
     * 
     *   then put the window where requested.
     *
     */
#if 0
    fprintf(stderr,
	    "PlaceWindow: UsePPosition = %s, Hints %s%s, attr x %d y %d\n",
       (tmp_win->use_p_position == XmUSE_PPOSITION_ON) ? "XmUSE_PPOSITION_ON" :
	    (tmp_win->use_p_position == XmUSE_PPOSITION_NON_ZERO)
	    ? "XmUSE_PPOSITION_NON_ZERO" : "???",
	    (tmp_win->hints.flags & USPosition) ? "USPosition" : "",
	    (tmp_win->hints.flags & PPosition) ? "PPosition" : "",
	    tmp_win->attr.x, tmp_win->attr.y);
#endif

    if (tmp_win->use_p_position == XmUSE_PPOSITION_ON &&
	(tmp_win->hints.flags & USPosition ||
	 tmp_win->hints.flags & PPosition))
	usePPos = True;
    else if (tmp_win->use_p_position == XmUSE_PPOSITION_NON_ZERO &&
	     (tmp_win->hints.flags & USPosition ||
	      tmp_win->hints.flags & PPosition) &&
	     (tmp_win->attr.x != 0 || tmp_win->attr.y != 0))
	usePPos = True;
    else
	usePPos = False;

    if (!(tmp_win->flags & TRANSIENT) &&
	!(PPosOverride) &&
	!(usePPos) &&
	!((tmp_win->wmhints) &&
	  (tmp_win->wmhints->flags & StateHint) &&
	  (tmp_win->wmhints->initial_state == IconicState)))
    {
	/* Get user's window placement */
	xl = -1;
	yt = -1;
	if (Mwm.client_auto_place)
	    smart_placement(scr, tmp_win,
			    tmp_win->frame_width + 2 * tmp_win->bw,
			    tmp_win->frame_height + 2 * tmp_win->bw,
			    &xl, &yt);
	if (xl < 0)
	{

	    if (MISC_Grab(scr, POSITION_CURS))
	    {

		/* Grabbed the pointer - continue */
		XGrabServer(dpy);

		if (XGetGeometry(dpy, tmp_win->w, &JunkRoot, &JunkX, &JunkY,
				 (unsigned int *)&DragWidth,
				 (unsigned int *)&DragHeight,
				 &JunkBW, &JunkDepth) == 0)
		{
		    XtFree((char *)tmp_win);
		    XUngrabServer(dpy);
		    return False;
		}

		DragWidth += 2 * tmp_win->boundary_width +
		    2 * tmp_win->matte_width;
		DragHeight +=
		    tmp_win->title_height +
		    2 * tmp_win->boundary_width +
		    2 * tmp_win->matte_width;

		if (Mwm.show_feedback & MWM_FEEDBACK_PLACEMENT)
		    XMapRaised(dpy, scr->size_win);

		show_feed = Mwm.show_feedback & MWM_FEEDBACK_MOVE;
		if (!(Mwm.show_feedback & MWM_FEEDBACK_PLACEMENT))
		    Mwm.show_feedback &= ~MWM_FEEDBACK_MOVE;

		MOVE_EventLoop(scr, tmp_win, 0, 0, DragWidth, DragHeight,
			       &xl, &yt, False, True);

		if (Mwm.show_feedback & MWM_FEEDBACK_PLACEMENT)
		    XUnmapWindow(dpy, scr->size_win);

		if (show_feed)
		    Mwm.show_feedback |= MWM_FEEDBACK_MOVE;

		XUngrabServer(dpy);

		MISC_Ungrab(scr);
	    }
	    else
	    {
		/* couldn't grab the pointer - better do something */
		XBell(dpy, scr->screen);
		xl = 0;
		yt = 0;
	    }
	}
	tmp_win->attr.y = yt - tmp_win->old_bw + tmp_win->bw;
	tmp_win->attr.x = xl - tmp_win->old_bw + tmp_win->bw;
	tmp_win->xdiff = xl;
	tmp_win->ydiff = yt;
    }
    else
    {
	/* the USPosition was specified, or the window is a transient, 
	 * or it starts iconic so place it automatically */

	tmp_win->xdiff = tmp_win->attr.x;
	tmp_win->ydiff = tmp_win->attr.y;
	/* put it where asked, mod title bar */
	/* if the gravity is towards the top, move it by the title height */
	tmp_win->attr.y -= gravy * (tmp_win->bw - tmp_win->old_bw);
	tmp_win->attr.x -= gravx * (tmp_win->bw - tmp_win->old_bw);
	if (gravy > 0)
	    tmp_win->attr.y -= tmp_win->title_height +
		2 * tmp_win->boundary_width +
		2 * tmp_win->matte_width;
	if (gravx > 0)
	    tmp_win->attr.x -= 2 * tmp_win->boundary_width +
		2 * tmp_win->matte_width;
    }
    return True;
}

/*
 * add a new window to the mwm list
 */
static MwmWindow *
add_window(ScreenInfo *scr, Window w)
{
    MwmWindow *tmp_win;		/* new mwm window structure */
    int i, width, height, tx, ty;
    int xws, yws, xbs, ybs;
    unsigned wws, hws, wbs, hbs;
    int boundingShaped, clipShaped;
    XTextProperty text_prop;

    NeedToResizeToo = False;

    /* allocate space for the mwm window */
    tmp_win = (MwmWindow *)XtCalloc(1, sizeof(MwmWindow));
    if (tmp_win == NULL)
    {
	return NULL;
    }

    tmp_win->w = w;

    tmp_win->classhint.res_name = NoName;
    tmp_win->classhint.res_class = NoName;
    XGetClassHint(dpy, tmp_win->w, &tmp_win->classhint);
    if (tmp_win->classhint.res_name == NULL)
	tmp_win->classhint.res_name = NoName;
    if (tmp_win->classhint.res_class == NULL)
	tmp_win->classhint.res_class = NoName;

    RES_GetClientDefaults(scr, tmp_win, tmp_win->classhint.res_name, tmp_win->classhint.res_class);

    if (XGetGeometry(dpy, tmp_win->w, &JunkRoot, &JunkX, &JunkY,
		     &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0)
    {
	XtFree((char *)tmp_win);
	return (NULL);
    }

    PROP_GetWmProtocols(tmp_win);

    PROP_GetWmColormapWindows(tmp_win);

    if (!(XGetWindowAttributes(dpy, tmp_win->w, &(tmp_win->attr))))
	tmp_win->attr.colormap = scr->mwm_root.attr.colormap;

    if (XGetWMName(dpy, tmp_win->w, &text_prop) != 0)
	tmp_win->name = (char *)text_prop.value;
    else
	tmp_win->name = NoName;

    tmp_win->wmhints = XGetWMHints(dpy, tmp_win->w);

    if (XGetTransientForHint(dpy, tmp_win->w, &tmp_win->transientfor))
	tmp_win->flags |= TRANSIENT;
    else
	tmp_win->flags &= ~TRANSIENT;

    tmp_win->old_bw = tmp_win->attr.border_width;

    XShapeSelectInput(dpy, tmp_win->w, ShapeNotifyMask);

    XShapeQueryExtents(dpy, tmp_win->w,
		       &boundingShaped, &xws, &yws, &wws, &hws,
		       &clipShaped, &xbs, &ybs, &wbs, &hbs);
    tmp_win->wShaped = boundingShaped;

    tmp_win->title_height = scr->components[MWM_TITLE_A].f_height + 3 +
	tmp_win->bw;

    PROP_GetWmIconName(tmp_win);

    PROP_GetMwmHints(tmp_win);

    DEC_SelectDecorations(scr, tmp_win);

    if ((tmp_win->wmhints)
	&& (tmp_win->wmhints->flags & (IconWindowHint | IconPixmapHint)))
    {
	/* window has its own icon */
	tmp_win->icon_bitmap_file = NULL;
    }
    /* use default icon if nothing specified */
    else if (tmp_win->icon_image == NULL)
	tmp_win->icon_bitmap_file = scr->DefaultIcon;

    PROP_GetWindowSizeHints(tmp_win);

    /* Tentative size estimate */
    tmp_win->frame_width = tmp_win->attr.width +
	2 * tmp_win->boundary_width +
	2 * tmp_win->matte_width;
    tmp_win->frame_height = tmp_win->attr.height +
	tmp_win->title_height +
	2 * tmp_win->boundary_width +
	2 * tmp_win->matte_width;

    WIN_ConstrainWindow(scr, tmp_win,
			&tmp_win->frame_width, &tmp_win->frame_height);

    if (!place_window(scr, tmp_win))
	return NULL;

    /*
     * Make sure the client window still exists.  We don't want to leave an
     * orphan frame window if it doesn't.  Since we now have the server
     * grabbed, the window can't disappear later without having been
     * reparented, so we'll get a DestroyNotify for it.  We won't have
     * gotten one for anything up to here, however.
     */
    XGrabServer(dpy);

    if (XGetGeometry(dpy, w, &JunkRoot, &JunkX, &JunkY,
		     &JunkWidth, &JunkHeight,
		     &JunkBW, &JunkDepth) == 0)
    {
	XtFree((char *)tmp_win);
	XUngrabServer(dpy);
	return (NULL);
    }

    XSetWindowBorderWidth(dpy, tmp_win->w, 0);

    if (tmp_win->icon_label == NULL)
	tmp_win->icon_label = tmp_win->classhint.res_name;
    tmp_win->icon_active_label = tmp_win->name;

    tmp_win->flags &= ~ICONIFIED;
    tmp_win->flags &= ~ICON_UNMAPPED;
    tmp_win->flags &= ~MAXIMIZED;

    /* add the window into the mwm list */
    MISC_AddToTree(scr, tmp_win);

    DEC_CreateDecorations(scr, tmp_win);

    if (XGetWMName(dpy, tmp_win->w, &text_prop) != 0)
	tmp_win->name = (char *)text_prop.value;
    else
	tmp_win->name = NoName;

    if (tmp_win->w != scr->pager_win && tmp_win->w != scr->restart_win &&
	tmp_win->w != scr->quit_win && tmp_win->w != scr->toggle_win)
	XAddToSaveSet(dpy, tmp_win->w);

    /*
     * Reparenting generates an UnmapNotify event, followed by a MapNotify.
     * Set the map state to False to prevent a transition back to
     * WithdrawnState in HandleUnmapNotify.  Map state gets set correctly
     * again in HandleMapNotify.
     */
    tmp_win->flags &= ~MAPPED;
    width = tmp_win->frame_width;
    tmp_win->frame_width = 0;
    height = tmp_win->frame_height;
    tmp_win->frame_height = 0;

    DEC_ConfigureDecorations(scr, tmp_win, tmp_win->frame_x, tmp_win->frame_y,
			     width, height, True);

    /* wait until the window is iconified and the icon window is mapped
     * before creating the icon window 
     */
    tmp_win->icon_w = None;

    grab_buttons(scr, tmp_win);
    grab_keys(scr, tmp_win);

    save_context(tmp_win);

    PROP_GetMwmMenu(tmp_win);

    MENU_BuildWindowMenu(scr, tmp_win);

    PROP_GetMwmMessages(tmp_win);

    WIN_Raise(scr, tmp_win);

    XUngrabServer(dpy);

    XGetGeometry(dpy, tmp_win->w, &JunkRoot, &JunkX, &JunkY,
		 &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);
    XTranslateCoordinates(dpy, tmp_win->frame, scr->root_win, JunkX, JunkY,
			  &tx, &ty, &JunkChild);
    tmp_win->xdiff -= tx;
    tmp_win->ydiff -= ty;

    if (Mwm.keyboard_focus_policy == XmEXPLICIT)
    {
	/* need to grab all buttons for window that we are about to
	 * unhighlight */
	for (i = 0; i < 3; i++)
	    if (scr->buttons2grab & (1 << i))
	    {
		XGrabButton(dpy, (i + 1), 0, tmp_win->frame, True,
			    ButtonPressMask, GrabModeSync, GrabModeAsync, None,
			    scr->cursors[SYS_CURS]);
		XGrabButton(dpy, (i + 1), LockMask, tmp_win->frame, True,
			    ButtonPressMask, GrabModeSync, GrabModeAsync, None,
			    scr->cursors[SYS_CURS]);
	    }
    }

    PROP_GetWmProtocols(tmp_win);
    PROP_GetWmColormapWindows(tmp_win);

    if (!(XGetWindowAttributes(dpy, tmp_win->w, &(tmp_win->attr))))
	tmp_win->attr.colormap = scr->mwm_root.attr.colormap;

    if (NeedToResizeToo)
    {
	int show_feed;

	XWarpPointer(dpy, scr->root_win, scr->root_win, 0, 0, scr->d_width,
		     scr->d_height,
		     tmp_win->frame_x + (tmp_win->frame_width >> 1),
		     tmp_win->frame_y + (tmp_win->frame_height >> 1));

	show_feed = Mwm.show_feedback & MWM_FEEDBACK_RESIZE;

	if (!(Mwm.show_feedback & MWM_FEEDBACK_PLACEMENT))
	    Mwm.show_feedback &= ~MWM_FEEDBACK_RESIZE;

	RESIZE_EventLoop(scr, tmp_win->w, tmp_win, 0, 0, 0, 0);

	if (show_feed)
	    Mwm.show_feedback |= MWM_FEEDBACK_RESIZE;
    }

    COLOR_InstallWindowColorMap(scr, scr->mwm_colormap);

    return (tmp_win);
}

/*
 * release a window and the subs
 */
static void
release_window(ScreenInfo *scr, MwmWindow *win)
{
    MwmWindow *tmp;

    for (tmp = win->child; tmp != NULL; tmp = tmp->next)
	release_window(scr, tmp);

    XUnmapWindow(dpy, win->frame);
    WIN_RestoreWithdrawn(scr, win, True);
    XDestroyWindow(dpy, win->frame);
}

/*
 * count the transient children of a window
 */
static int
count_transients(ScreenInfo *scr, MwmWindow *win)
{
    MwmWindow *tmp;
    int count = 0;

    for (tmp = win->child; tmp != NULL; tmp = tmp->next)
    {
	count++;
	count += count_transients(scr, tmp);
	if ((scr->pager_win) && !(tmp->flags & STICKY))
	    XRaiseWindow(dpy, tmp->pager_view);
	if ((tmp->flags & ICONIFIED))
	{
	    count += 2;
	}
    }

    return count;
}

/*
 * gather up the transients
 */
static void
gather_transients(MwmWindow *win, Window *wins, int *count)
{
    MwmWindow *tmp;

    for (tmp = win->child; tmp != NULL; tmp = tmp->next)
	gather_transients(tmp, wins, count);

    for (tmp = win->child; tmp != NULL; tmp = tmp->next)
    {
	wins[(*count)++] = tmp->frame;
	if ((tmp->flags & ICONIFIED))
	{
	    wins[(*count)++] = tmp->icon_w;
	    wins[(*count)++] = tmp->icon_pixmap_w;
	}
    }
}

/*
 * lower our children before ourselves
 */
static void
lower_children(MwmWindow *win)
{
    MwmWindow *tmp;

    if (win == NULL)
	return;

    for (tmp = win; tmp != NULL; tmp = tmp->next)
    {
	if (tmp->child)
	    lower_children(tmp->child);
	XLowerWindow(dpy, tmp->w);
    }
}

/*
 * Decorates all windows at start-up
 */
void
WIN_CaptureWindows(ScreenInfo *scr)
{
    unsigned int i, j;
    unsigned int nchildren;
    Window root, parent, *children;

    PPosOverride = True;

    if (!XQueryTree(dpy, scr->root_win, &root, &parent, &children, &nchildren))
	return;

    /*
     * weed out icon windows
     */
    for (i = 0; i < nchildren; i++)
    {
	if (children[i])
	{
	    XWMHints *wmhintsp = XGetWMHints(dpy, children[i]);

	    if (wmhintsp)
	    {
		if (wmhintsp->flags & IconWindowHint)
		{
		    for (j = 0; j < nchildren; j++)
		    {
			if (children[j] == wmhintsp->icon_window)
			{
			    children[j] = None;
			    break;
			}
		    }
		}
		XFree((char *)wmhintsp);
	    }
	}
    }

    /*
     * map all of the non-override windows
     */
    for (i = 0; i < nchildren; i++)
    {
	if (children[i] && mapped_not_override(scr, children[i]))
	{
	    /* Why the unmap? MLM */
	    /* Answering my own question:  if the unmap isn't there, when
	     * the window manager restarts, any transients lying around never
	     * get they're MAPPED flag set (no map event, so they don't go
	     * through normal channels.  I'm not happy with this, but for now
	     * I'm going to keep it.  Actually, it should be ok as long as
	     * any transient children aren't modal.  If they are, we'll need
	     * to fixup the window trees based on that modality after the
	     * capture procedure.
	     */
	    XUnmapWindow(dpy, children[i]);
	    WIN_MapWindow(scr, children[i]);
	}
    }

    isIconicState = DontCareState;

    if (nchildren > 0)
	XFree((char *)children);

    /* after the windows already on the screen are in place,
     * don't use PPosition */
    PPosOverride = False;
}

/*
 * release window decorations when exiting
 */
void
WIN_ReleaseWindows(ScreenInfo *scr)
{
    MwmWindow *tmp;		/* temp mwm window structure */

    /*
     * remove the frame components from all the windows
     */
    XGrabServer(dpy);

    if (scr->pager_win != None)
	XDestroyWindow(dpy, scr->pager_win);

    COLOR_InstallWindowColorMap(scr, &scr->mwm_root);	/* force reinstall */
    for (tmp = scr->mwm_root.next; tmp != NULL; tmp = tmp->next)
	release_window(scr, tmp);

    MENU_DestroyMenus(scr);

    XUngrabServer(dpy);
    XSetInputFocus(dpy, PointerRoot, RevertToPointerRoot, CurrentTime);
    XSync(dpy, 0);
}

/*
 * find the MwmWindow structure associated with a Window
 */
MwmWindow *
WIN_WindowToStruct(ScreenInfo *scr, Window target)
{
    MwmWindow *t, *tmp_win = 0;

    tmp_win = NULL;
    for (t = scr->mwm_root.next; t != NULL; t = t->next)
    {
	if (t->pager_view == target)
	{
	    tmp_win = t;
	}
    }
    return tmp_win;
}

/* 
 * set the focus window in a window tree
 */
void
WIN_SetFocusInTree(MwmWindow *leaf)
{
    MwmWindow *root;

    root = MISC_RootOfTree(leaf);

    if (root)
	root->focus_in_tree = leaf;
}

/*
 * Sets the input focus to the indicated window.
 */
void
WIN_SetFocus(ScreenInfo *scr, Window w, MwmWindow *Fw)
{
    int i;

    /* XmEXPLICIT keyboard focus policy queue manipulation */
    if (Fw && Fw != scr->mwm_focus && Fw != &scr->mwm_root)
    {
	MwmWindow *anc;

	anc = Fw->ancestor;

	MISC_RemoveFromTree(scr, Fw);
	MISC_AddToTree(scr, Fw);

	anc = MISC_RootOfTree(Fw);

	if (Fw != anc->focus_in_tree)
	{
	    Fw = anc->focus_in_tree;
	    /* Without the if statement below, we can crash here.
	     * Danny 16/4/97 */
	    if (Fw)
            {
		w = Fw->w;
            }
	}
    }

    if (Mwm.number_of_screens > 1)
    {
	XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
		      &JunkX, &JunkY, &JunkX, &JunkY, &JunkMask);
	if (JunkRoot != scr->root_win)
	{
	    if ((Mwm.keyboard_focus_policy == XmEXPLICIT) &&
		(scr->mwm_grabbing != NULL))
	    {
		/* Need to grab buttons for focus window */
		XSync(dpy, 0);
		for (i = 0; i < 3; i++)
		    if (scr->buttons2grab & (1 << i))
		    {
			XGrabButton(dpy, (i + 1), 0, scr->mwm_grabbing->frame, True,
				  ButtonPressMask, GrabModeSync, GrabModeAsync,
				    None, scr->cursors[SYS_CURS]);
			XGrabButton(dpy, (i + 1), LockMask, scr->mwm_grabbing->frame, True,
				  ButtonPressMask, GrabModeSync, GrabModeAsync,
				    None, scr->cursors[SYS_CURS]);
		    }
		scr->mwm_focus = NULL;
		scr->mwm_grabbing = NULL;
		XSetInputFocus(dpy, scr->no_focus_win, RevertToParent, MISC_FetchEventTime());
	    }
	    return;
	}
    }

    if ((Fw != NULL) && (Fw->Desk != scr->current_desk))
    {
	Fw = NULL;
	w = scr->no_focus_win;
    }

    if ((Mwm.keyboard_focus_policy == XmEXPLICIT) && (scr->mwm_grabbing != Fw))
    {
	/* need to grab all buttons for window that we are about to
	 * unfocus */
	if (scr->mwm_grabbing != NULL)
	{
	    XSync(dpy, 0);
	    for (i = 0; i < 3; i++)
		if (scr->buttons2grab & (1 << i))
		{
		    XGrabButton(dpy, (i + 1), 0, scr->mwm_grabbing->frame, True,
			    ButtonPressMask, GrabModeSync, GrabModeAsync, None,
				scr->cursors[SYS_CURS]);
		    XGrabButton(dpy, (i + 1), LockMask, scr->mwm_grabbing->frame, True,
			    ButtonPressMask, GrabModeSync, GrabModeAsync, None,
				scr->cursors[SYS_CURS]);
		}
	    scr->mwm_grabbing = NULL;
	}
	/* if we do click to focus, remove the grab on mouse events that
	 * was made to detect the focus change */
	if ((Mwm.keyboard_focus_policy == XmEXPLICIT) && (Fw != NULL))
	{
	    for (i = 0; i < 3; i++)
		if (scr->buttons2grab & (1 << i))
		{
		    XUngrabButton(dpy, (i + 1), 0, Fw->frame);
		    XUngrabButton(dpy, (i + 1), LockMask, Fw->frame);
		}
	    scr->mwm_grabbing = Fw;
	}
    }
    if ((Fw) && (Fw->flags & ICONIFIED) && (Fw->icon_w))
    {
	w = Fw->icon_w;
	ICON_UpdateWindow(scr, Fw, True);
    }

    if (!((Fw) && (Fw->wmhints) && (Fw->wmhints->flags & InputHint) &&
	  (Fw->wmhints->input == False)))
    {
        /* Window rw; */
        /* int rt; */

	/* Window will accept input focus */
        /* MLM: 6/??/98.  A very weird thing can happen here.  We set
         * the focus, but if you add a call just below here to XGetInputFocus(),
         * you'll see that the focus has reverted to the old value.  I *think*
         * this is due to the XSetInputFocus calls in the menu system -- I
         * think there is a race condition where a dying application with
         * a posted menu can reclaim the focus *after* their window manager
         * info is gone (matte and decor), but before the application
         * really terminates.  This manifests itself as a "missing" FocusIn
         * event, but we receive the FocusOut event when the window finally
         * goes away.  The (hack) fix for this is in events.c (see FocusOut
         * in the event handler).
         */
	XSetInputFocus(dpy, w, RevertToParent, MISC_FetchEventTime());
	scr->mwm_focus = Fw;
    }
    else if ((scr->mwm_focus) && (scr->mwm_focus->Desk == scr->current_desk))
    {
	/* Window doesn't want focus. Leave focus alone */
	/* XSetInputFocus (dpy,scr->mwm_highlight->w , RevertToParent, MISC_FetchEventTime()); */
    }
    else
    {
	XSetInputFocus(dpy, scr->no_focus_win, RevertToParent, MISC_FetchEventTime());
	scr->mwm_focus = NULL;
    }

    if ((Fw) && (Fw->flags & WM_TAKES_FOCUS))
	PROP_SendClientMessage(w, XA_WM_TAKE_FOCUS, MISC_FetchEventTime());

    XSync(dpy, 0);
}

/*
 * Moves focus to specified window 
 */
void
WIN_ChangeFocus(ScreenInfo *scr, MwmWindow *t, int DeIconifyOnly)
{
    int dx, dy;
    int cx, cy;
    int x, y;

    if (t == (MwmWindow *)0)
	return;

    if (t->Desk != scr->current_desk)
	DT_ChangeDesks(scr, 0, t->Desk);

    if (t->flags & ICONIFIED)
    {
	cx = t->icon_xl_loc + t->icon_w_width / 2;
	cy = t->icon_y_loc + t->icon_p_height +
	    (scr->components[MWM_ICON].f_height + 6) / 2;
    }
    else
    {
	cx = t->frame_x + t->frame_width / 2;
	cy = t->frame_y + t->frame_height / 2;
    }

    dx = (cx + scr->virt_x) / scr->d_width * scr->d_width;
    dy = (cy + scr->virt_y) / scr->d_height * scr->d_height;

    PAGER_MoveViewPort(scr, dx, dy, True);

    if (t->flags & ICONIFIED)
    {
	x = t->icon_xl_loc + t->icon_w_width / 2;
	y = t->icon_y_loc + t->icon_p_height +
	    (scr->components[MWM_ICON].f_height + 6) / 2;
    }
    else
    {
	x = t->frame_x;
	y = t->frame_y;
    }
    if (!(Mwm.keyboard_focus_policy == XmEXPLICIT))
	XWarpPointer(dpy, None, scr->root_win, 0, 0, 0, 0, x + 2, y + 2);
    WIN_Raise(scr, t);

    /* If the window is still not visible, make it visible! */
    if (((t->frame_x + t->frame_height) < 0) || (t->frame_y + t->frame_width < 0) ||
	(t->frame_x > scr->d_width) || (t->frame_y > scr->d_height))
    {
	DEC_ConfigureDecorations(scr, t, 0, 0, t->frame_width, t->frame_height, False);
	if (!(Mwm.keyboard_focus_policy == XmEXPLICIT))
	    XWarpPointer(dpy, None, scr->root_win, 0, 0, 0, 0, 2, 2);
    }
    MISC_Ungrab(scr);
    WIN_SetFocus(scr, t->w, t);
}

/*
 * Puts windows back where they were before mwm took over 
 */
void
WIN_RestoreWithdrawn(ScreenInfo *scr, MwmWindow *tmp, Boolean restart)
{
    int a, b, w2, h2;
    unsigned int bw, mask;
    XWindowChanges xwc;

    if (!tmp)
	return;

    if (XGetGeometry(dpy, tmp->w, &JunkRoot, &xwc.x, &xwc.y,
		     &JunkWidth, &JunkHeight, &bw, &JunkDepth))
    {
	XTranslateCoordinates(dpy, tmp->frame, scr->root_win, xwc.x, xwc.y,
			      &a, &b, &JunkChild);
	xwc.x = a + tmp->xdiff;
	xwc.y = b + tmp->ydiff;
	xwc.border_width = tmp->old_bw;
	mask = (CWX | CWY | CWBorderWidth);

	/* We can not assume that the window is currently on the screen.
	 * Although this is normally the case, it is not always true.  The
	 * most common example is when the user does something in an
	 * application which will, after some amount of computational delay,
	 * cause the window to be unmapped, but then switches screens before
	 * this happens.  The XTranslateCoordinates call above will set the
	 * window coordinates to either be larger than the screen, or negative.
	 * This will result in the window being placed in odd, or even
	 * unviewable locations when the window is remapped.  The followin code
	 * forces the "relative" location to be within the bounds of the display.
	 *
	 * gpw -- 11/11/93
	 *
	 * Unfortunately, this does horrendous things during re-starts, 
	 * hence the "if(restart) clause (RN) 
	 *
	 * Also, fixed so that it only does this stuff if a window is more than
	 * half off the screen. (RN)
	 */

	if (!restart)
	{
	    /* Don't mess with it if its partially on the screen now */
	    if ((tmp->frame_x < 0) || (tmp->frame_y < 0) ||
		(tmp->frame_x >= scr->d_width) ||
		(tmp->frame_y >= scr->d_height))
	    {
		w2 = (tmp->frame_width >> 1);
		h2 = (tmp->frame_height >> 1);
		if ((xwc.x < -w2) || (xwc.x > (scr->d_width - w2)))
		{
		    xwc.x = xwc.x % scr->d_width;
		    if (xwc.x < -w2)
			xwc.x += scr->d_width;
		}
		if ((xwc.y < -h2) || (xwc.y > (scr->d_height - h2)))
		{
		    xwc.y = xwc.y % scr->d_height;
		    if (xwc.y < -h2)
			xwc.y += scr->d_height;
		}
	    }
	}
	XReparentWindow(dpy, tmp->w, scr->root_win, xwc.x, xwc.y);

	if ((tmp->flags & ICONIFIED))
	{
	    if (tmp->icon_w)
		XUnmapWindow(dpy, tmp->icon_w);
	    if (tmp->icon_pixmap_w)
		XUnmapWindow(dpy, tmp->icon_pixmap_w);
	}

	XConfigureWindow(dpy, tmp->w, mask, &xwc);
	XSync(dpy, 0);
    }
}

/*
 * raise a window
 */
void
WIN_Raise(ScreenInfo *scr, MwmWindow *t)
{
    int count, i;
    Window *wins;

    MISC_SetTimer(0);

    /* raise the target, at least */
    count = 1;

    count += count_transients(scr, t);

    if ((t->flags & ICONIFIED))
	count += 2;

    if ((scr->pager_win) && !(t->flags & STICKY))
	XRaiseWindow(dpy, t->pager_view);

    wins = (Window *)XtMalloc(count * sizeof(Window));

    i = 0;

    /* now raise transients */
    gather_transients(t, wins, &i);

    if ((t->flags & ICONIFIED))
    {
	wins[i++] = t->icon_w;
	wins[i++] = t->icon_pixmap_w;
    }
    wins[i++] = t->frame;
    scr->mwm_last_raised = t;

    if (i > 0)
	XRaiseWindow(dpy, wins[0]);

    XRestackWindows(dpy, wins, i);
    XtFree((char *)wins);

    PAGER_Clear(scr);

    PAN_Raise(scr);
}

/*
 * lower a window
 */
void
WIN_Lower(ScreenInfo *scr, MwmWindow *t)
{
    if (t->child)
	lower_children(t->child);

    XLowerWindow(dpy, t->frame);

    MISC_SetTimer(0);

    if ((scr->pager_win) && !(t->flags & STICKY))
	XLowerWindow(dpy, t->pager_view);

    if ((t->flags & ICONIFIED))
    {
	XLowerWindow(dpy, t->icon_w);
	XLowerWindow(dpy, t->icon_pixmap_w);
    }
    scr->mwm_last_raised = (MwmWindow *)0;
    if (scr->pager_child_win)
	XLowerWindow(dpy, scr->pager_child_win);
    PAGER_Clear(scr);
}

/*
 * adjust the given width and height to account for the constraints imposed
 * by size hints
 * The general algorithm, especially the aspect ratio stuff, is borrowed from
 * uwm's CheckConsistency routine.
 */
void
WIN_ConstrainWindow(ScreenInfo *scr, MwmWindow *tmp_win,
		    int *widthp, int *heightp)
{
    int minWidth, minHeight, maxWidth, maxHeight, xinc, yinc, delta;
    int baseWidth, baseHeight;
    int dwidth = *widthp, dheight = *heightp;

    dwidth -= (2 * tmp_win->boundary_width + 2 * tmp_win->matte_width);
    dheight -= (tmp_win->title_height + 2 * tmp_win->boundary_width +
		2 * tmp_win->matte_width);

    minWidth = tmp_win->hints.min_width;
    minHeight = tmp_win->hints.min_height;

    baseWidth = tmp_win->hints.base_width;
    baseHeight = tmp_win->hints.base_height;

    maxWidth = tmp_win->hints.max_width;
    maxHeight = tmp_win->hints.max_height;

/*  maxWidth = scr->virt_x_max + scr->d_width;
   maxHeight = scr->virt_y_max + scr->d_height; */

    xinc = tmp_win->hints.width_inc;
    yinc = tmp_win->hints.height_inc;

    /*
     * First, clamp to min and max values
     */
    if (dwidth < minWidth)
	dwidth = minWidth;
    if (dheight < minHeight)
	dheight = minHeight;

    if (dwidth > maxWidth)
	dwidth = maxWidth;
    if (dheight > maxHeight)
	dheight = maxHeight;

    /*
     * Second, fit to base + N * inc
     */
    dwidth = ((dwidth - baseWidth) / xinc * xinc) + baseWidth;
    dheight = ((dheight - baseHeight) / yinc * yinc) + baseHeight;


    /*
     * Third, adjust for aspect ratio
     * The math looks like this:
     *
     * minAspectX    dwidth     maxAspectX
     * ---------- <= ------- <= ----------
     * minAspectY    dheight    maxAspectY
     *
     * If that is multiplied out, then the width and height are
     * invalid in the following situations:
     *
     * minAspectX * dheight > minAspectY * dwidth
     * maxAspectX * dheight < maxAspectY * dwidth
     * 
     */
    if (tmp_win->hints.flags & PAspect)
    {
	if (MinAspectX(tmp_win) * dheight > MinAspectY(tmp_win) * dwidth)
	{
	    delta = makemult(MinAspectX(tmp_win) * dheight /
			     MinAspectY(tmp_win) - dwidth, xinc);
	    if (dwidth + delta <= maxWidth)
		dwidth += delta;
	    else
	    {
		delta = makemult(dheight - dwidth * MinAspectY(tmp_win) /
				 MinAspectX(tmp_win), yinc);
		if (dheight - delta >= minHeight)
		    dheight -= delta;
	    }
	}

	if (MaxAspectX(tmp_win) * dheight < MaxAspectY(tmp_win) * dwidth)
	{
	    delta = makemult(dwidth * MaxAspectY(tmp_win) /
			     MaxAspectX(tmp_win) - dheight, yinc);
	    if (dheight + delta <= maxHeight)
		dheight += delta;
	    else
	    {
		delta = makemult(dwidth - MaxAspectX(tmp_win) * dheight /
				 MaxAspectY(tmp_win), xinc);
		if (dwidth - delta >= minWidth)
		    dwidth -= delta;
	    }
	}
    }


    /*
     * Fourth, account for border width and title height
     */
    *widthp = dwidth + 2 * tmp_win->boundary_width +
	2 * tmp_win->matte_width;
    *heightp = dheight + tmp_win->title_height +
	2 * tmp_win->boundary_width + 2 * tmp_win->matte_width;
}

/*
 * move/draw a window outline
 */
void
WIN_DrawOutline(ScreenInfo *scr, Window root, int x, int y, int width, int height)
{
    static int lastx = 0;
    static int lasty = 0;
    static int lastWidth = 0;
    static int lastHeight = 0;
    XRectangle rects[5];

    if (x == lastx && y == lasty && width == lastWidth && height == lastHeight)
	return;

    /* undraw the old one, if any */
    if (lastWidth || lastHeight)
    {
	rects[0].x = lastx;
	rects[0].y = lasty;
	rects[0].width = lastWidth;
	rects[0].height = lastHeight;
	rects[1].x = lastx + 1;
	rects[1].y = lasty + 1;
	rects[1].width = lastWidth - 2;
	rects[1].height = lastHeight - 2;
	rects[2].x = lastx + 2;
	rects[2].y = lasty + 2;
	rects[2].width = lastWidth - 4;
	rects[2].height = lastHeight - 4;
	rects[3].x = lastx + 3;
	rects[3].y = lasty + 3 + (lastHeight - 6) / 3;
	rects[3].width = lastWidth - 6;
	rects[3].height = (lastHeight - 6) / 3;
	rects[4].x = lastx + 3 + (lastWidth - 6) / 3;
	rects[4].y = lasty + 3;
	rects[4].width = (lastWidth - 6) / 3;
	rects[4].height = (lastHeight - 6);
	XDrawRectangles(dpy, scr->root_win, scr->resize_GC, rects, 5);
    }

    lastx = x;
    lasty = y;
    lastWidth = width;
    lastHeight = height;

    /* draw the new one, if any */
    if (lastWidth || lastHeight)
    {
	rects[0].x = lastx;
	rects[0].y = lasty;
	rects[0].width = lastWidth;
	rects[0].height = lastHeight;
	rects[1].x = lastx + 1;
	rects[1].y = lasty + 1;
	rects[1].width = lastWidth - 2;
	rects[1].height = lastHeight - 2;
	rects[2].x = lastx + 2;
	rects[2].y = lasty + 2;
	rects[2].width = lastWidth - 4;
	rects[2].height = lastHeight - 4;
	rects[3].x = lastx + 3;
	rects[3].y = lasty + 3 + (lastHeight - 6) / 3;
	rects[3].width = lastWidth - 6;
	rects[3].height = (lastHeight - 6) / 3;
	rects[4].x = lastx + 3 + (lastWidth - 6) / 3;
	rects[4].y = lasty + 3;
	rects[4].width = (lastWidth - 6) / 3;
	rects[4].height = (lastHeight - 6);
	XDrawRectangles(dpy, scr->root_win, scr->resize_GC, rects, 5);
    }
}

/*
 * Releases dynamically allocated space used to store window/icon names
 */
void
WIN_FreeNames(MwmWindow *tmp, Bool nukename, Bool nukeicon)
{
    if (!tmp)
	return;

    if (nukename && nukeicon)
    {
	if (tmp->name == tmp->icon_active_label)
	{
	    if (tmp->name != NoName)
		XFree(tmp->name);
	    tmp->name = NULL;
	    tmp->icon_active_label = NULL;
	}
	else
	{
	    if (tmp->name != NoName)
		XFree(tmp->name);
	    tmp->name = NULL;
	    if (tmp->icon_active_label != NoName)
		XFree(tmp->icon_active_label);
	    tmp->icon_active_label = NULL;
	}
    }
    else if (nukename)
    {
	if (tmp->name != tmp->icon_active_label && tmp->name != NoName)
	    XFree(tmp->name);
	tmp->name = NULL;
    }
    else
    {				/* if (nukeicon) */
	if (tmp->icon_active_label != tmp->name && tmp->icon_active_label != NoName)
	    XFree(tmp->icon_active_label);
	tmp->icon_active_label = NULL;
    }
}

/*
 * map a window
 */
void
WIN_MapWindow(ScreenInfo *scr, Window win)
{
    MwmWindow *tmp;

    if (XFindContext(dpy, win, MwmContext, (XPointer *)&tmp) == XCNOENT)
	tmp = NULL;

    XFlush(dpy);

    /* If the window has never been mapped before ... */
    if (!tmp)
    {
	/* Add decorations. */
	tmp = add_window(scr, win);
	if (tmp == NULL)
	    return;
    }

    /* If it's not merely iconified, and we have hints, use them. */
    if (!(tmp->flags & ICONIFIED))
    {
	int state;

	if (tmp->wmhints && (tmp->wmhints->flags & StateHint))
	    state = tmp->wmhints->initial_state;
	else
	    state = NormalState;

	if (tmp->flags & STARTICONIC)
	    state = IconicState;

	if (isIconicState != DontCareState)
	    state = isIconicState;

	XGrabServer(dpy);
	switch (state)
	{
	case DontCareState:
	case NormalState:
	case InactiveState:
	default:
	    if (tmp->Desk == scr->current_desk)
	    {
		XMapWindow(dpy, tmp->w);
		XMapWindow(dpy, tmp->frame);
		tmp->flags |= MAP_PENDING;
		PROP_SetState(tmp, NormalState);
		if (Mwm.keyboard_focus_policy == XmEXPLICIT &&
		    Mwm.startup_key_focus)
		{
		    WIN_SetFocusInTree(tmp);
		    WIN_SetFocus(scr, tmp->w, tmp);
		    MISC_SetFocusSequence(scr);
		}
	    }
	    else
	    {
		XMapWindow(dpy, tmp->w);
		PROP_SetState(tmp, NormalState);
	    }
	    break;

	case IconicState:
	    ICON_Iconify(scr, tmp, 0, 0);
	    break;
	}
	XSync(dpy, 0);
	XUngrabServer(dpy);
    }
    /* If no hints, or currently an icon, just "deiconify" */
    else
	ICON_DeIconify(scr, tmp);
}

/*
 * Handles destruction of a window 
 */
void
WIN_DestroyWindow(ScreenInfo *scr, MwmWindow *tmp)
{
    int i;

    /*
     * Warning, this is also called by HandleUnmapNotify; if it ever needs to
     * look at the event, HandleUnmapNotify will have to mash the UnmapNotify
     * into a DestroyNotify.
     */
    if (!tmp)
	return;

    MISC_DestroyChildren(scr, tmp);

    MENU_DestroyWindowMenu(scr, tmp);

    XUnmapWindow(dpy, tmp->frame);
    XSync(dpy, 0);

    if (tmp == scr->mwm_highlight)
    {
	scr->mwm_highlight = NULL;
    }

    if (scr->mwm_last_focus == tmp)
    {
	scr->mwm_last_focus = NULL;
    }

    if (scr->mwm_event == tmp)
    {
	scr->mwm_event = NULL;
    }

    if (tmp == scr->mwm_focus && Mwm.keyboard_focus_policy == XmEXPLICIT &&
	Mwm.auto_key_focus)
    {
	if (tmp->next != NULL)
	{
	    WIN_SetFocusInTree(tmp->next);
	    WIN_SetFocus(scr, tmp->next->w, tmp->next);
	}
	else if (tmp->ancestor)
	{
	    WIN_SetFocusInTree(tmp->ancestor);
	    WIN_SetFocus(scr, tmp->ancestor->w, tmp->ancestor);
	}
	else
        {
	    WIN_SetFocus(scr, scr->no_focus_win, NULL);
        }
    }

    MISC_RemoveFromTree(scr, tmp);

    if (scr->mwm_focus == tmp)
    {
	WIN_SetFocus(scr, scr->no_focus_win, NULL);
    }

    MISC_SetFocusSequence(scr);

    if (tmp == scr->mwm_pushed)
	scr->mwm_pushed = NULL;

    if (tmp == scr->mwm_colormap)
	scr->mwm_colormap = NULL;

    XDestroyWindow(dpy, tmp->frame);
    XDeleteContext(dpy, tmp->frame, MwmContext);

    XDestroyWindow(dpy, tmp->parent);

    XDeleteContext(dpy, tmp->parent, MwmContext);

    XDeleteContext(dpy, tmp->w, MwmContext);

    if ((tmp->icon_w) && (tmp->flags & PIXMAP_OURS))
	XFreePixmap(dpy, tmp->icon_pixmap);

    if ((scr->pager_win) && !(tmp->flags & STICKY))
	XDestroyWindow(dpy, tmp->pager_view);

    if (tmp->icon_w)
    {
	XDestroyWindow(dpy, tmp->icon_w);
	XDeleteContext(dpy, tmp->icon_w, MwmContext);
    }
    if ((tmp->flags & ICON_OURS) && (tmp->icon_pixmap_w != None))
	XDestroyWindow(dpy, tmp->icon_pixmap_w);
    if (tmp->icon_pixmap_w != None)
	XDeleteContext(dpy, tmp->icon_pixmap_w, MwmContext);

    for (i = 0; i < 4; i++)
    {
	if (tmp->icon_borders[i] != None)
	{
	    XDestroyWindow(dpy, tmp->icon_borders[i]);
	    XDeleteContext(dpy, tmp->icon_borders[i], MwmContext);
	}
    }

    if (tmp->decorations & MWM_DECOR_TITLE)
    {
	XDeleteContext(dpy, tmp->title, MwmContext);
	if (tmp->menub != None)
	    XDeleteContext(dpy, tmp->menub, MwmContext);
	if (tmp->menub != None)
	    XDeleteContext(dpy, tmp->menub, MwmContext);
    }
    if (tmp->decorations & MWM_DECOR_BORDER)
    {
	for (i = 0; i < 4; i++)
	    XDeleteContext(dpy, tmp->sides[i], MwmContext);
    }
    if (tmp->decorations & MWM_DECOR_RESIZEH)
    {
	for (i = 0; i < 4; i++)
	    XDeleteContext(dpy, tmp->corners[i], MwmContext);
    }

    WIN_FreeNames(tmp, True, True);

    if (tmp->wmhints)
	XFree((char *)tmp->wmhints);
    if (tmp->classhint.res_name && tmp->classhint.res_name != NoName)
	XFree((char *)tmp->classhint.res_name);
    if (tmp->classhint.res_class && tmp->classhint.res_class != NoName)
	XFree((char *)tmp->classhint.res_class);
    if (tmp->mwm_hints)
	XFree((char *)tmp->mwm_hints);
    if (tmp->mwm_menu)
	XFree((char *)tmp->mwm_menu);
    if (tmp->mwm_messages)
	XFree((char *)tmp->mwm_messages);

    if (tmp->cmap_windows != (Window *)NULL)
	XFree((void *)tmp->cmap_windows);

    XtFree((char *)tmp);

    PAGER_Clear(scr);
    XSync(dpy, 0);
}
