/* $Id: props.c,v 1.2 2007/09/12 20:05:58 jwrdegoede Exp $ */
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

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include "mwm.h"


Atom XA_MIT_PRIORITY_COLORS;
Atom XA_WM_CHANGE_STATE;
Atom XA_WM_STATE;
Atom XA_WM_COLORMAP_WINDOWS;
Atom XA_WM_PROTOCOLS;
Atom XA_WM_TAKE_FOCUS;
Atom XA_WM_DELETE_WINDOW;
Atom XA_WM_SAVE_YOURSELF;
Atom XA_WM_DESKTOP;
Atom XA_MWM_HINTS;
Atom XA_MWM_MESSAGES;
Atom XA_MWM_MENU;
Atom XA_MWM_INFO;

/***********************************************************************
 *
 * <Properties description>
 *
 ***********************************************************************/

/*
 * Intern some commonly used atoms
 */
void
PROP_Initialize(void)
{
    XA_MIT_PRIORITY_COLORS = XInternAtom(dpy, "_MIT_PRIORITY_COLORS", False);
    XA_WM_CHANGE_STATE = XInternAtom(dpy, "WM_CHANGE_STATE", False);
    XA_WM_STATE = XInternAtom(dpy, "WM_STATE", False);
    XA_WM_COLORMAP_WINDOWS = XInternAtom(dpy, "WM_COLORMAP_WINDOWS", False);
    XA_WM_PROTOCOLS = XInternAtom(dpy, "WM_PROTOCOLS", False);
    XA_WM_TAKE_FOCUS = XInternAtom(dpy, "WM_TAKE_FOCUS", False);
    XA_WM_DELETE_WINDOW = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
    XA_WM_SAVE_YOURSELF = XInternAtom(dpy, "WM_SAVE_YOURSELF", False);
    XA_WM_DESKTOP = XInternAtom(dpy, "WM_DESKTOP", False);
    XA_MWM_HINTS = XInternAtom(dpy, _XA_MWM_HINTS, False);
    XA_MWM_MESSAGES = XInternAtom(dpy, _XA_MWM_MESSAGES, False);
    XA_MWM_MENU = XInternAtom(dpy, _XA_MWM_MENU, False);
    XA_MWM_INFO = XInternAtom(dpy, _XA_MWM_INFO, False);
}

/*
 * set the behavior property on the root window.  This will be used for the
 * f.set_behavior function
 */
void
PROP_SetBehavior(ScreenInfo *scr, Boolean custom)
{
    long info[PROP_MWM_INFO_ELEMENTS];

    /* set the MWM_INFO property on the Root, notice that we
       use an array of longs here and not the PropMotifWmInfo struct,
       this is because this struct looks like this in lesstif:
       
       typedef struct {
           CARD32 flags;
           CARD32 wmWindow;
       } PropMotifWmInfo;
       
       But when setting 32 bit properties XChangeProperty expects an array of
       longs, which it will convert to 32 bit values if need. Thus passing an
       actual PropMotifWmInfo struct will mess things up on archs where longs
       are 64 bit. */
    
    if (custom)
	info[0] = MWM_INFO_STARTUP_CUSTOM;   /* set flags "member" */
    else
	info[0] = MWM_INFO_STARTUP_STANDARD; /* set flags "member" */

    info[1] = scr->root_win; /* set wmWindow "member" */

    XChangeProperty(dpy, scr->root_win, XA_MWM_INFO, XA_MWM_INFO,
		    32, PropModeReplace,
		    (unsigned char *)&info, PROP_MWM_INFO_ELEMENTS);
}

/*
 * clear the behavior property on the root window.
 */
void
PROP_ClearBehavior(ScreenInfo *scr)
{
    /* set the MWM_INFO property on the Root */
    XChangeProperty(dpy, scr->root_win, XA_MWM_INFO, XA_MWM_INFO,
		    32, PropModeReplace,
		    NULL, 0);
}

/*
 * get the current state of the behavior property.  This also will be used
 * by f.set_behavior
 */
int
PROP_GetBehavior(ScreenInfo *scr)
{
    int actual_format, ret;
    Atom actual_type;
    unsigned long nitems, bytesafter;
    /* We use a long pointer here and not a PropMotifWmInfo pointer,
       this is because this type looks like this in lesstif:
       
       typedef struct {
           CARD32 flags;
           CARD32 wmWindow;
       } PropMotifWmInfo;
       
       But when getting 32 bit properties XGetWindowProperty returns an array
       of longs. Thus interpreting the returned data as PropMotifWmInfo will
       mess things up on archs where longs are 64 bit. */
    unsigned long *info;

    if (XGetWindowProperty(dpy, scr->root_win, XA_MWM_INFO, 0L,
			   PROP_MOTIF_WM_INFO_ELEMENTS, False,
			   XA_MWM_INFO, &actual_type, &actual_format,
			   &nitems, &bytesafter,
			   (unsigned char **)&info) == Success)
    {
	if (nitems > 0 && info)
	    ret = info[0]; /* Return flags "member" */
	else
	    ret = 0;
	XFree((char *)info);
	return ret;
    }

    return 0;
}

/*
 * Make sure property priority colors is empty
 */
void
PROP_SetPriorityColors(ScreenInfo *scr)
{
    XChangeProperty(dpy, scr->root_win, XA_MIT_PRIORITY_COLORS,
		    XA_CARDINAL, 32, PropModeReplace, NULL, 0);
}

/*
 * check the desktop.
 */
Boolean
PROP_CheckDesktop(ScreenInfo *scr)
{
    Atom atype;
    int aformat;
    unsigned long nitems, bytes_remain;
    unsigned char *prop;
    Boolean restart = False;

    /*
     * Set the current desktop number to zero
     * Multiple desks are available even in non-virtual
     * compilations
     */
    scr->current_desk = 0;
    if ((XGetWindowProperty(dpy, scr->root_win, XA_WM_DESKTOP, 0L, 1L, True,
			    XA_WM_DESKTOP, &atype, &aformat, &nitems,
			    &bytes_remain, &prop)) == Success)
    {
	if (prop != NULL)
	{
	    restart = True;
	    scr->current_desk = *(unsigned long *)prop;
	}
    }

    return restart;
}

/*
 * This is used to tell applications which windows on the screen are
 * top level appication windows, and which windows are the icon windows
 * that go with them.
 */
void
PROP_SetState(MwmWindow *tmp_win, int state)
{
    unsigned long data[2];	/* "suggested" by ICCCM version 1 */

    data[0] = (unsigned long)state;
    data[1] = (unsigned long)tmp_win->icon_w;
/*  data[2] = (unsigned long) tmp_win->icon_pixmap_w; */

    XChangeProperty(dpy, tmp_win->w, XA_WM_STATE, XA_WM_STATE, 32,
		    PropModeReplace, (unsigned char *)data, 2);
}

/*
 * ICCCM Client Messages - Section 4.2.8 of the ICCCM dictates that all
 * client messages will have the following form:
 *
 *     event type       ClientMessage
 *     message type     XA_WM_PROTOCOLS
 *     window           tmp->w
 *     format           32
 *     data[0]          message atom
 *     data[1]          time stamp
 */
void
PROP_SendClientMessage(Window w, Atom a, Time timestamp)
{
    XClientMessageEvent ev;

    ev.type = ClientMessage;
    ev.window = w;
    ev.message_type = XA_WM_PROTOCOLS;
    ev.format = 32;
    ev.data.l[0] = a;
    ev.data.l[1] = timestamp;
    XSendEvent(dpy, w, False, 0L, (XEvent *)&ev);
}

/*
 * verify that a client is expecting to receive a ClientMessage from mwm
 */
Boolean
PROP_VerifyMwmMessage(MwmWindow *win, Atom message)
{
    unsigned int i;

    for (i = 0; i < win->num_messages; i++)
    {
	if (win->mwm_messages[i] == message)
	    return True;
    }

    return False;
}

/*
 * send a mwm specific message
 */
void
PROP_SendMwmMessage(Window w, Atom a, Time timestamp)
{
    XClientMessageEvent ev;

    ev.type = ClientMessage;
    ev.window = w;
    ev.message_type = XA_MWM_MESSAGES;
    ev.format = 32;
    ev.data.l[0] = a;
    ev.data.l[1] = timestamp;
    XSendEvent(dpy, w, False, 0L, (XEvent *)&ev);
}

/*
 * Reads the property MOTIF_WM_HINTS
 */
void
PROP_GetMwmHints(MwmWindow *win)
{
    int actual_format;
    Atom actual_type;
    unsigned long nitems, bytesafter;

    if (XGetWindowProperty(dpy, win->w, XA_MWM_HINTS, 0L,
			   PROP_MOTIF_WM_HINTS_ELEMENTS, False, XA_MWM_HINTS,
			   &actual_type, &actual_format, &nitems, &bytesafter,
			   (unsigned char **)&win->mwm_hints) == Success)
    {

	if (nitems >= PROP_MOTIF_WM_HINTS_ELEMENTS)
	{
	    /* Fixup X*Property long == 32 bits confusion if needed */
	    if (sizeof(long) != 4)
	    {
	        long *prop_hints = (long *)win->mwm_hints;
	        win->mwm_hints = XtMalloc(sizeof(MwmHints));
	        win->mwm_hints->flags       = prop_hints[0];
	        win->mwm_hints->functions   = prop_hints[1];
	        win->mwm_hints->decorations = prop_hints[2];
	        win->mwm_hints->input_mode  = prop_hints[3];
	        win->mwm_hints->status      = prop_hints[4];
	        XFree(prop_hints);
	    }
	    return;
        }

	XFree((char *)win->mwm_hints);
    }

    win->mwm_hints = NULL;
}

/*
 * Reads the property MOTIF_WM_MENU
 */
void
PROP_GetMwmMenu(MwmWindow *win)
{
    int actual_format;
    Atom actual_type;
    unsigned long nitems, bytesafter;

    if (XGetWindowProperty(dpy, win->w, XA_MWM_MENU,
			   0L, 1024L, False,
			   XA_MWM_MENU, &actual_type, &actual_format,
			   &nitems, &bytesafter,
			   (unsigned char **)&win->mwm_menu) == Success)
    {

	if (bytesafter)
	    fprintf(stderr, "Maximum size of MWM_MENU exceeded\n");

	return;
    }

    win->mwm_menu = NULL;
}

/*
 * Reads the property MOTIF_WM_MESSAGES
 */
void
PROP_GetMwmMessages(MwmWindow *win)
{
    int actual_format;
    Atom actual_type;
    unsigned long bytesafter;

    if (XGetWindowProperty(dpy, win->w, XA_MWM_MESSAGES, 0L, 1024L, False,
			   AnyPropertyType, &actual_type, &actual_format,
			   &win->num_messages, &bytesafter,
			   (unsigned char **)&win->mwm_messages) == Success)
    {

	if (bytesafter)
	    fprintf(stderr, "Maximum size of MWM_MESSAGES exceeded: %ld.\n", bytesafter);

	return;
    }

    win->mwm_messages = NULL;
}

/*
 * gets application supplied size info
 */
void
PROP_GetWindowSizeHints(MwmWindow *win)
{
    long supplied = 0;

    if (!XGetWMNormalHints(dpy, win->w, &win->hints, &supplied))
	win->hints.flags = 0;

    /* Beat up our copy of the hints, so that all important field are
     * filled in! */
    if (win->hints.flags & PResizeInc)
    {
	if (win->hints.width_inc == 0)
	    win->hints.width_inc = 1;
	if (win->hints.height_inc == 0)
	    win->hints.height_inc = 1;
    }
    else
    {
	win->hints.width_inc = 1;
	win->hints.height_inc = 1;
    }

    /*
     * ICCCM says that PMinSize is the default if no PBaseSize is given,
     * and vice-versa.
     */

    if (!(win->hints.flags & PBaseSize))
    {
	if (win->hints.flags & PMinSize)
	{
	    win->hints.base_width = win->hints.min_width;
	    win->hints.base_height = win->hints.min_height;
	}
	else
	{
	    win->hints.base_width = 0;
	    win->hints.base_height = 0;
	}
    }
    if (!(win->hints.flags & PMinSize))
    {
	win->hints.min_width = win->hints.base_width;
	win->hints.min_height = win->hints.base_height;
    }
    if (!(win->hints.flags & PMaxSize))
    {
	win->hints.max_width = MAX_WINDOW_WIDTH;
	win->hints.max_height = MAX_WINDOW_HEIGHT;
    }
    if (win->hints.max_width < win->hints.min_width)
	win->hints.max_width = MAX_WINDOW_WIDTH;
    if (win->hints.max_height < win->hints.min_height)
	win->hints.max_height = MAX_WINDOW_HEIGHT;

    /* Zero width/height windows are bad news! */
    if (win->hints.min_height <= 0)
	win->hints.min_height = 1;
    if (win->hints.min_width <= 0)
	win->hints.min_width = 1;

    if (!(win->hints.flags & PWinGravity))
    {
	win->hints.win_gravity = NorthWestGravity;
	win->hints.flags |= PWinGravity;
    }
}

/*
 * finds out which protocols the window supports
 */
void
PROP_GetWmProtocols(MwmWindow *win)
{
    unsigned long flags = 0L;
    Atom *protocols = NULL, *ap;
    int i, n;
    Atom atype;
    int aformat;
    unsigned long bytes_remain, nitems;

    if (win == NULL)
	return;

    /*
     * First, try the Xlib function to read the protocols.
     * This is what Twm uses.
     */
    if (XGetWMProtocols(dpy, win->w, &protocols, &n))
    {
	win->flags &= ~(WM_TAKES_FOCUS | WM_DELS_WINDOW | WM_SAVE_SELF | MWM_MESSAGES);
	for (i = 0, ap = protocols; i < n; i++, ap++)
	{
	    if (*ap == (Atom)XA_WM_TAKE_FOCUS)
		flags |= WM_TAKES_FOCUS;
	    if (*ap == (Atom)XA_WM_DELETE_WINDOW)
		flags |= WM_DELS_WINDOW;
	    if (*ap == (Atom)XA_WM_SAVE_YOURSELF)
		flags |= WM_SAVE_SELF;
	    if (*ap == (Atom)XA_MWM_MESSAGES)
		flags |= MWM_MESSAGES;
	}
	if (protocols)
	    XFree((char *)protocols);
    }
    /*
     * Next, read it the hard way. mosaic from Coreldraw needs to 
     * be read in this way.
     */
    else if ((XGetWindowProperty(dpy, win->w, XA_WM_PROTOCOLS, 0L, 10L, False,
				 XA_WM_PROTOCOLS, &atype, &aformat, &nitems,
				 &bytes_remain,
				 (unsigned char **)&protocols)) == Success)
    {
	win->flags &= ~(WM_TAKES_FOCUS | WM_DELS_WINDOW | WM_SAVE_SELF | MWM_MESSAGES);
	for (i = 0, ap = protocols; (unsigned long)i < nitems; i++, ap++)
	{
	    if (*ap == (Atom)XA_WM_TAKE_FOCUS)
		flags |= WM_TAKES_FOCUS;
	    if (*ap == (Atom)XA_WM_DELETE_WINDOW)
		flags |= WM_DELS_WINDOW;
	    if (*ap == (Atom)XA_WM_SAVE_YOURSELF)
		flags |= WM_SAVE_SELF;
	    if (*ap == (Atom)XA_MWM_MESSAGES)
		flags |= MWM_MESSAGES;
	}
	if (protocols)
	    XFree((char *)protocols);
    }
    win->flags |= flags;
}

/*
 * Gets the WM_COLORMAP_WINDOWS property from the window
 * This property typically doesn't exist, but a few applications
 * use it. These seem to occur mostly on SGI machines.
 */
void
PROP_GetWmColormapWindows(MwmWindow *tmp)
{
    if (tmp->cmap_windows != (Window *)NULL)
	XFree((void *)tmp->cmap_windows);

    if (!XGetWMColormapWindows(dpy, tmp->w, &tmp->cmap_windows,
			       &tmp->number_cmap_windows))
    {
	tmp->number_cmap_windows = 0;
	tmp->cmap_windows = NULL;
    }
}

/*
 * get the icon name
 */
void
PROP_GetWmIconName(MwmWindow *tmp)
{
    Atom actual_type;
    int actual_format;
    unsigned long nitems, bytesafter;
    unsigned char *prop;

    if (XGetWindowProperty(dpy, tmp->w, XA_WM_ICON_NAME, 0L, 200L, False,
		       XA_STRING, &actual_type, &actual_format, &nitems,
		       &bytesafter, &prop)
	== Success && prop != NULL)
    {
	tmp->icon_label = (char *)prop; /* FIXME */
    }
}
