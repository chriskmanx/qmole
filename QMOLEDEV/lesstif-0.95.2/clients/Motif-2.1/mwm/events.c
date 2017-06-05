/* $Id: events.c,v 1.1 2004/08/28 19:25:45 dannybackx Exp $ */
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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#include <Xm/XmosP.h>
#if XmVERSION >= 2
#include <XmI/XmI.h>
#endif
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>

#include <X11/extensions/shape.h>

#include "mwm.h"


/*
 * shorthand defines
 */
#define MAX_NAME_LEN 200L	/* truncate to this many */
#define MAX_ICON_NAME_LEN 200L	/* ditto */
#define MOD_MASK	(ShiftMask|ControlMask|Mod1Mask|Mod2Mask|Mod3Mask|\
			 Mod4Mask | Mod5Mask)

static int ShapeEventBase, ShapeErrorBase;

/*
 * Waits Mwm.click_time, or until it is evident that the user is not
 * clicking, but is moving the cursor.
 */
static Boolean
is_click(ScreenInfo *scr, int x, int y, XEvent *d)
{
    int xcurrent, ycurrent, total = 0;
    XEvent trash;

    xcurrent = x;
    ycurrent = y;
#if 0
    XAllowEvents(dpy, ReplayPointer, CurrentTime);
#endif
    while ((total < Mwm.click_time) &&
	   (x - xcurrent < Mwm.move_threshold) &&
	   (x - xcurrent > -Mwm.move_threshold) &&
	   (y - ycurrent < Mwm.move_threshold) &&
	   (y - ycurrent > -Mwm.move_threshold))
    {

	_XmMicroSleep(10000);
	total += 10;

	if (XCheckMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask, &trash))
	{
	    MISC_StashEventTime(&trash);
	    if (trash.xbutton.button == d->xbutton.button)
	    {
		*d = trash;
		return True;
	    }
	}

	if (XCheckMaskEvent(dpy, ButtonMotionMask | PointerMotionMask, &trash))
	{
	    xcurrent = trash.xmotion.x_root;
	    ycurrent = trash.xmotion.y_root;
	    MISC_StashEventTime(&trash);
	}
    }

    return False;
}

/*
 * This procedure handles both a client changing its own colormap, and
 * a client explicitly installing its colormap itself (only the window
 * manager should do that, so we must set it correctly).
 */
static void
color_map_notify(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    XColormapEvent *cevent = (XColormapEvent *)event;
    Boolean reinstall = False;


    if (!win)
	return;

#ifdef __cplusplus
    if (cevent->c_new)
#else
    if (cevent->new)
#endif
    {
	XGetWindowAttributes(dpy, win->w, &(win->attr));
	if (win == scr->mwm_colormap && win->number_cmap_windows == 0)
	    scr->last_cmap = win->attr.colormap;
	reinstall = True;
    }
    else if ((cevent->state == ColormapUninstalled) &&
	     (scr->last_cmap == cevent->colormap))
    {
	/* Some window installed its colormap, change it back */
	reinstall = True;
    }

    while (XCheckTypedEvent(dpy, ColormapNotify, event))
    {
	if (XFindContext(dpy, cevent->window,
			 MwmContext, (XPointer *)&win) == XCNOENT)
	    win = NULL;
#ifdef __cplusplus
	if ((win) && (cevent->c_new))
#else
	if ((win) && (cevent->new))
#endif
	{
	    XGetWindowAttributes(dpy, win->w, &(win->attr));
	    if (win == scr->mwm_colormap &&
		win->number_cmap_windows == 0)
		scr->last_cmap = win->attr.colormap;
	    reinstall = True;
	}
	else if ((win) &&
		 (cevent->state == ColormapUninstalled) &&
		 (scr->last_cmap == cevent->colormap))
	{
	    /* Some window installed its colormap, change it back */
	    reinstall = True;
	}
	else if ((win) &&
		 (cevent->state == ColormapInstalled) &&
		 (scr->last_cmap == cevent->colormap))
	{
	    /* The last color map installed was the correct one. Don't 
	     * change anything */
	    reinstall = False;
	}
    }

    if (reinstall)
	XInstallColormap(dpy, scr->last_cmap);
}

/*
 * handles focus in events
 */
static void
focus_in(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    XEvent d;
    Window w;

    w = event->xany.window;
    while (XCheckTypedEvent(dpy, FocusIn, &d))
	w = d.xany.window;

    if (XFindContext(dpy, w, MwmContext, (XPointer *)&win) == XCNOENT)
	win = NULL;

    if (!win)
	DEC_DrawDecorations(scr, scr->mwm_highlight, False, True, True, None);
    else if (win != scr->mwm_highlight)
	DEC_DrawDecorations(scr, win, True, True, True, None);

    if (win && Mwm.colormap_focus_policy == XmKEYBOARD)
	COLOR_InstallWindowColorMap(scr, win);
}

/*
 * this function deals with bad focus tracks
 * MLM: 6/??/98. Deal with what seems is a race condition between the
 * toolkit and the window manager.  When we set the input focus in
 * the unmap_notify() function, it can sometimes implicitly fail without
 * our knowing it.  I believe this is a race condition where the toolkit
 * calls XSetInputFocus() when dealing with the menu system.  The net
 * result is that we sometimes do not receive the FocusIn event we expect;
 * therefore the application terminates, and focus reverts to None.
 * We find out about that here.
 */
static void
focus_out(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    Window focusBug;
    int rt;

    XGetInputFocus(dpy, &focusBug, &rt);
    if (win == NULL && scr->mwm_focus != NULL && focusBug == None)
    {
        XSetInputFocus(dpy, scr->mwm_focus->w, RevertToParent, CurrentTime);
    }
}

/*
 * key press event handler
 */
static void
key_press(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    FuncKey *key;
    unsigned int modifier;
    Window dummy;

    if (event->xkey.window == scr->shield_win)
    {
	XBell(dpy, 100);
	return;
    }

    scr->event_context = EVENT_GetContext(scr, win, event, &dummy);

    modifier = (event->xkey.state & MOD_MASK);
    for (key = scr->keys; key != NULL; key = key->next)
    {
	scr->mwm_event = win;
	/* Here's a real hack - some systems have two keys with the
	 * same keysym and different keycodes. This converts all
	 * the cases to one keycode. */
	event->xkey.keycode =
	    XKeysymToKeycode(dpy,
			     XKeycodeToKeysym(dpy, event->xkey.keycode, 0));

	if ((key->keycode == event->xkey.keycode) &&
	    ((key->mods == (modifier & (~LockMask))) ||
	     (key->mods == AnyModifier)) &&
	    (key->cont & scr->event_context))
	{
	    FUNC_Execute(scr, key->func, key->action, event->xany.window, win,
			 event, scr->event_context, key->val1, key->val2,
			 key->val1_unit, key->val2_unit,
			 key->menu);
	    return;
	}
    }

    /* if we get here, no function key was bound to the key.  Send it
     * to the client if it was in a window we know about.
     */
    if (win)
    {
	if (event->xkey.window != win->w)
	{
	    event->xkey.window = win->w;
	    XSendEvent(dpy, win->w, False, KeyPressMask, event);
	}
    }
    scr->mwm_event = NULL;
}

/*
 * property notify event handler
 */
static void
property_notify(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    char *prop = NULL;
    Atom actual = None;
    int actual_format;
    unsigned long nitems, bytesafter;

    if ((!win) || (XGetGeometry(dpy, win->w, &JunkRoot, &JunkX, &JunkY,
			   &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0))
	return;

    switch (event->xproperty.atom)
    {
    case XA_WM_NAME:
	if (XGetWindowProperty(dpy, win->w, event->xproperty.atom, 0L,
			       MAX_NAME_LEN, False, XA_STRING, &actual,
			       &actual_format, &nitems, &bytesafter,
			       (unsigned char **)&prop) != Success ||
	    actual == None)
	    return;
	if (!prop)
	    prop = NoName;
	WIN_FreeNames(win, True, False);

	win->name = prop;

	/* fix the name in the title bar */
	if (!(win->flags & ICONIFIED))
	    DEC_DrawTitleBar(scr, win, (scr->mwm_highlight == win), True);

	/*
	 * if the icon name is NoName, set the name of the icon to be
	 * the same as the window 
	 */
	if (win->icon_active_label == NoName)
	{
	    win->icon_active_label = win->name;
	    ICON_UpdateWindow(scr, win, False);
	}
	break;

    case XA_WM_ICON_NAME:
	if (XGetWindowProperty(dpy, win->w, event->xproperty.atom, 0,
			       MAX_ICON_NAME_LEN, False, XA_STRING, &actual,
			       &actual_format, &nitems, &bytesafter,
			       (unsigned char **)&prop) != Success ||
	    actual == None)
	    return;
	if (!prop)
	    prop = NoName;
	WIN_FreeNames(win, False, True);
	win->icon_label = prop;
	ICON_UpdateWindow(scr, win, False);
	break;

    case XA_WM_HINTS:
	if (win->wmhints)
	    XFree((char *)win->wmhints);
	win->wmhints = XGetWMHints(dpy, event->xany.window);

	if (win->wmhints == NULL)
	    return;

	if ((win->wmhints->flags & IconPixmapHint) ||
	    (win->wmhints->flags & IconWindowHint))
	{
	    if (win->icon_w)
		XDestroyWindow(dpy, win->icon_w);
	    XDeleteContext(dpy, win->icon_w, MwmContext);
	    if (win->flags & ICON_OURS)
	    {
		if (win->icon_pixmap_w != None)
		{
		    XDestroyWindow(dpy, win->icon_pixmap_w);
		    XDeleteContext(dpy, win->icon_pixmap_w, MwmContext);
		}
	    }
	    else
		XUnmapWindow(dpy, win->icon_pixmap_w);
	    win->icon_w = None;
	    win->icon_pixmap_w = None;
	    win->icon_pixmap = (Window)NULL;
	    if (win->flags & ICONIFIED)
	    {
		win->flags &= ~ICONIFIED;
		win->flags &= ~ICON_UNMAPPED;
		ICON_CreateWindow(scr, win,
				  win->icon_x_loc, win->icon_y_loc);

		WIN_Lower(scr, win);
		ICON_AutoPlace(scr, win);
		if (win->Desk == scr->current_desk)
		{
		    if (win->icon_w)
			XMapWindow(dpy, win->icon_w);
		    if (win->icon_pixmap_w != None)
			XMapWindow(dpy, win->icon_pixmap_w);
		}
		win->flags |= ICONIFIED;
		ICON_DrawWindow(scr, win);
	    }
	}
	break;

    case XA_WM_NORMAL_HINTS:
	PROP_GetWindowSizeHints(win);
	break;

    default:
	if (event->xproperty.atom == XA_WM_PROTOCOLS)
	{
	    PROP_GetWmProtocols(win);
	}
	else if (event->xproperty.atom == XA_WM_COLORMAP_WINDOWS)
	{
	    PROP_GetWmColormapWindows(win);	/* frees old data */
	    COLOR_InstallWindowColorMap(scr, scr->mwm_colormap);
	}
	else if (event->xproperty.atom == XA_WM_STATE)
	{
	    if ((Mwm.keyboard_focus_policy == XmEXPLICIT) &&
		(win == scr->mwm_focus) && (win != NULL))
	    {
		scr->mwm_focus = NULL;
		WIN_SetFocusInTree(win);
		WIN_SetFocus(scr, win->w, win);
		MISC_SetFocusSequence(scr);
	    }
	}
	else if (event->xproperty.atom == XA_MWM_HINTS)
	{
	    if (win)
	    {
		int width, height;

		PROP_GetMwmHints(win);

		DEC_ReselectDecorations(scr, win);

		win->frame_width = win->attr.width +
		    2 * win->boundary_width +
		    2 * win->matte_width;
		win->frame_height = win->attr.height +
		    win->title_height +
		    2 * win->boundary_width +
		    2 * win->matte_width;

		WIN_ConstrainWindow(scr, win,
				    &win->frame_width,
				    &win->frame_height);

		width = win->frame_width;
		win->frame_width = 0;
		height = win->frame_height;
		win->frame_height = 0;

		DEC_ConfigureDecorations(scr, win,
					 win->frame_x, win->frame_y,
					 width, height,
					 True);

		if (scr->mwm_highlight == win)
		{
		    scr->mwm_highlight = NULL;
		    DEC_DrawDecorations(scr, win, True, True, True, None);
		}
		else
		    DEC_DrawDecorations(scr, win, False, False, True, None);
	    }
	}
	else if (event->xproperty.atom == XA_MWM_MENU)
	{
	    if (win)
	    {
		if (win->mwm_menu)
		    XFree((char *)win->mwm_menu);

		MENU_DestroyWindowMenu(scr, win);

		PROP_GetMwmMenu(win);

		MENU_BuildWindowMenu(scr, win);
	    }
	}
	else if (event->xproperty.atom == XA_MWM_MESSAGES)
	{
	    if (win)
	    {

		if (win->mwm_messages)
		    XFree((char *)win->mwm_messages);

		PROP_GetMwmMessages(win);
	    }
	}
    }
}

/*
 * client message event handler
 */
static void
client_message(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    XEvent button;

    if ((event->xclient.message_type == XA_WM_CHANGE_STATE) &&
	(win) && (event->xclient.data.l[0] == IconicState) &&
	!(win->flags & ICONIFIED))
    {
	XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
		      &(button.xmotion.x_root),
		      &(button.xmotion.y_root),
		      &JunkX, &JunkY, &JunkMask);
	button.type = 0;
	FUNC_Execute(scr, F_ICONIFY, NULLSTR, event->xany.window,
		     win, &button, C_FRAME, 0, 0, 0, 0,
		     (MenuRoot *) 0);
    }
}

/*
 * expose event handler
 */
static void
expose(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    if (event->xexpose.count != 0)
	return;

    if (win)
    {
	if ((win->w == scr->pager_win) ||
	    (win->w == scr->pager_child_win))
	{
	    PAGER_Redraw(scr);
	}
	if ((event->xany.window == win->title))
	{
	    DEC_DrawTitleBar(scr, win, (scr->mwm_highlight == win), False);
	}
	else
	{
	    DEC_DrawDecorations(scr, win,
				(scr->mwm_highlight == win),
				True, True, event->xany.window);
	}
    }
    else
    {
	if (WIN_WindowToStruct(scr, event->xany.window))
	    PAGER_Redraw(scr);
    }
}

/*
 * DestroyNotify event handler
 */
static void
destroy_notify(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    WIN_DestroyWindow(scr, win);
}

/*
 * MapRequest event handler
 */
static void
map_request(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    WIN_MapWindow(scr, event->xmaprequest.window);
}

/*
 * MapNotify event handler
 */
static void
map_notify(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    if (!win)
	return;

    /*
     * Need to do the grab to avoid race condition of having server send
     * MapNotify to client before the frame gets mapped; this is bad because
     * the client would think that the window has a chance of being viewable
     * when it really isn't.
     */
    XGrabServer(dpy);
    if (win->icon_w)
	XUnmapWindow(dpy, win->icon_w);
    if (win->icon_pixmap_w != None)
	XUnmapWindow(dpy, win->icon_pixmap_w);

    if (win->mwm_hints &&
	(win->mwm_hints->flags & MWM_HINTS_INPUT_MODE) &&
	(win->mwm_hints->input_mode == MWM_INPUT_SYSTEM_MODAL))
    {
	XMapRaised(dpy, scr->shield_win);
	WIN_Raise(scr, win);
    }

    XMapSubwindows(dpy, win->frame);

    if (win->Desk == scr->current_desk)
    {
	XMapWindow(dpy, win->frame);
    }

    if (Mwm.keyboard_focus_policy == XmEXPLICIT && Mwm.startup_key_focus)
    {
	WIN_SetFocusInTree(win);
	WIN_SetFocus(scr, win->w, win);
	MISC_SetFocusSequence(scr);
    }
    if ((!(win->decorations & (MWM_DECOR_BORDER | MWM_DECOR_TITLE))) &&
	(win->boundary_width < 2))
    {
	DEC_DrawDecorations(scr, win, False, True, True, win->frame);
    }
    XSync(dpy, 0);
    XUngrabServer(dpy);
    XFlush(dpy);
    win->flags |= MAPPED;
    win->flags &= ~MAP_PENDING;
    win->flags &= ~ICONIFIED;
    win->flags &= ~ICON_UNMAPPED;
}

/*
 * UnmapNotify event handler
 */
static void
unmap_notify(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    int dstx, dsty;
    Window dumwin;
    XEvent dummy;

    /*
     * The July 27, 1988 ICCCM spec states that a client wishing to switch
     * to WithdrawnState should send a synthetic UnmapNotify with the
     * event field set to (pseudo-)root, in case the window is already
     * unmapped (which is the case for mwm for IconicState).  Unfortunately,
     * we looked for the MwmContext using that field, so try the window
     * field also.
     */
    if (!win)
    {
	event->xany.window = event->xunmap.window;
	if (XFindContext(dpy, event->xany.window,
			 MwmContext, (XPointer *)&win) == XCNOENT)
	    win = NULL;
    }

    if (!win)
	return;

    if (win->mwm_hints &&
	(win->mwm_hints->flags & MWM_HINTS_INPUT_MODE) &&
	(win->mwm_hints->input_mode == MWM_INPUT_SYSTEM_MODAL))
    {
	XUnmapWindow(dpy, scr->shield_win);
    }

    if (win == scr->mwm_highlight)
	scr->mwm_highlight = NULL;

    if (scr->mwm_last_focus == win)
	scr->mwm_last_focus = NULL;

    if (win == scr->mwm_focus && Mwm.keyboard_focus_policy == XmEXPLICIT &&
	Mwm.auto_key_focus)
    {
	if (win->next)
	{
	    WIN_SetFocusInTree(win->next);
	    WIN_SetFocus(scr, win->next->w, win->next);
	}
	else if (win->ancestor)
	{
	    WIN_SetFocusInTree(win->ancestor);
	    WIN_SetFocus(scr, win->ancestor->w, win->ancestor);
	}
	else
        {
	    WIN_SetFocus(scr, scr->no_focus_win, NULL);
        }
    }

    if (scr->mwm_focus == win)
    {
	WIN_SetFocus(scr, scr->no_focus_win, NULL);
    }

    if (win == scr->mwm_pushed)
	scr->mwm_pushed = NULL;

    MISC_SetFocusSequence(scr);
    if (win == scr->mwm_colormap)
	scr->mwm_colormap = NULL;

    if (!(win->flags & MAPPED) && !(win->flags & ICONIFIED))
	return;

    XGrabServer(dpy);

    if (XCheckTypedWindowEvent(dpy, event->xunmap.window,
			       DestroyNotify, &dummy))
    {
        /* here was the problem with the menu focus.  If the
         * destroy notify happened later, we need to reset the focus,
         * as the application has had a window of opportunity to steal it
         * back.
         */
	WIN_DestroyWindow(scr, win);

	XUngrabServer(dpy);

	return;
    }

    /*
     * The program may have unmapped the client window, from either
     * NormalState or IconicState.  Handle the transition to WithdrawnState.
     *
     * We need to reparent the window back to the root (so that mwm exiting 
     * won't cause it to get mapped) and then throw away all state (pretend 
     * that we've received a DestroyNotify).
     */
    if (XTranslateCoordinates(dpy, event->xunmap.window, scr->root_win,
			      0, 0, &dstx, &dsty, &dumwin))
    {
	XEvent ev;
	Bool reparented;

	reparented = XCheckTypedWindowEvent(dpy, event->xunmap.window,
					    ReparentNotify, &ev);
	PROP_SetState(win, WithdrawnState);
	if (reparented)
	{
	    if (win->old_bw)
		XSetWindowBorderWidth(dpy, event->xunmap.window,
				      win->old_bw);
	    if (win->wmhints && (win->wmhints->flags & IconWindowHint))
		XUnmapWindow(dpy, win->wmhints->icon_window);
	}
	else
	    WIN_RestoreWithdrawn(scr, win, False);

	if (event->xunmap.window != scr->restart_win &&
	    event->xunmap.window != scr->quit_win &&
	    event->xunmap.window != scr->toggle_win)
	{
	    XRemoveFromSaveSet(dpy, event->xunmap.window);
	    XSelectInput(dpy, event->xunmap.window, NoEventMask);
	}
	else
	    XDeleteContext(dpy, event->xunmap.window, MwmContext);

	WIN_DestroyWindow(scr, win);	/* do not need to mash event before */
    }
    /* else window no longer exists and we'll get a destroy notify */

    XUngrabServer(dpy);

    XFlush(dpy);
}

/*
 * MotionNotify event handler
 */
static void
motion_notify(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    /* here is the code for dragging the viewport around within the pager. */
    if ((win) && (win->w == scr->pager_win) &&
	(!(win->flags & ICONIFIED)))
    {
	if (event->xmotion.state == Button3MotionMask)
	{
	    EnablePagerRedraw = False;
	    PAGER_SwitchPage(scr, False, False, event);
	}
    }
}

/*
 * ButtonPress event handler
 */
static void
button_press(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    unsigned int modifier;
    MouseButton *MouseEntry;
    char *Action;
    Window x;
    int LocalContext;
    static Time last_time = 0;
    Boolean click = False, click2 = False, match = False;

    if (event->xbutton.window == scr->shield_win)
    {
	XBell(dpy, 100);
	return;
    }

    /* click to focus stuff goes here */
    if ((Mwm.keyboard_focus_policy == XmEXPLICIT) &&
	(win != scr->mwm_grabbing) &&
	((event->xbutton.state &
	 (ControlMask|Mod1Mask|Mod2Mask|Mod3Mask|Mod4Mask|Mod5Mask)) == 0))
    {
	if (win)
	{
	    WIN_SetFocusInTree(win);
	    WIN_SetFocus(scr, win->w, win);
	    MISC_SetFocusSequence(scr);

	    if (win->focus_auto_raise)
	    {
		WIN_Raise(scr, win);
	    }

	    /* Why is this here? Seems to cause breakage with
	     * non-focusing windows! */
	    if (!(win->flags & ICONIFIED))
	    {
		XSync(dpy, 0);
		XAllowEvents(dpy, ReplayPointer, CurrentTime);
		XSync(dpy, 0);
		return;
	    }

	}
    }

    XSync(dpy, 0);
    XAllowEvents(dpy, ReplayPointer, CurrentTime);
    XSync(dpy, 0);

    /* here is the code for moving windows in the pager. */
    if ((win) && (win->w == event->xbutton.window) &&
	(win->w == scr->pager_win) && (event->xbutton.button == Button2))
    {
	PAGER_Update(scr, event);
	return;
    }
    if ((win) && (win->w == scr->pager_win) &&
	(!(win->flags & ICONIFIED)) &&
	(event->xbutton.button == Button3) &&
	(event->xany.window == scr->pager_win))
    {
	EnablePagerRedraw = False;
	PAGER_SwitchPage(scr, False, False, event);
    }

    scr->event_context = EVENT_GetContext(scr, win, event, &scr->pressed_win);
    LocalContext = scr->event_context;
    x = scr->pressed_win;

    if (scr->event_context == C_TITLE)
    {
	DEC_DrawTitleBar(scr, win, (scr->mwm_highlight == win), False);
    }
    else
    {
	DEC_DrawDecorations(scr, win, (scr->mwm_highlight == win),
			    True, True, scr->pressed_win);
    }

    scr->mwm_event = win;

    /* we have to execute a function or pop up a menu */
    modifier = (event->xbutton.state & MOD_MASK);

    /* need to search for an appropriate mouse binding */
    click = is_click(scr, event->xbutton.x, event->xbutton.y, event);
    if (click && (MISC_FetchEventTime() - last_time) < Mwm.double_click_time)
    {
	click2 = True;
    }

    last_time = MISC_FetchEventTime();

    /*
     * This loop only seems to handle double-clicked stuff
     */
    MouseEntry = scr->buttons;
    while (click2 && MouseEntry != (MouseButton *)0)
    {
	if ((MouseEntry->button == event->xbutton.button ||
	     MouseEntry->button == 0) &&
	    (MouseEntry->context & scr->event_context) &&
	    (MouseEntry->modifier == AnyModifier ||
	     MouseEntry->modifier == (modifier & (~LockMask))) &&
	    (MouseEntry->mask & (ButtonPressMask | ButtonReleaseMask)))
	{
	    /* got a match, now process it */
	    if (MouseEntry->func != F_NOP)
	    {
		if (MouseEntry->count == 2)
		{
		    match = True;
		    Action = MouseEntry->item ? MouseEntry->item->action : NULL;
		    FUNC_Execute(scr, MouseEntry->func, Action,
				 event->xany.window, win, event,
				 scr->event_context,
				 MouseEntry->val1, MouseEntry->val2,
				 MouseEntry->val1_unit, MouseEntry->val2_unit,
				 MouseEntry->menu);
		}
	    }
	}
	MouseEntry = MouseEntry->next;
    }

    /* Still only for double-clicks as "match" can only be set in the loop */
    if (match)
    {
	scr->pressed_win = None;
	if (LocalContext != C_TITLE)
	{
	    DEC_DrawDecorations(scr, scr->mwm_event,
				(scr->mwm_highlight == scr->mwm_event),
				True, True, x);
	}
	else
	{
	    DEC_DrawTitleBar(scr, scr->mwm_event,
			     (scr->mwm_highlight == scr->mwm_event), False);
	}
	scr->mwm_event = NULL;

	return;
    }

    /*
     * If we had a match above (in double-clicks), we don't come here.
     *
     * Now we traverse the list of button bindings.
     *
     * Careful inspection of mwmparse.y reveals that this is made up from
     * two sources: builtin bindings, and bindings read from the configuration
     * file.
     *
     * FIX ME the builtins are not completely motif compliant (e.g. the window
     * menu that's tied to <Btn1Down> on the root window).
     * BTW the builtins are defined in mwm.h.
     */
    MouseEntry = scr->buttons;
    while (MouseEntry != (MouseButton *)0)
    {
	if ((MouseEntry->button == event->xbutton.button ||
	     MouseEntry->button == 0) &&
	    (MouseEntry->context & scr->event_context) &&
	    (MouseEntry->modifier == AnyModifier ||
	     MouseEntry->modifier == (modifier & (~LockMask))) &&
	    (MouseEntry->mask & ButtonPressMask))
	{
	    /* got a match, now process it, unless the user clicked, and it's
	     * interactive */

	    /*
	     * Rewrote the test below so I can understand it. Is it just me?
	     */
#if 1
	    if (MouseEntry->func == F_NOP) {
		MouseEntry = MouseEntry->next;
		continue;
	    }

	    if (click && !click2 && (MouseEntry->func == F_MOVE ||
				       MouseEntry->func == F_RESIZE ||
				       MouseEntry->func == F_POPUP ||
				       MouseEntry->func == F_WINDOWLIST ||
				       MouseEntry->func == F_W_POPUP)) {
		MouseEntry = MouseEntry->next;
		continue;
	    }
#else
	    /* Uh oh. */
	    if (MouseEntry->func != F_NOP &&
		!(click && !click2 && (MouseEntry->func == F_MOVE ||
				       MouseEntry->func == F_RESIZE ||
				       MouseEntry->func == F_WINDOWLIST ||
				       MouseEntry->func == F_POPUP ||
				       MouseEntry->func == F_W_POPUP)))
#endif
	    {

		if (!(MouseEntry->mask & ButtonReleaseMask) &&
		    MouseEntry->count == 0)
		{
		    Action = MouseEntry->item ? MouseEntry->item->action : NULL;
		    FUNC_Execute(scr, MouseEntry->func, Action,
				 event->xany.window, win, event,
				 scr->event_context,
				 MouseEntry->val1, MouseEntry->val2,
				 MouseEntry->val1_unit, MouseEntry->val2_unit,
				 MouseEntry->menu);
		}
	    }
	}

	MouseEntry = MouseEntry->next;
    }

#if 1
    /* Hack.
	Support a single button press only for F_W_POPUP
     */
    MouseEntry = scr->buttons;
    while (!click && !click2 && MouseEntry != (MouseButton *)0)
    {
	if ((MouseEntry->button == event->xbutton.button ||
	     MouseEntry->button == 0) &&
	    (MouseEntry->context & scr->event_context) &&
	    (MouseEntry->modifier == AnyModifier ||
	     MouseEntry->modifier == (modifier & (~LockMask))) &&
	    (MouseEntry->mask & (ButtonPressMask | ButtonReleaseMask)))
	{

	    /* got a match, now process it */
	    if (MouseEntry->func == F_W_POPUP)
	    {
#if 0
		if (MouseEntry->count == 1)
#endif
		{
		    Action = MouseEntry->item ? MouseEntry->item->action : NULL;
		    /* win->custom_menu = NULL;	*/ /* FIX ME */
		    if (MouseEntry->menu == NULL)
			MouseEntry->menu = (MenuRoot *)DEFAULT_WIN_MENU_NAME;
		    FUNC_Execute(scr, F_W_POPUP, Action,
				 event->xany.window, win, event,
				 scr->event_context,
				 MouseEntry->val1, MouseEntry->val2,
				 MouseEntry->val1_unit, MouseEntry->val2_unit,
				 MouseEntry->menu);
		}
	    }
	}
	MouseEntry = MouseEntry->next;
    }
#endif

    /* This only works for clicks, not for <Btn1Down> */
    MouseEntry = scr->buttons;
    while (click && !click2 && MouseEntry != (MouseButton *)0)
    {
	if ((MouseEntry->button == event->xbutton.button ||
	     MouseEntry->button == 0) &&
	    (MouseEntry->context & scr->event_context) &&
	    (MouseEntry->modifier == AnyModifier ||
	     MouseEntry->modifier == (modifier & (~LockMask))) &&
	    (MouseEntry->mask & (ButtonPressMask | ButtonReleaseMask)))
	{

	    /* got a match, now process it */
	    if (MouseEntry->func != F_NOP)
	    {
		if (MouseEntry->count == 1)
		{
		    Action = MouseEntry->item ? MouseEntry->item->action : NULL;
		    FUNC_Execute(scr, MouseEntry->func, Action,
				 event->xany.window, win, event,
				 scr->event_context,
				 MouseEntry->val1, MouseEntry->val2,
				 MouseEntry->val1_unit, MouseEntry->val2_unit,
				 MouseEntry->menu);
		}
	    }
	}
	MouseEntry = MouseEntry->next;
    }

    scr->pressed_win = None;
    if (LocalContext != C_TITLE)
    {
	DEC_DrawDecorations(scr, scr->mwm_event,
			    (scr->mwm_highlight == scr->mwm_event),
			    True, True, x);
    }
    else
    {
	DEC_DrawTitleBar(scr, scr->mwm_event,
			 (scr->mwm_highlight == scr->mwm_event), False);
    }

    scr->mwm_event = NULL;
}

/*
 * ButtonRelease event handler
 */
static void
button_release(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    unsigned int modifier;
    MouseButton *MouseEntry;
    char *Action;

    if ((win) && (event->xany.window == scr->pager_win) &&
	(!(win->flags & ICONIFIED)))
    {
	switch (event->xbutton.button)
	{
	default:
	case Button1:
	    PAGER_SwitchPage(scr, True, True, event);
	    break;
	case Button3:
	    EnablePagerRedraw = True;
	    PAGER_SwitchPage(scr, False, False, event);
	    PAGER_Clear(scr);
	    break;
	}
    }

    scr->event_context = EVENT_GetContext(scr, win, event, &scr->pressed_win);
    modifier = (event->xbutton.state & MOD_MASK);

    /* need to search for an appropriate mouse binding */
    MouseEntry = scr->buttons;
    while (MouseEntry != (MouseButton *)0)
    {
	if ((MouseEntry->button == event->xbutton.button ||
	     MouseEntry->button == 0) &&
	    (MouseEntry->count == 0) &&
	    (MouseEntry->context & scr->event_context) &&
	    (MouseEntry->modifier == AnyModifier ||
	     MouseEntry->modifier == (modifier & (~LockMask))) &&
	    (MouseEntry->mask & ButtonReleaseMask))
	{
	    /* got a match, now process it */
	    if (MouseEntry->func != F_NOP)
	    {
		Action = MouseEntry->item ? MouseEntry->item->action : NULL;
		FUNC_Execute(scr, MouseEntry->func, Action,
			     event->xany.window, win, event,
			     scr->event_context,
			     MouseEntry->val1, MouseEntry->val2,
			     MouseEntry->val1_unit, MouseEntry->val2_unit,
			     MouseEntry->menu);
		break;
	    }
	}
	MouseEntry = MouseEntry->next;
    }
}

/*
 * EnterNotify event handler
 */
static void
enter_notify(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    XEnterWindowEvent *ewp = &event->xcrossing;
    XEvent d;

    /* look for a matching leaveNotify which would nullify this enterNotify */
    if (XCheckTypedWindowEvent(dpy, ewp->window, LeaveNotify, &d))
    {
	MISC_StashEventTime(&d);
	if ((d.xcrossing.mode == NotifyNormal) &&
	    (d.xcrossing.detail != NotifyInferior))
	    return;
    }

    /* an EnterEvent in one of the PanFrameWindows activates the Paging */
    if (PAN_IsPannerWindow(scr, ewp->window))
    {
	int delta_x = 0, delta_y = 0;

	/* this was in the HandleMotionNotify before, HEDU */
	PAN_PanDesktop(scr, scr->edge_scroll_x, scr->edge_scroll_y,
		       &event->xcrossing.x_root, &event->xcrossing.y_root,
		       &delta_x, &delta_y, True, event);
	return;
    }

    if (event->xany.window == scr->root_win)
    {
	if (!(Mwm.keyboard_focus_policy == XmEXPLICIT))
	{
	    WIN_SetFocus(scr, scr->no_focus_win, NULL);
	    MISC_SetFocusSequence(scr);
	}
	if (Mwm.colormap_focus_policy != XmKEYBOARD)
	    COLOR_InstallWindowColorMap(scr, NULL);
	return;
    }

    /* make sure its for one of our windows */
    if (!win)
	return;

    if (!(Mwm.keyboard_focus_policy == XmEXPLICIT))
    {
	if (scr->mwm_focus != win)
	{
	    if ((Mwm.auto_raise_delay > 0) && (!(win->flags & VISIBLE)))
		MISC_SetTimer(Mwm.auto_raise_delay);
	    WIN_SetFocusInTree(win);
	    WIN_SetFocus(scr, win->w, win);
	    MISC_SetFocusSequence(scr);
	}
	else
	{
	    WIN_SetFocusInTree(win);
	    WIN_SetFocus(scr, win->w, win);
	}
    }
    if (Mwm.colormap_focus_policy == XmPOINTER)
    {
	if ((!(win->flags & ICONIFIED)) && (event->xany.window == win->w))
	    COLOR_InstallWindowColorMap(scr, win);
	else
	    COLOR_InstallWindowColorMap(scr, NULL);
    }
}

/*
 * LeaveNotify event handler
 */
static void
leave_notify(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    /* If we leave the root window, then we're really moving
     * another screen on a multiple screen display, and we
     * need to de-focus and unhighlight to make sure that we
     * don't end up with more than one highlighted window at a time */
    if (event->xcrossing.window == scr->root_win)
    {
	if (event->xcrossing.mode == NotifyNormal)
	{
	    if (event->xcrossing.detail != NotifyInferior)
	    {
		if (scr->mwm_focus != NULL)
		{
		    WIN_SetFocus(scr, scr->no_focus_win, NULL);
		}
		if (scr->mwm_highlight != NULL)
		    DEC_DrawDecorations(scr, scr->mwm_highlight, False, True, True, None);
	    }
	}
    }
}

/*
 * ConfigureRequest event handler
 */
static void
configure_request(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    XWindowChanges xwc;
    unsigned long xwcm;
    int x, y, width, height;
    XConfigureRequestEvent *cre = &event->xconfigurerequest;

    /*
     * event->xany.window is event->xconfigurerequest.parent, so win will
     * be wrong
     */
    event->xany.window = cre->window;	/* mash parent field */
    if (XFindContext(dpy, cre->window, MwmContext, (XPointer *)&win) ==
	XCNOENT)
	win = NULL;

    /*
     * According to the July 27, 1988 ICCCM draft, we should ignore size and
     * position fields in the WM_NORMAL_HINTS property when we map a window.
     * Instead, we'll read the current geometry.  Therefore, we should respond
     * to configuration requests for windows which have never been mapped.
     */
    if (!win || (win->icon_w == cre->window))
    {
	xwcm = cre->value_mask &
	    (CWX | CWY | CWWidth | CWHeight | CWBorderWidth);
	xwc.x = cre->x;
	xwc.y = cre->y;
	if ((win) && ((win->icon_w == cre->window)))
	{
	    win->icon_xl_loc = cre->x;
	    win->icon_x_loc = cre->x +
		(win->icon_w_width - win->icon_p_width) / 2;
	    win->icon_y_loc = cre->y - win->icon_p_height;
	}
	xwc.width = cre->width;
	xwc.height = cre->height;
	xwc.border_width = cre->border_width;
	XConfigureWindow(dpy, event->xany.window, xwcm, &xwc);

	if (win)
	{
	    xwc.x = win->icon_x_loc;
	    xwc.y = win->icon_y_loc - win->icon_p_height;
	    xwcm = cre->value_mask & (CWX | CWY);
	    if (win->icon_pixmap_w != None)
		XConfigureWindow(dpy, win->icon_pixmap_w, xwcm, &xwc);
	    xwc.x = win->icon_x_loc;
	    xwc.y = win->icon_y_loc;
	    xwcm = cre->value_mask & (CWX | CWY);
	    if (win->icon_w != None)
		XConfigureWindow(dpy, win->icon_w, xwcm, &xwc);
	}
	return;
    }

    if (cre->value_mask & CWStackMode)
    {
	MwmWindow *otherwin;

	xwc.sibling = (((cre->value_mask & CWSibling) &&
			(XFindContext(dpy, cre->above, MwmContext,
				      (XPointer *)&otherwin) == XCSUCCESS))
		       ? otherwin->frame : cre->above);
	xwc.stack_mode = cre->detail;
	XConfigureWindow(dpy, win->frame,
			 cre->value_mask & (CWSibling | CWStackMode), &xwc);
    }

    {
	int xws, yws, xbs, ybs;
	unsigned wws, hws, wbs, hbs;
	int boundingShaped, clipShaped;

	XShapeQueryExtents(dpy, win->w, &boundingShaped, &xws, &yws, &wws,
			   &hws, &clipShaped, &xbs, &ybs, &wbs, &hbs);
	win->wShaped = boundingShaped;
    }

    /* Don't modify frame_XXX fields before calling SetupWindow! */
    x = win->frame_x;
    y = win->frame_y;
    width = win->frame_width;
    height = win->frame_height;

    /* for restoring */
    if (cre->value_mask & CWBorderWidth)
    {
	win->old_bw = cre->border_width;
    }
    /* override even if border change */

    if (cre->value_mask & CWX)
	x = cre->x - win->boundary_width - win->bw - win->matte_width;
    if (cre->value_mask & CWY)
	y = cre->y - win->boundary_width - win->title_height - win->bw - win->matte_width;
    if (cre->value_mask & CWWidth)
	width = cre->width + 2 * win->boundary_width +
	    2 * win->matte_width;
    if (cre->value_mask & CWHeight)
	height = cre->height + win->title_height + 2 * win->boundary_width +
	    2 * win->matte_width;

    /*
     * SetupWindow (x,y) are the location of the upper-left outer corner and
     * are passed directly to XMoveResizeWindow (frame).  The (width,height)
     * are the inner size of the frame.  The inner width is the same as the 
     * requested client window width; the inner height is the same as the
     * requested client window height plus any title bar slop.
     */
    DEC_ConfigureDecorations(scr, win, x, y, width, height, False);

    PAGER_Clear(scr);
}

/*
 * record fully visible windows for use in the RaiseLower function and the
 * OnTop type windows.
 */
static void
visibility_notify(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    XVisibilityEvent *vevent = (XVisibilityEvent *)event;

    if (win)
    {
	if (vevent->state == VisibilityUnobscured)
	    win->flags |= VISIBLE;
	else
	    win->flags &= ~VISIBLE;
    }
}

/*
 * shape notification event handler
 */
static void
shape_notify(ScreenInfo *scr, MwmWindow *win, XEvent *event)
{
    XShapeEvent *sev = (XShapeEvent *) & event;

    if (!win)
	return;
    if (sev->kind != ShapeBounding)
	return;
    win->wShaped = sev->shaped;

    DEC_SetShape(win, win->frame_width);
}

/*
 * event handling initialization
 */
void
EVENT_Initialize(void)
{
    XShapeQueryExtension(dpy, &ShapeEventBase, &ShapeErrorBase);
}

/*
 * return the window context for an XEvent.
 */
int
EVENT_GetContext(ScreenInfo *scr, MwmWindow *t, XEvent *e, Window *w)
{
    int ctxt, i;

    if (!t)
	return C_ROOT;

    ctxt = C_NO_CONTEXT;
    *w = e->xany.window;

    if (*w == scr->no_focus_win)
	return C_ROOT;

    /* Since key presses and button presses are grabbed in the frame
     * when we have re-parented windows, we need to find out the real
     * window where the event occured */
    if ((e->type == KeyPress) && (e->xkey.subwindow != None))
	*w = e->xkey.subwindow;

    if ((e->type == ButtonPress) && (e->xbutton.subwindow != None) &&
	((e->xbutton.subwindow == t->w) || (e->xbutton.subwindow == t->parent)))
	*w = e->xbutton.subwindow;

    if (*w == scr->root_win)
	ctxt = C_ROOT;
    if (t)
    {
	if (*w == t->title)
	    ctxt = C_TITLE;
	if ((*w == t->w) || (*w == t->parent))
	    ctxt = C_WINDOW;
	if (*w == t->icon_w)
	    ctxt = C_ICON;
	if (*w == t->icon_pixmap_w)
	    ctxt = C_ICON;
	if (*w == t->frame)
	    ctxt = C_FRAME;
	for (i = 0; i < 4; i++)
	    if (*w == t->corners[i])
	    {
		ctxt = C_FRAME;
	    }
	for (i = 0; i < 4; i++)
	    if (*w == t->sides[i])
	    {
		ctxt = C_FRAME;
	    }
	if (*w == t->menub)
	{
	    ctxt = C_MENUB;
	}
	if (*w == t->minimizeb)
	{
	    ctxt = C_MINIMIZEB;
	}
	if (*w == t->maximizeb)
	{
	    ctxt = C_MAXIMIZEB;
	}
    }
    return ctxt;
}

/*
 * Waits for next X event, or for an auto-raise timeout.
 */
int
EVENT_Next(XEvent *event)
{
#ifndef HAVE_GETITIMER
    struct itimerval
    {
	struct timeval it_value;
    };
#endif
    struct itimerval value;
    fd_set in_fdset, out_fdset;
    Window child;
    int retval, i;
    ScreenInfo *scr;

    /* Do this prior to the select() call, in case the timer already expired,
     * in which case the select would never return. */
    if (alarmed)
    {
	alarmed = False;

	for (i = 0; i < Mwm.number_of_screens; i++)
	{
	    scr = Mwm.screen_info[i];
	    XQueryPointer(dpy, scr->root_win, &JunkRoot, &child, &JunkX, &JunkY,
			  &JunkX, &JunkY, &JunkMask);
	    if ((scr->mwm_focus != NULL) && (child == scr->mwm_focus->frame))
	    {
		if (!(scr->mwm_focus->flags & VISIBLE) &&
		    scr->mwm_focus->focus_auto_raise)
		{
		    WIN_Raise(scr, scr->mwm_focus);
		    PAGER_Clear(scr);
		}
	    }
	}
	return 0;
    }

#ifdef HAVE_GETITIMER
    getitimer(ITIMER_REAL, &value);
#else
    value.it_value.tv_usec = 10000;
    value.it_value.tv_sec = 0;
#endif

    FD_ZERO(&in_fdset);
    FD_SET(x_fd, &in_fdset);
    FD_ZERO(&out_fdset);

    /* Do this IMMEDIATELY prior to select, to prevent any nasty
     * queued up X events from just hanging around waiting to be
     * flushed */
    XFlush(dpy);
    if (XPending(dpy))
    {
	XNextEvent(dpy, event);
	MISC_StashEventTime(event);
	return 1;
    }

    /* Zap all those zombies! */
    /* If we get to here, then there are no X events waiting to be processed.
     * Just take a moment to check for dead children. */
    ReapChildren();

    XFlush(dpy);
    if ((value.it_value.tv_usec != 0) ||
	(value.it_value.tv_sec != 0))
    {
#ifdef __hpux
	retval = select(fd_width, (int *)&in_fdset, 0, 0, &value.it_value);
#else
	retval = select(fd_width, &in_fdset, 0, 0, &value.it_value);
#endif
    }
    else
    {
#ifdef __hpux
	retval = select(fd_width, (int *)&in_fdset, (int *)&out_fdset,
			0, NULL);
#else
	retval = select(fd_width, &in_fdset, &out_fdset, 0, NULL);
#endif
    }

    return 0;
}

/*
 * handle a single X event stored in global var Event
 */
void
EVENT_Dispatch(XEvent *event)
{
    Window w = event->xany.window;
    Widget wid;
    ScreenInfo *scr;
    MwmWindow *win;

    MISC_StashEventTime(event);
    scr = SCREEN_EventToStruct(event);

    if (XFindContext(dpy, w, MwmContext, (XPointer *)&win) == XCNOENT)
	win = NULL;

    if ((wid = XtWindowToWidget(event->xany.display, w)) != NULL)
    {
	Boolean ret;

	if (event->xany.type == MapNotify && XtIsShell(wid))
	{
	    map_request(scr, win, event);
	    map_notify(scr, win, event);
	}

	if (event->xany.type == UnmapNotify && XtIsShell(wid))
	    unmap_notify(scr, win, event);

#if XtSpecificationRelease >= 6
	ret = XtDispatchEventToWidget(wid, event);
#else
	ret = XtDispatchEvent(event);
#endif

	if (ret)
	    return;
    }

    switch (event->type)
    {
    case Expose:
	expose(scr, win, event);
	break;

    case DestroyNotify:
	destroy_notify(scr, win, event);
	break;

    case MapRequest:
	map_request(scr, win, event);
	break;

    case MapNotify:
	map_notify(scr, win, event);
	break;

    case UnmapNotify:
	unmap_notify(scr, win, event);
	break;

    case MotionNotify:
	motion_notify(scr, win, event);
	break;

    case ButtonPress:
	button_press(scr, win, event);
	break;

    case ButtonRelease:
	button_release(scr, win, event);
	break;

    case EnterNotify:
	enter_notify(scr, win, event);
	break;

    case LeaveNotify:
	leave_notify(scr, win, event);
	break;

    case FocusIn:
	focus_in(scr, win, event);
	break;

    case FocusOut:
        focus_out(scr, win, event);
        break;
        
    case ConfigureRequest:
	configure_request(scr, win, event);
	break;

    case ClientMessage:
	client_message(scr, win, event);
	break;

    case PropertyNotify:
	property_notify(scr, win, event);
	break;

    case KeyPress:
	key_press(scr, win, event);
	break;

    case VisibilityNotify:
	visibility_notify(scr, win, event);
	break;

    case ColormapNotify:
	color_map_notify(scr, win, event);
	break;

    default:
	if (event->type == (ShapeEventBase + ShapeNotify))
	    shape_notify(scr, win, event);
	break;
    }
}
