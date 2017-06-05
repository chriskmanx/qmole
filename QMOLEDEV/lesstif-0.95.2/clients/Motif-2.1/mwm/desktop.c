/* $Id: desktop.c,v 1.1 2004/08/28 19:25:45 dannybackx Exp $ */
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

/****************************************************************************
 *
 * <Desktop description>
 *
 ****************************************************************************/

/*
 * Unmaps a window on transition to a new desktop
 */
static void
remove_from_desktop(ScreenInfo *scr, MwmWindow *t)
{
    XWindowAttributes winattrs;
    unsigned long eventMask;

    /*
     * Prevent the receipt of an UnmapNotify, since that would
     * cause a transition to the Withdrawn state.
     */
    XGetWindowAttributes(dpy, t->w, &winattrs);
    eventMask = winattrs.your_event_mask;
    XSelectInput(dpy, t->w, eventMask & ~StructureNotifyMask);
    if (t->flags & ICONIFIED)
    {
	if (t->icon_pixmap_w != None)
	    XUnmapWindow(dpy, t->icon_pixmap_w);
	if (t->icon_w != None)
	    XUnmapWindow(dpy, t->icon_w);
    }
    else if (t->flags & (MAPPED | MAP_PENDING))
    {
	XUnmapWindow(dpy, t->frame);
    }
    XSelectInput(dpy, t->w, eventMask);
    PAGER_UpdateView(scr, t);
}

/*
 * Maps a window on transition to a new desktop
 */
static void
add_to_desktop(ScreenInfo *scr, MwmWindow *t)
{
    if (t->flags & ICONIFIED)
    {
	if (t->icon_pixmap_w != None)
	    XMapWindow(dpy, t->icon_pixmap_w);
	if (t->icon_w != None)
	    XMapWindow(dpy, t->icon_w);
    }
    else if (t->flags & MAPPED)
    {
	XMapWindow(dpy, t->frame);
	t->flags |= MAP_PENDING;
	XMapWindow(dpy, t->parent);
    }
    PAGER_UpdateView(scr, t);
}

/*
 * save the desktop state
 */
void
DT_SaveState(ScreenInfo *scr)
{
    MwmWindow *t;
    unsigned long data[1];

    for (t = scr->mwm_root.next; t != NULL; t = t->next)
    {
	data[0] = (unsigned long)t->Desk;
	XChangeProperty(dpy, t->w, XA_WM_DESKTOP, XA_WM_DESKTOP, 32,
			PropModeReplace, (unsigned char *)data, 1);
    }

    data[0] = (unsigned long)scr->current_desk;
    XChangeProperty(dpy, scr->root_win, XA_WM_DESKTOP, XA_WM_DESKTOP, 32,
		    PropModeReplace, (unsigned char *)data, 1);

    XSync(dpy, 0);
}

/*
 * Move to a new desktop
 */
void
DT_ChangeDesks(ScreenInfo *scr, int val1, int val2)
{
    int oldDesk;
    MwmWindow *t;
    MwmWindow *FocusWin = 0;
    static MwmWindow *StickyWin = 0;

    oldDesk = scr->current_desk;

    if (val1 != 0)
    {
	scr->current_desk = scr->current_desk + val1;
    }
    else
    {
	scr->current_desk = val2;
	if (scr->current_desk == oldDesk)
	    return;
    }

    /* Scan the window list, mapping windows on the new Desk,
     * unmapping windows on the old Desk */
    XGrabServer(dpy);
    for (t = scr->mwm_root.next; t != NULL; t = t->next)
    {
	/* Only change mapping for non-sticky windows */
	if ((!(t->flags & STICKY)) && (!(t->flags & ICON_UNMAPPED)))
	{
	    if (t->Desk == oldDesk)
	    {
		if (scr->mwm_focus == t)
		    t->FocusDesk = oldDesk;
		else
		    t->FocusDesk = -1;
		remove_from_desktop(scr, t);
	    }
	    else if (t->Desk == scr->current_desk)
	    {
		add_to_desktop(scr, t);
		if (t->FocusDesk == scr->current_desk)
		{
		    FocusWin = t;
		}
	    }
	}
	else
	{
	    /* Window is sticky */
	    t->Desk = scr->current_desk;
	    if (scr->mwm_focus == t)
	    {
		t->FocusDesk = oldDesk;
		StickyWin = t;
	    }
	}
    }
    XUngrabServer(dpy);
    for (t = scr->mwm_root.next; t != NULL; t = t->next)
    {
	/* If its an icon, and its sticking, autoplace it so
	 * that it doesn't wind up on top a a stationary
	 * icon */
	if ((t->flags & STICKY) && (t->flags & ICONIFIED) &&
	    (!(t->flags & ICON_MOVED)) && (!(t->flags & ICON_UNMAPPED)))
	    ICON_AutoPlace(scr, t);
    }
    /* Better re-draw the pager now */
    PAGER_Clear(scr);

    if (Mwm.keyboard_focus_policy == XmEXPLICIT)
    {
	if (Mwm.auto_key_focus)
	{
	    if (FocusWin)
	    {
		WIN_SetFocusInTree(FocusWin);
		WIN_SetFocus(scr, FocusWin->w, FocusWin);
	    }
	    else if (StickyWin && (StickyWin->flags && STICKY))
	    {
		WIN_SetFocusInTree(StickyWin);
		WIN_SetFocus(scr, StickyWin->w, StickyWin);
	    }
	}
	else
        {
	    WIN_SetFocus(scr, scr->no_focus_win, NULL);
        }
    }
}

/*
 * Move to a new desktop
 */
void
DT_WindowChangingDesks(ScreenInfo *scr, MwmWindow *t, int val1)
{
    if (val1 == t->Desk)
	return;

    /* Scan the window list, mapping windows on the new Desk,
     * unmapping windows on the old Desk */
    /* Only change mapping for non-sticky windows */
    if ((!(t->flags & STICKY)) && (!(t->flags & ICON_UNMAPPED)))
    {
	if (t->Desk == scr->current_desk)
	{
	    t->Desk = val1;
	    remove_from_desktop(scr, t);
	}
	else if (val1 == scr->current_desk)
	{
	    t->Desk = val1;
	    /* If its an icon, auto-place it */
	    if (t->flags & ICONIFIED)
		ICON_AutoPlace(scr, t);
	    add_to_desktop(scr, t);
	}
	else
	    t->Desk = val1;

    }
    /* Better re-draw the pager now */
    PAGER_Clear(scr);
}
