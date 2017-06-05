/* $Id: misc.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $ */
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
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <X11/Intrinsic.h>
#include <X11/keysym.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/XmosP.h>
#if XmVERSION >= 2
#include <XmI/XmI.h>
#endif

#include "mwm.h"


static Time lastTimestamp = CurrentTime;

/*
 * find a window in the tree
 */
static MwmWindow *
find_by_window(MwmWindow *win, Window w)
{
    MwmWindow *child, *tmp;

    if (win->w == w)
	return win;

    /* search breadth first */
    for (tmp = win->next; tmp != NULL; tmp = tmp->next)
    {
	if ((child = find_by_window(tmp, w)) != NULL)
	    return child;
    }

    /* then depth */
    for (tmp = win->child; tmp != NULL; tmp = tmp->child)
    {
	if ((child = find_by_window(tmp, w)) != NULL)
	    return child;
    }

    return NULL;
}


/*
 * find a window in the tree.  Returns a SIBLING in a window group.  The
 * caller must determine if the ancestor is valid.
 */
static MwmWindow *
find_by_group(MwmWindow *win, XID group)
{
    MwmWindow *child, *tmp;

    if (win->wmhints && (win->wmhints->flags & WindowGroupHint) &&
	win->wmhints->window_group == group)
    {
	return win;
    }

    tmp = win->next;
    if (tmp != NULL)
    {
	if ((child = find_by_group(tmp, group)) != NULL)
        {
	    return child;
        }
    }

    tmp = win->child;
    if (tmp != NULL)
    {
	if ((child = find_by_group(tmp, group)) != NULL)
        {
	    return child;
        }
    }

    return NULL;
}

/*
 * add a new child to a window group
 */
static void
add_child(MwmWindow *win, MwmWindow *child)
{
    if (win == NULL || child == NULL)
	return;

    if (win->child == NULL)
    {
	win->child = child;
	child->ancestor = win;
	child->next = child->prev = NULL;
	return;
    }

    child->ancestor = win;
    child->next = win->child;
    child->next->prev = child;
    win->child = child;
}

/*
 * remove a child from a window group
 * the child itself may have transient children.  These must be taken
 * care of elsewhere
 */
static void
remove_child(MwmWindow *win, MwmWindow *child)
{
    if (child == NULL || win == NULL)
	return;

    if (win->child == child)
    {
	win->child = child->next;
	if (child->next != NULL)
	    child->next->prev = NULL;
	child->next = child->prev = child->ancestor = NULL;
	return;
    }

    if (child->next != NULL)
	child->next->prev = child->prev;
    child->prev->next = child->next;
    child->next = child->prev = child->ancestor = NULL;
}

/*
 * print one window tree
 */
static void
print_window_tree(MwmWindow *win, int depth)
{
    int i;
    MwmWindow *tmp;

    if (win == NULL)
    {
	for (i = 0; i < depth; i++)
	    printf(" ");
	printf("none\n");
	return;
    }

    for (tmp = win; tmp != NULL; tmp = tmp->next)
    {
	for (i = 0; i < depth; i++)
	    printf(" ");
	if (tmp->name)
	    printf("Window: %08lx %s\n", tmp->w, tmp->name);
	else
	    printf("Window: %08lx\n", tmp->w);
	for (i = 0; i < depth; i++)
	    printf(" ");
	printf("Children:\n");
	print_window_tree(win->child, depth + 4);
    }
}

/*
 * see if a window is in a subtree
 */
static Boolean
window_in_subtree(MwmWindow *win, MwmWindow *sub)
{
    MwmWindow *tmp;

    if (sub == win)
	return True;

    for (tmp = win->child; tmp != NULL; tmp = tmp->next)
    {
	if (window_in_subtree(tmp, sub))
	    return True;
    }
    return False;
}

/*
 * Removes expose events for a specific window from the queue 
 */
extern int
MISC_FlushExpose(Window w)
{
    XEvent dummy;
    int i = 0;

    while (XCheckTypedWindowEvent(dpy, w, Expose, &dummy))
	i++;
    return i;
}

/*
 * Start/Stops the auto-raise timer
 */
extern void
MISC_SetTimer(int delay)
{
#ifdef HAVE_SETITIMER
    struct itimerval value;

    value.it_value.tv_usec = 1000 * (delay % 1000);
    value.it_value.tv_sec = delay / 1000;
    value.it_interval.tv_usec = 0;
    value.it_interval.tv_sec = 0;
    setitimer(ITIMER_REAL, &value, NULL);
#endif
}

/*
 * Records the time of the last processed event. Used in XSetInputFocus
 */
extern Boolean
MISC_StashEventTime(XEvent *ev)
{
    Time NewTimestamp = CurrentTime;

    switch (ev->type)
    {
    case KeyPress:
    case KeyRelease:
	NewTimestamp = ev->xkey.time;
	break;
    case ButtonPress:
    case ButtonRelease:
	NewTimestamp = ev->xbutton.time;
	break;
    case MotionNotify:
	NewTimestamp = ev->xmotion.time;
	break;
    case EnterNotify:
    case LeaveNotify:
	NewTimestamp = ev->xcrossing.time;
	break;
    case PropertyNotify:
	NewTimestamp = ev->xproperty.time;
	break;
    case SelectionClear:
	NewTimestamp = ev->xselectionclear.time;
	break;
    case SelectionRequest:
	NewTimestamp = ev->xselectionrequest.time;
	break;
    case SelectionNotify:
	NewTimestamp = ev->xselection.time;
	break;
    default:
	return False;
    }
    if (NewTimestamp > lastTimestamp)
	lastTimestamp = NewTimestamp;
    return True;
}

/*
 * fetch the last saved time
 */
extern Time
MISC_FetchEventTime()
{
    return lastTimestamp;
}

/******************************************************************************
 *
 * Cleare the CIRCULATED field of the window flags.
 * 
 *****************************************************************************/
extern void
MISC_SetFocusSequence(ScreenInfo *scr)
{
    MwmWindow *temp;
    int i = 0;

    temp = scr->mwm_root.next;
    while (temp != NULL)
    {
	temp->focus_sequence = i++;
	temp = temp->next;
    }
}

/*****************************************************************************
 *
 * Grab the pointer and keyboard
 *
 ****************************************************************************/
extern Boolean
MISC_Grab(ScreenInfo *scr, int cursor)
{
    int i = 0, val = 0;
    unsigned int mask;

    XSync(dpy, 0);
    /* move the keyboard focus prior to grabbing the pointer to
     * eliminate the enterNotify and exitNotify events that go
     * to the windows */
    if (scr->mwm_last_focus == NULL)
	scr->mwm_last_focus = scr->mwm_focus;
    WIN_SetFocus(scr, scr->no_focus_win, NULL);

    mask = ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
	PointerMotionMask | EnterWindowMask | LeaveWindowMask;
    while ((i < 1000) && (val = XGrabPointer(dpy, scr->root_win, True, mask,
				   GrabModeAsync, GrabModeAsync, scr->root_win,
					  scr->cursors[cursor], CurrentTime) !=
			  GrabSuccess))
    {
	i++;

	/* If you go too fast, other windows may not get a change to release
	 * any grab that they have. */
	_XmMicroSleep(1000);
    }

    /* If we fall out of the loop without grabbing the pointer, its
       time to give up */
    XSync(dpy, 0);

    if (val != GrabSuccess)
	return False;

    return True;
}


/*****************************************************************************
 *
 * UnGrab the pointer and keyboard
 *
 ****************************************************************************/
extern void
MISC_Ungrab(ScreenInfo *scr)
{
    Window w;

    XSync(dpy, 0);
    XUngrabPointer(dpy, CurrentTime);

    if (scr->mwm_last_focus != NULL)
    {
	w = scr->mwm_last_focus->w;

	/* if the window still exists, focus on it */
	if (w)
	{
	    WIN_SetFocusInTree(scr->mwm_last_focus);
	    WIN_SetFocus(scr, w, scr->mwm_last_focus);
	}
	scr->mwm_last_focus = NULL;
    }
    XSync(dpy, 0);
}

/*
 * Wait for all mouse buttons to be released 
 * This can ease some confusion on the part of the user sometimes 
 * 
 * Discard superflous button events during this wait period.
 */
extern void
MISC_WaitForButtonsUp(ScreenInfo *scr)
{
    Bool AllUp = False;
    XEvent JunkEvent;
    unsigned int mask;

    while (!AllUp)
    {
	XAllowEvents(dpy, ReplayPointer, CurrentTime);
	XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
		      &JunkX, &JunkY, &JunkX, &JunkY, &mask);

	if ((mask & (Button1Mask | Button2Mask | Button3Mask |
		     Button4Mask | Button5Mask)) == 0)
	    AllUp = True;
    }
    XSync(dpy, 0);
    while (XCheckMaskEvent(dpy,
			ButtonPressMask | ButtonReleaseMask | ButtonMotionMask,
			   &JunkEvent))
    {
	MISC_StashEventTime(&JunkEvent);
	XAllowEvents(dpy, ReplayPointer, CurrentTime);
    }

}

/*
 * For menus, move, and resize operations, we can effect keyboard 
 * shortcuts by warping the pointer.
 */
extern void
MISC_KeyboardShortcut(ScreenInfo *scr, XEvent *event, int ReturnEvent)
{
    int x, y, x_root, y_root;
    int move_size, x_move, y_move;
    KeySym keysym;

    /* Pick the size of the cursor movement */
    move_size = scr->components[MWM_MENU].f_height + HEIGHT_EXTRA;
    if (event->xkey.state & ControlMask)
	move_size = 1;
    if (event->xkey.state & ShiftMask)
	move_size = 100;

    keysym = XLookupKeysym(&event->xkey, 0);

    x_move = 0;
    y_move = 0;
    switch (keysym)
    {
    case XK_Up:
    case XK_k:
    case XK_p:
	y_move = -move_size;
	break;
    case XK_Down:
    case XK_n:
    case XK_j:
	y_move = move_size;
	break;
    case XK_Left:
    case XK_b:
    case XK_h:
	x_move = -move_size;
	break;
    case XK_Right:
    case XK_f:
    case XK_l:
	x_move = move_size;
	break;
    case XK_Return:
    case XK_space:
	/* beat up the event */
	event->type = ReturnEvent;
	break;
    default:
	break;
    }
    XQueryPointer(dpy, scr->root_win, &JunkRoot, &event->xany.window,
		  &x_root, &y_root, &x, &y, &JunkMask);

    if ((x_move != 0) || (y_move != 0))
    {
	/* beat up the event */
	XWarpPointer(dpy, None, scr->root_win, 0, 0, 0, 0, x_root + x_move,
		     y_root + y_move);

	/* beat up the event */
	event->type = MotionNotify;
	event->xkey.x += x_move;
	event->xkey.y += y_move;
	event->xkey.x_root += x_move;
	event->xkey.y_root += y_move;
    }
}

/*
 * find the root of a window tree
 */
extern MwmWindow *
MISC_RootOfTree(MwmWindow *win)
{
    MwmWindow *anc;

    if (win == NULL)
	return NULL;

    anc = win;
    while (anc->ancestor != NULL)
	anc = anc->ancestor;

    return anc;
}

/*
 * print the window tree
 */
extern void
MISC_PrintTree(ScreenInfo *scr)
{
    print_window_tree(scr->mwm_root.next, 0);
}

/*
 * add a window to the window tree
 */
extern void
MISC_AddToTree(ScreenInfo *scr, MwmWindow *win)
{
    MwmWindow *tmp;

    if ((win->flags & TRANSIENT) && win->transientfor != None)
    {
	tmp = find_by_window(&scr->mwm_root, win->transientfor);
	if (tmp)
	{
	    add_child(tmp, win);
	    return;
	}
    }

    if (win->wmhints && (win->wmhints->flags & WindowGroupHint))
    {
	tmp = find_by_group(&scr->mwm_root, win->wmhints->window_group);
	if (tmp && tmp->ancestor != NULL)
	{
	    add_child(tmp->ancestor, win);
	    return;
	}
    }

    win->next = scr->mwm_root.next;
    if (scr->mwm_root.next != NULL)
	scr->mwm_root.next->prev = win;
    win->prev = &scr->mwm_root;
    scr->mwm_root.next = win;
}

/*
 * remove a window from the window tree
 */
extern void
MISC_RemoveFromTree(ScreenInfo *scr, MwmWindow *win)
{
    if (win->ancestor != NULL)
    {
	remove_child(win->ancestor, win);
    }
    else
    {
	if (win->prev != NULL)
	    win->prev->next = win->next;
	if (win->next != NULL)
	    win->next->prev = win->prev;
    }
}

/*
 * after starting, fix up the transients whose parent windows weren't
 * captured yet
 */
extern void
MISC_FixupTransients(ScreenInfo *scr)
{
    MwmWindow *tmp, *ptr;

    for (tmp = scr->mwm_root.next; tmp != NULL;)
    {
	ptr = tmp->next;
	if (tmp->flags & TRANSIENT)
	{
	    tmp->ancestor = NULL;
	    MISC_RemoveFromTree(scr, tmp);
	    tmp->prev = tmp->next = NULL;
	    MISC_AddToTree(scr, tmp);
	}
	tmp = ptr;
    }

    /* under the assumption that their parent may not be found, clear the
     * transient flag */
    for (tmp = scr->mwm_root.next; tmp != NULL; tmp = tmp->next)
    {
	if (tmp->flags & TRANSIENT)
	{
	    tmp->flags &= ~TRANSIENT;
	    tmp->ancestor = NULL;
	}
    }
}

extern void
MISC_DestroyChildren(ScreenInfo *scr, MwmWindow *win)
{
    MwmWindow *child, *tmp;

    for (child = win->child; child != NULL; child = tmp)
    {
	tmp = child->next;
	WIN_DestroyWindow(scr, child);
    }
}

/* Debugging purposes only */
extern const char *
_MwmPrintF(int x)
{
	switch(x) {
	case F_NOP:	return "F_NOP";
	case F_BEEP:	return "F_BEEP";
	case F_CHANGE_WINDOWS_DESK:	return "F_CHANGE_WINDOWS_DESK";
	case F_CIRCULATE_DOWN:	return "F_CIRCULATE_DOWN";
	case F_CIRCULATE_UP:	return "F_CIRCULATE_UP";
	case F_CLOSE:	return "F_CLOSE";
	case F_DESK:	return "F_DESK";
	case F_EXEC:	return "F_EXEC";
	case F_FOCUS:	return "F_FOCUS";
	case F_FOCUS_COLOR:	return "F_FOCUS_COLOR";
	case F_FOCUS_KEY:	return "F_FOCUS_KEY";
	case F_GOTO_PAGE:	return "F_GOTO_PAGE";
	case F_ICONIFY:	return "F_ICONIFY";
	case F_LOWER:	return "F_LOWER";
	case F_MAXIMIZE:	return "F_MAXIMIZE";
	case F_MOVE:	return "F_MOVE";
	case F_MOVECURSOR:	return "F_MOVECURSOR";
	case F_NEXT_CMAP:	return "F_NEXT_CMAP";
	case F_NEXT_KEY:	return "F_NEXT_KEY";
	case F_NORMALIZE:	return "F_NORMALIZE";
	case F_NORM_AND_RAISE:	return "F_NORM_AND_RAISE";
	case F_PACK_ICONS:	return "F_PACK_ICONS";
	case F_PASS_KEYS:	return "F_PASS_KEYS";
	case F_POPUP:	return "F_POPUP";
	case F_PREV_CMAP:	return "F_PREV_CMAP";
	case F_PREV_KEY:	return "F_PREV_KEY";
	case F_QUIT:	return "F_QUIT";
	case F_RAISE:	return "F_RAISE";
	case F_RAISE_IT:	return "F_RAISE_IT";
	case F_RAISELOWER:	return "F_RAISELOWER";
	case F_RESIZE:	return "F_RESIZE";
	case F_RESTART:	return "F_RESTART";
	case F_REFRESH:	return "F_REFRESH";
	case F_REFRESH_WIN:	return "F_REFRESH_WIN";
	case F_RESTORE_AND_RAISE:	return "F_RESTORE_AND_RAISE";
	case F_SCREEN:	return "F_SCREEN";
	case F_SCROLL:	return "F_SCROLL";
	case F_SEND_MSG:	return "F_SEND_MSG";
	case F_SET_BEHAVIOR:	return "F_SET_BEHAVIOR";
	case F_STICK:	return "F_STICK";
	case F_TITLE:	return "F_TITLE";
	case F_TOGGLE_PAGE:	return "F_TOGGLE_PAGE";
	case F_WARP:	return "F_WARP";
	case F_WINDOWLIST:	return "F_WINDOWLIST";
	case F_W_POPUP:	return "F_W_POPUP";
	default:	return "??";
	}
}

extern const char *
_MwmPrintC(int x)
{
	static char res[256];

	res[0] = 0;
	if (x == C_NO_CONTEXT)
		return "C_NO_CONTEXT";
	if (x & C_WINDOW)
		strcat(res, " | C_WINDOW");
	if (x & C_TITLE)
		strcat(res, " | C_TITLE");
	if (x & C_ICON)
		strcat(res, " | C_ICON");
	if (x & C_ROOT)
		strcat(res, " | C_ROOT");
	if (x & C_FRAME)
		strcat(res, " | C_FRAME");
	if (x & C_MENUB)
		strcat(res, " | C_MENUB");
	if (x & C_MINIMIZEB)
		strcat(res, " | C_MINIMIZEB");
	if (x & C_MAXIMIZEB)
		strcat(res, " | C_MAXIMIZEB");
	return &res[2];
}
