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
 * This module is based on Twm, but has been significantly modified 
 * by Rob Nation 
 ****************************************************************************/
/***********************************************************************
 * The rest of it is all my fault -- MLM
 * mwm - "LessTif Window Manager"
 ***********************************************************************/


#include <LTconfig.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>

#include <X11/keysym.h>

#include "mwm.h"



static int menu_on = 0;
static MenuRoot *ActiveMenu = NULL;	/* the active menu */
static MenuItem *ActiveItem = NULL;	/* the active menu item */
int menuFromFrameOrWindowOrTitlebar = False;
int Stashed_X, Stashed_Y;
static int MenuY = 0;
static MenuRoot *PrevMenu = NULL;
static MenuItem *PrevItem = NULL;
static int PrevY = 0;

/*
 * Calculate the pixel offsets to the start of the character position we
 * want to underline and to the next character in the string.  Shrink by
 * one pixel from each end and the draw a line that long two pixels below
 * the character...
 */
static void
draw_underline(ScreenInfo *scr, Window w, GC gc, int x, int y,
	       char *txt, int posn)
{
    int off1 = XTextWidth(scr->components[MWM_MENU].font,
			  txt, posn);
    int off2 = XTextWidth(scr->components[MWM_MENU].font,
			  txt, posn + 1) - 1;

    XDrawLine(dpy, w, gc, x + off1, y + 2, x + off2, y + 2);
}

/*
 * Draws two horizontal lines to form a separator
 */
static void
draw_separator(Window w, GC TopGC, GC BottomGC, int x1, int y1, int x2, int y2,
	       int extra_off)
{
    XDrawLine(dpy, w, TopGC, x1, y1, x2, y2);
    XDrawLine(dpy, w, BottomGC, x1 - extra_off, y1 + 1, x2 + extra_off, y2 + 1);
}

/*
 *  Draws a little Triangle pattern within a window
 */
static void
draw_arrow(Window w, GC GC1, GC GC2, GC GC3, int l, int u, int r, int b)
{
    int m;

    m = (u + b) / 2;

    XDrawLine(dpy, w, GC1, l, u, l, b);

    XDrawLine(dpy, w, GC2, l, b, r, m);
    XDrawLine(dpy, w, GC3, r, m, l, u);
}

/*
 * add relief lines to a rectangular window
 */
static void
relieve_rectangle(Window win, int x, int y, int w, int h, GC Hilite, GC Shadow)
{
    XDrawLine(dpy, win, Hilite, x, y, w + x - 1, y);
    XDrawLine(dpy, win, Hilite, x, y, x, h + y - 1);

    XDrawLine(dpy, win, Shadow, x, h + y - 1, w + x - 1, h + y - 1);
    XDrawLine(dpy, win, Shadow, w + x - 1, y, w + x - 1, h + y - 1);
}

/*
 * add relief lines to the sides only of a rectangular window
 */
static void
relieve_half_rectangle(Window win, int x, int y, int w, int h,
		       GC Hilite, GC Shadow)
{
    XDrawLine(dpy, win, Hilite, x, y - 1, x, h + y);
    XDrawLine(dpy, win, Hilite, x + 1, y, x + 1, h + y - 1);

    XDrawLine(dpy, win, Shadow, w + x - 1, y - 1, w + x - 1, h + y);
    XDrawLine(dpy, win, Shadow, w + x - 2, y, w + x - 2, h + y - 1);
}

/*
 * Checks the function described in menuItem mi, and sees if it
 * is an allowed function for window Tmp_Win,
 * according to the motif way of life.
 * 
 * This routine is used to determine whether or not to grey out menu items.
 * FIXME: This needs to be beefed up to handle other functions (like
 * f.send_msg) -- MLM
 */
static Boolean
function_allowed(MwmWindow *tmp, MenuItem *mi)
{
    /* Complex functions are a little tricky... ignore them for now */

    /* Move is a funny hint. Keeps it out of the menu, but you're still
     * allowed * to move. */
    if ((mi->func == F_MOVE) && tmp && !(tmp->functions & MWM_FUNC_MOVE))
	return False;

    if (mi->func == F_RESIZE && tmp && !(tmp->functions & MWM_FUNC_RESIZE))
	return False;

    /* Cannot iconify if the window is already iconified or the window
       does not allow minimize */
    if ((mi->func == F_ICONIFY && tmp) &&
          ((tmp->flags & ICONIFIED) ||
           !(tmp->functions & MWM_FUNC_MINIMIZE)))
	return False;
    
    /* Cannot resize if iconified */
    if (mi->func == F_RESIZE && tmp && (tmp->flags & ICONIFIED))
        return False;

    /* Can only normalize if iconified or maximized */
    if (mi->func == F_NORMALIZE && tmp &&
        !(tmp->flags & ICONIFIED) && !(tmp->flags & MAXIMIZED))
	return False;

    if (mi->func == F_MAXIMIZE && tmp && !(tmp->functions & MWM_FUNC_MAXIMIZE))
	return False;

    if (mi->func == F_CLOSE && tmp && !(tmp->functions & MWM_FUNC_CLOSE))
	return False;

    return True;
}


/*
 * draws a single entry in a poped up menu
 */
static void
paint_entry(ScreenInfo *scr, MwmWindow *tmp, MenuRoot * mr, MenuItem *mi)
{
    int y_offset, text_y, d, y_height;
    GC ShadowGC, ReliefGC, currentGC;

    y_offset = mi->y_offset;
    y_height = mi->y_height;
    text_y = y_offset + scr->components[MWM_MENU].f_y;

    ShadowGC = scr->components[MWM_MENU].bot_GC;
    ReliefGC = scr->components[MWM_MENU].top_GC;

    if ((!mi->prev) || (!mi->prev->state))
	XClearArea(dpy, mr->w, 0, y_offset - 1, mr->width, y_height + 2, 0);
    else
	XClearArea(dpy, mr->w, 0, y_offset + 1, mr->width, y_height - 1, 0);
    if ((mi->state) && (mi->func != F_TITLE) &&
	(mi->func != F_NOP) && *mi->item)
    {
	relieve_rectangle(mr->w, 3, y_offset, mr->width - 5, mi->y_height,
			  ReliefGC, ShadowGC);
	relieve_rectangle(mr->w, 2, y_offset - 1,
			  mr->width - 3, mi->y_height + 2,
			  ReliefGC, ShadowGC);
    }
    relieve_half_rectangle(mr->w, 0, y_offset - 1, mr->width,
			   y_height + 2, ReliefGC, ShadowGC);

    if (mi->func == F_TITLE)
    {
	text_y += HEIGHT_EXTRA >> 1;
	XDrawLine(dpy, mr->w, ShadowGC, 2, y_offset + y_height - 2,
		  mr->width - 3, y_offset + y_height - 2);
	XDrawLine(dpy, mr->w, ShadowGC, 2, y_offset + y_height - 4,
		  mr->width - 3, y_offset + y_height - 4);
    }
    else
	text_y += HEIGHT_EXTRA >> 1;

    if (mi->func == F_NOP && *mi->item == 0)
    {
	draw_separator(mr->w, ShadowGC, ReliefGC,
		       2, y_offset - 1 + HEIGHT_SEPARATOR / 2,
		       mr->width - 3, y_offset - 1 + HEIGHT_SEPARATOR / 2, 0);
    }
    if (mi->next == NULL)
	draw_separator(mr->w, ShadowGC, ShadowGC, 1, mr->height - 2,
		       mr->width - 2, mr->height - 2, 1);

    if (mi == mr->first)
	draw_separator(mr->w, ReliefGC, ReliefGC, 0, 0, mr->width - 1, 0, -1);

    if (function_allowed(tmp, mi))
	currentGC = scr->components[MWM_MENU].normal_GC;
    else {
    /* MLM: FIXME: SHOULD GRAY OUT ITEM */
	currentGC = scr->components[MWM_MENU].grayed_GC;
    }

    if (*mi->item)
	XDrawString(dpy, mr->w, currentGC, mi->x, text_y,
		    mi->item, mi->strlen);

    if (mi->strlen2 > 0 && mi->item2 && *mi->item2)
	XDrawString(dpy, mr->w, currentGC, mi->x2, text_y,
		    mi->item2, mi->strlen2);

    /* pete@tecc.co.uk: If the item has a hot key, underline it */
    if (mi->hotkey > 0)
	draw_underline(scr, mr->w, currentGC, mi->x, text_y,
		       mi->item, mi->hotkey - 1);

    if (mi->hotkey < 0)
	draw_underline(scr, mr->w, currentGC, mi->x2, text_y,
		       mi->item2, -1 - mi->hotkey);

    d = (scr->components[MWM_MENU].f_height + HEIGHT_EXTRA - 7) / 2;
    if (mi->func == F_POPUP)
    {
	if (mi->state)
	    draw_arrow(mr->w, ShadowGC, ReliefGC, ShadowGC, mr->width - d - 8,
		       y_offset + d - 1, mr->width - d - 1, y_offset + d + 7);
	else
	    draw_arrow(mr->w, ReliefGC, ShadowGC, ReliefGC, mr->width - d - 8,
		       y_offset + d - 1, mr->width - d - 1, y_offset + d + 7);
    }
}

/*
 * draws the entire menu
 */
static void
paint_menu(ScreenInfo *scr, MwmWindow *tmp, MenuRoot * mr, XEvent *e)
{
    MenuItem *mi;

    for (mi = mr->first; mi != NULL; mi = mi->next)
    {
	/* be smart about handling the expose, redraw only the entries
	 * that we need to
	 */
	if (e->xexpose.y < (mi->y_offset + mi->y_height) &&
	    (e->xexpose.y + e->xexpose.height) > mi->y_offset)
	{
	    paint_entry(scr, tmp, mr, mi);
	}
    }
    XSync(dpy, 0);
}

/*
 * Updates menu display to reflect the highlighted item
 */
static int
find_entry(ScreenInfo *scr, MwmWindow *tmp)
{
    MenuItem *mi;
    MenuRoot *actual_mr;
    int retval = MENU_NOP;
    MenuRoot *PrevPrevMenu;
    MenuItem *PrevPrevItem;
    int PrevPrevY;
    int x, y, ChildY;
    Window Child;

    XQueryPointer(dpy, scr->root_win, &JunkRoot, &Child,
		  &JunkX, &ChildY, &x, &y, &JunkMask);
    XQueryPointer(dpy, ActiveMenu->w, &JunkRoot, &JunkChild,
		  &JunkX, &ChildY, &x, &y, &JunkMask);

    /* look for the entry that the mouse is in */
    for (mi = ActiveMenu->first; mi; mi = mi->next)
	if (y >= mi->y_offset && y < mi->y_offset + mi->y_height)
	    break;
    if (x < 0 || x > ActiveMenu->width)
	mi = NULL;


    /* if we weren't on the active entry, let's turn the old active one off */
    if ((ActiveItem) && (mi != ActiveItem))
    {
	ActiveItem->state = 0;
	paint_entry(scr, tmp, ActiveMenu, ActiveItem);
    }

    /* if we weren't on the active item, change the active item and turn it
     * on */
    if ((mi != ActiveItem) && (mi != NULL) && function_allowed(tmp, mi))
    {
	mi->state = 1;
	paint_entry(scr, tmp, ActiveMenu, mi);
    }

    ActiveItem = mi;

    if (ActiveItem)
    {
	/* create a new sub-menu */
	if (ActiveItem->func == F_POPUP)
	{

	    PrevPrevMenu = PrevMenu;
	    PrevPrevItem = PrevItem;
	    PrevPrevY = PrevY;
	    PrevY = MenuY;
	    PrevMenu = ActiveMenu;
	    PrevItem = ActiveItem;
	    retval = MENU_PopupMenu(scr, ActiveItem->menu);

	    /* Unfortunately, this is needed (why?) for multi-screen
	     * operation */
	    MISC_FlushExpose(ActiveMenu->w);
	    for (mi = ActiveMenu->first; mi != NULL; mi = mi->next)
		paint_entry(scr, tmp, ActiveMenu, mi);

	    XSync(dpy, 0);
	    MenuY = PrevY;
	    PrevMenu = PrevPrevMenu;
	    PrevItem = PrevPrevItem;
	    PrevY = PrevPrevY;
	}
    }
    /* end a sub-menu */
    if (XFindContext(dpy, Child,
		     MenuContext, (XPointer *)&actual_mr) == XCNOENT)
    {
	return retval;
    }

    if (actual_mr != ActiveMenu)
    {
	if (actual_mr == PrevMenu)
	{
	    if ((PrevItem->y_offset + PrevY > ChildY) ||
		((PrevItem->y_offset + PrevItem->y_height + PrevY) < ChildY))
	    {
		return SUBMENU_DONE;
	    }
	}
	else
	    return SUBMENU_DONE;
    }
    return retval;
}

/*
 * Function called from update_menu instead of Keyboard_Shortcuts()
 * when a KeyPress event is received.  If the key is alphanumeric,
 * then the menu is scanned for a matching hot key.  Otherwise if
 * it was the escape key then the menu processing is aborted.
 * If none of these conditions are true, then the default processing
 * routine is called.
 */
static void
menu_shortcuts(ScreenInfo *scr, XEvent *ev)
{
    MenuItem *mi;
    KeySym keysym = XLookupKeysym(&ev->xkey, 0);

    /* Try to match hot keys */
    if (((keysym >= XK_a) && (keysym <= XK_z)) ||	/* consider alphabetic */
	((keysym >= XK_0) && (keysym <= XK_9)))
    {				/* ...or numeric keys  */
	/* Search menu for matching hotkey */
	for (mi = ActiveMenu->first; mi; mi = mi->next)
	{
	    char key;

	    if (mi->hotkey == 0)
		continue;	/* Item has no hotkey   */
	    key = (mi->hotkey > 0) ?	/* Extract hot character */
		mi->item[mi->hotkey - 1] : mi->item2[-1 - mi->hotkey];

	    /* Convert to lower case to match the keysym */
	    if (isupper(key))
		key = tolower(key);

	    if (keysym == key)
	    {
		/* Force a menu exit */
		ActiveItem = mi;
		ev->type = ButtonRelease;
		return;
	    }
	}
    }

    switch (keysym)
    {
    case XK_Escape:
	/* escape exits menu */
	ActiveItem = NULL;
	ev->type = ButtonRelease;
	break;

	/* Nothing special --- Allow other shortcuts (cursor movement)    */
    default:
	MISC_KeyboardShortcut(scr, ev, ButtonRelease);
	break;
    }
}

/*
 * pop up a pull down menu
 */
static Boolean
pop_up_menu(ScreenInfo *scr, MenuRoot * menu, int x, int y)
{

    if ((!menu) || (menu->w == None) || (menu->items == 0) || (menu->in_use))
	return False;

    menu_on++;
    COLOR_PushRootColorMap(scr);

    Stashed_X = x;
    Stashed_Y = y;

    /* pop up the menu */
    ActiveMenu = menu;
    ActiveItem = NULL;

    /* clip to screen */
    if (x + menu->width > scr->d_width - 2)
	x = scr->d_width - menu->width - 2;
    if (x < 0)
	x = 0;

    if (y + menu->height > scr->d_height - 2)
	y = scr->d_height - menu->height - 2;
    if (y < 0)
	y = 0;

    MenuY = y;
    XMoveWindow(dpy, menu->w, x, y);
    XMapRaised(dpy, menu->w);
    menu->in_use = True;
    return True;
}

/*
 * unhighlight the current menu selection and take down the menus
 */
static void
pop_down_menu(ScreenInfo *scr)
{
    if (ActiveMenu == NULL)
	return;

    menu_on--;
    if (ActiveItem)
	ActiveItem->state = 0;

    XUnmapWindow(dpy, ActiveMenu->w);

    COLOR_PopRootColorMap(scr);
    XFlush(dpy);
    if (scr->event_context & (C_WINDOW | C_FRAME | C_TITLE | C_FRAME))
	menuFromFrameOrWindowOrTitlebar = True;
    else
	menuFromFrameOrWindowOrTitlebar = False;
    ActiveMenu->in_use = False;
}

/*
 * Updates menu display to reflect the highlighted item
 * 
 *  Returns:
 *      0 on error condition
 *      1 on return from submenu to parent menu
 *      2 on button release return
 */
static int
update_menu(ScreenInfo *scr, MwmWindow *tmp)
{
    int done, func;
    int retval;
    MenuRoot *actual_mr;
    XEvent oevent;
    int x, y;

    find_entry(scr, tmp);
    while (True)
    {
	/* block until there is an event */
	XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask | ExposureMask |
	      KeyPressMask | VisibilityChangeMask | ButtonMotionMask, &oevent);
	MISC_StashEventTime(&oevent);
	done = 0;
	if (oevent.type == MotionNotify)
	{
	    /* discard any extra motion events before a release */
	    while ((XCheckMaskEvent(dpy, ButtonMotionMask | ButtonReleaseMask,
				  &oevent)) && (oevent.type != ButtonRelease));
	}

	/* Handle a limited number of key press events to allow mouseless
	 * operation */
	if (oevent.type == KeyPress)
	    menu_shortcuts(scr, &oevent);

	switch (oevent.type)
	{
	case ButtonRelease:
	    /* The following lines holds the menu when the button is released */
	    if (!ActiveItem && (menu_on > 1))
	    {

		XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
			      &JunkX, &JunkY, &x, &y, &JunkMask);
		if ((XFindContext(dpy, JunkChild, MenuContext,
				  (XPointer *)&actual_mr) != XCNOENT) &&
		    (actual_mr != ActiveMenu))
		{
		    done = 1;
		    break;
		}
	    }
	    pop_down_menu(scr);
	    if (ActiveItem)
	    {
		func = ActiveItem->func;
		done = 1;
		if (scr->mwm_event)
		{
		    FUNC_Execute(scr, func, ActiveItem->action,
				 scr->mwm_event->frame, scr->mwm_event, &oevent,
				 scr->event_context,
				 ActiveItem->val1, ActiveItem->val2,
				 ActiveItem->val1_unit, ActiveItem->val2_unit,
				 ActiveItem->menu);
		}
		else
		{
		    FUNC_Execute(scr, func, ActiveItem->action,
				 None, None, &oevent, scr->event_context,
				 ActiveItem->val1, ActiveItem->val2,
				 ActiveItem->val1_unit, ActiveItem->val2_unit,
				 ActiveItem->menu);
		}
	    }
	    ActiveItem = NULL;
	    ActiveMenu = NULL;
	    menuFromFrameOrWindowOrTitlebar = False;
	    return MENU_DONE;

	case ButtonPress:
	    XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
			  &JunkX, &JunkY, &x, &y, &JunkMask);
	    if ((XFindContext(dpy, JunkChild, MenuContext,
			      (XPointer *)&actual_mr) == XCNOENT))
	    {
		pop_down_menu(scr);
		ActiveItem = NULL;
		ActiveMenu = NULL;
		menuFromFrameOrWindowOrTitlebar = False;
		XPutBackEvent(dpy, &oevent);
		return MENU_DONE;
	    }
	    done = 1;
	    break;

	case KeyPress:
	case VisibilityNotify:
	    done = 1;
	    break;

	case MotionNotify:
	    done = 1;

	    retval = find_entry(scr, tmp);
	    if ((retval == MENU_DONE) || (retval == SUBMENU_DONE))
	    {
		pop_down_menu(scr);
		ActiveItem = NULL;
		ActiveMenu = NULL;
		menuFromFrameOrWindowOrTitlebar = False;
	    }

	    if (retval == MENU_DONE)
		return MENU_DONE;
	    else if (retval == SUBMENU_DONE)
		return MENU_NOP;

	    break;

	case Expose:
	    /* grab our expose events, let the rest go through */
	    if ((XFindContext(dpy, oevent.xany.window, MenuContext,
			      (XPointer *)&actual_mr) != XCNOENT))
	    {
		paint_menu(scr, tmp, actual_mr, &oevent);
		done = 1;
	    }
	    break;

	default:
	    break;
	}

	if (!done)
	    EVENT_Dispatch(&oevent);
	XFlush(dpy);
    }
}

/*
 * Look for hotkey markers in a MenuItem (pete@tecc.co.uk)
 */
void
MENU_FindHotKey(MenuItem *it, KeySym key)
{
    char *str, *ptr;

    if (key != AnyKey)
    {
	str = XKeysymToString(key);
	for (ptr = it->item; ptr && *ptr && *ptr != *str; ptr++)
	    ;
	if (ptr)
	{
	    if (!*ptr)
		it->hotkey = 0;
	    else
		it->hotkey = ptr - it->item + 1;
	}
	else
	    it->hotkey = 0;
    }
    else
	it->hotkey = 0;
}

/*
 * create a new menu root
 */
MenuRoot *
MENU_Create(const char *name)
{
    MenuRoot *tmp;

    tmp  = (MenuRoot *) XtMalloc(sizeof(MenuRoot));
    tmp->name   = XtNewString(name);
    tmp->first  = NULL;
    tmp->last   = NULL;
    tmp->items  = 0;
    tmp->width  = 0;
    tmp->width2 = 0;
    tmp->w      = None;

    return (tmp);
}

/*
 * add a menu to the menu list
 */
void
MENU_Add(ScreenInfo *scr, MenuRoot * menu)
{
    if (scr->max_popups == 0)
    {
	scr->max_popups = 8;
	scr->num_popups = 0;
	scr->popups = (MenuRoot **) XtMalloc(scr->max_popups *
					     sizeof(MenuRoot *));
    }
    else if (scr->num_popups == scr->max_popups)
    {
	scr->max_popups *= 2;
	scr->popups = (MenuRoot **) XtRealloc((char *)scr->popups,
					      scr->max_popups *
					      sizeof(MenuRoot *));
    }
    scr->popups[scr->num_popups] = menu;
    scr->num_popups++;
}

/*
 * remove a menu from the screen's list of popups
 */
void
MENU_Remove(ScreenInfo *scr, MenuRoot * menu)
{
    int i;

    for (i = 0; i < scr->num_popups; i++)
    {
	if (menu == scr->popups[i])
	{
	    if (scr->num_popups != (i + 1))
		memmove(&scr->popups[i], &scr->popups[i + 1], 
		      (scr->num_popups - i - 1) * sizeof(MenuRoot *));
	    scr->num_popups--;
	}
    }
}

/*
 * add an item to a root menu
 */
void
MENU_AddItem(ScreenInfo *scr, MenuRoot * menu, char *item, char *item2,
	     char *action, int func, long func_val_1, long func_val_2,
	     char unit_1, char unit_2)
{
    MenuItem *tmp;
    int width;


    if (item == NULL)
	return;
    tmp = (MenuItem *)XtMalloc(sizeof(MenuItem));
    if (menu->first == NULL)
    {
	menu->first = tmp;
	tmp->prev = NULL;
    }
    else
    {
	menu->last->next = tmp;
	tmp->prev = menu->last;
    }
    menu->last = tmp;

    tmp->item = item;
    if (item != (char *)0)
    {
	MENU_FindHotKey(tmp, AnyKey);
	tmp->strlen = strlen(item);
    }
    else
	tmp->strlen = 0;
    tmp->item2 = item2;
    if (item2)
	tmp->strlen2 = strlen(item2);
    else
	tmp->strlen2 = 0;

    tmp->menu = 0;

    if (func == F_POPUP)
    {
	int i;

	if (action != (char *)0)
	{
	    for (i = 0; i < scr->num_popups; i++)
#if 0
  /* amai: it seems we don't ever use this catch!?! */
		if (strcasecmp(scr->popups[i]->name, action) == 0)
#else
		if (strcmp(scr->popups[i]->name, action) == 0)
#endif
		{
		    tmp->menu = scr->popups[i];
		    break;
		}
	}
	if (tmp->menu == (MenuRoot *) 0)
	{
	    fprintf(stderr, "No popup menu %s.", action);
	    func = F_NOP;
	}
    }
    tmp->action = action;
    tmp->next = NULL;
    tmp->state = 0;
    tmp->func = func;
    tmp->val1 = func_val_1;
    tmp->val2 = func_val_2;
    if ((unit_1 == 'p') || (unit_1 == 'P'))
	tmp->val1_unit = 100;
    else
	tmp->val1_unit = scr->d_width;
    if ((unit_2 == 'p') || (unit_2 == 'P'))
	tmp->val2_unit = 100;
    else
	tmp->val2_unit = scr->d_height;

    width = XTextWidth(scr->components[MWM_MENU].font, item, tmp->strlen);
    if (tmp->func == F_POPUP)
	width += 15;
    if (width <= 0)
	width = 1;
    if (width > menu->width)
	menu->width = width;
    width = XTextWidth(scr->components[MWM_MENU].font, item2, tmp->strlen2);
    if (width < 0)
	width = 0;
    if (width > menu->width2)
	menu->width2 = width;
    if ((width == 0) && (tmp->strlen2 > 0))
	menu->width2 = 1;

    menu->items++;
}

/*
 * Generates the window for a menu
 */
void
MENU_Realize(ScreenInfo *scr, MenuRoot * mr)
{
    MenuItem *cur;
    unsigned long valuemask;
    XSetWindowAttributes attributes;
    int y;

    /* lets first size the window accordingly */
    mr->width += 10;
    if (mr->width2 > 0)
	mr->width += 5;

    /* allow two pixels for top border */
    for (y = 2, cur = mr->first; cur != NULL; cur = cur->next)
    {
	cur->y_offset = y;
	cur->x = 5;
	if (cur->func == F_TITLE)
	{
	    /* Title */
	    if (cur->strlen2 == 0)
		cur->x = (mr->width - XTextWidth(scr->components[MWM_MENU].font,
						 cur->item,
						 cur->strlen)) >> 1;

	    cur->y_height = scr->components[MWM_MENU].f_height +
		HEIGHT_EXTRA + HEIGHT_EXTRA_TITLE;
	}
	else if (cur->func == F_NOP && *cur->item == 0)
	    /* Separator */
	    cur->y_height = HEIGHT_SEPARATOR;
	else
	    /* Normal text entry */
	    cur->y_height = scr->components[MWM_MENU].f_height + HEIGHT_EXTRA;
	y += cur->y_height;
	if (mr->width2 == 0)
	{
	    cur->x2 = cur->x;
	}
	else
	{
	    cur->x2 = mr->width - 5;
	}
    }
    mr->in_use = 0;
    mr->height = y + 2;

    valuemask = (CWBackPixel | CWEventMask | CWCursor | CWSaveUnder);
    attributes.background_pixel = scr->components[MWM_MENU].background;
    attributes.event_mask = (ExposureMask | EnterWindowMask);
    attributes.cursor = scr->cursors[MENU_CURS];
    attributes.save_under = True;
    mr->width = mr->width + mr->width2;
    mr->w = XCreateWindow(dpy, scr->root_win, 0, 0, (unsigned int)(mr->width),
			  (unsigned int)mr->height, (unsigned int)0,
			  CopyFromParent, (unsigned int)InputOutput,
			  (Visual *)CopyFromParent,
			  valuemask, &attributes);
    XSaveContext(dpy, mr->w, MenuContext, (XPointer)mr);
}

/*
 * realize all the menus
 */
void
MENU_RealizeMenus(ScreenInfo *scr)
{
    int i;

    for (i = 0; i < scr->num_popups; i++)
	MENU_Realize(scr, scr->popups[i]);
}

/*
 * menu ordering isn't required by mwm (meaning you can use f.menu <menu-name>
 * before <menu-name> has been declared).  Therefore, we have to defer linking
 * menus together until after the parse phase.
 */
void
MENU_LinkUp(ScreenInfo *scr)
{
    int i, k;
    MenuRoot *mr = NULL;
    MenuItem *mi = NULL;
    FuncKey *fk = NULL;
    MouseButton *mb = NULL;

    for (i = 0; i < scr->num_popups; i++)
    {

	mr = scr->popups[i];

	mi = mr->first;
	while (mi != NULL)
	{

	    if (mi->func == F_POPUP)
	    {

		for (k = 0; k < scr->num_popups; k++)
		{
		    if (strcmp(scr->popups[k]->name, mi->action) == 0)
		    {
			mi->menu = scr->popups[k];
			break;
		    }
		}
		if (mi->menu == NULL)
		{
		    fprintf(stderr, "Can't find requested menu: %s", mi->action);
		    mi->func = F_NOP;
		}
	    }

	    if (mi == mr->last)
		break;
	    else
		mi = mi->next;
	}
    }

    for (mb = scr->buttons; mb != NULL; mb = mb->next)
    {
	if (mb->func == F_POPUP || mb->func == F_W_POPUP)
	{
	    for (k = 0; k < scr->num_popups; k++)
	    {
		if (strcmp(scr->popups[k]->name, (char *)mb->menu) == 0)
		{
		    mb->menu = scr->popups[k];
		    break;
		}
	    }
	    if (mb->menu == NULL)
	    {
		fprintf(stderr, "Can't find requested menu: %s", mi->action);
		mb->func = F_NOP;
	    }
	}
    }

    for (fk = scr->keys; fk != NULL; fk = fk->next)
    {
	if (fk->func == F_POPUP || fk->func == F_W_POPUP)
	{
	    for (k = 0; k < scr->num_popups; k++)
	    {
		if (strcmp(scr->popups[k]->name, fk->action) == 0)
		{
		    fk->menu = scr->popups[k];
		    break;
		}
	    }
	    if (fk->menu == NULL)
	    {
		fprintf(stderr, "Can't find requested menu: %s", mi->action);
		fk->func = F_NOP;
	    }
	}
    }
}

/*
 * do a window menu popup
 */
int
MENU_WinMenu(ScreenInfo *scr, MenuRoot *menu,
	     MwmWindow *win, Boolean button, Boolean icon)
{
    int prevStashedX = 0, prevStashedY = 0;
    MenuRoot *PrevActiveMenu = 0;
    MenuItem *PrevActiveItem = 0;
    int retval = MENU_NOP;
    int x, y;

    /* this condition could get ugly */
    if (menu->in_use)
	return MENU_ERROR;

    if (!win)
	return MENU_ERROR;

    /*
     * In case we wind up with a move from a menu which is
     * from a window border, we'll return to here to start
     * the move.  At least, if we came from a button press
     */
    x = y = 0;
    if (button && !icon)
    {
	XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
		      &x, &y, &JunkX, &JunkY, &JunkMask);
    }
    else if ((win->flags & ICONIFIED) && win->icon_w != None)
    {
	/* the icon window had better be a child of the root window */
	if (win->icon_pixmap_w != None)
	{
	    XGetGeometry(dpy, win->icon_pixmap_w, &JunkRoot, &x, &y,
			 &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);
	}
	else
	{
	    XGetGeometry(dpy, win->icon_w, &JunkRoot, &x, &y,
			 &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);
	}

	if (y - menu->height > 0)
	{
	    y -= menu->height;
	}
	else
	{
	    y += JunkHeight;
	}
    }
    else
    {
	XGetGeometry(dpy, win->w, &JunkRoot, &JunkX, &JunkY,
		     &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);

	XTranslateCoordinates(dpy, win->w, scr->root_win, JunkX, JunkY,
			      &x, &y, &JunkChild);

	x -= win->matte_width;
	y -= win->matte_width;
    }

    if (!MISC_Grab(scr, MENU_CURS))
    {
	XBell(dpy, scr->screen);
	return MENU_DONE;
    }

    if (pop_up_menu(scr, menu, x, y))
    {
	retval = update_menu(scr, win);
    }
    else
    {
	XBell(dpy, scr->screen);
    }

    ActiveMenu = PrevActiveMenu;
    ActiveItem = PrevActiveItem;
    if ((ActiveItem) && (menu_on))
    {
	ActiveItem->state = 1;
    }

    Stashed_X = prevStashedX;
    Stashed_Y = prevStashedY;


    if (!menu_on)
    {
	MISC_Ungrab(scr);
    }

    return retval;
}

/*
 * Initiates a menu pop-up
 */
int
MENU_PopupMenu(ScreenInfo *scr, MenuRoot * menu)
{
    int prevStashedX = 0, prevStashedY = 0;
    MenuRoot *PrevActiveMenu = 0;
    MenuItem *PrevActiveItem = 0;
    int retval = MENU_NOP;
    int x, y, d;

    /* this condition could get ugly */
    if (menu->in_use)
	return MENU_ERROR;

    /* In case we wind up with a move from a menu which is
     * from a window border, we'll return to here to start
     * the move */
    XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
		  &x, &y, &JunkX, &JunkY, &JunkMask);

    x -= (menu->width >> 1);
    y -= ((scr->components[MWM_MENU].f_height + HEIGHT_EXTRA) >> 1);

    if (menu_on)
    {
	prevStashedX = Stashed_X;
	prevStashedY = Stashed_Y;
	PrevActiveMenu = ActiveMenu;
	PrevActiveItem = ActiveItem;
	/* this d is from paint_entry() */
	d = (scr->components[MWM_MENU].f_height + HEIGHT_EXTRA - 7) / 2;
	if (ActiveMenu)
	    x = Stashed_X + (ActiveMenu->width ) -d ;
	
	if (ActiveItem)
	    y = ActiveItem->y_offset + MenuY +
		((scr->components[MWM_MENU].f_height + HEIGHT_EXTRA) >> 2);
    }
    else
    {
	if (!MISC_Grab(scr, MENU_CURS))
	{
	    XBell(dpy, scr->screen);
	    return MENU_DONE;
	}
	x += (menu->width >> 1) - 3;
    }
    if (pop_up_menu(scr, menu, x, y))
    {
	retval = update_menu(scr, NULL);
    }
    else
	XBell(dpy, scr->screen);

    ActiveMenu = PrevActiveMenu;
    ActiveItem = PrevActiveItem;
    if ((ActiveItem) && (menu_on))
	ActiveItem->state = 1;
    Stashed_X = prevStashedX;
    Stashed_Y = prevStashedY;


    if (!menu_on)
	MISC_Ungrab(scr);

    return retval;
}

/*
 * destroy one menu
 */
void
MENU_Destroy(MenuRoot * menu)
{
    MenuItem *mi, *tmp;
    if (!menu)
	return;

    XDestroyWindow(dpy, menu->w);

    for (mi = menu->first; mi != NULL; mi = tmp)
    {
	tmp = mi->next;
	XtFree((char *)mi);
	mi = tmp;
    }

    XtFree((char *)menu);
}

/*
 * destroy all menus
 */
void
MENU_DestroyMenus(ScreenInfo *scr)
{
    int i;

    for (i = 0; i < scr->num_popups; i++)
	if (scr->popups[i]->w != None)
	    XDestroyWindow(dpy, scr->popups[i]->w);
}

/*
 * reset the menuing system
 */
void
MENU_Reset(void)
{
    ActiveItem = NULL;
    ActiveMenu = NULL;
}

#define INTERNAL_MENU "INTERNAL_MWM_WIN_MENU_"
/*
 * if the user has a custom window menu, build it
 */
void
MENU_BuildWindowMenu(ScreenInfo *scr, MwmWindow *win)
{
    MenuRoot *mr, *def;
    char *buf;
    int i;
    MenuItem *mi, *tmp;

    mr = NULL;

    if (win->mwm_menu && strlen(win->mwm_menu) != 0)
    {

	buf = XtMalloc(strlen(win->mwm_menu) + 64);
	sprintf(buf, "Menu %s {\n", INTERNAL_MENU);
	strcat(buf, win->mwm_menu);
	strcat(buf, "\n}");

	PARSE_buf(scr, buf);

	for (i = 0; i < scr->num_popups; i++)
	{
	    if (strcmp(scr->popups[i]->name, INTERNAL_MENU) == 0)
		break;
	}
	if (i == scr->num_popups)
	{
	    fprintf(stderr, "Couldn't find user defined menu!\n");
	    return;
	}

	mr = scr->popups[i];
	MENU_Remove(scr, mr);
	win->custom_menu = mr;
    }

    for (i = 0; i < scr->num_popups; i++)
    {
	if (strcmp(scr->popups[i]->name, win->window_menu) == 0)
	    break;
    }
    if (i == scr->num_popups)
    {
	fprintf(stderr, "Couldn't find default window menu!\n");
	return;
    }

    def = scr->popups[i];

    if (mr)
    {
	if (def->width > mr->width)
	    mr->width = def->width;
	if (def->width2 > mr->width2)
	    mr->width2 = def->width2;
    }
    else
    {
	mr = (MenuRoot *) XtMalloc(sizeof(MenuRoot));
	memcpy(mr, def, sizeof(MenuRoot));
	mr->first = mr->last = NULL;
    }

    for (mi = def->last; mi != NULL; mi = mi->prev)
    {
	tmp = (MenuItem *)XtMalloc(sizeof(MenuItem));
	memcpy(tmp, mi, sizeof(MenuItem));
	if (mr->first == NULL)
	{
	    mr->first = tmp;
	    tmp->prev = NULL;
	    tmp->next = NULL;
	}
	else
	{
	    tmp->next = mr->first;
	    mr->first->prev = tmp;
	    mr->first = tmp;
	}
	if (mr->last == NULL)
	    mr->last = tmp;
	mr->items++;
    }

    MENU_Realize(scr, mr);
}

/*
 * if a window has a custom window menu, destroy it
 */
void
MENU_DestroyWindowMenu(ScreenInfo *scr, MwmWindow *win)
{
    if (!win->custom_menu)
	return;

    MENU_Destroy(win->custom_menu);

    win->custom_menu = NULL;
}

/*
 * convert a keysym and a mod mask into a string
 */
char
 *
MENU_AcceleratorString(ScreenInfo *scr, KeySym keysym, int modifiers)
{
    Boolean haveone = False;
    static char buf[128];
    char *key;

    buf[0] = 0;

    if (modifiers & ControlMask)
    {
	strcat(buf, "<Ctrl");
	haveone = True;
    }
    if (modifiers & ShiftMask)
    {
	if (haveone)
	    strcat(buf, "+");
	else
	    strcat(buf, "<");
	strcat(buf, "Shift");
	haveone = True;
    }
    if (modifiers & scr->alt_mask)
    {
	if (haveone)
	    strcat(buf, "+");
	else
	    strcat(buf, "<");
	strcat(buf, "Alt");
	haveone = True;
    }
    if (modifiers & LockMask)
    {
	if (haveone)
	    strcat(buf, "+");
	else
	    strcat(buf, "<");
	strcat(buf, "Lock");
	haveone = True;
    }
    if (modifiers & Mod1Mask && scr->alt_mask != Mod1Mask)
    {
	if (haveone)
	    strcat(buf, "+");
	else
	    strcat(buf, "<");
	strcat(buf, "Mod1");
	haveone = True;
    }
    if (modifiers & Mod2Mask && scr->alt_mask != Mod2Mask)
    {
	if (haveone)
	    strcat(buf, "+");
	else
	    strcat(buf, "<");
	strcat(buf, "Mod2");
	haveone = True;
    }
    if (modifiers & Mod3Mask && scr->alt_mask != Mod3Mask)
    {
	if (haveone)
	    strcat(buf, "+");
	else
	    strcat(buf, "<");
	strcat(buf, "Mod3");
	haveone = True;
    }
    if (modifiers & Mod4Mask && scr->alt_mask != Mod4Mask)
    {
	if (haveone)
	    strcat(buf, "+");
	else
	    strcat(buf, "<");
	strcat(buf, "Mod4");
	haveone = True;
    }
    if (modifiers & Mod5Mask && scr->alt_mask != Mod5Mask)
    {
	if (haveone)
	    strcat(buf, "+");
	else
	    strcat(buf, "<");
	strcat(buf, "Mod5");
	haveone = True;
    }
    if ((key = XKeysymToString(keysym)) != NULL)
    {
	if (haveone)
	    strcat(buf, ">");
	strcat(buf, key);
    }
    return buf;
}
