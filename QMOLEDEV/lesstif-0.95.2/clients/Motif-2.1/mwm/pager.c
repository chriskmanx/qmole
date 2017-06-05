/* $Id: pager.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $ */
/****************************************************************************
 * This module is all new
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


Bool pagerOn = True;
Bool EnablePagerRedraw = True;
Bool DoHandlePageing = True;

static char *pager_name = "Mwm Pager";

static XClassHint classhints =
{
    "pager",
    "Mwm"
};

static XSizeHints sizehints =
{
    (PMinSize | PResizeInc | PBaseSize | PWinGravity),
    0, 0, 100, 100,		/* x, y, width and height */
    1, 1,			/* Min width and height */
    0, 0,			/* Max width and height */
    1, 1,			/* Width and height increments */
    {0, 0},
    {0, 0},			/* Aspect ratio - not used */
    1, 1,			/* base size */
    (NorthWestGravity)		/* gravity */
};

/*
 * draw the lines delimiting the virtual screens
 */
static void
draw_partitions(ScreenInfo *scr)
{
    int y, y1, y2, x, x1, x2;
    int MaxW, MaxH, width, height;

    MaxW = scr->virt_x_max + scr->d_width;
    MaxH = scr->virt_y_max + scr->d_height;

    width = scr->mwm_pager->frame_width -
	2 * scr->mwm_pager->boundary_width -
	2 * scr->mwm_pager->matte_width;
    height = scr->mwm_pager->frame_height -
	scr->mwm_pager->title_height -
	2 * scr->mwm_pager->boundary_width -
	2 * scr->mwm_pager->matte_width;

    x = scr->d_width;
    y1 = 0;
    y2 = height;
    while (x < MaxW)
    {
	x1 = x * width / MaxW;
	XDrawLine(dpy, scr->pager_win,
		  scr->components[MWM_PAGER].normal_GC, x1, y1, x1, y2);
	x += scr->d_width;
    }

    y = scr->d_height;
    x1 = 0;
    x2 = width;
    while (y < MaxH)
    {
	y1 = y * height / MaxH;
	XDrawLine(dpy, scr->pager_win,
		  scr->components[MWM_PAGER].normal_GC, x1, y1, x2, y1);
	y += scr->d_height;
    }
}

/*
 * clear the pager area.  This will cause the pager to be redrawn.
 */
void
PAGER_Clear(ScreenInfo *scr)
{
    if ((scr->mwm_pager) && (EnablePagerRedraw))
	XClearArea(dpy, scr->pager_win, 0, 0, scr->mwm_pager->frame_width,
		   scr->mwm_pager->frame_height, True);
}

/*
 * redraw the pager: we try to be clever - re-draw the pager by causing
 * an expose event, so that the redraw occurs when the expose arrives.
 * The advantage is that the number of re-draws will be minimized.
 */
void
PAGER_Redraw(ScreenInfo *scr)
{
    MwmWindow *t;

    if (!scr->mwm_pager)
	return;

    MISC_FlushExpose(scr->pager_win);
    MISC_FlushExpose(scr->pager_child_win);

    if (scr->components[MWM_PAGER].f_height > 0)
    {
	if (scr->mwm_highlight != NULL)
	{
	    if (!(scr->mwm_highlight->flags & STICKY) &&
		(scr->mwm_highlight->icon_label != NULL))
	    {
		MISC_FlushExpose(scr->mwm_highlight->pager_view);
		XDrawImageString(dpy, scr->mwm_highlight->pager_view,
				 scr->components[MWM_PAGER].normal_GC,
				 2, scr->components[MWM_PAGER].f_y + 2,
				 scr->mwm_highlight->icon_label,
				 strlen(scr->mwm_highlight->icon_label));
	    }
	}

	for (t = scr->mwm_root.next; t != NULL; t = t->next)
	{
	    if (t != scr->mwm_highlight)
	    {
		if (!(t->flags & STICKY) &&
		    (t->icon_label != NULL))
		{
		    MISC_FlushExpose(t->pager_view);
		    XDrawImageString(dpy, t->pager_view,
				     scr->components[MWM_PAGER].normal_GC,
				     2, scr->components[MWM_PAGER].f_y + 2,
				     t->icon_label, strlen(t->icon_label));
		}
	    }
	}
    }
    draw_partitions(scr);
}

/*
 * update the child.  MLM -- I'm still not sure what this is.
 */
void
PAGER_UpdateViewPort(ScreenInfo *scr)
{
    int width, height, x1, x2, y1, y2;

    if ((scr->pager_child_win) && (scr->mwm_pager))
    {
	width = scr->mwm_pager->frame_width -
	    2 * scr->mwm_pager->boundary_width -
	    2 * scr->mwm_pager->matte_width;
	height = scr->mwm_pager->frame_height -
	    scr->mwm_pager->title_height -
	    2 * scr->mwm_pager->boundary_width -
	    2 * scr->mwm_pager->matte_width;
	x1 = scr->virt_x * width / (scr->virt_x_max + scr->d_width) + 1;
	y1 = scr->virt_y * height / (scr->virt_y_max + scr->d_height) + 1;
	x2 = (scr->d_width) * width / (scr->virt_x_max + scr->d_width) - 1;
	y2 = (scr->d_height) * height / (scr->virt_y_max + scr->d_height) - 1;
	if (x1 == 1)
	{
	    x1--;
	    x2++;
	}
	if (y1 == 1)
	{
	    y1--;
	    y2++;
	}
	XMoveResizeWindow(dpy, scr->pager_child_win, x1, y1, x2, y2);
    }
}

/*
 * update the pager view
 */
void
PAGER_UpdateView(ScreenInfo *scr, MwmWindow *t)
{
    unsigned int width, height;
    int ww, wh;
    int wx, wy;
    int MaxH, MaxW;

    if ((!scr->mwm_pager) || (!pagerOn))
	return;

    width = scr->mwm_pager->frame_width -
	2 * scr->mwm_pager->boundary_width -
	2 * scr->mwm_pager->matte_width;
    height = scr->mwm_pager->frame_height -
	scr->mwm_pager->title_height -
	2 * scr->mwm_pager->boundary_width -
	2 * scr->mwm_pager->matte_width;

    MaxW = scr->virt_x_max + scr->d_width;
    MaxH = scr->virt_y_max + scr->d_height;

    if ((!(t->flags & STICKY)) &&
	(!((t->flags & ICONIFIED) && (t->flags & ICON_UNMAPPED))) &&
	(t->Desk == scr->current_desk))
    {
	if (t->flags & ICONIFIED)
	{
	    /* show the icon loc */
	    wx = (t->icon_x_loc + scr->virt_x) * (int)width / MaxW;;
	    wy = (t->icon_y_loc + scr->virt_y) * (int)height / MaxH;
	    ww = t->icon_w_width * (int)width / MaxW;
	    wh = (t->icon_w_height + t->icon_p_height) * (int)height / MaxH;
	}
	else
	{
	    /* show the actual window */
	    wx = (t->frame_x + scr->virt_x) * (int)width / MaxW;
	    wy = (t->frame_y + scr->virt_y) * (int)height / MaxH;
	    ww = t->frame_width * (int)width / MaxW;
	    wh = t->frame_height * (int)height / MaxH;
	}
	if (ww < 2)
	    ww = 2;
	if (wh < 2)
	    wh = 2;
	XMoveResizeWindow(dpy, t->pager_view, wx, wy, ww, wh);
    }
    else
    {
	/* window is sticky - make sure that the pager_view window is not 
	 * visible */
	XMoveResizeWindow(dpy, t->pager_view, -10, -10, 5, 5);
    }

    PAGER_Clear(scr);
}

/*
 *  Moves the viewport within thwe virtual desktop
 */
void
PAGER_MoveViewPort(ScreenInfo *scr, int newx, int newy, Boolean grab)
{
    MwmWindow *t;
    int deltax, deltay;
    XEvent oevent;

    if (grab)
	XGrabServer(dpy);

    if (newx > scr->virt_x_max)
	newx = scr->virt_x_max;
    if (newy > scr->virt_y_max)
	newy = scr->virt_y_max;
    if (newx < 0)
	newx = 0;
    if (newy < 0)
	newy = 0;

    deltay = scr->virt_y - newy;
    deltax = scr->virt_x - newx;

    scr->virt_x = newx;
    scr->virt_y = newy;

    if ((deltax != 0) || (deltay != 0))
    {
	for (t = scr->mwm_root.next; t != NULL; t = t->next)
	{
	    /* If the window is iconified, and sticky Icons is set,
	     * then the window should essentially be sticky */
	    if (!(t->flags & STICKY))
	    {
		t->icon_x_loc += deltax;
		t->icon_xl_loc += deltax;
		t->icon_y_loc += deltay;

		if (t->icon_pixmap_w != None)
		    XMoveWindow(dpy, t->icon_pixmap_w, t->icon_x_loc,
				t->icon_y_loc);
		if (t->icon_w != None)
		    XMoveWindow(dpy, t->icon_w, t->icon_x_loc,
				t->icon_y_loc + t->icon_p_height);

		DEC_ConfigureDecorations(scr, t,
					 t->frame_x + deltax,
					 t->frame_y + deltay,
					 t->frame_width, t->frame_height,
					 False);
	    }
	}
	for (t = scr->mwm_root.next; t != NULL; t = t->next)
	{
	    /* If its an icon, and its sticking, autoplace it so
	     * that it doesn't wind up on top a a stationary
	     * icon */
	    if ((t->flags & STICKY) &&
		(t->flags & ICONIFIED) && (!(t->flags & ICON_MOVED)) &&
		(!(t->flags & ICON_UNMAPPED)))
		ICON_AutoPlace(scr, t);
	}

    }
    /* fix up the viewport indicator */
    PAGER_UpdateViewPort(scr);
    PAN_CheckBounds(scr);

    /* do this with PanFrames too ??? HEDU */
    while (XCheckTypedEvent(dpy, MotionNotify, &oevent))
	MISC_StashEventTime(&oevent);
    if (grab)
	XUngrabServer(dpy);
}

/*
 * switch among pages
 */
void
PAGER_SwitchPage(ScreenInfo *scr, Bool align, Bool ChangeFocus, XEvent *event)
{
    int x, y;
    unsigned int width, height;
    MwmWindow *tmp_win;
    Window dumwin;

    if (!scr->mwm_pager)
	return;

    XTranslateCoordinates(dpy, event->xbutton.window, scr->pager_win,
			  event->xbutton.x, event->xbutton.y, &x, &y, &dumwin);

    width = scr->mwm_pager->frame_width -
	2 * scr->mwm_pager->boundary_width -
	2 * scr->mwm_pager->matte_width;
    height = scr->mwm_pager->frame_height -
	scr->mwm_pager->title_height -
	2 * scr->mwm_pager->boundary_width -
	2 * scr->mwm_pager->matte_width;

    if (x < 0)
	x = 0;
    if (y < 0)
	y = 0;
    x = x * (scr->virt_x_max + scr->d_width) / width;
    y = y * (scr->virt_y_max + scr->d_height) / height;
    if (align)
    {
	x = (x / scr->d_width) * scr->d_width;
	y = (y / scr->d_height) * scr->d_height;
    }
    if (x < 0)
	x = 0;
    if (y < 0)
	y = 0;
    PAGER_MoveViewPort(scr, x, y, True);
    if ((ChangeFocus) &&
	(Mwm.keyboard_focus_policy == XmEXPLICIT &&
	 Mwm.auto_key_focus))
    {
	tmp_win = WIN_WindowToStruct(scr, event->xbutton.subwindow);
	if (tmp_win)
	{
	    WIN_Raise(scr, tmp_win);
	    WIN_SetFocusInTree(tmp_win);
	    WIN_SetFocus(scr, tmp_win->w, tmp_win);
	    MISC_SetFocusSequence(scr);
	}
    }
}

/*
 * creates the pager window, if needed
 */
void
PAGER_Initialize(ScreenInfo *scr, Position x, Position y)
{
    XTextProperty name;
    int width, height, window_x, window_y;
    unsigned long valuemask;
    XSetWindowAttributes attributes;

    width = (scr->virt_x_max + scr->d_width) / scr->virt_scale;
    height = (scr->virt_y_max + scr->d_height) / scr->virt_scale;

    if (x >= 0)
	window_x = x;
    else
    {
	sizehints.win_gravity = NorthEastGravity;
	window_x = scr->d_width - width + x - 2;
    }

    if (y >= 0)
	window_y = y;
    else
    {
	window_y = scr->d_height - height + y - 2;
	if (x < 0)
	    sizehints.win_gravity = SouthEastGravity;
	else
	    sizehints.win_gravity = SouthWestGravity;
    }
    valuemask = (CWBackPixel | CWBorderPixel | CWEventMask | CWCursor);
    attributes.background_pixel = scr->components[MWM_PAGER].background;
    attributes.border_pixel = scr->components[MWM_PAGER].background;
    attributes.cursor = scr->cursors[DEFAULT_CURS];
    attributes.event_mask = (ExposureMask | EnterWindowMask | ButtonReleaseMask |
			     ButtonMotionMask);
    sizehints.width = width;
    sizehints.height = height;
    sizehints.x = window_x;
    sizehints.y = window_y;

    scr->pager_win = XCreateWindow(dpy, scr->root_win, window_x, window_y, width,
				   height, (unsigned int)1,
				   CopyFromParent, InputOutput,
				   (Visual *)CopyFromParent,
				   valuemask, &attributes);
    XSetWMNormalHints(dpy, scr->pager_win, &sizehints);
    XStringListToTextProperty(&pager_name, 1, &name);
    XSetWMName(dpy, scr->pager_win, &name);
    XSetWMIconName(dpy, scr->pager_win, &name);
    XSetClassHint(dpy, scr->pager_win, &classhints);
    XFree((char *)name.value);

    attributes.event_mask = KeyPressMask | ExposureMask;
    attributes.background_pixel = scr->components[MWM_PAGER].background;
    attributes.cursor = scr->cursors[DEFAULT_CURS];
    scr->pager_child_win = XCreateWindow(dpy, scr->pager_win, -10, -10, 10, 10, 0,
					 CopyFromParent,
					 InputOutput, CopyFromParent,
					 CWEventMask | CWBackPixel | CWCursor,
					 &attributes);
    XStoreName(dpy, scr->pager_child_win, "PagerChildWin");
    XMapRaised(dpy, scr->pager_child_win);
}

/*
 * move a window via the pager
 */
void
PAGER_Update(ScreenInfo *scr, XEvent *event)
{
    MwmWindow *tmp_win;
    unsigned int width, height;
    int Xoff, Yoff, x, y, MaxW, MaxH, xl, yt, xl1, yt1;
    Window target;
    Bool done, finished = False;
    XEvent oevent = *event;

    /* I tried to implement a feature so that when windows which could be
     * opaque moved in a normal move would get the full size windows moved in
     * conjunction with the pager version of the window, but it seems to
     * be buggy on some machines */
#ifdef BROKEN_STUFF
    Bool OpaqueMove = False;
    int dwidth, dheight;

#endif
    if (!scr->mwm_pager)
	return;

    EnablePagerRedraw = False;
    target = oevent.xbutton.subwindow;
    tmp_win = WIN_WindowToStruct(scr, target);

    if (tmp_win == NULL)
	return;


    MaxW = scr->virt_x_max + scr->d_width;
    MaxH = scr->virt_y_max + scr->d_height;

    width = scr->mwm_pager->frame_width -
	2 * scr->mwm_pager->boundary_width -
	2 * scr->mwm_pager->matte_width;
    height = scr->mwm_pager->frame_height -
	scr->mwm_pager->title_height -
	2 * scr->mwm_pager->boundary_width -
	2 * scr->mwm_pager->matte_width;

    XQueryPointer(dpy, target, &JunkRoot, &JunkChild,
		  &JunkX, &JunkY, &Xoff, &Yoff, &JunkMask);

    XQueryPointer(dpy, scr->pager_win, &JunkRoot, &JunkChild,
		  &JunkX, &JunkY, &xl, &yt, &JunkMask);
    if (xl < 0)
	xl = 0;
    if (yt < 0)
	yt = 0;
    if (xl > width)
	xl = width;
    if (yt > height)
	yt = height;
    xl -= Xoff;
    yt -= Yoff;
    xl1 = xl;
    yt1 = yt;

    while (!finished)
    {
	/* block until there is an interesting event */
	XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask | KeyPressMask |
		   PointerMotionMask | ButtonMotionMask | ExposureMask |
		   VisibilityChangeMask, &oevent);
	MISC_StashEventTime(&oevent);

	if (oevent.type == MotionNotify)
	    /* discard any extra motion events before a logical release */
	    while (XCheckMaskEvent(dpy, PointerMotionMask | ButtonMotionMask |
				   ButtonRelease, &oevent))
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
	case KeyPress:
	    /* throw away enter and leave events until release */
	    done = True;
	    break;
	case ButtonRelease:
	    XQueryPointer(dpy, scr->pager_win, &JunkRoot, &JunkChild,
			  &JunkX, &JunkY, &xl, &yt, &JunkMask);
	    if (xl < 0)
		xl = 0;
	    if (yt < 0)
		yt = 0;
	    if (xl > width)
		xl = width;
	    if (yt > height)
		yt = height;
	    xl -= Xoff;
	    yt -= Yoff;
	    done = True;
	    finished = True;
	    break;

	case MotionNotify:
	    XQueryPointer(dpy, scr->pager_win, &JunkRoot, &JunkChild,
			  &JunkX, &JunkY, &xl, &yt, &JunkMask);
	    if (xl < 0)
		xl = 0;
	    if (yt < 0)
		yt = 0;
	    if (xl > width)
		xl = width;
	    if (yt > height)
		yt = height;

	    /* redraw the rubberband */
	    xl -= Xoff;
	    yt -= Yoff;

	    done = True;
	    break;

	default:
	    break;
	}
	if (!done)
	{
	    EVENT_Dispatch(&oevent);
	}
	XMoveWindow(dpy, target, xl, yt);
	draw_partitions(scr);

    }

    x = xl * MaxW / (int)width - scr->virt_x;
    y = yt * MaxH / (int)height - scr->virt_y;

    PAGER_UpdateView(scr, tmp_win);
    if ((xl1 != xl) || (yt1 != yt))
    {
	if ((tmp_win->flags & ICONIFIED))
	{
	    tmp_win->icon_x_loc = x;
	    tmp_win->icon_xl_loc = x -
		(tmp_win->icon_w_width - tmp_win->icon_p_width) / 2;
	    tmp_win->icon_y_loc = y;
	    XMoveWindow(dpy, tmp_win->icon_w, tmp_win->icon_xl_loc, y + tmp_win->icon_p_height);

	    if (tmp_win->icon_pixmap_w != None)
		XMoveWindow(dpy, tmp_win->icon_pixmap_w, x, y);

	    tmp_win->flags |= ICON_MOVED;

	}
	else
	{
	    /* show the actual window */
	    DEC_ConfigureDecorations(scr, tmp_win, x, y, tmp_win->frame_width, tmp_win->frame_height, False);
	}
    }
    EnablePagerRedraw = True;
    PAGER_Clear(scr);
}
