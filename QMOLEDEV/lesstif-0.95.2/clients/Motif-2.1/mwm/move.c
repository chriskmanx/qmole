/* $Id: move.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $ */
/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
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


Bool NeedToResizeToo=False;

/*
 * display the position in the dimensions window
 */
static void
display_position(ScreenInfo *scr, MwmWindow *tmp_win, int x, int y, int Init)
{
    char str[100];
    int offset;
    int ssw;

    ssw = XTextWidth(scr->components[MWM_FEEDBACK].font,
		     " +8888 x +8888 ", 15);
    sprintf(str, " %+-4d %+-4d ", x, y);
    if (Init)
    {
	XClearWindow(dpy, scr->size_win);
	if (scr->d_depth >= 2)
	    DEC_DrawShadows(tmp_win, scr->size_win, 0, 0,
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

    offset = (ssw + SIZE_HINDENT * 2 -
	      XTextWidth(scr->components[MWM_FEEDBACK].font,
			 str, strlen(str))) / 2;
    XDrawString(dpy, scr->size_win, scr->components[MWM_FEEDBACK].normal_GC,
		offset,
		scr->components[MWM_FEEDBACK].f_y + SIZE_VINDENT,
		str, strlen(str));
}

/*
 * move a window, accepting input from the user
 */
void
MOVE_Interactive(ScreenInfo *scr, Window *win, MwmWindow *tmp_win,
		 int *FinalX, int *FinalY, XEvent *eventp)
{
    int origDragX, origDragY, DragX, DragY, DragWidth, DragHeight;
    int XOffset, YOffset;
    Window w;
    Bool opaque_move = False;

    COLOR_PushRootColorMap(scr);
    if (menuFromFrameOrWindowOrTitlebar)
    {
	/* warp the pointer to the cursor position from before menu appeared */
	XWarpPointer(dpy, None, scr->root_win, 0, 0, 0, 0,
		     Stashed_X, Stashed_Y);
	XFlush(dpy);
    }


    DragX = eventp->xbutton.x_root;
    DragY = eventp->xbutton.y_root;

    /* If this is left commented out, then the move starts from the button
     * press location instead of the current location, which seems to be an
     * improvement */
    /*  XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
       &DragX, &DragY,    &JunkX, &JunkY, &JunkMask); */

    if (!MISC_Grab(scr, MOVE_CURS))
    {
	XBell(dpy, scr->screen);
	return;
    }

    pagerOn = False;

    w = tmp_win->frame;

    if (tmp_win->flags & ICONIFIED)
    {
	if (tmp_win->icon_pixmap_w != None)
	{
	    XUnmapWindow(dpy, tmp_win->icon_w);
	    w = tmp_win->icon_pixmap_w;
	}
	else
	    w = tmp_win->icon_w;
    }
    
    *win = w;

    XGetGeometry(dpy, w, &JunkRoot, &origDragX, &origDragY,
		 (unsigned int *)&DragWidth, (unsigned int *)&DragHeight,
		 &JunkBW, &JunkDepth);

    if (DragWidth * DragHeight <
	(scr->OpaqueSize * scr->d_width * scr->d_height) / 100)
	opaque_move = True;
    else
	XGrabServer(dpy);

    if ((!opaque_move) && (tmp_win->flags & ICONIFIED))
	XUnmapWindow(dpy, w);

    DragWidth += JunkBW;
    DragHeight += JunkBW;
    XOffset = origDragX - DragX;
    YOffset = origDragY - DragY;

    if (Mwm.show_feedback & MWM_FEEDBACK_MOVE)
	XMapRaised(dpy, scr->size_win);

    MOVE_EventLoop(scr, tmp_win, XOffset, YOffset, DragWidth, DragHeight,
		   FinalX, FinalY, opaque_move, False);

    if (Mwm.show_feedback & MWM_FEEDBACK_MOVE)
	XUnmapWindow(dpy, scr->size_win);

    COLOR_PopRootColorMap(scr);

    if (!opaque_move)
	XUngrabServer(dpy);

    MISC_Ungrab(scr);


    pagerOn = True;
}

/*
 * Move the rubberband around, return with the new window location
 */
void
MOVE_EventLoop(ScreenInfo *scr, MwmWindow *tmp_win, int XOffset, int YOffset,
	       int Width, int Height, int *FinalX, int *FinalY,
	       Boolean opaque_move, Boolean AddWindow)
{
    Bool finished = False;
    Bool done;
    int xl, yt, delta_x, delta_y;
    unsigned int pagerwidth, pagerheight;
    int ww, wh;
    int wx, wy;
    int MaxH, MaxW;
    int last_x = -10000, last_y = -10000;
    XEvent oevent;

    XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild, &xl, &yt,
		  &JunkX, &JunkY, &JunkMask);
    xl += XOffset;
    yt += YOffset;

    if (!opaque_move && AddWindow)
	WIN_DrawOutline(scr, scr->root_win, xl, yt, Width, Height);

    if (Mwm.show_feedback & MWM_FEEDBACK_MOVE)
	display_position(scr, tmp_win, xl + scr->virt_x, yt + scr->virt_y, True);

    while (!finished)
    {
	/* block until there is an interesting event */
	XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask | KeyPressMask |
		 PointerMotionMask | ButtonMotionMask | ExposureMask, &oevent);
	MISC_StashEventTime(&oevent);

	/* discard any extra motion events before a logical release */
	if (oevent.type == MotionNotify)
	{
	    while (XCheckMaskEvent(dpy, PointerMotionMask | ButtonMotionMask |
				   ButtonPressMask | ButtonRelease, &oevent))
	    {
		MISC_StashEventTime(&oevent);
		if (oevent.type == ButtonRelease)
		    break;
	    }
	}

	done = False;
	/* Handle a limited number of key press events to allow mouseless
	 * operation */
	if (oevent.type == KeyPress)
	    MISC_KeyboardShortcut(scr, &oevent, ButtonRelease);
	switch (oevent.type)
	{
	case KeyPress:
	    done = True;
	    break;
	case ButtonPress:
	    XAllowEvents(dpy, ReplayPointer, CurrentTime);
	    if (oevent.xbutton.button == 1 && oevent.xbutton.state & ShiftMask)
	    {
		NeedToResizeToo = True;
		/* Fallthrough to button-release */
	    }
	    else
	    {
		done = 1;
		break;
	    }
	case ButtonRelease:
	    if (!opaque_move)
		WIN_DrawOutline(scr, scr->root_win, 0, 0, 0, 0);
	    xl = oevent.xmotion.x_root + XOffset;
	    yt = oevent.xmotion.y_root + YOffset;

	    /* Resist moving windows over the edge of the screen! */
	    if (((xl + Width) >= scr->d_width) &&
		((xl + Width) < scr->d_width + scr->MoveResistance))
		xl = scr->d_width - Width - tmp_win->bw;
	    if ((xl <= 0) && (xl > -scr->MoveResistance))
		xl = 0;
	    if (((yt + Height) >= scr->d_height) &&
		((yt + Height) < scr->d_height + scr->MoveResistance))
		yt = scr->d_height - Height - tmp_win->bw;
	    if ((yt <= 0) && (yt > -scr->MoveResistance))
		yt = 0;

	    *FinalX = xl;
	    *FinalY = yt;

	    done = True;
	    finished = True;
	    break;

	case MotionNotify:
	    /* update location of the pager_view window */
	    if ((scr->mwm_pager != NULL) &&
		(xl < scr->mwm_pager->frame_x + scr->mwm_pager->frame_width) &&
		(xl + Width > scr->mwm_pager->frame_x) &&
		(yt < scr->mwm_pager->frame_y + scr->mwm_pager->frame_height) &&
		(yt + Height > scr->mwm_pager->frame_y) && (!opaque_move))
		WIN_DrawOutline(scr, scr->root_win, 0, 0, 0, 0);
	    xl = oevent.xmotion.x_root;
	    yt = oevent.xmotion.y_root;
	    PAN_PanDesktop(scr, scr->edge_scroll_x, scr->edge_scroll_y,
			   &xl, &yt, &delta_x, &delta_y, False, &oevent);
	    /* redraw the rubberband */
	    xl += XOffset;
	    yt += YOffset;

	    /* Resist moving windows over the edge of the screen! */
	    if (((xl + Width) >= scr->d_width) &&
		((xl + Width) < scr->d_width + scr->MoveResistance))
		xl = scr->d_width - Width - tmp_win->bw;
	    if ((xl <= 0) && (xl > -scr->MoveResistance))
		xl = 0;
	    if (((yt + Height) >= scr->d_height) &&
		((yt + Height) < scr->d_height + scr->MoveResistance))
		yt = scr->d_height - Height - tmp_win->bw;
	    if ((yt <= 0) && (yt > -scr->MoveResistance))
		yt = 0;

	    if (scr->mwm_pager)
	    {
		pagerwidth = scr->mwm_pager->frame_width -
		    2 * scr->mwm_pager->boundary_width -
		    2 * scr->mwm_pager->matte_width;
		pagerheight = scr->mwm_pager->frame_height -
		    scr->mwm_pager->title_height -
		    2 * scr->mwm_pager->boundary_width -
		    2 * scr->mwm_pager->matte_width;

		MaxW = scr->virt_x_max + scr->d_width;
		MaxH = scr->virt_y_max + scr->d_height;

		if (!(tmp_win->flags & STICKY))
		{
		    /* show the actual window */
		    wx = (xl + scr->virt_x) * (int)pagerwidth / MaxW;
		    wy = (yt + scr->virt_y) * (int)pagerheight / MaxH;
		    if ((last_x - wx >= 2) || (last_x - wx <= -2) ||
			(last_y - wy >= 2) || (last_y - wy <= -2))
		    {
			ww = Width * (int)pagerwidth / MaxW;
			wh = Height * (int)pagerheight / MaxH;
			if (ww < 2)
			    ww = 2;
			if (wh < 2)
			    wh = 2;
			XMoveResizeWindow(dpy,
					  tmp_win->pager_view, wx, wy, ww, wh);
			last_x = wx;
			last_y = wy;
		    }
		}
	    }

	    if (!opaque_move)
		WIN_DrawOutline(scr, scr->root_win, xl, yt, Width, Height);
	    else
	    {
		if (tmp_win->flags & ICONIFIED)
		{
		    tmp_win->icon_x_loc = xl;
		    tmp_win->icon_xl_loc = xl -
			(tmp_win->icon_w_width - tmp_win->icon_p_width) / 2;
		    tmp_win->icon_y_loc = yt;
		    if (tmp_win->icon_pixmap_w != None)
			XMoveWindow(dpy, tmp_win->icon_pixmap_w,
				    tmp_win->icon_x_loc, yt);
		    else if (tmp_win->icon_w != None)
			XMoveWindow(dpy, tmp_win->icon_w, tmp_win->icon_xl_loc,
				    yt + tmp_win->icon_p_height);

		}
		else
		    XMoveWindow(dpy, tmp_win->frame, xl, yt);
	    }
	    if (Mwm.show_feedback & MWM_FEEDBACK_MOVE)
		display_position(scr, tmp_win,
				 xl + scr->virt_x, yt + scr->virt_y, False);
	    done = True;
	    break;

	default:
	    break;
	}
	if (!done)
	{
	    if (!opaque_move)
		WIN_DrawOutline(scr, scr->root_win, 0, 0, 0, 0);
	    EVENT_Dispatch(&oevent);
	    if (!opaque_move)
		WIN_DrawOutline(scr, scr->root_win, xl, yt, Width, Height);
	}
    }
}
