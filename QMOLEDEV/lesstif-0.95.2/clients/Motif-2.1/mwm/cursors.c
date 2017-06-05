/* $Id: cursors.c,v 1.1 2004/08/28 19:25:45 dannybackx Exp $ */
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

#include <X11/cursorfont.h>


#define modal_width 16
#define modal_height 16
static unsigned char modal_bits[] =
{
    0xc0, 0x03, 0xf0, 0x0f, 0xfc, 0x3f, 0xfc, 0x3f, 0xfe, 0x7f, 0xfe, 0x7f,
    0xff, 0xff, 0x01, 0x80, 0x01, 0x80, 0xff, 0xff, 0xfe, 0x7f, 0xfe, 0x7f,
    0xfc, 0x3f, 0xfc, 0x3f, 0xf0, 0x0f, 0xc0, 0x03};
#define modal_x_hot 8
#define modal_y_hot 8

#define modal_mask_width 16
#define modal_mask_height 16
static unsigned char modal_mask_bits[] =
{
    0xc0, 0x03, 0xf0, 0x0f, 0xfc, 0x3f, 0xfc, 0x3f, 0xfe, 0x7f, 0xfe, 0x7f,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe, 0x7f, 0xfe, 0x7f,
    0xfc, 0x3f, 0xfc, 0x3f, 0xf0, 0x0f, 0xc0, 0x03};


#define hourglass_width 16
#define hourglass_height 16
static unsigned char hourglass_bits[] =
{
    0x00, 0x00, 0xfe, 0x7f, 0x04, 0x20, 0x0c, 0x30, 0xf4, 0x2f, 0xe4, 0x27,
    0xc4, 0x23, 0x84, 0x21, 0x84, 0x21, 0xc4, 0x22, 0x24, 0x25, 0x94, 0x28,
    0xfc, 0x3f, 0xfc, 0x7f, 0xfe, 0x7f, 0x00, 0x00};
#define hourglass_x_hot 8
#define hourglass_y_hot 8

#define hourglass_mask_width 16
#define hourglass_mask_height 16
static unsigned char hourglass_mask_bits[] =
{
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f,
    0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f,
    0xfe, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

/*
 * define cursors
 */
void
CURS_Initialize(ScreenInfo *scr)
{
    Pixmap modal, modal_mask;
    Pixmap hourglass, hourglass_mask;
    XColor black, white;

    scr->cursors[POSITION_CURS] = XCreateFontCursor(dpy, XC_top_left_corner);
    scr->cursors[DEFAULT_CURS] = XCreateFontCursor(dpy, XC_top_left_arrow);
    scr->cursors[SYS_CURS] = XCreateFontCursor(dpy, XC_hand2);
    scr->cursors[TITLE_CURS] = XCreateFontCursor(dpy, XC_top_left_arrow);
    scr->cursors[MOVE_CURS] = XCreateFontCursor(dpy, XC_fleur);
    scr->cursors[MENU_CURS] = XCreateFontCursor(dpy, XC_arrow);
    scr->cursors[WAIT_CURS] = XCreateFontCursor(dpy, XC_watch);
    scr->cursors[SELECT_CURS] = XCreateFontCursor(dpy, XC_dot);
    scr->cursors[DESTROY_CURS] = XCreateFontCursor(dpy, XC_pirate);
    scr->cursors[LEFT_CURS] = XCreateFontCursor(dpy, XC_left_side);
    scr->cursors[RIGHT_CURS] = XCreateFontCursor(dpy, XC_right_side);
    scr->cursors[TOP_CURS] = XCreateFontCursor(dpy, XC_top_side);
    scr->cursors[BOTTOM_CURS] = XCreateFontCursor(dpy, XC_bottom_side);
    scr->cursors[TOP_LEFT_CURS] = XCreateFontCursor(dpy, XC_top_left_corner);
    scr->cursors[TOP_RIGHT_CURS] = XCreateFontCursor(dpy, XC_top_right_corner);
    scr->cursors[BOT_LEFT_CURS] =
	XCreateFontCursor(dpy, XC_bottom_left_corner);
    scr->cursors[BOT_RIGHT_CURS] =
	XCreateFontCursor(dpy, XC_bottom_right_corner);

    if ((!XParseColor(dpy, DefaultColormap(dpy, scr->screen), "White", &white)
	 && !XParseColor(dpy, DefaultColormap(dpy, scr->screen),
			 "#FFFFFFFFFFFF", &white)) ||
	!XAllocColor(dpy, DefaultColormap(dpy, scr->screen), &white))
	white.pixel = WhitePixel(dpy, scr->screen);

    if ((!XParseColor(dpy, DefaultColormap(dpy, scr->screen), "Black", &black)
	 && !XParseColor(dpy, DefaultColormap(dpy, scr->screen),
			 "#000000000000", &black)) ||
	!XAllocColor(dpy, DefaultColormap(dpy, scr->screen), &black))
	black.pixel = BlackPixel(dpy, scr->screen);


    modal = XCreateBitmapFromData(dpy, RootWindow(dpy, scr->screen),
				  (char *)modal_bits,
				  modal_width, modal_height);
    modal_mask = XCreateBitmapFromData(dpy, RootWindow(dpy, scr->screen),
				       (char *)modal_mask_bits,
				       modal_mask_width, modal_mask_height);
    scr->cursors[SYS_MODAL_CURS] = XCreatePixmapCursor(dpy,
						       modal,
						       modal_mask,
						       &black, &white,
						       modal_x_hot,
						       modal_y_hot);


    hourglass = XCreateBitmapFromData(dpy, RootWindow(dpy, scr->screen),
				      (char *)hourglass_bits,
				      hourglass_width, hourglass_height);
    hourglass_mask = XCreateBitmapFromData(dpy, RootWindow(dpy, scr->screen),
					   (char *)hourglass_mask_bits,
					   hourglass_mask_width,
					   hourglass_mask_height);
    scr->cursors[HOURGLASS_CURS] = XCreatePixmapCursor(dpy,
						       hourglass,
						       hourglass_mask,
						       &black, &white,
						       hourglass_x_hot,
						       hourglass_y_hot);
}
