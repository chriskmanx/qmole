/*--------------------------------*-C-*---------------------------------*
 * File:	scrollbar-xterm.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1997,1998   mj olesen <olesen@me.QueensU.CA>
 * Copyright (c) 1999-2001   Geoff Wing <gcw@pobox.com>
 * Copyright (c) 2004        Jingmin Zhou <jimmyzhou@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *----------------------------------------------------------------------*/
/*
** $Id: scrollbar-xterm.c,v 1.17 2004/11/11 00:20:28 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"


#ifdef DEBUG_VERBOSE
#define DEBUG_LEVEL 1
#else 
#define DEBUG_LEVEL 0
#endif

#if DEBUG_LEVEL
#define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
#define DBG_MSG(d,x)
#endif


/*----------------------------------------------------------------------*/
#ifdef HAVE_SCROLLBARS
#ifdef XTERM_SCROLLBAR


#define CHOOSE_GC_FG(R, PIXCOL)	\
	XSetForeground ((R)->Xdisplay, (R)->scrollBar.gc, (PIXCOL))


#define x_stp_width		(8)
#define x_stp_height	(2)
const unsigned char x_stp_bits[] = { 0x55, 0xaa };


/* EXTPROTO */
void
rxvt_scrollbar_init_xterm (rxvt_t* r)
{
	XGCValues       gcvalue;
	unsigned long	gcmask;


	r->scrollBar.gc = None;

	/* Initialize the colors */
	r->scrollBar.xterm_fg = r->PixColors[Color_fg];
	r->scrollBar.xterm_bg = r->PixColors[Color_bg];
	r->scrollBar.xterm_shadow = r->PixColors[Color_border];


	gcvalue.stipple = XCreateBitmapFromData (r->Xdisplay,
			r->scrollBar.win, (char *)x_stp_bits,
			x_stp_width, x_stp_height);
	assert (None != gcvalue.stipple);

	gcvalue.fill_style = FillOpaqueStippled;
	gcvalue.foreground = r->scrollBar.xterm_fg;
#ifdef TRANSPARENT
	/* set background color when there's no transparent */
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar)))
#endif
#ifdef BACKGROUND_IMAGE
	/* set background color when there's no bg image */
	if (None == r->scrollBar.pixmap)
#endif
	gcvalue.background = r->scrollBar.xterm_bg;
	gcmask = GCForeground | GCFillStyle | GCStipple;
#ifdef TRANSPARENT
	/* set background color when there's no transparent */
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar)))
#endif
#ifdef BACKGROUND_IMAGE
	/* set background color when there's no bg image */
	if (None == r->scrollBar.pixmap)
#endif
	gcmask  |= GCBackground;

	r->scrollBar.gc = XCreateGC (r->Xdisplay, r->scrollBar.win,
			gcmask, &gcvalue);
	assert (None != r->scrollBar.gc);

#ifdef TRANSPARENT
	/* set background color when there's no transparent */
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar)))
#endif
#ifdef BACKGROUND_IMAGE
	/* set background color when there's no bg image */
	if (None == r->scrollBar.pixmap)
#endif
		XSetWindowBackground (r->Xdisplay, r->scrollBar.win,
			r->scrollBar.xterm_bg);
}


void
rxvt_scrollbar_exit_xterm (rxvt_t* r)
{
	/* No resources to free */
}


/* EXTPROTO */
int
rxvt_scrollbar_show_xterm(rxvt_t *r, int update __attribute__((unused)), int last_top, int last_bot, int scroller_len)
{
    int			xsb = 0;
    int			sbwidth = r->scrollBar.width - 1;
	int			clear = 0;


	/* scrollbar slider */
#ifdef TRANSPARENT
	if ((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar))
		clear = 1;
	else
#endif
#ifdef BACKGROUND_IMAGE
	if (None != r->scrollBar.pixmap)
		clear = 1;
	else
#endif
		clear = 0;


    xsb = (r->Options & Opt_scrollBar_right) ? 1 : 0;
    if (last_top < r->scrollBar.top)
		XClearArea(r->Xdisplay, r->scrollBar.win,
			r->sb_shadow + xsb, last_top,
			sbwidth, (r->scrollBar.top - last_top), False);

    if (r->scrollBar.bot < last_bot)
		XClearArea(r->Xdisplay, r->scrollBar.win,
			r->sb_shadow + xsb, r->scrollBar.bot,
			sbwidth, (last_bot - r->scrollBar.bot + clear), False);

	/* scrollbar slider */
	CHOOSE_GC_FG(r, r->scrollBar.xterm_fg);
    XFillRectangle(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		r->sb_shadow + xsb + 1, r->scrollBar.top,
		sbwidth - 2, scroller_len);

	CHOOSE_GC_FG(r, r->scrollBar.xterm_shadow);
    XDrawLine(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		xsb ? r->sb_shadow : r->sb_shadow + sbwidth, r->scrollBar.beg,
		xsb ? r->sb_shadow : r->sb_shadow + sbwidth, r->scrollBar.end);

    return 1;
}

#endif	/* XTERM_SCROLLBAR */
#endif	/* HAVE_SCROLLBARS */
/*----------------------- end-of-file (C source) -----------------------*/
