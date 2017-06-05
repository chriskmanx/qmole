/*--------------------------------*-C-*---------------------------------*
 * File:	scrollbar-plain.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1997,1998   mj olesen <olesen@me.QueensU.CA>
 * Copyright (c) 1999-2001   Geoff Wing <gcw@pobox.com>
 * Copyright (c) 2004        Marc Lehmann <pcg@goof.com>
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
** $Id: scrollbar-plain.c,v 1.6 2004/12/08 20:30:17 cvs Exp $
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
#ifdef PLAIN_SCROLLBAR


#define CHOOSE_GC_FG(R, PIXCOL)	\
	XSetForeground ((R)->Xdisplay, (R)->scrollBar.gc, (PIXCOL))


/* EXTPROTO */
void
rxvt_scrollbar_init_plain (rxvt_t* r)
{
	XGCValues       gcvalue;
	unsigned long	gcmask;


	r->scrollBar.gc = None;

	/* Initialize the colors */
#ifdef KEEP_SCROLLCOLOR
	r->scrollBar.plain_fg = r->PixColors[Color_scroll];
#else
	r->scrollBar.plain_fg = r->PixColors[Color_bg];
#endif
	r->scrollBar.plain_bg = r->PixColors[Color_fg];

	/* Create GC */
	gcvalue.foreground = r->scrollBar.plain_fg;
#ifdef TRANSPARENT
	/* set background color when there's no transparent */
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar)))
#endif
#ifdef BACKGROUND_IMAGE
	/* set background color when there's no bg image */
	if (None == r->scrollBar.pixmap)
#endif
	gcvalue.background = r->scrollBar.plain_bg;
	gcmask = GCForeground;
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
			r->scrollBar.plain_bg);
}


void
rxvt_scrollbar_exit_plain (rxvt_t* r)
{
	/* No resources to free */
}


/* EXTPROTO */
int
rxvt_scrollbar_show_plain(rxvt_t *r, int update __attribute__((unused)), int last_top, int last_bot, int scroller_len)
{
    int			xsb = 0;
    int			sbwidth = r->scrollBar.width - 1;
	int			clear = 0;	/* Call XClearArea? */


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


	CHOOSE_GC_FG(r, r->scrollBar.plain_fg);
	if (clear)	{
		/* transparent background or background image */
		XClearArea (r->Xdisplay, r->scrollBar.win,
			r->sb_shadow + xsb, r->scrollBar.top,
			sbwidth, scroller_len,
			False);
		XDrawRectangle(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
			r->sb_shadow + xsb, r->scrollBar.top,
			sbwidth, scroller_len);
	}
	else	{
		XFillRectangle(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
			r->sb_shadow + xsb, r->scrollBar.top,
			sbwidth, scroller_len);
	}

    return 1;
}

#endif	/* PLAIN_SCROLLBAR */
#endif	/* HAVE_SCROLLBARS */
/*----------------------- end-of-file (C source) -----------------------*/
