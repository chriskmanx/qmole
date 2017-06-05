/*--------------------------------*-C-*---------------------------------*
 * File:	scrollbar-rxvt.c
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
** $Id: scrollbar-rxvt.c,v 1.18 2004/11/11 00:20:28 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"


#ifdef DEBUG_VERBOSE
# define DEBUG_LEVEL 1
#else 
# define DEBUG_LEVEL 0
#endif

#if DEBUG_LEVEL
# define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
# define DBG_MSG(d,x)
#endif


/*----------------------------------------------------------------------*/
#ifdef HAVE_SCROLLBARS
#ifdef RXVT_SCROLLBAR


#define CHOOSE_GC_FG(R, PIXCOL)	\
	XSetForeground ((R)->Xdisplay, (R)->scrollBar.gc, (PIXCOL))


/* draw triangular button with a shadow of SHADOW (1 or 2) pixels */
/* INTPROTO */
void
rxvt_draw_button(rxvt_t *r, int x, int y, int state, int dirn)
{
	unsigned int	sz, sz2;
	XPoint			pt[3];
	unsigned long	top, bot;

	sz = r->scrollBar.width;
	sz2 = sz / 2;
	switch (state) {
	case +1:
		top = r->scrollBar.rxvt_topshadow;
		bot = r->scrollBar.rxvt_botshadow;
		break;
	case -1:
		top = r->scrollBar.rxvt_botshadow;
		bot = r->scrollBar.rxvt_topshadow;
		break;
	default:
		top = bot = r->scrollBar.rxvt_fg;
		break;
	}

	/* fill triangle */
	pt[0].x = x;
	pt[1].x = x + sz - 1;
	pt[2].x = x + sz2;
	if (dirn == UP) {
		pt[0].y = pt[1].y = y + sz - 1;
		pt[2].y = y;
	} else {
		pt[0].y = pt[1].y = y;
		pt[2].y = y + sz - 1;
	}

	CHOOSE_GC_FG (r, r->scrollBar.rxvt_fg);
	XFillPolygon(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		 pt, 3, Convex, CoordModeOrigin);

	/* draw base */
	CHOOSE_GC_FG (r, (dirn == UP) ? bot : top);
	XDrawLine(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		  pt[0].x, pt[0].y, pt[1].x, pt[1].y);

	/* draw shadow on left */
	pt[1].x = x + sz2 - 1;
	pt[1].y = y + (dirn == UP ? 0 : sz - 1);
	CHOOSE_GC_FG (r, top);
	XDrawLine(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		  pt[0].x, pt[0].y, pt[1].x, pt[1].y);

#if (SHADOW > 1)
	/* doubled */
	pt[0].x++;
	if (dirn == UP) {
		pt[0].y--;
		pt[1].y++;
	} else {
		pt[0].y++;
		pt[1].y--;
	}
	CHOOSE_GC_FG (r, top);
	XDrawLine(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		  pt[0].x, pt[0].y, pt[1].x, pt[1].y);
#endif
	/* draw shadow on right */
	pt[1].x = x + sz - 1;
	/*  pt[2].x = x + sz2; */
	pt[1].y = y + (dirn == UP ? sz - 1 : 0);
	pt[2].y = y + (dirn == UP ? 0 : sz - 1);
	CHOOSE_GC_FG (r, bot);
	XDrawLine(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		  pt[2].x, pt[2].y, pt[1].x, pt[1].y);
#if (SHADOW > 1)
	/* doubled */
	pt[1].x--;
	if (dirn == UP) {
		pt[2].y++;
		pt[1].y--;
	} else {
		pt[2].y--;
		pt[1].y++;
	}
	CHOOSE_GC_FG (r, bot);
	XDrawLine(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		  pt[2].x, pt[2].y, pt[1].x, pt[1].y);
#endif
}



/* EXTPROTO */
void
rxvt_scrollbar_init_rxvt (rxvt_t* r)
{
	XGCValues		gcvalue;
	unsigned long	gcmask;


	/* Initialize the colors */
	r->scrollBar.rxvt_fg = r->PixColors[Color_trough];
	r->scrollBar.rxvt_bg = r->PixColors[Color_bottomShadow];
	r->scrollBar.rxvt_topshadow = r->PixColors[Color_topShadow];
	r->scrollBar.rxvt_botshadow = r->PixColors[Color_bottomShadow];


	gcvalue.foreground = r->scrollBar.rxvt_fg;
# ifdef TRANSPARENT
	/* set background color when there's no transparent */
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar)))
# endif
# ifdef BACKGROUND_IMAGE
	/* set background color when there's no bg image */
	if (None == r->scrollBar.pixmap)
# endif
	gcvalue.background = r->scrollBar.rxvt_bg;
	gcmask = GCForeground;
# ifdef TRANSPARENT
	/* set background color when there's no transparent */
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar)))
# endif
# ifdef BACKGROUND_IMAGE
	/* set background color when there's no bg image */
	if (None == r->scrollBar.pixmap)
# endif
	gcmask |= GCBackground;
	r->scrollBar.gc = XCreateGC (r->Xdisplay, r->scrollBar.win,
						gcmask, &gcvalue);
	assert (None != r->scrollBar.gc);

# ifdef TRANSPARENT
	/* set background color when there's no transparent */
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar)))
# endif
# ifdef BACKGROUND_IMAGE
	/* set background color when there's no bg image */
	if (None == r->scrollBar.pixmap)
# endif
		XSetWindowBackground (r->Xdisplay, r->scrollBar.win,
			r->scrollBar.rxvt_bg);
}


/* EXTPROTO */
void
rxvt_scrollbar_exit_rxvt (rxvt_t* r)
{
	/* No resources to free */
}


/* EXTPROTO */
int
rxvt_scrollbar_show_rxvt(rxvt_t *r, int update __attribute__((unused)), int last_top, int last_bot, int scroller_len)
{
	int			 sbshadow = r->sb_shadow;
	int			 sbwidth = (int)r->scrollBar.width;


	if (last_top < r->scrollBar.top)
		XClearArea(r->Xdisplay, r->scrollBar.win,
			sbshadow, last_top,
			sbwidth, (r->scrollBar.top - last_top),
			False);

	if (r->scrollBar.bot < last_bot)
		XClearArea(r->Xdisplay, r->scrollBar.win,
			sbshadow, r->scrollBar.bot,
			sbwidth, (last_bot - r->scrollBar.bot),
			False);

	/* scrollbar slider */
#ifdef TRANSPARENT
	/* clear background when there's transparent */
	if ((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar))	{
		XClearArea (r->Xdisplay, r->scrollBar.win,
			sbshadow, r->scrollBar.top, sbwidth, scroller_len,
			False);
	}
	else
#endif
# ifdef BACKGROUND_IMAGE
	/* clear background when there's bg image */
	if (None != r->scrollBar.pixmap)	{
		XClearArea (r->Xdisplay, r->scrollBar.win,
			sbshadow, r->scrollBar.top, sbwidth, scroller_len,
			False);
	}
	else
# endif
	{
		CHOOSE_GC_FG (r, r->scrollBar.rxvt_fg);
		XFillRectangle (r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
			sbshadow, r->scrollBar.top, sbwidth, scroller_len);
	}

	if (sbshadow)
		/* trough shadow */
		rxvt_draw_shadow(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
			r->scrollBar.rxvt_botshadow, r->scrollBar.rxvt_topshadow,
			0, 0,
			sbwidth + 2 * sbshadow, /* scrollbar_TotalWidth() */
			r->scrollBar.end + (sbwidth + 1) + sbshadow);
	/* shadow for scrollbar slider */
	rxvt_draw_shadow(r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		r->scrollBar.rxvt_topshadow, r->scrollBar.rxvt_botshadow,
		sbshadow, r->scrollBar.top, sbwidth,
		scroller_len);

	/* Redraw scrollbar arrows */
	rxvt_draw_button(r, sbshadow, sbshadow,
			 (scrollbar_isUp() ? -1 : +1), UP);
	rxvt_draw_button(r, sbshadow, (r->scrollBar.end + 1),
			 (scrollbar_isDn() ? -1 : +1), DN);
	return 1;
}

#endif	/* RXVT_SCROLLBAR */
#endif	/* HAVE_SCROLLBARS */
/*----------------------- end-of-file (C source) -----------------------*/
