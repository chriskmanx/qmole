/*--------------------------------*-C-*---------------------------------*
 * File:	scrollbar.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1997,1998   mj olesen <olesen@me.QueensU.CA>
 * Copyright (c) 1998        Alfredo K. Kojima <kojima@windowmaker.org>
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
** $Id: scrollbar.c,v 1.33 2004/11/16 22:44:31 cvs Exp $
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


#ifdef HAVE_SCROLLBARS

/*
** Note: These beg/end values save the length of scrollbar
** EXCLUDING the top/bottom arrow button if there are!!!
*/
#define R_SCROLLBEG_PLAIN	(0)
#define R_SCROLLEND_PLAIN	VT_HEIGHT(r)

#define R_SCROLLBEG_XTERM	(0)
#define R_SCROLLEND_XTERM	VT_HEIGHT(r)

#define R_SCROLLBEG_NEXT	(0)
#define R_SCROLLEND_NEXT	(VT_HEIGHT(r) - (NEXT_SB_TBTN_HEIGHT + \
							NEXT_SB_PAD))

#define R_SCROLLBEG_RXVT	(r->scrollBar.width + 1) + r->sb_shadow
#define R_SCROLLEND_RXVT	(VT_HEIGHT(r) - R_SCROLLBEG_RXVT - \
							((r->sb_shadow)<<1))

#define R_SCROLLBEG_SGI		(SGI_SB_BUTTON_HEIGHT)
#define R_SCROLLEND_SGI		(VT_HEIGHT(r) - R_SCROLLBEG_SGI - \
							((r->sb_shadow)<<1))

/*----------------------------------------------------------------------*/

/* EXTPROTO */
void
rxvt_scrollbar_init(rxvt_t* r)
{
	char*		scrollalign = (char*) r->h->rs[Rs_scrollBar_align];
	char*		scrollstyle = (char*) r->h->rs[Rs_scrollBar_style];
	char*		thickness = (char*) r->h->rs[Rs_scrollBar_thickness];
	int			i;
	short		style = R_SB_UNKNOWN, width = 0;


	if (scrollstyle) {
		/* use specified style by default */
# ifdef PLAIN_SCROLLBAR
		if (0 == STRNCASECMP (scrollstyle, "plain", 5))
			style = R_SB_PLAIN;
# endif
# ifdef XTERM_SCROLLBAR
		if (0 == STRNCASECMP (scrollstyle, "xterm", 5))
			style = R_SB_XTERM;
# endif
# ifdef RXVT_SCROLLBAR
		if (0 == STRNCASECMP (scrollstyle, "rxvt", 4))
			style = R_SB_RXVT;
# endif
# ifdef NEXT_SCROLLBAR
		if (0 == STRNCASECMP (scrollstyle, "next", 4))
			style = R_SB_NEXT;
# endif
# ifdef SGI_SCROLLBAR
		if (0 == STRNCASECMP (scrollstyle, "sgi", 3))
			style = R_SB_SGI;
# endif
	}

	/*
	** If something goes wrong with default style, or default style
	** is not specified by the user, we decide it.
	*/
	if (R_SB_UNKNOWN == style || !scrollstyle)	{
# ifdef PLAIN_SCROLLBAR
		style = R_SB_PLAIN;	/* may be overrided below */
# endif
# ifdef XTERM_SCROLLBAR
		style = R_SB_XTERM;	/* may be overrided below */
# endif
# ifdef RXVT_SCROLLBAR
		style = R_SB_RXVT;	/* may be overrided below */
# endif
# ifdef NEXT_SCROLLBAR
		style = R_SB_NEXT;
# endif
# ifdef SGI_SCROLLBAR
		if (R_SB_UNKNOWN == style)
			style = R_SB_SGI;
# endif
		assert (R_SB_UNKNOWN != style);	/* impossible case */
	}

	switch (style)	{
	case R_SB_PLAIN:
		width = SB_WIDTH_PLAIN;
		break;
	case R_SB_XTERM:
		width = SB_WIDTH_XTERM;
		break;
	case R_SB_RXVT:
		width = SB_WIDTH_RXVT;
		break;
	case R_SB_NEXT:
		width = SB_WIDTH_NEXT;
		break;
	case R_SB_SGI:
		width = SB_WIDTH_SGI;
		break;
	default :
		assert (0);	/* should not reach here */
	}


	if (style != R_SB_NEXT)	/* dishonour request - for now */
		if (thickness && (i = atoi(thickness)) >= SB_WIDTH_MINIMUM)
			width = min(i, SB_WIDTH_MAXIMUM);

# ifdef RXVT_SCROLLBAR
	if (!(r->Options & Opt_scrollBar_floating) && style == R_SB_RXVT)
		r->sb_shadow = SHADOW;
# else
	r->sb_shadow = 0;
# endif

	r->scrollBar.style = style;
	r->scrollBar.width = width;

	/* r->h->scrollbar_align = R_SB_ALIGN_CENTRE; */
	if (scrollalign) {
		if (STRNCASECMP(scrollalign, "top", 3) == 0)
			r->h->scrollbar_align = R_SB_ALIGN_TOP;
		else if (STRNCASECMP(scrollalign, "bottom", 6) == 0)
			r->h->scrollbar_align = R_SB_ALIGN_BOTTOM;
	}
# if defined(PLAIN_SCROLLBAR)
	if (r->scrollBar.style == R_SB_PLAIN)
		r->scrollBar.update = rxvt_scrollbar_show_plain;
# endif
# if defined(XTERM_SCROLLBAR)
	if (r->scrollBar.style == R_SB_XTERM)
		r->scrollBar.update = rxvt_scrollbar_show_xterm;
# endif
# if defined(NEXT_SCROLLBAR)
	if (r->scrollBar.style == R_SB_NEXT)
		r->scrollBar.update = rxvt_scrollbar_show_next;
# endif
# if defined(RXVT_SCROLLBAR)
	if (r->scrollBar.style == R_SB_RXVT) 
		r->scrollBar.update = rxvt_scrollbar_show_rxvt;
# endif
# if defined(SGI_SCROLLBAR)
	if (r->scrollBar.style == R_SB_SGI) 
		r->scrollBar.update = rxvt_scrollbar_show_sgi;
# endif


	r->scrollBar.win = None;
	r->scrollBar.state = 0;
}


/*
** Hide the scrollbar
*/
/* EXTPROTO */
int
rxvt_scrollbar_hide (rxvt_t* r)
{
	int		changed = 0;
	assert (None != r->scrollBar.win);
	changed = r->scrollBar.state;
	XUnmapWindow(r->Xdisplay, r->scrollBar.win);
	r->scrollBar.state = 0;
	/* scrollbar_setIdle(); */

	return (changed);
}


/*
** Show the scrollbar
*/
/* EXTPROTO */
int
rxvt_scrollbar_show (rxvt_t* r)
{
	int		changed = 0;
	assert (None != r->scrollBar.win);
	changed = !r->scrollBar.state;
	XMapWindow(r->Xdisplay, r->scrollBar.win);
	r->scrollBar.state = 1;
	return (changed);
}


/* EXTPROTO */
void
rxvt_scrollbar_create (rxvt_t* r)
{
	int				sb_x, sb_y;
	unsigned int	sb_w, sb_h;


	DBG_MSG(1,(stderr,"Create scrollbar\n"));
	assert (None != r->TermWin.parent);

	sb_x = (r->Options & Opt_scrollBar_right) ? VT_WIDTH(r) : 0;
	sb_y = r->h->window_vt_y;
	sb_w = rxvt_scrollbar_rwidth (r);	/* real scrollbar width */
	sb_h = VT_HEIGHT(r);	/* same height as vt window */

	/* set scrollbar parameters */
# ifdef PLAIN_SCROLLBAR
	if (r->scrollBar.style == R_SB_PLAIN) {
		r->scrollBar.beg = R_SCROLLBEG_PLAIN;
		r->scrollBar.end = R_SCROLLEND_PLAIN;
	}
# endif
# ifdef XTERM_SCROLLBAR
	if (r->scrollBar.style == R_SB_XTERM) {
		r->scrollBar.beg = R_SCROLLBEG_XTERM;
		r->scrollBar.end = R_SCROLLEND_XTERM;
	}
# endif
# ifdef NEXT_SCROLLBAR
	if (r->scrollBar.style == R_SB_NEXT) {
		r->scrollBar.beg = R_SCROLLBEG_NEXT;
		r->scrollBar.end = R_SCROLLEND_NEXT;
	}
# endif
# ifdef RXVT_SCROLLBAR
	if (r->scrollBar.style == R_SB_RXVT) {
		r->scrollBar.beg = R_SCROLLBEG_RXVT;
		r->scrollBar.end = R_SCROLLEND_RXVT;
	}
# endif
# ifdef SGI_SCROLLBAR
	if (r->scrollBar.style == R_SB_SGI) {
		r->scrollBar.beg = R_SCROLLBEG_SGI;
		r->scrollBar.end = R_SCROLLEND_SGI;
	}
# endif


	r->scrollBar.win = XCreateSimpleWindow(r->Xdisplay,
						r->TermWin.parent,
						sb_x, sb_y, sb_w, sb_h,
						0,
						r->PixColors[Color_fg],
						r->PixColors[Color_bg]);
	assert (None != r->scrollBar.win);

# ifdef DEBUG_X
	rxvt_set_win_title (r, r->scrollBar.win, "scrollbar");
# endif

	XDefineCursor(r->Xdisplay, r->scrollBar.win, r->h->bar_pointer);
	XSelectInput(r->Xdisplay, r->scrollBar.win,
				(ExposureMask | ButtonPressMask | ButtonReleaseMask
				| Button1MotionMask | Button2MotionMask
				| Button3MotionMask));

# ifdef BACKGROUND_IMAGE
	r->scrollBar.pixmap = None;	/* initialize it to None */
#  ifdef TRANSPARENT
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar)))
#  endif
	if (r->h->rs[Rs_scrollbarPixmap])	{
		long	w = 0, h = 0;
		r->scrollBar.pixmap = rxvt_load_pixmap (r,
								r->h->rs[Rs_scrollbarPixmap], &w, &h);
		if (None != r->scrollBar.pixmap)
			XSetWindowBackgroundPixmap (r->Xdisplay, r->scrollBar.win,
				r->scrollBar.pixmap);
	}
# endif

# ifdef TRANSPARENT
	if ((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_scrollbar))	{
		XSetWindowBackgroundPixmap (r->Xdisplay, r->scrollBar.win,
			ParentRelative);
	}
# endif
	r->scrollBar.state = 0;


	/* Final initialization of the scrollbars */
# ifdef PLAIN_SCROLLBAR
	if (r->scrollBar.style == R_SB_PLAIN)
		rxvt_scrollbar_init_plain (r);
# endif
# ifdef XTERM_SCROLLBAR
	if (r->scrollBar.style == R_SB_XTERM)
		rxvt_scrollbar_init_xterm (r);
# endif
# ifdef RXVT_SCROLLBAR
	if (r->scrollBar.style == R_SB_RXVT)
		rxvt_scrollbar_init_rxvt (r);
# endif
# ifdef NEXT_SCROLLBAR
	if (r->scrollBar.style == R_SB_NEXT)
		rxvt_scrollbar_init_next (r);
# endif
# ifdef SGI_SCROLLBAR
	if (r->scrollBar.style == R_SB_SGI)
		rxvt_scrollbar_init_sgi (r);
# endif
}


/* EXTPROTO */
void
rxvt_scrollbar_clean_exit (rxvt_t* r)
{
	r->scrollBar.win = None;	/* Destroyed by XDestroySubwindows */

	if (None != r->scrollBar.gc)	{
		XFreeGC (r->Xdisplay, r->scrollBar.gc);
		r->scrollBar.gc = None;
	}

#ifdef BACKGROUND_IMAGE
	if (None != r->scrollBar.pixmap)	{
		XFreePixmap (r->Xdisplay, r->scrollBar.pixmap);
		r->scrollBar.pixmap = None;
	}
#endif

# ifdef PLAIN_SCROLLBAR
	if (r->scrollBar.style == R_SB_PLAIN)
		rxvt_scrollbar_exit_plain (r);
# endif
# ifdef XTERM_SCROLLBAR
	if (r->scrollBar.style == R_SB_XTERM)
		rxvt_scrollbar_exit_xterm (r);
# endif
# ifdef RXVT_SCROLLBAR
	if (r->scrollBar.style == R_SB_RXVT)
		rxvt_scrollbar_exit_rxvt (r);
# endif
# ifdef NEXT_SCROLLBAR
	if (r->scrollBar.style == R_SB_NEXT)
		rxvt_scrollbar_exit_next (r);
# endif
# ifdef SGI_SCROLLBAR
	if (r->scrollBar.style == R_SB_SGI)
		rxvt_scrollbar_exit_sgi (r);
# endif
}


/* EXTPROTO */
void
rxvt_scrollbar_resize(rxvt_t *r)
{
	int				sb_x, sb_y;
	unsigned int	sb_w, sb_h;


	sb_x = (r->Options & Opt_scrollBar_right) ? VT_WIDTH(r) : 0;
	sb_y = r->h->window_vt_y;
	sb_w = rxvt_scrollbar_rwidth (r);	/* real scrollbar width */
	sb_h = VT_HEIGHT(r);	/* same height as vt window */
	XMoveResizeWindow (r->Xdisplay, r->scrollBar.win,
		sb_x, sb_y, sb_w, sb_h);


# ifdef PLAIN_SCROLLBAR
	if (r->scrollBar.style == R_SB_PLAIN) {
		r->scrollBar.beg = R_SCROLLBEG_PLAIN;
		r->scrollBar.end = R_SCROLLEND_PLAIN;
	}
# endif
# ifdef XTERM_SCROLLBAR
	if (r->scrollBar.style == R_SB_XTERM) {
		r->scrollBar.beg = R_SCROLLBEG_XTERM;
		r->scrollBar.end = R_SCROLLEND_XTERM;
	}
# endif
# ifdef NEXT_SCROLLBAR
	if (r->scrollBar.style == R_SB_NEXT) {
		r->scrollBar.beg = R_SCROLLBEG_NEXT;
		r->scrollBar.end = R_SCROLLEND_NEXT;
	}
# endif
# ifdef RXVT_SCROLLBAR
	if (r->scrollBar.style == R_SB_RXVT) {
		r->scrollBar.beg = R_SCROLLBEG_RXVT;
		r->scrollBar.end = R_SCROLLEND_RXVT;
	}
# endif
# ifdef SGI_SCROLLBAR
	if (r->scrollBar.style == R_SB_SGI) {
		r->scrollBar.beg = R_SCROLLBEG_SGI;
		r->scrollBar.end = R_SCROLLEND_SGI;
	}
# endif

	rxvt_scrollbar_update(r, 0);
}


/* EXTPROTO */
int
rxvt_scrollbar_visible(rxvt_t *r)
{
	return (None != r->scrollBar.win && r->scrollBar.state);
}


/*
 * Update current scrollbar view w.r.t. slider heights, etc.
 */
/* EXTPROTO */
int
rxvt_scrollbar_update (rxvt_t* r, int update)
{
	int			 ret = 0;
	int			 top, bot, len, adj;

	if (!rxvt_scrollbar_visible(r))
		return 0;

	if (update) {
		/* first line in the view */
		top = (AVTS(r)->nscrolled - AVTS(r)->view_start);
		/* last line in the view */
		bot = top + (r->TermWin.nrow - 1);
		/* total lines in scroll and view */
		len = max((AVTS(r)->nscrolled + (r->TermWin.nrow - 1)), 1);
		adj = (((bot - top) * scrollbar_size()) % len) > 0 ? 1 : 0;

		/*
		** scrollBar.beg  : beginning of scrollbar, always 0
		** scrollBar.end  : end of scrollbar excluding buttons
		** scrollbar_size : scrollbar length excluding minimal height
		**
		** scrollBar.top  : beginning of scroller
		** scrollBar.bot  : end of scroller
		** scroller_len  : length of scroller
		*/
		r->scrollBar.top = (r->scrollBar.beg + (top * scrollbar_size()) / len);
		r->h->scroller_len = ((bot - top) * scrollbar_size()) / len +
								scrollbar_minheight() + adj;
		r->scrollBar.bot = (r->scrollBar.top + r->h->scroller_len);

		/* no change */
		if (r->scrollBar.top == r->h->last_top &&
			r->scrollBar.bot == r->h->last_bot &&
			(r->scrollBar.state == r->h->last_state ||
			 !scrollbar_isUpDn()))
			return 0;
	}

	ret = r->scrollBar.update(r, update, r->h->last_top, r->h->last_bot,
					r->h->scroller_len);

	r->h->last_top = r->scrollBar.top;
	r->h->last_bot = r->scrollBar.bot;
	r->h->last_state = r->scrollBar.state;

	return ret;
}


/* EXTPROTO */
unsigned short
rxvt_scrollbar_width(rxvt_t *r)
{
	if (None == r->scrollBar.win || !r->scrollBar.state)
		return 0;
	return (r->scrollBar.width + r->sb_shadow * 2);
}


/* EXTPROTO */
unsigned short
rxvt_scrollbar_rwidth(rxvt_t *r)
{
	return (r->scrollBar.width + r->sb_shadow * 2);
}


/* EXTPROTO */
int
rxvt_is_scrollbar_win(rxvt_t *r, Window w)
{
	return (w == r->scrollBar.win);
}

#endif /* HAVE_SCROLLBARS */
/*----------------------- end-of-file (C source) -----------------------*/
