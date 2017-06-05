/*--------------------------------*-C-*---------------------------------*
 * File:	tabbar.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 2002        Alexis <materm@tuxfamily.org>
 * Copyright (c) 2004        Terry Griffin <griffint@pobox.com>
 * Copyright (c) 2004        Sergey Popov <p_sergey@jungo.com>
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
** $Id: tabbar.c,v 1.124 2005/08/31 05:30:41 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"


#ifdef HAVE_LIBXPM

#include "close_term.xpm"
#include "term.xpm"
#include "right.xpm"
#include "left.xpm"
#include "close_term_d.xpm"
#include "term_d.xpm"
#include "right_d.xpm"
#include "left_d.xpm"

#else

#include "close_term.xbm"
#include "term.xbm"
#include "right.xbm"
#include "left.xbm"

#endif /* HAVE_LIBXPM */


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


/* border between the tabs */
#define TAB_BORDER		((int) 1)
/* margin around the text of the tab */
#define TXT_MARGIN		((int) 3)
/*
** Parameters to draw top tabbar
*/
/* space between top window border and tab top */
#define TAB_TOPOFF		((int) 0)
/* space between top window border and tab bottom */
#define TAB_BOTOFF		((int) (r->TermWin.fheight + 2*TXT_MARGIN))
/*
** Parameters to draw bottom tabbar
*/
/* space between top window border and tab top */
#define TAB_TOPOFF_BT	((int) TXT_MARGIN - 1)
/* space between top window border and tab bottom */
#define TAB_BOTOFF_BT	((int) (r->TermWin.fheight + 2*TXT_MARGIN))


/* X offset of text in tab */
#define TXT_XOFF		((int) 10)
/* height of text in tab */
#define TXT_YOFF		((int) (r->TermWin.fheight + TXT_MARGIN + TAB_BORDER))

/* width of No. idx tab */
#define TAB_WIDTH(idx)	((int) (TAB_BORDER + r->vts[idx]->tab_width))

/* size of button */
#define BTN_WIDTH		((int) 18)
#define BTN_HEIGHT		((int) 18)
/* space between top window border and button top */
#define BTN_TOPOFF		(max (0, ((TAB_BOTOFF - BTN_HEIGHT)/2)))
/* space between bottom window border and button top */
#define BTN_TOPOFF_BT	(max (TAB_TOPOFF_BT+1, ((TAB_BOTOFF_BT - BTN_HEIGHT)/2)))
/* space between buttons */
#define BTN_SPACE		((int) 5)

/* width of tabbar that can be used to draw tabs */
#define TAB_SPACE		(TWIN_WIDTH(r)- \
	((r->Options2 & Opt2_hideButtons) ? 0 : 1) * \
	(4 * (BTN_WIDTH+BTN_SPACE) + TAB_BORDER))


#define CHOOSE_GC_FG(R, PIXCOL)	\
	XSetForeground ((R)->Xdisplay, (R)->tabBar.gc, (PIXCOL))


enum {XPM_TERM,XPM_CLOSE,XPM_LEFT,XPM_RIGHT,NB_XPM};

#ifdef HAVE_LIBXPM
static char** xpm_name[] = {
	term_xpm,close_term_xpm,
	left_xpm,right_xpm
};
static char** xpm_d_name[] = {
	term_d_xpm,close_term_d_xpm,
	left_d_xpm,right_d_xpm
};
#else
static char *xpm_name[] = {
	term_bits,close_term_bits,
	left_bits,right_bits
};
#endif
	
static Pixmap img[NB_XPM];
#ifdef HAVE_LIBXPM
static Pixmap img_e[NB_XPM]; /* enable image */
static Pixmap img_emask[NB_XPM]; /* shape mask image */
static Pixmap img_d[NB_XPM]; /* disable image */
static Pixmap img_dmask[NB_XPM]; /* shape mask image */
#endif
	
extern char **cmd_argv;


/*
** Width between two tabs:
** From the left of the first tab to the right of the second tab
*/
/* INTPROTO */
static int
width_between (rxvt_t* r, int start, int end)
{
	register int	i, w=0;
	
	for (i = start; i <= end; i++) {
		w += TAB_WIDTH(i);
	}
	
	return w;
}


/*
** Find most left tab within specified distance. Note that the
** distance does not include the width of tab[start]. It means
** distance = (beginning of tab[start] - 0)
*/
/* INTPROTO */
static int
find_left_tab (rxvt_t* r, int start, int distance)
{
	register int	i, left;

	/* Sanatization */
	if (0 == start)
		return 0;

	/* BUG: tab overlap with button */
	if (distance < 0)
		return start;

	left = distance;
	for (i = start - 1; i >= 0; i --)	{
		if (left < TAB_WIDTH(i))
			break;
		left -= (TAB_WIDTH(i));
	}
	return (i + 1);
}



/*
** Find most right tab within specified distance. Note that the
** distance does not include the width of tab[start]. It means
** distance = (beginning of first button - end of tab[start])
*/
/* INTPROTO */
static int
find_right_tab (rxvt_t* r, int start, int distance)
{
	register int	i, left;

	/* Sanatization */
	if (LTAB(r) == start)
		return start;

	/* BUG: tab overlap with button */
	if (distance < 0)
		return start;

	left = distance;
	for (i = start + 1; i <= LTAB(r); i ++)	{
		if (left < TAB_WIDTH(i))
			break;
		left -= (TAB_WIDTH(i));
	}
	return (i - 1);
}


/* EXTPROTO */
void
rxvt_tabbar_set_visible_tabs (rxvt_t* r)
{
	/* set first visible tab to active tab */
	FVTAB(r) = ATAB(r);

	/* always try visualize the right tabs */
	LVTAB(r) = find_right_tab (r, FVTAB(r),
		TAB_SPACE - TAB_WIDTH(FVTAB(r)));

	if (LVTAB(r) == LTAB(r) && 0 != FVTAB(r))	{
		/* now try to visualize the left tabs */
		register int size = TAB_SPACE -
			width_between (r, FVTAB(r), LVTAB(r));
		FVTAB(r) = find_left_tab (r, FVTAB(r), size);
	}
}


#ifdef MULTICHAR_SET
/*
**	x, y      : starting position of string, no need to adjust y
**  str       : string to draw
**  len       : byte length of the string, not number of characters!
**  multichar : whether the string is multichar string
**  active    : active or inactive tab
*/
static inline void
draw_string (rxvt_t* r, int x, int y, char* str, int len, int multichar, int active)
{
	if (multichar)	{
		/*
		** Draw the multichar string
		*/
# ifdef XFT_SUPPORT
		if ((r->Options & Opt_xft) && (NULL != r->tabBar.xftwin))	{
#  ifdef HAVE_ICONV_H
			if (ENC_NOENC != r->encoding_method &&
				(iconv_t) -1 != r->TermWin.xfticonv)	{
				char			buf[1024];
				int				plen = 1023;
				char*			pstr = buf;
				int				olen = len;
				char*			ostr = str;

				/* convert to UTF-8 */
				iconv (r->TermWin.xfticonv, &ostr, (size_t*) &olen,
					&pstr, (size_t*) &plen);
				*pstr = (char) 0;	/* set end of string */

				rxvt_draw_string_xft (r, r->tabBar.xftwin, r->tabBar.gc,
					active ? &(r->tabBar.xftfg) : &(r->tabBar.xftifg),
					x, y, buf, len, XftDrawStringUtf8);
			}
			else
#  endif
			{
				DBG_MSG(1, (stderr, "XFT non-iconv tab title\n"));
				rxvt_draw_string_xft (r, r->tabBar.xftwin, r->tabBar.gc,
					active ? &(r->tabBar.xftfg) : &(r->tabBar.xftifg),
					x, y, str, len, XftDrawString8);
			}
		}
		else
# endif	/* XFT_SUPPORT */
		{
			if (ENC_NOENC != r->encoding_method)	{
				XSetFont (r->Xdisplay, r->tabBar.gc, r->TermWin.mfont->fid);
				r->h->multichar_decode (str, len);
				rxvt_draw_string_x11 (r, r->tabBar.win, r->tabBar.gc,
					x, y, str, len/2, XDrawString16);
			}
			else	{
				XSetFont (r->Xdisplay, r->tabBar.gc, r->TermWin.font->fid);
				rxvt_draw_string_x11 (r, r->tabBar.win, r->tabBar.gc,
					x, y, str, len, XDrawString);
			}
		}
	}

	else	{
		/*
		** Draw the non-multichar string
		*/
# ifdef XFT_SUPPORT
		if ((r->Options & Opt_xft) && (NULL != r->tabBar.xftwin))	{
			rxvt_draw_string_xft (r, r->tabBar.xftwin, r->tabBar.gc,
				active ? &(r->tabBar.xftfg) : &(r->tabBar.xftifg),
				x, y, str, len, XftDrawString8);
		}
		else
# endif	/* XFT_SUPPORT */
		{
			XSetFont (r->Xdisplay, r->tabBar.gc, r->TermWin.font->fid);
			rxvt_draw_string_x11 (r, r->tabBar.win, r->tabBar.gc,
				x, y, str, len, XDrawString);
		}
	}
}
#endif	/* MULTICHAR_SET */


/*
** Draw tab title string
*/
/* INTPROTO */
static void
draw_title (rxvt_t* r, const char* orgstr, int x, int y, int active)
{
	char		str[MAX_DISPLAY_TAB_TXT + 1];
#ifdef MULTICHAR_SET
	char		buf[MAX_TAB_TXT + 1];
	const char*	sptr;
	const char*	ptr;
	int			multichar;
	int			len;


	STRNCPY (str, orgstr, MAX_DISPLAY_TAB_TXT);
	str[MAX_DISPLAY_TAB_TXT] = (char) 0;

	/* adjust y offset */
# ifdef XFT_SUPPORT
	if ((r->Options & Opt_xft) && (NULL != r->tabBar.xftwin))
		y -= r->TermWin.xftfont->descent;
	else
# endif
	y -= r->TermWin.font->descent;

	sptr = ptr = str;
	multichar = (*ptr & 0x80);
	while (*ptr)	{
		if (multichar && (*ptr & 0x80))		/* multichar */
			ptr ++;
		else if (!multichar && !(*ptr & 0x80))	/* single char */
			ptr ++;
		else	{
			len = ptr - sptr;
			/* adjust bytes, must be 2x for multichar */
			if (multichar && (len % 2) != 0)	{
				len ++; ptr ++;
				/* continue to next byte, we shouldn't stop here */
				continue;
			}
			assert (len <= MAX_TAB_TXT);

			memcpy (buf, sptr, len);
			buf[len] = (char) 0;
			draw_string (r, x, y, buf, len, multichar, active);

			/* adjust start position */
			x += Width2Pixel(len);
			/*
#ifdef XFT_SUPPORT
			if ((r->Options & Opt_xft) && r->tabBar.xftwin)	{
				x += Width2Pixel(len);
			}
			else
#endif
			{
				if (multichar)
					x += XTextWidth (r->TermWin.mfont, buf, len/2);
				else
					x += XTextWidth (r->TermWin.font, buf, len);
			}
			*/

			/* ok, now the next sub-string */
			sptr = ptr;
			multichar = (*ptr & 0x80);
			if ((char) 0 == *ptr)
				break;	/* in case ptr is increased at line 356 */
			ptr ++;
		}
	}

	/* last sub-string */
	len = ptr - sptr;
	if (0 != len)	{	/* in case last sub-string is empty */
		memcpy (buf, sptr, len);
		buf[len] = (char) 0;
		draw_string (r, x, y, buf, len, multichar, active);
	}

#else	/* MULTICHAR_SET */

	STRNCPY (str, orgstr, MAX_DISPLAY_TAB_TXT);
	str[MAX_DISPLAY_TAB_TXT] = (char) 0;

# ifdef XFT_SUPPORT
	if ((r->Options & Opt_xft) && (NULL != r->tabBar.xftwin))	{
		/* adjust y offset */
		y -= r->TermWin.xftfont->descent;
		rxvt_draw_string_xft (r, r->tabBar.xftwin, r->tabBar.gc,
			active ? &(r->tabBar.xftfg) : &(r->tabBar.xftifg),
			x, y,
			(XftChar8*) str, STRLEN(str),
			XftDrawString8);
	}
	else
# endif	/* XFT_SUPPORT */
	{
		/* adjust y offset */
		y -= r->TermWin.font->descent;
		rxvt_draw_string_x11 (r, r->tabBar.win, r->tabBar.gc,
			x, y, (char*) str, STRLEN(str), XDrawString);
	}

#endif	/* MULTICHAR_SET */
}


/*
** Draw all visible tabs at top
*/
/* INTPROTO */
static void
draw_tabs_top (rxvt_t* r)
{
	register int	i, x;

	if (LTAB(r) < 0)
		return;
	if (None == r->tabBar.win)
		return;
	if (!r->tabBar.state)
		return;

	/* Sanatization */
	assert (LTAB(r) >= 0);
	assert (FVTAB(r) >= 0);
	assert (FVTAB(r) <= LTAB(r));
	assert (LVTAB(r) >= 0);
	assert (LVTAB(r) <= LTAB(r));
	assert (ATAB(r) >= FVTAB(r));
	assert (ATAB(r) <= LVTAB(r));


	/* redraw the inactive tabs background */
	for (i = FVTAB(r), x=TAB_BORDER;
		i <= LVTAB(r);
		x += TAB_WIDTH(i), i++) {
		int		clear = 0;	/* use ClearArea or FillRectangle */
#ifdef BACKGROUND_IMAGE
		if ((None != r->tabBar.pixmap) &&
			(r->Options & Opt_tabPixmap))
			clear = 1;	/* use background image */
#endif
#ifdef TRANSPARENT
		if ((r->Options & Opt_transparent) &&
			(r->Options & Opt_transparent_tabbar))
			clear = 1;	/* transparent override background image */
#endif
		if (!clear)	{
			if (i != ATAB(r))	{
				CHOOSE_GC_FG (r, r->tabBar.ibg);
			}
			else	{
				CHOOSE_GC_FG (r, r->tabBar.bg);
			}
			XFillRectangle  (r->Xdisplay, r->tabBar.win,
				r->tabBar.gc,
				x, TAB_TOPOFF,
				PVTS(r, i)->tab_width,
				TAB_BOTOFF - TAB_TOPOFF + 1);
				/* rxvt_tabbar_rheight (r)); */
		}
		/* else, We do not need to call ClearArea since it's 
		** already called */
	}


	CHOOSE_GC_FG (r, r->tabBar.frame);
	for (i = FVTAB(r), x=TAB_BORDER;
		i <= LVTAB(r);
		x += TAB_WIDTH(i), i++) {
		/* draw top horizontal line of the tab */
		XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
			x, TAB_TOPOFF, x+PVTS(r, i)->tab_width-1, TAB_TOPOFF);
		/* inactive tab has a thick top horizontal line */
		if (i != ATAB(r))
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_TOPOFF+1, x+PVTS(r, i)->tab_width, TAB_TOPOFF+1);

		/* draw bottom horizontal line of the tab */
		if (i != ATAB(r))
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_BOTOFF,
				x+PVTS(r, i)->tab_width, TAB_BOTOFF);

		/* draw left vertical frame line of the tab */
		XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
			x, TAB_TOPOFF,
			x, TAB_BOTOFF);
	}
	/* draw bottom horizontal frame line after the tabs */
	XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
		x, TAB_BOTOFF, TWIN_WIDTH(r), TAB_BOTOFF);


	CHOOSE_GC_FG (r, r->tabBar.delimit);
	for (i = FVTAB(r), x=TAB_BORDER;
		i <= LVTAB(r);
		x += TAB_WIDTH(i), i++) {
		/* draw right vertical line of the tab */
		XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
			x + PVTS(r, i)->tab_width, TAB_TOPOFF,
			x + PVTS(r, i)->tab_width, TAB_BOTOFF-1);
	}


	for (i = FVTAB(r), x=TAB_BORDER;
		i <= LVTAB(r);
		x += TAB_WIDTH(i), i++) {
		/* draw the text of the tab */
		CHOOSE_GC_FG (r, (i == ATAB(r)) ? r->tabBar.fg : r->tabBar.ifg);
		draw_title (r, PVTS(r, i)->tab_title, x+TXT_XOFF, TXT_YOFF,
			i == ATAB(r));
	}

}


/* INTPROTO */
static void
switch_tabs_top (rxvt_t* r, int active, int inactive)
{
	register int	i, x, count = 0;

	assert (LTAB(r) >= 0);
	assert (None != r->tabBar.win);
	assert (active >= FVTAB(r) && active <= LVTAB(r));
	assert (inactive >= FVTAB(r) && inactive <= LVTAB(r));
	assert (active != inactive);


	DBG_MSG(1, (stderr, "switch_tabs_top (%d, %d)\n", active, inactive));
	for (i = FVTAB(r), x=TAB_BORDER;
		i <= LVTAB(r);
		x += TAB_WIDTH(i), i++) {
		if (i == inactive)	{
			/* Change it to active tab. */
			int		clear = 0;	/* use ClearArea or FillRectangle */
#ifdef BACKGROUND_IMAGE
			if ((None != r->tabBar.pixmap) &&
				(r->Options & Opt_tabPixmap))
				clear = 1;	/* use background image */
#endif
#ifdef TRANSPARENT
			if ((r->Options & Opt_transparent) &&
				(r->Options & Opt_transparent_tabbar))
				clear = 1;	/* transparent override background image */
#endif
			if (clear)
				XClearArea (r->Xdisplay, r->tabBar.win,
					x, TAB_TOPOFF,
					PVTS(r, i)->tab_width,
					TAB_BOTOFF - TAB_TOPOFF + 1,
					/* rxvt_tabbar_rheight (r), */
					False);
			else	{
				CHOOSE_GC_FG (r, r->tabBar.bg);
				XFillRectangle (r->Xdisplay, r->tabBar.win,
					r->tabBar.gc,
					x, TAB_TOPOFF,
					PVTS(r, i)->tab_width,
					TAB_BOTOFF - TAB_TOPOFF + 1);
					/* rxvt_tabbar_rheight (r)); */
			}


			CHOOSE_GC_FG (r, r->tabBar.frame);
			/* draw top horizontal line of the tab */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_TOPOFF, x+PVTS(r, i)->tab_width-1, TAB_TOPOFF);
			/* draw left vertical frame line of the tab */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_TOPOFF,
				x, TAB_BOTOFF);

			/* draw right vertical line of the tab */
			CHOOSE_GC_FG (r, r->tabBar.delimit);
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x + PVTS(r, i)->tab_width, TAB_TOPOFF,
				x + PVTS(r, i)->tab_width, TAB_BOTOFF-1);

			/* draw the text of the active tab */
			CHOOSE_GC_FG (r, r->tabBar.fg);
			draw_title (r, PVTS(r, i)->tab_title, x+TXT_XOFF, TXT_YOFF,
				1);	/* active tab */

			/* no need to continue loop */
			count ++;
			if (2 == count)
				break;
		}

		if (i == active)	{
			/* Change it to inactive tab */
			int		clear = 0;	/* use ClearArea or FillRectangle */
#ifdef BACKGROUND_IMAGE
			if ((None != r->tabBar.pixmap) &&
				(r->Options & Opt_tabPixmap))
				clear = 1;	/* use background image */
#endif
#ifdef TRANSPARENT
			if ((r->Options & Opt_transparent) &&
				(r->Options & Opt_transparent_tabbar))
				clear = 1;	/* transparent override background image */
#endif
			if (clear)
				XClearArea (r->Xdisplay, r->tabBar.win,
					x, TAB_TOPOFF,
					PVTS(r, i)->tab_width,
					TAB_BOTOFF - TAB_TOPOFF + 1,
					/* rxvt_tabbar_rheight (r), */
					False);
			else {
				CHOOSE_GC_FG (r, r->tabBar.ibg);
				XFillRectangle  (r->Xdisplay, r->tabBar.win,
					r->tabBar.gc,
					x, TAB_TOPOFF,
					PVTS(r, i)->tab_width,
					TAB_BOTOFF - TAB_TOPOFF + 1);
					/* rxvt_tabbar_rheight (r)); */
			}

			CHOOSE_GC_FG (r, r->tabBar.frame);
			/* draw top horizontal line of the tab */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_TOPOFF, x+PVTS(r, i)->tab_width-1, TAB_TOPOFF);
			/* inactive tab has a thick top horizontal line */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_TOPOFF+1, x+PVTS(r, i)->tab_width, TAB_TOPOFF+1);

			/* draw bottom horizontal line of the tab */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_BOTOFF,
				x+PVTS(r, i)->tab_width, TAB_BOTOFF);

			/* draw left vertical frame line of the tab */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_TOPOFF,
				x, TAB_BOTOFF);

			/* draw right vertical line of the tab */
			CHOOSE_GC_FG (r, r->tabBar.delimit);
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x + PVTS(r, i)->tab_width, TAB_TOPOFF,
				x + PVTS(r, i)->tab_width, TAB_BOTOFF-1);

			/* draw the text of the inactive tab */
			CHOOSE_GC_FG (r, r->tabBar.ifg);
			draw_title (r, PVTS(r, i)->tab_title, x+TXT_XOFF, TXT_YOFF,
				0);	/* inactive tab */

			/* no need to continue loop */
			count ++;
			if (2 == count)
				break;
		}
	}

	/* Need to update tabbar buttons */
	if ((r->Options2 & Opt2_protectSecondary) &&
		(PVTS(r, active)->current_screen !=
		 PVTS(r, inactive)->current_screen))
		rxvt_tabbar_draw_buttons (r);
}


/* EXTPROTO */
void
rxvt_tabbar_highlight_tab (rxvt_t* r, int page)
{
	register int	i, x;
	int				sx, sy;
	unsigned int	rw, rh;


	if (LTAB(r) < 0)
		return ;
	if (None == r->tabBar.win)
		return;
	assert (page <= LTAB(r));
	/* highlight flag is already set, simply return */
	if (PVTS(r, page)->highlight)
		return;	
	/* set highlight flag */
	PVTS(r, page)->highlight = 1;
	if (!r->tabBar.state)
		return;

	/* Sanatization */
	assert (LTAB(r) >= 0);
	assert (FVTAB(r) >= 0);
	assert (FVTAB(r) <= LTAB(r));
	assert (LVTAB(r) >= 0);
	assert (LVTAB(r) <= LTAB(r));
	assert (ATAB(r) >= FVTAB(r));
	assert (ATAB(r) <= LVTAB(r));

	/* do not highlight invisible/active tab */
	if (page < FVTAB(r) || page > LVTAB(r) || page == ATAB(r))
		return ;

	for (i = FVTAB(r), x=TAB_BORDER; i < page; x += TAB_WIDTH(i), i++)
		;

	/* set dash-line attributes */
	XSetLineAttributes (r->Xdisplay, r->tabBar.gc, 1UL,
		LineOnOffDash, CapButt, JoinMiter);

	XSetForeground (r->Xdisplay, r->tabBar.gc, r->tabBar.ifg);
	if (!(r->Options2 & Opt2_bottomTabbar))	{
		sx = x+1;
		sy = TAB_TOPOFF+2;
		rw = PVTS(r, page)->tab_width-2;
		rh = TAB_BOTOFF-TAB_TOPOFF - 3;
	}
	else	{
		sx = x+1;
		sy = TAB_TOPOFF_BT+1;
		rw = PVTS(r, page)->tab_width-2;
		rh = rxvt_tabbar_rheight(r)-TAB_TOPOFF_BT - 3;
	}
	XDrawRectangle (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
		sx, sy, rw, rh);

	/* restore solid-line attributes */
	XSetLineAttributes (r->Xdisplay, r->tabBar.gc, 1UL,
		LineSolid, CapButt, JoinMiter);
}


/*
** Draw all visible tabs at bottom
*/
/* INTPROTO */
static void
draw_tabs_bottom (rxvt_t* r)
{
	register int	i, x;

	if (LTAB(r) < 0)
		return;
	if (None == r->tabBar.win)
		return;
	if (!r->tabBar.state)
		return;

	/* Sanatization */
	assert (LTAB(r) >= 0);
	assert (FVTAB(r) >= 0);
	assert (FVTAB(r) <= LTAB(r));
	assert (LVTAB(r) >= 0);
	assert (LVTAB(r) <= LTAB(r));
	assert (ATAB(r) >= FVTAB(r));
	assert (ATAB(r) <= LVTAB(r));


	/* redraw the inactive tabs background */
	for (i = FVTAB(r), x=TAB_BORDER;
		i <= LVTAB(r);
		x += TAB_WIDTH(i), i++) {
		int		clear = 0;	/* use ClearArea or FillRectangle */
#ifdef BACKGROUND_IMAGE
		if ((None != r->tabBar.pixmap) &&
			(r->Options & Opt_tabPixmap))
			clear = 1;	/* use background image */
#endif
#ifdef TRANSPARENT
		if ((r->Options & Opt_transparent) &&
			(r->Options & Opt_transparent_tabbar))
			clear = 1;	/* transparent override background image */
#endif
		if (!clear)	{
			if (i != ATAB(r))	{
				CHOOSE_GC_FG (r, r->tabBar.ibg);
				XFillRectangle  (r->Xdisplay, r->tabBar.win,
					r->tabBar.gc,
					x, TAB_TOPOFF_BT,
					PVTS(r, i)->tab_width,
					rxvt_tabbar_rheight (r) - TAB_TOPOFF_BT);
			}
			else	{
				CHOOSE_GC_FG (r, r->tabBar.bg);
				XFillRectangle  (r->Xdisplay, r->tabBar.win,
					r->tabBar.gc,
					x, 0,
					PVTS(r, i)->tab_width, rxvt_tabbar_rheight (r));
			}
		}
		/* else, We do not need to call ClearArea since it's 
		** already called */
	}


	CHOOSE_GC_FG (r, r->tabBar.frame);
	for (i = FVTAB(r), x=TAB_BORDER;
		i <= LVTAB(r);
		x += TAB_WIDTH(i), i++) {
		/* draw left vertical frame line of the tab */
		XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
			x, TAB_TOPOFF_BT,
			x, rxvt_tabbar_rheight(r)-1);
	}


	CHOOSE_GC_FG (r, r->tabBar.delimit);
	for (i = FVTAB(r), x=TAB_BORDER;
		i <= LVTAB(r);
		x += TAB_WIDTH(i), i++) {
		/* draw bottom horizontal line of the tab */
		XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
			x, rxvt_tabbar_rheight(r)-1,
			x+PVTS(r, i)->tab_width-1, rxvt_tabbar_rheight(r)-1);

		/* draw top horizontal line of the tab */
		if (i != ATAB(r))
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_TOPOFF_BT,
				x+PVTS(r, i)->tab_width, TAB_TOPOFF_BT);

		/* inactive tab has a thick bottom horizontal line */
		if (i != ATAB(r))
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, rxvt_tabbar_rheight(r)-2,
				x+PVTS(r, i)->tab_width, rxvt_tabbar_rheight(r)-2);

		/* draw right vertical line of the tab */
		XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
			x + PVTS(r, i)->tab_width, TAB_TOPOFF_BT,
			x + PVTS(r, i)->tab_width, rxvt_tabbar_rheight(r)-1);
	}

	/* draw top horizontal frame line after the tabs */
	XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
		x, TAB_TOPOFF_BT, TWIN_WIDTH(r), TAB_TOPOFF_BT);


	for (i = FVTAB(r), x=TAB_BORDER;
		i <= LVTAB(r);
		x += TAB_WIDTH(i), i++) {
		/* draw the text of the tab */
		CHOOSE_GC_FG (r, (i == ATAB(r)) ? r->tabBar.fg : r->tabBar.ifg);
		draw_title (r, PVTS(r, i)->tab_title, x+TXT_XOFF, TXT_YOFF,
			i == ATAB(r));
	}

}


/* INTPROTO */
static void
switch_tabs_bottom (rxvt_t* r, int active, int inactive)
{
	register int	i, x, count = 0;

	assert (LTAB(r) >= 0);
	assert (None != r->tabBar.win);
	assert (active >= FVTAB(r) && active <= LVTAB(r));
	assert (inactive >= FVTAB(r) && inactive <= LVTAB(r));
	assert (active != inactive);


	DBG_MSG(1, (stderr, "switch_tabs_top (%d, %d)\n", active, inactive));
	for (i = FVTAB(r), x=TAB_BORDER;
		i <= LVTAB(r);
		x += TAB_WIDTH(i), i++) {
		if (i == inactive)	{
			/* Change it to active tab. */
			int		clear = 0;	/* use ClearArea or FillRectangle */
#ifdef BACKGROUND_IMAGE
			if ((None != r->tabBar.pixmap) &&
				(r->Options & Opt_tabPixmap))
				clear = 1;	/* use background image */
#endif
#ifdef TRANSPARENT
			if ((r->Options & Opt_transparent) &&
				(r->Options & Opt_transparent_tabbar))
				clear = 1;	/* transparent override background image */
#endif
			if (clear)
				XClearArea (r->Xdisplay, r->tabBar.win,
					x, 0,
					PVTS(r, i)->tab_width, rxvt_tabbar_rheight (r),
					False);
			else	{
				CHOOSE_GC_FG (r, r->tabBar.bg);
				XFillRectangle (r->Xdisplay, r->tabBar.win,
					r->tabBar.gc,
					x, 0,
					PVTS(r, i)->tab_width, rxvt_tabbar_rheight (r));
			}

			/* draw left vertical frame line of the tab */
			CHOOSE_GC_FG (r, r->tabBar.frame);
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_TOPOFF_BT,
				x, rxvt_tabbar_rheight(r)-1);

			CHOOSE_GC_FG (r, r->tabBar.delimit);
			/* draw bottom horizontal line of the tab */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, rxvt_tabbar_rheight(r)-1,
				x+PVTS(r, i)->tab_width-1, rxvt_tabbar_rheight(r)-1);
			/* draw right vertical line of the tab */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x + PVTS(r, i)->tab_width, TAB_TOPOFF_BT,
				x + PVTS(r, i)->tab_width, rxvt_tabbar_rheight(r)-1);

			/* draw the text of the active tab */
			CHOOSE_GC_FG (r, r->tabBar.fg);
			draw_title (r, PVTS(r, i)->tab_title, x+TXT_XOFF, TXT_YOFF,
				1);	/* active tab */

			/* no need to continue loop */
			count ++;
			if (2 == count)
				break;
		}

		if (i == active)	{
			/* Change it to inactive tab */
			int		clear = 0;	/* use ClearArea or FillRectangle */
#ifdef BACKGROUND_IMAGE
			if ((None != r->tabBar.pixmap) &&
				(r->Options & Opt_tabPixmap))
				clear = 1;	/* use background image */
#endif
#ifdef TRANSPARENT
			if ((r->Options & Opt_transparent) &&
				(r->Options & Opt_transparent_tabbar))
				clear = 1;	/* transparent override background image */
#endif
			if (clear)
				XClearArea (r->Xdisplay, r->tabBar.win,
					x, TAB_TOPOFF_BT,
					PVTS(r, i)->tab_width,
					rxvt_tabbar_rheight (r) - TAB_TOPOFF_BT,
					False);
			else {
				CHOOSE_GC_FG (r, r->tabBar.ibg);
				XFillRectangle  (r->Xdisplay, r->tabBar.win,
					r->tabBar.gc,
					x, TAB_TOPOFF_BT,
					PVTS(r, i)->tab_width,
					rxvt_tabbar_rheight (r) - TAB_TOPOFF_BT);
			}

			CHOOSE_GC_FG (r, r->tabBar.delimit);
			/* draw bottom horizontal line of the tab */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, rxvt_tabbar_rheight (r)-1,
				x+PVTS(r, i)->tab_width-1, rxvt_tabbar_rheight (r)-1);
			/* inactive tab has a thick top horizontal line */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, rxvt_tabbar_rheight (r)-2,
				x+PVTS(r, i)->tab_width, rxvt_tabbar_rheight (r)-2);

			/* draw right vertical line of the tab */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x + PVTS(r, i)->tab_width, TAB_TOPOFF_BT,
				x + PVTS(r, i)->tab_width, rxvt_tabbar_rheight(r)-1);

			/* draw top horizontal line of the tab */
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_TOPOFF_BT,
				x+PVTS(r, i)->tab_width, TAB_TOPOFF_BT);

			/* draw left vertical frame line of the tab */
			CHOOSE_GC_FG (r, r->tabBar.frame);
			XDrawLine  (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
				x, TAB_TOPOFF_BT,
				x, rxvt_tabbar_rheight(r)-1);

			/* draw the text of the inactive tab */
			CHOOSE_GC_FG (r, r->tabBar.ifg);
			draw_title (r, PVTS(r, i)->tab_title, x+TXT_XOFF, TXT_YOFF,
				0);	/* inactive tab */

			/* no need to continue loop */
			count ++;
			if (2 == count)
				break;
		}
	}

	/* Need to update tabbar buttons */
	if ((r->Options2 & Opt2_protectSecondary) &&
		(PVTS(r, active)->current_screen !=
		 PVTS(r, inactive)->current_screen))
		rxvt_tabbar_draw_buttons (r);
}


/*
** Buttons
*/
/* EXTPROTO */
void
rxvt_tabbar_draw_buttons (rxvt_t* r)
{
	register int	i;
	int				topoff;
	unsigned long	frame;


	if (LTAB(r) < 0)
		return;
	if (None == r->tabBar.win)
		return;
	if (!r->tabBar.state)
		return;

	/* whether the buttons are hidden */
	if (r->Options2 & Opt2_hideButtons)
		return;


	topoff = !(r->Options2 & Opt2_bottomTabbar) ?
				BTN_TOPOFF : BTN_TOPOFF_BT;
	frame = !(r->Options2 & Opt2_bottomTabbar) ?
				r->tabBar.frame : r->tabBar.delimit;

	CHOOSE_GC_FG (r, r->tabBar.fg);
	for (i = NB_XPM; i >= 1; i--) {
#ifdef HAVE_LIBXPM
		register int	curimg = NB_XPM - i;

		switch (curimg)	{
		case XPM_TERM:
			img[XPM_TERM] = (LTAB(r) == MAX_PAGES - 1) ? 
				img_d[XPM_TERM] : img_e[XPM_TERM];
			break;
		case XPM_CLOSE:
			img[XPM_CLOSE] = ((r->Options2 & Opt2_protectSecondary) &&
							PRIMARY != AVTS(r)->current_screen) ?
					img_d[XPM_CLOSE] : img_e[XPM_CLOSE];
			break;
		case XPM_LEFT:
			img[XPM_LEFT] = (FVTAB(r) == 0) ? 
				img_d[XPM_LEFT] : img_e[XPM_LEFT];
			break;
		case XPM_RIGHT:
			img[XPM_RIGHT] = (LVTAB(r) == LTAB(r)) ? 
				img_d[XPM_RIGHT] : img_e[XPM_RIGHT];
			break;
		}
#endif
		if (None != img[NB_XPM-i])	{
			XCopyArea  (r->Xdisplay, img[NB_XPM-i], r->tabBar.win,
				r->tabBar.gc, 0, 0,
				BTN_WIDTH, BTN_HEIGHT,
				TWIN_WIDTH(r)-(i*(BTN_WIDTH+BTN_SPACE)), topoff);
		}
	}


	CHOOSE_GC_FG (r, r->tabBar.frame);
	for (i = NB_XPM; i >= 1; i--) {
		/*
		XDrawRectangle (r->Xdisplay, r->tabBar.win,
			r->tabBar.gc,
			TWIN_WIDTH(r)-(i*(BTN_WIDTH+BTN_SPACE)), topoff,
			BTN_WIDTH, BTN_HEIGHT);
		*/
		int		sx = TWIN_WIDTH(r) - (i*(BTN_WIDTH+BTN_SPACE));
		/* draw top line */
		XDrawLine (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
			sx, topoff, sx + BTN_WIDTH, topoff);
		/* draw left line */
		XDrawLine (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
			sx, topoff, sx, topoff + BTN_HEIGHT);
	}
	CHOOSE_GC_FG (r, r->tabBar.delimit);
	for (i = NB_XPM; i >= 1; i--) {
		int		sx = TWIN_WIDTH(r) - (i*(BTN_WIDTH+BTN_SPACE));
		/* draw bottom line */
		XDrawLine (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
			sx, topoff+BTN_HEIGHT, sx+BTN_WIDTH, topoff+BTN_HEIGHT);
		/* draw right line */
		XDrawLine (r->Xdisplay, r->tabBar.win, r->tabBar.gc,
			sx+BTN_WIDTH, topoff, sx+BTN_WIDTH, topoff+BTN_HEIGHT);
	}
}



/*
** Initialize global data structure of all tabs
*/
/* INTPROTO */
static void
init_tabbar (rxvt_t* r)
{
	r->tabBar.state = 0;	/* not mapped yet */

	LTAB(r) = -1;	/* the last tab */
	r->tabBar.atab = 0;	/* the active tab */
	FVTAB(r) = 0;	/* first visiable tab */
	LVTAB(r) = 0;	/* last visiable tab */
	r->tabBar.ptab = 0;		/* previous active tab */

	/* Make sure that font has been initialized */
#ifdef XFT_SUPPORT
	if (r->Options & Opt_xft)
		assert (NULL != r->TermWin.xftfont);
	else
#endif
	assert (None != r->TermWin.font);
	assert (r->TermWin.fheight > 0);

	/* resource string are static, needn't to free */
	r->tabBar.rsfg =
	r->tabBar.rsbg =
	r->tabBar.rsifg =
	r->tabBar.rsibg = 0;
}



/* INTPROTO */
void
rxvt_kill_page (rxvt_t* r, int page)
{
	kill (PVTS(r, page)->cmd_pid, SIGHUP);
}


/*
** Append a new tab after the last tab
*/
/* EXTPROTO */
void
rxvt_append_page (rxvt_t* r, const char TAINTED * title)
{
	int		num_fds;
	char**	argv;


	/* Sanatization */
	assert (LTAB(r) < MAX_PAGES);
	if (LTAB(r) == MAX_PAGES-1) {
		DBG_MSG(1,(stderr,"sorry, too many tabs\n"));
		return ;
	}

	DBG_MSG(1,(stderr,"append_page (%s)\n", title ? title : "nil"));

	/* indicate that we add a new tab */
	LTAB(r)++;

	rxvt_create_termwin (r, LTAB(r), title);
	DBG_MSG(1,(stderr,"last page is %d\n",LTAB(r)));

	/* load tab command */
	argv = LVTS(r)->command_argv;
	/* is the first tab command */
	if ((0 == LTAB(r)) &&
		/* no tab command input, or user has supplied command */
		(NULL == argv || NULL != cmd_argv))
		argv = cmd_argv;
	if (r->Options2 & Opt2_cmdAllTabs)	{
		/* use -e command for all tab commands */
		if (cmd_argv)
			argv = cmd_argv;
	}
	else	{
		/* if run shell for all tab commands, set argv to NULL */
		if (r->Options2 & Opt2_tabShell)
			argv = NULL;
	}
	LVTS(r)->cmd_fd = rxvt_run_command (r, LTAB(r),
						(const char**) argv);


	/* If run command failed, rollback */
	assert (-1 != LVTS(r)->cmd_fd);
	if (-1 == LVTS(r)->cmd_fd)	{
		rxvt_destroy_termwin (r, LTAB(r));
		LTAB(r) --;
		return;
	}
	DBG_MSG(2,(stderr,"page %d's cmd_fd is %d\n", LTAB(r), LVTS(r)->cmd_fd));

	/* Reduce r->num_fds so that select() is more efficient */
	num_fds = max(STDERR_FILENO, LVTS(r)->cmd_fd);
	MAX_IT(num_fds, r->Xfd);
	MAX_IT(num_fds, r->num_fds-1);
/* #ifdef __sgi */
#ifdef OS_IRIX
	/* Alex Coventry says we need 4 & 7 too */
	MAX_IT(num_fds, 7);
#endif
	r->num_fds = num_fds + 1;	/* counts from 0 */
	DBG_MSG(1, (stderr, "Adjust num_fds to %d\n", r->num_fds));


	/* Initialize the screen data structures */
	rxvt_scr_reset (r, LTAB(r));
	rxvt_scr_refresh (r, LTAB(r), FAST_REFRESH);


	/* Now we actually execute the command after executing shell,
	** but we need careful check first */
	if ((r->Options2 & Opt2_tabShell) &&
		/* the user is not using -e command for all tabs */
		!(r->Options2 & Opt2_cmdAllTabs) &&
		/* the user has specified the tab command */
		(NULL != r->h->rs[Rs_command+LTAB(r)]) &&
		/* if tab command is only used in initialization, we are not
		** after the initialization */
		!((r->Options2 & Opt2_cmdInitTabs) &&
		  (NULL == LVTS(r)->command_argv)))	{
		const char*		cmd = r->h->rs[Rs_command+LTAB(r)];

		rxvt_tt_write (r, LTAB(r), (const unsigned char*) cmd, STRLEN(cmd));
		rxvt_tt_write (r, LTAB(r), (const unsigned char*) "\n", 1);
	}


	/* Now update active page information */
	PTAB(r) = ATAB(r); /* set last previous tab */
	ATAB(r) = LTAB(r); /* set the active tab */

	/* update mapped flag */
	AVTS(r)->mapped = 1;
	/* first tab is special since ptab = atab now */
	if (PTAB(r) != ATAB(r))
		PVTS(r, r->tabBar.ptab)->mapped = 0;

	/* adjust visible tabs */
	rxvt_tabbar_set_visible_tabs (r);

	if (r->tabBar.state)	{
		/* If tabbar is mapped, refresh it */
		rxvt_tabbar_expose (r);
	}

	/* synchronize terminal title with tab title */
	if (r->Options2 & Opt2_syncTabTitle)
		rxvt_set_term_title (r, (const unsigned char*) PVTS(r, ATAB(r))->tab_title);

	/* synchronize icon name to tab title */
	if (r->Options2 & Opt2_syncTabIcon)
		rxvt_set_icon_name (r, (const unsigned char*) PVTS(r, ATAB(r))->tab_title);
}


/*
** Called by the handler of SIGCHLD; destroy the terminal and its tab
*/
/* EXTPROTO */
void
rxvt_remove_page (rxvt_t* r, int page)
{
	register int	i;


	DBG_MSG(1,(stderr,"remove_page(%d)\n", page));


	/* clean utmp/wtmp entry */
#ifdef UTMP_SUPPORT
	rxvt_privileges (RESTORE);
	rxvt_cleanutent (r, page); 
	rxvt_privileges (IGNORE);
#endif

	/* free virtual terminal related resources */
	assert (PVTS(r, page)->ttydev);
	free (PVTS(r, page)->ttydev);
	assert (PVTS(r, page)->cmd_fd >= 0);
	close (PVTS(r, page)->cmd_fd);
	if (PVTS(r, page)->v_buffer)	{
		free (PVTS(r, page)->v_buffer);
		PVTS(r, page)->v_buffer = NULL;
	}

	/* to adjust num_fds if necessary */
	if (PVTS(r, page)->cmd_fd == r->num_fds-1)	{
		r->num_fds --;
		DBG_MSG(1, (stderr, "Adjust num_fds to %d\n", r->num_fds));
	}

	/* free screen structure */
	rxvt_scr_release (r, page);

	/* destroy the virtual terminal window */
	rxvt_destroy_termwin (r, page);

	/* quit the last the terminal, exit the application */
	if (LTAB(r) == 0)	{
		rxvt_clean_exit (r);
	}

	/* update TermWin and tab_widths */
	for (i = page; i < LTAB(r); i++)
		PVTS(r, i) = PVTS(r, i+1);

	/* update total number of tabs */
	LTAB(r)--;

	/* update selection */
	if (page == r->selection.vt)
		rxvt_process_selectionclear (r, page);
	else if (r->selection.vt > page)
		r->selection.vt --;

	/*
	** Now we try to set correct atab, ptab, fvtab, and lvtab
	** Must be careful here!!!
	*/
	/* update previous active tab */
	if (PTAB(r) > LTAB(r))	/* in case PTAB is invalid */
		PTAB(r) = 0;
	else if (PTAB(r) > page)
		PTAB(r)--;
	/* update active tab */
	if (ATAB(r) >= page) {
		/* if ATAB(r) == page == 0, do not update ATAB(r) */
		if (!(ATAB(r) == page && 0 == page))
			ATAB(r)--;
	}

	/* always set mapped flag */
	AVTS(r)->mapped = 1;

	/* adjust visible tabs */
	rxvt_tabbar_set_visible_tabs (r);
	/* redraw the tabs and buttons */
	if (r->tabBar.state)
		rxvt_tabbar_expose (r);

	/* Switch fg/bg colors */
	rxvt_switch_fgbg_color (r, ATAB(r));
	XMapRaised  (r->Xdisplay, AVTS(r)->vt);
	rxvt_scr_touch (r, ATAB(r), True);

	/* synchronize terminal title with tab title */
	if (r->Options2 & Opt2_syncTabTitle)
		rxvt_set_term_title (r, (const unsigned char*) PVTS(r, ATAB(r))->tab_title);

	/* synchronize icon name to tab title */
	if (r->Options2 & Opt2_syncTabIcon)
		rxvt_set_icon_name(r, (const unsigned char*) PVTS(r, ATAB(r))->tab_title);
}


/*
** Set new title for a tab
*/
/* EXTPROTO */
void
rxvt_tabbar_set_title (rxvt_t* r, int page, const unsigned char TAINTED * str)
{
	char UNTAINTED *		n_title;

	assert (str);
	assert (page >= 0 && page <= LTAB(r));
	assert (PVTS(r, page)->tab_title);

	n_title = STRNDUP (str, MAX_TAB_TXT);
	/*
	** If strdup succeeds, set new title
	*/
	if (NULL != n_title)	{
		free (PVTS(r, page)->tab_title);
		PVTS(r, page)->tab_title = n_title;

		/* Compute the new width of the tab */
		PVTS(r, page)->tab_width = rxvt_tab_width (r, n_title);
	}

	/*
	** If visible tab's title is changed, refresh tab bar
	*/
	if (page >= FVTAB(r) && page <= LVTAB(r))	{
		/* adjust visible tabs */
		rxvt_tabbar_set_visible_tabs (r);
		/* Now it's ready to redraw the tabs */
		rxvt_tabbar_expose (r);
	}

	/* synchronize terminal title with active tab title */
	if ((r->Options2 & Opt2_syncTabTitle) &&
		(page == ATAB(r)))
		rxvt_set_term_title (r, (const unsigned char*) PVTS(r, ATAB(r))->tab_title);

	/* synchronize icon name to tab title */
	if ((r->Options2 & Opt2_syncTabIcon) &&
		(page == ATAB(r)))
		rxvt_set_icon_name(r, (const unsigned char*) PVTS(r, ATAB(r))->tab_title);
}


/*
** Activate a page terminal
*/
/* EXTPROTO */
void
rxvt_activate_page (rxvt_t* r, int index)
{
	int		fvtab, lvtab, old_atab;


	/* shortcut */
	if (/* !r->tabBar.state ||
		None == r->tabBar.win || */
		index == ATAB(r))
		return;

	/* save the old first/last visible tab */
	fvtab = FVTAB(r);
	lvtab = LVTAB(r);
	old_atab  = r->tabBar.atab;	/* old active tab */

	AVTS(r)->mapped = 0;
	r->tabBar.ptab = ATAB(r);
	ATAB(r) = index;
	AVTS(r)->mapped = 1;
	AVTS(r)->highlight = 0;	/* clear highlight flag */
	
	/*
	** Now the visible tabs may be changed, recompute the visible
	** tabs before redrawing.
	*/
	if (index < FVTAB(r) || index > LVTAB(r))	{
		/* adjust visible tabs */
		rxvt_tabbar_set_visible_tabs (r);
	}


	if (fvtab != FVTAB(r) || lvtab != LVTAB(r))
		/* redraw both the tabs and buttons */
		rxvt_tabbar_expose (r);
	else	{
		/* this avoid refresh the entire tabbar */
		if (!(r->Options2 & Opt2_bottomTabbar))
			switch_tabs_top (r, old_atab, r->tabBar.atab);
		else
			switch_tabs_bottom (r, old_atab, r->tabBar.atab);
	}


	/* Switch VT fg/bg colors */
	rxvt_switch_fgbg_color (r, ATAB(r));
	XMapRaised  (r->Xdisplay, AVTS(r)->vt);
	rxvt_scr_touch (r, ATAB(r), True);
	DBG_MSG(1,(stderr,"active page is %d\n",ATAB(r)));

	/* synchronize terminal title with tab title */
	if (r->Options2 & Opt2_syncTabTitle)
		rxvt_set_term_title (r, (const unsigned char*) PVTS(r, ATAB(r))->tab_title);

	/* synchronize icon name to tab title */
	if (r->Options2 & Opt2_syncTabIcon)
		rxvt_set_icon_name(r, (const unsigned char*) PVTS(r, ATAB(r))->tab_title);
}


/*
** Change the width of the tab bar
*/
/* EXTPROTO */
void
rxvt_tabbar_resize (rxvt_t* r)
{
	register int	i;
	int				sx, sy;


	sx = 0;
	sy = 0;
#if defined(HAVE_MENUBAR) && (MENUBAR_MAX > 0)
	sy += rxvt_menubar_height (r);
#endif
	if (r->Options2 & Opt2_bottomTabbar)
		sy += VT_HEIGHT(r);
	XMoveResizeWindow  (r->Xdisplay, r->tabBar.win,
		sx, sy, TWIN_WIDTH(r), rxvt_tabbar_rheight (r));

	/* recompute width of each tab */
	for (i = 0; i <= LTAB(r); i ++)
		PVTS(r, i)->tab_width = rxvt_tab_width (r, PVTS(r, i)->tab_title);

	/* adjust visible tabs */
	rxvt_tabbar_set_visible_tabs (r);
	/* redraw the tabs and buttons */
	rxvt_tabbar_expose (r);
}


/*
** Determine the position of pointer click and dispatch the event
*/
/* EXTPROTO */
void
rxvt_tabbar_dispatcher (rxvt_t* r, XButtonEvent* ev)
{
	register int	x, y, z, but;


	DBG_MSG(2,(stderr,"click in (%d,%d)\n",x,y));
	x = ev->x;
	y = ev->y;
	but = -1;

	/* let's decode where the user click */
	z = TWIN_WIDTH(r) - x;
	if (!(r->Options2 & Opt2_hideButtons) &&
		z < 4*(BTN_WIDTH+BTN_SPACE) &&
		(z%(BTN_WIDTH+BTN_SPACE)) > BTN_SPACE) {
		but = z/(BTN_WIDTH+BTN_SPACE);
		DBG_MSG(1,(stderr,"click on button %d\n",but));
		switch(but) {
		case 0 : /* right shift */
			if (r->tabBar.atab < LTAB(r))
				rxvt_activate_page (r, r->tabBar.atab+1);
			break;
		case 1 : /* left shift */
			if (r->tabBar.atab > 0)
				rxvt_activate_page (r, r->tabBar.atab-1);
			break;
		case 2 : /* delete the active vt if it's in primary screen */
			if (!(r->Options2 & Opt2_protectSecondary) ||
				((r->Options2 & Opt2_protectSecondary) &&
				 (PRIMARY == AVTS(r)->current_screen)))
				rxvt_kill_page (r, ATAB(r));
			break;
		case 3 : /* create a new vt*/
			rxvt_append_page (r, NULL);
			break;
		default :
			break;
		}
	}
	else if ( x < TAB_SPACE && LTAB(r) >= 0) {
		register int	w = 0;
		register int	i;
		for (i = FVTAB(r);
			w < x && i <= LVTAB(r); i++) {
			w += TAB_WIDTH(i);
		}

		if (w - TAB_BORDER >= x) {
			but = i - 1;

			DBG_MSG(2,(stderr,"click on tab %d\n",but));
			switch (ev->button)	{
			case Button1:
			default:
				/* activate the selected tab */
				rxvt_activate_page (r, but);
				break;

			case Button2:
				/* change tab title on middle click */
				if (NULL != r->selection.text)
					rxvt_tabbar_set_title (r, but, r->selection.text);
				break;
			}
		}
		else	{
			/* change tab title of active tab on middle click */
			if ((Button2 == ev->button) && (NULL != r->selection.text))
				rxvt_tabbar_set_title (r, ATAB(r), r->selection.text);
		}
	}
}


/*
** Is the tabbar visible
*/
/* EXTPROTO */
int
rxvt_tabbar_visible (rxvt_t* r)
{
	return (None != r->tabBar.win && r->tabBar.state);
}


/*
** Expose handler for tabbar
*/
/* EXTPROTO */
void
rxvt_tabbar_expose (rxvt_t* r)
{
	XClearWindow (r->Xdisplay, r->tabBar.win);

	/* draw the tabs and blank space*/
	if (!(r->Options2 & Opt2_bottomTabbar))
		draw_tabs_top (r);
	else
		draw_tabs_bottom (r);
	/* draw the buttons */
	rxvt_tabbar_draw_buttons (r);
}



/*
** Hide the tabbar
*/
/* EXTPROTO */
int
rxvt_tabbar_hide (rxvt_t* r)
{
	int		changed = 0;

	assert (None != r->tabBar.win);
	changed = r->tabBar.state;
	XUnmapWindow  (r->Xdisplay, r->tabBar.win);
	r->tabBar.state = 0;

	return (changed);
}



/*
** Show the tabbar
*/
/* EXTPROTO */
int
rxvt_tabbar_show (rxvt_t* r)
{
	int		changed = 0;

	assert (None != r->tabBar.win);
	changed = !r->tabBar.state;
	XMapWindow  (r->Xdisplay, r->tabBar.win);
	r->tabBar.state = 1;

	return (changed);
}



/*
** Create the tab bar window
*/
/* EXTPROTO */
void
rxvt_tabbar_create (rxvt_t* r)
{
	XColor			color;
	XGCValues		gcvalue;
	unsigned long	gcmask;
	register int	i;
	int				sx, sy;
#ifdef HAVE_LIBXPM
	XpmAttributes	xpm_attr;
	/*
	** Make sure symbol `background' exists in all .xpm files!
	** This elimate the background color so that the buttons
	** look transparent.
	*/
	XpmColorSymbol	xpm_color_sym = {"background", NULL, 0};
#endif


	init_tabbar (r);
	DBG_MSG(1,(stderr,"Creating tabbar\n"));


	/* initialize the colors */
	if (XDEPTH <= 2)	{
		r->tabBar.fg = r->PixColors[Color_fg];
		r->tabBar.bg = r->PixColors[Color_bg];
		r->tabBar.ifg = r->PixColors[Color_fg];
		r->tabBar.ibg = r->PixColors[Color_bg];
		r->tabBar.frame = r->PixColors[Color_bg];
		r->tabBar.delimit = r->PixColors[Color_fg];
	}
	else 	{
		/* create the foreground color */
		if (r->h->rs[Rs_tabfg] && 
			rxvt_parse_alloc_color (r, &color, r->h->rs[Rs_tabfg]))
			r->tabBar.fg = color.pixel;
		else
			r->tabBar.fg = r->PixColors[Color_Black];

		/* create the background color */
		if (r->h->rs[Rs_tabbg]	&&
			rxvt_parse_alloc_color (r, &color, r->h->rs[Rs_tabbg]))
			r->tabBar.bg = color.pixel;
		else	{
			color.red = 0xd300;
			color.green = 0xd300;
			color.blue = 0xdd00;
			if (rxvt_alloc_color (r, &color, "Active_Tab"))
				r->tabBar.bg = color.pixel;
			else
				r->tabBar.bg = r->PixColors[Color_bg];
		}

		/* create the tab frame color */
		r->tabBar.frame = WhitePixelOfScreen (DefaultScreenOfDisplay (r->Xdisplay));

		/* create the inactive tab foreground color */
		if (r->h->rs[Rs_itabfg] &&
			rxvt_parse_alloc_color (r, &color, r->h->rs[Rs_itabfg]))
			r->tabBar.ifg = color.pixel;
		else
			r->tabBar.ifg = r->PixColors[Color_Black];

		/* create the inactive tab background color */
		if (r->h->rs[Rs_itabbg] &&
			rxvt_parse_alloc_color (r, &color, r->h->rs[Rs_itabbg]))
			r->tabBar.ibg = color.pixel;
		else	{
			color.red = 0xa100;
			color.green = 0xa100;
			color.blue = 0xac00;
			if (rxvt_alloc_color (r, &color, "Inactive_Tab_Bg"))
				r->tabBar.ibg = color.pixel;
			else
				r->tabBar.ibg = r->PixColors[Color_bg];
		}

		/* create the delimit color */
		color.red = 0x5000;
		color.green = 0x5000;
		color.blue = 0x5000;
		if (rxvt_alloc_color (r, &color, "Tab_Delimit"))
			r->tabBar.delimit = color.pixel;
		else
			r->tabBar.delimit = r->PixColors[Color_fg];
	}

#ifdef XFT_SUPPORT
	if (r->Options & Opt_xft)	{
		rxvt_alloc_xft_color (r, r->tabBar.fg, &(r->tabBar.xftfg));
		rxvt_alloc_xft_color (r, r->tabBar.ifg, &(r->tabBar.xftifg));
	}
#endif


	sx = 0;
	sy = 0;
#if defined(HAVE_MENUBAR) && (MENUBAR_MAX > 0)
	sy += rxvt_menubar_height (r);
#endif
	if (r->Options2 & Opt2_bottomTabbar)
		sy += VT_HEIGHT(r);
	/* create the window of the tabbar */
	r->tabBar.win = XCreateSimpleWindow(
		r->Xdisplay, r->TermWin.parent,
		sx, sy, TWIN_WIDTH(r), rxvt_tabbar_rheight (r),
		0, 0, r->tabBar.bg);
	assert (None != r->tabBar.win);

#ifdef XFT_SUPPORT
	if (r->Options & Opt_xft)	{
		r->tabBar.xftwin = XftDrawCreate (r->Xdisplay, r->tabBar.win,
								XVISUAL, XCMAP);
	}
#endif

#ifdef DEBUG_X
	rxvt_set_win_title (r, r->tabBar.win, "tabbar");
#endif


#ifdef BACKGROUND_IMAGE
	r->tabBar.pixmap = None;	/* initialize it to None */
# ifdef TRANSPARENT
	/* transparent can override background image */
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_tabbar)))
# endif
	if (r->h->rs[Rs_tabbarPixmap])	{
		long	w = 0, h = 0;
		r->tabBar.pixmap = rxvt_load_pixmap (r,
							r->h->rs[Rs_tabbarPixmap], &w, &h);
		if (None != r->tabBar.pixmap)
			XSetWindowBackgroundPixmap (r->Xdisplay, r->tabBar.win,
				r->tabBar.pixmap);
	}
#endif

#ifdef TRANSPARENT
	if ((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_tabbar))	{
		XSetWindowBackgroundPixmap (r->Xdisplay, r->tabBar.win,
			ParentRelative);
	}
#endif


	/* create the GC for the tab window */
	gcvalue.foreground = r->tabBar.fg;
#ifdef TRANSPARENT
	/* set background color when there's no transparent */
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_tabbar)))
#endif
#ifdef BACKGROUND_IMAGE
	/* set background color when there's no bg image */
	if (None == r->tabBar.pixmap)
#endif
	gcvalue.background = r->tabBar.bg;
	gcvalue.line_width = 1;
	gcmask = GCForeground | GCLineWidth;
#ifdef TRANSPARENT
	/* set background color when there's no transparent */
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_tabbar)))
#endif
#ifdef BACKGROUND_IMAGE
	/* set background color when there's no bg image */
	if (None == r->tabBar.pixmap)
#endif
	gcmask |= GCBackground;
	r->tabBar.gc = XCreateGC (r->Xdisplay, r->tabBar.win,
		gcmask, &gcvalue);
	assert (None != r->tabBar.gc);


	XDefineCursor (r->Xdisplay, r->tabBar.win, r->h->bar_pointer);
	XSelectInput (r->Xdisplay, r->tabBar.win,
		ExposureMask|ButtonPressMask|ButtonReleaseMask);

#ifdef XFT_SUPPORT
	if (!(r->Options & Opt_xft))
#endif
	XSetFont (r->Xdisplay, r->tabBar.gc, r->TermWin.font->fid);


#ifdef HAVE_LIBXPM
	xpm_color_sym.pixel = r->tabBar.bg;
	xpm_attr.colorsymbols = &xpm_color_sym;
	xpm_attr.numsymbols = 1;
	xpm_attr.visual = XVISUAL;
	xpm_attr.colormap = XCMAP;
	xpm_attr.depth = XDEPTH;
	xpm_attr.closeness = 65535;
	xpm_attr.valuemask = XpmVisual | XpmColormap | XpmDepth |
		XpmCloseness | XpmReturnPixels | XpmColorSymbols;
#endif

	/* now, create the buttons */
	for (i = 0; i < NB_XPM; i++) {
#ifdef HAVE_LIBXPM
		XpmCreatePixmapFromData (r->Xdisplay, r->tabBar.win,
			xpm_name[i], &img_e[i], &img_emask[i], &xpm_attr);
		assert (None != img_e[i]);
		XpmCreatePixmapFromData (r->Xdisplay, r->tabBar.win,
			xpm_d_name[i], &img_d[i], &img_dmask[i], &xpm_attr);
		assert (None != img_d[i]);
#else
		img[i] = XCreatePixmapFromBitmapData (r->Xdisplay,
			r->tabBar.win, xpm_name[i], BTN_WIDTH, BTN_HEIGHT,
			r->tabBar.fg, r->tabBar.bg, XDEPTH);
		assert (None != img[i]);
#endif
	}
}


/*
** Create the tab bar window
*/
/* EXTPROTO */
void
rxvt_tabbar_clean_exit (rxvt_t* r)
{
	register int	i;


	r->tabBar.win = None;	/* destroyed by XDestroySubwindows */

	/* free resource strings */
	if (r->tabBar.rsfg)
		free ((void*) r->h->rs[Rs_tabfg]);
	if (r->tabBar.rsbg)
		free ((void*) r->h->rs[Rs_tabbg]);
	if (r->tabBar.rsifg)
		free ((void*) r->h->rs[Rs_itabfg]);
	if (r->tabBar.rsibg)
		free ((void*) r->h->rs[Rs_itabbg]);

	if (None != r->tabBar.gc)	{
		XFreeGC (r->Xdisplay, r->tabBar.gc);
		r->tabBar.gc = None;
	}

#ifdef BACKGROUND_IMAGE
	if (None != r->tabBar.pixmap)	{
		XFreePixmap (r->Xdisplay, r->tabBar.pixmap);
		r->tabBar.pixmap = None;
	}
#endif

	for (i = 0; i < NB_XPM; i ++)	{
#ifdef HAVE_LIBXPM
		if (None != img_e[i])	{
			XFreePixmap (r->Xdisplay, img_e[i]);
			img_e[i] = None;
		}
		if (None != img_emask[i])	{
			XFreePixmap (r->Xdisplay, img_emask[i]);
			img_emask[i] = None;
		}
		if (None != img_d[i])	{
			XFreePixmap (r->Xdisplay, img_d[i]);
			img_d[i] = None;
		}
		if (None != img_dmask[i])	{
			XFreePixmap (r->Xdisplay, img_dmask[i]);
			img_dmask[i] = None;
		}
#else
		if (None != img[i])
			XFreePixmap (r->Xdisplay, img[i]);
#endif
		img[i] = None;
	}	/* for */
}


/* EXTPROTO */
unsigned short
rxvt_tabbar_height (rxvt_t* r)
{
	if (None == r->tabBar.win || !r->tabBar.state)
		return 0;
	return (rxvt_tabbar_rheight(r));
}


/* EXTPROTO */
unsigned short
rxvt_tabbar_rheight (rxvt_t* r)
{
	return (r->TermWin.fheight + 2*TXT_MARGIN + 2*TAB_BORDER);
}



/* EXTPROTO */
unsigned int
rxvt_tab_width (rxvt_t* r, const char* str)
{
	int		len;

	assert (str);
	len = STRLEN (str);
	if (len > MAX_DISPLAY_TAB_TXT)
		len = MAX_DISPLAY_TAB_TXT;
#ifdef XFT_SUPPORT
	if ((r->Options & Opt_xft) && (NULL != r->tabBar.xftwin))	{
		return (2 * TXT_XOFF + Width2Pixel(len));
	}
	else
#endif	/* XFT_SUPPORT */
	return (2 * TXT_XOFF + XTextWidth (r->TermWin.font, str, len));
}


/* EXTPROTO */
int
rxvt_is_tabbar_win (rxvt_t* r, Window w)
{
	return (w == r->tabBar.win);
}


/* EXTPROTO */
void
rxvt_tabbar_change_color (rxvt_t* r, int item, const char* str)
{
	XColor		xcol;
	int			changed = 0;


	switch (item)	{
	case Xterm_tabfg:
		if (r->h->rs[Rs_tabfg] &&
			!STRCASECMP(str, r->h->rs[Rs_tabfg]))
			break;	/* no color change */
		if (rxvt_parse_alloc_color (r, &xcol, str))	{
			r->tabBar.fg = xcol.pixel;
#ifdef XFT_SUPPORT
			rxvt_alloc_xft_color (r, xcol.pixel, &(r->tabBar.xftfg));
#endif
			if (r->tabBar.rsfg)		/* free previous string */
				free ((void*) r->h->rs[Rs_tabfg]);
			r->h->rs[Rs_tabfg] = STRDUP(str);
			r->tabBar.rsfg = 1;		/* free resource string later */
			changed = 1;
		}
		break;

	case Xterm_tabbg:
		if (r->h->rs[Rs_tabbg] &&
			!STRCASECMP(str, r->h->rs[Rs_tabbg]))
			break;	/* no color change */
		if (rxvt_parse_alloc_color (r, &xcol, str))	{
			r->tabBar.bg = xcol.pixel;
			if (r->tabBar.rsbg)		/* free previous string */
				free ((void*) r->h->rs[Rs_tabbg]);
			r->h->rs[Rs_tabbg] = STRDUP(str);
			r->tabBar.rsbg = 1;		/* free resource string later */
			changed = 1;
		}
		break;

	case Xterm_itabfg:
		if (r->h->rs[Rs_itabfg] &&
			!STRCASECMP(str, r->h->rs[Rs_itabfg]))
			break;	/* no color change */
		if (rxvt_parse_alloc_color (r, &xcol, str))	{
			r->tabBar.ifg = xcol.pixel;
#ifdef XFT_SUPPORT
			rxvt_alloc_xft_color (r, xcol.pixel, &(r->tabBar.xftifg));
#endif
			if (r->tabBar.rsifg)	/* free previous string */
				free ((void*) r->h->rs[Rs_itabfg]);
			r->h->rs[Rs_itabfg] = STRDUP(str);
			r->tabBar.rsifg = 1;	/* free resource string later */
			changed = 1;
		}
		break;

	case Xterm_itabbg:
		if (r->h->rs[Rs_itabbg] &&
			!STRCASECMP(str, r->h->rs[Rs_itabbg]))
			break;
		if (rxvt_parse_alloc_color (r, &xcol, str))	{
			r->tabBar.ibg = xcol.pixel;
			if (r->tabBar.rsibg)	/* free previous string */
				free ((void*) r->h->rs[Rs_itabbg]);
			r->h->rs[Rs_itabbg] = STRDUP(str);
			r->tabBar.rsibg = 1;	/* free resource string later */
			changed = 1;
		}
		break;
	
	default:
		break;
	}

	if (changed)	{
		if (Xterm_tabbg == item)
#ifdef TRANSPARENT
		/* set background color when there's no transparent */
		if (!((r->Options & Opt_transparent) &&
			(r->Options & Opt_transparent_tabbar)))
#endif
#ifdef BACKGROUND_IMAGE
		/* set background color when there's no bg image */
		if (None == r->tabBar.pixmap)
#endif
			XSetWindowBackground (r->Xdisplay, r->tabBar.win,
				r->tabBar.bg);

		rxvt_tabbar_expose (r);	/* refresh tabbar */
	}
}

/* EXTPROTO */
void
rxvt_tabbar_move_tab (rxvt_t* r, int to_right)
{
	int		cur_page = ATAB(r);
	int		new_page;

	/* shortcut, needn't do anything if there's only one tab */
	if (0 == LTAB(r))
		return;
	/* move the last page to right, do nothing */
	if (to_right && cur_page == LTAB(r))
		return;
	/* move the first page to left, do nothing */
	if (!to_right && cur_page == 0)
		return;

	new_page = to_right? cur_page+1 : cur_page-1;
	SWAP_IT (r->vts[cur_page], r->vts[new_page], term_t*);

	/* adjust active tab */
	ATAB(r) = new_page;
	/* adjust previous active tab */
	if (PTAB(r) == new_page)
		PTAB(r) = cur_page;
	if (new_page < FVTAB(r) || new_page > LVTAB(r))	{
		rxvt_tabbar_set_visible_tabs (r);
	}

	/* refresh tabbar */
	rxvt_tabbar_expose (r);
}

/*----------------------- end-of-file (C source) -----------------------*/
