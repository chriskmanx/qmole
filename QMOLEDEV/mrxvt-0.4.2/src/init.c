/*--------------------------------*-C-*---------------------------------*
 * File:	init.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1992       John Bovey <jdb@ukc.ac.uk>
 * Copyright (c) 1994       Robert Nation <nation@rocket.sanders.lockheed.com>
 * Copyright (c) 1998-2001  Geoff Wing <gcw@pobox.com>
 * Copyright (c) 1999       D J Hawkey Jr <hawkeyd@visi.com>
 * Copyright (c) 2003       marcus at #fluxbox on freenode.net
 * Copyright (c) 2004       Mr. Dobalina <losermcloser@users.sourceforge.net>
 * Copyright (c) 2003-2004	Marc Lehmann <pcg@goof.com>
 * Copyright (c) 2004-2005  Jingmin Zhou <jimmyzhou@users.sourceforge.net>
 * Copyright (c) 2005       Gautam Iyer <gautam@math.uchicago.edu>
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
** $Id: init.c,v 1.165 2005/10/08 01:07:45 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"


#ifdef DEBUG_VERBOSE
# define DEBUG_LEVEL	1
# define DEBUG_X
#else
# define DEBUG_LEVEL	0
# undef DEBUG_X
#endif

#if DEBUG_LEVEL
# define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
# define DBG_MSG(d,x)
#endif

/* #define XTERM_REVERSE_VIDEO 1 */


/*--------------------------------------------------------------------*
 *         BEGIN `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/
#if defined(OS_SVR4) && !defined(_POSIX_VERSION)
int    rxvt_getdtablesize     ();
#endif
int    rxvt_xerror_handler    (const Display*, const XErrorEvent*);
void   rxvt_init_colors       (rxvt_t*);
void   rxvt_init_win_size     (rxvt_t*);
char** rxvt_string_to_argv    (char*, int*);
void   rxvt_color_aliases     (rxvt_t*, int);
void   rxvt_get_ourmods       (rxvt_t*);
int    rxvt_run_child         (rxvt_t*, int, const char**);
void   rxvt_get_ttymode       (ttymode_t*, int);
/*--------------------------------------------------------------------*
 *         END   `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/



const char *const def_colorName[] = {
	COLOR_FOREGROUND,
	COLOR_BACKGROUND,
/* low-intensity colors */
	"black",			/* 0: black			(#000000) */
#ifndef NO_BRIGHTCOLOR
	"red3",			/* 1: red				(#CD0000) */
	"green3",			/* 2: green			(#00CD00) */
	"yellow3",			/* 3: yellow			(#CDCD00) */
	"blue3",			/* 4: blue				(#0000CD) */
	"magenta3",			/* 5: magenta			(#CD00CD) */
	"cyan3",			/* 6: cyan				(#00CDCD) */
# ifdef XTERM_COLORS
	"grey90",			/* 7: white			(#E5E5E5) */
# else
	"antiquewhite",		/* 7: white			(#FAEBD7) */
# endif
/* high-intensity colors */
# ifdef XTERM_COLORS
	"grey30",			/* 8: bright black		(#4D4D4D) */
# else
	"grey25",			/* 8: bright black		(#404040) */
# endif
#endif				/* NO_BRIGHTCOLOR */
	"red",			/* 1/9: bright red		(#FF0000) */
	"green",			/* 2/10: bright green   (#00FF00) */
	"yellow",			/* 3/11: bright yellow  (#FFFF00) */
	"blue",			/* 4/12: bright blue	(#0000FF) */
	"magenta",			/* 5/13: bright magenta (#FF00FF) */
	"cyan",			/* 6/14: bright cyan	(#00FFFF) */
	"white",			/* 7/15: bright white   (#FFFFFF) */
#ifdef TTY_256COLOR
# ifdef XTERM_COLORS
	/* use the same color cube as xterm. 16-231 is a more or less uniform
	 * rgb ramp, and 231-255 is a greyscale ramp */
	"rgb:00/00/00",		/* default ff6-255 color table	 */
	"rgb:00/00/5f",		/* consists of 6 6x6 colour cubes */
	"rgb:00/00/87",		/* and a 24 greyscale ramp w/o	*/
	"rgb:00/00/af",		/* black or white			*/
	"rgb:00/00/d7",
	"rgb:00/00/ff",
	"rgb:00/5f/00",
	"rgb:00/5f/5f",
	"rgb:00/5f/87",
	"rgb:00/5f/af",
	"rgb:00/5f/d7",
	"rgb:00/5f/ff",
	"rgb:00/87/00",
	"rgb:00/87/5f",
	"rgb:00/87/87",
	"rgb:00/87/af",
	"rgb:00/87/d7",
	"rgb:00/87/ff",
	"rgb:00/af/00",
	"rgb:00/af/5f",
	"rgb:00/af/87",
	"rgb:00/af/af",
	"rgb:00/af/d7",
	"rgb:00/af/ff",
	"rgb:00/d7/00",
	"rgb:00/d7/5f",
	"rgb:00/d7/87",
	"rgb:00/d7/af",
	"rgb:00/d7/d7",
	"rgb:00/d7/ff",
	"rgb:00/ff/00",
	"rgb:00/ff/5f",
	"rgb:00/ff/87",
	"rgb:00/ff/af",
	"rgb:00/ff/d7",
	"rgb:00/ff/ff",
	"rgb:5f/00/00",
	"rgb:5f/00/5f",
	"rgb:5f/00/87",
	"rgb:5f/00/af",
	"rgb:5f/00/d7",
	"rgb:5f/00/ff",
	"rgb:5f/5f/00",
	"rgb:5f/5f/5f",
	"rgb:5f/5f/87",
	"rgb:5f/5f/af",
	"rgb:5f/5f/d7",
	"rgb:5f/5f/ff",
	"rgb:5f/87/00",
	"rgb:5f/87/5f",
	"rgb:5f/87/87",
	"rgb:5f/87/af",
	"rgb:5f/87/d7",
	"rgb:5f/87/ff",
	"rgb:5f/af/00",
	"rgb:5f/af/5f",
	"rgb:5f/af/87",
	"rgb:5f/af/af",
	"rgb:5f/af/d7",
	"rgb:5f/af/ff",
	"rgb:5f/d7/00",
	"rgb:5f/d7/5f",
	"rgb:5f/d7/87",
	"rgb:5f/d7/af",
	"rgb:5f/d7/d7",
	"rgb:5f/d7/ff",
	"rgb:5f/ff/00",
	"rgb:5f/ff/5f",
	"rgb:5f/ff/87",
	"rgb:5f/ff/af",
	"rgb:5f/ff/d7",
	"rgb:5f/ff/ff",
	"rgb:87/00/00",
	"rgb:87/00/5f",
	"rgb:87/00/87",
	"rgb:87/00/af",
	"rgb:87/00/d7",
	"rgb:87/00/ff",
	"rgb:87/5f/00",
	"rgb:87/5f/5f",
	"rgb:87/5f/87",
	"rgb:87/5f/af",
	"rgb:87/5f/d7",
	"rgb:87/5f/ff",
	"rgb:87/87/00",
	"rgb:87/87/5f",
	"rgb:87/87/87",
	"rgb:87/87/af",
	"rgb:87/87/d7",
	"rgb:87/87/ff",
	"rgb:87/af/00",
	"rgb:87/af/5f",
	"rgb:87/af/87",
	"rgb:87/af/af",
	"rgb:87/af/d7",
	"rgb:87/af/ff",
	"rgb:87/d7/00",
	"rgb:87/d7/5f",
	"rgb:87/d7/87",
	"rgb:87/d7/af",
	"rgb:87/d7/d7",
	"rgb:87/d7/ff",
	"rgb:87/ff/00",
	"rgb:87/ff/5f",
	"rgb:87/ff/87",
	"rgb:87/ff/af",
	"rgb:87/ff/d7",
	"rgb:87/ff/ff",
	"rgb:af/00/00",
	"rgb:af/00/5f",
	"rgb:af/00/87",
	"rgb:af/00/af",
	"rgb:af/00/d7",
	"rgb:af/00/ff",
	"rgb:af/5f/00",
	"rgb:af/5f/5f",
	"rgb:af/5f/87",
	"rgb:af/5f/af",
	"rgb:af/5f/d7",
	"rgb:af/5f/ff",
	"rgb:af/87/00",
	"rgb:af/87/5f",
	"rgb:af/87/87",
	"rgb:af/87/af",
	"rgb:af/87/d7",
	"rgb:af/87/ff",
	"rgb:af/af/00",
	"rgb:af/af/5f",
	"rgb:af/af/87",
	"rgb:af/af/af",
	"rgb:af/af/d7",
	"rgb:af/af/ff",
	"rgb:af/d7/00",
	"rgb:af/d7/5f",
	"rgb:af/d7/87",
	"rgb:af/d7/af",
	"rgb:af/d7/d7",
	"rgb:af/d7/ff",
	"rgb:af/ff/00",
	"rgb:af/ff/5f",
	"rgb:af/ff/87",
	"rgb:af/ff/af",
	"rgb:af/ff/d7",
	"rgb:af/ff/ff",
	"rgb:d7/00/00",
	"rgb:d7/00/5f",
	"rgb:d7/00/87",
	"rgb:d7/00/af",
	"rgb:d7/00/d7",
	"rgb:d7/00/ff",
	"rgb:d7/5f/00",
	"rgb:d7/5f/5f",
	"rgb:d7/5f/87",
	"rgb:d7/5f/af",
	"rgb:d7/5f/d7",
	"rgb:d7/5f/ff",
	"rgb:d7/87/00",
	"rgb:d7/87/5f",
	"rgb:d7/87/87",
	"rgb:d7/87/af",
	"rgb:d7/87/d7",
	"rgb:d7/87/ff",
	"rgb:d7/af/00",
	"rgb:d7/af/5f",
	"rgb:d7/af/87",
	"rgb:d7/af/af",
	"rgb:d7/af/d7",
	"rgb:d7/af/ff",
	"rgb:d7/d7/00",
	"rgb:d7/d7/5f",
	"rgb:d7/d7/87",
	"rgb:d7/d7/af",
	"rgb:d7/d7/d7",
	"rgb:d7/d7/ff",
	"rgb:d7/ff/00",
	"rgb:d7/ff/5f",
	"rgb:d7/ff/87",
	"rgb:d7/ff/af",
	"rgb:d7/ff/d7",
	"rgb:d7/ff/ff",
	"rgb:ff/00/00",
	"rgb:ff/00/5f",
	"rgb:ff/00/87",
	"rgb:ff/00/af",
	"rgb:ff/00/d7",
	"rgb:ff/00/ff",
	"rgb:ff/5f/00",
	"rgb:ff/5f/5f",
	"rgb:ff/5f/87",
	"rgb:ff/5f/af",
	"rgb:ff/5f/d7",
	"rgb:ff/5f/ff",
	"rgb:ff/87/00",
	"rgb:ff/87/5f",
	"rgb:ff/87/87",
	"rgb:ff/87/af",
	"rgb:ff/87/d7",
	"rgb:ff/87/ff",
	"rgb:ff/af/00",
	"rgb:ff/af/5f",
	"rgb:ff/af/87",
	"rgb:ff/af/af",
	"rgb:ff/af/d7",
	"rgb:ff/af/ff",
	"rgb:ff/d7/00",
	"rgb:ff/d7/5f",
	"rgb:ff/d7/87",
	"rgb:ff/d7/af",
	"rgb:ff/d7/d7",
	"rgb:ff/d7/ff",
	"rgb:ff/ff/00",
	"rgb:ff/ff/5f",
	"rgb:ff/ff/87",
	"rgb:ff/ff/af",
	"rgb:ff/ff/d7",
	"rgb:ff/ff/ff",
# else	/* XTERM_COLORS */
	"rgbi:0/0/0",		/* default 16-255 color table	 */
	"rgbi:0/0/.2",		/* consists of 6 6x6 colour cubes */
	"rgbi:0/0/.4",		/* and a 24 greyscale ramp w/o	*/
	"rgbi:0/0/.6",		/* black or white			*/
	"rgbi:0/0/.8",
	"rgbi:0/0/1",
	"rgbi:0/.2/0",
	"rgbi:0/.2/.2",
	"rgbi:0/.2/.4",
	"rgbi:0/.2/.6",
	"rgbi:0/.2/.8",
	"rgbi:0/.2/1",
	"rgbi:0/.4/0",
	"rgbi:0/.4/.2",
	"rgbi:0/.4/.4",
	"rgbi:0/.4/.6",
	"rgbi:0/.4/.8",
	"rgbi:0/.4/1",
	"rgbi:0/.6/0",
	"rgbi:0/.6/.2",
	"rgbi:0/.6/.4",
	"rgbi:0/.6/.6",
	"rgbi:0/.6/.8",
	"rgbi:0/.6/1",
	"rgbi:0/.8/0",
	"rgbi:0/.8/.2",
	"rgbi:0/.8/.4",
	"rgbi:0/.8/.6",
	"rgbi:0/.8/.8",
	"rgbi:0/.8/1",
	"rgbi:0/1/0",
	"rgbi:0/1/.2",
	"rgbi:0/1/.4",
	"rgbi:0/1/.6",
	"rgbi:0/1/.8",
	"rgbi:0/1/1",
	"rgbi:.2/0/0",
	"rgbi:.2/0/.2",
	"rgbi:.2/0/.4",
	"rgbi:.2/0/.6",
	"rgbi:.2/0/.8",
	"rgbi:.2/0/1",
	"rgbi:.2/.2/0",
	"rgbi:.2/.2/.2",
	"rgbi:.2/.2/.4",
	"rgbi:.2/.2/.6",
	"rgbi:.2/.2/.8",
	"rgbi:.2/.2/1",
	"rgbi:.2/.4/0",
	"rgbi:.2/.4/.2",
	"rgbi:.2/.4/.4",
	"rgbi:.2/.4/.6",
	"rgbi:.2/.4/.8",
	"rgbi:.2/.4/1",
	"rgbi:.2/.6/0",
	"rgbi:.2/.6/.2",
	"rgbi:.2/.6/.4",
	"rgbi:.2/.6/.6",
	"rgbi:.2/.6/.8",
	"rgbi:.2/.6/1",
	"rgbi:.2/.8/0",
	"rgbi:.2/.8/.2",
	"rgbi:.2/.8/.4",
	"rgbi:.2/.8/.6",
	"rgbi:.2/.8/.8",
	"rgbi:.2/.8/1",
	"rgbi:.2/1/0",
	"rgbi:.2/1/.2",
	"rgbi:.2/1/.4",
	"rgbi:.2/1/.6",
	"rgbi:.2/1/.8",
	"rgbi:.2/1/1",
	"rgbi:.4/0/0",
	"rgbi:.4/0/.2",
	"rgbi:.4/0/.4",
	"rgbi:.4/0/.6",
	"rgbi:.4/0/.8",
	"rgbi:.4/0/1",
	"rgbi:.4/.2/0",
	"rgbi:.4/.2/.2",
	"rgbi:.4/.2/.4",
	"rgbi:.4/.2/.6",
	"rgbi:.4/.2/.8",
	"rgbi:.4/.2/1",
	"rgbi:.4/.4/0",
	"rgbi:.4/.4/.2",
	"rgbi:.4/.4/.4",
	"rgbi:.4/.4/.6",
	"rgbi:.4/.4/.8",
	"rgbi:.4/.4/1",
	"rgbi:.4/.6/0",
	"rgbi:.4/.6/.2",
	"rgbi:.4/.6/.4",
	"rgbi:.4/.6/.6",
	"rgbi:.4/.6/.8",
	"rgbi:.4/.6/1",
	"rgbi:.4/.8/0",
	"rgbi:.4/.8/.2",
	"rgbi:.4/.8/.4",
	"rgbi:.4/.8/.6",
	"rgbi:.4/.8/.8",
	"rgbi:.4/.8/1",
	"rgbi:.4/1/0",
	"rgbi:.4/1/.2",
	"rgbi:.4/1/.4",
	"rgbi:.4/1/.6",
	"rgbi:.4/1/.8",
	"rgbi:.4/1/1",
	"rgbi:.6/0/0",
	"rgbi:.6/0/.2",
	"rgbi:.6/0/.4",
	"rgbi:.6/0/.6",
	"rgbi:.6/0/.8",
	"rgbi:.6/0/1",
	"rgbi:.6/.2/0",
	"rgbi:.6/.2/.2",
	"rgbi:.6/.2/.4",
	"rgbi:.6/.2/.6",
	"rgbi:.6/.2/.8",
	"rgbi:.6/.2/1",
	"rgbi:.6/.4/0",
	"rgbi:.6/.4/.2",
	"rgbi:.6/.4/.4",
	"rgbi:.6/.4/.6",
	"rgbi:.6/.4/.8",
	"rgbi:.6/.4/1",
	"rgbi:.6/.6/0",
	"rgbi:.6/.6/.2",
	"rgbi:.6/.6/.4",
	"rgbi:.6/.6/.6",
	"rgbi:.6/.6/.8",
	"rgbi:.6/.6/1",
	"rgbi:.6/.8/0",
	"rgbi:.6/.8/.2",
	"rgbi:.6/.8/.4",
	"rgbi:.6/.8/.6",
	"rgbi:.6/.8/.8",
	"rgbi:.6/.8/1",
	"rgbi:.6/1/0",
	"rgbi:.6/1/.2",
	"rgbi:.6/1/.4",
	"rgbi:.6/1/.6",
	"rgbi:.6/1/.8",
	"rgbi:.6/1/1",
	"rgbi:.8/0/0",
	"rgbi:.8/0/.2",
	"rgbi:.8/0/.4",
	"rgbi:.8/0/.6",
	"rgbi:.8/0/.8",
	"rgbi:.8/0/1",
	"rgbi:.8/.2/0",
	"rgbi:.8/.2/.2",
	"rgbi:.8/.2/.4",
	"rgbi:.8/.2/.6",
	"rgbi:.8/.2/.8",
	"rgbi:.8/.2/1",
	"rgbi:.8/.4/0",
	"rgbi:.8/.4/.2",
	"rgbi:.8/.4/.4",
	"rgbi:.8/.4/.6",
	"rgbi:.8/.4/.8",
	"rgbi:.8/.4/1",
	"rgbi:.8/.6/0",
	"rgbi:.8/.6/.2",
	"rgbi:.8/.6/.4",
	"rgbi:.8/.6/.6",
	"rgbi:.8/.6/.8",
	"rgbi:.8/.6/1",
	"rgbi:.8/.8/0",
	"rgbi:.8/.8/.2",
	"rgbi:.8/.8/.4",
	"rgbi:.8/.8/.6",
	"rgbi:.8/.8/.8",
	"rgbi:.8/.8/1",
	"rgbi:.8/1/0",
	"rgbi:.8/1/.2",
	"rgbi:.8/1/.4",
	"rgbi:.8/1/.6",
	"rgbi:.8/1/.8",
	"rgbi:.8/1/1",
	"rgbi:1/0/0",
	"rgbi:1/0/.2",
	"rgbi:1/0/.4",
	"rgbi:1/0/.6",
	"rgbi:1/0/.8",
	"rgbi:1/0/1",
	"rgbi:1/.2/0",
	"rgbi:1/.2/.2",
	"rgbi:1/.2/.4",
	"rgbi:1/.2/.6",
	"rgbi:1/.2/.8",
	"rgbi:1/.2/1",
	"rgbi:1/.4/0",
	"rgbi:1/.4/.2",
	"rgbi:1/.4/.4",
	"rgbi:1/.4/.6",
	"rgbi:1/.4/.8",
	"rgbi:1/.4/1",
	"rgbi:1/.6/0",
	"rgbi:1/.6/.2",
	"rgbi:1/.6/.4",
	"rgbi:1/.6/.6",
	"rgbi:1/.6/.8",
	"rgbi:1/.6/1",
	"rgbi:1/.8/0",
	"rgbi:1/.8/.2",
	"rgbi:1/.8/.4",
	"rgbi:1/.8/.6",
	"rgbi:1/.8/.8",
	"rgbi:1/.8/1",
	"rgbi:1/1/0",
	"rgbi:1/1/.2",
	"rgbi:1/1/.4",
	"rgbi:1/1/.6",
	"rgbi:1/1/.8",
	"rgbi:1/1/1",
# endif	/* XTERM_COLORS */
	"rgb:08/08/08", /* xterm, rxvt, mrxvt use the same greyscale ramp */
	"rgb:12/12/12",
	"rgb:1c/1c/1c",
	"rgb:26/26/26",
	"rgb:30/30/30",
	"rgb:3a/3a/3a",
	"rgb:44/44/44",
	"rgb:4e/4e/4e",
	"rgb:58/58/58",
	"rgb:62/62/62",
	"rgb:6c/6c/6c",
	"rgb:76/76/76",
	"rgb:80/80/80",
	"rgb:8a/8a/8a",
	"rgb:94/94/94",
	"rgb:9e/9e/9e",
	"rgb:a8/a8/a8",
	"rgb:b2/b2/b2",
	"rgb:bc/bc/bc",
	"rgb:c6/c6/c6",
	"rgb:d0/d0/d0",
	"rgb:da/da/da",
	"rgb:e4/e4/e4",
	"rgb:ee/ee/ee",
#endif	/* TTY_256COLOR */
#ifndef NO_CURSORCOLOR
	COLOR_CURSOR_BACKGROUND,
	COLOR_CURSOR_FOREGROUND,
#endif				/* ! NO_CURSORCOLOR */
	NULL,			/* Color_pointer					*/
	NULL,			/* Color_border					*/
	NULL,			/* Color_ufbg					*/
#ifndef NO_BOLD_UNDERLINE_REVERSE
	NULL,			/* Color_BD						*/
	NULL,			/* Color_UL						*/
	NULL,			/* Color_RV						*/
#endif				/* ! NO_BOLD_UNDERLINE_REVERSE */
#ifdef OPTION_HC
	NULL,
#endif
#ifdef KEEP_SCROLLCOLOR
	COLOR_SCROLLBAR,
	COLOR_SCROLLTROUGH,
#endif				/* KEEP_SCROLLCOLOR */
#ifdef TINTING_SUPPORT
	NULL,
#endif
};


/*
** MUST sync with rxvt.h:enum XA_XXXX
*/
const char *const xa_names[NUM_XA] = {
	"COMPOUND_TEXT",
	"UTF8_STRING",
	"TEXT",
	"MULTIPLE",	
	"TARGETS",	
	"TIMESTAMP",
	"VT_SELECTION",
	"INCR",
	"WM_DELETE_WINDOW",
	"_NET_WM_DESKTOP",
	"_WIN_WORKSPACE",
	"_NET_WM_NAME",
	"_NET_WM_ICON_NAME",
	"WM_CLIENT_LEADER",
	"_NET_WM_WINDOW_OPACITY",
#ifndef NO_FRILLS
	"_NET_WM_PID",
#endif
#ifdef HAVE_X11_SM_SMLIB_H
	"SM_CLIENT_ID",
#endif
#ifdef USE_XIM
	"WM_LOCALE_NAME",
#endif
#ifdef TRANSPARENT
	"_XROOTPMAP_ID",
#endif
#ifdef OFFIX_DND
	"DndProtocol",
	"DndSelection",
#endif
	"CLIPBOARD"
};

/*----------------------------------------------------------------------*/
/* substitute system functions */
#if defined(OS_SVR4) && ! defined(_POSIX_VERSION)
/* INTPROTO */
int
rxvt_getdtablesize(void)
{
	struct rlimit   rlim;

	getrlimit(RLIMIT_NOFILE, &rlim);
	return rlim.rlim_cur;
}
#endif
/*----------------------------------------------------------------------*/
/* EXTPROTO */
int
rxvt_init_vars(rxvt_t *r)
{
	register int		i;
	struct rxvt_hidden*	h;


	MEMSET(r, 0, sizeof(rxvt_t));

	h = r->h = (struct rxvt_hidden *)rxvt_calloc(1, sizeof(struct rxvt_hidden));

	for (i = 0; i < MAX_PAGES; i ++)	{
		/* Initialize vts_idx for each term_t structure */
		r->vterm[i].vts_idx = -1;
		/* Initialize each vts pointer */
		r->vts[i] = NULL;
	}

	/*
	** hotkeys[MAX_HOTKEYS] is a valid entry!!! it will be used to
	** save the first hotkey entry if we toggle disableHotkeys
	*/
	r->hotkeys = (hotkeys_t*) rxvt_malloc(
		sizeof(hotkeys_t) * (MAX_HOTKEYS + 1));

	r->PixColors = (unsigned long*) rxvt_malloc(
		sizeof(unsigned long) * (TOTAL_COLORS + 2 * MAX_PAGES));
#ifdef OFF_FOCUS_FADING
	r->PixColorsUnfocus = (unsigned long*) rxvt_malloc(
		sizeof(unsigned long) * (TOTAL_COLORS + 2 * MAX_PAGES));
#endif
#ifdef XFT_SUPPORT
	r->XftColors = (XftColor*) rxvt_malloc (
		sizeof (XftColor) * (TOTAL_COLORS + 2 * MAX_PAGES));
#endif
	if (NULL == r->h || 
		NULL == r->hotkeys ||
		NULL == r->PixColors
#ifdef OFF_FOCUS_FADING
		|| NULL == r->PixColorsUnfocus
#endif
#ifdef XFT_SUPPORT
		|| NULL == r->XftColors
#endif
		)
		return -1;


	r->Xdisplay = NULL;
#ifdef USE_XIM
	r->TermWin.fontset = NULL;
#endif
	r->TermWin.font = NULL;
#ifdef MULTICHAR_SET
	r->TermWin.mfont = NULL;
#endif
#ifndef NO_BOLDFONT
	r->TermWin.bfont = NULL;
#endif

#ifdef XFT_SUPPORT
	r->TermWin.xftpattern = NULL;
	r->TermWin.xftfont = NULL;
# ifndef NO_BOLDFONT
	r->TermWin.xftbpattern = NULL;
	r->TermWin.xftbfont = NULL;
	r->TermWin.bf_switched = 0;
# endif	/* NO_BOLDFONT */
# ifdef MULTICHAR_SET
#  ifdef HAVE_ICONV_H
	r->TermWin.xfticonv = (iconv_t) -1;
#  endif
	r->TermWin.xftmpattern = NULL;
	r->TermWin.xftmfont = NULL;
# endif	/* MULTICHAR_SET */
#endif	/* XFT_SUPPORT */

#ifdef OFF_FOCUS_FADING
	r->color_switched = 0;	/* color is not switched */
#endif
	h->xa[XA_COMPOUND_TEXT] =
	h->xa[XA_MULTIPLE] =
	h->xa[XA_TARGETS] =
	h->xa[XA_TEXT] =
	h->xa[XA_TIMESTAMP] =
	h->xa[XA_VT_SELECTION] =
	h->xa[XA_INCR] = None;
	h->locale = NULL;
# ifdef HAVE_MENUBAR
	h->BuildMenu = NULL;	/* the menu currently being built */
#  if (MENUBAR_MAX > 1)
	h->CurrentBar = NULL;
#  endif			/* (MENUBAR_MAX > 1) */
# endif
# ifdef USE_XIM
	h->Input_Context = NULL;
# endif
	/* h->v_bufstr = NULL; */
	h->buffer = NULL;
	h->compose.compose_ptr = NULL;
#ifdef TRANSPARENT
	h->am_pixmap_trans = 0;
	h->am_transparent = 0;
#endif


	h->MEvent.time = CurrentTime;
	h->MEvent.button = AnyButton;
	r->Options = DEFAULT_OPTIONS;
	r->Options2 = DEFAULT_OPTIONS2;
	h->want_refresh = 1;
	h->ttygid = -1;
	r->Xfd = -1;
	r->vt_died = 0;

	/* default values */
#ifdef NO_FRILLS
	r->TermWin.int_bwidth = DEFAULT_INTERNALBORDERWIDTH;
	r->TermWin.ext_bwidth = DEFAULT_EXTERNALBORDERWIDTH;
#else
	r->TermWin.int_bwidth = (INTERNALBORDERWIDTH >= 0 && INTERNALBORDERWIDTH <= MAX_INTERNALBORDERWIDTH) ? INTERNALBORDERWIDTH : DEFAULT_INTERNALBORDERWIDTH;
	r->TermWin.ext_bwidth = (EXTERNALBORDERWIDTH >= 0 && EXTERNALBORDERWIDTH <= MAX_EXTERNALBORDERWIDTH) ? EXTERNALBORDERWIDTH : DEFAULT_EXTERNALBORDERWIDTH;
#endif

#ifndef NO_LINESPACE
	r->TermWin.lineSpace = (LINESPACE >= 0 && LINESPACE <= MAX_LINESPACE) ? LINESPACE : DEFAULT_LINESPACE;
#endif

#ifdef CURSOR_BLINK
	r->h->blinkInterval = DEFAULT_BLINK_TIME;
#endif
#ifdef POINTER_BLANK
	r->h->pointerBlankDelay = DEFAULT_BLANKDELAY;
#endif

	r->numPixColors = TOTAL_COLORS;

	/* Initialize selection data */
#ifndef NO_NEW_SELECTION
	r->selection_style = NEW_SELECT;
#else
	r->selection_style = OLD_SELECT;
#endif
	r->selection.vt = -1;
	r->selection.op = SELECTION_CLEAR;
	r->selection.screen = PRIMARY;
	r->selection.clicks = 0;
	r->selection.text = NULL;
	r->selection.len = 0;
	r->selection.beg.row = 0;
	r->selection.beg.col = 0;
	r->selection.end.row = 0;
	r->selection.end.col = 0;

#ifndef NO_BRIGHTCOLOR
	h->colorfgbg = DEFAULT_RSTYLE;
#endif
#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
	h->ks_bigfont = XK_greater;
	h->ks_smallfont = XK_less;
#endif
#ifdef GREEK_SUPPORT
	h->ks_greekmodeswith = GREEK_KEYBOARD_MODESWITCH;
#endif
	h->refresh_limit = 1;
	h->refresh_type = SLOW_REFRESH;
	h->prev_nrow = h->prev_ncol = 0;

	r->encoding_method = ENC_NOENC;
	h->multichar_decode = rxvt_decode_dummy;

	h->oldcursor.row = h->oldcursor.col = -1;
	h->last_bot = h->last_state = -1;
#ifdef HAVE_MENUBAR
	h->menu_readonly = 1;
# if !(MENUBAR_MAX > 1)
	h->CurrentBar = &(h->BarList);
# endif				/* (MENUBAR_MAX > 1) */
#endif

#ifdef HAVE_X11_SM_SMLIB_H
	r->TermWin.sm_conn = NULL;
	r->TermWin.ice_conn = NULL;
	r->TermWin.ice_fd = -1;
	r->TermWin.sm_client_id = NULL;
#endif

	return 0;
}


/* EXTPROTO */
void
rxvt_init_secondary(rxvt_t *r)
{
	int			i, num_fds;
#ifdef TTY_GID_SUPPORT
	struct group   *gr = getgrnam("tty");

	if (gr) {		/* change group ownership of tty to "tty" */
		r->h->ttygid = gr->gr_gid;
	}
	else
#endif				/* TTY_GID_SUPPORT */
	{
		r->h->ttygid = getgid();
	}

	rxvt_set_default_locale (r);

	/* get number of available file descriptors */
#if defined(_POSIX_VERSION) || ! defined(OS_SVR4)
	num_fds = (int)sysconf(_SC_OPEN_MAX);
#else
	num_fds = rxvt_getdtablesize();
#endif

	/*
	** Close all unused file descriptors
	** We don't want them, we don't need them.
	*/
	if ((i = open("/dev/null", O_RDONLY)) < 0) {
		/* TODO: BOO HISS */
		dup2(STDERR_FILENO, STDIN_FILENO);
	}
	else if (i > STDIN_FILENO) {
		dup2(i, STDIN_FILENO);
		close(i);
	}
	dup2(STDERR_FILENO, STDOUT_FILENO);
	for (i = STDERR_FILENO + 1; i < num_fds; i++) {
/* #ifdef __sgi */
#ifdef OS_IRIX
		/* Alex Coventry says we need 4 & 7 too */
		if (i == 4 || i == 7)
			continue;
#endif
		close(i);
	}

	/* Now set the correct num_fds */
	r->num_fds = STDERR_FILENO + 1;
#ifdef OS_IRIX
	r->num_fds = 7 + 1;
#endif
}


/* INTPROTO */
hotkeys_t
make_hotkeys_t (unsigned short func, unsigned short flag, KeySym keysym)
{
	hotkeys_t	value;
	value.func = func;
	value.flag = flag;
	value.keysym = keysym;
	return (value);
}


/* EXTPROTO */
void
rxvt_init_hotkeys (rxvt_t* r)
{
	register int	i = 0;
	hotkeys_t*		hk;


	/* initialize hotkey handlers */
	rxvt_init_hotkey_handlers (r);

	assert (NULL != r->hotkeys);
	hk = r->hotkeys;

	/* if shft is 1, keysym must be upper case!!! */

	/* gnome-terminal hotkeys */
	hk[i++] = make_hotkeys_t (HKF_NEW_TAB,		(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_T);
	hk[i++] = make_hotkeys_t (HKF_KILL_TAB,		(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_W);
	hk[i++] = make_hotkeys_t (HKF_PREV_TAB,		(HK_INTERNAL|HK_CTRL), XK_Prior);
	hk[i++] = make_hotkeys_t (HKF_NEXT_TAB,		(HK_INTERNAL|HK_CTRL), XK_Next);
	hk[i++] = make_hotkeys_t (HKF_TAB_1,		(HK_INTERNAL|HK_META), XK_1);
	hk[i++] = make_hotkeys_t (HKF_TAB_2,		(HK_INTERNAL|HK_META), XK_2);
	hk[i++] = make_hotkeys_t (HKF_TAB_3,		(HK_INTERNAL|HK_META), XK_3);
	hk[i++] = make_hotkeys_t (HKF_TAB_4,		(HK_INTERNAL|HK_META), XK_4);
	hk[i++] = make_hotkeys_t (HKF_TAB_5,		(HK_INTERNAL|HK_META), XK_5);
	hk[i++] = make_hotkeys_t (HKF_TAB_6,		(HK_INTERNAL|HK_META), XK_6);
	hk[i++] = make_hotkeys_t (HKF_TAB_7,		(HK_INTERNAL|HK_META), XK_7);
	hk[i++] = make_hotkeys_t (HKF_TAB_8,		(HK_INTERNAL|HK_META), XK_8);
	hk[i++] = make_hotkeys_t (HKF_TAB_9,		(HK_INTERNAL|HK_META), XK_9);
	hk[i++] = make_hotkeys_t (HKF_SMALL_FONT,	(HK_INTERNAL|HK_CTRL), XK_minus);
	hk[i++] = make_hotkeys_t (HKF_LARGE_FONT,	(HK_INTERNAL|HK_CTRL), XK_equal);
	hk[i++] = make_hotkeys_t (HKF_SMALL_FONT,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_underscore);
	hk[i++] = make_hotkeys_t (HKF_LARGE_FONT,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_plus);

	/* konsole hotkeys */
	hk[i++] = make_hotkeys_t (HKF_LMOVE_TAB,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_Left);
	hk[i++] = make_hotkeys_t (HKF_RMOVE_TAB,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_Right);
	hk[i++] = make_hotkeys_t (HKF_NEW_TAB,		(HK_INTERNAL|HK_CTRL|HK_META), XK_n);
	hk[i++] = make_hotkeys_t (HKF_NEW_TAB,		(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_N);
	hk[i++] = make_hotkeys_t (HKF_PREV_TAB,		(HK_INTERNAL|HK_SHFT), XK_Left);
	hk[i++] = make_hotkeys_t (HKF_NEXT_TAB,		(HK_INTERNAL|HK_SHFT), XK_Right);
	hk[i++] = make_hotkeys_t (HKF_CHANGE_TITLE,	(HK_INTERNAL|HK_CTRL|HK_META), XK_s);

	/* vi-like hotkeys */
	hk[i++] = make_hotkeys_t (HKF_PREV_TAB,		(HK_INTERNAL|HK_CTRL|HK_META), XK_h);
	hk[i++] = make_hotkeys_t (HKF_NEXT_TAB,		(HK_INTERNAL|HK_CTRL|HK_META), XK_l);
	
	/* screen-like hotkeys */
	hk[i++] = make_hotkeys_t (HKF_PREV_ATAB,	(HK_INTERNAL|HK_CTRL|HK_META), XK_p);

	/* our own default hotkeys */
	hk[i++] = make_hotkeys_t (HKF_CHANGE_TITLE,	(HK_INTERNAL|HK_SHFT), XK_Delete);
	hk[i++] = make_hotkeys_t (HKF_PREV_ATAB,	(HK_INTERNAL|HK_CTRL), XK_Tab);
	hk[i++] = make_hotkeys_t (HKF_KILL_TAB,		(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_K);
	hk[i++] = make_hotkeys_t (HKF_TAB_1,		(HK_INTERNAL|HK_CTRL|HK_META), XK_1);
	hk[i++] = make_hotkeys_t (HKF_TAB_2,		(HK_INTERNAL|HK_CTRL|HK_META), XK_2);
	hk[i++] = make_hotkeys_t (HKF_TAB_3,		(HK_INTERNAL|HK_CTRL|HK_META), XK_3);
	hk[i++] = make_hotkeys_t (HKF_TAB_4,		(HK_INTERNAL|HK_CTRL|HK_META), XK_4);
	hk[i++] = make_hotkeys_t (HKF_TAB_5,		(HK_INTERNAL|HK_CTRL|HK_META), XK_5);
	hk[i++] = make_hotkeys_t (HKF_TAB_6,		(HK_INTERNAL|HK_CTRL|HK_META), XK_6);
	hk[i++] = make_hotkeys_t (HKF_TAB_7,		(HK_INTERNAL|HK_CTRL|HK_META), XK_7);
	hk[i++] = make_hotkeys_t (HKF_TAB_8,		(HK_INTERNAL|HK_CTRL|HK_META), XK_8);
	hk[i++] = make_hotkeys_t (HKF_TAB_9,		(HK_INTERNAL|HK_CTRL|HK_META), XK_9);
	hk[i++] = make_hotkeys_t (HKF_LMOVE_TAB,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_less);
	hk[i++] = make_hotkeys_t (HKF_RMOVE_TAB,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_greater);
	hk[i++] = make_hotkeys_t (HKF_DUMP_SCREEN,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_Z);
	hk[i++] = make_hotkeys_t (HKF_INC_OPACITY,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_O);
	hk[i++] = make_hotkeys_t (HKF_DEC_OPACITY,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_U);
	hk[i++] = make_hotkeys_t (HKF_TRANSPARENCY,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_R);
	hk[i++] = make_hotkeys_t (HKF_HIDE_TABBAR,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_H);
	hk[i++] = make_hotkeys_t (HKF_HIDE_SCROLLBAR,(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_S);
	hk[i++] = make_hotkeys_t (HKF_HIDE_MENUBAR,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_M);
	hk[i++] = make_hotkeys_t (HKF_HIDE_BUTTON,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_A);
	hk[i++] = make_hotkeys_t (HKF_VERYBOLD,		(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_F);
	hk[i++] = make_hotkeys_t (HKF_HOLD_EXIT,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_E);
	hk[i++] = make_hotkeys_t (HKF_BROADCAST,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_B);
	hk[i++] = make_hotkeys_t (HKF_SAVE_CONFIG,	(HK_INTERNAL|HK_CTRL|HK_SHFT), XK_X);
	hk[i++] = make_hotkeys_t (HKF_LARGE_FONT,	(HK_INTERNAL|HK_SHFT), XK_Home);
	hk[i++] = make_hotkeys_t (HKF_SMALL_FONT,	(HK_INTERNAL|HK_SHFT), XK_End);
	hk[i++] = make_hotkeys_t (HKF_SCROLL_UP,	(HK_INTERNAL|HK_SHFT), XK_Up);
	hk[i++] = make_hotkeys_t (HKF_SCROLL_DOWN,	(HK_INTERNAL|HK_SHFT), XK_Down);
	hk[i++] = make_hotkeys_t (HKF_SCROLL_PGUP,	(HK_INTERNAL|HK_SHFT), XK_Prior);
	hk[i++] = make_hotkeys_t (HKF_SCROLL_PGDOWN,(HK_INTERNAL|HK_SHFT), XK_Next);

	/* rxvt/xterm hotkeys */

	assert (i <= MAX_HOTKEYS);

	/*
	** hotkeys[MAX_HOTKEYS] is a valid entry!!! it will be used to
	** save the first hotkey entry if we toggle disableHotkeys
	*/
	for (; i <= MAX_HOTKEYS; i ++)	{
		hk[i] = make_hotkeys_t (HKF_DUMMY, 0, 0);
	}
}


/* EXTPROTO */
void
rxvt_toggle_hotkeys (rxvt_t* r, int enable)
{
	if (enable)	{
		if (HKF_DUMMY != r->hotkeys[0].func)	/* already enabled */
			return;

		SWAP_IT (r->hotkeys[0], r->hotkeys[MAX_HOTKEYS], hotkeys_t);
	}
	else	{
		if (HKF_DUMMY == r->hotkeys[0].func)	/* already disabled */
			return;

		SWAP_IT (r->hotkeys[0], r->hotkeys[MAX_HOTKEYS], hotkeys_t);
	}
}


/* INTPROTO */
int
rxvt_xerror_handler(const Display *display __attribute__((unused)), const XErrorEvent *event)
{
	char		error_msg[1024];
	rxvt_t*		r = rxvt_get_r();

	XGetErrorText (r->Xdisplay, event->error_code, error_msg, 1023);
	rxvt_print_error("%s", error_msg);
	if (r->h->allowedxerror == -1) {
		r->h->allowedxerror = event->error_code;
	}
	return 0;		/* ignored anyway */
}



#ifdef TEXT_SHADOW
/* INTPROTO */
void
rxvt_init_shadow_mode (rxvt_t* r, const char* shadow_mode)
{
	if (!shadow_mode ||
		!STRCASECMP ("botright", shadow_mode) ||
		!STRCASECMP ("default", shadow_mode))	{
		r->TermWin.shadow_mode = SHADOW_BOTRIGHT;
	}
	else if (!STRCASECMP ("botleft", shadow_mode))	{
		r->TermWin.shadow_mode = SHADOW_BOTLEFT;
	}
	else if (!STRCASECMP ("topright", shadow_mode))	{
		r->TermWin.shadow_mode = SHADOW_TOPRIGHT;
	}
	else if (!STRCASECMP ("topleft", shadow_mode))	{
		r->TermWin.shadow_mode = SHADOW_TOPLEFT;
	}
	else if (!STRCASECMP ("top", shadow_mode))	{
		r->TermWin.shadow_mode = SHADOW_TOP;
	}
	else if (!STRCASECMP ("bottom", shadow_mode))	{
		r->TermWin.shadow_mode = SHADOW_BOTTOM;
	}
	else if (!STRCASECMP ("left", shadow_mode))	{
		r->TermWin.shadow_mode = SHADOW_LEFT;
	}
	else if (!STRCASECMP ("right", shadow_mode))	{
		r->TermWin.shadow_mode = SHADOW_RIGHT;
	}
	else if (!STRCASECMP ("none", shadow_mode))	{
		r->TermWin.shadow_mode = SHADOW_NONE;
	}
	else	{	/* no match == default */
		r->TermWin.shadow_mode = SHADOW_NONE;
	}
}
#endif


/*----------------------------------------------------------------------*/
/* EXTPROTO */
const char**
rxvt_init_resources(rxvt_t* r, int argc, const char *const *argv)
{
	register int	i, r_argc;
	const char**	cmd_argv;
	const char**	r_argv;
	const char**	rs;


/*
 * Look for -exec option.  Find => split and make cmd_argv[] of command args
 */
	for (r_argc = 0; r_argc < argc; r_argc++)
	if (!STRCMP(argv[r_argc], "-e") || !STRCMP(argv[r_argc], "-exec"))
		break;
	r_argv = (const char **)rxvt_malloc(sizeof(char*) * (r_argc + 1));

	for (i = 0; i < r_argc; i++)
	r_argv[i] = (const char *)argv[i];
	r_argv[i] = NULL;
	if (r_argc == argc)
		cmd_argv = NULL;
	else {
		cmd_argv = (const char **)rxvt_malloc(sizeof(char*) * (argc - r_argc));

		for (i = 0; i < argc - r_argc - 1; i++)
			cmd_argv[i] = (const char *)argv[i + r_argc + 1];
		cmd_argv[i] = NULL;
	}

	/* clear all resources */
	rs = r->h->rs;
	for (i = 0; i < NUM_RESOURCES;)
		rs[i++] = NULL;

	rs[Rs_name] = rxvt_r_basename(argv[0]);

	/*
	** Open display, get options/resources and create the window
	*/
	if ((rs[Rs_display_name] = getenv("DISPLAY")) == NULL)
		rs[Rs_display_name] = ":0";

	rxvt_get_options(r, r_argc, r_argv);
	free(r_argv);

#ifdef LOCAL_X_IS_UNIX
	if (rs[Rs_display_name][0] == ':') {
		int		l = 5 + STRLEN(rs[Rs_display_name]);
		if (l <= 0 || l > 1024) /* possible integer overflow */
			l = 1024;
		val = rxvt_malloc(l);
		STRCPY(val, "unix");
		STRNCAT(val, rs[Rs_display_name], l-5);
		val[l-1] = (char) 0;
		DBG_MSG(1, (stderr, "Open X display %s\n", val));
		r->Xdisplay = XOpenDisplay(val);
		free(val);
	}
#endif

	if (r->Xdisplay == NULL)	{
		DBG_MSG(1, (stderr, "Open X display %s\n", rs[Rs_display_name] ? rs[Rs_display_name] : "nil"));
		r->Xdisplay = XOpenDisplay(rs[Rs_display_name]);
		if (NULL == r->Xdisplay)	{
			rxvt_print_error("can't open display %s", rs[Rs_display_name]);
			exit(EXIT_FAILURE);
		}
	}


#ifdef DEBUG_X
	XSynchronize(r->Xdisplay, True);
	XSetErrorHandler((XErrorHandler) abort);
	/* XSetErrorHandler((XErrorHandler) rxvt_xerror_handler); */
#else
	XSetErrorHandler((XErrorHandler) rxvt_xerror_handler);
#endif

	/* Initialize all atoms after establishing connection to X */
	for (i = 0; i < NUM_XA; i++)	{
		r->h->xa[i] = XInternAtom(r->Xdisplay, xa_names[i], False);
	}


	rxvt_extract_resources(r, r->Xdisplay, rs[Rs_name]);

	/*
	** set any defaults not already set
	*/
	if (cmd_argv && cmd_argv[0]) {
		if (!rs[Rs_title])
			rs[Rs_title] = rxvt_r_basename(cmd_argv[0]);
		if (!rs[Rs_iconName])
			rs[Rs_iconName] = rs[Rs_title];
	}
	else {
		if (!rs[Rs_title])
			rs[Rs_title] = rs[Rs_name];
		if (!rs[Rs_iconName])
			rs[Rs_iconName] = rs[Rs_name];
	}

	if (rs[Rs_saveLinesAll])	{
		register int	tmp = atoi(rs[Rs_saveLinesAll]);
		r->TermWin.saveLines = (tmp >=0 && tmp <= MAX_SAVELINES) ?  tmp : DEFAULT_SAVELINES;
	}

#ifndef NO_FRILLS
	if (rs[Rs_int_bwidth])	{
		register int	tmp = atoi (rs[Rs_int_bwidth]);
		r->TermWin.int_bwidth = (tmp >= 0 && tmp <= MAX_INTERNALBORDERWIDTH) ? tmp : DEFAULT_INTERNALBORDERWIDTH;
	}

	if (rs[Rs_ext_bwidth])	{
		register int	tmp = atoi (rs[Rs_ext_bwidth]);
		r->TermWin.ext_bwidth = (tmp >= 0 && tmp <= MAX_EXTERNALBORDERWIDTH) ? tmp : DEFAULT_EXTERNALBORDERWIDTH;
	}
#endif

#ifndef NO_LINESPACE
	if (rs[Rs_lineSpace])	{
		register int	tmp = atoi (rs[Rs_lineSpace]);
		r->TermWin.lineSpace = (tmp >= 0 && tmp <= MAX_LINESPACE) ? tmp : DEFAULT_LINESPACE;
	}
#endif

#ifdef POINTER_BLANK
	if (rs[Rs_pointerBlankDelay])	{
		register int	tmp = atoi (rs[Rs_pointerBlankDelay]);
		r->h->pointerBlankDelay = (tmp >= 0 && tmp <= MAX_BLANKDELAY) ? tmp : DEFAULT_BLANKDELAY;
	}
#endif

	/* Handle opacity of translucent window */
	if (rs[Rs_opacity])		{
		register int	tmp = atoi (rs[Rs_opacity]);
		r->TermWin.opacity = (tmp >= 0 && tmp <= 100) ? 100 - tmp : 0;

#ifdef TRANSPARENT
		if (None != r->h->xa[XA_NET_WM_WINDOW_OPACITY] &&
			r->Options & Opt_transparent)	{
			/* Override pseudo-transparent */
			r->Options &= ~Opt_transparent;
		}
#endif
	}
	if (rs[Rs_opacityDegree])		{
		register int	tmp = atoi (rs[Rs_opacityDegree]);
		r->TermWin.opacity_degree = (tmp > 0 && tmp <= 100) ? tmp : 1;
	}

#if defined(BACKGROUND_IMAGE) || defined(TRANSPARENT)
	if (rs[Rs_backgroundFade])	{
		register int	fade;
		fade = atoi (rs[Rs_backgroundFade]);
		if (fade < 0 || fade > 100)
			fade = 100;
		r->TermWin.bgfade = 100 - fade;
	}

# ifdef TINTING_SUPPORT
	if (rs[Rs_shade])	{
		register int	shade;
		shade = atoi (rs[Rs_shade]);
		if (shade < 0 || shade > 100)
			shade = 100;
		r->TermWin.shade = 100 - shade;
	}
# endif
#endif

#ifdef OFF_FOCUS_FADING
	if (rs[Rs_fade])	{
		register int	fade;
		fade = atoi (rs[Rs_fade]);
		if (fade < 0 || fade > 100)
			fade = 100;
		r->TermWin.fade = 100 - fade;
	}
#endif

#ifdef CURSOR_BLINK
	if (rs[Rs_cursorBlinkInterval])	{
		register long	tmp = (long) atoi (rs[Rs_cursorBlinkInterval]);
		r->h->blinkInterval = (tmp >= MIN_BLINK_TIME && tmp <= MAX_BLINK_TIME) ? tmp : DEFAULT_BLINK_TIME;
	}
	/* convert msec to usec */
	r->h->blinkInterval *= 1000;
#endif

#ifdef PRINTPIPE
	if (!rs[Rs_print_pipe])
		rs[Rs_print_pipe] = PRINTPIPE;
#endif
	if (!rs[Rs_cutchars])
		rs[Rs_cutchars] = CUTCHARS;
#ifdef ACS_ASCII
	if (!rs[Rs_acs_chars])
		rs[Rs_acs_chars] = ACS_CHARS;
	if ((i = STRLEN(rs[Rs_acs_chars])) < 0x20) {
		val = rxvt_realloc((void *)rs[Rs_acs_chars], 0x20);
		for (; i < 0x20; )
			val[i] = ' ';
		rs[Rs_acs_chars] = val;
	}
#endif
#ifndef NO_BACKSPACE_KEY
	if (!rs[Rs_backspace_key])
# ifdef DEFAULT_BACKSPACE
		r->h->key_backspace = DEFAULT_BACKSPACE;
# else
		r->h->key_backspace = "DEC";	/* can toggle between \010 or \177 */
# endif
	else {
		char*	val = STRDUP(rs[Rs_backspace_key]);
		rxvt_str_trim(val);
		rxvt_str_escaped(val);
		r->h->key_backspace = val;
	}
#endif
#ifndef NO_DELETE_KEY
	if (!rs[Rs_delete_key])
# ifdef DEFAULT_DELETE
		r->h->key_delete = DEFAULT_DELETE;
# else
		r->h->key_delete = "\033[3~";
# endif
	else {
		char* val = STRDUP(rs[Rs_delete_key]);
		rxvt_str_trim(val);
		rxvt_str_escaped(val);
		r->h->key_delete = val;
	}
#endif
	if (rs[Rs_answerbackstring]) {
		rxvt_str_trim((char *)rs[Rs_answerbackstring]);
		rxvt_str_escaped((char *)rs[Rs_answerbackstring]);
	}

	if (rs[Rs_selectstyle]) {
		if (STRNCASECMP(rs[Rs_selectstyle], "oldword", 7) == 0)
			r->selection_style = OLD_WORD_SELECT;
#ifndef NO_OLD_SELECTION
		else if (STRNCASECMP(rs[Rs_selectstyle], "old", 3) == 0)
			r->selection_style = OLD_SELECT;
#endif
	}


	/* Set default X11 fonts */
	rxvt_set_default_font_x11 (r);
#ifdef XFT_SUPPORT
	if (rs[Rs_xftsz])	{
		int		sz = (int) atof (rs[Rs_xftsz]);
		r->TermWin.xftsize = (sz >= MIN_XFT_FONT_SIZE) ? sz : MIN_XFT_FONT_SIZE;
	}
	else /* default xft font size */
		r->TermWin.xftsize = DEFAULT_XFT_FONT_SIZE;
# ifdef MULTICHAR_SET
	if (rs[Rs_xftmsz])	{
		int		sz = (int) atof (rs[Rs_xftmsz]);
		r->TermWin.xftmsize = (sz >= MIN_XFT_FONT_SIZE) ? sz : MIN_XFT_FONT_SIZE;
	}
	else /* default xft font size */
		r->TermWin.xftmsize = DEFAULT_XFT_FONT_SIZE;
# endif	/* MULTICHAR_SET */

	/* Set default Freetype fonts */
	rxvt_set_default_font_xft (r);
#endif	/* XFT_SUPPORT */


#ifdef TEXT_SHADOW
	rxvt_init_shadow_mode (r, rs[Rs_textShadowMode]);
#endif

#ifdef XTERM_REVERSE_VIDEO
	/* this is how xterm implements reverseVideo */
	if (r->Options & Opt_reverseVideo) {
		if (!rs[Rs_color + Color_fg])
			rs[Rs_color + Color_fg] = def_colorName[Color_bg];
		if (!rs[Rs_color + Color_bg])
			rs[Rs_color + Color_bg] = def_colorName[Color_fg];

		for (i = 0; i < MAX_PAGES; i++) {
			int		vtfg = Rs_color + TOTAL_COLORS + i;
			int		vtbg = Rs_color + TOTAL_COLORS + MAX_PAGES + i;
			char*	fg = (char*) rs[vtfg];
			char*	bg = (char*) rs[vtbg];
			/* foreground color of i terminal */
			if (ISSET_VTFG(r, i))
				rs[vtfg] = ISSET_VTBG(r, i) ? bg :
								def_colorName[Color_bg];
			/* background color of i terminal */
			if (ISSET_VTBG(r, i))
				rs[vtbg] = ISSET_VTFG(r, i) ? fg :
							def_colorName[Color_fg];
		}
	}
#endif

	for (i = 0; i < NRS_COLORS; i++)
		if (!rs[Rs_color + i])
			rs[Rs_color + i] = def_colorName[i];

#ifndef XTERM_REVERSE_VIDEO
	/* this is how we implement reverseVideo */
	if (r->Options & Opt_reverseVideo)	{
		SWAP_IT(rs[Rs_color + Color_fg], rs[Rs_color + Color_bg], const char *);
		for (i = 0; i < MAX_PAGES; i++)	{
			int		vtfg = Rs_color + TOTAL_COLORS + i;
			int		vtbg = Rs_color + TOTAL_COLORS + MAX_PAGES + i;
			SWAP_IT(rs[vtfg], rs[vtbg], const char*);
		}
	}
#endif

/* convenient aliases for setting fg/bg to colors */
	rxvt_color_aliases(r, Color_fg);
	rxvt_color_aliases(r, Color_bg);
#ifndef NO_CURSORCOLOR
	rxvt_color_aliases(r, Color_cursor);
	rxvt_color_aliases(r, Color_cursor2);
#endif				/* NO_CURSORCOLOR */
	rxvt_color_aliases(r, Color_pointer);
	rxvt_color_aliases(r, Color_border);
#ifndef NO_BOLD_UNDERLINE_REVERSE
	rxvt_color_aliases(r, Color_BD);
	rxvt_color_aliases(r, Color_UL);
	rxvt_color_aliases(r, Color_RV);
#endif				/* ! NO_BOLD_UNDERLINE_REVERSE */

	return cmd_argv;
}

/*----------------------------------------------------------------------*/
/* EXTPROTO */
void
rxvt_init_env(rxvt_t *r)
{
	int				i;
	unsigned int	u;
	char*			val;


#ifdef DISPLAY_IS_IP
	/* Fixup display_name for export over pty to any interested
	** terminal clients via "ESC[7n" (e.g. shells).  Note we use
	** the pure IP number (for the first non-loopback interface)
	** that we get from rxvt_network_display(). This is more
	** "name-resolution-portable", if you will, and probably allows
	** for faster x-client startup if your name server is beyond
	** a slow link or overloaded at client startup. Of course that
	** only helps the shell's child processes, not us.
	**
	** Giving out the display_name also affords a potential
	** security hole
	*/
	val = rxvt_network_display(r->h->rs[Rs_display_name]);
	r->h->rs[Rs_display_name] = (const char *)val;
	if (val == NULL)
#endif				/* DISPLAY_IS_IP */
	val = XDisplayString(r->Xdisplay);
	if (r->h->rs[Rs_display_name] == NULL)
		r->h->rs[Rs_display_name] = val;	/* use broken `:0' value */

	i = STRLEN(val) + 9;
	if (i <= 0 || i > 1024)	/* possible integer overflow */
		i = 1024;
	r->h->env_display = rxvt_malloc(i * sizeof(char));
	STRCPY (r->h->env_display, "DISPLAY=");
	STRNCAT (r->h->env_display, val, i-9);
	r->h->env_display[i-1] = (char) 0;

	/* avoiding the math library:
	 * i = (int)(ceil(log10((unsigned int)r->TermWin.parent))) */
	for (i = 0, u = (unsigned int)r->TermWin.parent; u; u /= 10, i++)
		;
	MAX_IT(i, 1);
	r->h->env_windowid = rxvt_malloc((i + 10) * sizeof(char));

	sprintf(r->h->env_windowid, "WINDOWID=%u",
		(unsigned int)r->TermWin.parent);

	/*
	** add entries to the environment:
	** @ DISPLAY:   in case we started with -display
	** @ WINDOWID:  X window id number of the window
	** @ COLORTERM: terminal sub-name and also indicates its color
	** @ TERM:		terminal name
	** @ TERMINFO:	path to terminfo directory
	*/
#ifdef HAVE_PUTENV
	putenv(r->h->env_display);
	putenv(r->h->env_windowid);

# ifdef RXVT_TERMINFO
	putenv("TERMINFO=" RXVT_TERMINFO);
# endif
	if (XDEPTH <= 2)
		putenv("COLORTERM=" COLORTERMENV "-mono");
	else
		putenv("COLORTERM=" COLORTERMENVFULL);
	if (r->h->rs[Rs_term_name] != NULL) {
		int		l = 6 + STRLEN(r->h->rs[Rs_term_name]);
		if (l <= 0 || l > 1024)	/* possible integer overflow */
			l = 1024;
		r->h->env_term = rxvt_malloc(l * sizeof(char));
		STRCPY (r->h->env_term, "TERM=");
		STRNCAT (r->h->env_term, r->h->rs[Rs_term_name], l-6);
		r->h->env_term[l-1] = (char) 0;
		putenv(r->h->env_term);
	}
	else
		putenv("TERM=" TERMENV);
#endif	/* HAVE_PUTENV */

#ifdef HAVE_UNSETENV
	/* avoid passing old settings and confusing term size */
	unsetenv("LINES");
	unsetenv("COLUMNS");
	unsetenv("TERMCAP");	/* terminfo should be okay */
#endif				/* HAVE_UNSETENV */

	/*
	** allocate environment variable for MRXVT_TABTITLE, we will
	** use it in rxvt_create_termwin later for each tab terminal
	*/
	r->h->env_tabtitle = rxvt_malloc(sizeof(TABTITLEENV) + MAX_TAB_TXT + 1);
}

/*----------------------------------------------------------------------*/
/*
 * This is more or less stolen straight from XFree86 xterm.
 * This should support all European type languages.
 */
/* EXTPROTO */
void
rxvt_init_xlocale(rxvt_t *r)
{
#ifdef USE_XIM
	if (r->h->locale == NULL)
		rxvt_print_error("Setting locale failed.");
	else {
		XChangeProperty(r->Xdisplay, r->TermWin.parent,
			r->h->xa[XA_WM_LOCALE_NAME], XA_STRING, 8, PropModeReplace,
			(unsigned char *)r->h->locale, STRLEN(r->h->locale));

		if (XSupportsLocale() != True) {
			rxvt_print_error("The locale is not supported by Xlib");
			return;
		}
		rxvt_IM_set_fontset (r, 0);

		/* see if we can connect yet */
		rxvt_IM_init_callback (r->Xdisplay, NULL, NULL);

		/* To avoid Segmentation Fault in C locale: Solaris only? */
		if (STRCMP(r->h->locale, "C"))
			XRegisterIMInstantiateCallback(r->Xdisplay, NULL, NULL,
				NULL, rxvt_IM_init_callback, NULL);
	}
#endif
}

/*----------------------------------------------------------------------*/
/* EXTPROTO */
void
rxvt_init_command(rxvt_t* r, const char *const *argv)
{
	/*
	** Initialize the command connection. This should be called
	** after the X server connection is established.
	*/
	struct sigaction	act;


	/*
	** Enable delete window protocol so that if the top-level window
	** of the terminal is destroyed by the Session Manager, we can
	** receive a ClientMessage event and do something gracefully.
	*/
	XSetWMProtocols (r->Xdisplay, r->TermWin.parent,
			&(r->h->xa[XA_WMDELETEWINDOW]), 1);

#ifdef META8_OPTION
	r->h->meta_char = (r->Options & Opt_meta8 ? 0x80 : C0_ESC);
#endif
	rxvt_get_ourmods(r);

#ifdef GREEK_SUPPORT
	greek_init();
#endif

	r->Xfd = XConnectionNumber(r->Xdisplay);

#ifdef CURSOR_BLINK
	if (r->Options & Opt_cursorBlink)
		(void)gettimeofday(&r->h->lastcursorchange, NULL);
#endif


	act.sa_handler = rxvt_Exit_signal;
	act.sa_flags = 0;
	sigemptyset (&act.sa_mask);
	sigaction (SIGHUP, &act, NULL);
#ifndef OS_SVR4
	sigaction (SIGINT, &act, NULL);
#endif
	sigaction (SIGQUIT, &act, NULL);
	sigaction (SIGTERM, &act, NULL);

	act.sa_handler = rxvt_Child_signal;
	act.sa_flags = 0;
	sigaction (SIGCHLD, &act, NULL);

	/*
	signal(SIGHUP, rxvt_Exit_signal);
#ifndef OS_SVR4
	signal(SIGINT, rxvt_Exit_signal);
#endif
	signal(SIGQUIT, rxvt_Exit_signal);
	signal(SIGTERM, rxvt_Exit_signal);
	signal(SIGCHLD, rxvt_Child_signal);
	*/

	/* need to trap SIGURG for SVR4 (Unixware) rlogin */
	/* signal (SIGURG, SIG_DFL); */
}



#ifdef OFF_FOCUS_FADING
/* EXTPROTO */
unsigned long
rxvt_fade_color (rxvt_t* r, unsigned long pixel)
{
	if (r->h->rs[Rs_fade])	{
		XColor	faded_xcol;

		faded_xcol.pixel = pixel;
		XQueryColor (r->Xdisplay, XCMAP, &faded_xcol);
		faded_xcol.red   = (faded_xcol.red / 100) * r->TermWin.fade;
		faded_xcol.green = (faded_xcol.green / 100) * r->TermWin.fade;
		faded_xcol.blue  = (faded_xcol.blue / 100) * r->TermWin.fade;

		rxvt_alloc_color (r, &faded_xcol, "Faded");
		return faded_xcol.pixel;
	}

	return pixel;
}
#endif


/*
** rxvt_restore_ufbg_color should always be called before
** rxvt_restore_pix_color.
*/
/* EXTPROTO */
int
rxvt_restore_ufbg_color (rxvt_t* r)
{
	/* Restore bg and ufbg color state now */
	if (ISSET_PIXCOLOR(r->h, Color_ufbg) && r->ufbg_switched)	{
		DBG_MSG(2, (stderr, "switch back to bg color\n"));
		SWAP_IT (r->PixColors[Color_bg], r->PixColors[Color_ufbg],
			unsigned long);
		r->ufbg_switched = 0;
		return (1);	/* switched */
	}
	return (0);	/* no change */
}


/*
** rxvt_switch_pix_color should always be called before
** rxvt_switch_ufbg_color.
*/
/* EXTPROTO */
int
rxvt_switch_ufbg_color (rxvt_t* r)
{
	if (ISSET_PIXCOLOR(r->h, Color_ufbg) && !r->ufbg_switched)	{
		DBG_MSG(2, (stderr, "switch to ufbg color\n"));
		SWAP_IT (r->PixColors[Color_bg], r->PixColors[Color_ufbg],
			unsigned long);
		r->ufbg_switched = 1;
		return (1);	/* switched */
	}
	return (0);	/* no change */
}


#ifdef OFF_FOCUS_FADING
/* EXTPROTO */
int
rxvt_restore_pix_color (rxvt_t* r)
{
	/* Restore off-focus color state now */
	if (r->h->rs[Rs_fade] && r->color_switched)	{
		DBG_MSG(2, (stderr, "switch back to focus color\n"));
		SWAP_IT (r->PixColors, r->PixColorsUnfocus, unsigned long*);
		r->color_switched = 0;
		return (1);	/* switched */
	}
	return (0);	/* no change */
}


/* EXTPROTO */
int
rxvt_switch_pix_color (rxvt_t* r)
{
	if (r->h->rs[Rs_fade] && !r->color_switched)	{
		DBG_MSG(2, (stderr, "switch back to unfocus color\n"));
		SWAP_IT (r->PixColors, r->PixColorsUnfocus, unsigned long*);
		r->color_switched = 1;
		return (1);	/* switched */
	}
	return (0);	/* no change */
}
#endif


/* INTPROTO */
void
rxvt_init_colors (rxvt_t *r)
{
	register int	i;


	for (i = 0; i < (XDEPTH <= 2 ? 2 : NRS_COLORS); i++) {
		XColor			xcol;

		if (!r->h->rs[Rs_color + i])
			continue;

		if (!rxvt_parse_alloc_color(r, &xcol, r->h->rs[Rs_color + i])) {
#ifndef XTERM_REVERSE_VIDEO
			if (i < 2 && (r->Options & Opt_reverseVideo)) {
				r->h->rs[Rs_color + i] = def_colorName[!i];
			}
			else
#endif
				r->h->rs[Rs_color + i] = def_colorName[i];

			if (!r->h->rs[Rs_color + i])
				continue;
			if (!rxvt_parse_alloc_color (r, &xcol,
				r->h->rs[Rs_color + i])) {
				switch (i) {
				case Color_fg:
				case Color_bg:
					/* fatal: need bg/fg color */
					rxvt_print_error("aborting");
					exit(EXIT_FAILURE);
					/* NOTREACHED */
					break;
#ifndef NO_CURSORCOLOR
				case Color_cursor2:
					xcol.pixel = r->PixColors[Color_fg];
					break;
#endif				/* ! NO_CURSORCOLOR */
				case Color_pointer:
					xcol.pixel = r->PixColors[Color_fg];
					break;
				default:
					xcol.pixel = r->PixColors[Color_bg];	/* None */
					break;
				}
			}
		}
		r->PixColors[i] = xcol.pixel;
#ifdef OFF_FOCUS_FADING
		r->PixColorsUnfocus[i] = rxvt_fade_color (r, xcol.pixel);
#endif
#ifdef XFT_SUPPORT
		rxvt_alloc_xft_color (r, xcol.pixel, &(r->XftColors[i]));
#endif
		SET_PIXCOLOR(r->h, i);
	}

	if (XDEPTH <= 2 || !r->h->rs[Rs_color + Color_pointer])	{
		r->PixColors[Color_pointer] = r->PixColors[Color_fg];
#ifdef XFT_SUPPORT
		r->XftColors[Color_pointer] = r->XftColors[Color_fg];
#endif
	}
	if (XDEPTH <= 2 || !r->h->rs[Rs_color + Color_border])	{
		r->PixColors[Color_border] = r->PixColors[Color_fg];
#ifdef XFT_SUPPORT
		r->XftColors[Color_border] = r->XftColors[Color_fg];
#endif
	}


	/* we have not switched the bg/ufbg color */
	r->ufbg_switched = 0;

	/* Save global foreground/background colors */
	r->h->global_fg = r->PixColors[Color_fg];
	r->h->global_bg = r->PixColors[Color_bg];
#ifdef XFT_SUPPORT
	r->h->global_xftfg = r->XftColors[Color_fg];
	r->h->global_xftbg = r->XftColors[Color_bg];
#endif


	/* Initialize fg/bg colors for individual terminals */
	for (i = 0; i < MAX_PAGES; i++) {
		XColor		xcol;
		int			vtfg = Rs_color + TOTAL_COLORS + i;
		int			vtbg = Rs_color + TOTAL_COLORS + MAX_PAGES + i;

		/* foreground color of i terminal */
		if (ISSET_VTFG(r, i) &&
			rxvt_parse_alloc_color(r, &xcol, r->h->rs[vtfg]))	{
			VTFG(r, i) = xcol.pixel;
#ifdef XFT_SUPPORT
			rxvt_alloc_xft_color (r, VTFG(r, i), &(VTXFTFG(r, i)));
#endif
		}

		/* background color of i terminal */
		if (ISSET_VTBG(r, i) &&
			rxvt_parse_alloc_color(r, &xcol, r->h->rs[vtbg]))	{
			VTBG(r, i) = xcol.pixel;
#ifdef XFT_SUPPORT
			rxvt_alloc_xft_color (r, VTBG(r, i), &(VTXFTBG(r, i)));
#endif
		}
	}


	/*
	** get scrollBar/menuBar shadow colors
	**
	** The calculations of topShadow/bottomShadow values are adapted
	** from the fvwm window manager.
	**/
#ifdef KEEP_SCROLLCOLOR
	if (XDEPTH <= 2) {	/* Monochrome */
		r->PixColors[Color_scroll] = r->PixColors[Color_fg];
		r->PixColors[Color_topShadow] = r->PixColors[Color_bg];
		r->PixColors[Color_bottomShadow] = r->PixColors[Color_bg];
	}
	else {
		XColor			xcol[3];
		/* xcol[0] == white
		 * xcol[1] == top shadow
		 * xcol[2] == bot shadow */

		xcol[1].pixel = r->PixColors[Color_scroll];
# ifdef PREFER_24BIT
		xcol[0].red = xcol[0].green = xcol[0].blue = (unsigned short)~0;
		rxvt_alloc_color(r, &(xcol[0]), "White");
/*		XFreeColors(r->Xdisplay, XCMAP, &(xcol[0].pixel), 1, ~0); */
		XQueryColors(r->Xdisplay, XCMAP, &(xcol[1]), 1);
# else
		xcol[0].pixel = WhitePixel(r->Xdisplay, XSCREEN);
		XQueryColors(r->Xdisplay, XCMAP, xcol, 2);
# endif

		/* bottomShadowColor */
		xcol[2].red = xcol[1].red / 2;
		xcol[2].green = xcol[1].green / 2;
		xcol[2].blue = xcol[1].blue / 2;
		if (!rxvt_alloc_color(r, &(xcol[2]), "Color_bottomShadow"))
			xcol[2].pixel = r->PixColors[Color_Black];
		r->PixColors[Color_bottomShadow] = xcol[2].pixel;

		/* topShadowColor */
		xcol[1].red = max((xcol[0].red / 5), xcol[1].red);
		xcol[1].green = max((xcol[0].green / 5), xcol[1].green);
		xcol[1].blue = max((xcol[0].blue / 5), xcol[1].blue);
		xcol[1].red = min(xcol[0].red, (xcol[1].red * 7) / 5);
		xcol[1].green = min(xcol[0].green, (xcol[1].green * 7) / 5);
		xcol[1].blue = min(xcol[0].blue, (xcol[1].blue * 7) / 5);

		if (!rxvt_alloc_color(r, &(xcol[1]), "Color_topShadow"))
			xcol[1].pixel = r->PixColors[Color_White];
		r->PixColors[Color_topShadow] = xcol[1].pixel;
	}
#endif	/* KEEP_SCROLLCOLOR */


#ifdef TEXT_SHADOW
	if (r->h->rs[Rs_textShadow])	{
		XColor	xcol;
		if (rxvt_parse_alloc_color (r, &xcol, r->h->rs[Rs_textShadow]))
			r->TermWin.shadow = xcol.pixel;
		else
			r->TermWin.shadow = r->PixColors[Color_Black];
# ifdef XFT_SUPPORT
		rxvt_alloc_xft_color (r, r->TermWin.shadow, &(r->TermWin.xftshadow));
# endif
	}
#endif
}


/*----------------------------------------------------------------------*/
/* color aliases, fg/bg bright-bold */
/* INTPROTO */
void
rxvt_color_aliases(rxvt_t* r, int idx)
{
	if (r->h->rs[Rs_color + idx] &&
		isdigit((int) *(r->h->rs[Rs_color + idx]))) {
		int			i = atoi(r->h->rs[Rs_color + idx]);

		if (i >= 8 && i <= 15) {	/* bright colors */
			i -= 8;
#ifndef NO_BRIGHTCOLOR
			r->h->rs[Rs_color + idx] = r->h->rs[Rs_color + minBrightCOLOR + i];
			return;
#endif
		}
		if (i >= 0 && i <= 7)	/* normal colors */
			r->h->rs[Rs_color + idx] = r->h->rs[Rs_color + minCOLOR +i];
	}
}


/* INTPROTO */
void
rxvt_init_win_size (rxvt_t* r)
{
	int				flags = 0;	/* must initialize to 0!!! */
	short			recalc_x = 0, recalc_y = 0;
	int				x, y;
	unsigned int	w, h;


	r->szHint.flags = PMinSize | PResizeInc | PBaseSize | PWinGravity;
	r->szHint.win_gravity = NorthWestGravity;

	/* Set default terminal columns and rows */
	r->TermWin.ncol = 80;
	r->TermWin.nrow = 24;
	r->szHint.x = 0;
	r->szHint.y = 0;

	/* Parse geometry */
	if (r->h->rs[Rs_geometry])
		flags = XParseGeometry(r->h->rs[Rs_geometry], &x, &y, &w, &h);
	if (flags & WidthValue) {
		r->TermWin.ncol = BOUND_POSITIVE_INT16(w);
		r->szHint.flags |= USSize;
	}
	if (flags & HeightValue) {
		r->TermWin.nrow = BOUND_POSITIVE_INT16(h);
		r->szHint.flags |= USSize;
	}
	if (flags & XValue) {
		r->szHint.x = x;
		r->szHint.flags |= USPosition;
		if (flags & XNegative) {
			recalc_x = 1;
			r->szHint.win_gravity = NorthEastGravity;
		}
	}
	if (flags & YValue) {
		r->szHint.y = y;
		r->szHint.flags |= USPosition;
		if (flags & YNegative) {
			recalc_y = 1;
			if (r->szHint.win_gravity == NorthEastGravity)
				r->szHint.win_gravity = SouthEastGravity;
			else
				r->szHint.win_gravity = SouthWestGravity;
		}
	}

	/* Calculate the base width and height */
	r->szHint.base_width = 2 * r->TermWin.int_bwidth;
	r->szHint.base_height = 2 * r->TermWin.int_bwidth;
#ifdef HAVE_SCROLLBARS
	if (r->Options & Opt_scrollBar)
		r->szHint.base_width += rxvt_scrollbar_rwidth (r);
#endif
#if defined(HAVE_MENUBAR) && (MENUBAR_MAX > 1)
	if (r->Options & Opt_showMenu)
		r->szHint.base_height += rxvt_menubar_rheight (r);
#endif
	if (!(r->Options2 & Opt2_hideTabbar))
		r->szHint.base_height += rxvt_tabbar_rheight (r);

	/* Calculate the terminal increment width and height */
	r->szHint.width_inc = r->TermWin.fwidth;
	r->szHint.height_inc = r->TermWin.fheight;

	/* Set the terminal minimal width and height */
	r->szHint.min_width = r->szHint.base_width + r->szHint.width_inc;
	r->szHint.min_height = r->szHint.base_height + r->szHint.height_inc;

	/* Set the terminal width and height */
	r->szHint.width = r->szHint.base_width + Width2Pixel (r->TermWin.ncol);
	r->szHint.height = r->szHint.base_height + Height2Pixel (r->TermWin.nrow);

	/* Recalculate the starting position */
	if (recalc_x)
		r->szHint.x += (DisplayWidth(r->Xdisplay, XSCREEN)
			- r->szHint.width - 2 * r->TermWin.ext_bwidth);
	if (recalc_y)
		r->szHint.y += (DisplayHeight(r->Xdisplay, XSCREEN)
			- r->szHint.height - 2 * r->TermWin.ext_bwidth);

	/* Set the terminal window starting position */
	r->h->window_vt_x = (r->Options & Opt_scrollBar_right) ? 
			0 : r->szHint.base_width - 2*r->TermWin.int_bwidth;
	r->h->window_vt_y = r->szHint.base_height - 2*r->TermWin.int_bwidth;
	if ((r->Options2 & Opt2_bottomTabbar) &&
		!(r->Options2 & Opt2_hideTabbar))
		r->h->window_vt_y -= rxvt_tabbar_rheight (r);
}


/*----------------------------------------------------------------------*/
/*
 * Probe the modifier keymap to get the Meta (Alt) and Num_Lock settings
 * Use resource ``modifier'' to override the Meta modifier
 */
/* INTPROTO */
void
rxvt_get_ourmods(rxvt_t *r)
{
	int					i, j, k;
	int					requestedmeta, realmeta, realalt;
	const char*			cm;
	const char*			rsmod;
	XModifierKeymap*	map;
	KeyCode*			kc;
	const unsigned int	modmasks[] = {
			Mod1Mask, Mod2Mask, Mod3Mask, Mod4Mask, Mod5Mask
		};


	requestedmeta = realmeta = realalt = 0;

	rsmod = r->h->rs[Rs_modifier];
	if (rsmod &&
		STRCASECMP(rsmod, "mod1") >= 0 &&
		STRCASECMP(rsmod, "mod5") <= 0)
		requestedmeta = rsmod[3] - '0';

	map = XGetModifierMapping(r->Xdisplay);
	kc = map->modifiermap;
	for (i = 1; i < 6; i++) {
		k = (i + 2) * map->max_keypermod;	/* skip shift/lock/control */
		for (j = map->max_keypermod; j--; k++) {
			if (kc[k] == 0)
				break;

			switch (XKeycodeToKeysym(r->Xdisplay, kc[k], 0)) {
			case XK_Num_Lock:
				r->h->ModNumLockMask = modmasks[i - 1];
				/* FALLTHROUGH */
			default:
				continue;	/* for(;;) */
			case XK_Meta_L:
			case XK_Meta_R:
				cm = "meta";
				realmeta = i;
				break;
			case XK_Alt_L:
			case XK_Alt_R:
				cm = "alt";
				realalt = i;
				break;
			case XK_Super_L:
			case XK_Super_R:
				cm = "super";
				break;
			case XK_Hyper_L:
			case XK_Hyper_R:
				cm = "hyper";
				break;
			}

			if (rsmod && STRNCASECMP(rsmod, cm, STRLEN(cm)) == 0)
				requestedmeta = i;
		}
	}
	XFreeModifiermap(map);

	i = (requestedmeta ? requestedmeta : (
			realmeta ? realmeta : (
			realalt ? realalt : 0)));

	if (i)
		r->h->ModMetaMask = modmasks[i - 1];
}



/* INTPROTO */
char**
rxvt_string_to_argv (char* string, int* argc)
{
	register int	i;
	char**			pret;
	char*			pbeg;
	char*			pcur;

	/* not implemented yet */
	*argc = 0;
	if (NULL == string)
		return NULL;

#define MAX_ARGV	(64)
	/* Up to 64 argv */
	pret = (char**) rxvt_malloc (MAX_ARGV * sizeof (char*));

	DBG_MSG(1, (stderr, "fetch command argv for the tab\n"));
	pbeg = pcur = string;
	for (i = 0; i < MAX_ARGV-1; i ++)	{
		int		dq = 0;	/* double quote */
		int		sq = 0;	/* single quote */
		/* set default argument to NULL */
		pret[i] = NULL;

		/* skip any spaces and non-printable */
		while (*pcur && 
			(isspace ((int) *pcur) || !isprint ((int) *pcur)))
			pcur ++;
		/* stop if reach end of string */
		if (!*pcur)
			break;

		/* beginning of the token */
		if (isalnum ((int) *pcur) || ispunct ((int) *pcur))	{
			if ('\"' == *pcur)	{
				/* beginning of double quote */
				dq = 1;	pbeg = pcur + 1; pcur ++;
			}
			else if ('\'' == *pcur)	{
				/* beginning of single quote */
				sq = 1;	pbeg = pcur + 1; pcur ++;
			}
			else /* normal characters */
				pbeg = pcur;
		}
#ifdef DEBUG
		else	/* shouldn't happen */
			assert (0);
#endif

		/* move forward one character */
		pcur ++;

		/* now fetch the new token */
		while (*pcur &&					/* not end of string */
			((dq && *pcur != '\"') ||	/* not end of double quote */
			 (sq && *pcur != '\'') ||	/* not end of single quote */
			 (!dq && !sq && !isspace ((int) *pcur))))
			*pcur ++;

		if (!*pcur &&	/* end of string */
			(dq || sq))	/* no match of quote is found */
			goto NotMatch;

		if (!*pcur)	{	/* end of string */
			pret[i] = STRDUP (pbeg);
			DBG_MSG(1, (stderr, "   argv[%d] = %s\n", i, pret[i]));
			break;		/* ready to return */
		}

		if ((dq && *pcur == '\"') ||	/* end of double quote */
			(sq && *pcur == '\'') ||	/* end of single quote */
			(!dq && !sq && isspace ((int) *pcur)))	{	/* space */
			int		len = sizeof (char) * (pcur - pbeg) + 1;

			assert (len > 0);	/* possible integer overflow? */
			pret[i] = (char*) rxvt_malloc (len * sizeof(char));
			MEMCPY (pret[i], pbeg, len-1);
			pret[i][len-1] = (char) 0;
			DBG_MSG(1, (stderr, "   argv[%d] = %s\n", i, pret[i]));

			/* forward to next character */
			pcur ++;

			/* fetch next token */
			continue;
		}

		/* shouldn't get here */
		assert (0);
	}

#undef MAX_ARGV
	/* set the end of argv */
	if (pret[i])	{
		*argc = i+1;
		pret[i+1] = NULL;
	}
	else if (i)	{	/* non-empty argv */
		*argc = i;
	}
	else	{	/* empty argv */
		free (pret);
		return NULL;
	}
	return pret;

NotMatch:
	free (pret);
	return NULL;
}


/* EXTPROTO */
void
rxvt_switch_fgbg_color (rxvt_t* r, int page)
{
	/* Restore bg/ufbg color */
	rxvt_restore_ufbg_color (r);
#ifdef OFF_FOCUS_FADING
	/* Restore off-focus color */
	rxvt_restore_pix_color (r);
#endif

	/*
	** Set fg/bg color of individual terminal
	*/
	r->PixColors[Color_fg] = *(PVTS(r, page)->p_fg);
	r->PixColors[Color_bg] = *(PVTS(r, page)->p_bg);
#ifdef XFT_SUPPORT
	r->XftColors[Color_fg] = *(PVTS(r, page)->p_xftfg);
	r->XftColors[Color_bg] = *(PVTS(r, page)->p_xftbg);
#endif	/* XFT_SUPPORT */

	/*
	** Set foreground/background color for GC. This is necessary.
	** Since all VTs share the same GC, if we do not set the color
	** here, color from other VTs will be used to draw the following
	** text till there is a color change.
	*/
	XSetForeground (r->Xdisplay, r->TermWin.gc, r->PixColors[Color_fg]);
	XSetBackground (r->Xdisplay, r->TermWin.gc, r->PixColors[Color_bg]);
}


/* INTPROTO */
termenv_t
rxvt_get_termenv (const char* env)
{
	if (NULL == env)
		return (TERMENV_XTERM);
	else if (0 == STRCASECMP (env, "xterm"))
		return (TERMENV_XTERM);
	else if (0 == STRCASECMP (env, "rxvt"))
		return (TERMENV_RXVT);
	else if (0 == STRCASECMP (env, "vt102"))
		return (TERMENV_VT102);
	else if (0 == STRCASECMP (env, "vt100"))
		return (TERMENV_VT100);
	else if (0 == STRCASECMP (env, "ansi"))
		return (TERMENV_ANSI);
	else if (0 == STRCASECMP (env, "dumb"))
		return (TERMENV_DUMB);
	else
		return (TERMENV_XTERM);
}


/* INTPROTO */
void
rxvt_init_vts (rxvt_t* r, int page)
{
#ifdef TTY_GID_SUPPORT
	struct group*	gr = getgrnam("tty");
#endif
	struct tms		tp;
	register int	i;


	assert (page < MAX_PAGES);

	/* look for an unused term_t structure */
	for (i = 0; i < MAX_PAGES; i ++)
		if (-1 == r->vterm[i].vts_idx)
			break;
	assert (i != MAX_PAGES);
	DBG_MSG(1, (stderr, "Find vterm[%d] for pointer vts[%d]\n", i, page));

	/* clear the term_t structure */
	r->vts[page] = &(r->vterm[i]);
	MEMSET (r->vts[page], 0, sizeof (r->vterm[0]));
	/* set vts_idx for the vterm */
	PVTS(r, page)->vts_idx = i;

#ifdef TTY_GID_SUPPORT
	/* change group ownership of tty to "tty" */
	if (gr)	{
		PVTS(r, page)->ttymode = S_IRUSR | S_IWUSR | S_IWGRP;
	}
	else
#endif		/* TTY_GID_SUPPORT */
	{
		PVTS(r, page)->ttymode = S_IRUSR | S_IWUSR | S_IWGRP | S_IWOTH;
	}

	/* Initialize term_t (vts) structure */
	/*
	** How to get saveLines? Try the following sequence:
	** . individual saveLines in .Xdefaults or command line option
	** . saveLines for all tabs in .Xdefaults or command line option
	** . Default saveLines
	*/
	if (r->h->rs[Rs_saveLines+page])	{
		register int	tmp = atoi(r->h->rs[Rs_saveLines+page]);
		PVTS(r, page)->saveLines = (tmp >=0 && tmp <= MAX_SAVELINES) ?  tmp : DEFAULT_SAVELINES;
	}
	else if (r->h->rs[Rs_saveLinesAll])	{
		PVTS(r, page)->saveLines = r->TermWin.saveLines;
	}
	else
		PVTS(r, page)->saveLines = (SAVELINES >= 0 && SAVELINES <= MAX_SAVELINES) ? SAVELINES : DEFAULT_SAVELINES;

	/* will be set in rxvt_create_termwin */
	PVTS(r, page)->vt = None;
#ifdef XFT_SUPPORT
	PVTS(r, page)->xftvt = NULL;
#endif
	PVTS(r, page)->tab_title = NULL;
#ifdef BACKGROUND_IMAGE
	PVTS(r, page)->pixmap = None;
	PVTS(r, page)->bg.pixmap = None;
	PVTS(r, page)->bg.x = PVTS(r, page)->bg.y = 50;
#endif
	PVTS(r, page)->cmd_pid = -1;
	PVTS(r, page)->cmd_fd = PVTS(r, page)->tty_fd = -1;
#ifdef UTMP_SUPPORT
	PVTS(r, page)->next_utmp_action = SAVE;
#endif
#ifndef NO_SETOWNER_TTYDEV
	PVTS(r, page)->next_tty_action = SAVE;
#endif

	PVTS(r, page)->hold = 0;		/* clear hold flag */
	PVTS(r, page)->dead = 0;		/* clear dead flag */
	PVTS(r, page)->highlight = 0;	/* clear highlight flag */

	/* Get term_env type */
	PVTS(r, page)->termenv = rxvt_get_termenv (
		r->h->rs[Rs_term_name] ? r->h->rs[Rs_term_name] : TERMENV);

	/* get command argv from resources */
	PVTS(r, page)->command_argv = rxvt_string_to_argv (
				(char*) r->h->rs[Rs_command+i],
				&(PVTS(r, page)->command_argc));

	/* Initialize PrivateModes and SavedModes */
	PVTS(r, page)->PrivateModes = PVTS(r, page)->SavedModes =
		PrivMode_Default;
	if (r->Options & Opt_scrollTtyOutputInhibit)
		PVTS(r, page)->PrivateModes |= PrivMode_TtyOutputInh;
	if (r->Options & Opt_scrollTtyKeypress)
		PVTS(r, page)->PrivateModes |= PrivMode_Keypress;
	if (!(r->Options & Opt_jumpScroll))
		PVTS(r, page)->PrivateModes |= PrivMode_smoothScroll;
#ifndef NO_BACKSPACE_KEY
	if (STRCMP(r->h->key_backspace, "DEC") == 0)
		PVTS(r, page)->PrivateModes |= PrivMode_HaveBackSpace;
#endif
#ifdef HAVE_SCROLLBARS
	if (rxvt_scrollbar_visible(r)) {
		PVTS(r, page)->PrivateModes |= PrivMode_scrollBar;
		PVTS(r, page)->SavedModes |= PrivMode_scrollBar;
	}
#endif
#ifdef HAVE_MENUBAR
	if (rxvt_menubar_visible(r)) {
		PVTS(r, page)->PrivateModes |= PrivMode_menuBar;
		PVTS(r, page)->SavedModes |= PrivMode_menuBar;
	}
#endif

	/* Now set VT fg/bg color */
	PVTS(r, page)->p_fg = ISSET_VTFG(r, page) ?
			&(VTFG(r, page)) : &(r->h->global_fg);
	PVTS(r, page)->p_bg = ISSET_VTBG(r, page) ?
			&(VTBG(r, page)) : &(r->h->global_bg);
#ifdef XFT_SUPPORT
	PVTS(r, page)->p_xftfg = ISSET_VTFG(r, page) ?
			&(VTXFTFG(r, page)) : &(r->h->global_xftfg);
	PVTS(r, page)->p_xftbg = ISSET_VTBG(r, page) ?
			&(VTXFTBG(r, page)) : &(r->h->global_xftbg);
#endif

	/* Initialize input buffer */
	PVTS(r, page)->cmdbuf_ptr =
		PVTS(r, page)->cmdbuf_endp = 
		PVTS(r, page)->cmdbuf_base;
	
	/* Initialize write out buffer */
	PVTS(r, page)->v_buffer =
		PVTS(r, page)->v_bufstr =
		PVTS(r, page)->v_bufptr =
		PVTS(r, page)->v_bufend = NULL;

	/* Initialize checksum */
	PVTS(r, page)->checksum = times (&tp);

	/* Set screen structure initialization flag */
	PVTS(r, page)->init_screen = 0;
}


/*----------------------------------------------------------------------*/
/* rxvt_destroy_termwin() - destroy a terminal window */
/* EXTPROTO */
void
rxvt_destroy_termwin(rxvt_t* r, int page)
{
	assert (page < MAX_PAGES);

	/* free command argv */
	if (PVTS(r, page)->command_argv && PVTS(r, page)->command_argc)	{
		register int	i;
		for (i = 0; i < PVTS(r, page)->command_argc; i ++)
			free (PVTS(r, page)->command_argv[i]);
		free (PVTS(r, page)->command_argv);
		PVTS(r, page)->command_argv = NULL;
		PVTS(r, page)->command_argc = 0;
	}

	assert (PVTS(r, page)->tab_title);
	free (PVTS(r, page)->tab_title);
	PVTS(r, page)->tab_title = NULL;

#ifdef XFT_SUPPORT
	if (r->Options & Opt_xft)	{
		if (PVTS(r, page)->xftvt)
			XftDrawDestroy (PVTS(r, page)->xftvt);
		PVTS(r, page)->xftvt = NULL;
	}
#endif
	assert (None != PVTS(r, page)->vt);
	XDestroyWindow (r->Xdisplay, PVTS(r, page)->vt);
	PVTS(r, page)->vt = None;

#ifdef BACKGROUND_IMAGE
	if (None != PVTS(r, page)->pixmap)	{
		XFreePixmap (r->Xdisplay, PVTS(r, page)->pixmap);
		PVTS(r, page)->pixmap = None;
	}
	if (None != PVTS(r, page)->bg.pixmap)	{
		XFreePixmap (r->Xdisplay, PVTS(r, page)->bg.pixmap);
		PVTS(r, page)->bg.pixmap = None;
	}
#endif

	/* Set vterm index to -1, so that we know it's unused */
	PVTS(r, page)->vts_idx = -1;
}



char*	g_default_tab_title = "Terminal";
/* rxvt_create_termwin() - create a terminal window */
/* EXTPROTO */
void
rxvt_create_termwin(rxvt_t* r, int page, const char TAINTED * title)
{
#ifdef DEBUG_X
	char			vt_name[32];
#endif
	long			vt_emask;
	char TAINTED *	t;


	assert (page < MAX_PAGES);

	rxvt_init_vts (r, page);

	/*
	** How to get the tab title? Try the following steps in sequence:
	** . runtime supplied tab title (e.g., escape sequence)
	** . individual tab title in .Xdefaults or command line option
	** . tab title for all tabs in .Xdefaults or command line option
	** . default tab title
	*/
	t = (char TAINTED *) title;
	if (NULL == t)
		t = (char*) r->h->rs[Rs_tabtitle+page];
	if (NULL == t)
		t = (char*) r->h->rs[Rs_tabtitleAll];
	if (NULL == t)
		t = g_default_tab_title;
	PVTS(r, page)->tab_title = (char UNTAINTED *) STRNDUP(t, MAX_TAB_TXT);
#ifdef HAVE_PUTENV
	/* Set environment variable of tab title */
	sprintf (r->h->env_tabtitle, TABTITLEENV "%s", PVTS(r, page)->tab_title);
	putenv (r->h->env_tabtitle);
#endif

	PVTS(r, page)->tab_width = rxvt_tab_width (r, PVTS(r, page)->tab_title);

	/*
	** Now switch fg/bg colors before creating VT because this
	** will use the fg/bg colors
	*/
	rxvt_switch_fgbg_color (r, page);

	/* create the terminal window */
	DBG_MSG (1, (stderr, "Create VT %d (%dx%d+%dx%d)\n", page, r->h->window_vt_x, r->h->window_vt_y, VT_WIDTH(r), VT_HEIGHT(r)));
	PVTS(r, page)->vt = XCreateSimpleWindow (r->Xdisplay,
						r->TermWin.parent,
						r->h->window_vt_x, r->h->window_vt_y,
						VT_WIDTH(r), VT_HEIGHT(r),
						0,
						r->PixColors[Color_fg],
						r->PixColors[Color_bg]);
	assert (None != PVTS(r, page)->vt);
#ifdef XFT_SUPPORT
	if (r->Options & Opt_xft)	{
		PVTS(r, page)->xftvt = XftDrawCreate (r->Xdisplay,
			PVTS(r, page)->vt, XVISUAL, XCMAP);
		assert (NULL != PVTS(r, page)->xftvt);
	}
#endif


#ifdef DEBUG_X
	sprintf (vt_name, "vt %d window", page);
	rxvt_set_win_title (r, PVTS(r, page)->vt, vt_name);
#endif


	/* define cursor for the terminal window */
	rxvt_pointer_unblank(r, page);

	/* define event mask fo the terminal window */
	vt_emask = (ExposureMask | ButtonPressMask | ButtonReleaseMask
		| PropertyChangeMask);
#ifdef POINTER_BLANK
	if ((r->Options & Opt_pointerBlank))
		vt_emask |= PointerMotionMask;
	else
#endif
		vt_emask |= (Button1MotionMask | Button3MotionMask);
	XSelectInput(r->Xdisplay, PVTS(r, page)->vt, vt_emask);

#ifdef TRANSPARENT
	/* Set transparent background */
	if (r->Options & Opt_transparent)	{
		XSetWindowBackgroundPixmap (r->Xdisplay, PVTS(r, page)->vt,
			ParentRelative);
	}
#endif

	/*
	** Load the background image for terminal window when not
	** transparent
	*/
#ifdef BACKGROUND_IMAGE
# ifdef TRANSPARENT
	if (!(r->Options & Opt_transparent))
# endif
	if (r->h->rs[Rs_backgroundPixmap+page] ||
		r->h->rs[Rs_backgroundPixmapAll]) {
		/* Load pixmap for each individual tab */
		const char*	pf = r->h->rs[Rs_backgroundPixmap+page];
		const char*	p;
		/*
		** If each individual pixmap is unset, load pixmap for all
		** tabs
		*/
		if (NULL == pf)
			pf = r->h->rs[Rs_backgroundPixmapAll];

		p = pf;
		if ((p = STRCHR(p, ';')) != NULL) {
			p++;
			rxvt_scale_pixmap(r, page, p);
		}
		rxvt_load_bg_pixmap(r, page, pf);
		/* rxvt_scr_touch(r, page, True); */
	}
#endif

	XMapWindow (r->Xdisplay, PVTS(r, page)->vt);
}


/* INTPROTO */
void
rxvt_set_borderless (rxvt_t* r)
{
	Atom		prop;
	CARD32		hints;		/* KDE/GNOME hints */
	MWMHints	mwmhints;	/* Motif hints */

	hints = (CARD32) 0;
	mwmhints.flags = MWM_HINTS_DECORATIONS;
	mwmhints.decorations = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH;

	/* Motif compatible WM */
	prop = XInternAtom (r->Xdisplay, "_MOTIF_WM_HINTS", True);
	if (None != prop)
		XChangeProperty (r->Xdisplay, r->TermWin.parent, prop, prop,
			32, PropModeReplace, (unsigned char*) &mwmhints,
			PROP_MWM_HINTS_ELEMENTS);

	/* GNOME compatible WM */
	prop = XInternAtom (r->Xdisplay, "_WIN_HINTS", True);
	if (None != prop)
		XChangeProperty (r->Xdisplay, r->TermWin.parent, prop, prop,
			32, PropModeReplace, (unsigned char*) &hints, 1);

	/* KDE compatible WM */
	prop = XInternAtom (r->Xdisplay, "KWM_WIN_DECORATION", True);
	if (None != prop)
		XChangeProperty (r->Xdisplay, r->TermWin.parent, prop, prop,
			32, PropModeReplace, (unsigned char*) &hints, 1);
}


/* INTPROTO */
void
rxvt_set_desktop (rxvt_t* r, CARD32 desktop)
{
	/* GNOME compatible WM */
	if (desktop >= 0 && desktop <= 64 &&
		None != r->h->xa[XA_WIN_WORKSPACE])
		XChangeProperty(r->Xdisplay, r->TermWin.parent,
			r->h->xa[XA_WIN_WORKSPACE], XA_CARDINAL, 32,
			PropModeReplace, (unsigned char*) &desktop, 1L);

	/* WindowMaker/FreeDesktop.org compatible WM */
	if (desktop >= 0 && desktop <= 64 &&
		None != r->h->xa[XA_NET_WM_DESKTOP])
		XChangeProperty(r->Xdisplay, r->TermWin.parent, 
			r->h->xa[XA_NET_WM_DESKTOP], XA_CARDINAL, 32,
			PropModeReplace, (unsigned char*) &desktop, 1L);
}


/* EXTPROTO */
CARD32
rxvt_get_desktop (rxvt_t* r)
{
	int		result;
	Atom	ret_type;
	int		format;
	unsigned long	nitems;
	unsigned long	bytes_after;
	long*	cardinals;
	CARD32	desktop;

	if (None == r->h->xa[XA_NET_WM_DESKTOP])
		return 0;

	result = XGetWindowProperty (r->Xdisplay, r->TermWin.parent,
				r->h->xa[XA_NET_WM_DESKTOP], 0L, LONG_MAX, False,
				XA_CARDINAL, &ret_type, &format, &nitems,
				&bytes_after, (unsigned char**) &cardinals);
	if (Success != result)
		return 0;
	if (XA_CARDINAL != ret_type || 0 == format)
		return 0;

	desktop = (CARD32) cardinals[0];
	if (desktop < 0 || desktop > 64)
		desktop = 0;

	XFree (cardinals);
	return desktop;
}


/*----------------------------------------------------------------------*/
/* rxvt_create_show_windows() - Open and map the window */
/* EXTPROTO */
void
rxvt_create_show_windows(rxvt_t* r, int argc, const char *const *argv)
{
	XClassHint				class_hint;
	XWMHints				wm_hint;
	XTextProperty			win_prop;
	XTextProperty			icon_prop;
	XGCValues				gcvalue;
	unsigned long			gcmask;
#ifndef NO_FRILLS
	CARD32					pid = (CARD32) getpid ();
#endif
#ifdef TRANSPARENT
	register int			i;
#endif
#ifdef POINTER_BLANK
	static const XColor		blackcolour = { 0, 0, 0, 0, 0, 0 };
#endif

#ifdef PREFER_24BIT
	XSetWindowAttributes	attributes;
	XWindowAttributes		gattr;


	XCMAP = DefaultColormap(r->Xdisplay, XSCREEN);
	XVISUAL = DefaultVisual(r->Xdisplay, XSCREEN);

	if (r->Options & Opt_transparent) {
		XGetWindowAttributes(r->Xdisplay,
			RootWindow(r->Xdisplay, XSCREEN), &gattr);
		XDEPTH = gattr.depth;
	}
	else {
		XDEPTH = DefaultDepth(r->Xdisplay, XSCREEN);
		/*
		** If depth is not 24, look for a 24bit visual.
		*/
		if (XDEPTH != 24) {
			XVisualInfo	 vinfo;

			if (XMatchVisualInfo(r->Xdisplay, XSCREEN, 24, TrueColor,
				&vinfo)) {
				XDEPTH = 24;
				XVISUAL = vinfo.visual;
				XCMAP = XCreateColormap(r->Xdisplay,
							RootWindow(r->Xdisplay, XSCREEN),
							XVISUAL, AllocNone);
			}
		}
	}
#endif


	/* grab colors before netscape does */
	rxvt_init_colors (r);

	/*
	** Initialize fonts.
	** . Always load X11 fonts since pointer_blank uses it
	** . Load XFT font after X11 fonts. If succeeds, XFT font will
	** update font width/height and be used by default
	*/
#ifdef XFT_SUPPORT
	if (r->Options & Opt_xft)	{
		if (!rxvt_init_font_xft (r))	{
			DBG_MSG (1, (stderr, "Failed to load FreeType font, fallback to X11 font\n"));
			/* disable xft */
			r->Options &= ~Opt_xft;
		}
	}
#endif
	/* init fallback X11 font */
	rxvt_init_font_x11 (r);


	/*
	** must initialize scrollbar before initialize window size and
	** create windows.
	*/
#ifdef HAVE_SCROLLBARS
	rxvt_scrollbar_init (r);
#endif
	rxvt_init_win_size (r);

	/*
	** parent window - reverse video so we can see placement errors
	** sub-window placement & size in rxvt_resize_subwindows()
	*/

#ifdef PREFER_24BIT
	attributes.background_pixel = r->PixColors[Color_bg];
	attributes.border_pixel = r->PixColors[Color_border];
	attributes.colormap = XCMAP;
	r->TermWin.parent = XCreateWindow(r->Xdisplay, XROOT,
					r->szHint.x, r->szHint.y,
					r->szHint.width, r->szHint.height,
					r->TermWin.ext_bwidth,
					XDEPTH, InputOutput,
					XVISUAL,
					CWBackPixel | CWBorderPixel
					| CWColormap, &attributes);
#else
	r->TermWin.parent = XCreateSimpleWindow(r->Xdisplay, XROOT,
						r->szHint.x, r->szHint.y,
						r->szHint.width,
						r->szHint.height,
						r->TermWin.ext_bwidth,
						r->PixColors[Color_border],
						r->PixColors[Color_bg]);
#endif
	assert (None != r->TermWin.parent);


#ifdef XFT_SUPPORT
	if (r->Options & Opt_xft)	{
		/* create XFT draw, test only */
		XftDraw*		xftdraw = XftDrawCreate (r->Xdisplay,
			r->TermWin.parent, XVISUAL, XCMAP);
		if (xftdraw)	{
			XftDrawDestroy (xftdraw);
			/* some cleanup work if successfully create xft window */
# ifdef POINTER_BLANK
			/* disable pointer blank */
			r->Options &= ~Opt_pointerBlank;
# endif
		}
	}
#endif


# ifdef HAVE_X11_SM_SMLIB_H
	if (r->Options2 & Opt2_enableSessionMgt)
		rxvt_session_init (r);
# endif


	/*
	** Now set window properties, like title, icon name and hints
	*/
	/* window title name */
	win_prop.value = (unsigned char*) r->h->rs[Rs_title];
	win_prop.nitems = STRLEN (win_prop.value);
	win_prop.encoding = XA_STRING;
	win_prop.format = 8; 
	/* icon name */
	icon_prop.value = (unsigned char*) r->h->rs[Rs_iconName];
	icon_prop.nitems = STRLEN (icon_prop.value);
	icon_prop.encoding = XA_STRING;
	icon_prop.format = 8; 
	/* window manager hints */
	wm_hint.flags = (InputHint | StateHint | WindowGroupHint);
	wm_hint.input = True;
	wm_hint.initial_state = (r->Options & Opt_iconic ? IconicState
							: NormalState);
	wm_hint.window_group = r->TermWin.parent;
	/* class hints */
	class_hint.res_name = (char*) r->h->rs[Rs_name];
	class_hint.res_class = (char*) APL_CLASS;
	XSetWMProperties (r->Xdisplay, r->TermWin.parent,
		&win_prop, &icon_prop, (char**)argv, argc,
		&r->szHint, &wm_hint, &class_hint);

	/* set terminal title */
	rxvt_set_term_title (r, win_prop.value);
	/* set icon title */
	rxvt_set_icon_name (r, icon_prop.value);
	/* command line */
	XSetCommand (r->Xdisplay, r->TermWin.parent, (char**) argv, argc);

	/* override redirect */
	if (r->Options2 & Opt2_overrideRedirect)	{
		XSetWindowAttributes	attrib;
		attrib.override_redirect = True;
		XChangeWindowAttributes(r->Xdisplay, r->TermWin.parent,
			CWOverrideRedirect, &attrib);
	}

#ifndef NO_FRILLS
	XChangeProperty (r->Xdisplay, r->TermWin.parent,
		r->h->xa[XA_NET_WM_PID], XA_CARDINAL, 32,
		PropModeReplace, (unsigned char*) &pid, 1);
#endif

	if (r->Options2 & Opt2_borderLess)	{
		rxvt_set_borderless (r);
	}
	if (r->h->rs[Rs_desktop])	{
		CARD32	desktop = (CARD32) atoi (r->h->rs[Rs_desktop]);
		rxvt_set_desktop (r, desktop);
	}

	/*
	** set WM_CLIENT_LEADER property so that session management proxy
	** can handle us even session management is not enabled.
	*/
	if (None != r->h->xa[XA_WM_CLIENT_LEADER])
		XChangeProperty(r->Xdisplay, r->TermWin.parent,
			r->h->xa[XA_WM_CLIENT_LEADER], XA_WINDOW, 32,
			PropModeReplace, (unsigned char*) &(r->TermWin.parent), 1L);

# ifdef HAVE_X11_SM_SMLIB_H
	if (NULL != r->TermWin.sm_conn &&
		NULL != r->TermWin.sm_client_id &&
		STRCMP (r->TermWin.sm_client_id, ""))	{
		if (None != r->h->xa[XA_SM_CLIENT_ID])
			XChangeProperty(r->Xdisplay, r->TermWin.parent,
				r->h->xa[XA_SM_CLIENT_ID], XA_STRING, 8,
				PropModeReplace,
				(unsigned char*) r->TermWin.sm_client_id, 
				STRLEN(r->TermWin.sm_client_id));
	}
# endif	/* HAVE_X11_SM_SMLIB_H */


#ifdef TRANSPARENT
	r->TermWin.pixmap = None;	/* Initialize it to None */
	r->TermWin.parenttree[0] = r->TermWin.parent;
	for (i = 1; i < PARENT_NUMBER; i ++)
		r->TermWin.parenttree[i] = None;

	if (r->Options & Opt_transparent)	{
		XSetWindowBackgroundPixmap (r->Xdisplay, r->TermWin.parent,
			ParentRelative);
	}
#endif	/* TRANSPARENT */


	XSelectInput(r->Xdisplay, r->TermWin.parent,
			(KeyPressMask
#if defined(MOUSE_WHEEL) && defined(MOUSE_SLIP_WHEELING)
			| KeyReleaseMask
#endif
			| FocusChangeMask
#ifdef MONITOR_ENTER_LEAVE
			| EnterWindowMask | LeaveWindowMask
#endif
			| VisibilityChangeMask
			| StructureNotifyMask));


	/*
	** vt cursor: Black-on-White is standard, but this is more
	** popular
	*/
	r->term_pointer = XCreateFontCursor(r->Xdisplay, XC_xterm);
	/* scrollbar/menubar/tabbar window pointer */
	r->h->bar_pointer = XCreateFontCursor(r->Xdisplay, XC_left_ptr);

#ifdef POINTER_BLANK
	if (!(r->Options & Opt_pointerBlank))
		r->h->blank_pointer = None;
	else
		r->h->blank_pointer = XCreateGlyphCursor(r->Xdisplay,
			r->TermWin.font->fid, r->TermWin.font->fid, ' ', ' ',
			(XColor*) &blackcolour, (XColor*) &blackcolour);
#endif


	/* graphics context for the vt window */
#ifdef XFT_SUPPORT
	if (!(r->Options & Opt_xft))
#endif
	gcvalue.font = r->TermWin.font->fid;
	gcvalue.foreground = r->PixColors[Color_fg];
	gcvalue.background = r->PixColors[Color_bg];
	gcvalue.graphics_exposures = 1;
	gcmask = GCForeground | GCBackground | GCGraphicsExposures;
#ifdef XFT_SUPPORT
	if (!(r->Options & Opt_xft))
#endif
	gcmask |= GCFont;
	r->TermWin.gc = XCreateGC(r->Xdisplay, r->TermWin.parent,
					gcmask, &gcvalue);

#ifdef HAVE_SCROLLBARS
	rxvt_scrollbar_create (r);
	if (r->Options & Opt_scrollBar)	{
		rxvt_scrollbar_show (r);
	}
#endif
#ifdef HAVE_MENUBAR
	rxvt_menubar_create (r);
# if (MENUBAR_MAX)
	if (r->Options & Opt_showMenu)
		rxvt_menubar_show (r);
	if (r->h->rs[Rs_menu])	{
		rxvt_menubar_load_file (r, r->h->rs[Rs_menu]);
	}
# endif
#endif
	rxvt_tabbar_create (r);
	if (!(r->Options2 & Opt2_hideTabbar))
		rxvt_tabbar_show (r);

	XMapWindow (r->Xdisplay, r->TermWin.parent);
}

/*----------------------------------------------------------------------*/
/*
 * Run the command in a subprocess and return a file descriptor for the
 * master end of the pseudo-teletype pair with the command talking to
 * the slave.
 */
/* EXTPROTO */
int
rxvt_run_command(rxvt_t* r, int page, const char** argv)
{
	int			cfd, er;
	int			i;

	/* get master (pty) */
	if ((cfd = rxvt_get_pty(&(PVTS(r, page)->tty_fd),
		(char**) &(PVTS(r, page)->ttydev))) < 0) {
		rxvt_print_error("can't open pseudo-tty");
		return -1;
	}
#ifdef FD_SETSIZE
	if (r->Xfd > FD_SETSIZE || cfd > FD_SETSIZE) {
		rxvt_print_error("fd too high: %d max", FD_SETSIZE);
		return -1;
	}
#endif
	fcntl(cfd, F_SETFL, O_NDELAY);

	/* get slave (tty) */
	if (PVTS(r, page)->tty_fd < 0) {
#if !defined(NO_SETOWNER_TTYDEV) && !defined(OS_CYGWIN)
		rxvt_privileged_ttydev(r, page, SAVE);
#endif
		if ((PVTS(r, page)->tty_fd = rxvt_get_tty(PVTS(r, page)->ttydev)) < 0) {
			close(cfd);
			rxvt_print_error("can't open slave tty %s", PVTS(r, page)->ttydev);
			return -1;
		}
	}

	/* Get tty mode before fork */
#ifndef NO_BACKSPACE_KEY
	if (r->h->key_backspace[0] && !r->h->key_backspace[1])
		er = r->h->key_backspace[0];
	else if (STRCMP(r->h->key_backspace, "DEC") == 0)
		er = '\177';	/* the initial state anyway */
	else
#endif
		er = -1;
	rxvt_get_ttymode(&(PVTS(r, page)->tio), er);


	DBG_MSG(1,(stderr, "argv = 0x%x\n", (unsigned int) argv));
#ifndef __QNX__
/* spin off the command interpreter */
	switch (PVTS(r, page)->cmd_pid = fork()) {
	case -1:
		rxvt_print_error("can't fork");
		return -1;
	case 0:
		/*
		** To debug the child, follow these steps:
		** . enable sleep in the following
		** . launch gdb, set breakpoint before fork
		** . run the program, step over fork, then get child pid
		** . launch another gdb, attach to child process via pid
		** . in child's gdb, set breakpoint after sleep
		** . run 'continue' in child's gdb, debug child process
		*/
		/* sleep(10);	*/
		/*
		** Close all file descriptors except PVTS(r, page)->tty_fd
		** and STDERR
		*/
		for (i = STDERR_FILENO + 1; i < r->num_fds; i ++)
			if (i != PVTS(r, page)->tty_fd)
				close (i);

		if (rxvt_control_tty(PVTS(r, page)->tty_fd, PVTS(r, page)->ttydev) < 0)
			rxvt_print_error("could not obtain control of tty");
		else {
		/* Reopen stdin, stdout and stderr over the tty file descriptor */
			dup2(PVTS(r, page)->tty_fd, STDIN_FILENO);
			dup2(PVTS(r, page)->tty_fd, STDOUT_FILENO);
			dup2(PVTS(r, page)->tty_fd, STDERR_FILENO);
			if (PVTS(r, page)->tty_fd > 2)
				close(PVTS(r, page)->tty_fd);
			rxvt_run_child(r, page, argv);
		}
		return -1;
		/* exit(EXIT_FAILURE); */
		/* NOTREACHED */
	default:
		{
#if defined(HAVE_STRUCT_UTMP) && defined(HAVE_TTYSLOT)
			int			fdstdin;

			fdstdin = dup(STDIN_FILENO);
			dup2(PVTS(r, page)->tty_fd, STDIN_FILENO);
#endif
#ifdef UTMP_SUPPORT
# ifdef UTEMPTER_SUPPORT
			/* utempter hack, it needs cmd_fd */
			PVTS(r, page)->cmd_fd = cfd;
# endif
			rxvt_privileged_utmp(r, page, SAVE);
# ifdef UTEMPTER_SUPPORT
			/* utempter hack, restore cmd_fd */
			PVTS(r, page)->cmd_fd = -1;
# endif
#endif
#if defined(HAVE_STRUCT_UTMP) && defined(HAVE_TTYSLOT)
			dup2(fdstdin, STDIN_FILENO);
			close(fdstdin);
#endif
		}
		/* keep STDERR_FILENO, PVTS(r, page)->cmd_fd, r->Xfd open */
		close(PVTS(r, page)->tty_fd);
		PVTS(r, page)->tty_fd = -1;
		break;
	}
#else				/* __QNX__ uses qnxspawn() */
	fchmod(PVTS(r, page)->tty_fd, 0622);
	fcntl(PVTS(r, page)->tty_fd, F_SETFD, FD_CLOEXEC);
	fcntl(cfd, F_SETFD, FD_CLOEXEC);

	if (rxvt_run_child(r, page, argv) == -1)
		/*exit(EXIT_FAILURE);*/
		return -1;
#endif

	return cfd;
}

/* ------------------------------------------------------------------------- *
 *							CHILD PROCESS OPERATIONS						*
 * ------------------------------------------------------------------------- */
/*
 * The only open file descriptor is the slave tty - so no error messages.
 * returns are fatal
 */
/* INTPROTO */
int
rxvt_run_child(rxvt_t* r, int page, const char **argv)
{
#ifdef SIGTSTP
	struct sigaction	ignore;
#endif
	struct sigaction	deflt;
	char*				login;


	/* DBG_MSG(1,(stderr, "argv = %x\n", argv)); */

	/* init terminal attributes */
	SET_TTYMODE(STDIN_FILENO, &(PVTS(r, page)->tio));

	if (r->Options & Opt_console) {	/* be virtual console, fail silently */
#ifdef TIOCCONS
		unsigned int	on = 1;
		ioctl(STDIN_FILENO, TIOCCONS, &on);
#elif defined (SRIOCSREDIR)
		int			fd;
		fd = open(CONSOLE, O_WRONLY, 0);
		if (fd >= 0) {
			if (ioctl(fd, SRIOCSREDIR, NULL) < 0)
			close(fd);
		}
#endif	/* SRIOCSREDIR */
	}

	/* reset signals and spin off the command interpreter */
	deflt.sa_handler = SIG_DFL;
	deflt.sa_flags = 0;
	sigemptyset (&deflt.sa_mask);
	sigaction (SIGINT, &deflt, NULL);
	sigaction (SIGQUIT, &deflt, NULL);
	sigaction (SIGCHLD, &deflt, NULL);
	/*
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
	signal(SIGCHLD, SIG_DFL);
	*/

	/*
	** mimick login's behavior by disabling the job control signals
	** a shell that wants them can turn them back on
	*/
#ifdef SIGTSTP
	ignore.sa_handler = SIG_IGN;
	ignore.sa_flags = 0;
	sigemptyset (&ignore.sa_mask);
	sigaction (SIGTSTP, &ignore, NULL);
	sigaction (SIGTTIN, &ignore, NULL);
	sigaction (SIGTTOU, &ignore, NULL);
	/*
	signal(SIGTSTP, SIG_IGN);
	signal(SIGTTIN, SIG_IGN);
	signal(SIGTTOU, SIG_IGN);
	*/
#endif	/* SIGTSTP */

	/* set window size */
	rxvt_tt_winsize(STDIN_FILENO, r->TermWin.ncol, r->TermWin.nrow, 0);

#ifndef __QNX__
/* command interpreter path */
	if (argv != NULL) {
# ifdef DEBUG_VERBOSE
		int			i;
		for (i = 0; argv[i]; i++)
			DBG_MSG(2,(stderr, "argv [%d] = \"%s\"\n", i, argv[i]));
# endif
		execvp(argv[0], (char *const *)argv);
		/* no error message: STDERR is closed! */
	}
	else {
		const char	 *argv0, *shell;

		if (NULL == (shell = getenv("SHELL")) ||
			((char) 0 == *shell))	{
# ifdef HAVE_GETPWUID
			struct passwd* pwent = getpwuid (getuid ());
			if ((NULL == pwent) ||
				(NULL == (shell = pwent->pw_shell)) ||
				((char) 0 == *shell))
# endif	/* HAVE_GETPWUID */
			shell = "/bin/sh";
		}

		argv0 = (const char *)rxvt_r_basename(shell);
		if (r->Options & Opt_loginShell) {
			int		l = STRLEN(argv0) + 2;
			if (l <= 0 || l > 4096)	/* possible integer overflow */
				l = 4096;
			login = rxvt_malloc(l * sizeof(char));
	
			login[0] = '-';
			STRNCPY(&login[1], argv0, l-2);
			login[l-1] = (char) 0;
			argv0 = login;
		}
		execlp(shell, argv0, NULL);
		/* no error message: STDERR is closed! */
	}
#else				/* __QNX__ uses qnxspawn() */
	{
		char	iov_a[10] = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
		char*	command = NULL, fullcommand[_MAX_PATH];
		char**	arg_v, *arg_a[2] = { NULL, NULL };

		if (argv != NULL) {
			if (access(argv[0], X_OK) == -1) {
				if (STRCHR(argv[0], '/') == NULL) {
					searchenv(argv[0], "PATH", fullcommand);
					if (fullcommand[0] != '\0')
						command = fullcommand;
				}
				if (access(command, X_OK) == -1)
					return -1;
			}
			else
				command = argv[0];
			arg_v = argv;
		}
		else {
			if ((command = getenv("SHELL")) == NULL || *command == '\0')
				command = "/bin/sh";

			arg_a[0] = my_basename(command);
			if (r->Options & Opt_loginShell) {
				int		l = STRLEN(arg_a[0]) + 2;
				if (l <= 0 || l > 4096)	/* possible integer overflow */
					l = 4096;
				login = rxvt_malloc(l * sizeof(char));

				login[0] = '-';
				STRNCPY(&login[1], arg_a[0], l-2);
				login[l-1] = (char) 0;
				arg_a[0] = login;
			}
			arg_v = arg_a;
		}
		iov_a[0] = iov_a[1] = iov_a[2] = PVTS(r, page)->tty_fd;
		PVTS(r, page)->cmd_pid = qnx_spawn(0, 0, 0, -1, -1,
					_SPAWN_SETSID | _SPAWN_TCSETPGRP,
					command, arg_v, environ, iov_a, 0);
		if (login)
			free(login);
		close(PVTS(r, page)->tty_fd);
		return PVTS(r, page)->cmd_fd;
	}
#endif
	return -1;
}

/* ------------------------------------------------------------------------- *
 *							GET TTY CURRENT STATE							*
 * ------------------------------------------------------------------------- */
/* rxvt_get_ttymode() */
/* INTPROTO */
void
rxvt_get_ttymode(ttymode_t *tio, int erase)
{
#ifdef HAVE_TERMIOS_H
/*
 * standard System V termios interface
 */
	if (GET_TERMIOS(STDIN_FILENO, tio) < 0) {
	/* return error - use system defaults */
	tio->c_cc[VINTR] = CINTR;
	tio->c_cc[VQUIT] = CQUIT;
	tio->c_cc[VERASE] = CERASE;
	tio->c_cc[VKILL] = CKILL;
	tio->c_cc[VSTART] = CSTART;
	tio->c_cc[VSTOP] = CSTOP;
	tio->c_cc[VSUSP] = CSUSP;
# ifdef VDSUSP
	tio->c_cc[VDSUSP] = CDSUSP;
# endif
# ifdef VREPRINT
	tio->c_cc[VREPRINT] = CRPRNT;
# endif
# ifdef VDISCRD
	tio->c_cc[VDISCRD] = CFLUSH;
# endif
# ifdef VWERSE
	tio->c_cc[VWERSE] = CWERASE;
# endif
# ifdef VLNEXT
	tio->c_cc[VLNEXT] = CLNEXT;
# endif
	}
	tio->c_cc[VEOF] = CEOF;
	tio->c_cc[VEOL] = VDISABLE;
# ifdef VEOL2
	tio->c_cc[VEOL2] = VDISABLE;
# endif
# ifdef VSWTC
	tio->c_cc[VSWTC] = VDISABLE;
# endif
# ifdef VSWTCH
	tio->c_cc[VSWTCH] = VDISABLE;
# endif
# if VMIN != VEOF
	tio->c_cc[VMIN] = 1;
# endif
# if VTIME != VEOL
	tio->c_cc[VTIME] = 0;
# endif
	if (erase != -1)
	tio->c_cc[VERASE] = (char)erase;

/* input modes */
	tio->c_iflag = (BRKINT | IGNPAR | ICRNL
# ifdef IMAXBEL
					| IMAXBEL
# endif
					| IXON);

/* output modes */
	tio->c_oflag = (OPOST | ONLCR);

/* control modes */
	tio->c_cflag = (CS8 | CREAD);

/* line discipline modes */
	tio->c_lflag = (ISIG | ICANON | IEXTEN | ECHO
# if defined (ECHOCTL) && defined (ECHOKE)
					| ECHOCTL | ECHOKE
# endif
					| ECHOE | ECHOK);
# else				/* HAVE_TERMIOS_H */

/*
 * sgtty interface
 */

/* get parameters -- gtty */
	if (ioctl(STDIN_FILENO, TIOCGETP, &(tio->sg)) < 0) {
		tio->sg.sg_erase = CERASE;	/* ^H */
		tio->sg.sg_kill = CKILL;	/* ^U */
	}
	if (erase != -1)
		tio->sg.sg_erase = (char)erase;

	tio->sg.sg_flags = (CRMOD | ECHO | EVENP | ODDP);

/* get special characters */
	if (ioctl(STDIN_FILENO, TIOCGETC, &(tio->tc)) < 0) {
		tio->tc.t_intrc = CINTR;	/* ^C */
		tio->tc.t_quitc = CQUIT;	/* ^\ */
		tio->tc.t_startc = CSTART;	/* ^Q */
		tio->tc.t_stopc = CSTOP;	/* ^S */
		tio->tc.t_eofc = CEOF;	/* ^D */
		tio->tc.t_brkc = -1;
	}
/* get local special chars */
	if (ioctl(STDIN_FILENO, TIOCGLTC, &(tio->lc)) < 0) {
		tio->lc.t_suspc = CSUSP;	/* ^Z */
		tio->lc.t_dsuspc = CDSUSP;	/* ^Y */
		tio->lc.t_rprntc = CRPRNT;	/* ^R */
		tio->lc.t_flushc = CFLUSH;	/* ^O */
		tio->lc.t_werasc = CWERASE;	/* ^W */
		tio->lc.t_lnextc = CLNEXT;	/* ^V */
	}
/* get line discipline */
	ioctl(STDIN_FILENO, TIOCGETD, &(tio->line));
# ifdef NTTYDISC
	tio->line = NTTYDISC;
# endif				/* NTTYDISC */
	tio->local = (LCRTBS | LCRTERA | LCTLECH | LPASS8 | LCRTKIL);
#endif				/* HAVE_TERMIOS_H */

/*
 * Debugging
 */
#ifdef DEBUG_TTYMODE
#ifdef HAVE_TERMIOS_H
/* c_iflag bits */
	fprintf(stderr, "Input flags\n");

/* cpp token stringize doesn't work on all machines <sigh> */
# define FOO(flag,name)			\
	if ((tio->c_iflag) & flag)		\
	fprintf (stderr, "%s ", name)

/* c_iflag bits */
	FOO(IGNBRK, "IGNBRK");
	FOO(BRKINT, "BRKINT");
	FOO(IGNPAR, "IGNPAR");
	FOO(PARMRK, "PARMRK");
	FOO(INPCK, "INPCK");
	FOO(ISTRIP, "ISTRIP");
	FOO(INLCR, "INLCR");
	FOO(IGNCR, "IGNCR");
	FOO(ICRNL, "ICRNL");
	FOO(IXON, "IXON");
	FOO(IXOFF, "IXOFF");
# ifdef IUCLC
	FOO(IUCLC, "IUCLC");
# endif
# ifdef IXANY
	FOO(IXANY, "IXANY");
# endif
# ifdef IMAXBEL
	FOO(IMAXBEL, "IMAXBEL");
# endif
	fprintf(stderr, "\n");

# undef FOO
# define FOO(entry, name)					\
	fprintf(stderr, "%-8s = %#04o\n", name, tio->c_cc [entry])

	FOO(VINTR, "VINTR");
	FOO(VQUIT, "VQUIT");
	FOO(VERASE, "VERASE");
	FOO(VKILL, "VKILL");
	FOO(VEOF, "VEOF");
	FOO(VEOL, "VEOL");
# ifdef VEOL2
	FOO(VEOL2, "VEOL2");
# endif
# ifdef VSWTC
	FOO(VSWTC, "VSWTC");
# endif
# ifdef VSWTCH
	FOO(VSWTCH, "VSWTCH");
# endif
	FOO(VSTART, "VSTART");
	FOO(VSTOP, "VSTOP");
	FOO(VSUSP, "VSUSP");
# ifdef VDSUSP
	FOO(VDSUSP, "VDSUSP");
# endif
# ifdef VREPRINT
	FOO(VREPRINT, "VREPRINT");
# endif
# ifdef VDISCRD
	FOO(VDISCRD, "VDISCRD");
# endif
# ifdef VWERSE
	FOO(VWERSE, "VWERSE");
# endif
# ifdef VLNEXT
	FOO(VLNEXT, "VLNEXT");
# endif
	fprintf(stderr, "\n");
# undef FOO
# endif				/* HAVE_TERMIOS_H */
#endif				/* DEBUG_TTYMODE */
}

/*----------------------- end-of-file (C source) -----------------------*/
