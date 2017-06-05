 /*--------------------------------*-C-*---------------------------------*
 * File:	scrollbar-sgi.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1998        Alfredo K. Kojima <kojima@windowmaker.org>
 * Copyright (c) 1998        Sasha Vasko
 * Copyright (c) 2000        Frank Everdij <frank@ff-net.demon.nl>
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
** $Id: scrollbar-sgi.c,v 1.9 2005/03/21 00:42:08 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"


#ifdef DEBUG_VERBOSE
#define DEBUG_LEVEL	1
#else
#define DEBUG_LEVEL	0
#endif

#if DEBUG_LEVEL
#define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
#define DBG_MSG(d,x)
#endif


/*----------------------------------------------------------------------*/
#ifdef HAVE_SCROLLBARS
#ifdef SGI_SCROLLBAR

#define CHOOSE_GC_FG(R, PIXCOL)	\
	XSetForeground ((R)->Xdisplay, (R)->scrollBar.gc, (PIXCOL))


static char *SCROLLER_DIMPLE[] = {
	"            ",
	"############",
	".==========%",
	".==========%",
	"            ",
	"############",
	".==========%",
	".==========%",
	"            ",
	"############"
};

#define SCROLLER_DIMPLE_WIDTH   12
#define SCROLLER_DIMPLE_HEIGHT  10



static char *SCROLLER_ARROW_UP[] = {
	"============",
	"============",
	"=====*======",
	"=====**=====",
	"=====**=====",
	"====****====",
	"====****====",
	"===******===",
	"===******===",
	"==********==",
	"==********==",
	"============",
	"============",
	"============"
};

static char *SCROLLER_ARROW_DOWN[] = {
	"============",
	"============",
	"=*********==",
	"==********==",
	"==********==",
	"===******===",
	"===******===",
	"====****====",
	"====****====",
	"=====**=====",
	"=====**=====",
	"============",
	"============",
	"============"
};



static char *LO_SCROLLER_ARROW_UP[] = {
	"============",
	"============",
	"=====-======",
	"=====--=====",
	"=====--=====",
	"====----====",
	"====----====",
	"===------===",
	"===------===",
	"==--------==",
	"==--------==",
	"============",
	"============",
	"============"
};

static char *LO_SCROLLER_ARROW_DOWN[] = {
	"============",
	"============",
	"=---------==",
	"==--------==",
	"==--------==",
	"===------===",
	"===------===",
	"====----====",
	"====----====",
	"=====--=====",
	"=====--=====",
	"============",
	"============",
	"============"
};



static char *HI_SCROLLER_ARROW_UP[] = {
	"            ",
	"            ",
	"     %      ",
	"     %%     ",
	"     %%     ",
	"    %%%%    ",
	"    %%%%    ",
	"   %%%%%%   ",
	"   %%%%%%   ",
	"  %%%%%%%%  ",
	"  %%%%%%%%  ",
	"            ",
	"            ",
	"            "
};

static char *HI_SCROLLER_ARROW_DOWN[] = {
	"            ",
	"            ",
	" %%%%%%%%%  ",
	"  %%%%%%%%  ",
	"  %%%%%%%%  ",
	"   %%%%%%   ",
	"   %%%%%%   ",
	"    %%%%    ",
	"    %%%%    ",
	"     %%     ",
	"     %%     ",
	"            ",
	"            ",
	"            "
};

#define ARROW_SOURCE_WIDTH   12
#define ARROW_SOURCE_HEIGHT  14

#ifndef SCROLL_SHADOW_HEIGHT
# define SCROLL_SHADOW_HEIGHT 1
#endif
/* end unconfigurable stuff */


#define stp_width 8
#define stp_height 8
static unsigned char stp_bits[] = {
   0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa};


/* INTPROTO */
static Pixmap
create_icon (rxvt_t* r, char **data, int sx, int sy, unsigned int width, unsigned int height)
{
    register int	i, k;
	register int	x, y;
	unsigned long	pixel;
	Pixmap			pixmap;

    
	pixmap = XCreatePixmap (r->Xdisplay, r->scrollBar.win,
				width, height, XDEPTH);
	if (None == pixmap)
		return None;

    y = sy;
    for (i = 0; i < height ; y++, i++ ) {
		x = sx ;
        for (k = 0; k < width ; k++, x++ ) {
			switch (data[y][x]) {               
			case ' ':
			case 'w':
				pixel = r->scrollBar.sgi_white;  break;
			case '.':
			case 'l':
				pixel = r->scrollBar.sgi_fg;  break;
			case '-':
			case 'g':
				pixel = r->scrollBar.sgi_lmedium;  break;
			case '%':
			case 'd':
				pixel = r->scrollBar.sgi_dark;  break;
			case '*':
			case 'v':
				pixel = r->scrollBar.sgi_vdark;  break;
			case '#':
			case 'b':
				pixel = r->scrollBar.sgi_black;  break;
			case '=':
			case 'm':
			default:
				pixel = r->scrollBar.sgi_bg;
				break;
			}
			CHOOSE_GC_FG(r, pixel);
			XDrawPoint (r->Xdisplay, pixmap, r->scrollBar.gc, k, i);
        }
    }

	return (pixmap);
}



/* Draw bevel & arrows */
/* INTPROTO */
static void
sgi_draw_bevel (rxvt_t* r, Drawable d, int x, int y, int w, int h)
{
	CHOOSE_GC_FG(r, r->scrollBar.sgi_fg);
    XDrawLine(r->Xdisplay, d, r->scrollBar.gc, x, y, x+w, y);
    XDrawLine(r->Xdisplay, d, r->scrollBar.gc, x, y, x, y+h);

    XDrawLine(r->Xdisplay, d, r->scrollBar.gc, x+1, y+1, x+w-1, y+1);
    XDrawLine(r->Xdisplay, d, r->scrollBar.gc, x+1, y+1, x+1, y+h-1);

	CHOOSE_GC_FG(r, r->scrollBar.sgi_dark);
    XDrawLine(r->Xdisplay, d, r->scrollBar.gc, x+w, y+1, x+w, y+h);
    XDrawLine(r->Xdisplay, d, r->scrollBar.gc, x+1, y+h, x+w, y+h);

    XDrawLine(r->Xdisplay, d, r->scrollBar.gc, x+w-1, y+2, x+w-1, y+h-1);
    XDrawLine(r->Xdisplay, d, r->scrollBar.gc, x+2, y+h-1, x+w-1, y+h-1);
}


/* INTPROTO */
static void
scrollbar_fill_back (rxvt_t* r, unsigned int height)
{
	/* fill background */
	XClearArea (r->Xdisplay, r->scrollBar.win,
		0, SGI_SB_BUTTON_HEIGHT,
		SB_WIDTH_SGI, height, False);

	/* fill top button space */
	CHOOSE_GC_FG(r, r->scrollBar.sgi_fg);
    XFillRectangle (r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		0, 0,
		SB_WIDTH_SGI, SGI_SB_BUTTON_HEIGHT);
	/* fill bottom button space */
    XFillRectangle (r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		0, height+SGI_SB_BUTTON_HEIGHT,
		SB_WIDTH_SGI, SGI_SB_BUTTON_HEIGHT);

	CHOOSE_GC_FG(r, r->scrollBar.sgi_dark);
	/* right vertical border line */
	XDrawLine (r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		SB_WIDTH_SGI-1, 0,
		SB_WIDTH_SGI-1, height+SGI_SB_BUTTONS_HEIGHT);
	/* bottom horizontal line */
	XDrawLine (r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		2, height+SGI_SB_BUTTONS_HEIGHT-1,
		SB_WIDTH_SGI, height+SGI_SB_BUTTONS_HEIGHT-1);
}


/* EXTPROTO */
void 
rxvt_scrollbar_init_sgi (rxvt_t* r)
{
    XGCValues		gcvalue;
	unsigned long	gcmask;
    Pixmap			stipple;
    XColor			xcol;
    unsigned int	x_offset, y_offset;


	/* Initialize colors */
    r->scrollBar.sgi_black =
		BlackPixelOfScreen(DefaultScreenOfDisplay(r->Xdisplay));
    r->scrollBar.sgi_white =
		WhitePixelOfScreen(DefaultScreenOfDisplay(r->Xdisplay));
	/* alloc light gray */
    xcol.red = 0xde00;
    xcol.green = 0xde00;
    xcol.blue = 0xde00;
    if (!rxvt_alloc_color (r, &xcol, "light gray")) {
#ifndef NO_BRIGHTCOLOR
		r->scrollBar.sgi_fg = r->PixColors [Color_AntiqueWhite];
#else
		r->scrollBar.sgi_fg = r->PixColors [Color_White];
#endif	
    }
	else
		r->scrollBar.sgi_fg = xcol.pixel;
	/* alloc light medium gray */
    xcol.red = 0xaa00;
    xcol.green = 0xaa00;
    xcol.blue = 0xaa00;
    if (!rxvt_alloc_color (r, &xcol, "light medium gray")) {
#ifndef NO_BRIGHTCOLOR
		r->scrollBar.sgi_lmedium = r->PixColors [Color_AntiqueWhite];
#else
		r->scrollBar.sgi_lmedium = r->PixColors [Color_White];
#endif	
    }
	else
	    r->scrollBar.sgi_lmedium = xcol.pixel;
	/* alloc medium gray */
    xcol.red = 0xbd00;
    xcol.green = 0xbd00;
    xcol.blue = 0xbd00;
    if (!rxvt_alloc_color (r, &xcol, "medium gray")) {
#ifndef NO_BRIGHTCOLOR
		r->scrollBar.sgi_bg = r->PixColors [Color_Black];
#else
		r->scrollBar.sgi_bg = r->PixColors [Color_Grey50];
#endif	
    }
	else
	    r->scrollBar.sgi_bg = xcol.pixel;
	/* alloc dark gray */
    xcol.red = 0x8e00;
    xcol.green = 0x8e00;
    xcol.blue = 0x8e00;
    if (!rxvt_alloc_color (r, &xcol, "dark gray")) {
#ifndef NO_BRIGHTCOLOR
		r->scrollBar.sgi_dark = r->PixColors [Color_Grey25];
#else
		r->scrollBar.sgi_dark = r->PixColors [Color_Black];
#endif	
    }
    r->scrollBar.sgi_dark = xcol.pixel;
	/* alloc very dark gray */
    xcol.red = 0x5e00;
    xcol.green = 0x5e00;
    xcol.blue = 0x5e00;
    if (!rxvt_alloc_color (r, &xcol, "very dark gray")) {
#ifndef NO_BRIGHTCOLOR
		r->scrollBar.sgi_vdark = r->PixColors [Color_Grey25];
#else
		r->scrollBar.sgi_vdark = r->PixColors [Color_Black];
#endif	
    }
    r->scrollBar.sgi_vdark = xcol.pixel;

	/* Create GCs */
	gcvalue.foreground = r->scrollBar.sgi_white;
	gcmask = GCForeground;
	r->scrollBar.gc = XCreateGC (r->Xdisplay, r->scrollBar.win,
						gcmask, &gcvalue);

    stipple = XCreateBitmapFromData(r->Xdisplay, r->scrollBar.win,
				    (const char*) stp_bits, stp_width, stp_height);
	assert (None != stipple);
    gcvalue.foreground = r->scrollBar.sgi_dark;
    gcvalue.background = r->scrollBar.sgi_bg;
    gcvalue.fill_style = FillStippled;
    gcvalue.stipple = stipple;
	gcmask = GCForeground | GCBackground | GCStipple | GCFillStyle;
    r->scrollBar.sgi_stippleGC = XCreateGC(r->Xdisplay,
					r->scrollBar.win, gcmask, &gcvalue);
	assert (None != r->scrollBar.sgi_stippleGC);

	/* Create icons */
	r->scrollBar.sgi_dimple = create_icon (r, SCROLLER_DIMPLE,
		0, 0,
		SCROLLER_DIMPLE_WIDTH, SCROLLER_DIMPLE_HEIGHT);
	assert (None != r->scrollBar.sgi_dimple);

    x_offset = y_offset = (ARROW_SOURCE_WIDTH-SGI_ARROW_WIDTH)>>1;
	r->scrollBar.sgi_upArrow = create_icon (r, SCROLLER_ARROW_UP,
		x_offset, y_offset,
		ARROW_SOURCE_WIDTH, ARROW_SOURCE_HEIGHT);
	r->scrollBar.sgi_upArrowHi = create_icon (r, HI_SCROLLER_ARROW_UP,
		x_offset, y_offset,
		ARROW_SOURCE_WIDTH, ARROW_SOURCE_HEIGHT);
	r->scrollBar.sgi_upArrowLow = create_icon (r, LO_SCROLLER_ARROW_UP,
		x_offset, y_offset,
		ARROW_SOURCE_WIDTH, ARROW_SOURCE_HEIGHT);
	r->scrollBar.sgi_downArrow = create_icon (r, SCROLLER_ARROW_DOWN,
		x_offset, y_offset,
		ARROW_SOURCE_WIDTH, ARROW_SOURCE_HEIGHT);
	r->scrollBar.sgi_downArrowHi = create_icon (r,
		HI_SCROLLER_ARROW_DOWN,
		x_offset, y_offset,
		ARROW_SOURCE_WIDTH, ARROW_SOURCE_HEIGHT);
	r->scrollBar.sgi_downArrowLow = create_icon (r,
		LO_SCROLLER_ARROW_DOWN,
		x_offset, y_offset,
		ARROW_SOURCE_WIDTH, ARROW_SOURCE_HEIGHT);


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
			r->scrollBar.sgi_bg);
}


/* EXTPROTO */
void 
rxvt_scrollbar_exit_sgi (rxvt_t* r)
{
	if (None != r->scrollBar.sgi_stippleGC)	{
		XFreeGC (r->Xdisplay, r->scrollBar.sgi_stippleGC);
		r->scrollBar.sgi_stippleGC = None;
	}
	if (None != r->scrollBar.sgi_dimple)	{
		XFreePixmap (r->Xdisplay, r->scrollBar.sgi_dimple);
		r->scrollBar.sgi_dimple = None;
	}
	if (None != r->scrollBar.sgi_upArrow)	{
		XFreePixmap (r->Xdisplay, r->scrollBar.sgi_upArrow);
		r->scrollBar.sgi_upArrow = None;
	}
	if (None != r->scrollBar.sgi_upArrowHi)	{
		XFreePixmap (r->Xdisplay, r->scrollBar.sgi_upArrowHi);
		r->scrollBar.sgi_upArrowHi = None;
	}
	if (None != r->scrollBar.sgi_upArrowLow)	{
		XFreePixmap (r->Xdisplay, r->scrollBar.sgi_upArrowLow);
		r->scrollBar.sgi_upArrowLow = None;
	}
	if (None != r->scrollBar.sgi_downArrow)	{
		XFreePixmap (r->Xdisplay, r->scrollBar.sgi_downArrow);
		r->scrollBar.sgi_downArrow = None;
	}
	if (None != r->scrollBar.sgi_downArrowHi)	{
		XFreePixmap (r->Xdisplay, r->scrollBar.sgi_downArrowHi);
		r->scrollBar.sgi_downArrowHi = None;
	}
	if (None != r->scrollBar.sgi_downArrowLow)	{
		XFreePixmap (r->Xdisplay, r->scrollBar.sgi_downArrowLow);
		r->scrollBar.sgi_downArrowLow = None;
	}
}


/* EXTPROTO */
int
rxvt_scrollbar_show_sgi (rxvt_t* r, int update, int last_top, int last_bot, int scroller_len)
{
	register int	new_height = 
			r->scrollBar.end + SGI_SB_BUTTON_HEIGHT + 
			((r->sb_shadow) << 1);


	if (!update)
		scrollbar_fill_back (r, r->scrollBar.end);

    if (0 == AVTS(r)->nscrolled) {
		/* top button */
		XCopyArea (r->Xdisplay, r->scrollBar.sgi_upArrowLow,
			r->scrollBar.win, r->scrollBar.gc,
			/* src x, y, width, height */
			0, 0, SGI_ARROW_WIDTH, SGI_ARROW_HEIGHT,
			/* dst x, y */
			((SB_WIDTH_SGI)>>1)-(SGI_ARROW_WIDTH>>1)+1,
			SGI_BEVEL_SIZE);
		/* bottom button */
		XCopyArea (r->Xdisplay, r->scrollBar.sgi_downArrowLow,
			r->scrollBar.win, r->scrollBar.gc,
			/* src x, y, width, height */
			0, 0, SGI_ARROW_WIDTH, SGI_ARROW_HEIGHT,
			/* dst x, y */
			((SB_WIDTH_SGI)>>1)-(SGI_ARROW_WIDTH>>1)+1,
			new_height - SGI_ARROW_HEIGHT - SGI_BEVEL_SIZE);
    }
    else	{
		/* (AVTS(r)->nscrolled > 0) */
		DBG_MSG(2, (stderr, "top=%d, bot=%d, last_top=%d, last_bot=%d\n", r->scrollBar.top, r->scrollBar.bot, last_top, last_bot));
		if (last_top < r->scrollBar.top)
			XClearArea (r->Xdisplay, r->scrollBar.win,
			0, last_top,
			SB_WIDTH_SGI-1, (last_top - r->scrollBar.top), False);

		if (r->scrollBar.bot < last_bot)
			XClearArea (r->Xdisplay, r->scrollBar.win,
			0, r->scrollBar.bot,
			SB_WIDTH_SGI-1, (last_bot - r->scrollBar.bot + 1), False);

		XClearArea (r->Xdisplay, r->scrollBar.win,
			0, r->scrollBar.top,
			SB_WIDTH_SGI-1, scroller_len, False);

		/* scroller frame */
		CHOOSE_GC_FG(r, r->scrollBar.sgi_vdark);
		XDrawRectangle (r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
			0, r->scrollBar.top,
			SB_WIDTH_SGI-1, scroller_len);

		/* bevel of scroller, inside scroller frame */
		sgi_draw_bevel (r, r->scrollBar.win,
			1, r->scrollBar.top+1,
			SB_WIDTH_SGI-3, scroller_len-2);
		/*
		sgi_draw_bevel (r, r->scrollBar.win,
			0, 0,
			SB_WIDTH_SGI-1, SGI_SB_BUTTON_HEIGHT);
	    sgi_draw_bevel (r, r->scrollBar.win,
			0, new_height-SGI_SB_BUTTON_HEIGHT, 
			SB_WIDTH_SGI-1, SGI_SB_BUTTON_HEIGHT);
		*/
	
		/* dimple */
		XCopyArea (r->Xdisplay, r->scrollBar.sgi_dimple,
			r->scrollBar.win, r->scrollBar.gc,
			0, 0,
			SCROLLER_DIMPLE_WIDTH, SCROLLER_DIMPLE_HEIGHT,
			((SB_WIDTH_SGI)>>1)-(SCROLLER_DIMPLE_WIDTH>>1)+1,
			r->scrollBar.top + ((scroller_len-SCROLLER_DIMPLE_HEIGHT)>>1));
	
		/* top button */
		XCopyArea (r->Xdisplay, (scrollbar_isUp()) ?
			r->scrollBar.sgi_upArrowHi : r->scrollBar.sgi_upArrow,
			r->scrollBar.win, r->scrollBar.gc,
			/* src x, y, width, height */
			0, 0, SGI_ARROW_WIDTH, SGI_ARROW_HEIGHT,
			/* dst x, y */
			((SB_WIDTH_SGI)>>1)-(SGI_ARROW_WIDTH>>1)+1,
			SGI_BEVEL_SIZE);

		/* bottom button */
		XCopyArea (r->Xdisplay, (scrollbar_isDn()) ?
			r->scrollBar.sgi_downArrowHi : r->scrollBar.sgi_downArrow,
			r->scrollBar.win, r->scrollBar.gc,
			/* src x, y, width, height */
			0, 0, SGI_ARROW_WIDTH, SGI_ARROW_HEIGHT,
			/* dst x, y */
			((SB_WIDTH_SGI)>>1)-(SGI_ARROW_WIDTH>>1)+1,
			new_height - SGI_ARROW_HEIGHT - SGI_BEVEL_SIZE);
    }

	/* bottom line of top button */
	CHOOSE_GC_FG(r, r->scrollBar.sgi_fg);
	XDrawLine (r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		0, SGI_SB_BUTTON_HEIGHT-1,
		SB_WIDTH_SGI-1, SGI_SB_BUTTON_HEIGHT-1);
	/* top line of bottom button */
	XDrawLine (r->Xdisplay, r->scrollBar.win, r->scrollBar.gc,
		0, new_height-SGI_SB_BUTTON_HEIGHT,
		SB_WIDTH_SGI-1, new_height-SGI_SB_BUTTON_HEIGHT);
    
    return 1;
}

#endif	/* SGI_SCROLLBAR */
#endif	/* HAVE_SCROLLBARS */
/*----------------------- end-of-file (C source) -----------------------*/
