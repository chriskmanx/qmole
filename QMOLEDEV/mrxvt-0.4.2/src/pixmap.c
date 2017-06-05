/*--------------------------------*-C-*---------------------------------*
 * File:	pixmap.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1997        Carsten Haitzler <raster@zip.com.au>
 * Copyright (c) 1997,1998   Oezguer Kesim <kesim@math.fu-berlin.de>
 * Copyright (c) 1998-2001   Geoff Wing <gcw@pobox.com>
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
** $Id: pixmap.c,v 1.24 2004/11/20 21:33:15 cvs Exp $
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


#ifdef BACKGROUND_IMAGE

/*--------------------------------------------------------------------*
 *         BEGIN `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/
void rxvt_pixmap_incr (unsigned int*, unsigned int*, float*, float*, unsigned int, unsigned int);
/*--------------------------------------------------------------------*
 *         END   `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/



/*
 * These GEOM strings indicate absolute size/position:
 * @ `WxH+X+Y'
 * @ `WxH+X'	-> Y = X
 * @ `WxH'	  -> Y = X = 50
 * @ `W+X+Y'	-> H = W
 * @ `W+X'	  -> H = W, Y = X
 * @ `W'		-> H = W, X = Y = 50
 * @ `0xH'	  -> H *= H/100, X = Y = 50 (W unchanged)
 * @ `Wx0'	  -> W *= W/100, X = Y = 50 (H unchanged)
 * @ `=+X+Y'	-> (H, W unchanged)
 * @ `=+X'	  -> Y = X (H, W unchanged)
 *
 * These GEOM strings adjust position relative to current position:
 * @ `+X+Y'
 * @ `+X'	   -> Y = X
 *
 * And this GEOM string is for querying current scale/position:
 * @ `?'
 */
/* EXTPROTO */
int
rxvt_scale_pixmap(rxvt_t *r, int page, const char *geom)
{
	int			 flags, changed = 0;
	int			 x = 0, y = 0;
	unsigned int	w = 0, h = 0;
	unsigned int	n;
	char		   *p, *str;
	bgPixmap_t	 *bgpixmap = &(PVTS(r, page)->bg);

#define MAXLEN_GEOM		sizeof("[1000x1000+1000+1000]")

	if (geom == NULL)
		return 0;
	str = rxvt_malloc(MAXLEN_GEOM + 1);
	if (!STRCMP(geom, "?")) {
		sprintf(str, "[%dx%d+%d+%d]",	/* can't presume snprintf() ! */
			min(bgpixmap->w, 9999), min(bgpixmap->h, 9999),
			min(bgpixmap->x, 9999), min(bgpixmap->y, 9999));
		rxvt_xterm_seq(r, page, XTerm_title, str, CHAR_ST);
		free(str);
		return 0;
	}

	if ((p = STRCHR(geom, ';')) == NULL)
	p = STRCHR(geom, '\0');
	n = (p - geom);
	if (n <= MAXLEN_GEOM) {
	STRNCPY(str, geom, n);
	str[n] = '\0';

	flags = XParseGeometry(str, &x, &y, &w, &h);
	if (!flags) {
		flags |= WidthValue;
		w = 0;
	}			/* default is tile */
	if (flags & WidthValue) {
		if (!(flags & XValue))
		x = 50;
		if (!(flags & HeightValue))
		h = w;
		if (w && !h) {
		w = (bgpixmap->w * w) / 100;
		h = bgpixmap->h;
		} else if (h && !w) {
		w = bgpixmap->w;
		h = (bgpixmap->h * h) / 100;
		}
		if (w > 1000)
		w = 1000;
		if (h > 1000)
		h = 1000;
		if (bgpixmap->w != (short)w) {
		bgpixmap->w = (short)w;
		changed++;
		}
		if (bgpixmap->h != (short)h) {
		bgpixmap->h = (short)h;
		changed++;
		}
	}
	if (!(flags & YValue)) {
		if (flags & XNegative)
		flags |= YNegative;
		y = x;
	}

	if (!(flags & WidthValue) && geom[0] != '=') {
		x += bgpixmap->x;
		y += bgpixmap->y;
	} else {
		if (flags & XNegative)
		x += 100;
		if (flags & YNegative)
		y += 100;
	}
	MIN_IT(x, 100);
	MIN_IT(y, 100);
	MAX_IT(x, 0);
	MAX_IT(y, 0);
	if (bgpixmap->x != x) {
		bgpixmap->x = x;
		changed++;
	}
	if (bgpixmap->y != y) {
		bgpixmap->y = y;
		changed++;
	}
	}
	free(str);
	return changed;
}


/* EXTPROTO */
void
rxvt_resize_pixmap(rxvt_t *r, int page)
{
	XGCValues		gcvalue;
	GC				gc;
	unsigned int	width = VT_WIDTH(r);
	unsigned int	height = VT_HEIGHT(r);

	if (PVTS(r, page)->pixmap != None)
		XFreePixmap(r->Xdisplay, PVTS(r, page)->pixmap);

	if (PVTS(r, page)->bg.pixmap == None) {
		/* So be it: I'm not using pixmaps */
		PVTS(r, page)->bg.pixmap = None;
# ifdef TRANSPARENT
		if (!(r->Options & Opt_transparent) ||
			r->h->am_transparent == 0)
# endif
		XSetWindowBackground(r->Xdisplay, PVTS(r, page)->vt,
			r->PixColors[Color_bg]);
		return;
	}

	gcvalue.foreground = r->PixColors[Color_bg];
	gc = XCreateGC(r->Xdisplay, PVTS(r, page)->vt, GCForeground, &gcvalue);

	if ((None != gc) &&
		(None != PVTS(r, page)->bg.pixmap)) {
		/* we have a specified pixmap */
		unsigned int	w = PVTS(r, page)->bg.w;
		unsigned int	h = PVTS(r, page)->bg.h;
		unsigned int	x = PVTS(r, page)->bg.x;
		unsigned int	y = PVTS(r, page)->bg.y;
		unsigned int	xpmh = PVTS(r, page)->xpm_attr.height;
		unsigned int	xpmw = PVTS(r, page)->xpm_attr.width;
		unsigned int	pixmapw, pixmaph;
#ifdef TINTING_SUPPORT
		GC				sgc;	/* dummy gc for fade/shade */
#endif

		/*
		 * don't zoom pixmap too much nor expand really small pixmaps
		 */
		if (w > 1000 || h > 1000)
			w = 1;
		else if (width > (10 * xpmw)
			 || height > (10 * xpmh))
			w = 0;		/* tile */

		if (w == 0) {
			/* basic X tiling - let the X server do it */
			PVTS(r, page)->pixmap = XCreatePixmap(r->Xdisplay,
				PVTS(r, page)->vt, xpmw, xpmh, (unsigned int)XDEPTH);
			XCopyArea(r->Xdisplay, PVTS(r, page)->bg.pixmap,
				PVTS(r, page)->pixmap, gc, 0, 0, xpmw, xpmh, 0, 0);

			pixmapw = xpmw;
			pixmaph = xpmh;
		}
		else {
			float		  incr, p;
			Pixmap		  tmp;

			PVTS(r, page)->pixmap = XCreatePixmap(r->Xdisplay,
				PVTS(r, page)->vt, width, height, (unsigned int)XDEPTH);

			/* horizontal scaling */
			rxvt_pixmap_incr(&w, &x, &incr, &p, width, xpmw);

			tmp = XCreatePixmap(r->Xdisplay, PVTS(r, page)->vt,
					width, xpmh, (unsigned int)XDEPTH);
			XFillRectangle(r->Xdisplay, tmp, gc, 0, 0, width,
				   xpmh);

			for ( /*nil */ ; x < w; x++, p += incr) {
				if (p >= xpmw)
					p = 0;
				/* copy one column from the original pixmap to the
				** tmp pixmap */
				XCopyArea(r->Xdisplay, PVTS(r, page)->bg.pixmap, tmp,
					gc, (int)p, 0, 1, xpmh, (int)x, 0);
			}

			/* vertical scaling */
			rxvt_pixmap_incr(&h, &y, &incr, &p, height, xpmh);

			if (y > 0)
				XFillRectangle(r->Xdisplay, PVTS(r, page)->pixmap, gc,
					0, 0, width, y);
			if (h < height)
				XFillRectangle(r->Xdisplay, PVTS(r, page)->pixmap, gc,
					0, (int)h, width, height - h + 1);
			for ( /*nil */ ; y < h; y++, p += incr) {
				if (p >= xpmh)
					p = 0;
				/* copy one row from the tmp pixmap to the main
				** pixmap */
				XCopyArea(r->Xdisplay, tmp, PVTS(r, page)->pixmap, gc,
					0, (int)p, width, 1, 0, (int)y);
			}
			XFreePixmap(r->Xdisplay, tmp);

			pixmapw = width;
			pixmaph = height;
		}

#ifdef TINTING_SUPPORT
		/* Alloc dummy GC */
		sgc = XCreateGC (r->Xdisplay, PVTS(r, page)->vt, 0UL, &gcvalue);
		if (None != sgc)	{
			/* Background fade support */
			if (r->h->rs[Rs_backgroundFade])	{
				rxvt_shade_pixmap (r, PVTS(r, page)->pixmap, sgc,
					0, 0, pixmapw, pixmaph, r->TermWin.bgfade,
					r->PixColors[Color_White]);
			}

			/* Tinting support */
			if (ISSET_PIXCOLOR (r->h, Color_tint) &&
				r->h->rs[Rs_shade]) {
				rxvt_shade_pixmap (r, PVTS(r, page)->pixmap, sgc,
					0, 0, pixmapw, pixmaph, r->TermWin.shade,
					r->PixColors[Color_tint]);
			}
			/* Free dummy GC */
			XFreeGC (r->Xdisplay, sgc);
		}	/* None != sgc */
#endif	/* TINTING_SUPPORT */

		/* Free gc */
		XFreeGC(r->Xdisplay, gc);
	}

	XSetWindowBackgroundPixmap(r->Xdisplay, PVTS(r, page)->vt,
		PVTS(r, page)->pixmap);
# ifdef TRANSPARENT
	r->h->am_transparent = 0;
# endif

	XClearWindow(r->Xdisplay, PVTS(r, page)->vt);

	XSync(r->Xdisplay, False);
}

/*
 * Calculate tiling sizes and increments
 * At start, p == 0, incr == xpmwidthheight
 */
/* INTPROTO */
void
rxvt_pixmap_incr(unsigned int *wh, unsigned int *xy, float *incr, float *p, unsigned int widthheight, unsigned int xpmwidthheight)
{
	unsigned int	cwh, cxy;
	float		   cincr, cp;

	cp = 0;
	cincr = (float)xpmwidthheight;
	cxy = *xy;
	cwh = *wh;
	if (cwh == 1) {	/* display one image, no horizontal/vertical scaling */
	cincr = (float)widthheight;
	if (xpmwidthheight <= widthheight) {
		cwh = xpmwidthheight;
		cxy = (cxy * (widthheight - cwh)) / 100;	/* beware! order */
		cwh += cxy;
	} else {
		cxy = 0;
		cwh = widthheight;
	}
	} else if (cwh < 10) {	/* fit WH images across/down screen */
	cincr *= cwh;
	cxy = 0;
	cwh = widthheight;
	} else {
	cincr *= 100.0 / cwh;
	if (cwh < 100) {	/* contract */
		float		   pos;

		cwh = (cwh * widthheight) / 100;
		pos = (float)cxy / 100 * widthheight - (cwh / 2);

		cxy = (widthheight - cwh);
		if (pos <= 0)
		cxy = 0;
		else if (pos < cxy)
		cxy = pos;
		cwh += cxy;
	} else {	/* expand */
		if (cxy > 0) {	/* position */
		float		   pos;

		pos = (float)cxy / 100 * xpmwidthheight - (cincr / 2);
		cp = xpmwidthheight - cincr;
		if (pos <= 0)
			cp = 0;
		else if (pos < cp)
			cp = pos;
		}
		cxy = 0;
		cwh = widthheight;
	}
	}
	cincr /= widthheight;
	*wh = cwh;
	*xy = cxy;
	*incr = cincr;
	*p = cp;
}


/* EXTPROTO */
Pixmap
rxvt_load_bg_pixmap(rxvt_t *r, int page, const char *file)
{
	Pixmap		pixmap;
	long		w = 0, h = 0;

	assert(file != NULL);

	pixmap = rxvt_load_pixmap (r, file, &w, &h);
	if (PVTS(r, page)->bg.pixmap != None)
		XFreePixmap (r->Xdisplay, PVTS(r, page)->bg.pixmap);
	PVTS(r, page)->bg.pixmap = pixmap;

	if (None == pixmap)	{
		XSetWindowBackground(r->Xdisplay, PVTS(r, page)->vt, r->PixColors[Color_bg]);
		return None;
	}

	PVTS(r, page)->xpm_attr.closeness = 30000;
	PVTS(r, page)->xpm_attr.colormap = XCMAP;
	PVTS(r, page)->xpm_attr.visual = XVISUAL;
	PVTS(r, page)->xpm_attr.depth = XDEPTH;
	PVTS(r, page)->xpm_attr.valuemask = (XpmCloseness | XpmColormap |
			XpmVisual | XpmDepth | XpmSize | XpmReturnPixels);
	PVTS(r, page)->xpm_attr.width = w;
	PVTS(r, page)->xpm_attr.height = h;
	rxvt_resize_pixmap(r, page);
	return (pixmap);
}


/* EXTPROTO */
Pixmap
rxvt_load_pixmap(rxvt_t *r, const char *file, long* pwidth, long* pheight)
{
	char*			f;
	int				flen;
#if defined(USE_JPEG) || defined(USE_PNG)
	long			w = 0, h = 0;
#endif
	XpmAttributes	xpm_attr;
	Pixmap			pixmap = None;


	assert(file != NULL);
	if ((char) 0 == *file) { /* No file to load */
		return None;
	}

	xpm_attr.closeness = 30000;
	xpm_attr.colormap = XCMAP;
	xpm_attr.visual = XVISUAL;
	xpm_attr.depth = XDEPTH;
	xpm_attr.valuemask = (XpmCloseness | XpmColormap |
			XpmVisual | XpmDepth | XpmSize | XpmReturnPixels);


	/* search environment variables here too */
	if (NULL == (f = (char*) rxvt_File_find (file, ".xpm", r->h->rs[Rs_path]))
#ifdef USE_JPEG
		&& NULL == (f = (char*) rxvt_File_find (file, ".jpg", r->h->rs[Rs_path]))
		&& NULL == (f = (char*) rxvt_File_find (file, ".jpeg", r->h->rs[Rs_path]))
#endif
#ifdef USE_PNG
		&& NULL == (f = (char*) rxvt_File_find (file, ".png", r->h->rs[Rs_path]))
#endif
		)	{
		char		   *p;
		/* semi-colon delimited */
		if (NULL == (p = STRCHR(file, ';')))
			p = STRCHR(file, '\0');
		rxvt_print_error("couldn't load image file \"%.*s\"", (p - file), file);
		return None;
	}

	flen = STRLEN (f);
#ifdef USE_JPEG
	if ((flen >= 4 && !STRNCASECMP (f+flen-4, ".jpg", 4)) ||
		(flen >= 5 && !STRNCASECMP (f+flen-5, ".jpeg",5)))	{
		GC		gc = DefaultGC (r->Xdisplay, XSCREEN);
		if (!JpegReadFileToPixmap (r->Xdisplay, XROOT, gc, f,
			&pixmap, &w, &h))	{
			*pwidth = w;
			*pheight = h;
		}
	}
	else
#endif
#ifdef USE_PNG
	if (flen >= 4 && !STRNCASECMP (f+flen-4, ".png", 4))	{
		GC		gc = DefaultGC (r->Xdisplay, XSCREEN);
		if (!PngReadFileToPixmap (r->Xdisplay, XROOT, gc, f,
			&pixmap, &w, &h))	{
			*pwidth = w;
			*pheight = h;
		}
	}
	else
#endif
#ifdef HAVE_LIBXPM
	if (!XpmReadFileToPixmap(r->Xdisplay, XROOT, f,
		&pixmap, NULL, &xpm_attr))	{
		*pwidth = xpm_attr.width;
		*pheight = xpm_attr.height;
	}
#endif
	{
		/* empty to suppress compile error */
	}

	free(f);
	if (None == pixmap)	{
		char		   *p;
		/* semi-colon delimited */
		if ((p = STRCHR(file, ';')) == NULL)
		p = STRCHR(file, '\0');
		rxvt_print_error("couldn't load image file \"%.*s\"", (p - file), file);
	}

	return	(pixmap);
}

#endif	/* BACKGROUND_IMAGE */

/*----------------------- end-of-file (C source) -----------------------*/
