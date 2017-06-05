/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/TextOut.c,v 1.10 2008/01/02 19:42:57 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/TextOut.c,v 1.10 2008/01/02 19:42:57 dannybackx Exp $";

/* Do NOT use this (USE_XT_GC) :
 * it requires setting all the dynamic GC fields before every use of
 * the GC. As we have too many GC fields that can change, this is
 * really a bad idea.
 * Setting this macro leads to use of XtAllocateGC and XtGetGC,
 * and similar for the free operations.
 * This path has been explored more than once, by different
 * developers. Please keep the comments here so it gets
 * remembered.
 */
#undef	USE_XT_GC
/*
 * End of the USE_XT_GC blurb.
 */

#include <LTconfig.h>

#include <stdio.h>
#include <limits.h>		/* for INT_MAX */
#include <string.h>
#include <stdlib.h>

#include <Xm/XmP.h>
#include <Xm/TextP.h>
#include <Xm/TextOutP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/AtomMgr.h>
#include <XmI/XmI.h>
#include <XmI/AtomMgrI.h>

#include <XmI/DebugUtil.h>


#undef VERBOSE

static void Draw(XmTextWidget w, LineNum num, XmTextPosition start, XmTextPosition end, XmHighlightMode highlight);

#define Offset(field) XtOffsetOf(XmTextInnerRec, inner.out.field)


#define	USE_AVERAGE_WIDTH 1

#undef	Out_FontTextWidth
#define	Out_FontTextWidth(o,s,l)	(int)_XmOut_FontTextWidth(o, s, l)
#define	Out_FontMaxWidth(o)	(int)_XmOut_FontMaxWidth(o)

static void _XmTextOutSetRenderTable(Widget w, int o, XrmValue *v);
	
/* Resources for the TextInner class */
static XtResource output_resources[] =
{
    {
	XmNblinkRate, XmCBlinkRate, XmRInt,
	sizeof(int), Offset(blinkrate),
	XmRImmediate, (XtPointer)500
    },
    {
	XmNcolumns, XmCColumns, XmRShort,
	sizeof(short), Offset(columns),
	XmRImmediate, (XtPointer)20
    },
    {
	XmNcursorPositionVisible, XmCCursorPositionVisible, XmRBoolean,
	sizeof(Boolean), Offset(cursor_position_visible),
	XmRImmediate, (XtPointer)True
    },
    /*
     * FontList and RenderTable resources :
     * Note that the right order is important here !
     */
    {
	"keep.off", "Keep.off", XmRBoolean,
	sizeof(Boolean), Offset(check_set_render_table),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(fontlist),
	XmRCallProc, (XtPointer)_XmTextOutSetRenderTable
    },
    {
	XmNrenderTable, XmCRenderTable, XmRRenderTable,
	sizeof(XmFontList), Offset(fontlist),
	XmRCallProc, (XtPointer)_XmTextOutSetRenderTable
    },
    /*
     * End of the fontList and renderTable resources
     */
    {
	XmNresizeHeight, XmCResizeHeight, XmRBoolean,
	sizeof(Boolean), Offset(resizeheight),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNresizeWidth, XmCResizeWidth, XmRBoolean,
	sizeof(Boolean), Offset(resizewidth),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNrows, XmCRows, XmRShort,
	sizeof(short), Offset(rows),
	XmRImmediate, (XtPointer)1
    },
    {
	XmNwordWrap, XmCWordWrap, XmRBoolean,
	sizeof(Boolean), Offset(wordwrap),
	XmRImmediate, (XtPointer)False
    },

    /* ScrolledText resources */
    {
	XmNscrollHorizontal, XmCScroll, XmRBoolean,
	sizeof(Boolean), Offset(scrollhorizontal),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNscrollLeftSide, XmCScrollSide, XmRBoolean,
	sizeof(Boolean), Offset(scrollleftside),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNscrollTopSide, XmCScrollSide, XmRBoolean,
	sizeof(Boolean), Offset(scrolltopside),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNscrollVertical, XmCScroll, XmRBoolean,
	sizeof(Boolean), Offset(scrollvertical),
	XmRImmediate, (XtPointer)True
    },
};

/*
 * Common initialization procedures -------------------------------------------
 */
extern int
_XmFontCalculateAverageCharacterWidth(Widget w, XFontStruct *fs)
{
	unsigned long width;
	Atom atom;

	if (fs == NULL) {
		/* Especially important in Xft case */
		return 0;
	}

	/* this is copied from a similar function in Screen.c */
	if ((atom = XmInternAtom(XtDisplay(w), _XA_AVERAGE_WIDTH, True))
			&& XGetFontProperty(fs, atom, &width) && width) {
		width = (width + 5) / 10; /*CP: just to round it up slightly */
	} else if ((! XGetFontProperty(fs, XA_QUAD_WIDTH, &width)) || width == 0) {
		/*
		 * Use the width of '0' if that's available, otherwise the maximum width
		 * like the manual page of XmText says.
		 */
		if (fs->per_char && fs->min_char_or_byte2 <= '0' && fs->max_char_or_byte2 >= '0')
			width = fs->per_char['0' - fs->min_char_or_byte2].width;
		else
			width = fs->max_bounds.width ;                                          
	}
	return width;
}

/* FIX ME - this thing is clearly overly simplistic. */
/* I think it should :
 *    - grab a fontset whenever it can (bypassing fonts)
 *      - make sure you pick any fontset (preferably) or font (if all else
 *        fails) from the list if nothing matches the tag we want
 */
/*
 * Note : never *ever* use XmFONTLIST_DEFAULT_TAG without checking
 * XmSTRING_DEFAULT_CHARSET as well. Pre-1.2 docs use the latter everywhere.
 */
static void
FontInit(XmTextWidget w)
{
	OutputData o = Text_OutputData(w);
	XmFontListEntry entry = NULL;
	XmFontList oldlist = Out_FontList(o);
	XFontStruct *fs;
	int i;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "FontInit(fontlist %p)\n", oldlist));
	DEBUGOUT(_LtDebugPrintRenderTable(__FILE__, (Widget)w, "FontInit(): ", oldlist));

	/* This part is probably all wrong ... FIX ME */
	for (i = 0; Out_FontList(o)->renditions[i]->tag != NULL; i++) {
		if (strcmp(XmFONTLIST_DEFAULT_TAG,
				Out_FontList(o)->renditions[i]->tag) == 0 ||
				strcmp(XmSTRING_DEFAULT_CHARSET,
				Out_FontList(o)->renditions[i]->tag) == 0) {
			entry = Out_FontList(o)->renditions[i];

			if (entry->font == NULL) {
				entry = NULL;
				continue;
			}

			break;
		}
	}

	if (!entry) {
		for (i = 0; Out_FontList(o)->renditions[i]->tag != NULL; i++) {
			if (Out_FontList(o)->renditions[i]->type == XmFONT_IS_FONTSET) {
				entry = Out_FontList(o)->renditions[i];

				if (entry->font == NULL) {
					entry = NULL;
					continue;
				}

				break;
			}
		}
	}

	if (!entry) {
		for (i = 0; Out_FontList(o)->renditions[i]->tag != NULL; i++) {
			if (Out_FontList(o)->renditions[i]->type == XmFONT_IS_FONT) {
				entry = Out_FontList(o)->renditions[i];

				if (entry->font == NULL) {
					entry = NULL;
					continue;
				}

				break;
			}
		}
	}
	/* (Hopefully) end of the all wrong part */

#if	USE_XFT
	Out_FontType(o) = XmFONT_IS_FONT;
	o->xft_font = NULL;
	
	/*
	 * We need a hack to get Xft fonts accepted.
	 * The current (initial hack) is to use Xft for fonts if
	 * the first item in the fontList is an Xft font.
	 */
	if (Out_FontList(o) && Out_FontList(o)->renditions[0]
			&& Out_FontList(o)->renditions[0]->type == XmFONT_IS_XFT) {
		Out_FontType(o) = XmFONT_IS_XFT;
		entry = Out_FontList(o)->renditions[0];
		o->xft_font = entry->xft_font;
		Out_Font(o) = NULL;
	}
#endif

	/* This should only be the very very last resort. */
#ifdef	USE_XFT
	if (entry == NULL || (entry->font == NULL && entry->type != XmFONT_IS_XFT))
#else
	if (entry == NULL || entry->font == NULL)
#endif
	{
		Out_FontList(o) = _XmFontListCreateDefault(XtDisplay((Widget)w));
		entry = Out_FontList(o)->renditions[0];
	} else {	/* Make copy. Original will be free */
		/*  amai, 20010313:
		 *  I do not understand why this is necessary, but
		 *  it seems to be ...
		 *  Why does copying the list affect behaviour?!
		 *  Try to omit the copying step and run test/Xm/text/test17!
		 *  The original will _not_ be freed, or at least it won't be
		 *  always freed, see test/Xm/text/test11, the leak check!
		 */
		Out_FontList(o) = XmFontListAppendEntry(NULL, entry);
		if (Out_FontListCreated(o))
			XmFontListFree(oldlist);
		entry = Out_FontList(o)->renditions[0];
		Out_FontListCreated(o) = True;
	}

	/* We may get a font or a fontset here */
	if (entry->type == XmFONT_IS_FONT)
	{
		fs = (XFontStruct *)entry->font;

		Out_FontAverageWidth(o) =
			(fs->max_bounds.width + fs->min_bounds.width) / 2;
		Out_UseFontSet(o) = False;
		Out_Font(o) = fs;
		Out_Font_Ascent(o) = Out_FontAscent(o);
		Out_Font_Descent(o) = Out_FontDescent(o);
#ifdef	USE_XFT
	} else if (entry->type == XmFONT_IS_XFT) {
		Out_FontAverageWidth(o) = Out_XftFont(o)->max_advance_width;
		Out_FontAverageWidth(o) = entry->font_average_width;	/* FIX ME HACK */
		Out_UseFontSet(o) = False;	/* FIX ME now sure what this means now */
		Out_Font_Ascent(o) = Out_FontAscent(o);
		Out_Font_Descent(o) = Out_FontDescent(o);
#endif
	} else if (entry->type == XmFONT_IS_FONTSET) {
		XFontStruct **fsl;
		char **nl;
		int r, i, max, min, ma, md;

		r = XFontsOfFontSet((XFontSet)entry->font, &fsl, &nl);
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "XFontsOfFontSet : %d\n", r));
		for (i = 0; i < r; i++) {
			DEBUGOUT(_LtDebug0(__FILE__, NULL, "\t%s\n", nl[i]));
		}

		if (r == 0) {
			DEBUGOUT(_LtDebug(__FILE__, NULL, "No font in fontset\n"));
			abort();
		}

		/* correct calculation */
		ma = md = max = 0; min = INT_MAX;
		for(i=0; i<r; i++) {
			if (fsl[i]->max_bounds.width  > max)
				max = fsl[i]->max_bounds.width;
			if (fsl[i]->min_bounds.width  < min)
				min = fsl[i]->min_bounds.width;
			if (fsl[i]->max_bounds.ascent > ma)
				ma = fsl[i]->max_bounds.ascent;
			if (fsl[i]->max_bounds.descent > md)
				md = fsl[i]->max_bounds.descent;
		}
		Out_FontAverageWidth(o) = (max + min) / 2;
		Out_Font_Ascent(o) = ma;
		Out_Font_Descent(o) = md;

		/* Just pick the first one */
		Out_Font(o) = fsl[0];

		Out_UseFontSet(o) = True;
	} else {
		/* This is not supposed to happen. */
		_XmWarning((Widget)w, "XmText FontInit: untreated rendition type\n");
	}

	if (Out_FontAverageWidth(o) <= 0) {
		Out_FontAverageWidth(o) = _XmFontCalculateAverageCharacterWidth((Widget)w, fs);
	}
	Out_TabWidth(o) = 8 * Out_FontAverageWidth(o);
#ifdef	USE_XFT
	if (Out_XftFont(o))
		Out_XftFont(o)->height = Out_Font_Ascent(o) + Out_Font_Descent(o);
	else
		o->lineheight = Out_Font_Ascent(o) + Out_Font_Descent(o);
#else
	Out_FontHeight(o) = Out_Font_Ascent(o) + Out_Font_Descent(o);
#endif
	Out_CursorHeight(o) = Out_FontHeight(o);


	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"FontInit - fl %p font %p : ascent=%d descent=%d height=%d avewidth=%d\n",
		Out_FontList(o), Out_Font(o),
		Out_Font_Ascent(o), Out_Font_Descent(o),
		Out_FontHeight(o), Out_FontAverageWidth(o)));
}


/*
 * This thing still creates GC's the anti-social way - with XCreateGC.
 *
 * By using XtGetGC or XtAllocateGC, Xt will allow us to share GC's
 * with other widgets. We'd just have to be sure about what we want to be
 * the properties of the thing.
 *
 * Note: the explanation for this is in comments at the top of this file.
 */
static void
GCInitialize(XmTextWidget w)
{
    OutputData o = Text_OutputData(w);
    XGCValues values;
    XtGCMask mask, dynamic, dontcare;

    /*
     * DrawGC
     *      Need to change :	ClipRectangles
     *				GCFillStyle | GCStipple
     */
	values.line_style = LineSolid;
	values.line_width = 0;
	values.fill_style = FillSolid;

	mask = GCLineStyle | GCLineWidth | GCForeground | GCBackground
		| GCClipXOrigin | GCClipYOrigin;
	if (Out_FontType(o) == XmFONT_IS_FONT) {
		values.font = Out_Font(o)->fid;
		mask |= GCFont;
	} else
#ifdef	USE_XFT
	if (Out_FontType(o) == XmFONT_IS_XFT) {
		/* FIX ME XmFONT_IS_XFT */
	}
#endif
#ifdef	USE_BIDI
	else if (Out_FontType(o) == XmFONT_IS_XOC) {
		/* FIX ME XmFONT_IS_BIDI */
	}
#endif
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.clip_x_origin = 0;
    values.clip_y_origin = 0;
    dynamic = 0;
    dontcare = 0;

    if (! w->core.sensitive) {
	Screen	*s = XtScreen((Widget)w);

	mask |= GCStipple;
	values.fill_style = FillStippled;
	values.stipple = XmGetPixmapByDepth(s, "50_foreground", 1, 0, 1);
    }

#ifdef	USE_XT_GC
    Out_DrawGC(o) = XtAllocateGC((Widget)w, w->core.depth,
	mask, &values, dynamic, dontcare);
#else
    Out_DrawGC(o) = XCreateGC(XtDisplay((Widget)w), XtWindow(w), mask, &values);
#endif
    Out_DrawGCInverted(o) = False;

    /*
     * Cursor GC
     *      Need to change :        Tile/Stipple Origin, Stipple
     */
    values.line_style = LineSolid;
    values.line_width = 0;
    values.fill_style = FillStippled;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    mask = GCLineStyle | GCLineWidth | GCFillStyle |
	GCForeground | GCBackground;
    dynamic = GCTileStipXOrigin | GCTileStipYOrigin | GCStipple;

    dontcare = 0;

#ifdef USE_XT_GC
    Out_CursorGC(o) = XtAllocateGC((Widget)w, w->core.depth,
	mask, &values, dynamic, dontcare);
#else
    Out_CursorGC(o) = XCreateGC(XtDisplay((Widget)w), XtWindow(w), mask, &values);
#endif

    /*
     * CopyGC
     *      At least this one is not changed anywhere :-)
     */
    values.line_style = LineSolid;
    values.line_width = 0;
    values.fill_style = FillSolid;
    values.foreground = XtBackground(w);
    values.background = Prim_Foreground(w);
    mask = GCLineStyle | GCLineWidth | GCFillStyle |
	GCForeground | GCBackground;

    Out_CopyGC(o) = XtGetGC((Widget)w, mask, &values);
}


static void
GCPixmap(XmTextWidget tw)
{
	OutputData o = Text_OutputData(tw);
	Widget w = (Widget)tw;
	static char dots[] = {2, 1, 1};
	GC gc;
	int width=0, width_erase=0, height=0;
	Display *display;
	XGCValues values;
	XtGCMask mask;

	values.line_style = LineSolid;
	values.line_width = 0;
	values.fill_style = FillSolid;
	values.foreground = 0;
	values.background = 0;

	display = XtDisplay(w);
	width = Out_CursorWidth(o) ;

	if (Out_FontType(o) == XmFONT_IS_FONT) {
		width_erase = 2 * Out_Font(o)->max_bounds.width;
		height = Out_CursorHeight(o) = Out_FontHeight(o);
	} else
#ifdef	USE_XFT
	if (Out_FontType(o) == XmFONT_IS_XFT) {
		width_erase = 2 * ((XftFont *)o->xft_font)->max_advance_width;
		width_erase = 2 * Out_FontAverageWidth(o);	/* FIX ME HACK */
		height = Out_CursorHeight(o) = Out_XftFont(o)->height;
	}
#endif

	if (Out_CursorIBeam(o)) {
		XFreePixmap(XtDisplay(w), Out_CursorIBeam(o));
	}
	Out_CursorIBeam(o) = (Pixmap)NULL;

	if (Out_CursorStipple(o)) {
		XFreePixmap(XtDisplay(w), Out_CursorStipple(o));
	}
	Out_CursorStipple(o) = (Pixmap)NULL;

	if (Out_CursorSave(o)) {
		XFreePixmap(XtDisplay(w), Out_CursorSave(o));
	}
	Out_CursorSave(o) = (Pixmap)NULL;

	Out_CursorSaveValid(o) = False;

	if (height > 0) {
		Out_CursorIBeam(o) = XCreatePixmap(display,
			RootWindowOfScreen(XtScreen(w)),
			width, height, 1);
		Out_CursorStipple(o) = XCreatePixmap(display,
			RootWindowOfScreen(XtScreen(w)),
			width, height, 1);
		Out_CursorSave(o) = XCreatePixmap(display,
			RootWindowOfScreen(XtScreen(w)),
			width_erase, height, w->core.depth);
		values.line_style = LineSolid;
		values.line_width = 0;
		values.fill_style = FillSolid;
		values.foreground = 0;
		values.background = 0;
		mask = GCLineStyle | GCLineWidth | GCFillStyle |
			GCForeground | GCBackground;
		gc = XCreateGC(display, Out_CursorIBeam(o), mask, &values);

		XFillRectangle(display, Out_CursorIBeam(o), gc, 0, 0, width, height);
		XFillRectangle(display, Out_CursorStipple(o), gc, 0, 0, width, height);

		XSetForeground(display, gc, 1);

		if (Text_InputData(w)->overstrike) {
			/* Overwrite cursor */
			XDrawLine(display, Out_CursorIBeam(o), gc,
				0, 0,
				0, height - 1);
			XDrawLine(display, Out_CursorIBeam(o), gc,
				0, height - 1,
				width - 1, height - 1);
			XDrawLine(display, Out_CursorIBeam(o), gc,
				width - 1, height - 1,
				width - 1, 0);
			XDrawLine(display, Out_CursorIBeam(o), gc,
				width - 1, 0,
				0, 0);
		} else {
			/* I-beam */
			XDrawLine(display, Out_CursorIBeam(o), gc,
				2, 1, 2, height - 2);
			XDrawLine(display, Out_CursorIBeam(o), gc,
				0, 0, 4, 0);
			XDrawLine(display, Out_CursorIBeam(o), gc,
				0, height - 1,
				4, height - 1);
		}

		XSetLineAttributes(display, gc, 0, LineOnOffDash, CapRound, JoinRound);
		XSetDashes(display, gc, 0, &dots[1], (int)dots[0]);

		XDrawLine(display, Out_CursorStipple(o), gc, 2, 1, 2, height - 2);
		XDrawLine(display, Out_CursorStipple(o), gc, 1, 0, 3, 0);
		XDrawLine(display, Out_CursorStipple(o), gc, 1, height - 1,
			3, height - 1);

		XFreeGC(display, gc);
	}
}


static void
GCClip(XmTextWidget w)
{
	OutputData o = Text_OutputData(w);
	XRectangle clip;

	clip.x = 0;
	clip.y = 0;
	clip.width = XtWidth(w) - ( Out_RightMargin(o) + Out_LeftMargin(o) );
	clip.height = XtHeight(w) - ( Out_TopMargin(o) + Out_BottomMargin(o) );

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
				"XmText GCClip gc %p wid %d ht %d (%d - %d - %d, %d - %d - %d)\n",
				Out_DrawGC(o),
				clip.width, clip.height,
				XtWidth(w), Out_RightMargin(o), Out_LeftMargin(o),
				XtHeight(w), Out_TopMargin(o), Out_BottomMargin(o)));

	/* These calls change the GCs */
	XSetClipRectangles(XtDisplay((Widget)w), Out_DrawGC(o),
			Out_LeftMargin(o), Out_TopMargin(o), &clip, 1, Unsorted);

	GCPixmap(w);
}


/* Take current dimensions, calculate #rows, #columns from it */
static void
SizeRecalc(XmTextWidget w)
{
	OutputData o = Text_OutputData(w);
	int previousColumns = Out_Columns(o);

	Out_XDraw(o) = Out_LeftMargin(o);

#ifdef	USE_XFT
	/* HACK */
	if (Out_XftFont(o) && Out_XftFont(o)->height < 0)
		Out_XftFont(o)->height = Out_XftFont(o)->ascent + Out_XftFont(o)->descent;
	if (Out_XftFont(o)) {
			Out_Columns(o) = (XtWidth(w) - (Out_LeftMargin(o) + Out_RightMargin(o)))
				/ Out_XftFont(o)->max_advance_width;
			Out_Columns(o) = (XtWidth(w) - (Out_LeftMargin(o) + Out_RightMargin(o)))
				/ Out_FontAverageWidth(o);	/* FIX ME HACK */
			DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
				"SizeRecalc: xft cols %d = (wid %d - lm %d - rm %d) / fw %d, "
				"ht %d = %d + %d\n",
				Out_Columns(o), XtWidth(w), Out_LeftMargin(o), Out_RightMargin(o),
				Out_XftFont(o)->max_advance_width,
				Out_XftFont(o)->height,
				Out_XftFont(o)->ascent, Out_XftFont(o)->descent));
	} else 
#endif
	{
#ifdef USE_AVERAGE_WIDTH
		Out_Columns(o) = (XtWidth(w) - ( Out_LeftMargin(o) + Out_RightMargin(o) ) )
			/ Out_FontAverageWidth(o);
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			"SizeRecalc: cols %d = (wid %d - lm %d - rm %d) / fw %d\n",
			Out_Columns(o), XtWidth(w), Out_LeftMargin(o), Out_RightMargin(o),
			Out_FontAverageWidth(o)));
#else
		Out_Columns(o) = (XtWidth(w) - ( Out_LeftMargin(o) + Out_RightMargin(o) ) )
			/ Out_FontMaxWidth(o);
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			"SizeRecalc: cols %d = (wid %d - lm %d - rm %d) / fw %d\n",
			Out_Columns(o), XtWidth(w), Out_LeftMargin(o), Out_RightMargin(o),
			Out_FontMaxWidth(o)));
#endif
	}

	/* Y offset for a row R is calculated as Out_YDraw(o) - R*fontHeight,
	 * and I add FontAscent here because y positions for fonts are
	 * measured from the baseline of the font. */
	Out_YDraw(o) = Out_TopMargin(o) + Out_Font_Ascent(o);

	if (Text_EditMode(w) == XmSINGLE_LINE_EDIT) {
		Out_Rows(o) = 1;
	} else {
		Out_Rows(o) = (XtHeight(w)
			- ( Out_TopMargin(o) + Out_BottomMargin(o) ) )
			/ Out_FontHeight(o);
		if ( Out_Rows(o) < 1 ) {
			Out_Rows(o) = 1;
		}
	}
	if (Out_ResizeHeight(o) && XtHeight(w)
		< (Out_TopMargin(o) + Out_BottomMargin(o) + Out_FontHeight(o) * Out_Rows(o))) {
		XtHeight(w) = Out_TopMargin(o) + Out_BottomMargin(o)
			+ Out_FontHeight(o) * Out_Rows(o);
	}

	Out_NumberLines(o) = Out_Rows(o);
	if (Out_Columns(o) != previousColumns && Out_WordWrap(o)) {
		_XmTextUpdateLineTable((Widget)w, 0, 0, NULL, False);
	}

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"SizeRecalc wid %d ht %d -> rows %d cols %d\n",
		XtWidth(w), XtHeight(w), Out_Rows(o), Out_Columns(o)));
}




/* Text Drawing functions ------------------------------------------------ */

/*
 * This thing calls "Draw" which in turn calls "DrawText".
 * Both expect their last argument to be a flag indicating which type
 * of highlighting is to be used :
 *      - XmHIGHLIGHT_NORMAL (no highlighting)
 *      - XmHIGHLIGHT_SELECTED (reversed colours)
 *      - XmHIGHLIGHT_SECONDARY_SELECTED (underlined)
 *
 * This means that this function should break up those parts of text that
 * are indicated as selected and/or highlighted.
 *
 * Highlighted stuff is recorded in an array. The array size is
 *      Text_Highlight(w).number
 * Each element in the array has a mode and a position, e.g.
 *      Text_Highlight(w).list[n].position
 */
static void
DrawAll(XmTextWidget w)
{
    Cardinal i;
    int resty, mh, hi;
    int			highlight = XmHIGHLIGHT_NORMAL;		/* initial value */
    XmTextPosition	np, first;

    OutputData o = Text_OutputData(w);

#ifdef	DANNY
    fprintf(stderr, "XmText DrawAll(%s dpy %p)\n", XtName(w), XtDisplay(w));
#endif

    /*
     * Without this, the area under the cursor is saved before it
     * is actually drawn; I can't figure out how that can happen but
     * it does. Registering when you redraw that the saved image is
     * invalid helps because we no longer restore invalid images...
     */
    Out_CursorSaveValid(o) = False;

    mh = Text_Highlight(w).number;
    hi = 0;
    np = -1;
    if (mh)
    {
	if (Text_Highlight(w).list[0].position == 0)
	{
	    highlight = Text_Highlight(w).list[0].mode;
	    hi++;
	}

	if (hi < mh)
	{
	    np = Text_Highlight(w).list[hi].position;
	}
	else
	{
	    np = -1;
	}
    }
    while ( np != -1 && Text_Line(w)[0].start > np )
	{
	highlight = Text_Highlight(w).list[hi].mode;
	hi++;
	if (hi < mh)
	    {
	    np = Text_Highlight(w).list[hi].position;
	    }
	else
	    {
	    np = -1;
	    }
	}
    if ( np < Text_Line(w)[0].start )
	{
	np = -1;
	}
 
    for (i = 0; i < (Text_LineCount(w) < Out_Rows(o) ?
		     Text_LineCount(w) : Out_Rows(o)); i++)
    {
	Line line = &Text_Line(w)[i];
	Line next = &Text_Line(w)[i + 1];

#ifdef VERBOSE
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		   "DrawAll: line[%d]: start=%d end=%d, highlight change %d\n",
			  i, line->start, next->start - 1, np));
#endif

       /* Cleaning part of widget */
    XClearArea(XtDisplay(w), XtWindow(w),
              Prim_HighlightThickness(w) + Prim_ShadowThickness(w),
               Out_TopMargin(o) + i * Out_FontHeight(o),
              XtWidth(w) -
              2 * (Prim_HighlightThickness(w) + Prim_ShadowThickness(w)),               
               Out_FontHeight(o),
              False);

	/* Does highlighting change in this range of text ?? */
	if (np >= line->start && np <= next->start - 1)
	{			/* Yes */
	    /* Often highlight changes more than once per line */

	    /* Draw the first part */
	    first = line->start;

	    /* Check whether a change is due right now */
	    if (first == np)
	    {
		if (hi < mh)
		{
		    if (Text_Highlight(w).list[hi].position == first)
		    {
			highlight = Text_Highlight(w).list[hi].mode;
			hi++;

			if (hi < mh)
			{
			    np = Text_Highlight(w).list[hi].position;
			}
			else
			{
			    np = -1;
			}
		    }
		}
	    }

	    while (np != -1 && np <= next->start - 1)
	    {
	    	XmTextPosition drawTo = np;
#if 0
		if ( np < next->start - 1 )
		    {
		    	/*CP:16 May 1999: basically do not include the return char 
		    	** if the next highlight position is not past the end of the line. */
		    drawTo ++;
		    }
#endif
		(*Text_Output(w)->Draw)(w, i, first, drawTo, highlight);
		first = np;

		/* Find the next change */
		highlight = Text_Highlight(w).list[hi].mode;
		hi++;
		if (hi == mh - 1) {
		    np = -1;
		} else if (hi < mh) {
		    np = Text_Highlight(w).list[hi].position;
		}
	    }

	    /* Draw the last part */
	    if (np != next->start - 1)
	    {
		XmTextPosition endPos = next->start - 1;
		if ( np > endPos )
		     {
			/*CP:16 May 1999: basically include the return char 
			** if the next highlight position is past the end of the line. */
		     endPos ++;
		     }
		(*Text_Output(w)->Draw)(w, i, first, endPos,
			highlight);
	    }
	}
	else
	{
	    /* Sanity checks */
	    if (next->start > 0 && line->start < next->start - 1)
	    {
			/*CP:16 May 1999: this point is reached only for lines which
			** are inside a selection area, so also include the newline. */
		(*Text_Output(w)->Draw)(w, i, line->start, next->start,
			highlight);
	    }
	    else if ( (  line->start == next->start - 1 ) &&
	    		highlight != XmHIGHLIGHT_NORMAL )
	    {
		(*Text_Output(w)->Draw)(w, i, line->start, next->start,
			highlight);
	    }
	}
    }

       /* Cleaning rest of widget */
    resty = Out_TopMargin(o) + i * Out_FontHeight(o) ;
    XClearArea(XtDisplay(w), XtWindow(w),
              Prim_HighlightThickness(w) + Prim_ShadowThickness(w),
               resty,
              XtWidth(w) -
              2 * (Prim_HighlightThickness(w) + Prim_ShadowThickness(w)),               
              XtHeight(w) - resty - 
              (Prim_HighlightThickness(w) + Prim_ShadowThickness(w)),           
              False);
}




/* Utilities ------------------------------------------------------------- */


#define Out_NextTabStop(o,x) \
    (Out_TabWidth(o)*((x+Out_TabWidth(o))/Out_TabWidth(o)))

/* XTextWidth clone, but taking TABS into account.
 * FIX ME: Must start from col 0 (or a tab boundary) for tab position to be
 *        accurate.
 */
static int
_XmTextNextX(XmTextWidget w, int x, char *ptr, int len)
{
    OutputData o = Text_OutputData(w);
    XFontStruct *fs = Out_Font(o);
    XRectangle ink, log;

    while (len > 0)
    {
	unsigned int c;

	c = (unsigned char)*ptr;
	if (c == '\t') {
	    x = Out_NextTabStop(o, x);
	} else if (c == '\0') {
	    x = 0;
	} else {
	    if (Out_FontList(o)->renditions[0]->type == XmFONT_IS_FONT) {
                x += XTextWidth(fs, ptr, 1);
            } else if (Out_FontList(o)->renditions[0]->type == XmFONT_IS_FONTSET) {
                XmbTextExtents((XFontSet)Out_FontList(o)->renditions[0]->font, ptr, 1, &ink, &log);
                x += log.width;
#ifdef	USE_XFT
            } else if (Out_XftFont(o)) {
	    	XGlyphInfo	ext;
		XftTextExtents8(XtDisplay((Widget)w), Out_XftFont(o),
			(unsigned char*)ptr, 1, &ext);
		x += ext.width;
#endif
	    } else {
		/* ??? */
	    }

#if 0
            break;

	    if (c < fs->min_char_or_byte2 || c > fs->max_char_or_byte2)
	    {
		c = fs->default_char;

		/*
		 * Hack by vogt@kvi.nl
		 * Updated 8 nov 2000 by Tom Pollard pollard@schrodinger.com
		 */
		if (c < fs->min_char_or_byte2 || c > fs->max_char_or_byte2) {
			DEBUGOUT(_LtDebug(__FILE__, (Widget)w, 
				"_XmTextNextX: fs->default_char %d -> 32\n",
				c));
			c = 32;
		}
		/* end hack */
	    }

	    if (fs->per_char)
	    {
		c -= fs->min_char_or_byte2;
		x += fs->per_char[c].width;
	    }
	    else
		x += fs->max_bounds.width;
#endif
	}

	ptr++;
	len--;
    }

    return x;
}


static Dimension
FontTextWidth(XmTextWidget w, XmTextPosition pos, XmTextPosition end)
{
    XmTextBlockRec block;
    Dimension width;

    if (Text_Source(w) == NULL)
    {
	_XmWarning((Widget)w, "FontTextWidth: no source\n");

	return 1;
    }

    (*Text_Source(w)->ReadSource) (Text_Source(w), pos, end, &block);

    width = _XmTextNextX(w, 0, block.ptr, block.length);

#ifdef VERBOSE
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "FontTextWidth(%s) => %d\n", block.ptr, width));
#endif

    XtFree(block.ptr);

    return width;
}


static XmTextPosition
XToPos(XmTextWidget w, LineNum row, int x)
{
    OutputData o = Text_OutputData(w);
    Line line = &Text_Line(w)[row];
    Line next = &Text_Line(w)[row + 1];
    XmTextPosition start, end, pos;
    XmTextBlockRec block;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "XToPos(row %d, x %d) TotalLines %d\n",
		      row, x, Text_TotalLines(w)));

    /* Just guessing - Is this right ?? Danny */
    if (row >= Text_TotalLines(w) - 0)
    {
	next = &Text_Line(w)[Text_TotalLines(w) - 1];
	end = next->start - 1;

	if (end < 0)
	{
	    end = 0;
	}

	return end;
    }

    start = line->start;
    if (next->start == (XmTextPosition)PASTENDPOS)
    {
	end = Text_LastPos(w);	/* Avoid PASTENDPOS */
    }
#if 0
    else if (next->past_end)		/* There's nothing there ... */
    {
	end = start;			/* FIXME - this is probably wrong */
    }
#endif
    else
    {
	end = next->start - 1;
    }

    /* x is now the width from the start of the line to the pixel pos */
    x -= Out_LeftMargin(o) - Out_XOffset(o);

    (*Text_Source(w)->ReadSource) (Text_Source(w), start, end, &block);

    /* check if the cursor is before the 1st character */
    if (x <= 0)
    {
	pos = start;
    }
    /* OK, how 'bout after the last character */
    else if (x > _XmTextNextX(w, 0, block.ptr, block.length))
    {
	pos = end;
    }
    /* must be in between somewhere... */
    else
    {
	int count, prevTot, tot, i, diff, mb;
	
	prevTot = tot = 0;
	count = end - start;
	pos = -1;
	diff = x;
	mb = 0;
	for (i = 0; i < count; i++)
	{
	    tot = Out_FontTextWidth(o, block.ptr, i);
	    if (x < tot)
	    {
		pos = start + i;
		if (tot - x > diff)
		    pos = pos - 1 - mb;

		break;
	    }
	    if (i && prevTot == tot) /* Fix at 1k2.2 */
		mb++;
	    else
		mb = 0;
	    prevTot = tot;
	    diff = x - tot;
	}

	if (pos < 0)
	{
	    pos = end;
	}
    }

    XtFree(block.ptr);

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		 "XToPos: x=%d start=%d end=%d pos=%d\n", x, start, end, pos));

    return pos;
}


static LineNum
YToLineNum(XmTextWidget w, Position y)
{
    OutputData o = Text_OutputData(w);
    LineNum line, l;

    l = line = (LineNum)((y - Out_TopMargin(o))
		/ Out_FontHeight(o));

    if (line >= Text_LineCount(w))
    {
	line = Text_LineCount(w);
    }

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"YToLineNum: y=%d line=%d (l %d Text_LineCount %d)\n",
		y, line, l, Text_LineCount(w)));

    return line;
}


static void
DrawText(XmTextWidget w, int x, int y, char *ptr, int len, XmHighlightMode highlight)
{
	OutputData o = Text_OutputData(w);
	int	xoff, xstart = x, xend;
	Pixel	fg, bg;

#ifdef VERBOSE
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"DrawText: x %d y %d '%s' len %d hl %d\n",
		x, y, (ptr == 0) ? "(NULL)" : ptr, len, highlight));
#endif

	xoff = Out_LeftMargin(o) - Out_XOffset(o);

#if 1
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"XmText DrawText x %d y %d '%s' len %d hl %d, fh %d\n",
		x + xoff, y, (ptr == 0) ? "(NULL)" : ptr, len, highlight,
		Out_FontHeight(o)));
#endif

	fg = Prim_Foreground(w);
	bg = XtBackground(w);

	/* Only change the GC if we need to */
	if (highlight == XmHIGHLIGHT_SELECTED) {
		fg = XtBackground(w);
		bg = Prim_Foreground(w);
	} else {
		fg = Prim_Foreground(w);
		bg = XtBackground(w);
	}

	while (len > 0 && *ptr) {
		char *start;
		Dimension piecewidth, pieceheight;
		int count;

		start = ptr;
		count = 0;
		while (len > 0 && *ptr && *ptr != '\t' && *ptr != '\n' ) {
			ptr++, count++, len--;
		}

		if (count > 0) {
			piecewidth = _XmTextNextX(w, 0, start, count);
			pieceheight = 10;	/* FIX ME */

			if (count > 0) {
				/* This is for bug #558840.
				 * Can be tested in test/Xm/text/test1 by double-clicking.
				 */
				unsigned long	mask = GCForeground | GCBackground;
				XGCValues	v;

				v.foreground = bg;
				v.background = fg;
				XChangeGC(XtDisplay(w), Out_DrawGC(o), mask, &v);
				XFillRectangle(XtDisplay(w), XtWindow(w), Out_DrawGC(o),
					x + xoff, y - Out_Font_Ascent(o),
					piecewidth, y + Out_Font_Descent(o));

				v.background = bg;
				v.foreground = fg;
				XChangeGC(XtDisplay(w), Out_DrawGC(o), mask, &v);

				if (Out_FontList(o)->renditions[0]->type == XmFONT_IS_FONT) {
					XDrawString(XtDisplay(w), XtWindow(w), Out_DrawGC(o),
						 x + xoff, y, start, count);
				} else if (Out_FontList(o)->renditions[0]->type == XmFONT_IS_FONTSET) {
					XmbDrawString(XtDisplay(w), XtWindow(w),
						(XFontSet)Out_FontList(o)->renditions[0]->font,
						Out_DrawGC(o),
						x + xoff, y,
						start, count);
#ifdef	USE_XFT
				} else if (Out_XftFont(o)) {
					_XmXftDrawString(XtDisplay(w), XtWindow(w),
						Out_FontList(o)->renditions[0],
						1,
						x + xoff, y,
						start, count);
#endif
				} else {
					/* ??? */
				}

			}

#ifdef VERBOSE
	    if (_LtDebugInDebug(__FILE__, (Widget)w))
	    {
		char temp[256];

		strncpy(temp, start, count);
		temp[count] = '\0';
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
				  "Draw: x=%d y=%d -->%s<--\n", x, y, temp));
	    }
#endif

			x += piecewidth;
		}

	if (len > 0 && *ptr == '\t')
	{
	    int left = x;
	    x = Out_NextTabStop(o, x);
	    if (highlight == XmHIGHLIGHT_SELECTED) {
		XSetForeground(XtDisplay(w), Out_DrawGC(o), Prim_Foreground(w));
		XFillRectangle(XtDisplay(w), XtWindow(w), Out_DrawGC(o),
			left + xoff, y - Out_Font_Ascent(o),
			x - left, Out_FontHeight(o));
		XSetForeground(XtDisplay(w), Out_DrawGC(o), XtBackground(w));
	    }

	    ptr++, len--;
	}
	if (len > 0 && *ptr == '\n')
	{
	    int left = x;

	    x = Out_NextTabStop(o, x);

	    if (highlight == XmHIGHLIGHT_SELECTED) {
		XSetForeground(XtDisplay(w), Out_DrawGC(o), Prim_Foreground(w));
		XFillRectangle(XtDisplay(w), XtWindow(w), Out_DrawGC(o),
			left + xoff, y - Out_Font_Ascent(o),
			Out_ScrollWidth(o) - ( left + xoff ),
			Out_FontHeight(o));
		XSetForeground(XtDisplay(w), Out_DrawGC(o), XtBackground(w));
	    }

	    ptr++, len--;
	}
    }

    xend = x;

    if (highlight == XmHIGHLIGHT_SECONDARY_SELECTED)
    {
	XDrawLine(XtDisplay(w), XtWindow(w), Out_DrawGC(o),
		  xoff + xstart, y + 1, xoff + xend, y + 1);
    }
}


static void
ChangeHOffset(XmTextWidget w, int offset)
{
    OutputData o = Text_OutputData(w);

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "ChangeHOffset(%d)\n", offset));

    if (Out_XOffset(o) == offset)
    {
	return;
    }

    Out_XOffset(o) = offset;
    Out_CursorSaveValid(o) = False;

    (*Text_Output(w)->expose) ((Widget)w, NULL, (Region)NULL);

}


extern void
_XmRedisplayHBar(XmTextWidget w, int offset)
{
    OutputData o = Text_OutputData(w);
    Dimension displayWidth, maxWidth;
    int oldMin, oldMax, oldSize, oldValue;
    int newValue;
    int i;

    Out_ScrollWidth(o) = XtWidth(w) - ( Out_RightMargin(o) + Out_LeftMargin(o) );
    if (Out_Hbar(o) == NULL)
    {
	return;
    }

    ChangeHOffset(w, offset);
    if (!Out_ScrollHorizontal(o))
    {
	return;
    }

    maxWidth = 0;
    for (i = 0; i < (Text_LineCount(w) < Out_Rows(o) ?
		     Text_LineCount(w) : Out_Rows(o)); i++)
    {
	if (maxWidth < Text_Line(w)[i].extra->width)
	{
	    maxWidth = Text_Line(w)[i].extra->width;
	}
    }

    displayWidth = XtWidth(w) - ( Out_RightMargin(o) + Out_LeftMargin(o) );

    if (maxWidth < displayWidth)
    {
	maxWidth = displayWidth;
    }

    Out_ScrollWidth(o) = maxWidth;

    XtVaGetValues(Out_Hbar(o),
		  XmNmaximum, &oldMax,
		  XmNminimum, &oldMin,
		  XmNsliderSize, &oldSize,
		  XmNvalue, &oldValue,
		  NULL);

    newValue = _XmMin(Out_XOffset(o), maxWidth - displayWidth);

    if (oldMax != maxWidth || oldMin != 0 ||
	oldSize != displayWidth || oldValue != newValue)
    {
	XtVaSetValues(Out_Hbar(o),
		      XmNmaximum, maxWidth,
		      XmNminimum, 0,
		      XmNsliderSize, displayWidth,
		      XmNvalue, newValue,
		      NULL);
    }

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
	       "_XmRedisplayHBar: oldmax=%d newmax=%d oldsize=%d newsize=%d\n",
		      oldMax, maxWidth, oldSize, displayWidth));
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "_XmRedisplayHBar: oldvalue=%d newvalue=%d\n",
		      oldValue, newValue));

}


extern void
_XmChangeVSB(XmTextWidget w, XmTextPosition pos)
{
    OutputData o = Text_OutputData(w);
    int top_index, pos_index, newValue;
    int oldMin = 0, oldMax = 0, oldSize = 0, oldValue = 0, newMax;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "_XmChangeVSB: pos=%d\n", pos));

    top_index = _XmTextGetTableIndex(w, Text_TopPos(w));
    pos_index = _XmTextGetTableIndex(w, pos);

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "_XmChangeVSB: top_index=%d pos_index=%d\n",
		      top_index, pos_index));

    if (Out_Vbar(o))
    {
	XtVaGetValues(Out_Vbar(o),
		      XmNmaximum, &oldMax,
		      XmNminimum, &oldMin,
		      XmNsliderSize, &oldSize,
		      XmNvalue, &oldValue,
		      NULL);
    }

/* SG (15/08/1998)    newMax = _XmMax(Text_TotalLines(w) - 1, Out_Rows(o));
   with the original code it was not possible to use the scrollbar to scroll
   down to the last line. Most obviously if out_rows is 1 (hence the slider
   size is 1) and there are 2 lines of text, setting scrollbar max to 1
   with a slider size of 1 means you cannot move the scroll bar at all ! */
    newMax = _XmMax(Text_TotalLines(w) , Out_Rows(o));

    if (newMax == 0)
    {
	newMax = 1;		/* Avoid another warning message ?? */
    }

    /* Setting XmNminimum == XmNmaximum is not a good idea */

#if 0
    /* rws 16 Jan 1999
       What does this do????  The scrollbar value should be the index of the
       first visible line, right??
     */
    newValue = pos_index > (newMax - Out_Rows(o))
	? (newMax - Out_Rows(o)) : pos_index;
#else
    newValue = top_index;
#endif

    if (Out_Vbar(o) && (oldMax != newMax || oldMin != 0 ||
			oldSize != Out_Rows(o) || oldValue != newValue))
    {
	XmScrollBarCallbackStruct cbs;

	XtVaSetValues(Out_Vbar(o),
		      XmNmaximum, newMax,
		      XmNminimum, 0,
		      XmNvalue, newValue,
		      XmNsliderSize, Out_Rows(o),
		      XmNpageIncrement, Out_Rows(o),
		      NULL);

	cbs.event = NULL;
	cbs.value = newValue;
	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.pixel = 0;

	XtCallCallbacks(Out_Vbar(o), XmNvalueChangedCallback, &cbs);
    }

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		   "_XmChangeVSB: oldmax=%d newmax=%d oldsize=%d newsize=%d\n",
		      oldMax, newMax, oldSize, Out_Rows(o)));

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "_XmChangeVSB: oldvalue=%d newvalue=%d\n",
		      oldValue, newValue));

}

/* Cursor functions -------------------------------------------------------- */

static void
CursorInit(XmTextWidget w)
{
    OutputData o = Text_OutputData(w);

    Out_CursorIBeam(o) = (Pixmap)NULL;
    Out_CursorStipple(o) = (Pixmap)NULL;
    Out_CursorSave(o) = (Pixmap)NULL;

    /* Out_CursorPositionVisible(o) = True; */
    Out_CursorSaveValid(o) = False;
}


void
_XmCursorOverstrike(Widget w)
{
    OutputData o = Text_OutputData(w);
    /* These change the GC !! (Danny) */
    if (Out_HasFocus(o)) {
	GCPixmap((XmTextWidget)w);
	if (Text_InputData(w)->overstrike) {
		XSetStipple(XtDisplay((Widget)w), Out_CursorGC(o),
			Out_CursorIBeam(o));
	} else {
		XSetStipple(XtDisplay((Widget)w), Out_CursorGC(o),
			Out_CursorIBeam(o));
	}
    } else {
	XSetStipple(XtDisplay((Widget)w), Out_CursorGC(o),
		    Out_CursorStipple(o));
    }
}

static void
CursorSet(XmTextWidget w, Boolean focus)
{
    OutputData o = Text_OutputData(w);

    Out_HasFocus(o) = focus;

    /* These change the GC !! (Danny) */
    if (focus) {
	XSetStipple(XtDisplay((Widget)w), Out_CursorGC(o),
		Out_CursorIBeam(o));
    } else {
	XSetStipple(XtDisplay((Widget)w), Out_CursorGC(o),
		    Out_CursorStipple(o));
    }
}


static void
CursorSaveUnderIBeam(XmTextWidget w, Position x, Position y)
{
    OutputData o = Text_OutputData(w);

    if (!XtIsRealized((Widget)w))
    {
	return;
    }

#ifdef VERBOSE
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "CursorSaveUnderIBeam\n"));
#endif

    /* Make sure that the text is drawn before saving the cursor */
    XFlush(XtDisplay((Widget)w));

    /* save the area under the cursor */
    XFillRectangle(XtDisplay((Widget)w), Out_CursorSave(o), Out_CopyGC(o),
		   0, 0, Out_CursorWidth(o), Out_CursorHeight(o) );

    XCopyArea(XtDisplay((Widget)w), XtWindow((Widget)w), Out_CursorSave(o),
	      Out_CursorGC(o),
	      x - 2, y,
	      Out_CursorWidth(o), Out_CursorHeight(o), 0, 0);

    Out_CursorSaveValid(o) = True;
}


static void
CursorRestoreUnderIBeam(XmTextWidget w, Position x, Position y)
{
    OutputData o = Text_OutputData(w);

    if (!XtIsRealized((Widget)w))
    {
	return;
    }

#ifdef VERBOSE
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "CursorRestoreUnderIBeam\n"));
#endif

    /* Make sure that the text is drawn before erasing the cursor */
    XFlush(XtDisplay((Widget)w));

    XCopyArea(XtDisplay((Widget)w), Out_CursorSave(o), XtWindow((Widget)w),
	      Out_CursorGC(o), 0, 0, 5, Out_FontHeight(o),
	      x - 2, y);

    Out_CursorSaveValid(o) = False;	/* Huh ?? FIX ME */
}


static void
CursorDrawIBeam(XmTextWidget w, int x, int y)
{
    OutputData o = Text_OutputData(w);

    /* save the area under the cursor */
    CursorSaveUnderIBeam(w, x, y);

    /* Bail if the GC's don't exist yet. */
    if (!XtIsRealized((Widget)w))
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "CursorDrawIBeam: no GC\n"));
	return;
    }

    /* These change the GC !! (Danny) */
    XSetTSOrigin(XtDisplay((Widget)w), Out_CursorGC(o),
		 x - 2, y);

    if (Text_InputData(w)->overstrike) {
    } else {
    }

	XFillRectangle(XtDisplay((Widget)w), XtWindow((Widget)w),
		Out_CursorGC(o),
		x - 2, y,
		Out_CursorWidth(o), Out_CursorHeight(o));
}


static void
CursorDraw(XmTextWidget w)
{
    OutputData o = Text_OutputData(w);

    if (Out_CursorPositionVisible(o))
    {
	CursorDrawIBeam(w, Out_CursorX(o), 
		( Out_CursorY(o) - Out_CursorHeight(o) ) + Out_Font_Descent(o) );
	Out_BlinkOn(o) = True;
    }
}


static void
CursorErase(XmTextWidget w)
{
    OutputData o = Text_OutputData(w);

    if (Out_CursorSaveValid(o))
    {
	CursorRestoreUnderIBeam(w, Out_CursorX(o), 
		( Out_CursorY(o) - Out_CursorHeight(o) ) + Out_Font_Descent(o) );
    }

    Out_BlinkOn(o) = False;
}

/* Callbacks --------------------------------------------------------------- */

static void
HandleTimer(XtPointer client_data, XtIntervalId *timer)
{
    XmTextWidget w = (XmTextWidget)client_data;
    OutputData o = Text_OutputData(w);

    OnOrOff nextBlinkState = Out_BlinkState(o) == on ? off : on;
    (*Text_Output(w)->DrawInsertionPoint) (w, Text_CursorPos(w),
					   nextBlinkState );
    Out_BlinkState(o) = nextBlinkState;
    Out_TimerId(o) = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)w),
				     Out_BlinkRate(o), HandleTimer,
				     (XtPointer)w);
}


static void
HandleHBar(Widget sw, XtPointer client_data, XtPointer call_data)
{
    XmTextWidget w = (XmTextWidget)client_data;
    OutputData o = Text_OutputData(w);
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)call_data;
    Dimension displayWidth, newOffset = 0;

    switch (cbs->reason)
    {
    case XmCR_DECREMENT:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		  "HandleHbar reason=XmCR_DECREMENT value= %d\n", cbs->value));

	newOffset = Out_XOffset(o) > Out_FontAverageWidth(o) ?
	    Out_XOffset(o) - Out_FontAverageWidth(o) : 0;
	break;

    case XmCR_DRAG:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			"HandleHbar reason=XmCR_DRAG value=%d\n", cbs->value));

	newOffset = cbs->value / Out_FontAverageWidth(o) *
	    Out_FontAverageWidth(o);
	break;

    case XmCR_INCREMENT:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		   "HandleHbar reason=XmCR_INCREMENT value=%d\n", cbs->value));

	displayWidth = XtWidth(w) - ( Out_RightMargin(o) + Out_LeftMargin(o) );

	if (Out_XOffset(o) < Out_ScrollWidth(o) - displayWidth - Out_FontAverageWidth(o))
	{
	    newOffset = Out_XOffset(o) + Out_FontAverageWidth(o);
	}
	else
	{
	    newOffset = Out_ScrollWidth(o) - displayWidth;
	}
	break;

    case XmCR_PAGE_DECREMENT:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleHbar reason=XmCR_PAGE_DECREMENT value=%d\n",
			  cbs->value));

#ifdef USE_AVERAGE_WIDTH
	newOffset = Out_XOffset(o) > Out_Columns(o) * Out_FontAverageWidth(o) ?
	    Out_XOffset(o) - Out_Columns(o) * Out_FontAverageWidth(o) : 0;
#else
	newOffset = Out_XOffset(o) > Out_Columns(o) * Out_FontMaxWidth(o) ?
	    Out_XOffset(o) - Out_Columns(o) * Out_FontMaxWidth(o) : 0;
#endif
	break;

    case XmCR_PAGE_INCREMENT:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleHbar reason=XmCR_PAGE_INCREMENT value=%d\n",
			  cbs->value));

	displayWidth = XtWidth(w) - 2 * (Prim_HighlightThickness(w) +
				Prim_ShadowThickness(w) + Text_MarginWidth(w));

#ifdef USE_AVERAGE_WIDTH
	if (Out_XOffset(o) < Out_ScrollWidth(o) - displayWidth -
	    Out_Columns(o) * Out_FontAverageWidth(o))
	{
	    newOffset = Out_XOffset(o) + Out_Columns(o) * Out_FontAverageWidth(o);
#else
	if (Out_XOffset(o) < Out_ScrollWidth(o) - displayWidth -
	    Out_Columns(o) * Out_FontMaxWidth(o))
	{
	    newOffset = Out_XOffset(o) + Out_Columns(o) * Out_FontMaxWidth(o);
#endif
	}
	else
	{
	    newOffset = Out_ScrollWidth(o) - displayWidth;
	}

	break;

    case XmCR_TO_BOTTOM:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			"HandleHbar reason=XmCR_TO_BOTTOM value=%d pixel=%d\n",
			  cbs->value, cbs->pixel));

	newOffset = 0;
	break;

    case XmCR_TO_TOP:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleHbar reason=XmCR_TO_TOP value=%d pixel=%d\n",
			  cbs->value, cbs->pixel));

	displayWidth = XtWidth(w) - 2 * (Prim_HighlightThickness(w) +
				Prim_ShadowThickness(w) + Text_MarginWidth(w));
	newOffset = Out_ScrollWidth(o) - displayWidth;
	break;

    case XmCR_VALUE_CHANGED:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleHbar reason=XmCR_VALUE_CHANGED value=%d\n",
			  cbs->value));
	newOffset = cbs->value / Out_FontAverageWidth(o) *
	    Out_FontAverageWidth(o);
	break;

    default:
	newOffset = Out_XOffset(o);
	break;
    }

    ChangeHOffset(w, newOffset);

    XtVaSetValues(Out_Hbar(o), XmNvalue, Out_XOffset(o), NULL);
}


static void
HandleVBar(Widget sw, XtPointer client_data, XtPointer call_data)
{
    XmTextWidget w = (XmTextWidget)client_data;
    OutputData o = Text_OutputData(w);
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)call_data;
    Dimension index;

    switch (cbs->reason)
    {
    case XmCR_DECREMENT:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleVbar reason=XmCR_DECREMENT\n"));

	XmTextScroll((Widget)w, -1);
	break;

    case XmCR_DRAG:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleVbar reason=XmCR_DRAG value=%d\n",
			  cbs->value));

	index = cbs->value;
	Text_TopPos(w) = Text_LineTable(w)[index].start_pos;
	Text_NeedsRefigureLines(w) = True;
	break;

    case XmCR_INCREMENT:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleVbar reason=XmCR_INCREMENT\n"));
	XmTextScroll((Widget)w, 1);
	break;

    case XmCR_PAGE_DECREMENT:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleVbar reason=XmCR_PAGE_DECREMENT\n"));

	XmTextScroll((Widget)w, -Out_Rows(o));
	break;

    case XmCR_PAGE_INCREMENT:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleVbar reason=XmCR_PAGE_INCREMENT\n"));

	XmTextScroll((Widget)w, Out_Rows(o));
	break;

    case XmCR_TO_BOTTOM:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleVbar reason=XmCR_TO_BOTTOM\n"));

	index = Text_TotalLines(w) - 1 - Out_Rows(o);
	Text_TopPos(w) = Text_LineTable(w)[index].start_pos;
	Text_NeedsRefigureLines(w) = True;
	break;

    case XmCR_TO_TOP:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleVbar reason=XmCR_TO_TOP\n"));

	Text_TopPos(w) = Text_FirstPos(w);
	Text_NeedsRefigureLines(w) = True;
	break;

    case XmCR_VALUE_CHANGED:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleVbar reason=XmVALUE_CHANGED\n"));

	index = cbs->value;
	Text_TopPos(w) = Text_LineTable(w)[index].start_pos;
	Text_NeedsRefigureLines(w) = True;
	break;

    default:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "HandleVbar reason=UNKNOWN\n %d", cbs->reason));
	break;
    }

    _XmRedisplayHBar(w, Out_XOffset(o));

/*CP : I do not think this is needed here
    Text_NeedsRefigureLines(w) = True;
*/
    (*w->core.widget_class->core_class.expose) ((Widget)w, NULL, (Region)NULL);

}


static void
HandleFocusEvents(Widget aw, XtPointer client_data, XEvent* event, Boolean* continueToDispatch )
{
    XmTextWidget w = (XmTextWidget)aw;
    OutputData o = Text_OutputData(w);

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "HandleFocusEvent event=%s\n",
		      event->type == FocusIn ? "FocusIn" : "FocusOut"));

    if ( event->type == FocusIn && event->xany.send_event && !Out_HasFocus(o) )
    {
	if (Text_FocusCallback(w) )
	{
	    XmAnyCallbackStruct cbs;
	    cbs.reason = XmCR_FOCUS;
	    cbs.event = event;

	    XtCallCallbackList((Widget)w, Text_FocusCallback(w), &cbs);
	}

	Out_HasFocus(o) = True;

	XSetStipple(XtDisplay(aw), Out_CursorGC(o), Out_CursorIBeam(o));

	if (Out_BlinkRate(o) > 0)
	{
/*CP:Handle timer will do all this
	    Out_TimerId(o) = XtAppAddTimeOut(XtWidgetToApplicationContext(aw),
					     Out_BlinkRate(o), HandleTimer,
					     (XtPointer)w);
*/
	    HandleTimer((XtPointer)w, 0);
	}
	(*Text_Output(w)->DrawInsertionPoint) (w, Text_CursorPos(w), on);
    }
    else if ( event->type == FocusOut && Out_HasFocus(o) )
    {
	if (Text_LosingFocusCallback(w))
	{
	    XmTextVerifyCallbackStruct cbs;
	    cbs.reason = XmCR_LOSING_FOCUS;
	    cbs.event = event;
	    cbs.currInsert = cbs.newInsert = Text_CursorPos(w);
	    cbs.startPos = cbs.endPos = 0;
	    cbs.text = NULL;

	    XtCallCallbackList((Widget)w, Text_LosingFocusCallback(w), &cbs);
	}
	Out_HasFocus(o) = False;

	XSetStipple(XtDisplay(aw), Out_CursorGC(o), Out_CursorStipple(o));
	(*Text_Output(w)->DrawInsertionPoint) (w, Text_CursorPos(w), on);

	if (Out_TimerId(o))
	{
	    XtRemoveTimeOut(Out_TimerId(o));
	    Out_TimerId(o) = 0;
	}
    }

}



/* Output Methods --------------------------------------------------------- */

static XmTextPosition
XYToPos(XmTextWidget w, Position x, Position y)
{
    LineNum row = YToLineNum(w, y);
    if ( row == Text_LineCount(w))
	return Text_LastPos(w);
    return XToPos(w, row, x);
}


/*
 * Returns whether the cursor is in an area that's currently visible.
 */
static Boolean
PosToXY(XmTextWidget w, XmTextPosition pos, Position *x, Position *y)
{
    OutputData o = Text_OutputData(w);
    Cardinal i;

    *x = *y = -1;		/* On purpose ! */

    if (pos < Text_TopPos(w))
    {
	return False;
    }

    for (i = 0; i < Text_LineCount(w); i++)
    {
	Line line = &Text_Line(w)[i];
	Line next = &Text_Line(w)[i + 1];

	if (pos >= line->start && pos < next->start)
	{
	    *y = Out_YDraw(o) + i * Out_FontHeight(o);
	    *x = Out_XDraw(o) - Out_XOffset(o) +
		FontTextWidth(w, line->start, pos);
	    return True;
	}
    }

    return False;
}


static void
MakePositionVisible(XmTextWidget w, XmTextPosition pos)
{
    OutputData o = Text_OutputData(w);
    unsigned int line, newTopLine;
    int start, offset;
    Dimension xpos, last_xpos;
    Boolean UpdateVSB = False;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "MakePositionVisible pos=%d\n", pos));

    line = _XmTextGetTableIndex(w, pos);
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "\tline=%d, top_line=%d, line_count=%d, rows=%d redisplay=%s refigure=%s\n", 
		      line,
		      Text_TopLine(w),
		      Text_LineCount(w),
		      Out_Rows(o),
		      Text_NeedsRedisplay(w) ? "True" : "False",
		      Text_NeedsRefigureLines(w) ? "True" : "False" ));

    if (line < Text_TopLine(w))
    {
	newTopLine = line;
    }
    else if (line > Text_TopLine(w) + Out_Rows(o) - 1)
    {
	newTopLine = line - Out_Rows(o) + 1;
    }
    else
    {
	newTopLine = Text_TopLine(w);
    }

    if (newTopLine != Text_TopLine(w))
    {
	Text_TopPos(w) = Text_LineTable(w)[newTopLine].start_pos;
	Text_NeedsRedisplay(w) = True;
	Text_NeedsRefigureLines(w) = True;
	UpdateVSB = True;
    }

    start = Text_LineTable(w)[line].start_pos;
    xpos = FontTextWidth(w, start, pos);
    last_xpos = XtWidth(w) - ( Out_RightMargin(o) + Out_LeftMargin(o) );

    if (xpos - Out_XOffset(o) > last_xpos)
    {
	offset = xpos - last_xpos;
    }
    else if (xpos < Out_XOffset(o))
    {
	offset = xpos;
    }
    else
    {
	offset = Out_XOffset(o);
    }

    /* FIX ME should Out_ScrollHorizontal and Out_ScrollVertical be False if
     * Text is not child of scrolledwindow
     */
    if (offset != Out_XOffset(o))
    {
	if (Out_ScrollHorizontal(o) && Out_Hbar(o))
	{
	    _XmRedisplayHBar(w, offset);
	}
	else
	{
	    ChangeHOffset(w, offset);
	}
    }

    if (Out_ScrollVertical(o) && Out_Vbar(o) && UpdateVSB)
    {
	_XmChangeVSB(w, pos);
    }
}


static Boolean
MoveLines(XmTextWidget w, LineNum from, LineNum to, LineNum dest)
{
    return False;
}


static Boolean
MeasureLine(XmTextWidget w, LineNum num, XmTextPosition pos,
	    XmTextPosition *next, LineTableExtraRec **extra)
{
    OutputData o = Text_OutputData(w);
    Dimension width = 0;
    XmTextPosition start, end;
    LineTableExtra e;

#ifdef VERBOSE
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
	  "MeasureLine: line=%d pos=%d %s %s\n",
	  num, pos, 
	  next ? "wants next" : "",
	  extra ? "wants width": ""));
#endif
    start = pos < Text_LastPos(w) ? pos : Text_LastPos(w);
    if (start < Text_LastPos(w))
    {
	end = (*Text_Source(w)->Scan) (Text_Source(w), pos,
				       XmSELECT_LINE, XmsdRight, 1, False);
    }
    else
    {
	end = Text_LastPos(w);
    }
#ifdef VERBOSE
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
    	"\tmeasure from %d to %d\n", start, end));
#endif

    if (next)
    {
	if (end >= Text_LastPos(w))
	{
	    *next = (XmTextPosition)PASTENDPOS;
#ifdef VERBOSE
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"\tnext PASTENDPOS\n"));
#endif
	}
	else
	{
	    *next = end + 1;
#ifdef VERBOSE
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"\tnext %d\n", *next));
#endif
	}

	width = FontTextWidth(w, start, end);

	if (extra)
	{
	    e = (LineTableExtra)XtMalloc(sizeof(LineTableExtraRec));
	    e->width = width;
	    e->wrappedbychar = False;

	    *extra = e;
	}
    }

#if VERBOSE
    /* This is incredibly verbose; besides it seems to work. */
    if (_LtDebugInDebug(__FILE__, (Widget)w))
    {
	if (next)
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			      "MeasureLine: line=%d pos=%d next=%d width=%d\n",
			      num, pos, *next, width));
	else
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			    "MeasureLine: line=%d pos=%d next=NULL width=%d\n",
			      num, pos, width));
    }
#endif

    if (num <= Out_Rows(o) - 1)
    {
	return True;
    }
    else
    {
	_XmRedisplayHBar(w, Out_XOffset(o));

	return False;
    }
}


/*
 * DrawInsertionPoint() gets called all the time from TextIn.c, which doesn't
 *      keep track of whether the insertion point should be visible at all.
 * We should therefore implement XmNcursorPositionVisible here.
 *
 * Between calls of DrawInsertionPoint() we should keep track of whether and
 *      where the cursor is with Out_CursorX(o) and Out_CursorY(o); set them
 *      to -1 for no cursor.
 */
static void
DrawInsertionPoint(XmTextWidget w, XmTextPosition pos, OnOrOff state)
{
    OutputData o = Text_OutputData(w);
    Position x, y;

#if 1
/* rws 1 Apr 2000
WRT the disappearing cursor....

HandleTimer seems to be getting called when it should. This takes care of
setting Out_BlinkState, then calls DrawInsertionPoint, which is also
messing with Out_BlinkState. I'm not sure what Out_BlinkOn is keeping track
of, but I would expect the two if clauses near the start of
DrawInsertionPoint to be a little more symetrical wrt Out_BlinkOn and
Out_BlinkState. Commenting out the two lines messing with the BlinkState
seems to keep my cursor from disappearing. I've bracketted the section in
question with #if 1, it needs a little more investigation.....
*/
    if ( state == off )
    {
	if ( Out_BlinkOn(o) == 0 )
	{
	    Out_BlinkState(o) = off;
	}
	Out_BlinkOn(o) -- ;
    }
    if ( state == on )
    {
	if ( Out_BlinkOn(o) < 0 )
	{
	    Out_BlinkOn(o)++ ;
	}
	Out_BlinkState(o) = on;
    }
#endif

    if ( Out_BlinkOn(o) != 0 )
    {
	return;
    }

    if (pos == PASTENDPOS)	/* No No No : don't want this */
    {
	pos = Text_LastPos(w);	/* ??? Danny 6/7/97 */
    }

#ifdef VERBOSE
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
	      "DrawInsertionPoint Cursor : Pos %d, Save %sValid, %s Visible\n",
		      pos, Out_CursorSaveValid(o) ? "" : "not ",
		      Out_CursorPositionVisible(o) ? "" : "not"));
#endif

#if 0
    /* Experimental */
    /* Figure out what to do with the cursor here, before actually showing it. */
    if (!Out_CursorPositionVisible(o))
    {
	if (state == off)
	    CursorErase(w);
	return;
    }
#endif

    if (!XtIsRealized((Widget)w))
    {
	return;
    }
    /* Force to get forcus to display strings quickly. */
    XmImSetFocusValues((Widget)w, NULL, 0);

    /* erase previously visible cursor */
    if (Out_CursorX(o) >= 0 && Out_CursorY(o) >= 0)
    {
	CursorErase(w);
    }

    if (state == on)
    {
	/* draw new cursor
	 * PosToXY returns whether the position is in the area currently
	 * shown
	 */
	if ((*Text_Output(w)->PosToXY) (w, pos, &x, &y))
	{
	    Out_CursorX(o) = x;
	    Out_CursorY(o) = y;

#if 0
	/*
	 * This shouldn't be necessary 'cause CursorDraw calls CursorDrawIBeam
	 * which already takes care of this.
	 */
	    if (!Out_CursorSaveValid(o))
	    {
		CursorSaveUnderIBeam(w, Out_CursorX(o), Out_CursorY(o));
	    }
#endif

#ifdef VERBOSE
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "Draw cursor at %d %d\n",
			      Out_CursorX(o), Out_CursorY(o)));
#endif

	    CursorDraw(w);
	}
    }

#if VERBOSE
    if (_LtDebugInDebug(__FILE__, (Widget)w))
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "DrawInsertionPoint: x=%d pos=%d state=%s\n",
			  Out_CursorX(o), pos, (state == on ? "on" : "off")));
    }
#endif
}


static void
Draw(XmTextWidget w, LineNum num, XmTextPosition start, XmTextPosition end,
     XmHighlightMode highlight)
{
    OutputData o = Text_OutputData(w);
    XmTextBlockRec block;
    Line line = &Text_Line(w)[num];

#ifdef	DANNY
    fprintf(stderr, "XmText Draw(%s dpy %p)\n", XtName(w), XtDisplay(w));
#endif

    if (start > Text_LastPos(w)) {
	return;
    }

    if (end > Text_LastPos(w))
    {
	end = Text_LastPos(w);
	if (line->start > end)
	{
	    return;
	}
    }

    (*Text_Source(w)->ReadSource) (Text_Source(w), line->start, end, &block);

    if (block.length > 0)
    {
	int x, y;
	char *ptr;
	int len;

	ptr = block.ptr + start - line->start;
	len = end - start;
	if (start > line->start)
	{
	    x = _XmTextNextX(w, 0, block.ptr, start - line->start);
	}
	else
	{
	    x = 0;
	}

	y = Out_YDraw(o) + num * Out_FontHeight(o);

	DrawText(w, x, y, ptr, len, highlight);
    }

    XtFree(block.ptr);
}


static void
OutputInvalidate(XmTextWidget w, XmTextPosition start, XmTextPosition end,
		 long delta)
{
    OutputData o = Text_OutputData(w);
    Out_CursorSaveValid(o) = False;
}


static void
GetPreferredSize(Widget aw, Dimension *width, Dimension *height)
{
	XmTextWidget	w = (XmTextWidget)aw;
	OutputData	o = Text_OutputData(w);

	*height = Out_TopMargin(o) + Out_BottomMargin(o) +
		Out_Rows(o) * Out_FontHeight(o);

#ifdef	USE_XFT
	if (Out_XftFont(o)) {
		*width = Out_LeftMargin(o) + Out_RightMargin(o) +
			Out_Columns(o) * Out_FontAverageWidth(o);
		DEBUGOUT(_LtDebug(__FILE__, aw, "GetPreferredSize (fw %d )-> wid %d ht %d\n",
			Out_FontAverageWidth(o), *width, *height));
	} else
#endif
	{
#ifdef USE_AVERAGE_WIDTH
		*width = Out_LeftMargin(o) + Out_RightMargin(o) +
			Out_Columns(o) * Out_FontAverageWidth(o) ;
		DEBUGOUT(_LtDebug(__FILE__, aw, "GetPreferredSize (fw %d )-> wid %d ht %d\n",
				Out_FontAverageWidth(o), *width, *height));
#else
		*width = Out_LeftMargin(o) + Out_RightMargin(o) +
			Out_Columns(o) * Out_FontMaxWidth(o) ;
		DEBUGOUT(_LtDebug(__FILE__, aw, "GetPreferredSize (fw %d )-> wid %d ht %d\n",
				Out_FontMaxWidth(o), *width, *height));
#endif
	}
}


static void
OutputGetValues(Widget w, ArgList args, Cardinal num_args)
{
    XmTextInnerWidget iw = Text_InnerWidget(w);

#if VERBOSE
    /* Just print a list of resources requested */
    DEBUGOUT(_LtDebug(__FILE__, w, "OutputGetValues"));

    if (_LtDebugInDebug(__FILE__, w) && num_args != 0)
    {
	int i;

	DEBUGOUT(_LtDebug0(__FILE__, w, "("));
	for (i = 0; i < num_args - 1; i++)
	{
	    DEBUGOUT(_LtDebug0(__FILE__, w, "%s, ", args[i].name));
	}

	DEBUGOUT(_LtDebug0(__FILE__, w, "%s)", args[num_args - 1].name));
    }
    DEBUGOUT(_LtDebug0(__FILE__, w, "\n"));
#endif

    XtGetSubvalues(iw, output_resources, XtNumber(output_resources),
		   args, num_args);
    if (_LtDebugInDebug(__FILE__, w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "OutputGetValues :\n"));
	_LtDebugPrintArgList(__FILE__, w, args, num_args, True);
    }
}


/*
 * Oh boy. We're getting called directly, not through Xt. Therefore, we
 * have different old, new widgets (XmText), but they all point to the
 * same OutputData.
 *
 * So, enter the hack of the month : we'll have to take care of this. :-(
 * The trouble is all the stuff that's underneath... we'll see how far this
 * leads us. We may have to find another way to solve this.
 *
 * Note we're replacing the *old* variables by temp variables; so the functions
 * we call from here all get the real stuff and change things in the right
 * places.
 *
 * 19/5/97 : already on two levels (OutputRec and OutputDataRec).
 */
static Boolean
OutputSetValues(Widget old, Widget request, Widget new_w,
		ArgList args, Cardinal *num_args)
{
    OutputRec oldOutput;
    OutputDataRec oldData;
    OutputData nd = Text_OutputData(new_w), od = &oldData;
    XmTextInnerWidget iw = Text_InnerWidget(new_w);
    Boolean redraw = False;


    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "OutputSetValues: %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t    new X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    /* Insert new variables */
    oldOutput = *Text_Output(old);	/* Copy old values */
    Text_Output(old) = &oldOutput;
    *od = *nd;			/* Copy old values */
    Text_OutputData(old) = od;

    /* Ready */

    /* Copy whatever we get passed into our data structures */
    XtSetSubvalues(iw, output_resources, XtNumber(output_resources),
		   args, *num_args);

    Out_RightMargin(nd) = Out_LeftMargin(nd) = Prim_ShadowThickness(new_w) + 
		Prim_HighlightThickness(new_w) + Text_MarginWidth(new_w) ;
    Out_TopMargin(nd) = Out_BottomMargin(nd) = Prim_ShadowThickness(new_w) + 
		Prim_HighlightThickness(new_w) + Text_MarginHeight(new_w) ;

    if (Out_RightMargin(od) != Out_RightMargin(nd) ||  Out_LeftMargin(nd) !=  Out_LeftMargin(od) 
	|| Out_TopMargin(nd) != Out_TopMargin(od) || Out_BottomMargin(nd) != Out_BottomMargin(od))
    {
	(*Text_Output(old)->resize) (new_w, True);          
	redraw = True;
    }
    /* Check if we need to act */

    if (Out_FontList(od) != Out_FontList(nd))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "OutputSetValues: FontList change.\n"));

	/* T. Straumann: make a copy of the font list and free the old one */
	Out_FontList(nd) = XmFontListCopy(Out_FontList(nd));
	/* rws 17 Jul 1999
	   But only free it if we created it....
	   xmgrace Data->Results
	*/
	if (Out_FontListCreated(od) && Out_FontList(od)) {
		XmFontListFree(Out_FontList(od));
	}

	FontInit((XmTextWidget)new_w);
	/* From OutputCreate : */
	XtHeight(new_w) = Out_FontHeight(nd)
	    + 2 * (Prim_ShadowThickness(new_w) + Text_MarginHeight(new_w));
#ifdef USE_AVERAGE_WIDTH
	XtWidth(new_w) = Out_Columns(nd) * Out_FontAverageWidth(nd)
		+ Out_LeftMargin(nd) + Out_RightMargin(nd) ;
#else
	XtWidth(new_w) = Out_Columns(nd) * Out_FontMaxWidth(nd)
		+ Out_LeftMargin(nd) + Out_RightMargin(nd) ;
#endif

	if (Out_DrawGC(nd)) {
	    /*
	     * We need to change the font id in the DrawGC,
	     * not the other GC's.
	     *
	     * Note we can only do this because the mask used when
	     * we got this GC specifies that we can change the font.
	     */
	    XGCValues	values;

	    if (Out_FontType(nd) == XmFONT_IS_FONT) {
	    	values.font = Out_Font(nd)->fid;
		XChangeGC(XtDisplay(new_w), Out_DrawGC(nd), GCFont, &values);
	    } else
#ifdef	USE_XFT
	    if (Out_FontType(nd) == XmFONT_IS_XFT) {
	    	/* Probably nothing to do here */
	    }
#endif

	    {
		/*
		 * Need braces here to provide an 'else' statement
		 * in case that USE_XFT is not defined.
		 */

		/* We may still need to do things here.
		 * CP Hennessy suggest calling
			GCClip((XmTextWidget)new_w);
		 */
	    }
	}

	redraw = True;
    }

    if (Out_WordWrap(od) != Out_WordWrap(nd))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "OutputSetValues: WordWrap change.\n"));
		/*CP:15 May 1999:rebuild the line table */
	_XmTextUpdateLineTable(new_w, 0, 0, NULL, False); 
	redraw = True;
    }

    if (Out_CursorPositionVisible(od) != Out_CursorPositionVisible(nd))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "OutputSetValues: CursorPosVis change.\n"));

	/* FIX ME ?? */
	DrawInsertionPoint((XmTextWidget)new_w, Text_CursorPos(new_w), off);

	redraw = True;
    }

    if (Out_Rows(od) != Out_Rows(nd))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w, "OutputSetValues: Rows change.\n"));

	/* FIX ME ? */
	XtHeight(new_w) = Out_Rows(nd) * Out_FontHeight(nd)
	    + Out_TopMargin(nd) + Out_BottomMargin(nd) ;

	redraw = True;
    }

    if (Out_Columns(od) != Out_Columns(nd))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w, "OutputSetValues: Columns change.\n"));

	/* FIX ME ? */
#ifdef	USE_XFT
	if (Out_XftFont(nd)) {
		XtWidth(new_w) = Out_Columns(nd) * Out_XftFont(nd)->max_advance_width
			+ Out_LeftMargin(nd) + Out_RightMargin(nd);
		XtWidth(new_w) = Out_Columns(nd) * Out_FontAverageWidth(nd)
			+ Out_LeftMargin(nd) + Out_RightMargin(nd);	/* FIX ME HACK */
	} else
#endif
	{
#ifdef USE_AVERAGE_WIDTH
		XtWidth(new_w) = Out_Columns(nd) * Out_FontAverageWidth(nd)
			+ Out_LeftMargin(nd) + Out_RightMargin(nd);
#else
		XtWidth(new_w) = Out_Columns(nd) * Out_FontMaxWidth(nd)
			+ Out_LeftMargin(nd) + Out_RightMargin(nd);
#endif
	}

	redraw = True;
    }

    if ( XtBackground(old) != XtBackground(new_w) )
    {
	if (Out_DrawGC(nd)) {
	    XGCValues	values;

	    values.background =  XtBackground(new_w);
	    XChangeGC(XtDisplay(new_w), Out_DrawGC(nd), GCBackground, &values );
	}
    }

    if ( Prim_Foreground(old) != Prim_Foreground(new_w) )
    {
	if (Out_DrawGC(nd)) {
	    XGCValues	values;

	    values.foreground =  Prim_Foreground(new_w);
	    XChangeGC(XtDisplay(new_w), Out_DrawGC(nd), GCForeground, &values );
	}
    }

    if (Out_BlinkRate(od) != Out_BlinkRate(nd))
    {
	/* FIX ME */
    }

    if (Out_ResizeHeight(od) != Out_ResizeHeight(nd))
    {
	/* FIX ME */
    }

    if (Out_ResizeWidth(od) != Out_ResizeWidth(nd))
    {
	/* FIX ME */
    }

    if (Out_ScrollHorizontal(od) != Out_ScrollHorizontal(nd))
    {
	/* FIX ME */
    }

    if (Out_ScrollVertical(od) != Out_ScrollVertical(nd))
    {
	/* FIX ME */
    }

    if (Out_ScrollLeftSide(od) != Out_ScrollLeftSide(nd))
    {
	/* FIX ME */
    }

    if (Out_ScrollTopSide(od) != Out_ScrollTopSide(nd))
    {
	/* FIX ME */
    }

    /* Done, restore old stuff */
    Text_Output(old) = Text_Output(new_w);
    Text_OutputData(old) = nd;

    return redraw;
}


static void
destroy(Widget aw)
{
    XmTextWidget w = (XmTextWidget)aw;
    OutputData o = Text_OutputData(w);

    DEBUGOUT(_LtDebug(__FILE__, aw, "destroy\n"));

    if (o && Out_TimerId(o))
    {
	XtRemoveTimeOut(Out_TimerId(o));
	Out_TimerId(o) = 0;
    }

    if (o && Out_DrawGC(o))
    {
#ifdef	USE_XT_GC
	XtReleaseGC(aw, Out_DrawGC(o));
#else
	XFreeGC(XtDisplay(w), Out_DrawGC(o));
#endif
    }
    if (o && Out_CursorGC(o))
    {
#ifdef	USE_XT_GC
	XtReleaseGC(aw, Out_CursorGC(o));
#else
	XFreeGC(XtDisplay(w), Out_CursorGC(o));
#endif
    }
    if (o && Out_CopyGC(o))
    {
	XtReleaseGC((Widget)w, Out_CopyGC(o));
    }

    if (Out_CursorIBeam(o))
    {
	XFreePixmap(XtDisplay((Widget)w), Out_CursorIBeam(o));
    }
    if (Out_CursorStipple(o))
    {
	XFreePixmap(XtDisplay((Widget)w), Out_CursorStipple(o));
    }
    if (Out_CursorSave(o))
    {
	XFreePixmap(XtDisplay((Widget)w), Out_CursorSave(o));
    }

    if (Out_FontListCreated(o) && Out_FontList(o))
    {
	XmFontListFree(Out_FontList(o));
    }
#ifdef	USE_XFT
	/* FIX ME */
#endif

    XtFree((char *)Text_Output(w));
}


static void
OutputExpose(Widget aw, XEvent *event, Region region)
{
	XmTextWidget w = (XmTextWidget)aw;

	DEBUGOUT(_LtDebug(__FILE__, aw, "OutputExpose: cursor pos: %d\n",
		Text_CursorPos(w)));

	if (!XtIsRealized(aw)) {
		return;
	}

/*
    XClearArea(XtDisplay(w), XtWindow(w),
	       Prim_HighlightThickness(w) + Prim_ShadowThickness(w),
	       Prim_HighlightThickness(w) + Prim_ShadowThickness(w),
	       XtWidth(w) -
	       2 * (Prim_HighlightThickness(w) + Prim_ShadowThickness(w)),
	       XtHeight(w) -
	       2 * (Prim_HighlightThickness(w) + Prim_ShadowThickness(w)),
	       False);
*/

    DrawAll(w);

    if (Prim_Highlighted(aw))
    {
	(*PrimC_BorderHighlight(XtClass(aw))) (aw);
    }
    else
    {
	(*PrimC_BorderUnhighlight(XtClass(aw))) (aw);
    }

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		   XtWidth(w) - 2 * Prim_HighlightThickness(w),
		   XtHeight(w) - 2 * Prim_HighlightThickness(w),
		   Prim_ShadowThickness(w),
		   XmSHADOW_IN);

    (*Text_Output(w)->DrawInsertionPoint) (w, Text_CursorPos(w), on);
}


static void
OutputRealize(Widget aw, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
	XmTextWidget w = (XmTextWidget)aw;

	DEBUGOUT(_LtDebug(__FILE__, aw, "OutputRealize (fl %p)\n",
		Out_FontList(Text_OutputData(aw))));

	GCInitialize(w);
	GCClip(w);

	CursorSet(w, False);

#if 0
	CursorSaveUnderIBeam(w, 0, 0);
#endif
}


static void
resize(Widget aw, Boolean state)
{
	XmTextWidget w = (XmTextWidget)aw;
	OutputData o = Text_OutputData(w);

	SizeRecalc(w);

	/* SG (15/08/1998) Update scrollbars immediately on resize
	 * to give the correct visual feedback to the user and keep
	 * scroll bars in sync with any keyboard actions after this resize */

	_XmChangeVSB(w, Text_TopPos(w));
        _XmRedisplayHBar(w, Out_XOffset(o));

	/* Only adjust the GCs if they have already been created! */
	if (Out_DrawGC(o)) {
		GCClip(w);
	}
}


static OutputRec outputRec =
{
    /* _OutputDataRec            */ NULL,
    /* XYToPosProc               */ XYToPos,
    /* PosToXYProc               */ PosToXY,
    /* MeasureLineProc           */ MeasureLine,
    /* DrawProc                  */ Draw,
    /* DrawInsertionPointProc    */ DrawInsertionPoint,
    /* MakePositionVisibleProc   */ MakePositionVisible,
    /* MoveLinesProc             */ MoveLines,
    /* InvalidateProc            */ OutputInvalidate,
    /* GetPreferredSizeProc      */ GetPreferredSize,
    /* GetValuesProc             */ OutputGetValues,
    /* SetValuesProc             */ OutputSetValues,
    /* XmRealizeOutProc          */ OutputRealize,
    /* XtWidgetProc              */ destroy,
    /* XmResizeFlagProc          */ resize,
    /* XtExposeProc              */ OutputExpose
};


extern void
_XmTextOutputCreate(Widget aw, ArgList args, Cardinal num_args)
{
    XmTextWidget w = (XmTextWidget)aw;
    OutputData o;
    XmTextInnerWidget iw = (XmTextInnerWidget)w->text.inner_widget;
    Dimension width, height;

    DEBUGOUT(_LtDebug(__FILE__, aw, "_XmTextOutputCreate("));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, aw, args, num_args, False));
    DEBUGOUT(_LtDebug0(__FILE__, aw, ")\n"));

    Text_Output(w) = (Output)XtMalloc(sizeof(OutputRec));
    memcpy(Text_Output(w), &outputRec, sizeof(OutputRec));

    Text_OutputData(w) = o = &iw->inner.out;
    XtGetSubresources(aw, iw,
		      aw->core.name,
		      aw->core.widget_class->core_class.class_name,
		      output_resources,
		      XtNumber(output_resources),
		      args, num_args);

    Out_XOffset(o) = 0;

    /*CP: debugging */
    XtAddEventHandler(aw, FocusChangeMask, False, HandleFocusEvents, NULL);

    CursorInit(w);

    /* Use DrawGC as a flag to see if the GCs have been created */
    Out_DrawGC(o) = NULL;
    Out_RightMargin(o) = Out_LeftMargin(o) = Prim_ShadowThickness(w) + Prim_HighlightThickness(w)
		+ Text_MarginWidth(w) ;
    Out_TopMargin(o) = Out_BottomMargin(o) = Prim_ShadowThickness(w) + Prim_HighlightThickness(w)
		+ Text_MarginHeight(w) ;


    Out_CursorWidth(o) = 5; /*CP: M*tif seems to use 5, but why ?? */

    /* Get the font information */
    if ( !Out_FontList(o) || (Out_FontList(o) == (XmFontList)XmUNSPECIFIED) )
    {
	Out_FontList(o) = _XmGetDefaultFontList((Widget)w, XmTEXT_FONTLIST);
	/* T. Straumann: changed this to 'true'; XmGetDefaultFontList() seems
	 *				 to make a copy.
	 */
	Out_FontListCreated(o) = True;
    }
    else
    {
	/* Out_FontListCreated(o) = True; */
	Out_FontListCreated(o) = False;
	/* we created this, so free it when the time comes */
	/* rws 12 Apr 1999
	   If we created this, where???? A True value here is causing ml to
	   segfault in FontListFree whenever the message window is destroyed.
	 */
    }

    FontInit(w);

    DEBUGOUT(_LtDebug(__FILE__, aw,
		      "OutputCreate (initially) : rows %d cols %d\n",
		      Out_Rows(o), Out_Columns(o)));

    /* Check window dimensions */
    if (XtWidth(w) == (Dimension)0)
    {
	if (Out_Columns(o) <= 0)
	{
	    Out_Columns(o) = 20;
	}
    }
    else
    {
	/* Reverse of this thing also copied into OutputSetValues */
#ifdef	USE_XFT
	if (Out_XftFont(o)) {
		Out_Columns(o) = (XtWidth(w) -
			(Out_LeftMargin(o) + Out_RightMargin(o)))
			/ Out_XftFont(o)->max_advance_width;
		Out_Columns(o) = (XtWidth(w) -
			(Out_LeftMargin(o) + Out_RightMargin(o)))
			/ Out_FontAverageWidth(o);	/* FIX ME HACK */
	} else
#endif
	{
#ifdef USE_AVERAGE_WIDTH
	Out_Columns(o) = (XtWidth(w) - ( Out_LeftMargin(o) + Out_RightMargin(o) )
		) / Out_FontAverageWidth(o);
#else
	Out_Columns(o) = (XtWidth(w) - ( Out_LeftMargin(o) + Out_RightMargin(o) )
		) / Out_FontMaxWidth(o);
#endif
	}
    }

    if (XtHeight(w) == (Dimension)0)
    {
	if (Out_Rows(o) <= 0)
	{
	    Out_Rows(o) = 10;
	}
    }
    else
    {
	Out_Rows(o) = (XtHeight(w) - ( Out_TopMargin(o) + Out_BottomMargin(o) )
		) / Out_FontHeight(o);
    }

    if (Text_EditMode(w) == XmSINGLE_LINE_EDIT)
    {
	Out_Rows(o) = 1;
	if (XtHeight(w) != 0 &&
	    (XtHeight(w) < Out_FontHeight(o)
		+ Out_TopMargin(o) + Out_BottomMargin(o) ) )
	{
	    /* Statement below also copied into OutputSetValues */
	    XtHeight(w) = Out_FontHeight(o)
		+ Out_TopMargin(o) + Out_BottomMargin(o);
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, aw,
		      "OutputCreate: rows %d cols %d\n",
		      Out_Rows(o), Out_Columns(o)));
    Out_ColumnsSet(o) = Out_Columns(o);
    Out_RowsSet(o) = Out_Rows(o);

    if (XtWidth(w) == 0 || XtHeight(w) == 0) {
	(*Text_Output(w)->GetPreferredSize) (aw, &width, &height);
	XtWidth(w) = width;
	XtHeight(w) = height;
    }

    SizeRecalc(w);

    /* Create scrollbars if child of scrolledwindow */

    if (XmIsScrolledWindow(XtParent(w)))
    {
	Widget sw = XtParent(w);
	Boolean scrollingPolicy;

	XtVaGetValues(sw, XmNscrollingPolicy, &scrollingPolicy, NULL);

	if (scrollingPolicy == XmAUTOMATIC)
	{
	    Out_ScrollVertical(o) = False;
	    Out_ScrollHorizontal(o) = False;
	}

	if (Out_ScrollHorizontal(o))
	{

	    if (SW_HSB(sw) != NULL)
	    {
		Out_Hbar(o) = (Widget)SW_HSB(sw);
	    }
	    else
	    {
		Out_Hbar(o) = XtVaCreateWidget("HorScrollBar",
					       xmScrollBarWidgetClass,
					       sw,
					       XmNorientation, XmHORIZONTAL,
					       NULL);
	    }


	    XtAddCallback((Widget)Out_Hbar(o), XmNdecrementCallback,
			  HandleHBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Hbar(o), XmNdragCallback,
			  HandleHBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Hbar(o), XmNincrementCallback,
			  HandleHBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Hbar(o), XmNpageDecrementCallback,
			  HandleHBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Hbar(o), XmNpageIncrementCallback,
			  HandleHBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Hbar(o), XmNtoBottomCallback,
			  HandleHBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Hbar(o), XmNtoTopCallback,
			  HandleHBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Hbar(o), XmNvalueChangedCallback,
			  HandleHBar,
			  (XtPointer)w);
	    XtManageChild(Out_Hbar(o));

	    /*CP:Friday 14 May 1999: Set HBar resources to initially sane values */
	    XtVaSetValues(Out_Hbar(o),
			  XmNincrement, 1,
			  XmNpageIncrement, 1,
			  XmNsliderSize, 1,
			  XmNmaximum, 1, 
			  XmNminimum, 0,
			  NULL);
	}
	else
	{
	    Out_Hbar(o) = NULL;
	}

	if (Out_ScrollVertical(o) && Text_EditMode(w) != XmSINGLE_LINE_EDIT)
	{
	    if (SW_VSB(sw) != NULL)
	    {
		Out_Vbar(o) = (Widget)SW_VSB(sw);
	    }
	    else
	    {
		Out_Vbar(o) = XtVaCreateWidget("VertScrollBar",
					       xmScrollBarWidgetClass,
					       sw,
					       NULL);
	    }

	    XtAddCallback((Widget)Out_Vbar(o), XmNdecrementCallback,
			  HandleVBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Vbar(o), XmNdragCallback,
			  HandleVBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Vbar(o), XmNincrementCallback,
			  HandleVBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Vbar(o), XmNpageDecrementCallback,
			  HandleVBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Vbar(o), XmNpageIncrementCallback,
			  HandleVBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Vbar(o), XmNtoBottomCallback,
			  HandleVBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Vbar(o), XmNtoTopCallback,
			  HandleVBar,
			  (XtPointer)w);
	    XtAddCallback((Widget)Out_Vbar(o), XmNvalueChangedCallback,
			  HandleVBar,
			  (XtPointer)w);
	    XtManageChild(Out_Vbar(o));

	    /* Set VSB resources to initially sane values */
	    XtVaSetValues(Out_Vbar(o),
			  XmNincrement, 1,
			  XmNpageIncrement, Out_Rows(o),
			  XmNsliderSize, Out_Rows(o),
			  XmNmaximum, Out_Rows(o), /*CP:Friday 14 May 1999 a reasonable initial value */
			  NULL);
	}
	else
	{
	    Out_Vbar(o) = NULL;
	}

	XmScrolledWindowSetAreas(sw, Out_Hbar(o), Out_Vbar(o), aw);

	if (Out_ScrollLeftSide(o) && Out_ScrollTopSide(o))
	{
	    XtVaSetValues(sw,
			  XmNscrollBarPlacement, XmTOP_LEFT,
			  NULL);
	}
	else if (!Out_ScrollLeftSide(o) && Out_ScrollTopSide(o))
	{
	    XtVaSetValues(sw,
			  XmNscrollBarPlacement, XmTOP_RIGHT,
			  NULL);
	}
	else if (Out_ScrollLeftSide(o) && !Out_ScrollTopSide(o))
	{
	    XtVaSetValues(sw,
			  XmNscrollBarPlacement, XmBOTTOM_LEFT,
			  NULL);
	}
	else if (!Out_ScrollLeftSide(o) && !Out_ScrollTopSide(o))
	{
	    XtVaSetValues(sw,
			  XmNscrollBarPlacement, XmBOTTOM_RIGHT,
			  NULL);
	}
    }

}



/*
 * Quasi-Public functions -----------------------------------------------------
 */


extern void
_XmTextDrawDestination(XmTextWidget widget)
{
}


extern void
_XmTextClearDestination(XmTextWidget widget, Boolean ignore_sens)
{
}


extern void
_XmTextDestinationVisible(Widget w, Boolean turn_on)
{
}


extern void
_XmTextChangeBlinkBehavior(XmTextWidget widget, Boolean newvalue)
{
}


extern void
_XmTextAdjustGC(XmTextWidget w)
{
}


extern Boolean
_XmTextShouldWordWrap(XmTextWidget w)
{
    OutputData o = Text_OutputData(w);

    if (! Out_WordWrap(o) )
    {
	return False;
    }
    if (Text_EditMode(w) == XmSINGLE_LINE_EDIT)
    {
	return False;
    }
    if ( Out_ScrollHorizontal(o) &&
	XtClass(XtParent(w)) == xmScrolledWindowWidgetClass)
    {
	return False;
    }
    if ( Out_ResizeWidth(o) )
    {
	return False;
    }

    return True;
}


extern Boolean
_XmTextScrollable(XmTextWidget w)
{
    OutputData o = Text_OutputData(w);

    return o->scrollvertical;
}


extern void
_XmTextOutputGetSecResData(XmSecondaryResourceData *secResDataRtn)
{
}


extern int
_XmTextGetNumberLines(XmTextWidget w)
{
    return w->text.total_lines - 1;
}


extern Boolean
_XmTextGetDisplayRect(Widget w, XRectangle *display_rect)
{
    return False;
}


extern void
_XmTextMarginsProc(Widget w, XmBaselineMargins *margins_rec)
{
}


extern void
_XmTextChangeHOffset(XmTextWidget widget, int length)
{
}


extern void
_XmTextToggleCursorGC(Widget widget)
{
}


extern void
_XmTextFreeContextData(Widget w, XtPointer clientData, XtPointer callData)
{
}


extern void
_XmTextResetClipOrigin(XmTextWidget tw, XmTextPosition position,
		       Boolean clip_mask_reset)
{
}


/*
 * Return the position of the end of this line, to implement wrapping.
 * FIX ME (currently quite simplistic)
 * FIX ME (should "extra" be filled up ?)
 */
XmTextPosition
_XmTextFindLineEnd(XmTextWidget w, XmTextPosition pos, LineTableExtra *extra)
{
    Dimension avail, width, oldwidth;
    XmTextPosition wordend, last, end;
    OutputData o = Text_OutputData(w);

    avail = XtWidth(w) - (Out_RightMargin(o) + Out_LeftMargin(o));

    /* What's our line ? */
    end = (*Text_Source(w)->Scan) (Text_Source(w), pos,
				   XmSELECT_LINE, XmsdRight, 1, False);

    /* Do we need to spend time on this; the line may be short enough */
    width = FontTextWidth(w, pos, end);
    if (width < avail)
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "_XmTextFindLineEnd(%d) => %d [avail %d width %d]\n",
			  pos, end, avail, width));

	return end;
    }

    /* Unfortunately, we need to investigate */
    wordend = pos;
    do
    {
	last = wordend;
	wordend = (*Text_Source(w)->Scan) (Text_Source(w), last + 1,
					   XmSELECT_WORD,
					   XmsdRight, 1, False);
	oldwidth = width;
	width = FontTextWidth(w, pos, wordend);
    }
    while (width <= avail && wordend < end);

    if (last == pos){
	int i;
	/* In this case, we need to clip at non_word_boundary. */
	width = 0;
	for(i=pos+1; i<end; i++){
	    oldwidth = width;
	    width = FontTextWidth(w, pos, i);
	    if (width >= avail){
		last = i - 1;
		break;
	    }
	    last = i;
	}
    }

    width = oldwidth;

    if (extra)
    {
	LineTableExtra e = (LineTableExtra)XtMalloc(sizeof(LineTableExtraRec));
	e->width = width;
	e->wrappedbychar = False;

	*extra = e;
    }

    /* Take the previous position */
    if (last < end)
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "_XmTextFindLineEnd(%d) => %d [avail %d width %d]\n",
			  pos, last, avail, width));

	return last;
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "_XmTextFindLineEnd(%d) => %d [avail %d width %d]\n",
			  pos, PASTENDPOS, avail, width));

	return PASTENDPOS;
    }
}


extern void
_XmTextMovingCursorPosition(XmTextWidget tw, XmTextPosition position)
{
	XmTextShowPosition((Widget)tw, position);
}


extern Boolean
_XmTextGetBaselines(Widget widget, Dimension **baselines, int *line_count)
{
    return False;
}


/* XTextWidth clone for TextFieldWidget. */
extern int
_XmOut_FontTextWidth(OutputData o, char *s, int l)
{
	if (Out_FontList(o)->renditions[0]->type == XmFONT_IS_FONT) {
		return XTextWidth(Out_Font(o), s, l);
#ifdef	USE_XFT
	} else if (Out_XftFont(o)) {
		XGlyphInfo	ext;
		XftTextExtents8(Out_FontList(o)->renditions[0]->dpy,
			Out_XftFont(o), (unsigned char*)s, l, &ext);

		return ext.width;
#endif
	} else {
		return XmbTextEscapement((XFontSet)Out_FontList(o)->renditions[0]->font, s, l);
	}

}


/* Calculate FontMaxWidth from fontList */
extern int
_XmOut_FontMaxWidth(OutputData o)
{
	if (Out_FontList(o)->renditions[0]->type == XmFONT_IS_FONT) {
		return Out_Font(o)->max_bounds.width;
	} else
#ifdef	USE_XFT
	if (Out_FontList(o)->renditions[0]->type == XmFONT_IS_XFT) {
		return Out_FontAverageWidth(o);	/* FIX ME HACK */
		return Out_XftFont(o)->max_advance_width;
	} else
#endif
	{
		XFontStruct **fsl;
		char **nl;
		int i, num, max;

		num = XFontsOfFontSet((XFontSet)Out_FontList(o)->renditions[0]->font, &fsl, &nl);
		max = 0;
		for(i=0; i<num; i++){
			if (fsl[i]->max_bounds.width  > max)
				max = fsl[i]->max_bounds.width;
		}
		return max;
	}
}

static void
_XmTextOutSetRenderTable(Widget w, int ofs, XrmValue *v)
{
	OutputData o = Text_OutputData(w);

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmTextOutSetRenderTable (%d)\n",
		o->check_set_render_table));

	++o->check_set_render_table;
	switch (o->check_set_render_table)
	{
	case 1:
		/*
		 * Either the font list or render table resource has
		 * not been set, but do not know yet if both have not
		 * been set.  For now, preserve the value in case one
		 * of the resources has been set.
		 */
		/* Fall through intentionally. */

	case 2:
		/*
		 * Neither the font list nor render table resource has
		 * been set.  Since the structure was initialized from
		 * outputRec, preserve the value that is there.
		 */
		v->addr = (char *)&(o->fontlist);
		break;

	default:
		/* This should never happen. */
		v->addr = NULL;
		break;
	}
}
