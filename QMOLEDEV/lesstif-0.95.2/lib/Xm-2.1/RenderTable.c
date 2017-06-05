/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/RenderTable.c,v 1.6 2005/06/24 11:36:03 dannybackx Exp $
 *
 * Copyright (C) 1998 Free Software Foundation, Inc.
 * Copyright © 1998, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/RenderTable.c,v 1.6 2005/06/24 11:36:03 dannybackx Exp $";

#include <LTconfig.h>

#include <string.h>

#include <Xm/XmP.h>
#include <XmI/XmI.h>

#include <XmI/DebugUtil.h>

#ifdef	USE_XFT
#include <X11/Xft/Xft.h>
#endif

static XmRendition _XmRenditionCopy(XmRendition s)
{
	s->count++;
#if 0
	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmRenditionCopy(%p) rc %d\n",
		s, s->count));
#endif
	return s;
}

static void _XmRenditionFree(XmRendition r)
{
	_XmFontListEntryFree(r);
}

extern XmRenderTable
XmRenderTableAddRenditions(XmRenderTable oldtable,
                           XmRendition  *renditions,
                           Cardinal rendition_count,
                           XmMergeMode merge_mode)
{
	/* FIX ME simplistic */
	int		count, i, off;
	XmRenderTable	r;

	off = oldtable ? oldtable->count : 0;
	count = off + rendition_count;
	r = __XmFontListAlloc(count);

	if (oldtable)
		for (i=0; i<oldtable->count; i++)
			r->renditions[i] = _XmRenditionCopy(oldtable->renditions[i]);
	for (i=0; i<rendition_count; i++)
		r->renditions[i+off] = _XmRenditionCopy(renditions[i]);

	r->dpy = r->renditions[0]->dpy;
	return r;
}

XmRenderTable _XmRenderTablePushRendition(XmRenderTable old, XmRendition rend)
{
	int		count, i, off;
	XmRenderTable	r;

	off = old ? old->count : 0;
	count = off + 1;
	r = __XmFontListAlloc(count);

	r->renditions[0] = _XmRenditionCopy(rend);
	for (i=0; i<off; i++)
		r->renditions[i+1] = _XmRenditionCopy(old->renditions[i]);

	r->dpy = r->renditions[0]->dpy;
	r->count = count;
	return r;
}

XmRenderTable _XmRenderTablePopRendition(XmRenderTable old, XmRendition rend)
{
	XmRenderTable	r;
	int		i;

	if (!old)
		return NULL;
	r = __XmFontListAlloc(old->count - 1);
	for (i=1; i<old->count; i++)
		r->renditions[i-1] = _XmRenditionCopy(old->renditions[i]);
	r->dpy = old->dpy;
	r->count = old->count - 1;
	return r;
}

/*
 * Be sure to maintain the same allocation as XmFontList
 *
 * NULL tag means copy all renditions.
 */
extern XmRenderTable
XmRenderTableCopy(XmRenderTable table, XmStringTag *tags, int tag_count)
{
	int		i, j, k, count;
	XmRenderTable	r;

	if (table == NULL)
		return NULL;

	if (!tags) {
		/* Allocate & copy */
		r = __XmFontListAlloc(table->count);
		for (i=0; i < table->count; i++)
			r->renditions[i] = _XmRenditionCopy(table->renditions[i]);
		r->count = table->count;
		r->dpy = table->dpy;
		return r;
	}
	for (i=0, count=0; i<table->count; i++) {
		for (j=0; j<tag_count; j++)
			if (strcmp(table->renditions[i]->tag, tags[j]) == 0) {
				count++;
				j = tag_count;	/* end loop */
			}
	}

	/* Allocate & copy */
	r = __XmFontListAlloc(count);

	for (i=0, j=0; i < table->count; i++)
		for (k=0; k<tag_count; k++) {
			if (strcmp(table->renditions[i]->tag, tags[k]) == 0) {
				r->renditions[j] = _XmRenditionCopy(table->renditions[i]);
				j++;
				/* force end of internal loop */
				k = tag_count;
			}
		}
	r->count = count;
	r->dpy = table->dpy;
	return r;
}


extern XmRenderTable
XmRenderTableCvtFromProp(Widget widget,
                         char *property,
                         unsigned int length)
{
	return NULL;
}


extern unsigned int
XmRenderTableCvtToProp(Widget widget,
                       XmRenderTable table,
                       char **prop_return)
{
	return 0;
}


extern void
XmRenderTableFree(XmRenderTable table)
{
	XmFontListFree(table);
}

extern XmRendition
XmRenderTableGetRendition(XmRenderTable table, XmStringTag tag)
{
	int	i;

	for (i=0; i<table->count; i++) {
		if (tag == table->renditions[i]->tag)
			return _XmRenditionCopy(table->renditions[i]);
		else if (tag && table->renditions[i]->tag &&
				strcmp(table->renditions[i]->tag, tag) == 0)
			return _XmRenditionCopy(table->renditions[i]);
	}
	return NULL;
}

#ifdef	USE_XFT
extern XmRendition
XmeRenderTableGetXftRendition(XmRenderTable table)
{
	if (table == NULL || table->count == 0)
		return NULL;
	return _XmRenditionCopy(table->renditions[0]);
}
#endif

extern XmRendition *
XmRenderTableGetRenditions(XmRenderTable table,
                           XmStringTag *tags,
                           Cardinal tag_count)
{
	return NULL;
}


extern int XmRenderTableGetTags(XmRenderTable table, XmStringTag **tag_list)
{
	return 0;
}

/*
 * This function deallocates the original render table and the matching
 * renditions after extracting the required information.
 */
extern XmRenderTable
XmRenderTableRemoveRenditions(XmRenderTable table, XmStringTag *tags, int tag_count)
{
	int		i, j, k, count, match;
	XmRenderTable	r;

	count = table->count;
	if (tags) {
		for (i=0; i<table->count; i++) {
			for (j=0; j<tag_count; j++)
				if (strcmp(table->renditions[i]->tag, tags[j]) == 0) {
					count--;
					j = tag_count;	/* end loop */
				}
		}
	}

	/* Allocate & copy */
	r = __XmFontListAlloc(count);

	for (i=0, j=0; i < table->count; i++) {
		for (match=0, k=0; k<tag_count && match == 0; k++)
			if (strcmp(table->renditions[i]->tag, tags[k]) == 0)
				match = 1;
		if (match)
			continue;

		r->renditions[j] = _XmRenditionCopy(table->renditions[i]);
		j++;
		/* force end of internal loop */
		k = tag_count;
	}
	r->count = count;

	return r;
}

extern Boolean
XmeRenderTableGetDefaultFont(XmRenderTable renderTable,
			     XFontStruct **fontStruct)
{
	return False;
}

/*
 * Note that this uses the same structure as a XmFontListEntry.
 */
extern XmRendition 
XmRenditionCreate(Widget widget, XmStringTag tag, ArgList al, Cardinal ac)
{
	int		i;
	XmRendition	r;

	r = _XmFontListEntryCreate();
	r->tag = XtNewString(tag);
	r->dpy = XtDisplay(widget);

	/* Assign defaults */
	r->font = (XFontSet)XmAS_IS;
	r->font_name = NULL;	/* instead of XmAS_IS */
	r->type = XmAS_IS;
	r->rendition_foreground = XmAS_IS;
	r->rendition_background = XmAS_IS;
	r->load_model = XmAS_IS;
	r->strike_thru_type = XmAS_IS;
	r->tab_list = (XmTabList)XmAS_IS;
	r->underline_type = XmAS_IS;

#ifdef	USE_XFT
	r->xft_font = NULL;
	r->font_average_width = 0;
	memset(&r->xft_foreground, 0, sizeof(XftColor));
	memset(&r->xft_background, 0, sizeof(XftColor));
	r->pattern = NULL;
	r->font_style = NULL;
	r->font_foundry = NULL;
	r->font_encoding = NULL;
	r->font_size = 0;
	r->pixel_size = 0;
	r->font_slant = 0;
	r->font_spacing = 0;
	r->font_weight = 0;
#endif

	
#ifdef	DEBUG_POINTERS
	DEBUGOUT(_LtDebug(__FILE__, widget, "XmRenditionCreate() -> %p\n", r));
#endif
	for (i=0; i<ac; i++) {
		if (strcmp(al[i].name, XmNrenditionBackground) == 0) {
			r->rendition_background = al[i].value;
		}
		if (strcmp(al[i].name, XmNrenditionForeground) == 0) {
			r->rendition_foreground = al[i].value;
		}
		if (strcmp(al[i].name, XmNfontName) == 0) {
			XtFree(r->font_name);
			r->font_name = XtNewString((char *)al[i].value);
		}
		if (strcmp(al[i].name, XmNfont) == 0) {
			r->font = (XFontSet)al[i].value;
		}
		if (strcmp(al[i].name, XmNfontType) == 0) {
			r->type = (XmFontType)al[i].value;
		}
		if (strcmp(al[i].name, XmNloadModel) == 0) {
			r->load_model = al[i].value;
		}
		if (strcmp(al[i].name, XmNstrikethruType) == 0) {
			r->strike_thru_type = al[i].value;
		}
		if (strcmp(al[i].name, XmNtabList) == 0) {
			r->tab_list = XmTabListCopy((XmTabList)al[i].value, 0, 0);
		}
		if (strcmp(al[i].name, XmNunderlineType) == 0) {
			r->underline_type = al[i].value;
		}
#ifdef	USE_XFT
		/* Xft specific resources ?? FIX ME */
#endif
	}

	return r;
}


extern void XmRenditionFree(XmRendition rendition)
{
	/* XXX FIX ME XXX: Is this right? */
	_XmRenditionFree(rendition);
}

#ifndef	XtRXftColor
#define	XtRXftColor	"XftColor"
#endif

extern void
XmRenditionUpdate(XmRendition r, ArgList al, Cardinal ad)
{
	int		i;
	Boolean		ok;
	Widget		w = NULL;	/* Crash !! */
	XrmValue	to, from;

	for (i=0; i<ad; i++) {
#ifdef	USE_XFT
		if (strcmp(al[i].name, XmNrenditionForeground) == 0) {
			/* XftColor */
			from.addr = &al[i].value;
			from.size = sizeof(XtPointer);
			to.addr = &r->xft_foreground;
			to.size = sizeof(XftColor);

			ok = XtConvertAndStore(w,
				XmRString, &from,
				XtRXftColor, &to);
		} else
		if (strcmp(al[i].name, XmNrenditionBackground) == 0) {
			/* XftColor */
			from.addr = &al[i].value;
			from.size = sizeof(XtPointer);
			to.addr = &r->xft_background;
			to.size = sizeof(XftColor);
			ok = XtConvertAndStore(w,
				XmRString, &from,
				XtRXftColor, &to);
		} else
#endif
		{
			;	/* Nothing */
		}
	}
#if 0
	_XmWarning(NULL, "XmRenditionUpdate(): not yet implemented!");
#endif
}


extern void XmRenditionRetrieve(XmRendition r, ArgList al, Cardinal ac)
{
	int		i;
#if 0
	_XmWarning(NULL, "XmRenditionRetrieve(): not yet implemented!");
#endif

	for (i=0; i<ac; i++) {
		if (strcmp(al[i].name, XmNrenditionBackground) == 0) {
			*(Pixel *)al[i].value = r->rendition_background;
		} else if (strcmp(al[i].name, XmNrenditionForeground) == 0) {
			*(Pixel *)al[i].value = r->rendition_foreground;
		} else if (strcmp(al[i].name, XmNfontName) == 0) {
			*(String *)al[i].value = XtNewString(r->font_name);
		} else if (strcmp(al[i].name, XmNfont) == 0) {
			*(XFontSet *)al[i].value = r->font;
		} else if (strcmp(al[i].name, XmNfontType) == 0) {
			*(XmFontType *)al[i].value = r->type;
		} else if (strcmp(al[i].name, XmNloadModel) == 0) {
			*(unsigned char *)al[i].value = r->load_model;
		} else if (strcmp(al[i].name, XmNstrikethruType) == 0) {
			*(unsigned char *)al[i].value = r->strike_thru_type;
		} else if (strcmp(al[i].name, XmNtabList) == 0) {
			*(XmTabList *)al[i].value = XmTabListCopy(r->tab_list, 0, 0);
		} else if (strcmp(al[i].name, XmNunderlineType) == 0) {
			*(unsigned char *)al[i].value = r->underline_type;
		}
#ifdef	USE_XFT
		/* Xft specific resources ?? FIX ME */
#endif
	}
}

/* If there are unspecified values in the primary rendition,
 * then the widget must create an "effective" rendition for that
 * segment. This is formed by using the previous (active) rendition
 * to fill in the unspecified values of the primary rendition.
 * ..
 * Finally if the resulting rendition still has resources with
 * unspecified values and the segment has a locale or charset tag
 * (these are optional and mutually exclusive) this tag is matched
 * with a rendition in the render table, and the missing rendition
 * values are filled in from that entry.
 *
 * If no matching rendition is found for a particular tag, then the
 * XmNoRenditionCallback of the XmDisplay object is called and the
 * render table is searched again for that tag.
 * If the resulting rendition does not specify a font or fontset,
 * then  ...
 */
void _XmRenderTableFinaliseTag(Widget w, XmRenderTable r, char *tag)
{
	int		i;
	int		found = 0;
	XmFontListEntry	e;
	char		*fn;

	/* FIX ME */
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmRenderTableFinaliseTag(%s)\n", tag));
#if 1
	/* Experimental start */
	if (r->dpy == 0)
		r->dpy = XtDisplay(w);
	/* Experimental end */
#endif
	for (i=0; i<r->count; i++) {
		if (strcmp(tag, r->renditions[i]->tag) == 0) {
#if	USE_XFT
			if (r->renditions[i]->type == XmAS_IS) {
				XftResult	res;
				XftPattern	*p, *p2;

				p = FcPatternCreate();
/* Don't even specify anything	XftPatternAddString(p, XFT_FAMILY, "fixed"); */
				p2 = XftFontMatch(XtDisplay(w), 0, p, &res);
				r->renditions[i]->xft_font =
					XftFontOpenPattern(XtDisplay(w), p2);
				_XmXftFontAverageWidth(w,
					(XtPointer)r->renditions[i]->xft_font,
					&r->renditions[i]->font_average_width,
					&r->renditions[i]->font_average_height);
				r->renditions[i]->type = XmFONT_IS_XFT;
				r->renditions[i]->font = NULL;			/* HACK */

				DEBUGOUT(_LtDebug(__FILE__, w,
					"_XmRenderTableFinaliseTag(%s): AS IS\n", tag));
			}
#endif
			if (r->renditions[i]->font == 0
				|| r->renditions[i]->type == XmAS_IS
				|| r->renditions[i]->font == (XFontSet)XmAS_IS)
			{
				found = 1;

				if (r->renditions[i]->font_name
				&& r->renditions[i]->font_name != (char *)XmAS_IS)
					fn = r->renditions[i]->font_name;
				else
					fn = XmDEFAULT_FONT;

				e = XmFontListEntryLoad(r->dpy,
					fn,
					XmFONT_IS_FONT,
					tag);
				if (e == NULL || e->font == NULL) {
					if (e)
						XmFontListEntryFree(&e);
					continue;
				}
				r->renditions[i]->font = e->font;
				XmFontListEntryFree(&e);
				return;
			}
		}
	}

	if (r->renditions[0]->font == NULL ||
			r->renditions[0]->font == (XFontSet)XmAS_IS) {
		XmFontListEntry e = XmFontListEntryLoad(r->dpy,
			XmDEFAULT_FONT, XmFONT_IS_FONT, XmFONTLIST_DEFAULT_TAG);
		r->renditions[0]->font = e->font;
		XmFontListEntryFree(&e);
	}
}

/*
 * This version takes the internal _XmString representation
 */
void __XmRenderTableFinalise(Widget w, XmRenderTable r, _XmString xms)
{
	int			i;

	DEBUGOUT(_LtDebug(__FILE__, w, "__XmRenderTableFinalise(rt %p xms %p)\n", r, xms));

	if (xms == NULL) {
		_XmRenderTableFinaliseTag(w, r, XmFONTLIST_DEFAULT_TAG);
		return;
	}

	for (i=0; i<xms->number_of_components; i++) {
		/* Run through the string, make sure all tags "work" */
		switch (xms->components[i]->type) {
		case XmSTRING_COMPONENT_RENDITION_BEGIN:
		case XmSTRING_COMPONENT_CHARSET:
			_XmRenderTableFinaliseTag(w, r, xms->components[i]->data);
			break;
		case XmSTRING_COMPONENT_LOCALE_TEXT:
			_XmRenderTableFinaliseTag(w, r, xms->components[i]->data);
			break;
		default:
               ;       /* Nothing to do */
		}
	}
}

/*
 * This one takes external XmString representation
 */
void _XmRenderTableFinalise(Widget w, XmRenderTable r, XmString xms)
{
	XmStringContext		ctx;
	XmStringComponentType	t;
	char			*tag = NULL, *txt = NULL;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmRenderTableFinalise(rt %p xms %p)\n", r, xms));

	if (xms == NULL) {
		_XmRenderTableFinaliseTag(w, r, XmFONTLIST_DEFAULT_TAG);
		return;
	}

	/* Run through the string, make sure all tags "work" */
	if (! XmStringInitContext(&ctx, xms)) {
		_XmRenderTableFinaliseTag(w, r, XmFONTLIST_DEFAULT_TAG);
		return;
	}

	/* FIX ME XmStringGetNextComponent() is obsolete */
	while ((t = XmStringGetNextComponent(ctx, &txt,
			&tag, NULL, NULL, NULL, NULL)) != XmSTRING_COMPONENT_END) {
		switch (t) {
		case XmSTRING_COMPONENT_RENDITION_BEGIN:
		case XmSTRING_COMPONENT_CHARSET:
			_XmRenderTableFinaliseTag(w, r, tag);
		default:
               ;       /* Nothing to do */
		}
		XtFree(tag);
		XtFree(txt);
		tag = NULL;
		txt = NULL;
	}
#if 0
	if (r->renditions[0]->font == NULL ||
			r->renditions[0]->font == (XFontSet)XmAS_IS) {
		XmFontListEntry	e = XmFontListEntryLoad(r->dpy,
			XmDEFAULT_FONT, XmFONT_IS_FONT, XmFONTLIST_DEFAULT_TAG);
		r->renditions[0]->font = e->font;
		XmFontListEntryFree(&e);
	}
#endif
}

Boolean
XmeUseXftFont(XmRenderTable r)
{
#ifdef	USE_XFT
	if (r == NULL || r->count == 0)
		return False;
	if (r->renditions[0]->type == XmFONT_IS_XFT)
		return True;
	return False;
#else
	return False;
#endif
}

XtPointer
XmeXftFont(XmRendition r)
{
#ifdef	USE_XFT
	if (r == NULL)
		return NULL;
	if (r->type == XmFONT_IS_XFT)
		return r->xft_font;
	return NULL;
#else
	return NULL;
#endif
}
