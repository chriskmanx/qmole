/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/FontList.c,v 1.10 2006/04/19 18:42:22 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 2000, 2001, 2002, 2004 LessTif Development Team
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

static const char rcsid[] = "$Id: FontList.c,v 1.10 2006/04/19 18:42:22 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <XmI/DebugUtil.h>

#ifdef	USE_XFT
#include <X11/Xft/Xft.h>
#endif

#undef	DEBUG_POINTERS

/**************************** INTERNAL FUNCTIONS **************************/
XmFontListEntry _XmFontListEntryCreate(void)
{
	XmFontListEntry	r;

	r = (XmFontListEntry)XtMalloc(sizeof(struct __XmRenditionRec));
	memset(r, 0, sizeof(struct __XmRenditionRec));

	r->count = 1;		/* This is a usage count, initialise to 1. */

	r->type = XmAS_IS;
	r->rendition_foreground = XmAS_IS;
	r->rendition_background = XmAS_IS;
	r->load_model = XmAS_IS;
	r->strike_thru_type = XmAS_IS;
	r->tab_list = (XmTabList)XmAS_IS;
	r->underline_type = XmAS_IS;

#ifdef DEBUG_POINTERS
	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmFontListEntryCreate %p\n", r));
#endif
	return r;
}

XmFontList __XmFontListAlloc(int numberOfEntries)
{
	XmFontList newFontList;

	/* Allocate one more, this entry should have tag NULL */
	newFontList = (XmFontList)XtMalloc(sizeof(struct __XmRenderTableRec));

	newFontList->count = numberOfEntries;		/* #entries to be provided for */

	newFontList->renditions = (struct __XmRenditionRec **)XtCalloc(
		numberOfEntries + 1,
		sizeof(struct __XmRenditionRec *));
	newFontList->dpy = NULL;

	/* This is the one with the NULL tag. */
	newFontList->renditions[numberOfEntries] = _XmFontListEntryCreate();
	DEBUGOUT(_LtDebug(__FILE__, NULL, "__XmFontListAlloc(%d) -> %p\n",
		numberOfEntries, newFontList));
	return newFontList;
}


static int __XmFontListNumEntries(XmFontList flist)
{
	return flist->count;
}


static void
__XmFontListDealloc(XmFontList list)
{
	int	i;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "__XmFontListDealloc(%p), %d entries\n",
				list, list->count));

	if (list) {
		if (list->renditions) {
			for (i=0; i<list->count; i++)
				if (list->renditions[i]) {
					_XmFontListEntryFree(list->renditions[i]);
				}
			_XmFontListEntryFree(list->renditions[list->count]);
			XtFree((char *)list->renditions);
		}
		XtFree((char *)list);
	}
}

/************************* LOW LEVEL FUNCTIONS **************************/
extern XmFontList
_XmFontListCreateDefault(Display *d)
{
	struct __XmRenditionRec	*defaultEntry;
	XFontStruct		*fs = XLoadQueryFont(d, XmDEFAULT_FONT);
	XmFontList		r;

	defaultEntry = _XmFontListEntryCreate();

	defaultEntry->tag = XtNewString(XmFONTLIST_DEFAULT_TAG);
	defaultEntry->type = XmFONT_IS_FONT;
	defaultEntry->font = (XtPointer)fs;
#if (XmVERSION > 1)
	defaultEntry->rendition_background = XmAS_IS;
	defaultEntry->rendition_foreground = XmAS_IS;
	defaultEntry->font_name = NULL;
	defaultEntry->load_model = 0;
	defaultEntry->strike_thru_type = 0;
	defaultEntry->underline_type = 0;
	defaultEntry->tab_list = NULL;
	defaultEntry->dpy = d;
#endif
#ifdef	USE_XFT
	/* FIX ME */
	defaultEntry->xft_font = NULL;
	defaultEntry->font_average_width = 0;
	defaultEntry->font_average_height = 0;
	memset(&defaultEntry->xft_foreground, 0, sizeof(XftColor));
	memset(&defaultEntry->xft_background, 0, sizeof(XftColor));
	defaultEntry->pattern = NULL;
	defaultEntry->font_style = NULL;
	defaultEntry->font_foundry = NULL;
	defaultEntry->font_encoding = NULL;
	defaultEntry->font_size = 0;
	defaultEntry->pixel_size = 0;
	defaultEntry->font_slant = 0;
	defaultEntry->font_spacing = 0;
	defaultEntry->font_weight = 0;
#endif

	r = XmFontListAppendEntry(NULL, defaultEntry);
#ifdef DEBUG_POINTERS
	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmFontListCreateDefault() -> %p\n", r));
#endif
	return r;
}


/************************** PUBLIC FUNCTIONS ***********************/
extern XmFontList
XmFontListAppendEntry(XmFontList old, XmFontListEntry entry)
{
	XmFontList newFontList;
	int i;

	if (entry == NULL) {
		/* We can't just return "old" here, because of memory allocation
		 * rules: old can be freed by the caller; while the result of
		 * XmFontListAppendEntry should be newly allocated (and hence
		 * should be freed by the caller as well).
	         *
	     	 *    return XmFontListCopy(old);
		 *
	         * amai: well thought, but a more literal interpretation of the
		 *       man page indeed indicates that 'old' is returned literally and
		 *       a quick test on DU/ Motif 1.2.4 confirms that!
		 */
#ifdef DEBUG_POINTERS
		DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListAppendEntry(%p, NULL) -> %p\n",
			old, old));
#endif
		return old;
	}

	if (old == NULL) {
		newFontList = __XmFontListAlloc(1);
		i = 0;
	} else {
		newFontList = __XmFontListAlloc(__XmFontListNumEntries(old) + 1);

		for (i = 0; i < old->count; i++) {
			/* Copy the pointer */
			newFontList->renditions[i] = old->renditions[i];
			old->renditions[i]->count++;
			DEBUGOUT(_LtDebug(__FILE__, NULL,
				"XmFontListAppendEntry(%p,%p) -> rendition %p %d count ++ %d\n",
				old, entry, old->renditions[i], i, old->renditions[i]->count));
		}

#if 1
		/* HUH DANNY HACK DANNY FIX ME FIXME */
		__XmFontListDealloc(old);
#endif
	}

	/* Additionally allocate the structure here !! */
	newFontList->renditions[i] = _XmFontListEntryCreate();

	newFontList->renditions[i]->tag = XtNewString(entry->tag);
	newFontList->renditions[i]->type = entry->type;
	newFontList->renditions[i]->font = entry->font;
#if (XmVERSION > 1)
	newFontList->renditions[i]->rendition_background = entry->rendition_background;
	newFontList->renditions[i]->rendition_foreground = entry->rendition_foreground;
	newFontList->renditions[i]->font_name = XtNewString(entry->font_name);
	newFontList->renditions[i]->load_model = entry->load_model;
	newFontList->renditions[i]->strike_thru_type = entry->strike_thru_type;
	newFontList->renditions[i]->underline_type = entry->underline_type;
	newFontList->renditions[i]->tab_list = XmTabListCopy(entry->tab_list, 0, 0);
	newFontList->renditions[i]->dpy = entry->dpy;

	newFontList->dpy = entry->dpy;

#ifdef	USE_XFT
	newFontList->renditions[i]->xft_font = entry->xft_font;
	newFontList->renditions[i]->font_average_width = entry->font_average_width;
	newFontList->renditions[i]->font_average_height = entry->font_average_height;
	newFontList->renditions[i]->xft_foreground = entry->xft_foreground;
	newFontList->renditions[i]->xft_background = entry->xft_background;
	newFontList->renditions[i]->pattern = entry->pattern;
	newFontList->renditions[i]->font_style = XtNewString(entry->font_style);
	newFontList->renditions[i]->font_foundry = XtNewString(entry->font_foundry);
	newFontList->renditions[i]->font_encoding = XtNewString(entry->font_encoding);
	newFontList->renditions[i]->font_size = entry->font_size;
	newFontList->renditions[i]->pixel_size = entry->pixel_size;
	newFontList->renditions[i]->font_slant = entry->font_slant;
	newFontList->renditions[i]->font_spacing = entry->font_spacing;
	newFontList->renditions[i]->font_weight = entry->font_weight;
#endif	/* USE_XFT */

#endif	/* XmVERSION > 1 */

#ifdef DEBUG_POINTERS
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListAppendEntry(%p, %p) -> %p\n",
		old, entry, newFontList));
#endif
	return newFontList;
}


extern XmFontList
XmFontListCreate(XFontStruct *font, XmStringCharSet charset)
{
    /* this function is obsolete 1.2 and above and should not be used.
       Use XmFontListAppendEntry instead! */

	XmFontList		r;
	XmFontListEntry		entry;
#if (XmVERSION > 1)
#ifdef	LESSTIF_VERBOSE
	static Boolean warn=False;

	if (!warn) {
		warn=True;
		_XmWarning(NULL, "XmFontListCreate() is an obsolete function!\n");
	}
#endif
#endif
	entry = _XmFontListEntryCreate();

	entry->tag = XtNewString(charset);
	entry->type = XmFONT_IS_FONT;
	entry->font = (XtPointer)font;

	r = XmFontListAppendEntry(NULL, entry);

	return r;
}

#ifdef	USE_XFT
extern XmFontList
XmFontListCreateXft(XftFont *font, XmStringCharSet charset)
{
    /* this function is obsolete 1.2 and above and should not be used.
       Use XmFontListAppendEntry instead! */

	XmFontList		r;
	XmFontListEntry		entry;
#if (XmVERSION > 1)
#ifdef	LESSTIF_VERBOSE
	static Boolean warn=False;

	if (!warn) {
		warn=True;
		_XmWarning(NULL, "XmFontListCreate() is an obsolete function!\n");
	}
#endif
#endif
	entry = _XmFontListEntryCreate();

	entry->tag = XtNewString(charset);
	entry->type = XmFONT_IS_XFT;
	entry->xft_font = (XtPointer)font;
	entry->font = NULL;

	r = XmFontListAppendEntry(NULL, entry);

	return r;
}
#endif

/*
 * this function is "obsolete" since 1.2 ; however Xmt uses it.
 */
extern XmFontList
XmFontListAdd(XmFontList old, XFontStruct *font, XmStringCharSet charset)
{
	XmFontList			newFontList;
	XmFontListEntry		entry;
#ifdef LESSTIF_VERBOSE
#if (XmVERSION > 1)
	static Boolean		warn=False;

	if (!warn) {
		warn=True;
		_XmWarning(NULL, "XmFontListAdd() is an obsolete function!\n");
	}
#endif
#endif

	if (!old)
		return (XmFontList)NULL;
	if (!font || !charset)
		return old;

	entry = _XmFontListEntryCreate();
	entry->tag = XtNewString(charset);
	entry->type = XmFONT_IS_FONT;
	entry->font = (XtPointer)font;

	newFontList = XmFontListAppendEntry(old, entry);
	/* amai: de-allocation of 'old' is done by XmFontListAppendEntry() */

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListAdd(%p, %p, _) -> %p\n",
		old, font, newFontList));

	return newFontList;
}


extern XmFontList XmFontListCopy(XmFontList fontlist)
{
	XmFontList newFontList;
	int i;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListCopy(dpy %p, fl %p)\n",
				fontlist->dpy, fontlist));
#ifdef DEBUG_POINTERS
	if (fontlist->count > 10) {
		DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListCopy(%p) count %d !!\n",
					fontlist, fontlist->count));
	} else {
		DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListCopy(%p)\n", fontlist));
	}
#endif
	if (!fontlist) {
		return (XmFontList)NULL;
	}
	newFontList = __XmFontListAlloc(__XmFontListNumEntries(fontlist));

	for (i = 0; i < fontlist->count; i++) {
		/* Additionally allocate the structure here !! */
		newFontList->renditions[i] = fontlist->renditions[i];
		fontlist->renditions[i]->count++;
		DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListCopy(%p) entry %p %d count ++ %d\n",
					fontlist, i, fontlist->renditions[i],
					fontlist->renditions[i]->count));
	}
#ifdef DEBUG_POINTERS
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListCopy(%p) -> %p\n",
		fontlist, newFontList));
#endif
	newFontList->dpy = fontlist->dpy;
	return newFontList;
}

extern XmFontListEntry
XmFontListEntryCreate(char *tag, XmFontType type, XtPointer font)
{
	XmFontListEntry entry = _XmFontListEntryCreate();

	entry->tag  = XtNewString(tag);
	entry->type = type;
	entry->font = font;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListEntryCreate() -> %p\n", entry));

	return entry;
}

/*
 * Free the memory associated with a font list entry, but do not close the font.
 */
extern void
XmFontListEntryFree(XmFontListEntry *pentry)
{
	XmFontListEntry	e;

	if (pentry == NULL || *pentry == NULL) {
#ifdef DEBUG_POINTERS
		DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListEntryFree(NULL)\n"));
#endif
		return;
	}

	e = *pentry;
	e->count--;		/* Decrement the usage counter */

	if (e->count > 0) {
#ifdef DEBUG_POINTERS
		DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListEntryFree(%p) use_count %d\n",
			e, ((pentry && *pentry) ? (*pentry)->count : -99)));
#endif
		return;
	}
#ifdef DEBUG_POINTERS
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListEntryFree(%p) free !\n", e));
#endif

#ifdef	USE_XFT
	if (e->dpy) {
		if (e->xft_font) {
			XftFontClose(e->dpy, e->xft_font);
			e->xft_font = NULL;
			e->font_average_width = 0;
			e->font_average_height = 0;
		}
		if (e->pattern) {
			FcPatternDestroy(e->pattern);
			e->pattern = NULL;
		}
	}
#endif

	XtFree(e->tag);
	XtFree(e->font_name);
	XtFree((char *)e);
}

void
_XmFontListEntryFree(XmFontListEntry e)
{
	XmFontListEntryFree(&e);
}

extern XtPointer
XmFontListEntryGetFont(XmFontListEntry entry,
		       XmFontType *type_return)
{
    if (entry == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListEntryGetFont(NULL)\n"));
	if (type_return)
	    *type_return = XmFONT_IS_FONT;
	return (XtPointer)NULL;
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListEntryGetFont()\n"));

    if (type_return)
    {
	*type_return = entry->type;
    }

    return (XtPointer)entry->font;
}


extern char *
XmFontListEntryGetTag(XmFontListEntry entry)
{
    return XtNewString(entry->tag);
}

extern XmFontListEntry
XmFontListEntryLoad(Display *display, char *font_name, XmFontType type, char *tag)
{
	XmFontListEntry	entry;
	XrmValue	fromString, toFont, cvtArg[2];
	Boolean		success = False;
	char		*p, *locale;
	XtCacheRef	cacherefs[2];

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListEntryLoad(%s, tag %s)\n",
		font_name, tag));

	/* Strip trailing semi-colon */
	for (p = font_name; *p; p++) {
	}

	while (p > font_name && *(p - 1) == ',') {
		p--;
		*p = '\0';
	}

	if (*font_name == '"' && *(p-1) == '"') {
		font_name++;
		p--;
		*p = '\0';
	}

	fromString.addr = font_name;
	fromString.size = strlen(font_name) + 1;

	cvtArg[0].addr = (XPointer)&display;
	cvtArg[0].size = sizeof(Display *);

	entry = (XmFontListEntry)XtMalloc(sizeof(struct __XmRenditionRec));
	memset(entry, 0, sizeof(struct __XmRenditionRec));
	entry->font = NULL; /* for the XtCallConverter() call */
	entry->count = 1;
	entry->dpy = display;
	entry->rendition_foreground = XmAS_IS;
	entry->rendition_background = XmAS_IS;
#ifdef	USE_XFT
	/* Nothing to do, already cleared by memset() */
#endif

	switch (type) {
	case XmFONT_IS_FONT:
		toFont.addr = (XPointer)&(entry->font);
		toFont.size = sizeof(XFontStruct *);

#if 0
		fprintf(stderr, "XtCallConverter(XtCvtStringToFontStruct)\n");
#endif
		success = XtCallConverter(display, XtCvtStringToFontStruct, cvtArg,
			(Cardinal)1, &fromString, &toFont, cacherefs);
		break;

	case XmFONT_IS_FONTSET:
		toFont.addr = (XPointer)&(entry->font);
		toFont.size = sizeof(XFontSet);

		/* XtCvtStringToFontSet needs 2 arguments : display and locale */
		locale = setlocale(LC_CTYPE, NULL);
		setlocale(LC_CTYPE, locale);
		cvtArg[1].addr = (XPointer)XtNewString(locale);
		cvtArg[1].size = sizeof(char *);

#if 0
		fprintf(stderr, "XtCallConverter(XtCvtStringToFontSet)\n");
#endif
		success = XtCallConverter(display, XtCvtStringToFontSet, cvtArg,
			(Cardinal)2, &fromString, &toFont, cacherefs);
		XtFree(cvtArg[1].addr);
		break;
	case XmFONT_IS_XFT:
#ifdef	USE_XFT
#endif
		break;
	case XmFONT_IS_XOC:
#ifdef	USE_BIDI
#endif
		break;
	}

	if (!success || !entry->font) {
		DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListEntryLoad => NULL\n"));
		XtFree((char *)entry);
		return (XmFontListEntry)NULL;
	} else {
		DEBUGOUT(_LtDebug(__FILE__, NULL,
			"XmFontListEntryLoad success!  fle %p fs %p fid %p\n",
			entry, entry->font, ((XFontStruct *)(entry->font))->fid));
		entry->tag = XtNewString(tag);
		entry->type = type;
		entry->font_name = XtNewString(font_name);

		return entry;
	}
}

extern void
XmFontListFree(XmFontList list)
{
    if (!list || list == (XmFontList)XmUNSPECIFIED)
    {
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmFontListFree(0x%x)\n", list));

    __XmFontListDealloc(list);
}

extern void
XmFontListFreeFontContext(XmFontContext context)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "XmFontListFreeFontContext(0x%x)\n", context));
    XtFree((char *)context);
}


extern Boolean
XmFontListInitFontContext(XmFontContext *context,
			  XmFontList fontlist)
{
    if (fontlist && context)
    {
	*context = (XmFontContext)XtMalloc(sizeof(struct _XmFontListContextRec));
	(*context)->fontlist = fontlist;
	(*context)->current_entry = -1;

	return True;
    }
    else
    {
	return False;
    }
}


extern XmFontListEntry XmFontListNextEntry(XmFontContext context)
{
	context->current_entry++;

	if (context->current_entry < __XmFontListNumEntries(context->fontlist)) {
		return (context->fontlist->
				renditions[context->current_entry]);
	} else {
		return NULL;
	}
}


extern Boolean
XmFontListGetNextFont(XmFontContext context, XmStringCharSet *charset, XFontStruct **font)
{
    context->current_entry++;

#ifdef	USE_XFT_THIS_IS_BAD
    if (context->current_entry < __XmFontListNumEntries(context->fontlist)) {
	if (context->fontlist->renditions[context->current_entry]->type == XmFONT_IS_XFT) {
		*font = (XFontStruct *)(context->fontlist->renditions
			[context->current_entry]->xft_font);
		*charset = XtNewString(context->fontlist->renditions
			[context->current_entry]->tag);
		return True;
	}
    }
    return False;
#else
    if (context->current_entry < __XmFontListNumEntries(context->fontlist))
    {
	if (context->fontlist->renditions[context->current_entry]->type == XmFONT_IS_FONT)
	{
	    *font = (XFontStruct *)(context->fontlist->renditions
				    [context->current_entry]->font);
	    *charset = XtNewString(context->fontlist->renditions
				   [context->current_entry]->tag);
	}
	else
	{
	    XFontStruct **foo;
	    char **bar;

	    if (XFontsOfFontSet((XFontSet)(context->fontlist->renditions
					   [context->current_entry]->font),
				&foo,
				&bar) < 1)
	    {
		*font = NULL;
	    }
	    else
	    {
		*font = foo[0];
	    }

	    *charset = XtNewString(context->fontlist->renditions
				   [context->current_entry]->tag);
	}
	return True;
    }
    else
    {
	return False;
    }
#endif
}


extern XmFontList
XmFontListRemoveEntry(XmFontList oldlist,
		      XmFontListEntry entry)
{
    XmFontList newFontList;
    int i, j;
    int oldnr, newnr;

    if (!oldlist)
       return (XmFontList)NULL;
    if (!entry)
       return oldlist;

    /* entry match >= 0 entries in oldlist, right?! */
    oldnr = __XmFontListNumEntries(oldlist);
    newFontList = __XmFontListAlloc(oldnr);
    j = 0;
    for (i = 0; oldlist->renditions[i]->tag != NULL; i++)
    {
	if (!(!strcmp(entry->tag, oldlist->renditions[i]->tag) &&
	      entry->type == oldlist->renditions[i]->type &&
	      entry->font == oldlist->renditions[i]->font))
	{
	    newFontList->renditions[j]->tag  = XtNewString(oldlist->renditions[i]->tag);
	    newFontList->renditions[j]->type = oldlist->renditions[i]->type;
	    newFontList->renditions[j]->font = oldlist->renditions[i]->font;

	    j++;
	}
    }
    newnr = j;

    /* Motif manpage tells to check here */
    if (oldnr==newnr)
    {
       __XmFontListDealloc(newFontList);
       newFontList = 0;
       return oldlist;
     }
    else
    {
       __XmFontListDealloc(oldlist);
       oldlist = 0;
       return newFontList;
     }
}

#ifdef	USE_XFT
/*
 * FIX ME this should be cached.
 */
static struct _XmXftDrawCacheStruct {
	Display	*dpy;
	Window	w;
	XftDraw	*d;
} *_XmXftDrawCache = NULL;
static int _XmXftDrawCacheSize = 0;

XftDraw *
_XmXftDrawCreate(Display *dpy, Window w)
{
	XWindowAttributes	wa;
	XftDraw			*d;
	int			i;

	for (i=0; i<_XmXftDrawCacheSize; i++) {
		if (_XmXftDrawCache[i].dpy == dpy
				&& _XmXftDrawCache[i].w == w) {
			return _XmXftDrawCache[i].d;
		}
	}
	(void)XGetWindowAttributes(dpy, w, &wa);
	d = XftDrawCreate(dpy, w, DefaultVisual(dpy, 0), wa.colormap);

	/* Store it in the cache. Look for an empty slot first */
	for (i=0; i<_XmXftDrawCacheSize; i++)
		if (_XmXftDrawCache[i].dpy == NULL) {
			_XmXftDrawCache[i].dpy = dpy;
			_XmXftDrawCache[i].d = d;
			_XmXftDrawCache[i].w = w;
			return d;
		}
	i = _XmXftDrawCacheSize;	/* Next free index */
	_XmXftDrawCacheSize = _XmXftDrawCacheSize * 2 + 8;
	_XmXftDrawCache = (struct _XmXftDrawCacheStruct *)
		XtRealloc((char *)_XmXftDrawCache,
		sizeof(struct _XmXftDrawCacheStruct) * _XmXftDrawCacheSize);

	_XmXftDrawCache[i].dpy = dpy;
	_XmXftDrawCache[i].d = d;
	_XmXftDrawCache[i].w = w;
	
	return d;
}

void
_XmXftDrawDestroy(Display *dpy, Window w, XftDraw *d)
{
	int	i;

	for (i=0; i<_XmXftDrawCacheSize; i++)
		if (_XmXftDrawCache[i].dpy == dpy
				&& _XmXftDrawCache[i].w == w) {
			_XmXftDrawCache[i].dpy = NULL;
			_XmXftDrawCache[i].d = NULL;
			_XmXftDrawCache[i].w = 0;
			XftDrawDestroy(d);
			return;
		}
	_XmWarning(NULL, "_XmXftDrawDestroy() this should not happen\n");
}

void
_XmXftDrawString(Display *dpy, Window wd, XmRendition r, int bpc, Position x, Position y, char *s, int len)
{
	XGlyphInfo	info;
	XftDraw	*d = _XmXftDrawCreate(dpy, wd);

	switch (bpc) {
	case 2:
		XftDrawString16(d, &r->xft_foreground, r->xft_font,
			x, y, (XftChar16 *)s, len);
		break;
	case 4:
		XftDrawString32(d, &r->xft_foreground, r->xft_font,
			x, y, (XftChar32 *)s, len);
		break;
	default:
		_XmWarning(NULL, "_XmXftDrawString(unsupported bpc %d)\n", bpc);
	case 1:
#if	0	/* Experiment */
	{
		XftTextExtents8(dpy, r->xft_font, s, len, &info);
		XClearArea(dpy, wd, x, y, info.width, info.height, False);
		DEBUGOUT(_LtDebug(__FILE__, NULL, "Clear %d %d %d %d\n",
			x, y, info.width, info.height));
	}
#endif
		/* Experiment */
		XftTextExtents8(dpy, r->xft_font, (unsigned char*)s, len, &info);
		XftDrawRect(d, &r->xft_background, x, y - info.height, info.width, info.height);
		/* End experiment */

		XftDrawString8(d, &r->xft_foreground, r->xft_font,
			x, y, (unsigned char*)s, len);
		break;
	}
}

void
_XmXftFontAverageWidth(Widget w, XtPointer f, int *wid, int *ht)
{
	XftFont		*fp = (XftFont *)f;
	static char	*s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
	int		l = 62 /* strlen(s) */;
	XGlyphInfo	ext;

	XftTextExtents8(XtDisplay(w), fp, (unsigned char*)s, l, &ext);
        if (wid)
		*wid = ext.width / l;
	if (ht)
		*ht = ext.height;
}

void
_XmXftSetClipRectangles(Widget w, Position x, Position y, XRectangle *rects, int n)
{
	XftDraw	*d = _XmXftDrawCreate(XtDisplay(w), XtWindow(w));

	XftDrawSetClipRectangles(d, x, y, rects, n);
	DEBUGOUT(_LtDebug(__FILE__, w, "XftSetClip [%d,%d dim %d,%d]\n",
		x, y, rects[0].width, rects[0].height));
}
#endif
