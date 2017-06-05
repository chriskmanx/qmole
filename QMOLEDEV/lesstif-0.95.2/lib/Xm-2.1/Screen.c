/**
 *
 * $Id: Screen.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2003 LessTif Development Team 
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

static const char rcsid[] = "$Id: Screen.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuUtilP.h>
#include <Xm/DisplayP.h>
#include <Xm/ScreenP.h>
#include <Xm/DragIconP.h>
#include <Xm/RepType.h>
#include <XmI/AtomMgrI.h>
#include <XmI/ImageCacheI.h>

#include <XmI/DebugUtil.h>


/* Some global variables */
XrmQuark _XmInvalidCursorIconQuark=0;
XrmQuark _XmValidCursorIconQuark=0;
XrmQuark _XmNoneCursorIconQuark=0;
XrmQuark _XmDefaultDragIconQuark=0;
XrmQuark _XmMoveCursorIconQuark=0;
XrmQuark _XmCopyCursorIconQuark=0;
XrmQuark _XmLinkCursorIconQuark=0;

/* Forward Declarations */

static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static void insert_child(Widget w);
static void delete_child(Widget w);
static void default_horizontal_font_unit(Widget w, int offset, XrmValue *val);
static void default_vertical_font_unit(Widget w, int offset, XrmValue *val);

/*
 * Resources for the primitive class
 */
#define Offset(field) XtOffsetOf(XmScreenRec, screen.field)
static XtResource resources[] =
{
    {
	XmNdarkThreshold, XmCDarkThreshold, XmRInt,
	sizeof(int), Offset(darkThreshold),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNlightThreshold, XmCLightThreshold, XmRInt,
	sizeof(int), Offset(lightThreshold),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNforegroundThreshold, XmCForegroundThreshold, XmRInt,
	sizeof(int), Offset(foregroundThreshold),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNdefaultNoneCursorIcon, XmCDefaultNoneCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultNoneCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultValidCursorIcon, XmCDefaultValidCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultValidCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultInvalidCursorIcon, XmCDefaultInvalidCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultInvalidCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultMoveCursorIcon, XmCDefaultMoveCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultMoveCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultLinkCursorIcon, XmCDefaultLinkCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultLinkCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultCopyCursorIcon, XmCDefaultCopyCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultCopyCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultSourceCursorIcon, XmCDefaultSourceCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultSourceCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNmenuCursor, XmCCursor, XmRCursor,
	sizeof(Cursor), Offset(menuCursor),
	XmRString, (XtPointer)"arrow"
    },
    {
	XmNunpostBehavior, XmCUnpostBehavior, XmRUnpostBehavior,
	sizeof(unsigned char), Offset(unpostBehavior),
	XmRImmediate, (XtPointer)XmUNPOST_AND_REPLAY
    },
    {
	XmNfont, XmCFont, XmRFontStruct,
	sizeof(XFontStruct *), Offset(font_struct),
	XmRString, (XtPointer)"Fixed"
    },
    {
	XmNhorizontalFontUnit, XmCHorizontalFontUnit, XmRInt,
	sizeof(int), Offset(h_unit),
	XmRCallProc, (XtPointer)default_horizontal_font_unit
    },
    {
	XmNverticalFontUnit, XmCVerticalFontUnit, XmRInt,
	sizeof(int), Offset(v_unit),
	XmRCallProc, (XtPointer)default_vertical_font_unit
    },
    {
	XmNmoveOpaque, XmCMoveOpaque, XmRBoolean,
	sizeof(Boolean), Offset(moveOpaque),
	XmRImmediate, (XtPointer)False
    },
};


static XmBaseClassExtRec _XmScreenCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL,
    /* set_values_prehook        */ NULL,
    /* initialize_posthook       */ NULL,
    /* set_values_posthook       */ NULL,
    /* secondary_object_class    */ NULL,
    /* secondary_object_create   */ NULL,
    /* get_secondary_resources   */ NULL,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ NULL,
    /* get_values_posthook       */ NULL,
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ False,
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

XmScreenClassRec xmScreenClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &coreClassRec,
        /* class_name            */ "XmScreen",
	/* widget_size           */ sizeof(XmScreenRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ False /*True*/,
	/* compress_exposure     */ XtExposeNoCompress /*XtExposeCompressMaximal*/,
	/* compress_enterleave   */ False /*True*/,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL /*XtInheritSetValuesAlmost*/,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmScreenCoreClassExtRec
    },
    /* Desktop Class part */
    {
        /* child_class           */ NULL,
        /* insert_child          */ insert_child,
        /* delete_child          */ delete_child,
        /* extension             */ NULL
    },
    /* Screen Class part */
    {
        /* extension             */ NULL
    }
};


WidgetClass xmScreenClass = (WidgetClass)&xmScreenClassRec;

/*
 * MLM - pretty much ripped off from Display.c
 * Following is all that stuff (variables) that is needed in order to put
 * the management of XmScreen widgets per screen to live.
 */
#define PSWC_None               ((XContext) 0)
static XContext PerScreenWidgetContext = PSWC_None;


static void
class_initialize(void)
{
    _XmScreenCoreClassExtRec.record_type = XmQmotif;

    _XmInvalidCursorIconQuark = XrmStringToQuark(XmNdefaultInvalidCursorIcon);
    _XmValidCursorIconQuark   = XrmStringToQuark(XmNdefaultValidCursorIcon);
    _XmNoneCursorIconQuark    = XrmStringToQuark(XmNdefaultNoneCursorIcon);
    _XmDefaultDragIconQuark   = XrmStringToQuark(XmNdefaultSourceCursorIcon);
    _XmMoveCursorIconQuark    = XrmStringToQuark(XmNdefaultMoveCursorIcon);
    _XmCopyCursorIconQuark    = XrmStringToQuark(XmNdefaultCopyCursorIcon);
    _XmLinkCursorIconQuark    = XrmStringToQuark(XmNdefaultLinkCursorIcon);
}


static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmSCREEN_BIT);
}


static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
	XtPointer FirstScreenWidget;

	DEBUGOUT(_LtDebug(__FILE__, new_w, "%s:initialize(%d)\n", 
		__FILE__, __LINE__));

	/*
	 * This is due to the %&"% children management mechanism needed for the
	 * shell "shadow hierarchy". See Desktop.c for more details on this.
	 */
	Screen_NumChildren(new_w) = 0;
	Screen_NumSlots(new_w) = 0;
	Screen_Children(new_w) = NULL;

	/*
	 * If haven't yet allocated the context with all kind of information
	 * about LessTif goodies we'll do it right now. This context contains
	 * LessTif goodies on a per display basis.
	 */
	if (PerScreenWidgetContext == PSWC_None)
	{
		PerScreenWidgetContext = XUniqueContext();
	}

	/*
	 * Make sure that there hasn't already allocated another XmScreen
	 * widget. Then register this widget as the XmScreen widget for the
	 * appropiate screen.
	 */
	if (XFindContext(XtDisplay(new_w), RootWindowOfScreen(XtScreen(new_w)),
		PerScreenWidgetContext, (XPointer *)&FirstScreenWidget)
		== XCSUCCESS) {
		_XmError(new_w, "Attempt to create a second XmScreen widget.");
	} else {
		XSaveContext(XtDisplay(new_w), RootWindowOfScreen(XtScreen(new_w)),
			PerScreenWidgetContext, (XPointer)new_w);
	}

	Screen_MwmPresent(new_w) = XmIsMotifWMRunning(new_w);

	Screen_ScratchPixmaps(new_w) = NULL;
	Screen_ScreenInfo(new_w) = NULL;
	Screen_CursorCache(new_w) = NULL;

	Screen_MoveCursorIcon(new_w) = NULL;
	Screen_CopyCursorIcon(new_w) = NULL;
	Screen_LinkCursorIcon(new_w) = NULL;
	Screen_StateCursorIcon(new_w) = NULL;
	Screen_StateCursorIcon(new_w) = NULL;
	Screen_SourceCursorIcon(new_w) = NULL;

	XQueryBestCursor(XtDisplay(new_w), RootWindowOfScreen(XtScreen(new_w)),
		32, 32,
		&Screen_MaxCursorWidth(new_w),
		&Screen_MaxCursorHeight(new_w));

	Screen_NullCursor(new_w) = None;

	_XmPickupUnspecifiedPixmaps(XtDisplay(new_w));
}


static void
destroy(Widget w)
{
    XmScratchPixmap	pix, tmp;
    XmScreen		sw;

    XtFree((char *)Screen_Children(w));
    for (pix = Screen_ScratchPixmaps(w); pix != NULL; pix = tmp)
    {
	tmp = pix->next;
	XFreePixmap(XtDisplay(w), pix->pixmap);
	XtFree((char *)pix);
    }

    if (XFindContext(XtDisplay(w), RootWindowOfScreen(XtScreen(w)),
		     PerScreenWidgetContext, (XPointer *)&sw) == XCSUCCESS
	&& sw == (XmScreen)w)
	XDeleteContext(XtDisplay(w), RootWindowOfScreen(XtScreen(w)),
		       PerScreenWidgetContext);

    _XmInvalidateColorCache(False);
    _LtImageCacheScreenDestroy(XtScreen(w));
}


static void
insert_child(Widget w)
{
    Widget MeTheParent;

    /*
     * The next access is really FINE. The child to be inserted must always
     * be of class xmDesktopClass.
     */
    MeTheParent = Desktop_Parent(w);

    /*
     * Make free room for the new child, if necessary.
     */
    if (Screen_NumChildren(MeTheParent) == Screen_NumSlots(MeTheParent))
    {
	Screen_NumSlots(MeTheParent) += Screen_NumSlots(MeTheParent) / 2 + 2;
	Screen_Children(MeTheParent) = (WidgetList)
	    XtRealloc((char *)Screen_Children(MeTheParent),
		      sizeof(Widget) * Screen_NumSlots(MeTheParent));
    }

    Screen_Children(MeTheParent)[Screen_NumChildren(MeTheParent)] = w;
    Screen_NumChildren(MeTheParent)++;

}


static void
delete_child(Widget w)
{
    Widget MeTheParent;
    WidgetList Children;
    int NumChildren, i;

    MeTheParent = Desktop_Parent(w);	/* We're working on a xmDesktop desc. */
    Children = Screen_Children(MeTheParent);
    NumChildren = Screen_NumChildren(MeTheParent);

    for (i = 0; i < NumChildren; i++)
    {
	if (*Children == w)
	{
	    for (i++; i < NumChildren; i++)
	    {
		Children[0] = Children[1];
		Children++;
	    }
	    Screen_NumChildren(MeTheParent)--;

	    break;
	}

	Children++;
    }
}


static void
realize(Widget w,
	XtValueMask *value_mask,
	XSetWindowAttributes *attributes)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:realize(%d)\n", 
    			__FILE__, __LINE__));

}


static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean need_refresh = False;

    if (Screen_UnpostBehavior(new_w) != Screen_UnpostBehavior(old)
	&& !XmRepTypeValidValue( XmRepTypeGetId(XmRUnpostBehavior),
				 Screen_UnpostBehavior(new_w),
				 new_w))
	Screen_UnpostBehavior(new_w) = Screen_UnpostBehavior(old);

    if ((Screen_DarkThreshold(old) != Screen_DarkThreshold(new_w) ||
	 Screen_LightThreshold(old) != Screen_LightThreshold(new_w) ||
	 Screen_ForegroundThreshold(old) != Screen_ForegroundThreshold(new_w)))
	_XmInvalidateColorCache(True);

    if (Screen_FontStruct(new_w) != Screen_FontStruct(old))
    {
	XrmValue value;

	if (Screen_HorizUnit(new_w) == Screen_HorizUnit(old))
	{
	    default_horizontal_font_unit(new_w, 0, &value);
	    Screen_HorizUnit(new_w) = *(int *)value.addr;
	}
	if (Screen_VertUnit(new_w) == Screen_VertUnit(old))
	{
	    default_vertical_font_unit(new_w, 0, &value);
	    Screen_VertUnit(new_w) = *(int *)value.addr;
	}
    }

    return need_refresh;
}


static void
default_horizontal_font_unit(Widget w, int offset, XrmValue *val)
{
    unsigned long prop;
    Atom atom;
    XFontStruct *font;
    static int h_unit;
    

    val->addr = (XPointer)&h_unit;
    val->size = sizeof h_unit;
    h_unit =
	/* No font specified: 10 */
	!(font = Screen_FontStruct(w))
	? 10
	/* AVERAGE_WIDTH / 10 (if it exists) */
	: ((atom = XmInternAtom(XtDisplay(w), _XA_AVERAGE_WIDTH, True))
	   && XGetFontProperty(font, atom, &prop))
	? (prop + 5) / 10
	/* QUAD_WIDTH (if it exists) */
	: XGetFontProperty(font, XA_QUAD_WIDTH, &prop)
	? prop
	/* (max + min) / 2.3 */
	: ((font->min_bounds.width + font->max_bounds.width) * 10 + 11) / 23;
}


static void
default_vertical_font_unit(Widget w, int offset, XrmValue *val)
{
    unsigned long prop, prop2;
    Atom atom;
    XFontStruct *font;
    static int v_unit;

    val->addr = (XPointer)&v_unit;
    val->size = sizeof v_unit;
    v_unit =
	/* No font specified: 10 */
	!(font = Screen_FontStruct(w))
	? 10
	/* PIXEL_SIZE / 1.8 (if it exists) */
	: ((atom = XmInternAtom(XtDisplay(w), _XA_PIXEL_SIZE, True))
	   && XGetFontProperty(font, atom, &prop))
	? (prop * 10 + 9) / 18
	/* POINT_SIZE * RESOLUTION_Y / 1400 (if they exist) */
	: (XGetFontProperty(font, XA_POINT_SIZE, &prop)
	   && (atom = XmInternAtom(XtDisplay(w), _XA_RESOLUTION_Y, True))
	   && XGetFontProperty(font, atom, &prop2))
	? (prop * prop2 + 700) / 1400
	/* (ascent + descent) / 2.2 */
	: ((font->ascent + font->descent) * 10 + 11) / 22;
}


extern XmDragIconObject
_XmScreenGetOperationIcon(Widget w, unsigned char operation)
{
    XmScreen scr;
    XrmQuark q;
    XmDragIconObject *d = NULL, *o = NULL;

    scr = (XmScreen)XmGetXmScreen(XtScreenOfObject(w));

    if (operation == XmDROP_MOVE)
    {
	q = _XmMoveCursorIconQuark;
	d = &Screen_DefaultMoveCursorIcon(scr);
	o = &Screen_MoveCursorIcon(scr);
    }
    else if (operation == XmDROP_COPY)
    {
	q = _XmCopyCursorIconQuark;
	d = &Screen_DefaultCopyCursorIcon(scr);
	o = &Screen_CopyCursorIcon(scr);
    }
    else if (operation == XmDROP_LINK)
    {
	q = _XmLinkCursorIconQuark;
	d = &Screen_DefaultLinkCursorIcon(scr);
	o = &Screen_LinkCursorIcon(scr);
    }
    else
    {
	return NULL;
    }

    if (*d)
    {
	return *d;
    }

    if (*o)
    {
	*d = *o;
	return *o;
    }

    *o = (XmDragIconObject)XmCreateDragIcon((Widget)scr,
					    XrmQuarkToString(q),
					    NULL, 0);
    *d = *o;

    return *d;
}


extern XmDragIconObject
_XmScreenGetStateIcon(Widget w, unsigned char state)
{
    XmScreen scr;
    XrmQuark q;
    XmDragIconObject *d = NULL;

    scr = (XmScreen)XmGetXmScreen(XtScreenOfObject(w));

    if (state == XmNO_DROP_SITE)
    {
	q = _XmNoneCursorIconQuark;
	d = &Screen_DefaultNoneCursorIcon(scr);
    }
    else if (state == XmINVALID_DROP_SITE)
    {
	q = _XmValidCursorIconQuark;
	d = &Screen_DefaultInvalidCursorIcon(scr);
    }
    else if (state == XmVALID_DROP_SITE)
    {
	q = _XmInvalidCursorIconQuark;
	d = &Screen_DefaultValidCursorIcon(scr);
    }
    else
    {
	q = _XmNoneCursorIconQuark;
	d = &Screen_DefaultNoneCursorIcon(scr);
    }

    if (*d)
    {
	return *d;
    }

    if (!Screen_StateCursorIcon(scr))
    {
	Screen_StateCursorIcon(scr) =
	    (XmDragIconObject)XmCreateDragIcon((Widget)scr,
					       XrmQuarkToString(q),
					       NULL, 0);
    }

    if (!Screen_DefaultNoneCursorIcon(scr))
    {
	Screen_DefaultNoneCursorIcon(scr) = Screen_StateCursorIcon(scr);
    }

    if (!Screen_DefaultInvalidCursorIcon(scr))
    {
	Screen_DefaultInvalidCursorIcon(scr) = Screen_StateCursorIcon(scr);
    }

    if (!Screen_DefaultValidCursorIcon(scr))
    {
	Screen_DefaultValidCursorIcon(scr) = Screen_StateCursorIcon(scr);
    }

    return Screen_StateCursorIcon(scr);
}


extern XmDragIconObject
_XmScreenGetSourceIcon(Widget w)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(XtScreenOfObject(w));

    if (Screen_DefaultSourceCursorIcon(scr) != NULL)
    {
	return Screen_DefaultSourceCursorIcon(scr);
    }

    if (Screen_SourceCursorIcon(scr) != NULL)
    {
	Screen_DefaultSourceCursorIcon(scr) = Screen_SourceCursorIcon(scr);
    }
    else
    {
	Screen_SourceCursorIcon(scr) =
	    (XmDragIconObject)XmCreateDragIcon((Widget)scr,
				     XrmQuarkToString(_XmDefaultDragIconQuark),
					       NULL, 0);

	Screen_DefaultSourceCursorIcon(scr) = Screen_SourceCursorIcon(scr);
    }

    return Screen_DefaultSourceCursorIcon(scr);
}


extern Pixmap
_XmAllocScratchPixmap(XmScreen xmScreen,
		      Cardinal depth,
		      Dimension width,
		      Dimension height)
{
    XmScratchPixmap pix = NULL;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)xmScreen, "%s:_XmAllocScratchPixmap(%d) - %dx%dx%d\n", 
    			__FILE__, __LINE__,
    			width, height, depth));

    for (pix = Screen_ScratchPixmaps(xmScreen); pix != NULL; pix = pix->next)
    {
	if (!pix->inUse && pix->depth == depth &&
	    pix->width == width && pix->height == height)
	{
	    pix->inUse = True;
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)xmScreen, "\tfrom cache %p\n", 
    			pix->pixmap));
	    break;
	}
    }

    if (pix == NULL)
    {
	pix = (XmScratchPixmap)XtMalloc(sizeof(XmScratchPixmapRec));
	pix->inUse = True;
	pix->depth = depth;
	pix->width = width;
	pix->height = height;
	pix->pixmap = XCreatePixmap(XtDisplay((Widget)xmScreen),
				    RootWindowOfScreen(XtScreen(xmScreen)),
				    width, height, depth);
	pix->next = Screen_ScratchPixmaps(xmScreen);
	Screen_ScratchPixmaps(xmScreen) = pix;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)xmScreen, "\tnew %p\n", 
			    pix->pixmap));
    }
    return pix->pixmap;
}


extern void
_XmFreeScratchPixmap(XmScreen xmScreen, Pixmap pixmap)
{
    XmScratchPixmap pix;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)xmScreen, "%s:_XmFreeScratchPixmap(%d) - %p\n", 
    			__FILE__, __LINE__,
    			pixmap));
    for (pix = Screen_ScratchPixmaps(xmScreen); pix != NULL; pix = pix->next)
    {
	if (pix->pixmap == pixmap)
	{
	    pix->inUse = False;
	    break;
	}
    }
    if (pix == NULL)
    {
    	_XmWarning((Widget)xmScreen, "%s:_XmFreeScratchPixmap(%d) - Trying to free pixmap %p\n    that is not in the cache",
    			__FILE__, __LINE__, pixmap);
    }
}


extern XmDragCursorCache *
_XmGetDragCursorCachePtr(XmScreen xmScreen)
{
    return &Screen_CursorCache(xmScreen);
}


extern void
_XmGetMaxCursorSize(Widget w,
		    Dimension *width,
		    Dimension *height)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(XtScreen(w));
    *width = Screen_MaxCursorWidth(scr);
    *height = Screen_MaxCursorHeight(scr);
}


extern Cursor
_XmGetNullCursor(Widget w)
{
    XmScreen scr;
    Pixmap pix;
    static char nullpix[4] =
    {0, 0, 0, 0};
    XColor fg, bg;

    scr = (XmScreen)XmGetXmScreen(XtScreen(w));

    if (Screen_NullCursor(scr))
    {
	return Screen_NullCursor(scr);
    }

    pix = XCreatePixmapFromBitmapData(XtDisplayOfObject(w),
				      RootWindowOfScreen(XtScreenOfObject(w)),
				      nullpix, 4, 4,
				      0, 0, 1);

    fg.pixel = bg.pixel = 0;

    Screen_NullCursor(scr) = XCreatePixmapCursor(XtDisplayOfObject(w),
						 pix, pix, &fg, &bg, 0, 0);

    XFreePixmap(XtDisplayOfObject(w), pix);

    return Screen_NullCursor(scr);
}


extern Cursor
_XmGetMenuCursorByScreen(Screen *screen)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(screen);
    return Screen_MenuCursor(scr);
}


extern Boolean
_XmGetMoveOpaqueByScreen(Screen *screen)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(screen);
    return Screen_MoveOpaque(scr);
}


extern unsigned char
_XmGetUnpostBehavior(Widget wid)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(XtScreen(wid));
    return Screen_UnpostBehavior(scr);
}


extern int
_XmGetFontUnit(Screen *screen, int dimension)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(screen);

    if (dimension == XmVERTICAL)
    {
	return Screen_VertUnit(scr);
    }

    return Screen_HorizUnit(scr);
}


extern void
_XmScreenRemoveFromCursorCache(XmDragIconObject icon)
{
    XmScreen scr;
    XmDragCursorCache *ptr, tmp;
    static XmDragIconRec nullIcon =
    {
	{0},{0}, {0}};
    Boolean match;

    scr = (XmScreen)XmGetXmScreen(XtScreen(icon));

    ptr = &Screen_CursorCache(scr);
    while (ptr && *ptr)
    {
	match = False;

	if ((*ptr)->sourceIcon == icon)
	{
	    match = True;
	    (*ptr)->sourceIcon = &nullIcon;
	}

	if ((*ptr)->stateIcon == icon)
	{
	    match = True;
	    (*ptr)->stateIcon = &nullIcon;
	}

	if ((*ptr)->opIcon == icon)
	{
	    match = True;
	    (*ptr)->stateIcon = &nullIcon;
	}

	if (match && (*ptr)->cursor != None)
	{
	    XFreeCursor(DisplayOfScreen(XtScreen(icon)), (*ptr)->cursor);
	    (*ptr)->cursor = None;
	}

	if ((*ptr)->sourceIcon == &nullIcon || (*ptr)->stateIcon == &nullIcon ||
	    (*ptr)->opIcon == &nullIcon)
	{

	    if ((*ptr)->cursor == None)
	    {
		tmp = *ptr;
		*ptr = (*ptr)->next;
		XtFree((char *)tmp);
	    }
	}

	ptr = &((*ptr)->next);
    }
}


extern XmScreenInfo *
_XmGetScreenInfo(Widget scr)
{
    if (!Screen_ScreenInfo(scr))
    {
	XmScreenInfo *info = (XmScreenInfo *)XtMalloc(sizeof(XmScreenInfo));
	XmMenuState state = (XmMenuState)XtCalloc(1, sizeof(XmMenuStateRec));

	state->MU_InDragMode = False;
	state->MU_InPMMode = False;
	state->RC_menuFocus.oldWidget = NULL;
	state->RC_menuFocus.oldFocus = None;
	state->RC_menuFocus.oldRevert = RevertToNone;

	info->menu_state = (XtPointer)state;
	info->destroyCallbackAdded = False;

	Screen_ScreenInfo(scr) = (XtPointer)info;
    }

    return (XmScreenInfo *)Screen_ScreenInfo(scr);
}


extern Widget
XmGetXmScreen(Screen *scr)
{
    Widget disp;
    Widget screen;
    int argc;
    Arg args[4];
    char name[128];
    int screen_no, max_screens;

    DEBUGOUT(_LtDebug0(__FILE__, (Widget)NULL, "%s:XmGetXmScreen(%d)\n", 
    			__FILE__, __LINE__));

    disp = XmGetXmDisplay(DisplayOfScreen(scr));

    if ((PerScreenWidgetContext == PSWC_None) ||
	(XFindContext(DisplayOfScreen(scr),
		      RootWindowOfScreen(scr),
		      PerScreenWidgetContext,
		      (XPointer *)&screen) != XCSUCCESS))
    {
	argc = 0;
	XtSetArg(args[argc], XmNwidth, 1); argc++;
	XtSetArg(args[argc], XmNheight, 1); argc++;
	XtSetArg(args[argc], XmNmappedWhenManaged, False); argc++;
	XtSetArg(args[argc], XmNscreen, scr); argc++;

	max_screens = ScreenCount(DisplayOfScreen(scr));
	for (screen_no = 0; screen_no < max_screens; screen_no++)
	{
	    if (ScreenOfDisplay(DisplayOfScreen(scr), screen_no) ==	scr)
	    {
		break;
	    }
	}
	sprintf(name, "screen%d", screen_no);

	screen = XtCreateWidget(name, xmScreenClass, disp, args, argc);
    }

    return screen;
}
