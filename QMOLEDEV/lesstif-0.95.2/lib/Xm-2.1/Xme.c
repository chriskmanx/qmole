/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Xme.c,v 1.2 2005/03/19 16:10:06 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Xme.c,v 1.2 2005/03/19 16:10:06 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include <Xm/XmP.h>
#include <Xm/DialogS.h>
#include <Xm/DragIconP.h>
#include <Xm/XmosP.h>
#include <Xm/VirtKeysP.h>
#include <Xm/VendorSEP.h>
#include <Xm/ColorObjP.h>
#include <Xm/TransferP.h>

#include <XmI/XmI.h>
#include <XmI/AtomMgrI.h>
#include <XmI/MessagesI.h>

#include <Xm/SpecRenderT.h>

#include <XmI/DebugUtil.h>


/* required prototypes of exported, but private Intrinsic stuff: */
extern void _XtAddCallback(XtCallbackList *, XtCallbackProc, XtPointer);
extern void _XtRemoveCallback(XtCallbackList *, XtCallbackProc, XtPointer);


/* DragIcon */
extern Widget
XmeGetTextualDragIcon(Widget w)
{
    return _XmGetTextualDragIcon(w);
}


/* GeoUtils */
extern XtGeometryResult
XmeReplyToQueryGeometry(Widget w,
			XtWidgetGeometry *request,
			XtWidgetGeometry *reply)
{
    return _XmGMReplyToQueryGeometry(w, request, reply);
}


/* GadgetUtil */
extern void
XmeConfigureObject(Widget g,
		   Position x, Position y,
		   Dimension width, Dimension height, Dimension border_width)
{
    _XmConfigureObject(g, x, y, width, height, border_width);
}


/* GadgetUtil */
extern void
XmeRedisplayGadgets(Widget w, XEvent *event, Region region)
{
    _XmRedisplayGadgets(w, event, region);
}


/* ImageCache */
extern Boolean
XmeGetPixmapData(Screen *screen, Pixmap pixmap,
		 char **image_name,
		 int *depth, Pixel *foreground, Pixel *background,
		 int *hot_x, int *hot_y,
		 unsigned int *width, unsigned int *height)
{
    return _XmGetPixmapData(screen, pixmap,
			    image_name,
			    depth, foreground, background,
			    hot_x, hot_y, width, height);
}


extern Boolean
XmeNamesAreEqual(char *in_str, char *text_str)
{
    return _XmStringsAreEqual(in_str, text_str);
}


/* ResInd */
extern void
XmeFromHorizontalPixels(Widget widget,
			int offset,
			XtArgVal *value)
{
    _XmFromHorizontalPixels(widget, offset, value);
}


/* ResInd */
extern void
XmeFromVerticalPixels(Widget widget,
		      int offset,
		      XtArgVal *value)
{
    _XmFromVerticalPixels(widget, offset, value);
}


/* ResInd */
extern XmImportOperator
XmeToHorizontalPixels(Widget widget,
		      int offset,
		      XtArgVal *value)
{
    return _XmToHorizontalPixels(widget, offset, value);
}


/* ResInd */
extern XmImportOperator
XmeToVerticalPixels(Widget widget,
		    int offset,
		    XtArgVal *value)
{
    return _XmToVerticalPixels(widget, offset, value);
}


/* Screen */
extern Cursor
XmeGetNullCursor(Widget w)
{
    return _XmGetNullCursor(w);
}


/* Traversal */
extern Boolean
XmeFocusIsInShell(Widget wid)
{
    return _XmFocusIsInShell(wid);
}


/* Traversal */
extern void
XmeNavigChangeManaged(Widget w)
{
    _XmNavigChangeManaged(w);
}


/* Vendor */
/* FIXME: does this still exist??
   amai: no, not in Motif 2.1 */
#if XmVersion == 2000
extern Display *
XmeGetDefaultDisplay(void)
{
    return _XmGetDefaultDisplay();
}
#endif


extern void 
XmeAddFocusChangeCallback(Widget w, XtCallbackProc cp, XtPointer udata)
{
    XmBaseClassExt *bce;
    XtPointer nsec;
    int size;

    if(!XmIsVendorShell(w))
    {
        XmeWarning(w, _XmMsgProtocols_0000);
        return;
    }

    bce = _XmGetBaseClassExtPtr(XtClass(w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;
    nsec = XtCalloc(1, size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = w;
    ((XmExtRec *)nsec)->object.xrm_name = w->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = w;
    ExtObj_ExtensionType(nsec) = XmSHELL_EXTENSION;

    _XtAddCallback(&VSEP_FocusMovedCallback(nsec),
		   cp, udata);
    XtFree((char *)nsec);
}


extern void
XmeRemoveFocusChangeCallback(Widget w, XtCallbackProc cp, XtPointer udata)
{
    XmBaseClassExt *bce;
    XtPointer nsec;
    int size;

    if(!XmIsVendorShell(w))
    {
        XmeWarning(w, _XmMsgProtocols_0000);
        return;
    }

    bce = _XmGetBaseClassExtPtr(XtClass(w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;
    nsec = XtCalloc(1, size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = w;
    ((XmExtRec *)nsec)->object.xrm_name = w->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = w;
    ExtObj_ExtensionType(nsec) = XmSHELL_EXTENSION;

    _XtRemoveCallback(&VSEP_FocusMovedCallback(nsec),
                      cp, udata);
    XtFree((char *)nsec);
}


/* Visual */
extern void
XmeGetDefaultPixel(Widget widget,
		   int type,
		   int offset,
		   XrmValue *value)
{
    static Pixel pix;

    if (type == XmBACKGROUND)
    {
	_XmBackgroundColorDefault(widget, offset, value);
	return;
    }
    value->addr = (XPointer)&pix;
    value->size = sizeof pix;
    pix = _XmAccessColorData(_XmGetColors(XtScreenOfObject(widget),
					  ColormapOfObject(widget),
					  XmIsGadget(widget) ?
					  XmParentBackground(widget) :
					  XtBackground(widget)),
					  type);
}


/* VirtKeys */
extern void
XmeVirtualToActualKeysym(Display *Dsp,
			 KeySym VirtualKeysym,
			 KeySym *RealKeysymReturn,
			 Modifiers *ModifierReturn)
{
    _XmVirtualToActualKeysym(Dsp, VirtualKeysym, RealKeysymReturn,
			     ModifierReturn);
}


/* XmString */
extern void
XmeStringUpdateWMShellTitle(XmString xmstr, Widget shell)
{
   /* This is not specified by Motif 2.1 ... */
    _XmStringUpdateWMShellTitle(xmstr, shell);
}


extern void
XmeSetWMShellTitle(XmString xmstr, Widget shell)
{
    /* Apparently the same thing as the above. */
    _XmStringUpdateWMShellTitle(xmstr, shell);
}


/* Xmos */
extern int
XmeMicroSleep(long secs)
{
    return _XmMicroSleep(secs);
}

static XmFontList
XmeGetDefaultRenderTableFromFixedFont(Widget w)
{
	XmRenderTable	rt;
	XmRendition	r;
	Arg		al[4];
	int		ac = 0;
	XFontStruct	*fs;

	fs = XLoadQueryFont(XtDisplay(w), "fixed");
	if (! fs)
		return NULL;
	XtSetArg(al[ac], XmNfont, fs); ac++;
	/* Create a default render table from the default system font */
	r = XmRenditionCreate(w, XtDefaultFont, al, ac);
	rt = XmRenderTableAddRenditions(NULL, &r, 1, XmMERGE_REPLACE);
	return rt;
}

XmFontList
XmeGetDefaultRenderTable(Widget w, unsigned int rendertableType)
{
	Widget			p;	/* parent */
	XmSpecRenderTrait	t;	/* trait */

	if (! w)
		return XmeGetDefaultRenderTableFromFixedFont(w);

	for (p=XtParent(w); p; p = XtParent(p))
		if ((t = XmeTraitGet(p, XmQTspecifyRenderTable)))
			break;

	if (! t)
		return XmeGetDefaultRenderTableFromFixedFont(w);

	return t->getRenderTable(p, rendertableType);
}


/* Transfer */
extern Atom
*XmeStandardTargets(Widget w,
                    int count,
                    int *count_return)
{
   _XmWarning(NULL, "XmeStandardTargets(): not yet implemented!");
   
   *count_return=0;
   return (Atom *)NULL;
}


/* Transfer */
extern void 
XmeStandardConvert(Widget w,
                   XtPointer ignore,
                   XmConvertCallbackStruct *call_data)
{
   Atom target_type = call_data->target;
#if 0
   switch (target_type) {
      case BACKGROUND: 
         break;
      case CLASS: 
         break;
      case CLIENT_WINDOW: 
         break;
      case COLORMAP: 
         break;
      case FOREGROUND: 
         break;
      case NAME: 
         break;
      case TARGETS: 
         break;
      case _MOTIF_RENDER_TABLE: 
         break;
      case _MOTIF_ENCODING_REGISTRY:
         break;
      default:
         break;
   }
#endif
   _XmWarning(NULL, "XmeStandardConvert(): not yet implemented!");
}


extern void
XmeQueryBestCursorSize(Widget widget, Dimension *width, Dimension *height) 
{

   /* amai: is "max"=="best"? */
   _XmGetMaxCursorSize(widget, width, height);
}


extern void
XmeResolvePartOffsets(WidgetClass widget_class,
                           XmOffsetPtr *offset,
                           XmOffsetPtr *constraint_offset)
{
   _XmWarning(NULL, "XmeResolvePartOffsets(): not yet implemented!");
}


extern void 
XmeConvertMerge(XtPointer data, Atom type,
                int format, unsigned long length,
                XmConvertCallbackStruct *call_data)
{
   _XmWarning(NULL, "XmeConvertMerge(): not yet implemented!");
}


extern Boolean
XmeGetDesktopColorCells(Screen *screen,
                        Colormap colormap,
                        XColor *colors,
                        int n_colors,
                        int *n_colors_ret)
{
   _XmWarning(NULL, "XmeGetDesktopColorCells(): not yet implemented!");
   return False;
}


extern Boolean
XmeGetColorObjData(int *screen,
                   int *coloruse,
                   XmPixelSet *pixel_set,
                   unsigned short pixel_set_size,
                   short *active,
                   short *inactive,
                   short *primary,
                   short *secondary,
                   short *text)
{
   _XmWarning(NULL, "XmeGetColorObjData(): not yet implemented!");
   return False;
}


extern Widget
XmeCreateClassDialog(WidgetClass widget_class,
                     Widget parent,
                     String name,
                     ArgList args,
                     Cardinal arg_count)
{
   Widget dsw, cw;
   String name_popup;
   Arg al[10];
   int ac;

   
   name_popup=(String)XtMalloc(strlen(name)+7);
   strcpy(name_popup, name);
   strcat(name_popup, "_popup");
   dsw=XmCreateDialogShell(parent, name_popup, args, arg_count);
   XtFree(name_popup);

   ac=0;
   XtSetArg(al[ac], XmNallowShellResize, True); ac++;
   XtSetValues(dsw, al, ac);
   
   cw= XtCreateWidget(name, widget_class, dsw,
                      args, arg_count);

   XtAddCallback(cw, XmNdestroyCallback,
                 _XmDestroyParentCallback, (XtPointer)cw);
		  
   return cw;
}
