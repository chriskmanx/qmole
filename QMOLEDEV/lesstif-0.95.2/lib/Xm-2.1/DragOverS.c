/**
 *
 * $Id: DragOverS.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
 *
 * derived from Xt Vendor class.c 
 *
 * Copyright (C) 1995-2001 LessTif Development Team 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright 1989 Massachusetts Institute of Technology
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

static const char rcsid[] = "$Id: DragOverS.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/XmosP.h>
#include <Xm/VendorSP.h>
#include <Xm/VendorSEP.h>
#include <Xm/DragOverSP.h>
#include <Xm/DropSMgrP.h>
#include <Xm/DragCP.h>
#include <Xm/ScreenP.h>

#include <XmI/DebugUtil.h>

/* static void class_initialize(); */

static void class_part_initialize(WidgetClass wclass);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static Boolean set_values(Widget old, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static void expose(Widget w, XEvent *event, Region region);

static void destroy(Widget w);

static void set_mode(Widget w, int mode);

static XtResource resources[] =
{
    {
	XmNoverrideRedirect, XmCOverrideRedirect, XmRBoolean,
      sizeof(Boolean), XtOffsetOf(XmDragOverShellRec, shell.override_redirect),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhotX, XmCHot, XmRPosition,
	sizeof(Position), XtOffsetOf(XmDragOverShellRec, drag.hotX),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNhotY, XmCHot, XmRPosition,
	sizeof(Position), XtOffsetOf(XmDragOverShellRec, drag.hotY),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNdragOverMode, XmCDragOverMode, XmRUnsignedChar,
	sizeof(unsigned char), XtOffsetOf(XmDragOverShellRec, drag.mode),
	XmRImmediate, (XtPointer)XmCURSOR
    }
};

/* *INDENT-OFF* */
#if 0
static XmBaseClassExtRec _XmDragOverSCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ XmInheritInitializePrehook,
    /* set_values_prehook        */ XmInheritSetValuesPrehook,
    /* initialize_posthook       */ XmInheritInitializePosthook,
    /* set_values_posthook       */ XmInheritSetValuesPosthook,
    /* secondary_object_class    */ XmInheritClass,
    /* secondary_object_create   */ XmInheritSecObjectCreate,
    /* get_secondary_resources   */ XmInheritGetSecResData,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ XmInheritGetValuesPrehook,
    /* get_values_posthook       */ XmInheritGetValuesPosthook,
    /* class_part_init_prehook   */ XmInheritClassPartInitPrehook,
    /* class_part_init_posthook  */ XmInheritClassPartInitPosthook,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ False,
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};
#endif

XmDragOverShellClassRec xmDragOverShellClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &vendorShellClassRec,
        /* class_name            */ "XmDragOverShell",
        /* size                  */ sizeof(XmDragOverShellRec),
        /* class_initialize      */ NULL /*class_initialize*/,
        /* class_part_initialize */ class_part_initialize,
        /* class_inited          */ False,
        /* initialize            */ initialize,
      	/* initialize_hook       */ NULL,		
      	/* realize               */ XtInheritRealize,
      	/* actions               */ NULL,
      	/* num_actions           */ 0,
      	/* resources             */ resources,
      	/* resource_count        */ XtNumber(resources),
      	/* xrm_class             */ NULLQUARK,
      	/* compress_motion       */ False,
      	/* compress_exposure     */ True,
      	/* compress_enterleave   */ False,
      	/* visible_interest      */ False,
      	/* destroy               */ destroy,
      	/* resize                */ XtInheritResize,
      	/* expose                */ expose,
      	/* set_values            */ set_values,
      	/* set_values_hook       */ NULL,
      	/* set_values_almost     */ XtInheritSetValuesAlmost,  
      	/* get_values_hook       */ NULL,
      	/* accept_focus          */ NULL,
      	/* intrinsics version    */ XtVersion,
      	/* callback offsets      */ NULL,
      	/* tm_table              */ NULL,
      	/* query_geometry        */ XtInheritQueryGeometry,
      	/* display_accelerator   */ NULL,
      	/* extension             */ NULL
    },
    /* Composite Class Part */
    {
	/* geometry_manager */ XtInheritGeometryManager,
        /* change_managed   */ XtInheritChangeManaged,
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ NULL
    },
    /* Shell Class Part */
    {
	/* extension */ NULL
    },
    /* WMShell Class Part*/
    {
	/* extension */ NULL
    },
    /* Vendor Shell Class */
    {
	/* extension */	NULL
    },
    /* XmDragOverShell Class Part */
    {
	/* extension */ NULL
    }
};
/* *INDENT-ON* */


WidgetClass xmDragOverShellWidgetClass = (WidgetClass)(&xmDragOverShellClassRec);

/*
 * Things to remember:
 *  o We're a popup shell.
 *  o Our parent is a DragContext.
 *  o blendModel comes from DragContext, and is one of:
 *      XmBLEND_ALL, XmBLEND_STATE_SOURCE, XmBLEND_JUST_SOURCE, XmBLEND_NONE
 *  o mode and activeMode is ours, and is (I think) one of:
 *      XmWINDOW, XmPIXMAP, XmCURSOR
 *    AH.  An update.  Turns out that you use XmCURSOR if you're XmDYNAMIC,
 *    XmPIXMAP if it's preregister, and XmWINDOW when dragging is done.
 *  o provides the canvas for animation effects
 */

#if 0
static void 
class_initialize()
{
    _XmDragOverSCoreClassExtRec.record_type = XmQmotif;
}
#endif

static void 
class_part_initialize(WidgetClass widget_class)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:class_part_initialize(%d)\n",
    	__FILE__, __LINE__));
    _XmFastSubclassInit(widget_class, XmDRAG_OVER_SHELL_BIT);
}


static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s:initialize(%d) - %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    XtWidth(new_w) = 0;
    XtHeight(new_w) = 0;

    DO_ActiveMode(new_w) = XmCURSOR;

    DO_InitialX(new_w) = DO_HotX(new_w);
    DO_InitialY(new_w) = DO_HotY(new_w);

    DO_CursorBlend(new_w).sourceIcon = NULL;
    DO_CursorBlend(new_w).sourceX = 0;
    DO_CursorBlend(new_w).sourceY = 0;
    DO_CursorBlend(new_w).mixedIcon = NULL;
    DO_CursorBlend(new_w).gc = NULL;

    DO_RootBlend(new_w).sourceIcon = NULL;
    DO_RootBlend(new_w).sourceX = 0;
    DO_RootBlend(new_w).sourceY = 0;
    DO_RootBlend(new_w).mixedIcon = NULL;
    DO_RootBlend(new_w).gc = NULL;

    DO_NCCursor(new_w) = None;
    DO_ActiveCursor(new_w) = None;

    DO_Backing(new_w).pixmap = XmUNSPECIFIED_PIXMAP;
    DO_Backing(new_w).x = 0;
    DO_Backing(new_w).y = 0;

    DO_TmpPix(new_w) = XmUNSPECIFIED_PIXMAP;
    DO_TmpBit(new_w) = XmUNSPECIFIED_PIXMAP;

    DO_IsVisible(new_w) = False;

    /* grabs on this here would be a really bad idea */
    XtRemoveAllCallbacks(new_w, XmNpopupCallback);
    XtRemoveAllCallbacks(new_w, XmNpopdownCallback);

    /* gotta build the mixed cursor */
    _XmDragOverChange(new_w, XmNO_DROP_SITE);
}

static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:destroy(%d)\n",
    	__FILE__, __LINE__));
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Widget dc = XtParent(new_w);	/* this had BETTER be a DragContext */

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s:set_values(%d) - %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    if (DO_Mode(new_w) != DO_Mode(old))
    {

	if (DC_BlendModel(dc) != XmBLEND_NONE)
	{

	    if (DO_Mode(new_w) == XmPIXMAP &&
		DO_RootBlend(new_w).sourceIcon == NULL)
	    {
		set_mode(new_w, XmCURSOR);
	    }
	    else
	    {
		set_mode(new_w, DO_Mode(new_w));
	    }
	}
    }

    return False;
}

static void
expose(Widget w, XEvent *event, Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:expose(%d)\n",
    	__FILE__, __LINE__));
}

/*
 * compute the position of the op and state cursors (obj1) relative to
 * the source cursor (obj2)
 */
static void
compute_pos(Widget w, XmDragIconObject obj1, XmDragIconObject obj2,
	    Position *x, Position *y)
{
    Widget par;			/* This had BETTER be a drag context */
    Widget dos;
    Dimension bw = 0, st = 0, ht = 0;
    Window root, child;
    int rootx, rooty, childx, childy;
    unsigned int mask;


    switch (DI_Attachment(obj1))
    {
    default:
	_XmWarning(w, "Invalid attachment for DragIcon.");
	/* FALL THROUGH */

    case XmATTACH_NORTH_WEST:
	*x = DI_OffsetX(obj1);
	*y = DI_OffsetY(obj1);
	return;

    case XmATTACH_NORTH:
	*x = (DI_Width(obj2) >> 1) + DI_OffsetX(obj1);
	*y = DI_OffsetY(obj1);
	return;

    case XmATTACH_NORTH_EAST:
	*x = DI_Width(obj2) + DI_OffsetX(obj1);
	*y = DI_OffsetY(obj1);
	return;

    case XmATTACH_EAST:
	*x = DI_Width(obj2) + DI_OffsetX(obj1);
	*y = (DI_Height(obj2) >> 1) + DI_OffsetY(obj1);
	return;

    case XmATTACH_SOUTH_EAST:
	*x = DI_Width(obj2) + DI_OffsetX(obj1);
	*y = DI_Height(obj2) + DI_OffsetY(obj1);
	return;

    case XmATTACH_SOUTH:
	*x = (DI_Width(obj2) >> 1) + DI_OffsetX(obj1);
	*y = DI_Height(obj2) + DI_OffsetY(obj1);
	return;

    case XmATTACH_SOUTH_WEST:
	*x = DI_OffsetX(obj1);
	*y = DI_Height(obj2) + DI_OffsetY(obj1);
	return;

    case XmATTACH_WEST:
	*x = DI_OffsetX(obj1);
	*y = (DI_Height(obj2) >> 1) + DI_OffsetY(obj1);
	return;

    case XmATTACH_CENTER:
	*x = (DI_Width(obj2) >> 1) + DI_OffsetX(obj1);
	*y = (DI_Height(obj2) >> 1) + DI_OffsetY(obj1);
	return;

    case XmATTACH_HOT:
	par = XtParent(w);

	if (DC_OrigDragOver(par) != NULL)
	{
	    dos = (Widget)DC_OrigDragOver(par);
	}
	else
	{
	    dos = w;
	}

	if (DO_RootBlend(dos).mixedIcon != NULL)
	{
	    *x = DI_HotX(DO_RootBlend(dos).mixedIcon) -
		DO_RootBlend(dos).sourceX -
		DI_HotX(obj1);
	    *y = DI_HotY(DO_RootBlend(dos).mixedIcon) -
		DO_RootBlend(dos).sourceY -
		DI_HotY(obj1);
	}
	else if (DO_CursorBlend(dos).mixedIcon != NULL)
	{
	    *x = DI_HotX(DO_CursorBlend(dos).mixedIcon) -
		DO_CursorBlend(dos).sourceX -
		DI_HotX(obj1);
	    *y = DI_HotY(DO_CursorBlend(dos).mixedIcon) -
		DO_CursorBlend(dos).sourceY -
		DI_HotY(obj1);
	}
	else
	{
	    XtVaGetValues(DC_SourceWidget(par),
			  XmNborderWidth, &bw,
			  XmNshadowThickness, &st,
			  XmNhighlightThickness, &ht,
			  NULL);

	    XQueryPointer(XtDisplay(dos), XtWindow(DC_SourceWidget(par)),
			  &root, &child, &rootx, &rooty,
			  &childx, &childy, &mask);

	    *x = childx - DI_HotX(obj1) - bw - ht - st;
	    *y = childy - DI_HotY(obj1) - bw - ht - st;
	}
    }
}

/*
 * compute the size of the combined cursor
 */
static void
compute_size(Widget w,
	     XmDragIconObject src, XmDragIconObject state, XmDragIconObject op,
	     Dimension *wd, Dimension *ht)
{
    Position cx = 0, cy = 0;
    Position x = 0, y = 0, stx, sty, sox, soy;

    if (state != NULL)
    {
	compute_pos(w, state, src, &stx, &sty);
	if (stx < x)
	{
	    x = stx;
	}
	if (sty < y)
	{
	    y = sty;
	}
    }

    if (op != NULL)
    {
	if (DI_Attachment(state) == XmATTACH_HOT && state != NULL)
	{
	    sox = stx + DI_HotX(state) - DI_HotX(op);
	    soy = sty + DI_HotY(state) - DI_HotY(op);
	}
	else
	{
	    compute_pos(w, op, src, &sox, &soy);
	}

	if (sox < x)
	{
	    x = sox;
	}
	if (soy < y)
	{
	    y = soy;
	}
    }

    cx = -x;
    cy = -y;
    cx += DI_Width(src);
    cy += DI_Height(src);

    if (state != NULL)
    {
	stx -= x;
	sty -= y;
	if (stx + DI_Width(state) > cx)
	{
	    cx = stx + DI_Width(state);
	}
	if (sty + DI_Height(state) > cy)
	{
	    cy = sty + DI_Height(state);
	}
    }

    if (op != NULL)
    {
	sox -= x;
	soy -= y;
	if (sox + DI_Width(op) > cx)
	{
	    cx = sox + DI_Width(op);
	}
	if (soy + DI_Height(op) > cy)
	{
	    cy = soy + DI_Height(op);
	}
    }

    *wd = cx;
    *ht = cy;
}

static Boolean
cursor_fits(Widget w,
	    XmDragIconObject src, XmDragIconObject state, XmDragIconObject op)
{
    Dimension cw, ch, sw, sh;

    if (DI_Depth(src) != 1 || DI_Pixmap(src) == XmUNSPECIFIED_PIXMAP)
    {
	return False;
    }

    compute_size(w, src, state, op, &cw, &ch);

    _XmGetMaxCursorSize(w, &sw, &sh);

    if (cw > sw || ch > sh)
    {
	return False;
    }

    return True;
}

static void
copy_to_window(Widget w, XmDragIconObject mixedIcon, Window win,
	       Position x, Position y)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmDragOver: COPY TO WINDOW\n"));

    XSetFunction(XtDisplay(w), DO_RootBlend(w).gc, GXcopy);

    if (DI_Mask(mixedIcon) != XmUNSPECIFIED_PIXMAP)
    {
	XSetClipMask(XtDisplay(w), DO_RootBlend(w).gc, DI_Mask(mixedIcon));
    }
    else
    {
	XSetClipMask(XtDisplay(w), DO_RootBlend(w).gc, None);
    }

    if (DI_Depth(mixedIcon) == 1)
    {
	XCopyPlane(XtDisplay(w),
		   DI_Pixmap(mixedIcon), win,
		   DO_RootBlend(w).gc,
		   0, 0, XtWidth(w), XtHeight(w),
		   x, y, 1);
    }
    else if (DI_Depth(mixedIcon) == CoreDepth(w))
    {
	XCopyArea(XtDisplay(w),
		  DI_Pixmap(mixedIcon), win,
		  DO_RootBlend(w).gc,
		  0, 0, XtWidth(w), XtHeight(w),
		  x, y);
    }

    XSetClipMask(XtDisplay(w), DO_RootBlend(w).gc, None);
}

/*
 * blend the icons into a single image.
 */
static void
blend_icons(Widget w, XmDragOverBlend blend,
	    XmDragIconObject src, XmDragIconObject state, XmDragIconObject op)
{
    XGCValues values;
    Display *disp = XtDisplay(w);
    Screen *scr = XtScreen(w);
    XRectangle rect;
    unsigned char bm = DC_BlendModel(XtParent(w));

    if (blend->gc == NULL)
    {
	values.foreground = 1;
	values.background = 0;
	values.graphics_exposures = False;
	values.fill_style = FillSolid;
	values.function = GXclear;
	blend->gc = XCreateGC(disp, DI_Pixmap(blend->mixedIcon),
			    GCForeground | GCBackground | GCGraphicsExposures |
			      GCFillStyle | GCFunction,
			      &values);
    }
    else
    {
	values.foreground = BlackPixelOfScreen(scr);
	values.background = WhitePixelOfScreen(scr);
	values.graphics_exposures = False;
	values.fill_style = FillSolid;
	values.function = GXclear;
	XChangeGC(disp, blend->gc,
		  GCForeground | GCBackground | GCGraphicsExposures |
		  GCFillStyle | GCFunction,
		  &values);
    }

    rect.x = rect.y = 0;
    rect.width = DI_Width(blend->mixedIcon);
    rect.height = DI_Height(blend->mixedIcon);
    XSetClipRectangles(disp, blend->gc, 0, 0, &rect, 1, Unsorted);

    /* clear it out */
    if (DI_Mask(blend->mixedIcon) != XmUNSPECIFIED_PIXMAP)
    {
	XCopyPlane(disp,
		   DI_Mask(blend->mixedIcon), DI_Mask(blend->mixedIcon),
		   blend->gc,
		   0, 0,
		   DI_Width(blend->mixedIcon), DI_Height(blend->mixedIcon),
		   0, 0, 1);
    }

    XSetFunction(disp, blend->gc, GXor);

    if (DI_Mask(blend->mixedIcon) != XmUNSPECIFIED_PIXMAP &&
	bm != XmBLEND_NONE)
    {

	/* copy in the masks */
	rect.x = DI_XOffset(src);
	rect.y = DI_YOffset(src);
	rect.width = DI_Width(src);
	rect.height = DI_Height(src);
	XSetClipRectangles(disp, blend->gc, 0, 0, &rect, 1, Unsorted);

	if (DI_Mask(src) != XmUNSPECIFIED_PIXMAP)
	{
	    XCopyPlane(disp, DI_Mask(src), DI_Mask(blend->mixedIcon),
		       blend->gc, 0, 0, DI_Width(src), DI_Height(src),
		       DI_XOffset(src), DI_YOffset(src), 1);
	}
	else
	{
	    XSetFunction(disp, blend->gc, GXset);
	    XFillRectangles(disp, DI_Mask(blend->mixedIcon),
			    blend->gc, &rect, 1);
	    XSetFunction(disp, blend->gc, GXor);
	}

	if ( state != NULL )
	  {
		rect.x = DI_XOffset(state);
		rect.y = DI_YOffset(state);
		rect.width = DI_Width(state);
		rect.height = DI_Height(state);
		XSetClipRectangles(disp, blend->gc, 0, 0, &rect, 1, Unsorted);
		if (DI_Mask(state) != XmUNSPECIFIED_PIXMAP &&
			bm != XmBLEND_JUST_SOURCE)
		  {
			XCopyPlane(disp, DI_Mask(state), DI_Mask(blend->mixedIcon),
					   blend->gc, 0, 0, DI_Width(state), DI_Height(state),
					   DI_XOffset(state),
					   DI_YOffset(state), 1);
		  }
		else if (bm != XmBLEND_JUST_SOURCE)
		  {
			XSetFunction(disp, blend->gc, GXset);
			XFillRectangles(disp, DI_Mask(blend->mixedIcon),
							blend->gc, &rect, 1);
			XSetFunction(disp, blend->gc, GXor);
		  }
	  }

	if ( op != NULL )
	  {
		rect.x = DI_XOffset(op);
		rect.y = DI_YOffset(op);
		rect.width = DI_Width(op);
		rect.height = DI_Height(op);
		XSetClipRectangles(disp, blend->gc, 0, 0, &rect, 1, Unsorted);
		if (DI_Mask(op) != XmUNSPECIFIED_PIXMAP &&
			bm != XmBLEND_JUST_SOURCE && bm != XmBLEND_STATE_SOURCE)
		  {
			XCopyPlane(disp, DI_Mask(op), DI_Mask(blend->mixedIcon),
					   blend->gc, 0, 0, DI_Width(op), DI_Height(op),
					   DI_XOffset(op),
					   DI_YOffset(op), 1);
		  }
		else if (bm != XmBLEND_JUST_SOURCE && bm != XmBLEND_STATE_SOURCE)
		  {
			XSetFunction(disp, blend->gc, GXset);
			XFillRectangles(disp, DI_Mask(blend->mixedIcon),
							blend->gc, &rect, 1);
			XSetFunction(disp, blend->gc, GXor);
		  }
	  }
	}

    values.function = GXclear;
    XChangeGC(disp, blend->gc,
	      GCFunction,
	      &values);

    if (DI_Pixmap(blend->mixedIcon) != XmUNSPECIFIED_PIXMAP)
    {
	XCopyArea(disp,
		  DI_Pixmap(blend->mixedIcon), DI_Pixmap(blend->mixedIcon),
		  blend->gc,
		  0, 0,
		  DI_Width(blend->mixedIcon), DI_Height(blend->mixedIcon),
		  0, 0);
    }

    values.function = GXcopy;
    XChangeGC(disp, blend->gc,
	      GCFunction,
	      &values);

    rect.x = rect.y = 0;
    rect.width = DI_Width(blend->mixedIcon);
    rect.height = DI_Height(blend->mixedIcon);
    XSetClipRectangles(disp, blend->gc, 0, 0, &rect, 1, Unsorted);

    if (bm == XmBLEND_NONE)
    {
	return;
    }

    if (src && DI_Pixmap(src) != XmUNSPECIFIED_PIXMAP)
    {

	if (DI_Mask(src) != XmUNSPECIFIED_PIXMAP)
	{
	    XSetClipOrigin(disp, blend->gc,
			   DI_XOffset(src), DI_YOffset(src));
	    XSetClipMask(disp, blend->gc, DI_Mask(src));
	}
	else
	{
	    rect.x = DI_XOffset(src);
	    rect.y = DI_YOffset(src);
	    rect.width = DI_Width(src);
	    rect.height = DI_Height(src);
	    XSetClipRectangles(disp, blend->gc, 0, 0, &rect, 1, Unsorted);
	}
	if (DI_Depth(src) == DI_Depth(blend->mixedIcon))
	{
	    XCopyArea(disp, DI_Pixmap(src), DI_Pixmap(blend->mixedIcon),
		      blend->gc, 0, 0, DI_Width(src), DI_Height(src),
		      DI_XOffset(src), DI_YOffset(src));
	}
	else
	{
	    XCopyPlane(disp, DI_Pixmap(src), DI_Pixmap(blend->mixedIcon),
		       blend->gc, 0, 0, DI_Width(src), DI_Height(src),
		       DI_XOffset(src), DI_YOffset(src), 1);
	}

    }
    if (state && DI_Pixmap(state) != XmUNSPECIFIED_PIXMAP &&
	bm != XmBLEND_JUST_SOURCE)
    {

	if (DI_Mask(state) != XmUNSPECIFIED_PIXMAP)
	{
	    XSetClipOrigin(disp, blend->gc,
			   DI_XOffset(state), DI_YOffset(state));
	    XSetClipMask(disp, blend->gc, DI_Mask(state));
	}
	else
	{
	    rect.x = DI_XOffset(state);
	    rect.y = DI_YOffset(state);
	    rect.width = DI_Width(state);
	    rect.height = DI_Height(state);
	    XSetClipRectangles(disp, blend->gc, 0, 0, &rect, 1, Unsorted);
	}
	if (DI_Depth(state) == DI_Depth(blend->mixedIcon))
	{
	    XCopyArea(disp, DI_Pixmap(state), DI_Pixmap(blend->mixedIcon),
		      blend->gc, 0, 0, DI_Width(state), DI_Height(state),
		      DI_XOffset(state),
		      DI_YOffset(state));
	}
	else
	{
	    XCopyPlane(disp, DI_Pixmap(state), DI_Pixmap(blend->mixedIcon),
		       blend->gc, 0, 0, DI_Width(state), DI_Height(state),
		       DI_XOffset(state),
		       DI_YOffset(state), 1);
	}

    }
    if (op && DI_Pixmap(op) != XmUNSPECIFIED_PIXMAP &&
	bm != XmBLEND_JUST_SOURCE && bm != XmBLEND_STATE_SOURCE)
    {

	if (DI_Mask(op) != XmUNSPECIFIED_PIXMAP)
	{
	    XSetClipOrigin(disp, blend->gc,
			   DI_XOffset(op), DI_YOffset(op));
	    XSetClipMask(disp, blend->gc, DI_Mask(op));
	}
	else
	{
	    rect.x = DI_XOffset(op);
	    rect.y = DI_YOffset(op);
	    rect.width = DI_Width(op);
	    rect.height = DI_Height(op);
	    XSetClipRectangles(disp, blend->gc, 0, 0, &rect, 1, Unsorted);
	}
	if (DI_Depth(op) == DI_Depth(blend->mixedIcon))
	{
	    XCopyArea(disp, DI_Pixmap(op), DI_Pixmap(blend->mixedIcon),
		      blend->gc, 0, 0, DI_Width(op), DI_Height(op),
		      DI_XOffset(op),
		      DI_YOffset(op));
	}
	else
	{
	    XCopyPlane(disp, DI_Pixmap(op), DI_Pixmap(blend->mixedIcon),
		       blend->gc, 0, 0, DI_Width(op), DI_Height(op),
		       DI_XOffset(op),
		       DI_YOffset(op), 1);
	}
    }
}

/*
 * destroy the blended cursor
 */
static void
destroy_mixed(Widget w, XmDragIconObject icon)
{
    XmScreen scr = (XmScreen)XmGetXmScreen(XtScreen(w));

    if (DI_Pixmap(icon))
    {
	_XmFreeScratchPixmap(scr, DI_Pixmap(icon));
	DI_Pixmap(icon) = XmUNSPECIFIED_PIXMAP;
    }

    if (DI_Mask(icon))
    {
	_XmFreeScratchPixmap(scr, DI_Mask(icon));
	DI_Pixmap(icon) = XmUNSPECIFIED_PIXMAP;
    }

    XtDestroyWidget((Widget)icon);
}

/*
 * combine the cursors
 */
static void
make_soup(Widget w,
	  XmDragIconObject src, XmDragIconObject state, XmDragIconObject op,
	  XmDragOverBlend blend, Boolean clip)
{
    XmScreen s = (XmScreen)XmGetXmScreen(XtScreen(w));
    Position stx = 0, sty = 0;
    Position sox = 0, soy = 0;
    Position srx = 0, sry = 0;
    Position maxx, maxy;
    Dimension sw, sh, wd, ht;
    Position minx = 0, miny = 0;
    Pixmap pix, mask;
    int depth, argc;
    Position hotx, hoty;
    Arg args[8];
    XmDragOverBlend cblend = &DO_CursorBlend(w);
    XmDragIconObject cmix = blend->mixedIcon;

    if (state != NULL)
    {
	compute_pos(w, state, src, &stx, &sty);
	if (stx < minx)
	{
	    minx = stx;
	}
	if (sty < miny)
	{
	    miny = sty;
	}
    }

    if (op != NULL)
    {
	if (DI_Attachment(state) == XmATTACH_HOT && state != NULL)
	{
	    sox = stx + DI_HotX(state) - DI_HotX(op);
	    soy = sty + DI_HotY(state) - DI_HotY(op);
	}
	else
	{
	    compute_pos(w, op, src, &sox, &soy);
	}

	if (sox < minx)
	{
	    minx = sox;
	}
	if (soy < miny)
	{
	    miny = soy;
	}
    }

    srx -= minx;
    sry -= miny;
    maxx = srx + DI_Width(src);
    maxy = sry + DI_Height(src);

    if (state != NULL)
    {
	stx -= minx;
	sty -= miny;
	if (stx + DI_Width(state) > maxx)
	{
	    maxx = stx + DI_Width(state);
	}
	if (sty + DI_Height(state) > maxy)
	{
	    maxy = sty + DI_Height(state);
	}
	hotx = stx + DI_HotX(state);
	hoty = sty + DI_HotY(state);
    }
    else
    {
	hotx = srx + DI_HotX(src);
	hoty = sry + DI_HotY(src);
    }

    if (op != NULL)
    {
	sox -= minx;
	soy -= miny;
	if (sox + DI_Width(op) > maxx)
	{
	    maxx = sox + DI_Width(op);
	}
	if (soy + DI_Height(op) > maxy)
	{
	    maxy = soy + DI_Height(op);
	}
    }

    wd = maxx;
    ht = maxy;

    if (clip)
    {

	_XmGetMaxCursorSize(w, &sw, &sh);

#if 0
	printf("DIM: %d %d %d %d %d %d %d %d %d %d\n",
	       wd, ht, sox, soy, stx, sty, srx, sry, hotx, hoty);
#endif

	if (wd > sw)
	{
	    if (hotx <= ((Position)sw >> 1))
	    {
		minx = 0;
	    }
	    else if (hotx >= (wd - (sw >> 1)))
	    {
		minx = wd - sw;
	    }
	    else
	    {
		minx = (wd - sw) >> 1;
	    }

	    hotx -= minx;
	    sox -= minx;
	    stx -= minx;
	    srx -= minx;
	    wd = sw;
	}

	if (ht > sh)
	{
	    if (hoty <= ((Position)sh >> 1))
	    {
		miny = 0;
	    }
	    else if (hoty >= (ht - (sh >> 1)))
	    {
		miny = ht - sh;
	    }
	    else
	    {
		miny = (ht - sh) >> 1;
	    }

	    hoty -= miny;
	    soy -= miny;
	    sty -= miny;
	    sry -= miny;
	    ht = sw;
	}
    }

#if 0
    printf("DIM: %d %d %d %d %d %d %d %d %d %d\n",
	   wd, ht, sox, soy, stx, sty, srx, sry, hotx, hoty);
#endif

    if (cblend == blend)
    {
	depth = 1;
    }
    else
    {
	depth = CoreDepth(w);
    }

    if (cmix != NULL &&
	(DI_Depth(cmix) != depth ||
	 DI_Width(cmix) != wd || DI_Height(cmix) != ht))
    {
	destroy_mixed(w, cmix);
	cmix = NULL;
	blend->mixedIcon = NULL;
    }

    if (DI_Mask(src) == XmUNSPECIFIED_PIXMAP &&
	(state == NULL || DI_Mask(state) == XmUNSPECIFIED_PIXMAP) &&
	(op == NULL || DI_Mask(op) == XmUNSPECIFIED_PIXMAP))
    {
    }


    pix = _XmAllocScratchPixmap(s, depth, wd, ht);
    mask = _XmAllocScratchPixmap(s, 1, wd, ht);

    argc = 0;
    XtSetArg(args[argc], XmNpixmap, pix); argc++;
    XtSetArg(args[argc], XmNmask, mask); argc++;
    XtSetArg(args[argc], XmNdepth, depth); argc++;
    XtSetArg(args[argc], XmNwidth, wd); argc++;
    XtSetArg(args[argc], XmNheight, ht); argc++;
    XtSetArg(args[argc], XmNhotX, hotx); argc++;
    XtSetArg(args[argc], XmNhotY, hoty); argc++;

    blend->mixedIcon = (XmDragIconObject)XmCreateDragIcon(w, "mixedIcon",
							  args, argc);
    DI_XOffset(src) = srx;
    DI_YOffset(src) = sry;
	if ( op != NULL )
	  {
		DI_XOffset(op) = sox;
		DI_YOffset(op) = soy;
	  }
	if ( state != NULL )
	  {
		DI_XOffset(state) = stx;
		DI_YOffset(state) = sty;
	  }

    blend_icons(w, blend, src, state, op);
}

/*
 * create a pixmap cursor combining the icon objects
 */
static Cursor
get_cursor(Widget w,
	   XmDragIconObject src, XmDragIconObject state, XmDragIconObject op,
	   Boolean clip, Boolean dirty)
{
    Screen *scr = XtScreen(w);
    Display *disp = XtDisplay(w);
    Cursor ret = None;
    XColor backfore[2];
    XmDragCursorCache *cache = NULL, tmp = NULL;
    Pixmap pix, mask;
    Boolean in_cache = True;

    if (clip && !cursor_fits(w, src, state, op))
    {
	return None;
    }

    backfore[0].pixel = DO_CursorBackground(w);
    backfore[1].pixel = DO_CursorForeground(w);
    XQueryColors(disp, CoreColormap(w), backfore, 2);


    if (state == NULL || DI_Attachment(state) != XmATTACH_HOT)
    {

	cache = _XmGetDragCursorCachePtr((XmScreen)XmGetXmScreen(scr));
	if (*cache != NULL)
	{
	    tmp = *cache;

	    while (tmp)
	    {
		if (tmp->sourceIcon == src && tmp->opIcon == op &&
		    tmp->stateIcon == state)
		{

		    if (dirty)
		    {
			break;
		    }

		    XRecolorCursor(disp, tmp->cursor,
				   &backfore[1], &backfore[0]);

		    return tmp->cursor;
		}

		tmp = tmp->next;
	    }
	}
    }
    else
	in_cache = False;


    make_soup(w, src, state, op, &DO_CursorBlend(w), clip);

    if (DI_Mask(DO_CursorBlend(w).mixedIcon) != XmUNSPECIFIED_PIXMAP)
    {
	mask = DI_Mask(DO_CursorBlend(w).mixedIcon);
    }
    else
    {
	mask = None;
    }

    pix = DI_Pixmap(DO_CursorBlend(w).mixedIcon);

    ret = XCreatePixmapCursor(disp, pix, mask, &backfore[1], &backfore[0],
			      DI_HotX(DO_CursorBlend(w).mixedIcon),
			      DI_HotY(DO_CursorBlend(w).mixedIcon));

    /* if it should be cached, update it there.  The caller will see that
     * it gets put into the DO_ActiveCursor field */
    if (in_cache)
    {
	if (tmp)
	{
	    XFreeCursor(disp, tmp->cursor);
	}
	else
	{
	    tmp = (XmDragCursorCache)XtMalloc(sizeof(XmDragCursorRec));
	    tmp->next = *cache;
	    tmp->cursor = ret;
	    tmp->stateIcon = state;
	    tmp->opIcon = op;
	    tmp->sourceIcon = src;
	    *cache = tmp;
	}

	tmp->cursor = ret;
    }
    /* otherwise, set this as the NonCached cursor */
    else
    {
	if (DO_NCCursor(w) != None)
	{
	    XFreeCursor(disp, DO_NCCursor(w));
	}

	DO_NCCursor(w) = ret;
    }

    return ret;
}

/*
 * (possibly) allocate colors for the cursor
 */
static Boolean
get_colors(Widget w)
{
    Widget par = XtParent(w);	/* This had BETTER be a DragContext */
    Screen *scr = XtScreen(w);
    Display *disp = XtDisplay(w);
    XColor backfore[2];
    XGCValues values;
    Boolean new_gc = False;
    static int warned = False;

    backfore[0].pixel = DC_CursorBackground(par);

    switch (DO_CursorState(w))
    {
    case XmNO_DROP_SITE:
	backfore[1].pixel = DC_NoneCursorForeground(par);
	break;

    case XmINVALID_DROP_SITE:
	backfore[1].pixel = DC_InvalidCursorForeground(par);
	break;

    case XmVALID_DROP_SITE:
	backfore[1].pixel = DC_ValidCursorForeground(par);
	break;

    default:
	_XmWarning(w, "DragCursor in invalid state: %d.", DO_CursorState(w));
	backfore[1].pixel = DC_NoneCursorForeground(par);
	break;
    }

    if ((CoreColormap(w) != DefaultColormapOfScreen(scr) &&
         (XQueryColors(disp, CoreColormap(w), backfore, 2),
          !XAllocColor(disp, DefaultColormapOfScreen(scr), &backfore[0]) ||
          !XAllocColor(disp, DefaultColormapOfScreen(scr), &backfore[1]))) ||
        backfore[0].pixel == backfore[1].pixel)
    {
        warned = True;
        _XmWarning(w,
            "Cannot allocate colormap entry, some colors may be incorrect");
	backfore[0].pixel = BlackPixelOfScreen(scr);
	backfore[1].pixel = WhitePixelOfScreen(scr);
    }

    /* root GC not created */
    if (DO_RootBlend(w).gc == NULL)
    {

	DO_CursorBackground(w) = backfore[0].pixel;
	DO_CursorForeground(w) = backfore[1].pixel;

	values.foreground = backfore[1].pixel;
	values.background = backfore[0].pixel;
	/* just like PanedWindow for line effects. */
	values.subwindow_mode = IncludeInferiors;
	values.graphics_exposures = False;

	DO_RootBlend(w).gc = XCreateGC(disp, RootWindowOfScreen(scr),
				       GCForeground | GCBackground |
				       GCSubwindowMode | GCGraphicsExposures,
				       &values);
	new_gc = True;
    }
    /* root GC already created?  Update fg and bg */
    else
    {

	DO_CursorBackground(w) = backfore[0].pixel;
	DO_CursorForeground(w) = backfore[1].pixel;

	values.foreground = backfore[1].pixel;
	values.background = backfore[0].pixel;

	XChangeGC(disp, DO_RootBlend(w).gc, GCForeground | GCBackground,
		  &values);
    }

    return new_gc;
}

static void
set_mode(Widget w, int mode)
{
    XmDragOverBlend blend;
    XmDragIconObject src;
    XmScreen scr = (XmScreen)XmGetXmScreen(XtScreen(w));
    XSetWindowAttributes attr;

    if (DO_ActiveMode(w) == XmCURSOR && mode != XmCURSOR)
    {

	DO_ActiveCursor(w) = _XmGetNullCursor(w);

	XChangeActivePointerGrab(XtDisplay(w),
				 ButtonPressMask | ButtonReleaseMask |
				 ButtonMotionMask,
				 DO_ActiveCursor(w),
				 DC_LastChangeTime(w));

    }
    else if (DO_ActiveMode(w) != XmCURSOR)
    {

	if (DO_ActiveMode(w) == XmWINDOW)
	{
	    XtPopdown(w);
	}

	XSetClipMask(XtDisplay(w), DO_RootBlend(w).gc, None);

	XCopyArea(XtDisplay(w), DO_Backing(w).pixmap,
		  RootWindowOfScreen(XtScreen(w)),
		  DO_RootBlend(w).gc, 0, 0,
		  XtWidth(w), XtHeight(w),
		  DO_Backing(w).x, DO_Backing(w).y);
    }

    DO_IsVisible(w) = False;
    DO_ActiveMode(w) = mode;

    if (mode == XmCURSOR)
    {

	_XmDragOverChange(w, DO_CursorState(w));

	return;
    }

    if (DO_RootBlend(w).sourceIcon != NULL)
    {
	src = DO_RootBlend(w).sourceIcon;
	blend = &DO_RootBlend(w);
    }
    else
    {
	src = DO_CursorBlend(w).sourceIcon;
	blend = &DO_CursorBlend(w);
    }

    make_soup(w, src, DO_StateIcon(w), DO_OpIcon(w),
	      blend, False);

    DO_Backing(w).x = DO_HotX(w) - DI_HotX(blend->mixedIcon);
    DO_Backing(w).y = DO_HotY(w) - DI_HotY(blend->mixedIcon);
    XtX(w) = DO_Backing(w).x;
    XtY(w) = DO_Backing(w).y;

    if (XtWidth(w) != DI_Width(blend->mixedIcon) ||
	XtHeight(w) != DI_Height(blend->mixedIcon))
    {

	XtWidth(w) = DI_Width(blend->mixedIcon);
	XtHeight(w) = DI_Height(blend->mixedIcon);

	if (DO_Backing(w).pixmap != XmUNSPECIFIED_PIXMAP)
	{
	    _XmFreeScratchPixmap(scr, DO_Backing(w).pixmap);
	    DO_Backing(w).pixmap = XmUNSPECIFIED_PIXMAP;
	}

	if (DO_TmpPix(w) != XmUNSPECIFIED_PIXMAP)
	{
	    _XmFreeScratchPixmap(scr, DO_TmpPix(w));
	    DO_TmpPix(w) = XmUNSPECIFIED_PIXMAP;
	}

	if (DO_TmpBit(w) != XmUNSPECIFIED_PIXMAP)
	{
	    _XmFreeScratchPixmap(scr, DO_TmpBit(w));
	    DO_TmpBit(w) = XmUNSPECIFIED_PIXMAP;
	}
    }

    if (DO_Backing(w).pixmap == XmUNSPECIFIED_PIXMAP)
	DO_Backing(w).pixmap =
	    _XmAllocScratchPixmap(scr,
				  CoreDepth(w),
				  XtWidth(w), XtHeight(w));

    XSetClipMask(XtDisplay(w), DO_RootBlend(w).gc, None);

    XCopyArea(XtDisplay(w),
	      RootWindowOfScreen(XtScreen(w)),
	      DO_Backing(w).pixmap,
	      DO_RootBlend(w).gc,
	      DO_Backing(w).x, DO_Backing(w).y,
	      XtWidth(w), XtHeight(w), 0, 0);

    if (DO_ActiveMode(w) != XmPIXMAP)
    {

	XtPopup(w, XtGrabNone);

	attr.cursor = _XmGetNullCursor(w);

	XChangeWindowAttributes(XtDisplay(w), XtWindow(w),
				CWCursor, &attr);
    }

    copy_to_window(w, blend->mixedIcon,
		   RootWindowOfScreen(XtScreen(w)),
		   XtX(w), XtY(w));

    DO_IsVisible(w) = True;
}

static void
change_window(Widget w)
{
}

/*
 * melt the icon
 */
static void
melt_cursor(Widget w)
{
  DEBUGOUT(_LtDebug( __FILE__, w, "Melting not implemented.\n" ));
}

/*
 * return the icon back to the drag site
 */
#define ZAP_EFFECT2 5
#define ZAP_SIZE2 6

static void
zap_cursor(Widget w)
{
    Screen  *screen = XtScreen(w);
    GC gc;
    XGCValues	gc_values;
    XSegment segments[4];
    int zap_count, idx;
    int adjust_w, adjust_h;

    DEBUGOUT(_LtDebug(__FILE__, w, "zap_cursor is implemented.\n"));
    zap_count = 0;
    gc = DO_RootBlend(w).gc;

    for ( idx = 0; idx < 4; idx++ )
    {
    	segments[idx].x1 = DO_InitialX(w);
    	segments[idx].y1 = DO_InitialY(w);
    }
    segments[0].x2 = XtX(w);
    segments[0].y2 = XtY(w);

    segments[1].x2 = XtX(w);
    segments[1].y2 = XtY(w) + XtHeight(w);

    segments[2].x2 = XtX(w) + XtWidth(w);
    segments[2].y2 = XtY(w) + XtHeight(w);

    segments[3].x2 = XtX(w) + XtWidth(w);
    segments[3].y2 = XtY(w);

    adjust_h = (DO_InitialY(w) - (XtY(w) + (XtHeight(w)>>1))) / ZAP_EFFECT2;
    adjust_w = (DO_InitialX(w) - (XtX(w) + (XtWidth(w)>>1))) / ZAP_EFFECT2;

    gc_values.foreground = (~0);
    gc_values.function = GXxor;
    gc_values.clip_mask = (Pixmap)NULL;
    XChangeGC( DisplayOfScreen(screen), gc,
    	       GCFunction |GCForeground | GCClipMask,
    	       &gc_values );

    XDrawSegments( DisplayOfScreen(screen), RootWindowOfScreen(screen),
    	    	   gc, segments, 4);

    XFlush( DisplayOfScreen(screen) );

    if ( abs(adjust_h) <= ZAP_SIZE2
    && abs(adjust_w) <= ZAP_SIZE2 )
    {
    	zap_count = ZAP_EFFECT2;
    }

    for ( ; ; zap_count++)
    {
	_XmMicroSleep( 50000 );

	XDrawSegments( DisplayOfScreen(screen), RootWindowOfScreen(screen),
    	    	       gc, segments, 4);

	gc_values.foreground = DO_CursorForeground(w);
	gc_values.function = GXcopy;
	XChangeGC( DisplayOfScreen(screen), gc,
	    	   GCFunction | GCForeground,
    		   &gc_values );

	XCopyArea( DisplayOfScreen(screen), DO_Backing(w).pixmap,
    		   RootWindowOfScreen(screen), gc,
		   0, 0, XtWidth(w), XtHeight(w),
		   segments[0].x2, segments[0].y2 );

	if ( zap_count == 5 )
    	    break;

    	segments[0].x2 += adjust_w;
    	segments[0].y2 += adjust_h;
    	segments[1].x2 += adjust_w;
    	segments[1].y2 += adjust_h;
    	segments[2].x2 += adjust_w;
    	segments[2].y2 += adjust_h;
    	segments[3].x2 += adjust_w;
    	segments[3].y2 += adjust_h;

    	XCopyArea( DisplayOfScreen(screen), RootWindowOfScreen(screen),
    	       DO_Backing(w).pixmap, gc,
	       segments[0].x2, segments[0].y2,
	       XtWidth(w), XtHeight(w), 0 ,0 );

    	copy_to_window( w,
	    DO_RootBlend(w).mixedIcon ? DO_RootBlend(w).mixedIcon
	    : DO_CursorBlend(w).mixedIcon,
    	    RootWindowOfScreen(screen),
	    segments[0].x2, segments[0].y2 );

    	gc_values.foreground = (~0);
    	gc_values.function = GXxor;
    	XChangeGC( DisplayOfScreen(screen), gc,
	    	GCFunction | GCForeground,
    	    	&gc_values );

    	XDrawSegments( DisplayOfScreen(screen), RootWindowOfScreen(screen),
	    	       gc, segments, 4);
    	XFlush( DisplayOfScreen(screen) );
    }

    XFlush( DisplayOfScreen(screen) );
}
#undef ZAP_EFFECT2
#undef ZAP_SIZE2

void
_XmDragOverHide(Widget w, Position clipOriginX, Position clipOriginY,
		XmRegion clipRegion)
{
    Widget dc = XtParent(w);	/* this had BETTER be a DragContext */
    Boolean clip_on = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmDragOverHide(%d)\n",
    	__FILE__, __LINE__));
    if (!DO_IsVisible(w) || DC_BlendModel(dc) == XmBLEND_NONE ||
	DO_ActiveMode(w) == XmCURSOR)
    {
	return;
    }

    if (DO_ActiveMode(w) == XmWINDOW)
    {
	XtPopdown(w);
    }

    if (DO_ActiveMode(w) != XmWINDOW && clipRegion != NULL)
    {
	clip_on = True;
	_XmRegionSetGCRegion(XtDisplay(w), DO_RootBlend(w).gc,
			     clipOriginX, clipOriginY, clipRegion);
    }
    else
    {
	XSetClipMask(XtDisplay(w), DO_RootBlend(w).gc,
		     None);
    }

    if (DO_Backing(w).pixmap != XmUNSPECIFIED_PIXMAP)
    {
	XCopyArea(XtDisplay(w), DO_Backing(w).pixmap,
		  RootWindowOfScreen(XtScreen(w)),
		  DO_RootBlend(w).gc, 0, 0,
		  XtWidth(w), XtHeight(w),
		  DO_Backing(w).x, DO_Backing(w).y);
    }

    if (clip_on)
    {
	XSetClipMask(XtDisplay(w), DO_RootBlend(w).gc, None);
    }

    DO_IsVisible(w) = False;
}

void
_XmDragOverShow(Widget w, Position clipOriginX, Position clipOriginY,
		XmRegion clipRegion)
{
    Widget dc = XtParent(w);	/* this had BETTER be a DragContext */
    Boolean clip_on = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmDragOverShow(%d)\n",
    	__FILE__, __LINE__));
    if (!DO_IsVisible(w) || DC_BlendModel(dc) == XmBLEND_NONE ||
	DO_ActiveMode(w) == XmCURSOR)
    {
	return;
    }

    /* note that we clip differently on the show (the origin
     * offsets).  This is invertible with Hide, above i -- see
     * the offsets in the XCopyArea */
    if (DO_ActiveMode(w) != XmWINDOW && clipRegion != NULL)
    {
	clip_on = True;
	_XmRegionSetGCRegion(XtDisplay(w), DO_RootBlend(w).gc,
			     clipOriginX - DO_Backing(w).x,
			     clipOriginY - DO_Backing(w).y,
			     clipRegion);
    }
    else
    {
	XSetClipMask(XtDisplay(w), DO_RootBlend(w).gc,
		     None);
    }


    if (DO_Backing(w).pixmap != XmUNSPECIFIED_PIXMAP)
    {
	XCopyArea(XtDisplay(w),
		  RootWindowOfScreen(XtScreen(w)),
		  DO_Backing(w).pixmap,
		  DO_RootBlend(w).gc,
		  DO_Backing(w).x, DO_Backing(w).y,
		  XtWidth(w), XtHeight(w), 0, 0);
    }

    if (clip_on)
    {
	XSetClipMask(XtDisplay(w), DO_RootBlend(w).gc, None);
    }

    if (DO_ActiveMode(w) != XmPIXMAP)
    {
	XtPopup(w, XtGrabNonexclusive);
    }

    if (DO_RootBlend(w).mixedIcon != NULL)
    {
	copy_to_window(w, DO_RootBlend(w).mixedIcon, XtWindow(w), 0, 0);
    }
    else
    {
	copy_to_window(w, DO_CursorBlend(w).mixedIcon, XtWindow(w), 0, 0);
    }

    DO_IsVisible(w) = True;
}

void
_XmDragOverMove(Widget w, Position x, Position y)
{
    XmScreen scr = (XmScreen)XmGetXmScreen(XtScreen(w));
    Widget dc = XtParent(w);	/* this had BETTER be a DragContext */
    Pixmap opix, npix;
    XmDragIconObject di;

    DO_HotX(w) = x;
    DO_HotY(w) = y;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmDragOverMove\n"));

    if (!DO_IsVisible(w) || DC_BlendModel(dc) == XmBLEND_NONE ||
	DO_ActiveMode(w) == XmCURSOR)
    {
	return;
    }

    if (DO_RootBlend(w).mixedIcon != NULL)
    {
	di = DO_RootBlend(w).mixedIcon;
    }
    else
    {
	di = DO_CursorBlend(w).mixedIcon;
    }

    x -= DI_HotX(w);
    y -= DI_HotY(w);

    XtX(w) = x;
    XtY(w) = y;

    if (DO_ActiveMode(w) == XmWINDOW)
    {
	XMoveWindow(XtDisplay(w), XtWindow(w), x, y);
    }

    opix = DO_Backing(w).pixmap;

    if (DO_TmpPix(w) == XmUNSPECIFIED_PIXMAP)
    {
	DO_TmpPix(w) = _XmAllocScratchPixmap(scr,
					     CoreDepth(w),
					     XtWidth(w), XtHeight(w));
    }

    npix = DO_TmpPix(w);

    XSetClipMask(XtDisplay(w), DO_RootBlend(w).gc, None);
    XSetFunction(XtDisplay(w), DO_RootBlend(w).gc, GXcopy);

    XCopyArea(XtDisplay(w), RootWindowOfScreen(XtScreen(w)),
	      npix, DO_RootBlend(w).gc, x, y,
	      XtWidth(w), XtHeight(w), 0, 0);

    /* we've got to check and see if the new backing overlaps
     * the old.  If it does, we've got to fart around a bit. */
    if (x + XtWidth(w) > DO_Backing(w).x &&
	x < XtWidth(w) + DO_Backing(w).x &&
	y < XtHeight(w) + DO_Backing(w).y &&
	y < XtHeight(w) + DO_Backing(w).y)
    {
    }
    else
    {
	XCopyArea(XtDisplay(w), opix,
		  RootWindowOfScreen(XtScreen(w)),
		  DO_RootBlend(w).gc,
		  0, 0, XtWidth(w), XtHeight(w),
		  DO_Backing(w).x, DO_Backing(w).y);

	copy_to_window(w, di, RootWindowOfScreen(XtScreen(w)), x, y);
    }

    DO_Backing(w).x = x;
    DO_Backing(w).y = y;
    DO_Backing(w).pixmap = npix;
    DO_TmpPix(w) = opix;
}

void
_XmDragOverChange(Widget w, unsigned char dropSiteStatus)
{
    Widget par = XtParent(w);	/* This had BETTER be a DragContext */
    XmDragIconObject src_icon = NULL, op_icon = NULL, state_icon = NULL;
    Boolean src_pix = True, dirty, change;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmDragOverChange(%d)\n",
    	__FILE__, __LINE__));
    DO_CursorState(w) = dropSiteStatus;

    if (DO_Mode(w) == XmWINDOW)
    {
	return;
    }

    if (DC_BlendModel(par) == XmBLEND_NONE)
    {
	return;
    }

    if (DO_Mode(w) == XmPIXMAP)
    {
	src_icon = DC_SourcePixmapIcon(par);
    }

    if (src_icon == NULL || XtScreen(w) != XtScreen(src_icon) ||
	(DI_Depth(src_icon) != CoreDepth(w) && DI_Depth(src_icon) != 1))
    {

	src_pix = False;

	src_icon = DC_SourceCursorIcon(par);

	if (src_icon == NULL || XtScreen(w) != XtScreen(src_icon) ||
	    (DI_Depth(src_icon) != CoreDepth(w) && DI_Depth(src_icon) != 1))
	{
	    src_icon = _XmScreenGetSourceIcon(w);
	}
    }

    /*
     * gather up the cursors according to blendModel
     */
    switch (DC_BlendModel(par))
    {
    default:
	_XmWarning(w, "Invalid blend model.");
	/* FALL THROUGH (i.e., if they screw up, give them everything */

    case XmBLEND_ALL:

	op_icon = DC_OperationCursorIcon(par);

	if (op_icon == NULL || XtScreen(w) != XtScreen(op_icon) ||
	    DI_Depth(op_icon) != 1)
	{

	    op_icon = _XmScreenGetOperationIcon(w, DC_Operation(par));

	    if (op_icon && DI_Depth(op_icon) != 1)
	    {
		op_icon = NULL;
	    }
	}
	/* FALL THROUGH */

    case XmBLEND_STATE_SOURCE:

	state_icon = DC_StateCursorIcon(par);

	if (state_icon == NULL || XtScreen(w) != XtScreen(state_icon) ||
	    DI_Depth(state_icon) != 1)
	{

	    state_icon = _XmScreenGetStateIcon(w, dropSiteStatus);

	    if (state_icon && DI_Depth(state_icon) != 1)
	    {
		state_icon = NULL;
	    }
	}
	/* FALL THROUGH */

    case XmBLEND_JUST_SOURCE:
	break;
    }

    if (!_XmDragIconIsDirty(src_icon) &&
	(op_icon == NULL || !_XmDragIconIsDirty(op_icon)) &&
	(state_icon == NULL || !_XmDragIconIsDirty(state_icon)))
    {
	dirty = False;
    }
    else
    {
	dirty = True;
    }

    if (!get_colors(w) &&
	DO_StateIcon(w) == state_icon &&
	DO_OpIcon(w) == op_icon &&
	DO_RootBlend(w).sourceIcon == src_icon &&
	!dirty)
    {
	change = False;
    }
    else
    {
	change = True;
    }

    DO_StateIcon(w) = state_icon;
    DO_OpIcon(w) = op_icon;
    DO_CursorBlend(w).sourceIcon = src_icon;

    if (!src_pix &&
	(DO_ActiveCursor(w) = get_cursor(w, src_icon, state_icon, op_icon,
					 False, dirty)) != None)
    {
	_XmDragIconClean(src_icon, state_icon, op_icon);

	if (DO_ActiveMode(w) != XmCURSOR)
	{
	    _XmDragOverHide(w, 0, 0, NULL);
	    DO_ActiveMode(w) = XmCURSOR;
	}

	XChangeActivePointerGrab(XtDisplay(w),
				 ButtonPressMask | ButtonReleaseMask |
				 ButtonMotionMask,
				 DO_ActiveCursor(w),
				 DC_LastChangeTime(par));

	DO_RootBlend(w).sourceIcon = NULL;

	if (DO_RootBlend(w).mixedIcon)
	{
	    destroy_mixed(w, DO_RootBlend(w).mixedIcon);
	    DO_RootBlend(w).mixedIcon = NULL;
	}

    }
    else if (DO_Mode(w) == XmCURSOR)
    {

	DO_RootBlend(w).sourceIcon = src_icon;

	DO_ActiveCursor(w) = get_cursor(w, src_icon, state_icon, op_icon,
					True, dirty);

	_XmDragIconClean(src_icon, state_icon, op_icon);

	if (DO_ActiveMode(w) != XmCURSOR)
	{
	    _XmDragOverHide(w, 0, 0, NULL);
	    DO_ActiveMode(w) = XmCURSOR;
	}

	XChangeActivePointerGrab(XtDisplay(w),
				 ButtonPressMask | ButtonReleaseMask |
				 ButtonMotionMask,
				 DO_ActiveCursor(w),
				 DC_LastChangeTime(par));
    }
    else
    {

	DO_RootBlend(w).sourceIcon = src_icon;

	if (change || DO_ActiveMode(w) != XmPIXMAP)
	{
	    _XmDragIconClean(src_icon, state_icon, op_icon);

	    set_mode(w, XmPIXMAP);
	}
    }
}

/*
 * called on BRelease
 */
void
_XmDragOverFinish(Widget w, unsigned char completionStatus)
{
    Widget dc = XtParent(w);	/* this had BETTER be a drag context */
    Display *disp = XtDisplay(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmDragOverFinish(%d)\n",
    	__FILE__, __LINE__));
    if (DC_BlendModel(dc) == XmBLEND_NONE)
    {
	return;
    }

    XGrabServer(disp);

    change_window(w);

    if (completionStatus != XmDROP)
    {
	melt_cursor(w);
    }
    else
    {
	zap_cursor(w);
    }

    XtPopdown(w);

    DO_IsVisible(w) = False;

    XUngrabServer(disp);
}

Cursor
_XmDragOverGetActiveCursor(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmDragOverGetActiveCursor(%d)\n",
    	__FILE__, __LINE__));
    return DO_ActiveCursor(w);
}

void
_XmDragOverSetInitialPosition(Widget w, Position initialX, Position initialY)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmDragOverSetInitialPosition(%d)\n",
    	__FILE__, __LINE__));
    DO_InitialX(w) = initialX;
    DO_InitialY(w) = initialY;
}
