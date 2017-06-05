/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Form.c,v 1.3 2007/09/12 20:31:05 jwrdegoede Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright © 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Form.c,v 1.3 2007/09/12 20:31:05 jwrdegoede Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/FormP.h>
#include <Xm/DialogS.h>

#include <XmI/DebugUtil.h>

#undef	DEBUG
#define	PRINT_ATTACHMENT_REPORT
#define	PRINT_REPORT
#define	PRINT_ASSIGNMENTS
#define	PRINT_PREFERRED
#define	PRINT_LINENO

/*
 * Whether to produce sensible warnings that we've never seen Motif generate
 * Do this with configure now.
 *      #define LESSTIF_VERBOSE
 */

/* Forward Declarations */

static void _XmFormExportOffset(Widget widget, int offset, XtArgVal *value);
static XmImportOperator _XmFormImportOffset(Widget widget, int offset, XtArgVal *value);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

/*
   static void realize(Widget w, XtValueMask *value_mask, 
   XSetWindowAttributes *attributes);
 */

static void resize(Widget w);

static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);

static void change_managed(Widget w);

static void constraint_initialize(Widget request, Widget new_w,
				  ArgList args, Cardinal *num_args);

static Boolean constraint_set_values(Widget curr, Widget req, Widget new_w,
				     ArgList args, Cardinal *num_args);

static void _XmFormLayout(Widget f, Widget cw, XtWidgetGeometry *cg,
			  Dimension *width, Dimension *height);

static XtGeometryResult _XmFormGeomRequest(Widget f,
					   Dimension *wd, Dimension *ht);

static void _XmFormConfigureChildren(Widget f, Widget cw, XtWidgetGeometry *cg);

static void _XmFormFindPreferred(Widget f, Widget cw, XtWidgetGeometry *cg);

static void _XmFormPrintAttachmentReport(Widget f);
static void _XmCheckAttachedWidget(Widget f, int i, XmFormConstraints con);

static void _XmFormAllPaths(Widget f, Dimension *ww, Dimension *hh);

/*
 * Resources for the Form class
 */
#define Offset(field) XtOffsetOf(XmFormRec, form.field)
static XtResource resources[] =
{
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmFormRec, bulletin_board.margin_width),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), XtOffsetOf(XmFormRec, bulletin_board.margin_height),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNhorizontalSpacing, XmCSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(horizontal_spacing),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNverticalSpacing, XmCSpacing, XmRVerticalDimension,
	sizeof(Dimension), Offset(vertical_spacing),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNfractionBase, XmCMaxValue, XmRInt,
	sizeof(int), Offset(fraction_base),
	XmRImmediate, (XtPointer)100
    },
    {
	XmNrubberPositioning, XmCRubberPositioning, XmRBoolean,
	sizeof(Boolean), Offset(rubber_positioning),
	XmRImmediate, (XtPointer)False
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNmarginWidth,
	sizeof(Dimension), XtOffsetOf(XmFormRec, bulletin_board.margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), XtOffsetOf(XmFormRec, bulletin_board.margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNhorizontalSpacing,
	sizeof(Dimension), Offset(horizontal_spacing),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNverticalSpacing,
	sizeof(Dimension), Offset(vertical_spacing),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};

#undef Offset

#define Offset(field) XtOffsetOf(XmFormConstraintRec, form.field)

#define LEFT 0
#define RIGHT 1
#define TOP 2
#define BOTTOM 3

/*
 * The opposite direction
 */
#define	OPPOSITE_DIRECTION(x)	((x & 2) | (1 - (x & 1)))

static XtResource formConstraintResources[] =
{
    {
	XmNtopAttachment, XmCAttachment, XmRAttachment,
	sizeof(unsigned char), Offset(att[TOP].type),
	XmRImmediate, (XtPointer)XmATTACH_NONE
    },
    {
	XmNbottomAttachment, XmCAttachment, XmRAttachment,
	sizeof(unsigned char), Offset(att[BOTTOM].type),
	XmRImmediate, (XtPointer)XmATTACH_NONE
    },
    {
	XmNleftAttachment, XmCAttachment, XmRAttachment,
	sizeof(unsigned char), Offset(att[LEFT].type),
	XmRImmediate, (XtPointer)XmATTACH_NONE
    },
    {
	XmNrightAttachment, XmCAttachment, XmRAttachment,
	sizeof(unsigned char), Offset(att[RIGHT].type),
	XmRImmediate, (XtPointer)XmATTACH_NONE
    },
    {
	XmNtopWidget, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(att[TOP].w),
	XtRImmediate, (XtPointer)NULL
    },
    {
	XmNbottomWidget, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(att[BOTTOM].w),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNleftWidget, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(att[LEFT].w),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNrightWidget, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(att[RIGHT].w),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNtopPosition, XmCPosition, XmRInt,
	sizeof(int), Offset(att[TOP].percent),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNbottomPosition, XmCPosition, XmRInt,
	sizeof(int), Offset(att[BOTTOM].percent),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNleftPosition, XmCPosition, XmRInt,
	sizeof(int), Offset(att[LEFT].percent),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNrightPosition, XmCPosition, XmRInt,
	sizeof(int), Offset(att[RIGHT].percent),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNtopOffset, XmCOffset, XmRVerticalInt,
	sizeof(int), Offset(att[TOP].offset),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNbottomOffset, XmCOffset, XmRVerticalInt,
	sizeof(int), Offset(att[BOTTOM].offset),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNleftOffset, XmCOffset, XmRHorizontalInt,
	sizeof(int), Offset(att[LEFT].offset),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNrightOffset, XmCOffset, XmRHorizontalInt,
	sizeof(int), Offset(att[RIGHT].offset),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNresizable, XmCBoolean, XmRBoolean,
	sizeof(Boolean), Offset(resizable),
	XmRImmediate, (XtPointer)True
    }
};

static XmSyntheticResource constraint_syn_resources[] =
{
    {
	XmNtopOffset,
	sizeof(int), Offset(att[TOP].offset),
	_XmFormExportOffset, _XmFormImportOffset
	/*
	   _XmFromVerticalPixels, _XmToVerticalPixels
	 */
    },
    {
	XmNbottomOffset,
	sizeof(int), Offset(att[BOTTOM].offset),
	_XmFormExportOffset, _XmFormImportOffset
	/*
	   _XmFromVerticalPixels, _XmToVerticalPixels
	 */
    },
    {
	XmNleftOffset,
	sizeof(int), Offset(att[LEFT].offset),
	_XmFormExportOffset, _XmFormImportOffset
	/*
	   _XmFromHorizontalPixels, _XmToHorizontalPixels
	 */
    },
    {
	XmNrightOffset,
	sizeof(int), Offset(att[RIGHT].offset),
	_XmFormExportOffset, _XmFormImportOffset
	/*
	   _XmFromHorizontalPixels, _XmToHorizontalPixels
	 */
    }
};

XmFormClassRec xmFormClassRec =
{
    /* Core class part */
    {
	/* superclass            */ (WidgetClass)&xmBulletinBoardClassRec,
	/* class_name            */ "XmForm",
	/* widget_size           */ sizeof(XmFormRec),
	/* class_initialize      */ NULL,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize /*realize *//* see comments in realize XtInheritRealize */ ,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ False,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ False,
	/* visible_interest      */ False,
	/* destroy               */ NULL,
	/* resize                */ resize,
	/* expose                */ XtInheritExpose, /* Motif has one */
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost, /* Motif has one */
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager,
	/* change_managed   */ change_managed,
	/* insert_child     */ XtInheritInsertChild,
	/* delete_child     */ XtInheritDeleteChild, /* Motif has one */
	/* extension        */ NULL,
    },
    /* Constraint class part */
    {
	/* subresources      */ formConstraintResources,
	/* subresource_count */ XtNumber(formConstraintResources),
	/* constraint_size   */ sizeof(XmFormConstraintRec),
	/* initialize        */ constraint_initialize,
	/* destroy           */ NULL,
	/* set_values        */ constraint_set_values,
	/* extension         */ NULL,
    },
    /* XmManager class part */
    {
	/* translations                 */ XtInheritTranslations,
	/* syn_resources                */ syn_resources,
	/* num_syn_resources            */ XtNumber(syn_resources),
	/* syn_constraint_resources     */ constraint_syn_resources,
	/* num_syn_constraint_resources */ XtNumber(constraint_syn_resources),
	/* parent_process               */ XmInheritParentProcess,
	/* extension                    */ NULL
    },
    /* XmBulletinBoard part */
    {
	/* always_install_accelerators  */ False,
	/* geo_matrix_create            */ NULL,
	/* focus_moved_proc             */ XmInheritFocusMovedProc,
	/* extension                    */ NULL
    },
    /* XmForm part */
    {
	/* extension                    */ NULL
    }
};


WidgetClass xmFormWidgetClass = (WidgetClass)&xmFormClassRec;


static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmFORM_BIT);
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

    if (BB_MarginWidth(new_w) == XmINVALID_DIMENSION)
    {
	BB_MarginWidth(new_w) = 0;
    }
    if (BB_MarginHeight(new_w) == XmINVALID_DIMENSION)
    {
	BB_MarginHeight(new_w) = 0;
    }
    Form_ProcessingConstraints(new_w) = False;	/* Avoid UMR later */
}


#if 0
static void
realize(Widget w,
	XtValueMask *value_mask,
	XSetWindowAttributes *attributes)
{
/* rws 7 Feb 1998
   Motif inherits the realize method for Form but, since we are not
   participating in all of the GeoUtils stuff for Form this is necessary.
   If we inherit this from BulletinBoard we end up enforcing margins and
   other stuff that should not be done.  Using the GeoUtils would be one
   way to fix this but this is _much_ simpler at this stage!

   This showed up in xdir.  The llnlxdirButton in the top right beside
   the menubar was being placed wrong.  Form was calculating a y value
   of -2, because of the topOffset, and BulletinBoard was then enforcing
   a margin of 0.
 */
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:realize(%d) - %dx%d\n",
		      __FILE__, __LINE__,
		      XtWidth(w), XtHeight(w)));

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass
}
#endif

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean relayout = False, refresh = False;

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

    BB_InSetValues(new_w) = True;

    if (BB_MarginHeight(new_w) != BB_MarginHeight(old) ||
	BB_MarginWidth(new_w) != BB_MarginWidth(old) ||
	Form_FractionBase(new_w) != Form_FractionBase(old))
    {
	relayout = True;
	refresh = True;
    }
    if (XtWidth(new_w) == 0 || XtHeight(new_w) == 0)
    {
	relayout = True;
	refresh = True;
    }

    if (relayout && XtIsRealized(new_w) && XtIsManaged(new_w))
    {
	Dimension wd, ht;
	/* rws 3 Nov 1998
	   The realized and managed bit come from budget->new transaction
	   form/test44
	   This is the same sort of thing as constraint_set_values, but it
	   has alot more examples.
	 */

	/* rws 17 Jul 1997
	 * If the height/width is explicitly changed we must try our best
	 * to honor the new size. The initial flag is irrelevent. No matter
	 * when the change occurs we must try to honor it.  Besides if we
	 * set the width/height to 0 Form will calculate its smallest size.
	 * If the request is larger than that it will not be honored.
	 *
	 * test15 sub test 1
	 */
	wd = XtWidth(new_w);
	ht = XtHeight(new_w);

	_XmFormLayout(new_w, NULL, NULL, &wd, &ht);

	XtWidth(new_w) = wd;
	XtHeight(new_w) = ht;
    }

    BB_InSetValues(new_w) = False;

    return refresh;
}


static void
resize(Widget w)
{
    Dimension wd, ht;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:resize(%d) - %dx%d\n",
		      __FILE__, __LINE__,
		      XtWidth(w), XtHeight(w)));

    wd = XtWidth(w);
    ht = XtHeight(w);

    _XmFormLayout(w, NULL, NULL, &wd, &ht);

    _XmFormConfigureChildren(w, NULL, NULL);
}


/*
 * query_geometry
 *
 * The purpose of this method is for other widgets to ask what the preferred
 *      geometry of this one is.
 * The other widget (the parent, usually) can either propose a geometry, to
 *      which we answer, or call XtQueryGeometry(w, NULL, ___). In that case,
 *      we'll get a proposed which has no entries filled out. (Xt does some
 *      magic so we'll never see the NULL).
 * It is possible, and even common practice, to call this with the second and
 *      third parameter being identical. We must be prepared for this.
 *      That's why we copy the contents of *proposed.
 */
static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *desired)
{
    /* form/test24 */
    Dimension wd, ht;
    XtWidgetGeometry pp;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:query_geometry(%d) - proposed %s\n",
		      __FILE__, __LINE__,
		      _LtDebugWidgetGeometry2String(proposed)));

    pp = *proposed;

    if (BB_ResizePolicy(w) == XmRESIZE_NONE)
    {
	wd = XtWidth(w);
	ht = XtHeight(w);
    }
    else
    {
	wd = XtIsRealized(w) ? 0 : XtWidth(w);
	ht = XtIsRealized(w) ? 0 : XtHeight(w);

	_XmFormLayout(w, NULL, NULL, &wd, &ht);
	wd = pp.request_mode & CWWidth ? pp.width < wd ? wd : pp.width : wd;
	ht = pp.request_mode & CWHeight ? pp.height < ht ? ht : pp.height : ht;

	if (BB_ResizePolicy(w) == XmRESIZE_GROW)
	{
	    if (wd < XtWidth(w))
	    {
		wd = XtWidth(w);
	    }
	    if (ht < XtHeight(w))
	    {
		ht = XtHeight(w);
	    }
	}
    }

    desired->request_mode = CWWidth | CWHeight;
    desired->width = wd == 0 ? 1 : wd;
    desired->height = ht == 0 ? 1 : ht;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:query_geometry(%d) - desired %s\n",
		      __FILE__, __LINE__,
		      _LtDebugWidgetGeometry2String(desired)));
    return _XmGMReplyToQueryGeometry(w, &pp, desired);
}


/*
 * GeometryManager Method
 *
 * If request is unacceptable and we have no compromise, return XtGeometryNo.
 * If request is acceptable, return XtGeometryYes or XtGeometryDone (depending
 *     on whether LessTif wants widgets to do this by themselves).
 * If we propose a compromise, return XtGeometryAlmost.
 * If XtCWQueryOnly is set, don't change anything.
 *
 * Note that if you're not in test mode, and the result of the query will be
 * YES,and the geometry change implies that the form needs to be resized, then
 * resizing the form is one of the things this function needs to do !
 * Note also that doing so doesn't imply that we would have to reply with
 * XtGeometryDone. Done implies that we changed the child widget's geometry.
 *
 * According to Asente & Swick, do not resize the calling widget from here 
 * (p. 734).
 */
static XtGeometryResult
geometry_manager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
    Widget f = XtParent(w);
    XtWidgetGeometry mr, orig;
    int		good, ask;
    Dimension wd, ht;
    Dimension initial_width, initial_height;
    XtGeometryResult res;
    XmFormConstraints con;

    DEBUGOUT(_LtDebug2("GM", f, w, "GeometryManager - request %s\n",
		       _LtDebugWidgetGeometry2String(request)));

    if ((request->request_mode & (CWX | CWY | CWWidth | CWHeight)) == 0) {
	Form_ProcessingConstraints(f) = False;
	*reply = *request;
	return XtGeometryYes;
    }

    DEBUGOUT(_LtDebug2(__FILE__, f, w, "GeometryManager - request %s\n",
		       _LtDebugWidgetGeometry2String(request)));
    DEBUGOUT(_LtDebug2("GM", f, w, "GeometryManager - request %s\n",
		       _LtDebugWidgetGeometry2String(request)));

    orig = mr = *request;
    /*
     * Every child has a preferred width, based on geometry requests it makes
     * (whether they are granted or not).
     */
	con = (XmFormConstraints)CoreConstraints(w);

	if (!Form_ProcessingConstraints(f)) {
	    if (mr.request_mode & CWWidth) {
		FCP_PrefW(con) = mr.width;
	    }
	    if (mr.request_mode & CWHeight) {
		FCP_PrefH(con) = mr.height;
	    }
	} else {
	    Form_ProcessingConstraints(f) = False;
	}

    initial_width = XtWidth(f);
    initial_height = XtHeight(f);

    wd = XtIsRealized(f) ? 0 : XtWidth(f);
    ht = XtIsRealized(f) ? 0 : XtHeight(f);

    /* Ask form's layout algorithm what it thinks about this */
    _XmFormLayout(f, w, &mr, &wd, &ht);

    DEBUGOUT(_LtDebug2(__FILE__, f, w,
		       "GeometryManager: Form Layout makes child geo"
		       " %dx%d form %dx%d\n",
		       mr.width, mr.height, wd, ht));
    DEBUGOUT(_LtDebug2("GM", f, w,
		       "GeometryManager: Form Layout makes child geo"
		       " %dx%d%+i%+i form %dx%d %dx%d\n",
		       mr.width, mr.height, mr.x, mr.y, wd, ht,
		       XtWidth(f), XtHeight(f)));

    if ((res = _XmFormGeomRequest(f, &wd, &ht)) == XtGeometryYes) {
	/* Ask form's layout algorithm what it thinks about this, again.
	 * Why?  Because the parent may have returned a compromise geometry,
	 * and we don't really know.
	 */
	_XmFormLayout(f, w, &mr, &wd, &ht);

	DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, w,
			   "GeometryManager: Form Second Layout makes child geo"
			   " %dx%d form %dx%d\n",
			   mr.width, mr.height, wd, ht));
    } else {
	mr = *request;
	wd = XtWidth(f);
	ht = XtHeight(f);
	_XmFormLayout(f, w, &mr, &wd, &ht);
	DEBUGOUT(_LtDebug2("GM", f, w,
			   "GeometryManager: Form Layout 2 makes child geo"
			   " %dx%d form %dx%d %dx%d\n",
			   mr.width, mr.height, wd, ht,
			   XtWidth(f), XtHeight(f)));
    }

    /*
     * Now "mr" contains the new geometry, and "orig" contains the request.
     * Now that form has looked at the geometry, tell our caller what happened.
     */
#define       Wants(x)        (orig.request_mode & x)

    *reply = mr;
    good = ask = 0;
    if (Wants(CWX)) {
	ask++;
	if ((mr.request_mode & CWX) && mr.x == orig.x) {
	    good++;
	}
    }
    if (Wants(CWY)) {
	ask++;
	if ((mr.request_mode & CWY) && mr.y == orig.y) {
	    good++;
   	}
    }
    if (Wants(CWWidth)) {
	ask++;
	if ((mr.request_mode & CWWidth) && mr.width == orig.width) {
	    good++;
	}
    }
    if (Wants(CWHeight)) {
	ask++;
	if ((mr.request_mode & CWHeight) && mr.height == orig.height) {
	    good++;
	}
    }
    if (Wants(CWBorderWidth)) {
	ask++;
	if ((mr.request_mode & CWBorderWidth) && mr.border_width == orig.border_width) {
	    good++;
	}
    }
#undef        Wants

    if (good == ask) {

	/* In the case where the request is granted, we may have to resize form
	 * itself
	 */
	_XmFormConfigureChildren(f, w, &mr);

	DEBUGOUT(_LtDebug2("nedit", f, w, "GeometryManager : [%s] -> [%s] => good %d ask %d, YES\n",
				_LtDebugWidgetGeometry2String(&orig),
				_LtDebugWidgetGeometry2String(reply),
				good, ask));
	return XtGeometryYes;
    } else {
	DEBUGOUT(_LtDebug2("GM", f, w, "GeometryManager: %dx%d %dx%d form %dx%d\n",
			   wd, ht,
			   initial_width, initial_height,
			   XtWidth(f), XtHeight(f)
			   ));
	if (wd != initial_width || ht != initial_height) {
	    XtWidth(f) = wd = initial_width;
	    XtHeight(f) = ht = initial_height;
	    _XmFormLayout(f, w, &mr, &wd, &ht);
	}
	_XmFormConfigureChildren(f, NULL, &mr);
    }

    DEBUGOUT(_LtDebug2("nedit", f, w, "GeometryManager : [%s] -> [%s] => good %d ask %d %s\n",
				_LtDebugWidgetGeometry2String(&orig),
				_LtDebugWidgetGeometry2String(reply),
				good, ask,
				(good == 0) ? "NO" : "ALMOST"));
   return (good == 0) ? XtGeometryNo : XtGeometryAlmost;
}


/*
 * change_managed method
 *
 * Called when adding a managed child, or by managing/unmanaging an
 *      existing child.
 *
 * Before Realize, change_managed is called only once (just before realize).
 *      At that time, we should do the layout algorithm to determine the
 *      form's initial size depending on the children.
 */
static void
change_managed(Widget w)
{
    Dimension wd, ht;

    DEBUGOUT(_LtDebug(__FILE__, w, "change_managed %s %s\n",
		      Form_InitialWidth(w) ? "InitialWidth" : " ",
		      Form_InitialHeight(w) ? "InitialHeight" : ""));
    DEBUGOUT(_LtDebug("GM", w, "change_managed %s %s %dx%d %s\n",
		      Form_InitialWidth(w) ? "InitialWidth" : " ",
		      Form_InitialHeight(w) ? "InitialHeight" : "",
		      XtWidth(w), XtHeight(w),
		      XtIsManaged(w) ? "Managed" : "Not Managed"));

    if (XtIsRealized(w) && !XtIsManaged(w))
    {
	return;
    }
    {
    Cardinal i;

    	for (i = 0; i < MGR_NumChildren(w); i++)
    	{
	XmFormConstraints con = (XmFormConstraints)CoreConstraints(MGR_Children(w)[i]);

	    if (XtIsManaged(MGR_Children(w)[i]))
	    {
    		if (FCP_PrefW(con) != XmINVALID_DIMENSION)
    		{
		    XtWidth(MGR_Children(w)[i]) = FCP_PrefW(con);
    		}
    		if (FCP_PrefH(con) != XmINVALID_DIMENSION)
    		{
		    XtHeight(MGR_Children(w)[i]) = FCP_PrefH(con);
    		}
	    }
    	}
    }
    wd = XtIsRealized(w) ? 0 : XtWidth(w);
    ht = XtIsRealized(w) ? 0 : XtHeight(w);

    _XmFormLayout(w, NULL, NULL, &wd, &ht);

    if (_XmFormGeomRequest(w, &wd, &ht) == XtGeometryYes)
    {
	DEBUGOUT(_LtDebug("DX", w, "change_managed Got yes %dx%d\n",
		      XtWidth(w), XtHeight(w)));
	/* Why layout again?  Cause we might have gotten a compromise */
	_XmFormLayout(w, NULL, NULL, &wd, &ht);

	_XmFormConfigureChildren(w, NULL, NULL);
    }
    else
    {
	DEBUGOUT(_LtDebug("DX", w, "change_managed Got no  %dx%d\n",
		      XtWidth(w), XtHeight(w)));
	/*
	 * MLM: I kept adding this, and then taking it out.
	 * Once and for all: this *has* to be here.  Above, we
	 * calculated the *desired* geometry.  However, if
	 * we get here, we got *No* back from our parent.  So
	 * we *must* calculate our layout based on our current
	 * geometry.  One optimization is possible, I think:
	 * if we get here, and wd == XtWidth(w) && ht = XtHeight(w),
	 * then this could probably be skipped.  Maybe later;
	 * it's safe to always do this.
	 */
	wd = XtWidth(w);
	ht = XtHeight(w);
	_XmFormLayout(w, NULL, NULL, &wd, &ht);

	_XmFormConfigureChildren(w, NULL, NULL);
    }

    _XmNavigChangeManaged(w);
    DEBUGOUT(_LtDebug("DX", w, "change_managed %dx%d\n",
		      XtWidth(w), XtHeight(w)));

    return;
}


static int
_XmGetOffset(Widget w, int i)
{
	XmFormConstraints con = (XmFormConstraints)CoreConstraints(w);
	int offset;

	if (FCP_Atta(con, i).offset == XmINVALID_DIMENSION) {
		if (FCP_Atta(con, i).type != XmATTACH_POSITION) {
			if (i == TOP || i == BOTTOM) {
				offset = Form_VertSpacing(XtParent(w));
			} else {
				offset = Form_HorizSpacing(XtParent(w));
			}
		} else {
			offset = 0;
		}
	} else {
		offset = FCP_Atta(con, i).offset;
	}
	return(offset);
}


/*
 * Called in the process of adding a child
 */
static void
constraint_initialize(Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
    XmFormConstraints con = (XmFormConstraints)CoreConstraints(new_w);
    Widget f = XtParent(new_w);
    int i;

    DEBUGOUT(_LtDebug2(__FILE__, f, new_w,
		       "%s:constraint_initialize(%d) - %i args\n"
		       "\trequest X %5i Y %5i W %5i H %5i\n"
		       "\t  new_w X %5i Y %5i W %5i H %5i\n"
		       "\t   Form W %5i H %5i\n"
		       "\t%s %s\n",
		       __FILE__, __LINE__,
		       *num_args,
		       XtX(request), XtY(request),
		       XtWidth(request), XtHeight(request),
		       XtX(new_w), XtY(new_w),
		       XtWidth(new_w), XtHeight(new_w),
		       XtWidth(XtParent(new_w)), XtHeight(XtParent(new_w)),
		       XtIsManaged(new_w) ? "Managed" : "Not Managed",
		       XtIsRealized(new_w) ? "Realized" : "Not Realized"
		       ));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    /* do this for all four contraints (TOP, BOTTOM, LEFT, RIGHT) */
    for (i = 0; i < 4; i++)
    {
	switch (FCP_Atta(con, i).type)
	{
	case XmATTACH_OPPOSITE_WIDGET:
	case XmATTACH_WIDGET:
	    _XmCheckAttachedWidget(f, i, con);
	    /* Fall tru */

	case XmATTACH_FORM:
	case XmATTACH_NONE:
	    FCP_Atta(con, i).percent = 0;
	    break;

	case XmATTACH_POSITION:
	    /*
	    FCP_Atta(con, i).percent =
		(int)(((float)FCP_Atta(con, i).value /
		       (float)Form_FractionBase(f)) * 100.0);
	    */
	    break;
	}
    }

    /* rws 2 Jul 1998
       Mozilla NavCenter window stop the left hand selector pane from coming
       up 12 pixels wide
       form/test56
     */
    FCP_PrefW(con) = XmINVALID_DIMENSION;
    FCP_PrefH(con) = XmINVALID_DIMENSION;
}


static Boolean
constraint_set_values(Widget current, Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
    XmFormConstraints ncon = (XmFormConstraints)CoreConstraints(new_w);
    XmFormConstraints old = (XmFormConstraints)CoreConstraints(current);
    Widget f = XtParent(new_w);
    Boolean changed = False;
    int i;

    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, new_w,
		       "%s:constraint_set_values(%d) - %i args\n"
		       "\tcurrent X %5i Y %5i W %5i H %5i\n"
		       "\trequest X %5i Y %5i W %5i H %5i\n"
		       "\t  new_w X %5i Y %5i W %5i H %5i\n"
		       "\t   Form W %5i H %5i\n",
		       __FILE__, __LINE__,
		       *num_args,
		       XtX(current), XtY(current),
		       XtWidth(current), XtHeight(current),
		       XtX(request), XtY(request),
		       XtWidth(request), XtHeight(request),
		       XtX(new_w), XtY(new_w),
		       XtWidth(new_w), XtHeight(new_w),
		       XtWidth(XtParent(new_w)), XtHeight(XtParent(new_w))));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));
    DEBUGOUT(_LtDebug2("DX", (Widget)f, new_w,
		       "%s:constraint_set_values(%d) - %i args\n"
		       "\tcurrent X %5i Y %5i W %5i H %5i\n"
		       "\trequest X %5i Y %5i W %5i H %5i\n"
		       "\t  new_w X %5i Y %5i W %5i H %5i\n"
		       "\t   Form W %5i H %5i\n",
		       __FILE__, __LINE__,
		       *num_args,
		       XtX(current), XtY(current),
		       XtWidth(current), XtHeight(current),
		       XtX(request), XtY(request),
		       XtWidth(request), XtHeight(request),
		       XtX(new_w), XtY(new_w),
		       XtWidth(new_w), XtHeight(new_w),
		       XtWidth(XtParent(new_w)), XtHeight(XtParent(new_w))));
    DEBUGOUT(_LtDebugPrintArgList("DX", new_w, args, *num_args, False));

    Form_ProcessingConstraints(f) = True;
    for (i = 0; i < 4; i++)
    {
	if (FCP_Atta(ncon, i).percent != FCP_Atta(old, i).percent)
	{
	    changed = True;
	}

	if (FCP_Atta(ncon, i).type != FCP_Atta(old, i).type ||
	    FCP_Atta(ncon, i).w != FCP_Atta(old, i).w ||
	    FCP_Atta(ncon, i).offset != FCP_Atta(old, i).offset)
	{
	    changed = True;

	    if (FCP_Atta(ncon, i).type != XmATTACH_WIDGET &&
	        FCP_Atta(ncon, i).type != XmATTACH_OPPOSITE_WIDGET)
	    {
		FCP_Atta(ncon, i).w = NULL;
	    }
	    _XmCheckAttachedWidget(f, i, ncon);
#if 0
	    if (FCP_Atta(ncon, i).type == XmATTACH_NONE &&
	        FCP_Atta(ncon, i).type != FCP_Atta(old, i).type &&
	        (i == LEFT || i == RIGHT))
	    {
	    	fprintf(stderr, "%s %i %i\n", XtName(new_w), XtWidth(request), FCP_PrefW(ncon));
/*	    	XtWidth(request) = FCP_PrefW(ncon); */
	    }
	    if (FCP_Atta(ncon, i).type == XmATTACH_NONE &&
	        FCP_Atta(ncon, i).type != FCP_Atta(old, i).type &&
	        (i == TOP || i == BOTTOM))
	    {
	    	fprintf(stderr, "%s %i %i\n", XtName(new_w), XtHeight(request), FCP_PrefH(ncon));
/*	    	XtHeight(request) = FCP_PrefH(ncon); */
	    }
#endif
	}
    }

    if (XtWidth(current) != XtWidth(new_w))
    {
	FCP_PrefW(ncon) = XtWidth(new_w);
	/* rws 28 Jun 1999
	   A constraint didn't change. Xt will pick up on the width/height
	   change, so let it handle it.
	changed = True;
	*/
    }

    if (XtHeight(current) != XtHeight(new_w))
    {
	FCP_PrefH(ncon) = XtHeight(new_w);
	/*
	changed = True;
	*/
    }

    if (!FCP_Resizable(ncon))
    {
	/* refuse request */
	/* MLM -- but AFTER recording preferred */
	XtWidth(new_w) = XtWidth(current);
	XtHeight(new_w) = XtHeight(current);

	changed = False;

	DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, new_w,
			   "constraint_set_values - refused resize\n"));
    }

    /* rws 24 Oct 1998 */
    /* scrolledwindow/test13 */
    /* form/test15 */
    /* Xinvest when centering in the frame */
    DEBUGOUT(_LtDebug2("DX", (Widget)f, new_w,"%s %s %s %s\n",
    	changed ? "True" : "False",
    	XtIsRealized(f) ? "True" : "False",
    	XtIsManaged(f) ? "True" : "False",
    	XtIsManaged(new_w) ? "True" : "False"
    	));
    /* rws 19 Oct 1999
       openDX splash screen (dx -edit) The pixmap should be centered
     */
    if (changed && XtIsRealized(f) && XtIsManaged(new_w))
    {
	/* rws 23 Jun 1999
	   OpenDX sub-classes XmForm to get WorkSpace, which is the main
	   canvas for dx -edit. This has todo with the auto positioning that
	   goes on in there. (Bug 107)
	 */
	Dimension wd, ht;
	XtWidgetGeometry cg;

	DEBUGOUT(_LtDebug(__FILE__, f,
			  "GeomRequest in constraint_set_values\n"));

	if (XtWidth(f) == 0 && XtHeight(f) == 0)
	{
	    return changed;
	}

	wd = XtIsRealized(f) ? 0 : XtWidth(f);
	ht = XtIsRealized(f) ? 0 : XtHeight(f);

	cg.request_mode = CWWidth | CWHeight;
	cg.width = XtWidth(request);
	cg.height = XtHeight(request);
	_XmFormLayout(f, new_w, &cg, &wd, &ht);

	if (_XmFormGeomRequest(f, &wd, &ht) == XtGeometryYes)
	{
	    _XmFormLayout(f, new_w, &cg, &wd, &ht);
	    _XmFormConfigureChildren(f, new_w, &cg);
	}
	else
	{
	    wd = XtWidth(f);
	    ht = XtHeight(f);

	    _XmFormLayout(f, new_w, &cg, &wd, &ht);
	    _XmFormConfigureChildren(f, new_w, &cg);
	}
	/*
	if (XtWidth(current) == XtWidth(new_w) &&
	    XtHeight(current) == XtHeight(new_w))
	    */
	{
	    /* form/test56 */
	    XtX(new_w)++;
	}
    }
    else
    {
	Form_ProcessingConstraints(f) = False;
    }

    return False;
    /* return changed; */
}

/************************* LAYOUT METHODS *****************************/

/*
 * These are just to save typing and make things readable
 * Note: most of these will only work in a loop over form's children,
 * in which a couple of variables are set correctly :
 *
 */

/*
 * Set the geometry fields of the indicated widget (child or self)
 */
#if	defined(DEBUG) || defined(PRINT_ASSIGNMENTS)

#define	SETX(p)		{ SetX(f, child, p, __LINE__); changed = True; }
#define	SETY(p)		{ SetY(f, child, p, __LINE__); changed = True; }
#define	SETW(p)		{ SetW(f, child, p, __LINE__); changed = True; }
#define	SETH(p)		{ SetH(f, child, p, __LINE__); changed = True; }

/*
 * Set geometry of a widget referred to with XmATTACH_WIDGET or so
 *
 * x is something like constraints->form.att[BOTTOM], p is the value to
 * be set
 */
#define	SETWX(x,p)	{ SetX(f, (x).w, p, __LINE__); changed = True; }
#define	SETWY(x,p)	{ SetY(f, (x).w, p, __LINE__); changed = True; }

#define	SETFW(v)	{ if (ParentChangeMode) { SetFW(f, child, v, __LINE__); fw = v; changed = True; } }
#define	SETFH(v)	{ if (ParentChangeMode) { SetFH(f, child, v, __LINE__); fh = v; changed = True; } }

#else /* !(defined(DEBUG) || defined(PRINT_ASSIGNMENTS)) */

#define	SETX(p)		{ FCP_X(con) = p; changed = True; }
#define	SETY(p)		{ FCP_Y(con) = p; changed = True; }
#define	SETW(p)		{ FCP_W(con) = p; changed = True; }
#define	SETH(p)		{ FCP_H(con) = p; changed = True; }

/*
 * Set geometry of a widget referred to with XmATTACH_WIDGET or so
 *
 * q is something like constraints->form.att[BOTTOM], p is the value to
 * be set
 */
#define	SETWX(q,p)	{ FormC_X((q).w) = p; changed = True; }
#define	SETWY(q,p)	{ FormC_Y((q).w) = p; changed = True; }

#define	SETFW(v)	{ if (ParentChangeMode) { fw = v; changed = True; } }
#define	SETFH(v)	{ if (ParentChangeMode) { fh = v; changed = True; } }

#endif /* defined(DEBUG) || defined(PRINT_ASSIGNMENTS) */

/* For widget attachments : same stuff but not for current widget */
#define	AttachedX	FormC_X
#define	AttachedY	FormC_Y
#define	AttachedWd	FormC_W
#define	AttachedHt	FormC_H


/*

 */
#if	defined(DEBUG) || defined(PRINT_ASSIGNMENTS)
static void
SetW(Widget f, Widget child, int p, int line)
{
    XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
		       "set width to %d (line %d)\n", p, line));
#else
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child, "set width to %d\n", p));
#endif
#endif
    FCP_W(con) = p;
}

static void
SetH(Widget f, Widget child, int p, int line)
{
    XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
		       "set height to %d (line %d)\n", p, line));
#else
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child, "set height to %d\n", p));
#endif
#endif
    FCP_H(con) = p;
}


static void
SetX(Widget f, Widget child, int p, int line)
{
    XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
		       "set x to %d (line %d)\n", p, line));
#else
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child, "set x to %d\n", p));
#endif
#endif
    FCP_X(con) = p;
}


static void
SetY(Widget f, Widget child, int p, int line)
{
    XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
		       "set y to %d (line %d)\n", p, line));
#else
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child, "set y to %d\n", p));
#endif
#endif
    FCP_Y(con) = p;
}


static void
SetFW(Widget f, Widget child, int p, int line)
{
#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
		       "set form width to %d (line %d)\n", p, line));
#else
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
		       "set form width to %d\n", p));
#endif
#endif
}


static void
SetFH(Widget f, Widget child, int p, int line)
{
#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
    _LtDebug2(__FILE__, (Widget)f, child,
	      "set form height to %d (line %d)\n", p, line);
#else
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
		       "set form height to %d\n", p));
#endif
#endif
}

#endif


static void
_XmCheckAttachedWidget(Widget f, int i, XmFormConstraints con)
{
    Widget w;

    /*
     * Check if the widget attached to is a child of this Form.
     * If not, see if one of its ancestors is. If yes, replace the
     * widget by its ancestor that is our child.
     */
    w = FCP_Atta(con, i).w;

    if (w && XtParent(w) != (Widget)f)
    {
	while (w)
	{
	    if (XtParent(w) == (Widget)f)
	    {
		DEBUGOUT(_LtDebug2(__FILE__, (Widget)f,
				   FCP_Atta(con, i).w,
				   "Replace child by %s\n", XtName(w)));
#ifdef	LESSTIF_VERBOSE
		/*
		 * Not sure if we should do this. I've never seen Motif issue a warning
		 * for this (nor an error). It seems better to indicate that something
		 * is wrong though.
		 */
		{
		    String pp[3];
		    Cardinal np = 3;

		    pp[0] = XtName(f);
		    pp[1] = XtName(FCP_Atta(con, i).w);
		    pp[2] = XtName(w);

		    XtAppWarningMsg(XtWidgetToApplicationContext((Widget)f),
				    "formGeometry", "formIllegalAttachment",
				    "LessTifError",
				    "XmForm %s : attachment to %s "
				    "which is not a child,\n"
				    "\treplaced with %s\n",
				    pp, &np);
		}
#endif

		FCP_Atta(con, i).w = w;
		break;
	    }
	    else
	    {
		w = XtParent(w);
	    }

	    if (w == NULL)
	    {
#ifdef	LESSTIF_VERBOSE
		String pp[2];
		Cardinal np = 2;

		pp[0] = XtName(f);
		pp[1] = XtName(FCP_Atta(con, i).w);

		XtAppWarningMsg(XtWidgetToApplicationContext((Widget)f),
				"formGeometry", "formIllegalAttachment",
				"LessTifError",
				"XmForm %s : attachment to %s "
				"which is not a child,\n\treset to NULL\n",
				pp, &np);
#endif
		FCP_Atta(con, i).w = NULL;
	    }
	}
    }
}


/* define the maximum iterations of the layout routine to
 * be the same as Motif's */

#if 0
/* Motif says 10000 here */
#define MAX_ITERATIONS 10000
#else
#define	MAX_ITERATIONS	200
#endif

/*
 * Find Preferred Geometry for the form's children
 *
 * Note: currently we're not called efficiently. We should find out when
 *      exactly we need to update this information !
 * Doing all this over and over again is useless and sloooow.
 *
 * FIX ME
 *
 * rws 16 Apr 1997
 * The initial preferred size is now set in constraint init, as long as this
 * values is updated in constraint set values and in geometry manager, find
 * preferred should not be necessary.
 *
 * rws 18 Apr 1997
 * According to the X Toolkit Intrinsics Programming Manual, Motif Edition
 * "Only the Frame, MainWindow, and ScrolledWindow classes in the Motif
 *  set call XtQueryGeomtry()" (Bottom Pg 388)
 */
static void
_XmFormFindPreferred(Widget f, Widget cw, XtWidgetGeometry *cg)
{
    Cardinal i;
    Boolean changed;		/* is needed because of SETX macros */

    DEBUGOUT(_LtDebug(__FILE__, (Widget)f, "_XmFormFindPreferred() - start\n"));
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, cw, "_XmFormFindPreferred() - request %s\n",
		       _LtDebugWidgetGeometry2String(cg)));
    for (i = 0; i < MGR_NumChildren(f); i++)
    {
	Widget child = MGR_Children(f)[i];
	XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

	if (XtIsManaged(child))
	{
	    XtWidgetGeometry p;

	    if (cw && cw == child && cg)
	    {
		p.x = (cg->request_mode & CWX) ? cg->x : (XtWidth(f) != 0 ? 0 : XtX(child));
		p.y = (cg->request_mode & CWY) ? cg->y : (XtHeight(f) != 0 ? 0 : XtY(child));
		p.width = (cg->request_mode & CWWidth) ? cg->width : XtWidth(child);
		p.height = (cg->request_mode & CWHeight) ? cg->height : XtHeight(child);
		p.border_width = (cg->request_mode & CWBorderWidth) ? cg->border_width : XtBorderWidth(child);
	    }
	    else
	    {
		/*
		 * I'm not sure why this test was here, it breaks test60
		 * because we forget to look at the current x/y for
		 * XmATTACH_SELF cases.
		 * Danny 16/6/2002
		 *
		p.x = XtWidth(f) != 0 ? 0 : XtX(child);
		p.y = XtHeight(f) != 0 ? 0 : XtY(child);
		 */
		p.x = XtX(child);
		p.y = XtY(child);
		p.width = XtWidth(child);
		p.height = XtHeight(child);
		p.border_width = XtBorderWidth(child);
		p.request_mode = CWWidth | CWHeight | CWBorderWidth;
	    }

	    if (FCP_Atta(con, LEFT).type == XmATTACH_SELF) {
		p.request_mode |= CWX;
	    } else {
		SETX(p.x - 0 * p.border_width);
	    }
	    if (FCP_Atta(con, TOP).type == XmATTACH_SELF) {
		p.request_mode |= CWY;
	    } else if (FCP_Atta(con, TOP).type == XmATTACH_WIDGET &&
		FCP_Atta(con, TOP).w && !XtIsManaged(FCP_Atta(con, TOP).w)) {
		/* Danny experimental bug #531123 */
		SETY(XtY(FCP_Atta(con, TOP).w));
		DEBUGOUT(_LtDebug2(__FILE__, f, child, "Yow(%s) %d\n",
			XtName(FCP_Atta(con, TOP).w),
			XtY(FCP_Atta(con, TOP).w)));
	    } else {
		SETY(p.y - 0 * p.border_width);
	    }

	    /* rws 21 Aug 1997
	       I took out the realized test to fix xdir->Connect->anon.
	       The down side is that it screws up mfm's display. Looking
	       through the Form cvs logs this test was put here specifically
	       for mfm.  mfm is having problems in this area with or without
	       the test... so I took it out for now.
	     */
	    if (FCP_PrefW(con) != XmINVALID_DIMENSION &&
	        cg && ((cg->request_mode & CWWidth) == CWWidth) &&
	        !Form_ProcessingConstraints(f))
	    {
		SETW(FCP_PrefW(con) + 2 * XtBorderWidth(child));
	    }
	    else
	    {
		SETW(p.width + 2 * p.border_width);
		/* rws 15 Dec 1999
		mainw/test16
		*/
	        if (FCP_PrefW(con) == XmINVALID_DIMENSION || (!Form_ProcessingConstraints(f) &&
	            cg && ((cg->request_mode & CWWidth) == CWWidth)))
	        {
		    if (p.width > 0)
		    {
			FCP_PrefW(con) = p.width;
		    }
		}
	    }

	    if (FCP_PrefH(con) != XmINVALID_DIMENSION &&
	        cg && ((cg->request_mode & CWHeight) == CWHeight) &&
	        !Form_ProcessingConstraints(f))
	    {
		SETH(FCP_PrefH(con) + 2 * XtBorderWidth(child));
	    }
	    else
	    {
		SETH(p.height + 2 * p.border_width);
		/* rws 15 Dec 1999
		mainw/test16
		*/
	        if (FCP_PrefH(con) == XmINVALID_DIMENSION || (!Form_ProcessingConstraints(f) &&
	            cg && ((cg->request_mode & CWHeight) == CWHeight)))
	        {
		    if (p.height > 0)
		    {
			FCP_PrefH(con) = p.height;
		    }
		}
	    }
	}
	else
	{
	    /*
	    FCP_PrefW(con) = XmINVALID_DIMENSION;
	    FCP_PrefH(con) = XmINVALID_DIMENSION;
	    */
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, (Widget)f, "_XmFormFindPreferred() - end\n"));
}

/*
 * print information about the constraints
 */
#define	PRINTIT(x)							\
    if (FCP_Atta(con, x).type == XmATTACH_WIDGET ||			\
	FCP_Atta(con, x).type == XmATTACH_OPPOSITE_WIDGET)		\
    {									\
	if (FCP_Atta(con, x).w == NULL)					\
	{								\
	    DEBUGOUT(_LtDebug0(__FILE__, (Widget)f,  "%s(%s)\t",	\
			       _LtDebugAttachment2String(FCP_Atta(con, x).type),	\
			       "(null)"));				\
	}								\
	else								\
	{								\
	    DEBUGOUT(_LtDebug0(__FILE__, (Widget)f,  "%s(%s,%d)\t",	\
			       _LtDebugAttachment2String(FCP_Atta(con, x).type),	\
			       XtName(FCP_Atta(con, x).w),		\
			       FCP_Atta(con, x).offset));		\
	}								\
    }									\
    else if (FCP_Atta(con, x).type == XmATTACH_POSITION)		\
    {									\
	DEBUGOUT(_LtDebug0(__FILE__, (Widget)f, "%s(%d/%d,%d)\t",	\
		 _LtDebugAttachment2String(FCP_Atta(con, x).type),		\
		 FCP_Atta(con, x).percent, Form_FractionBase(f),		\
		 FCP_Atta(con, x).offset));				\
    }									\
    else								\
    {									\
	DEBUGOUT(_LtDebug0(__FILE__, (Widget)f,  "%s(%d)\t",		\
		 _LtDebugAttachment2String(FCP_Atta(con, x).type),		\
		 FCP_Atta(con, x).offset));				\
    }


extern void
_XmFormPrintAttachmentReport(Widget f)
{
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
		      "Attachment Report : (Top,Bottom,Left,Right)\n"));

    for (i = 0; i < MGR_NumChildren(f); i++)
    {
	Widget child = MGR_Children(f)[i];
	XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

	DEBUGOUT(_LtDebug0(__FILE__, (Widget)f,
			   "child %s\t\t", XtName(child)));

	PRINTIT(TOP);
	PRINTIT(BOTTOM);
	DEBUGOUT(_LtDebug0(__FILE__, (Widget)f, "\n\t\t\t\t"));
	PRINTIT(LEFT);
	PRINTIT(RIGHT);

	DEBUGOUT(_LtDebug0(__FILE__, (Widget)f, "\n"));
    }
}
#undef	PRINTIT


/*
 * XmFormPath
 *
 * Try to find children whose ATTACH_WIDGET properties give you a path from
 * one side of the Form to the other.
 *
 * Return the size of the Form in that direction.
 */
static int
XmFormPath(Widget f, Widget c, int dir, XmFormAttachment atta)
{
	XmFormConstraints con;
	int x;

	if (c == NULL || !XtIsManaged(c)) {
		DEBUGOUT(_LtDebug2(__FILE__, f, c, "XmFormPath -> 0\n"));
		return 0;
	}

	con = (XmFormConstraints)CoreConstraints(c);

	atta[dir].type = FCP_Atta(con, dir).type;
	atta[dir].percent = FCP_Atta(con, dir).percent;
	atta[dir].offset = _XmGetOffset(c, dir);
	atta[dir].w = c;
	x = 0;
	if (dir == TOP || dir == BOTTOM)
	{
	    x = FCP_H(con);
	    atta[dir].tempValue = FCP_Y(con);
	}
	else if (dir == LEFT || dir == RIGHT)
	{
	    x = FCP_W(con);
	    atta[dir].tempValue = FCP_X(con);
	}

	switch (FCP_Atta(con, dir).type)
	{
	case XmATTACH_FORM:
	    if (dir == TOP || dir == BOTTOM)
	    {
		x += BB_MarginHeight(f);
		x += _XmGetOffset(c, dir);
		atta[dir].tempValue -= BB_MarginHeight(f);
		atta[dir].tempValue -= _XmGetOffset(c, dir);
	    }
	    else if (dir == LEFT || dir == RIGHT)
	    {
		x += BB_MarginWidth(f);
		x += _XmGetOffset(c, dir);
		atta[dir].tempValue -= BB_MarginWidth(f);
		atta[dir].tempValue -= _XmGetOffset(c, dir);
	    }
	    break;
	case XmATTACH_POSITION:
	    if (dir == TOP || dir == BOTTOM)
	    {
	    	x += Form_VertSpacing(f);
	    }
	    else if (dir == LEFT || dir == RIGHT)
	    {
	    	x += Form_HorizSpacing(f);
	    }
	    x += _XmGetOffset(c, dir);
	    break;
	case XmATTACH_WIDGET:
	    x += _XmGetOffset(c, dir);
	    atta[dir].tempValue -= _XmGetOffset(c, dir);
	    break;
	case XmATTACH_OPPOSITE_WIDGET:
	    if (FCP_Atta(con, dir).type == FCP_Atta(con, OPPOSITE_DIRECTION(dir)).type)
	    {
		/* 48 */
		XmFormConstraints con1;

		con1 = (XmFormConstraints)CoreConstraints(FCP_Atta(con, dir).w);

		if (dir == TOP || dir == BOTTOM)
		{
		    x = FCP_H(con1);
		    FCP_H(con) = x;
		}
		else
		{
		    /* 51 */
		    x = FCP_W(con1);
		    FCP_W(con) = x;
		}
	    } else {
		XmFormConstraints con1;

		con1 = (XmFormConstraints)CoreConstraints(FCP_Atta(con, dir).w);

		atta[dir].type = FCP_Atta(con1, dir).type;
		atta[dir].percent = FCP_Atta(con1, dir).percent;
		atta[dir].offset = _XmGetOffset(FCP_Atta(con, dir).w, dir);
		atta[dir].w = FCP_Atta(con, dir).w;
		x += _XmGetOffset(c, dir);
		atta[dir].tempValue = 0;
		if (FCP_Atta(con1, dir).type != XmATTACH_NONE)
		{
		    x += _XmGetOffset(FCP_Atta(con, dir).w, dir);
		    if (FCP_Atta(con1, dir).type == XmATTACH_WIDGET && FCP_Atta(con1, dir).w)
		    {
			x += XmFormPath(f, FCP_Atta(con1, dir).w, dir, atta);
		    }
		}
	    }
	    break;
	}

	if (FCP_Atta(con, dir).type == XmATTACH_WIDGET && FCP_Atta(con, dir).w)
	{
	    x += XmFormPath(f, FCP_Atta(con, dir).w, dir, atta);
	}

	DEBUGOUT(_LtDebug2(__FILE__, f, c, "XmFormPath-> %d\n", x));
	return x;
}


static void
_XmFormAllPaths(Widget f, Dimension *ww, Dimension *hh)
{
    int y;
    Cardinal i;
    XmFormAttachmentRec atta[4];

    *hh = 0;
    for (i = 0; i < MGR_NumChildren(f); i++)
    {
	Widget child = MGR_Children(f)[i];
	XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

	if (XtIsManaged(child))
	{
	int z;

	    atta[TOP].tempValue = 0;
	    atta[BOTTOM].tempValue = 0;
	    y = XmFormPath(f, MGR_Children(f)[i], TOP, atta);
	    z = XmFormPath(f, MGR_Children(f)[i], BOTTOM, atta);

		DEBUGOUT(_LtDebug2(__FILE__, f, child,
			"_XmFormAllPaths %d + %d - %d -> %d\n",
			y, z, FCP_H(con), y+z-FCP_H(con)));
	    y += z;
	    y -= FCP_H(con);

	    if (atta[TOP].type == XmATTACH_POSITION &&
		atta[BOTTOM].type == XmATTACH_POSITION)
	    {
		/* test 2 test30 nedit */
		if (atta[BOTTOM].percent != atta[TOP].percent) {
			y = (y * Form_FractionBase(f) / (atta[BOTTOM].percent -
						 atta[TOP].percent));
	DEBUGOUT(_LtDebug(__FILE__, f, "_XmFormAllPaths %d y = %d\n", __LINE__, y));
		}
		atta[TOP].tempValue = 0;
	    }
	    else if (atta[BOTTOM].type == XmATTACH_POSITION)
	    {
	    int y1;

		/* test18 test19 test27 */
		/* Hack divide by 0 */
		if (atta[BOTTOM].percent == 0) {
			y1 = (int)(((10 * (y) * Form_FractionBase(f)) + 5) / 10);
	DEBUGOUT(_LtDebug(__FILE__, f, "_XmFormAllPaths %d y = %d\n", __LINE__, y1));
		} else {
			y1 = (int)(((10 * (y) * Form_FractionBase(f) / atta[BOTTOM].percent) + 5) / 10);
	DEBUGOUT(_LtDebug(__FILE__, f, "_XmFormAllPaths %d y = %d\n", __LINE__, y1));
		}
		/* Hack divide by 0 */
		if (Form_FractionBase(f) == 0) {
			atta[TOP].tempValue = 100;	/* Does this make sense ? */
		} else {
			atta[TOP].tempValue = -y + (y1 * atta[BOTTOM].percent) / Form_FractionBase(f);
		}
		y = y1;
	    }
	    else if (atta[TOP].type == XmATTACH_POSITION)
	    {
		/* test28 test29 */
		/* Hack divide by 0 */
		if (Form_FractionBase(f) != atta[TOP].percent) {
			y = (int)(((10 * (y) * Form_FractionBase(f) / (Form_FractionBase(f) - atta[TOP].percent)) + 5) / 10);
		}
		/* Hack divide by 0 */
		if (Form_FractionBase(f) != 0) {
			atta[TOP].tempValue = (y * atta[TOP].percent) / Form_FractionBase(f);
		}
	    }
	    else if (atta[TOP].type == XmATTACH_WIDGET)
	    {
	    	/* FIX ME */
	    }
	    else if (atta[TOP].type == XmATTACH_OPPOSITE_WIDGET)
	    {
	    	/* FIX ME */
	    }
	    else if (atta[TOP].type == XmATTACH_FORM)
	    {
	    	/* FIX ME */
	    }
	    else
	    {
		y += atta[TOP].tempValue;
	DEBUGOUT(_LtDebug(__FILE__, f, "_XmFormAllPaths %d y = %d\n", __LINE__, y));
	    }
	    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
			       "_XmFormAllPaths H %5i %i %i\n", y, atta[TOP].tempValue, atta[BOTTOM].tempValue));
	    if (y > *hh)
	    {
		*hh = y;
	    }
	}
    }

    *ww = 0;
    for (i = 0; i < MGR_NumChildren(f); i++)
    {
	Widget child = MGR_Children(f)[i];
	XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

	if (XtIsManaged(child))
	{
	int z;

	    atta[LEFT].tempValue = 0;
	    atta[RIGHT].tempValue = 0;
	    y = XmFormPath(f, MGR_Children(f)[i], LEFT, atta);
	    z = XmFormPath(f, MGR_Children(f)[i], RIGHT, atta);
	    /*
	    printf("%s %i %i %i\n", XtName(child), y, z, FCP_W(con));
	    */
	    y += z;
	    y -= FCP_W(con);

	    if (atta[LEFT].type == XmATTACH_POSITION &&
		atta[RIGHT].type == XmATTACH_POSITION)
	    {
		/* Hack divide by 0 */
		if (atta[RIGHT].percent != atta[LEFT].percent)
			y = (y * Form_FractionBase(f) / (atta[RIGHT].percent - atta[LEFT].percent));
		atta[LEFT].tempValue = 0;
	    }
	    else if (atta[RIGHT].type == XmATTACH_POSITION)
	    {
	    int y1;

		/* Hack divide by 0 */
		if (atta[RIGHT].percent != 0)
			y1 = ((10 * (y) * Form_FractionBase(f) / atta[RIGHT].percent) + 5) / 10;
		else
			y1 = y;
		/* Hack divide by 0 */
		if (Form_FractionBase(f) != 0)
			atta[LEFT].tempValue = -y + (y1 * atta[RIGHT].percent) / Form_FractionBase(f);
		else
			atta[LEFT].tempValue = y;
		y = y1;
	    }
	    else if (atta[LEFT].type == XmATTACH_POSITION)
	    {
		/* test13 test31 mgdiff */
		/* Hack divide by 0 */
		if (Form_FractionBase(f) != atta[LEFT].percent)
			y = ((10 * (y) * Form_FractionBase(f) / (Form_FractionBase(f) - atta[LEFT].percent)) + 5) / 10;
		if (Form_FractionBase(f) != 0)
			atta[LEFT].tempValue = ((y * atta[LEFT].percent) / Form_FractionBase(f));
		else
			atta[LEFT].tempValue = y;
	    }
	    else
	    {
		y += atta[LEFT].tempValue;
	    }
	    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
			       "_XmFormAllPaths W %5i %i %i\n", y, atta[LEFT].tempValue, atta[RIGHT].tempValue));
	    if (y > *ww)
	    {
		*ww = y;
	    }
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
		      "_XmFormAllPaths returning %d %d\n", *ww, *hh));
}


/*
 * _XmFormLayout() - run the XmForm layout algorithm
 *
 * The MODE parameter is a bit set composed from the following bits :
 *      #define Mode_Normal     0x00    (no bits set)
 *      #define Mode_Test       0x01    (don't actually change anything)
 *      #define Mode_Resize     0x02    (can resize ourselves if == 0)
 *
 * If Mode_Test is set, then the cw and cg parameters must be valid, and the
 *      cw widget must be a managed child of form.
 *
 * Mode_Test is used by GeometryManager to see whether a geometry change is ok.
 *      In this case, a widget and its proposed geometry are passed.
 *      XmFormLayout will apply the changes temporarily, and after the layout
 *      algorithm it'll return the resulting geometry of that widget.
 *      GeometryManager will draw its own conclusions.
 *
 * Mode_Test can also be used with the form itself as widget being observed.
 *
 * ParentChangeMode (mode & Mode_Resize) :
 *      Allow XmFormLayout to change the form's geometry.
 *      Is set depending on the method from which this function is called.
 *      (Mode_Resize is set when called from the widget's resize method.)
 *
 * The FG parameter is used to indicate form geometry changes when called from
 *      GeometryManageer.
 *
 * For this routine, the fields x, y, w, h were added to the constraint record,
 *      in FormP.h.  Why ? In Mode_Test, the full algorithm must work (making
 *      changes to children widgets and their geometry); however nothing can
 *      be actually changed.  The working variables for XmFormLayout are in
 *      the constraint record.
 *
 * 31/7/1996 addition : There are circumstances in which the geometry of the
 *      CW widget should not be changed. Allowing all changes would create
 *      potential infinite loop situations. This needs to be added. Without
 *      this change, the use of e.g. editres to decrease the size of a widget
 *      can induce an infinite loop. (If we didn't have MAX_ITERATIONS, of
 *      course).
 *
 * FIX ME.
 *
 * 1/8/1996 addition : cw and cg should be taken into account even when not
 *    in test mode.
 */
static void
_XmFormLayout(Widget f, Widget cw, XtWidgetGeometry *cg,
	      Dimension *width, Dimension *height)
{
    int number_of_iterations = 0;
    Boolean changed;
    Cardinal i;
    Dimension fw, fh;		/* Form dimensions */
    Boolean WidthChangeMode = True, HeightChangeMode = True;
    Boolean ParentChangeMode = True;

#define	Wants(x)	(cg->request_mode & x)

    DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
		      "_XmFormLayout with width %d height %d\n",
		      XtWidth(f), XtHeight(f)));
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, cw,
		       "_XmFormLayout request %s\n",
		       _LtDebugWidgetGeometry2String(cg)));

    /*
     * Special case ... should this be a special case (maybe our code below
     * is wrong !)
     * What if there are no children ?
     */
    if (_XmGeoCount_kids((CompositeRec *)f) == 0)
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
			  "_XmFormLayout: Form has no managed kids!\n"));

	*width = XtWidth(f);
	*height = XtHeight(f);

	return;
    }
    /* End special case */

    fw = *width;
    fh = *height;

    /* Find all preferred sizes */
    _XmFormFindPreferred(f, cw, cg);

    if (fw > 1)
    {
	WidthChangeMode = False;
    }
    else
    {
	WidthChangeMode = True;
    }

    if (fh > 1)
    {
	HeightChangeMode = False;
    }
    else
    {
	HeightChangeMode = True;
    }

    if (!WidthChangeMode && !HeightChangeMode)
    {
	ParentChangeMode = False;
    }

    DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
		      "XmFormLayout\n\tWidthChangeMode  %s\n"
		      "\tHeightChangeMode %s\n\tParentChangeMode %s\n",
		      _LtDebugBoolean2String(WidthChangeMode),
		      _LtDebugBoolean2String(HeightChangeMode),
		      _LtDebugBoolean2String(ParentChangeMode)));

    for (i = 0; i < MGR_NumChildren(f); i++)
    {
	/* These two variables are needed to make the macros work ! */
	Widget child = MGR_Children(f)[i];
	XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

	if (!XtIsManaged(MGR_Children(f)[i]))
	{
	    continue;
	}

	/*
	 * INITIALIZE
	 */
	/*
	FCP_WidthFromSide(con) = False;
	FCP_HeightFromSide(con) = False;
	*/

#if 1
	/* rws 3 Nov 1998
	   form/test44 shows that this is _not_ the case, even though it was
	   meant to show something totally different!!
	   This also shows that form/test11 has been correct, for the wrong
	   reason.  Form needs to honor the x/y requests of its kids.
	 */
	/* rws 20 Nov 1998
	   However taking this out messes up the FileList dialog of Plan
	   Re-reading the man page says that this should be done!
	 */
	/* rws 20 Jan 1999
	   Maxwell shows that this forced attachment does not get done
	   unless the Form is realized.  The main document window will have
	   a statusBar that spans the entire height of the window if the
	   Realized check is not done.  However, this messes up Plan's
	   FileList dialog....aaaarrrrggggg
	 */
	/*
	 * Every child of a form needs to have a left or a right attachment.
	 * If they have neither, fix it based on XmNrubberPositioning.
	 */
	if (FCP_Atta(con, LEFT).type == XmATTACH_NONE &&
	    FCP_Atta(con, RIGHT).type == XmATTACH_NONE)
	{
	    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
			       "left & right were NONE\n"));

	    if (Form_RubberPos(f) == True)
	    {
		FCP_Atta(con, LEFT).type = XmATTACH_POSITION;
		FCP_Atta(con, LEFT).percent = XtX(child);
		/*
		FCP_Atta(con, LEFT).percent =
		    (FCP_Atta(con, LEFT).value / Form_FractionBase(f));
		    */

		DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
				   "Left set to ATTACH_POSITION %d\n",
				   FCP_Atta(con, LEFT).percent));
	    }
	    else
	    {
		FCP_Atta(con, LEFT).type = XmATTACH_FORM;
		/* rws 20 Nov 1998
		   plan FileList dialog needs the - BB stuff
		 */
		FCP_Atta(con, LEFT).offset = XtX(child) - BB_MarginWidth(f);

		DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
				   "Left set to ATTACH_FORM %d\n",
				   FCP_Atta(con, LEFT).offset));
	    }
	}

	/*
	 * Every child of a form needs to have a top or a bottom attachment.
	 * If they have neither, fix it based on XmNrubberPositioning.
	 */
	if (FCP_Atta(con, TOP).type == XmATTACH_NONE &&
	    FCP_Atta(con, BOTTOM).type == XmATTACH_NONE)
	{
	    DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
			       "top & bottom were NONE\n"));

	    if (Form_RubberPos(f) == True)
	    {
		FCP_Atta(con, TOP).type = XmATTACH_POSITION;
		FCP_Atta(con, TOP).percent = XtY(child);
		/*
		FCP_Atta(con, TOP).percent =
		    (FCP_Atta(con, TOP).value / Form_FractionBase(f));
		    */

		DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
				   "Top set to ATTACH_POSITION %d\n",
				   FCP_Atta(con, TOP).percent));
	    }
	    else
	    {
		FCP_Atta(con, TOP).type = XmATTACH_FORM;
		FCP_Atta(con, TOP).offset = XtY(child) - BB_MarginHeight(f);

		DEBUGOUT(_LtDebug2(__FILE__, (Widget)f, child,
				   "Top set to ATTACH_FORM %d\n",
				   FCP_Atta(con, TOP).offset));
	    }
	}
#endif
    }

#ifdef	PRINT_ATTACHMENT_REPORT
    _XmFormPrintAttachmentReport(f);
#endif

#ifdef PRINT_REPORT
    DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
		      "Initial geometry : %d %d\n", fw, fh));
    DEBUGOUT(_LtDebug0(__FILE__, (Widget)f, "Initial child geometry :\n"));

    for (i = 0; i < MGR_NumChildren(f); i++)
    {
	Widget child = MGR_Children(f)[i];
	XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

	DEBUGOUT(_LtDebug0(__FILE__, (Widget)f,
			   "Child #%d (%s)\tx %d y %d w %d h %d %s\n",
			   i, XtName(child),
			   FCP_X(con), FCP_Y(con),
			   FCP_W(con), FCP_H(con),
			   XtIsManaged(child) ? "" : "Not Managed"));
    }
#endif

    /*
     * In these two nested loops we repeatedly try to fix all geometries for
     * all children, and for the form itself.
     *
     * The outer loop runs until either nothing has changed, or MAX_ITERATIONS
     * times.
     *
     * The inner loop goes over all (managed) children.
     */
    do
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
			  "Layout iteration %d\n", number_of_iterations));

	changed = False;

	/*
	 * Tryout
	 */
	if (MGR_NumChildren(f) > 0 && ParentChangeMode)
	{
	    Dimension ww, hh;
	    Widget child = MGR_Children(f)[0];

	    /* rws 20 Apr 1997
	     * Unless an attachment changes this should never change, right??
	     * If so it should be moved to before the loops.
	     * rws 14 May 1997
	     * Not true! Some of the widgets in the attached chain may
	     * have their sizes changed due to the constraints, if this
	     * is the case we need this call.
	     */
	    _XmFormAllPaths(f, &ww, &hh);

	    if (number_of_iterations / 2 * 2 == number_of_iterations)
	    {
		if (WidthChangeMode && fw != ww)
		{
		    SETFW(ww);
		}
		if (HeightChangeMode && fh != hh)
		{
		    SETFH(hh);
		}
	    }
	    else
	    {
		changed = True;
	    }
	}
	/* End tryout */

	/*
	 * Inner loop
	 */
	for (i = 0; i < MGR_NumChildren(f); i++)
	{
	    Widget child = MGR_Children(f)[i];
	    XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

	    if (!XtIsManaged(child))
	    {
		continue;
	    }

	    /****************************************************************
	     *              A  T  T  A  C  H  M  E  N  T  S
	     *
	     * We see more or less the same code four times (TOP, BOTTOM,
	     * LEFT, RIGHT) here.
	     *
	     * Not sure whether it is at all possible to generalise the code
	     * such that we could write this in a shorter way though.
	     *
	     * 21 Apr 1997
	     * When the top/left side of a widget is move do not move the
	     * bottom/right
	     ****************************************************************/

	    /****************************************************************
	     *
	     *                        T    O    P
	     *
	     ****************************************************************/
	    switch (FCP_Atta(con, TOP).type)
	    {
		/* TOP */
	    case XmATTACH_FORM:
		/* 1 3 4 5 6 8 9 10 11 12 13 14 15 17 19 20 21 22 23 24 25 27 */
		if (FCP_Y(con) != _XmGetOffset(child, TOP) +
		    BB_MarginHeight(f))
		{
		    if (!HeightChangeMode && FCP_Atta(con, BOTTOM).type != XmATTACH_NONE)
		    {
			/* 1 */
			    SETH(FCP_Y(con) + FCP_H(con) -
				 (_XmGetOffset(child, TOP) +
				  BB_MarginHeight(f)));
		    }

		    SETY(_XmGetOffset(child, TOP) + BB_MarginHeight(f));
		}
		break;

		/* TOP */
	    case XmATTACH_OPPOSITE_FORM:
		/* This is in Ted, an easy Rich Text processor */

		if (FCP_Y(con) != fh + _XmGetOffset(child, TOP) -
		    BB_MarginHeight(f))
		{
		    if (!HeightChangeMode && FCP_Atta(con, BOTTOM).type != XmATTACH_NONE)
		    {
			    SETH(FCP_Y(con) + FCP_H(con) -
				 (fh + _XmGetOffset(child, TOP) -
				  BB_MarginHeight(f)));
		    }

		    SETY(fh + _XmGetOffset(child, TOP) - BB_MarginHeight(f));
		}
		break;

		/* TOP */
	    case XmATTACH_WIDGET:
		/* 11 12 13 14 17 19 23 25 36 37 4 6 */
		if (!(FCP_Atta(con, TOP).w))
		{
#ifdef	LESSTIF_VERBOSE
		    _XmWarning(child, "top: XmATTACH_WIDGET without "
			       "widget changed to XmATTACH_FORM");
#endif
		    FCP_Atta(con, TOP).type = XmATTACH_FORM;
		    changed = True;
		    break;
		}

		if (FCP_Y(con) != (AttachedY(FCP_Atta(con, TOP).w) +
				   AttachedHt(FCP_Atta(con, TOP).w) +
				   _XmGetOffset(child, TOP)))
		{
		    int yy = AttachedY(FCP_Atta(con, TOP).w) +
		    AttachedHt(FCP_Atta(con, TOP).w) +
		    _XmGetOffset(child, TOP);

		    if (!HeightChangeMode && FCP_Atta(con, BOTTOM).type != XmATTACH_NONE)
		    {
			/* 14 17 19 23 25 37 6 */
			    SETH(FCP_Y(con) + FCP_H(con) - (yy));
		    }

		    SETY(yy);
		}
		break;

		/* TOP */
	    case XmATTACH_OPPOSITE_WIDGET:
		/* 12 13 20 4 */
		if (!(FCP_Atta(con, TOP).w))
		{
#ifdef	LESSTIF_VERBOSE
		    _XmWarning(child, "top: XmATTACH_OPPOSITE_WIDGET "
			       "without widget changed to XmATTACH_FORM");
#endif
		    FCP_Atta(con, TOP).type = XmATTACH_FORM;
		    changed = True;
		    break;
		}

		if (FCP_Y(con) != (AttachedY(FCP_Atta(con, TOP).w) +
				   _XmGetOffset(child, TOP)))
		{
		    int yy = AttachedY(FCP_Atta(con, TOP).w) +
		    _XmGetOffset(child, TOP);

		    if (!HeightChangeMode && FCP_Atta(con, BOTTOM).type != XmATTACH_NONE)
		    {
			/* 12 13 */
			    SETH(FCP_Y(con) - (yy));
		    }
		    SETY(yy);
		}
		break;

		/* TOP */
	    case XmATTACH_POSITION:
		/* 2 28 29 30 7 */
		{
		    int pos = (((10 * FCP_Atta(con, TOP).percent * fh /
				 Form_FractionBase(f)) +
				(10 * _XmGetOffset(child, TOP))) + 5) / 10;

		    if (FCP_Y(con) != pos)
		    {
			DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
					  "Computed pos as %d\n", pos));

			if (!HeightChangeMode && FCP_Atta(con, BOTTOM).type != XmATTACH_NONE)
			{
			    /* 2 28 29 30 7 */
				SETH(FCP_Y(con) + FCP_H(con) - (pos));
			}

			SETY(pos);
		    }
		}
		break;

		/* TOP */
	    case XmATTACH_NONE:
		break;

		/* TOP */
	    case XmATTACH_SELF:
		/*
		 * o If a child's left (or right) attachment is  set  to
		 *   XmATTACH_SELF,  its  corresponding  left (or right)
		 *   offset is forced to  0.   The  attachment  is  then
		 *   changed  to XmATTACH_POSITION, with a position that
		 *   corresponds to x value  of  the  child's  left  (or
		 *   right)  edge.   To  fix the position of a side at a
		 *   specific  x  value  use  XmATTACH_FORM   or   XmAT-
		 *   TACH_OPPOSITE_FORM with the x value as the left (or
		 *   right) offset.
		 */
		/* Xinvest */
		SETY(XtY(child));
		FCP_Atta(con, TOP).percent = (FCP_Y(con) +
					    _XmGetOffset(child, TOP)) *
		    Form_FractionBase(f) / fh;
		FCP_Atta(con, TOP).type = XmATTACH_POSITION;
		FCP_Atta(con, TOP).offset = 0;
		changed = True;
		break;

		/* TOP */
	    default:
		_XmWarning(child, "Illegal top attachment");
	    }

	    /****************************************************************
	     *
	     *                  B   0   T   T   0   M
	     *
	     ****************************************************************/
	    switch (FCP_Atta(con, BOTTOM).type)
	    {
		/* BOTTOM */
	    case XmATTACH_FORM:
		/* 1 14 15 16 17 20 21 23 25 28 29 3 31 5 6 8 */
		if (FCP_Y(con) + FCP_H(con) != fh -
		    _XmGetOffset(child, BOTTOM) -
		    BB_MarginHeight(f))
		{
		    if (!HeightChangeMode && FCP_Atta(con, TOP).type != XmATTACH_NONE)
		    {
			/* 1 17 23 28 29 6 */
			SETH(fh - _XmGetOffset(child, BOTTOM) -
				 BB_MarginHeight(f) - FCP_Y(con));
		    }
		    if (!HeightChangeMode || FCP_Atta(con, TOP).type == XmATTACH_NONE)
		    {
			SETY(fh - _XmGetOffset(child, BOTTOM) -
			     BB_MarginHeight(f) - FCP_H(con));
		    }
		}
		break;

		/* BOTTOM */
	    case XmATTACH_OPPOSITE_FORM:
		/* lxb */

		if (FCP_H(con) + FCP_Y(con) != BB_MarginHeight(f) -
		    _XmGetOffset(child, BOTTOM))
		{
		    if (!HeightChangeMode && FCP_Atta(con, TOP).type != XmATTACH_NONE)
		    {
			    SETH(BB_MarginHeight(f) -
				 _XmGetOffset(child, BOTTOM) -
				 FCP_Y(con));
		    }
		    if (!HeightChangeMode || FCP_Atta(con, TOP).type == XmATTACH_NONE)
		    {

			SETY(BB_MarginHeight(f) -
			     _XmGetOffset(child, BOTTOM) -
			     FCP_H(con));
		    }
		}
		break;

		/* BOTTOM */
	    case XmATTACH_WIDGET:
		/* 14 15 25 30 8 */
		if (!(FCP_Atta(con, BOTTOM).w))
		{
#ifdef	LESSTIF_VERBOSE
		    _XmWarning(child, "bottom: XmATTACH_WIDGET without "
			       "widget changed to XmATTACH_FORM");
#endif

		    FCP_Atta(con, BOTTOM).type = XmATTACH_FORM;
		    changed = True;
		    break;
		}

		if (FCP_Y(con) + FCP_H(con) !=
		    AttachedY(FCP_Atta(con, BOTTOM).w) -
		    _XmGetOffset(child, BOTTOM))
		{
		    if (!HeightChangeMode && FCP_Atta(con, TOP).type != XmATTACH_NONE)
		    {
			/* 14 15 25 30 8 */
			    SETH(AttachedY(FCP_Atta(con, BOTTOM).w) -
				 _XmGetOffset(child, BOTTOM) - FCP_Y(con));
		    }
		    if (!HeightChangeMode || FCP_Atta(con, TOP).type == XmATTACH_NONE)
		    {
		    SETY(AttachedY(FCP_Atta(con, BOTTOM).w) -
			 _XmGetOffset(child, BOTTOM) - FCP_H(con));
		    }
		}
		break;

		/* BOTTOM */
	    case XmATTACH_POSITION:
		/* 18 19 2 27 30 7 */
		{
		    int pos = (((10 * FCP_Atta(con, BOTTOM).percent * fh /
				 Form_FractionBase(f)) -
				(10 * _XmGetOffset(child, BOTTOM))) + 5) / 10;

		    if (FCP_Y(con) + FCP_H(con) != pos)
		    {
			/* 19 2 30 */
			if (!HeightChangeMode && FCP_Atta(con, TOP).type != XmATTACH_NONE)
			{
			    /* 19 2 */
				SETH(pos - FCP_Y(con));
			}
			if (!HeightChangeMode || FCP_Atta(con, TOP).type == XmATTACH_NONE)
			{
			    SETY(pos - FCP_H(con));
			}
		    }
		}
		break;

		/* BOTTOM */
	    case XmATTACH_NONE:
		/*
		if (!HeightChangeMode && FCP_Y(con) + FCP_H(con) > fh)
		{
		    SETH(fh - FCP_Y(con));
		}
		*/
		break;

		/* BOTTOM */
	    case XmATTACH_OPPOSITE_WIDGET:
		/* 12 13 21 8 */
		if (!(FCP_Atta(con, BOTTOM).w))
		{
#ifdef	LESSTIF_VERBOSE
		    _XmWarning(child, "bottom: XmATTACH_OPPOSITE_WIDGET "
			       "without widget changed to XmATTACH_FORM");
#endif

		    FCP_Atta(con, BOTTOM).type = XmATTACH_FORM;
		    changed = True;
		    break;
		}

		if (FCP_Y(con) + FCP_H(con) !=
		    (AttachedY(FCP_Atta(con, BOTTOM).w) +
		     AttachedHt(FCP_Atta(con, BOTTOM).w) -
		     _XmGetOffset(child, BOTTOM)))
		{
		    /* 12 13 21 8 */
		    if (!HeightChangeMode && FCP_Atta(con, TOP).type != XmATTACH_NONE)
		    {
			/* 12 13 8 */
			    SETH(AttachedY(FCP_Atta(con, BOTTOM).w) +
				 AttachedHt(FCP_Atta(con, BOTTOM).w) -
				 FCP_Y(con) - _XmGetOffset(child, BOTTOM));
		    }
		    if (!HeightChangeMode || FCP_Atta(con, TOP).type == XmATTACH_NONE)
		    {
			SETY(AttachedY(FCP_Atta(con, BOTTOM).w) +
			     AttachedHt(FCP_Atta(con, BOTTOM).w) -
			     FCP_H(con) - _XmGetOffset(child, BOTTOM));
		    }
		}
		break;

		/* BOTTOM */
	    case XmATTACH_SELF:
#if 0
		/* Meditres has this */
		_XmWarning(child, "bottom ATTACH_SELF %s %d"
			   " no test coverage", __FILE__, __LINE__);
#endif
		SETH(XtHeight(child));
		FCP_Atta(con, BOTTOM).percent = (FCP_Y(con) + FCP_H(con) +
					       _XmGetOffset(child, BOTTOM)) *
		    Form_FractionBase(f) / fh;

		FCP_Atta(con, BOTTOM).type = XmATTACH_POSITION;
		FCP_Atta(con, BOTTOM).offset = 0;
		changed = True;
		break;

		/* Now for the illegal cases */
	    default:
		_XmWarning(child, "Illegal bottom attachment");
	    }

	    /****************************************************************
	     *
	     *                      L    E    F    T
	     *
	     ****************************************************************/
	    switch (FCP_Atta(con, LEFT).type)
	    {
		/* LEFT */
	    case XmATTACH_FORM:
		/* 1 10 11 12 13 14 15 16 17 18 19 2 20 21 22 23 24 25 27 28 29 3 4 6 8 9 */
		if (FCP_X(con) != _XmGetOffset(child, LEFT) +
		    BB_MarginWidth(f))
		{
		    if (!WidthChangeMode && FCP_Atta(con, RIGHT).type != XmATTACH_NONE)
		    {
			/* 12 13 2 4 */
			SETW(FCP_X(con) + FCP_W(con) -
				 (_XmGetOffset(child, LEFT) +
				  BB_MarginWidth(f)));
		    }

		    SETX(_XmGetOffset(child, LEFT) + BB_MarginWidth(f));
		}
		break;

		/* LEFT */
	    case XmATTACH_POSITION:
		/* 13 31 7 8 */
		{
			/* Roundoff stuff complicates this,
			 * it's really :
			 * pos = percent * fw / ffb + offset
			 */
		    int pos = (((10 * FCP_Atta(con, LEFT).percent * fw /
				 Form_FractionBase(f)) +
				(10 * _XmGetOffset(child, LEFT))) + 5) / 10;

		    if (FCP_X(con) != pos)
		    {
			if (!WidthChangeMode && FCP_Atta(con, RIGHT).type != XmATTACH_NONE)
			{
			    /* 13 7 8 */
				SETW(FCP_X(con) + FCP_W(con) - (pos));
			}

			SETX(pos);
		    }
		}
		break;

		/* LEFT */
	    case XmATTACH_WIDGET:
		/* 1 15 20 21 22 4 6 8 */
		if (!(FCP_Atta(con, LEFT).w))
		{
#ifdef	LESSTIF_VERBOSE
		    _XmWarning(child, "left: XmATTACH_WIDGET without "
			       "widget changed to XmATTACH_FORM");
#endif

		    FCP_Atta(con, LEFT).type = XmATTACH_FORM;
		    changed = True;
		    break;
		}
		if (FCP_X(con) != (AttachedX(FCP_Atta(con, LEFT).w) +
				   AttachedWd(FCP_Atta(con, LEFT).w) +
				   _XmGetOffset(child, LEFT)))
		{
		    int xx = AttachedX(FCP_Atta(con, LEFT).w) +
		    AttachedWd(FCP_Atta(con, LEFT).w) +
		    _XmGetOffset(child, LEFT);

		    if (!WidthChangeMode && FCP_Atta(con, RIGHT).type != XmATTACH_NONE)
		    {
			/* 1 15 22 4 6 8 */
			    SETW(FCP_X(con) + FCP_W(con) - (xx));
		    }

		    SETX(xx);
		}
		break;

		/* LEFT */
	    case XmATTACH_OPPOSITE_WIDGET:
		/* 4 */
		if (!(FCP_Atta(con, LEFT).w))
		{
#ifdef	LESSTIF_VERBOSE
		    _XmWarning(child, "left: XmATTACH_OPPOSITE_WIDGET "
			       "without widget changed to XmATTACH_FORM");
#endif

		    FCP_Atta(con, LEFT).type = XmATTACH_FORM;
		    changed = True;
		    break;
		}
		if (FCP_X(con) != AttachedX(FCP_Atta(con, LEFT).w) +
		    _XmGetOffset(child, LEFT))
		{
		    int xx = AttachedX(FCP_Atta(con, LEFT).w) +
		    _XmGetOffset(child, LEFT);

		    if (!WidthChangeMode && FCP_Atta(con, RIGHT).type != XmATTACH_NONE)
		    {
			/* 4 */
			    SETW(FCP_X(con) + FCP_W(con) - (xx));
		    }

		    SETX(xx);
		}
		break;

		/* LEFT */
	    case XmATTACH_NONE:
		break;

		/* LEFT */
	    case XmATTACH_OPPOSITE_FORM:
		/* mfm */
		if (FCP_X(con) != fw + _XmGetOffset(child, LEFT) -
		    BB_MarginWidth(f))
		{
		    if (!WidthChangeMode && FCP_Atta(con, RIGHT).type != XmATTACH_NONE)
		    {
			    SETW(FCP_X(con) + FCP_W(con) -
				 (fw + _XmGetOffset(child, LEFT) -
				  BB_MarginWidth(f)));
		    }

		    SETX(fw + _XmGetOffset(child, LEFT) - BB_MarginWidth(f));
		}
		break;

		/* LEFT */
	    case XmATTACH_SELF:
		/* 60, Xinvest */
		SETX(XtX(child));
		changed = True;
		FCP_Atta(con, LEFT).percent = Form_FractionBase(f)
			* (FCP_X(con) + _XmGetOffset(child, LEFT))
			/ fw;
		DEBUGOUT(_LtDebug2(__FILE__, f, child,
			"LEFT SELF Yow %d  * (x %d + %d) / %d => %d\n",
			Form_FractionBase(f),
			FCP_X(con), _XmGetOffset(child, LEFT),
			fw,
			FCP_Atta(con, LEFT).percent));
		/* We're using this value, so reset it only at the end. */
		FCP_Atta(con, LEFT).offset = 0;
		FCP_Atta(con, LEFT).type = XmATTACH_POSITION;
		break;

		/* Now for the illegal cases */
	    default:
		_XmWarning(child, "Illegal left attachment");
	    }

	    /****************************************************************
	     *
	     *                   R    I    G    H    T
	     *
	     ****************************************************************/
	    switch (FCP_Atta(con, RIGHT).type)
	    {
		/* RIGHT */
	    case XmATTACH_FORM:	/* GOOD EXAMPLE */
		/* 1 10 11 12 13 14 15 2 22 25 3 4 5 6 8 9 */
		if (FCP_X(con) + FCP_W(con) != fw -
		    _XmGetOffset(child, RIGHT) -
		    BB_MarginWidth(f))
		{
		    if (!WidthChangeMode && FCP_Atta(con, LEFT).type != XmATTACH_NONE)
		    {
			/* 1 12 13 15 2 22 25 4 6 8 */
			SETW(fw - _XmGetOffset(child, RIGHT) -
				 BB_MarginWidth(f) - FCP_X(con));
		    }

		    if (!WidthChangeMode || FCP_Atta(con, LEFT).type == XmATTACH_NONE)
		    {
			SETX(fw - _XmGetOffset(child, RIGHT) -
			     BB_MarginWidth(f) - FCP_W(con));
		    }
		}
		break;

		/* RIGHT */
	    case XmATTACH_WIDGET:
		if (!(FCP_Atta(con, RIGHT).w))
		{
#ifdef	LESSTIF_VERBOSE
		    _XmWarning(child, "right: XmATTACH_WIDGET without "
			       "widget changed to XmATTACH_FORM");
#endif

		    FCP_Atta(con, RIGHT).type = XmATTACH_FORM;
		    changed = True;
		    break;
		}
		if (FCP_X(con) + FCP_W(con) !=
		    AttachedX(FCP_Atta(con, RIGHT).w) -
		    _XmGetOffset(child, RIGHT))
		{
		    if (!WidthChangeMode && FCP_Atta(con, LEFT).type != XmATTACH_NONE)
		    {
			    SETW(AttachedX(FCP_Atta(con, RIGHT).w) -
				 _XmGetOffset(child, RIGHT) - FCP_X(con));
		    }
		    if (!WidthChangeMode || FCP_Atta(con, LEFT).type == XmATTACH_NONE)
		    {
			SETX(AttachedX(FCP_Atta(con, RIGHT).w) -
			     _XmGetOffset(child, RIGHT) - FCP_W(con));
		    }
		}
		break;

		/* RIGHT */
	    case XmATTACH_OPPOSITE_WIDGET:
		/* 4 */
		if (!(FCP_Atta(con, RIGHT).w))
		{
#ifdef	LESSTIF_VERBOSE
		    _XmWarning(child, "right : XmATTACH_OPPOSITE_WIDGET "
			       "without widget changed to XmATTACH_FORM");
#endif

		    FCP_Atta(con, RIGHT).type = XmATTACH_FORM;
		    changed = True;
		    break;
		}

		if (FCP_X(con) + FCP_W(con) !=
		    (AttachedX(FCP_Atta(con, RIGHT).w) +
		     AttachedWd(FCP_Atta(con, RIGHT).w) -
		     _XmGetOffset(child, RIGHT)))
		{
		    if (!WidthChangeMode && FCP_Atta(con, LEFT).type != XmATTACH_NONE)
		    {
			/* 4 */
			    SETW(AttachedX(FCP_Atta(con, RIGHT).w) +
				 AttachedWd(FCP_Atta(con, RIGHT).w) -
				 FCP_X(con) -
				 _XmGetOffset(child, RIGHT));
		    }
		    if (!WidthChangeMode || FCP_Atta(con, LEFT).type == XmATTACH_NONE)
		    {
			SETX(AttachedX(FCP_Atta(con, RIGHT).w) +
			     AttachedWd(FCP_Atta(con, RIGHT).w) -
			     FCP_W(con) - _XmGetOffset(child, RIGHT));
		    }
		}
		break;

		/* RIGHT */
	    case XmATTACH_POSITION:
		/* 6 7 8 */
		{
		    int pos = (((10 * FCP_Atta(con, RIGHT).percent * fw /
				 Form_FractionBase(f)) -
				(10 * _XmGetOffset(child, RIGHT))) + 5) / 10;

		    if (FCP_X(con) + FCP_W(con) != pos)
		    {
			if (!WidthChangeMode && FCP_Atta(con, LEFT).type != XmATTACH_NONE)
			{
			    /* 6 7 */
				SETW(pos - FCP_X(con));
			}
			if (!WidthChangeMode || FCP_Atta(con, LEFT).type == XmATTACH_NONE)
			{
			    SETX(pos - FCP_W(con));
			}
		    }
		}
		break;

		/* RIGHT */
	    case XmATTACH_NONE:
		/*
		if (!WidthChangeMode && FCP_X(con) + FCP_W(con) > fw)
		{
		    SETW(fw - FCP_X(con));
		}
		*/
		break;

		/* RIGHT */
	    case XmATTACH_OPPOSITE_FORM:
		/* mfm */

		if (FCP_W(con) + FCP_X(con) != BB_MarginWidth(f) -
		    _XmGetOffset(child, RIGHT))
		{
		    if (!WidthChangeMode && FCP_Atta(con, LEFT).type != XmATTACH_NONE)
		    {
			    SETW(BB_MarginWidth(f) - _XmGetOffset(child, RIGHT) -
				 FCP_X(con));
		    }
		    if (!WidthChangeMode || FCP_Atta(con, LEFT).type == XmATTACH_NONE)
		    {
		    SETX(BB_MarginWidth(f) - _XmGetOffset(child, RIGHT) -
			 FCP_W(con));
		    }
		}
		break;

		/* RIGHT */
	    case XmATTACH_SELF:
		SETW(XtWidth(child));
		FCP_Atta(con, RIGHT).percent = Form_FractionBase(f)
			* (FCP_X(con) + FCP_W(con) + _XmGetOffset(child, RIGHT))
			/ fw;
		changed = True;
		DEBUGOUT(_LtDebug2(__FILE__, f, child,
			"RIGHT SELF Yow ffb %d * (x %d + w %d + %d) / %d => %d\n",
			Form_FractionBase(f),
			FCP_X(con), FCP_W(con),
			_XmGetOffset(child, RIGHT), fw,
			FCP_Atta(con, RIGHT).percent));
		/* We're using this value so only reset it at the end */
		FCP_Atta(con, RIGHT).offset = 0;
		FCP_Atta(con, RIGHT).type = XmATTACH_POSITION;
		break;

		/* Now for the illegal cases */
	    default:
		_XmWarning(child, "Illegal right attachment\n");
	    }

	    /*
	     * End of the inner loop (all children)
	     */
	}

	number_of_iterations++;

	/*
	 * End of outer loop (iterations)
	 */
    }
    while (changed && number_of_iterations < MAX_ITERATIONS);

    /*
     * After loops ...
     *
     * Print a warning if necessary.
     *
     * Then depending on our parameters either copy back information,
     * or try to modify the widgets involved.
     */
    if (number_of_iterations == MAX_ITERATIONS)
    {
	_XmWarning((Widget)f,
		   "Layout algorithm bailing out after %d iterations",
		   MAX_ITERATIONS);
	DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
			  "Bailing out after %d iterations",
			  MAX_ITERATIONS));
    }

    /*
     * handy/dandy debug data.
     */
#ifdef	PRINT_REPORT
    DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
		      "XmFormLayout results : form size %d %d\n", fw, fh));
    for (i = 0; i < MGR_NumChildren(f); i++)
    {
	Widget child = MGR_Children(f)[i];
	XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

	DEBUGOUT(_LtDebug0(__FILE__, (Widget)f,
			   "\tChild #%d (%s)\tx %d y %d w %d h %d %s\n",
			   i, XtName(child), FCP_X(con),
			   FCP_Y(con), FCP_W(con), FCP_H(con),
			   XtIsManaged(child) ? "" : "Not Managed"));
    }
#endif

    /*
     * Report Form geometry
     */
    *width = fw;
    *height = fh;

    /*
     * If in test mode for a child widget, copy back the answer
     */
    /* not form? it's a child */
    if (cw && cg)
    {
	XmFormConstraints con =
	(XmFormConstraints)CoreConstraints(cw);

	cg->x = FCP_X(con);
	cg->y = FCP_Y(con);
	cg->width = FCP_W(con);
	cg->height = FCP_H(con);
	cg->width -= 2 * XtBorderWidth(cw);
	cg->height -= 2 * XtBorderWidth(cw);
	cg->request_mode = CWX | CWY | CWWidth | CWHeight;
    }
}


static XtGeometryResult
_XmFormGeomRequest(Widget f, Dimension *width, Dimension *height)
{
    XtWidgetGeometry request;
    XtGeometryResult result;

    /* Myself */
    request.request_mode = CWWidth | CWHeight | CWBorderWidth;
    request.width = *width;
    request.height = *height;
    request.border_width = XtBorderWidth(f);

    DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
		      "Form is making a GeometryRequest to %s!!!!!!!!!\n",
		      XtName(XtParent(f))));

    result = XtGeometryNo;

    if (BB_ResizePolicy(f) == XmRESIZE_NONE &&
	XtWidth(f) != 0 && XtHeight(f) != 0)
    {
	return result;
    }
    else if (BB_ResizePolicy(f) == XmRESIZE_GROW &&
	     XtWidth(f) != 0 && XtHeight(f) != 0)
    {
	if (*width < XtWidth(f) || *height < XtHeight(f))
	{
	    return result;
	}
    }

    /* Breaks XmBDFed otherwise */
    if (request.width != 0 && request.height != 0)
    {
	result = _XmMakeGeometryRequest((Widget)f, &request);

	DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
			  "_XmMakeGeometryRequest (w %d h %d) => %s\n",
			  request.width, request.height,
			  _LtDebugGeometryResult2String(result)));
	DEBUGOUT(_LtDebug("GM", (Widget)f,
			  "_XmMakeGeometryRequest (w %d h %d) => %s\n",
			  request.width, request.height,
			  _LtDebugGeometryResult2String(result)));

	if (result == XtGeometryYes)
	{
	    *width = request.width;
	    *height = request.height;

	    DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
			      "Got Yes for _XmMakeGeometryRequest: %d %d\n",
			      *width, *height));
	    DEBUGOUT(_LtDebug("GM", (Widget)f,
			      "Got Yes for _XmMakeGeometryRequest: %d %d\n",
			      *width, *height));
	}
	else
	{
	    *width = XtWidth(f);
	    *height = XtHeight(f);
	}
    }
    else
    {
	*width = XtWidth(f);
	*height = XtHeight(f);
    }

    return result;
}


static void
_XmFormConfigureChildren(Widget f, Widget cw, XtWidgetGeometry *cg)
{
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
		      "_XmFormConfigureChildren:  Child is %s\n",
		      cw ? XtName(cw) : "(null)"));

    /* Children first */
    for (i = 0; i < MGR_NumChildren(f); i++)
    {
	Widget child = MGR_Children(f)[i];
	XmFormConstraints con = (XmFormConstraints)CoreConstraints(child);

	if (!XtIsManaged(child))
	    continue;
	if (cw && cw == child)
	{
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)f,
			      "_XmFormConfigureChildren:  Child is %s\n",
			      cw ? XtName(cw) : "(null)"));

	    XtX(child) = FCP_X(con);
	    XtY(child) = FCP_Y(con);
	    XtWidth(child) = (FCP_W(con) -
			      (2 * child->core.border_width)) < 1
		? 1
		: FCP_W(con) -
		(2 * child->core.border_width);
	    XtHeight(child) = (FCP_H(con) -
			       (2 * child->core.border_width)) < 1
		? 1
		: FCP_H(con) -
		(2 * child->core.border_width);
	}
	else
	{
	    _XmConfigureObject(child,
			       FCP_X(con),
			       FCP_Y(con),
			       (FCP_W(con) -
				(2 * child->core.border_width)) < 0
			       ? 0
			       : FCP_W(con) -
			       (2 * child->core.border_width),
			       (FCP_H(con) -
				(2 * child->core.border_width)) < 0
			       ? 0
			       : FCP_H(con) -
			       (2 * child->core.border_width),
			       child->core.border_width);
	}
    }
#undef	Wants
}


static void
_XmFormExportOffset(Widget widget, int offset, XtArgVal *value)
{
    if (*value == XmINVALID_DIMENSION)
    {
	*value = 0;
    }
}


static XmImportOperator
_XmFormImportOffset(Widget widget, int offset, XtArgVal *value)
{
    return XmSYNTHETIC_LOAD;
}

/************************ EXTERNAL INTERFACE ****************************/


extern Widget
XmCreateForm(Widget parent, char *name,
	     Arg *arglist, Cardinal argCount)
{
    return XtCreateWidget(name, xmFormWidgetClass, parent,
			  arglist, argCount);
}


extern Widget
XmCreateFormDialog(Widget parent, char *name,
		   Arg *arglist, Cardinal argcount)
{
    Widget shell, r;
    char *shell_name;
    int ac;
    Cardinal i;
    Arg *al = (Arg *)XtCalloc(argcount + 1, sizeof(Arg));

    ac = 0;
    XtSetArg(al[ac], XmNallowShellResize, True);
    ac++;
    for (i = 0; i < argcount; i++)
    {
	XtSetArg(al[ac], arglist[i].name, arglist[i].value);
	ac++;
    }

    shell_name = _XmMakeDialogName(name);
    shell = XmCreateDialogShell(parent, shell_name, al, ac);
    XtFree(shell_name);

    r = XmCreateForm(shell, name, al, ac);
    XtFree((char *)al);

    return r;
}
