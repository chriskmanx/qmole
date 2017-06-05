/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/MainW.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002 LessTif Development Team
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

static const char rcsid[] = "$Id: MainW.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/BaseClassP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/RowColumnP.h>
#include <Xm/CommandP.h>
#include <Xm/MainWP.h>
#include <Xm/SeparatoG.h>
#include <Xm/SeparatoGP.h>

#include <XmI/DebugUtil.h>

#define	VALID(w)	(w != NULL && XtIsManaged(w))

/* Forward Declarations */

#if 0
static void class_initialize();
#endif

static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void resize(Widget w);

#if 0
static void realize(Widget w, Mask *value_mask,
		    XSetWindowAttributes *attributes);
#endif

static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);
static void change_managed(Widget w);
static void insert_child(Widget w);
static void delete_child(Widget w);
static void _XmMainWindowPreferredSize(Widget w, Widget child,
				       XtWidgetGeometry *cg, XmMWValues * vals);
static XtGeometryResult _XmMainWindowGeomRequest(Widget w, XmMWValues * vals);
static void _XmMainWindowLayout(Widget w, Widget child,
				XtWidgetGeometry *cg, XmMWValues * vals);
static void _XmMainWindowConfigureChildren(Widget w, Widget child,
					   XtWidgetGeometry *cg,
					   XmMWValues * vals);

void _XmConfigureScrollBars(Widget w, Widget child,
			    XtWidgetGeometry *childgeom, XmSWValues * vals);

void _XmRepositionScrolledWindow(Widget w,
				 XtPointer client,
				 XtPointer call);

void _XmFixupScrollBars(Widget w, Dimension ww, Dimension wh);

static void ReparentChild(Widget w, Widget child);

/*
 * Resources for the MainWindow class
 */
#define Offset(field) XtOffsetOf(XmMainWindowRec, mwindow.field)
static XtResource resources[] =
{
    {
	XmNcommandWindow, XmCCommandWindow, XmRWidget,
	sizeof(Widget), Offset(CommandWindow),
	XmRImmediate, NULL
    },
    {
  XmNcommandWindowLocation, XmCCommandWindowLocation, XmRCommandWindowLocation,
	sizeof(unsigned char), Offset(CommandLoc),
	XmRImmediate, (XtPointer)XmCOMMAND_ABOVE_WORKSPACE
    },
    {
	XmNmenuBar, XmCMenuBar, XmRWidget,
	sizeof(Widget), Offset(MenuBar),
	XmRImmediate, NULL
    },
    {
	XmNmessageWindow, XmCMessageWindow, XmRWidget,
	sizeof(Widget), Offset(Message),
	XmRImmediate, NULL
    },
    {
    XmNmainWindowMarginWidth, XmCMainWindowMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)0
    },
    {
    XmNmainWindowMarginHeight, XmCMainWindowMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNshowSeparator, XmCShowSeparator, XmRBoolean,
	sizeof(Boolean), Offset(ShowSep),
	XtRImmediate, (XtPointer)False
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNmainWindowMarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmainWindowMarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};

/* Add Actions and Translations -- FIX ME */
#if 0
static XmBaseClassExtRec _XmMainWindowCoreClassExtRec = {
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
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

static XmManagerClassExtRec _XmMainWMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIX ME */
};
#endif

XmMainWindowClassRec xmMainWindowClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmScrolledWindowClassRec,
        /* class_name            */ "XmMainWindow",
	/* widget_size           */ sizeof(XmMainWindowRec),
	/* class_initialize      */ NULL /*class_initialize*/,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize /*realize*/,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressSeries,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ NULL,
	/* resize                */ resize,
	/* expose                */ XtInheritExpose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations /*NULL*/,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)NULL /*&_XmMainWindowCoreClassExtRec*/
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager, 
        /* change_managed   */ change_managed, 
        /* insert_child     */ insert_child,
        /* delete_child     */ delete_child,
        /* extension        */ NULL,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,
        /* subresource_count */ 0,
        /* constraint_size   */ 0,
        /* initialize        */ NULL,
        /* destroy           */ NULL,
        /* set_values        */ NULL,
        /* extension         */ NULL,
    },
    /* XmManager class part */
    {
	/* translations                 */ XtInheritTranslations,
	/* syn_resources                */ syn_resources,
	/* num_syn_resources            */ XtNumber(syn_resources),
	/* syn_constraint_resources     */ NULL,
	/* num_syn_constraint_resources */ 0,
	/* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)NULL /*&_XmMainWMClassExtRec*/
    },
    /* XmScrolledWindow part */
    {
	/* extension */ NULL,
    },
    /* XmMainWindow part */
    {
	/* extension */ NULL,	
    },
};


WidgetClass xmMainWindowWidgetClass = (WidgetClass)&xmMainWindowClassRec;

#if 0
static void
class_initialize()
{
    _XmMainWindowCoreClassExtRec.record_type = XmQmotif;
}
#endif

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmMAIN_WINDOW_BIT);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    /* Revert setting from XmScrolledWindow's initialize */
    /* T. Straumann:
     *        do this only if an initial window size
     *        was specified. The default (100x100 for
     *        XmAUTOMATIC should be set for the
     *        working window, not the whole mainWindow.
     *        Furthermore, there is a weird difference
     *        between >tif's ScrolledW and MainW regarding the
     *        default size of 100x100.
     *        The 100x100 is the _overall_ default size of
     *        their ScrolledW. Their MainW however chooses
     *        a default size of the entire clip window as
     *
     *            (100 + spacing + highlightThickness + VsbW)
     *          x
     *            (100 + spacing + highlightThickness + HsbH)
     *        
     *        I.e. if scrollbars are not needed and/or the
     *        highlightThickness is > 0, the effective space
     *        left for the clip window is actually bigger
     *        than 100x100.
     *
     *        NOTE:  highlightThickness means the highlight
     *               Thickness of VSB/HSB. >tif's ScrolledW
     *               (and also MainW) will do VERY weird
     *               layout if
     *               
     *               VSB.highlightThickness != HSB.highlightThickness
     *
     *               I refuse spending any effort to re-implement
     *               this behavior. IT SUCKS.
     */
    if (XtWidth(request) == 0)
    {
	XtWidth(new_w) = 0;
	if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
	{
	    SW_GivenWidth(new_w) = 100 + SW_Spacing(new_w)
#ifdef I_WANT_THEIR_SUCKING_EXTRA_SEP
		+ Prim_HighlightThickness(SW_VSB(new_w))
#endif
		+ XtWidth(SW_VSB(new_w));
	}
	else
	{
	    /* T. Straumann: FIXME (don't know what they do in this case) */
	}
    }
    if (XtHeight(request) == 0)
    {
	XtHeight(new_w) = 0;
	if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
	{
	    SW_GivenHeight(new_w) = 100 + SW_Spacing(new_w)
#ifdef I_WANT_THEIR_SUCKING_EXTRA_SEP
		+ Prim_HighlightThickness(SW_HSB(new_w))
#endif
		+ XtHeight(SW_HSB(new_w));
	}
	else
	{
	    /* T. Straumann: FIXME (don't know what they do in this case) */
	}
    }

    /* Override setting of XmScrolledWindow's Margin[Width|Height] resources */
    SW_MarginWidth(new_w) = MW_MarginWidth(new_w);
    SW_MarginHeight(new_w) = MW_MarginHeight(new_w);

    MW_Sep1(new_w) = (XmSeparatorGadget)XmCreateSeparatorGadget(new_w,
								"Separator1",
								args,
								*num_args);
    MW_Sep2(new_w) = (XmSeparatorGadget)XmCreateSeparatorGadget(new_w,
								"Separator2",
								args,
								*num_args);
    MW_Sep3(new_w) = (XmSeparatorGadget)XmCreateSeparatorGadget(new_w,
								"Separator3",
								args,
								*num_args);
    if (MW_ShowSep(new_w))
    {
	if (VALID(MW_MenuBar(new_w)))
	{
	    XtManageChild((Widget)MW_Sep1(new_w));
	}
	if (VALID(MW_CommandWindow(new_w)))
	{
	    XtManageChild((Widget)MW_Sep2(new_w));
	}
	if (VALID(MW_MessageWindow(new_w)))
	{
	    XtManageChild((Widget)MW_Sep3(new_w));
	}
    }
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh = False;
    Boolean relayout = False;
#define NE(x)	(x(old) != x(new_w))

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

    if (NE(MW_CommandLoc))
    {
	refresh = True;
	/* T. Straumann: mainw/test10 failed; we need a relayout to propagate the change */
	relayout = True;
    }

    if (NE(SW_WorkWindow))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "SetValues: Changed the work window !\n"));

	if (SW_ClipWindow(new_w))
	{
	    if (SW_WorkWindow(old))
	    {
		ReparentChild(new_w, SW_WorkWindow(old));
	    }
	    ReparentChild((Widget)SW_ClipWindow(new_w), SW_WorkWindow(new_w));
	}

	if (MW_CommandWindow(new_w) == SW_WorkWindow(new_w))
	{
	    MW_CommandWindow(new_w) = NULL;
	}
	if (MW_MessageWindow(new_w) == SW_WorkWindow(new_w))
	{
	    MW_MessageWindow(new_w) = NULL;
	}
	if (MW_MenuBar(new_w) == SW_WorkWindow(new_w))
	{
	    MW_MenuBar(new_w) = NULL;
	}

	refresh = True;
    }

    if (NE(MW_CommandWindow))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "SetValues: Changed the command window !\n"));

	if (SW_VisualPolicy(new_w) == XmCONSTANT &&
	    MW_CommandWindow(new_w) &&
	    XtParent(MW_CommandWindow(new_w)) == (Widget)SW_ClipWindow(new_w))
	{
	    ReparentChild(new_w, MW_CommandWindow(new_w));
	}

	if (SW_WorkWindow(new_w) == MW_CommandWindow(new_w))
	{
	    SW_WorkWindow(new_w) = NULL;
	}
	if (MW_MessageWindow(new_w) == MW_CommandWindow(new_w))
	{
	    MW_MessageWindow(new_w) = NULL;
	}
	if (MW_MenuBar(new_w) == MW_CommandWindow(new_w))
	{
	    MW_MenuBar(new_w) = NULL;
	}

	refresh = True;
    }

    if (NE(MW_MessageWindow))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "SetValues: Changed the message window !\n"));

	if (SW_VisualPolicy(new_w) == XmCONSTANT &&
	    MW_MessageWindow(new_w) &&
	    XtParent(MW_MessageWindow(new_w)) == (Widget)SW_ClipWindow(new_w))
	{
	    ReparentChild(new_w, MW_MessageWindow(new_w));
	}

	if (MW_CommandWindow(new_w) == MW_MessageWindow(new_w))
	{
	    MW_CommandWindow(new_w) = NULL;
	}
	if (SW_WorkWindow(new_w) == MW_MessageWindow(new_w))
	{
	    SW_WorkWindow(new_w) = NULL;
	}
	if (MW_MenuBar(new_w) == MW_MessageWindow(new_w))
	{
	    MW_MenuBar(new_w) = NULL;
	}

	refresh = True;
    }

    if (NE(MW_MenuBar))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "SetValues: Changed the menu bar !\n"));

	if (SW_VisualPolicy(new_w) == XmCONSTANT &&
	    XtParent(MW_MenuBar(new_w)) == (Widget)SW_ClipWindow(new_w))
	{
	    ReparentChild(new_w, MW_MenuBar(new_w));
	}

	if (MW_CommandWindow(new_w) == MW_MenuBar(new_w))
	{
	    MW_CommandWindow(new_w) = NULL;
	}
	if (SW_WorkWindow(new_w) == MW_MenuBar(new_w))
	{
	    SW_WorkWindow(new_w) = NULL;
	}
	if (MW_MessageWindow(new_w) == MW_MenuBar(new_w))
	{
	    MW_MessageWindow(new_w) = NULL;
	}

	refresh = True;
    }

    if (MW_ShowSep(new_w))
    {
	if (VALID(MW_MenuBar(new_w)))
	{
	    XtManageChild((Widget)MW_Sep1(new_w));
	}
	else
	{
	    XtUnmanageChild((Widget)MW_Sep1(new_w));
	}
	if (VALID(MW_CommandWindow(new_w)))
	{
	    XtManageChild((Widget)MW_Sep2(new_w));
	}
	else
	{
	    XtUnmanageChild((Widget)MW_Sep2(new_w));
	}
	if (VALID(MW_MessageWindow(new_w)))
	{
	    XtManageChild((Widget)MW_Sep3(new_w));
	}
	else
	{
	    XtUnmanageChild((Widget)MW_Sep3(new_w));
	}
    }
    else if (NE(MW_ShowSep))
    {
	XtUnmanageChild((Widget)MW_Sep1(new_w));
	XtUnmanageChild((Widget)MW_Sep2(new_w));
	XtUnmanageChild((Widget)MW_Sep3(new_w));
	refresh = True;
    }

    if (NE(MW_MarginHeight))
    {
	SW_MarginHeight(new_w) = MW_MarginHeight(new_w);
	refresh = True;
    }

    if (NE(MW_MarginWidth))
    {
	SW_MarginWidth(new_w) = MW_MarginWidth(new_w);
	refresh = True;
    }

    if (XtIsRealized(new_w) && refresh)
    {
	XmMWValues vals;

	_XmMainWindowPreferredSize(new_w, NULL, NULL, &vals);
	XtWidth(new_w) = vals.MwW;
	XtHeight(new_w) = vals.MwH;
	if (relayout)
	{
	    _XmMainWindowLayout(new_w, NULL, NULL, &vals);
	    _XmMainWindowConfigureChildren(new_w, NULL, NULL, &vals);
	}
    }

    return refresh;
}

static void
resize(Widget w)
{
    XmMWValues vals;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:resize(%d) (%dx%d)\n",
		      __FILE__, __LINE__,
		      XtWidth(w), XtHeight(w)));

    SW_FromResize(w) = True;

    _XmMainWindowPreferredSize(w, NULL, NULL, &vals);

    vals.MwW = XtWidth(w);
    vals.MwH = XtHeight(w);

    _XmMainWindowLayout(w, NULL, NULL, &vals);

    _XmMainWindowConfigureChildren(w, NULL, NULL, &vals);

    SW_FromResize(w) = False;
}

#if 0
static void
realize(Widget w, Mask *value_mask, XSetWindowAttributes *attributes)
{
/* Motif inherits this method */
/* and we could also, if ScrolledW gets rid of the geo stuff in realize */

    /*
       XmMWValues vals;
     */

    DEBUGOUT(_LtDebug(__FILE__, w, "Realize ...\n"));

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass

#if 0
    /* mainw/test13 */
    if (XtWidth(w) != 0 && XtWidth(w) != 1)
    {
	SW_GivenWidth(w) = XtWidth(w);
    }
    if (XtHeight(w) != 0 && XtHeight(w) != 1)
    {
	SW_GivenHeight(w) = XtHeight(w);
    }

    _XmMainWindowPreferredSize(w, NULL, NULL, &vals);

    _XmMainWindowGeomRequest(w, &vals);

    /* ordinarily, we'd look at the return code and reset w/h based on
     * it.  But our routine does that for us. */

    _XmMainWindowLayout(w, NULL, NULL, &vals);

    _XmMainWindowConfigureChildren(w, NULL, NULL, &vals);

    DEBUGOUT(_LtDebug(__FILE__, w, "Realize => size %d %d\n",
		      XtWidth(w), XtHeight(w)));
#endif
}
#endif

static XtGeometryResult
query_geometry(Widget w,
	       XtWidgetGeometry *intended,
	       XtWidgetGeometry *preferred)
{
#if 0
    XtWidgetGeometry wants;
    XmMWValues vals;

    wants = *intended;

    _XmMainWindowPreferredSize(w, w, NULL, &vals);

    if (preferred)
    {
	preferred->width = vals.MwW;
	preferred->height = vals.MwH;
    }

    if (((wants.request_mode & CWWidth) &&
	 wants.width == preferred->width) &&
	((wants.request_mode & CWHeight) &&
	 wants.height == preferred->height))
    {
	return XtGeometryNo;
    }

    if ((wants.request_mode & CWWidth) &&
	wants.width != preferred->width)
    {
	return XtGeometryAlmost;	/* Something's different */
    }
    if ((wants.request_mode & CWHeight) &&
	wants.height != preferred->height)
    {
	return XtGeometryAlmost;	/* Something's different */
    }

    return XtGeometryYes;
#else
    XtWidgetGeometry wants;
    XmMWValues vals;

    /* see if they propose a width */
    if (intended->request_mode & CWWidth)
    {
	/* yes, propagate it to _XmMainWindowPreferredSize() */
	wants.request_mode = CWWidth;
	wants.width = intended->width;
	_XmMainWindowPreferredSize(w, w, &wants, &vals);
    }
    else
    {
	_XmMainWindowPreferredSize(w, w, NULL, &vals);
    }

    wants = *intended;

    if (preferred)
    {
	preferred->width = vals.MwW;
	preferred->height = vals.MwH;
    }

    return _XmGMReplyToQueryGeometry(w, &wants, preferred);
#endif
}

static void
CreateManagedList(Widget w, Widget **managed, int *num_managed,
		  int *num_allocated)
{
    if (XtIsComposite(w) && MGR_NumChildren(w) > 0)
    {
	Cardinal i;
	for (i = 0; i < MGR_NumChildren(w); i++)
	{
	    CreateManagedList(MGR_Children(w)[i], managed, num_managed,
			      num_allocated);
	}
    }
    if (XtIsManaged(w))
    {
	if (*num_managed == *num_allocated)
	{
	    /* Allocate more space */
	    *num_allocated += (*num_allocated / 2) + 2;
	    *managed =
		(WidgetList)XtRealloc((char *)*managed,
				      (unsigned)(*num_allocated *
						 sizeof(Widget)));
	}
	*managed[*num_managed++] = w;
    }
}

static void
ReparentChild(Widget w, Widget child)
{

    Widget old_parent = XtParent(child), *managed;
    Cardinal i, j;
    int num_managed, num_allocated;
    Boolean is_realized;

    if (old_parent == w)
    {
	return;
    }
    /* first make a list of children who needs to be managed again */
    num_managed = 0;
    num_allocated = 2;
    managed = (Widget *)XtMalloc(num_allocated * sizeof(Widget));
    CreateManagedList(child, &managed, &num_managed, &num_allocated);
    for (i = 0; i < MGR_NumChildren(old_parent); i++)
    {
	if (MGR_Children(old_parent)[i] == child)
	{
	    break;
	}
    }
    if (MGR_NumChildren(w) == MGR_NumSlots(w))
    {
	/* Allocate more space */
	MGR_NumSlots(w) += (MGR_NumSlots(w) / 2) + 2;
	MGR_Children(w) =
	    (WidgetList)XtRealloc((char *)MGR_Children(w),
				  (unsigned)(MGR_NumSlots(w) *
					     sizeof(Widget)));
    }
    if ((is_realized = XtIsRealized(child)))
    {
	XtUnrealizeWidget(child);
    }
    MGR_Children(w)[MGR_NumChildren(w)++] = child;
    XtParent(child) = w;
    if (is_realized)
    {
	XtRealizeWidget(child);
    }
    for (j = 0; j < (int)num_managed; j++)
    {
	XtManageChild(managed[j]);
    }
    XtFree((char *)managed);

    for (j = i + 1; j < MGR_NumChildren(old_parent); j++)
    {
	MGR_Children(old_parent)[j - 1] = MGR_Children(old_parent)[j];
    }
    MGR_NumChildren(old_parent)--;
}




static void
change_managed(Widget w)
{
    XmMWValues vals;

    DEBUGOUT(_LtDebug(__FILE__, w, "ChangeManaged\n"));
    /*
     * It is possible that some of the MainWindows children is made children
     * of ClipWindow by the ScrolledWindow. Steal them back.
     */
    if (SW_VisualPolicy(w) == XmCONSTANT &&
	MGR_NumChildren(SW_ClipWindow(w)) > 1)
    {
	XmDrawingAreaWidget cw = SW_ClipWindow(w);
	Widget mw_children[3];
	Cardinal i, num_mw_children = 0;

	for (i = 0; i < MGR_NumChildren(cw); i++)
	{
	    if (MGR_Children(cw)[i] == MW_CommandWindow(w) ||
		MGR_Children(cw)[i] == MW_MenuBar(w) ||
		MGR_Children(cw)[i] == MW_MessageWindow(w))
	    {
		mw_children[num_mw_children++] = MGR_Children(cw)[i];
	    }
	}
	for (i = 0; i < num_mw_children; i++)
	{
	    ReparentChild(w, mw_children[i]);
	}
    }
    if (SW_HSB(w) && XtIsManaged(SW_HSB(w)))
    {
	SW_HasHSB(w) = True;
    }
    else
    {
	SW_HasHSB(w) = False;
    }

    if (SW_VSB(w) && XtIsManaged(SW_VSB(w)))
    {
	SW_HasVSB(w) = True;
    }
    else
    {
	SW_HasVSB(w) = False;
    }

    if (MW_ShowSep(w))
    {
	if (VALID(MW_MenuBar(w)))
	{
	    XtManageChild((Widget)MW_Sep1(w));
	}
	else
	{
	    XtUnmanageChild((Widget)MW_Sep1(w));
	}
	if (VALID(MW_CommandWindow(w)))
	{
	    XtManageChild((Widget)MW_Sep2(w));
	}
	else
	{
	    XtUnmanageChild((Widget)MW_Sep2(w));
	}
	if (VALID(MW_MessageWindow(w)))
	{
	    XtManageChild((Widget)MW_Sep3(w));
	}
	else
	{
	    XtUnmanageChild((Widget)MW_Sep3(w));
	}
    }
    _XmMainWindowPreferredSize(w, NULL, NULL, &vals);

#if 0				/* T. Straumann: what's that for ? */
    if (!XtIsRealized(w))
    {
	/* mainw/test11 */
	if (SW_GivenWidth(w) == 0)
	{
	    vals.MwW = SW_GivenWidth(w);
	}
	if (SW_GivenHeight(w) == 0)
	{
	    vals.MwH = SW_GivenHeight(w);
	}
    }
#endif

    _XmMainWindowGeomRequest(w, &vals);

    /* ordinarily, we'd look at the return code and reset w/h based on
     * it.  But our routine does that for us. */

    _XmMainWindowLayout(w, NULL, NULL, &vals);

    _XmMainWindowConfigureChildren(w, NULL, NULL, &vals);
}


/*
 * This is the new implementation of MainWindow Layout.
 * Danny 16/1/1997
 *
 * Parameters :
 *      w is the MainWindow
 *      ParentResize - True if we are allowed to try to change our own size
 *      child - the 'instigator' (the child causing this request, its
 *              geometry should not be changed)
 *      TestMode - True if we're not allowed to change anything
 *      cg - the childs geometry
 *      mwg - return the mainwindow geometry
 *
 * Stuff to layout is :
 *      - MenuBar (MW_MenuBar)
 *      - Command Area (MW_CommandWindow)
 *      - Work Area (SW_WorkWindow)             note SW_
 *      - Message Area (MW_MessageWindow)
 * If MW_CommandLoc == XmCOMMAND_ABOVE_WORKSPACE then the order as given above.
 * Otherwise reverse order of Command Area and Work Area.
 *
 * If MW_ShowSep then separators (three) must be shown.
 */

static void
_XmMainWindowPreferredSize(Widget w, Widget child, XtWidgetGeometry *cg,
			   XmMWValues * vals)
{
    /* Dimension variables typed as int to allow for negative values. */
    Dimension curh, savedh;
    Boolean childNotHappy;

    int curw;
    int maxw = -1;

    Widget chld[3], sep[3];

    struct wxh
    {
	Dimension w, h;
	char *name;
    }
    chldwh[3], sepwh[3];

    int i, nchld, nsep = 0;

    memset(vals, 0, sizeof(XmMWValues));

    /* ignore the instigator if resizing */
    if (SW_FromResize(w))
    {
	maxw = XtWidth(w) - 2 * SW_MarginWidth(w);
    }
    else
    {
	if (cg && child)
	{
	    /* T. Straumann: hmmm... we must take care. If the instigator
	     *                             is the work window in variable mode, we
	     *               have to ask scrolledW first. However, to do its
	     *               computation correctly, scrolledW needs to know
	     *               its XtWidth and XtHeight respectively. Unfortunately,
	     *               the height left for the scrolled area
	     *               depends on all the other items (menu bar etc.) which
	     *               we don't know yet.
	     *               Therefore, we just take their actual height.
	     */
	    if (child == SW_WorkWindow(w) && SW_VisualPolicy(child) == XmVARIABLE)
	    {
		/* T. Straumann: first, compute the height left. This calculation is
		 *                             based on the actual height.
		 */
		Dimension swh;
		savedh = swh = XtHeight(w);

		if (VALID(MW_MenuBar(w)))
		{
		    swh -= XtHeight(MW_MenuBar(w));
		}
		if (VALID(MW_CommandWindow(w)))
		{
		    swh -= XtHeight(MW_CommandWindow(w));
		}
		if (VALID(MW_MessageWindow(w)))
		{
		    swh -= XtHeight(MW_MessageWindow(w));
		}
		if (MW_ShowSep(w))
		{
		    swh -= XtHeight(MW_Sep1(w)) + XtHeight(MW_Sep2(w)) + XtHeight(MW_Sep3(w));
		}
		/* Now, we do some hacking. The scrolled window routine should believe
		 * this is all it has got.
		 */
		XtHeight(w) = swh > 0 ? swh : 0;

		/*
		 * T. Straumann:  let the scrolledW do its job
		 * NOTE: this is _ugly_, should be a method, but alas, we must not
		 *               add to ScrolledWP.h
		 * Hey, what about an class extension record?
		 */
		_XmScrolledWPreferredSize(w, child, cg, (XmSWValues *) vals);
		/* restore the height */
		XtHeight(w) = savedh;

		maxw = vals->MwW - 2 * SW_MarginWidth(w);
	    }
	    else
	    {			/* it's not a child already handled by ScrolledW */

		/* T. Straumann: allow changes of border width of work window,
		 *               and other children
		 */
		if (child == MW_CommandWindow(w)
		    || child == MW_MessageWindow(w) || child == MW_MenuBar(w))
		{
		    cg->request_mode &= (CWWidth | CWHeight | XtCWQueryOnly | CWBorderWidth);
		}
		else
		{
		    cg->request_mode &= (CWWidth | CWHeight | XtCWQueryOnly);
		}
		/* T. Straumann: if an instigator asks for a new width, we try to calculate the
		 *                             preferred size based on this value.
		 *                               -1 (default) meaning that no width is asked for.
		 */
		if (cg->request_mode & CWWidth)
		{
		    maxw = cg->width;
		    maxw += 2 * (cg->request_mode & CWBorderWidth ?
				 cg->border_width : XtBorderWidth(child));
		}
	    }
	}
    }				/* if FromResize */

    /*
     * As in all the complicated layout functions, this is the algorithm :
     * - keep everything in local variables for implementing TestMode
     * - first figure out how much space we need
     * - if we're allowed, request geometry change
     * - layout children (in local variables !) based on geometry
     *      which may be different from the geometry requested above
     * - if not in TestMode, apply all changes
     *      Otherwise, return some values in parameters.
     */

    /* Talk */
    DEBUGOUT(_LtDebug2(__FILE__, w, child, "_XmMainWindowPreferredSize request %s\n",
		       _LtDebugWidgetGeometry2String(cg)));

    /* T. Straumann: do querying the kids using a loop (I'm lazy) */

    nchld = 0;
    chld[nchld] = VALID(MW_MenuBar(w)) ? MW_MenuBar(w) : 0;
    chldwh[nchld].name = "menu bar";
    chldwh[nchld].w = chldwh[nchld].h = 0;
    nchld++;
    chld[nchld] = VALID(MW_CommandWindow(w)) ? MW_CommandWindow(w) : 0;
    chldwh[nchld].name = "command window";
    chldwh[nchld].w = chldwh[nchld].h = 0;
    nchld++;
    chld[nchld] = VALID(MW_MessageWindow(w)) ? MW_MessageWindow(w) : 0;
    chldwh[nchld].name = "message window";
    chldwh[nchld].w = chldwh[nchld].h = 0;
    nchld++;

    if (MW_ShowSep(w))
    {
	nsep = 0;
	sep[nsep] = VALID(MW_Sep1(w)) ? (Widget)MW_Sep1(w) : 0;
	sepwh[nsep].w = sepwh[nsep].h = 0;
	sepwh[nsep].name = "Sep1";
	nsep++;
	sep[nsep] = VALID(MW_Sep2(w)) ? (Widget)MW_Sep2(w) : 0;
	sepwh[nsep].w = sepwh[nsep].h = 0;
	sepwh[nsep].name = "Sep2";
	nsep++;
	sep[nsep] = VALID(MW_Sep3(w)) ? (Widget)MW_Sep3(w) : 0;
	sepwh[nsep].w = sepwh[nsep].h = 0;
	sepwh[nsep].name = "Sep3";
	nsep++;
    }


    /* T. Straumann: 

     * find out how big we wanna be:
     *
     * repeat asking all children for their size given a proposed
     * width until the proposed width seems acceptable for all children.
     *
     * The proposed width is initially set to 
     *
     *  - the width proposed by the instigator if any.
     *  - the current width of mainw (minus margin & friends) if we are
     *    resizing.
     *
     * (These are orthogonal; SW_FromResize implies cg==NULL)
     *
     * If any child is not happy with the proposed width, the proposed 
     * width (`curw') is increased to the maximal width of all children
     * and the query process restarted.
     *
     * Note that we have to repeat this until things get stable. Consider
     * e.g. the following scenario:
     *
     * child A accepts the proposed width but arranges for a bigger
     * height (e.g. wraps a line).
     * child B rejects -> width is increased.       
     * child A does not need the bigger height anymore.
     *
     * If we wouldn't restart the query, the recorded height of A
     * would be wrong.
     */
    do
    {

	/* calculate the size based on a proposed width of the last maxw */
	curw = maxw;

	maxw = 0;
	childNotHappy = False;

	/*
	 * Find out how big we need to be.
	 *      Only count SW_MarginWidth etc. at the end.
	 *      Don't need to care about child order yet.
	 */
	curh = 0;

	for (i = 0; i < nchld; i++)
	{
	    if (chld[i])
	    {
		Dimension bw = XtBorderWidth(chld[i]);
		XtWidgetGeometry rcg, rcg_r;

		DEBUGOUT(_LtDebug2(__FILE__, w, chld[i],
		       "_XmMainWindowPreferredSize: %s current wid %d ht %d\n",
			 chldwh[i].name, XtWidth(chld[i]), XtHeight(chld[i])));

		rcg.request_mode = (curw >= 0) ? CWWidth : 0;
		rcg.width = (Dimension)curw;

		if (SW_FromResize(w))
		{
		    /* T. Straumann: account for border */
		    rcg.width -= 2 * bw;
		}

		XtQueryGeometry(chld[i], &rcg, &rcg_r);

#define	Wants(xx)	(cg && ((cg->request_mode) & xx))

		/*
		 * If we are resizing, the proposed width is mandatory and we
		 * don't have to loop.
		 */
		if (!SW_FromResize(w) && Wants(CWWidth) && rcg.width != rcg_r.width)
		{
		    childNotHappy = True;
		}
		chldwh[i].w = rcg_r.width;
		chldwh[i].h = rcg_r.height;

		if (child && chld[i] == child)
		{
		    if (Wants(CWWidth) && chldwh[i].w != cg->width)
		    {
			chldwh[i].w = cg->width;
			childNotHappy = True;
			DEBUGOUT(_LtDebug0(__FILE__, w, "\tbut it wants width %d\n",
					   cg->width));
		    }
		    if (Wants(CWHeight) && chldwh[i].h != cg->height)
		    {
			chldwh[i].h = cg->height;
			childNotHappy = True;
			DEBUGOUT(_LtDebug0(__FILE__, w, "\tbut it wants height %d\n",
					   cg->height));
		    }
		    /* T. Straumann: allow changes of border width */
		    if (Wants(CWBorderWidth) && bw != cg->border_width)
		    {
			childNotHappy = True;
			bw = cg->border_width;
			/* FIXME: need to store the border width somewhere? */
		    }
		}
		chldwh[i].w += 2 * bw;
		chldwh[i].h += 2 * bw;

		DEBUGOUT(_LtDebug2(__FILE__, w, chld[i],
				   "%s w %d h %d\n",
				   chldwh[i].name, chldwh[i].w, chldwh[i].h));

		curh += chldwh[i].h;
		if (maxw < chldwh[i].w)
		{
		    maxw = chldwh[i].w;
		}
	    }
	}			/* for menubar command_window message_window */

	/* T. Straumann: moved separator stuff here, so we know the
	 *                     remaining space.
	 */

	if (MW_ShowSep(w))
	{
	    for (i = 0; i < nsep; i++)
	    {
		if (sep[i])
		{
		    sepwh[i].w = XtWidth(sep[i]);
		    sepwh[i].h = XtHeight(sep[i]);

		    if (sep[i] == child)
		    {
			if (Wants(CWWidth))
			{
			    sepwh[i].w = cg->width;
			}
			if (Wants(CWHeight))
			{
			    sepwh[i].h = cg->height;
			}
		    }

		    curh += sepwh[i].h;
		}
	    }

	}

	/* T. Straumann: ok - now it's time to let ScrolledW do the rest.
	   *               first, we have to hack the size of the widget.
	   * NOTE: this is _ugly_, should be a method, but alas, we must not
	   *             add to ScrolledWP.h
	   * Hey, what about an class extension record?
	 */
	savedh = XtHeight(w);
	XtHeight(w) = savedh > curh ? savedh - curh : 0;
	_XmScrolledWPreferredSize(w, child, cg, (XmSWValues *) vals);
	XtHeight(w) = savedh;

	/* move the SwW to www etc. */
	vals->www = vals->MwW - 2 * SW_MarginWidth(w);
	vals->wwh = vals->MwH - 2 * SW_MarginHeight(w);

	/* T. Straumann: removed the code that was pasted from ScrolledW;
	   *               instead, we call _XmScrolledWPreferredSize()
	 */

	curh += vals->wwh;
	if (maxw < vals->www)
	{
	    maxw = vals->www;
	}

    }
    while (childNotHappy && maxw != curw);
    /*
     * T. Straumann
     * repeat the child query process based on a new proposed width.
     * Consider the following:
     * Children are queried proposing a new width (proposed by instigator).
     * Child arranges for a height based on new width.
     * Another child (the widest one) sets the width of MainW.
     * First child doesn't need up the height it requested when the layout
     * is done eventually (because the actual width is bigger than the one
     * proposed at the time of the query).
     */
    curw = maxw;

    /* Now count them in */
    if (curw > 0)
    {
	curw += 2 * SW_MarginWidth(w);
    }
    if (curh > 0)
    {
	curh += 2 * SW_MarginHeight(w);
    }

    /* fill in the values */
    i = 0;
    vals->mbw = chldwh[i].w;
    vals->mbh = chldwh[i].h;
    i++;
    vals->cww = chldwh[i].w;
    vals->cwh = chldwh[i].h;
    i++;
    vals->mww = chldwh[i].w;
    vals->mwh = chldwh[i].h;
    i++;

    i = 0;
    vals->s1w = sepwh[i].w;
    vals->s1h = sepwh[i].h;
    i++;
    vals->s2w = sepwh[i].w;
    vals->s2h = sepwh[i].h;
    i++;
    vals->s3w = sepwh[i].w;
    vals->s3h = sepwh[i].h;
    i++;

    vals->MwW = (Dimension)curw;
    vals->MwH = curh;
}
#undef	Wants

static XtGeometryResult
_XmMainWindowGeomRequest(Widget w, XmMWValues * vals)
{
    XtGeometryResult res;
    XtWidgetGeometry geo;

    /*
     * MLM: This breaks xmgr.  Don't do this!
     * Well, ok, don't use 100x100
     * also check out mainw/test12
     */
#if 0
#if 0
    if (SW_GivenWidth(w) != 0)
    {
	vals->MwW = SW_GivenWidth(w);
    }

    if (SW_GivenHeight(w) != 0)
    {
	vals->MwH = SW_GivenHeight(w);
    }
#else
    if (!XtIsRealized(w))
    {
	if (SW_GivenWidth(w) != 0)
	{
	    vals->MwW = SW_GivenWidth(w);
	}
	if (SW_GivenHeight(w) != 0)
	{
	    vals->MwH = SW_GivenHeight(w);
	}
    }
#endif
#endif

    geo.width = vals->MwW;
    geo.height = vals->MwH;
    geo.request_mode = CWWidth | CWHeight;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmMainWindowGeomRequests: request geo %s: "
		      "am %d %d given: %d %d\n",
		      _LtDebugWidgetGeometry2String(&geo),
		      XtWidth(w), XtHeight(w),
		      SW_GivenWidth(w), SW_GivenHeight(w)));

    if ((res = _XmMakeGeometryRequest(w, &geo)) == XtGeometryYes)
    {
	vals->MwW = geo.width;
	vals->MwH = geo.height;
    }
    else
    {
	vals->MwW = XtWidth(w);
	vals->MwH = XtHeight(w);

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmMainWindowGeomRequests CONF got %s\n",
			  _LtDebugWidgetGeometry2String(&geo)));
    }

    return res;
}

static void
_XmMainWindowLayout(Widget w, Widget child, XtWidgetGeometry *cg,
		    XmMWValues * vals)
{
    Position curx, cury;
    int savedh;

    /*
     * Now actually lay out the children.
     * Assume that resizing the widget larger than requested grows
     * the work area, and similarly with resizes smaller than requested.
     */
    /* T. Straumann: shadow only around the working area */
    curx = SW_MarginWidth(w);
    cury = SW_MarginHeight(w);

    if (VALID(MW_MenuBar(w)))
    {
	vals->mbx = curx;
	vals->mby = cury;
	vals->mbw = vals->MwW - 2 * SW_MarginWidth(w);

	cury += vals->mbh;
    }

    if (MW_ShowSep(w) && VALID(MW_Sep1(w)))
    {
	vals->s1x = curx;
	vals->s1y = cury;
	vals->s1w = vals->MwW - 2 * SW_MarginWidth(w);

	cury += vals->s1h;
    }

    if (MW_CommandLoc(w) == XmCOMMAND_ABOVE_WORKSPACE)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmMainWindowLayout COMMAND_ABOVE_WORKSPACE\n"));
	if (VALID(MW_CommandWindow(w)))
	{
	    vals->cwx = curx;
	    vals->cwy = cury;
	    vals->cww = vals->MwW - 2 * SW_MarginWidth(w);

	    cury += vals->cwh;
	}
	if (MW_ShowSep(w) && VALID(MW_Sep2(w)))
	{
	    vals->s2x = curx;
	    vals->s2y = cury;
	    vals->s2w = vals->MwW - 2 * SW_MarginWidth(w);

	    cury += vals->s2h;
	}

	vals->wwy = cury;

	/*
	 * Now work from below (in opposite direction) to get to the size
	 * of WorkWindow.
	 */
	cury = vals->MwH - SW_MarginHeight(w);
	if (VALID(MW_MessageWindow(w)))
	{
	    vals->mwx = curx;
	    vals->mwy = cury - vals->mwh;
	    vals->mww = vals->MwW - 2 * SW_MarginWidth(w);

	    cury = vals->mwy;
	}
	if (MW_ShowSep(w) && VALID(MW_Sep3(w)))
	{
	    vals->s3x = curx;
	    vals->s3y = cury - vals->s3h;
	    vals->s3w = vals->MwW - 2 * SW_MarginWidth(w);

	    cury = vals->s3y;
	}

	if (VALID(SW_WorkWindow(w)))
	{
	    vals->wwx = curx;
	    /* Statement to set WWY is moved up to where CURY is still useful */
	    vals->www = vals->MwW - 2 * SW_MarginWidth(w);
	    vals->wwh = cury - vals->wwy;
	}
    }
    else
    {				/* XmCOMMAND_BELOW_WORKSPACE */
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmMainWindowLayout COMMAND_BELOW_WORKSPACE\n"));
	/*
	 * Start working from the bottom immediately, eventually getting
	 * to the WorkWindow which will have to swallow all size changes.
	 */
	vals->wwy = cury;	/* For later on */
	cury = vals->MwH - SW_MarginHeight(w);

	if (VALID(MW_MessageWindow(w)))
	{
	    cury -= vals->mwh;

	    vals->mwx = curx;
	    vals->mwy = cury;
	    vals->mww = vals->MwW - 2 * SW_MarginWidth(w);
	}
	if (MW_ShowSep(w) && VALID(MW_Sep3(w)))
	{
	    cury -= vals->s3h;

	    vals->s3x = curx;
	    vals->s3y = cury;
	    vals->s3w = vals->MwW - 2 * SW_MarginWidth(w);
	}
	if (VALID(MW_CommandWindow(w)))
	{
	    cury -= vals->cwh;

	    vals->cwx = curx;
	    vals->cwy = cury;
	    vals->cww = vals->MwW - 2 * SW_MarginWidth(w);
	}
	if (MW_ShowSep(w) && VALID(MW_Sep2(w)))
	{
	    cury -= vals->s2h;

	    vals->s2x = curx;
	    vals->s2y = cury;
	    vals->s2w = vals->MwW - 2 * SW_MarginWidth(w);
	}

	if (VALID(SW_WorkWindow(w)))
	{
	    vals->wwx = curx;
	    /* See above. wwy = cury; */
	    vals->www = vals->MwW - 2 * SW_MarginWidth(w);
	    vals->wwh = cury - vals->wwy;
	}
    }

    savedh = vals->MwH;
    vals->MwH = vals->wwy + vals->wwh + SW_MarginHeight(w);
    vals->SwY = vals->wwy - SW_MarginHeight(w);
    /* T. Straumann: let the ScrolledW do its part of the job
     * NOTE: this is _ugly_, should be a method, but alas, we must not
     *               add to ScrolledWP.h
     * Hey, what about an class extension record?
     */
    _XmScrolledWLayout(w, child, cg, (XmSWValues *) vals);
    vals->MwH = savedh;

    if (child)
    {
	if (child == MW_MenuBar(w))
	{
	    cg->x = vals->mbx;
	    cg->y = vals->mby;
	    cg->width = vals->mbw < 0 ? XtWidth(child) : vals->mbw;
	    cg->height = vals->mbh < 0 ? XtHeight(child) : vals->mbh;
	    cg->request_mode = CWWidth | CWHeight | CWX | CWY;
	}
	else if (child == MW_CommandWindow(w))
	{
	    cg->x = vals->cwx;
	    cg->y = vals->cwy;
	    cg->width = vals->cww < 0 ? XtWidth(child) : vals->cww;
	    cg->height = vals->cwh < 0 ? XtHeight(child) : vals->cwh;
	    cg->request_mode = CWWidth | CWHeight | CWX | CWY;
	}
	else if (child == MW_MessageWindow(w))
	{
	    cg->x = vals->mwx;
	    cg->y = vals->mwy;
	    cg->width = vals->mww < 0 ? XtWidth(child) : vals->mww;
	    cg->height = vals->mwh < 0 ? XtHeight(child) : vals->mwh;
	    cg->request_mode = CWWidth | CWHeight | CWX | CWY;
	}
	else if (child == (Widget)MW_Sep1(w))
	{
	    cg->x = vals->s1x;
	    cg->y = vals->s1y;
	    cg->width = vals->s1w < 0 ? XtWidth(child) : vals->s1w;
	    cg->height = vals->s1h < 0 ? XtHeight(child) : vals->s1h;
	    cg->request_mode = CWWidth | CWHeight | CWX | CWY;
	}
	else if (child == (Widget)MW_Sep2(w))
	{
	    cg->x = vals->s2x;
	    cg->y = vals->s2y;
	    cg->width = vals->s2w < 0 ? XtWidth(child) : vals->s2w;
	    cg->height = vals->s2h < 0 ? XtHeight(child) : vals->s2h;
	    cg->request_mode = CWWidth | CWHeight | CWX | CWY;
	}
	else if (child == (Widget)MW_Sep3(w))
	{
	    cg->x = vals->s3x;
	    cg->y = vals->s3y;
	    cg->width = vals->s3w < 0 ? XtWidth(child) : vals->s3w;
	    cg->height = vals->s3h < 0 ? XtHeight(child) : vals->s3h;
	    cg->request_mode = CWWidth | CWHeight | CWX | CWY;
	}

	DEBUGOUT(_LtDebug2(__FILE__, w, child, "feedback => %s\n",
			   _LtDebugWidgetGeometry2String(cg)));
    }
}


/* Configure the children, or return their geometry if they're the instigator */
/* T. Straumann: there is no field for borderWidth in vals;
 *               however, we can get away with using the value
 *               from cg. The only case when the bw is changed seems
 *               be a geometry request by a child (e.g. as a consequence
 *               of a XtSetValues()). In this case however, we have the
 *               new value at hand, namely in cg.
 *
 *               I also added the child and cg parameters - I don't like
 *               macros with this kind of side effects.
 */
#define	CONF(wid, xx, yy, ww, hh, child, cg)				\
    {								\
	DEBUGOUT(_LtDebug2(__FILE__, w, wid,                    \
                           "CONF: x %d y %d w %d h %d\n",   \
			   xx, yy, ww, hh));	\
	if (child && wid == child)		\
	{								\
	    XtX(wid) = (cg->request_mode & CWX ? cg->x : xx);	\
	    XtY(wid) = (cg->request_mode & CWY ? cg->y : yy);	\
		if ( cg->request_mode & CWBorderWidth )				\
		{\
			XtBorderWidth(wid) = cg->border_width;			\
		}\
	    XtWidth(wid) = (cg->request_mode & CWWidth ? cg->width : ww) - 2 * XtBorderWidth(wid);\
	    XtHeight(wid) = (cg->request_mode & CWHeight ? cg->height : hh) - 2 * XtBorderWidth(wid);\
	}								\
	else							\
	{								\
	    _XmConfigureObject(	wid,	\
							xx, yy,	\
							ww - 2 * XtBorderWidth(wid), hh - 2 * XtBorderWidth(wid),\
							XtBorderWidth(wid));			\
	}								\
    }

static void
_XmMainWindowConfigureChildren(Widget w, Widget child,
			       XtWidgetGeometry *cg, XmMWValues * vals)
{
    if (VALID(MW_MenuBar(w)))
    {
	CONF(MW_MenuBar(w), vals->mbx, vals->mby, vals->mbw, vals->mbh, child, cg);
    }
    if (MW_ShowSep(w) && VALID(MW_Sep1(w)))
    {
	CONF((Widget)MW_Sep1(w), vals->s1x, vals->s1y, vals->s1w, vals->s1h, child, cg);
    }
    if (VALID(MW_CommandWindow(w)))
    {
	CONF(MW_CommandWindow(w), vals->cwx, vals->cwy, vals->cww, vals->cwh, child, cg);
    }
    if (MW_ShowSep(w) && VALID(MW_Sep2(w)))
    {
	CONF((Widget)MW_Sep2(w), vals->s2x, vals->s2y, vals->s2w, vals->s2h, child, cg);
    }
    if (VALID(MW_MessageWindow(w)))
    {
	CONF(MW_MessageWindow(w), vals->mwx, vals->mwy, vals->mww, vals->mwh, child, cg);
    }
    if (MW_ShowSep(w) && VALID(MW_Sep3(w)))
    {
	CONF((Widget)MW_Sep3(w), vals->s3x, vals->s3y, vals->s3w, vals->s3h, child, cg);
    }

    /* T. Straumann: let the ScrolledW do its part of the job
     * NOTE: this is _ugly_, should be a method, but alas, we must not
     *               add to ScrolledWP.h
     * Hey, what about an class extension record?
     */
    _XmScrolledWConfigureChildren(w, child, cg, (XmSWValues *) vals);
}

static XtGeometryResult
geometry_manager(Widget w,
		 XtWidgetGeometry *desired,
		 XtWidgetGeometry *allowed)
{
    XmMWValues vals;
    Widget mw = XtParent(w);
    XtWidgetGeometry wants;
    XtGeometryResult rval;

    DEBUGOUT(_LtDebug2(__FILE__, XtParent(w), w,
		       "geometry_manager request %s\n",
		       _LtDebugWidgetGeometry2String(desired)));

#define Wants(flag)     (wants.request_mode & flag)

    wants = *desired;

    if (Wants(XtCWQueryOnly))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Geometry Mgr Query Only Unimplemented\n"));
	return XtGeometryYes;
    }
    if (Wants(CWX) || Wants(CWY))
	return XtGeometryNo;

    /*
     * Special case: Work Window trying to resize itself in XmAUTOMATIC
     * (T. Straumann: copied this from ScrolledW.c; consult comments there)
     */
    if (SW_ScrollPolicy(mw) == XmAUTOMATIC && w == (Widget)SW_ClipWindow(mw))
    {
	DEBUGOUT(_LtDebug2(__FILE__, mw, w,
			   "BEGIN AUTOMATIC FAKE\n\n"));

	DEBUGOUT(_LtDebug2(__FILE__, mw, w,
			   "geometry_manager: resize WorkWindow: %d %d\n",
			   wants.width, wants.height));
	/* bypass the clipwindow; the _real_ instigator is the work window */
	_XmMainWindowPreferredSize(mw, SW_WorkWindow(mw), &wants, &vals);
	_XmMainWindowLayout(mw, SW_WorkWindow(mw), &wants, &vals);
	_XmMainWindowConfigureChildren(mw, SW_WorkWindow(mw), &wants, &vals);
	DEBUGOUT(_LtDebug2(__FILE__, mw, w, "END AUTOMATIC FAKE\n"));
	/* consult ScrolledW.c about this return value */
	return XtGeometryNo;
    }

    /*
     * We control the XY of all children.  Width/Height is all I care about
     */
    _XmMainWindowPreferredSize(mw, w, &wants, &vals);

    /*
     * T. Straumann: we have to do the layout before asking the parent
     *                               for more space. Otherwise, we may end up with a resized
     *               MainW but possibly we'll still reject the child's
     *               request after calculating the layout (--> new window, old
     *               configuration :-( ).
     */
    _XmMainWindowLayout(mw, w, &wants, &vals);

    /* T. Straumann: express this a little nicer */
    rval = ((Wants(CWWidth) && (wants.width != desired->width))
	    || (Wants(CWHeight) && (wants.height != desired->height))
    || (Wants(CWBorderWidth) && (wants.border_width != desired->border_width)))
    /*
     * Try this : if we have a difference, just report "Almost"
     */
#undef DO_RETURN_ALMOST
    /* till 26 may 1999; re-enabled the `almost' return value.
     * This fixed `thot' and I couldn't observe anything strange
     * happening with grace 5.0.2
     * amai 20010228: well, situation is that #undefining it
     * makes emacs 20.x to start up at least instead of an
     * (almost, finished by crash probably) endless loop.
     * Grace 5.1.x (CVS) seems to be still happy, I played with the
     * Zoom function - no problem showed up. I haven't checked 
     * Thot so far.
     */
#ifdef DO_RETURN_ALMOST
    /* rws 20 Nov 1998
       This is causing tons of second request refused messages in xmgrace
       if you click on the Zoom button and then move the cursor around
       the work space
     */
	? XtGeometryAlmost : XtGeometryYes;
#else
	? XtGeometryNo : XtGeometryYes;
#endif

    if (XtGeometryYes == rval)
    {
	/* FIX ME: Pay attention to return code? */
	if (_XmMainWindowGeomRequest(mw, &vals) == XtGeometryYes)
	{
	    /* ok, apply the new layout */
	}
	else
	{
	    /* sigh... they didn't allow the size change. The new layout is actually
	     * a resize...
	     */
	    SW_FromResize(mw) = True;
	    _XmMainWindowPreferredSize(mw, w, &wants, &vals);
	    vals.MwW = XtWidth(mw);
	    vals.MwH = XtHeight(mw);
	    _XmMainWindowLayout(mw, w, &wants, &vals);
	    SW_FromResize(mw) = False;
	    rval = ((Wants(CWWidth) && (wants.width != desired->width))
		    || (Wants(CWHeight) && (wants.height != desired->height))
		    || (Wants(CWBorderWidth) && (wants.border_width != desired->border_width)))
#ifdef DO_RETURN_ALMOST
		? XtGeometryAlmost : XtGeometryYes;
#else
		? XtGeometryNo : XtGeometryYes;
#endif
	}
    }

    /* T. Straumann: we are also able to manage the border width */
    wants.request_mode = desired->request_mode & (CWWidth | CWHeight | CWBorderWidth);

    *allowed = wants;


    if (rval == XtGeometryYes)
    {
	_XmMainWindowConfigureChildren(mw, w, &wants, &vals);
    }
    else if (rval == XtGeometryAlmost)
    {
	DEBUGOUT(_LtDebug2(__FILE__, mw, w,
		       "geometry_manager : child wants %s gets %s => Almost\n",
			   _LtDebugWidgetGeometry2String(&wants),
			   _LtDebugWidgetGeometry2String(desired)));
    }

    /* Strip off unwanted bits */

    allowed->request_mode &= wants.request_mode;
    if (allowed->request_mode & CWX && desired->x == allowed->x)
    {
    	allowed->request_mode &= ~CWX;
    }
    if (allowed->request_mode & CWY && desired->y == allowed->y)
    {
    	allowed->request_mode &= ~CWY;
    }
    if (allowed->request_mode & CWWidth && desired->width == allowed->width)
    {
    	allowed->request_mode &= ~CWWidth;
    }
    if (allowed->request_mode & CWHeight && desired->height == allowed->height)
    {
    	allowed->request_mode &= ~CWHeight;
    }
    return rval;
#undef	Wants
}

Widget
XmCreateMainWindow(Widget parent,
		   char *name,
		   Arg *argList,
		   Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmMainWindowWidgetClass,
			  parent,
			  argList, argcount);

}

Widget
XmMainWindowSep1(Widget widget)
{
    return (Widget)MW_Sep1(widget);
}

Widget
XmMainWindowSep2(Widget widget)
{
    return (Widget)MW_Sep2(widget);
}

Widget
XmMainWindowSep3(Widget widget)
{
    return (Widget)MW_Sep3(widget);
}

void
XmMainWindowSetAreas(Widget widget,
		     Widget menu_bar,
		     Widget command_window,
		     Widget horizontal_scrollbar,
		     Widget vertical_scrollbar,
		     Widget work_region)
{
    Pixel trough;
    Arg args[5];
    int n = 0;

    DEBUGOUT(_LtDebug(__FILE__, widget, "XmMainWindowSetAreas ["));

#define	P(cw, t)						\
    if (cw)							\
	{ DEBUGOUT(_LtDebug0(__FILE__, widget, t, XtName(cw))); }\
    else							\
	{ DEBUGOUT(_LtDebug0(__FILE__, widget, t, ": NULL")); }

    P(menu_bar, " MenuBar %s");
    P(command_window, " CommandWindow %s");
    P(horizontal_scrollbar, " Hor.Scrollbar %s");
    P(vertical_scrollbar, " Vert.Scrollbar %s");
    P(work_region, " WorkRegion %s");
    DEBUGOUT(_LtDebug0(__FILE__, widget, "]\n"));

#if 1
    if (menu_bar)
    {
    	XtSetArg(args[n], XmNmenuBar, menu_bar); n++;
    }
    if (command_window)
    {
    	XtSetArg(args[n], XmNcommandWindow, command_window); n++;
    }
    if (work_region)
    {
    	XtSetArg(args[n], XmNworkWindow, work_region); n++;
    }
    if (horizontal_scrollbar)
    {
    	XtSetArg(args[n], XmNhorizontalScrollBar, horizontal_scrollbar); n++;
    }
    if (vertical_scrollbar)
    {
    	XtSetArg(args[n], XmNverticalScrollBar, vertical_scrollbar); n++;
    }

    XtSetValues(widget, args, n);
#else
    /* rws 11 Sep 1999
       This is _way_ too simplistic. Take a look at set_values. We need to
       take into account the re-parenting of the work window!!!!!
     */
    if (menu_bar)
    {
	MW_MenuBar(widget) = menu_bar;

	if (menu_bar == MW_MessageWindow(widget))
	{
	    MW_MessageWindow(widget) = NULL;
	}
    }
    if (command_window)
    {
	MW_CommandWindow(widget) = command_window;
	if (command_window == MW_MessageWindow(widget))
	{
	    MW_MessageWindow(widget) = NULL;
	}
    }
    if (work_region)
    {
	SW_WorkWindow(widget) = work_region;
	if (work_region == MW_MessageWindow(widget))
	{
	    MW_MessageWindow(widget) = NULL;
	}
    }
    if (horizontal_scrollbar)
    {
	SW_HSB(widget) = (XmScrollBarWidget)horizontal_scrollbar;
	if (horizontal_scrollbar == MW_MessageWindow(widget))
	{
	    MW_MessageWindow(widget) = NULL;
	}
    }
    if (vertical_scrollbar)
    {
	SW_VSB(widget) = (XmScrollBarWidget)vertical_scrollbar;
	if (vertical_scrollbar == MW_MessageWindow(widget))
	{
	    MW_MessageWindow(widget) = NULL;
	}
    }
#endif

    if (horizontal_scrollbar || vertical_scrollbar)
    {
	XmGetColors(XtScreen(widget), CoreColormap(widget),
		    XtBackground(widget), NULL, NULL, NULL, &trough);
    }

    if (horizontal_scrollbar)
    {
	XtVaSetValues(horizontal_scrollbar,
		      XmNforeground, MGR_Foreground(widget),
		      XmNbackground, XtBackground(widget),
		      XmNtroughColor, trough,
		      XmNtopShadowColor, MGR_TopShadowColor(widget),
		      XmNtopShadowPixmap, MGR_TopShadowPixmap(widget),
		      XmNbottomShadowColor, MGR_BottomShadowColor(widget),
		      XmNbottomShadowPixmap, MGR_BottomShadowPixmap(widget),
		      NULL);
    }
    if (vertical_scrollbar)
    {
	XtVaSetValues(vertical_scrollbar,
		      XmNforeground, MGR_Foreground(widget),
		      XmNbackground, XtBackground(widget),
		      XmNtroughColor, trough,
		      XmNtopShadowColor, MGR_TopShadowColor(widget),
		      XmNtopShadowPixmap, MGR_TopShadowPixmap(widget),
		      XmNbottomShadowColor, MGR_BottomShadowColor(widget),
		      XmNbottomShadowPixmap, MGR_BottomShadowPixmap(widget),
		      NULL);
    }

#if 0
    if (XtIsRealized(widget))
    {
	XmMWValues vals;

	_XmMainWindowPreferredSize(widget, NULL, NULL, &vals);

	_XmMainWindowGeomRequest(widget, &vals);

	_XmMainWindowLayout(widget, NULL, NULL, &vals);

	_XmMainWindowConfigureChildren(widget, NULL, NULL, &vals);
    }
#else
#endif
}

static void
insert_child(Widget w)
{
    Widget p = XtParent(w);

    if ((XmIsRowColumn(w) && RC_Type(w) == XmMENU_BAR) ||
	XmIsSeparator(w) || XmIsSeparatorGadget(w))
    {
#define	superclass	(&xmManagerClassRec)
	(*superclass->composite_class.insert_child) (w);
#undef	superclass

	if (XmIsRowColumn(w) && RC_Type(w) == XmMENU_BAR)
	{
	    DEBUGOUT(_LtDebug2(__FILE__, p, w,
			       "insert_child : this is the menu bar\n"));
	    MW_MenuBar(p) = w;
	}
	else if (XmIsSeparator(w) || XmIsSeparatorGadget(w))
	{
	    DEBUGOUT(_LtDebug2(__FILE__, p, w,
			       "insert_child : this is a separator\n"));
	}
	else if (MW_MessageWindow(p) == NULL &&
		 w != MW_CommandWindow(p) &&
		 w != MW_MenuBar(p) &&
		 w != SW_WorkWindow(p) &&
		 w != (Widget)SW_ClipWindow(p) &&
		 w != (Widget)SW_HSB(p) &&
		 w != (Widget)SW_VSB(p))
	{
	    DEBUGOUT(_LtDebug2(__FILE__, p, w,
			       "insert_child : this is the message window\n"));
	    MW_MessageWindow(p) = w;
	}
    }
    else
    {
	/* 
	 * Now we have a scrollbar or a workwindow - let scrolledwindow
	 * handle this
	 */
#define	superclass	(&xmScrolledWindowClassRec)
	(*superclass->composite_class.insert_child) (w);
#undef	superclass

	if (MW_MessageWindow(p) == NULL &&
	    w != MW_CommandWindow(p) &&
	    w != MW_MenuBar(p) &&
	    w != SW_WorkWindow(p) &&
	    w != (Widget)SW_ClipWindow(p) &&
	    w != (Widget)SW_HSB(p) &&
	    w != (Widget)SW_VSB(p))
	{
	    DEBUGOUT(_LtDebug2(__FILE__, p, w,
			       "insert_child : this is the message window\n"));
	    MW_MessageWindow(p) = w;
	}
    }
}

static void
delete_child(Widget w)
{
	Widget	mw = XtParent(w);

	DEBUGOUT(_LtDebug2(__FILE__, mw, w, "DeleteChild\n"));

#define	superclass	(&xmScrolledWindowClassRec)
	(*superclass->composite_class.delete_child) (w);
#undef	superclass

	if (w == MW_MenuBar(mw))
		MW_MenuBar(mw) = NULL;
	else if (w == MW_CommandWindow(mw))
		MW_CommandWindow(mw) = NULL;
	else if (w == MW_MessageWindow(mw))
		MW_MessageWindow(mw) = NULL;
	else if (w == SW_WorkWindow(mw))
		SW_WorkWindow(mw) = NULL;
	else if (w == (Widget)SW_ClipWindow(mw))
		SW_ClipWindow(mw) = NULL;
	else if (w == (Widget)SW_HSB(mw))
		SW_HSB(mw) = NULL;
	else if (w == (Widget)SW_VSB(mw))
		SW_VSB(mw) = NULL;
}
