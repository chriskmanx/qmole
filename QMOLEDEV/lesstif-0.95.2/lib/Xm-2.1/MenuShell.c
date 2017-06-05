/**
 * 
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/MenuShell.c,v 1.6 2005/03/19 10:02:25 dannybackx Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright © 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/MenuShell.c,v 1.6 2005/03/19 10:02:25 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/RowColumnP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/TransltnsP.h>

#include <Xm/SpecRenderT.h>

#include <XmI/DebugUtil.h>

/* ? */
#undef USE_FOCUS

/* Forward Declarations */
static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);

#if 0
static void expose(Widget w, XEvent *event, Region region);
#endif

static void resize(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);
static void change_managed(Widget w);
static void insert_child(Widget w);

#define Offset(field) XtOffsetOf(XmMenuShellRec, menu_shell.field)

/* Resources for the MenuShell class */
static XtResource resources[] =
{
    {
	XmNdefaultFontList, XmCDefaultFontList, XmRFontList,
	sizeof(XmFontList), Offset(default_font_list),
	XmRString, (XtPointer)NULL
    },
    {
	XmNlabelFontList, XmCLabelFontList, XmRFontList,
	sizeof(XmFontList), Offset(label_font_list),
	XmRFontList, (XtPointer)NULL
    },
    {
	XmNbuttonFontList, XmCButtonFontList, XmRFontList,
	sizeof(XmFontList), Offset(button_font_list),
	XmRFontList, (XtPointer)NULL
    }
};

static void MenuShellPopdownDone(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MenuShellPopdownOne(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MenuShellPopdownEveryone(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void _XmXtMenuPopup(Widget widget, XEvent *event, String *params, Cardinal *num_params);
static void _XmXtMenuPopdown(Widget widget, XEvent *event, String *params, Cardinal *num_params);
static void MenuShellPopupSharedMenuPane(Widget w, Widget w2, XEvent *event);
static XmRenderTable GetRenderTable(Widget w, XtEnum renderTableType);

static XtActionsRec actions[] =
{
    {"MenuShellPopdownOne", MenuShellPopdownOne},
    {"MenuShellPopdownDone", MenuShellPopdownDone},
    {"XtMenuPopup", _XmXtMenuPopup},
    {"XtMenuPopdown", _XmXtMenuPopdown},
    {"ClearTraversal", _XmClearTraversal},
};

static XmBaseClassExtRec _XmMenuSCoreClassExtRec = {
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


XmMenuShellClassRec xmMenuShellClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &overrideShellClassRec,
        /* class_name            */ "XmMenuShell",
	/* widget_size           */ sizeof(XmMenuShellRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal /*XtExposeCompressMultiple*/,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ XtInheritExpose /*expose*/,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmMenuShell_translations,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmMenuSCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager,
        /* change_managed   */ change_managed,
        /* insert_child     */ insert_child,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ NULL,	
    },
    /* Shell class part */
    {
	/* extension        */ NULL,
    },
    /* Override class part */
    {
	/* extension        */ NULL,
    },
    /* XmMenuShell class part */
    {
        /* popdownOne          */ MenuShellPopdownOne, 
        /* popdownEveryone     */ MenuShellPopdownEveryone,
        /* popdownDone         */ MenuShellPopdownDone, 
        /* popupSharedMenupane */ MenuShellPopupSharedMenuPane,
	/* extension           */ NULL,
    },
};

static XmSpecRenderTraitRec _XmMenuShellTraitRec = {
	/* version	*/	0,
	/* cb		*/	GetRenderTable
};

WidgetClass xmMenuShellWidgetClass = (WidgetClass)&xmMenuShellClassRec;


static void
class_initialize(void)
{
	_XmInitializeExtensions();
	_XmMenuSCoreClassExtRec.record_type = XmQmotif;


	if (! XmeTraitSet((XtPointer)xmMenuShellWidgetClass, XmQTspecifyRenderTable,
				(XtPointer)&_XmMenuShellTraitRec)) {
		_XmWarning(NULL, "XmMenuShell ClassInitialize: XmeTraitSet failed\n");
	}
}


static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmMENU_SHELL_BIT);
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

    if (MS_DefaultFontList(new_w) != NULL)
    {
	MS_DefaultFontList(new_w) = XmFontListCopy(MS_DefaultFontList(new_w));
    }
    else
    {
	MS_DefaultFontList(new_w) = _XmGetDefaultFontList(new_w, XmBUTTON_FONTLIST);
    }
    if (MS_ButtonFontList(new_w) == NULL)
    {
	if (MS_DefaultFontList(new_w) != NULL) {
	    MS_ButtonFontList(new_w) = XmFontListCopy(MS_DefaultFontList(new_w));
	} else {
	    MS_ButtonFontList(new_w) = _XmGetDefaultFontList(new_w, XmBUTTON_FONTLIST);
	}
    } else {
	MS_ButtonFontList(new_w) = XmFontListCopy(MS_ButtonFontList(new_w));
    }

    if (MS_LabelFontList(new_w) == NULL) {
	if (MS_DefaultFontList(new_w) != NULL) {
	    MS_LabelFontList(new_w) = XmFontListCopy(MS_DefaultFontList(new_w));
	} else {
	    MS_LabelFontList(new_w) = _XmGetDefaultFontList(new_w, XmLABEL_FONTLIST);
	}
    } else {
	MS_LabelFontList(new_w) = XmFontListCopy(MS_LabelFontList(new_w));
    }

    XtBorderWidth(new_w) = 0;

    /* menu shells must be given non-zero width's and height's when
     * they are initialized, as we create the window (realize the widget) here.
     */

    if (XtWidth(new_w) == 0 || XtHeight(new_w) == 0)
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "Initialize: dimensions %d %d changed to 1x1",
			  XtWidth(new_w), XtHeight(new_w)));

	XtWidth(new_w) = XtHeight(new_w) = 1;
    }

    MS_PrivateShell(new_w) = False;

    MS_FocusData(new_w) = _XmCreateFocusData();
    MS_FocusPolicy(new_w) = XmEXPLICIT;

    XtRealizeWidget(new_w);
}

static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Destroy\n"));
    _XmDestroyFocusData(MS_FocusData(w));

    XmFontListFree(MS_DefaultFontList(w));
    XmFontListFree(MS_ButtonFontList(w));
    XmFontListFree(MS_LabelFontList(w));
}

static void
resize(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Resize -- (%d, %d)\n",
		      XtWidth(w), XtHeight(w)));
}

static void
realize(Widget w,
	XtValueMask *value_mask,
	XSetWindowAttributes *attributes)
{
    /* Motif inherits this method */
    *value_mask = CWBackPixmap | CWBorderPixel | CWBitGravity |
	CWOverrideRedirect | CWEventMask | CWSaveUnder | CWColormap;

    /* FIX ME: might it be that Xt passes these in attributes ? */
    attributes->background_pixmap = None;
    attributes->save_under = True;	/* FIX ME: hardwired? */
    attributes->bit_gravity = NorthWestGravity;
    attributes->override_redirect = True;
    attributes->event_mask = ButtonPressMask | ButtonReleaseMask |
	StructureNotifyMask;

    if (XtWidth(w) == 0)
    {
	XtWidth(w) = 1;
    }
    if (XtHeight(w) == 0)
    {
	XtHeight(w) = 1;
    }

#define superclass (&overrideShellClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass

    DEBUGOUT(_LtDebug(__FILE__, w, "Realize (size %dx%d)\n",
		      XtWidth(w), XtHeight(w)));
}

#if 0
static void
expose(Widget w, XEvent *event, Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Expose\n"));
}
#endif

static Boolean
set_values(Widget current, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
	DEBUGOUT(_LtDebug(__FILE__, new_w, "SetValues\n"));
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

	if (MS_DefaultFontList(new_w) != MS_DefaultFontList(current)) {
		XmFontListFree(MS_DefaultFontList(current));
		MS_DefaultFontList(new_w) = XmFontListCopy(MS_DefaultFontList(new_w));
	}

	if (MS_ButtonFontList(new_w) != MS_ButtonFontList(current)) {
		XmFontListFree(MS_ButtonFontList(current));
		MS_ButtonFontList(new_w) = XmFontListCopy(MS_ButtonFontList(new_w));
	}

	if (MS_LabelFontList(new_w) != MS_LabelFontList(current)) {
		XmFontListFree(MS_LabelFontList(current));
		MS_LabelFontList(new_w) = XmFontListCopy(MS_LabelFontList(new_w));
	}

	return True;
}

static XtGeometryResult
geometry_manager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
    XtGeometryResult res;
    XtWidgetGeometry wants;
    Widget ms = XtParent(w);

    DEBUGOUT(_LtDebug2(__FILE__, ms, w,
		       "geometry_manager: request %s, allow_shell_resize %s\n",
		       _LtDebugWidgetGeometry2String(request),
		       ((XmMenuShellWidget)ms)->shell.allow_shell_resize
		       ? "True"
		       : "False"));

    if ((request->request_mode & (CWWidth | CWHeight)) == 0)
    {
	return XtGeometryYes;
    }

    DEBUGOUT(_LtDebug(__FILE__, ms,
		      "geometry_manager: %s\n",
		      _LtDebugWidgetGeometry2String(request)));

    wants = *request;

    res = _XmMakeGeometryRequest(ms, &wants);

    if (res == XtGeometryNo)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "XtGeometryNo returned... THIS SHOULD NOT HAPPEN\n"));
    }
    *reply = wants;

    if (wants.request_mode & CWWidth)
    {
	XtWidth(w) = wants.width;
    }
    if (wants.request_mode & CWHeight)
    {
	XtHeight(w) = wants.height;
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "geometry_manager: size %dx%d => Yes\n",
		      reply->width, reply->height));

    return XtGeometryYes;
}

static void
change_managed(Widget w)
{
    XtWidgetGeometry geo;
    Widget child;
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "ChangeManaged: trying to find child to manage\n"));

    /* FIX ME: isn't having no managed children perfectly valid ? */
    child = NULL;
    for (i = 0; i < MGR_NumChildren(w); i++)
    {
	DEBUGOUT(_LtDebug2(__FILE__, w,
			   MGR_Children(w)[i], "ChangeManaged [%d] %s\n",
			   i,
			   XtIsManaged(MGR_Children(w)[i])
			   ? "Managed"
			   : "Not Managed"));
	if (XtIsManaged(MGR_Children(w)[i]))
	{

	    if (!MS_PrivateShell(w))
	    {
		child = MGR_Children(w)[i];
		break;
	    }
	    /* I don't think this is really necessary */
	    else
	    {
		Widget tmp = MGR_Children(w)[i];

		if (RC_Type(tmp) != XmMENU_POPUP && RC_CascadeBtn(tmp))
		{
		    if (XmIsCascadeButton(RC_CascadeBtn(tmp)) &&
			CB_IsArmed(RC_CascadeBtn(tmp)))
		    {
			child = tmp;
			break;
		    }
		    else if (XmIsCascadeButtonGadget(RC_CascadeBtn(tmp)) &&
			     CBG_IsArmed(RC_CascadeBtn(tmp)))
		    {
			child = tmp;
			break;
		    }
		}
	    }
	}
#if 0
	/* rws 26 Sep 1999
	   This is for http://www.bartels.de/baedform.htm. It seems that it
	   un-manages the menus somewhwhere along the line. We manage them
	   in insert_child, and expect them to stay managed.
	 */
	 /* rws 29 Sep 1999
	    However this messes up Mozilla's Go menu, sigh. So instead just
	    make sure the menu is managed in MenuShellPopupSharedMenuPane
	  */
	else
	{
	    if (MS_PrivateShell(w))
	    {
		if (!XtIsManaged(MGR_Children(w)[i]))
		{
		    XtManageChild(MGR_Children(w)[i]);
		}
	    }
	}
#endif
    }

    if (!child)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "change_managed: no managed children so we must"
			  " be popping down\n"));

 	if (Shell_PoppedUp(w))
	{
	    if (MS_PrivateShell(w))
	    {
		DEBUGOUT(_LtDebug(__FILE__, w, "  PrivateShell\n"));

		_XmXtMenuPopdown(w, NULL, NULL, NULL);
	    }
	    else
	    {
		DEBUGOUT(_LtDebug(__FILE__, w, "  public shell\n"));
		_XmRemoveGrab(w);

		XtUnmapWidget(w);
		XtCallCallbacks(w, XmNpopdownCallback, NULL);
		Shell_PoppedUp(w) = False;

		if (_XmIsActiveTearOff(MGR_Children(w)[0]))
		{
		Widget rc = MGR_Children(w)[0];

		    RCClass_MenuProcs(XtClass(rc))(XmMENU_RESTORE_TEAROFF_TO_TOPLEVEL_SHELL,
					     rc, NULL);
		}
	    }
	}

	return;
    }

    /* MenuShell's take their height/width from the child */
    /* MLM: either of these work, I think, but for now... */
    geo.width = XtWidth(child) == 0 ? 1 : XtWidth(child);
    geo.height = XtHeight(child) == 0 ? 1 : XtHeight(child);

    /* FIX ME: should look at the flags */
    _XmResizeObject(w, geo.width, geo.height, 0);

    DEBUGOUT(_LtDebug2(__FILE__, w, child,
		       "ChangeManaged width %d height %d\n",
		       XtWidth(w), XtHeight(w)));

    /* MenuShell children should be at 0,0 positions !
     * Danny 17/11/96 */
    /* Actually they are at -border_width (similiar to Vendor.c)
     * rws 24 Feb 1197 */
    _XmMoveObject(child, -XtBorderWidth(child), -XtBorderWidth(child));

    if (RC_Type(child) == XmMENU_POPUP)
    {
	XmMenuState state = _XmGetMenuState(child);

	DEBUGOUT(_LtDebug2(__FILE__, w, child, "Popping up\n"));

	_XmPostPopupMenu(child,
			 (XEvent *)&state->RC_ButtonEventStatus.event);

	state->RC_ButtonEventStatus.event.type = 0;
    }
    else
    {
	/* FIX ME: skip if shared menu shell. Children of shared menu shells
	 * do get managed upon creation. More correctly: in insert_child.
	 */
	if (RC_Type(child) == XmMENU_PULLDOWN)
	{
	    DEBUGOUT(_LtDebug(__FILE__, child, "Pulldown posting\n"));

	    if (RC_CascadeBtn(child))
	    {
	        DEBUGOUT(_LtDebug(__FILE__, child, "Child is cascade\n"));
		if (XtIsManaged(child))
		{
	            DEBUGOUT(_LtDebug(__FILE__, w, "Popping up\n"));

		    if (MS_PrivateShell(w))
		    {
			DEBUGOUT(_LtDebug(__FILE__, w, "Popping up private shell\n"));
			MSClass_PopupSharedMenuPane(w)(w, child, NULL);
		    }
		    else
		    {
			DEBUGOUT(_LtDebug(__FILE__, w, "Popping up public shell\n"));
		    	XtManageChild(child);
			XMapRaised(XtDisplay(w), XtWindow(w));
			XtCallCallbacks(w, XmNpopupCallback, NULL);
#ifdef	NEW_MAPCALLBACK
    /*
    _XmCallRowColumnMapCallback(w2, event);
    */
#endif
			Shell_PoppedUp(w) = True;
		    }
		}
		else
		{
	            DEBUGOUT(_LtDebug(__FILE__, w, "Popping down\n"));

		    if (MS_PrivateShell(w))
		    {
			DEBUGOUT(_LtDebug(__FILE__, w, "  PrivateShell\n"));
			_XmXtMenuPopdown(w, NULL, NULL, NULL);
		    }
		    else
		    {
			DEBUGOUT(_LtDebug(__FILE__, w, "  public shell\n"));
			if (RC_Type(child) == XmMENU_POPUP || RC_Type(child) == XmMENU_OPTION)
			{
			    _XmRemoveGrab(w);
			}

			XtUnmapWidget(w);
			XtCallCallbacks(w, XmNpopdownCallback, NULL);
			XtUnmanageChild(child);
		    }
		}
	    }
	    else
	    {
		DEBUGOUT(_LtDebug(__FILE__, w, "Not cascade\n"));
		/* Not attached to a cascade button. */
		if (XtIsManaged(child))
		{
		    /* Probably the user managed the pulldown after creation. */
		    /* FIX ME: Motif 2.0 does issue a warning here. */
		    DEBUGOUT(_LtDebug(__FILE__, child,
				      "Unmanaging in change_managed...\n"));
		    XtUnmanageChild(child);
		}
	    }
	}
    }
}

static void
insert_child(Widget w)
{
    if (!XmIsRowColumn(w))
    {
	_XmWarning(w,
		   "MenuShell widgets must have a xmRowColumnWidgetClass "
		   "child.");
	return;
    }
    else
    {
	/* T. Straumann: M*TIF enforces 0 borderWidth on MenuShell's child */
	if ( 0 != XtBorderWidth(w) )
	{
		XtVaSetValues(w,XmNborderWidth,0,NULL);
	}
#define superclass (&overrideShellClassRec)
	(*superclass->composite_class.insert_child) (w);
#undef superclass

	/* this might not need to get done.
	 * Does the composite class's insert child realize
	 * a widget if the composite is realized? */
	XtRealizeWidget(w);

	if (MS_PrivateShell(XtParent(w)))
	{
	    XtManageChild(w);
	}
    }
}


static void
MenuShellPopdownDone(Widget w, XEvent *event,
		     String *params, Cardinal *num_params)
{
    Cardinal numparams = 0;
    Widget rc, toplevelrc;
    Widget menu_shell = NULL;
    Cardinal i;
    XmMenuState state = _XmGetMenuState(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "MenuShellPopdownDone()\n"));

    /* make sure the trigger event here hasn't already been processed */
    if (_XmMenuGetInPMMode(w) && event && event->type == ButtonRelease &&
	event->xbutton.time <= state->RC_ButtonEventStatus.time)
    {
      DEBUGOUT(_LtDebug(__FILE__, w, "MenuShellPopdownDone(): exiting b/c already processed\n"));
	return;
    }

#if 0
    /*
     * Don't use an event that's within the multi click time.
     * Hopefully this is an event which we can discard :-)
     * If we get bug reports about things not disappearing when
     * the user is fast, this is the reason.
     * Danny 22/04/1998
     *
     * I also have a bad feeling about the test above. Is it meant to do
     * what I changed it to below ?
     */
    if (_XmMenuGetInPMMode(w) && event && event->type == ButtonRelease &&
	event->xbutton.time <= state->RC_ButtonEventStatus.time
				+ XtGetMultiClickTime(XtDisplay(w)))
    {
      DEBUGOUT(_LtDebug(__FILE__, w, "MenuShellPopdownDone(): exiting b/c user too fast\n"));
	return;
    }
#endif

    /* must be a menu shell here:
    assert(XmIsMenuShell(w)); */

    if (MGR_NumChildren(w) == 0) {
      DEBUGOUT(_LtDebug(__FILE__, w, "MenuShellPopdownDone(): exiting b/c no children\n"));
	return;
    }
    /* Find a child that is managed */
    rc = NULL;
    for (i = 0; i < MGR_NumChildren(w); i++)
    {
	rc = MGR_Children(w)[i];
	if (XmIsRowColumn(rc) && XtIsManaged(rc))
	{
	    break;
	}
    }

    if (!rc)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "MenuShellPopdownDone - NO RC\n"));
	return;
    }

    DEBUGOUT(_LtDebug2(__FILE__, w, rc,
		       "MenuShellPopdownDone - found RC %s, posted: %s\n",
			XtName(rc), RC_PopupPosted(rc) ? XtName(RC_PopupPosted(rc)) : "NULL"));

#if 1
    /*
     * Don't use an event that's within the multi click time.
     * Hopefully this is an event which we can discard :-)
     * If we get bug reports about things not disappearing when
     * the user is fast, this is the reason.
     * Danny 22/04/1998
     *
     * I also have a bad feeling about the test above. Is it meant to do
     * what I changed it to below ?
     */
    /* Danny had this ifdef'd out, but it seems to be exactly what
     * Motif does, so I put it back in.  In Motif, a quick click/release 
     * causes the menu to "stick" open.
     * 	-dwilliss 28-Sep-04
     */
    if (_XmMenuGetInPMMode(w) && event && event->type == ButtonRelease &&
	event->xbutton.time <= state->RC_ButtonEventStatus.time
				+ XtGetMultiClickTime(XtDisplay(w)))
    {
        if (RC_Type(rc) != XmMENU_OPTION)
        {
	   /* Not that this _shouldn't_ be done for option menus, but I'm
	    * not sure if this is the correct way to do it for option menus,
	    * so in that case, leave them alone for now
	    *  -- dwilliss 28-Sep-04
	    */
	    _XmMenuFocus(rc, XmMENU_FOCUS_SAVE, CurrentTime);
	    RCClass_MenuTraverse(rc, XmTRAVERSE_HOME);
	    XAllowEvents(XtDisplay(rc), SyncPointer, event->xbutton.time);
        }
      DEBUGOUT(_LtDebug(__FILE__, w, "MenuShellPopdownDone(): exiting b/c user too fast\n"));
	return;
    }
#endif


    _XmGetActiveTopLevelMenu(rc, &toplevelrc);

    if (!toplevelrc)
    {
	DEBUGOUT(_LtDebug2(__FILE__, w, rc, "No toplevelrc\n"));

#if 0
	/*
	 * This probably only happens for accelerators, and in that
	 * case there's nothing to unmanage.
	 * Removing this call fixes the problem in XmHTML's example_2.
	 * Danny 3/2/1998.
	 */
	XtUnmanageChild(rc);
#endif
	return;			/* FIX ME */
    }

    if (XmIsOptionMenu(toplevelrc))
    {
	if (RC_PopupPosted(toplevelrc))
	{
	    menu_shell = XtParent(RC_PopupPosted(toplevelrc));
	}
	else
	{
	    DEBUGOUT(_LtDebug2(__FILE__, w, rc,
			       "No posted menu\n"));
	    return;
	}
    }
    else
    {
	menu_shell = XtParent(toplevelrc);
    }

    if (XmIsMenuShell(menu_shell))
    {
	/* assert(menu_shell && XmIsMenuShell(menu_shell)); */

	if (Shell_PoppedUp(menu_shell))
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
		      "calling PopdownEveryone on toplevel menushell\n"));

	    MSClass_PopdownEveryone(menu_shell)(menu_shell, event, params, &numparams);
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
		      "MenuShellPopdownDone - MenuShell is not popped up, must have been an accelerator\n"));
	    return;
	}
    }
    else if (XtIsTransientShell(menu_shell))
    {
	MSClass_PopdownEveryone(w)(w, event, params, &numparams);
    }

    /* is a popup? */
    if (_XmMenuGetInPMMode(w))
    {
	Widget msh = XtParent(rc);

	if (msh && XmIsMenuShell(msh) && MS_PrivateShell(msh))
	{
	    _XmXtMenuPopdown(msh, NULL, NULL, NULL);
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, rc, "Unmanaging...\n"));
	    {
	    Widget realpar;

		if (XtIsShell(XtParent(rc)))
		{
		    realpar = XtParent(XtParent(rc));
		}
		else
		{
		    realpar = XtParent(rc);
		}
		DEBUGOUT(_LtDebug("EMACS", realpar, "%s:%s(%d) - UNGRAB %i %i\n", __FILE__, "MenuShellPopdownDone" , __LINE__,
			      RC_PostButton(rc), RC_PostModifiers(rc)));
		XtUngrabButton(realpar,
			       RC_PostButton(rc), RC_PostModifiers(rc));
		XtUngrabKeyboard(realpar, CurrentTime);	/* XXXX 761607 */
	    }
	    XtUnmanageChild(rc);
	    if (_XmIsActiveTearOff(rc))
	    {
		DEBUGOUT(_LtDebug(__FILE__, rc, "Unmanaging... ActiveTearOff\n"));
		XtManageChild(rc);
	    }
	}
    }

    /* FIX ME: this is not really elegant. */
    if (RC_LastSelectToplevel(rc) &&
	(XmIsMenuBar(RC_LastSelectToplevel(rc)) ||
	 XmIsOptionMenu(RC_LastSelectToplevel(rc))))
    {
	Widget tmp = RC_LastSelectToplevel(rc);

	DEBUGOUT(_LtDebug(__FILE__, tmp,
			  "  calling menuDisarm on lastSelectTopLevel\n"));
	RCClass_MenuProcs(XtClass(tmp))(XmMENU_DISARM, tmp, NULL);
    }
    else if (XtParent(menu_shell) &&
	     (XmIsMenuBar(XtParent(menu_shell)) ||
	      XmIsOptionMenu(XtParent(menu_shell))))
    {
	Widget tmp = XtParent(menu_shell);

	DEBUGOUT(_LtDebug(__FILE__, tmp,
			  "  calling menuDisarm on XtParent(menu_shell)\n"));

	RCClass_MenuProcs(XtClass(tmp))(XmMENU_DISARM, tmp, NULL);
    }

    _XmMenuSetInPMMode(w, False);

    _XmSetInDragMode(w, False);
}


static void
MenuShellPopdownOne(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
    Widget rc = NULL;

    DEBUGOUT(_LtDebug(__FILE__, w, "MenuShellPopdownOne()\n"));

    /* must be a menu shell here: 
    assert(XmIsMenuShell(w)); */

    if (MGR_NumChildren(w) == 0)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "MenuShellPopdownOne: no children\n"));
	return;
    }

    if (MS_PrivateShell(w) && XmIsRowColumn(XtParent(w)) &&
	XmIsMenuBar(XtParent(w)))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "MenuShellPopdownOne: parent %s\n",
			  XtName(XtParent(w))));
	rc = RC_PopupPosted(XtParent(w));
    }
    else
    {
	rc = MGR_Children(w)[0];
    }

    if (!rc || !XmIsRowColumn(rc))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "MenuShellPopdownOne: no RC\n"));
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "  Child menu pane is %s\n", XtName(rc)));

    /* unhighlight the cascade button that popped us up. */
    if (RC_CascadeBtn(rc))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "  Removing myself from the cascade\n"));

	/* disarm the cascade button that pulled us down. */
	if (XmIsPrimitive(RC_CascadeBtn(rc)))
	{
	    CB_SetArmed(RC_CascadeBtn(rc), False);
	}
	else
	{
	    CBG_SetArmed(RC_CascadeBtn(rc), False);
	}
	XmCascadeButtonHighlight(RC_CascadeBtn(rc), False);

	/* clean up everything so noone thinks we're
	   popped up. */
	RC_PopupPosted(XtParent(RC_CascadeBtn(rc))) = NULL;
	RC_CascadeBtn(rc) = NULL;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "  Popping down\n"));

    if (MS_PrivateShell(w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "  PrivateShell\n"));

	_XmXtMenuPopdown(w, NULL, NULL, NULL);
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "  public shell\n"));
	if (RC_Type(rc) == XmMENU_POPUP || RC_Type(rc) == XmMENU_OPTION)
	{
	    _XmRemoveGrab(w);
	}

	XtUnmapWidget(w);
	XtCallCallbacks(w, XmNpopdownCallback, NULL);
	XtUnmanageChild(rc);
    }
	_XmCallRowColumnUnmapCallback(rc, event);
}


static void
MenuShellPopdownEveryone(Widget w, XEvent *event,
			 String *params, Cardinal *num_params)
{
    Cardinal i;
    Widget rc;

    /* must be a menu shell here:
    assert(XmIsMenuShell(w)); */

    DEBUGOUT(_LtDebug(__FILE__, w, "Popping down everyone - %i kids\n",
    		MGR_NumChildren(w)));

    if (MGR_NumChildren(w) == 0)
    {
	return;
    }

    for (i = 0; i < MGR_NumChildren(w); i++)
    {
	rc = MGR_Children(w)[i];

	DEBUGOUT(_LtDebug2(__FILE__, w, rc,"child %i\n", i));
	if (!rc)
	{
	    continue;
	}

	if (RC_PopupPosted(rc))
	{
	    /* pop down our own sub tree, then ourselves. */
	    Widget shell = XtParent(RC_PopupPosted(rc));

	    if (!shell)
	    {
		continue;
	    }

	    if (XmIsMenuShell(shell))
	    {
		DEBUGOUT(_LtDebug(__FILE__, shell,
			  "  recursing in PopdownEveryone.\n"));
		MenuShellPopdownEveryone(shell, event, params, num_params);
	    }
	}
	DEBUGOUT(_LtDebug(__FILE__, w, "  calling popdownOne.\n"));

	MenuShellPopdownOne(XtParent(rc), event, params, num_params);
#if 0
	_XmCallRowColumnUnmapCallback(rc, event);
#endif
	DEBUGOUT(_LtDebug2(__FILE__, w, rc,"child %i parent %s\n", 
			i,
			XtName(XtParent(rc))));
    }
    for (i = 0; i < MGR_NumChildren(w); i++)
    {
	rc = MGR_Children(w)[i];
	if (_XmIsActiveTearOff(rc))
	{
	    RCClass_MenuProcs(XtClass(rc))(XmMENU_RESTORE_TEAROFF_TO_TOPLEVEL_SHELL,
					 rc, event);
	}
    }
}


static void
MenuShellPopupSharedMenuPane(Widget w, Widget w2, XEvent *event)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Popping up shared pane\n"));

    /* must be a menu shell here:
    assert(XmIsMenuShell(w)); */

    XtManageChild(w2);
    XRaiseWindow(XtDisplay(w2), XtWindow(w2));
    _XmResizeObject(w, XtWidth(w2) == 0 ? 1 : XtWidth(w2), XtHeight(w2) == 0 ? 1 : XtHeight(w2), 0);
 
    XtRealizeWidget(w);
    XMapRaised(XtDisplay(w), XtWindow(w));
    XtCallCallbacks(w, XmNpopupCallback, NULL);
#ifdef	NEW_MAPCALLBACK
    /*
    _XmCallRowColumnMapCallback(w2, event);
    */
#endif
    Shell_PoppedUp(w) = True;
 
#ifdef USE_FOCUS
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "FOCUS SET: %s %s\n", XtName(w), XtName(w2)));

    _XmMenuFocus(w2, XmMENU_FOCUS_SET, CurrentTime);
#endif
 
    XAllowEvents(XtDisplay(w2), SyncPointer/*ReplayPointer quick release problem */, CurrentTime);
 
    _XmAddGrab(w, False, False);
}


static void
_XmXtMenuPopup(Widget widget, XEvent *event,
	       String *params, Cardinal *num_params)
{
    /* for popup menus */
    XtRealizeWidget(widget);
    XMapRaised(XtDisplay(widget), XtWindow(widget));
    XtCallCallbacks(widget, XmNpopupCallback, NULL);
#ifdef	NEW_MAPCALLBACK
    /*
    _XmCallRowColumnMapCallback(w2, event);
    */
#endif
    Shell_PoppedUp(widget) = True;
 
    XAllowEvents(XtDisplay(widget), SyncBoth, event->xbutton.time);

    _XmGrabKeyboard(widget, True, GrabModeSync, GrabModeSync, CurrentTime);

    _XmGrabPointer(widget, True, (ButtonPressMask | ButtonReleaseMask |
                                  EnterWindowMask | LeaveWindowMask),
                   GrabModeSync, GrabModeSync, None,
                   _XmGetMenuCursorByScreen(XtScreen(widget)), CurrentTime);

    _XmAddGrab(widget, True, True);

    XAllowEvents(XtDisplay(widget), SyncPointer, CurrentTime);
}


static void
_XmXtMenuPopdown(Widget widget, XEvent *event,
		 String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmXtMenuPopdown()\n"));
    if (MS_PrivateShell(widget))
    {
        XtUnmapWidget(widget);
	XtCallCallbacks(widget, XmNpopdownCallback, NULL);
	Shell_PoppedUp(widget) = False;

#ifdef USE_FOCUS
#if 0
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  "RESTORE: %s\n", XtName(widget)));

        _XmMenuFocus(widget, XmMENU_FOCUS_RESTORE, CurrentTime);
#endif
#endif

        _XmRemoveGrab(widget);
    }
}


extern void
_XmEnterRowColumn(Widget widget, XtPointer closure,
		  XEvent *event, Boolean *cont)
{
    DEBUGOUT(_LtDebug(__FILE__, widget,
		      "_XmEnterRowColumn(not implemented)\n"));
}


extern void
_XmClearTraversal(Widget wid, XEvent *event,
		  String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, wid,
		      "_XmClearTraversal(not implemented)\n"));

    XAllowEvents(XtDisplay(wid), SyncPointer, CurrentTime);
}


extern void
_XmSetLastManagedMenuTime(Widget wid,
			  Time newTime)
{
    XmMenuState state = _XmGetMenuState(wid);

    state->MS_LastManagedMenuTime = newTime;
}


extern Widget
XmCreateMenuShell(Widget parent, char *name, ArgList arglist, Cardinal argcount)
{
    while (parent && !XtIsComposite(parent))
	parent = XtParent(parent);

    return XtCreatePopupShell(name, xmMenuShellWidgetClass, parent,
			      arglist, argcount);
}

static XmRenderTable GetRenderTable(Widget w, XtEnum renderTableType)
{
	XmMenuShellWidget bb = (XmMenuShellWidget)w;

	switch(renderTableType) {
	case XmLABEL_RENDER_TABLE:
		return bb->menu_shell.label_font_list;
	case XmBUTTON_RENDER_TABLE:
		return bb->menu_shell.button_font_list;
	case XmTEXT_RENDER_TABLE:
		return NULL;	/* ?? FIX ME FIXME */
#if 0
		return bb->menu_shell.text_font_list;
#endif
	}
	return NULL;
}

