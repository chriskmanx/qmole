/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/DialogS.c,v 1.3 2007/02/04 13:26:15 dannybackx Exp $
 *
 * derived from Xt Vendor class.c 
 *
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/DialogS.c,v 1.3 2007/02/04 13:26:15 dannybackx Exp $";

/*
 * This controls questionable behavior.  It will go away
 *
#define USE_SHELL_PARENT
 */

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/DialogSP.h>
#include <Xm/DialogSEP.h>
#include <Xm/VendorSP.h>
#include <Xm/VendorSEP.h>
#include <Xm/BulletinBP.h>
#include <Xm/DisplayP.h>
#include <X11/StringDefs.h>
#include <X11/ShellP.h>

#include <XmI/DebugUtil.h>


static void class_initialize(void);
static void class_part_initialize(WidgetClass wclass);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void change_managed(Widget w);
static void insert_child(Widget w);
static void delete_child(Widget w);
static void StructureNotifyHandler(Widget w, XtPointer closure,
				   XEvent *event, Boolean *cont);
static void WmProtocolHandler(Widget w, XtPointer client, XtPointer call);
static Boolean set_values(Widget old, Widget req, Widget new_w,
			  ArgList args, Cardinal *num_args);

static XmBaseClassExtRec _XmDialogSCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ XmInheritInitializePrehook,
    /* set_values_prehook        */ XmInheritSetValuesPrehook,
    /* initialize_posthook       */ XmInheritInitializePosthook,
    /* set_values_posthook       */ XmInheritSetValuesPosthook,
    /* secondary_object_class    */ (WidgetClass)&xmDialogShellExtClassRec,
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
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

XmDialogShellClassRec xmDialogShellClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &transientShellClassRec,
        /* class_name            */ "XmDialogShell",
        /* size                  */ sizeof(XmDialogShellRec),
        /* class_initialize      */ class_initialize,
        /* class_part_initialize */ class_part_initialize,
        /* class_inited          */ False,
        /* initialize            */ initialize,
      	/* initialize_hook       */ NULL,		
      	/* realize               */ XtInheritRealize,
      	/* actions               */ NULL,
      	/* num_actions           */ 0,
      	/* resources             */ NULL,
      	/* resource_count        */ 0,
      	/* xrm_class             */ NULLQUARK,
      	/* compress_motion       */ False,
      	/* compress_exposure     */ True,
      	/* compress_enterleave   */ False,
      	/* visible_interest      */ False,
      	/* destroy               */ NULL,
      	/* resize                */ XtInheritResize,
      	/* expose                */ NULL,
      	/* set_values            */ set_values,
      	/* set_values_hook       */ NULL,			
      	/* set_values_almost     */ XtInheritSetValuesAlmost,  
      	/* get_values_hook       */ NULL,
      	/* accept_focus          */ NULL,
      	/* intrinsics version    */ XtVersion,
      	/* callback offsets      */ NULL,
      	/* tm_table              */ XtInheritTranslations,
      	/* query_geometry        */ XtInheritQueryGeometry,
      	/* display_accelerator   */ NULL,
      	/* extension             */ (XtPointer)&_XmDialogSCoreClassExtRec
    },
    /* Composite Class Part */
    {
	/* geometry_manager */ XtInheritGeometryManager,
        /* change_managed   */ change_managed,
        /* insert_child     */ insert_child,
        /* delete_child     */ delete_child,
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
    /* TransientShell Class Part */
    {
	/* extension */ NULL
    },
    /* XmDialogShell Class Part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmDialogShellWidgetClass = (WidgetClass) (&xmDialogShellClassRec);

/*
 * resources 
 */
static XtResource ext_resources[] = {
    {
        XmNdeleteResponse, XmCDeleteResponse, XmRDeleteResponse,
        sizeof(unsigned char),
	XtOffsetOf(XmDialogShellExtRec, vendor.delete_response),
        XmRImmediate, (XtPointer)XmUNMAP
    },
};

XmDialogShellExtClassRec xmDialogShellExtClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmVendorShellExtClassRec,
        /* class_name            */ "XmDialogShellExtClass",
	/* widget_size           */ sizeof(XmDialogShellExtRec),
	/* class_initialize      */ NULL,
	/* class_part_initialize */ NULL,
	/* class_inited          */ False,
	/* initialize            */ NULL,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ ext_resources,
	/* num_resources         */ XtNumber(ext_resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ 0,
	/* compress_exposure     */ 0,
	/* compress_enterleave   */ 0,
	/* visible_interest      */ 0,
	/* destroy               */ NULL,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ NULL,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
        /* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* XmExtObject part */
    {
        /* syn_resources      */ NULL,
        /* num_syn_resources  */ 0,
        /* extension          */ NULL
    },
    /* Desktop Class part */
    {
        /* child_class           */ NULL,
        /* insert_child          */ XmInheritWidgetProc,
        /* delete_child          */ XmInheritWidgetProc,
        /* extension             */ NULL
    },
    /* ShellExt Class part */
    {
	/* structure_notify   */ StructureNotifyHandler,
	/* extension          */ NULL
    },
    /* VendorClassExt Part */
    {
	/* delete_window_handler */ WmProtocolHandler,
	/* offset_handler        */ NULL,
	/* extension             */ NULL
    },
    /* DialogShellExt Part */
    {
	/* extension          */ NULL
    }
};


WidgetClass xmDialogShellExtObjectClass = (WidgetClass)&xmDialogShellExtClassRec;

static void 
class_initialize(void)
{
    int ncom, addon;
    XtResourceList combined, shells;
    Cardinal nshells, i, j;

    _XmDialogSCoreClassExtRec.record_type = XmQmotif;

    ncom = XtNumber(ext_resources) +
                xmVendorShellExtClassRec.object_class.num_resources;

    _XmTransformSubResources(xmVendorShellExtClassRec.object_class.resources,
                             xmVendorShellExtClassRec.object_class.
				num_resources,
                             &shells, &nshells);

    combined = (XtResourceList)XtMalloc(sizeof(XtResource) * ncom);

    memcpy(combined, shells, nshells * sizeof(XtResource));

    /* can't just copy.  We know we need to drop one */
    for (i = 0, addon = 0; i < XtNumber(ext_resources); i++)
    {
	for (j = 0; j < nshells; j++)
	{
	    if (strcmp(combined[j].resource_name,
		       ext_resources[i].resource_name) == 0)
	    {
		combined[j] = ext_resources[i];
		break;
	    }
	}

	if (j == nshells)
	{
	    combined[nshells + addon] = ext_resources[i];
	    addon++;
	}
    }

    XtFree((char *)shells);

    xmDialogShellExtClassRec.object_class.resources = combined;
    xmDialogShellExtClassRec.object_class.num_resources = nshells + addon;

}

static void 
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmDIALOG_SHELL_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Widget par;

    par = _XmFindTopMostShell(XtParent(new_w));

    if (par && XtIsRealized(par))
    {
	Arg args[2];
	int argc = 0;

	XtSetArg(args[argc], XmNtransientFor, par);
	argc++;
	XtSetArg(args[argc], XmNwindowGroup, XtWindow(par));
	argc++;
	XtSetValues(new_w, args, argc);
    }

    if (XtWidth(new_w) == 0)
    {
	XtWidth(new_w) = 1;
    }
    if (XtHeight(new_w) == 0)
    {
	XtHeight(new_w) = 1;
    }
}


static Widget
GetChild(Widget w)
{
    CompositeWidget cw = (CompositeWidget)w;
    Cardinal i;
    extern WidgetClass xmVendorShellExtObjectClass;	/* FIX ME */

    /*
     * LessTif implementation dependency : vendor shell extension objects must
     * be ignored
     */
    for (i = 0; i < cw->composite.num_children; i++)
    {
	if ((!XtIsSubclass(cw->composite.children[i],
			   xmVendorShellExtObjectClass)) &&
	    (!XtIsSubclass(cw->composite.children[i], xmDisplayClass)))
	{
	    return cw->composite.children[i];
	}
    }

    return NULL;
}

static Boolean
set_values(Widget old, Widget req, Widget new_w, ArgList args, Cardinal *num_args)
{
    Widget child;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s(%d):set_values - %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(req), XtY(req),
		      XtWidth(req), XtHeight(req),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    if (CoreMappedWhenManaged(old) || !CoreMappedWhenManaged(new_w))
	return False;

    child = GetChild(new_w);
    if (!XtIsManaged(child))
	return False;

    XtPopup(new_w, XtGrabNone);
    return False;
}

static void
change_managed(Widget w)
{
    Widget child;

    child = GetChild(w);

    DEBUGOUT(_LtDebug2(__FILE__, w, child, "ChangeManaged\n"));

    /* not doing this was why popup dialogs wouldn't take focus */
    XtSetKeyboardFocus(w, child);

    if (XtIsManaged(child))
    {
	Boolean InitialPlacement = False;

	DEBUGOUT(_LtDebug2(__FILE__, w, child, "... was not managed\n"));
	if (!XtIsRealized(child))
	{
	    DEBUGOUT(_LtDebug(__FILE__, child, "... Realizing\n"));

	    XtRealizeWidget(child);

	    if (XtX(w) == 0 && XtY(w) == 0)
	    {
		InitialPlacement = True;
	    }
	}

	/* Pick up child size */
	(void)XtMakeResizeRequest(w, XtWidth(child), XtHeight(child),
				  NULL, NULL);

	/* Dialog's child should be at 0,0 */
	if (XtX(child) != 0 || XtY(child) != 0)
	{
	    DEBUGOUT(_LtDebug2(__FILE__, w, child,
		    "Child position %d,%d set to 0,0\n",
		    XtX(child), XtY(child)));
	    _XmMoveObject(child, 0, 0);
	}

	DEBUGOUT(_LtDebug2(__FILE__, w, child, "Shell size %d %d %d %d\n",
			   XtWidth(child), XtHeight(child),
			   XtX(w), XtY(w)));

	if (XmIsBulletinBoard(child) &&
	    (InitialPlacement || BB_DefaultPosition(child)))
	{
	    Position px, py;
	    Widget p;

	    p = XtParent(w);
	    /* XXX XtX and XtY give position of parent relative to its parent.
	      we need it's position relative to the screen. Let XtTranslateCoords do it. */
	    px = (XtWidth(p) - XtWidth(child)) / 2;
	    py = (XtHeight(p) - XtHeight(child)) / 2;
	    if (XtIsRealized(p))
	    {
	       XtTranslateCoords(p, px, py, &px, &py);
	    }

	    DEBUGOUT(_LtDebug(__FILE__, w, "def. pos. %d %d\n", px, py));

	    if (px < 0)
	    {
		px = 0;
	    }
	    if (py < 0)
	    {
		py = 0;
	    }

	    XtMoveWidget(w, px, py);
	    BB_DefaultPosition(child) = False;
	}

	if (XmIsBulletinBoard(child))
	    _XmBulletinBoardMap(child);

	if (CoreMappedWhenManaged(w))
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "XtPopup\n"));

	    XtPopup(w, XtGrabNone);

	    DEBUGOUT(_LtDebug2(__FILE__, w, child,
			       "Mapping shell, just to be sure\n"));

	    XtMapWidget(w);
	}
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "XtPopDown\n"));
	XtPopdown((Widget)w);

	/* rws 1 Jun 1997
	 * Made this a 0 again because any modal grabs added to the shell with
	 * the above XtPopup do not get removed unless there is a corresponding
	 * XtPopdown.
	 * MLM: enabled this again, but moved it to *after* the popdown.  This
	 * fixes certain cases (like nedit->open->ok, with no file selected).
	 */
	DEBUGOUT(_LtDebug(__FILE__, w, "XtUnmapWidget\n"));
	if (XmIsBulletinBoard(child)) {
		_XmBulletinBoardUnmap(child);
	}
	XtUnmapWidget(w);
    }

    _XmNavigChangeManaged(w);
}

static void
insert_child(Widget w)
{
    CompositeWidget p = (CompositeWidget)XtParent(w);

    DEBUGOUT(_LtDebug2(__FILE__, (Widget)p, w, "insert_child\n"));

    /* Avoid nasty side effects with the shell extension object */
    if (!XtIsRectObj(w))
    {
	return;
    }

    if (!XtIsRealized((Widget)p))
    {
	/*
	 * Avoid Xt errors on zero width/height here by
	 * temporarily setting p's width/height to 1 and
	 * restoring them after realize.
	 */
	Dimension ww, hh;

	ww = XtWidth(p);
	hh = XtHeight(p);

	XtWidth(p) = XtHeight(p) = 1;

	XtRealizeWidget((Widget)p);

	XtWidth(p) = ww;
	XtHeight(p) = hh;
    }

#define superclass (&transientShellClassRec)
    (*superclass->composite_class.insert_child) (w);
#undef superclass
}

static void
delete_child(Widget w)
{
    Widget s = XtParent(w);

    DEBUGOUT(_LtDebug2(__FILE__, s, w, "delete_child\n"));
    DEBUGOUT(_LtDebug2("RWS", s, w,"%s:delete_child(%d)\n",
    	__FILE__, __LINE__
    	));

    if (!XtIsRectObj(w))
    {
	return;
    }

    /*
     * The XtIsManaged part is probably never true. Should check Xt manuals.
     * When we have two children, this means only one will be left. We know
     * that's the extension object, so unmap ourselves.
     */
    if (XtIsManaged(w) || MGR_NumChildren(s) == 1)
    {
#if 1
	DEBUGOUT(_LtDebug(__FILE__, s, "XtUnmapWidget\n"));
	XtUnmapWidget(s);
#else
	DEBUGOUT(_LtDebug(__FILE__, s, "XtPopdown\n"));
	XtPopdown(s);
#endif
    }

#define superclass (&transientShellClassRec)
    (*superclass->composite_class.delete_child) (w);
#undef superclass
}

static void
WmProtocolHandler(Widget w, XtPointer client, XtPointer call)
{
    XmVendorShellExtObject ve = (XmVendorShellExtObject)client;
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, w, "Dialog's WmProtocolHandler\n"));

    switch (VSEP_DeleteResponse(ve))
    {
    case XmDESTROY:
	XtDestroyWidget(w);
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "WmProtocolHandler(DeleteResponse XmDESTROY)\n"));
	break;

    case XmUNMAP:
	/* The word says UNMAP but we really have to unMANAGE */
	for (i = 0; i < MGR_NumChildren(w); i++)
	{
	    if (XtIsManaged(MGR_Children(w)[i]))
	    {
		DEBUGOUT(_LtDebug2(__FILE__, w, MGR_Children(w)[i],
				   "XtUnmanageChild(child)\n"));
		XtUnmanageChild(MGR_Children(w)[i]);
		return;
	    }
	}
	break;

    case XmDO_NOTHING:
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "WmProtocolHandler(DeleteResponse XmNO_NOTHING)\n"));
	return;
    }
}

String
_XmMakeDialogName(String name)
{
    String s;

    s = XtMalloc((name ? strlen(name) : 0) + strlen(XmDIALOG_SUFFIX) + 1);
    if (name)
    {
	strcpy(s, name);
    }
    else
    {
	s[0] = '\0';
    }

    strcat(s, XmDIALOG_SUFFIX);

    return s;
}

Widget
XmCreateDialogShell(Widget parent,
		    char *name,
		    Arg *arglist,
		    Cardinal argcount)
{
    Widget composite_parent;

    /*
     * First we find the first widget (starting at the parent argument)
     * that is a composite subclass. We use this as the parent when
     * creating the shell
     * Correction: find shell parent, so that we can correctly set
     * transientFor. -- MLM
     */
    composite_parent = parent;
#ifdef USE_SHELL_PARENT
    while (!XtIsVendorShell(composite_parent))
    {
	composite_parent = XtParent(composite_parent);
    }
#else
    while (!XtIsComposite(composite_parent))
    {
	composite_parent = XtParent(composite_parent);
    }
#endif

    return XtCreatePopupShell(name,
			      xmDialogShellWidgetClass,
			      composite_parent,
			      arglist,
			      argcount);
}

static void
StructureNotifyHandler(Widget w, XtPointer closure,
		       XEvent *event, Boolean *cont)
{
    XConfigureEvent *cev = (XConfigureEvent *)event;
    XMapEvent *mev = (XMapEvent *)event;
    XUnmapEvent *uev = (XUnmapEvent *)event;
    XReparentEvent *rev = (XReparentEvent *)event;

    if (!XtIsSubclass(w, xmDialogShellWidgetClass))
    {
	return;
    }

    switch (event->type)
    {
    case ConfigureNotify:
	DEBUGOUT(_LtDebug(__FILE__, w, "CONFIGURE NOTIFY: layout %d %d %d %d\n",
			  cev->x, cev->y, cev->width, cev->height));
	break;

    case MapNotify:
	DEBUGOUT(_LtDebug(__FILE__, w, "MAP NOTIFY: window %08x\n",
			  mev->window));
	/* rws 22 Sep 1998
	   This is causing nedit's File->Open to un-map as soon as it is
	   mapped.  It seems that XtPopup never gets called for it.  Does
	   nedit map this itself???
	 */
	/*
	if (!Shell_PoppedUp(w))
	{
		XtUnmapWidget(w);
	}
	*/

	break;

    case UnmapNotify:
	DEBUGOUT(_LtDebug(__FILE__, w, "UNMAP NOTIFY: window %08x\n",
			  uev->window));
	break;

    case ReparentNotify:
	DEBUGOUT(_LtDebug(__FILE__, w, "REPARENT NOTIFY: window %08x\n",
			  rev->window));
	break;

    default:
	DEBUGOUT(_LtDebug(__FILE__, w, "Got UNKNOWN TYPE: %d\n",
			  event->type));
	break;
    }
}
