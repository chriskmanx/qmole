/**
 *
 * $Id: TearOff.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright (C) 1996-2001 LessTif Development Team 
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

static const char rcsid[] = "$Id: TearOff.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>

#include <XmI/XmI.h>
#include <Xm/MwmUtil.h>
#include <Xm/Protocols.h>
#include <Xm/TearOffP.h>
#include <Xm/TearOffBP.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/MenuShellP.h>
#include <Xm/PushB.h>
#include <XmI/AtomMgrI.h>

#include <XmI/DebugUtil.h>

#define TEAR_OFF_SHELL_NAME " Tear-off"

XmExcludedParentPaneRec _XmExcludedParentPane =
{
    0, NULL, 0
};

static void
_XmTearOffFocusChangeEventHandler(Widget reportingWidget, XtPointer data,
				  XEvent *event, Boolean *cont)
{
	DEBUGOUT(_LtDebug("TEAROFF", reportingWidget,
		      "_XmTearOffFocusChangeEventHandler() - %s\n",
		      _LtDebugEventType2String(event->type)));
	DEBUGOUT(_LtDebug(__FILE__, reportingWidget,
		      "_XmTearOffFocusChangeEventHandler() - %s\n",
		      _LtDebugEventType2String(event->type)));
	switch(event->type)
	{
	case FocusIn:
	    /*
	    if (MGR_NumChildren(reportingWidget) > 1)
	    {
		_XmMenuArmItem(MGR_Children(reportingWidget)[1]);
	    }
	    */
	    break;
	case FocusOut:
	    /*
	    _XmMenuDisarmItem(MGR_ActiveChild(reportingWidget));
	    */
	    {
	    Cardinal i;
	    Widget w = reportingWidget;

		for (i = 0; i < MGR_NumChildren(w); i++)
		{
		    _XmMenuDisarmItem(MGR_Children(w)[i]);
		}
	    }
	    break;
	default:
		_XmWarning(reportingWidget, "%s(%d) _XmTearOffFocusChangeEventHandler unknown event type",__FILE__, __LINE__);
		break;
	}
}

void
_XmTearOffBtnDownEventHandler(Widget reportingWidget, XtPointer data,
			      XEvent *event, Boolean *cont)
{
Widget menu;

    DEBUGOUT(_LtDebug(__FILE__, reportingWidget,
		      "_XmTearOffBtnDownEventHandler()\n"));
    if (XmIsRowColumn(reportingWidget))
    {
	menu = reportingWidget;
    }
    else
    {
	menu = XtParent(reportingWidget);
    }
    if (RC_TornOff(menu) && !RC_IsArmed(menu))
    {
#if 0
	RCClass_MenuProcs(XtClass(menu))(XmMENU_ARM, menu, NULL);
#else
	{
	/* rws 20 Dec 1997
	   This is pretty much verbatim the MENU_ARM procedure from
	   RowColumn, _except_ for the XCheckWindowEvent call.  The pointer
	   grab seems to send a LeaveWindow event to the reportingWidget
	   which causes it to disarm! (FIX ME (At least have a look!))
	 */
	Widget w = menu;
	Display *dpy = XtDisplay(w);
	/* grab the keyboard and freeze everything so that we can be sure that
	   the next event goes to the right place. */

	_XmGrabKeyboard(w, True,
			GrabModeSync, GrabModeSync,
			CurrentTime);

#if 0
	/* get the window that previously had the focus
	 * before popping up the menu so we can revert
	 * to it after we're done. */
	_XmMenuFocus(w, XmMENU_FOCUS_SAVE, CurrentTime);

	DEBUGOUT(_LtDebug(__FILE__, w, "RC FOCUS SET: %s\n", XtName(w)));

	_XmMenuFocus(w, XmMENU_FOCUS_SET, CurrentTime);
#endif

	XAllowEvents(dpy, AsyncKeyboard, CurrentTime);

	_XmAddGrab(w, True, True);

	/* Initiate a new pointer grab for the menu bar */
	/* Pointer mode will change to Sync anyway with the
	   XAllowEvents. It's also set here, according to xscope - Chris 6/23 */
	_XmGrabPointer(w, True,
		       (ButtonPressMask | ButtonReleaseMask | EnterWindowMask |
			LeaveWindowMask),
		       GrabModeSync,
		       GrabModeAsync,
		       None,
		       _XmGetMenuCursorByScreen(XtScreen(w)),
		       CurrentTime);


	{
	XEvent junk;

		/* rws 20 Dec 1997
		   The pointer grab just generated a LeaveEvent in the
		   reporting widget. This event will cause the button to
		   disarm.  It should stay armed, therefore discard the
		   event.  (FIX ME)
		 */
		XCheckWindowEvent(dpy,XtWindow(reportingWidget),
			LeaveWindowMask, &junk);
	}
	/* Process events until the next button event */
	XAllowEvents(dpy, SyncPointer, CurrentTime);

	RC_SetArmed(w, True);
	}
#endif
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, reportingWidget,
		      "_XmTearOffBtnDownEventHandler() - Not torn/armed\n"));
    }
}

void
_XmTearOffBtnUpEventHandler(Widget reportingWidget, XtPointer data,
			    XEvent *event, Boolean *cont)
{
Widget menu;

    DEBUGOUT(_LtDebug(__FILE__, reportingWidget,
		      "_XmTearOffBtnUpEventHandler()\n"));
    if (XmIsRowColumn(reportingWidget))
    {
	menu = reportingWidget;
    }
    else
    {
	menu = XtParent(reportingWidget);
    }
    if (RC_TornOff(menu) /*&& RC_IsArmed(menu)*/)
    {
	if (!RC_PopupPosted(menu))
	{
	RCClass_MenuProcs(XtClass(menu))(XmMENU_DISARM, menu, NULL);
	}
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, reportingWidget,
		      "_XmTearOffBtnUpEventHandler() - Not torn/armed\n"));
    }
}

void
_XmDestroyTearOffShell(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmDestroyTearOffShell()\n"));
}

void
_XmDismissTearOff(Widget shell, XtPointer closure, XtPointer call_data)
{
    Widget menu;
    ShellWidget sw = (ShellWidget)shell;
    XEvent *event = (XEvent *)call_data;

    DEBUGOUT(_LtDebug(__FILE__, shell, "_XmDismissTearOff()\n"));

    if (XtIsSubclass(shell, transientShellWidgetClass))
    {
	menu = sw->composite.children[0];
    }
    else
    {
	/* Not sure if this ever happens. FIX ME */
	menu = sw->composite.children[1]; /* FIX ME - 0 is the vendor ext */
    }

    _XmRestoreTearOffToMenuShell(menu, event);

    XtPopdown(shell);
    RC_SetTearOffActive(menu, 0);
    /*
    _XmCallRowColumnUnmapCallback(menu, event);
    if (RC_TearOffDeactivate_cb(menu))
    {
    XmAnyCallbackStruct cbs;

	cbs.reason = XmCR_TEAR_OFF_DEACTIVATE;
	cbs.event = event;
	XtCallCallbackList(menu, RC_TearOffDeactivate_cb(menu), (XtPointer)&cbs);
    }
    */
}

void
_XmTearOffInitiate(Widget w, XEvent *event)
{
    Widget menu;
    Widget shell;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTearOffInitiate()\n"));

    _XmUngrabKeyboard(w, CurrentTime);
    _XmUngrabPointer(w, CurrentTime);
    _XmSetInDragMode(w, False);
    menu = XtParent(w);

    if (!XmIsRowColumn(menu))
    {
	DEBUGOUT(_LtDebug2(__FILE__, menu, w, "Parent is not a menu\n"));
	return;
    }

    shell = XtParent(menu);

    /* FIX ME: PopdownEveryone */
    {
#if 1
    Widget tmp;
    Boolean poppedUp;

    tmp = RC_CascadeBtn(menu);
    RC_MenuButtonPopdown(w, event, &poppedUp);
    RC_CascadeBtn(menu) = tmp;
    /*
    if (RC_CascadeBtn(menu))
    {
	tmp = XtParent(RC_CascadeBtn(menu));
	Lab_MenuDisarm(RC_CascadeBtn(menu));
	RC_PopupPosted(tmp) = NULL;
	RCClass_MenuProcs(XtClass(tmp))(XmMENU_DISARM, tmp, NULL);
    }
    */
    RC_TearOffLastSelectToplevel(menu) = RC_LastSelectToplevel(menu);
    /*
    RC_LastSelectToplevel(menu) = menu;
    */
#else
    Widget cb;
    Widget posted = NULL;

	if (RC_CascadeBtn(menu))
	{
	    posted = RC_PopupPosted(XtParent(RC_CascadeBtn(menu)));
	}
	cb = RC_CascadeBtn(menu);
	{
	Boolean poppedUp;

	    RC_MenuButtonPopdown(w, event, &poppedUp);
	}
	RC_CascadeBtn(menu) = cb;
	if (RC_CascadeBtn(menu))
	{
	    RC_PopupPosted(XtParent(RC_CascadeBtn(menu))) = posted;
	}
#endif
    }

    if (RC_ParentShell(menu))
    {
	RC_SetFromInit(menu, 0);
    }
    else
    {
	RC_SetFromInit(menu, 1);
    }

    _XmRestoreTearOffToToplevelShell(menu, event);
#if 1
    RC_CascadeBtn(menu) = NULL;
#if 0
    if (MGR_NumChildren(menu) > 0)
    {
	_XmMenuFocus(menu, XmMENU_FOCUS_SAVE, CurrentTime);
	if (XmGetTearOffControl(menu) != MGR_Children(menu)[0])
	{
	    RC_TearOffFocusItem(menu) = MGR_Children(menu)[0];
	}
	else if (MGR_NumChildren(menu) > 1)
	{
	  /* FIXME: this is just a hack to avoid core dumping when tearing
	   off menus */
	  Widget w=MGR_Children(menu)[1];
	  if (w &&  /* XtSensitive(w)? FIXME */
	      (
	       (XmIsLabel(w) &&
		!((XmLabelClassRec *)(XtClass(w)))->label_class.menuProcs) ||
	       (XmIsLabelGadget(w) &&
		!((XmLabelGadgetClassRec *)(XtClass(w)))->label_class.menuProcs)))
	  {
	    /* it's a label[g] w/o a menuProc */
	    RC_TearOffFocusItem(menu) = NULL;
	  } else {
	    RC_TearOffFocusItem(menu) = w;
	  }
	}
	if (RC_TearOffFocusItem(menu))
	{
		Lab_MenuArm(RC_TearOffFocusItem(menu));
	}
    }
#endif
#endif
    RC_SetTearOffActive(menu, 1);
    if (RC_TearOffActivate_cb(menu))
    {
    XmAnyCallbackStruct cbs;

	cbs.reason = XmCR_TEAR_OFF_ACTIVATE;
	cbs.event = event;
	XtCallCallbackList(menu, RC_TearOffActivate_cb(menu), (XtPointer)&cbs);
    }
    _XmCallRowColumnMapCallback(menu, event);
    _XmMenuFocus(menu, XmMENU_FOCUS_SAVE, CurrentTime);
    RCClass_MenuTraverse(menu, XmTRAVERSE_HOME);
}

void
_XmAddTearOffEventHandlers(Widget w)
{
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmAddTearOffEventHandlers()\n"));
    XtAddEventHandler(w, FocusChangeMask, False, _XmTearOffFocusChangeEventHandler, NULL);
    /*
    XtAddEventHandler(w, ButtonPressMask, False, _XmTearOffBtnDownEventHandler, NULL);
    */
    XtAddEventHandler(w, ButtonReleaseMask, False, _XmTearOffBtnUpEventHandler, NULL);
    for (i = 0; i < MGR_NumChildren(w); i++)
    {
    Widget child = XmIsGadget(MGR_Children(w)[i]) ? XtParent(MGR_Children(w)[i]) : MGR_Children(w)[i];

	XtAddEventHandler(child /*MGR_Children(w)[i]*/, ButtonPressMask, False, _XmTearOffBtnDownEventHandler, NULL);
	XtAddEventHandler(child /*MGR_Children(w)[i]*/, ButtonReleaseMask, False, _XmTearOffBtnUpEventHandler, NULL);
    }
}

Boolean
_XmIsTearOffShellDescendant(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmIsTearOffShellDescendant()\n"));

    return False;
}

void
_XmLowerTearOffObscuringPoppingDownPanes(Widget ancestor,
					 Widget tearOff)
{
    DEBUGOUT(_LtDebug2(__FILE__, ancestor, tearOff,
		       "_XmLowerTearOffObscuringPoppingDownPanes()\n"));
}

void
_XmRestoreExcludedTearOffToToplevelShell(Widget w,
					 XEvent *event)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmRestoreExcludedTearOffToToplevelShell()\n"));
}

static Widget
CreateTopLevelShell(Widget w)
{
    Widget shell;
    int nargs;
    Arg args[10];
    char *to_shell_name;
    char *to_shell_title;
    Atom delete_atom;
    Widget parent_shell;

    DEBUGOUT(_LtDebug(__FILE__, w, "CreateTopLevelShell()\n"));

    /* create the tear off shell's name */
    to_shell_name = XtMalloc(strlen(TEAR_OFF_SHELL_NAME) +
				 (RC_CascadeBtn(w)
				  ? strlen(XtName(RC_CascadeBtn(w))) + 2
				  : 2));

    sprintf(to_shell_name, "%s%s",
		RC_CascadeBtn(w) ? XtName(RC_CascadeBtn(w)) : "",
		TEAR_OFF_SHELL_NAME);
    {
	XmString string;
	unsigned char type;

	if ( RC_Type(w) != XmMENU_POPUP)
	{
	    XtVaGetValues(RC_CascadeBtn(w),
		    XmNlabelString, &string,
		    XmNlabelType, &type,
		    NULL);
	    if (type == XmSTRING)
	    {
		/* Invoke XmString -> String converter */
		Cardinal zero = 0;
		XrmValue from;
		XrmValue to;
		from.addr = (char *)string;

		if (XmCvtXmStringToText(XtDisplay(w), NULL, &zero,
					&from, &to, NULL))
		{
		    to_shell_title = to.addr;
		}
		else
		{
		    /* converter failed - use simple fallback */
		    if ( ! XmStringGetLtoR(string, XmFONTLIST_DEFAULT_TAG,
				    &to_shell_title) )
			{
			to_shell_title = XtNewString("");
			}
		}
		XmStringFree(string);
	    }
	    else
	    {
		to_shell_title = XtNewString("");
	    }
	}
	else
	{
		to_shell_title = XtNewString("");
	}
    }

    /* find the toplevel shell in this heirarchy and make
     * that the parent of the transient shell */
    for (shell = w;
	     !XtIsSubclass(shell, vendorShellWidgetClass) && XtParent(shell);
	     shell = XtParent(shell))
    {
    }

    nargs = 0;
    XtSetArg(args[nargs], XmNdeleteResponse, XmDO_NOTHING); nargs++;
    XtSetArg(args[nargs], XmNallowShellResize, True); nargs++;
    XtSetArg(args[nargs], XmNtransientFor, shell); nargs++;
    XtSetArg(args[nargs], XmNtitle, to_shell_title); nargs++;
    XtSetArg(args[nargs], XmNmwmFunctions,
	 MWM_FUNC_ALL & ~(MWM_FUNC_RESIZE | MWM_FUNC_MINIMIZE |
				  MWM_FUNC_MAXIMIZE)); nargs++;
    XtSetArg(args[nargs], XmNmwmDecorations,
	 MWM_DECOR_BORDER | MWM_DECOR_MENU | MWM_DECOR_TITLE); nargs++;

    parent_shell = XtCreatePopupShell(to_shell_name, transientShellWidgetClass,
					  shell, args, nargs);

    XtFree(to_shell_name);
    XtFree(to_shell_title);

    /*
     * Get a popdown handler too
     */
    delete_atom = XmInternAtom(XtDisplay(parent_shell),
			       _XA_WM_DELETE_WINDOW, False);
    XmAddWMProtocolCallback(parent_shell, delete_atom,
			    _XmDismissTearOff, NULL);

    return parent_shell;

}

void
_XmRestoreTearOffToToplevelShell(Widget w, XEvent *event)
{
    /* w represents the row column */
    Widget menu_shell = XtParent(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmRestoreTearOffToToplevelShell()\n"));

    /* Tear off buttons should not appear in torn off shells */
    if (RC_TearOffControl(w))
    {
	XtUnmanageChild(RC_TearOffControl(w));
    }

    /* if we already torn off */
    if (RC_TornOff(w))
    {
	return;
    }

    /* should we create the shell? */
    if (RC_FromInit(w) || RC_ParentShell(w) == NULL)
    {
	RC_ParentShell(w) = CreateTopLevelShell(w);
    }

    XtUnmanageChild(w);

    XSync(XtDisplay(w), False);

    XGrabServer(XtDisplay(w));

    XSync(XtDisplay(w), False);

    (*((CompositeWidgetClass)XtClass(menu_shell))
     ->composite_class.delete_child) (w);

    /* set our parent to the created transient shell */
    XtParent(w) = RC_ParentShell(w);
    /* POINT AT WHICH PARENT IS CHANGED */

    if (event)
    {
    XtX(XtParent(w)) = event->xbutton.x_root;
    XtY(XtParent(w)) = event->xbutton.y_root;
    }
    XtWidth(XtParent(w)) = XtWidth(w) + 2 * XtBorderWidth(w);
    XtHeight(XtParent(w)) = XtHeight(w) + 2 * XtBorderWidth(w);

    XtRealizeWidget(XtParent(w));

    (*((CompositeWidgetClass)XtClass(XtParent(w)))
     ->composite_class.insert_child) (w);

    /* reparent the window */
    /* there shouldn't be any question over whether or not *we* are realized.
     * otherwise, how'd we get here? Or our parent, as a matter of fact,
     * due to the XtRealizeWidget() above */
#if 0
    if (XtIsRealized(XtParent(w)) & XtIsRealized(w))
#endif
	XReparentWindow(XtDisplay(w),
			XtWindow(w),
			XtWindow(XtParent(w)),
			0, 0);

    XUngrabServer(XtDisplay(w));

    XtManageChild(w);

    XtPopup(XtParent(w), XtGrabNone);

    XFlush(XtDisplay(w));

    RC_SetTornOff(w, 1);
    RC_SetFromInit(w, 0);
    RC_ParentShell(w) = menu_shell;	/* save off the menu shell */
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmRestoreTearOffToToplevelShell() new parent %s\n",
		XtName(XtParent(w))));
    _XmAddTearOffEventHandlers(w);
}

void
_XmRestoreTearOffToMenuShell(Widget w,
			     XEvent *event)
{
    /* w represents the row column */
    Widget transient = XtParent(w);
    Widget menu_shell;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmRestoreTearOffToMenuShell()\n"));

    /* Tear off buttons should appear in menu shells */
    if (!RC_TearOffControl(w))
    {
	/* FIX ME: just manage existing one (if any?) */
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmRestoreTearOffToMenuShell creating"
			  " tear off control\n"));

	RC_TearOffControl(w) =
	    XtVaCreateManagedWidget("TearOffControl",
				    xmTearOffButtonWidgetClass,
				    w,
				    XmNpositionIndex, 0,
				    NULL);
    }

    /* if we're not really in a torn off menu. */
    if (!RC_TornOff(w))
    {
	return;
    }

    if (_XmIsActiveTearOff(w))
    {
    Pixmap MenuImage = (Pixmap)NULL;
    GC gc;
    XGCValues values;
    unsigned long mask = 0;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmRestoreTearOffToMenuShell() - Grab a copy of the transientShell %s %s %d %d\n",
		XtIsManaged(transient) ? "managed" : "not managed",
		XtIsManaged(w) ? "managed" : "not managed",
		XtWidth(transient),
		XtHeight(transient)
		));
	if (RC_TearOffDirty(w))
	{
		XtVaGetValues(transient,
			XmNbackgroundPixmap, &MenuImage,
			NULL);
		XFreePixmap(XtDisplay(transient), MenuImage);
	}
	/* rws 21 Dec 1997
	   We should be able to share the GC from the TearOffButton!!
	 */
	values.subwindow_mode = IncludeInferiors;
	mask = GCSubwindowMode;
	gc = XCreateGC(XtDisplay(transient), XtWindow(transient), mask, &values);
	MenuImage = XCreatePixmap(XtDisplay(transient),
			    XtWindow(transient),
			    XtWidth(transient),
			    XtHeight(transient),
			    transient->core.depth);
	{
	Cardinal i;

	    for (i = 0; i < MGR_NumChildren(w); i++)
	    {
	    	_XmMenuDisarmItem(MGR_Children(w)[i]);
	    }
	}
	XmUpdateDisplay(transient);
	XCopyArea(XtDisplay(transient), XtWindow(transient), MenuImage, gc,
		    0, 0,
		    XtWidth(transient), XtHeight(transient),
		    0, 0);

	{
	/* rws Just so I can tell when the bogus one is displayed */
	XDrawLine(XtDisplay(transient), MenuImage, gc,
		0, 0,
		XtWidth(transient), XtHeight(transient));
	XDrawLine(XtDisplay(transient), MenuImage, gc,
		0, XtHeight(transient),
		XtWidth(transient), 0);
	}

	XFreeGC(XtDisplay(w), gc);
	XtVaSetValues(transient,
		XmNbackgroundPixmap, MenuImage,
		NULL);
	RC_SetTearOffDirty(w, True);
	_XmCallRowColumnUnmapCallback(w, event);
	if (RC_TearOffDeactivate_cb(w))
	{
	XmAnyCallbackStruct cbs;

	    cbs.reason = XmCR_TEAR_OFF_DEACTIVATE;
	    cbs.event = event;
	    XtCallCallbackList(w, RC_TearOffDeactivate_cb(w), (XtPointer)&cbs);
	}
    }

    menu_shell = RC_ParentShell(w);

    if (!MS_PrivateShell(menu_shell))
    {
	XtUnmanageChild(w);
    }

    XSync(XtDisplay(w), False);
    XGrabServer(XtDisplay(w));

    (*((CompositeWidgetClass)XtClass(transient))
     ->composite_class.delete_child) (w);

    /* set our parent back to the menu shell */
    XtParent(w) = menu_shell;
    /* POINT AT WHICH PARENT IS CHANGED */

    XtRealizeWidget(XtParent(w));

    (*((XmMenuShellWidgetClass)XtClass(XtParent(w)))
     ->composite_class.insert_child) (w);

    /* reparent the window */
    /* there shouldn't be any question over whether or not *we* are realized.
     * otherwise, how'd we get here? Or our parent, as a matter of fact,
     * due to the XtRealizeWidget() above */
#if 0
    if (XtIsRealized(XtParent(w)) & XtIsRealized(w))
#endif
	XReparentWindow(XtDisplay(w),
			XtWindow(w),
			XtWindow(XtParent(w)),
			0, 0);

    XUngrabServer(XtDisplay(w));

    XtManageChild(RC_TearOffControl(w));

    XFlush(XtDisplay(XtParent(w)));

    RC_SetTornOff(w, 0);
    RC_ParentShell(w) = transient;	/* save off the transient */
}
