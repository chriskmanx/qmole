/**
 *
 * $Id: MenuUtil.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

static const char rcsid[] = "$Id: MenuUtil.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/TearOffP.h>
#include <Xm/Screen.h>
#include <Xm/ScreenP.h>
#include <Xm/XmosP.h>

#include <XmI/DebugUtil.h>


extern Boolean
_XmMenuGetInPMMode(Widget w)
{
    XmMenuState state = _XmGetMenuState(w);

    return state->MU_InPMMode;
}


extern void
_XmMenuSetInPMMode(Widget w, Boolean flag)
{
    XmMenuState state = _XmGetMenuState(w);

    state->MU_InPMMode = flag;
}


extern Boolean
_XmGetInDragMode(Widget w)
{
    XmMenuState state = _XmGetMenuState(w);

    return state->MU_InDragMode;
}


extern void
_XmSetInDragMode(Widget w, Boolean flag)
{
    XmMenuState state = _XmGetMenuState(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmSetInDragMode() - %s\n",
    		_LtDebugBoolean2String(flag)));
    state->MU_InDragMode = flag;
}


extern Widget
_XmGetRC_PopupPosted(Widget rc)
{
    if (!XmIsRowColumn(rc))
    {
	return NULL;
    }

    if (RC_PopupPosted(rc))
    {
	return XtParent(RC_PopupPosted(rc));
    }
    else
    {
	return NULL;		/* FIX ME */
    }
}


/*
 * A wrapper around the Xt Intrinsic's pointer grabbing. This
 * one retries a failing grab for several times and if all fails
 * spits out a warning message.
 */
extern int
_XmGrabPointer(Widget w,
	       int owner_events,
	       unsigned int event_mask,
	       int pointer_mode,
	       int keyboard_mode,
	       Window confine_to,
	       Cursor cursor,
	       Time time)
{
    int result, retries;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGrabPointer()\n"));

    for (retries = 4; retries >= 0; retries--)
    {
	result = XtGrabPointer(XmIsGadget(w) ? XtParent(w) : w,
			       owner_events, event_mask,
			       pointer_mode, keyboard_mode,
			       confine_to, cursor, time);

	if (result == GrabSuccess)
	{
	    return result;
	}

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmGrabPointer => %s, trying again\n",
			  (result == AlreadyGrabbed) ? "AlreadyGrabbed" :
			  (result == GrabInvalidTime) ? "GrabInvalidTime" :
			  (result == GrabNotViewable) ? "GrabNotViewable" :
			  (result == GrabFrozen) ? "GrabFrozen" : "??"));

	if (retries)
	{
	    _XmSleep(1);
	}
    }

    _XmWarning(w, "Can't grab the pointer.");

    return result;
}


extern void
_XmUngrabPointer(Widget w, Time t)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmUngrabPointer\n"));

    XtUngrabPointer(XmIsGadget(w) ? XtParent(w) : w, t);
}


/*
 * Same as the _XmGrabPointer wrapper above, but this time for grabbing
 * the keyboard.
 */
extern int
_XmGrabKeyboard(Widget widget,
		int owner_events,
		int pointer_mode,
		int keyboard_mode,
		Time time)
{
    int result, retries;

    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmGrabKeyboard()\n"));

    for (retries = 4; retries >= 0; retries--)
    {
	result = XtGrabKeyboard(XmIsGadget(widget) ? XtParent(widget) : widget,
				owner_events,
				pointer_mode, keyboard_mode,
				time);

	if (result == GrabSuccess)
	{
	    return result;
	}

	DEBUGOUT(_LtDebug(__FILE__, widget,
			  "_XmGrabKeyboard : trying again\n"));

	if (retries)
	{
	    _XmSleep(1);
	}
    }

    _XmWarning(widget, "Can't grab the keyboard.");

    return result;
}


extern void
_XmUngrabKeyboard(Widget w, Time t)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmUngrabKeyboard\n"));

    XtUngrabKeyboard(XmIsGadget(w) ? XtParent(w) : w, t);
}


extern void
_XmMenuEscape(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    /* Widget cb = NULL; */
    XmRowColumnWidget rc;

    if (ev && !_XmIsEventUnique(ev))
    {
	return;
    }

    _XmRecordEvent(ev);

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmMenuEscape()\n"));
    DEBUGOUT(_LtDebug("ESC", w, "_XmMenuEscape()\n"));
    if (XmIsRowColumn(w))
    {
	rc = (XmRowColumnWidget)w;
    }
    else
    {
	rc = (XmRowColumnWidget)XtParent(w);
    }
    DEBUGOUT(_LtDebug(__FILE__, w, "    Menu      %s (%s)\n", XtName(rc), _LtDebugRcType2String(RC_Type(rc))));
    DEBUGOUT(_LtDebug("ESC", w, "    Menu      %s (%s)\n", XtName(rc), _LtDebugRcType2String(RC_Type(rc))));

    if (RC_Type(rc) == XmMENU_BAR)
    {
    	if (RC_PopupPosted(rc))
    	{
	    /* This should not happen!  If a menu bar has a menu posted the
	       events should be going to that menu, not the menu bar
	     */
	    RCClass_MenuProcs(XtClass(rc))(XmMENU_BAR_CLEANUP,
						 (Widget)rc,
						 NULL);
    	}
    	else
    	{
    	}
    }
    else if (_XmIsActiveTearOff((Widget)rc))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "    Menu is TornOff\n"));
	DEBUGOUT(_LtDebug("ESC", w, "    Menu is TornOff\n"));
	if (RC_PopupPosted(rc))
	{
	Boolean poppedUp;
	Widget menu = RC_PopupPosted(rc);

	    DEBUGOUT(_LtDebug(__FILE__, w, "    Menu has %s posted\n", XtName(menu)));
	    DEBUGOUT(_LtDebug("ESC", w, "    Menu has %s posted\n", XtName(menu)));
	    while (RC_PopupPosted(menu))
	    {
		menu = RC_PopupPosted(menu);
		DEBUGOUT(_LtDebug(__FILE__, w, "    which has %s posted\n", XtName(menu)));
	    }
	    RC_MenuShellPopdown(RC_CascadeBtn(menu), ev, &poppedUp);
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "    Menu has nothing posted\n"));
	    DEBUGOUT(_LtDebug("ESC", w, "    Menu has nothing posted\n"));
	    if (RC_TornOff(rc))
	    {
		RCClass_MenuProcs(XtClass(rc))(XmMENU_DISARM, (Widget)rc, NULL);
		_XmDismissTearOff(XtParent(rc), NULL, NULL);
	    }
	    else
	    {
	    Boolean poppedUp;
	    Widget cb = RC_CascadeBtn(rc);
	    unsigned char type = XmIsGadget(cb) ? LabG_MenuType(cb) : Lab_MenuType(cb);

		if (type == XmMENU_BAR)
		{
		    RCClass_MenuProcs(XtClass(XtParent(cb)))(XmMENU_BAR_CLEANUP,
							 XtParent(cb),
							 NULL);
		}
		else
		{
		    RC_MenuShellPopdown(cb, ev, &poppedUp);
		    _XmMenuArmItem(cb);
		}
	    }
	}
    }
    else if (RC_CascadeBtn(rc))
    {
    Boolean poppedUp;
    Widget cb = RC_CascadeBtn(rc);
    unsigned char type = XmIsGadget(cb) ? LabG_MenuType(cb) : Lab_MenuType(cb);

	DEBUGOUT(_LtDebug(__FILE__, w, "    Posted by %s (%s)\n", XtName(cb), _LtDebugRcType2String(type)));
	DEBUGOUT(_LtDebug("ESC", w, "    Posted by %s (%s)\n", XtName(cb), _LtDebugRcType2String(type)));

	if (type == XmMENU_BAR)
	{
	    RCClass_MenuProcs(XtClass(XtParent(cb)))(XmMENU_BAR_CLEANUP,
						 XtParent(cb),
						 NULL);
	}
	else
	{
	    RC_MenuShellPopdown(cb, ev, &poppedUp);
	    _XmMenuArmItem(cb);
	}
    }
    else
    {
    Boolean poppedUp;

	DEBUGOUT(_LtDebug(__FILE__, w, "    must be a popup\n"));
    	RC_MenuButtonPopdown(MGR_ActiveChild(rc), ev, &poppedUp);
    }
    return;
}


extern void
_XmMenuTraverseLeft(Widget w,
		    XEvent *ev,
		    String *params,
		    Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmMenuTraverseLeft()\n"));

    if (!_XmIsEventUnique(ev)) return;
    _XmRecordEvent(ev);
    /* rws 27 Mar 1999
       If we are in a menu that also has gadgets, and the gadget is the 
       currently armed item, and the pointer is over a widget in the menu,
       the widget will get the event that should be going to the gadget, as
       it should.  In this case we need to traverse from the gadget.
       (nedit Preferences menu)
     */
    if (MGR_ActiveChild(XtParent(w)) && XmIsGadget(MGR_ActiveChild(XtParent(w))))
    {
	_XmMenuTraversalHandler(XtParent(w), MGR_ActiveChild(XtParent(w)), XmTRAVERSE_LEFT);
    }
    else
    {
	_XmMenuTraversalHandler(XtParent(w), w, XmTRAVERSE_LEFT);
    }
}


extern void
_XmMenuTraverseRight(Widget w,
		     XEvent *ev,
		     String *params,
		     Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmMenuTraverseRight()\n"));

    if (!_XmIsEventUnique(ev)) return;
    _XmRecordEvent(ev);
    if (MGR_ActiveChild(XtParent(w)) && XmIsGadget(MGR_ActiveChild(XtParent(w))))
    {
	_XmMenuTraversalHandler(XtParent(w), MGR_ActiveChild(XtParent(w)), XmTRAVERSE_RIGHT);
    }
    else
    {
	_XmMenuTraversalHandler(XtParent(w), w, XmTRAVERSE_RIGHT);
    }
}


extern void
_XmMenuTraverseUp(Widget w,
		  XEvent *ev,
		  String *params,
		  Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmMenuTraverseUp()\n"));

    if (!_XmIsEventUnique(ev)) return;
    _XmRecordEvent(ev);
    if (MGR_ActiveChild(XtParent(w)) && XmIsGadget(MGR_ActiveChild(XtParent(w))))
    {
	_XmMenuTraversalHandler(XtParent(w), MGR_ActiveChild(XtParent(w)), XmTRAVERSE_UP);
    }
    else
    {
	_XmMenuTraversalHandler(XtParent(w), w, XmTRAVERSE_UP);
    }
}


extern void
_XmMenuTraverseDown(Widget w,
		    XEvent *ev,
		    String *params,
		    Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmMenuTraverseDown()\n"));
    DEBUGOUT(_LtDebug2("TRAV", XtParent(w), w, "_XmMenuTraverseDown() - %s %s\n",
    	_XmIsEventUnique(ev) ? "unique" : "not-unique",
    	MGR_ActiveChild(XtParent(w)) ? XtName(MGR_ActiveChild(XtParent(w))) : "no-active"
    	));

    if (!_XmIsEventUnique(ev)) return;
    _XmRecordEvent(ev);
    if (MGR_ActiveChild(XtParent(w)) && XmIsGadget(MGR_ActiveChild(XtParent(w))))
    {
	_XmMenuTraversalHandler(XtParent(w), MGR_ActiveChild(XtParent(w)), XmTRAVERSE_DOWN);
    }
    else
    {
	_XmMenuTraversalHandler(XtParent(w), w, XmTRAVERSE_DOWN);
    }
}


extern void
_XmRC_GadgetTraverseDown(Widget w,
			 XEvent *ev,
			 String *params,
			 Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmRC_GadgetTraverseDown()\n"));
    DEBUGOUT(_LtDebug("TRAV", w, "_XmRC_GadgetTraverseDown() - %s %s\n",
    	_XmIsEventUnique(ev) ? "unique" : "not-unique",
    	MGR_ActiveChild((w)) ? XtName(MGR_ActiveChild((w))) : "no-active"
    	));

    if (!_XmIsEventUnique(ev)) return;
    _XmRecordEvent(ev);
    if (RC_Type(w) != XmMENU_BAR && MGR_ActiveChild(w) /*&& XmIsGadget(MGR_ActiveChild(w))*/)
    {
	_XmMenuTraversalHandler(w, MGR_ActiveChild(w), XmTRAVERSE_DOWN);
    }
}


extern void
_XmRC_GadgetTraverseUp(Widget w,
		       XEvent *ev,
		       String *params,
		       Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmRC_GadgetTraverseUp()\n"));

    if (!_XmIsEventUnique(ev)) return;
    _XmRecordEvent(ev);
    if (RC_Type(w) != XmMENU_BAR && MGR_ActiveChild(w) /*&& XmIsGadget(MGR_ActiveChild(w))*/)
    {
	_XmMenuTraversalHandler(w, MGR_ActiveChild(w), XmTRAVERSE_UP);
    }
}


extern void
_XmRC_GadgetTraverseLeft(Widget w,
			 XEvent *ev,
			 String *params,
			 Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmRC_GadgetTraverseLeft()\n"));

    if (!_XmIsEventUnique(ev)) return;
    _XmRecordEvent(ev);
    if (MGR_ActiveChild(w) /*&& XmIsGadget(MGR_ActiveChild(w))*/)
    {
	_XmMenuTraversalHandler(w, MGR_ActiveChild(w), XmTRAVERSE_LEFT);
    }
}


extern void
_XmRC_GadgetTraverseRight(Widget w,
			  XEvent *ev,
			  String *params,
			  Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmRC_GadgetTraverseRight()\n"));

    if (!_XmIsEventUnique(ev)) return;
    _XmRecordEvent(ev);
    if (MGR_ActiveChild(w) /*&& XmIsGadget(MGR_ActiveChild(w))*/)
    {
	_XmMenuTraversalHandler(w, MGR_ActiveChild(w), XmTRAVERSE_RIGHT);
    }
}


/*
 * apparently has something to do with the MenuProcEntry.
 */
extern XtPointer
_XmGetMenuProcContext(void)
{
    return NULL;
}


/*
 * apparently has something to do with the MenuProcEntry.
 */
extern void
_XmSaveMenuProcContext(XtPointer address)
{
}


extern void
_XmSetMenuTraversal(Widget wid, Boolean traversalOn)
{
}


extern Widget
_XmMenuNextItem(Widget menu, Widget current_item)
{
    int current_item_index;
    int next_item_index;

    /* printf("in XmMenuNextItem\n"); */
    for (current_item_index = 0;
         current_item_index < (int)MGR_NumChildren(menu);
         current_item_index++)
    {
	if (MGR_Children(menu)[current_item_index] == current_item)
	{
	    break;
	}
    }

    {
	int valid_control;

	next_item_index = current_item_index;
	if (MGR_NumChildren(menu) > 1)
	{
	    for (valid_control = current_item_index + 1 < (int)MGR_NumChildren(menu) ? current_item_index + 1 : 0; valid_control != current_item_index; valid_control = valid_control + 1 < MGR_NumChildren(menu) ? valid_control + 1 : 0)
	    {
	      Widget w = MGR_Children(menu)[valid_control];
	      /* printf("Examining %s (valid_control
		 %d)\n",XtName(w),valid_control); */
	      /* printf("Newmenus\n"); */
	    if (w && XtIsSensitive(w) && XtIsManaged(w) &&
		((XmIsLabel(w) && (((XmLabelClassRec *)(XtClass(w)))->label_class.menuProcs) && Prim_TraversalOn(w)) ||
		 (XmIsLabelGadget(w) && (((XmLabelGadgetClassRec *)(XtClass(w)))->label_class.menuProcs) && G_TraversalOn(w))))
		{
		  /* printf("%s passed test\n",XtName(w)); */
		  
		    next_item_index = valid_control;
		    break;
		}
	    }
	}
    }
    /*    printf("returning %s index %d\n",
	   XtName(MGR_Children(menu)[next_item_index]),
	   next_item_index); */
    return(MGR_Children(menu)[next_item_index]);
}


static Widget
_XmMenuPrevItem(Widget menu, Widget current_item)
{
    int current_item_index;
    int next_item_index;

    for (current_item_index = 0; current_item_index < (int)MGR_NumChildren(menu); current_item_index++)
    {
	if (MGR_Children(menu)[current_item_index] == current_item)
	{
	    break;
	}
    }
    {
	int valid_control;

	next_item_index = current_item_index;
	if (MGR_NumChildren(menu) > 1)
	{
	    for (valid_control = current_item_index - 1 >= 0 ? current_item_index - 1 : MGR_NumChildren(menu) - 1; valid_control != current_item_index; valid_control = valid_control - 1 >= 0 ? valid_control - 1 : MGR_NumChildren(menu) - 1)
	    {
	    Widget w = MGR_Children(menu)[valid_control];

    if (w && XtIsSensitive(w) && XtIsManaged(w) &&
	    ((XmIsLabel(w) && (((XmLabelClassRec *)(XtClass(w)))->label_class.menuProcs) && Prim_TraversalOn(w)) ||
	     (XmIsLabelGadget(w) && (((XmLabelGadgetClassRec *)(XtClass(w)))->label_class.menuProcs) && G_TraversalOn(w))))
		{
			next_item_index = valid_control;
			break;
		}
	    }
	}
    }

    return(MGR_Children(menu)[next_item_index]);
}


extern void
_XmMenuDisarmItem(Widget w)
{
    if (w &&
	    ((XmIsLabel(w) && (((XmLabelClassRec *)(XtClass(w)))->label_class.menuProcs)) ||
	     (XmIsLabelGadget(w) && (((XmLabelGadgetClassRec *)(XtClass(w)))->label_class.menuProcs))))
    {
	Lab_MenuDisarm(w);
    }
}


extern void
_XmMenuArmItem(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmMenuArmItem()\n"));
    DEBUGOUT(_LtDebug("MENU", w, "_XmMenuArmItem() - %s\n"
    	));
    if (w && (!XtIsSensitive(w) || !XtIsManaged(w) ||
	    (!(XmIsLabel(w) && (((XmLabelClassRec *)(XtClass(w)))->label_class.menuProcs) && Prim_TraversalOn(w)) &&
	     !(XmIsLabelGadget(w) && (((XmLabelGadgetClassRec *)(XtClass(w)))->label_class.menuProcs) && G_TraversalOn(w)))))
    {
    Widget next;

    	next = _XmMenuNextItem(XtParent(w),w);
    	if (next == w)
    	{
    		w = NULL;
    	}
    	else
    	{
    		w = next;
    	}
    }
    if (w)
    {
	if (XmIsGadget(w))
	{
	    _XmMenuFocus(XtParent(w), XmMENU_FOCUS_SET, CurrentTime);
	}
	else
	{
	    _XmMenuFocus(w, XmMENU_FOCUS_SET, CurrentTime);
	}
	Lab_MenuArm(w);
    }
}


extern void
_XmMenuTraversalHandler(Widget w, Widget pw, XmTraversalDirection direction)
{
    DEBUGOUT(_LtDebug2(__FILE__, w, pw, "_XmMenuTraversalHandler()\n"));
    switch(direction)
    {
    case XmTRAVERSE_DOWN:
    	if ((XmIsCascadeButton(pw) && CB_Submenu(pw)) && RC_Type(w) == XmMENU_BAR)
    	{
	    _XmWarning(pw, "%s(%i) - Traversal down in MENU_BAR not written yet!", __FILE__,__LINE__);
	    DEBUGOUT(_LtDebug2(__FILE__, w, pw, "    Post %s\n", XtName(CB_Submenu(pw))));
	    DEBUGOUT(_LtDebug2(__FILE__, w, pw, "    Arm %s\n", XtName(MGR_Children(CB_Submenu(pw))[0])));
    	}
    	else if (RC_Type(w) == XmMENU_PULLDOWN || RC_Type(w) == XmMENU_POPUP)
    	{
	    DEBUGOUT(_LtDebug2(__FILE__, w, pw, "    Arm %s\n", XtName(_XmMenuNextItem(w, pw))));
	    _XmMenuDisarmItem(pw);
	    _XmMenuArmItem(_XmMenuNextItem(w,pw));

	    DEBUGOUT(_LtDebug2(__FILE__, w, pw, "    Active menu item is now %s\n", XtName(_XmMenuNextItem(w, pw))));
    	}
    	else
    	{
	    _XmWarning(pw, "%s(%i) - Traversal down in this situation not written yet!\n    w = %s pw = %s",
	    		__FILE__,__LINE__,
	    		XtName(w), XtName(pw));
    	}
    	break;
    case XmTRAVERSE_UP:
    	if (RC_Type(w) == XmMENU_PULLDOWN || RC_Type(w) == XmMENU_POPUP)
    	{
	    DEBUGOUT(_LtDebug2(__FILE__, w, pw, "    Arm %s\n", XtName(_XmMenuPrevItem(w, pw))));
	    _XmMenuDisarmItem(pw);
	    _XmMenuArmItem(_XmMenuPrevItem(w,pw));

	    DEBUGOUT(_LtDebug2(__FILE__, w, pw, "    Active menu item is now %s\n", XtName(_XmMenuPrevItem(w, pw))));
    	}
    	else
    	{
	    _XmWarning(pw, "%s(%i) - Traversal up in this situation not written yet!\n    w = %s pw = %s",
	    		__FILE__,__LINE__,
	    		XtName(w), XtName(pw));
    	}
    	break;
    case XmTRAVERSE_LEFT:
    	if (RC_Type(w) == XmMENU_BAR)
    	{
    		Cardinal num_params = 0;

		if (XmIsGadget(_XmMenuPrevItem(w,pw)))
		{
		    (*GC_ArmAndActivate(XtClass(_XmMenuPrevItem(w,pw))))(_XmMenuPrevItem(w,pw), NULL, NULL, &num_params);
		}
		else
		{
		    (*PrimC_ArmAndActivate(XtClass(_XmMenuPrevItem(w,pw))))(_XmMenuPrevItem(w,pw), NULL, NULL, &num_params);
		}
    	}
    	else
    	{
	    if (RC_CascadeBtn(w) && RC_Type(XtParent(RC_CascadeBtn(w))) != XmMENU_BAR)
	    {
	    Widget cb = RC_CascadeBtn(w);
	    Boolean poppedUp;

	    	RC_MenuShellPopdown(cb, NULL, &poppedUp);
	    	_XmMenuArmItem(cb);
	    }
	    else if (RC_LastSelectToplevel(w))
	    {
		if (RC_CascadeBtn(w)  && !RC_TornOff(w))
		{
		    DEBUGOUT(_LtDebug2(__FILE__, w, pw, "    RC_LastSelectTopLevel %s %s\n", 
			    XtName(RC_LastSelectToplevel(w)),
			    /* XtName(MGR_ActiveChild(RC_LastSelectToplevel(w))), */
			    XtName(RC_CascadeBtn(w))));
		    _XmMenuTraversalHandler( RC_LastSelectToplevel(w), 
				    RC_CascadeBtn(w),
				    /* MGR_ActiveChild(RC_LastSelectToplevel(w)), */
				    direction);
		}
	    }
	    else
	    {
	    	_XmWarning(w, "%s:_XmMenuTraversalHandler(%d) - I have no idea what todo!",
	    		__FILE__, __LINE__);
	    }
    	}
    	break;
    case XmTRAVERSE_RIGHT:
    	if (RC_Type(w) == XmMENU_BAR)
    	{
    		Cardinal num_params = 0;

		if (XmIsGadget(_XmMenuNextItem(w,pw)))
		{
		    (*GC_ArmAndActivate(XtClass(_XmMenuNextItem(w,pw))))(_XmMenuNextItem(w,pw), NULL, NULL, &num_params);
		}
		else
		{
		    (*PrimC_ArmAndActivate(XtClass(_XmMenuNextItem(w,pw))))(_XmMenuNextItem(w,pw), NULL, NULL, &num_params);
		}
    	}
    	else if (RC_Type(w) == XmMENU_PULLDOWN || RC_Type(w) == XmMENU_POPUP)
    	{
    		if (XmIsCascadeButton(pw) && CB_Submenu(pw))
    		{
    		Cardinal num_params = 0;

    			(*PrimC_ArmAndActivate(XtClass(pw)))(pw, NULL, NULL, &num_params);
			RCClass_MenuTraverse(CB_Submenu(pw), XmTRAVERSE_HOME);
    		}
    		else if (XmIsCascadeButtonGadget(pw) && CBG_Submenu(pw))
    		{
    		Cardinal num_params = 0;

    			(*GC_ArmAndActivate(XtClass(pw)))(pw, NULL, NULL, &num_params);
			RCClass_MenuTraverse(CBG_Submenu(pw), XmTRAVERSE_HOME);
    		}
    		else
    		{
		    if (RC_CascadeBtn(w)  && !RC_TornOff(w))
		    {
			DEBUGOUT(_LtDebug2(__FILE__, w, pw, "    RC_LastSelectTopLevel %s %s\n", 
			    XtName(RC_LastSelectToplevel(w)),
			    /* XtName(MGR_ActiveChild(RC_LastSelectToplevel(w))), */
			    XtName(RC_CascadeBtn(w))));
			if (RC_Type(RC_LastSelectToplevel(w)) == XmMENU_BAR)
			{
			    DEBUGOUT(_LtDebug2("TRAV", w, pw, "    RC_LastSelectTopLevel %s %s %s posted\n", 
				XtName(RC_LastSelectToplevel(w)),
				XtName(RC_CascadeBtn(w)),
				RC_PopupPosted(w) ? XtName(RC_PopupPosted(w)) : "nothing"
				));
			    _XmMenuTraversalHandler( RC_LastSelectToplevel(w), 
				    RC_CascadeBtn(RC_PopupPosted(RC_LastSelectToplevel(w))),
				    direction);
			}
		    }
    		}
    	}
    	else
    	{
	    _XmWarning(pw, "%s(%i) - Traversal right in this situation not written yet!", __FILE__,__LINE__);
    	}
    	break;
    default:
    	_XmWarning(pw, "%s(%i) - Traversal request in invalid direction", __FILE__,__LINE__);
    	break;
    }
}

static int num_saved = 0;
static String saved_trans[32];


extern void
_XmSaveCoreClassTranslations(Widget widget)
{
    saved_trans[num_saved] = CoreClassTranslations(widget);
    num_saved++;
}


extern void
_XmRestoreCoreClassTranslations(Widget widget)
{
    if (num_saved > 0)
    {
	num_saved--;
	CoreClassTranslations(widget) = saved_trans[num_saved];
    }
}


extern void
XmSetMenuCursor(Display *display, Cursor cursorId)
{
    int	i;

    /* Set the menuCursor in the screen object for each screen in the display.
     * But don't bother with that pesky resource interface.
     */

    if (display && cursorId)
	for (i = ScreenCount(display) - 1; i >= 0; i--)
	    Screen_MenuCursor(XmGetXmScreen(ScreenOfDisplay(display, i))) =
		cursorId;
}


extern Cursor
XmGetMenuCursor(Display *display)
{
    return _XmGetMenuCursorByScreen(DefaultScreenOfDisplay(display));
}


extern XmMenuState
_XmGetMenuState(Widget widget)
{
    XmScreenInfo *info = _XmGetScreenInfo(XmGetXmScreen(XtScreen(widget)));

    return (XmMenuState)info->menu_state;
}

#if XmVERSION > 1
/* Use routines in PopupUtil.c if a 2.0 build */
#else
/* For the prototype police */
extern void _XmPopup(Widget shell, XtGrabKind grab_kind);
extern void _XmPopdown(Widget shell);

extern void
_XmPopup(Widget shell, XtGrabKind grab_kind)
{
    XtPopup(shell, grab_kind);
}


extern void
_XmPopdown(Widget shell)
{
    XtPopdown(shell);
}
#endif /* XmVERSION > 1 */
