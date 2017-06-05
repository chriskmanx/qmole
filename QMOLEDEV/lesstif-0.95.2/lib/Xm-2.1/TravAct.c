/**
 *
 * $Id: TravAct.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $
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

static const char rcsid[] = "$Id: TravAct.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <XmI/TraversalI.h>
#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/ManagerP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/GadgetP.h>
#include <Xm/VendorSEP.h>
#include <X11/X.h>

#include <XmI/DebugUtil.h>

typedef struct _XmFocusFlag
{
    struct _XDisplay *display;
    unsigned short flags;
}
XmFocusFlagRec, *XmFocusFlag;

static XmFocusFlag flag_list = NULL;
static int flag_list_size = 0;

/* For the prototype police */
extern void _XmSetFocusFlag(Widget w, unsigned mask, Boolean value);
extern unsigned short _XmGetFocusFlag(Widget w, unsigned int mask);
extern void _XmPrimitiveFocusInInternal(Widget w, XEvent *event,
					String *params, Cardinal *num_params);

/*
 * apparently a flag is kept on a per- display basis.  Why, I don't
 * know, but I'm trying to find out.  It seems to be related somehow
 * to the help system and focus...  FIX ME
 */
void
_XmSetFocusFlag(Widget w, unsigned mask, Boolean value)
{
    int i = 0;

    for (i = 0; i < flag_list_size; i++)
    {
	if (XtDisplay(w) == flag_list[i].display)
	{
	    if (value)
	    {
		flag_list[i].flags |= mask;
		return;
	    }
	    else
	    {
		flag_list[i].flags &= ~mask;
		return;
	    }
	}
    }

    i = flag_list_size;
    flag_list_size++;
    flag_list = (XmFocusFlag)XtRealloc((char *)flag_list,
				       flag_list_size * sizeof(XmFocusFlagRec));

    flag_list[i].display = XtDisplay(w);
    if (!value)
    {
	flag_list[i].flags = 0;
    }
    else
    {
	flag_list[i].flags = mask;
    }
}

/*
 * this is how we get the flags
 */
unsigned short
_XmGetFocusFlag(Widget w, unsigned int mask)
{
    int i;

    for (i = 0; i < flag_list_size; i++)
    {
	if (flag_list[i].display == XtDisplay(w))
	{
	    return flag_list[i].flags & mask;
	}
    }

    return 0;
}

void
_XmSetFocusResetFlag(Widget w, Boolean value)
{
    _XmSetFocusFlag(w, XmFOCUS_RESET, value);
}

Boolean
_XmGetFocusResetFlag(Widget w)
{
    return _XmGetFocusFlag(w, XmFOCUS_RESET);
}

void
_XmTrackShellFocus(Widget widget,
		   XtPointer client_data,
		   XEvent *event,
		   Boolean *dontSwallow)
{
    XFocusChangeEvent *fev = (XFocusChangeEvent *)event;
    XCrossingEvent *cev = (XCrossingEvent *)event;
    XmVendorShellExtRec *ve = (XmVendorShellExtRec *)client_data;
    XmRelations focal_point;
    XmFocusData fd;

    if (event->type == FocusIn || event->type == FocusOut)
    {
	DEBUGOUT(_LtDebug("RWS", widget,"%s:_XmTrackShellFocus(%d) - %s\n",
	    __FILE__, __LINE__,
	    ve ? XtName(ve) : "NULL"
	    ));
	DEBUGOUT(_LtDebug("RWS", widget,
		   "FOCUS EVENT: Shell: 0x%08x %s\n", widget, XtName(widget)));
	DEBUGOUT(_LtDebug("RWS", widget,
			  " type %s mode %s detail %s window %p(%s)\n",
			  _LtDebugEventType2String(fev->type), 
			  _LtDebugFocusMode2String(fev->mode), 
			  _LtDebugFocusDetail2String(fev->detail),
			  fev->window,
			  XtName(XtWindowToWidget(XtDisplay(widget), fev->window))));
	DEBUGOUT(_LtDebug(__FILE__, widget,
		   "FOCUS EVENT: Shell: 0x%08x %s\n", widget, XtName(widget)));
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  " type %s mode %s detail %s window %p(%s)\n",
			  _LtDebugEventType2String(fev->type), 
			  _LtDebugFocusMode2String(fev->mode), 
			  _LtDebugFocusDetail2String(fev->detail),
			  fev->window,
			  XtName(XtWindowToWidget(XtDisplay(widget), fev->window))));
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  "ENTER/LEAVE EVENT: Shell: 0x%08x %s\n",
			  widget, XtName(widget)));
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  " type %d mode %d detail %d\n",
			  cev->type, cev->mode, cev->detail));
    }

    if (CoreBeingDestroyed(widget))
    {
	*dontSwallow = False;
	return;
    }

    if (ve == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, widget, "_XmTrackShellFocus: ve NULL\n"));
	return;
    }

    if (VSEP_FocusData(ve) == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, widget, "_XmTrackShellFocus: FocusData NULL\n"));
	return;
    }

    fd = VSEP_FocusData(ve);
    focal_point = fd->focal_point;

    switch (event->type)
    {
    case EnterNotify:
    case LeaveNotify:
	if (cev->detail == NotifyInferior)
	{
	    break;
	}
	if (cev->focus)
	{
	    switch (focal_point)
	    {
	    case XmNO_RELATION:
		if (cev->type == EnterNotify)
		{
		    focal_point = XmUNCLE;
		}
		break;

	    case XmUNCLE:
		if (cev->type == LeaveNotify)
		{
		    focal_point = XmNO_RELATION;
		}
		break;

	    default:
		break;
	    }
	}

	break;

    case FocusIn:
	switch (fev->detail)
	{
	case NotifyAncestor:
	case NotifyInferior:
	case NotifyNonlinear:
	    focal_point = XmME;
	    break;

	case NotifyVirtual:
	case NotifyNonlinearVirtual:
	    focal_point = XmNEPHEW;
	    break;

	case NotifyPointer:
	    focal_point = XmUNCLE;
	    break;

	case NotifyPointerRoot:
	case NotifyDetailNone:
	default:
	    break;
	}
	break;

    case FocusOut:
	switch (fev->detail)
	{
	case NotifyAncestor:
	case NotifyVirtual:
	case NotifyNonlinear:
	case NotifyNonlinearVirtual:
	case NotifyPointer:
	    focal_point = XmNO_RELATION;
	    break;

	case NotifyInferior:
	case NotifyPointerRoot:
	case NotifyDetailNone:
	default:
	    break;
	}
	break;

    default:
	break;
    }

    if (focal_point == XmNO_RELATION)
    {
	fd->old_focus_item = NULL;
	if (fd->tree.num_entries != 0)
	{
	     DEBUGOUT(_LtDebug(__FILE__, widget,
		"_XmTrackShellFocus: _XmFreeTravGraph\n"));
	    _XmFreeTravGraph(&fd->tree);
	}
    }

    DEBUGOUT(_LtDebug("RWS", widget,"%s:_XmTrackShellFocus(%d) - %d %d %d %d %d %s %s\n",
	    __FILE__, __LINE__,
	    fd->focus_policy, XmEXPLICIT,
	    focal_point, fd->focal_point, XmNO_RELATION,
	    fd->focus_item ? XtName(fd->focus_item) : "NULL",
	    fd->focus_item ? (CoreBeingDestroyed(fd->focus_item) ? "des" : "not-des") : ""
	    ));
    if (fd->focus_policy == XmEXPLICIT && focal_point != fd->focal_point &&
	fd->focus_item != NULL)
    {
	if (fd->focal_point == XmNO_RELATION || focal_point == XmNO_RELATION)
	{
	    if (fd->focal_point == XmNO_RELATION)
	    {
		DEBUGOUT(_LtDebug("RWS", widget,
			"_XmTrackShellFocus: _XmCallFocusMoved(NULL, _)\n"));
		DEBUGOUT(_LtDebug(__FILE__, widget,
			"_XmTrackShellFocus: _XmCallFocusMoved(NULL, _)\n"));
		if (!CoreBeingDestroyed(fd->focus_item))
		{
		    _XmCallFocusMoved(NULL, fd->focus_item, event);
		}
		else
		{
		    _XmWarning(widget, "%s:_XmTrackShellFocus(%d) - bad news!!!!",
		    	__FILE__, __LINE__);
		    fd->focus_item = NULL;
		}
	    }
	    else
	    {
		DEBUGOUT(_LtDebug("RWS", widget,
			"_XmTrackShellFocus: _XmCallFocusMoved(_, NULL)\n"));
		DEBUGOUT(_LtDebug(__FILE__, widget,
			"_XmTrackShellFocus: _XmCallFocusMoved(_, NULL)\n"));
		if (!CoreBeingDestroyed(fd->focus_item))
		{
		    _XmCallFocusMoved(fd->focus_item, NULL, event);
		}
		else
		{
		    _XmWarning(widget, "%s:_XmTrackShellFocus(%d) - bad news!!!!",
		    	__FILE__, __LINE__);
		    fd->focus_item = NULL;
		}
	    }
	}
    }

    fd->focal_point = focal_point;
}

static Boolean
set_pointer_item(Widget w, XEvent *event)
{
    XCrossingEvent *ev = (XCrossingEvent *)event;
    XmFocusData fd = _XmGetFocusData(w);

    if (!fd)
    {
	return False;
    }

    fd->flush = True;

    /* if we've already seen it once, skip it */
    if (ev->type == fd->last_enter_leave.type &&
	ev->serial == fd->last_enter_leave.serial &&
	ev->time == fd->last_enter_leave.time &&
	ev->x == fd->last_enter_leave.x &&
	ev->y == fd->last_enter_leave.y)
    {
	return False;
    }

    fd->old_pointer_item = fd->pointer_item;
    fd->pointer_item = w;
    fd->last_enter_leave = *ev;

    return True;
}

static void
flush_pointer_item(Widget w, XEvent *event)
{
    XmFocusData fd = _XmGetFocusData(w);
    XCrossingEvent *ev = (XCrossingEvent *)event;
    XCrossingEvent sev;

    if (!fd)
    {
	return;
    }

    if (!fd->flush)
    {
	return;
    }

    sev = fd->last_enter_leave;
    fd->flush = False;

    sev.serial = ev->serial;
    sev.time = ev->time;
    sev.focus = True;

    XtDispatchEvent((XEvent *)&sev);
}

void
_XmPrimitiveEnter(Widget w, XEvent *event,
		  String *params, Cardinal *num_params)
{
    XCrossingEvent *ev = (XCrossingEvent *)event;

    _XmObjectLock(w);
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmPrimitiveEnter()\n"));

    if (_XmGetFocusPolicy(w) != XmPOINTER)
    {
	_XmObjectUnlock(w);
	return;
    }

    if (ev->focus)
    {
	_XmCallFocusMoved(XtParent(w), w, event);
	_XmWidgetFocusChange(w, XmENTER);
    }

    set_pointer_item(w, event);
    _XmObjectUnlock(w);
}

void
_XmPrimitiveLeave(Widget w, XEvent *event,
		  String *params, Cardinal *num_params)
{
    XCrossingEvent *ev = (XCrossingEvent *)event;

    _XmObjectLock(w);
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmPrimitiveLeave()\n"));

    if (_XmGetFocusPolicy(w) != XmPOINTER)
    {
	_XmObjectUnlock(w);
	return;
    }

    if (ev->focus)
    {
	_XmCallFocusMoved(w, XtParent(w), event);
	_XmWidgetFocusChange(w, XmLEAVE);
    }
    _XmObjectUnlock(w);
}

void
_XmPrimitiveFocusInInternal(Widget w, XEvent *event,
			    String *params, Cardinal *num_params)
{
    XFocusChangeEvent *ev = (XFocusChangeEvent *)event;

    _XmObjectLock(w);
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmPrimitiveFocusInInternal()\n"));

    if (!ev->send_event)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmPrimitiveFocusInInternal: !send_event\n"));
	_XmObjectUnlock(w);
	return;
    }

    if (_XmGetFocusFlag(w, XmFOCUS_IGNORE))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmPrimitiveFocusInInternal: FOCUS_IGNORE\n"));
	_XmObjectUnlock(w);
	return;
    }

    if (_XmGetFocusPolicy(w) != XmEXPLICIT)
    {

	DEBUGOUT(_LtDebug(__FILE__, w,
			"_XmPrimitiveFocusInInternal: Not EXPLICIT policy\n"));

	if (!XtIsShell(XtParent(w)))
	{
	    _XmObjectUnlock(w);
	    return;
	}

	flush_pointer_item(w, event);

	_XmObjectUnlock(w);
	return;
    }

    if (_XmGetActiveTabGroup(w) == NULL)
    {

	DEBUGOUT(_LtDebug(__FILE__, w,
			"_XmPrimitiveFocusInInternal: No Active Tab Group\n"));

	_XmMgrTraversal(_XmFindTopMostShell(w), XmTRAVERSE_NEXT_TAB_GROUP);

	_XmObjectUnlock(w);
	return;
    }

    _XmWidgetFocusChange(w, XmFOCUS_IN);
    _XmObjectUnlock(w);
}

void
_XmPrimitiveFocusOut(Widget w, XEvent *event,
		     String *params, Cardinal *num_params)
{
    XFocusChangeEvent *ev = (XFocusChangeEvent *)event;

    _XmObjectLock(w);
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmPrimitiveFocusOut(): send_event: %d\n",
		      ev->send_event));

    if (!ev->send_event)
    {
	_XmObjectUnlock(w);
	return;
    }

    if (CoreBeingDestroyed(w))
    {
	_XmObjectUnlock(w);
	return;
    }

    if (_XmGetFocusPolicy(w) != XmEXPLICIT)
    {
	_XmObjectUnlock(w);
	return;
    }

    _XmWidgetFocusChange(w, XmFOCUS_OUT);
    _XmObjectUnlock(w);
}

void
_XmPrimitiveFocusIn(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
    _XmObjectLock(w);
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmPrimitiveFocusIn()\n"));

    _XmPrimitiveFocusInInternal(w, event, params, num_params);
    _XmObjectUnlock(w);
}

void
_XmPrimitiveUnmap(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    _XmObjectLock(w);
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmPrimitiveUnmap()\n"));

    _XmValidateFocus(w);
    _XmObjectUnlock(w);
}

void
_XmEnterGadget(Widget w, XEvent *event,
	       String *params, Cardinal *num_params)
{
    XmFocusData fd;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGadgetEnter()\n"));

    if (_XmGetFocusPolicy(w) != XmPOINTER)
    {
	return;
    }

    if ((fd = _XmGetFocusData(w)) == NULL)
    {
	return;
    }

    if (fd->focal_point == XmNO_RELATION)
    {
	return;
    }

    _XmCallFocusMoved(fd->old_focus_item, w, event);

    _XmWidgetFocusChange(w, XmENTER);
}

void
_XmLeaveGadget(Widget w, XEvent *event,
	       String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGadgetLeave()\n"));

    if (_XmGetFocusPolicy(w) != XmPOINTER)
    {
	return;
    }

    _XmCallFocusMoved(w, XtParent(w), event);
    _XmWidgetFocusChange(w, XmLEAVE);
}

void
_XmFocusInGadget(Widget w, XEvent *event,
		 String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGadgetFocusIn()\n"));

    if (_XmGetFocusPolicy(w) != XmEXPLICIT)
    {
	return;
    }

    _XmWidgetFocusChange(w, XmFOCUS_IN);
}

void
_XmFocusOutGadget(Widget w, XEvent *event,
		  String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGadgetFocusOut()\n"));

    if (_XmGetFocusPolicy(w) != XmEXPLICIT)
    {
	return;
    }

    _XmWidgetFocusChange(w, XmFOCUS_OUT);
}

void
_XmManagerEnter(Widget w, XEvent *event,
		String *params, Cardinal *num_params)
{
    XCrossingEvent *ev = (XCrossingEvent *)event;
    Widget pw;


    if (_XmGetFocusPolicy(w) != XmPOINTER)
    {
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerEnter() return (focusPolicy != POINTER)\n"));
	return;
    }

    if (!set_pointer_item(w, event))
    {
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerEnter() return (! set_pointer_item)\n"));
	return;
    }

    if (!ev->focus)
    {
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerEnter() return (!ev->focus)\n"));
	return;
    }

    if (ev->detail == NotifyInferior)
    {
	pw = XtWindowToWidget(ev->display, ev->subwindow);
    }
    else
    {
	pw = XtParent(w);
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerEnter() : XmCallFocusMoved, XmWidgetFocusChange\n"));
    _XmCallFocusMoved(pw, w, event);

    _XmWidgetFocusChange(w, XmENTER);
}

void
_XmManagerLeave(Widget w, XEvent *event,
		String *params, Cardinal *num_params)
{
    XCrossingEvent *ev = (XCrossingEvent *)event;
    Widget pw;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerLeave()\n"));

    if (ev->type != LeaveNotify)
    {
	return;
    }

    if (_XmGetFocusPolicy(w) != XmPOINTER)
    {
	return;
    }

    if (ev->detail == NotifyInferior)
    {
	pw = XtWindowToWidget(ev->display, ev->subwindow);
    }
    else
    {
	pw = XtParent(w);
    }

    if (!set_pointer_item(w, event))
    {
	return;
    }

    if (!ev->focus)
    {
	return;
    }

    _XmCallFocusMoved(w, pw, event);

    _XmWidgetFocusChange(w, XmLEAVE);
}

void
_XmManagerFocusInInternal(Widget w, XEvent *event,
			  String *params, Cardinal *num_params)
{
    XFocusChangeEvent *ev = (XFocusChangeEvent *)event;
    Widget tg;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerFocusInInternal()\n"));

    if (!ev->send_event)
    {
	return;
    }

    if (_XmGetFocusFlag(w, XmFOCUS_RESET | XmFOCUS_IGNORE))
    {
	return;
    }

    if (_XmGetFocusPolicy(w) == XmPOINTER)
    {
	flush_pointer_item(w, event);
	return;
    }

    if ((tg = _XmGetActiveTabGroup(w)) == NULL)
    {
	w = _XmFindTopMostShell(w);
	_XmMgrTraversal(w, XmTRAVERSE_NEXT_TAB_GROUP);
	return;
    }

    if (MGR_ActiveChild(w) != NULL && XmIsGadget(MGR_ActiveChild(w)))
    {
	_XmDispatchGadgetInput(MGR_ActiveChild(w), event, XmFOCUS_IN_EVENT);
    }

    _XmWidgetFocusChange(w, XmFOCUS_IN);
}

void
_XmManagerFocusIn(Widget w, XEvent *event,
		  String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerFocusIn()\n"));

    _XmManagerFocusInInternal(w, event, params, num_params);
}

void
_XmManagerFocusOut(Widget w, XEvent *event,
		   String *params, Cardinal *num_params)
{
    XFocusChangeEvent *ev = (XFocusChangeEvent *)event;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerFocusOut()\n"));

    if (!ev->send_event)
    {
	return;
    }

    if (_XmGetFocusFlag(w, XmFOCUS_IGNORE))
    {
	return;
    }

    if (_XmGetFocusPolicy(w) != XmEXPLICIT)
    {
	return;
    }

    if (MGR_ActiveChild(w) != NULL && XmIsGadget(MGR_ActiveChild(w)))
    {
	_XmDispatchGadgetInput(MGR_ActiveChild(w), event, XmFOCUS_OUT_EVENT);
    }

    _XmWidgetFocusChange(w, XmFOCUS_OUT);
}

void
_XmManagerUnmap(Widget w, XEvent *event,
		String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerUnmap()\n"));

    _XmValidateFocus(w);
}
