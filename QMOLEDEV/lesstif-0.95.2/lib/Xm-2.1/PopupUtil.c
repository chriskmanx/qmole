/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PopupUtil.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1998 Free Software Foundation, Inc.
 * Copyright (C) 1998-2001 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PopupUtil.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

/*
 * This code borrows heavily from Popup.c.  The copyright there is as follows:
 */

/***********************************************************

Copyright (c) 1987, 1988, 1994  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.


Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Digital not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include <LTconfig.h>

/* Define USE_XT_POPUP if you want to use Xt's version */
/* define USE_XT_POPUP */

#include <Xm/XmP.h>
#include <XmI/PopupUtil.h>

#ifdef USE_XT_POPUP
#else
#include <XmI/MessagesI.h>
#include <X11/StringDefs.h>
#endif

#include <XmI/DebugUtil.h>


/* This file contains functions from previous versions that are now obsolete */

/*
  MODULE: PopupUtil.c
  DESCRIPTION:
  This module contains routines that reimplement the XtPopup family of 
  routines. 
  END:
*/

extern String XtCXtToolkitError;  /* this should be put somewhere in XmI */
/* Public routine(s): */

/*
  FUNCTION: _LTPopup
  SYNOPSIS: static void _LTPopup(Widget w, XtGrabKind grab_kind, Boolean spring_loaded)
  DESCRIPTION:
  Similar to _XtPopup
  END:
*/
static void
_LTPopup(Widget w, XtGrabKind grab_kind, Boolean spring_loaded)
{
    ShellWidget shell_widget = (ShellWidget) w;

    if (!XtIsShell(w)) 
    {
        XtAppErrorMsg(XtWidgetToApplicationContext(w),
                      "invalidClass",
                      "xmPopup",
                      XtCXtToolkitError,
#if XmVERSION > 1
                      _XmMsgMenuShell_0002,
#else
                      "XmPopup requires a subclass of shellWidgetClass"
#endif
                      (String *)NULL, 
                      (Cardinal *)NULL);
    }

    if (!shell_widget->shell.popped_up) 
    {
        XtGrabKind call_data = grab_kind;
        XtCallCallbacks(w, XtNpopupCallback, (XtPointer)&call_data);
        shell_widget->shell.popped_up = TRUE;
        shell_widget->shell.grab_kind = grab_kind;
        shell_widget->shell.spring_loaded = spring_loaded;
        if (shell_widget->shell.create_popup_child_proc != NULL) 
        {
            (*(shell_widget->shell.create_popup_child_proc))(w);
        }

        if (grab_kind == XtGrabExclusive) 
        {
            _XmAddGrab(w, TRUE, spring_loaded);
        } 
        else if (grab_kind == XtGrabNonexclusive) 
        {
            _XmAddGrab(w, FALSE, spring_loaded);
        }

        XtRealizeWidget(w);
        XMapRaised(XtDisplay(w), XtWindow(w));
    } 
    else
    {
        XRaiseWindow(XtDisplay(w), XtWindow(w));
    }
}

#if XtSpecificationRelease < 6
    /* Not available in R5 */
#else
    /* This is an R6 thing */
/*
  FUNCTION: _LTCallChangeHookCallbacks
  SYNOPSIS: static void _LTCallChangeHookCallbacks(Widget w, int type, XtPointer edata)
  DESCRIPTION:
  This function calls the XtNchangeHook callbacks.
  END:
*/
static void
_LTCallChangeHookCallbacks(Widget w, String type, XtPointer edata)
{
    Widget hookobj;
    XtCallbackList changehook_callbacks;

    hookobj = XtHooksOfDisplay(XtDisplay(w));
    if (XtHasCallbacks(hookobj, XtNchangeHook) == XtCallbackHasSome) {
        XtChangeHookDataRec call_data;

        call_data.type = type;
        call_data.widget = w;
        call_data.event_data = edata;

        XtVaGetValues(hookobj, XtNchangeHook, &changehook_callbacks, NULL);

        XtCallCallbackList(hookobj, changehook_callbacks, (XtPointer)&call_data);
    }
}
#endif

/*
  FUNCTION: _XmPopup
  SYNOPSIS: void _XmPopup(Widget w, XtGrabKind kind)
  DESCRIPTION:
  This function is similar to XtPopup.
  END:
*/
void
_XmPopup(Widget w, XtGrabKind kind)
{
#ifdef USE_XT_POPUP
    XtPopup(w, kind);
#else
    _LTPopup(w, kind, False);
#if XtSpecificationRelease < 6
#else
    _LTCallChangeHookCallbacks(w, XtHpopup, (XtPointer) kind);
#endif
#endif
}

/*
  FUNCTION: _XmPopupSpringLoaded
  SYNOPSIS: void _XmPopupSpringLoaded(Widget w)
  DESCRIPTION:
  This function is similar to XtPopupSpringLoaded.
  END:
*/
void
_XmPopupSpringLoaded(Widget w)
{
#ifdef USE_XT_POPUP
    XtPopupSpringLoaded(w);
#else
    _LTPopup(w, XtGrabExclusive, True);
#if XtSpecificationRelease < 6
#else
    _LTCallChangeHookCallbacks(w, XtHpopupSpringLoaded, (XtPointer) NULL);
#endif
#endif
}

/*
  FUNCTION: _XmPopdown
  SYNOPSIS: void _XmPopdown(Widget w)
  DESCRIPTION:
  This function is similar to XtPopdown
  END:
*/
void
_XmPopdown(Widget w)
{
#ifdef USE_XT_POPUP
    XtPopdown(w);
#else

    /* Unmap a shell widget if it is mapped, and remove from grab list */
    ShellWidget shell_widget = (ShellWidget) w;

    if (!XtIsShell(w)) 
    {
        XtAppErrorMsg(XtWidgetToApplicationContext(w),
                      "invalidClass",
                      "xmPopdown",
                      XtCXtToolkitError,
#if XmVERSION > 1
                      _XmMsgMenuShell_0003,
#else
                      "XmPopdown requires a subclass of shellWidgetClass",
#endif
                      (String *)NULL, 
                      (Cardinal *)NULL);
    }

    if (shell_widget->shell.popped_up) 
    {
        XtGrabKind grab_kind = shell_widget->shell.grab_kind;
        XWithdrawWindow(XtDisplay(w), 
                        XtWindow(w),
                        XScreenNumberOfScreen(XtScreen(w)));
        if (grab_kind != XtGrabNone) 
        {
            _XmRemoveGrab(w);
        }
        shell_widget->shell.popped_up = FALSE;
        XtCallCallbacks(w, XtNpopdownCallback, (XtPointer)&grab_kind);
    }

#if XtSpecificationRelease < 6
#else
    _LTCallChangeHookCallbacks(w, XtHpopdown, (XtPointer) NULL);
#endif
#endif
}
