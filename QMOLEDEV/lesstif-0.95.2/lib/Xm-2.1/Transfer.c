/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Transfer.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $
 *
 * Copyright (C) 2000-2001 LessTif Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Transfer.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $";

/*
 * For now this file contains mostly stubs for the stuff
 * from include/Motif-2.x/Transfer.h
 */

#include <LTconfig.h>

#include <Xm/XmP.h>
#include <Xm/Transfer.h>
#include <Xm/TransferP.h>

#include <XmI/DebugUtil.h>


static void dropInitCB(Widget w, XtPointer c, XtPointer d);


extern void
XmTransferDone(XtPointer transfer_id,
               XmTransferStatus status) {

  _XmWarning(NULL, "XmTransferDone(): not yet implemented!");
}


extern void
XmTransferValue(XtPointer transfer_id,
                Atom target,
                XtCallbackProc proc,
                XtPointer client_data,
                Time time) {

  _XmWarning(NULL, "XmTransferValue(): not yet implemented!");
}


extern void
XmTransferSetParameters(XtPointer transfer_id,
                        XtPointer parm,
                        int parm_fmt,
                        unsigned long parm_length,
                        Atom parm_type) {

  _XmWarning(NULL, "XmTransferSetParameters(): not yet implemented!");
}


extern void
XmTransferStartRequest(XtPointer transfer_id) {

  _XmWarning(NULL, "XmTransferStartRequest(): not yet implemented!");
}


extern void
XmTransferSendRequest(XtPointer transfer_id,
                      Time time) {

  _XmWarning(NULL, "XmTransferStartRequest(): not yet implemented!");
}


extern void
XmeTransferAddDoneProc(XtPointer transfer_id,
                       XmSelectionFinishedProc done_proc)
{
  _XmWarning(NULL, "XmeTransferAddDoneProc(): not yet implemented!");
}


extern Boolean
XmeSecondarySink(Widget widget,
                 Time time)
{
  _XmWarning(NULL, "XmeSecondarySink(): not yet implemented!");
  return False;
}


extern Boolean
XmeSecondarySource(Widget widget,
                   Time time)
{
  _XmWarning(NULL, "XmeSecondarySource(): not yet implemented!");
  return False;
}


extern void
XmeSecondaryTransfer(Widget widget,
                     Atom target,
                     XtEnum op,
                     Time time)
{
  _XmWarning(NULL, "XmeSecondaryTransfer(): not yet implemented!");
}


extern Widget
XmeDragSource(Widget widget,
              XtPointer location_data,
              XEvent *event,
              ArgList args,
              Cardinal arg_count)
{
  _XmWarning(NULL, "XmeDragSource(): not yet implemented!");
  return (Widget)NULL;
}


extern Boolean
XmeNamedSink(Widget widget,
             Atom named_selection,
             XtEnum op,
             XtPointer location_data,
             Time time)
{
  _XmWarning(NULL, "XmeNamedSink(): not yet implemented!");
  return False;
}


extern Boolean
XmeNamedSource(Widget widget,
               Atom named_selection, 
               Time time)
{
  _XmWarning(NULL, "XmeNamedSource(): not yet implemented!");
  return False;
}


extern Boolean
XmePrimarySink(Widget widget,
               XtEnum op, 
               XtPointer location_data,
               Time time)
{
  _XmWarning(NULL, "XmePrimarySink(): not yet implemented!");
  return False;
}


extern Boolean 
XmePrimarySource(Widget widget, Time time)
{
  _XmWarning(NULL, "XmePrimarySource(): not yet implemented!");
  return False;
}


extern Atom
XmeGetEncodingAtom(Widget widget)
{
/* amai: copied from lib/Xm/Selection.c:_XmTextConvert() ... */

    Atom    encoding=0;
    int status;
    char *list = "ABC";
    XTextProperty   text_prop_ret;

    status = XmbTextListToTextProperty( XtDisplay(widget), &list,
    	    	    	    	     1, XTextStyle, &text_prop_ret );
    encoding = 0x270f; /* ? */
    if ( status  == Success )
    {
    	encoding = text_prop_ret.encoding;
    }
    XFree( (char *)text_prop_ret.value );

    return encoding;
}


extern void
XmeDropSink(Widget w,
            ArgList args,
            Cardinal arg_count)
{
#if 0
   XmDropSiteRegister(w, args, arg_count);
#endif

  _XmWarning(NULL, "XmeDropSink(): not yet implemented!");
}


extern Boolean
XmeClipboardSink(Widget w, XtEnum op, XtPointer location_data) {

  _XmWarning(NULL, "XmeClipboardSink(): not yet implemented!");

  return False;
}


extern Boolean
XmeClipboardSource(Widget w, XtEnum op, Time time) {

  _XmWarning(NULL, "XmeClipboardSource(): not yet implemented!");

  return False;
}


/*
 *                 Private stuff
 */


static void
dropInitCB(Widget w, XtPointer c, XtPointer d) {

  /* see docs for XmeDropSink() */
}

