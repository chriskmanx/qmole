/**
 *
 * $Header: /cvsroot/lesstif/lesstif/test/common/Test.h,v 1.23 2002/05/01 15:19:16 amai Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
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

#ifndef _TEST_TEST_H
#define _TEST_TEST_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Test.c */
extern int GlobalErrors;

int  LessTifTestGetSlop(Widget w);
void LessTifTestSetSlop(Widget w, int slop);
void LessTifTestDelay(Widget w, unsigned long interval);
int  LessTifTestResizeWidget(Widget w, Dimension wt, Dimension ht);
int  LessTifTestPushButton(Widget w);
int  LessTifTestWaitForIt(Widget w);
int  LessTifTestMainLoop(Widget w);
void LessTifTestFlushEvents(Widget w);
void LessTifTestWarpPointerAbove(Widget w);
void LessTifTestWarpPointer(Widget w);
void LessTifTestKeysym(Widget w, KeySym keysym);
void LessTifTestEsc(Widget w);
void LessTifTestBtnDown(Widget w, int button);
void LessTifTestBtnUp(Widget w, int button);
void LessTifTestBtn1Down(Widget w);
void LessTifTestBtn1Up(Widget w);
void LessTifTestBtn2Down(Widget w);
void LessTifTestBtn2Up(Widget w);
void LessTifTestBtn3Down(Widget w);
void LessTifTestBtn3Up(Widget w);
void LessTifTestPrintEvents(Widget w, Boolean flag);
void LessTifTestKeyPress(Widget w, KeySym keysym, unsigned int state);
void LessTifTestKeyRelease(Widget w, KeySym keysym, unsigned int state);

const char *XdbGeometryResult2String(XtGeometryResult r);
const char *XdbWidgetGeometry2String(XtWidgetGeometry *g);
const char *XdbBoolean2String(int b);


/* XdbPrintTree.c */
void XdbPrintTree(Widget w);
void XdbPrintCompleteTree(Widget w);
void XdbPrintTreeCB(Widget w, XtPointer client, XtPointer call);

/* XdbPrintResources.c */
void XdbPrintResources(Widget w);

/* DumpLayout.c */
void PrintDetails(Widget W, XtWidgetGeometry *Expected);
void PrintDetails2(Widget W, XtWidgetGeometry *Expected);


/* 
   Note: It's generally considered a BadIdea(tm) to put functions in
   header files, because some programs (perhaps lint or ctrace) don't like
   that.  Some C compilers may not like it either.  However, this function
   can't be in the library, because it needs to be defined differently
   depending on if we're compiling a Motif or a LessTif version of the tests.
   Therefore, for now, we're putting it here.  If it breaks something, we'll
   have to figure out something else.  This function is so simple that it
   could be a macro anyway, so that may be a solution. 

   This code *CANNOT* be rolled into Test.c, because Test.c is compiled only
   once, and will pick up only the Motif or the LessTif headers, and these
   defines may differ between them.  Changing this would require changing
   *all* the Makefile.am headers in the test tree.
*/

#ifndef LIB_LTTEST
const char *
XdbReason2String(int reason)
{
     switch (reason) {
     case XmCR_NONE:                   return "XmCR_NONE";
     case XmCR_HELP:                   return "XmCR_HELP";
     case XmCR_VALUE_CHANGED:          return "XmCR_VALUE_CHANGED";
     case XmCR_INCREMENT:              return "XmCR_INCREMENT";
     case XmCR_DECREMENT:              return "XmCR_DECREMENT";
     case XmCR_PAGE_INCREMENT:         return "XmCR_PAGE_INCREMENT";
     case XmCR_PAGE_DECREMENT:         return "XmCR_PAGE_DECREMENT";
     case XmCR_TO_TOP:                 return "XmCR_TO_TOP";
     case XmCR_TO_BOTTOM:              return "XmCR_TO_BOTTOM";
     case XmCR_DRAG:                   return "XmCR_DRAG";
     case XmCR_ACTIVATE:               return "XmCR_ACTIVATE";
     case XmCR_ARM:                    return "XmCR_ARM";
     case XmCR_DISARM:                 return "XmCR_DISARM";
#ifdef LesstifVersion
     case XmCR_DUMMY13:                return "XmCR_DUMMY13"; 
     case XmCR_DUMMY14:                return "XmCR_DUMMY14"; 
     case XmCR_DUMMY15:                return "XmCR_DUMMY15";
#endif
     case XmCR_MAP:                    return "XmCR_MAP";
     case XmCR_UNMAP:                  return "XmCR_UNMAP";
     case XmCR_FOCUS:                  return "XmCR_FOCUS";
     case XmCR_LOSING_FOCUS:           return "XmCR_LOSING_FOCUS";
     case XmCR_MODIFYING_TEXT_VALUE:   return "XmCR_MODIFYING_TEXT_VALUE";
     case XmCR_MOVING_INSERT_CURSOR:   return "XmCR_MOVING_INSERT_CURSOR";
     case XmCR_EXECUTE:                return "XmCR_EXECUTE";
     case XmCR_SINGLE_SELECT:          return "XmCR_SINGLE_SELECT";
     case XmCR_MULTIPLE_SELECT:        return "XmCR_MULTIPLE_SELECT";
     case XmCR_EXTENDED_SELECT:        return "XmCR_EXTENDED_SELECT";
     case XmCR_BROWSE_SELECT:          return "XmCR_BROWSE_SELECT";
     case XmCR_DEFAULT_ACTION:         return "XmCR_DEFAULT_ACTION";
     case XmCR_CLIPBOARD_DATA_REQUEST: return "XmCR_CLIPBOARD_DATA_REQUEST";
     case XmCR_CLIPBOARD_DATA_DELETE:  return "XmCR_CLIPBOARD_DATA_DELETE";
     case XmCR_CASCADING:              return "XmCR_CASCADING";
     case XmCR_OK:                     return "XmCR_OK";
     case XmCR_CANCEL:                 return "XmCR_CANCEL";
#ifdef LesstifVersion
     case XmCR_DUMMY33:                return "XmCR_DUMMY33"; 
#endif
     case XmCR_APPLY:                  return "XmCR_APPLY";
     case XmCR_NO_MATCH:               return "XmCR_NO_MATCH";
     case XmCR_COMMAND_ENTERED:        return "XmCR_COMMAND_ENTERED";
     case XmCR_COMMAND_CHANGED:        return "XmCR_COMMAND_CHANGED";
     case XmCR_EXPOSE:                 return "XmCR_EXPOSE";
     case XmCR_RESIZE:                 return "XmCR_RESIZE";
     case XmCR_INPUT:                  return "XmCR_INPUT";
     case XmCR_GAIN_PRIMARY:           return "XmCR_GAIN_PRIMARY";
     case XmCR_LOSE_PRIMARY:           return "XmCR_LOSE_PRIMARY";
     case XmCR_CREATE:                 return "XmCR_CREATE";
     case XmCR_TEAR_OFF_ACTIVATE:      return "XmCR_TEAR_OFF_ACTIVATE";
     case XmCR_TEAR_OFF_DEACTIVATE:    return "XmCR_TEAR_OFF_DEACTIVATE";
     case XmCR_OBSCURED_TRAVERSAL:     return "XmCR_OBSCURED_TRAVERSAL";
#ifndef     XmCR_PROTOCOLS
#define     XmCR_PROTOCOLS     6666
#endif
     case XmCR_PROTOCOLS:              return "XmCR_PROTOCOLS";

     default:                          return "???";
     }
}
#endif /* #ifndef LIB_LTTEST */


#ifdef __cplusplus
}
#endif

#endif /* _TEST_TEST_H */
