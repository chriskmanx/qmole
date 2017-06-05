/**
 *
 * $Header: /cvsroot/lesstif/lesstif/test/common/DumpLayout.c,v 1.24 2002/05/01 16:01:26 amai Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright (C) 1996-2002 LessTif Development Team
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


/* rws 7 Apr 1997
   It is best to start this with -geometry +500+500 (or whatever)
   otherwise the time to place it by the window manager bungs up the
   first test

   Is there a event, or callback one can use to figure out when the
   window has been placed????
 */
 
#include "LTTconfig.h"

#include <stdlib.h>
#include <stdio.h>

#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#define LIB_LTTEST
#include "Test.h"

typedef struct
{
    Widget W;
    XtWidgetGeometry *Expected;
    int version;
} Request;

static Boolean GlobalResult = True;
static int slop;


static void
PrintWidgetDetails(Widget W, XtWidgetGeometry *ExpectedResults)
{
    static int result_index = 0;
    static int recurse_level = 0;
    XtWidgetGeometry geo;
    Boolean Result;
    int i;
    WidgetList kids;
    Cardinal numkids = 0;
    XtGeometryMask mode;
    XtWidgetGeometry *Expected;

    if (ExpectedResults == NULL)
    {
	Expected = &geo;
	if (result_index == 0 && recurse_level == 0)
	{
	    printf("/* Note: the following values are the result of\n"
		   " * querying the current geometry.\n"
		   " */\n");
	    printf("{\n");
	    printf("static XtWidgetGeometry Expected[] = {\n");
	}
    }
    else
    {
	Expected = &ExpectedResults[result_index];
    }
    mode = Expected->request_mode;
    XtVaGetValues(W,
		  XtNchildren, &kids,
		  XtNnumChildren, &numkids,
		  XtNwidth, &geo.width,
		  XtNheight, &geo.height,
		  XtNx, &geo.x,
		  XtNy, &geo.y,
		  NULL);
    if (XtIsManaged(W))
    {
	Result = (mode & CWX ? abs(Expected->x - geo.x) <= slop : True) &&
	    (mode & CWY ? abs(Expected->y - geo.y) <= slop : True) &&
	    (mode & CWWidth ? abs(Expected->width - geo.width) <= slop : True) &&
	    (mode & CWHeight ? abs(Expected->height - geo.height) <= slop : True);

	GlobalResult = GlobalResult && Result;

	if (ExpectedResults)
	{
	    printf("%15s x %-4i(%4i) y %-4i(%4i) w %-4i(%4i) h %-4i(%4i) %s\n",
		   XtName(W),
		   geo.x, (Expected->request_mode & CWX ? Expected->x : geo.x),
		   geo.y, (Expected->request_mode & CWY ? Expected->y : geo.y),
		   geo.width, (Expected->request_mode & CWWidth ? Expected->width : geo.width),
		   geo.height, (Expected->request_mode & CWHeight ? Expected->height : geo.height),
		   Result ? "Good" : "Bad");
	}
	else
	{
	    if (result_index == 0 || XtIsShell(XtParent(W)))
	    {
		printf("   {CWWidth | CWHeight            ,");
	    }
	    else
	    {
		printf("   {CWWidth | CWHeight | CWX | CWY,");
	    }
	    printf(" %4i, %4i, %4i, %4i, 0,0,0, /* %s */},\n",
		   geo.x, geo.y, geo.width, geo.height, XtName(W));
	}

	result_index++;
    }
    for (i = 0; i < numkids; i++)
    {
	recurse_level++;
	PrintWidgetDetails(kids[i], ExpectedResults);
	recurse_level--;
    }
    if (ExpectedResults == NULL && recurse_level == 0)
    {
	printf("};\n");
	printf("/* toplevel should be replaced with to correct applicationShell */\n");
	printf("PrintDetails(toplevel, Expected);\n}\nLessTifTestMainLoop(toplevel);\n");
    }

}

static void
PrintWidgetDetails2(Widget W, XtWidgetGeometry *ExpectedResults)
{
    static int result_index = 0;
    static int recurse_level = 0;
    XtWidgetGeometry geo;
    Boolean Result;
    int i;
    WidgetList kids;
    Cardinal numkids = 0;
    XtGeometryMask mode;
    XtWidgetGeometry *Expected;
    CoreWidget cw = (CoreWidget)W;

    if (ExpectedResults == NULL)
    {
	Expected = &geo;
	if (result_index == 0 && recurse_level == 0)
	{
	    printf("/* Note: the following values are the result of\n"
		   " * querying the current geometry.\n"
		   " */\n");
	    printf("{\n");
	    printf("static XtWidgetGeometry Expected[] = {\n");
	}
    }
    else
    {
	Expected = &ExpectedResults[result_index];
    }
    mode = Expected->request_mode;
    XtVaGetValues(W,
		  XtNchildren, &kids,
		  XtNnumChildren, &numkids,
		  XtNwidth, &geo.width,
		  XtNheight, &geo.height,
		  XtNx, &geo.x,
		  XtNy, &geo.y,
		  NULL);
#if 0
    if (XtIsManaged(W))
    {
#endif
	Result = (mode & CWX ? abs(Expected->x - geo.x) <= slop : True) &&
	    (mode & CWY ? abs(Expected->y - geo.y) <= slop : True) &&
	    (mode & CWWidth ? abs(Expected->width - geo.width) <= slop : True) &&
	    (mode & CWHeight ? abs(Expected->height - geo.height) <= slop : True);

	GlobalResult = GlobalResult && Result;

	if (ExpectedResults)
	{
	    printf("%15s x %-4i(%4i) y %-4i(%4i) w %-4i(%4i) h %-4i(%4i) %s\n",
		   XtName(W),
		   geo.x, (Expected->request_mode & CWX ? Expected->x : geo.x),
		   geo.y, (Expected->request_mode & CWY ? Expected->y : geo.y),
		   geo.width, (Expected->request_mode & CWWidth ? Expected->width : geo.width),
		   geo.height, (Expected->request_mode & CWHeight ? Expected->height : geo.height),
		   Result ? "Good" : "Bad");
	}
	else
	{
	    if (result_index == 0)
	    {
		printf("{  CWWidth | CWHeight            ,");
	    }
	    else
	    {
		printf("{  CWWidth | CWHeight | CWX | CWY,");
	    }
	    printf(" %4i, %4i, %4i, %4i, 0,0,0 }, /* %s */\n",
		   geo.x, geo.y, geo.width, geo.height, XtName(W));
	}

	result_index++;
#if 0
    }
#endif
    /* do children */
    for (i = 0; i < numkids; i++)
    {
	recurse_level++;
	PrintWidgetDetails2(kids[i], ExpectedResults);
	recurse_level--;
    }

    /* do popups */
    /* pgw@hungry.com: Check to make sure the popup list is not NULL */
    if (cw->core.popup_list != NULL)
    {
	for (i=0; i<cw->core.num_popups; i++)
	{
	    recurse_level++;
	    PrintWidgetDetails2(cw->core.popup_list[i], ExpectedResults);
	    recurse_level--;
	}
    }
}

static Boolean
_PrintDetails(Request * request)
{
    if (request->Expected)
    {
    int slop = LessTifTestGetSlop(request->W);

      if (slop > 0)
      {
      	printf("geometrySlop %i\n", slop);
      }
      printf("BEGIN_GEOMETRY\n");
    }
    XSync(XtDisplay(request->W), False);
    while (XtAppPending(XtWidgetToApplicationContext(request->W)))
    {
	XtAppProcessEvent(XtWidgetToApplicationContext(request->W), XtIMAll);
	XFlush(XtDisplay(request->W));
    }

    if(request->version == 2)
    {
        PrintWidgetDetails2(request->W, request->Expected);
    }
    else
    {
        PrintWidgetDetails(request->W, request->Expected);
    }

    if (request->Expected)
    {
	printf("GEOMETRY_RESULT: %s\n", GlobalResult ? "All good" : "One or more failed");
	printf("END_GEOMETRY\n");
    }
    if (!GlobalResult)
	GlobalErrors++;
    return (True);
}

static void
PrintDetailsVersion(Widget W, XtWidgetGeometry *Expected, int version)
{
    static Request request;

    slop = LessTifTestGetSlop(W);
    if (Expected == NULL)
    {
	GlobalErrors++;
    }

    request.W = W;
    request.Expected = Expected;
    request.version = version;

    XtAppAddTimeOut(XtWidgetToApplicationContext(W), 0UL,
                    (XtTimerCallbackProc)_PrintDetails, &request);
}


extern void
PrintDetails(Widget W, XtWidgetGeometry *Expected)
{
    PrintDetailsVersion(W, Expected, 1);
}


extern void
PrintDetails2(Widget W, XtWidgetGeometry *Expected)
{
    PrintDetailsVersion(W, Expected, 2);
}
