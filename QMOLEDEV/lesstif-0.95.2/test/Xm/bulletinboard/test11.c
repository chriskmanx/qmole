/**
 *
 * $Id: test11.c,v 1.10 2001/05/19 21:34:48 amai Exp $
 *
 **/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/Form.h>

#include "../../common/Test.h"

Boolean GlobalResult = True;

static String FallBack[] = {
	"*geometry: +500+500",
	"*borderWidth: 1",
NULL};

typedef struct {
		XtGeometryResult result;
		XtWidgetGeometry geo;
} QueryResult;


static Boolean QueryGeometry(Widget W, XtWidgetGeometry *request, QueryResult *Expected, XtWidgetGeometry *desired)
{
Boolean status = True;
XtWidgetGeometry OriginalRequest;
XtGeometryResult result;

	if (request)
		OriginalRequest = *request;

	printf("QueryGeometry asked for (%s) ",
		XdbWidgetGeometry2String(request));

	result = XtQueryGeometry(W, request, desired);

	printf("got (%s %s) ",
		XdbWidgetGeometry2String(desired),
		XdbGeometryResult2String(result));
	printf("expected (%s %s %s) ",
		XdbWidgetGeometry2String(&Expected->geo),
		XdbGeometryResult2String(Expected->result),
		XdbWidgetGeometry2String(request));
	/*
	printf("original return (%s) ",
		XdbWidgetGeometry2String(request));
		*/

	if ((desired->request_mode & Expected->geo.request_mode) == Expected->geo.request_mode &&
	    desired->width  == Expected->geo.width  &&
	    desired->height == Expected->geo.height &&
	    result == Expected->result &&
	    (!request ? True : (OriginalRequest.request_mode == request->request_mode &&
	    			OriginalRequest.width == request->width &&
	    			OriginalRequest.height == request->height))
	   )
	{
		printf("Passed\n");
	}
	else
	{
		printf("Failed\n");
		status = False;
	}
	return(status);
}

static void DoTests(Widget Form)
{
XtWidgetGeometry Preferred;

	{ /* Ask what it wants */
	QueryResult Expect;
	XtWidgetGeometry desired;
	Dimension mh, mw;
	XmFontList fontlist;
	XmString string;

		Expect.geo.request_mode = CWWidth | CWHeight;
		Expect.geo.width = 11;
		Expect.geo.height = 11;
		Expect.result = XtGeometryAlmost;
		

		GlobalResult &= QueryGeometry(Form, NULL, &Expect, &desired);
		Preferred = desired;
	}
	{ /* Propose what it wants */
	QueryResult Expect;
	XtWidgetGeometry Proposed;
	XtWidgetGeometry desired;

		Expect.geo.request_mode = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = Preferred.height;
		Expect.result = XtGeometryYes;

		Proposed.request_mode = CWWidth | CWHeight;
		Proposed.width = Preferred.width;
		Proposed.height = Preferred.height;
		GlobalResult &= QueryGeometry(Form, &Proposed, &Expect, &desired);
	}
	{ /* propose something bigger in width */
	QueryResult Expect;
	XtWidgetGeometry Proposed;
	XtWidgetGeometry desired;

		Expect.geo.request_mode = CWWidth | CWHeight;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = Preferred.height;
		Expect.result = XtGeometryAlmost;

		Proposed.request_mode = CWWidth;
		Proposed.width = Preferred.width + 1;
		Proposed.height = Preferred.height;
		GlobalResult &= QueryGeometry(Form, &Proposed, &Expect, &desired);
	}
	{ /* propose something bigger */
	QueryResult Expect;
	XtWidgetGeometry Proposed;
	XtWidgetGeometry desired;

		Expect.geo.request_mode = CWWidth | CWHeight;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = Preferred.height;
		Expect.result = XtGeometryAlmost;

		Proposed.request_mode = CWWidth | CWHeight;
		Proposed.width = Preferred.width + 1;
		Proposed.height = Preferred.height + 1;
		GlobalResult &= QueryGeometry(Form, &Proposed, &Expect, &desired);
	}
	{ /* propose something smaller */
	QueryResult Expect;
	XtWidgetGeometry Proposed;
	XtWidgetGeometry desired;

		Expect.geo.request_mode = CWWidth | CWHeight;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = Preferred.height;
		Expect.result = XtGeometryAlmost;

		Proposed.request_mode = CWWidth | CWHeight;
		Proposed.width = Preferred.width - 1;
		Proposed.height = Preferred.height - 1;
		GlobalResult &= QueryGeometry(Form, &Proposed, &Expect, &desired);
	}
}

int
main(int argc, char **argv)
{
  Widget toplevel;
  Widget Form;
  Widget Label;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "test5", NULL, (Cardinal)0, &argc, argv, FallBack, NULL);

  printf("\nCreate a BulletinBoard\n");
  Label = XmCreateBulletinBoard(toplevel, "TestWidget", NULL, 0);
  DoTests(Label);

  printf("\nManage the BulletinBoard\n");
  XtManageChild(Label);
  DoTests(Label);

  printf("\nRealize the BulletinBoard\n");
  XtRealizeWidget(toplevel);
  DoTests(Label);

  printf("%s\n",GlobalResult ? "All Passed" : "One or more Failed");


/*
Create a BulletinBoard
QueryGeometry asked for (NULL_GEOMETRY) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 11 h 11) got (w 11 h 11 Yes) expected (GEOMETRY_NO_FIELDS Yes) Passed
QueryGeometry asked for (w 12) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 12 h 12) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 10 h 10) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed

Manage the BulletinBoard
QueryGeometry asked for (NULL_GEOMETRY) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 11 h 11) got (w 11 h 11 Yes) expected (GEOMETRY_NO_FIELDS Yes) Passed
QueryGeometry asked for (w 12) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 12 h 12) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 10 h 10) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed

Realize the BulletinBoard
QueryGeometry asked for (NULL_GEOMETRY) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 11 h 11) got (w 11 h 11 Yes) expected (GEOMETRY_NO_FIELDS Yes) Passed
QueryGeometry asked for (w 12) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 12 h 12) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 10 h 10) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
All Passed
*/
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  500,  500,    1,    1, 0,0,0, /* TestWidget */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  XtAppMainLoop(app);

  exit(GlobalResult ? 0 : 1);
}
