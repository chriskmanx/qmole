/**
 *
 * $Id: test48.c,v 1.3 2001/05/15 14:46:10 amai Exp $
 *
 **/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/RowColumnP.h>

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



static Boolean 
MakeGeometry(Widget W, XtWidgetGeometry *request, QueryResult *Expected, XtWidgetGeometry *desired)
{
Boolean status = True;
XtWidgetGeometry OriginalRequest;
XtGeometryResult result;

	if (request)
		OriginalRequest = *request;

	printf("MakeGeometryRequest asked for (%s) ",
		XdbWidgetGeometry2String(request));

	result = XtMakeGeometryRequest(W, request, desired);

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
	    (!(Expected->geo.request_mode & CWWidth) || (desired->width  == Expected->geo.width))  &&
	    (!(Expected->geo.request_mode & CWHeight) || (desired->height == Expected->geo.height)) &&
	    result == Expected->result)
	{
		printf("Passed");
	}
	else
	{
		printf("Failed");
		status = False;
	}
	printf("\n");
	return(status);
}

static void DoTests(Widget Form)
{
XtWidgetGeometry Preferred;
XtWidgetGeometry desired;

	XtQueryGeometry(Form, NULL, &Preferred);
	printf("Preferred label size (%s)\n", XdbWidgetGeometry2String(&Preferred));
	{ /* Propose what it wants */
	QueryResult Expect;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.height = 1;
		Expect.result = XtGeometryYes;

		GlobalResult &= MakeGeometry(Form, &Preferred, &Expect, &desired);
	}
	{ /* Propose bigger width */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.width += 1;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryNo;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose bigger width */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.width += 2;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryNo;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose bigger height */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.height += 1;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryNo;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose bigger height */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.height += 2;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryNo;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose bigger */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.width += 1;
		Request.height += 1;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryNo;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose bigger */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.width += 2;
		Request.height += 2;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryNo;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose smaller width */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.width -= 1;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryYes;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose smaller width */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.width -= 2;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryYes;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose smaller height */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.height -= 1;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryYes;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose smaller height */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.height -= 2;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryYes;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose smaller */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.width -= 1;
		Request.height -= 1;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryYes;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
	{ /* Propose smaller */
	QueryResult Expect;
	XtWidgetGeometry Request = Preferred;

		Request.width -= 2;
		Request.height -= 2;

		Expect.geo.request_mode = CWBorderWidth;
		Expect.geo.border_width = 0;
		Expect.geo.width = Preferred.width;
		Expect.geo.height = 1;
		Expect.result = XtGeometryYes;

		GlobalResult &= MakeGeometry(Form, &Request, &Expect, &desired);
	}
}

int
main(int argc, char **argv)
{
  Widget toplevel;
  Widget Form;
  Widget label;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "test23", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateRowColumn(toplevel, "TestWidget", NULL, 0);

  label = XmCreateLabel(Form, "TestWidget", NULL, 0);

  XtManageChild(label);

  XtManageChild(Form);

  XtRealizeWidget(toplevel);

  LessTifTestWaitForIt(toplevel);
  DoTests(label);


{
XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,   72,   25, 0,0,0, /* TestWidget */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   64,   15, 0,0,0, /* TestWidget */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

  printf("%s\n",GlobalResult ? "All Passed" : "One or more Failed");
      LessTifTestMainLoop(toplevel);

  exit(GlobalResult ? 0 : 1);
}
