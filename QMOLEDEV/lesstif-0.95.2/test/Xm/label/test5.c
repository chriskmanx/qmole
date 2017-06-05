/**
 *
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/label/test5.c,v 1.4 2001/05/15 13:40:54 amai Exp $
 *
 **/

#include <stdio.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

#include "../../common/Test.h"


Boolean GlobalResult = True;
extern int GlobalErrors;

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
	printf("original return (%s) ",
		XdbWidgetGeometry2String(request));

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
		XtVaGetValues(Form,
			XmNfontList, &fontlist,
			XmNlabelString, &string,
			XmNmarginHeight, &mh,
			XmNmarginWidth, &mw,
			NULL);
		Expect.geo.width = XmStringWidth(fontlist, string) + 2 * mw;
		Expect.geo.height = XmStringHeight(fontlist, string) + 2 * mh;
		Expect.result = XtGeometryNo;
		

		GlobalResult &= QueryGeometry(Form, NULL, &Expect, &desired);
		Preferred = desired;
	}
	{ /* Propose what it wants */
	QueryResult Expect;
	XtWidgetGeometry Proposed;
	XtWidgetGeometry desired;

		Expect.geo.request_mode = CWWidth | CWHeight;
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
		Expect.result = XtGeometryNo;

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
		Expect.result = XtGeometryNo;

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
		Expect.result = XtGeometryNo;

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

  toplevel = XtVaAppInitialize(&app, "test5", NULL, 0, &argc, argv, FallBack, NULL);

  printf("\nCreate a Label\n");
  Label = XmCreateLabel(toplevel, "TestWidget", NULL, 0);
  DoTests(Label);

  printf("\nManage the Label\n");
  XtManageChild(Label);
  DoTests(Label);

  printf("\nRealize the Shell\n");
  XtRealizeWidget(toplevel);
  DoTests(Label);

  printf("%s\n",GlobalResult ? "All Passed" : "One or more Failed");
  GlobalErrors = GlobalResult ? 0 : 1;

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	66,	200,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	162,	64,	26,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	50,	58,	100,	0,0,0,	/* two */
};

  /*
  PrintDetails(toplevel, Expected);
  */
  }
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */

  exit(GlobalResult ? 0 : 1);
}
