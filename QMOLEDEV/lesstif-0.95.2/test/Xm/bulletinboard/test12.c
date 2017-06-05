/**
 *
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/bulletinboard/test12.c,v 1.6 2001/04/24 12:07:59 amai Exp $
 *
 **/

#include <stdlib.h>
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/BulletinBP.h>

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
	WidgetList children;
	Cardinal numKids;

		Expect.geo.request_mode = CWWidth | CWHeight;
		XtVaGetValues(Form,
			XmNchildren, &children,
			XmNnumChildren, &numKids,
			NULL);
		if (numKids == 0 || !XtIsManaged(children[0]))
		{
			Expect.geo.width = 11;
			Expect.geo.height = 11;
			Expect.result = XtGeometryAlmost;
		}
		else
		{
		Dimension bw;

			XtVaGetValues(children[0],
				XmNborderWidth, &bw,
				XmNwidth, &Expect.geo.width,
				XmNheight, &Expect.geo.height,
				NULL);
			Expect.geo.width += 2 * bw + 11;
			Expect.geo.height += 2 * bw + 11;

			if (XtIsRealized(Form))
			{
			    Expect.result = XtGeometryNo;
			    Expect.geo.width += 2 * bw + 11 - 3;
			    Expect.geo.height += 2 * bw + 11 - 3;
			}
			else
			{
				Expect.result = XtGeometryAlmost;
			}
		}

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
		if (XtIsRealized(Form))
		{
		    Expect.result = XtGeometryNo;
		}
		else
		{
		    Expect.result = XtGeometryAlmost;
		}

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
		if (XtIsRealized(Form))
		{
		    Expect.result = XtGeometryNo;
		}
		else
		{
		    Expect.result = XtGeometryAlmost;
		}

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
		if (XtIsRealized(Form))
		{
			Expect.result = XtGeometryNo;
		}
		else
		{
			Expect.result = XtGeometryAlmost;
		}

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
  Widget label;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "test23", NULL, 0, &argc, argv, FallBack, NULL);

  printf("Create a BulletinBoard\n");
  Form = XmCreateBulletinBoard(toplevel, "TestWidget", NULL, 0);
  
  DoTests(Form);

  printf("\nCreate a label in the BulletinBoard\n");
  label = XmCreateLabel(Form, "TestWidget", NULL, 0);
  
  DoTests(Form);

  printf("\nManage the label\n");
  XtManageChild(label);
  
  DoTests(Form);

  printf("\nManage the BulletinBoard\n");
  XtManageChild(Form);
  
  DoTests(Form);

  printf("\nRealize the Shell\n");
  XtRealizeWidget(toplevel);
  
  DoTests(Form);

  /*
  printf("%s\n",GlobalResult ? "All Passed" : "One or more Failed");
  */


/*
Create a BulletinBoard
QueryGeometry asked for (NULL_GEOMETRY) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 11 h 11) got (w 11 h 11 Yes) expected (w 11 h 11 Yes) Passed
QueryGeometry asked for (w 12) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 12 h 12) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 10 h 10) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed

Create a label in the BulletinBoard
QueryGeometry asked for (NULL_GEOMETRY) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 11 h 11) got (w 11 h 11 Yes) expected (w 11 h 11 Yes) Passed
QueryGeometry asked for (w 12) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 12 h 12) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed
QueryGeometry asked for (w 10 h 10) got (w 11 h 11 Almost) expected (w 11 h 11 Almost) Passed

Manage the label
QueryGeometry asked for (NULL_GEOMETRY) got (w 77 h 30 Almost) expected (w 77 h 30 Almost) Passed
QueryGeometry asked for (w 77 h 30) got (w 77 h 30 Yes) expected (w 77 h 30 Yes) Passed
QueryGeometry asked for (w 78) got (w 77 h 30 Almost) expected (w 77 h 30 Almost) Passed
QueryGeometry asked for (w 78 h 31) got (w 77 h 30 Almost) expected (w 77 h 30 Almost) Passed
QueryGeometry asked for (w 76 h 29) got (w 77 h 30 Almost) expected (w 77 h 30 Almost) Passed

Manage the BulletinBoard
QueryGeometry asked for (NULL_GEOMETRY) got (w 77 h 30 Almost) expected (w 77 h 30 Almost) Passed
QueryGeometry asked for (w 77 h 30) got (w 77 h 30 Yes) expected (w 77 h 30 Yes) Passed
QueryGeometry asked for (w 78) got (w 77 h 30 Almost) expected (w 77 h 30 Almost) Passed
QueryGeometry asked for (w 78 h 31) got (w 77 h 30 Almost) expected (w 77 h 30 Almost) Passed
QueryGeometry asked for (w 76 h 29) got (w 77 h 30 Almost) expected (w 77 h 30 Almost) Passed

Realize the Shell
QueryGeometry asked for (NULL_GEOMETRY) got (w 87 h 40 No) expected (w 87 h 40 No) Passed
QueryGeometry asked for (w 87 h 40) got (w 87 h 40 Yes) expected (w 87 h 40 Yes) Passed
QueryGeometry asked for (w 88) got (w 87 h 40 No) expected (w 87 h 40 No) Passed
QueryGeometry asked for (w 88 h 41) got (w 87 h 40 No) expected (w 87 h 40 No) Passed
QueryGeometry asked for (w 86 h 39) got (w 87 h 40 No) expected (w 87 h 40 No) Passed
*/
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,   87,   40, 0,0,0, /* TestWidget */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   64,   17, 0,0,0, /* TestWidget */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(GlobalResult ? 0 : 1);
}
