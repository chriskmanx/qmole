/**
 *
 * $Id: test24.c,v 1.8 2001/05/16 09:12:34 amai Exp $
 *
 **/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/FormP.h>

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
			Expect.geo.width = 1;
			Expect.geo.height = 1;
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
			Expect.geo.width += 2 * bw;
			Expect.geo.height += 2 * bw;
			if (XtIsRealized(Form))
			{
				Expect.result = XtGeometryNo;
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
		Expect.geo.width = Preferred.width + 1;
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
		Expect.geo.width = Preferred.width + 1;
		Expect.geo.height = Preferred.height + 1;
		Expect.result = XtGeometryYes;

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

  printf("Create a Form\n");
  Form = XmCreateForm(toplevel, "TestWidget", NULL, 0);
  {
  XmFormPart Expected = { 0,
  			  0,
  			  100,
  			  False,
  			  NULL,
  			  11,
  			  54,
  			  False};

	  /*
	  CompareFormPart(Form, &Expected);
	  */
  }
  DoTests(Form);

  printf("\nCreate a label in the Form\n");
  label = XmCreateLabel(Form, "TestWidget", NULL, 0);
  {
  XmFormPart Expected = { 0,
  			  0,
  			  100,
  			  False,
  			  NULL,
  			  11,
  			  54,
  			  False};

	  /*
	  CompareFormPart(Form, &Expected);
	  */
  }
  DoTests(Form);

  printf("\nManage the label\n");
  XtManageChild(label);
  {
  XmFormPart Expected = { 0,
  			  0,
  			  100,
  			  False,
  			  NULL,
  			  11,
  			  54,
  			  False};
	  Expected.first_child = label;

	  /*
	  CompareFormPart(Form, &Expected);
	  */
  }
  DoTests(Form);

  printf("\nManage the Form\n");
  XtManageChild(Form);
  {
  XmFormPart Expected = { 0,
  			  0,
  			  100,
  			  False,
  			  NULL,
  			  11,
  			  54,
  			  False};
	  Expected.first_child = label;

	  /*
	  CompareFormPart(Form, &Expected);
	  */
  }
  DoTests(Form);

  printf("\nRealize the Shell\n");
  XtRealizeWidget(toplevel);
  {
  XmFormPart Expected = { 0,
  			  0,
  			  100,
  			  False,
  			  NULL,
  			  11,
  			  54,
  			  False};
	  Expected.first_child = label;

	  /*
	  CompareFormPart(Form, &Expected);
	  */
  }
  DoTests(Form);

  /*
  printf("%s\n",GlobalResult ? "All Passed" : "One or more Failed");
  */

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	66,	19,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	64,	17,	0,0,0,	/* two */
};

  PrintDetails(toplevel, Expected);
  }
  printf("%s\n",GlobalResult ? "All Passed" : "One or more Failed");
      LessTifTestMainLoop(toplevel);

  exit(GlobalResult ? 0 : 1);
}
