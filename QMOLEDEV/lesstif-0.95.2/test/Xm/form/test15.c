/* rws 7 Apr 1997
	It is best to start this with -geometry +500+500 (or whatever)
	otherwise the time to place it by the window manager bungs up the
	first test

	Is there a event, or callback one can use to figure out when the
	window has been placed????
*/
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>

#define TOPLABEL 0
#define BOTLABEL 1

typedef struct {
	int time;
	Boolean autoExit;
       } AppResources_t, *AppResourcesPtr;
AppResources_t AppResources;

static String FallBack[] = {
	"*geometry: +500+500",
	"*borderWidth: 1",
NULL};

static XtResource resources[] = {
	{"time","time",XtRInt,sizeof(int),XtOffset(AppResourcesPtr,time),XtRImmediate, (void *)1000},
	{"autoExit","autoExit",XtRBoolean,sizeof(Boolean),XtOffset(AppResourcesPtr,autoExit),XtRImmediate, (void *)False},
};

static XrmOptionDescRec opTable[] = {
	{"-time",".time",XrmoptionSepArg, NULL},
	{"-autoExit",".autoExit",XrmoptionNoArg, "True"},
};


Boolean GlobalResult1 = True;
XtWidgetGeometry *ExpectedResults;

XtAppContext app;

static void FinishTest(Widget W);
static Boolean DoAllTests(Widget W);

static Boolean
Skeleton(Widget W)
{
WidgetList kids;
Cardinal numkids;

XtWidgetGeometry Form;
XtWidgetGeometry Kid1;
XtWidgetGeometry Kid2;

	XtVaGetValues(W,
		XmNchildren, &kids,
		XmNnumChildren, &numkids,
		XmNx, &Form.x,
		XmNy, &Form.y,
		XmNwidth, &Form.width,
		XmNheight, &Form.height,
		NULL);

	XtVaGetValues(kids[TOPLABEL],
		XmNx, &Kid1.x,
		XmNy, &Kid1.y,
		XmNwidth, &Kid1.width,
		XmNheight, &Kid1.height,
		XmNborderWidth, &Kid1.border_width,
		NULL);

	XtVaGetValues(kids[BOTLABEL],
		XmNx, &Kid2.x,
		XmNy, &Kid2.y,
		XmNwidth, &Kid2.width,
		XmNheight, &Kid2.height,
		XmNborderWidth, &Kid2.border_width,
		NULL);

	XtVaSetValues(kids[TOPLABEL],
		NULL);

	XtVaSetValues(kids[BOTLABEL],
		NULL);

	ExpectedResults[0].request_mode = CWWidth | CWHeight;
	ExpectedResults[0].x = Form.x;
	ExpectedResults[0].y = Form.y;
	ExpectedResults[0].width = Form.width;
	ExpectedResults[0].height = Form.height;

	ExpectedResults[1].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[1].x = Kid1.x;
	ExpectedResults[1].y = Kid1.x;
	ExpectedResults[1].width = Kid1.width;
	ExpectedResults[1].height = Kid1.height;

	ExpectedResults[2].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[2].x = Kid2.x;
	ExpectedResults[2].y = Kid2.y;
	ExpectedResults[2].width = Kid2.width;
	ExpectedResults[2].height = Kid2.height;

	printf("Test ? a skeleton for more tests (does nothing)\n");
	FinishTest(W);
	return(True);
}

static Boolean
Test6(Widget W)
{
WidgetList kids;
Cardinal numkids;

XtWidgetGeometry Form;
XtWidgetGeometry Kid1;
XtWidgetGeometry Kid2;

	fprintf(stderr, "Test 6 Starting...\n");
	XtVaGetValues(W,
		XmNchildren, &kids,
		XmNnumChildren, &numkids,
		XmNx, &Form.x,
		XmNy, &Form.y,
		XmNwidth, &Form.width,
		XmNheight, &Form.height,
		NULL);

	XtVaGetValues(kids[TOPLABEL],
		XmNx, &Kid1.x,
		XmNy, &Kid1.y,
		XmNwidth, &Kid1.width,
		XmNheight, &Kid1.height,
		XmNborderWidth, &Kid1.border_width,
		NULL);

	XtVaGetValues(kids[BOTLABEL],
		XmNx, &Kid2.x,
		XmNy, &Kid2.y,
		XmNwidth, &Kid2.width,
		XmNheight, &Kid2.height,
		XmNborderWidth, &Kid2.border_width,
		NULL);

	XtUnrealizeWidget(XtParent(W));
	XtUnrealizeWidget(W);
	XtUnrealizeWidget(kids[TOPLABEL]);
	XtUnrealizeWidget(kids[BOTLABEL]);
	XtUnmanageChild(kids[BOTLABEL]);

	XtVaSetValues(W,
		XmNwidth, 215,
		XmNheight, 0,
		NULL);

	XtVaSetValues(kids[TOPLABEL],
		XmNwidth, 0,
		XmNheight, 0,
		NULL);

	XtVaSetValues(kids[BOTLABEL],
		XmNwidth, 0,
		XmNheight, 0,
		NULL);

	XtVaSetValues(kids[0],
		XmNtopAttachment, XmATTACH_NONE,
		NULL);
	XtVaSetValues(kids[0],
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, kids[1],
		XmNleftOffset, 10,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 10,
		XmNwidth, 90,
		XmNheight, 0,
		NULL);

	XtVaSetValues(kids[1],
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
	XtVaSetValues(kids[1],
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_NONE,
		XmNleftOffset, 10,
		XmNwidth, 100,
		XmNheight, 0,
		NULL);

	XtManageChild(kids[0]);
	XtManageChild(kids[1]);
	XtManageChild(W);
	XtRealizeWidget(XtParent(W));

	ExpectedResults[0].request_mode = CWWidth | CWHeight;
	ExpectedResults[0].x = Form.x;
	ExpectedResults[0].y = Form.y;
	ExpectedResults[0].width = Form.width;
	ExpectedResults[0].height = Form.height;

	ExpectedResults[1].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[1].x = Kid1.x;
	ExpectedResults[1].y = Kid1.x;
	ExpectedResults[1].width = Kid1.width;
	ExpectedResults[1].height = Kid1.height;

	ExpectedResults[2].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[2].x = Kid2.x;
	ExpectedResults[2].y = Kid2.y;
	ExpectedResults[2].width = Kid2.width;
	ExpectedResults[2].height = Kid2.height;

	printf("Test 6\n");
	FinishTest(W);
	return(True);
}

static Boolean
Test5(Widget W)
{
WidgetList kids;
Cardinal numkids;

XtWidgetGeometry Form;
XtWidgetGeometry Kid1;
XtWidgetGeometry Kid2;

	fprintf(stderr,"Test 5 starting...\n");
	XtVaGetValues(W,
		XmNchildren, &kids,
		XmNnumChildren, &numkids,
		XmNx, &Form.x,
		XmNy, &Form.y,
		XmNwidth, &Form.width,
		XmNheight, &Form.height,
		NULL);

	XtVaGetValues(kids[TOPLABEL],
		XmNx, &Kid1.x,
		XmNy, &Kid1.y,
		XmNwidth, &Kid1.width,
		XmNheight, &Kid1.height,
		XmNborderWidth, &Kid1.border_width,
		NULL);

	XtVaGetValues(kids[BOTLABEL],
		XmNx, &Kid2.x,
		XmNy, &Kid2.y,
		XmNwidth, &Kid2.width,
		XmNheight, &Kid2.height,
		XmNborderWidth, &Kid2.border_width,
		NULL);

	XtUnmanageChild(kids[TOPLABEL]);

	XtVaSetValues(W,
		NULL);

	XtVaSetValues(kids[TOPLABEL],
		NULL);

	XtVaSetValues(kids[BOTLABEL],
		XmNwidth, 100,
		XmNheight, 50,
		NULL);
	XtManageChild(kids[BOTLABEL]);

	ExpectedResults[0].request_mode = CWWidth | CWHeight;
	ExpectedResults[0].x = Form.x;
	ExpectedResults[0].y = Form.y;
	ExpectedResults[0].width = 100 + 2 * Kid2.border_width;
	ExpectedResults[0].height = 50 + 2 * Kid2.border_width;

	ExpectedResults[1].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[1].x = Kid1.x;
	ExpectedResults[1].y = Kid1.x;
	ExpectedResults[1].width = Kid1.width;
	ExpectedResults[1].height = Kid1.height;

	ExpectedResults[2].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[2].x = Kid2.x;
	ExpectedResults[2].y = Kid2.y;
	ExpectedResults[2].width = 100;
	ExpectedResults[2].height = 50;

	printf("Test 5\n");
	FinishTest(W);
	return(True);
}

static Boolean
Test4(Widget W)
{
WidgetList kids;
Cardinal numkids;

XtWidgetGeometry Form;
XtWidgetGeometry Kid1;
XtWidgetGeometry Kid2;

	fprintf(stderr,"Test 4 starting...\n");
	XtVaGetValues(W,
		XmNchildren, &kids,
		XmNnumChildren, &numkids,
		XmNx, &Form.x,
		XmNy, &Form.y,
		XmNwidth, &Form.width,
		XmNheight, &Form.height,
		NULL);

	XtVaGetValues(kids[TOPLABEL],
		XmNx, &Kid1.x,
		XmNy, &Kid1.y,
		XmNwidth, &Kid1.width,
		XmNheight, &Kid1.height,
		XmNborderWidth, &Kid1.border_width,
		NULL);

	XtVaGetValues(kids[BOTLABEL],
		XmNx, &Kid2.x,
		XmNy, &Kid2.y,
		XmNwidth, &Kid2.width,
		XmNheight, &Kid2.height,
		XmNborderWidth, &Kid2.border_width,
		NULL);

	XtUnmanageChild(W);
	XtVaSetValues(kids[BOTLABEL],
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_NONE,
		NULL);

	XtVaSetValues(kids[TOPLABEL],
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, kids[1],
		NULL);

	XtManageChild(kids[BOTLABEL]);
	XtManageChild(W);

	XtVaGetValues(kids[BOTLABEL],
		XmNx, &Kid2.x,
		XmNy, &Kid2.y,
		XmNwidth, &Kid2.width,
		XmNheight, &Kid2.height,
		XmNborderWidth, &Kid2.border_width,
		NULL);

	ExpectedResults[0].request_mode = CWWidth | CWHeight;
	ExpectedResults[0].x = Form.x;
	ExpectedResults[0].y = Form.y;
	ExpectedResults[0].width = Form.width;
	ExpectedResults[0].height = Form.height;

	ExpectedResults[1].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[1].x = Kid2.width + 2 * Kid2.border_width;
	ExpectedResults[1].y = Kid1.x;
	ExpectedResults[1].width = Form.width - Kid2.width - 2 * Kid1.border_width - 2 * Kid2.border_width;
	ExpectedResults[1].height = Kid1.height;

	ExpectedResults[2].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[2].x = Kid2.x;
	ExpectedResults[2].y = 0;
	ExpectedResults[2].width = Kid2.width;
	ExpectedResults[2].height = Kid2.height;

	printf("Test 4\n");
	FinishTest(W);
	return(True);
}

static Boolean
Test3(Widget W)
{
WidgetList kids;
Cardinal numkids;

XtWidgetGeometry Form;
XtWidgetGeometry Kid1;
XtWidgetGeometry Kid2;

	fprintf(stderr,"Test 3 starting...\n");
	XtVaGetValues(W,
		XmNchildren, &kids,
		XmNnumChildren, &numkids,
		XmNx, &Form.x,
		XmNy, &Form.y,
		XmNwidth, &Form.width,
		XmNheight, &Form.height,
		NULL);

	XtVaGetValues(kids[TOPLABEL],
		XmNx, &Kid1.x,
		XmNy, &Kid1.y,
		XmNwidth, &Kid1.width,
		XmNheight, &Kid1.height,
		XmNborderWidth, &Kid1.border_width,
		NULL);

	XtVaGetValues(kids[BOTLABEL],
		XmNx, &Kid2.x,
		XmNy, &Kid2.y,
		XmNwidth, &Kid2.width,
		XmNheight, &Kid2.height,
		XmNborderWidth, &Kid2.border_width,
		NULL);

	XtVaSetValues(kids[TOPLABEL],
		XmNrightAttachment, XmATTACH_NONE,
		XmNwidth, Kid1.width * 4,
		NULL);

	ExpectedResults[0].request_mode = CWWidth | CWHeight;
	ExpectedResults[0].x = 0;
	ExpectedResults[0].y = 0;
	ExpectedResults[0].width = Kid1.width * 4 + 2 * Kid1.border_width;
	ExpectedResults[0].height = Form.height;

	ExpectedResults[1].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[1].x = 0;
	ExpectedResults[1].y = 0;
	ExpectedResults[1].width = Kid1.width * 4;
	ExpectedResults[1].height = Kid1.height;

	ExpectedResults[2].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[2].request_mode = 0;
	ExpectedResults[2].x = 0;
	ExpectedResults[2].y = 0;
	ExpectedResults[2].width = 0;
	ExpectedResults[2].height = 0;

	printf("Test 3\n");
	FinishTest(W);
	return(True);
}

static Boolean
Test2(Widget W)
{
Dimension h,w;
Dimension ch,cw,bw,mw;
Position cx,cy;
WidgetList kids;
Cardinal numkids;
int i;
XmString string;
XmFontList font_list;

	fprintf(stderr,"Test 2 starting...\n");
	XtVaGetValues(W,
		XmNchildren, &kids,
		XmNnumChildren, &numkids,
		NULL);
	XtVaSetValues(kids[TOPLABEL],
		XmNbottomAttachment, XmATTACH_NONE,
		NULL);
	for (i = 1; i<numkids; i++)
	{
		XtUnmanageChild(kids[i]);
	}
	XtVaGetValues(kids[TOPLABEL],
		XmNlabelString, &string,
		XmNfontList, &font_list,
		XmNborderWidth, &bw,
		XmNmarginWidth, &mw,
		XmNwidth, &cw,
		NULL);
	ExpectedResults[1].width = XmStringWidth(font_list,string) + 2 * mw;
	XmStringFree(string);

	ExpectedResults[0].request_mode = CWWidth | CWHeight;
	ExpectedResults[0].x = 0;
	ExpectedResults[0].y = 0;
	ExpectedResults[0].width = ExpectedResults[1].width + 2 * bw;
	ExpectedResults[0].height = 19;

	ExpectedResults[1].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[1].x = 0;
	ExpectedResults[1].y = 0;
	ExpectedResults[1].height = 17;

	ExpectedResults[2].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[2].request_mode = 0;
	ExpectedResults[2].x = 0;
	ExpectedResults[2].y = 0;
	ExpectedResults[2].width = 0;
	ExpectedResults[2].height = 0;

	printf("Test 2 Unmanage all but the first child\n");
	FinishTest(W);
	return(True);
}

static Boolean
Test1(Widget W)
{
Dimension h,w;
Dimension ch,cw,bw;
Position cx,cy;
WidgetList kids;
Cardinal numkids;

	XtVaGetValues(W,
		XmNchildren, &kids,
		XmNnumChildren, &numkids,
		XmNwidth, &w,
		XmNheight, &h,
		NULL);

	ExpectedResults[0].request_mode = CWWidth | CWHeight;
	ExpectedResults[0].x = 0;
	ExpectedResults[0].y = 0;
	ExpectedResults[0].width = w * 2;
	ExpectedResults[0].height = h * 2;

	XtVaGetValues(kids[TOPLABEL],
		XmNborderWidth, &bw,
		NULL);
	ExpectedResults[1].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[1].x = 0;
	ExpectedResults[1].y = 0;
	ExpectedResults[1].width = w * 2 - 2 * bw;
	ExpectedResults[1].height = h * 2 - 4 * bw - 17;

	ExpectedResults[2].request_mode = CWX | CWY | CWWidth | CWHeight;
	ExpectedResults[2].x = 0;
	ExpectedResults[2].y = h * 2 - 2 * bw - 17;
	ExpectedResults[2].width = w * 2 - 2 * bw;
	ExpectedResults[2].height = 17;

	XtVaSetValues(W,
		XmNwidth, w * 2,
		XmNheight, h * 2,
		NULL);

	printf("Test 1 double form height and width\n");
	FinishTest(W);
	return(True);
}

static Boolean (*TestList[])() = {
		Test1,
		Test2,
		Test3,
		Test4,
		Test5,
		/*
		Test6,
		*/
NULL};


static void
PrintWidgetDetails(Widget W, int index)
{
Position x,y;
Dimension h,w;
Boolean Result;
XtGeometryMask mode;

	if (!XtIsManaged(W)) return;
	mode = ExpectedResults[index].request_mode;
	w = h = x = y = 0;
	XtVaGetValues(W,
		XmNwidth, &w,
		XmNheight, &h,
		XmNx, &x,
		XmNy, &y,
		NULL);
	Result = (mode & CWX ? ExpectedResults[index].x == x : True) &&
	         (mode & CWY ? ExpectedResults[index].y == y : True) &&
	         (mode & CWWidth ? ExpectedResults[index].width == w : True) &&
	         (mode & CWHeight ? ExpectedResults[index].height == h : True);

	GlobalResult1 = GlobalResult1 && Result;

	printf("%14s x %-4i(%4i) y %-4i(%4i) w %-4i(%4i) h %-4i(%4i) %s\n",XtName(W),
		x, (ExpectedResults[index].request_mode & CWX ? ExpectedResults[index].x : x),
		y, (ExpectedResults[index].request_mode & CWY ? ExpectedResults[index].y : y),
		w, (ExpectedResults[index].request_mode & CWWidth ? ExpectedResults[index].width : w),
		h, (ExpectedResults[index].request_mode & CWHeight ? ExpectedResults[index].height : h),
		Result ? "Good" : "Bad");
}

static Boolean
_PrintDetails(Widget W)
{
int i;
WidgetList kids;
Cardinal numkids;

	XmUpdateDisplay(W);
	PrintWidgetDetails(W, 0);
	if (XmIsForm(W))
	{
		XtVaGetValues(W,
			XmNchildren, &kids,
			XmNnumChildren, &numkids,
			NULL);
		for (i=0; i<numkids; i++)
		{
			PrintWidgetDetails(kids[i], i+1);
		}
		printf("\n");
	}
	return(True);
}

static void
PrintDetails(Widget W)
{
  XtAppAddTimeOut(XtWidgetToApplicationContext(W), 0,
		  (XtTimerCallbackProc)_PrintDetails, W);
}

static void
Event(Widget W, Widget w)
{
	printf("Event %s %s\n",XtName(W),XtName(w));
	PrintDetails(w);
}

static Boolean
DoAllTests(Widget W)
{
static int TestNumber = 0;

	if (TestList[TestNumber] != NULL)
	{
		(*TestList[TestNumber])(W);
		TestNumber++;
	}
	else
	{
		printf("%s\n",GlobalResult1 ? "All tests passed" : "One or more tests failed");
		if (AppResources.autoExit) exit(0);
	}
	return(True);
}

static void
FinishTest(Widget W)
{
	PrintDetails(W);
	XtAppAddTimeOut(XtWidgetToApplicationContext(W), AppResources.time,
			(XtTimerCallbackProc)DoAllTests, W);
}

int
main(int argc, char **argv)
{
  Widget toplevel, Form, BottomLabel, TopLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,
  	"Form1", 
  	opTable, XtNumber(opTable),
  	&argc, argv,
  	FallBack, NULL);

  XtGetApplicationResources(toplevel,
  		&AppResources,
  		resources, XtNumber(resources),
  		NULL,0);

  XtVaSetValues(toplevel,
  	XmNresizePolicy, XmRESIZE_ANY,
  	XmNallowShellResize, True,
  	NULL);

  Form = XmCreateForm(toplevel,"Form",NULL,0);
  XtVaSetValues(Form,
  	XmNresizePolicy, XmRESIZE_ANY,
  	XmNallowShellResize, True,
  	NULL);

  TopLabel = XmCreateLabel(Form, "TopLabelTop", NULL, 0);
  BottomLabel = XmCreateLabel(Form, "BottomLabel", NULL, 0);

  ExpectedResults = (XtWidgetGeometry *)XtMalloc(3 * sizeof(XtWidgetGeometry));
  ExpectedResults[0].request_mode = CWWidth | CWHeight;
  ExpectedResults[0].x = 72;
  ExpectedResults[0].y = 38;
  ExpectedResults[0].width = 72;
  ExpectedResults[0].height = 38;

  XtVaSetValues(TopLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, BottomLabel,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(TopLabel);
  ExpectedResults[1].request_mode = CWX | CWY | CWWidth | CWHeight;
  ExpectedResults[1].x = 0;
  ExpectedResults[1].y = 0;
  ExpectedResults[1].width = 70;
  ExpectedResults[1].height = 17;

  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_NONE,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(BottomLabel);
  ExpectedResults[2].request_mode = CWX | CWY | CWWidth | CWHeight;
  ExpectedResults[2].x = 0;
  ExpectedResults[2].y = 19;
  ExpectedResults[2].width = 70;
  ExpectedResults[2].height = 17;

  XtManageChild(Form);

  XtAddEventHandler(TopLabel, ButtonPressMask, True,
		    (XtEventHandler)Event, Form);
  XtAddEventHandler(BottomLabel, ButtonPressMask, True,
		    (XtEventHandler)Event, Form);

  XtRealizeWidget(toplevel);

  printf("Initial\n");
  XtVaSetValues(TopLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, BottomLabel,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);

  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_NONE,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);
  PrintDetails(Form);

  /* I would like to wait until the window manager has placed the thing */
  XtAppAddTimeOut(app, AppResources.time,
		  (XtTimerCallbackProc)DoAllTests, Form);

      XtAppMainLoop(app);
      /*
      LessTifTestMainLoop(toplevel);
      */

  exit(0);
}
