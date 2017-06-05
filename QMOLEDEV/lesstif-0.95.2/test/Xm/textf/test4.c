/* $Id: test4.c,v 1.4 2000/08/29 21:59:22 dannybackx Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/TextF.h>

typedef struct {
	int time;
	Boolean autoExit;
       } AppResources_t, *AppResourcesPtr;
AppResources_t AppResources;

static String FallBack[] = {
	"*autoExit: False",
NULL};

static XtResource resources[] = {
	{"time","time",XtRInt,sizeof(int),XtOffset(AppResourcesPtr,time),XtRImmediate, (XtPointer)1000},
	{"autoExit","autoExit",XtRBoolean,sizeof(Boolean),XtOffset(AppResourcesPtr,autoExit),XtRImmediate, (XtPointer)False},
};

static XrmOptionDescRec opTable[] = {
	{"-time",".time",XrmoptionSepArg, NULL},
	{"-autoExit",".autoExit",XrmoptionNoArg, "True"},
};

typedef struct {
		Boolean valueChanged;
		Boolean modifyVerify;
		Boolean modifyVerifyW;
		Boolean motionVerify;
} test_t;

test_t TestResults;

String TestString = "Hello there";


Boolean GlobalResult = True;

XtAppContext app;

static void
valueChange(Widget W, XtPointer a, XmAnyCallbackStruct *cbs)
{
	TestResults.valueChanged = True;
	if (!TestResults.modifyVerifyW || !TestResults.modifyVerifyW)
	{
		printf("valueChanged - modifyVerify not called before valueChanged\n");
		GlobalResult = False;
	}
	if (cbs->reason != XmCR_VALUE_CHANGED)
	{
		printf("valueChanged reason is wrong\n");
		GlobalResult = False;
	}
}

static void
modifyVerify(Widget W, XtPointer a, XmTextVerifyPtr cbs)
{
	TestResults.modifyVerify = True;
	if (TestResults.modifyVerifyW || 
	    TestResults.valueChanged ||
	    TestResults.motionVerify)
	{
		printf("modifyVerify - called out of order\n");
		GlobalResult = False;
	}
	if (cbs->reason != XmCR_MODIFYING_TEXT_VALUE)
	{
		printf("modifyVerify - reason is wrong\n");
		GlobalResult = False;
	}
	if (cbs->doit != True)
	{
		printf("modifyVerify - doit is wrong\n");
		GlobalResult = False;
	}
	if (cbs->currInsert != 0)
	{
		printf("modifyVerify - currInsert is %d should be 0\n",cbs->currInsert);
		GlobalResult = False;
	}
	if (cbs->newInsert != 0)
	{
		printf("modifyVerify - newInsert is %d should be 0\n",cbs->newInsert);
		GlobalResult = False;
	}
	if (cbs->startPos != 0)
	{
		printf("modifyVerify - startPos is %d should be 0\n",cbs->startPos);
		GlobalResult = False;
	}
	if (cbs->endPos != 0)
	{
		printf("modifyVerify - endPos is %d should be %d\n",cbs->endPos, 0);
		GlobalResult = False;
	}
	if (strcmp(cbs->text->ptr, TestString) != 0)
	{
		printf("modifyVerify - text is %s should be %s\n",cbs->text->ptr, TestString);
		GlobalResult = False;
	}
	if (strlen(cbs->text->ptr) != strlen(TestString))
	{
		printf("modifyVerify - length is %d should be %d\n",strlen(cbs->text->ptr), strlen(TestString));
		GlobalResult = False;
	}
	if (cbs->text->format != XmFMT_8_BIT)
	{
		printf("modifyVerify - format is wrong\n");
		GlobalResult = False;
	}
}

static void
modifyVerifyW(Widget W, XtPointer a, XmTextVerifyPtr cbs)
{
	TestResults.modifyVerifyW = True;
	if (!TestResults.modifyVerify || 
	    TestResults.valueChanged ||
	    TestResults.motionVerify)
	{
		printf("modifyVerifyWcs called out of order\n");
		GlobalResult = False;
	}
}

static void
motionVerify(Widget W, XtPointer a, XmTextVerifyPtr cbs)
{
	TestResults.motionVerify = True;
	if (!TestResults.modifyVerifyW || 
	    !TestResults.modifyVerify || 
	    !TestResults.valueChanged)
	{
		printf("motionVerify - called out of order\n");
		GlobalResult = False;
	}
}

static void
TestSetString(Widget W)
{
char *string;

	TestResults.valueChanged = False;
	TestResults.modifyVerify = False;
	TestResults.modifyVerifyW = False;
	TestResults.motionVerify = False;
	XtAddCallback(W, XmNvalueChangedCallback, (XtCallbackProc)valueChange, NULL);
	XtAddCallback(W, XmNmodifyVerifyCallback, (XtCallbackProc)modifyVerify, NULL);
	XtAddCallback(W, XmNmodifyVerifyCallbackWcs, (XtCallbackProc)modifyVerifyW, NULL);
	XtAddCallback(W, XmNmotionVerifyCallback, (XtCallbackProc)motionVerify, NULL);
	XmTextFieldSetString(W, TestString);

	string = XmTextFieldGetString(W);
	if (strcmp(TestString, string) != 0)
	{
		printf("GetString != SetString (\"%s\" != \"%s\")\n",string,TestString);
		GlobalResult = False;
	}
	if (!TestResults.valueChanged)
	{
		printf("XmNvalueChangedCallback not called\n");
		GlobalResult = False;
	}
	if (!TestResults.modifyVerify)
	{
		printf("XmNmodifyVerifyCallback not called\n");
		GlobalResult = False;
	}
	if (!TestResults.modifyVerifyW)
	{
		printf("XmNmodifyVerifyCallbackWcs not called\n");
		GlobalResult = False;
	}
	if (!TestResults.motionVerify)
	{
		printf("XmNmotionVerifyCallback not called\n");
		GlobalResult = False;
	}
}

static void
StartTests(Widget W, Widget Text, XConfigureEvent *event)
{
	if (event->type == MapNotify)
	{
	    XtRemoveEventHandler(W, 
		    StructureNotifyMask, 
		    False, 
		    (XtEventHandler)StartTests, 
		    NULL);
	    TestSetString(Text);
	    if (GlobalResult)
	    {
	    	printf("\nAll tests passed\n");
	    }
	    else
	    {
	    	printf("\nOne or more tests failed\n");
	    }
	    if (AppResources.autoExit)
	    {
		XtAppAddTimeOut(XtWidgetToApplicationContext(W), 5000,
		  (XtTimerCallbackProc)exit, (XtPointer)!GlobalResult);
	    }
	}
}

int
main(int argc, char **argv)
{
  Widget toplevel, Text;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,
  	"TopLevel", 
  	opTable, XtNumber(opTable),
  	&argc, argv,
  	FallBack, NULL);

  XtGetApplicationResources(toplevel,
  		&AppResources,
  		resources, XtNumber(resources),
  		NULL,0);

  XtVaSetValues(toplevel,
  	NULL);

  Text = XmCreateTextField(toplevel, "Text", NULL, 0);
  XtManageChild(Text);

  XtRealizeWidget(toplevel);

  XtAddEventHandler(toplevel, 
	    StructureNotifyMask, 
	    False, 
	    (XtEventHandler)StartTests, 
	    Text);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,  138,   31, 0,0,0, /* Text */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
