#include <stdio.h>
#include <Xm/PushB.h>
#include <Xm/DrawnB.h>
#include <Xm/CascadeB.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include "../../common/Test.h"

static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

typedef struct _CallbackResultStruct {
	char * WidgetName;
	int Reason;
	String entryData;
	Boolean IsRC;
	char *EntryWidget;
	String widgetData;
	int widgetReason;
} _CallbackResult;

_CallbackResult Results[] = { /* insert results of Motif run after this */
"RC", XmCR_ACTIVATE, "RC data", True, "PB", "PB data", XmCR_ACTIVATE,\
"RC", XmCR_ACTIVATE, "RC data", True, "PB", "PB data 1", XmCR_ACTIVATE,\
"RC", XmCR_ACTIVATE, "RC data", True, "DB", "DB data", XmCR_ACTIVATE,\
"RC", XmCR_ACTIVATE, "RC data", True, "TB", "TB data", XmCR_VALUE_CHANGED,\
	NULL, 0,  /* This must stay here */
};
int ResultIndex = 0;

static void
GenericCallback(Widget w, String id, XmAnyCallbackStruct *cbs)
{
Boolean IsRC = XmIsRowColumn(w);
String Name = w ? XtName(w) : "";
int Reason = cbs ? cbs->reason : XmCR_NONE;
XmRowColumnCallbackStruct *rc_cbs = (XmRowColumnCallbackStruct *)cbs;

    if (XtNumber(Results) == 1 || ResultIndex + 1 >= XtNumber(Results))
    {
    static Boolean FirstCall = True;

	if (FirstCall)
	{
		printf("/^_CallbackResult/a\\\n");
	}
	printf("\"%s\", %*s%s, \"%s\", %s, \"%s\", \"%s\", %s,\\\n", 
	    w ? XtName(w) : "",
	    /*20 - strlen(Name)*/ 0, "",
	    cbs ? XdbReason2String(cbs->reason) : "XmCR_NONE /* cbs is NULL */",
	    id,
	    IsRC ? "True" : "False",
	    IsRC ? (rc_cbs->widget ? XtName(rc_cbs->widget) : "") : "",
	    IsRC ? (rc_cbs->data ? rc_cbs->data : "") : "",
	    IsRC ? (rc_cbs->callbackstruct ? XdbReason2String(((XmAnyCallbackStruct *)(rc_cbs->callbackstruct))->reason) : "XmCR_NONE") : "0"
	    );
	GlobalErrors = 1;
	FirstCall = False;
    }
    else
    {
	printf("%s(%s), %*s%s(%s), %*s %s(%s), %s(%s), %s(%s) %s(%s), %s(%s) ", 
	    Name,
	    Results[ResultIndex].WidgetName,
	    /*20 - (strlen(Name) + strlen(Results[ResultIndex].WidgetName) + 2)*/ 0, "",
	    XdbReason2String(Reason),
	    XdbReason2String(Results[ResultIndex].Reason),
	    /*50 - strlen(XdbReason2String(Reason)) - strlen(XdbReason2String(Results[ResultIndex].Reason))*/ 0, "",
	    id, Results[ResultIndex].entryData,
	    IsRC ? "True" : "False", Results[ResultIndex].IsRC ? "True" : "False",
	    IsRC && rc_cbs->widget ? XtName(rc_cbs->widget) : "", Results[ResultIndex].EntryWidget,
	    IsRC ? rc_cbs->data : "", IsRC ? Results[ResultIndex].widgetData : "",
	    IsRC ? (rc_cbs->callbackstruct ? XdbReason2String(((XmAnyCallbackStruct *)(rc_cbs->callbackstruct))->reason) : "XmCR_NONE") : "", XdbReason2String(Results[ResultIndex].widgetReason)
	    );

    	if (strcmp(Name, Results[ResultIndex].WidgetName) == 0 &&
    	    strcmp(id, Results[ResultIndex].entryData) == 0 &&
    	    Results[ResultIndex].Reason == Reason &&
    	    IsRC == Results[ResultIndex].IsRC &&
    	    (IsRC && strcmp(XtName(rc_cbs->widget), Results[ResultIndex].EntryWidget) == 0) &&
    	    (IsRC && strcmp(rc_cbs->data, Results[ResultIndex].widgetData) == 0)
    	    )
    	{
	    printf("okay\n");
    	}
    	else
    	{
	    printf("bad\n");
	    GlobalErrors++;
    	}
    }
    ResultIndex++;
}

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget RC;
  Widget Button;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  RC = XmCreateRowColumn(Shell,"RC",NULL,0);
  XtAddCallback(RC, XmNentryCallback, (XtCallbackProc)GenericCallback, "RC data");

  Button = XmCreatePushButton(RC,"PB",NULL,0);
  XtAddCallback(Button, XmNactivateCallback, (XtCallbackProc)GenericCallback, "PB data");
  XtAddCallback(Button, XmNactivateCallback, (XtCallbackProc)GenericCallback, "PB data 1");
  XtManageChild(Button);

  /*
  Button = XmCreateCascadeButton(RC,"CB",NULL,0);
  XtAddCallback(Button, XmNactivateCallback, (XtCallbackProc)GenericCallback, "CB data");
  XtManageChild(Button);
  */

  Button = XmCreateDrawnButton(RC,"DB",NULL,0);
  XtAddCallback(Button, XmNactivateCallback, (XtCallbackProc)GenericCallback, "DB data");
  XtVaSetValues(Button,
  	XmNheight, 20,
  	NULL);
  XtManageChild(Button);

  Button = XmCreateToggleButton(RC,"TB",NULL,0);
  XtAddCallback(Button, XmNvalueChangedCallback, (XtCallbackProc)GenericCallback, "TB data");
  XtManageChild(Button);

  XtManageChild(RC);

  XtRealizeWidget(Shell);
  LessTifTestWaitForIt(Shell);
  {
  WidgetList kids;
  Cardinal num_kids;
  int i;

  	XtVaGetValues(RC,
  		XmNchildren, &kids,
  		XmNnumChildren, &num_kids,
  		NULL);
  	for (i = 0; i < num_kids; i++)
  	{
  		LessTifTestBtn1Down(kids[i]);
  		LessTifTestBtn1Up(kids[i]);
  	}
  }
  LessTifTestMainLoop(Shell);
  exit(0);
}
