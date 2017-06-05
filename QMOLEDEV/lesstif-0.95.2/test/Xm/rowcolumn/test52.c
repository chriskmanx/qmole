#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/CascadeB.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/PushBP.h>
#include <Xm/RepType.h>
#include <Xm/RowColumnP.h>
#include <Xm/SelectioB.h>
#include <Xm/DrawingA.h>
#include <stdio.h>
#include "../../common/Test.h"

Widget toplevel, box, push;
XtAppContext	app_context;
int DialogDone=False,DialogReturn;
Widget menubar;
Widget cascade1;
Widget pane1;
Widget button1;
Widget pane1_tear_off;

char *fallback[] = {
	"*tearOffModel:				tear_off_enabled",
	NULL	/* The end */
};

static char *
XdbRcType2String(unsigned char t)
{
    static char res[40];

    switch (t)
    {
    case XmWORK_AREA:
	return "XmWORK_AREA";

    case XmMENU_BAR:
	return "XmMENU_BAR";

    case XmMENU_PULLDOWN:
	return "XmMENU_PULLDOWN";

    case XmMENU_POPUP:
	return "XmMENU_POPUP";

    case XmMENU_OPTION:
	return "XmMENU_OPTION";

    default:
	sprintf(res, "Invalid RC Type %d", t);
	return res;
    }
}

static void
ReportRC(Widget w)
{
XmRowColumnPart *rc = &((XmRowColumnWidget)w)->row_column;

	printf("%7s %17s %15s %9s %11s %11s %11s %11s %17s %8s %11s %11s %s\n",
		XtName(w),
		XtName(XtParent(w)),
		XdbRcType2String(rc->type),
		rc->armed & XmRC_ARMED_BIT ? "armed" : "not-armed",
		rc->cascadeBtn ? XtName(rc->cascadeBtn) : "NULL",
		rc->memory_subwidget ? XtName(rc->memory_subwidget) : "NULL",
		rc->lastSelectToplevel ? XtName(rc->lastSelectToplevel) : "NULL",
		rc->popupPosted ? XtName(rc->popupPosted) : "NULL",
		rc->ParentShell ? XtName(rc->ParentShell) : "NULL",
		rc->to_state ? "torn" : "not-torn",
		rc->tear_off_lastSelectToplevel ? XtName(rc->tear_off_lastSelectToplevel) : "NULL",
		rc->tear_off_focus_item ? XtName(rc->tear_off_focus_item) : "NULL",
		"");
}

static void
Report()
{
	ReportRC(menubar);
	ReportRC(pane1);
	printf("\n");
}

int
main(int argc, char **argv)
{
/* Install converter to make the command line indicated above work */
    XmRepTypeInstallTearOffModelConverter();

/* Toplevel and Menu Bar */
    toplevel = XtVaAppInitialize(&app_context, "test50", NULL, 0, 
    	&argc, argv, fallback, NULL);
    menubar = XmCreateMenuBar(toplevel, "menubar", NULL, 0);
{
XmRowColumnPart *rc = &((XmRowColumnWidget)menubar)->row_column;

	rc->ParentShell = NULL;
	rc->tear_off_focus_item = NULL;
}


/* First Menu */
    pane1 = XmCreatePulldownMenu(menubar, "pane1", NULL, 0);
{
XmRowColumnPart *rc = &((XmRowColumnWidget)pane1)->row_column;

	rc->ParentShell = NULL;
	rc->tear_off_focus_item = NULL;
}
    /*
    XtAddCallback(pane1, XmNmapCallback, (void *)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane1, XmNtearOffMenuActivateCallback, (void *)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane1, XmNtearOffMenuDeactivateCallback, (void *)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane1, XmNunmapCallback, (void *)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(XtParent(pane1), XmNpopupCallback, (void *)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(XtParent(pane1), XmNpopdownCallback, (void *)GenericCallback, (XtPointer)__LINE__);
    */
    pane1_tear_off = XmGetTearOffControl(pane1);

    cascade1 = XtVaCreateManagedWidget("cascade1",
				       xmCascadeButtonWidgetClass, menubar, 
				       XmNsubMenuId,	pane1,
				       NULL);
    /*
    XtAddCallback(cascade1, XmNactivateCallback, (void *)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(cascade1, XmNcascadingCallback, (void *)GenericCallback, (XtPointer)__LINE__);
    */

    button1 = XtVaCreateManagedWidget("button1", 
    				xmPushButtonWidgetClass, pane1,
				NULL);

    XtManageChild(menubar);
    XtRealizeWidget(toplevel);
    LessTifTestWaitForIt(toplevel);
    printf("%7s %17s %15s %9s %11s %11s %11s %11s %17s %8s %11s %11s %s\n",
		"widget",
		"parent",
		"type",
		"arm",
		"cascadeBtn",
		"mem",
		"last",
		"popup",
		"shell",
		"torn",
		"to_last",
		"focus",
		"\n");

    Report();
    LessTifTestBtn1Down(cascade1);
    Report();
    LessTifTestBtn1Up(pane1_tear_off);
    Report();
    /*
    XtCallActionProc(XtParent(pane1_tear_off), "MenuEscape", NULL, NULL, 0);
    Report();
    */

    LessTifTestMainLoop(toplevel);
    exit(0);
}
