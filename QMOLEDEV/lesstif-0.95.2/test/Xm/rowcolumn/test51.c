/* $Id: test51.c,v 1.29 2000/08/29 21:59:21 dannybackx Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/PushBP.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/RepType.h>
#include <Xm/RowColumn.h>
#include <Xm/SelectioB.h>
#include <Xm/DrawingA.h>
#include <Xm/Label.h>
#include <X11/keysym.h>
#include "../../common/Test.h"

/*
#define xmCascadeButtonWidgetClass xmCascadeButtonGadgetClass
#define xmPushButtonWidgetClass xmPushButtonGadgetClass
*/

#define NEW_ACTION(fn,st) static void fn (Widget w, XEvent *event, String *params, Cardinal *num_params) \
{ \
char *buf; \
\
    buf = XtMalloc(strlen(st) + 4); \
    sprintf(buf, "Old%s", st); \
    fprintf(stdout, "MyAction_%s(%s, %s) - %s %i params\n", st, XtName(w), \
    	XtClass(w)->core_class.class_name, \
    	event ? "event" : "no event", *num_params); \
    XtCallActionProc(w, buf, event, params, *num_params); \
    XtFree(buf); \
}

Widget toplevel, box, push;
XtAppContext	app_context;
int DialogDone=False,DialogReturn;

char *fallback[] = {
	"*tearOffModel:				tear_off_enabled",
	"*exitDelay: 100",
	"*cascade1.mnemonic:			c",
	NULL	/* The end */
};

typedef struct _CallbackResultStruct {
	char * WidgetName;
	int Reason;
} _CallbackResult;

_CallbackResult Results[] = { /* insert results of Motif run after this */
	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"pane1",                XmCR_UNMAP,\
	"button1",              XmCR_ACTIVATE,\
	"button1",              XmCR_DISARM,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"pane1",                XmCR_UNMAP,\
	"button1",              XmCR_ACTIVATE,\
	"button1",              XmCR_DISARM,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"cascade2",             XmCR_CASCADING,\
	"pane2",                XmCR_MAP,\
	"pane2",                XmCR_UNMAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"cascade2",             XmCR_CASCADING,\
	"pane2",                XmCR_MAP,\
	"button2",              XmCR_ARM,\
	"button2",              XmCR_DISARM,\
	"pane2",                XmCR_UNMAP,\
	"button1",              XmCR_ARM,\
	"pane1",                XmCR_UNMAP,\
	"button1",              XmCR_ACTIVATE,\
	"button1",              XmCR_DISARM,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"cascade2",             XmCR_CASCADING,\
	"pane2",                XmCR_MAP,\
	"pane2",                XmCR_UNMAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"cascade2",             XmCR_CASCADING,\
	"pane2",                XmCR_MAP,\
	"button2",              XmCR_ARM,\
	"button2",              XmCR_DISARM,\
	"pane2",                XmCR_UNMAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"cascade2",             XmCR_CASCADING,\
	"pane2",                XmCR_MAP,\
	"button2",              XmCR_ARM,\
	"pane2",                XmCR_UNMAP,\
	"pane1",                XmCR_UNMAP,\
	"button2",              XmCR_ACTIVATE,\
	"button2",              XmCR_DISARM,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"cascade2",             XmCR_CASCADING,\
	"pane2",                XmCR_MAP,\
	"button1",              XmCR_DISARM,\
	"button2",              XmCR_ARM,\
	"pane2",                XmCR_UNMAP,\
	"pane1",                XmCR_UNMAP,\
	"button2",              XmCR_ACTIVATE,\
	"button2",              XmCR_DISARM,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"pane1",                XmCR_UNMAP,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"pane1",                XmCR_UNMAP,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"cascade2",             XmCR_CASCADING,\
	"pane2",                XmCR_MAP,\
	"pane2",                XmCR_UNMAP,\
	"toggle1",              XmCR_ARM,\
	"toggle1",              XmCR_DISARM,\
	"cascade2",             XmCR_CASCADING,\
	"pane2",                XmCR_MAP,\
	"pane2",                XmCR_UNMAP,\
	"toggle1",              XmCR_ARM,\
	"pane1",                XmCR_UNMAP,\
	"toggle1",              XmCR_VALUE_CHANGED,\
	"toggle1",              XmCR_DISARM,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"toggle1",              XmCR_ARM,\
	"pane1",                XmCR_UNMAP,\
	"toggle1",              XmCR_VALUE_CHANGED,\
	"toggle1",              XmCR_DISARM,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"button3",              XmCR_ARM,\
	"pane1",                XmCR_UNMAP,\
	"button3",              XmCR_ACTIVATE,\
	"button3",              XmCR_DISARM,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"pane1",                XmCR_UNMAP,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"pane1",                XmCR_UNMAP,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"cascade2",             XmCR_CASCADING,\
	"pane2",                XmCR_MAP,\
	"button1",              XmCR_DISARM,\
	"button2",              XmCR_ARM,\
	"button2",              XmCR_DISARM,\
	"pane2",                XmCR_UNMAP,\
	"pane1",                XmCR_UNMAP,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"toggle1",              XmCR_ARM,\
	"toggle1",              XmCR_DISARM,\
	"pane1",                XmCR_UNMAP,\
	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"toggle1",              XmCR_ARM,\
	"toggle1",              XmCR_DISARM,\
	"pane1",                XmCR_UNMAP,\
	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"cascade3",             XmCR_CASCADING,\
	"pane1",                XmCR_UNMAP,\
	"cascade3",             XmCR_ACTIVATE,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"cascade3",             XmCR_CASCADING,\
	"button1",              XmCR_DISARM,\
	"pane1",                XmCR_UNMAP,\
	"cascade3",             XmCR_ACTIVATE,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"cascade3",             XmCR_CASCADING,\
	"pane1",                XmCR_UNMAP,\
	"pane3",                XmCR_MAP,\
	"button4",              XmCR_ARM,\
	"pane3",                XmCR_UNMAP,\
	"button4",              XmCR_ACTIVATE,\
	"button4",              XmCR_DISARM,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"cascade3",             XmCR_CASCADING,\
	"pane1",                XmCR_UNMAP,\
	"pane3",                XmCR_MAP,\
	"button1",              XmCR_DISARM,\
	"button4",              XmCR_ARM,\
	"pane3",                XmCR_UNMAP,\
	"button4",              XmCR_ACTIVATE,\
	"button4",              XmCR_DISARM,\

	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"pane1",                XmCR_UNMAP,\
	"pane1",                XmCR_TEAR_OFF_ACTIVATE,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"button1",              XmCR_DISARM,\
	"pane1",                XmCR_UNMAP,\
	"pane1",                XmCR_TEAR_OFF_DEACTIVATE,\
	"cascade1",             XmCR_CASCADING,\
	"pane1",                XmCR_MAP,\
	"pane1",                XmCR_UNMAP,\
	"pane1",                XmCR_TEAR_OFF_ACTIVATE,\
	"pane1",                XmCR_MAP,\
	"button1",              XmCR_ARM,\
	"cascade2",             XmCR_CASCADING,\
	"pane2",                XmCR_MAP,\
	"button1",              XmCR_DISARM,\
	"pane2",                XmCR_UNMAP,\
	"pane2",                XmCR_TEAR_OFF_ACTIVATE,\
	"pane2",                XmCR_MAP,\
	"button2",              XmCR_ARM,\
	"button2",              XmCR_ACTIVATE,\
	"button2",              XmCR_DISARM,\
	"button2",              XmCR_ARM,\
        "button2",              XmCR_DISARM,\
        "pane2",                XmCR_UNMAP,\
        "pane2",                XmCR_TEAR_OFF_DEACTIVATE,\
        "pane1",                XmCR_UNMAP,\
        "pane1",                XmCR_TEAR_OFF_DEACTIVATE,\
        "cascade1",		XmCR_CASCADING, \
        "pane1",		XmCR_MAP, \
        "button1",		XmCR_ARM, \
        "button1",		XmCR_DISARM, \
        "pane1",		XmCR_UNMAP, \
        "button3",		XmCR_ARM, \
        "button3",		XmCR_ACTIVATE, \
        "button3",		XmCR_DISARM, \

	NULL, 0,  /* This must stay here */
};
int ResultIndex = 0;

NEW_ACTION(DelayedArm, "DelayedArm")
NEW_ACTION(CheckDisarm, "CheckDisarm")
NEW_ACTION(StartDrag, "StartDrag")
NEW_ACTION(_XmCBMenuBarDoSelect, "DoSelect")
NEW_ACTION(KeySelect, "KeySelect")
NEW_ACTION(_XmCBMenuBarSelect, "MenuBarSelect")
NEW_ACTION(MenuBarEnter, "MenuBarEnter")
NEW_ACTION(MenuBarLeave, "MenuBarLeave")
NEW_ACTION(CleanupMenuBar, "CleanupMenuBar")
NEW_ACTION(_XmCBHelp1, "Help")

static XtActionsRec My_CB_Actions[] =
{
    {"DelayedArm", DelayedArm},
    {"CheckDisarm", CheckDisarm},
    {"StartDrag", StartDrag},
    {"DoSelect", _XmCBMenuBarDoSelect},
    {"KeySelect", KeySelect},
    {"MenuBarSelect", _XmCBMenuBarSelect},
    {"MenuBarEnter", MenuBarEnter},
    {"MenuBarLeave", MenuBarLeave},
    {"CleanupMenuBar", CleanupMenuBar},
    {"Help", _XmCBHelp1},
};

NEW_ACTION(PBArm, "Arm")
NEW_ACTION(PBMultiArm, "MultiArm")
NEW_ACTION(PBActivate, "Activate")
NEW_ACTION(PBMultiActivate, "MultiActivate")
NEW_ACTION(PBArmAndActivate, "ArmAndActivate")
NEW_ACTION(PBDisarm, "Disarm")
NEW_ACTION(PBButtonDown, "BtnDown")
NEW_ACTION(PBButtonUp, "BtnUp")
NEW_ACTION(PBEnterWindow, "Enter")
NEW_ACTION(PBLeaveWindow, "Leave")
NEW_ACTION(PBHelp, "Help")

static XtActionsRec My_PB_Actions[] =
{
    {"Arm", PBArm},
    {"MultiArm", PBMultiArm},
    {"Activate", PBActivate},
    {"MultiActivate", PBMultiActivate},
    {"ArmAndActivate", PBArmAndActivate},
    {"Disarm", PBDisarm},
    {"BtnDown", PBButtonDown},
    {"BtnUp", PBButtonUp},
    {"Enter", PBEnterWindow},
    {"Leave", PBLeaveWindow},
    {"Help", PBHelp},
};

NEW_ACTION(RC_XmHelp, "Help")
NEW_ACTION(RC_XmMenuHelp, "MenuHelp")
NEW_ACTION(RC_XmMenuBtnDown, "MenuBtnDown")
NEW_ACTION(RC_XmMenuBtnUp, "MenuBtnUp")
NEW_ACTION(RC_XmPulldownBtnDown, "PulldownBtnDown")
NEW_ACTION(RC_XmPulldownBtnUp, "PulldownBtnUp")
NEW_ACTION(RC_XmPopupBtnDown, "PopupBtnDown")
NEW_ACTION(RC_XmPopupBtnUp, "PopupBtnUp")
NEW_ACTION(RC_XmMenuBarBtnDown, "MenuBarBtnDown")
NEW_ACTION(RC_XmMenuBarBtnUp, "MenuBarBtnUp")
NEW_ACTION(RC_XmWorkAreaBtnDown, "WorkAreaBtnDown")
NEW_ACTION(RC_XmWorkAreaBtnUp, "WorkAreaBtnUp")
NEW_ACTION(RC_MenuBarGadgetSelect, "MenuBarGadgetSelect")
NEW_ACTION(RC_XmRC_FocusOut, "FocusOut")
NEW_ACTION(RC_XmRC_FocusIn, "FocusIn")
NEW_ACTION(RC_Unmap, "Unmap")
NEW_ACTION(RC_XmNoop, "Noop")
NEW_ACTION(RC_XmMenuTraverseLeft, "MenuTraverseLeft")
NEW_ACTION(RC_XmMenuTraverseRight, "MenuTraverseRight")
NEW_ACTION(RC_XmMenuTraverseUp, "MenuTraverseUp")
NEW_ACTION(RC_XmMenuTraverseDown, "MenuTraverseDown")
NEW_ACTION(RC_XmMenuEscape, "MenuEscape")
NEW_ACTION(RC_MenuFocusIn, "MenuFocusIn")
NEW_ACTION(RC_MenuFocusOut, "MenuFocusOut")
NEW_ACTION(RC_MenuUnmap, "MenuUnmap")
NEW_ACTION(RC_MenuEnter, "MenuEnter")
NEW_ACTION(RC_XmMenuReturn, "MenuGadgetReturn")
NEW_ACTION(RC_MenuGadgetTraverseLeft, "MenuGadgetTraverseLeft")
NEW_ACTION(RC_MenuGadgetTraverseRight, "MenuGadgetTraverseRight")
NEW_ACTION(RC_MenuGadgetTraverseUp, "MenuGadgetTraverseUp")
NEW_ACTION(RC_MenuGadgetTraverseDown, "MenuGadgetTraverseDown")

static XtActionsRec My_RC_Actions[] =
{
    {"Help", RC_XmHelp},
    {"MenuHelp", RC_XmMenuHelp},
    {"MenuBtnDown", RC_XmMenuBtnDown},
    {"MenuBtnUp", RC_XmMenuBtnUp},
    {"PulldownBtnDown", RC_XmPulldownBtnDown},
    {"PulldownBtnUp", RC_XmPulldownBtnUp},
    {"PopupBtnDown", RC_XmPopupBtnDown},
    {"PopupBtnUp", RC_XmPopupBtnUp},
    {"MenuBarBtnDown", RC_XmMenuBarBtnDown},
    {"MenuBarBtnUp", RC_XmMenuBarBtnUp},
    {"WorkAreaBtnDown", RC_XmWorkAreaBtnDown},
    {"WorkAreaBtnUp", RC_XmWorkAreaBtnUp},
    {"MenuBarGadgetSelect", RC_MenuBarGadgetSelect},
    {"FocusOut", RC_XmRC_FocusOut},
    {"FocusIn", RC_XmRC_FocusIn},
    {"Unmap", RC_Unmap},
    {"Noop", RC_XmNoop},
    {"MenuTraverseLeft", RC_XmMenuTraverseLeft},
    {"MenuTraverseRight", RC_XmMenuTraverseRight},
    {"MenuTraverseUp", RC_XmMenuTraverseUp},
    {"MenuTraverseDown", RC_XmMenuTraverseDown},
    {"MenuEscape", RC_XmMenuEscape},
    {"MenuFocusIn", RC_MenuFocusIn},
    {"MenuFocusOut", RC_MenuFocusOut},
    {"MenuUnmap", RC_MenuUnmap},
    {"MenuEnter", RC_MenuEnter},
    {"MenuGadgetReturn", RC_XmMenuReturn},
    {"MenuGadgetEscape", RC_XmMenuEscape},
    {"MenuGadgetTraverseLeft", RC_MenuGadgetTraverseLeft},
    {"MenuGadgetTraverseRight", RC_MenuGadgetTraverseRight},
    {"MenuGadgetTraverseUp", RC_MenuGadgetTraverseUp},
    {"MenuGadgetTraverseDown", RC_MenuGadgetTraverseDown},
};


static void
HijackActions(WidgetClass wc, XtActionsRec *new, int num_new)
{
    XtActionsRec *NewActions;
    int i;
    int j;

    NewActions = (XtActionsRec *)XtMalloc((num_new + wc->core_class.num_actions) * sizeof(XtActionsRec));
    for (i = 0; i < num_new; i++)
    {
	NewActions[i] = new[i];
    }
    for (i = 0; i < wc->core_class.num_actions; i++)
    {
	NewActions[num_new + i] = wc->core_class.actions[i];
	for (j = 0; j < num_new; j++)
	{
	    if (strcmp(NewActions[num_new + i].string, new[j].string) == 0)
	    {
		NewActions[num_new + i].string = XtMalloc(strlen(wc->core_class.actions[i].string) + 4);
		sprintf(NewActions[num_new + i].string, "Old%s", wc->core_class.actions[i].string);
		break;
	    }
	}
    }
    wc->core_class.actions = NewActions;
    wc->core_class.num_actions += num_new;
}

static void
GenericCallback(Widget w, int id, XmAnyCallbackStruct *cbs)
{
String Name = w ? XtName(w) : "";
int Reason = cbs ? cbs->reason : XmCR_NONE;

    if (XtNumber(Results) == 1 || ResultIndex + 1 >= XtNumber(Results))
    {
    static Boolean FirstCall = True;

	if (FirstCall)
	{
		printf("/^_CallbackResult/a\\\n");
	}
	printf("\t\"%s\", %*s%s,\\\n", 
	    w ? XtName(w) : "",
	    20 - strlen(Name), "",
	    cbs ? XdbReason2String(cbs->reason) : "XmCR_NONE /* cbs is NULL */");
	GlobalErrors = 1;
	FirstCall = False;
    }
    else
    {
	printf("%s(%s), %*s%s(%s)%*s", 
	    Name,
	    Results[ResultIndex].WidgetName,
	    20 - (strlen(Name) + strlen(Results[ResultIndex].WidgetName) + 2), "",
	    XdbReason2String(Reason),
	    XdbReason2String(Results[ResultIndex].Reason),
	    50 - strlen(XdbReason2String(Reason)) - strlen(XdbReason2String(Results[ResultIndex].Reason)), "");

    	if (strcmp(Name, Results[ResultIndex].WidgetName) == 0 &&
    	    Results[ResultIndex].Reason == Reason
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
    Widget menubar;
    Widget cascade1, pane1, button1, pane1_tear_off, toggle1, button3, label1;
    Widget cascade2, pane2, button2, pane2_tear_off;
    Widget cascade3, pane3, button4, pane3_tear_off;
    int mappingDelay;

/* Install converter to make the command line indicated above work */
    XmRepTypeInstallTearOffModelConverter();
    /*
    HijackActions(xmCascadeButtonWidgetClass, My_CB_Actions, XtNumber(My_CB_Actions));
    HijackActions(xmPushButtonWidgetClass, My_PB_Actions, XtNumber(My_PB_Actions));
    HijackActions(xmRowColumnWidgetClass, My_RC_Actions, XtNumber(My_RC_Actions));
    */

/* Toplevel and Menu Bar */
    toplevel = XtVaAppInitialize(&app_context, "test50", NULL, 0, 
    	&argc, argv, fallback, NULL);
    XtVaSetValues(toplevel,
    	XmNallowShellResize, True,
    	NULL);
    menubar = XmCreateMenuBar(toplevel, "menubar", NULL, 0);
    XtVaSetValues(menubar,
    	XmNwidth, 100,
    	XmNheight, 100,
    	NULL);


/* First Menu */
    pane1 = XmCreatePulldownMenu(menubar, "pane1", NULL, 0);
    XtAddCallback(pane1, XmNmapCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane1, XmNtearOffMenuActivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane1, XmNtearOffMenuDeactivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane1, XmNunmapCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    /*XtAddCallback(XtParent(pane1), XmNpopupCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(XtParent(pane1), XmNpopdownCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);*/
    pane1_tear_off = XmGetTearOffControl(pane1);

    cascade1 = XtVaCreateWidget("cascade1",
				       xmCascadeButtonWidgetClass, menubar, 
				       XmNsubMenuId,	pane1,
				       NULL);
    XtVaGetValues(cascade1,
    	XmNmappingDelay, &mappingDelay,
    	NULL);
    XtAddCallback(cascade1, XmNactivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(cascade1, XmNcascadingCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);

    button1 = XtVaCreateManagedWidget("button1", 
    				xmPushButtonWidgetClass, pane1,
				NULL);
    XtAddCallback(button1, XmNactivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(button1, XmNarmCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(button1, XmNdisarmCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);

/* Second Menu */
    pane2 = XmCreatePulldownMenu(pane1, "pane2", NULL, 0);
    XtAddCallback(pane2, XmNmapCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane2, XmNtearOffMenuActivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane2, XmNtearOffMenuDeactivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane2, XmNunmapCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    /*XtAddCallback(XtParent(pane2), XmNpopupCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(XtParent(pane2), XmNpopdownCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);*/
    pane2_tear_off = XmGetTearOffControl(pane2);

    cascade2 = XtVaCreateManagedWidget("cascade2",
				       xmCascadeButtonWidgetClass, pane1, 
				       XmNsubMenuId,	pane2,
				       NULL);
    XtAddCallback(cascade2, XmNactivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(cascade2, XmNcascadingCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);

    button2 = XtVaCreateManagedWidget("button2", 
    				xmPushButtonWidgetClass, pane2,
				NULL);
    XtAddCallback(button2, XmNactivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(button2, XmNarmCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(button2, XmNdisarmCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);

    XtManageChild(menubar);
    XtRealizeWidget(toplevel);
    LessTifTestWaitForIt(toplevel);

    /*
    LessTifTestPrintEvents(toplevel, True);
    */

    LessTifTestWarpPointer(menubar);
    LessTifTestBtn1Down(menubar);
    LessTifTestBtn1Up(menubar);

    /*
    LessTifTestPrintEvents(toplevel, False);
    */
    XtManageChild(cascade1);
    /*
    LessTifTestPrintEvents(toplevel, True);
    */

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(button1);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(button1);
    LessTifTestBtn1Up(button1);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestWarpPointer(pane1_tear_off);
    LessTifTestWarpPointer(button1);
    LessTifTestWarpPointer(cascade2);
    LessTifTestDelay(cascade2, mappingDelay);
    LessTifTestWarpPointer(button1);
    LessTifTestWarpPointer(cascade2);
    LessTifTestDelay(cascade2, mappingDelay);
    LessTifTestWarpPointer(button2);
    LessTifTestBtn1Up(button1);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestWarpPointer(pane1_tear_off);
    LessTifTestWarpPointer(button1);
    LessTifTestWarpPointer(cascade2);
    LessTifTestDelay(cascade2, mappingDelay);
    LessTifTestWarpPointer(button1);
    LessTifTestWarpPointer(cascade2);
    LessTifTestDelay(cascade2, mappingDelay);
    LessTifTestWarpPointer(button2);
    LessTifTestWarpPointer(cascade2);
    LessTifTestDelay(cascade2, mappingDelay);
    LessTifTestWarpPointer(button1);
    LessTifTestWarpPointer(cascade2);
    LessTifTestDelay(cascade2, mappingDelay);
    LessTifTestWarpPointer(button2);
    LessTifTestBtn1Up(button2);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(cascade2);
    LessTifTestBtn1Up(cascade2);
    LessTifTestBtn1Down(button2);
    LessTifTestBtn1Up(button2);
    printf("\n");

    {
    Dimension w, h;

    	XtVaGetValues(menubar,
    		XmNwidth, &w,
    		XmNheight, &h,
    		NULL);
    	LessTifTestResizeWidget(XtParent(menubar), 3 * w, h);
    }
    /*
    LessTifTestPrintEvents(toplevel, True);
    */
    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(menubar);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(menubar);
    LessTifTestBtn1Up(menubar);
    printf("\n");

    toggle1 = XmCreateToggleButton(pane1, "toggle1", NULL, 0);
    XtAddCallback(toggle1, XmNvalueChangedCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(toggle1, XmNarmCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(toggle1, XmNdisarmCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtManageChild(toggle1);

    LessTifTestBtn1Down(cascade1);
    LessTifTestWarpPointer(pane1_tear_off);
    LessTifTestWarpPointer(button1);
    LessTifTestWarpPointer(cascade2);
    LessTifTestDelay(cascade2, mappingDelay);
    LessTifTestWarpPointer(toggle1);
    LessTifTestWarpPointer(cascade2);
    LessTifTestDelay(cascade2, mappingDelay);
    LessTifTestWarpPointer(toggle1);
    LessTifTestBtn1Up(toggle1);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(toggle1);
    LessTifTestBtn1Up(toggle1);
    printf("\n");

    button3 = XtVaCreateManagedWidget("button3", 
    				xmPushButtonWidgetClass, pane1,
				NULL);
    XtAddCallback(button3, XmNactivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(button3, XmNarmCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(button3, XmNdisarmCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(button3);
    LessTifTestBtn1Up(button3);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(menubar);
    LessTifTestBtn1Up(menubar);
    printf("\n");

    label1 = XmCreateLabel(pane1, "ThisIsALabel", NULL, 0);
    XtVaSetValues(label1,
    	XmNpositionIndex, 0,
    	NULL);
    XtManageChild(label1);
    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(menubar);
    LessTifTestBtn1Up(menubar);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(cascade2);
    LessTifTestBtn1Up(cascade2);
    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(menubar);
    printf("\n");

    XtSetSensitive(button1, False);
    XtSetSensitive(cascade2, False);
    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(menubar);
    LessTifTestBtn1Up(menubar);

    XtSetSensitive(button1, True);
    XtUnmanageChild(button1);
    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(menubar);
    LessTifTestBtn1Up(menubar);

    XtManageChild(button1);
    XtSetSensitive(cascade2, True);

    cascade3 = XtVaCreateWidget("cascade3",
				       xmCascadeButtonWidgetClass, menubar, 
				       NULL);
    XtAddCallback(cascade3, XmNactivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(cascade3, XmNcascadingCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtManageChild(cascade3);

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade3);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(cascade3);
    LessTifTestBtn1Up(cascade3);
    printf("\n");

    pane3 = XmCreatePulldownMenu(menubar, "pane3", NULL, 0);
    XtAddCallback(pane3, XmNmapCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane3, XmNtearOffMenuActivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane3, XmNtearOffMenuDeactivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(pane3, XmNunmapCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    pane3_tear_off = XmGetTearOffControl(pane3);
    button4 = XtVaCreateManagedWidget("button4", 
    				xmPushButtonWidgetClass, pane3,
				NULL);
    XtAddCallback(button4, XmNactivateCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(button4, XmNarmCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtAddCallback(button4, XmNdisarmCallback, (XtCallbackProc)GenericCallback, (XtPointer)__LINE__);
    XtVaSetValues(cascade3,
    	XmNsubMenuId, pane3,
    	NULL);

    LessTifTestBtn1Down(cascade1);
    LessTifTestWarpPointer(pane1_tear_off);
    LessTifTestWarpPointer(cascade1);
    LessTifTestWarpPointer(cascade3);
    LessTifTestWarpPointer(pane3_tear_off);
    LessTifTestBtn1Up(button4);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn1Down(cascade3);
    LessTifTestBtn1Up(cascade3);
    LessTifTestBtn1Down(button4);
    LessTifTestBtn1Up(button4);
    printf("\n");

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(pane1_tear_off);
    LessTifTestWaitForIt(XtParent(pane1));
    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(pane1_tear_off);
    LessTifTestWaitForIt(XtParent(pane1));
    LessTifTestBtn1Down(cascade2);
    LessTifTestBtn1Up(pane2_tear_off);
    LessTifTestWaitForIt(XtParent(pane2));
    LessTifTestBtn1Down(button2);
    LessTifTestBtn1Up(button2);

#if 0
    LessTifTestEsc(button2);
    LessTifTestEsc(button1);

    XtVaSetValues(button3,
    	XmNmnemonic, 'b',
    	NULL);
    LessTifTestKeyPress(cascade1, XK_Alt_L, 0x0);
    LessTifTestKeyPress(cascade1, XK_C, 0x8);
    LessTifTestKeyRelease(XtParent(cascade1), XK_C, 0x8);
    LessTifTestKeyRelease(XtParent(cascade1), XK_Alt_L, 0x8);
    LessTifTestWaitForIt(XtParent(pane1));
    LessTifTestKeyRelease(XtParent(cascade1), XK_B, 0x0);

    LessTifTestBtn1Down(cascade1);
    LessTifTestBtn1Up(cascade1);
    LessTifTestBtn3Down(cascade3);
    LessTifTestBtn3Up(cascade3);
    printf("\n");

    /*
    LessTifTestPrintEvents(toplevel, False);
    */

    printf("\n"); /* need final \n for output file or sed doesn't work on
		     SGI */
#endif
    printf("%s\n", GlobalErrors == 0 ? "All okay" : "One or more failed");
    LessTifTestMainLoop(toplevel);
    exit(0);
}
