/* $Id: test45.c,v 1.6 2001/05/15 14:46:10 amai Exp $ */
/** test45 -- a menu bar with two pulldown menu, all made of widgets
**/

#include <stdlib.h>
#include <stdio.h>
#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>
#include <Xm/PushBP.h>
#include <Xm/CascadeBP.h>

#define NEW_ACTION(fn,st) static void fn (Widget w, XEvent *event, String *params, Cardinal *num_params) \
{ \
char *buf; \
\
    buf = XtMalloc(strlen(st) + 4); \
    sprintf(buf, "Old%s", st); \
    fprintf(stderr, "MyAction_%s(%s, %s) - %s %i params\n", st, XtName(w), \
    	XtClass(w)->core_class.class_name, \
    	event ? "event" : "no event", *num_params); \
    XtCallActionProc(w, buf, event, params, *num_params); \
    XtFree(buf); \
}

static void (*CB_ArmAndActivate) () = NULL;

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

static void (*PB_ArmAndActivate) () = NULL;

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
MyCB_ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    fprintf(stderr, "MyCB_ArmAndActivate(%s)\n",
	    XtName(w));
    (*CB_ArmAndActivate) (w, event, params, num_params);
}

static void
MyPB_ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    fprintf(stderr, "MyPB_ArmAndActivate(%s)\n",
	    XtName(w));
    (*PB_ArmAndActivate) (w, event, params, num_params);
}

static XtActionProc
HijackArmAndActivate(WidgetClass wc, XtActionProc new)
{
    XtActionProc Old;

    Old = ((XmPrimitiveWidgetClass)wc)->primitive_class.arm_and_activate;
    ((XmPrimitiveWidgetClass)wc)->primitive_class.arm_and_activate = new;
    return (Old);
}

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
PopupCallback(Widget w)
{
	printf("PopupCallback(%s)\n", XtName(w));
}

static void
PopdownCallback(Widget w)
{
	printf("PopdownCallback(%s)\n", XtName(w));
}

static void
MapCallback(Widget w)
{
	printf("MapCallback(%s)\n", XtName(w));
}

static void
UnmapCallback(Widget w)
{
	printf("UnmapCallback(%s)\n", XtName(w));
}

static void
CascadingCallback(Widget w)
{
	printf("CascadingCallback(%s)\n", XtName(w));
}

void
exit_cb(void)
{
    exit(0);
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget cascade1, cascade2, cascade3;
    Widget pane1, pane2, pane3;
    Widget button, button1, button2, button3, button4, button5;

    CB_ArmAndActivate = HijackArmAndActivate(xmCascadeButtonWidgetClass, MyCB_ArmAndActivate);
    HijackActions(xmCascadeButtonWidgetClass, My_CB_Actions, XtNumber(My_CB_Actions));

    /*
    PB_ArmAndActivate = HijackArmAndActivate(xmPushButtonWidgetClass, MyPB_ArmAndActivate);
    HijackActions(xmPushButtonWidgetClass, My_PB_Actions, XtNumber(My_PB_Actions));

    HijackActions(xmRowColumnWidgetClass, My_RC_Actions, XtNumber(My_RC_Actions));
    */

    toplevel = XtVaAppInitialize(&theApp, "test33", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XmCreateMenuBar(toplevel, "menubar", NULL, 0);
    XtAddCallback(rc, XmNmapCallback, (XtCallbackProc)MapCallback, NULL);
    XtAddCallback(rc, XmNunmapCallback, (XtCallbackProc)UnmapCallback, NULL);

    pane1 = XmCreatePulldownMenu(rc, "pane1", NULL, 0);
    XtAddCallback(XtParent(pane1), XmNpopupCallback, (XtCallbackProc)PopupCallback, NULL);
    XtAddCallback(XtParent(pane1), XmNpopdownCallback, (XtCallbackProc)PopdownCallback, NULL);
    XtAddCallback(pane1, XmNmapCallback, (XtCallbackProc)MapCallback, NULL);
    XtAddCallback(pane1, XmNunmapCallback, (XtCallbackProc)UnmapCallback, NULL);

    pane2 = XmCreatePulldownMenu(rc, "pane2", NULL, 0);
    XtAddCallback(XtParent(pane2), XmNpopupCallback, (XtCallbackProc)PopupCallback, NULL);
    XtAddCallback(XtParent(pane2), XmNpopdownCallback, (XtCallbackProc)PopdownCallback, NULL);
    XtAddCallback(pane2, XmNmapCallback, (XtCallbackProc)MapCallback, NULL);
    XtAddCallback(pane2, XmNunmapCallback, (XtCallbackProc)UnmapCallback, NULL);

    cascade1 = XtVaCreateManagedWidget("File",
				       xmCascadeButtonWidgetClass,
				       rc,
				       XmNsubMenuId, pane1,
				       XmNmnemonic, 'F',
				       NULL);
    XtAddCallback(cascade1, XmNcascadingCallback, (XtCallbackProc)CascadingCallback, NULL);

    cascade2 = XtVaCreateManagedWidget("Edit",
				       xmCascadeButtonWidgetClass,
				       rc,
				       XmNsubMenuId, pane2,
				       XmNmnemonic, 'E',
				       NULL);
    XtAddCallback(cascade2, XmNcascadingCallback, (XtCallbackProc)CascadingCallback, NULL);

    button1 = XtVaCreateManagedWidget("Quit",
				      xmPushButtonWidgetClass,
				      pane1,
				      XmNmnemonic, 'Q',
				      NULL);
    XtAddCallback(button1, XmNactivateCallback, (XtCallbackProc)exit_cb, NULL);

    pane3 = XmCreatePulldownMenu(pane1,
				 "pane3",
				 NULL, 0);

    cascade3 = XtVaCreateManagedWidget("cascade3",
				       xmCascadeButtonWidgetClass,
				       pane1,
				       XmNsubMenuId, pane3,
				       NULL);

    button2 = XtVaCreateManagedWidget("button2",
				      xmPushButtonWidgetClass,
				      pane3,
				      NULL);

    button3 = XtVaCreateManagedWidget("button3",
				      xmPushButtonWidgetClass,
				      pane3,
				      NULL);

    button4 = XtVaCreateManagedWidget("button4",
				      xmPushButtonWidgetClass,
				      pane2,
				      NULL);

    button5 = XtVaCreateManagedWidget("button5",
				      xmPushButtonWidgetClass,
				      pane2,
				      NULL);

    XtManageChild(rc);

    XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	90,	31,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	5,	5,	40,	21,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	45,	5,	40,	21,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);
    */
    exit(0);
}
