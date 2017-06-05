/*
 * Also test with
 *	test1 -xrm "*tearOffModel: tear_off_enabled"
 */
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeBG.h>
#include <Xm/RepType.h>

void pb_activate_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    fprintf(stderr, "* Widget = %s - Activated\n", XtName(w));
}

void pb_arm_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    fprintf(stderr, "* Widget = %s - Armed\n", XtName(w));
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget cascade1;
    Widget pane1;
    Widget button1, button2, button3;

/* Install converter to make the command line indicated above work */
    XmRepTypeInstallTearOffModelConverter();

    toplevel = XtVaAppInitialize(&theApp, "test1", NULL, 0, &argc, argv, NULL, NULL);

    rc = XmCreateMenuBar(toplevel, "menubar", NULL, 0);

    pane1 = XmCreatePulldownMenu(rc, "pane1", NULL, 0);

    cascade1 = XtVaCreateManagedWidget("cascade1", xmCascadeButtonGadgetClass, rc,
		XmNsubMenuId, pane1,
	NULL);

    button1 = XtVaCreateManagedWidget("button1", xmPushButtonWidgetClass, pane1, NULL);
    button2 = XtVaCreateManagedWidget("button2", xmPushButtonWidgetClass, pane1, NULL);
    button3 = XtVaCreateManagedWidget("button3", xmPushButtonWidgetClass, pane1, NULL);

    XtAddCallback(button1, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button2, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button3, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button1, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button2, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button3, XmNarmCallback, pb_arm_callback, NULL);

    XtManageChild(rc);
    XtRealizeWidget(toplevel);
    

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   74,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   64,   21, 0,0,0, /* cascade1 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
    exit(0);
}
