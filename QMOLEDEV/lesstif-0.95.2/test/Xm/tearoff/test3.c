/* $Header: /cvsroot/lesstif/lesstif/test/Xm/tearoff/test3.c,v 1.2 1998/10/06 02:19:14 jon Exp $ */

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <stdio.h>

void Callback(Widget w, XtPointer client, XtPointer call)
{
    printf("* Widget = %s - %s\n", XtName(w), (char *)client);
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget cascade1;
    Widget pane1;
    Widget w;

    toplevel = XtVaAppInitialize(&theApp, "test1", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XmCreateMenuBar(toplevel, "menubar", NULL, 0);

    pane1 = XmCreatePulldownMenu(rc, "pane1", NULL, 0);

    /* put a tear off button on this pulldown */
    XtVaSetValues(pane1, 
                  XmNtearOffModel, XmTEAR_OFF_ENABLED,
                  NULL);

    cascade1 = XtVaCreateManagedWidget("cascade1", xmCascadeButtonWidgetClass, rc,
		XmNsubMenuId, pane1,
	NULL);

    w = XtVaCreateManagedWidget("button 1", xmPushButtonWidgetClass, pane1, NULL);
    XtAddCallback(w, XmNactivateCallback, Callback, "Activate");
    XtAddCallback(w, XmNarmCallback, Callback, "Arm");
    XtAddCallback(w, XmNdisarmCallback, Callback, "Disarm");

    w = XtVaCreateManagedWidget("button 2", xmPushButtonWidgetClass, pane1, NULL);
    XtAddCallback(w, XmNactivateCallback, Callback, "Activate");
    XtAddCallback(w, XmNarmCallback, Callback, "Arm");
    XtAddCallback(w, XmNdisarmCallback, Callback, "Disarm");

    w = XtVaCreateManagedWidget("button 2", xmSeparatorWidgetClass, pane1, NULL);

    w = XtVaCreateManagedWidget("gadget 3", xmPushButtonGadgetClass, pane1, NULL);
    XtAddCallback(w, XmNactivateCallback, Callback, "Activate");
    XtAddCallback(w, XmNarmCallback, Callback, "Arm");
    XtAddCallback(w, XmNdisarmCallback, Callback, "Disarm");

    XtManageChild(rc);
    XtRealizeWidget(toplevel);

    fprintf(stderr, "TearOff is %p\n", XmGetTearOffControl(pane1));

    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   74,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   64,   21, 0,0,0, /* cascade1 */ 
    };
    PrintDetails(    toplevel ,Expected);
};
   LessTifTestMainLoop(    toplevel );    
    exit(0);
}


