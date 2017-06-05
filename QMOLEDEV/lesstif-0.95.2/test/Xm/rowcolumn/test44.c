/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test44.c,v 1.2 2001/05/15 14:46:10 amai Exp $
I have found a bug in LessTif 0.87.0 concerning pulldown menu traversal. 
When entering a submenu using the arrowkeys, LessTif triggers a SIGSEGV 
signal. The problem seems to have to do with the use of cascade button 
gadgets. The following changes to test/Xm/rowcolumn/test7.c makes the 
bug show:
*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>
#include <Xm/PushBP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBG.h>



void pb_activate_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    unsigned char x;

    XtVaGetValues(XtParent(w), XmNpacking, &x, NULL);
    fprintf(stderr, "* Widget = %s - Activated\n", XtName(w));
    fprintf(stderr, "# RC Packing is %d\n", x);
}

void pb_arm_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    fprintf(stderr, "* Widget = %s - Armed\n", XtName(w));
}

void pb_disarm_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    fprintf(stderr, "* Widget = %s - Disarm\n", XtName(w));
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget cascade1, cascade2, cascade3;
    Widget pane1, pane2, pane3;
    Widget button, button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test7", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XmCreateMenuBar(toplevel,
			 "menubar",
			 NULL, 0);

    pane1 = XmCreatePulldownMenu(rc,
				 "pane1",
				 NULL, 0);

    pane2 = XmCreatePulldownMenu(rc,
				 "pane2",
				 NULL, 0);

    cascade1 = XtVaCreateManagedWidget("File",
				       xmCascadeButtonGadgetClass,
				       rc,
				       XmNsubMenuId, pane1,
				       XmNmnemonic, 'F',
				       NULL);

    cascade2 = XtVaCreateManagedWidget("Edit",
				       xmCascadeButtonWidgetClass,
				       rc,
				       XmNsubMenuId, pane2,
				       XmNmnemonic, 'E',
				       NULL);

    button1 = XtVaCreateManagedWidget("Quit",
				      xmPushButtonWidgetClass,
				      pane1,
				      XmNmnemonic, 'Q',
				      NULL);

    pane3 = XmCreatePulldownMenu(pane1,
                                 "pane3",
                                 NULL, 0);

    cascade3 = XtVaCreateManagedWidget("cascade3",
				       xmCascadeButtonGadgetClass,
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

    XtAddCallback(button1, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button2, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button3, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button4, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button5, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button1, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button2, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button3, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button4, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button5, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button1, XmNdisarmCallback, pb_disarm_callback, NULL);
    XtAddCallback(button2, XmNdisarmCallback, pb_disarm_callback, NULL);
    XtAddCallback(button3, XmNdisarmCallback, pb_disarm_callback, NULL);
    XtAddCallback(button4, XmNdisarmCallback, pb_disarm_callback, NULL);
    XtAddCallback(button5, XmNdisarmCallback, pb_disarm_callback, NULL);

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
