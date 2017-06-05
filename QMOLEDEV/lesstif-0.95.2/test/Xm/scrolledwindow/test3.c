/* $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test3.c,v 1.5 2002/05/01 15:27:19 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/ScrolledWP.h>

#include "../../common/Test.h"


int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget shell, toplevel;
    Widget one, two, three;

    shell = XtVaAppInitialize(&theApp, "scrolledW", NULL, 0,
				 &argc, argv, NULL, NULL);

    toplevel = XmCreateScrolledWindow(shell, "ScrolledWindow", NULL, 0);
    XtManageChild(toplevel);

    one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				  NULL);

    two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNleftAttachment, XmATTACH_NONE,
				  XmNrightAttachment, XmATTACH_FORM,
				  NULL);

    three = XtVaCreateManagedWidget("three", xmPushButtonWidgetClass, one,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_WIDGET,
				    XmNrightWidget, two,
				    NULL);

    XtAddCallback(two, XmNactivateCallback, XdbPrintTreeCB, toplevel);
    XtAddCallback(three, XmNactivateCallback, XdbPrintTreeCB, toplevel);

    XtRealizeWidget(shell);


  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,   72,   25, 0,0,0, /* ScrolledWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   72,   25, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,   42,    0,   30,   25, 0,0,0, /* two */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   42,   25, 0,0,0, /* three */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

    exit(0);
}

