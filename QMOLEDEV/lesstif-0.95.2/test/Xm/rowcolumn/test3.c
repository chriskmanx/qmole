/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test3.c,v 1.6 2002/05/01 15:39:21 amai Exp $ */
/** test3 -- horizontal tight layout of widgets with XmNadjustLast = False.

    resulting layout should be:

    button1 button2 button3 button4 button5

    and if the window is resized smaller, the layout should change to something like:

    button1 button2 button3 
    button4 button5 

**/

#include <stdlib.h>

#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>
#include <Xm/PushB.h>

#include "../../common/Test.h"


int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test3", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XtVaCreateManagedWidget("rowcolumn",
				 xmRowColumnWidgetClass,
				 toplevel,
				 XmNorientation, XmHORIZONTAL,
				 XmNpacking, XmPACK_TIGHT,
				 XmNadjustLast, False,
				 NULL);

    button1 = XtVaCreateManagedWidget("button1",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    button2 = XtVaCreateManagedWidget("button2",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    button3 = XtVaCreateManagedWidget("button3",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    button4 = XtVaCreateManagedWidget("button4",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    button5 = XtVaCreateManagedWidget("button5",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	{CWWidth | CWHeight,		0,	0,	288,	31,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	3,	3,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	60,	3,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	117,	3,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	174,	3,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	231,	3,	54,	25,	0,0,0,},
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}


