/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test2.c,v 1.6 2002/05/01 15:39:21 amai Exp $ */
/** test2 -- vertical column layout of widgets with two columns and XmNadjustLast = False.

    resulting layout should be:

    button1 button4
    button2 button5
    button3

    the layout should not change when the window is resized
**/

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>

#include "../../common/Test.h"


int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc, sep;
    Widget button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test2", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XtVaCreateManagedWidget("rowcolumn",
				 xmRowColumnWidgetClass,
				 toplevel,
				 XmNorientation, XmVERTICAL,
				 XmNpacking, XmPACK_COLUMN,
				 XmNnumColumns, 2,
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

    sep = XtVaCreateManagedWidget("button3",
				      xmSeparatorWidgetClass,
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
  	{CWWidth | CWHeight,		0,	0,	117,	87,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	3,	3,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	3,	31,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	3,	59,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	60,	3,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	60,	31,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	60,	59,	54,	25,	0,0,0,},
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}


