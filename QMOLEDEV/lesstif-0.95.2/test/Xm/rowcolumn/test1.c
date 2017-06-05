/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test1.c,v 1.13 2002/05/01 15:39:21 amai Exp $ */
/** test1 -- vertical tight layout of widgets with XmNadjustLast = False.

    resulting layout should be something like this:

    button1
    button2
    button3
    button4
    button5

    resizing smaller will yield something like this:

    button1 button4
    button2 button5
    button3

**/

#include <stdlib.h>
#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>
#include <Xm/PushB.h>

#include "../../common/Test.h"


void
resize(Widget w, XtPointer a, XtPointer b)
{
    XtUnmanageChild(w);
    XtVaSetValues(w, XmNwidth, 100, XmNheight, 100, NULL);
    XtManageChild(w);
}

void
changelab(Widget w, XtPointer a, XtPointer b)
{
    XmString str = XmStringCreateSimple("This is a changed string");

    XtVaSetValues(w, XmNlabelString, str, NULL);
}

void
unman(Widget w, XtPointer a, XtPointer b)
{
    XtUnmanageChild(w);
}

void
setpos(Widget w, XtPointer a, XtPointer b)
{
    XtVaSetValues(w, XmNpositionIndex, 1, NULL);
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test1", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XtVaCreateManagedWidget("rowcolumn",
				 xmRowColumnWidgetClass,
				 toplevel,
				 XmNorientation, XmVERTICAL,
				 XmNpacking, XmPACK_TIGHT,
				 XmNadjustLast, False,
				 XmNwidth, 500,
				 XmNheight, 500,
				 NULL);

    button1 = XtVaCreateManagedWidget("button1",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    button2 = XtVaCreateWidget("button2",
				      xmPushButtonWidgetClass,
				      rc,
				      XtNwidth, 200, XtNheight, 200,
				      NULL);
    XtManageChild(button2);

    XtAddCallback(button2, XmNactivateCallback, resize, NULL);

    button3 = XtVaCreateManagedWidget("button3",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);
    XtAddCallback(button3, XmNactivateCallback, changelab, NULL);

    button4 = XtVaCreateManagedWidget("button4",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);
    XtAddCallback(button4, XmNactivateCallback, unman, NULL);

    button5 = XtVaCreateManagedWidget("button5",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);
    XtAddCallback(button5, XmNactivateCallback, setpos, NULL);

    XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	{CWWidth | CWHeight,		0,	0,	60,	143,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	3,	3,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	3,	31,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	3,	59,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	3,	87,	54,	25,	0,0,0,},
  	{CWWidth | CWHeight | CWX | CWY,	3,	115,	54,	25,	0,0,0,},
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}
