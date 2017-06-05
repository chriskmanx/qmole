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

#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>
#include <Xm/PushB.h>

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
				 XmNorientation, XmHORIZONTAL,
				 XmNpacking, XmPACK_NONE,
				 XmNadjustLast, False,
				 XmNwidth, 500,
				 XmNheight, 500,
				 NULL);

    button1 = XtVaCreateManagedWidget("button1",
				      xmPushButtonWidgetClass,
				      rc,
				      XtNx, 10,
				      NULL);

    button2 = XtVaCreateWidget("button2",
				      xmPushButtonWidgetClass,
				      rc,
				      XtNwidth, 200, XtNheight, 200,
				      XtNx, 20,
				      NULL);
    XtManageChild(button2);

    XtAddCallback(button2, XmNactivateCallback, resize, NULL);

    button3 = XtVaCreateManagedWidget("button3",
				      xmPushButtonWidgetClass,
				      rc,
				      XtNx, 30,
				      NULL);
    XtAddCallback(button3, XmNactivateCallback, changelab, NULL);

    button4 = XtVaCreateManagedWidget("button4",
				      xmPushButtonWidgetClass,
				      rc,
				      XtNx, 40,
				      NULL);
    XtAddCallback(button4, XmNactivateCallback, unman, NULL);

    button5 = XtVaCreateManagedWidget("button5",
				      xmPushButtonWidgetClass,
				      rc,
				      XtNx, 50,
				      NULL);
    XtAddCallback(button5, XmNactivateCallback, setpos, NULL);

    XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	104,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	10,	0,	54,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	20,	0,	54,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	30,	0,	54,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	40,	0,	54,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	50,	0,	54,	25,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}


