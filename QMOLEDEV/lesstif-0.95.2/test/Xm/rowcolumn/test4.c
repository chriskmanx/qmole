/** test4 -- horizontal column layout of widgets with two columns and XmNadjustLast = False.

    resulting layout should be:

    button1 button2 button3 
    button4 button5

    the layout shouldn't change upon window resizing.

**/

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget button1, button2, button3, button4, button5;
    XmString str = XmStringCreateLtoR("This sucker is\nlong and tall",
				      XmFONTLIST_DEFAULT_TAG);

    toplevel = XtVaAppInitialize(&theApp, "rc-test4", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XtVaCreateManagedWidget("rowcolumn",
				 xmRowColumnWidgetClass,
				 toplevel,
				 XmNorientation, XmHORIZONTAL,
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
				      XmNheight, 40, /* this gets ignored */
				      NULL);

    button3 = XtVaCreateManagedWidget("button3",
				      xmPushButtonWidgetClass,
				      rc,
				      XmNlabelString, str, /* but this doesn't*/
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
  	CWWidth | CWHeight,		0,	0,	300,	85,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	102,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	201,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	44,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	102,	44,	96,	38,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}


